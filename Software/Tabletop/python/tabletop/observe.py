"""The two traces of a run as observers of its events (D-065; `docs/trace-format.md`):
`TextTrace`, one line per codelet for a person, ending with a line in the style of French's
reports, and `JsonLines`, one JSON record per event for machines, which `Hasher` reduces to
the SHA-256 digest `tabletop.run.fingerprint` returns.

French's screen counted the codelets run and showed the thermometer beside them [F95 p. 96
(img)], and his reports name the object Eliza touches, the structure and the number of
codelets [F95 p. 145; FCCA p. 392]. Neither trace is read by the engine: an observer only
listens (`events.Publisher`), so a run is the same with both attached or with none.
"""

import hashlib
import json
import sys

from tabletop import events as EV
from tabletop.config import codelet_label

SLIPNET, ANSWER_SCHEDULE, START = "slipnet", "answer-schedule", "start"


def report_sentence(answer: str, label: str, literal: bool, capped: bool,
                    structure_value: float, codelets: int, temperature: float) -> str:
    """The last line of the text trace, in the style of French's reports [F95 p. 145]
    (`docs/trace-format.md`): `Eliza touches small-glass3 (E1). Structure 186. Codelets 64.
    T 2.0.`, with `literal` after a literal answer (D-063) and `(capped)` after the count
    of a capped run (D-061)."""
    tag = f"({label})" if label else "(none)"
    word = " literal" if literal else ""
    cap = " (capped)" if capped else ""
    return (f"Eliza touches {answer} {tag}{word}. Structure {structure_value:.0f}. "
            f"Codelets {codelets}{cap}. T {temperature:.1f}.")


def codelet_text(codelet: str, args, generation: int = 0) -> str:
    """A codelet as the text trace prints it: the strip's label [F95 p. 107 (img)] (D-051),
    its arguments in parentheses and its generation after a slash when it is not 0
    (`FD-NNBR(fork2)/2`) [F95 p. 81]."""
    text = f"{codelet_label(codelet)}({' '.join(args)})"
    return f"{text}/{generation}" if generation else text


class TextTrace:
    """The text trace (`docs/trace-format.md`): the first line names the problem, the seed,
    the profile and the touched object; then one line per codelet (its ordinal, T, a(T),
    the codelet, what it did, and its side effects: jolts, postings, push and pop), with
    indented lines for what happens between codelets (the Slipnet's postings, a
    temperature change, codelets aged out or pushed off, the find-answer schedule, and the
    `RL` lines of the role events, D-105); the
    last line is `report_sentence`. With `quiet` only the first and last lines are
    printed. Lines go to `stream` (stdout by default) and are kept in `lines` (D-065)."""

    def __init__(self, stream=None, quiet: bool = False):
        self.stream = stream if stream is not None else sys.stdout
        self.quiet = quiet
        self.lines: list = []
        self._head = None        # the current codelet line, up to its action
        self._effects: list = []  # the codelet's side effects, in order
        self._notes: list = []    # indented lines after the codelet
        self._codelet = START
        self._last_t = None
        self._answers: list = []  # urgencies of the find-answers the schedule posted
        self._slipnet: list = []  # [label, count] of the codelets the Slipnet posted

    def emit(self, line: str) -> None:
        """Print and keep one line (D-065); a stream whose reader has gone (a closed pipe) is
        dropped silently and the run goes on."""
        self.lines.append(line)
        if self.stream is not None:
            try:
                print(line, file=self.stream)
            except BrokenPipeError:
                self.stream = None

    def flush(self) -> None:
        """Print the codelet line being assembled and the notes after it (D-065)."""
        if self._head is not None:
            tail = "; ".join(self._effects)
            self.emit(self._head + (f"; {tail}" if tail else ""))
        if self._slipnet:
            words = " ".join(f"{l} x{n}" if n > 1 else l for l, n in self._slipnet)
            self._notes.insert(0, f"slipnet posts {words}")
        if self._answers:
            count = len(self._answers)
            self._notes.append(f"answer schedule: FND-ANS x{count} at urgency {self._answers[-1]:.1f}")
        for note in self._notes:
            self.emit("      " + note)
        self._head, self._effects, self._notes = None, [], []
        self._answers, self._slipnet = [], []

    def on_event(self, e: EV.Event) -> None:
        """Turn one event into its part of a line (D-065)."""
        if isinstance(e, EV.RunStarted):
            changed = "".join(f"  {k}={v}" for k, v in e.changed)
            self.emit(f"tabletop {e.problem}  seed {e.seed}  profile {e.profile}{changed}  "
                      f"engine {e.engine}  Henry touches {e.touched}")
        elif isinstance(e, EV.RunEnded):
            self._end(e)
        elif self.quiet:
            return
        elif isinstance(e, EV.CodeletChosen):
            self.flush()
            self._codelet = e.codelet
            self._head = (f"{e.clock:4d}  T={e.temperature:.1f}  a={e.a:.1f}  "
                          f"{codelet_text(e.codelet, e.args, e.generation)}")
        elif isinstance(e, EV.CodeletRan):
            self._ran(e)
        elif isinstance(e, EV.NodeJolted) and e.by == self._codelet and self._head is not None:
            self._effects.append(f"jolt {e.node} +{e.amount:.0f}")
        elif isinstance(e, EV.ContextPopped):
            self._effects.append(f"pop {e.correspondence}")
        elif isinstance(e, EV.ContextPushed):
            self._effects.append(f"push {e.correspondence}")
        elif isinstance(e, EV.CodeletPosted):
            self._posted(e)
        elif isinstance(e, EV.CodeletRemoved):
            word = "aged out" if e.reason == "age" else "pushed off"
            self._notes.append(f"{word}: {codelet_text(e.codelet, e.args)} posted at {e.posted_at}")
        elif isinstance(e, EV.TemperatureUpdated):
            self._temperature(e)
        elif isinstance(e, EV.AnswerCodeletPosted):
            self._answers.append(e.urgency)
        elif isinstance(e, (EV.RoleTagged, EV.RoleDiscarded, EV.RoleConsumed, EV.RoleRecast)):
            self._notes.append(role_line(e))

    def _ran(self, e: EV.CodeletRan) -> None:
        if self._head is None:
            return
        if e.action == "fizzled":
            self._head += f"  -> fizzled ({e.detail})"
        else:
            self._head += f"  -> {e.action} {e.detail}".rstrip()
        if e.posted:
            labels = []
            for t in e.posted:
                label = codelet_label(t)
                if labels and labels[-1][0] == label:
                    labels[-1][1] += 1
                else:
                    labels.append([label, 1])
            words = " ".join(f"{l} x{n}" if n > 1 else l for l, n in labels)
            self._effects.append(f"posted {len(e.posted)} ({words})")

    def _posted(self, e: EV.CodeletPosted) -> None:
        if e.by == SLIPNET:
            label = codelet_label(e.codelet)
            if self._slipnet and self._slipnet[-1][0] == label:
                self._slipnet[-1][1] += 1
            else:
                self._slipnet.append([label, 1])
        elif e.by == START and e.clock > 0:
            if not self._notes or not self._notes[-1].startswith("rack reloaded"):
                self._notes.append("rack reloaded with the starting set")

    def _temperature(self, e: EV.TemperatureUpdated) -> None:
        if self._last_t is not None and e.temperature != self._last_t:
            self._notes.append(f"T {self._last_t:.1f} -> {e.temperature:.1f} (dQ {e.dq:.2f} "
                               f"Q {e.q:.2f} dWV {e.dwv} Tcp {e.tcp}) by {e.by}")
        self._last_t = e.temperature

    def _end(self, e: EV.RunEnded) -> None:
        if not self.quiet:
            self.flush()
        self.emit(report_sentence(e.answer, e.label, e.literal, e.capped, e.structure_value,
                                  e.codelets, e.temperature))


def role_line(e: EV.Event) -> str:
    """A role event as the text trace's `RL` note (D-105; `docs/trace-format.md`): `RL tag
    r1 cup2 LONER w1 s42 by perceived`, `RL drop r1 cup2 LONER (lost to r2)`, `RL used r1
    LONER by C1: create-cp cup2 small-glass1`, `RL recast cup1 [...] tiers ...`."""
    if isinstance(e, EV.RoleTagged):
        args = ":" + " ".join(e.args) if e.args else ""
        return (f"RL tag {e.tag} {e.item} {e.role}{args} w{e.weight} s{e.strength:.0f} "
                f"by {e.by}")
    if isinstance(e, EV.RoleDiscarded):
        return f"RL drop {e.tag} {e.item} {e.role} ({e.reason})"
    if isinstance(e, EV.RoleConsumed):
        return f"RL used {e.item} {e.role} by {e.consumer}: {e.effect}"
    tiers = " ".join(f"{t}:{c}:{w:g}" for t, c, w in e.tiers)
    word = " ambiguous" if e.ambiguous else ""
    return f"RL recast {e.touched} [{' '.join(e.signature)}] tiers {tiers}{word}"


def json_line(event: EV.Event) -> str:
    """One event as its JSON record on one line, keys in order and no whitespace
    (`docs/trace-format.md`, D-065), without the newline."""
    return json.dumps(EV.record_of(event), separators=(",", ":"))


class JsonLines:
    """The JSON-lines trace (`docs/trace-format.md`): one record per event, in order, each
    on its own line of `stream` (stdout by default). `count` is the lines written (D-065)."""

    def __init__(self, stream=None):
        self.stream = stream if stream is not None else sys.stdout
        self.count = 0

    def on_event(self, event: EV.Event) -> None:
        """Write the event's record (D-065); a stream whose reader has gone (a closed pipe) is
        dropped silently and the run goes on."""
        self.count += 1
        if self.stream is not None:
            try:
                self.stream.write(json_line(event) + "\n")
            except BrokenPipeError:
                self.stream = None


class Hasher:
    """The SHA-256 of the JSON-lines trace, line by line, each line ending in a newline
    (`docs/trace-format.md`, "What the fingerprint hashes"; D-065). `digest()` gives the
    hex digest, `count` the lines hashed."""

    def __init__(self):
        self._hash = hashlib.sha256()
        self.count = 0

    def on_event(self, event: EV.Event) -> None:
        """Hash the event's line (D-065)."""
        self._hash.update((json_line(event) + "\n").encode("utf-8"))
        self.count += 1

    def digest(self) -> str:
        """The hex digest so far (D-065)."""
        return self._hash.hexdigest()
