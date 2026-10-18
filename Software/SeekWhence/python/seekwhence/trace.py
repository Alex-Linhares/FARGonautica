"""The text trace in the transcripts' formats [M86 pp. 164-179] (DECISION D-055).

The three appendix runs print `-> (startup)`, a blank line and `please enter a term: 3` for
each term, `doing task Sparkler-plus on (glint1 glint1 2)` for a task with arguments and
`doing task Dissolver` for one without [M86 p. 164], and one line per happening
(`spark1 --- between glint1glint2`, `Same -cover proposed --> glint1`, `Glommer for Same
print-value fence members: (glint1 glint2)`, `failed to glom (glint1 glint2 glint3)`
[M86 pp. 164-165], `Glomtester failed glint2` [M86 p. 170], `create-template-glom (Cycle 3 2
(3 7))(glom1)`, `top-down glom glom2`, `template made : (form (Cycle 3 2 (3 7)) state
working coverage (1 2) glom glom2)`, `check-cycle template (Cycle 3 2 ((3 7)))`
[M86 p. 167], `gnoths constructed` [M86 p. 167], the hypothesis's form on its own
line after `doing task Hypothesizer` [M86 p. 171], `new term being hypothesis-filtered
through (Cycle 3 2 (3 7))`, `set-out -- validity: 0` [M86 p. 168], the Gnoth-setter's
`gnoths: (gnoth1 gnoth2 gnoth3)` and the two-line `slip-check: stayval: 15.0` /
`        best: (Y-group 4.0)` [M86 pp. 173, 176], the reconstruction's own `Changing to a new
reigning class. Countup --> S-group` (DECISION D-042), and the guess: a blank
line, `I have a guess!`, a blank line, `hypothesis: (Cycle 3 2 (3 7))`, the block `3 7 3 7`,
a blank line, `enter no if wrong, ok if right ok` with the answer on the prompt line, and
`bye` [M86 p. 166]; the block is printed flush left as runs 2 and 3 print it [M86 pp. 169,
175], where run 1 indents it [M86 p. 166]); the Gnoth-caster's `casts: ((S-group 1 2)
(S-group 1 1))`, the generalised form on the next line and `new hypoth candidate (S-group 1
2)` [M86 p. 174] (run 2's printout runs the casts and the form together on one wrapped line
[M86 p. 168]; the trace prints two lines, as run 3 does), and the reconstruction's own
`blasting away all gnoths and gloms` (DECISION D-050). `Trace` is an
observer that writes those lines from the engine's events, so that our trace can be read
side by side with hers; it changes nothing in the run. The second run prints no `doing task`
lines [M86 p. 167], which is `verbose=False` (DECISION D-055, `Config.verbose`). The two
prompt lines (`please enter a term: 3`, `enter no if wrong, ok if right ok`) are written by
the trace when it is the only writer, as in the engine's tests; a `Session` prints the
prompts itself before it reads the user's line and turns them off here with `prompts=False`
(DECISION D-069). `JsonLines` is a second observer, writing every event as one JSON object
per line, the repository's JSON-lines trace idea (the Numbo port's `--trace FILE`). Where the
72-column printout wrapped a task's arguments onto a continuation line (`doing task Bonder
on` / ` (Same print-value (remote) glint1 glint3)` [M86 p. 164]) the trace prints one line;
the test data's reader joins the printout's continuation lines the same way.
"""

import dataclasses
import json
import sys

from seekwhence.events import Event
from seekwhence.forms import form_text, is_form


def args_text(args) -> str:
    """A task's arguments as the transcripts print them: `(glint2 glint1 10)`, nested lists
    in their own parentheses, `(Same print-value (remote) glint1 glint3)` [M86 p. 164].
    Names and numbers print as they are, a structure by its `name`, a form in the program
    spelling (`(Cycle 3 2 (3 7))` [M86 p. 165])."""
    return "(" + " ".join(item_text(item) for item in args) + ")"


def item_text(item) -> str:
    """One argument as the transcripts print it: `t`/`nil` for a boolean or None, a number
    or a name as it is, a form in the program spelling, a list in parentheses, a structure
    by its `name` [M86 pp. 164-165] (DECISION D-055)."""
    if isinstance(item, bool):
        return "t" if item else "nil"
    if isinstance(item, (int, float)):
        return str(item)
    if isinstance(item, str):
        return item
    if item is None:
        return "nil"
    if is_form(item):
        return form_text(item)
    if isinstance(item, (tuple, list)):
        return args_text(item)
    name = getattr(item, "name", None)
    if isinstance(name, str):
        return name
    return str(item)


def task_line(type: str, args: tuple) -> str:
    """The `doing task` line: `doing task Tester` without arguments, `doing task
    Sparkler-plus on (glint2 glint1 10)` with them [M86 p. 164]."""
    if not args:
        return f"doing task {type}"
    if args == (None,):
        return f"doing task {type} on nil"     # `Template-evaluator on nil` [M86 p. 171]
    return f"doing task {type} on {args_text(args)}"


class Trace:
    """An observer that writes the transcripts' lines to `out` (DECISION D-055).

    `verbose` prints the `doing task` lines as runs 1 and 3 do [M86 pp. 164, 170]; without it
    only the happenings are printed, as in run 2 [M86 p. 167]. `prompts` prints the two
    prompt lines with the user's input on them (`please enter a term: 3` [M86 p. 164],
    `enter no if wrong, ok if right ok` [M86 p. 166]); a `Session` turns it off and prints
    them itself, before it reads the input (DECISION D-069). `lines` keeps everything
    written, for tests.
    """

    def __init__(self, out=None, verbose: bool = True, prompts: bool = True):
        self.out = out if out is not None else sys.stdout
        self.verbose = verbose
        self.prompts = prompts
        self.lines: list = []

    def write(self, line: str) -> None:
        """Write one line and remember it (DECISION D-055)."""
        self.lines.append(line)
        self.out.write(line + "\n")

    def on_event(self, event: Event) -> None:
        """Print the line the transcripts print for this event, if any [M86 pp. 164-179].
        One branch per event kind, in the order the lines first appear in the printouts; the
        function is long because the table is, and is kept whole so that each printed line
        can be read beside its event (the audit of item 24, `docs/citation-audit.md`)."""
        kind = event.kind
        if kind == "start":
            self.write("-> (startup)")
        elif kind == "term":
            if self.prompts:
                self.write("")
                self.write(f"please enter a term: {event.term}")
        elif kind == "chosen":
            if self.verbose:
                self.write(task_line(event.type, event.args))
        elif kind == "spark":
            self.write(f"{event.name} --- between {event.a}{event.b}")
        elif kind == "bond":
            self.write(f"{event.name} --- between {event.a}{event.b}")
        elif kind == "proposed":
            self.write(f"{event.family} -{event.test} proposed --> {event.target}")
        elif kind == "glomtester-failed":
            self.write(f"Glomtester failed {event.target}")
        elif kind == "glom":
            self.write(f"Glommer for {' '.join(event.type)} members: "
                       f"({' '.join(event.members)})")
        elif kind == "glom-failed":
            self.write(f"failed to glom ({' '.join(event.members)})")
        elif kind == "template-glom":
            self.write(f"create-template-glom {event.form}({' '.join(event.members)})")
        elif kind == "top-down":
            self.write(f"top-down glom {event.name}")
        elif kind == "template":
            # `template made : (form ...` from a Template-applier [M86 p. 167], `template:
            # (form ...` when the Template-scout applied the form itself [M86 p. 171]
            # (DECISION D-060)
            heading = "template:" if event.immediate else "template made :"
            self.write(f"{heading} (form {event.form} state {event.state} coverage "
                       f"({event.coverage[0]} {event.coverage[1]}) glom {event.glom})")
        elif kind == "template-check":
            if event.text:
                self.write(event.text)
        elif kind == "gnoths-constructed":
            self.write("gnoths constructed")
        elif kind == "hypothesis":
            self.write(event.form)
        elif kind == "filtered":
            self.write(f"new term being hypothesis-filtered through {event.form}")
        elif kind == "refuted":
            # printed by the terse run 2 after the filtering line [M86 p. 168] and by
            # neither refutation of the verbose run 3 [M86 pp. 171-172, 176]: in the mode
            # without task lines the refutation announces itself (DECISION D-055, item 21)
            if not self.verbose:
                self.write("set-out -- validity: 0")
        elif kind == "guess":
            self.write("")
            self.write("I have a guess!")
            self.write("")
            self.write(f"hypothesis: {event.form}")
            self.write(event.block)
        elif kind == "answered":
            if self.prompts:
                self.write("")
                self.write(f"enter no if wrong, ok if right {event.answer}")
            if event.accepted:
                self.write("bye")
        elif kind == "gnoths-set":
            self.write(f"gnoths: ({' '.join(event.names)})")
        elif kind == "slip-check":
            self.write(f"slip-check: stayval: {event.stayval}")
            best = "nil" if event.best is None else f"({event.best} {event.pressure})"
            self.write(f"        best: {best}")
        elif kind == "class-changed":
            self.write(f"Changing to a new reigning class. {event.old} --> {event.new}")
        elif kind == "casts":
            self.write(f"casts: {event.casts}")
            if event.generalised is not None:
                self.write(event.generalised)
        elif kind == "candidate":
            self.write(f"new hypoth candidate {event.form}")
        elif kind == "blasted":
            self.write("blasting away all gnoths and gloms")
        elif kind == "message":
            self.write(event.text)


# ---- the JSON-lines trace (DECISION D-069) ----------------------------------------------------

def json_value(value):
    """An event field as a JSON value: numbers, strings, booleans and None as they are,
    tuples and lists as lists, anything else (a form, a structure) as the text the
    transcripts would print for it (`item_text`) (DECISION D-069)."""
    if value is None or isinstance(value, (bool, int, float, str)):
        return value
    if isinstance(value, (tuple, list)):
        return [json_value(item) for item in value]
    return item_text(value)


def event_record(event: Event) -> dict:
    """One event as a dictionary: its `kind` first, then its fields in order, each a JSON
    value (DECISION D-069)."""
    record = {"kind": event.kind}
    for field in dataclasses.fields(event):
        record[field.name] = json_value(getattr(event, field.name))
    return record


class JsonLines:
    """An observer that writes every event as one JSON object per line to `out`, the
    machine-readable twin of `Trace` (DECISION D-069; the repository's JSON-lines traces:
    a trace should record enough to place a divergence). `count` is
    how many lines were written."""

    def __init__(self, out):
        self.out = out
        self.count = 0

    def on_event(self, event: Event) -> None:
        """Write the event's record as one line (DECISION D-069)."""
        self.out.write(json.dumps(event_record(event), separators=(",", ":")) + "\n")
        self.count += 1
