"""Session files: recording a run and replaying it, the Numbo GUI's `numbo/session.py`
adopted (ANSWERS.md 7; PANES.md section 5; G-046, G-047). Qt-free.

A session file is JSON lines, Numbo's shape with one line more, the run line, because a
Seek-Whence run is not only its seed and configuration (DECISION D-069) but also the
console lines the user typed and the answers to the guesses [M86 pp. 164-179]:

    {"format":"seekwhence-session","version":1}
    {"run":{"seed":54,"config":[["urgency_power",1],...],"inputs":["3","7","3","7"],
            "answers":["ok"],"mode":"scripted","preset":"run1_3737","engine":"0.0.1"}}
    {"kind":"start","seed":54,"config":[...]}      one line per event, in event_record form
    ...
    {"end":"seekwhence-session","events":174}

`SessionRecorder` is an observer that writes one: the header and the run line at once,
each event as it comes, and the end line with the count when the run ends (`ended`,
`capped`) or when it is closed, so a run saved part way is still a complete file (PANES.md
section 5: "the file then has no `ended` event"). `write_session` writes a recorded
history the same way; `read_session` validates the whole file (the header, the run line,
every event, the end line and its count) and raises `SessionError` naming the path and the
line for a file that is empty, not a session, of a newer version, corrupt, truncated (no
end line, or a cut line), miscounted, or continued after its end.

The events are stored as `seekwhence.trace.event_record` dicts, the engine's own JSON
form (DECISION D-069); `decode_event` gives back a typed event whose record equals the
stored one (lists become tuples again). A `Form` inside a task's arguments comes back as
its printed text, the text the trace prints for it, so what the panes show is the same
(G-046).

A replay re-executes the engine from the run line (`run_inputs`) through the run
controller and checks each event against the stored one as it goes, as Numbo's
`replay_oracle_trace` does: `ReplayChecker` is the observer that compares, `Divergence`
the first difference, `TraceMismatch` its error. `replay` does all of it Qt-free, for the
tests and the gate; the window does the same through its own controller (G-047).
"""

import dataclasses
import io
import json
import os

from seekwhence import __version__, events as ev
from seekwhence.config import Config
from seekwhence.gui.models.presets import RunInputs
from seekwhence.trace import event_record

__all__ = ["FORMAT", "VERSION", "MODES", "SessionError", "TraceMismatch", "RunLine",
           "SessionFile", "Divergence", "ReplayChecker", "ReplayResult", "SessionRecorder",
           "config_pairs", "config_from_pairs", "run_line_of", "run_inputs", "encode_event",
           "decode_event", "write_session", "read_session", "replay", "differences"]

FORMAT = "seekwhence-session"
VERSION = 1
MODES = ("scripted", "interactive", "judged")
# The kinds that end a run's events: a guess accepted, or the whole-run cap (DECISIONS
# D-011, D-053). A run that ended on `bye` or on the end of its input has no such event.
ENDING_KINDS = ("ended", "capped")


class SessionError(ValueError):
    """A session file that cannot be read or replayed; `path` and `line` (1-based, or
    None) say where (Numbo GUI, `session.py`)."""

    def __init__(self, message, path=None, line=None):
        self.path, self.line, self.reason = path, line, message
        where = ", ".join(x for x in (path and str(path),
                                      line is not None and f"line {line}") if x)
        super().__init__(f"{where}: {message}" if where else message)


class TraceMismatch(SessionError):
    """The re-run of a session file's run line differs from its stored events (Numbo GUI,
    `session.py`)."""


# ---------------------------------------------------------------------------------------
# Files and streams (Numbo GUI, `session.py`)

class _Opened:
    """`source` (a path or a text stream) as `(stream, name)`; a path is opened in `mode`
    and closed afterwards, a stream is left open (Numbo GUI, `session.py`)."""

    def __init__(self, source, mode="r"):
        self.owns = isinstance(source, (str, os.PathLike))
        self.stream = open(source, mode, encoding="utf-8") if self.owns else source
        self.name = (os.fspath(source) if self.owns
                     else getattr(source, "name", None) or "<stream>")

    def __enter__(self):
        return self.stream, self.name

    def __exit__(self, *exc):
        if self.owns:
            self.stream.close()


def _dumps(obj) -> str:
    return json.dumps(obj, allow_nan=False, separators=(",", ":"))


def _parse_line(line, name, lineno):
    try:
        return json.loads(line)
    except ValueError as e:
        cut = "" if line.endswith("\n") else " (truncated?)"
        raise SessionError(f"not valid JSON{cut}: {e}", name, lineno) from None


# ---------------------------------------------------------------------------------------
# The configuration as pairs (ANSWERS.md 5)

def _as_tuples(value):
    """JSON lists back to the tuples the `Config` and the events hold (G-046)."""
    if type(value) is list:
        return tuple(_as_tuples(v) for v in value)
    return value


def config_pairs(config: Config) -> list:
    """The configuration as `[field, value]` pairs in field order, as the `start` event
    records it (`World.config_pairs`; DECISION D-069; ANSWERS.md 5)."""
    return [[f.name, getattr(config, f.name)] for f in dataclasses.fields(config)]


def config_from_pairs(pairs) -> Config:
    """The `Config` the pairs stand for: `dataclasses.replace(Config(), **edits)`, the way
    the GUI builds a run's configuration (ANSWERS.md 5). A `SessionError` for a pair
    that is not `[name, value]`, a name `Config` lacks, or a value of another type than
    the default's (G-046)."""
    defaults = Config()
    names = {f.name for f in dataclasses.fields(defaults)}
    edits = {}
    if type(pairs) is not list:
        raise SessionError(f"the configuration is not a list of pairs: {pairs!r:.80}")
    for pair in pairs:
        if type(pair) is not list or len(pair) != 2 or type(pair[0]) is not str:
            raise SessionError(f"not a [field, value] pair: {pair!r:.80}")
        name, value = pair
        if name not in names:
            raise SessionError(f"Config has no field {name!r}")
        value = _as_tuples(value)
        if type(value) is not type(getattr(defaults, name)):
            raise SessionError(f"the configuration's {name} is {value!r:.60}, not a "
                               f"{type(getattr(defaults, name)).__name__}")
        edits[name] = value
    return dataclasses.replace(defaults, **edits)


# ---------------------------------------------------------------------------------------
# The run line (PANES.md section 5)

@dataclasses.dataclass(frozen=True)
class RunLine:
    """What a run was started from (PANES.md section 5): the `seed`, the `config`, the
    console `inputs` (terms and commands, as strings, in order), the `answers` to the
    guesses, the `mode` (`scripted`, or `interactive` when the user typed the inputs and
    answered the guesses in the GUI), the `preset` id and the `engine` version that ran
    it (G-046)."""
    seed: int
    config: Config
    inputs: tuple
    answers: tuple
    mode: str
    preset: str
    engine: str = __version__

    def to_json(self) -> dict:
        """The run line's object, `{"run": {...}}` (G-046)."""
        return {"run": {"seed": self.seed, "config": config_pairs(self.config),
                        "inputs": list(self.inputs), "answers": list(self.answers),
                        "mode": self.mode, "preset": self.preset, "engine": self.engine}}

    @classmethod
    def from_json(cls, obj) -> "RunLine":
        """The `RunLine` an object stands for; a `SessionError` for anything else
        (G-046)."""
        if type(obj) is not dict or set(obj) != {"run"} or type(obj["run"]) is not dict:
            raise SessionError(f"not a run line: {obj!r:.80}")
        run = obj["run"]
        keys = ("seed", "config", "inputs", "answers", "mode", "preset", "engine")
        missing = [k for k in keys if k not in run]
        extra = [k for k in run if k not in keys]
        if missing or extra:
            raise SessionError("the run line: " + "; ".join(
                x for x in (missing and "missing " + ", ".join(missing),
                            extra and "unexpected " + ", ".join(extra)) if x))
        if type(run["seed"]) is not int:
            raise SessionError(f"a bad seed {run['seed']!r}")
        for key in ("inputs", "answers"):
            if (type(run[key]) is not list
                    or not all(type(x) in (str, int) and type(x) is not bool for x in run[key])):
                raise SessionError(f"the run line's {key} must be a list of strings")
        if run["mode"] not in MODES:
            raise SessionError(f"a bad mode {run['mode']!r} (one of {', '.join(MODES)})")
        if type(run["preset"]) is not str or type(run["engine"]) is not str:
            raise SessionError("the run line's preset and engine must be strings")
        return cls(seed=run["seed"], config=config_from_pairs(run["config"]),
                   inputs=tuple(str(x) for x in run["inputs"]),
                   answers=tuple(str(x) for x in run["answers"]),
                   mode=run["mode"], preset=run["preset"], engine=run["engine"])


def run_line_of(inputs: RunInputs, typed=(), answered=()) -> RunLine:
    """The run line of a run started from `inputs` (`models/presets.py`): a scripted run's
    lines and answers are the inputs' own; a run whose lines the user `typed` (no
    scripted lines, G-015) stores them, and an interactive run stores the answers it was
    given (`answered`, the `answered` events' answers, G-008), as does a run `judged`
    against a continuation (the bench's knowing user, DECISION D-070; G-048), since every
    input is part of the run (ENGINE-API.md section 3; G-046)."""
    interactive = inputs.interactive or inputs.lines is None
    judged = inputs.continuation is not None
    lines = inputs.lines if inputs.lines is not None else tuple(typed)
    answers = tuple(answered) if inputs.interactive or judged else inputs.answers
    mode = "interactive" if interactive else "judged" if judged else "scripted"
    return RunLine(seed=inputs.seed, config=inputs.config,
                   inputs=tuple(str(x) for x in lines), answers=tuple(str(x) for x in answers),
                   mode=mode, preset=inputs.preset)


def run_inputs(run: RunLine, title="") -> RunInputs:
    """The `RunInputs` that re-execute a run line: always scripted, with the file's lines
    and answers, so the replay needs no one at the console (PANES.md section 5; G-047)."""
    return RunInputs(preset=run.preset, config=run.config, seed=run.seed,
                     lines=run.inputs, answers=run.answers, interactive=False,
                     title=title or f"replay: {run.preset}, seed {run.seed}")


# ---------------------------------------------------------------------------------------
# Events as JSON (DECISION D-069; Numbo GUI, `session.py`)

def encode_event(event) -> dict:
    """The event as a JSON object: `event_record`, its kind then its fields in order
    (DECISION D-069)."""
    return event_record(event)


def decode_event(obj):
    """The typed event a JSON object (`encode_event`'s) stands for, lists made tuples
    again; a `SessionError` if it stands for none (Numbo GUI, `session.py`; G-046)."""
    if type(obj) is not dict:
        raise SessionError(f"not a JSON object: {obj!r:.80}")
    if "kind" not in obj:
        raise SessionError("an event with no kind")
    cls = ev.EVENT_TYPES.get(obj["kind"])
    if cls is None:
        raise SessionError(f"unknown event kind {obj['kind']!r}")
    names = [f.name for f in dataclasses.fields(cls)]
    missing = [n for n in names if n not in obj]
    extra = [k for k in obj if k != "kind" and k not in names]
    if missing or extra:
        raise SessionError(f"a {cls.kind} event: " + "; ".join(
            x for x in (missing and "missing field(s) " + ", ".join(missing),
                        extra and "unexpected field(s) " + ", ".join(extra)) if x))
    return cls(**{n: _as_tuples(obj[n]) for n in names})


# ---------------------------------------------------------------------------------------
# Recording (Numbo GUI, `session.py`)

class SessionRecorder:
    """An observer that writes the session file `target` (a path, or a text stream it
    leaves open): the header and the `run` line now, then each event; the end line is
    written by `close()`, which an ending event calls when `close_at_end` (Numbo GUI,
    `session.py`; G-046)."""

    def __init__(self, target, run: RunLine, close_at_end=True):
        self._opened = _Opened(target, "w")
        self.stream, self.name = self._opened.stream, self._opened.name
        self.run = run
        self.close_at_end = close_at_end
        self.events_written = 0
        self.closed = False
        self.stream.write(_dumps({"format": FORMAT, "version": VERSION}) + "\n")
        self.stream.write(_dumps(run.to_json()) + "\n")

    def on_event(self, event) -> None:
        """Write the event's line (DECISION D-055)."""
        if self.closed:
            raise SessionError(f"the session recorder is closed; a {event.kind} event "
                               f"came after its end", self.name)
        self.stream.write(_dumps(encode_event(event)) + "\n")
        self.events_written += 1
        if self.close_at_end and event.kind in ENDING_KINDS:
            self.close()

    def close(self) -> None:
        """Write the end line and flush (a path's file is closed). Once (Numbo GUI,
        `session.py`)."""
        if self.closed:
            return
        self.closed = True
        self.stream.write(_dumps({"end": FORMAT, "events": self.events_written}) + "\n")
        self.stream.flush()
        self._opened.__exit__()


def write_session(target, run: RunLine, events) -> int:
    """Write the run line and the events as the session file `target` (a path or a text
    stream); returns how many events were written (Numbo GUI, `session.py`)."""
    recorder = SessionRecorder(target, run, close_at_end=False)
    try:
        for event in events:
            recorder.on_event(event)
    finally:
        recorder.close()
    return recorder.events_written


# ---------------------------------------------------------------------------------------
# Reading (Numbo GUI, `session.py`)

@dataclasses.dataclass(frozen=True)
class SessionFile:
    """A session file, read and checked: its `version`, `run` line, the typed `events`,
    their stored `records` (the parsed JSON, what a replay is compared with), and the
    `name` of the file (G-046)."""
    version: int
    run: RunLine
    events: tuple
    records: tuple
    name: str

    @property
    def ended(self) -> bool:
        """Whether the stored events end the run (`ended`, `capped`); otherwise the run
        was saved part way, or ended on `bye` or the end of its input, and a re-run may
        go on past the stored events (G-047)."""
        return bool(self.events) and self.events[-1].kind in ENDING_KINDS


def _check_header(obj, name) -> int:
    if type(obj) is not dict or obj.get("format") != FORMAT:
        raise SessionError(f"not a seekwhence session file: the first line is "
                           f"{obj!r:.80}", name, 1)
    version = obj.get("version")
    if type(version) is not int or version < 1:
        raise SessionError(f"a bad session version {version!r}", name, 1)
    if version > VERSION:
        raise SessionError(f"session version {version}, from a newer seekwhence; this one "
                           f"reads version {VERSION}", name, 1)
    return version


def read_session(source) -> SessionFile:
    """The session file `source` (a path or a text stream), whole and checked, as a
    `SessionFile`; a `SessionError` naming the file and the line for anything wrong
    (Numbo GUI, `session.py`'s `read_session` and `load_session`; G-046)."""
    with _Opened(source) as (f, name):
        lineno = 0
        version = None
        run = None
        events, records = [], []
        ended = False
        for lineno, line in enumerate(f, 1):
            if ended:
                raise SessionError("a line after the end line", name, lineno)
            obj = _parse_line(line, name, lineno)
            if lineno == 1:
                version = _check_header(obj, name)
            elif lineno == 2:
                try:
                    run = RunLine.from_json(obj)
                except SessionError as e:
                    raise SessionError(e.reason, name, lineno) from None
            elif type(obj) is dict and "end" in obj and "kind" not in obj:
                if obj != {"end": FORMAT, "events": obj.get("events")}:
                    raise SessionError(f"a bad end line {obj!r:.80}", name, lineno)
                if obj["events"] != len(events):
                    raise SessionError(f"the end line says {obj['events']} events, the "
                                       f"file has {len(events)}", name, lineno)
                ended = True
            else:
                try:
                    event = decode_event(obj)
                except SessionError as e:
                    raise SessionError(e.reason, name, lineno) from None
                events.append(event)
                records.append(obj)
        if lineno == 0:
            raise SessionError("an empty file, not a seekwhence session", name)
        if run is None:
            raise SessionError("truncated after line 1: no run line", name)
        if not ended:
            raise SessionError(f"truncated after line {lineno}: no end line (the recording "
                               f"was cut off)", name)
    return SessionFile(version, run, tuple(events), tuple(records), name)


# ---------------------------------------------------------------------------------------
# Replay (Numbo GUI, `session.py`'s `replay_oracle_trace`)

@dataclasses.dataclass(frozen=True)
class Divergence:
    """The first difference between a session file and its re-run: at event `index` (from
    0; its `line` in the file), `expected` (the stored record, or None past the file's
    end) and `got` (the re-run's record, or None after it ended); `error` is the
    `TraceMismatch` for it (Numbo GUI, `session.py`)."""
    index: int
    expected: object
    got: object
    error: TraceMismatch

    @property
    def line(self) -> int:
        """The file line of the stored event (the header and the run line come first)
        (G-046)."""
        return self.index + 3

    @property
    def message(self) -> str:
        """The error's text (Numbo GUI, `session.py`)."""
        return self.error.reason


def _show(x, limit=120) -> str:
    s = _dumps(x)
    return s if len(s) <= limit else s[:limit] + "..."


def differences(expected, got) -> str:
    """The fields of two records that differ, as `field: file ..., re-run ...` (the first
    four) (Numbo GUI, `session.py`)."""
    if type(expected) is dict and type(got) is dict:
        keys = list(expected) + [k for k in got if k not in expected]
        diffs = [k for k in keys if _dumps(expected.get(k)) != _dumps(got.get(k))
                 or (k in expected) != (k in got)]
        return "; ".join(f"{k}: file {_show(expected.get(k), 60)}, re-run "
                         f"{_show(got.get(k), 60)}" for k in diffs[:4])
    return f"file {_show(expected)}, re-run {_show(got)}"


class ReplayChecker:
    """The observer that checks a re-run against the file's stored events as they come
    (Numbo GUI, `session.py`'s `_TraceChecker`; G-047). Each event's record is compared
    with the stored one at the same index (as JSON text); the first difference is kept
    as `divergence` and given to `on_divergence`, and the events after it are not
    checked. A re-run that goes on past the stored events is a difference when the file's
    events end the run, and is let go on, counted in `beyond`, when they do not (the run
    was saved part way). `finish` adds the last check: a re-run that ended before the
    file's events did."""

    def __init__(self, file: SessionFile, on_divergence=None):
        self.file = file
        self.records = file.records
        self.name = file.name
        self.on_divergence = on_divergence
        self.seen = 0            # stored events matched so far
        self.beyond = 0          # re-run events past a part-way file's end
        self.divergence = None

    def _diverge(self, index, expected, got, message):
        self.divergence = Divergence(index, expected, got,
                                     TraceMismatch(message, self.name, index + 3))
        if self.on_divergence is not None:
            self.on_divergence(self.divergence)
        return self.divergence

    def on_event(self, event) -> None:
        """Check one event (DECISION D-055)."""
        self.check(event)

    def check(self, event):
        """Compare the re-run's next event with the stored one; returns the `Divergence`
        it makes, or None (G-047)."""
        if self.divergence is not None:
            return None
        index = self.seen + self.beyond
        got = encode_event(event)
        if index >= len(self.records):
            if self.file.ended:
                return self._diverge(index, None, got, (
                    f"the file ends after {len(self.records)} events, but the re-run goes "
                    f"on: {_show(got)}"))
            self.beyond += 1
            return None
        expected = self.records[index]
        if _dumps(expected) != _dumps(got):
            return self._diverge(index, expected, got, (
                f"the re-run differs from the file at event {index + 1}: "
                f"{differences(expected, got)}"))
        self.seen += 1
        return None

    def finish(self, stopped=False):
        """After the re-run: unless it was `stopped`, the file must have no event left;
        returns the first `Divergence`, found now or earlier, or None (G-047)."""
        if self.divergence is None and not stopped and self.seen < len(self.records):
            self._diverge(self.seen, self.records[self.seen], None, (
                f"the file goes on after the re-run ended: {len(self.records)} events "
                f"stored, {self.seen} re-run"))
        return self.divergence

    @property
    def verified(self) -> bool:
        """Whether every stored event was matched and nothing differed (G-047)."""
        return self.divergence is None and self.seen == len(self.records)


@dataclasses.dataclass(frozen=True)
class ReplayResult:
    """A replayed session file: the controller's `outcome` (`RunOutcome`), the re-run's
    `records`, the `divergence` (None: the re-run is the file) and whether the file was
    `verified` whole (G-047)."""
    outcome: object
    records: list
    divergence: object
    verified: bool


def replay(file: SessionFile, observers=(), out=None, check=True) -> ReplayResult:
    """Re-execute the file's run line through a Qt-free `RunController` with the
    `ReplayChecker` and `observers` attached, stopping at the first difference; raises
    its `TraceMismatch` at the end unless `check` is false (Numbo GUI, `session.py`'s
    `replay_oracle_trace`; G-047). `out` gets the transcript (captured by default)."""
    from seekwhence.events import Recorder
    from seekwhence.gui.models.run_controller import RunController

    recorder = Recorder()
    controller = RunController(observers=[recorder, *observers])
    checker = ReplayChecker(file, on_divergence=lambda d: controller.stop(wait=False))
    controller.subscribe(checker)
    inputs = run_inputs(file.run)
    controller.start_session(inputs.config, inputs.seed, inputs.lines, inputs.answers,
                             out=out)
    controller.run_to_end()
    controller.wait()
    outcome = controller.outcome
    if outcome.error is not None:
        raise outcome.error
    divergence = checker.finish(stopped=outcome.stopped)
    if check and divergence is not None:
        raise divergence.error
    return ReplayResult(outcome, [encode_event(e) for e in recorder.events], divergence,
                        checker.verified)
