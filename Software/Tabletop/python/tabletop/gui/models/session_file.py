"""Session files: recording a run and replaying it, the Numbo GUI's `numbo/session.py`
adopted through the Seek-Whence GUI's `models/session_file.py` (ANSWERS.md 5; PANES.md
section 5; G-048, G-049). Qt-free.

A session file is JSON lines: one header line, the engine's JSON-lines trace as its body,
and an end line when the run had ended (PANES.md section 5):

    {"tabletop-gui-session":1,"problem":"surround-05","profile":"standard","edits":{},
     "seed":3,"version":"0.0.1"}
    {"event":"RunStarted","clock":0,"draws":0,...}      one line per event, byte for byte
    ...                                                  as `python3 -m tabletop --json -`
    {"end":496,"digest":"<sha256 of the body lines>"}    absent when saved mid-run

The header (`SessionHeader`) is what the run was started from: the `problem`, a name of the
library or, for an edited table, the problem's own dict (`problem_dict`, the file form
`problems.problem_from_dict` reads back); the `profile` and the `edits` over it, so that the
run's configuration is `dataclasses.replace(PROFILES[profile], **edits)`, the way the GUI
builds every configuration (ANSWERS.md 3; `config_of`); the `seed`; and the engine's
`version`. The body lines are `observe.json_line` per event (`docs/trace-format.md`,
D-065), so for a named problem with no edits they equal the CLI's `--json -` output and the
end line's digest equals `run.fingerprint`'s (`observe.Hasher`, each line with its newline).

`SessionRecorder` is an observer that writes one: the header at once, each event as it
comes, and the end line when the `RunEnded` event comes, or on `close()` if the run had
ended; a run saved part way has no end line (PANES.md section 5). `write_session` writes a
recorded history the same way; `read_session` checks the whole file (the header, every
event line, the end line's count and digest) and raises `SessionError` naming the path and
the line for a file that is empty, not a session, of a newer version, corrupt, cut in the
middle of a line, missing its end line after a `RunEnded`, miscounted, whose digest is not
the body's, or continued after its end.

A replay re-executes the header's run through the run controller and checks each event
against the stored line as it comes, as the Numbo GUI's `replay_oracle_trace` does:
`ReplayChecker` is the observer that compares, `Divergence` the first difference,
`TraceMismatch` its error. `replay` does all of it Qt-free, for the tests; the window does
the same through its own controller (G-049).
"""

import dataclasses
import hashlib
import json
import os

from tabletop import __version__
from tabletop import events as ev
from tabletop.config import PROFILES, Config
from tabletop.gui.models.config_table import ConfigError, conform
from tabletop.observe import json_line
from tabletop.problems import Problem, catalogue, load_problem, problem_from_dict, validate

__all__ = ["FORMAT_KEY", "VERSION", "SessionError", "TraceMismatch", "SessionHeader",
           "SessionFile", "Divergence", "ReplayChecker", "ReplayResult", "SessionRecorder",
           "problem_dict", "header_of", "problem_of", "config_of", "decode_event",
           "digest_of", "write_session", "read_session", "replay", "differences"]

FORMAT_KEY = "tabletop-gui-session"
VERSION = 1
HEADER_KEYS = ("problem", "profile", "edits", "seed", "version")


class SessionError(ValueError):
    """A session file that cannot be read or replayed; `path` and `line` (1-based, or
    None) say where (Numbo GUI, `session.py`)."""

    def __init__(self, message, path=None, line=None):
        self.path, self.line, self.reason = path, line, message
        where = ", ".join(x for x in (path and str(path),
                                      line is not None and f"line {line}") if x)
        super().__init__(f"{where}: {message}" if where else message)


class TraceMismatch(SessionError):
    """The re-run of a session file's header differs from its stored events (Numbo GUI,
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


def digest_of(lines) -> str:
    """The SHA-256 hex digest of the body lines, each followed by a newline: what
    `run.fingerprint` hashes (`observe.Hasher`; `docs/trace-format.md`; D-065)."""
    hasher = hashlib.sha256()
    for line in lines:
        hasher.update((line + "\n").encode("utf-8"))
    return hasher.hexdigest()


# ---------------------------------------------------------------------------------------
# The problem as a dict (PANES.md section 5: "{problem dict for an edited table}")

def problem_dict(problem: Problem) -> dict:
    """The problem in the file form of `problems/` that `problems.problem_from_dict`
    reads back (D-003; G-048): the keys of `problems.REQUIRED`, the table as `width`,
    `height` and `frame_px`, each object with its provenance flattened (`figure`,
    `page`, `image`), `notes`, and `based_on` and `expected` when set. Tuples become
    lists when written and come back as tuples."""
    data = {"name": problem.name, "title": problem.title, "family": problem.family,
            "source": problem.source,
            "table": {"width": problem.width, "height": problem.height,
                      "frame_px": list(problem.frame_px)},
            "touched": problem.touched, "answers": dict(problem.answers),
            "objects": [{"id": o.id, "type": o.type, "x": o.x, "y": o.y,
                         "orientation": o.orientation, "box_px": list(o.box_px),
                         "figure": o.provenance.figure, "page": o.provenance.page,
                         "image": o.provenance.image} for o in problem.objects],
            "method": problem.method, "uncertainty": problem.uncertainty,
            "notes": list(problem.notes), "review": problem.review}
    if problem.based_on is not None:
        data["based_on"] = problem.based_on
    if problem.expected is not None:
        data["expected"] = problem.expected
    return data


def _is_library_problem(problem: Problem) -> bool:
    """Whether the problem is the library's problem of its name, unchanged (G-048)."""
    if problem.name not in catalogue():
        return False
    try:
        return load_problem(problem.name) == problem
    except (OSError, ValueError):
        return False


# ---------------------------------------------------------------------------------------
# The header (PANES.md section 5)

@dataclasses.dataclass(frozen=True)
class SessionHeader:
    """What a run was started from (PANES.md section 5; G-048): the `problem` (a name of
    the library, or the dict of an edited table), the `profile`, the `edits` over it
    (field to value, each in the engine's types), the `seed`, and the engine `version`
    that ran it."""
    problem: object
    profile: str
    edits: dict
    seed: int
    version: str = __version__

    def to_json(self) -> dict:
        """The header line's object (G-048)."""
        return {FORMAT_KEY: VERSION, "problem": self.problem, "profile": self.profile,
                "edits": dict(self.edits), "seed": self.seed, "version": self.version}

    @classmethod
    def from_json(cls, obj) -> "SessionHeader":
        """The `SessionHeader` an object stands for, every part checked; a
        `SessionError` for anything else (G-048)."""
        if type(obj) is not dict or FORMAT_KEY not in obj:
            raise SessionError(f"not a tabletop session file: the first line is {obj!r:.80}")
        version = obj[FORMAT_KEY]
        if type(version) is not int or version < 1:
            raise SessionError(f"a bad session version {version!r}")
        if version > VERSION:
            raise SessionError(f"session version {version}, from a newer tabletop GUI; this "
                               f"one reads version {VERSION}")
        missing = [k for k in HEADER_KEYS if k not in obj]
        extra = [k for k in obj if k != FORMAT_KEY and k not in HEADER_KEYS]
        if missing or extra:
            raise SessionError("the header: " + "; ".join(
                x for x in (missing and "missing " + ", ".join(missing),
                            extra and "unexpected " + ", ".join(extra)) if x))
        problem = obj["problem"]
        if type(problem) is str:
            if problem not in catalogue():
                raise SessionError(f"no problem named {problem!r} in the library")
        elif type(problem) is dict:
            faults = validate(problem)
            if faults:
                raise SessionError("the problem dict: " + "; ".join(faults))
        else:
            raise SessionError(f"the problem must be a name or a problem dict, got "
                               f"{problem!r:.60}")
        if obj["profile"] not in PROFILES:
            raise SessionError(f"no profile named {obj['profile']!r} (one of "
                               f"{', '.join(PROFILES)})")
        if type(obj["seed"]) is not int:
            raise SessionError(f"a bad seed {obj['seed']!r}")
        if type(obj["version"]) is not str:
            raise SessionError("the engine version must be a string")
        edits = _checked_edits(obj["edits"], obj["profile"])
        return cls(problem=problem, profile=obj["profile"], edits=edits, seed=obj["seed"],
                   version=obj["version"])


def _checked_edits(edits, profile: str) -> dict:
    """The edits as a dict in the engine's types, each conformed to its field (G-048)."""
    if type(edits) is not dict:
        raise SessionError(f"the edits must be an object, got {edits!r:.60}")
    base = PROFILES[profile]
    names = {f.name for f in dataclasses.fields(base)}
    out = {}
    for name, value in edits.items():
        if name not in names or name == "profile":
            raise SessionError(f"Config has no editable field {name!r}")
        try:
            out[name] = conform(value, getattr(base, name))
        except ConfigError as e:
            raise SessionError(f"the edit of {name}: {e}") from None
    return out


def header_of(problem, config: Config, seed: int) -> SessionHeader:
    """The header of a run started from `problem` (a name, or a `Problem`: the library's
    problem of its name is stored by name, any other as its dict), `config` and `seed`
    (G-048): the profile is the configuration's and the edits are the fields on which it
    differs from that profile. A configuration whose profile is none of `PROFILES` cannot
    be stored (`ValueError`): the GUI builds every configuration from a profile."""
    if config.profile not in PROFILES:
        raise ValueError(f"the configuration's profile {config.profile!r} is not one of "
                         f"{', '.join(PROFILES)}")
    if isinstance(problem, Problem):
        problem = problem.name if _is_library_problem(problem) else problem_dict(problem)
    edits = {k: v for k, v in config.differences(PROFILES[config.profile]).items()
             if k != "profile"}
    return SessionHeader(problem=problem, profile=config.profile, edits=edits, seed=seed)


def problem_of(header: SessionHeader) -> Problem:
    """The `Problem` the header's run was started from: the library's for a name,
    `problems.problem_from_dict` for a dict (after `problems.validate`, which
    `from_json` ran) (G-048)."""
    if isinstance(header.problem, str):
        return load_problem(header.problem)
    return problem_from_dict(header.problem)


def config_of(header: SessionHeader) -> Config:
    """The run's configuration: `dataclasses.replace(PROFILES[profile], **edits)`, the
    way the GUI builds a configuration (ANSWERS.md 3; G-048)."""
    return dataclasses.replace(PROFILES[header.profile], **header.edits)


# ---------------------------------------------------------------------------------------
# Events as JSON (D-065; Numbo GUI, `session.py`)

def _as_tuples(value):
    """JSON lists back to the tuples the events hold (G-048)."""
    if type(value) is list:
        return tuple(_as_tuples(v) for v in value)
    return value


def decode_event(obj):
    """The typed event a JSON record (`events.record_of`'s) stands for, lists made tuples
    again; a `SessionError` if it stands for none (Numbo GUI, `session.py`; G-048)."""
    if type(obj) is not dict:
        raise SessionError(f"not a JSON object: {obj!r:.80}")
    if "event" not in obj:
        raise SessionError("an event with no name")
    cls = next((c for c in ev.EVENT_TYPES if c.__name__ == obj["event"]), None)
    if cls is None:
        raise SessionError(f"unknown event {obj['event']!r}")
    names = [f.name for f in dataclasses.fields(cls)]
    missing = [n for n in names if n not in obj]
    extra = [k for k in obj if k != "event" and k not in names]
    if missing or extra:
        raise SessionError(f"a {cls.__name__} event: " + "; ".join(
            x for x in (missing and "missing field(s) " + ", ".join(missing),
                        extra and "unexpected field(s) " + ", ".join(extra)) if x))
    return cls(**{n: _as_tuples(obj[n]) for n in names})


# ---------------------------------------------------------------------------------------
# Recording (Numbo GUI, `session.py`)

class SessionRecorder:
    """An observer that writes the session file `target` (a path, or a text stream it
    leaves open): the header now, then each event's line; the end line (the count and the
    digest) when a `RunEnded` event comes, if `close_at_end`, or on `close()` once the run
    had ended; a run closed part way has no end line (PANES.md section 5; Numbo GUI,
    `session.py`; G-048)."""

    def __init__(self, target, header: SessionHeader, close_at_end=True):
        self._opened = _Opened(target, "w")
        self.stream, self.name = self._opened.stream, self._opened.name
        self.header = header
        self.close_at_end = close_at_end
        self.events_written = 0
        self.ended = False
        self.closed = False
        self._hash = hashlib.sha256()
        self.stream.write(_dumps(header.to_json()) + "\n")

    def on_event(self, event) -> None:
        """Write the event's line (D-065)."""
        if self.closed:
            raise SessionError(f"the session recorder is closed; a {event.name} event "
                               f"came after its end", self.name)
        if self.ended:
            raise SessionError(f"a {event.name} event after the RunEnded event", self.name)
        line = json_line(event) + "\n"
        self.stream.write(line)
        self._hash.update(line.encode("utf-8"))
        self.events_written += 1
        if event.name == "RunEnded":
            self.ended = True
            if self.close_at_end:
                self.close()

    def close(self) -> None:
        """Write the end line if the run had ended, then flush (a path's file is
        closed). Once (Numbo GUI, `session.py`)."""
        if self.closed:
            return
        self.closed = True
        if self.ended:
            self.stream.write(_dumps({"end": self.events_written,
                                      "digest": self._hash.hexdigest()}) + "\n")
        self.stream.flush()
        self._opened.__exit__()


def write_session(target, header: SessionHeader, events) -> int:
    """Write the header and the events as the session file `target` (a path or a text
    stream); the end line is written when the events end with `RunEnded`. Returns how
    many events were written (Numbo GUI, `session.py`)."""
    recorder = SessionRecorder(target, header, close_at_end=False)
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
    """A session file, read and checked: its `version`, `header`, the typed `events`,
    their stored `lines` (the body's text, what a replay is compared with) and `records`
    (`events.record_of` of each, the history's form, for the differences), whether the file `ended` (the end line was
    there; otherwise the run was saved part way and a re-run goes on past the stored
    events), and the `name` of the file (G-048)."""
    version: int
    header: SessionHeader
    events: tuple
    lines: tuple
    records: tuple
    ended: bool
    name: str

    @property
    def problem_name(self) -> str:
        """The problem's name, from the header's name or its dict (G-048)."""
        p = self.header.problem
        return p if isinstance(p, str) else p["name"]


def _check_end(obj, name, lineno, lines):
    if set(obj) != {"end", "digest"}:
        raise SessionError(f"a bad end line {obj!r:.80}", name, lineno)
    if obj["end"] != len(lines):
        raise SessionError(f"the end line says {obj['end']} events, the file has "
                           f"{len(lines)}", name, lineno)
    if obj["digest"] != digest_of(lines):
        raise SessionError("the digest is not the body's: the events were edited, or the "
                           "file was damaged", name, lineno)


def read_session(source) -> SessionFile:
    """The session file `source` (a path or a text stream), whole and checked, as a
    `SessionFile`; a `SessionError` naming the file and the line for anything wrong
    (Numbo GUI, `session.py`'s `read_session` and `load_session`; G-048)."""
    with _Opened(source) as (f, name):
        lineno = 0
        header = None
        events, lines, records = [], [], []
        ended = False
        for lineno, line in enumerate(f, 1):
            if ended:
                raise SessionError("a line after the end line", name, lineno)
            obj = _parse_line(line, name, lineno)
            if lineno == 1:
                try:
                    header = SessionHeader.from_json(obj)
                except SessionError as e:
                    raise SessionError(e.reason, name, 1) from None
            elif type(obj) is dict and "end" in obj and "event" not in obj:
                _check_end(obj, name, lineno, lines)
                ended = True
            else:
                try:
                    event = decode_event(obj)
                except SessionError as e:
                    raise SessionError(e.reason, name, lineno) from None
                if events and events[-1].name == "RunEnded":
                    raise SessionError("an event after the RunEnded event", name, lineno)
                events.append(event)
                lines.append(line.rstrip("\n"))
                records.append(ev.record_of(event))
        if lineno == 0:
            raise SessionError("an empty file, not a tabletop session", name)
        if not ended and events and events[-1].name == "RunEnded":
            raise SessionError(f"truncated after line {lineno}: the run ended but the end "
                               f"line is missing", name)
    return SessionFile(VERSION, header, tuple(events), tuple(lines), tuple(records), ended,
                       name)


# ---------------------------------------------------------------------------------------
# Replay (Numbo GUI, `session.py`'s `replay_oracle_trace`)

@dataclasses.dataclass(frozen=True)
class Divergence:
    """The first difference between a session file and its re-run: at event `index`
    (from 0; its `line` in the file), `expected` (the stored record, or None past the
    file's end) and `got` (the re-run's record, or None after it ended); `error` is the
    `TraceMismatch` for it (Numbo GUI, `session.py`)."""
    index: int
    expected: object
    got: object
    error: TraceMismatch

    @property
    def line(self) -> int:
        """The file line of the stored event (the header comes first) (G-048)."""
        return self.index + 2

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
    """The observer that checks a re-run against the file's stored lines as the events
    come (Numbo GUI, `session.py`'s `_TraceChecker`; G-049). Each event's JSON line is
    compared with the stored one at the same index; the first difference is kept as
    `divergence` and given to `on_divergence`, and the events after it are not checked.
    A re-run that goes on past the stored events is a difference when the file ended,
    and is let go on, counted in `beyond`, when it did not (the run was saved part way).
    `finish` adds the last check: a re-run that ended before the file's events did."""

    def __init__(self, file: SessionFile, on_divergence=None):
        self.file = file
        self.name = file.name
        self.on_divergence = on_divergence
        self.seen = 0            # stored events matched so far
        self.beyond = 0          # re-run events past a part-way file's end
        self.divergence = None

    def _diverge(self, index, expected, got, message):
        self.divergence = Divergence(index, expected, got,
                                     TraceMismatch(message, self.name, index + 2))
        if self.on_divergence is not None:
            self.on_divergence(self.divergence)
        return self.divergence

    def on_event(self, event) -> None:
        """Check one event (D-065)."""
        self.check(event)

    def check(self, event):
        """Compare the re-run's next event with the stored one; returns the `Divergence`
        it makes, or None (G-049)."""
        if self.divergence is not None:
            return None
        index = self.seen + self.beyond
        line = json_line(event)
        if index >= len(self.file.lines):
            if self.file.ended:
                return self._diverge(index, None, ev.record_of(event), (
                    f"the file ends after {len(self.file.lines)} events, but the re-run "
                    f"goes on: {line[:120]}"))
            self.beyond += 1
            return None
        if line != self.file.lines[index]:
            expected, got = self.file.records[index], ev.record_of(event)
            return self._diverge(index, expected, got, (
                f"the re-run differs from the file at event {index + 1}: "
                f"{differences(expected, got)}"))
        self.seen += 1
        return None

    def finish(self, stopped=False):
        """After the re-run: unless it was `stopped`, the file must have no event left;
        returns the first `Divergence`, found now or earlier, or None (G-049)."""
        if self.divergence is None and not stopped and self.seen < len(self.file.lines):
            self._diverge(self.seen, self.file.records[self.seen], None, (
                f"the file goes on after the re-run ended: {len(self.file.lines)} events "
                f"stored, {self.seen} re-run"))
        return self.divergence

    @property
    def verified(self) -> bool:
        """Whether every stored event was matched and nothing differed (G-049)."""
        return self.divergence is None and self.seen == len(self.file.lines)


@dataclasses.dataclass(frozen=True)
class ReplayResult:
    """A replayed session file: the controller's `outcome` (`RunOutcome`), the re-run's
    `records`, the `divergence` (None: the re-run is the file) and whether the file was
    `verified` whole (G-049)."""
    outcome: object
    records: list
    divergence: object
    verified: bool


def replay(file: SessionFile, observers=(), check=True) -> ReplayResult:
    """Re-execute the file's header through a Qt-free `RunController` with the
    `ReplayChecker` and `observers` attached, stopping at the first difference; raises
    its `TraceMismatch` at the end unless `check` is false (Numbo GUI, `session.py`'s
    `replay_oracle_trace`; G-049)."""
    from tabletop.gui.models.run_controller import RunController

    collector = ev.Collector()
    controller = RunController(observers=[collector, *observers])
    checker = ReplayChecker(file, on_divergence=lambda d: controller.stop(wait=False))
    controller.subscribe(checker)
    controller.start_run(problem_of(file.header), config_of(file.header), file.header.seed)
    controller.run_to_end()
    controller.wait()
    outcome = controller.outcome
    if outcome.error is not None:
        raise outcome.error
    divergence = checker.finish(stopped=outcome.stopped)
    if check and divergence is not None:
        raise divergence.error
    return ReplayResult(outcome, [ev.record_of(e) for e in collector.events], divergence,
                        checker.verified)
