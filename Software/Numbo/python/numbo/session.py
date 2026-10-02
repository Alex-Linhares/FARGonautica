"""Session recording and replay (not 1987 source; loop0003).

A session file is a run's typed events (observe.py, every kind) as JSON
lines:

    {"format":"numbo-session","version":1}
    {"kind":"start","problem":[114,11,20,7,1,6],"seed":1,...}
    ... one line per event: its kind, then its fields in order ...
    {"end":"numbo-session","events":286}

SessionRecorder is an observer that writes one; it writes the end line when
the run ends or when it is closed (a run stopped part way is still a
complete file).  read_session yields the same typed events back: JSON
arrays become tuples again, the {"obj": ...} and {"str": ...} objects of a
codelet's arguments stay dicts, a decomposition's steps become
DecompositionSteps, and 0 stays 0 while 0.0 stays 0.0.  A file that is
corrupted, truncated (no end line, or a cut line), of a newer version, or
whose end line has the wrong count raises SessionError, naming the file
and line.

replay publishes events to observers through a Subject, as a live run does,
so models and views can't tell a replay from a run.

An oracle trace (trace.py's JSON lines, or lisp/tests/oracle/lib/full-run.lisp's)
has only a subset of the events, so replay_oracle_trace re-runs its start
event's problem and seed (runs are deterministic) with the observers
attached, and checks the re-run's own oracle trace against the file line by
line as it goes: on_divergence hears of the first difference at the event
that wrote the line, and the replay then raises TraceMismatch (or, with
check=False, returns the divergence).  Lines are compared as parsed JSON,
as test_full_runs.py compares SBCL's traces with Python's.

replay_file opens either kind (detect_format).
"""

import dataclasses
import io
import json
import os

from numbo import observe

__all__ = ["FORMAT", "VERSION", "SessionError", "TraceMismatch", "SessionRecorder",
           "Session", "Divergence", "OracleReplay", "FileReplay", "encode_event",
           "decode_event", "write_session", "read_session", "load_session", "replay",
           "replay_oracle_trace", "detect_format", "replay_file"]

FORMAT = "numbo-session"
VERSION = 1

# The event fields that hold DecompositionSteps.
_STEP_FIELDS = {(observe.Decomposition, "steps"), (observe.Solved, "decomposition")}
_STEP_KEYS = [f.name for f in dataclasses.fields(observe.DecompositionStep)]


class SessionError(ValueError):
    """A session file or an oracle trace that can't be read or replayed.
    PATH and LINE (1-based, or None) say where."""

    def __init__(self, message, path=None, line=None):
        self.path, self.line, self.reason = path, line, message
        where = ", ".join(x for x in (path and str(path),
                                      line is not None and f"line {line}") if x)
        super().__init__(f"{where}: {message}" if where else message)


class TraceMismatch(SessionError):
    """The re-run of an oracle trace's problem and seed differs from the
    trace."""


# ---------------------------------------------------------------------------
# Files and streams

class _Opened:
    """SOURCE (a path or a text stream) as (stream, name); a path is opened
    in MODE and closed afterwards, a stream left open."""

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


def _dumps(obj):
    return json.dumps(obj, allow_nan=False, separators=(",", ":"))


def _parse_line(line, name, lineno):
    try:
        return json.loads(line)
    except ValueError as e:
        cut = "" if line.endswith("\n") else " (truncated?)"
        raise SessionError(f"not valid JSON{cut}: {e}", name, lineno) from None


# ---------------------------------------------------------------------------
# Events as JSON

def _encode(x):
    if type(x) is tuple:
        return [_encode(v) for v in x]
    if type(x) is dict:
        return {k: _encode(v) for k, v in x.items()}
    if type(x) is observe.DecompositionStep:
        return {k: getattr(x, k) for k in _STEP_KEYS}
    return x


def _decode(x):
    if type(x) is list:
        return tuple(_decode(v) for v in x)
    if type(x) is dict:
        return {k: _decode(v) for k, v in x.items()}
    return x


def _decode_steps(x):
    if type(x) is not list:
        raise SessionError(f"bad decomposition steps {x!r}")
    steps = []
    for step in x:
        if type(step) is not dict or list(step) != _STEP_KEYS:
            raise SessionError(f"bad decomposition step {step!r} (keys {_STEP_KEYS})")
        steps.append(observe.DecompositionStep(**step))
    return tuple(steps)


def encode_event(event):
    """EVENT as a JSON object: its kind, then its fields in order."""
    obj = {"kind": event.kind}
    for f in dataclasses.fields(event):
        obj[f.name] = _encode(getattr(event, f.name))
    return obj


def decode_event(obj):
    """The typed event a JSON object (encode_event's) stands for; a
    SessionError if it stands for none."""
    if type(obj) is not dict:
        raise SessionError(f"not a JSON object: {obj!r:.80}")
    if "kind" not in obj:
        raise SessionError("an event with no kind")
    cls = observe.EVENT_TYPES.get(obj["kind"])
    if cls is None or "kind" not in cls.__dict__:
        raise SessionError(f"unknown event kind {obj['kind']!r}")
    names = [f.name for f in dataclasses.fields(cls)]
    missing = [n for n in names if n not in obj]
    extra = [k for k in obj if k != "kind" and k not in names]
    if missing or extra:
        raise SessionError(f"a {cls.kind} event: " + "; ".join(
            x for x in (missing and "missing field(s) " + ", ".join(missing),
                        extra and "unexpected field(s) " + ", ".join(extra)) if x))
    return cls(**{n: _decode_steps(obj[n]) if (cls, n) in _STEP_FIELDS else _decode(obj[n])
                  for n in names})


# ---------------------------------------------------------------------------
# Recording

class SessionRecorder:
    """An observer that writes the session file TARGET (a path, or a text
    stream it leaves open): the header now, then each event.  The end line
    is written by close(), which the run's end calls when CLOSE_AT_END."""

    def __init__(self, target, close_at_end=True):
        self._opened = _Opened(target, "w")
        self.stream, self.name = self._opened.stream, self._opened.name
        self.close_at_end = close_at_end
        self.events_written = 0
        self.closed = False
        self.stream.write(_dumps({"format": FORMAT, "version": VERSION}) + "\n")

    def on_event(self, event):
        if self.closed:
            raise SessionError(f"the session recorder is closed; a {event.kind} event "
                               f"came after its end", self.name)
        self.stream.write(_dumps(encode_event(event)) + "\n")
        self.events_written += 1
        if self.close_at_end and isinstance(event, observe.RunEnded):
            self.close()

    def close(self):
        """Write the end line and flush (a path's file is closed).  Once."""
        if self.closed:
            return
        self.closed = True
        self.stream.write(_dumps({"end": FORMAT, "events": self.events_written}) + "\n")
        self.stream.flush()
        self._opened.__exit__()


def write_session(target, events):
    """Write EVENTS as the session file TARGET (a path or a text stream)."""
    recorder = SessionRecorder(target, close_at_end=False)
    try:
        for event in events:
            recorder.on_event(event)
    finally:
        recorder.close()


# ---------------------------------------------------------------------------
# Reading

def read_session(source):
    """The events of the session file SOURCE (a path or a text stream), one
    by one.  SessionError on a bad header, line or end, raised when the
    reading gets there."""
    with _Opened(source) as (f, name):
        lineno = 0
        count = 0
        ended = False
        for lineno, line in enumerate(f, 1):
            if ended:
                raise SessionError("a line after the end line", name, lineno)
            obj = _parse_line(line, name, lineno)
            if lineno == 1:
                _check_header(obj, name)
            elif type(obj) is dict and "end" in obj and "kind" not in obj:
                if obj != {"end": FORMAT, "events": obj.get("events")}:
                    raise SessionError(f"a bad end line {obj!r:.80}", name, lineno)
                if obj["events"] != count:
                    raise SessionError(f"the end line says {obj['events']} events, the file "
                                       f"has {count}", name, lineno)
                ended = True
            else:
                try:
                    event = decode_event(obj)
                except SessionError as e:
                    raise SessionError(e.reason, name, lineno) from None
                count += 1
                yield event
        if lineno == 0:
            raise SessionError("an empty file, not a numbo session", name)
        if not ended:
            raise SessionError(f"truncated after line {lineno}: no end line (the recording "
                               f"was cut off)", name)


def _check_header(obj, name):
    if type(obj) is not dict or obj.get("format") != FORMAT:
        raise SessionError(f"not a numbo session file: the first line is {obj!r:.80}", name, 1)
    version = obj.get("version")
    if type(version) is not int or version < 1:
        raise SessionError(f"a bad session version {version!r}", name, 1)
    if version > VERSION:
        raise SessionError(f"session version {version}, from a newer numbo; this one reads "
                           f"version {VERSION}", name, 1)
    return version


@dataclasses.dataclass(frozen=True)
class Session:
    """A session file's VERSION and EVENTS."""
    version: int
    events: tuple

    @property
    def start(self):
        """The run's start event (or None)."""
        return next((e for e in self.events if isinstance(e, observe.RunStarted)), None)

    @property
    def ended(self):
        """Whether the run ended (else the recording stopped part way)."""
        return bool(self.events) and isinstance(self.events[-1], observe.RunEnded)


def load_session(source):
    """The whole session file SOURCE, checked, as a Session."""
    return Session(VERSION, tuple(read_session(source)))


# ---------------------------------------------------------------------------
# Replay

_DEFAULT = object()


def replay(events, observers=(), on_error=_DEFAULT):
    """Publish EVENTS (e.g. read_session(path)) to OBSERVERS, in order,
    through a Subject, as a live run does.  An observer's exception goes to
    ON_ERROR (by default a traceback on stderr) and the replay goes on.
    Returns the number of events published."""
    subject = observe.Subject() if on_error is _DEFAULT else observe.Subject(on_error)
    for observer in observers:
        subject.subscribe(observer)
    n = 0
    for event in events:
        subject.publish(event)
        n += 1
    return n


@dataclasses.dataclass(frozen=True)
class Divergence:
    """The first difference between an oracle trace and its re-run: at the
    trace's LINE, EXPECTED (the trace's line, or None past its end) and GOT
    (the re-run's, or None after it ended).  ERROR is the exception the
    replay raises for it."""
    line: int
    expected: object
    got: object
    error: SessionError

    @property
    def message(self):
        return str(self.error)


def _show(x, limit=120):
    s = _dumps(x)
    return s if len(s) <= limit else s[:limit] + "..."


def _differences(expected, got):
    if type(expected) is dict and type(got) is dict:
        keys = list(expected) + [k for k in got if k not in expected]
        diffs = [k for k in keys if _dumps(expected.get(k)) != _dumps(got.get(k))
                 or (k in expected) != (k in got)]
        return "; ".join(f"{k}: trace {_show(expected.get(k), 60)}, re-run "
                         f"{_show(got.get(k), 60)}" for k in diffs[:4])
    return f"trace {_show(expected)}, re-run {_show(got)}"


class _TraceChecker:
    """The text stream the re-run's OracleTraceWriter writes to: each line
    is compared with the trace's next one, from LINES (an iterator of
    (line number, text))."""

    def __init__(self, lines, name, on_divergence):
        self.source = lines
        self.name = name
        self.on_divergence = on_divergence
        self.buffer = ""
        self.lines = 0
        self.divergence = None

    def write(self, text):
        self.buffer += text
        while "\n" in self.buffer:
            line, _, self.buffer = self.buffer.partition("\n")
            self._check(line)

    def flush(self):
        pass

    def _diverge(self, line, expected, got, error):
        self.divergence = Divergence(line, expected, got, error)
        if self.on_divergence is not None:
            self.on_divergence(self.divergence)

    def _check(self, text):
        if self.divergence is not None:
            return
        got = json.loads(text)
        n = self.lines + 1
        nxt = next(self.source, None)
        if nxt is None:
            self._diverge(n, None, got, TraceMismatch(
                f"the trace ends after line {n - 1}, but the re-run goes on (truncated?): "
                f"{_show(got)}", self.name))
            return
        n, line = nxt
        try:
            expected = json.loads(line)
        except ValueError as e:
            cut = "" if line.endswith("\n") else " (truncated?)"
            self._diverge(n, line, got, SessionError(f"not valid JSON{cut}: {e}",
                                                     self.name, n))
            return
        if _dumps(expected) != _dumps(got):
            self._diverge(n, expected, got, TraceMismatch(
                f"the re-run differs from the trace: {_differences(expected, got)}",
                self.name, n))
            return
        self.lines = n

    def finish(self):
        """After the re-run: the trace must have no more lines."""
        if self.divergence is None:
            nxt = next(self.source, None)
            if nxt is not None:
                n, line = nxt
                self._diverge(n, line.rstrip("\n"), None, TraceMismatch(
                    "the trace goes on after the re-run ended", self.name, n))


@dataclasses.dataclass(frozen=True)
class OracleReplay:
    """A replayed oracle trace: the re-run's RESULT (harness.run_config's),
    its printed OUTPUT (when no `out` was given), the trace's LINES that
    were verified, whether it has RNG_EVENTS, its PROBLEM, SEED and
    MAX_ITERATIONS, and the DIVERGENCE (None: the re-run is the trace)."""
    result: dict
    output: object
    lines: int
    rng_events: bool
    problem: tuple
    seed: int
    max_iterations: object
    divergence: object


def _numbered(f):
    for n, line in enumerate(f, 1):
        yield n, line


def _rng_events(peeked, source, limit=1000):
    """Whether a trace has its RNG draws: an rng line, or a setup-choose
    line with an "rng" field.  Lines read go into PEEKED."""
    for _ in range(limit):
        nxt = next(source, None)
        if nxt is None:
            return False
        peeked.append(nxt)
        try:
            obj = json.loads(nxt[1])
        except ValueError:
            return False
        ev = obj.get("ev") if type(obj) is dict else None
        if ev == "rng":
            return True
        if ev == "setup-choose":
            return "rng" in obj
    return False


def _start(obj, name):
    def bad(why):
        return SessionError(f"not an oracle trace: {why}", name, 1)

    if type(obj) is not dict or obj.get("ev") != "start":
        raise bad("the first line is not its start event")
    if obj.get("rng") != "splitmix64":
        raise SessionError(f"the trace's RNG is {obj.get('rng')!r}; numbo re-runs "
                           f"splitmix64 traces only", name, 1)
    problem, seed, cap = obj.get("problem"), obj.get("seed"), obj.get("max_iterations")
    if (type(problem) is not list or len(problem) != 6
            or not all(type(x) is int for x in problem)):
        raise bad(f"its problem {problem!r} is not a target and 5 bricks")
    if type(seed) is not int or not (cap is None or (type(cap) is int and cap >= 1)):
        raise bad(f"a bad seed {seed!r} or max_iterations {cap!r}")
    return tuple(problem), seed, cap


def replay_oracle_trace(source, observers=(), out=None, on_divergence=None, check=True):
    """Re-run the oracle trace SOURCE's problem and seed (with its cap)
    with OBSERVERS attached, checking the re-run against the trace as it
    goes.  OUT gets the printed text (by default it is captured into the
    result's output).  ON_DIVERGENCE(Divergence) is called at the first
    difference.  Raises its error at the end (TraceMismatch, or SessionError
    for a line that isn't JSON) unless CHECK is false.  Returns an
    OracleReplay."""
    from numbo import harness

    with _Opened(source) as (f, name):
        lines = _numbered(f)
        first = next(lines, None)
        if first is None:
            raise SessionError("an empty file, not an oracle trace", name)
        problem, seed, cap = _start(_parse_line(first[1], name, 1), name)
        peeked = [first]
        rng_events = _rng_events(peeked, lines)

        def all_lines():
            yield from peeked
            yield from lines

        checker = _TraceChecker(all_lines(), name, on_divergence)
        captured = io.StringIO() if out is None else None
        result = harness.run_config(list(problem), seed=seed, max_iterations=cap,
                                    trace=checker, rng_events=rng_events,
                                    out=captured if out is None else out,
                                    observers=observers)
        checker.finish()
    if check and checker.divergence is not None:
        raise checker.divergence.error
    return OracleReplay(result=result, output=captured and captured.getvalue(),
                        lines=checker.lines, rng_events=rng_events, problem=problem,
                        seed=seed, max_iterations=cap, divergence=checker.divergence)


# ---------------------------------------------------------------------------
# Either kind

def detect_format(source):
    """"session" or "oracle", from SOURCE's first line (a path, or a
    seekable text stream, which is rewound)."""
    seekable = not isinstance(source, (str, os.PathLike))
    with _Opened(source) as (f, name):
        position = f.tell() if seekable else None
        line = f.readline()
        if seekable:
            f.seek(position)
    if not line:
        raise SessionError("an empty file", name)
    obj = _parse_line(line, name, 1)
    if type(obj) is dict and obj.get("format") == FORMAT:
        return "session"
    if type(obj) is dict and obj.get("ev") == "start":
        return "oracle"
    raise SessionError("neither a numbo session nor an oracle trace", name, 1)


@dataclasses.dataclass(frozen=True)
class FileReplay:
    """A replayed file: its FORMAT ("session" or "oracle"), the number of
    EVENTS published, and for an oracle trace the OracleReplay."""
    format: str
    events: int
    oracle: object = None


class _Counter:
    def __init__(self):
        self.n = 0

    def on_event(self, event):
        self.n += 1


def replay_file(path, observers=(), **oracle_options):
    """Replay the session file or oracle trace PATH to OBSERVERS
    (ORACLE_OPTIONS: replay_oracle_trace's out, on_divergence, check)."""
    if detect_format(path) == "session":
        return FileReplay("session", replay(read_session(path), observers))
    counter = _Counter()
    r = replay_oracle_trace(path, list(observers) + [counter], **oracle_options)
    return FileReplay("oracle", counter.n, r)
