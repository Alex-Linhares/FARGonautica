"""Loop0006 item 14: session files (`tabletop/gui/models/session_file.py`; G-048, G-049),
Qt-free, after the Seek-Whence GUI's `test_session_file.py` and the Numbo GUI's session
tests (PANES.md section 5).

- A recorded run round-trips: the header, every event's record and line, `ended`.
- The body lines of a named problem equal `python3 -m tabletop P --seed N --json -`'s
  output (a subprocess), and the end line's digest equals `run.fingerprint`'s and the
  golden fingerprints.
- The header: a run's problem by name or, for a table that is not the library's, as its
  dict, which round-trips through `problems.problem_from_dict` for all 53 problems; the
  edits as the fields that differ from the profile; `config_of` is
  `dataclasses.replace(PROFILES[profile], **edits)`; bad headers are refused naming
  line 1.
- `decode_event` gives back the typed event whose line is the stored one; bad objects
  are refused.
- The recorder writes the end line at `RunEnded` or on close; a run saved part way has
  no end line and reads back with `ended` False.
- A truncated file (a cut line; a `RunEnded` with no end line), a wrong count, a digest
  that is not the body's, an event after the end and a line after the end line give a
  `SessionError` naming the file and the line.
- Replay equals live on two problems and an edited configuration, verified; a tampered
  event (re-digested) is detected at its index and line and stops the re-run; a file
  that goes on after the run, and a run that goes on after an ended file, are
  differences; a part-way file's run goes on; a stopped re-run is no difference.
"""

import dataclasses
import io
import json
import subprocess
import sys

import pytest

from tabletop import __version__
from tabletop import events as ev
from tabletop.config import PROFILES, Config
from tabletop.events import Collector, record_of
from tabletop.gui.models import session_file as sf
from tabletop.gui.models.event_log import EventLog, json_lines
from tabletop.observe import json_line
from tabletop.problems import load_all, load_problem, problem_from_dict, validate
from tabletop.run import fingerprint, run_problem

from golden import GOLDEN, golden_cases

FIRST = ("surround-05", 1)
SECOND = ("blockage-06", 2)


def save(golden_runs, case, path, events=None, profile="standard"):
    """The plain run's events (or `events`) written as a session file at `path`."""
    ref = golden_runs[case]
    header = sf.header_of(case[0], PROFILES[profile], case[1])
    sf.write_session(str(path), header, ref.events if events is None else events)
    return header


def lines_of(path):
    return path.read_text(encoding="utf-8").splitlines()


def write_lines(path, lines):
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def end_line(lines):
    """The end line for these body lines (the header left out)."""
    return json.dumps({"end": len(lines), "digest": sf.digest_of(lines)},
                      separators=(",", ":"))


def redigested(lines):
    """The file's lines with a fresh end line, as an editor who knows the format would
    leave them (the engine, not the file, is then what differs)."""
    return lines[:-1] + [end_line(lines[1:-1])]


# -- the round trip and the CLI's lines -----------------------------------------------------------

@pytest.mark.parametrize("case", [FIRST, SECOND])
def test_a_recorded_run_round_trips(golden_runs, tmp_path, case):
    ref = golden_runs[case]
    path = tmp_path / "session.jsonl"
    header = save(golden_runs, case, path)
    lines = lines_of(path)
    assert json.loads(lines[0]) == {"tabletop-gui-session": 1, "problem": case[0],
                                    "profile": "standard", "edits": {}, "seed": case[1],
                                    "version": __version__}
    end = json.loads(lines[-1])
    assert set(end) == {"end", "digest"} and end["end"] == len(ref.events)
    assert len(lines) == len(ref.events) + 2
    file = sf.read_session(str(path))
    assert file.version == 1 and file.name == str(path) and file.ended
    assert file.header == header and file.problem_name == case[0]
    assert list(file.records) == list(ref.records)
    assert [record_of(e) for e in file.events] == list(ref.records)
    assert list(file.lines) == [json_line(e) for e in ref.events]
    assert file.events[-1].name == "RunEnded"


def test_the_body_equals_the_clis_json_lines_and_the_digest_the_fingerprint(golden_runs, tmp_path):
    problem, seed = FIRST
    path = tmp_path / "s.jsonl"
    save(golden_runs, FIRST, path)
    lines = lines_of(path)
    body = lines[1:-1]
    # The CLI's `--json -` output, byte for byte (PANES.md section 5).
    out = subprocess.run([sys.executable, "-m", "tabletop", problem, "--seed", str(seed),
                          "--json", "-"], capture_output=True, text=True, check=True)
    assert out.stdout == "".join(line + "\n" for line in body)
    log = EventLog()
    log.load(golden_runs[FIRST].events)
    assert json_lines(log) == out.stdout
    # The digest is the fingerprint's.
    digest, draws = fingerprint(problem, seed)
    assert json.loads(lines[-1])["digest"] == digest == sf.digest_of(body)
    golden = json.loads(GOLDEN.read_text())
    assert golden[f"{problem} {seed}"] == [digest, draws]
    assert draws == golden_runs[FIRST].draws


def test_every_golden_digest_is_the_fingerprints(golden_runs):
    golden = json.loads(GOLDEN.read_text())
    for problem, seed in golden_cases():
        ref = golden_runs[(problem, seed)]
        stream = io.StringIO()
        sf.write_session(stream, sf.header_of(problem, PROFILES["standard"], seed), ref.events)
        lines = stream.getvalue().splitlines()
        assert json.loads(lines[-1]) == {"end": len(ref.events),
                                         "digest": golden[f"{problem} {seed}"][0]}


# -- the header (G-048) ----------------------------------------------------------------------------

def test_the_header_of_a_library_problem_is_its_name_and_of_another_its_dict():
    problem = load_problem("surround-05")
    config = PROFILES["standard"]
    assert sf.header_of("surround-05", config, 3).problem == "surround-05"
    assert sf.header_of(problem, config, 3).problem == "surround-05"
    moved = dataclasses.replace(problem, objects=problem.objects[::-1])
    header = sf.header_of(moved, config, 3)
    assert header.problem == sf.problem_dict(moved) and header.problem["name"] == "surround-05"
    assert sf.problem_of(header) == moved and sf.problem_of(sf.header_of(problem, config, 3)) == problem
    assert sf.header_of("surround-05", config, 3).version == __version__


def test_problem_dict_round_trips_every_problem_of_the_library():
    for problem in load_all():
        data = sf.problem_dict(problem)
        assert validate(data) == []
        again = json.loads(json.dumps(data))             # through JSON, as the file holds it
        assert problem_from_dict(again) == problem


def test_the_edits_are_the_fields_that_differ_from_the_profile():
    config = dataclasses.replace(PROFILES["human"], max_codelets=20)
    header = sf.header_of("surround-05", config, 1)
    assert header.profile == "human" and header.edits == {"max_codelets": 20}
    assert sf.config_of(header) == config
    assert sf.config_of(sf.header_of("surround-05", PROFILES["french-faults"], 1)) == \
        PROFILES["french-faults"]
    # A `type_weight` of 50 is `human`'s own value: no edit under `human`.
    assert sf.header_of("surround-05", dataclasses.replace(PROFILES["human"], type_weight=50),
                        1).edits == {}
    # A configuration built from no profile cannot be stored.
    with pytest.raises(ValueError, match="not one of"):
        sf.header_of("surround-05", dataclasses.replace(Config(), profile="custom"), 1)
    # An edit comes back in the engine's types through the file.
    edited = dataclasses.replace(Config(), max_codelets=7)
    stream = io.StringIO()
    sf.write_session(stream, sf.header_of("surround-05", edited, 2), [])
    stream.seek(0)
    file = sf.read_session(stream)
    assert file.header.edits == {"max_codelets": 7} and not file.ended and file.events == ()
    assert sf.config_of(file.header) == edited


@pytest.mark.parametrize("obj, words", [
    ({"problem": "surround-05"}, "not a tabletop session file"),
    ({"tabletop-gui-session": 2, "problem": "surround-05", "profile": "standard", "edits": {},
      "seed": 1, "version": "0"}, "newer tabletop GUI"),
    ({"tabletop-gui-session": "1"}, "bad session version"),
    ({"tabletop-gui-session": 1, "problem": "surround-05"}, "missing profile, edits, seed"),
    ({"tabletop-gui-session": 1, "problem": "surround-05", "profile": "standard", "edits": {},
      "seed": 1, "version": "0", "extra": 1}, "unexpected extra"),
    ({"tabletop-gui-session": 1, "problem": "no-such-99", "profile": "standard", "edits": {},
      "seed": 1, "version": "0"}, "no problem named"),
    ({"tabletop-gui-session": 1, "problem": {"name": "x"}, "profile": "standard", "edits": {},
      "seed": 1, "version": "0"}, "the problem dict: missing key"),
    ({"tabletop-gui-session": 1, "problem": 5, "profile": "standard", "edits": {},
      "seed": 1, "version": "0"}, "a name or a problem dict"),
    ({"tabletop-gui-session": 1, "problem": "surround-05", "profile": "fast", "edits": {},
      "seed": 1, "version": "0"}, "no profile named"),
    ({"tabletop-gui-session": 1, "problem": "surround-05", "profile": "standard", "edits": {},
      "seed": "1", "version": "0"}, "a bad seed"),
    ({"tabletop-gui-session": 1, "problem": "surround-05", "profile": "standard", "edits": {},
      "seed": 1, "version": 1}, "version must be a string"),
    ({"tabletop-gui-session": 1, "problem": "surround-05", "profile": "standard", "edits": [],
      "seed": 1, "version": "0"}, "edits must be an object"),
    ({"tabletop-gui-session": 1, "problem": "surround-05", "profile": "standard",
      "edits": {"profile": "human"}, "seed": 1, "version": "0"}, "no editable field"),
    ({"tabletop-gui-session": 1, "problem": "surround-05", "profile": "standard",
      "edits": {"max_codelets": "many"}, "seed": 1, "version": "0"}, "expected an integer"),
])
def test_bad_headers_are_refused_naming_line_1(tmp_path, obj, words):
    path = tmp_path / "bad.jsonl"
    write_lines(path, [json.dumps(obj)])
    with pytest.raises(sf.SessionError, match=words) as info:
        sf.read_session(str(path))
    assert info.value.path == str(path) and info.value.line == 1
    assert str(info.value).startswith(f"{path}, line 1: ")


# -- events (G-048) ----------------------------------------------------------------------------------

def test_decode_event_gives_the_typed_event(golden_runs):
    for event in golden_runs[FIRST].events:
        line = json_line(event)
        again = sf.decode_event(json.loads(line))
        assert again == event and json_line(again) == line


@pytest.mark.parametrize("obj, words", [
    ([1, 2], "not a JSON object"),
    ({"clock": 0}, "no name"),
    ({"event": "Nothing", "clock": 0, "draws": 0}, "unknown event"),
    ({"event": "AnswerGiven", "clock": 0}, "missing field"),
    ({"event": "RunEnded", "clock": 0, "draws": 0, "answer": "cup1", "label": "E1",
      "literal": False, "capped": False, "structure_value": 1.0, "codelets": 1,
      "temperature": 1.0, "worldview": [], "more": 1}, "unexpected field"),
])
def test_bad_event_objects_are_refused(obj, words):
    with pytest.raises(sf.SessionError, match=words):
        sf.decode_event(obj)


# -- the recorder (G-048) ----------------------------------------------------------------------------

def test_the_recorder_writes_the_end_line_at_the_run_end(golden_runs):
    ref = golden_runs[FIRST]
    header = sf.header_of(FIRST[0], PROFILES["standard"], FIRST[1])
    stream = io.StringIO()
    recorder = sf.SessionRecorder(stream, header)
    for event in ref.events:
        recorder.on_event(event)
    assert recorder.closed and recorder.ended and recorder.events_written == len(ref.events)
    whole = io.StringIO()
    sf.write_session(whole, header, ref.events)
    assert stream.getvalue() == whole.getvalue()
    with pytest.raises(sf.SessionError, match="closed"):
        recorder.on_event(ref.events[0])
    # As the run's own observer: the same file, and the run is unchanged.
    live = io.StringIO()
    result = run_problem(load_problem(FIRST[0]), PROFILES["standard"], FIRST[1],
                         [sf.SessionRecorder(live, header)])
    assert live.getvalue() == whole.getvalue() and result == ref.result


def test_a_run_saved_part_way_has_no_end_line(golden_runs, tmp_path):
    ref = golden_runs[FIRST]
    path = tmp_path / "part.jsonl"
    header = save(golden_runs, FIRST, path, events=ref.events[:100])
    lines = lines_of(path)
    assert len(lines) == 101 and "end" not in json.loads(lines[-1])
    file = sf.read_session(str(path))
    assert not file.ended and file.header == header
    assert list(file.records) == list(ref.records[:100])
    recorder = sf.SessionRecorder(io.StringIO(), header, close_at_end=False)
    for event in ref.events[:5]:
        recorder.on_event(event)
    recorder.close()
    assert recorder.closed and not recorder.ended
    assert recorder.stream.getvalue().count("\n") == 6


def test_a_truncated_file_gives_a_clear_error(golden_runs, tmp_path):
    whole = tmp_path / "whole.jsonl"
    save(golden_runs, FIRST, whole)
    lines = lines_of(whole)
    n = len(lines) - 2
    # A line cut in the middle.
    torn = tmp_path / "torn.jsonl"
    torn.write_text("\n".join(lines[:100]) + "\n" + lines[100][:25], encoding="utf-8")
    with pytest.raises(sf.SessionError, match=r"not valid JSON \(truncated\?\)") as info:
        sf.read_session(str(torn))
    assert info.value.path == str(torn) and info.value.line == 101
    # The run ended but the end line is missing: cut at a line boundary.
    cut = tmp_path / "cut.jsonl"
    write_lines(cut, lines[:-1])
    with pytest.raises(sf.SessionError, match=f"truncated after line {n + 1}: the run ended") as info:
        sf.read_session(str(cut))
    assert info.value.line is None
    # The wrong count.
    counted = tmp_path / "counted.jsonl"
    write_lines(counted, lines[:-1] + [json.dumps({"end": 3, "digest": sf.digest_of(lines[1:-1])})])
    with pytest.raises(sf.SessionError, match=f"says 3 events, the file has {n}") as info:
        sf.read_session(str(counted))
    assert info.value.line == len(lines)
    # A digest that is not the body's: an event edited in place.
    edited = tmp_path / "edited.jsonl"
    obj = json.loads(lines[50])
    obj["clock"] += 1
    write_lines(edited, lines[:50] + [json.dumps(obj, separators=(",", ":"))] + lines[51:])
    with pytest.raises(sf.SessionError, match="the digest is not the body's") as info:
        sf.read_session(str(edited))
    assert info.value.line == len(lines)
    # A bad end line.
    bad = tmp_path / "bad-end.jsonl"
    write_lines(bad, lines[:-1] + ['{"end": 1}'])
    with pytest.raises(sf.SessionError, match="a bad end line"):
        sf.read_session(str(bad))
    # An event after RunEnded, and a line after the end line.
    after = tmp_path / "after.jsonl"
    write_lines(after, lines[:-1] + [lines[5], lines[-1]])
    with pytest.raises(sf.SessionError, match="an event after the RunEnded event") as info:
        sf.read_session(str(after))
    assert info.value.line == len(lines)
    beyond = tmp_path / "beyond.jsonl"
    write_lines(beyond, lines + [lines[5]])
    with pytest.raises(sf.SessionError, match="a line after the end line") as info:
        sf.read_session(str(beyond))
    assert info.value.line == len(lines) + 1
    # An empty file.
    empty = tmp_path / "empty.jsonl"
    empty.write_text("", encoding="utf-8")
    with pytest.raises(sf.SessionError, match="an empty file"):
        sf.read_session(str(empty))


# -- replay (G-049) ----------------------------------------------------------------------------------

@pytest.mark.parametrize("case", [FIRST, SECOND])
def test_replay_equals_live(golden_runs, tmp_path, case):
    ref = golden_runs[case]
    path = tmp_path / "session.jsonl"
    save(golden_runs, case, path)
    result = sf.replay(sf.read_session(str(path)))
    assert result.verified and result.divergence is None
    assert result.records == list(ref.records)
    assert not result.outcome.stopped
    assert result.outcome.result == ref.result and result.outcome.result.draws == ref.draws


def test_replay_equals_live_under_an_edited_configuration(tmp_path):
    config = dataclasses.replace(PROFILES["human"], max_codelets=20)
    log = Collector()
    result = run_problem(load_problem("surround-05"), config, 1, [log])
    assert result.capped and result.codelets == 20
    assert log.events[0].changed == (("max_codelets", "20"), ("profile", "'human'"),
                                     ("type_weight", "50"))
    path = tmp_path / "edited.jsonl"
    sf.write_session(str(path), sf.header_of("surround-05", config, 1), log.events)
    file = sf.read_session(str(path))
    assert file.header.profile == "human" and file.header.edits == {"max_codelets": 20}
    assert sf.config_of(file.header) == config
    replayed = sf.replay(file)
    assert replayed.verified and replayed.outcome.result == result
    assert replayed.records == [record_of(e) for e in log.events]
    # As a problem dict instead of a name: the same run.
    header = sf.SessionHeader(sf.problem_dict(load_problem("surround-05")), "human",
                              {"max_codelets": 20}, 1)
    sf.write_session(str(path), header, log.events)
    file = sf.read_session(str(path))
    assert isinstance(file.header.problem, dict) and file.problem_name == "surround-05"
    assert sf.replay(file).verified


def test_a_tampered_event_is_detected(golden_runs, tmp_path):
    path = tmp_path / "tampered.jsonl"
    save(golden_runs, FIRST, path)
    lines = lines_of(path)
    k = 47                                   # the event tampered with, from 0: a CodeletPosted (43 at item 05; 44 at item 07; since item 08 index 44 is a NodeJolted)
    obj = json.loads(lines[k + 1])
    assert obj["event"] == "CodeletPosted"
    assert obj["urgency"] != 99
    was = obj["urgency"]
    obj["urgency"] = 99
    lines[k + 1] = json.dumps(obj, separators=(",", ":"))
    write_lines(path, redigested(lines))
    file = sf.read_session(str(path))
    with pytest.raises(sf.TraceMismatch, match="differs from the file at event 48") as info:
        sf.replay(file)
    assert info.value.line == k + 2 and info.value.path == str(path)
    assert f"urgency: file 99, re-run {was}" in str(info.value)
    result = sf.replay(file, check=False)
    assert not result.verified and result.outcome.stopped
    d = result.divergence
    assert d.index == k and d.line == k + 2
    assert d.expected == file.records[k] and d.got == golden_runs[FIRST].records[k]
    assert d.error is not None and d.message.startswith("the re-run differs")
    # The run stopped at the differing event: nothing after it was run.
    assert result.records == list(golden_runs[FIRST].records[:k + 1])


def test_a_file_that_goes_on_after_the_run_is_a_difference(golden_runs, tmp_path):
    ref = golden_runs[FIRST]
    path = tmp_path / "long.jsonl"
    more = ev.NodeJolted(clock=ref.events[-1].clock, draws=ref.draws, node="cup",
                         amount=1.0, activation=2.0, by="test")
    save(golden_runs, FIRST, path, events=list(ref.events[:-1]) + [more])
    file = sf.read_session(str(path))
    assert not file.ended                       # its last event is not RunEnded
    result = sf.replay(file, check=False)
    d = result.divergence
    assert d is not None and d.index == len(ref.events) - 1
    assert "the re-run differs from the file at event" in d.message
    assert result.records == list(ref.records)


def test_a_run_that_goes_on_after_an_ended_file_is_a_difference(golden_runs, tmp_path):
    ref = golden_runs[FIRST]
    path = tmp_path / "ended.jsonl"
    save(golden_runs, FIRST, path)
    file = sf.read_session(str(path))
    assert file.ended
    checker = sf.ReplayChecker(file)
    for event in ref.events:
        assert checker.check(event) is None
    assert checker.verified
    n = len(ref.events)
    d = checker.check(ref.events[5])
    assert d is not None and d.index == n and d.expected is None
    assert f"the file ends after {n} events, but the re-run goes on" in d.message
    assert checker.check(ref.events[6]) is None        # after the first, no more
    assert checker.finish() is d and not checker.verified
    # A file that goes on after the re-run ended.
    checker = sf.ReplayChecker(file)
    for event in ref.events[:10]:
        checker.check(event)
    d = checker.finish()
    assert d.index == 10 and d.got is None and "the file goes on after the re-run ended" in d.message


def test_a_part_way_file_replays_on_to_the_end(golden_runs, tmp_path):
    ref = golden_runs[SECOND]
    path = tmp_path / "part.jsonl"
    save(golden_runs, SECOND, path, events=ref.events[:200])
    file = sf.read_session(str(path))
    assert not file.ended
    seen = []
    checker = sf.ReplayChecker(file, on_divergence=seen.append)
    result = sf.replay(file, observers=[checker])
    assert result.verified and result.divergence is None and seen == []
    assert checker.verified and checker.beyond == len(ref.events) - 200
    assert result.records == list(ref.records)
    assert result.outcome.result.draws == ref.draws


def test_a_stopped_rerun_is_not_a_difference(golden_runs, tmp_path):
    path = tmp_path / "s.jsonl"
    save(golden_runs, FIRST, path)
    checker = sf.ReplayChecker(sf.read_session(str(path)))
    for event in golden_runs[FIRST].events[:10]:
        checker.check(event)
    assert checker.finish(stopped=True) is None
    assert checker.finish(stopped=False) is not None and not checker.verified


def test_the_differences_text():
    assert sf.differences({"event": "X", "n": 1}, {"event": "X", "n": 2}) == "n: file 1, re-run 2"
    assert sf.differences({"n": (1, 2)}, {"n": [1, 2]}) == ""
    assert sf.differences(1, 2) == "file 1, re-run 2"
