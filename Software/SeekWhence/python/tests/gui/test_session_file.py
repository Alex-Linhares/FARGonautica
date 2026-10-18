"""Loop0004 item 13: session files (`seekwhence/gui/models/session_file.py`), Qt-free,
after the Numbo GUI's session tests (PANES.md section 5; G-046, G-047).

- A recorded run round-trips: the run line, every event's record, the count, `ended`.
- The configuration round-trips through its pairs, defaults and edits alike; bad pairs
  are refused (ANSWERS.md 5).
- `decode_event` gives back the typed event, or an event with the same record when a
  task's arguments held a `Form`; bad objects are refused.
- The recorder writes the end line at the run's end or on close; a run saved part way is
  a complete file with no `ended` event.
- Bad headers, bad run lines, a truncated file (no end line, a cut line), a wrong count
  and a line after the end give a `SessionError` naming the file and the line.
- Replay equals live for the three appendix runs [M86 pp. 164-179]: the same records,
  draws and outcome, verified; a tampered event is detected at its index and line; a
  file that goes on after the run, and a run that goes on after a file that ended, are
  differences; a part-way file's run goes on, verified.
- The run line of scripted, typed and interactive runs (G-046).
"""

import dataclasses
import io
import json

import pytest

from seekwhence import __version__
from seekwhence import events as ev
from seekwhence.config import Config
from seekwhence.forms import Form
from seekwhence.gui.models import presets
from seekwhence.gui.models import session_file as sf
from seekwhence.session import Session
from seekwhence.trace import event_record

from golden import APPENDIX

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def save(appendix_runs, run_id, path, events=None):
    """The plain run's events (or `events`) written as a session file at `path`."""
    ref = appendix_runs[run_id]
    run = sf.run_line_of(presets.appendix_inputs(run_id))
    sf.write_session(str(path), run, ref.events if events is None else events)
    return run


def lines_of(path):
    return path.read_text(encoding="utf-8").splitlines()


def write_lines(path, lines):
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def end_line(n):
    return json.dumps({"end": sf.FORMAT, "events": n}, separators=(",", ":"))


# -- the configuration (ANSWERS.md 5) --------------------------------------------------------------

def test_the_configuration_round_trips_through_its_pairs():
    for config in (Config(), Config(verbose=False, urgency_power=2, guess_validity=5,
                                    s_links=(("Countup", "S-group", 0.1),))):
        pairs = json.loads(json.dumps(sf.config_pairs(config)))
        assert sf.config_from_pairs(pairs) == config
    assert [name for name, _ in sf.config_pairs(Config())] == [
        f.name for f in dataclasses.fields(Config())]


@pytest.mark.parametrize("pairs, words", [
    ([["no_such_field", 1]], "no field"),
    ([["urgency_power", "2"]], "not a int"),
    ([["verbose", 1]], "not a bool"),
    ([["urgency_power"]], "not a [field, value] pair"),
    ({"urgency_power": 1}, "not a list of pairs"),
])
def test_bad_configuration_pairs_are_refused(pairs, words):
    with pytest.raises(sf.SessionError, match=words.replace("[", r"\[")):
        sf.config_from_pairs(pairs)


# -- the file (PANES.md section 5) -------------------------------------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_a_recorded_run_round_trips(appendix_runs, tmp_path, run_id):
    ref = appendix_runs[run_id]
    path = tmp_path / "session.jsonl"
    run = save(appendix_runs, run_id, path)
    lines = lines_of(path)
    assert json.loads(lines[0]) == {"format": "seekwhence-session", "version": 1}
    assert set(json.loads(lines[1])["run"]) == {"seed", "config", "inputs", "answers", "mode",
                                                "preset", "engine"}
    assert json.loads(lines[-1]) == {"end": "seekwhence-session", "events": len(ref.events)}
    assert len(lines) == len(ref.events) + 3
    file = sf.read_session(str(path))
    assert file.version == 1 and file.name == str(path)
    assert file.run == run
    assert file.run.seed == ref.seed and file.run.config == ref.config
    assert list(file.run.inputs) == [str(x) for x in ref.lines]
    assert list(file.run.answers) == [str(x) for x in ref.answers]
    assert file.run.mode == "scripted" and file.run.preset == run_id
    assert file.run.engine == __version__
    assert list(file.records) == ref.records
    assert [event_record(e) for e in file.events] == ref.records
    assert file.ended and file.events[-1].kind == "ended"


def test_decode_event_gives_the_typed_event_or_one_with_the_same_record(appendix_runs):
    forms = 0
    for ref in appendix_runs.values():
        for event in ref.events:
            record = json.loads(json.dumps(event_record(event)))
            decoded = sf.decode_event(record)
            assert event_record(decoded) == record
            held_form = any(isinstance(a, Form) for a in getattr(event, "args", ()))
            if held_form:
                forms += 1
                assert decoded != event
            else:
                assert decoded == event
    assert forms > 0      # a Template-applier's argument is a Form [M86 p. 167]


@pytest.mark.parametrize("obj, words", [
    ([1, 2], "not a JSON object"),
    ({"seed": 1}, "no kind"),
    ({"kind": "no-such-kind"}, "unknown event kind"),
    ({"kind": "term", "n": 1}, "missing field"),
    ({"kind": "term", "n": 1, "term": 3, "cleaned": 0, "extra": 1}, "unexpected field"),
])
def test_bad_event_objects_are_refused(obj, words):
    with pytest.raises(sf.SessionError, match=words):
        sf.decode_event(obj)


def test_the_recorder_writes_the_end_line_at_the_run_end(appendix_runs):
    inputs = presets.appendix_inputs(RUN1)
    out = io.StringIO()
    recorder = sf.SessionRecorder(out, sf.run_line_of(inputs))
    session = Session(inputs.config, inputs.seed, out=io.StringIO(), lines=list(inputs.lines),
                      answers=list(inputs.answers), observers=[recorder])
    session.run()
    assert recorder.closed and recorder.events_written == 174           # [M86 p. 166]
    file = sf.read_session(io.StringIO(out.getvalue()))
    assert list(file.records) == appendix_runs[RUN1].records and file.ended
    with pytest.raises(sf.SessionError, match="closed"):
        recorder.on_event(ev.Message("late"))


def test_a_run_saved_part_way_is_a_complete_file_with_no_end_event(appendix_runs, tmp_path):
    ref = appendix_runs[RUN2]
    path = tmp_path / "part.jsonl"
    out = io.StringIO()
    recorder = sf.SessionRecorder(out, sf.run_line_of(presets.appendix_inputs(RUN2)),
                                  close_at_end=False)
    for event in ref.events[:60]:
        recorder.on_event(event)
    assert not recorder.closed
    recorder.close()
    recorder.close()                                    # once
    path.write_text(out.getvalue(), encoding="utf-8")
    file = sf.read_session(str(path))
    assert len(file.events) == 60 and not file.ended
    assert list(file.records) == ref.records[:60]


@pytest.mark.parametrize("text, words", [
    ("", "an empty file"),
    ('{"kind":"start","seed":1,"config":[]}\n', "not a seekwhence session file"),
    ('{"format":"seekwhence-session","version":2}\n', "from a newer seekwhence"),
    ('{"format":"seekwhence-session","version":"1"}\n', "a bad session version"),
    ('{"format":"seekwhence-session","version":1}\n', "truncated after line 1: no run line"),
])
def test_bad_headers_are_refused(tmp_path, text, words):
    path = tmp_path / "bad.jsonl"
    path.write_text(text, encoding="utf-8")
    with pytest.raises(sf.SessionError, match=words) as info:
        sf.read_session(str(path))
    assert info.value.path == str(path)


@pytest.mark.parametrize("run, words", [
    ({"seed": 1}, "missing config, inputs"),
    ({"seed": 1, "config": [], "inputs": [], "answers": [], "mode": "typed", "preset": "x",
      "engine": "0"}, "a bad mode"),
    ({"seed": "1", "config": [], "inputs": [], "answers": [], "mode": "scripted", "preset": "x",
      "engine": "0"}, "a bad seed"),
    ({"seed": 1, "config": [["nope", 1]], "inputs": [], "answers": [], "mode": "scripted",
      "preset": "x", "engine": "0"}, "no field 'nope'"),
    ({"seed": 1, "config": [], "inputs": [[3]], "answers": [], "mode": "scripted",
      "preset": "x", "engine": "0"}, "inputs must be a list of strings"),
])
def test_bad_run_lines_are_refused_naming_line_2(tmp_path, run, words):
    path = tmp_path / "bad.jsonl"
    write_lines(path, [json.dumps({"format": sf.FORMAT, "version": 1}),
                       json.dumps({"run": run}), end_line(0)])
    with pytest.raises(sf.SessionError, match=words) as info:
        sf.read_session(str(path))
    assert info.value.line == 2


def test_a_truncated_file_gives_a_clear_error(appendix_runs, tmp_path):
    whole = tmp_path / "whole.jsonl"
    save(appendix_runs, RUN1, whole)
    lines = lines_of(whole)
    # No end line: the recording was cut off.
    cut = tmp_path / "cut.jsonl"
    write_lines(cut, lines[:100])
    with pytest.raises(sf.SessionError, match="truncated after line 100: no end line") as info:
        sf.read_session(str(cut))
    assert info.value.path == str(cut) and info.value.line is None
    # A line cut in the middle.
    torn = tmp_path / "torn.jsonl"
    torn.write_text("\n".join(lines[:100]) + "\n" + lines[100][:25], encoding="utf-8")
    with pytest.raises(sf.SessionError, match=r"not valid JSON \(truncated\?\)") as info:
        sf.read_session(str(torn))
    assert info.value.line == 101
    # The wrong count.
    counted = tmp_path / "counted.jsonl"
    write_lines(counted, lines[:-1] + [end_line(3)])
    with pytest.raises(sf.SessionError, match="says 3 events, the file has 174") as info:
        sf.read_session(str(counted))
    assert info.value.line == len(lines)
    # A line after the end.
    after = tmp_path / "after.jsonl"
    write_lines(after, lines + [lines[5]])
    with pytest.raises(sf.SessionError, match="a line after the end line") as info:
        sf.read_session(str(after))
    assert info.value.line == len(lines) + 1


# -- replay (G-047) ------------------------------------------------------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_replay_equals_live_for_the_appendix_runs(appendix_runs, tmp_path, run_id):
    ref = appendix_runs[run_id]
    path = tmp_path / "session.jsonl"
    save(appendix_runs, run_id, path)
    result = sf.replay(sf.read_session(str(path)))
    assert result.verified and result.divergence is None
    assert result.records == ref.records
    assert not result.outcome.stopped
    assert result.outcome.result["draws"] == ref.draws
    assert result.outcome.result["outcome"] == ref.outcome == "ok"
    assert result.outcome.output == ref.text


def test_a_tampered_event_is_detected(appendix_runs, tmp_path):
    path = tmp_path / "tampered.jsonl"
    save(appendix_runs, RUN1, path)
    lines = lines_of(path)
    k = 48                                   # the event tampered with, from 0
    obj = json.loads(lines[k + 2])
    assert obj["kind"] == "posted" and obj["urgency"] == 10
    obj["urgency"] = 99
    lines[k + 2] = json.dumps(obj, separators=(",", ":"))
    write_lines(path, lines)
    file = sf.read_session(str(path))
    with pytest.raises(sf.TraceMismatch, match="differs from the file at event 49") as info:
        sf.replay(file)
    assert info.value.line == k + 3 and info.value.path == str(path)
    assert "urgency: file 99, re-run 10" in str(info.value)
    result = sf.replay(file, check=False)
    assert not result.verified and result.outcome.stopped
    d = result.divergence
    assert d.index == k and d.line == k + 3
    assert d.expected == file.records[k] and d.got == appendix_runs[RUN1].records[k]
    assert d.error is not None and d.message.startswith("the re-run differs")
    # The run stopped at the differing event: nothing after it was run.
    assert result.records == appendix_runs[RUN1].records[:k + 1]


def test_a_file_that_goes_on_after_the_run_is_a_difference(appendix_runs, tmp_path):
    ref = appendix_runs[RUN1]
    path = tmp_path / "long.jsonl"
    save(appendix_runs, RUN1, path, events=list(ref.events) + [ev.Message("one more")])
    file = sf.read_session(str(path))
    assert not file.ended                       # its last event is not `ended`
    result = sf.replay(file, check=False)
    d = result.divergence
    assert d is not None and d.index == len(ref.events) and d.got is None
    assert "the file goes on after the re-run ended: 175 events stored, 174 re-run" in d.message
    assert result.records == ref.records


def test_a_run_that_goes_on_after_an_ended_file_is_a_difference(appendix_runs, tmp_path):
    ref = appendix_runs[RUN1]
    path = tmp_path / "ended.jsonl"
    save(appendix_runs, RUN1, path)
    file = sf.read_session(str(path))
    assert file.ended
    checker = sf.ReplayChecker(file)
    for event in ref.events:
        assert checker.check(event) is None
    assert checker.verified
    d = checker.check(ev.Message("one more"))
    assert d is not None and d.index == 174 and d.expected is None
    assert "the file ends after 174 events, but the re-run goes on" in d.message
    assert checker.check(ev.Message("another")) is None       # after the first, no more
    assert checker.finish() is d and not checker.verified


def test_a_part_way_file_replays_on_to_the_end(appendix_runs, tmp_path):
    ref = appendix_runs[RUN3]
    path = tmp_path / "part.jsonl"
    save(appendix_runs, RUN3, path, events=ref.events[:200])
    file = sf.read_session(str(path))
    assert not file.ended
    seen = []
    checker = sf.ReplayChecker(file, on_divergence=seen.append)
    result = sf.replay(file, observers=[checker])
    assert result.verified and result.divergence is None and seen == []
    assert result.records == ref.records and len(result.records) == 602
    assert result.outcome.result["draws"] == ref.draws


def test_a_stopped_rerun_is_not_a_difference(appendix_runs, tmp_path):
    path = tmp_path / "s.jsonl"
    save(appendix_runs, RUN1, path)
    checker = sf.ReplayChecker(sf.read_session(str(path)))
    for event in appendix_runs[RUN1].events[:10]:
        checker.check(event)
    assert checker.finish(stopped=True) is None
    assert checker.finish(stopped=False) is not None and not checker.verified


def test_the_differences_text():
    assert sf.differences({"kind": "term", "n": 1}, {"kind": "term", "n": 2}) == \
        "n: file 1, re-run 2"
    assert sf.differences({"a": 1}, {"a": 1, "b": 2}) == "b: file null, re-run 2"
    assert sf.differences([1], {"a": 1}) == "file [1], re-run {\"a\":1}"


# -- the run line (G-046) ------------------------------------------------------------------------------

def test_the_run_line_of_scripted_typed_and_interactive_runs():
    scripted = sf.run_line_of(presets.appendix_inputs(RUN1), typed=["9"], answered=["no"])
    assert scripted.mode == "scripted"
    assert scripted.inputs == ("3", "7", "3", "show-hypothesis", "7")     # [M86 p. 166]
    assert scripted.answers == ("ok",) and scripted.preset == RUN1 and scripted.seed == 54
    assert scripted.engine == __version__
    typed = sf.run_line_of(presets.interactive_inputs(30), typed=["1", "2", "show"],
                           answered=["nope", "ok"])
    assert typed.mode == "interactive" and typed.inputs == ("1", "2", "show")
    assert typed.answers == ("nope", "ok") and typed.preset == presets.INTERACTIVE
    clicked = sf.run_line_of(presets.appendix_inputs(RUN3, interactive=True), answered=["no"])
    assert clicked.mode == "interactive" and clicked.answers == ("no",)
    assert clicked.inputs == tuple(str(x) for x in presets.appendix_inputs(RUN3).lines)
    inputs = sf.run_inputs(typed, "replay of x")
    assert inputs.lines == ("1", "2", "show") and inputs.answers == ("nope", "ok")
    assert not inputs.interactive and inputs.title == "replay of x" and inputs.seed == 30
    assert sf.run_inputs(scripted).title == f"replay: {RUN1}, seed 54"
    assert sf.RunLine.from_json(typed.to_json()) == typed
