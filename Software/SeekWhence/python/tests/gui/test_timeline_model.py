"""Loop0004 item 12: the timeline's model (`seekwhence/gui/models/timeline.py`) and the
event log's groups and lock (`models/event_log.py`), Qt-free.

- The markers of the three appendix runs: a tick per term at its `term` event
  [M86 p. 164]; run 1's guess answered `ok` [M86 p. 166]; run 3's `no` then `ok`
  [M86 pp. 175, 179], its two refutations [M86 pp. 172, 176] and its slip `Countup -->
  S-group` [M86 p. 174]; the blast on blackburn-8 [M86 p. 92].
- The parse strip: one entry per prompt and at the end, the parse of the snapshot in
  force; run 3's `((1 2) (1))` before term 4 and `((1 2) (1 2 3))` before term 6
  [M86 pp. 174, 178].
- `sync` is incremental and equals a full scan; a `start` clears.
- The pure functions: `marker_x` and `index_at` are inverses, `parse_cells` tile the
  strip, the styles, the tooltips, the position text, the go-to box.
- `GROUPS` partition `KINDS`; `set_hidden` loses no event recorded meanwhile.
"""

import io
import threading

from seekwhence import bench
from seekwhence import events as ev
from seekwhence import sequences as sq
from seekwhence.config import Config
from seekwhence.gui.models import timeline as tm
from seekwhence.gui.models.event_log import GROUPS, KINDS, EventLog, group_of
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX, appendix_inputs

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)


def observed(run_id, seed=None):
    """An appendix run through the tap: its history."""
    config, lines, answers = appendix_inputs(run_id)
    log = EventLog()
    tap = Tap(log)
    session = Session(config, SEEDS[run_id] if seed is None else seed, out=io.StringIO(),
                      lines=list(lines), answers=list(answers), observers=[tap])
    tap.attach_session(session)
    session.run()
    return log


def blackburn(name, seed, terms):
    """A Blackburn sequence fed as the bench feeds it, through the tap: its history."""
    seq = sq.find(name)
    log = EventLog()
    tap = Tap(log)
    session = Session(Config(), seed, out=io.StringIO(), lines=list(seq.prefix(terms)),
                      answerer=lambda console: bench.ContinuationAnswerer(console, seq.terms),
                      observers=[tap])
    tap.attach_session(session)
    session.run()
    return log


def synced(log):
    model = tm.TimelineModel(log)
    model.sync()
    return model


# -- the markers [M86 pp. 164-179] -----------------------------------------------------------------

def test_run_1_has_four_term_ticks_and_one_guess_answered_ok():
    log = observed(RUN1)
    model = synced(log)
    terms = model.markers_of("term")
    assert [m.label for m in terms] == ["1", "2", "3", "4"]
    assert [m.index for m in terms] == [log.term_index(n) for n in (1, 2, 3, 4)]
    assert terms[0].text == "term 1: 3" and terms[1].text == "term 2: 7"
    guesses = model.markers_of("guess")
    assert len(guesses) == 1
    guess = guesses[0]
    assert guess.verdict == "ok" and guess.label == "ok"
    assert log.events[guess.index].kind == "guess"
    assert guess.text.startswith("guess: (Cycle 3 2 (3 7))  3 7 3 7")
    assert guess.text.endswith("answered ok (ok)")
    assert model.markers_of("refutation") == [] and model.markers_of("slip") == []
    assert model.terms == 4


def test_run_3_has_two_refutations_a_slip_and_a_guess_refused_then_accepted():
    log = observed(RUN3)
    model = synced(log)
    kinds = [m.kind for m in model.markers]
    assert kinds == ["term", "term", "term", "refutation", "slip", "term", "guess", "term",
                     "refutation", "term", "guess"]
    refutations = model.markers_of("refutation")
    assert [log.events[m.index].kind for m in refutations] == ["refuted", "refuted"]
    assert refutations[0].text == "refuted: (Countup 1) predicted 3, term 3 is 1"
    assert refutations[1].text.startswith("refuted: (S-group 1 2) predicted 1, term 5 is 3")
    slip = model.markers_of("slip")[0]
    assert slip.text == "slip: Countup --> S-group (strict)"
    assert log.events[slip.index].kind == "class-changed"
    no, ok = model.markers_of("guess")
    assert (no.verdict, no.label) == ("no", "no") and (ok.verdict, ok.label) == ("ok", "ok")
    assert "answered nope (no)" in no.text and "answered ok (ok)" in ok.text
    assert "(1 2)(1 2)(1 2)" in no.text and "(1 2)(1 2 3)(1 2 3 4)" in ok.text
    assert no.index < ok.index
    assert model.terms == 6
    # Every marker's text ends with the tooltip's event number.
    assert tm.marker_tooltip(slip) == f"{slip.text}  (event {slip.index + 1})"


def test_a_guess_marker_is_grey_until_answered():
    log = EventLog()
    model = tm.TimelineModel(log)
    log.on_event(ev.RunStarted(1, ()))
    log.on_event(ev.GuessMade("(Countup 1)", "1 2", (1, 2)))
    model.sync()
    guess = model.markers[0]
    assert guess.verdict is None and guess.label == "?"
    assert tm.marker_style(guess).color == "#9a9a9a"
    log.on_event(ev.GuessAnswered("no", False))
    model.sync()
    guess = model.markers[0]
    assert guess.verdict == "no" and tm.marker_style(guess).color == "#c62828"
    assert len(model.markers) == 1


def test_the_blast_on_blackburn_8_is_a_marker():
    log = blackburn("blackburn-8", 1, 12)
    model = synced(log)
    blasts = model.markers_of("blast")
    assert blasts, "blackburn-8 on seed 1 blasts [M86 p. 92]"
    assert all(log.events[m.index].kind in ("blast", "blasted") for m in blasts)
    assert all(m.text.startswith("blast") for m in blasts)
    assert tm.marker_style(blasts[0]).shape == "bar"


# -- the parse strip [M86 p. 174] [FCCA pp. 41, 64-65] -------------------------------------------------

def test_run_3_s_parses_at_each_term_boundary():
    log = observed(RUN3)
    model = synced(log)
    assert [(e.terms, e.parse) for e in model.parses] == [
        (1, "()"), (2, "(((1 2)))"), (3, "((1 2) (1))"), (4, "((1 2) (1))"),
        (5, "((1 2) (1 2 3))"), (6, "((1 2) (1 2 3))")]
    prompts = [i for i, e in enumerate(log.events) if e.kind == "prompt"]
    ended = [i for i, e in enumerate(log.events) if e.kind == "ended"]
    assert [e.index for e in model.parses] == prompts + ended
    for entry in model.parses:
        assert entry.parse == log.state_at(entry.index).parse


def test_run_2_s_parses():
    model = synced(observed(RUN2))
    assert [e.parse for e in model.parses] == [
        "()", "()", "(((3 7)))", "(((3 7)) (3) (3))", "(((3 7)) (3) (3))"]


# -- sync and the position text ------------------------------------------------------------------------

def test_sync_is_incremental_and_equals_a_full_scan():
    full = observed(RUN3)
    log = EventLog()
    model = tm.TimelineModel(log)
    scanned = 0
    for i, event in enumerate(full.events):
        log.record(event, full.snapshots.get(i))
        if i % 7 == 0:
            scanned += model.sync()
    scanned += model.sync()
    assert scanned == len(full)
    assert model.sync() == 0
    reference = synced(full)
    assert model.markers == reference.markers and model.parses == reference.parses


def test_a_start_clears_the_markers():
    log = observed(RUN1)
    model = synced(log)
    assert model.markers
    log.on_event(ev.RunStarted(2, ()))
    model.sync()
    assert model.markers == [] and model.parses == [] and model.terms == 0
    log2 = EventLog()
    model2 = tm.TimelineModel(log2)
    assert model2.sync() == 0 and model2.markers == []


def test_the_position_text():
    log = observed(RUN1)
    model = tm.TimelineModel(log)
    assert tm.TimelineModel(EventLog()).text() == "no events"
    assert model.text() == "no events"
    model.set_position(0)
    assert model.text() == "event 1 of 174: start"
    assert model.text(1) == "event 2 of 174: term   term 1"
    assert model.text(173) == "event 174 of 174: ended   term 4   clock 52"
    assert model.text(500) == model.text(173)


# -- placing and styling -------------------------------------------------------------------------------

def test_marker_x_and_index_at_are_inverses():
    for count, width in ((1, 300), (2, 300), (174, 500), (29000, 640)):
        for index in (0, count // 3, count - 1):
            x = tm.marker_x(index, count, width)
            assert tm.INSET <= x <= width - tm.INSET
            assert tm.index_at(x, count, width) == index
    assert tm.index_at(-50, 174, 500) == 0 and tm.index_at(10000, 174, 500) == 173
    assert tm.marker_x(0, 1, 300) == tm.INSET and tm.index_at(150, 1, 300) == 0


def test_parse_cells_tile_the_strip():
    model = synced(observed(RUN3))
    count, width = model.count, 600
    cells = tm.parse_cells(model.parses, count, width)
    assert len(cells) == len(model.parses)
    assert cells[0][0] == tm.marker_x(model.parses[0].index, count, width)
    for (x0, x1, entry), (next_x0, _, _) in zip(cells, cells[1:]):
        assert x1 == next_x0 and x0 <= x1
    assert cells[-1][1] == width - tm.INSET
    assert [c[2] for c in cells] == model.parses
    assert tm.parse_cells([], count, width) == []


def test_marker_styles():
    styles = {kind: tm.marker_style(tm.Marker(0, kind, "", "")) for kind in tm.MARKER_KINDS}
    assert styles["term"].shape == "tick" and styles["term"].row == 0
    assert styles["guess"].shape == "diamond" and styles["guess"].row == 1
    assert styles["refutation"].shape == "triangle" and styles["refutation"].color == "#e67e00"
    assert styles["slip"].shape == "diamond" and styles["slip"].color == "#7b3fa0"
    assert styles["blast"].shape == "bar"
    assert tm.marker_style(tm.Marker(0, "guess", "ok", "", "ok")).color == "#1a9a3c"
    assert len({s.color for s in styles.values()}) == 5


# -- the go-to box -----------------------------------------------------------------------------------

def test_parse_goto():
    assert tm.parse_goto("term 5") == ("term", 5)
    assert tm.parse_goto(" Clock 812 ") == ("clock", 812)
    assert tm.parse_goto("event 402") == ("event", 402)
    assert tm.parse_goto("5") == ("term", 5)
    for bad in ("", "term", "term five", "iteration 3", "term 5 6", "-3"):
        assert tm.parse_goto(bad) is None, bad


def test_goto_index():
    log = observed(RUN3)
    assert tm.goto_index(log, "term", 5) == log.term_index(5) == 308
    assert tm.goto_index(log, "term", 7) is None
    assert tm.goto_index(log, "clock", 20) == log.clock_index(20)
    assert log.events[tm.goto_index(log, "clock", 20)].clock == 20
    assert tm.goto_index(log, "clock", 100000) is None
    assert tm.goto_index(log, "event", 1) == 0 and tm.goto_index(log, "event", 602) == 601
    assert tm.goto_index(log, "event", 0) is None and tm.goto_index(log, "event", 603) is None
    assert tm.goto_index(log, "iteration", 1) is None


# -- the log's groups and lock (G-044) ---------------------------------------------------------------

def test_the_groups_partition_the_kinds():
    names = [group for group, _ in GROUPS]
    assert names == ["run", "taskrack", "cytoplasm", "templates", "gnoths", "filtering",
                     "reformulation"]
    listed = [kind for _, kinds in GROUPS for kind in kinds]
    assert sorted(listed) == sorted(KINDS) and len(listed) == len(set(listed))
    assert group_of("chosen") == "taskrack" and group_of("blasted") == "reformulation"
    assert group_of("glint") == "cytoplasm" and group_of("no-such-kind") is None


def test_set_hidden_loses_no_event_recorded_meanwhile():
    log = EventLog()
    events = observed(RUN3).events
    stop = threading.Event()

    def feed():
        for event in events:
            log.record(event)
        stop.set()

    worker = threading.Thread(target=feed)
    worker.start()
    flips = 0
    while not stop.is_set():
        log.set_hidden({"posted"} if flips % 2 == 0 else set())
        flips += 1
    worker.join()
    log.set_hidden({"posted"})
    expected = [i for i, e in enumerate(events) if e.kind != "posted"]
    assert log.visible == expected
    log.set_hidden(set())
    assert log.visible == list(range(len(events)))
