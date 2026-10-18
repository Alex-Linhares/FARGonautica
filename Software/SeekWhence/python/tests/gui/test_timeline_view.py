"""Loop0004 item 12: the timeline toolbar (`seekwhence/gui/timeline.py`) in the window,
after the Numbo GUI's `tests/gui/test_log_replay.py`. pytest-qt, offscreen.

- The toolbar holds the bar; after run 3 the slider spans the history, the marker strip
  has the terms, the guesses (`no`, then `ok`), the refutations and the slip, and the
  parse strip the parse at each term boundary [M86 pp. 170-179].
- Seeking to any event gives the same pane models as playing up to it: every view's
  digest at sampled seeks, in any order, equals the live digest at that event.
- The slider, the buttons, a click on a marker or a parse cell and the go-to box
  (`term 5`, `clock 20`, `event 42`) seek; a bad go-to text is refused.
- Scrubbing is off while a run is live, and on again after a stop.
- A new run clears the markers.
"""

import random

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, Qt                                # noqa: E402

from seekwhence.gui.models import presets                             # noqa: E402
from seekwhence.gui.models import timeline as tm                      # noqa: E402

from golden import APPENDIX                                           # noqa: E402

TIMEOUT = 30000
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def pt(p):
    return (round(p.x(), 3), round(p.y(), 3))


def items(view, name):
    return {k: (pt(it.pos()), round(it.opacity(), 4), it.isVisible(), it.toolTip(), it.zValue())
            for k, it in getattr(view, name).items()}


def digest(w):
    """What every pane shows now (the stats' state aside: it is about the controller;
    and the timeline, which spans the whole history once the run is over)."""
    seq = w.sequence_view.model
    cy = w.cytoplasm_view.view
    stats = {k: v for k, v in w.stats_view.texts().items() if k != "state"}
    return dict(
        sequence=(tuple(seq.terms), seq.ghosts(), seq.pending, tuple(seq.badges), seq.form,
                  seq.refuted, seq.waiting),
        glints=items(cy, "glint_items"), arcs=items(cy, "arc_items"),
        hulls=items(cy, "hull_items"), markers=items(cy, "marker_items"),
        socratoplasm=(w.socratoplasm_view.parse(), w.socratoplasm_view.lines(),
                      w.socratoplasm_view.boxes()),
        hypothesis=(w.hypothesis_view.form(), w.hypothesis_view.lines(),
                    w.hypothesis_view.cells(), w.hypothesis_view.nodes()),
        platoplasm=(w.platoplasm_view.nodes(), w.platoplasm_view.links(),
                    w.platoplasm_view.atoms(), w.platoplasm_view.crown(),
                    w.platoplasm_view.lines()),
        taskrack=w.taskrack_view.lines(),
        reformulation=(w.reformulation_view.lines(), w.reformulation_view.freeze_dried_text()),
        console=w.console_view.list.now_row(),
        stats=stats,
        log=w.log_view.list.position,
    )


def assert_same(got, want, where):
    for key in want:
        assert got[key] == want[key], f"{key} differs at event {where}"


def play_live(window, qtbot, inputs):
    """Run to the end; the digest after each event is drawn."""
    digests = {}
    window.event_drawn.connect(lambda i: digests.__setitem__(i, digest(window)))
    run_to_end(qtbot, window, inputs)
    window.event_drawn.disconnect()
    return digests


# -- the bar after a run [M86 pp. 170-179] ------------------------------------------------------------

def test_the_toolbar_holds_the_bar_and_run_3_fills_it(window, qtbot):
    t = window.timeline
    assert t.parent() is window.timeline_bar or t in window.timeline_bar.findChildren(type(t))
    assert t.pane_id == "P10" and "FCCA" in t.pages
    assert t.text() == "no events" and not t.slider.isEnabled()
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    n = len(window.history)
    assert t.slider.isEnabled() and t.slider.maximum() == n - 1 and t.slider.value() == n - 1
    assert t.text() == f"event {n} of {n}: ended   term 6   clock 189"
    kinds = [m.kind for m in t.model.markers]
    assert kinds.count("term") == 6 and kinds.count("refutation") == 2
    assert kinds.count("slip") == 1 and kinds.count("guess") == 2
    assert [m.verdict for m in t.model.markers_of("guess")] == ["no", "ok"]
    cells = t.parses.cells()
    assert [c[2].parse for c in cells] == ["()", "(((1 2)))", "((1 2) (1))", "((1 2) (1))",
                                           "((1 2) (1 2 3))", "((1 2) (1 2 3))"]
    assert t.markers.paints > 0 and t.parses.paints > 0
    # The marker strip's tooltip names the marker under the mouse.
    slip = t.model.markers_of("slip")[0]
    x = tm.marker_x(slip.index, n, t.markers.width())
    assert t.markers.marker_at(x) == slip
    assert t.markers.marker_at(x + 20) is None or t.markers.marker_at(x + 20) != slip
    x0, x1, entry = cells[2]
    assert t.parses.cell_at((x0 + x1) / 2) == cells[2] and entry.parse == "((1 2) (1))"


# -- seeking equals playing (PANES.md P10) ---------------------------------------------------------------

def test_seeking_to_sampled_events_gives_what_playing_up_to_them_gave(window, qtbot):
    live = play_live(window, qtbot, presets.appendix_inputs(RUN3))
    n = len(window.history)
    assert len(live) == n
    sample = [0, 1, 9, 118, 185, 282, 300, 325, 579, 601, n - 1]
    sample += random.Random(12).sample(range(n), 12)
    random.Random(7).shuffle(sample)
    drawn = []
    window.event_drawn.connect(drawn.append)
    for index in sample:
        window.timeline.slider.setValue(index)
        assert window.position == index
        want = dict(live[index])
        if index == n - 1:
            # The console's summary line comes at the run's end, after the last event
            # was drawn live (G-018), so its cursor row is one further at a seek.
            want.pop("console")
        assert_same(digest(window), want, index)
    assert drawn == sample, "each seek draws once"


# -- the controls -----------------------------------------------------------------------------------

def test_the_slider_the_buttons_the_markers_and_the_cells_seek(window, qtbot):
    # The width the window had before G-058 (it opens at its minimum, 862 px, on the
    # offscreen screen now): the click "between markers" below is 2 events from a term
    # tick, outside the hit radius only when an event is several pixels wide.
    window.resize(2224, 950)
    qtbot.waitUntil(lambda: window.width() == 2224, timeout=TIMEOUT)
    live = play_live(window, qtbot, presets.appendix_inputs(RUN3))
    t = window.timeline
    n = len(window.history)
    t.slider.setValue(120)
    assert window.position == 120 and t.text().startswith("event 121 of ")
    assert_same(digest(window), live[120], 120)
    t.back_button.click()
    assert window.position == 119
    t.forward_button.click()
    t.forward_button.click()
    assert window.position == 121
    assert_same(digest(window), live[121], 121)
    # A click on the slip marker seeks to the class-changed event.
    slip = t.model.markers_of("slip")[0]
    x = tm.marker_x(slip.index, n, t.markers.width())
    qtbot.mouseClick(t.markers, Qt.MouseButton.LeftButton, pos=QPoint(round(x), 16))
    assert window.position == slip.index == 185
    assert window.history.events[185].kind == "class-changed"
    # A click between markers seeks to the nearest event.
    x = tm.marker_x(400, n, t.markers.width())
    assert t.markers.marker_at(x) is None
    qtbot.mouseClick(t.markers, Qt.MouseButton.LeftButton, pos=QPoint(round(x), 16))
    assert abs(window.position - 400) <= 1
    # A click on a parse cell seeks to its term boundary.
    x0, x1, entry = t.parses.cells()[2]
    qtbot.mouseClick(t.parses, Qt.MouseButton.LeftButton, pos=QPoint(round((x0 + x1) / 2), 9))
    assert window.position == entry.index == 282
    assert window.socratoplasm_view.parse() == "((1 2) (1))"
    assert_same(digest(window), live[282], 282)
    # The strips show the position.
    assert t.model.position == 282 and t.slider.value() == 282


def test_the_go_to_box(window, qtbot):
    live = play_live(window, qtbot, presets.appendix_inputs(RUN3))
    t = window.timeline
    h = window.history
    for text, index in (("term 5", h.term_index(5)), ("clock 20", h.clock_index(20)),
                        ("event 42", 41), ("3", h.term_index(3))):
        t.goto_edit.setText(text)
        assert t.go()
        assert window.position == index, text
        assert_same(digest(window), live[index], index)
    t.goto_edit.setText("term 2")
    qtbot.keyClick(t.goto_edit, Qt.Key.Key_Return)
    assert window.position == h.term_index(2) == 10
    assert window.stats_view.text("terms") == "2"
    assert window.jump_to("term", 5) and window.position == 308
    assert window.history.events[308].kind == "term"
    # Refused: no such term, event or clock; a text that does not read.
    assert not window.jump_to("term", 9) and window.position == 308
    assert "no term 9" in window.statusBar().currentMessage()
    assert not window.jump_to("event", 0) and not window.jump_to("clock", 10 ** 6)
    t.goto_edit.setText("iteration 3")
    assert not t.go() and window.position == 308
    assert t.goto_edit.styleSheet() != ""
    t.goto_edit.setText("event 1")
    assert t.go() and t.goto_edit.styleSheet() == "" and window.position == 0


def test_scrubbing_is_off_while_a_run_is_live(window, qtbot):
    window.set_inputs(presets.appendix_inputs(RUN1))
    window.step()
    qtbot.waitUntil(lambda: window.position == 0, timeout=TIMEOUT)
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    assert window.controller.is_alive()
    t = window.timeline
    assert not t.slider.isEnabled() and not t.go_button.isEnabled()
    assert not t.markers.scrubbable and not t.parses.scrubbable
    assert not window.seek(0)
    window._seek_from_timeline(0)
    assert "stop the run" in window.statusBar().currentMessage()
    assert not window.jump_to("term", 1)
    # The slider grows with the history and cannot be dragged.
    window.step()
    qtbot.waitUntil(lambda: window.position == 1, timeout=TIMEOUT)
    assert t.slider.maximum() == 1 and t.slider.value() == 1 and not t.slider.isEnabled()
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.stop()
    assert not window.controller.is_alive()
    assert t.slider.isEnabled() and t.markers.scrubbable
    assert window.seek(0) and window.position == 0


def test_a_new_run_clears_the_markers(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    t = window.timeline
    assert len(t.model.markers) == 11
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert [m.kind for m in t.model.markers] == ["term"] * 4 + ["guess"]
    assert t.slider.maximum() == len(window.history) - 1 == 173
    assert [c[2].parse for c in t.parses.cells()] == ["()", "()", "(((3 7)))", "(((3 7)))"]
