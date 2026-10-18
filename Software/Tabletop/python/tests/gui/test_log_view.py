"""The trace and event log pane and the timeline (item 11; `log_view.py`, `timeline.py`;
G-042, G-043), offscreen, one shared window and one run of `surround-05` seed 3: the run
through the window equals the plain run, and the trace list's and the timeline strip's
backing stores equal fresh renders at sampled events; **seeking to every sampled event
gives the same pane models as playing up to it** (every pane built so far, the
snapshot-dated ones where the snapshot in force is the same); seeking is refused while a
run is live, and the timeline's controls are disabled then; the Trace tab's lines are the
engine's trace, the quiet switch, the greyed lines after the position and the tinted
block, a click seeks to the line's block; the Events tab's rows, the Kinds filter, a
double-click and Enter seek; Save trace and Save events write the CLI's text; the
timeline's slider, buttons, go-to-codelet box, markers and strip; the hover texts; the
minimums.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QEvent, QPoint, Qt  # noqa: E402
from PySide6.QtGui import QColor, QHelpEvent  # noqa: E402
from PySide6.QtTest import QTest  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop import events as ev  # noqa: E402
from tabletop.gui import log_view as lv  # noqa: E402
from tabletop.gui import timeline as tv  # noqa: E402
from tabletop.gui.models import coderack as cr  # noqa: E402
from tabletop.gui.models import slipnet_grid as sg  # noqa: E402
from tabletop.gui.models import slipnet_network as sn  # noqa: E402
from tabletop.gui.models import temperature as tm  # noqa: E402
from tabletop.gui.models import timeline as tl  # noqa: E402
from tabletop.gui.models.event_log import json_lines, row_text  # noqa: E402
from tabletop.observe import TextTrace  # noqa: E402
from test_coderack_view import Probe, qtbot_wait  # noqa: E402
from test_table_view import EVERY, run_to_end  # noqa: E402

QUICK = ("surround-05", 3)
PROBES = {}
SAMPLED = {}        # index -> the pane models' state while playing


def event_models(w):
    """The pane models that are functions of the events alone, as plain data."""
    return (w.table_view.view.model.layout(),
            tm.thermometer_of(w.thermometer_view.model),
            tm.series_of(w.temperature_view.model),
            sg.grid_layout(w.slipnet_grid_view.model, 400, 300),
            sn.network_layout(w.slipnet_network_view.model, 700, 420),
            cr.strip_layout(w.coderack_view.model, 700, 90),
            w.competitions_view.model.lines(),
            # The timeline's count and its later markers are the whole history's (G-043).
            w.timeline.text().split(":", 1)[1],
            [(m.index, m.kind) for m in w.timeline.model.markers if m.index <= w.position])


def trace_lines(w):
    """The trace's lines so far: while playing, the ones the CLI has printed by then
    (a codelet's line is printed when the next is chosen); after a seek, every line of
    the run is known and the ones after the position are greyed (G-042)."""
    return [(line.text, line.start) for line in w.log_view.trace.model.lines]


def dated_models(w):
    """The pane models that date their numbers by the snapshot in force."""
    return (w.rack_view.model.rows(),
            w.worldview_view.model.entries(),
            w.worldview_view.model.lines())


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """The quick run through the shared window, the trace list and the strip probed, the
    pane models sampled at every `EVERY`th event."""
    w = module_window
    w.resize(1600, 1000)
    # The log dock floats (its own window) so that its lists are tall enough to probe
    # rows around a block (the rack dock floats the same way in `test_coderack_view.py`).
    w.log_dock.setFloating(True)
    w.log_dock.resize(1500, 420)
    w.log_dock.move(40, 40)
    qapp.processEvents()
    qtbot_wait(qapp)
    assert w.log_dock.isFloating()
    assert not w.log_view.trace.list.viewport().visibleRegion().isEmpty()
    assert not w.timeline.strip.visibleRegion().isEmpty()
    assert w.log_view.trace.list.rows_in_view() >= 12
    PROBES["trace"] = Probe(w, w.log_view.trace.list.viewport(), top=w.log_dock)
    PROBES["strip"] = Probe(w, w.timeline.strip)

    def sample(index):
        if index % EVERY == 0:
            SAMPLED[index] = (event_models(w), dated_models(w), trace_lines(w))

    w.event_drawn.connect(sample)
    outcome = run_to_end(w, qapp, *QUICK)
    w.event_drawn.disconnect(sample)
    for probe in PROBES.values():
        w.event_drawn.disconnect(probe.drawn)
    yield outcome
    # Dock the floating dock again: left floating, its window stays on the offscreen
    # screen after the module's window closes and takes the next module's mouse moves.
    w.log_dock.setFloating(False)
    qapp.processEvents()


@pytest.fixture(scope="module")
def quick():
    return plain_run(*QUICK, V2)


class Sink:
    def write(self, text):
        pass

    def flush(self):
        pass


def engine_lines(events, quiet=False):
    trace = TextTrace(Sink(), quiet=quiet)
    for event in events:
        trace.on_event(event)
    return trace.lines


def at_end(w, qapp):
    """Every test starts from the end of the run, so a failure cannot cascade."""
    assert w.seek(len(w.history) - 1)
    qapp.processEvents()


def darkest(widget, row):
    """The darkest channel value along a row of the list (greyscale text)."""
    image = widget.viewport().grab().toImage()
    y0 = int(widget.row_rect(row).top())
    return min(QColor(image.pixel(x, y)).red()
               for y in range(y0 + 2, y0 + widget.line - 2)
               for x in range(2, min(300, image.width() - 2)))


def right_edge(widget, row):
    image = widget.viewport().grab().toImage()
    y = int(widget.row_rect(row).top() + widget.line // 2)
    return QColor(image.pixel(image.width() - 3, y)).name()


# -- the run and the probes --------------------------------------------------------------------

def test_the_run_through_the_window_equals_the_plain_run(ran, module_window, quick):
    w = module_window
    assert w.history.records == list(quick.records)
    assert ran.result.answer == "spoon3" and len(w.history) == len(quick.events) == 720      # spoon3 (none), 720 events since v5's fix, D-116 to D-120 (cup2 (E2), 507 events from item 08's runs to v2; small-glass1, E1, at item 07)
    for name, probe in PROBES.items():
        assert len(probe.checked) >= 9, name
        assert probe.mismatches == [], name


def test_seeking_to_sampled_events_gives_the_same_models_as_playing(ran, module_window, qapp):
    """PANES.md P11's test: every pane model rebuilt by a seek equals the one playing
    gave, at every sampled event; the snapshot-dated models where the snapshot in force
    was already recorded while playing (a step's snapshot is recorded after its last event
    was drawn, G-024)."""
    w = module_window
    assert len(SAMPLED) >= 9
    compared_dated = 0
    for index in sorted(SAMPLED, reverse=True):
        assert w.seek(index) and w.position == index
        played_events, played_dated, played_lines = SAMPLED[index]
        assert event_models(w) == played_events, index
        lines = trace_lines(w)
        assert lines[:len(played_lines)] == played_lines, index
        if w.history.snapshot_index(index) != index:
            assert dated_models(w) == played_dated, index
            compared_dated += 1
    assert compared_dated >= 5
    at_end(w, qapp)


# -- the Trace tab -------------------------------------------------------------------------------

def test_the_trace_tab_shows_the_engines_trace_and_the_quiet_form(ran, module_window, quick, qapp):
    w = module_window
    at_end(w, qapp)
    tab = w.log_view.trace
    assert w.log_view.tabs.currentWidget() is tab
    assert [line.text for line in tab.model.lines] == engine_lines(quick.events)
    assert tab.list.row_count() == len(engine_lines(quick.events)) == 177   # 59 codelets since v5's fix, D-116 to D-120 (104 lines of 44 codelets from item 08's runs to v2; 218 lines of 77 at item 07)
    assert tab.count_label.text() == "177 lines"
    assert w.log_view.trace_text() == "".join(l + "\n" for l in engine_lines(quick.events))
    tab.quiet_box.setChecked(True)
    assert tab.list.row_count() == 2 and tab.count_label.text() == "2 lines"
    assert w.log_view.trace_text() == "".join(l + "\n" for l in engine_lines(quick.events, True))
    assert tab.list.row_display(1) == quick.report
    tab.set_quiet(False)
    assert not tab.quiet_box.isChecked() and tab.list.row_count() == 177


def test_the_lines_after_the_position_are_greyed_and_the_block_tinted(ran, module_window, qapp):
    w = module_window
    tab = w.log_view.trace
    index = w.history.codelet_index(19) + 3        # inside codelet 19's block (two lines: PK2SALO picks cup2 and a slipnet posting; since item 08's runs and still since v5's fix, D-116 to D-120; codelet 22's block at item 07)
    assert w.seek(index)
    w.log_view.redraw(now=True)
    qapp.processEvents()
    rows = tab.model.block_rows(index)
    assert tab.list.now_rows() == set(rows)
    past = rows[0] - 1
    future = rows[-1] + 1
    assert tab.list.row_for(index) in rows
    assert not tab.list.is_future(past) and tab.list.is_future(future)
    tab.list.verticalScrollBar().setValue(past)
    tab.list.viewport().repaint()
    assert tab.list.first_row() == past and future < past + tab.list.rows_in_view()
    assert darkest(tab.list, past) < 100
    assert darkest(tab.list, future) >= 140
    assert right_edge(tab.list, rows[1]) == lv.NOW        # a short note of the block
    assert right_edge(tab.list, past) == lv.BACKGROUND
    at_end(w, qapp)
    tab.list.viewport().repaint()
    assert not any(tab.list.is_future(r) for r in range(tab.list.row_count()))


def test_a_click_on_a_trace_line_seeks_to_its_block(ran, module_window, qapp):
    w = module_window
    at_end(w, qapp)
    tab = w.log_view.trace
    tab.list.verticalScrollBar().setValue(0)
    qapp.processEvents()
    row = 5                                        # a codelet line near the top
    line = tab.model.lines[row]
    assert isinstance(w.history.events[line.start], ev.CodeletChosen)
    drawn = []
    w.event_drawn.connect(drawn.append)
    try:
        y = tab.list.row_rect(row).top() + tab.list.line // 2
        QTest.mouseClick(tab.list.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(20, int(y)))
    finally:
        w.event_drawn.disconnect(drawn.append)
    assert w.position == line.start and drawn == [line.start]
    assert tab.list.cursor_row == row
    assert all(view.events == line.start + 1 for view in (w.log_view, w.timeline))
    at_end(w, qapp)


def test_the_hover_text_is_the_line(ran, module_window, qapp):
    w = module_window
    at_end(w, qapp)
    tab = w.log_view.trace
    tab.list.verticalScrollBar().setValue(0)
    pos = QPoint(20, int(tab.list.row_rect(1).top() + 3))
    tab.list.event(QHelpEvent(QEvent.Type.ToolTip, pos, tab.list.viewport().mapToGlobal(pos)))
    assert tab.list.toolTip() == tab.model.lines[1].text


# -- the Events tab ------------------------------------------------------------------------------

def test_the_events_tab_rows_and_the_kinds_filter(ran, module_window, qapp):
    w = module_window
    at_end(w, qapp)
    tab = w.log_view.events_tab
    n = len(w.history)
    assert tab.list.row_count() == n
    assert tab.list.row_display(0) == row_text(0, w.history.records[0])
    assert tab.count_label.text() == f"{n} of {n} events"
    posted = w.history.counts["CodeletPosted"]
    tab.kind_actions["CodeletPosted"].setChecked(False)
    assert tab.list.row_count() == n - posted and tab.hidden_kinds() == {"CodeletPosted"}
    assert tab.count_label.text() == f"{n - posted} of {n} events"
    assert all(w.history.events[tab.list.index_of(r)].name != "CodeletPosted"
               for r in range(0, tab.list.row_count(), 7))
    tab.hide_all_action.trigger()
    assert tab.list.row_count() == 0
    tab.show_all_action.trigger()
    assert tab.list.row_count() == n and not tab.hidden_kinds()


def test_a_double_click_or_enter_on_an_event_row_seeks(ran, module_window, qapp):
    w = module_window
    at_end(w, qapp)
    w.log_view.tabs.setCurrentIndex(1)
    qapp.processEvents()
    tab = w.log_view.events_tab
    tab.list.verticalScrollBar().setValue(0)
    qapp.processEvents()
    row = 3
    y = int(tab.list.row_rect(row).top() + tab.list.line // 2)
    QTest.mouseDClick(tab.list.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(20, y))
    assert w.position == tab.list.index_of(row) == 3
    tab.list.cursor_row = 10
    QTest.keyClick(tab.list, Qt.Key.Key_Return)
    assert w.position == 10
    QTest.keyClick(tab.list, Qt.Key.Key_Down)
    assert tab.list.cursor_row == 11
    QTest.keyClick(tab.list, Qt.Key.Key_End)
    assert tab.list.cursor_row == tab.list.row_count() - 1
    assert tab.list.now_rows() == {10} and tab.list.is_future(11) and not tab.list.is_future(10)
    w.log_view.tabs.setCurrentIndex(0)
    at_end(w, qapp)


def test_the_hidden_tab_paints_nothing(ran, module_window, qapp):
    w = module_window
    at_end(w, qapp)
    before = (w.log_view.trace.list.paints, w.log_view.events_tab.list.paints)
    assert w.seek(w.position - 1)
    qapp.processEvents()
    after = (w.log_view.trace.list.paints, w.log_view.events_tab.list.paints)
    assert after[0] == before[0] + 1 and after[1] == before[1]
    at_end(w, qapp)


# -- the files (PANES.md P10) -------------------------------------------------------------------

def test_save_trace_and_save_events_write_the_cli_text(ran, module_window, quick, qapp, tmp_path):
    w = module_window
    at_end(w, qapp)
    assert w.save_trace_action.isEnabled() and w.save_events_action.isEnabled()
    path = w.save_trace(tmp_path / "trace.txt")
    assert path.read_text(encoding="utf-8") == "".join(l + "\n" for l in engine_lines(quick.events))
    assert w.statusBar().currentMessage().startswith("trace saved: ")
    w.log_view.trace.set_quiet(True)
    path = w.save_trace(tmp_path / "quiet.txt")
    assert path.read_text(encoding="utf-8") == "".join(
        l + "\n" for l in engine_lines(quick.events, quiet=True))
    w.log_view.trace.set_quiet(False)
    path = w.save_events(tmp_path / "events.jsonl")
    assert path.read_text(encoding="utf-8") == json_lines(w.history)
    assert path.read_text(encoding="utf-8").count("\n") == len(quick.events)
    w._write(w.save_events, tmp_path / "missing" / "events.jsonl")
    assert w.statusBar().currentMessage().startswith("cannot save ")


# -- the timeline (PANES.md P11) ----------------------------------------------------------------

def test_the_slider_the_buttons_and_the_go_to_box(ran, module_window, qapp):
    w = module_window
    at_end(w, qapp)
    t = w.timeline
    n = len(w.history)
    assert t.slider.maximum() == n - 1 and t.slider.value() == n - 1
    assert t.slider.isEnabled() and t.strip.scrubbable
    assert t.text() == t.model.text(n - 1)
    assert (t.codelet_spin.minimum(), t.codelet_spin.maximum()) == (1, 59)   # since v5's fix (D-116 to D-120); 44 from item 08's runs to v2
    t.back_button.click()
    assert w.position == n - 2
    t.forward_button.click()
    assert w.position == n - 1
    t.codelet_spin.setValue(30)
    t.go_button.click()
    assert w.position == w.history.codelet_index(30)
    assert t.text().endswith("CodeletChosen   codelet 30")
    t.slider.setValue(100)
    assert w.position == 100 and t.slider.value() == 100
    assert w.jump_to_codelet(99) is False and w.position == 100
    at_end(w, qapp)


def test_the_strip_draws_the_markers_and_a_click_seeks(ran, module_window, qapp):
    w = module_window
    at_end(w, qapp)
    strip = w.timeline.strip
    # The cursor (a magenta line at the event shown) is moved to the start, off the
    # answer's bar: at the end it stands one event away, close to it at 720 events.
    assert w.seek(0)
    strip.repaint()
    image = strip.grab().toImage()
    answer = w.timeline.model.markers_of("answer")[0]
    x = tl.marker_x(answer.index, w.timeline.model.count, strip.width())
    y = tv.ROW_HEIGHT + 5
    assert QColor(image.pixel(int(x), y)).name() == "#1a1a1a"
    change = w.timeline.model.markers_of("correspondence")[0]      # cp1 built, a blue diamond
    x = tl.marker_x(change.index, w.timeline.model.count, strip.width())
    assert QColor(image.pixel(int(x), tv.ROW_HEIGHT + 5)).name() == "#1f5fbf"
    assert strip.marker_at(x) == change
    x_fight = tl.marker_x(w.timeline.model.markers_of("competition")[0].index,
                          w.timeline.model.count, strip.width())
    assert QColor(image.pixel(int(x_fight), tv.ROW_HEIGHT + 5)).name() == "#e6007e"
    # A click at the marker seeks to its event; one far from any marker to the nearest event.
    QTest.mouseClick(strip, Qt.MouseButton.LeftButton, pos=QPoint(int(x), 5))
    assert w.position == change.index
    pos = QHelpEvent(QEvent.Type.ToolTip, QPoint(int(x), 5), strip.mapToGlobal(QPoint(int(x), 5)))
    strip.event(pos)
    assert strip.toolTip() == tl.marker_tooltip(change)
    at_end(w, qapp)


def test_a_bare_timeline_and_log_pane_before_any_run(qapp):
    from tabletop.gui.models.event_log import EventLog
    bar = tv.TimelineBar(EventLog())
    assert bar.text() == "no events" and not bar.slider.isEnabled()
    assert bar.pane_id == "P11" and "Numbo GUI" in bar.pages
    pane = lv.LogView()
    assert pane.trace.list.row_count() == 0 and pane.events_tab.list.row_count() == 0
    assert pane.trace_text() == "" and pane.paints == 0
    assert pane.minimumSize().toTuple() == lv.MIN_SIZE
    assert "Numbo GUI" in pane.pages and pane.pane_id == "P10"
