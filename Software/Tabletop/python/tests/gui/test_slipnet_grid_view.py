"""The Slipnet grid pane (item 07; `slipnet_grid_view.py`; G-030 to G-032), offscreen,
one shared window and one run of `surround-05` seed 3: the run through the window
equals the plain run and the grid's backing store equals a fresh render at sampled
events; the drawn boxes equal the layout and the pixels show the squares; a fresh view
fed the history draws the window's picture; seeking shows a jolt's outline at the jolt's
event only; a click selects the node in the shared selection; the French option is one
action on the bar and in the View menu, offered for `surround-05` only; the hover text;
the option is remembered in the settings; the pane keeps its small minimum."""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, QSettings, Qt  # noqa: E402
from PySide6.QtGui import QColor  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop import events as ev  # noqa: E402
from tabletop.gui import slipnet_grid_view as gv  # noqa: E402
from tabletop.gui.main_window import MainWindow  # noqa: E402
from tabletop.gui.models import slipnet_grid as sg  # noqa: E402
from test_table_view import EVERY, fresh, run_to_end, same_picture, shown  # noqa: E402

QUICK = ("surround-05", 3)
PROBES = {}


class GridProbe:
    """At every `EVERY`th `event_drawn`: what the window shows of the grid equals a
    fresh render of it (Numbo GUI, `test_polish.py`)."""

    def __init__(self, window):
        self.window = window
        self.widget = window.slipnet_grid_view.grid
        self.mismatches = []
        self.checked = []
        window.event_drawn.connect(self.drawn)

    def drawn(self, index):
        if index % EVERY == 0:
            self.checked.append(index)
            if not same_picture(shown(self.window, self.widget), fresh(self.widget)):
                self.mismatches.append(index)


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """The quick run through the shared window with the grid probe attached; the grid
    dock raised so that the grid is on screen."""
    w = module_window
    w.resize(1600, 1200)            # boxes big enough for the pixel probes (G-031; the Coderack group grew in item 09, G-037; the ten rows need the height, 1200 gives about 27 px)
    w.slipnet_grid_dock.raise_()
    qapp.processEvents()
    PROBES["grid"] = GridProbe(w)
    outcome = run_to_end(w, qapp, *QUICK)
    w.event_drawn.disconnect(PROBES["grid"].drawn)
    return outcome


def pixel(widget, x, y):
    return QColor(widget.grab().toImage().pixel(int(x), int(y)))


def left_edge_colours(widget, box):
    """The colours across the box's left edge at its middle height, 0 to 5 px in: an
    outline's 2 px pen lands somewhere in them."""
    image = widget.grab().toImage()
    y = int(box.y + box.side / 2)
    return {QColor(image.pixel(int(box.x) + dx, y)).name() for dx in range(0, 6)}


def centre(box):
    return (box.x + box.side / 2, box.y + box.side / 2)


def inside_square(box):
    """A point inside the black square, clear of the label and the number: just right
    of the square's left edge, at the box's middle height."""
    return (box.x + box.side / 2 - box.square / 2 + 2, box.y + box.side / 2)


def test_the_run_through_the_window_equals_the_plain_run_and_the_pixels_are_the_events(module_window, ran):
    ref = plain_run(*QUICK, V2)
    assert list(module_window.history.records) == list(ref.records)
    assert ran.result.draws == ref.draws
    probe = PROBES["grid"]
    assert probe.checked == list(range(0, len(module_window.history), EVERY))
    assert probe.mismatches == []


def test_the_drawn_boxes_equal_the_layout_and_the_squares_show(module_window, ran, qapp):
    w = module_window
    pane = w.slipnet_grid_view
    assert pane.pane_id == "P3" and pane.events == len(w.history)
    assert pane.history is w.history and pane.grid.selection is w.selection
    w.slipnet_grid_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    lay = pane.grid.drawn
    assert lay == pane.layout_now and len(lay.boxes) == 63
    assert (lay.width, lay.height) == (pane.grid.width(), pane.grid.height())
    opposite = lay.box("opposite")
    floor = min(lay.boxes, key=lambda b: b.activation)      # some node stays at the floor
    assert opposite.activation >= 140 and opposite.square > 0.8 * sg.INNER * lay.side
    assert floor.activation == 1.0 and floor.square == 0.0 and floor.number == "1"
    assert lay.side >= 24, "the window was resized for the probes"
    assert pixel(pane.grid, *inside_square(opposite)) == gv.SQUARE
    assert pixel(pane.grid, *centre(floor)) == gv.BACKGROUND
    assert '[F95 p. 95]' in pane.pages and pane.pages in pane.toolTip()
    assert sg.NOT_FRENCHS not in pane.pages and "is the GUI's" in pane.pages
    assert sg.NOT_FRENCHS in pane.label.text() and '[F95 p. 95]' in pane.label.text()


def test_a_fresh_view_fed_the_history_draws_the_windows_picture(module_window, ran, tmp_path):
    w = module_window
    live = w.slipnet_grid_view
    events = w.history.events
    jolt = next(i for i, e in enumerate(events) if isinstance(e, ev.NodeJolted) and e.clock > 5)
    for index in (0, jolt, 211, len(events) - 1):
        assert w.seek(index)
        other = gv.SlipnetGridView(pages=live.pages)
        other.resize(live.size())
        other.grid.resize(live.grid.size())
        for event in events[:index + 1]:
            other.on_event(event)
        assert other.model.activation == live.model.activation
        assert other.layout_now == live.layout_now
        a = live.render_png(tmp_path / f"live-{index}.png")
        b = other.render_png(tmp_path / f"fresh-{index}.png")
        assert a == b, index


def test_seeking_shows_a_jolts_outline_at_its_event_only(module_window, ran):
    w = module_window
    pane = w.slipnet_grid_view
    events = w.history.events
    jolt = next(i for i, e in enumerate(events) if isinstance(e, ev.NodeJolted) and e.clock > 5)
    assert w.seek(jolt)
    lay = pane.grid.drawn
    box = lay.box(events[jolt].node)
    assert box.jolted and [b.name for b in lay.boxes if b.jolted] == [box.name]
    assert gv.JOLT.name() in left_edge_colours(pane.grid, box)
    assert w.seek(jolt + 1)
    lay = pane.grid.drawn
    assert not lay.box(box.name).jolted
    assert gv.JOLT.name() not in left_edge_colours(pane.grid, box)
    assert w.seek(0)
    assert all(b.number == "1" for b in pane.grid.drawn.boxes)
    assert w.seek(len(events) - 1)
    assert pane.grid.drawn.box("opposite").activation >= 140


def test_a_click_selects_the_node_in_the_shared_selection(module_window, ran, qtbot, qapp):
    w = module_window
    pane = w.slipnet_grid_view
    w.slipnet_grid_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    cup = pane.grid.drawn.box("cup")
    before = pane.paints
    qtbot.mouseClick(pane.grid, Qt.MouseButton.LeftButton, pos=QPoint(*map(int, centre(cup))))
    assert (w.selection.kind, w.selection.name) == ("node", "cup")
    assert w.selection.members == frozenset({"cup"})
    assert pane.grid.drawn.box("cup").selected and pane.paints == before + 1
    assert gv.SELECTED.name() in left_edge_colours(pane.grid, cup)
    assert w.statusBar().currentMessage().startswith("selected cup: cup: activation ")
    qtbot.mouseClick(pane.grid, Qt.MouseButton.LeftButton, pos=QPoint(*map(int, centre(cup))))
    assert w.selection.empty and not pane.grid.drawn.box("cup").selected
    assert w.statusBar().currentMessage() == ""
    # Another pane's selection reaches the grid; a click on no box clears it.
    w.selection.select("object", "cup1")
    assert not any(b.selected for b in pane.grid.drawn.boxes)
    qtbot.mouseClick(pane.grid, Qt.MouseButton.LeftButton,
                     pos=QPoint(pane.grid.width() - 2, pane.grid.height() - 2))
    assert w.selection.empty


def test_frenchs_option_is_one_action_on_the_bar_and_in_the_menu(module_window, ran, qapp):
    w = module_window
    pane = w.slipnet_grid_view
    assert [a.text() for a in w.grid_menu.actions()] == ["French's ch. 4"]
    assert list(w.grid_menu.actions()) == [pane.french_action]
    assert pane.buttons["french"].defaultAction() is pane.french_action
    assert pane.buttons["french"].minimumWidth() >= 24
    assert sg.NOT_FRENCHS in pane.french_action.toolTip()
    assert pane.grid_state() == {"french": False} and pane.french_action.isEnabled()
    w.slipnet_grid_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    before = pane.paints
    pane.buttons["french"].click()
    assert pane.grid.french and pane.grid_state() == {"french": True} and pane.paints == before + 1
    lay = pane.grid.drawn
    assert lay.box("opposite").french == 150 and lay.box("cup").french == 130
    assert lay.note.startswith("red: French's Fig. 4.9 at 55 codelets, beside the run's at codelet 59")   # since v5's fix (D-116 to D-120): 59 codelets (Fig. 4.7 at 47 beside codelet 44 from item 08's runs to v2; Fig. 4.9 at 55 beside codelet 77 at item 07)
    assert sum(1 for b in lay.boxes if b.french is not None) == 41
    w.grid_menu.actions()[0].trigger()
    assert not pane.grid.french and pane.grid.drawn.note == ""


def test_frenchs_option_is_offered_for_surround_05_only(golden_runs, qtbot):
    pane = gv.SlipnetGridView(pages="x")
    qtbot.addWidget(pane)
    pane.set_grid_state({"french": True})
    assert not pane.french_action.isEnabled() and pane.french_action.isChecked()
    for event in golden_runs[("blockage-06", 2)].events:
        pane.on_event(event)
    assert not pane.french_action.isEnabled() and pane.model.problem == "blockage-06"
    pane.resize(300, 320)
    lay = pane.layout_now
    assert all(b.french is None for b in lay.boxes) and "apply to surround-05 only" in lay.note
    for event in golden_runs[("surround-05", 1)].events:
        pane.on_event(event)
    assert pane.french_action.isEnabled()
    assert sum(1 for b in pane.layout_now.boxes if b.french is not None) == 41
    assert pane.events == len(golden_runs[("surround-05", 1)].events) == 2690     # since v5's fix, D-116 to D-120 (3174 in v4, D-115; 1355 in v3, D-114; 901 since item 09, 1204 at item 08, 647 at item 07)


def test_the_hover_text_is_the_nodes(module_window, ran, qtbot, qapp):
    w = module_window
    pane = w.slipnet_grid_view
    w.slipnet_grid_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    lay = pane.grid.drawn
    cup = lay.box("cup")
    qtbot.mouseMove(pane.grid, QPoint(*map(int, centre(cup))))
    qapp.processEvents()
    text = pane.grid.toolTip()
    assert text == pane.model.tooltip("cup") and text.startswith("cup: activation ")
    assert "level 1, depth 20" in text and "effective lengths as of codelet 59" in text    # since v5's fix (D-116 to D-120): 59 (44 from item 08's runs to v2)
    isolated = lay.box("isolated")
    qtbot.mouseMove(pane.grid, QPoint(*map(int, centre(isolated))))
    qapp.processEvents()
    assert "seventh row: not on French's screen" in pane.grid.toolTip()
    qtbot.mouseMove(pane.grid, QPoint(pane.grid.width() - 2, pane.grid.height() - 2))
    qapp.processEvents()
    assert pane.grid.toolTip() == sg.EXTRA_CAPTION


def test_the_option_is_remembered_in_the_settings(qtbot, ini, thread_errors):
    def settings():
        return QSettings(ini, QSettings.Format.IniFormat)

    w = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w)
    w.slipnet_grid_view.set_grid_state({"french": True})
    w.close()
    assert not w.controller.is_alive()
    assert settings().value("controls/grid-french") in (True, "true")
    w2 = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w2)
    assert w2.slipnet_grid_view.grid_state() == {"french": True}
    assert not w2.slipnet_grid_view.french_action.isEnabled()
    w2.close()
    assert not w2.controller.is_alive()
    s = settings()
    s.setValue("controls/grid-french", "maybe")
    s.sync()
    w3 = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w3)
    assert w3.slipnet_grid_view.grid_state() == {"french": False}
    w3.close()
    assert not w3.controller.is_alive()


def test_the_pane_is_small_enough(module_window):
    pane = module_window.slipnet_grid_view
    assert (pane.minimumWidth(), pane.minimumHeight()) == gv.MIN_SIZE == (150, 150)
    assert pane.label.minimumWidth() <= 1
