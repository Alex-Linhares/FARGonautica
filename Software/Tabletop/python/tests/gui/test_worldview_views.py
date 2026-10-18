"""The Worldview and the competitions panes (item 10; `worldview_view.py`,
`competitions_view.py`; G-039 to G-041), offscreen, one shared window with the
competitions dock floated so that both panes are on screen, and one run of `surround-04`
seed 187 (two fights: one won by the newcomer, then one held by the Old Worldview; item
05 of the roles loop left `blockage-06` seed 2 one fight, item 07 re-pinned every run
fact, item 08 left `surround-04` seed 69 one competition without a fight, item 09 left
seed 22 one and v5's fix left seed 187 two fights of its three): the run through the
window equals the plain
run and both panes' backing stores equal fresh renders at sampled events; the drawn lines
are the models', with the solid bar's pixels on a Worldview entry and the warning colour
absent where no pair is incoherent; a fresh pane fed the history renders byte-identically
to the window's at sampled indexes; seeking shows the fight's lines tinted at its event
only; a click selects the correspondence in the shared selection, the table and the
status bar follow, and a click on a fight selects its newcomer; the hover texts; the
Proposals tab on a run with proposals; the labels say GUI design; the panes keep their
small minimums.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QEvent, QPoint, QPointF, Qt  # noqa: E402
from PySide6.QtGui import QColor, QMouseEvent  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop import events as ev  # noqa: E402
from tabletop.gui import competitions_view as cv  # noqa: E402
from tabletop.gui import worldview_view as wv  # noqa: E402
from tabletop.gui.models import worldview as wm  # noqa: E402
from test_coderack_view import Probe, qtbot_wait  # noqa: E402
from test_table_view import EVERY, run_to_end  # noqa: E402

FIGHTS = ("surround-04", 187)    # three competitions: cp3 enters alone, cp2 beats it, then cp3 comes back and is held off
PROPOSALS = ("surround-05", 67)  # three bottom-up group proposals, one (p1) past stage 0
# Item 05 (the roles loop) moved both: blockage-06 seed 2 holds one fight now and surround-05
# seed 6 proposes no group (the same constants as `test_worldview_model.py`'s). Item 07 (ROW
# and COLUMN roles, the re-cast) changed every run: surround-04 seed 69 kept its three fights
# with other correspondences at other codelets, and surround-05 seed 2 proposed three groups
# with none past stage 0, so PROPOSALS moved to seed 8. Item 08 (GAP, LONER, STRAY and CORNER
# roles; the midline on tables of loners) changed every run again: surround-04 seed 69 holds
# one competition and no fight, so FIGHTS moved to seed 22, and surround-05 seed 8 proposes
# no group, so PROPOSALS moved to seed 27. Item 09 (FIGURE, GROUND, ECHO, TWIN and TYPE_KEY
# roles) changed every run again: surround-04 seed 22 holds one competition and no fight, so
# FIGHTS moved to seed 187 (cp3 enters alone at codelet 59, holds cp7 off at 69 and cp2 at
# 70, and cp2 beats it at 80), and surround-05 seed 27 proposes no group, so PROPOSALS moved
# to seed 68. Since v5's fix (D-116 to D-120) every run changed again: surround-04 seed 187
# keeps a won fight, a held one and its proposal (cp3 enters alone at codelet 59, cp2 beats
# it at 71, and cp3 comes back at 76 and is held off), so FIGHTS stays; surround-05 seed 68
# proposes two groups, so PROPOSALS moved to seed 67 (the same constants as
# `test_worldview_model.py`'s).
PROBES = {}


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """The fights run through the shared window with both panes on screen (both docks
    floated, as the rack dock is in `test_coderack_view.py`) and probed."""
    w = module_window
    w.resize(1600, 1000)
    for dock, size, at in ((w.competitions_dock, (760, 320), (40, 40)),
                           (w.worldview_dock, (900, 420), (40, 400))):
        dock.setFloating(True)
        dock.resize(*size)
        dock.move(*at)
    qapp.processEvents()
    qtbot_wait(qapp)
    assert w.competitions_dock.isFloating() and w.worldview_dock.isFloating()
    assert not w.worldview_view.worldview.viewport().visibleRegion().isEmpty()
    assert not w.competitions_view.list.viewport().visibleRegion().isEmpty()
    PROBES["worldview"] = Probe(w, w.worldview_view.worldview.viewport(), top=w.worldview_dock)
    PROBES["competitions"] = Probe(w, w.competitions_view.list.viewport(), top=w.competitions_dock)
    outcome = run_to_end(w, qapp, *FIGHTS)
    for probe in PROBES.values():
        w.event_drawn.disconnect(probe.drawn)
    return outcome


def line_index(widget, predicate):
    """The index of the first drawn line the predicate accepts."""
    return next(k for k, line in enumerate(widget.lines) if predicate(line))


def line_colours(widget, k):
    """The colours along the k-th line's left edge, where an entry's bar is drawn."""
    image = widget.viewport().grab().toImage()
    y = int(widget.line_top(k) + widget.line / 2)
    return {QColor(image.pixel(x, y)).name() for x in range(2, 10)}


def background_of(widget, k):
    """The colour of the k-th line's right end, where only the tint is."""
    image = widget.viewport().grab().toImage()
    y = int(widget.line_top(k) + widget.line / 2)
    return QColor(image.pixel(widget.viewport().width() - 3, y)).name()


def fed_view(cls, history, upto, qapp, size=(760, 320), selection=None):
    """A bare pane of a given size, shown offscreen, fed the history up to an index,
    with the snapshot in force."""
    view = cls()
    view.attach(history, selection)
    view.resize(*size)
    view.show()
    qapp.processEvents()
    for i, event in enumerate(history.events[:upto + 1]):
        view.set_position(i)
        view.on_event(event)
    view.redraw()
    qtbot_wait(qapp)            # the scrollbar a long list grows narrows the viewport (G-058)
    return view


def test_the_run_through_the_window_equals_the_plain_run_and_the_pixels_are_the_events(module_window, ran):
    ref = plain_run(*FIGHTS, V2)
    assert list(module_window.history.records) == list(ref.records)
    assert ran.result.draws == ref.draws
    for name, probe in PROBES.items():
        assert probe.checked == list(range(0, len(module_window.history), EVERY)), name
        assert probe.mismatches == [], name
    assert module_window.worldview_view.events == module_window.competitions_view.events \
        == len(ref.events)


# -- P5 (G-039, G-040) --------------------------------------------------------------------------

def test_the_drawn_lines_are_the_models_and_the_bars_pixels(module_window, ran):
    w = module_window
    pane = w.worldview_view
    assert pane.pane_id == "P5" and "GUI design, not French's" in pane.pages
    assert pane.pages in pane.toolTip() and '[F95 pp. 67-74]' in pane.label.text()
    widget = pane.worldview
    assert widget.lines == tuple(pane.model.lines())
    assert widget.lines[0].text == "one correspondence; no pairs to rate"
    k = line_index(widget, lambda line: line.role == wm.WORLDVIEW)
    assert widget.lines[k].text.startswith("cp2: cup2 → cup1")
    assert wv.BAR.name() in line_colours(widget, k)
    shadow = line_index(widget, lambda line: line.role == wm.SHADOW)
    assert widget.lines[shadow].text.startswith("cp3: ")
    colours = line_colours(widget, shadow)
    assert wv.BAR.name() in colours and wv.BACKGROUND.name() in colours   # dashed
    assert not any(line.role == wm.WARNING for line in widget.lines)
    assert background_of(widget, k) == wv.BACKGROUND.name()
    assert pane.model.rating().as_of == 85


def test_a_fresh_pane_fed_the_history_draws_the_windows_picture(module_window, ran, tmp_path, qapp):
    w = module_window
    try:
        for index in (200, 813, 818, len(w.history) - 1):  # the second fight and its change
            assert w.seek(index)
            fresh = fed_view(wv.WorldviewView, w.history, index, qapp,
                             w.worldview_view.size().toTuple())
            assert fresh.worldview.lines == w.worldview_view.worldview.lines
            assert fresh.worldview.viewport().size() == w.worldview_view.worldview.viewport().size()
            assert fresh.render_png(tmp_path / "fresh.png") == \
                w.worldview_view.render_png(tmp_path / "shown.png"), index
            fresh.close()
            fights = fed_view(cv.CompetitionsView, w.history, index, qapp,
                              w.competitions_view.size().toTuple())
            assert fights.render_png(tmp_path / "fresh-c.png") == \
                w.competitions_view.render_png(tmp_path / "shown-c.png"), index
            fights.close()
    finally:
        w.seek(len(w.history) - 1)


def test_a_click_selects_the_correspondence_and_the_other_panes_follow(module_window, ran, qtbot):
    w = module_window
    w.seek(len(w.history) - 1)
    widget = w.worldview_view.worldview
    k = line_index(widget, lambda line: line.role == wm.SHADOW)
    name = widget.lines[k].name
    y = widget.line_top(k) + widget.line / 2
    qtbot.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(60, int(y)))
    assert (w.selection.kind, w.selection.name) == ("correspondence", name)
    assert w.statusBar().currentMessage().startswith(f"selected {name}: {name}:")
    assert background_of(widget, k) == wv.SELECTED_TINT.name()
    assert w.table_view.view.model.selection_of(name)[1] == name
    qtbot.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(60, int(y)))
    assert w.selection.empty and background_of(widget, k) == wv.BACKGROUND.name()
    # A selection made elsewhere reaches the pane.
    w.selection.select(*w.worldview_view.model.selection_of("cp2"))
    k1 = line_index(widget, lambda line: line.name == "cp2")
    assert background_of(widget, k1) == wv.SELECTED_TINT.name()
    # The empty space under the lines clears.
    qtbot.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton,
                     pos=QPoint(60, widget.viewport().height() - 3))
    assert w.selection.empty


def test_the_hover_texts(module_window, ran):
    module_window.seek(len(module_window.history) - 1)
    widget = module_window.worldview_view.worldview
    k = line_index(widget, lambda line: line.role == wm.SHADOW)
    y = widget.line_top(k) + widget.line / 2
    event = QMouseEvent(QEvent.Type.MouseMove, QPointF(30, y), QPointF(30, y),
                        Qt.MouseButton.NoButton, Qt.MouseButton.NoButton,
                        Qt.KeyboardModifier.NoModifier)
    widget.mouseMoveEvent(event)
    assert widget.viewport().toolTip().startswith("cp3: cup1 (Henry's end)")
    assert "rival of cp2 (as of codelet 85)" in widget.viewport().toolTip()
    event = QMouseEvent(QEvent.Type.MouseMove, QPointF(30, 2), QPointF(30, 2),
                        Qt.MouseButton.NoButton, Qt.MouseButton.NoButton,
                        Qt.KeyboardModifier.NoModifier)
    widget.mouseMoveEvent(event)
    assert widget.viewport().toolTip() == "hover a line for its structure"


def test_the_proposals_tab_on_a_run_with_proposals(module_window, ran, qapp):
    w = module_window
    shown = w.worldview_view.proposals.lines
    assert shown[0].text.startswith("1 proposal (D-024")
    assert [l.name for l in shown[1:]] == ["p1"]
    assert w.worldview_view.tabs.tabText(1) == "Proposals"
    log = plain_log(*PROPOSALS)
    view = fed_view(wv.WorldviewView, log, len(log.events) - 1, qapp)
    lines = view.proposals.lines
    assert lines[0].text.startswith("3 proposals") and [l.name for l in lines[1:]] == [
        "p1", "p2", "p3"]
    view.tabs.setCurrentIndex(1)
    qapp.processEvents()
    before = view.proposals.paints
    view.redraw(now=True)
    assert view.proposals.paints == before + 1
    view.close()


def plain_log(problem, seed):
    """A history of the run with snapshots, through the tap (G-006)."""
    from tabletop.config import PROFILES
    from tabletop.gui.models.event_log import EventLog
    from tabletop.gui.models.tap import Tap
    from tabletop.problems import load_problem
    from tabletop.world import World
    log = EventLog()
    tap = Tap(log)
    world = World(load_problem(problem), V2_PROFILES["standard"], seed, observers=[tap])
    tap.attach(world)
    while not world.finished:
        world.step()
        tap.after_step(world)
    return log


# -- P6 (G-041) -----------------------------------------------------------------------------------

def test_the_competitions_pane_lists_the_fights_and_tints_the_one_shown(module_window, ran):
    w = module_window
    w.seek(len(w.history) - 1)
    pane = w.competitions_view
    assert pane.pane_id == "P6" and "GUI design, not French's" in pane.pages
    assert pane.pages in pane.toolTip() and "Fig. 3.11" in pane.label.text()
    widget = pane.list
    assert widget.lines == tuple(pane.model.lines())
    assert widget.lines[0].text == "3 competitions: 2 fights, 1 entered without a fight"
    held = [i for i, e in enumerate(w.history.events) if isinstance(e, ev.CompetitionHeld)]
    assert len(held) == 3
    cells = line_index(widget, lambda line: line.cells and line.cells[1][1] == wm.SHADOW)
    assert widget.lines[cells].cells[0][1] == wm.WORLDVIEW
    assert background_of(widget, cells) == wv.BACKGROUND.name()
    assert w.seek(held[-1])
    k = line_index(widget, lambda line: line.fresh)
    assert widget.lines[k].text.startswith("3. codelet 76: cp3 against cp2")   # the Old Worldview holds
    assert background_of(widget, k) == wv.FRESH_TINT.name()
    assert w.seek(held[-1] + 1)
    assert not any(line.fresh for line in widget.lines)
    assert background_of(widget, k) == wv.BACKGROUND.name()
    w.seek(len(w.history) - 1)


def test_a_click_on_a_fight_selects_its_newcomer(module_window, ran, qtbot):
    w = module_window
    w.seek(len(w.history) - 1)
    widget = w.competitions_view.list
    k = line_index(widget, lambda line: line.text.startswith("2. codelet 71"))   # cp2 wins
    y = widget.line_top(k) + widget.line / 2
    qtbot.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(60, int(y)))
    assert (w.selection.kind, w.selection.name) == ("correspondence", "cp2")
    assert background_of(widget, k) == wv.SELECTED_TINT.name()
    assert background_of(w.worldview_view.worldview,
                         line_index(w.worldview_view.worldview, lambda l: l.name == "cp2")) \
        == wv.SELECTED_TINT.name()
    qtbot.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(60, 2))
    assert w.selection.empty
    event = QMouseEvent(QEvent.Type.MouseMove, QPointF(30, y), QPointF(30, y),
                        Qt.MouseButton.NoButton, Qt.MouseButton.NoButton,
                        Qt.KeyboardModifier.NoModifier)
    widget.mouseMoveEvent(event)
    assert widget.viewport().toolTip().startswith("competition at codelet 71: cp2 against cp3")


def test_the_empty_banners_before_a_run(qtbot):
    pane = cv.CompetitionsView()
    pane.redraw()
    assert pane.list.lines == (wm.TextLine("no run yet", wm.MUTED),)
    view = wv.WorldviewView()
    view.redraw()
    assert view.worldview.lines[0].text == "no run yet"
    view.on_event(plain_run(*FIGHTS, V2).events[0])
    pane.on_event(plain_run(*FIGHTS, V2).events[0])
    view.redraw()
    pane.redraw()
    assert pane.list.lines == (wm.TextLine("no competition in this run", wm.MUTED),)
    assert view.worldview.lines[0].text == "no correspondence in the Worldview yet"


def test_the_panes_are_small_enough_for_the_smallest_window(module_window):
    for pane in (module_window.worldview_view, module_window.competitions_view):
        assert pane.minimumSize().width() <= 200 and pane.minimumSize().height() <= 70
        assert pane.minimumSizeHint().width() <= 260
