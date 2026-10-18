"""The table view (items 03 and 04; PANES.md P1; G-016 to G-023), offscreen: after
`surround-05` seed 3 through the window the scene's items equal the layout, the E arrow
is on `spoon3` (since v5's fix, D-116 to D-120; `cup2` before) and the H on `cup1`, the tooltips are the model's; the group and
correspondence items equal the layout and keep the end state after `RunEnded`; the pixel
probe (Numbo GUI, `test_polish.py`): at eleven sampled events what the window shows of
the table equals a fresh render of it, and a fresh view fed the same events renders the
same picture; an offscreen render (`render.py`) of the end state; seeking back and forward;
a click selects in the shared selection; zoom, fit and 1:1; `object_at`, `element_at`
and the hover text; the proposal items, on a fresh view fed `surround-05` seed 12 (three
proposals, none built, two past their first stage; the quick run proposes one group,
never past stage 0); the legend;
the pixmaps of every type and orientation; a fresh view draws the table from `set_problem`
before any event; the canvas is small enough for 1280 x 800.

The run facts are v5's (the structural Blockage fix, D-116 to D-120): 59 codelets, 720
events, the answer spoon3 (none: an object French prints no bar for), T 43.6; re-derived
from the engine as `test_table_model.py` and `test_overlays_view.py` did for the same run.
The run now holds a won fight (cp2, spoon3=cup1, beats cp1, cup2=cup1, at codelet 40) and
one group proposal (p1, fork1 and fork2, at codelet 21). Item 08's (roles loop0007) were 44
codelets, 507 events, the answer cup2 (E2), T 41.0, re-pinned from the engine like items
03b, 04b, 05 and 07 did; item 09's runs left seed 3 and the proposals run, seed 12,
unchanged. Seed 12 still proposes groups and builds none on v5 (seven proposals, p3 at
stage 2 and p4 at stage 1), so it stays."""

import time

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, Qt  # noqa: E402
from PySide6.QtGui import QColor, QImage  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop import events as ev  # noqa: E402
from tabletop.config import PROFILES  # noqa: E402
from tabletop.gui import icons, table_view as tv  # noqa: E402
from tabletop.gui.models import icon_shapes  # noqa: E402
from tabletop.gui.models.selection import Selection  # noqa: E402
from tabletop.gui.models import structures as st  # noqa: E402
from tabletop.objects import TYPES  # noqa: E402
from tabletop.problems import load_problem  # noqa: E402

QUICK = ("surround-05", 3)       # Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6.
                                 # (v5; item 08's: cup2 (E2), 75, 44, T 41.0)
PROPOSING = ("surround-05", 12)  # seven group proposals on v5, none built, p3 at stage 2 and p4
                                 # at stage 1 (three, p3 past stage 0, since item 08's runs,
                                 # unchanged by item 09's; seed 8, item 07's
                                 # seed, proposed nothing at item 08 and since item 09 one
                                 # group, p1 at codelet 53, never past stage 0; seed 2, the
                                 # seed before item 07, and seed 6, the seed before item 05,
                                 # propose nothing either)
TIMEOUT = 30.0
EVERY = 50                       # the pixel probe's sampling: 720 events give fifteen samples (v5)
PROBES = {}                      # the module run's pixel probe, for the tests that read it


def run_to_end(window, qapp, problem, seed):
    """Run the problem to the end through the window, turning the event loop.

    `TIMEOUT` bounds a stall, not the whole run: the run fails when no event has been
    delivered for `TIMEOUT` seconds (a hung handshake or worker), not when it is merely
    slow. Every event costs two thread wake-ups (the delivery to the GUI thread, the
    acknowledgement back to the worker), and on an oversubscribed machine each wake-up
    waits for a core: measured, the 952 events of `surround-04` seed 187 take 4.5 s on an
    idle machine and 21 s with 40 busy processes added on 32 cores, and the loop's gate
    saw them pass 30 s; the stall deadline keeps the backstop without a budget for the
    machine's load (the suite's own budget bounds the time)."""
    done = []
    window.run_finished.connect(done.append)
    try:
        window.start_run(problem, V2_PROFILES["standard"], seed)
        window.controller.run_to_end()
        last = None
        deadline = time.monotonic() + TIMEOUT
        while not done and time.monotonic() < deadline:
            qapp.processEvents()
            if window.position != last:
                last, deadline = window.position, time.monotonic() + TIMEOUT
    finally:
        window.run_finished.disconnect(done.append)
    assert done and done[0].error is None, "no event for TIMEOUT seconds: a stalled run"
    assert window.controller.wait(TIMEOUT), "the worker ends right after its outcome"
    assert not window.controller.is_alive()
    return done[0]


# -- the pixel probe (Numbo GUI, test_polish.py) ----------------------------------------------------

def shown(window, widget):
    """What the window's backing store holds for the visible part of the widget (what
    was painted, not a fresh render)."""
    r = widget.visibleRegion().boundingRect()
    top = widget.mapTo(window, r.topLeft())
    img = window.screen().grabWindow(window.winId(), top.x(), top.y(), r.width(),
                                     r.height()).toImage()
    return img.convertToFormat(QImage.Format.Format_RGB32)


def fresh(widget):
    """A fresh render of the visible part of the widget."""
    r = widget.visibleRegion().boundingRect()
    return widget.grab(r).toImage().convertToFormat(QImage.Format.Format_RGB32)


def same_picture(a, b, tolerance=3):
    """Equal, or no channel of any pixel more than `tolerance` apart: an antialiased
    edge rounds differently in the window's backing store than in a grab. One step
    until item 14 of the roles loop; three since, measured at the window geometry the
    fifth overlay button and the roles dock give (the table's viewport 238 x 478 instead
    of 214 x 383): one pixel at an object's box corner in Eliza's view, `surround-05`
    seed 3 at every sampled event, 3 apart with every overlay on, the same with every
    layer off but Eliza's view, with the roles item repainting whole and with
    `FullViewportUpdate` forced, so a device rounding, not a stale repaint (a stale
    pixel differs by tens; G-061)."""
    if a == b:
        return True
    if a.size() != b.size():
        return False
    da = bytes(a.constBits())[:a.sizeInBytes()]
    db = bytes(b.constBits())[:b.sizeInBytes()]
    return all(abs(x - y) <= tolerance for x, y in zip(da, db))


class PixelProbe:
    """At every `EVERY`th `event_drawn`: what the window shows of the table equals a
    fresh render of it (Numbo GUI, `test_polish.py`)."""

    def __init__(self, window):
        self.window = window
        self.viewport = window.table_view.view.viewport()
        self.mismatches = []
        self.checked = []
        window.event_drawn.connect(self.drawn)

    def drawn(self, index):
        if index % EVERY == 0:
            self.checked.append(index)
            if not same_picture(shown(self.window, self.viewport), fresh(self.viewport)):
                self.mismatches.append(index)


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """The quick run through the shared window, once per module (ANSWERS.md 10), with
    the pixel probe attached."""
    PROBES["quick"] = PixelProbe(module_window)
    outcome = run_to_end(module_window, qapp, *QUICK)
    assert module_window.statusBar().currentMessage() == \
        "Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6."   # v5 (item 08: cup2 (E2), 75, 44, T 41.0)
    module_window.event_drawn.disconnect(PROBES["quick"].drawn)
    return outcome


def the_view(window):
    return window.table_view.view


def test_the_scene_equals_the_layout_at_the_end_of_the_run(module_window, ran):
    view = the_view(module_window)
    lay = view.layout
    problem = load_problem(QUICK[0])
    assert set(view.object_items) == set(problem.ids()) == {o.id for o in lay.objects}
    for obj in lay.objects:
        item = view.object_items[obj.id]
        assert item.obj == obj
        assert (item.pos().x(), item.pos().y()) == (obj.x * tv.SCALE, obj.y * tv.SCALE)
        assert item.toolTip() == view.model.tooltip(obj.id) != ""
    assert set(view.arrow_items) == {"H", "E"}
    assert view.arrow_items["E"].arrow == lay.answer and lay.answer.object == "spoon3"   # cup2 before v5
    assert view.arrow_items["H"].arrow == lay.touch and lay.touch.object == "cup1"
    assert view.arrow_items["E"].toolTip() == "Eliza's answer (E)"
    assert view.model.finished and module_window.table_view.events == len(module_window.history)
    assert sum(o.noticed for o in lay.objects) == len(
        {e.object for e in module_window.history.events if isinstance(e, ev.ObjectNoticed)})


def test_the_structures_equal_the_layout_at_the_end_of_the_run(module_window, ran):
    view = the_view(module_window)
    lay = view.layout
    # Since v5's fix (D-116 to D-120): two gestalt groups (g1 at codelet 6, g2 at codelet
    # 13), one group proposal (p1, fork1 and fork2, codelet 21, never past stage 0), and
    # three correspondences built: cp1 (cup2=cup1, codelet 9) passed its test and entered
    # the Worldview without a rival at codelet 18; cp2 (spoon3=cup1, codelet 30) passed its
    # test and beat cp1 at codelet 40, so cp1 stands as the loser; cp3 (g2=fork1, codelet
    # 56) failed its test, was removed at once and has faded by the end (59 - 56 ==
    # FADE_CODELETS 3). Item 08's run had no proposal and one line, cp1, in the Worldview
    # (its cp2, cup2=g1, failed at codelet 36); item 07's had three gestalt groups, two
    # proposals at stage 0 and eight correspondences. Nothing fades or flashes at the end.
    assert set(view.group_items) == {g.name for g in lay.groups} == {"g1", "g2"}
    assert set(view.proposal_items) == {p.name for p in lay.proposals} == {"p1"}
    assert set(view.line_items) == {c.name for c in lay.correspondences} == {"cp1", "cp2"}
    for name, item in list(view.group_items.items()) + list(view.line_items.items()) \
            + list(view.proposal_items.items()):
        assert item.form == lay.structure(name) and item.name == name
        assert item.toolTip() == view.model.tooltip(name) != ""
        assert not item.selected
    cp2 = view.line_items["cp2"].form
    assert cp2.in_worldview and not st.line_style(cp2).dashed and not cp2.fading
    assert st.line_style(cp2).alpha == 255 and cp2.loser is False
    assert not cp2.new                    # built at codelet 30, long before the end
    assert "in the Worldview" in view.line_items["cp2"].toolTip()
    cp1 = view.line_items["cp1"].form     # the loser of codelet 40's fight
    assert not cp1.in_worldview and cp1.loser and st.line_style(cp1).dashed
    assert not cp1.new and not cp1.fading and st.line_style(cp1).alpha < 255
    (p1,) = lay.proposals
    assert p1.name == "p1" and p1.members == ("fork1", "fork2") and p1.stage == 0
    assert not p1.failed and st.box_style(p1).border == "dotted"
    assert all(item.form.wavy for item in view.group_items.values())
    assert view.group_items["g1"].zValue() < view.object_items["cup2"].zValue() \
        < view.line_items["cp2"].zValue() < view.arrow_items["E"].zValue()
    assert view.model.finished, "the end state stays drawn after RunEnded"


def test_the_pixels_are_the_events(module_window, ran, tmp_path):
    """Numbo's probe during the run: at fifteen sampled events (720 events, every 50th,
    since v5's fix; eleven of 507 at item 08, eighteen of 856 at item 07) the backing
    store equals a fresh
    render; and after it, at the same events, a fresh view fed the history draws the same
    picture as the window's view seeked there (ANSWERS.md 5)."""
    probe = PROBES["quick"]
    assert probe.checked == list(range(0, len(module_window.history), EVERY))
    assert len(probe.checked) == 15 and len(module_window.history) == 720   # 11 and 507 at item 08
    assert probe.mismatches == []
    w = module_window
    live = the_view(w)
    for index in probe.checked:
        assert w.seek(index)
        other = tv.TableView(w.history)
        other.set_problem(load_problem(QUICK[0]))
        for i, event in enumerate(w.history.events[:index + 1]):
            other.set_position(i)
            other.on_event(event)
        other.redraw(now=False)
        assert other.layout == live.layout and other.layout.phase == index % 2
        a = live.render_png(tmp_path / f"live-{index}.png")
        b = other.render_png(tmp_path / f"fresh-{index}.png")
        assert a == b, index
    assert w.seek(len(w.history) - 1)


def test_a_click_selects_in_the_shared_selection(module_window, ran, qtbot):
    w = module_window
    view = the_view(w)
    view.fit_to_view()
    heard = []
    view.selected.connect(heard.append)
    try:
        cup2 = view.layout.object("cup2")
        at_cup2 = view.mapFromScene(cup2.x * tv.SCALE, cup2.y * tv.SCALE)
        assert view.element_at(at_cup2) == "cup2"
        qtbot.mouseClick(view.viewport(), Qt.MouseButton.LeftButton, pos=at_cup2)
        assert (w.selection.kind, w.selection.name) == ("object", "cup2")
        assert view.object_items["cup2"].selected
        assert w.statusBar().currentMessage() == "selected cup2: cup2: cup"
        qtbot.mouseClick(view.viewport(), Qt.MouseButton.LeftButton, pos=at_cup2)
        assert w.selection.empty and not view.object_items["cup2"].selected
        assert w.statusBar().currentMessage() == ""
        # The Worldview line (cp2, spoon3 to cup1, since v5's fix; cp1, cup2 to cup1, at
        # item 08), at a point a third of the way along it from its upper end, cup1. At the
        # end of the run two lines stand, cp2 and the loser cp1, which share only cup1 and
        # part far from it (cp3, g2=fork1, failed its test at codelet 56 and has faded), so
        # the click is made at the end, with no seeking. (Item 07's Worldview line, cp2,
        # was covered at the end by two later-added lines sharing Z_LINE, so that run's
        # click was made at the event before cp5 and the window seeked back afterwards.)
        events = w.history.events
        assert w.seek(len(events) - 1) and set(view.line_items) == {"cp1", "cp2"}
        cp2 = view.layout.structure("cp2")
        (x1, y1), (x2, y2) = (cp2.a, cp2.b) if cp2.a[1] < cp2.b[1] else (cp2.b, cp2.a)
        assert (x1, y1) == (30.3, 15.3) and (x2, y2) == (13.9, 74.7)   # cup1 above, spoon3 below
        point = view.mapFromScene((x1 + (x2 - x1) / 3) * tv.SCALE, (y1 + (y2 - y1) / 3) * tv.SCALE)
        assert view.element_at(point) == "cp2"
        qtbot.mouseClick(view.viewport(), Qt.MouseButton.LeftButton, pos=point)
        assert (w.selection.kind, w.selection.name) == ("correspondence", "cp2")
        assert w.selection.members == {"cp2", "cup1", "spoon3"}
        assert view.line_items["cp2"].selected and view.object_items["cup1"].selected
        assert view.object_items["spoon3"].selected and not view.object_items["fork1"].selected
        assert not view.object_items["cup2"].selected and not view.line_items["cp1"].selected
        assert not view.object_items["small-glass1"].selected
        assert view.element_at(point) == "cp2", "uncovered at the end: see above"
        # A gestalt group's border band: g1's right edge, beside spoon2. (At item 07 its top
        # edge was covered by a third gestalt group, g3, lying inside it; since item 08's
        # runs there are two gestalt groups, g1 in Henry's half and g2 in Eliza's, and
        # neither lies inside the other.)
        g1, g2 = view.layout.structure("g1"), view.layout.structure("g2")
        left, top, right, bottom = g1.box
        assert bottom < g2.box[1] and not set(g2.members) & set(g1.members)
        edge = view.mapFromScene(right * tv.SCALE, (top + bottom) / 2 * tv.SCALE)
        assert view.element_at(edge) == "g1"
        qtbot.mouseClick(view.viewport(), Qt.MouseButton.LeftButton, pos=edge)
        assert (w.selection.kind, w.selection.name) == ("group", "g1")
        assert len(w.selection.members) == 7 and view.group_items["g1"].selected
        assert view.object_items["spoon1"].selected and not view.object_items["spoon3"].selected
        assert not view.line_items["cp2"].selected
        # Empty table clears (the top right corner, away from every line and box).
        empty = view.mapFromScene(95 * tv.SCALE, 15 * tv.SCALE)
        assert view.element_at(empty) is None
        qtbot.mouseClick(view.viewport(), Qt.MouseButton.LeftButton, pos=empty)
        assert w.selection.empty and not view.group_items["g1"].selected
        assert heard == ["cup2", "", "cp2", "g1", ""]
        # Another pane's selection reaches the table (cp2, spoon3=cup1, the Worldview line
        # since v5's fix; cp1, cup2=cup1, the run's only line at item 08; at item 07 this
        # was cp4, standing outside the Worldview, and before that cp2, the loser line).
        w.selection.select("correspondence", "cp2", ("spoon3", "cup1"))
        assert view.line_items["cp2"].selected and view.object_items["spoon3"].selected
        assert not view.group_items["g1"].selected and not view.object_items["cup2"].selected
        assert not view.line_items["cp1"].selected
        w.selection.clear()
    finally:
        view.selected.disconnect(heard.append)


def test_seeking_shows_the_structures_as_they_were(module_window, ran):
    w = module_window
    view = the_view(w)
    events = w.history.events
    # v5's run facts (item 08's built two correspondences, at events 143 and 416; item
    # 07's eight, 277 to 836). The run's first 300 events are item 08's.
    built = [i for i, e in enumerate(events) if isinstance(e, ev.CorrespondenceBuilt)]
    assert len(built) == 3 and built == [143, 371, 684]
    assert w.seek(0)
    assert view.group_items == {} and view.line_items == {} and view.proposal_items == {}
    assert w.seek(150)                            # between g1 (codelet 6, event 93) and g2
    assert set(view.group_items) == {"g1"}        # (codelet 13, event 192)
    assert set(view.line_items) == {"cp1"} and view.line_items["cp1"].form.new   # cp1 at
    assert view.proposal_items == {}              # codelet 9 (event 143) still flashes at 150
    assert w.seek(built[0])                       # cp1 at codelet 9: one gestalt group built
    assert set(view.line_items) == {"cp1"} and view.line_items["cp1"].form.new
    assert set(view.group_items) == {"g1"}        # (g2 comes at codelet 13)
    assert not view.line_items["cp1"].form.in_worldview   # (the Worldview at codelet 18)
    assert w.seek(158)                            # the first event of codelet 11: cp1, two
    assert not view.line_items["cp1"].form.new    # codelets old, flashes no more (NEW_CODELETS 2)
    assert w.seek(308)                            # p1 (fork1, fork2) proposed at codelet 21
    assert set(view.proposal_items) == {"p1"} and view.proposal_items["p1"].form.stage == 0
    assert w.seek(built[1])                       # cp2 (spoon3=cup1) at codelet 30 flashes;
    assert set(view.line_items) == {"cp1", "cp2"}     # cp1 is the Worldview (codelet 18)
    assert view.line_items["cp2"].form.new and not view.line_items["cp2"].form.fading
    assert not view.line_items["cp1"].form.new and view.line_items["cp1"].form.in_worldview
    assert set(view.group_items) == {"g1", "g2"} and set(view.proposal_items) == {"p1"}
    assert w.seek(493)                            # codelet 40: cp2 beats cp1 and enters
    assert view.line_items["cp2"].form.in_worldview and view.line_items["cp1"].form.loser
    assert w.seek(built[2])                       # cp3 (g2=fork1) at codelet 56 flashes, not
    assert view.line_items["cp3"].form.new        # yet tested
    assert w.seek(built[2] + 2)                   # cp3 failed its test and was removed at
    assert view.line_items["cp3"].form.fading     # event 686, codelet 56: fading at once
    assert w.seek(712)                            # the last event of codelet 58: still fading
    assert set(view.line_items) == {"cp1", "cp2", "cp3"} and view.line_items["cp3"].form.fading
    assert w.seek(713)                            # the first event of codelet 59: faded
    assert set(view.line_items) == {"cp1", "cp2"}     # (59 - 56 == FADE_CODELETS 3)
    assert w.seek(len(events) - 1)
    assert set(view.line_items) == {"cp1", "cp2"}     # cp3 (removed at 56) has faded
    assert set(view.proposal_items) == {"p1"} and view.proposal_items["p1"].form.stage == 0
    assert not view.line_items["cp2"].form.new and not view.line_items["cp2"].form.fading
    assert view.line_items["cp2"].form.in_worldview and view.line_items["cp1"].form.loser
    assert set(view.group_items) == {"g1", "g2"}


def test_an_offscreen_render_of_the_end_state(module_window, ran, tmp_path):
    view = the_view(module_window)
    path = tmp_path / "table.png"
    image = view.render_png(path)
    assert path.read_bytes()[:8] == b"\x89PNG\r\n\x1a\n"
    rect = view.sceneRect()
    assert (image.width(), image.height()) == (round(rect.width()), round(rect.height()))
    assert rect.width() == 110 * tv.SCALE + 2 * tv.MARGIN

    def pixel(scene_x, scene_y):
        return QColor(image.pixel(round(scene_x - rect.left()), round(scene_y - rect.top())))

    # The table's fill in an empty part of the table, the background outside it.
    assert pixel(55 * tv.SCALE, 35 * tv.SCALE).name() == tv.TABLE_FILL
    assert pixel(rect.left() + 2, rect.top() + 2).name() == tv.BACKGROUND
    # The E arrow's shaft is dark at its middle.
    arrow = view.layout.answer
    mid_x = (arrow.tail[0] + arrow.head[0]) / 2 * tv.SCALE
    mid_y = (arrow.tail[1] + arrow.head[1]) / 2 * tv.SCALE
    assert QColor(pixel(mid_x, mid_y)).lightness() < 120
    # The answer's icon (spoon3 since v5's fix, cup2 before) is darker than the table
    # inside its box.
    answer = view.layout.object(view.layout.answer.object)
    left, top, right, bottom = answer.box
    dark = sum(QColor(pixel(x * tv.SCALE, y * tv.SCALE)).lightness() < 160
               for x in (left + 0.5, answer.x, right - 0.5)
               for y in (top + 0.5, answer.y, bottom - 0.5))
    assert dark >= 1


def test_seeking_back_removes_the_e_arrow_and_forward_restores_it(module_window, ran):
    w = module_window
    view = the_view(w)
    events = w.history.events
    answered = next(i for i, e in enumerate(events) if isinstance(e, ev.AnswerGiven))
    assert w.seek(0)
    assert set(view.arrow_items) == {"H"} and view.layout.answer is None
    assert not any(o.noticed for o in view.layout.objects)
    assert len(view.object_items) == 13
    assert w.seek(answered - 1)
    assert "E" not in view.arrow_items
    assert w.seek(answered)                       # event 718 of 720 since v5 (505 of 507 at item 08)
    assert answered == 718 and view.arrow_items["E"].arrow.object == "spoon3"
    assert w.seek(len(events) - 1)
    assert view.arrow_items["E"].arrow.object == "spoon3" and view.model.finished


def test_zoom_fit_and_one_to_one(module_window, ran, qtbot):
    w = module_window
    view = the_view(w)
    canvas = w.table_view
    canvas.fit_button.click()
    assert view.auto_fit
    fitted = view.scale_factor()
    assert 0 < fitted <= tv.MAX_FIT_SCALE
    canvas.zoom_in_button.click()
    assert not view.auto_fit
    assert view.scale_factor() == pytest.approx(fitted * tv.ZOOM_STEP)
    canvas.zoom_out_button.click()
    assert view.scale_factor() == pytest.approx(fitted)
    canvas.reset_zoom_button.click()
    assert view.scale_factor() == pytest.approx(1.0) and not view.auto_fit
    qtbot.keyClick(view, Qt.Key.Key_Plus)
    assert view.scale_factor() == pytest.approx(tv.ZOOM_STEP)
    qtbot.keyClick(view, Qt.Key.Key_0)
    assert view.scale_factor() == pytest.approx(1.0)
    qtbot.keyClick(view, Qt.Key.Key_F)
    assert view.auto_fit and view.scale_factor() == pytest.approx(fitted)
    assert view.horizontalScrollBarPolicy() == Qt.ScrollBarPolicy.ScrollBarAlwaysOff
    qtbot.keyClick(view, Qt.Key.Key_Minus)
    assert view.horizontalScrollBarPolicy() == Qt.ScrollBarPolicy.ScrollBarAsNeeded
    view.fit_to_view()


def test_the_object_under_a_point_and_its_hover_text(module_window, ran):
    view = the_view(module_window)
    view.fit_to_view()
    spoon3 = view.layout.object("spoon3")   # the answer since v5 (none: the empty label)
    point = view.mapFromScene(spoon3.x * tv.SCALE, spoon3.y * tv.SCALE)
    assert view.object_at(point) == "spoon3"
    text = view.object_items["spoon3"].toolTip()
    assert text.startswith("spoon3: spoon pointing up\n") and text.endswith("Eliza's answer (E, )")
    assert "Eliza's half" in text and "descriptors: spoon, opposite" in text
    cup2 = view.layout.object("cup2")                   # item 08's answer (E2), now unmarked
    at_cup2 = view.mapFromScene(cup2.x * tv.SCALE, cup2.y * tv.SCALE)
    assert view.object_at(at_cup2) == "cup2"
    assert "Eliza's answer" not in view.object_items["cup2"].toolTip()
    assert "descriptors: cup, opposite, isolated" in view.object_items["cup2"].toolTip()
    glass = view.layout.object("small-glass1")          # item 07's answer, now unmarked
    at_glass = view.mapFromScene(glass.x * tv.SCALE, glass.y * tv.SCALE)
    assert view.object_at(at_glass) == "small-glass1"
    assert "Eliza's answer" not in view.object_items["small-glass1"].toolTip()
    empty = view.mapFromScene(55 * tv.SCALE, 35 * tv.SCALE)
    assert view.object_at(empty) is None
    assert view.object_at(QPoint(-50, -50)) is None


def test_the_proposal_items_equal_the_layout_and_take_the_shared_selection(module_window, ran):
    """The quick run proposes one group that never leaves stage 0 since v5's fix (none
    since item 08's runs; item 07's proposed two that never left stage 0), so the proposal
    items (G-020, G-021) are checked on a fresh view fed `surround-05` seed 12 (seven
    proposals on v5, none built, `p3` past its first two stages at codelets 43 and 47 and
    `p4` past its first at codelet 70; three, `p3` the same, from item 08's runs to item
    09's; since item 08: seed 8, item 07's
    seed, proposed nothing at item 08 and one group since item 09, never past stage 0;
    before item 07 this was seed 2 with nine, and before item 05 seed 6 with seven) with
    a shared `Selection` of its own (the window's names the selection in the status bar
    by its own run's hover text): the items equal the layout, their tooltips are the
    model's, and another pane's selection of a proposal reaches them."""
    w = module_window
    shared = Selection()
    other = tv.TableView(selection=shared)
    other.set_problem(load_problem(PROPOSING[0]))
    ref = plain_run(*PROPOSING, V2)
    assert ref.report == "Eliza touches cup2 (E2). Structure 95. Codelets 72. T 37.6."   # v5
    # (item 08's and 09's: small-glass1 (E1), Structure 86, Codelets 76, T 36.8)
    for i, event in enumerate(ref.events):
        other.set_position(i)
        other.on_event(event)
    other.redraw(now=False)
    lay = other.layout
    assert set(other.proposal_items) == {p.name for p in lay.proposals} == \
        {"p1", "p2", "p3", "p4", "p5", "p6", "p7"}
    assert set(other.group_items) == {"g1", "g2"} and set(other.line_items) == \
        {"cp1", "cp4"}   # the failed three (cp2, cp3, cp5; codelets 25, 54 and 65) faded;
    # cp1 (cup2=cup1) is the Worldview since codelet 46 and held off cp4 (small-glass1=
    # cup1) at codelet 69, which stands as the loser. (Item 09's: cp3, small-glass1=cup1,
    # beat cp1 and cp4, cup1=spoon3, stood outside.)
    assert other.line_items["cp1"].form.in_worldview and other.line_items["cp4"].form.loser
    assert not other.line_items["cp4"].form.in_worldview and other.line_items["cp1"].form.loser is False
    for name, item in other.proposal_items.items():
        assert item.form == lay.structure(name) and item.name == name
        assert item.toolTip() == other.model.tooltip(name) != ""
        assert st.box_style(item.form).border == "dotted" and not item.selected
    p1, p2, p3, p4 = (other.proposal_items[n].form for n in ("p1", "p2", "p3", "p4"))
    assert p1.members == ("spoon1", "spoon2") and p1.stage == 0 and not p1.failed   # codelet 28
    assert p2.members == ("fork1", "fork2") and p2.stage == 0 and not p2.failed     # codelet 33
    assert p3.members == ("spoon3", "spoon4") and p3.stage == 2 and not p3.failed   # codelet 36
    assert p4.members == ("knife1", "cup1") and p4.stage == 1 and not p4.failed     # codelet 53
    assert sum(p.stage for p in lay.proposals) == 3 and not any(p.failed for p in lay.proposals)
    assert other.proposal_items["p1"].zValue() < other.object_items["spoon1"].zValue()
    quick = the_view(w)                   # the quick run's one proposal, p1 (fork1, fork2)
    assert set(quick.proposal_items) == {"p1"} and len(quick.layout.proposals) == 1
    # Another pane's selection reaches the table.
    shared.select("proposal", "p1", ("spoon1", "spoon2"))
    assert other.proposal_items["p1"].selected and other.object_items["spoon1"].selected
    assert not other.proposal_items["p3"].selected and not other.object_items["spoon3"].selected
    assert other.model.selection_of("p1") == ("proposal", "p1", ("spoon1", "spoon2"))
    shared.clear()
    assert not other.proposal_items["p1"].selected and not other.object_items["spoon1"].selected
    assert w.selection.empty and not any(i.selected for i in quick.proposal_items.values())


def test_the_legend_says_the_icons_are_new_drawings(module_window):
    canvas = module_window.table_view
    assert canvas.legend.text().startswith("icons: new drawings, not French's")
    assert "[F95 pp. 96-107 (img)]" in canvas.legend.text()
    assert tv.legend_text() == canvas.legend.text()
    assert "icons: new drawings, not French's" in canvas.toolTip()
    assert "[F95 p. 26 (img)]" in the_view(module_window).toolTip()
    assert canvas.pane_id == "P1" and canvas.title == "Table"


def test_a_pixmap_for_every_type_and_orientation(qapp):
    for type_ in TYPES:
        for orientation in icon_shapes.orientations_of(type_):
            pixmap = icons.icon_pixmap(type_, orientation, height=40)
            assert not pixmap.isNull() and pixmap.height() == 40
            assert 4 <= pixmap.width() <= 40, (type_, orientation, pixmap.width())
            image = pixmap.toImage()
            drawn = sum(QColor(image.pixel(x, y)).alpha() > 0
                        for x in range(image.width()) for y in range(image.height()))
            assert drawn > 40, (type_, orientation)
    # A fork pointing left is wider than tall; pointing up, taller than wide.
    assert icons.icon_pixmap("fork", "left").width() > icons.icon_pixmap("fork", "up").width()


def test_a_fresh_view_draws_the_table_from_set_problem(qtbot):
    view = tv.TableView()
    qtbot.addWidget(view)
    assert view.object_items == {} and view.arrow_items == {}
    view.set_problem(load_problem("blockage-06"))
    problem = load_problem("blockage-06")
    assert set(view.object_items) == set(problem.ids())
    assert set(view.arrow_items) == {"H"}
    assert view.arrow_items["H"].arrow.object == problem.touched
    assert view.scene().sceneRect().width() == problem.width * tv.SCALE + 2 * tv.MARGIN
    assert view.name_items["Henry"].pos().y() < 0 < view.name_items["Eliza"].pos().y()
    assert view.layout.answer is None
    view.on_event(ev.RunStarted(clock=0, draws=0, problem="blockage-06", seed=1,
                                profile="standard", engine="compiled", changed=(),
                                touched=problem.touched,
                                objects=tuple(problem.ids())))
    view.redraw(now=False)
    assert set(view.object_items) == set(problem.ids())


def test_the_canvas_is_small_enough_for_the_smallest_window(module_window):
    canvas = module_window.table_view
    hint = canvas.minimumSizeHint()
    assert hint.width() <= 320 and hint.height() <= 200, (hint.width(), hint.height())
    assert the_view(module_window).minimumSize().width() == 160
