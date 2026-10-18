"""The roles pane and the roles in the other panes (item 14 of the roles loop;
`roles_view.py`, the roles layer of `table_view.py`, the roles' band of
`slipnet_grid_view.py`, the role dots of `slipnet_network_view.py`, the role marker of
`timeline.py`; G-060 to G-062), offscreen, one shared window with the roles dock floated
and the roles layer on, and one run of `blockage-07` seed 9 (every family read, a
barrier and its claimed counterpart, a re-cast and the axis; seed 16 from item 10 of the
roles loop to v4): the run through the window equals the plain run and the pixel probes of the roles
list and the table find no mismatch; the drawn lines are the model's with a solid bar on a
standing tag and a dashed one on a past tag; a fresh pane fed the history renders
byte-identically to the window's at sampled indexes; seeking tints a tag's lines at its
own event only; a click on a tag's lines selects its object in the shared selection and
the table follows; the hover text is the item's history; the table's roles item shows the
badges, the midline, the gaps, the images, the tiers and the block, and the legend says so;
the grid's roles' band is drawn in its own colour under its caption; the network's role
dots are flagged; the timeline marks the role changes; the Worldview pane marks the
role-proposed correspondence; the trace tab has the `RL` lines and the Events tab the
role events; the pane keeps its small minimum.
"""

import re

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QEvent, QPointF, Qt  # noqa: E402
from PySide6.QtGui import QColor, QMouseEvent  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop import events as ev  # noqa: E402
from tabletop import roles as R  # noqa: E402
from tabletop import slipnet_def as sd  # noqa: E402
from tabletop.gui import roles_view as rv  # noqa: E402
from tabletop.gui import slipnet_grid_view as gv  # noqa: E402
from tabletop.gui import worldview_view as wv  # noqa: E402
from tabletop.gui.models import roles as rm  # noqa: E402
from tabletop.gui.models import worldview as wm  # noqa: E402
from tabletop.gui.models import slipnet_grid as sg  # noqa: E402
from test_coderack_view import Probe, qtbot_wait  # noqa: E402
from test_table_view import EVERY, PixelProbe, run_to_end  # noqa: E402
from test_worldview_views import background_of, line_colours, line_index  # noqa: E402

RICH = ("blockage-07", 9)    # every family read; a barrier, its claim, a re-cast and the axis
# Since v5's fix (D-116 to D-120): seed 16, the run from item 10 of the roles loop to v4,
# no longer reads a barrier, so RICH moved to seed 9 (the same constant as
# `test_roles_model.py`'s: the first seed that reads every family with the barrier
# standing at the end and images on the touched object).
PROBES = {}
TAG_LINE = re.compile(r"r\d+ ")   # a tag's first line starts with its name (`tag_text`)


def bar_column(widget, k):
    """The colours down the k-th line's bar, at the bar's centre (`PAD + 1`, three
    pixels wide): a solid bar is one colour, a dashed one alternates with the
    background."""
    image = widget.viewport().grab().toImage()
    x = int(wv.PAD + 1)
    y0 = int(widget.line_top(k) + 2)
    return [QColor(image.pixel(x, y)).name() for y in range(y0, y0 + int(widget.line) - 4)]


def fresh_tags(widget):
    """The names of the tags whose lines are tinted as the event shown's."""
    return {line.text.split()[0] for line in widget.lines if line.fresh and TAG_LINE.match(line.text)}


def scroll_to(widget, k):
    """Scroll the k-th line to the top of the viewport and repaint."""
    widget.verticalScrollBar().setValue(0)
    widget.verticalScrollBar().setValue(max(0, int(widget.line_top(k) - wv.PAD)))
    widget.viewport().update()


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """The rich run through the shared window with the roles dock floated (its own
    window, as the rack dock is in `test_coderack_view.py`) and the roles layer on,
    probed on the roles list and on the table; the dock is re-docked at the end."""
    w = module_window
    w.resize(1600, 1000)
    w.roles_dock.setFloating(True)
    w.roles_dock.resize(900, 420)
    w.roles_dock.move(40, 40)
    w.table_view.view.layer_actions["roles"].setChecked(True)
    qapp.processEvents()
    qtbot_wait(qapp)
    for _ in range(100):            # a floated dock lays out on a later pass under load (G-012)
        if not w.roles_view.list.viewport().visibleRegion().isEmpty():
            break
        qtbot_wait(qapp, 20)
    assert w.roles_dock.isFloating() and not w.roles_view.list.viewport().visibleRegion().isEmpty()
    PROBES["roles"] = Probe(w, w.roles_view.list.viewport(), top=w.roles_dock)
    PROBES["table"] = PixelProbe(w)
    outcome = run_to_end(w, qapp, *RICH)
    for probe in PROBES.values():
        w.event_drawn.disconnect(probe.drawn)
    yield outcome
    # Re-dock at the module's end: a closed window's floating dock stays a visible
    # top-level window over the next module's panes and takes their cursor events.
    w.roles_dock.setFloating(False)
    qapp.processEvents()


def test_the_run_through_the_window_equals_the_plain_run_and_the_pixels_are_the_events(module_window, ran):
    w = module_window
    ref = plain_run(*RICH, V2)
    assert list(w.history.records) == list(ref.records) and ran.result.draws == ref.draws
    assert w.statusBar().currentMessage() == ref.report
    for name, probe in PROBES.items():
        assert probe.checked == list(range(0, len(w.history), EVERY)), name
        assert probe.mismatches == [], name
    assert w.table_view.view.overlay_state()["roles"]


def test_the_drawn_lines_are_the_models_with_solid_and_dashed_bars(module_window, ran):
    w = module_window
    widget = w.roles_view.list
    model = w.roles_view.model
    assert widget.lines == tuple(model.lines())
    assert model.events == len(w.history) and len(model.standing()) > 10
    standing = line_index(widget, lambda line: line.role == wm.WORLDVIEW)
    past = line_index(widget, lambda line: line.role == wm.SHADOW)
    bar, background = QColor(wv.BAR).name(), QColor(wv.BACKGROUND).name()
    try:
        scroll_to(widget, standing)
        assert bar in line_colours(widget, standing)
        assert set(bar_column(widget, standing)) == {bar}, "a solid bar"
        scroll_to(widget, past)
        column = bar_column(widget, past)
        assert bar in column and background in column, "a dashed bar"
        assert widget.lines[past].text.startswith(tuple(f"{t.name} " for t in model.tags.values()
                                                        if not t.standing))
    finally:
        widget.verticalScrollBar().setValue(0)
    assert w.roles_view.label.text().startswith(rm.NOT_FRENCHS)
    assert "P14 Roles" in w.roles_view.toolTip() and "re-interpretation" in w.roles_dock.toolTip()


def test_a_fresh_pane_fed_the_history_draws_the_windows_picture(module_window, ran, qapp, tmp_path):
    w = module_window
    events = w.history.events
    try:
        for index in (0, 400, len(events) - 1):
            assert w.seek(index)
            w.roles_view.list.verticalScrollBar().setValue(0)
            other = rv.RolesView()
            other.attach(w.history)
            other.resize(w.roles_view.size())
            other.show()
            qapp.processEvents()
            for i, event in enumerate(events[:index + 1]):
                other.set_position(i)
                other.on_event(event)
            other.redraw()
            qtbot_wait(qapp)        # the scrollbar a long list grows narrows the viewport (G-058)
            other.list.verticalScrollBar().setValue(0)
            assert other.list.lines == w.roles_view.list.lines
            assert other.list.viewport().size() == w.roles_view.list.viewport().size(), index
            a = w.roles_view.render_png(tmp_path / f"window-{index}.png")
            b = other.render_png(tmp_path / f"fresh-{index}.png")
            assert a == b, index
            other.close()
    finally:
        assert w.seek(len(events) - 1)


def test_seeking_tints_a_tags_lines_at_its_own_event_only(module_window, ran):
    w = module_window
    widget = w.roles_view.list
    model = w.roles_view.model
    tag = next(t for t in model.tags.values() if not t.standing)
    events = w.history.events
    try:
        assert w.seek(tag.discarded_index)
        assert isinstance(events[tag.discarded_index], ev.RoleDiscarded)
        fresh = [line for line in widget.lines if line.fresh]
        assert len(fresh) == 2 and all(line.name == tag.item for line in fresh)
        assert fresh_tags(widget) == {tag.name} and fresh[0].role == wm.SHADOW
        k = line_index(widget, lambda line: line.fresh)
        scroll_to(widget, k)
        assert background_of(widget, k) == QColor(wv.FRESH_TINT).name()
        assert background_of(widget, k + 2) != QColor(wv.FRESH_TINT).name()
        assert w.seek(tag.built_index)
        assert fresh_tags(widget) == {tag.name}
        assert [line.role for line in widget.lines if line.fresh] == [wm.WORLDVIEW, wm.DETAIL]
        # The next event: its own tag's lines, if it tags one (here the rival that won),
        # and nothing of the tag that went.
        after = events[tag.discarded_index + 1]
        assert w.seek(tag.discarded_index + 1)
        assert tag.name not in fresh_tags(widget)
        if isinstance(after, ev.RoleTagged):
            assert fresh_tags(widget) == {after.tag}
        elif not isinstance(after, (ev.RoleDiscarded, ev.RoleRecast)):
            assert not any(line.fresh for line in widget.lines)
    finally:
        widget.verticalScrollBar().setValue(0)
        assert w.seek(len(w.history) - 1)


def test_a_click_selects_the_tags_object_and_the_table_follows(module_window, ran, qapp):
    w = module_window
    widget = w.roles_view.list
    assert w.seek(len(w.history) - 1)
    k = line_index(widget, lambda line: line.role == wm.WORLDVIEW
                   and line.name in w.table_view.view.object_items)
    scroll_to(widget, k)
    y = widget.line_top(k) + widget.line / 2
    pos = QPointF(20.0, y)
    press = QMouseEvent(QEvent.Type.MouseButtonPress, pos, widget.viewport().mapToGlobal(pos.toPoint()),
                        Qt.MouseButton.LeftButton, Qt.MouseButton.LeftButton, Qt.KeyboardModifier.NoModifier)
    name = widget.lines[k].name
    try:
        widget.mousePressEvent(press)
        qapp.processEvents()
        assert w.selection.kind == "object" and w.selection.name == name
        assert w.table_view.view.object_items[name].selected
        assert w.statusBar().currentMessage().startswith(f"selected {name}")
        assert background_of(widget, k) == QColor(wv.SELECTED_TINT).name()
        # The roles layer's images follow the selected object (G-061).
        assert w.table_view.view.model.overlays.focus == name
        assert all(image.source == name for image in w.table_view.view.layout.overlays.images)
        widget.mousePressEvent(press)
        qapp.processEvents()
        assert not w.selection.name
        assert w.table_view.view.model.overlays.focus is None
    finally:
        w.selection.clear()
        widget.verticalScrollBar().setValue(0)
    text = widget.model.tooltip(name)
    assert text.startswith(f"{name}: ") and "standing" in text
    assert widget.model.tooltip(R.TABLE).startswith(f"{R.TABLE}: ")
    assert widget.model.selection_of(R.TABLE) is None


def test_the_tables_roles_item_shows_every_mark_and_the_legend_says_so(module_window, ran):
    w = module_window
    assert w.seek(len(w.history) - 1)
    view = w.table_view.view
    item = view.roles_item
    assert item.isVisible() and item.form == view.layout.overlays
    o = item.form
    assert o.roles_shown and o.badges and o.midline is not None and o.gaps and o.images
    assert o.tiers and o.blocks and o.axis == "point"
    assert item.boundingRect().contains(item.table) and item.table.width() == 110 * 6
    assert "roles (" in w.table_view.legend.text()
    assert "Roles" in [a.text() for a in w.overlays_menu.actions()]
    assert "roles" in view.overlay_state() and w.table_view.overlay_buttons["roles"].isChecked()
    assert "roles: " in view.object_items["cup1"].toolTip()
    assert any(line.role_proposed for line in view.layout.correspondences)


def test_the_grids_roles_band_is_drawn_in_its_own_colour_under_its_caption(module_window, ran, qapp):
    w = module_window
    pane = w.slipnet_grid_view
    w.slipnet_grid_dock.raise_()
    qapp.processEvents()
    pane.grid.update()
    qapp.processEvents()
    lay = pane.layout_now
    box = lay.box("barrier")
    assert box.role and not box.on_screen and lay.role_caption == sg.ROLE_CAPTION
    image = pane.grid.grab().toImage()
    colours = {QColor(image.pixel(int(box.x + box.side / 2 + k), int(box.y) + dy)).name()
               for k in range(0, 8) for dy in (-1, 0, 1)}
    assert QColor(gv.ROLE_BOX).name() in colours, "the dashed border of a role box"
    assert QColor(gv.EXTRA_BOX).name() not in colours and QColor(gv.BOX).name() not in colours
    assert not lay.box("cup").role and not lay.box("isolated").role
    assert lay.role_caption_y > lay.caption_y > lay.box("cup").y
    assert "roles' band" in sg.hover_text(pane.model, "barrier")


def test_the_network_the_timeline_the_worldview_and_the_log_show_the_roles(module_window, ran):
    w = module_window
    nl = w.slipnet_network_view.layout_now
    assert {d.name for d in nl.dots if d.role} == set(sd.ROLE_NODES) and len(sd.ROLE_NODES) == 15
    assert {l.name for l in nl.lines if l.role} == {link.name for link in sd.ROLE_LINKS}
    assert len(sd.ROLE_LINKS) == 29
    roles = w.timeline.model.markers_of("role")
    assert roles and all(w.history.events[m.index].__class__ in (ev.RoleDiscarded, ev.RoleRecast)
                         for m in roles)
    texts = [line.text for line in w.worldview_view.worldview.lines]
    assert any("role-proposed (" in text for text in texts)
    trace = "\n".join(line.text for line in w.log_view.trace.model.lines)
    assert "RL tag " in trace and "RL used " in trace and "RL recast " in trace and "RL drop " in trace
    assert w.history.counts.get("RoleTagged", 0) > 0


def test_the_panes_are_small_enough(module_window):
    assert module_window.roles_view.minimumSize().width() <= wv.MIN_SIZE[0]
    assert module_window.roles_view.minimumSize().height() <= wv.MIN_SIZE[1]
