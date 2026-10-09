"""List interaction and hover tooltips (loop0002 item 020): seqsee/gui/draw/lists (click_of_tags,
page_after), seqsee/gui/listactions.py, seqsee/gui/hover.py, seqsee/gui/qt/listpopup.py and
their wiring in seqsee/gui/qt/mainwindow.py / panes.py / runner.py.

Mirrors lib/SGUI/List.pm: Setup's '<1>' bindings on "$self-pageup" (PageNumber++),
"$self-pagedown" (PageNumber-- if PageNumber) and "$self-Clickable-Item" (the 'current'
item's useful tag → ProcessClickOnItem), ProcessClickOnItem (SelectedItem,
$Global::Break_Loop = 1, the popup deiconified) and CreatePopupWidget (Toplevel 'Actions for
<class>', one button per ActionButtons entry in hash order, each running its action on
SelectedItem, then Tk::Seqsee::Update and withdraw); the ActionButtons of
lib/SGUI/List/Groups.pm and lib/SGUI/List/Categories.pm (through lib/SWorkspace.pm's
__DeleteGroup, __ClearBarLines, __AddBarLines, __RemoveGroupsCrossingBarLines, SLTM's
FindActiveFollowers, SHistory's history_as_text, SThought's get_fringe_for / get_actions).
Golden: tests/golden/gui_list_interaction.json from oracle/gui_list_interaction.pl.

The hover tooltips on workspace objects have no Perl counterpart (Perl's canvas shows
nothing on hover); they describe the snapshot object drawn under the pointer.
"""
import dataclasses
import re
from pathlib import Path

import pytest

import gui_recipes
from golden import load
from seqsee import global_ as Global
from seqsee import sworkspace, util
from seqsee.gui import hover, listactions, snapshot
from seqsee.gui.draw import lists, views

CASES = load("gui_list_interaction")
POPUPS = [c for c in CASES if c["kind"] == "popup"]
CLICKS = [c for c in CASES if c["kind"] == "clicks"]
ACTIONS = [c for c in CASES if c["kind"] == "action"]
GROUPS = views.GROUPS_LIST
CATEGORIES = views.CATEGORIES_LIST
REF = re.compile(r"=(HASH|SCALAR|ARRAY|CODE)\(0x[0-9a-f]+\)")
PY = Path(__file__).resolve().parents[1]
SCREENS_DIR = PY / "docs" / "gui" / "screens"


def _label(part, entry):
    """What the oracle records for an entry: a group's bounds string, a category's name."""
    if part == CATEGORIES:
        return entry.category.name
    return entry.bounds_string


def _live_item(part, label):
    if part == GROUPS:
        return next(g for g in sworkspace.get_groups() if g.get_bounds_string() == label)
    for obj in sworkspace.get_groups() + sworkspace.get_elements():
        for cat in obj.get_categories():
            if cat.get_name() == label:
                return cat
    raise KeyError(label)


_ARGS = re.compile(r"args=\{ (.*?),  \}")
_ARG = re.compile(r"\w+ => «[^»]*»")


def _args_sorted(msg):
    """A message with each Codelet's ``args={ k => «v», ... }`` in sorted order: SCodelet's
    as_text lists them in hash order (``keys %args``), the port in insertion order."""
    if msg[0] is None:
        return msg
    return [_ARGS.sub(lambda m: "args={ " + ", ".join(sorted(_ARG.findall(m.group(1))))
                      + ",  }", msg[0]), msg[1]]


def _groups_state():
    return sorted(({"bounds": g.get_bounds_string(),
                    "locked": 1 if util.perl_true(g.get_is_locked_against_deletion()) else 0}
                   for g in sworkspace.get_groups()), key=lambda r: r["bounds"])


# ---- pure: the bindings --------------------------------------------------------------------
def test_click_of_tags_follows_the_bindings():
    name = GROUPS
    assert lists.click_of_tags((name + "-pageup", name)) == lists.Click("pageup", name, None)
    assert lists.click_of_tags((name + "-pagedown", name)) == lists.Click("pagedown", name,
                                                                          None)
    assert lists.click_of_tags((name, "obj7", name + "-Clickable-Item")) == lists.Click(
        "item", name, "obj7")
    # The useful tag is the first that is neither the list nor its Clickable tag.
    assert lists.click_of_tags(("obj7", name + "-Clickable-Item", name)) == lists.Click(
        "item", name, "obj7")
    assert lists.click_of_tags((name,)) is None                 # the "Page #..." text
    assert lists.click_of_tags(()) is None
    assert lists.click_of_tags(("obj3", "element", "0")) is None
    assert lists.click_of_tags(("hilit",)) is None


def test_page_after():
    assert lists.page_after("pageup", 0) == 1
    assert lists.page_after("pageup", 4) == 5               # clamped by the next draw
    assert lists.page_after("pagedown", 3) == 2
    assert lists.page_after("pagedown", 0) == 0


# ---- golden: the popups ----------------------------------------------------------------------
@pytest.mark.parametrize("case", POPUPS, ids=lambda c: c["list"])
def test_popup_actions_match_perl(case):
    assert listactions.popup_title(case["list"]) == case["title"]
    assert list(listactions.ACTIONS[case["list"]]) == case["buttons"]
    assert case["state"] == "withdrawn"


# ---- golden: the actions ---------------------------------------------------------------------
def _action_id(case):
    return "{}-{}-{}-{}".format(case["list"].rsplit("::", 1)[1], case["recipe"], case["action"],
                                case["item"].strip())


@pytest.mark.parametrize("case", ACTIONS, ids=_action_id)
def test_action_matches_perl(case):
    gui_recipes.build(case["recipe"])
    assert _groups_state() == case["groups_before"]
    item = _live_item(case["list"], case["item"])
    messages = []
    util.srand(20)                      # as the oracle (ActionFringe draws random numbers)
    listactions.run(case["list"], case["action"], item,
                    lambda msg, no_break=None: messages.append(
                        [REF.sub("=REF", util.perl_str(msg)), 1 if no_break else 0]))
    assert _groups_state() == case["groups_after"]
    assert [util.perl_num(b) for b in sworkspace.get_bar_lines()] == case["bar_lines"]
    assert [_args_sorted(m) for m in messages] == [_args_sorted(m) for m in case["messages"]]
    assert case["died"] is None and case["popup"] == "withdrawn" and case["updates"] == 1


def test_golden_covers_actions():
    for part in (GROUPS, CATEGORIES):
        assert {c["action"] for c in ACTIONS if c["list"] == part} == set(
            listactions.ACTIONS[part])
    assert any(c["messages"] for c in ACTIONS)
    assert any(c["bar_lines"] for c in ACTIONS)
    assert any(len(c["groups_after"]) < len(c["groups_before"]) for c in ACTIONS)


def test_unknown_action_raises():
    with pytest.raises(KeyError):
        listactions.run(GROUPS, "Explode", None, print)


# ---- Qt ------------------------------------------------------------------------------------
QtCore = pytest.importorskip("PySide6.QtCore")
QtWidgets = pytest.importorskip("PySide6.QtWidgets")

from PySide6.QtCore import QPoint, Qt  # noqa: E402

from seqsee.gui.qt import listpopup, mainwindow, render  # noqa: E402
from seqsee.gui.runner import Runner  # noqa: E402


def _ordered(snap, part, labels):
    """The snapshot with GetItemList in Perl's order (``labels``)."""
    if part == GROUPS:
        pool = {g.bounds_string: g for g in snap.groups}
        return dataclasses.replace(snap, groups=tuple(pool[b] for b in labels))
    by_name = {c.name: c for c in snap.categories}
    rest = tuple(c for c in snap.categories if c.name not in labels)
    return dataclasses.replace(snap, categories=tuple(by_name[n] for n in labels) + rest)


def _list_view(part, rect):
    """A canvas (x + w) × (y + h) with ``part`` at ``rect``, as SetupParts would place it."""
    x, y, w, h = rect
    cw, ch = x + w, y + h
    return views.View(part, ((part, 100 * x / cw, 100 * y / ch, 100 * w / cw, 100 * h / ch),)), \
        cw, ch


def _clicks_id(case):
    return "{}-{}-{}".format(case["list"].rsplit("::", 1)[1], case["recipe"],
                             "x".join(str(v) for v in case["rect"]))


@pytest.mark.gui
@pytest.mark.parametrize("case", CLICKS, ids=_clicks_id)
def test_clicks_match_perl(qapp, case):
    """Each click hits the topmost Qt item at the point, as Tk's 'current' item, and does
    what the binding of its tags does."""
    part = case["list"]
    gui_recipes.build(case["recipe"])
    base = snapshot.take()
    view, cw, ch = _list_view(part, case["rect"])
    page = 0
    for click in case["clicks"]:
        snap = _ordered(base, part, click["entries"])
        comp = views.compose(view, snap, cw, ch, pages={part: page})
        page = comp.lists[part].page_number
        scene = render.render(comp.ops, width=cw, height=ch)
        hit = listpopup.list_click(scene, click["x"], click["y"])
        selected = None
        if hit is not None and hit.kind == "item":
            entry = lists.entry_for(snap, hit.part, hit.key)
            selected = _label(part, entry)
        elif hit is not None:                       # PageNumber +/- 1, then ReDrawIt
            page = views.compose(view, snap, cw, ch, pages={
                part: lists.page_after(hit.kind, page)}).lists[part].page_number
        assert (page, selected) == (click["page"], click["selected"]), click
        assert (selected is not None) == bool(click["break_loop"])
        scene.clear()


class FakeRunner(QtCore.QObject):
    snapshot = QtCore.Signal(object)
    question = QtCore.Signal(object)
    question_closed = QtCore.Signal(object)
    message = QtCore.Signal(object)
    error = QtCore.Signal(str, str)
    command_done = QtCore.Signal(str, object)
    state_changed = QtCore.Signal(str)

    def __init__(self):
        super().__init__()
        self.calls = []
        self.state = "idle"

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)

        def call(*a, **k):
            self.calls.append((name,) + a)
        return call


@pytest.fixture
def win(qtbot):
    w = mainwindow.MainWindow()
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


@pytest.fixture
def fake(win):
    r = FakeRunner()
    win.attach_runner(r)
    return r


def _snap(recipe):
    gui_recipes.build(recipe)
    return snapshot.take()


def _click(qtbot, canvas, x, y, button=Qt.LeftButton):
    qtbot.mouseClick(canvas.viewport(), button, pos=QPoint(int(x), int(y)))


def _row_centre(comp, part, key):
    bar = next(o for o in comp.ops if o.TYPE == "rectangle" and key in o.tags
               and part + "-Clickable-Item" in o.tags)
    x1, y1, x2, y2 = bar.coords
    return (x1 + x2) / 2, (y1 + y2) / 2


@pytest.mark.gui
def test_popup_widget(qtbot):
    popups = listpopup.ListPopups()
    for case in POPUPS:
        p = popups.popup(case["list"])
        assert p.windowTitle() == case["title"]
        assert [b.text() for b in p.buttons] == case["buttons"]
        assert p.isHidden()
    assert popups.popup(GROUPS) is popups.popup(GROUPS)        # one per list


@pytest.mark.gui
def test_click_on_a_row_opens_the_popup_and_pauses(qtbot, win, fake):
    win.show_snapshot(_snap("groups_list"))
    win.set_view("Workspace + Groups")
    qtbot.waitUntil(lambda: win.composition is not None)
    g = win.snapshot.groups[1]
    x, y = _row_centre(win.composition, GROUPS, "obj%d" % g.oid)
    _click(qtbot, win.canvas, x, y)
    popup = win.list_popups.popup(GROUPS)
    assert popup.isVisible()
    assert popup.windowTitle() == "Actions for SGUI::List::Groups"
    assert g.bounds_string.strip() in popup.details.text()
    assert win.selected_items[GROUPS] == (win.snapshot, "obj%d" % g.oid)
    assert ("pause",) in fake.calls                          # $Global::Break_Loop = 1
    # A button runs the action on the selected item, then withdraws the popup.
    fake.calls.clear()
    popup.button("Lock").click()
    assert fake.calls == [("list_action", win.snapshot, GROUPS, "Lock", "obj%d" % g.oid)]
    assert popup.isHidden()


@pytest.mark.gui
def test_click_elsewhere_does_nothing(qtbot, win, fake):
    win.show_snapshot(_snap("groups_list"))
    win.set_view("Workspace + Groups")
    _click(qtbot, win.canvas, 5, 5)
    _click(qtbot, win.canvas, 30, 440)                       # the "Page #..." text
    assert not win.list_popups.popup(GROUPS).isVisible()
    assert fake.calls == []


@pytest.mark.gui
def test_page_squares_page_the_view_list(qtbot, win):
    win.show_snapshot(_snap("groups_many"))
    win.set_view("Workspace + Groups")                       # the list at (0, 225, 780, 225)
    w, h = win.canvas_size()
    top = (w - 20 + 5, h / 2 + 20 + 5)                       # page down (back)
    bottom = (w - 20 + 5, h - 20 + 5)                        # page up (forward)
    _click(qtbot, win.canvas, *bottom)
    assert win.pages[GROUPS] == 1 and win.composition.lists[GROUPS].page_number == 1
    _click(qtbot, win.canvas, *bottom)
    _click(qtbot, win.canvas, *bottom)                       # past the last: clamped
    last = win.composition.lists[GROUPS].page_number
    _click(qtbot, win.canvas, *top)
    assert win.composition.lists[GROUPS].page_number == last - 1
    for _ in range(5):
        _click(qtbot, win.canvas, *top)
    assert win.composition.lists[GROUPS].page_number == 0
    assert "Page #1." in " ".join(o.text or "" for o in win.composition.ops if o.TYPE == "text")


@pytest.mark.gui
def test_pane_list_pages_and_popup(qtbot, win, fake):
    win.show_snapshot(_snap("categories_many"))
    dock = win.pane_docks[CATEGORIES]
    dock.show()
    qtbot.waitUntil(lambda: dock.composition is not None)
    w, h = dock.canvas_size()
    _click(qtbot, dock.canvas, w - 20 + 5, h - 20 + 5)
    assert dock.pages[CATEGORIES] == 1
    assert win.pages.get(CATEGORIES, 0) == 0                 # the view's page doesn't move
    entry = lists.entry_for(win.snapshot, CATEGORIES, next(
        t for o in dock.composition.ops for t in o.tags if t.startswith("cat")))
    x, y = _row_centre(dock.composition, CATEGORIES, "cat%d" % entry.category.cid)
    _click(qtbot, dock.canvas, x, y)
    popup = win.list_popups.popup(CATEGORIES)
    assert popup.isVisible() and entry.category.name in popup.details.text()
    assert [b.text() for b in popup.buttons] == ["AddBarlinesBefore", "DeleteAllOther"]
    popup.button("DeleteAllOther").click()
    assert fake.calls[-1] == ("list_action", win.snapshot, CATEGORIES, "DeleteAllOther",
                              "cat%d" % entry.category.cid)


@pytest.mark.gui
def test_popup_close_withdraws(qtbot, win, fake):
    win.show_snapshot(_snap("groups_list"))
    win.process_click_on_item(GROUPS, "obj%d" % win.snapshot.groups[0].oid)
    popup = win.list_popups.popup(GROUPS)
    assert popup.isVisible()
    popup.close()
    assert popup.isHidden()
    win.process_click_on_item(GROUPS, "obj%d" % win.snapshot.groups[0].oid)
    assert popup.isVisible()                                  # the same window again


@pytest.mark.gui
def test_stream_list_popup_has_no_buttons(qtbot, win):
    win.show_snapshot(_snap("stream_list_many"))
    win.process_click_on_item(views.STREAM_LIST, "tht0")
    popup = win.list_popups.popup(views.STREAM_LIST)
    assert popup.isVisible() and popup.buttons == []
    assert "Thought" in popup.details.text()


@pytest.mark.gui
def test_list_action_without_runner(qtbot, win):
    win.show_snapshot(_snap("groups_list"))
    win.process_click_on_item(GROUPS, "obj%d" % win.snapshot.groups[0].oid)
    win.list_popups.popup(GROUPS).button("Delete").click()
    assert "No model attached" in win.statusBar().currentMessage()


@pytest.mark.gui
def test_runner_list_action_changes_the_model(qtbot):
    """A real Runner: the action runs on the worker on the live object behind the
    snapshot's tag; the command's closing snapshot (Tk::Seqsee::Update) shows the result."""
    gui_recipes.build("groups_list")
    runner = Runner(min_interval=0)
    snaps = []
    runner.snapshot.connect(snaps.append)
    runner.start()
    try:
        with qtbot.waitSignal(runner.command_done, timeout=5000):
            runner.call(lambda: None)
        snap = snaps[-1]
        big = next(g for g in snap.groups if g.bounds_string == " <0, 5> ")
        a = next(g for g in snap.groups if g.bounds_string == " <0, 2> ")
        with qtbot.waitSignal(runner.command_done, timeout=5000):
            runner.list_action(snap, GROUPS, "Unlock", "obj%d" % a.oid)
        assert not next(g for g in snaps[-1].groups if g.bounds_string == " <0, 2> ").is_locked
        with qtbot.waitSignal(runner.command_done, timeout=5000):
            runner.list_action(snap, GROUPS, "Delete", "obj%d" % big.oid)
        assert sorted(g.bounds_string for g in snaps[-1].groups) == [
            " <0, 2> ", " <3, 5> ", " <6, 8> "]
        # History: main::message without no_break waits for 'continue'.
        questions = []
        runner.question.connect(lambda q: (questions.append(q), runner.answer(q, "continue")))
        with qtbot.waitSignal(runner.command_done, timeout=5000):
            runner.list_action(snap, GROUPS, "History", "obj%d" % a.oid)
        assert questions and questions[0].text.startswith("History:")
        # An unknown snapshot or tag is an error, not a crash.
        errors = []
        runner.error.connect(lambda m, tb: errors.append(m))
        with qtbot.waitSignal(runner.command_done, timeout=5000):
            runner.list_action(snap, GROUPS, "Lock", "obj999")
        assert errors and "no longer" in errors[0]
    finally:
        assert runner.quit()


def test_take_fills_live_objects():
    gui_recipes.build("groups_list")
    live = {}
    snap = snapshot.take(live=live)
    for g in snap.groups:
        assert live["obj%d" % g.oid].get_bounds_string() == g.bounds_string
    for c in snap.categories:
        assert util.perl_str(live["cat%d" % c.cid].get_name()) == c.name


# ---- entries and details -------------------------------------------------------------------
def test_entry_for_each_list():
    snap = _snap("groups_list")
    g = snap.groups[0]
    assert lists.entry_for(snap, GROUPS, "obj%d" % g.oid) is g
    cat = snap.categories[0]
    assert lists.entry_for(snap, CATEGORIES, "cat%d" % cat.cid).category is cat
    assert lists.entry_for(snap, GROUPS, "obj999") is None
    assert lists.entry_for(snap, GROUPS, None) is None
    snap = _snap("stream_list_many")
    assert lists.entry_for(snap, views.STREAM_LIST, "tht0")[1] is snap.stream.current


def test_describe_entry():
    snap = _snap("groups_list")
    a = next(g for g in snap.groups if g.bounds_string == " <0, 2> ")
    text = hover.describe_entry(snap, GROUPS, "obj%d" % a.oid)
    assert "Group" in text and "<0, 2>" in text and "locked" in text and "55.55" in text
    assert "ascending" in text
    cat = next(c for c in snap.categories if c.name == "ascending")
    text = hover.describe_entry(snap, CATEGORIES, "cat%d" % cat.cid)
    assert "ascending" in text and "2 instances" in text
    assert hover.describe_entry(snap, GROUPS, "obj999") == ""


# ---- hover tooltips --------------------------------------------------------------------------
def _element_op(comp, index):
    return next(o for o in comp.ops if "element" in o.tags and str(index) in o.tags)


def test_hover_targets_on_the_workspace():
    snap = _snap("groups_relations")
    comp = views.compose("Workspace", snap, 780, 450)
    # An element: its text.
    op = _element_op(comp, 3)
    text = hover.tooltip(snap, "Workspace", 780, 450, *op.coords)
    elt = snap.elements[3]
    assert text.startswith("Element") and str(elt.mag) in text and "index 3" in text
    # A group: inside its oval, away from the elements' texts (above them).
    big = snap.largest_group
    oval = next(o for o in comp.ops if o.TYPE == "oval" and o.fill)
    x1, y1, x2, y2 = oval.coords
    text = hover.tooltip(snap, "Workspace", 780, 450, (x1 + x2) / 2, y1 + 4)
    assert text.startswith("Group") and big.bounds_string.strip() in text
    # A relation: the top of its curve.
    line = next(o for o in comp.ops if o.TYPE == "line" and o.arrow == "last")
    (ax, ay), (zx, zy), (bx, by) = line.coords[0:2], line.coords[2:4], line.coords[4:6]
    mx, my = (ax + 2 * zx + bx) / 4, (ay + 2 * zy + by) / 4      # the curve at t = 1/2
    text = hover.tooltip(snap, "Workspace", 780, 450, mx, my)
    assert text.startswith("Relation") and "succ" in text
    # Nothing.
    assert hover.tooltip(snap, "Workspace", 780, 450, 2, 2) == ""


def test_hover_on_other_parts_and_views():
    snap = _snap("groups_relations")
    comp = views.compose("Workspace + Groups", snap, 780, 450)
    op = _element_op(comp, 0)
    assert hover.tooltip(snap, "Workspace + Groups", 780, 450, *op.coords).startswith("Element")
    # The Groups list part has no hover targets.
    assert hover.tooltip(snap, "Workspace + Groups", 780, 450, 100, 260) == ""
    # The attention part says the attention.
    comp = views.compose("Workspace + Attention", snap, 780, 450)
    elt_ops = [o for o in comp.ops if "element" in o.tags and "0" in o.tags]
    x, y = elt_ops[1].coords                                      # the attention part's
    text = hover.tooltip(snap, "Workspace + Attention", 780, 450, x, y)
    assert text.startswith("Element") and "attention" in text
    # A view without a workspace.
    assert hover.tooltip(snap, views.pane_view(views.SLIPNET), 780, 450, 100, 100) == ""


def test_target_order_prefers_elements_then_relations_then_smallest_group():
    snap = _snap("nested_groups")
    comp = views.compose("Workspace", snap, 780, 450)
    op = _element_op(comp, 1)
    assert hover.tooltip(snap, "Workspace", 780, 450, *op.coords).startswith("Element")
    targets = hover.targets(snap, views.WORKSPACE, (0, 0, 780, 450))
    groups = [t for t in targets if t.kind == "group"]
    x1, y1, x2, y2 = groups[-1].coords
    inner = hover.target_at(groups, (x1 + x2) / 2, y1 + 2)
    assert inner.kind == "group"
    spans = [snap.obj(t.oid).span for t in groups if hover._in_oval(t.coords, (x1 + x2) / 2,
                                                                     y1 + 2)]
    assert snap.obj(inner.oid).span == min(spans)


@pytest.mark.gui
def test_window_hover_sets_the_tooltip(qtbot, win):
    win.show_snapshot(_snap("groups_relations"))
    win.set_view("Workspace")
    op = _element_op(win.composition, 2)
    assert win.tooltip_at(*op.coords).startswith("Element")
    qtbot.mouseMove(win.canvas.viewport(), QPoint(int(op.coords[0]), int(op.coords[1])))
    qtbot.waitUntil(lambda: win.canvas.toolTip().startswith("Element"))
    qtbot.mouseMove(win.canvas.viewport(), QPoint(3, 3))
    qtbot.waitUntil(lambda: win.canvas.toolTip() == "")
    assert win.tooltip_at(3, 3) == ""


@pytest.mark.gui
def test_pane_hover(qtbot, win):
    win.show_snapshot(_snap("groups_relations"))
    dock = win.pane_docks[views.WORKSPACE]
    dock.show()
    qtbot.waitUntil(lambda: dock.composition is not None)
    op = _element_op(dock.composition, 1)
    assert dock.tooltip_at(*op.coords).startswith("Element")


# ---- screenshot ------------------------------------------------------------------------------
@pytest.mark.gui
def test_popup_screenshot(qtbot, win, tmp_path, request):
    """The Groups popup after a click (``pytest --write-screens`` saves
    docs/gui/screens/list_popup_groups.png; Perl's is docs/gui/perl/list_popup_groups.png)."""
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    win.show_snapshot(_snap("groups_list"))
    a = next(g for g in win.snapshot.groups if g.bounds_string == " <0, 2> ")
    win.process_click_on_item(GROUPS, "obj%d" % a.oid)
    popup = win.list_popups.popup(GROUPS)
    qtbot.waitExposed(popup)
    assert popup.grab().save(str(out_dir / "list_popup_groups.png"), "PNG")


@pytest.mark.gui
def test_hover_screenshot(qtbot, win, tmp_path, request):
    """The window with the tooltip of a group drawn into it (no Perl counterpart):
    docs/gui/screens/hover_tooltip.png with ``--write-screens``."""
    from PySide6.QtGui import QPainter
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    win.show_snapshot(_snap("groups_relations"))
    win.set_view("Workspace")
    oval = next(o for o in win.composition.ops if o.TYPE == "oval" and o.fill)
    x1, y1, x2, y2 = oval.coords
    x, y = (x1 + x2) / 2, y1 + 4
    text = win.tooltip_at(x, y)
    assert text.startswith("Group")
    image = win.grab()
    label = QtWidgets.QLabel(text)
    label.setStyleSheet("background: #FFFFDC; border: 1px solid #777; padding: 3px;")
    label.adjustSize()
    origin = win.canvas.viewport().mapTo(win, QPoint(int(x) + 12, int(y) + 16))
    painter = QPainter(image)
    label.render(painter, origin)
    painter.end()
    assert image.save(str(out_dir / "hover_tooltip.png"), "PNG")
