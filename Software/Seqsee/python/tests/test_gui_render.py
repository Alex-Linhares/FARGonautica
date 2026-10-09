"""The Qt renderer (seqsee/gui/qt/render.py): draw ops → QGraphicsScene items.

Mirrors the Perl/Tk canvas item model that lib/SGUI/*.pm, lib/Tk/Seqsee.pm and
lib/Themes/Std2.pm draw with: createLine (arrow, arrowshape, smooth, dash, capstyle),
createOval, createRectangle (stipple), createPolygon, createArc (start, extent, style) and
createText (anchor, justify, X11 font names), as implemented by Tk 804 (tkCanvLine.c's
ConfigureArrows, tkTrig.c's TkMakeBezierCurve, tkCanvUtil.c's DashConvert, tkCanvText.c's
ComputeTextBbox). Every golden of the drawing items (gui_*.json) is rendered.
"""
import json
import math
from pathlib import Path

import pytest

import golden
from gui_screens import CONTROL_SCREENS, SCREENS, WINDOW_SCREENS, canvas_size, screen_case
from seqsee.gui.draw import ops

pytestmark = pytest.mark.gui

QtCore = pytest.importorskip("PySide6.QtCore")
QtGui = pytest.importorskip("PySide6.QtGui")
QtWidgets = pytest.importorskip("PySide6.QtWidgets")
Qt = QtCore.Qt

from seqsee.gui.qt import render  # noqa: E402

FONT20 = "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4"
FONT10 = "-adobe-helvetica-bold-r-normal--10-140-100-100-p-105-iso8859-4"
PY = Path(__file__).resolve().parents[1]
SCREENS_DIR = PY / "docs" / "gui" / "screens"
PERL_DIR = PY / "docs" / "gui" / "perl"
DRAWING_GOLDENS = sorted(p.stem for p in (PY / "tests" / "golden").glob("gui_*.json")
                         if p.stem not in ("gui_theme", "gui_window", "gui_controls",
                                           "gui_commentary", "gui_seqentry",
                                           "gui_list_interaction", "gui_entry"))


def styled():
    """The 'styled' recipe of oracle/gui_smoke.pl (as in test_gui_infra.py)."""
    return [
        ops.rectangle(5, 5, 395, 255, fill="#EEEEEE", outline="", tags=["bg"]),
        ops.line(20, 200, 80, 120, 140, 200, fill="red", width=3, smooth=1,
                 arrow="last", arrowshape=[8, 12, 10], tags=["reln", "r1"]),
        ops.line(160, 30, 380, 30, fill="#0000FF", width=2, dash="---"),
        ops.line(160, 50, 380, 50, dash=[6, 4], arrow="both", capstyle="round"),
        ops.oval(160, 70, 220, 130, fill="#CCFFDD", outline="navy blue", width=0),
        ops.rectangle(240, 70, 300, 130, fill="gray75", outline="#000000", width=4,
                      stipple="gray75"),
        ops.polygon(320, 130, 350, 70, 380, 130, fill="", outline="DarkGreen", smooth=1, width=2),
        ops.arc(160, 150, 260, 250, start=30, extent=120, style="arc", outline="#FF0000", width=2),
        ops.arc(270, 150, 370, 250, start=200, extent=90, style="chord", fill="yellow"),
        ops.text(20, 20, text="nw anchor", anchor="nw", font=FONT20, fill="#FF0000",
                 tags=["label"]),
        ops.text(80, 240, text="two\nlines", justify="center", font=FONT10),
    ]


def hexname(qcolor):
    return qcolor.name(QtGui.QColor.HexRgb).upper()


def poly_points(poly):
    return [(round(p.x(), 3), round(p.y(), 3)) for p in poly]


# --- pure helpers (Tk's geometry) --------------------------------------------------------

def test_parse_font_x11_and_tk_forms():
    f = render.parse_font(FONT20)
    assert (f.family, f.size, f.pixels, f.bold, f.italic) == ("helvetica", 20, True, True, False)
    f = render.parse_font("-adobe-times-medium-i-normal--*-140-75-75-p-0-iso8859-1")
    assert (f.family, f.size, f.pixels, f.bold, f.italic) == ("times", 14, False, False, True)
    f = render.parse_font(ops.DEFAULT_FONT)
    assert (f.family, f.size, f.pixels, f.bold) == ("helvetica", 12, True, False)
    f = render.parse_font("{Courier New} 10 bold italic underline")
    assert (f.family, f.size, f.pixels, f.bold, f.italic, f.underline) == (
        "courier new", 10, False, True, True, True)
    f = render.parse_font("Times")
    assert (f.family, f.size, f.pixels) == ("times", 12, True)
    assert render.parse_font(None) == render.parse_font(ops.DEFAULT_FONT)


def test_dash_patterns_follow_tk_dashconvert():
    # A dash string's lengths scale with the (rounded, at least 1) line width.
    assert render.dash_lengths("---", 4) == [24, 16, 24, 16, 24, 16]
    assert render.dash_lengths("-", 0) == [6, 4]
    assert render.dash_lengths(".", 2) == [4, 8]
    assert render.dash_lengths("_,", 1) == [8, 4, 4, 4]
    assert render.dash_lengths("- ", 1) == [6, 6]          # ' ' widens the previous gap
    # Lists are pixel lengths; an odd list repeats (as X does).
    assert render.dash_lengths((6, 4), 3) == [6, 4]
    assert render.dash_lengths((5,), 1) == [5, 5]
    assert render.dash_lengths(None, 1) is None
    assert render.dash_lengths("", 1) is None


def test_arrow_geometry_is_tk_configure_arrows():
    end, poly = render.arrow_geometry((0, 0), (100, 0), 3, (8, 12, 10))
    assert end == pytest.approx((94.8, 0))
    want = [(100, 0), (88, -10), (91.4, -1.5), (91.4, 1.5), (88, 10)]
    for got, exp in zip(poly, want):
        assert got == pytest.approx(exp)
    # Rotated: pointing down (+y) from (0, 0) to (0, 50) with Tk's default shape.
    end, poly = render.arrow_geometry((0, 0), (0, 50), 1, (8, 10, 3))
    frac = 0.5 / 3
    assert end == pytest.approx((0, 50 - (frac * 10 + 8 * (1 - frac) / 2)))
    assert poly[0] == pytest.approx((0, 50))
    assert poly[1] == pytest.approx((3, 40))
    assert poly[4] == pytest.approx((-3, 40))
    # A zero-length segment doesn't divide by zero.
    end, poly = render.arrow_geometry((5, 5), (5, 5), 1, (8, 10, 3))
    assert end == pytest.approx((5, 5))


def test_bezier_segments_are_tk_make_bezier_curve():
    # Two points: no curve.
    assert render.bezier_segments([(0, 0), (10, 0)]) == []
    # Three open points: one cubic from p0 to p2 (Tk's first/last-segment rules).
    (s, c1, c2, e), = render.bezier_segments([(0, 0), (30, 60), (60, 0)])
    assert s == (0, 0) and e == (60, 0)
    assert c1 == pytest.approx((20, 40)) and c2 == pytest.approx((40, 40))
    # Four points: the join is the midpoint of the middle segment.
    segs = render.bezier_segments([(0, 0), (12, 0), (12, 12), (24, 12)])
    assert len(segs) == 2 and segs[0][3] == pytest.approx((12, 6)) == segs[1][0]
    # Closed (first == last): every segment starts and ends at a midpoint.
    pts = [(0, 0), (20, 0), (20, 20), (0, 0)]
    segs = render.bezier_segments(pts)
    assert len(segs) == 3
    assert segs[0][0] == pytest.approx((10, 10)) and segs[-1][3] == pytest.approx((10, 10))


@pytest.mark.parametrize("anchor,origin", [
    ("nw", (10, 20)), ("n", (0, 20)), ("ne", (-10, 20)),
    ("w", (10, 15)), ("center", (0, 15)), ("e", (-10, 15)),
    ("sw", (10, 10)), ("s", (0, 10)), ("se", (-10, 10)),
])
def test_text_origin_is_tk_compute_text_bbox(anchor, origin):
    assert render.text_origin(anchor, 10, 20, 20, 10) == origin


def test_text_origin_rounds_like_tk():
    # ROUND(x) first, then integer halves of the size.
    assert render.text_origin("center", 10.6, 20.4, 21, 11) == (11 - 10, 20 - 5)


# --- Qt items ----------------------------------------------------------------------------

@pytest.fixture
def scene(qapp):
    s = render.render(styled(), width=400, height=260)
    yield s
    s.clear()


def items_in_order(scene):
    return render.op_items(scene)


def test_op_items_are_the_top_level_items(qapp):
    scene = render.render([ops.line(0, 0, 10, 10, arrow="both"), ops.rectangle(0, 0, 5, 5)])
    assert len(scene.items()) == 4
    items = render.op_items(scene)
    assert [type(i) for i in items] == [QtWidgets.QGraphicsPathItem,
                                        QtWidgets.QGraphicsRectItem]
    assert all(i.topLevelItem() == items[0] for i in items[0].childItems())
    assert len(scene.items()) == 4                        # nothing removed


def test_one_top_level_item_per_op_in_draw_order(scene):
    items = items_in_order(scene)
    assert len(items) == len(styled())
    for k, (item, op) in enumerate(zip(items, styled())):
        assert item.zValue() == k
        assert item.data(render.OP_ROLE) == op
        assert item.data(render.TAGS_ROLE) == list(op.tags)
    assert scene.sceneRect() == QtCore.QRectF(0, 0, 400, 260)


def test_rectangle_items(scene):
    bg, *_ = items_in_order(scene)
    assert isinstance(bg, QtWidgets.QGraphicsRectItem)
    assert bg.rect() == QtCore.QRectF(5, 5, 390, 250)
    assert bg.pen().style() == Qt.NoPen                   # outline ''
    assert hexname(bg.brush().color()) == "#EEEEEE"
    assert bg.brush().style() == Qt.SolidPattern
    stippled = items_in_order(scene)[5]
    assert stippled.pen().widthF() == 4
    assert stippled.pen().joinStyle() == Qt.MiterJoin
    assert stippled.brush().style() == Qt.TexturePattern  # -stipple gray75
    img = stippled.brush().textureImage()
    on = sum(img.pixelColor(x, y).alpha() > 0 for x in range(img.width())
             for y in range(img.height()))
    assert on / (img.width() * img.height()) == 0.75
    assert hexname(img.pixelColor(0, 0) if img.pixelColor(0, 0).alpha() else
                   img.pixelColor(1, 0)) == "#BFBFBF"     # X11 gray75, not Qt's


def test_line_items(scene):
    items = items_in_order(scene)
    smooth = items[1]
    assert isinstance(smooth, QtWidgets.QGraphicsPathItem)
    pen = smooth.pen()
    assert hexname(pen.color()) == "#FF0000" and pen.widthF() == 3
    assert pen.capStyle() == Qt.FlatCap and pen.joinStyle() == Qt.RoundJoin
    assert smooth.brush().style() == Qt.NoBrush
    path = smooth.path()
    assert path.elementCount() == 4                       # moveTo + one cubic
    assert path.elementAt(1).type == QtGui.QPainterPath.CurveToElement
    # The end moves back to the arrow's neck before smoothing.
    end, poly = render.arrow_geometry((80, 120), (140, 200), 3, (8, 12, 10))
    last = path.elementAt(3)
    assert (last.x, last.y) == pytest.approx(end)
    heads = smooth.childItems()
    assert len(heads) == 1
    head = heads[0]
    assert isinstance(head, QtWidgets.QGraphicsPolygonItem)
    assert poly_points(head.polygon()) == [(round(x, 3), round(y, 3)) for x, y in poly]
    assert hexname(head.brush().color()) == "#FF0000" and head.pen().style() == Qt.NoPen

    dashed = items[2]
    assert dashed.pen().style() == Qt.CustomDashLine
    # Qt measures dashes in pen widths: "---" at width 2 is 12 px on, 8 px off.
    assert dashed.pen().dashPattern() == [6, 4, 6, 4, 6, 4]
    assert dashed.childItems() == []

    both = items[3]
    assert both.pen().widthF() == 1 and both.pen().capStyle() == Qt.RoundCap
    assert both.pen().dashPattern() == [6, 4]
    assert len(both.childItems()) == 2
    path = both.path()
    first, last = path.elementAt(0), path.elementAt(path.elementCount() - 1)
    assert first.x > 160 and last.x < 380                 # both ends shortened


def test_oval_polygon_arc_items(scene):
    items = items_in_order(scene)
    oval = items[4]
    assert isinstance(oval, QtWidgets.QGraphicsEllipseItem)
    assert oval.rect() == QtCore.QRectF(160, 70, 60, 60)
    assert hexname(oval.brush().color()) == "#CCFFDD"
    assert hexname(oval.pen().color()) == "#000080"       # 'navy blue'
    assert oval.pen().widthF() == 1                       # width 0 still draws 1 px in Tk

    poly = items[6]
    assert isinstance(poly, QtWidgets.QGraphicsPathItem)
    assert poly.brush().style() == Qt.NoBrush             # fill ''
    assert hexname(poly.pen().color()) == "#006400" and poly.pen().widthF() == 2
    # Smooth polygons are closed curves: 3 corners → 3 cubics.
    assert poly.path().elementCount() == 1 + 3 * 3

    arc = items[7]
    assert isinstance(arc, QtWidgets.QGraphicsPathItem)
    assert arc.brush().style() == Qt.NoBrush              # style arc: never filled
    assert hexname(arc.pen().color()) == "#FF0000"
    # The arc runs counter-clockwise from 30° to 150° on the 100×100 circle at (210, 200)
    # (Qt approximates arcs with Béziers: within 0.05 px).
    start = arc.path().elementAt(0)
    assert (start.x, start.y) == pytest.approx(
        (210 + 50 * math.cos(math.radians(30)), 200 - 50 * math.sin(math.radians(30))), abs=0.05)
    end = arc.path().currentPosition()
    assert (end.x(), end.y()) == pytest.approx(
        (210 + 50 * math.cos(math.radians(150)), 200 - 50 * math.sin(math.radians(150))),
        abs=0.05)

    chord = items[8]
    assert hexname(chord.brush().color()) == "#FFFF00"
    assert chord.pen().widthF() == 1


def test_pieslice_goes_through_the_centre(qapp):
    scene = render.render([ops.arc(0, 0, 100, 100, start=0, extent=90)])
    item, = scene.items()
    assert item.path().contains(QtCore.QPointF(60, 40))
    assert not item.path().contains(QtCore.QPointF(40, 60))
    # The pieslice's outline closes at the centre.
    pts = [(item.path().elementAt(i).x, item.path().elementAt(i).y)
           for i in range(item.path().elementCount())]
    assert (50, 50) in [(round(x), round(y)) for x, y in pts]


def test_text_items(scene):
    items = items_in_order(scene)
    nw = items[9]
    assert isinstance(nw, render.TextItem)
    assert nw.text() == "nw anchor"
    assert hexname(nw.color()) == "#FF0000"
    font = nw.font()
    assert font.pixelSize() == 20 and font.bold()
    rect = nw.boundingRect()
    assert (rect.left(), rect.top()) == (20, 20)          # anchor nw: (x, y) is the top left
    metrics = QtGui.QFontMetrics(font)
    assert rect.width() == metrics.horizontalAdvance("nw anchor")
    assert rect.height() == metrics.height()

    two = items[10]
    assert two.lines() == ["two", "lines"] and two.justify() == "center"
    assert two.font().pixelSize() == 10
    rect = two.boundingRect()
    metrics = QtGui.QFontMetrics(two.font())
    assert rect.height() == 2 * metrics.height()
    assert rect.center().x() == pytest.approx(80, abs=1)  # anchor center
    assert rect.center().y() == pytest.approx(240, abs=1)
    # Centred justification: each line is centred in the box.
    xs = two.line_positions()
    assert xs[0][0] == pytest.approx(rect.left() + (rect.width()
                                     - metrics.horizontalAdvance("two")) / 2)
    assert xs[1][0] == pytest.approx(rect.left())


def test_text_anchors_place_the_box(qapp):
    measure = render.QtMeasure()
    for anchor in ("nw", "w", "sw", "center", "e", "s"):
        scene = render.render([ops.text(100, 50, text="Abc", anchor=anchor, font=FONT10)])
        item, = scene.items()
        w, h = measure("Abc", FONT10)
        assert (item.boundingRect().left(), item.boundingRect().top()) == \
            render.text_origin(anchor, 100, 50, w, h)


def test_empty_text_has_one_empty_line(qapp):
    scene = render.render([ops.text(10, 10)])
    item, = scene.items()
    assert item.lines() == [""] and item.boundingRect().width() == 0


def test_colours_use_tk_names_not_qt_names(qapp):
    scene = render.render([ops.rectangle(0, 0, 10, 10, fill="gray", outline="green"),
                           ops.line(0, 0, 10, 10, fill="maroon"),
                           ops.text(5, 5, text="x", fill="purple")])
    rect, line, text = items_in_order(scene)
    assert hexname(rect.brush().color()) == "#BEBEBE"
    assert hexname(rect.pen().color()) == "#00FF00"
    assert hexname(line.pen().color()) == "#B03060"
    assert hexname(text.color()) == "#A020F0"
    assert render.qcolor("#abc").name().upper() == "#A0B0C0"
    assert render.qcolor(None) is None


def test_unset_colours(qapp):
    scene = render.render([ops.oval(0, 0, 10, 10, outline=""),
                           ops.line(0, 0, 10, 10, fill="", arrow="last"),
                           ops.polygon(0, 0, 10, 0, 5, 5)])
    oval, line, poly = items_in_order(scene)
    assert oval.pen().style() == Qt.NoPen and oval.brush().style() == Qt.NoBrush
    assert line.pen().style() == Qt.NoPen
    assert line.childItems()[0].brush().style() == Qt.NoBrush
    assert isinstance(poly, QtWidgets.QGraphicsPolygonItem)
    assert poly.pen().style() == Qt.NoPen and hexname(poly.brush().color()) == "#000000"


def test_qfont_mapping(qapp):
    f = render.qfont(FONT20)
    assert f.pixelSize() == 20 and f.bold() and not f.italic()
    assert f.styleHint() == QtGui.QFont.SansSerif
    f = render.qfont("{Times} 12 italic")
    assert f.pixelSize() == 16 and f.italic() and f.styleHint() == QtGui.QFont.Serif
    f = render.qfont("-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1")
    assert f.pixelSize() == 13 and f.styleHint() == QtGui.QFont.TypeWriter
    assert render.qfont(FONT20) is render.qfont(FONT20)   # cached


def test_qt_measure(qapp):
    measure = render.QtMeasure()
    w1, h1 = measure("2", FONT20)
    w2, h2 = measure("22", FONT20)
    assert w2 > w1 > 0 and h1 == h2 == QtGui.QFontMetrics(render.qfont(FONT20)).height()
    assert measure("", FONT20) == (0, h1)
    assert measure("a\nbbbb", FONT10)[1] == 2 * measure("a", FONT10)[1]
    assert all(isinstance(v, int) for v in measure("xyz", FONT10))


def test_render_reuses_a_scene_and_returns_items(qapp):
    scene = QtWidgets.QGraphicsScene()
    items = render.add_ops(scene, styled())
    assert len(items) == len(styled())
    again = render.render(styled()[:2], scene=scene)
    assert again is scene and len(items_in_order(scene)) == 2


def test_render_image_pixels(qapp):
    img = render.render_image(styled(), 400, 260)
    assert (img.width(), img.height()) == (400, 260)
    assert hexname(img.pixelColor(390, 250)) == "#EEEEEE"   # the 'bg' rectangle
    assert hexname(img.pixelColor(2, 2)) == "#FFFFFF"       # the canvas background
    assert hexname(img.pixelColor(190, 100)) == "#CCFFDD"   # inside the oval
    img = render.render_image([], 10, 10, background="gray")
    assert hexname(img.pixelColor(5, 5)) == "#BEBEBE"


def test_from_dict_round_trips_the_goldens():
    for case in golden.load("gui_smoke"):
        for item in case.get("items", []):
            op = ops.from_dict(item)
            assert op.to_dict()["type"] == item["type"]
            assert op.to_dict()["coords"] == [float(c) for c in item["coords"]]
            assert op.to_dict()["opts"] == item["opts"]
            assert sorted(op.tags) == sorted(item["tags"])


# --- every golden ----------------------------------------------------------------------

def _golden_items(case):
    """Each item list in a golden case (cases hold one canvas)."""
    return case.get("items") or []


@pytest.mark.parametrize("name", DRAWING_GOLDENS)
def test_every_golden_renders(qapp, name):
    scene = QtWidgets.QGraphicsScene()
    n = 0
    for case in golden.load(name):
        items = [ops.from_dict(d) for d in _golden_items(case)]
        render.render(items, scene=scene)
        top = render.op_items(scene)
        assert len(top) == len(items)
        n += len(items)
    assert n > 0
    scene.clear()


def test_composed_view_renders_with_qt_metrics(qapp):
    import gui_recipes
    from seqsee.gui import snapshot
    from seqsee.gui.draw import views

    gui_recipes.build("groups_relations")
    snap = snapshot.take()
    measure = render.QtMeasure()
    comp = views.compose(0, snap, 780, 450, measure=measure)
    scene = render.render(comp.ops, width=780, height=450)
    assert len(render.op_items(scene)) == len(comp.ops)
    texts = [i for i in scene.items() if isinstance(i, render.TextItem)]
    assert len(texts) == sum(op.TYPE == "text" for op in comp.ops), comp.died


def test_every_perl_screenshot_has_a_screen_case():
    assert {p.stem for p in PERL_DIR.glob("*.png")} == (
        set(SCREENS) | set(WINDOW_SCREENS) | set(CONTROL_SCREENS))
    for name in SCREENS:
        case = screen_case(name)
        assert canvas_size(case)[0] > 0


@pytest.mark.parametrize("name", sorted(SCREENS))
def test_qt_screenshots(qapp, tmp_path, request, name):
    """Render the golden case of each Perl screenshot. With ``pytest --write-screens`` the
    PNGs go to python/docs/gui/screens/ (for the side-by-side look); otherwise to a temp dir."""
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    out_dir.mkdir(parents=True, exist_ok=True)
    case = screen_case(name)
    w, h = canvas_size(case)
    path = out_dir / f"{name}.png"
    render.save_png([ops.from_dict(d) for d in _golden_items(case)], path, w, h)
    img = QtGui.QImage(str(path))
    assert (img.width(), img.height()) == (w, h)
    if case.get("items"):
        assert any(img.pixelColor(x, y) != QtGui.QColor("#FFFFFF")
                   for x in range(0, w, 7) for y in range(0, h, 7))


def test_screens_are_committed():
    """The side-by-side Qt screenshots exist for every Perl one."""
    missing = [n for n in SCREENS if not (SCREENS_DIR / f"{n}.png").exists()]
    assert not missing, json.dumps(missing)
