"""The Qt canvas: Tk canvas commands on a QGraphicsScene (loop0003 item 02).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The reference is a real tkinter Canvas given the same commands
(canvas_streams.py, under Xvfb), committed as data/tk-display-lists.json:

- every stream (the two sgl-tcl fixtures and a synthetic one) replayed into the
  Qt canvas gives Tk's display list: ids, stacking order, kinds, coordinates,
  options, tags, hidden state, and every bbox and canvasx answer.  With Tk's own
  text metrics (recorded in the reference) everything is exact; with Qt's fonts,
  everything but the text items' extents is exact, and those are within
  TEXT_TOLERANCE (test_qt_fonts.py tests the fonts);
- the slow tier checks that the reference is what Tk says today;
- the canvas accepts every command and option of the item-01 inventory, and
  rejects others as Tk does;
- the scene follows the display list (items, z order, visibility, background);
- the SGL fixture drawn by the Qt canvas has render_sgl_fixture.py's pixels
  (hidden items, raise, delete, retag, move, erase, clear, fills) and looks
  like docs/screenshots/panels/sgl-fixture-python.png.
"""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import threading
from pathlib import Path

import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")

import canvas_streams as cs  # noqa: E402

HERE = Path(__file__).resolve().parent
PY = HERE.parent
REPO = PY.parent
SCREENSHOT = REPO / "docs" / "screenshots" / "panels" / "sgl-fixture-python.png"
QT_SCREENSHOT = REPO / "docs" / "screenshots" / "panels" / "sgl-fixture-qt.png"

# Qt's fonts against Tk's (core X fonts): the text items' extents may differ by
# this many pixels on each side.  Item 02 measured heights within 5 pixels (Qt's
# line metrics); item 03's ink metrics (metacat/qt/fonts.py) brought them, and
# the widths, within 2.
TEXT_TOLERANCE = 2


@pytest.fixture(scope="module")
def reference():
    return json.loads(cs.REFERENCE.read_text())


class TkMetrics:
    """Tk's own text metrics, from the reference: the Qt canvas's bbox rules then
    give Tk's answers exactly."""

    def __init__(self, metrics):
        self.metrics = metrics

    def text_width(self, font, line):
        return self.metrics["widths"][cs.font_key(font) + "|" + line]

    def linespace(self, font):
        return self.metrics["linespace"][cs.font_key(font)]

    def ascent(self, font):
        return (self.linespace(font) * 4) // 5


def qt_run(stream, measure=None):
    """the stream replayed into Qt canvases: (answers, {name: dump}, canvases)"""
    from metacat.qt.canvas import QtCanvas, color_rgb
    commands = cs.streams()[stream]
    canvases = {}
    for _, name, _ in commands:
        canvases.setdefault(name, QtCanvas(measure=measure))
    answers = cs.replay(commands, canvases)
    return answers, {name: cs.dump(c, color_rgb) for name, c in canvases.items()}, canvases


def has_text(dump):
    return any(i["type"] == "text" for i in dump["items"])


def close(a, b, tolerance):
    if a is None or b is None:
        return a == b
    return len(a) == len(b) and all(abs(x - y) <= tolerance for x, y in zip(a, b))


def test_reference_covers_every_stream(reference):
    assert set(reference["streams"]) == {"v1", "v2", "synthetic"}
    for name, data in reference["streams"].items():
        assert data["answers"], name
        assert data["metrics"]["widths"], name
    assert len(reference["streams"]["synthetic"]["s1"]["items"]) > 150


@pytest.mark.parametrize("stream", ["v1", "v2", "synthetic"])
def test_display_list_is_tks_with_tks_metrics(qapp, reference, stream):
    """everything exact: the commands' semantics and Tk's bbox rules"""
    ref = reference["streams"][stream]
    answers, dumps, _ = qt_run(stream, TkMetrics(ref["metrics"]))
    assert set(dumps) == set(ref) - {"answers", "metrics"}
    for name, dump in dumps.items():
        want = ref[name]
        assert [i["id"] for i in dump["items"]] == [i["id"] for i in want["items"]], name
        for got, exp in zip(dump["items"], want["items"]):
            assert got == exp, (name, exp["id"])
        assert dump["bbox all"] == want["bbox all"], name
    assert answers == ref["answers"]


@pytest.mark.parametrize("stream", ["v1", "v2", "synthetic"])
def test_display_list_with_qt_fonts(qapp, reference, stream):
    """with Qt's own fonts: all exact but the extents of text items, which are
    close to Tk's"""
    ref = reference["streams"][stream]
    answers, dumps, _ = qt_run(stream)
    for name, dump in dumps.items():
        want = ref[name]
        assert len(dump["items"]) == len(want["items"])
        for got, exp in zip(dump["items"], want["items"]):
            if exp["type"] == "text":
                assert close(got.pop("bbox"), exp["bbox"], TEXT_TOLERANCE), exp
                exp = {k: v for k, v in exp.items() if k != "bbox"}
            assert got == exp, (name, exp["id"])
        tolerance = TEXT_TOLERANCE if has_text(want) else 0
        assert close(dump["bbox all"], want["bbox all"], tolerance), name
    assert len(answers) == len(ref["answers"])
    for got, exp in zip(answers, ref["answers"]):
        if isinstance(exp[2], list):
            assert got[:2] == exp[:2] and close(got[2], exp[2], TEXT_TOLERANCE), exp
        else:
            assert got == exp


@pytest.mark.slow
@pytest.mark.skipif(shutil.which("xvfb-run") is None, reason="needs xvfb-run")
def test_reference_is_what_tk_says(tmp_path, reference):
    """canvas_streams.py under xvfb-run gives the committed reference (text
    extents and metrics within the tolerance: they come from the machine's
    fonts)"""
    out = tmp_path / "tk.json"
    env = dict(os.environ)
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run(["xvfb-run", "-a", "-s", "-screen 0 1280x1024x24", sys.executable,
                           str(HERE / "canvas_streams.py"), str(out)],
                          cwd=PY, env=env, capture_output=True, text=True, timeout=120)
    assert proc.returncode == 0, proc.stdout + proc.stderr
    fresh = json.loads(out.read_text())
    for stream, data in reference["streams"].items():
        new = fresh["streams"][stream]
        assert set(new) == set(data)
        for name in set(data) - {"answers", "metrics"}:
            for got, exp in zip(new[name]["items"], data[name]["items"], strict=True):
                if exp["type"] == "text":
                    assert close(got.pop("bbox"), exp.pop("bbox"), TEXT_TOLERANCE)
                    exp = dict(exp, bbox=None)
                    got = dict(got, bbox=None)
                assert got == exp
        assert [a[:2] for a in new["answers"]] == [a[:2] for a in data["answers"]]


# ---------------------------------------------------------------------------
# The inventory, errors, threads

def test_every_inventoried_command_and_option_is_accepted(qapp):
    from metacat.qt.canvas import QtCanvas
    inventory = json.loads((HERE / "data" / "tk-canvas-commands.json").read_text())
    values = {"-dash": "- ", "-fill": "red", "-outline": "blue", "-width": 2,
              "-state": "hidden", "-tags": "t", "-start": 10, "-extent": 100,
              "-style": "chord", "-anchor": "nw", "-font": ("helvetica", -12),
              "-text": "x"}
    for command, entry in inventory["commands"].items():
        c = QtCanvas()
        if command.startswith("create "):
            kind = command.split()[1]
            coords = [1, 2] if kind == "text" else [1, 2, 30, 40, 5, 60]
            if kind in ("rectangle", "oval", "arc"):
                coords = coords[:4]
            for option in entry["options"]:
                assert c.tcl("create", kind, *coords, option, values[option]) >= 1
        else:
            c.tcl("create", "rectangle", 1, 2, 3, 4, "-tags", "t")
            args = {"bbox": ["1"], "delete": ["t"], "move": ["t", 1, 2], "raise": ["t", "all"],
                    "scale": ["t", 0, 0, 2, 0.5], "canvasx": [3], "canvasy": [4],
                    "itemconfigure": ["t", *[a for o in entry["options"]
                                             for a in (o, values[o])]]}[command]
            c.tcl(command, *args)


def test_tk_errors(qapp):
    from metacat.qt.canvas import QtCanvas, TclError
    c = QtCanvas()
    for args in [("create", "window", 1, 2), ("create", "rectangle", 1, 2, 3, 4, "-bogus", 1),
                 ("create", "rectangle", 1, 2, 3), ("frobnicate",),
                 ("create", "rectangle", 1, 2, 3, 4, "-state", "dormant"),
                 ("create", "arc", 1, 2, 3, 4, "-style", "wedge"),
                 ("create", "line", 1, 2, 3, 4, "-fill", "no such colour")]:
        with pytest.raises(TclError):
            c.tcl(*args)
    assert c.tcl("find", "all") == []


def test_ids_are_never_reused(qapp):
    from metacat.qt.canvas import QtCanvas
    c = QtCanvas()
    assert c.tcl("create", "line", 0, 0, 1, 1) == 1
    c.tcl("delete", "all")
    assert c.tcl("create", "line", 0, 0, 1, 1) == 2
    assert c.tcl("find", "all") == [2]


def test_commands_from_many_threads(qapp):
    """the engine and the resize listener draw from their own threads: the
    display list stays consistent (unique ids, every item there)"""
    from metacat.qt.canvas import QtCanvas
    c = QtCanvas()
    ids = []

    def draw(k):
        for i in range(200):
            ids.append(c.tcl("create", "rectangle", i, k, i + 5, k + 5, "-tags", "t%d" % k))
            c.tcl("move", "t%d" % k, 1, 0)
            c.tcl("bbox", "t%d" % k)

    threads = [threading.Thread(target=draw, args=(k,)) for k in range(8)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    assert sorted(ids) == list(range(1, 1601))
    assert sorted(c.tcl("find", "all")) == list(range(1, 1601))
    c.sync()
    assert len(c.scene.items()) == 1600


# ---------------------------------------------------------------------------
# The scene

def scene_items(c):
    return sorted(c.scene.items(), key=lambda g: g.zValue())


def test_scene_follows_the_display_list(qapp):
    from metacat.gui import colors
    from metacat.qt.canvas import QtCanvas
    c = QtCanvas()
    c.tcl("create", "rectangle", 0, 0, 10, 10, "-fill", "red", "-tags", "a")
    c.tcl("create", "oval", 0, 0, 10, 10, "-fill", "blue", "-tags", "b")
    c.tcl("create", "text", 5, 5, "-text", "hi", "-font", ("helvetica", -12), "-tags", "a")
    assert c.scene.items() == []          # nothing until the GUI thread syncs
    c.sync()
    assert [g.item_id for g in scene_items(c)] == [1, 2, 3]
    c.tcl("raise", "a")
    c.tcl("itemconfigure", "b", "-state", "hidden")
    c.tcl("delete", "3")
    c.sync()
    assert [g.item_id for g in scene_items(c)] == [2, 1]
    assert [g.isVisible() for g in scene_items(c)] == [False, True]
    c.tcl("move", "1", 5, 7)
    c.sync()
    assert scene_items(c)[1].sceneBoundingRect().center().toTuple() == (10, 12)
    c.set_background_color_bang(colors.Rgb(255, 255, 240))
    assert c.get_background_color() == colors.Rgb(255, 255, 240)
    c.sync()
    assert c.scene.backgroundBrush().color().name() == "#fffff0"
    c.tcl("delete", "all")
    c.sync()
    assert c.scene.items() == []


def render(c, w, h):
    from PySide6.QtCore import QRectF
    from PySide6.QtGui import QImage, QPainter
    c.sync()
    image = QImage(w, h, QImage.Format_RGB32)
    painter = QPainter(image)
    c.scene.render(painter, QRectF(0, 0, w, h), QRectF(0, 0, w, h))
    painter.end()
    return image


def render_sgl_fixture():
    """sgl-fixture.scm drawn by metacat/gui/sgl.py on a Qt canvas, as
    render_sgl_fixture.py draws it on Tk: viewport v1 (640 x 480), the fonts
    measured on a Qt hidden canvas (fonts.ss's *hidden-canvas*)"""
    import test_sgl as t
    from metacat.gui import colors, fonts, sgl
    from metacat.qt.canvas import QtCanvas
    saved = fonts.g_hidden_canvas
    try:
        fonts.g_hidden_canvas = QtCanvas()
        fonts.load(["times", "helvetica", "courier"])
        sgl.load()
        canvas = QtCanvas()
        sections, viewports = t.fixture_forms()
        spec = next(v for v in viewports if v[0] == "v1")
        vp = t.make_viewport(sgl, canvas, *spec[1:])
        t.run_ops(sgl, fonts, colors, vp, sections["ops"], t.make_fonts(fonts, sections))
    finally:
        fonts.g_hidden_canvas = saved
    return canvas


def test_sgl_fixture_renders_like_tk(qapp):
    """render_sgl_fixture.py's pixel checks on the Qt canvas, and the picture
    against the tkinter one (inspected: docs/screenshots/panels/sgl-fixture-qt.png)"""
    import render_sgl_fixture as r
    from PySide6.QtGui import QColor, QImage
    from metacat.gui import colors
    image = render(render_sgl_fixture(), r.W, r.H)
    out = cs.HERE / "screenshots-qt" / "test_sgl_fixture_renders_like_tk" / "sgl-fixture-qt.png"
    out.parent.mkdir(parents=True, exist_ok=True)
    image.save(str(out))
    failures = []
    for (x, y), name, why in r.CHECKS:
        want = colors.swl_color(cs.chez.String(name))
        got = QColor(image.pixel(x, r.H - y))
        if (got.red(), got.green(), got.blue()) != (want.r, want.g, want.b):
            failures.append((x, y, name, why, got.name()))
    assert failures == []
    tk = QImage(str(SCREENSHOT)).convertToFormat(QImage.Format_RGB32)
    assert (tk.width(), tk.height()) == (r.W, r.H)
    same = sum(1 for y in range(r.H) for x in range(r.W)
               if _near(image.pixel(x, y), tk.pixel(x, y)))
    assert same / (r.W * r.H) > 0.97, same / (r.W * r.H)


def test_sgl_fixture_stream_renders(qapp):
    """the frozen v1 stream replayed into the Qt canvas passes the same pixel
    checks (its texts sit where the oracle's fixed metric put them)"""
    import render_sgl_fixture as r
    from PySide6.QtGui import QColor
    from metacat.gui import colors
    _, _, canvases = qt_run("v1")
    image = render(canvases["v1"], r.W, r.H)
    for (x, y), name, why in r.CHECKS:
        want = colors.swl_color(cs.chez.String(name))
        got = QColor(image.pixel(x, r.H - y))
        assert (got.red(), got.green(), got.blue()) == (want.r, want.g, want.b), why


def _near(p, q):
    return all(abs(((p >> s) & 255) - ((q >> s) & 255)) <= 24 for s in (0, 8, 16))
