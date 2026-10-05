"""The Qt GUI's fonts and colours (loop0003 item 03).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

- the font mapping: a Tk font word (face size style..., a Tcl list string,
  the -option form, TkDefaultFont) gives the family, pixel size and styles Tk
  reads from it, with Tk's errors; the QFont carries them;
- Qt picks the faces Tk picks: the widths of data/tk-fonts-colors.json's
  samples (Tk under Xvfb) agree, and the heights are close (Qt measures a
  font's ink over Latin-1, as X's core fonts do, not its line metrics);
- measurement is consistent: a text item's bbox is the measured width (plus
  Tk's cursor pixel) by the linespace, the panels' fonts.ss measures on a Qt
  hidden canvas with the same numbers, and the drawn ink lies inside the bbox;
- the small Coderack labels keep their i's and l's;
- every colour name of colors.py is read as Tk reads it, and Qt paints it so.
"""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

QtGui = pytest.importorskip("PySide6.QtGui")

HERE = Path(__file__).resolve().parent
REFERENCE = HERE / "data" / "tk-fonts-colors.json"

import tk_fonts_colors as tfc  # noqa: E402


@pytest.fixture(scope="module")
def reference():
    return json.loads(REFERENCE.read_text())


# ---------------------------------------------------------------------------
# The mapping table

# font word -> (family, pixel size, bold, italic, underline, overstrike)
TABLE = [
    (("helvetica", -11), ("helvetica", 11, False, False, False, False)),
    (("helvetica", -11, "normal"), ("helvetica", 11, False, False, False, False)),
    (("times", -22, "bold", "italic"), ("times", 22, True, True, False, False)),
    (("times", 12, "bold"), ("times", 16, True, False, False, False)),
    (("helvetica", 8), ("helvetica", 11, False, False, False, False)),
    (("helvetica", 18, "bold", "italic"), ("helvetica", 24, True, True, False, False)),
    (("courier", -11, "bold", "roman", "normal"), ("courier", 11, False, False, False, False)),
    (("courier", 10, "underline", "overstrike"), ("courier", 13, False, False, True, True)),
    ("helvetica -11", ("helvetica", 11, False, False, False, False)),
    ("{times new roman} 14 bold", ("times new roman", 19, True, False, False, False)),
    ("{palatino linotype} -9 italic", ("palatino linotype", 9, False, True, False, False)),
    ("-family courier -size -11 -weight bold -slant italic",
     ("courier", 11, True, True, False, False)),
    ("-family {times new roman} -size 12 -underline 1",
     ("times new roman", 16, False, False, True, False)),
    ("TkDefaultFont", ("helvetica", 12, False, False, False, False)),
    ("", ("helvetica", 12, False, False, False, False)),
    (("helvetica",), ("helvetica", 12, False, False, False, False)),
    (("helvetica", 0), ("helvetica", 12, False, False, False, False)),
    (("helvetica", "-7"), ("helvetica", 7, False, False, False, False)),
]


@pytest.mark.parametrize("font,want", TABLE, ids=[repr(f) for f, _ in TABLE])
def test_font_mapping_table(font, want):
    from metacat.qt.fontspec import parse_font
    spec = parse_font(font)
    assert (spec.family, spec.pixels, spec.bold, spec.italic, spec.underline,
            spec.overstrike) == want


@pytest.mark.parametrize("font,message", [
    (("helvetica", -11, "foo"), 'unknown font style "foo"'),
    (("helvetica", 12, "Bold"), 'unknown font style "Bold"'),
    (("helvetica", "10.6"), 'expected integer but got "10.6"'),
    ("helvetica -10.6", 'expected integer but got "-10.6"'),
    ("-family courier -weight heavy", 'bad -weight value "heavy": must be normal, or bold'),
])
def test_font_errors_are_tks(font, message):
    from metacat.qt.displaylist import TclError
    from metacat.qt.fontspec import parse_font
    with pytest.raises(TclError) as e:
        parse_font(font)
    assert str(e.value) == message


@pytest.mark.parametrize("font,want", TABLE, ids=[repr(f) for f, _ in TABLE])
def test_qfont_carries_the_mapping(qapp, font, want):
    from metacat.qt.fonts import qfont
    f = qfont(font)
    family, pixels, bold, italic, underline, overstrike = want
    assert f.family().lower() == family
    assert f.pixelSize() == pixels
    assert (f.bold(), f.italic(), f.underline(), f.strikeOut()) == (
        bold, italic, underline, overstrike)


def test_the_canvas_rejects_bad_fonts_as_tk_does(qapp):
    from metacat.qt.canvas import QtCanvas
    from metacat.qt.displaylist import TclError
    c = QtCanvas()
    with pytest.raises(TclError, match='unknown font style "heavy"'):
        c.tcl("create", "text", 0, 0, "-text", "x", "-font", ("helvetica", -11, "heavy"))
    assert not c.tcl("find", "all")


# ---------------------------------------------------------------------------
# Qt's faces and metrics against Tk's

def test_reference_covers_the_fonts_and_colours(reference):
    from metacat.gui import colors
    assert set(reference["fonts"]) == {tfc.font_key(f) for f in tfc.FONTS}
    assert set(reference["colors"]) == {n for n, *_ in colors._COLOR_TABLE}


def test_widths_are_tks(qapp, reference):
    """the same faces: 97% of the samples' widths are Tk's, 98% within a pixel.
    The rest are 22-pixel bold Helvetica and 24-point Times, where the X
    server's hinting of the Type 1 files and FreeType's of the OpenType ones
    round some advances differently (up to 6 pixels over 13 letters)."""
    from metacat.qt.fonts import QtMetrics
    m = QtMetrics()
    exact = near = total = 0
    for font in tfc.FONTS:
        for sample, want in reference["fonts"][tfc.font_key(font)]["widths"].items():
            got = m.text_width(font, sample)
            assert abs(got - want) <= 6, (font, sample, got, want)
            exact += got == want
            near += abs(got - want) <= 1
            total += 1
    assert exact >= 0.96 * total and near >= 0.98 * total, (exact, near, total)


def test_heights_are_close_to_tks(qapp, reference):
    """Qt's line metrics (the OS/2 table's win ascent) are up to 7 pixels taller
    than Tk's core fonts; the ink of Latin-1 is within 3, mostly within 1, and
    the ascent (where the baseline goes) within 1"""
    from metacat.qt.fonts import QtMetrics
    m = QtMetrics()
    near = 0
    for font in tfc.FONTS:
        tk = reference["fonts"][tfc.font_key(font)]
        assert abs(m.linespace(font) - tk["linespace"]) <= 3, (font, m.linespace(font), tk)
        assert abs(m.ascent(font) - tk["ascent"]) <= 1, (font, m.ascent(font), tk)
        assert m.linespace(font) == m.ascent(font) + m.descent(font)
        near += abs(m.linespace(font) - tk["linespace"]) <= 1
    assert near >= 0.75 * len(tfc.FONTS)


# ---------------------------------------------------------------------------
# Measuring with the fonts the canvas draws with

FONTS = [("helvetica", -7, "normal"), ("helvetica", -11), ("times", -22, "bold", "italic"),
         ("times", 12, "italic"), ("courier", -14, "bold"), "{palatino linotype} -16"]


@pytest.mark.parametrize("font", FONTS, ids=repr)
def test_a_text_items_bbox_is_the_measured_text(qapp, font):
    from metacat.qt.canvas import QtCanvas
    from metacat.qt.fonts import metrics, qfont
    fm = QtGui.QFontMetrics(qfont(font))
    c = QtCanvas()
    for text in ("Bond builders", "Whole-string", "x", "two\nlonger lines"):
        lines = text.split("\n")
        width = max(fm.horizontalAdvance(line) for line in lines)
        assert width == max(metrics().text_width(font, line) for line in lines)
        item = c.tcl("create", "text", 10, 20, "-text", text, "-anchor", "nw", "-font", font)
        assert list(c.tcl("bbox", item)) == [9, 20, 10 + width + 1,
                                       20 + metrics().linespace(font) * len(lines)]


def render(canvas, w, h):
    canvas.sync()
    image = QtGui.QImage(w, h, QtGui.QImage.Format_RGB32)
    image.fill(QtGui.QColor("white"))
    painter = QtGui.QPainter(image)
    canvas.scene.render(painter, image.rect().toRectF(), image.rect().toRectF())
    painter.end()
    return image


def ink(image, box=None):
    """the bounding box of the non-white pixels [x1, y1, x2, y2), or None"""
    x1, y1, x2, y2 = box or (0, 0, image.width(), image.height())
    xs, ys = [], []
    for y in range(y1, y2):
        for x in range(x1, x2):
            if image.pixelColor(x, y).lightness() < 200:
                xs.append(x)
                ys.append(y)
    return (min(xs), min(ys), max(xs) + 1, max(ys) + 1) if xs else None


@pytest.mark.parametrize("font", FONTS, ids=repr)
def test_drawn_text_fills_its_bbox(qapp, font):
    """the scene draws with the measured font: the ink lies inside the bbox
    and spans its width (but the side bearings)"""
    from metacat.qt.canvas import QtCanvas
    c = QtCanvas()
    item = c.tcl("create", "text", 20, 20, "-text", "Bond builders Å", "-anchor", "nw",
                 "-font", font)
    x1, y1, x2, y2 = c.tcl("bbox", item)
    got = ink(render(c, 400, 120))
    assert got is not None
    assert x1 <= got[0] and got[2] <= x2 and y1 <= got[1] and got[3] <= y2, (got, (x1, y1, x2, y2))
    assert got[0] - x1 <= 3 and x2 - got[2] <= 4, (got, (x1, y1, x2, y2))
    assert got[1] - y1 <= 2, (got, (x1, y1, x2, y2))


def test_the_panels_measure_on_a_qt_hidden_canvas(qapp):
    """fonts.ss measures by creating a text on *hidden-canvas* and asking its
    bbox; after install() that canvas is a QtCanvas with the drawing fonts"""
    from metacat.gui import fonts as gfonts
    from metacat.objects import tell
    from metacat import chez
    from metacat.qt import fonts
    from metacat.qt.canvas import QtCanvas
    saved = (gfonts.g_hidden_canvas, gfonts._families, gfonts.serif, gfonts.sans_serif,
             gfonts.fancy)
    try:
        hidden = fonts.install()
        assert isinstance(hidden, QtCanvas) and gfonts.g_hidden_canvas is hidden
        assert gfonts._families == fonts.families()
        for face, size, style in (("helvetica", -7, ["normal"]), (gfonts.serif, -22, ["bold"]),
                                  (gfonts.fancy, -12, ["italic"])):
            f = gfonts.make_mfont(face, size, style)
            word = gfonts.swl.tcl_word(tell(f, "get-swl-font"))
            line = fonts.metrics().linespace(word)
            w, h, base = tell(f, "get-pixel-size", chez.String("Bond builders"))
            assert (w, h) == (fonts.metrics().text_width(word, "Bond builders") + 2, line)
            assert base == round(h / 5)
        assert not hidden.tcl("find", "all")
    finally:
        (gfonts.g_hidden_canvas, gfonts._families, gfonts.serif, gfonts.sans_serif,
         gfonts.fancy) = saved


def test_families_are_fontconfigs(qapp):
    """lower-case family names without Qt's [foundry]; fonts.ss's faces count
    when the face fontconfig gives them is not its generic fallback"""
    from metacat.gui import fonts as gfonts
    from metacat.qt import fonts
    names = fonts.families()
    assert names and names == sorted(set(names))
    assert all(n == n.lower() and "[" not in n for n in names)
    assert "xyzzy no such face" not in names
    fallback = QtGui.QFontInfo(QtGui.QFont("xyzzy no such face")).family()
    for face in gfonts.g_serif_faces + gfonts.g_sans_serif_faces + gfonts.g_fancy_faces:
        resolved = QtGui.QFontInfo(QtGui.QFont(face)).family()
        assert (face in names) == (resolved not in fonts.generic_faces()), (face, resolved)
    assert fallback in fonts.generic_faces()


def test_tiny_coderack_labels_keep_their_thin_letters(qapp):
    """the UFO of docs/anomalies_and_quirks.md: Tk's core fonts under Xvfb lose
    the i's and l's of 8- and 9-pixel Helvetica; Qt draws them.  From 7 pixels
    up, "illil" shows five strokes with paler gaps; at 5 and 6 pixels each
    letter is one pixel wide, and every column of the text is dark."""
    from metacat.qt.canvas import QtCanvas
    from metacat.qt.fonts import metrics
    for size in (5, 6, 7, 8, 9, 10, 11):
        font = ("helvetica", -size, "normal")
        c = QtCanvas()
        c.tcl("create", "text", 10, 10, "-text", "illil", "-anchor", "nw", "-font", font)
        image = render(c, 60, 30)
        darkest = [min(image.pixelColor(x, y).lightness() for y in range(30))
                   for x in range(60)]
        dark = [v < 160 for v in darkest]
        width = metrics().text_width(font, "illil")
        if size >= 7:
            # antialiased, a 1-pixel stroke can straddle two columns: count the
            # columns darker than their neighbours
            strokes = sum(1 for x in range(1, 59) if darkest[x] < 200
                          and darkest[x] < darkest[x - 1] and darkest[x] <= darkest[x + 1])
            assert strokes == 5, (size, darkest)
        else:
            assert width == 5 and all(dark[10:15]), (size, darkest)


# ---------------------------------------------------------------------------
# Colours

def test_colour_names_are_read_as_tk_reads_them(reference):
    from metacat.gui import colors
    from metacat.qt.displaylist import color_rgb
    tip403 = {"gray", "grey", "green", "maroon", "purple"}
    for name, r, g, b in colors._COLOR_TABLE:
        assert list(color_rgb(name)) == reference["colors"][name], name
        if name.lower() not in tip403:
            assert color_rgb(name) == (r, g, b), name
        assert color_rgb(name.upper()) == color_rgb(name)


def test_colour_constants_round_trip():
    """every Rgb of colors.py and constants.py, as swl.tcl_word gives it to the
    canvas, is the same colour"""
    from metacat.gui import colors, constants, swl
    from metacat.qt.displaylist import color_rgb
    rgbs = [v for m in (colors, constants) for v in vars(m).values()
            if isinstance(v, colors.Rgb)]
    rgbs += [colors.swl_color(n) for n, *_ in colors.g_color_names]
    assert len(rgbs) > 700
    for rgb in rgbs:
        assert color_rgb(swl.tcl_word(rgb)) == (rgb.r, rgb.g, rgb.b)


def test_qt_paints_the_colours_tk_reads(qapp):
    from metacat.gui import colors
    from metacat.qt.canvas import QtCanvas
    from metacat.qt.displaylist import color_rgb
    names = [n for n, *_ in colors._COLOR_TABLE[::37]] + ["#abc", "Grey", "MediumSeaGreen"]
    c = QtCanvas()
    for k, name in enumerate(names):
        c.tcl("create", "rectangle", 4 * k, 0, 4 * k + 4, 4, "-fill", name, "-outline", "")
    image = render(c, 4 * len(names), 4)
    for k, name in enumerate(names):
        got = image.pixelColor(4 * k + 2, 2)
        assert (got.red(), got.green(), got.blue()) == color_rgb(name), name


@pytest.mark.slow
@pytest.mark.skipif(shutil.which("xvfb-run") is None, reason="needs xvfb-run")
def test_reference_is_what_tk_says(tmp_path, reference):
    out = tmp_path / "tk.json"
    env = dict(os.environ)
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run(["xvfb-run", "-a", "-s", "-screen 0 1280x1024x24", sys.executable,
                           str(HERE / "tk_fonts_colors.py"), str(out)],
                          cwd=HERE.parent, env=env, capture_output=True, text=True,
                          timeout=120)
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert json.loads(out.read_text()) == reference
