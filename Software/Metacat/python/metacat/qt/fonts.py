"""Tk font words as QFonts, the text metrics the Qt canvas lays out with, and the
font families fonts.ss chooses its faces from.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026), for loop0003 items 02 and 03.

A Tk font word (``fontspec.parse_font``: face size style..., a Tcl list string,
the -option form, TkDefaultFont) becomes a QFont with the same family, pixel
size, weight, slant, underline and overstrike.  Qt asks fontconfig for the
face, as Tk does, so both draw "helvetica" with Nimbus Sans and "times" with
Nimbus Roman.  The canvas measures and draws with that same QFont:

- a text's width is QFontMetrics.horizontalAdvance, Tk's ``font measure``
  (the same hinted advances: 97% of the reference samples agree exactly);
- the ascent and descent are the ink of the font's Latin-1 glyphs, as X's core
  fonts compute them (the Tk of the tkinter GUI has no Xft), not Qt's line
  metrics, which follow the OS/2 table's win ascent and are up to 7 pixels
  taller than Tk's (docs/anomalies_and_quirks.md).  linespace = ascent +
  descent, as Tk's.

``families()`` stands for Tk's ``font families`` (swl:font-families): the
families Qt knows, lower-cased, plus the faces of fonts.ss's preference lists
that fontconfig maps onto a real face (``palatino`` onto P052, ``times new
roman`` onto Liberation Serif) rather than its generic fallback.  ``install()``
makes metacat/gui/fonts.py choose its faces from them and measure on a Qt
hidden canvas, so the panels lay out with the drawing fonts.
"""
from __future__ import annotations

import math
import threading

from PySide6.QtGui import QFont, QFontDatabase, QFontInfo, QFontMetrics, QRawFont

from metacat.qt.fontspec import parse_font

# the characters whose ink makes a font's ascent and descent (printable Latin-1)
LATIN_1 = [chr(c) for c in list(range(0x21, 0x7f)) + list(range(0xa1, 0x100))]

_fonts = {}
_lock = threading.Lock()


def _key(font):
    return tuple(parse_font(font))


def qfont(font):
    """port: Tk_GetFont: the QFont of a Tk font word (cached)"""
    key = _key(font)
    with _lock:
        f = _fonts.get(key)
        if f is None:
            spec = parse_font(font)
            f = QFont(spec.family)
            f.setPixelSize(spec.pixels)
            f.setBold(spec.bold)
            f.setItalic(spec.italic)
            f.setUnderline(spec.underline)
            f.setStrikeOut(spec.overstrike)
            f.setHintingPreference(QFont.PreferFullHinting)
            _fonts[key] = f
        return f


def ink_extents(f):
    """the (ascent, descent) of a QFont's Latin-1 ink, in whole pixels"""
    raw = QRawFont.fromFont(f)
    top = bottom = 0.0
    for c in LATIN_1:
        glyphs = raw.glyphIndexesForString(c)
        if glyphs and glyphs[0]:
            r = raw.boundingRect(glyphs[0])
            top, bottom = max(top, -r.top()), max(bottom, r.bottom())
    if top == 0 and bottom == 0:          # no outlines: Qt's line metrics
        fm = QFontMetrics(f)
        return fm.ascent(), fm.descent()
    return math.ceil(top - 1e-6), math.ceil(bottom - 1e-6)


class QtMetrics:
    """The measurer of the display list: whole pixels, as Tk measures, from the
    QFont the canvas draws with (cached per font and string; any thread)."""

    def __init__(self):
        self._metrics = {}
        self._extents = {}
        self._widths = {}
        self._lock = threading.Lock()

    def _fm(self, font):
        key = _key(font)
        fm = self._metrics.get(key)
        if fm is None:
            with self._lock:
                fm = self._metrics[key] = QFontMetrics(qfont(font))
        return key, fm

    def text_width(self, font, line):
        key, fm = self._fm(font)
        w = self._widths.get((key, line))
        if w is None:
            w = self._widths[(key, line)] = fm.horizontalAdvance(line)
        return w

    def extents(self, font):
        """(ascent, descent): Tk's font metrics -ascent and -descent"""
        key = _key(font)
        e = self._extents.get(key)
        if e is None:
            with self._lock:
                e = self._extents[key] = ink_extents(qfont(font))
        return e

    def ascent(self, font):
        return self.extents(font)[0]

    def descent(self, font):
        return self.extents(font)[1]

    def linespace(self, font):
        ascent, descent = self.extents(font)
        return ascent + descent


_metrics = None


def metrics():
    """the shared QtMetrics"""
    global _metrics
    if _metrics is None:
        _metrics = QtMetrics()
    return _metrics


# ---------------------------------------------------------------------------
# Families (GUI thread: they need the QGuiApplication)

NO_SUCH_FACE = "xyzzy no such face"


def _resolved(name):
    return QFontInfo(QFont(name)).family()


def generic_faces():
    """the faces fontconfig falls back on: what an unknown name and the generic
    families resolve to"""
    return {_resolved(n) for n in (NO_SUCH_FACE, "sans-serif", "serif", "monospace")}


def _family_name(name):
    """Qt's "Nimbus Sans [UKWN]" as the "nimbus sans" Tk would list"""
    name = name.strip()
    if name.endswith("]") and " [" in name:
        name = name[:name.rindex(" [")]
    return name.lower()


def families():
    """port: swl:font-families for the Qt GUI (sorted, lower-case)"""
    from metacat.gui import fonts as gfonts
    names = {_family_name(n) for n in QFontDatabase.families()}
    generic = generic_faces()
    for face in gfonts.g_serif_faces + gfonts.g_sans_serif_faces + gfonts.g_fancy_faces:
        if face not in names and _resolved(face) not in generic:
            names.add(face)
    return sorted(names)


def install():
    """make metacat/gui/fonts.py (fonts.ss) use the Qt fonts: its faces chosen
    from families(), and its *hidden-canvas* a Qt canvas measuring with them.
    Returns the hidden canvas.  GUI thread."""
    from metacat.gui import fonts as gfonts
    from metacat.qt.canvas import HiddenCanvas
    gfonts.load(families())
    gfonts.g_hidden_canvas = HiddenCanvas()
    return gfonts.g_hidden_canvas
