"""fonts.ss: SWL fonts, Metacat's fixed fonts and the face choices.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from fonts.ss, with racket/gui/fonts.rkt as a
worked translation.

The definitions are the original's, in its order.  What SWL provided:
- ``(create <font> face size style)`` is ``SwlFont``: the requested face, size (points
  if positive, pixels if negative, as in Tk) and style list.  Its Tk form is the font
  description ``(face size style...)`` (``swl.tcl_word``).  Its
  ``get_actual_values`` stands for Tk's ``font actual``: like the Racket port, it
  reports the request, with a pixel size given as points at 96 dpi
  (docs/divergences.md).
- text is measured, as in the original, by creating a text item on
  ``*hidden-canvas*`` and asking Tk for its ``bbox``, through ``swl.swl_tcl_eval``.
  ``create_mcat_logo`` makes that canvas (a real Tk canvas); the tests put a
  recording canvas with a fixed metric there instead.
- ``swl:font-families`` is Tk's ``font families``, as lower-case symbols, unless
  ``load(families)`` fixed the list (the tests use the oracle prelude's).

``serif``, ``sans_serif`` and ``fancy`` are computed by ``load()``, not at import,
because asking Tk for its families needs a display.  This module imports tkinter
only inside ``swl_font_families`` and ``create_mcat_logo``.
"""
from __future__ import annotations

from fractions import Fraction

from metacat import chez, utilities
from metacat.gui import swl
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import base_object


def _symbol_p(x):
    return isinstance(x, str) and not isinstance(x, (chez.String, chez.Char))


class SwlFont:
    """port: SWL's <font> (a Tk font): face, size and style list"""
    __slots__ = ("face", "size", "style")

    def __init__(self, face, size, style):
        self.face = face
        self.size = size
        self.style = list(style)

    def get_actual_values(self):
        """port: SWL <font> get-actual-values (Tk's font actual: face, points,
        weight), reported from the request at 96 dpi as in the Racket port"""
        size = self.size
        if size < 0:
            size = utilities.round_(Fraction(-size * 72, 96))
        return (self.face, size, "bold" if "bold" in self.style else "normal")

    def tcl_word(self):
        """port: the Tk font description"""
        return (str(self.face), self.size, *[str(s) for s in self.style])

    def __repr__(self):
        return "SwlFont(%r, %r, %r)" % (self.face, self.size, self.style)


_families = None


def swl_font_families():
    """port: SWL's swl:font-families (Tk's font families, as lower-case symbols)"""
    if _families is not None:
        return _families
    import tkinter.font
    return [f.lower() for f in tkinter.font.families()]


def swl_font(face, size, *style):
    """fonts.ss: swl-font"""
    if not style or _symbol_p(style[0]):
        return SwlFont(face, size, style)
    return SwlFont(face, size, style[0])


g_scrollbar_width = False
g_scrollbar_height = False

# the hidden canvas is used by fonts to compute dimensions of text
g_mcat_logo = False
g_hidden_canvas = False


def create_mcat_logo(root=None):
    """fonts.ss: create-mcat-logo"""
    # port: a Tk toplevel with two canvases, as SWL made it; the scrollbar sizes are
    # those of a tkinter Scrollbar.  %logo-background-color% and %logo-font% are
    # gui/constants.py's once it is loaded (white and times 24 bold italic before).
    global g_scrollbar_width, g_scrollbar_height, g_mcat_logo, g_hidden_canvas
    import sys
    import tkinter
    from metacat.gui import general_graphics
    K = sys.modules.get("metacat.gui.constants")
    background = (swl.tcl_word(K.p_logo_background_color) if K is not None else "white")
    font = (swl.tcl_word(K.p_logo_font) if K is not None and K.p_logo_font
            else ("times", 24, "bold", "italic"))
    top = tkinter.Toplevel(root, width=110, height=80)
    top.title("Logo")
    top.resizable(False, False)
    top.protocol("WM_DELETE_WINDOW",
                 lambda: general_graphics.toplevel_destroy_action(top))
    logo = tkinter.Canvas(top, width=110, height=80, background=background,
                          highlightthickness=0)
    hidden = tkinter.Canvas(top, width=110, height=80)
    logo.pack(expand=True, fill="both")
    probe = tkinter.Scrollbar(top, orient="vertical")
    g_scrollbar_width = probe.winfo_reqwidth()
    g_scrollbar_height = tkinter.Scrollbar(top, orient="horizontal").winfo_reqheight()
    logo.create_text(55, 50, text="Metacat", anchor="s", font=font)
    g_mcat_logo = swl.TkCanvas(logo, background=None)
    g_hidden_canvas = swl.TkCanvas(hidden)
    return "done"


# positive size value indicates font size in points, negative value
# indicates font size in pixels

class MFont(SchemeObject):
    """fonts.ss: make-mfont (the closure)"""
    __slots__ = ("face", "style", "font")

    def __init__(this, face, size, style):
        this.face = face
        this.style = style
        this.font = make_fixed_font(face, size, style)

    @message("object-type")
    def object_type(this, self):
        return "mfont"

    @message("resize")
    def resize(this, self, new_size):
        this.font = make_fixed_font(this.face, chez.sub(new_size), this.style)
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.font)


def make_mfont(face, size, style):
    """fonts.ss: make-mfont"""
    return MFont(face, size, style)


def get_actual_font_values(font):
    """fonts.ss: get-actual-font-values"""
    return list(font.get_actual_values())


def get_actual_font_size(font):
    """fonts.ss: get-actual-font-size"""
    return get_actual_font_values(font)[1]


class FixedFont(SchemeObject):
    """fonts.ss: make-fixed-font (the closure)"""
    __slots__ = ("face", "size", "style", "font")

    def __init__(this, face, size, style, font):
        this.face = face
        this.size = size
        this.style = style
        this.font = font

    @message("object-type")
    def object_type(this, self):
        return "fixed-font"

    @message("print")
    def print_(this, self):
        chez.printf("Font requested: ~s~n", [this.face, this.size, this.style])
        chez.printf("Font assigned:  ~s~n", get_actual_font_values(this.font))
        if g_hidden_canvas:
            size = tell(self, "get-pixel-size", chez.String("M"))
            chez.printf("M pixel matrix: ~a x ~a, offset ~a~n", size[0], size[1], size[2])
        else:
            chez.printf("(pixel matrix info unavailable)~n")

    @message("get-swl-font")
    def get_swl_font(this, self):
        return this.font

    @message("get-actual-values")
    def get_actual_values(this, self):
        return get_actual_font_values(this.font)

    @message("get-face")
    def get_face(this, self):
        return get_actual_font_values(this.font)[0]

    @message("get-size")
    def get_size(this, self):
        return get_actual_font_values(this.font)[1]

    @message("get-style")
    def get_style(this, self):
        return get_actual_font_values(this.font)[2]

    @message("get-pixel-size")
    def get_pixel_size(this, self, string):
        if g_hidden_canvas:
            bb = swl.swl_tcl_to_scheme(
                swl.swl_tcl_eval(g_hidden_canvas, "bbox",
                                 swl.swl_tcl_eval(g_hidden_canvas, "create", "text", 0, 0,
                                                  "-text", string, "-anchor", "nw",
                                                  "-font", this.font)))
            width = chez.sub(bb[2], bb[0])
            height = chez.sub(bb[3], bb[1])
            baseline_offset = utilities.round_(chez.mul(Fraction(1, 5), height))
            swl.swl_tcl_eval(g_hidden_canvas, "delete", "all")
            return [width, height, baseline_offset]
        return chez.error(False, "need to run (create-mcat-logo) first")

    @message("get-pixel-width")
    def get_pixel_width(this, self, text):
        return tell(self, "get-pixel-size", text)[0]

    @message("get-pixel-height")
    def get_pixel_height(this, self):
        return tell(self, "get-pixel-size", chez.String("M"))[1]

    @message("get-baseline-offset")
    def get_baseline_offset(this, self):
        return tell(self, "get-pixel-size", chez.String("M"))[2]

    @message("show-char-info")
    def show_char_info(this, self):
        chez.printf("Character widths:")
        for ascii_ in range(32, 127):
            if ascii_ % 4 == 0:
                chez.newline()
            char = chez.String(chr(ascii_))
            width = tell(self, "get-pixel-width", char)
            if ascii_ == 32:
                chez.printf("  space ~a", width)
            else:
                chez.printf("      ~a ~a", char, width)
        chez.newline()
        chez.printf("Character height: ~a~n", tell(self, "get-pixel-height"))
        chez.printf("Baseline offset: ~a~n", tell(self, "get-baseline-offset"))

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_fixed_font(face, size, style):
    """fonts.ss: make-fixed-font"""
    if isinstance(face, chez.String):
        font = swl_font(str(face), size, style)
    else:
        font = swl_font(face, size, style)
    if get_actual_font_size(font) < 7 and chez.member("small fonts", swl_font_families()):
        font = swl_font("small fonts", get_actual_font_size(font), "normal")
    return FixedFont(face, size, style, font)


g_serif_faces = ["times new roman", "times"]

g_sans_serif_faces = ["helvetica", "arial"]

g_fancy_faces = ["palatino linotype", "palatino", "new century schoolbook",
                 "times new roman", "times", "bookman old style", "georgia", "book antiqua"]


def select_face(preferences, available, style):
    """fonts.ss: select-face"""
    while preferences:
        if chez.member(preferences[0], available) is not False:
            return preferences[0]
        preferences = preferences[1:]
    # printf "Warning: all requested ~a fonts are unavailable" (commented out)
    if style == "serif":
        return "times"
    if style == "sans-serif":
        return "helvetica"
    if style == "fancy":
        return "times"
    return None


serif = sans_serif = fancy = False


def load(families=None):
    """port: fonts.ss's top-level face choices, made when the views start.
    families fixes swl:font-families (otherwise Tk is asked)."""
    global _families, serif, sans_serif, fancy
    if families is not None:
        _families = list(families)
    serif = select_face(g_serif_faces, swl_font_families(), "serif")
    sans_serif = select_face(g_sans_serif_faces, swl_font_families(), "sans-serif")
    fancy = select_face(g_fancy_faces, swl_font_families(), "fancy")
