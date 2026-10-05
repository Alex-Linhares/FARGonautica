"""sgl-interpreter.ss: the SGL interpreter and its viewport (a Tk canvas).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from sgl-interpreter.ss, with
racket/gui/sgl.rkt as a worked translation.

Marshall's comment: Metacat's graphics were originally implemented using a
proprietary windowing and graphics system for Scheme called SchemeXM/SGL,
developed by John B. Zuckerman at Motorola.  SGL was a symbolic graphics language
built on top of SchemeXM, which was in turn built on top of Chez Scheme and X.  In
order to port Metacat to SWL without having to completely rewrite all of the
graphics code, he implemented an SGL interpreter in SWL.  This file contains the
bulk of the interpreter.  The language forms (sgl-interpreter.ss's header):

  (rectangle (x1 y1) (x2 y2))            (filled-rectangle (x1 y1) (x2 y2))
  (arc (xc yc) (xdiam ydiam) start sweep) (filled-arc ...)
  (line (x1 y1) (x2 y2) ...)             (polyline (x1 y1) (x2 y2) (x3 y3) ...)
  (polypoints (x1 y1) ...)               (dashed-polypoints (x1 y1) ...)
  (text "string")  (text (x y) "string")  (text (text-relative (+x +y)) "string")
  (let-sgl ((origin (x y)) (line-width n) (foreground-color c) (background-color c)
            (line-style {dashed|dotted|solid}) (font f)
            (text-justification {center|left|right}) (text-mode {image|normal}))
    <sgl-expression> ...)
  (ring (x y) outerdiam innerdiam [start sweep])
  (polygon (x1 y1) ...)  (filled-polygon (x1 y1) ...)
  (erase <color> <sgl-expression>)  (clear [<color>])
  (rule <rule-type> <clauses> <sgl-expression>)

The interpreter (draw!, erase!, draw-exps, draw-exp, lookup, extend, ...) is the
original's, definition for definition.  SWL's ``<viewport>`` class becomes
``Viewport``: its methods send the original's Tcl commands, argument for argument,
to the window it draws on (``swl.TkCanvas`` for a tkinter Canvas), so the canvas
items, tags, dashes, anchors and fonts are the ones the original made.
tests/test_sgl.py checks the command stream against the oracle's.  SGL data use
chez.py's representation: symbols are str, strings chez.String (a colour name or
a text must be one), numbers int/Fraction/float, lists Python lists.  SWL's
``send`` becomes a method call.  This module does not import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez
from metacat.gui import colors, fonts, swl
from metacat.objects import tell
from metacat.utilities import exists_p

# port: metacat.ss's configuration; the oracle prelude's *platform*, and Tk 8.5
g_platform = "linux"
g_tcl_or_tk_version = 8.5
g_tcl_or_tk_version_8_3_p = g_tcl_or_tk_version >= 8.3

_HALF = Fraction(1, 2)


def _string_p(x):
    return isinstance(x, chez.String)


def _null_p(x):
    return isinstance(x, list) and not x


# ------------------------------------------------------------------------------
# The following workaround is necessary because swl0.9u comes bundled with
# Tcl/Tk version 8.0, which doesn't support the -dash or -state options at all.

def remove_unsupported_tcl_args(args):
    """sgl-interpreter.ss: remove-unsupported-tcl-args"""
    out = []
    i = 0
    while i < len(args):
        if args[i] == "-dash" or args[i] == "-state":
            i += 2
        else:
            out.append(args[i])
            i += 1
    return out


def _tcl_eval_8_0(*args):
    return swl.swl_tcl_eval(*remove_unsupported_tcl_args(list(args)))


tcl_eval = swl.swl_tcl_eval if g_tcl_or_tk_version_8_3_p else _tcl_eval_8_0


# ------------------------------------------------------------------------------
# SGL interpreter

# these two functions work around a bug in SWL 0.9u
def my_screen_to_canvas_x(win, x):
    """sgl-interpreter.ss: my-screen->canvas-x"""
    return _flonum_to_fixnum(swl.swl_tcl_eval(win, "canvasx", x))


def my_screen_to_canvas_y(win, y):
    """sgl-interpreter.ss: my-screen->canvas-y"""
    return _flonum_to_fixnum(swl.swl_tcl_eval(win, "canvasy", y))


def _flonum_to_fixnum(answer):
    # (flonum->fixnum (string->number answer)): Tk answers a string, or tkinter a number
    n = chez.string_to_number(answer) if isinstance(answer, str) else answer
    return int(n)


def nop_event_handler(*ignore):
    """sgl-interpreter.ss: nop-event-handler"""
    return None


def default_press_handler(win, x, y):
    """sgl-interpreter.ss: default-press-handler"""
    chez.printf("mouse pressed at (~a ~a)~%", chez.inexact(x), chez.inexact(y))


class Viewport:
    """sgl-interpreter.ss: <viewport> (define-class, a subclass of SWL's <canvas>)

    canvas is the Tk canvas it stands for: any window with tcl(*args),
    get_background_color() and set_background_color_bang(color) (swl.TkCanvas)."""

    def __init__(self, canvas, px, py, xp, yp):
        self.canvas = canvas
        self.resize_handler = nop_event_handler
        self.left_press_handler = nop_event_handler
        self.right_press_handler = nop_event_handler
        self.pixel_to_x = px
        self.pixel_to_y = py
        self.x_to_pixel = xp
        self.y_to_pixel = yp

    # port: the <canvas> part
    def tcl(self, *args):
        """port: the canvas's widget command"""
        return self.canvas.tcl(*args)

    def get_background_color(self):
        """port: SWL <canvas> get-background-color"""
        return self.canvas.get_background_color()

    def set_background_color_bang(self, color):
        """port: SWL <canvas> set-background-color!"""
        return self.canvas.set_background_color_bang(color)

    # the public methods, in the original's order
    def set_resize_handler_bang(self, resize):
        self.resize_handler = resize

    def configure(self, w, h):
        return self.resize_handler(self, w, h)

    def set_mouse_handlers_bang(self, left_press, right_press):
        if exists_p(left_press):
            self.left_press_handler = left_press
        if exists_p(right_press):
            self.right_press_handler = right_press

    def mouse_press(self, i, j, mods):
        """mods: the event's modifiers, a collection of symbols (left-button,
        right-button, shift, ...), standing for SWL's event-case"""
        # port: SWL's (event-case ((modifier= mods)) ...): exactly these modifiers
        mods = frozenset(mods)
        if mods in (frozenset(["right-button"]), frozenset(["shift", "left-button"])):
            x = self.pixel_to_x(chez.add(i, my_screen_to_canvas_x(self, 1)))
            y = self.pixel_to_y(chez.add(j, my_screen_to_canvas_y(self, 1)))
            return self.right_press_handler(self, x, y)
        if mods == frozenset(["left-button"]):
            x = self.pixel_to_x(chez.add(i, my_screen_to_canvas_x(self, 1)))
            y = self.pixel_to_y(chez.add(j, my_screen_to_canvas_y(self, 1)))
            return self.left_press_handler(self, x, y)
        return None   # (send-base self mouse-press i j mods): SWL's canvas ignores it

    def draw_open_rectangle(self, fg, lw, ls, ox, oy, x1, y1, x2, y2, tag):
        if not (x1 == x2 or y1 == y2):
            tcl_eval(self, "create", "rectangle",
                     self.x_to_pixel(x1, ox), self.y_to_pixel(y1, oy),
                     self.x_to_pixel(x2, ox), self.y_to_pixel(y2, oy),
                     "-outline", fg, "-width", lw, "-dash", ls, "-tags", tag)

    def draw_filled_rectangle(self, fg, ox, oy, x1, y1, x2, y2, tag):
        if not (x1 == x2 or y1 == y2):
            tcl_eval(self, "create", "rectangle",
                     self.x_to_pixel(x1, ox), self.y_to_pixel(y1, oy),
                     self.x_to_pixel(x2, ox), self.y_to_pixel(y2, oy),
                     "-outline", fg, "-fill", fg, "-tags", tag)

    # This is used only by the theme-panel 'redraw-panel method:
    def draw_hidden_filled_rectangle(self, fg, x1, y1, x2, y2, tag):
        tcl_eval(self, "create", "rectangle",
                 self.x_to_pixel(x1, 0), self.y_to_pixel(y1, 0),
                 self.x_to_pixel(x2, 0), self.y_to_pixel(y2, 0),
                 "-outline", fg, "-fill", fg, "-tags", tag, "-state", "hidden")

    def draw_line_segments(self, fg, lw, ls, ox, oy, points, tag):
        while points:
            x1, y1 = points[0][0], points[0][1]
            x2, y2 = points[1][0], points[1][1]
            tcl_eval(self, "create", "line",
                     self.x_to_pixel(x1, ox), self.y_to_pixel(y1, oy),
                     self.x_to_pixel(x2, ox), self.y_to_pixel(y2, oy),
                     "-fill", fg, "-width", lw, "-dash", ls, "-tags", tag)
            points = points[2:]

    def draw_polyline(self, fg, lw, ls, ox, oy, points, tag):
        tcl_eval(self, "create", "line",
                 *generate_polyline_coords(points, ox, oy, self.x_to_pixel, self.y_to_pixel),
                 "-fill", fg, "-width", lw, "-dash", ls, "-tags", tag)

    def draw_open_oval(self, fg, lw, ls, ox, oy, x1, y1, x2, y2, tag):
        if not (x1 == x2 or y1 == y2):
            tcl_eval(self, "create", "oval",
                     self.x_to_pixel(x1, ox), self.y_to_pixel(y1, oy),
                     self.x_to_pixel(x2, ox), self.y_to_pixel(y2, oy),
                     "-outline", fg, "-width", lw, "-dash", ls, "-tags", tag)

    def draw_filled_oval(self, fg, ox, oy, x1, y1, x2, y2, tag):
        if not (x1 == x2 or y1 == y2):
            tcl_eval(self, "create", "oval",
                     self.x_to_pixel(x1, ox), self.y_to_pixel(y1, oy),
                     self.x_to_pixel(x2, ox), self.y_to_pixel(y2, oy),
                     "-outline", fg, "-fill", fg, "-tags", tag)

    def draw_open_arc(self, fg, lw, ls, ox, oy, x1, y1, x2, y2, start, sweep, tag):
        if not (x1 == x2 or y1 == y2):
            tcl_eval(self, "create", "arc",
                     self.x_to_pixel(x1, ox), self.y_to_pixel(y1, oy),
                     self.x_to_pixel(x2, ox), self.y_to_pixel(y2, oy),
                     "-style", "arc", "-outline", fg, "-width", lw, "-dash", ls,
                     "-start", start, "-extent", sweep, "-tags", tag)

    def draw_filled_arc(self, fg, ox, oy, x1, y1, x2, y2, start, sweep, tag):
        if not (x1 == x2 or y1 == y2):
            tcl_eval(self, "create", "arc",
                     self.x_to_pixel(x1, ox), self.y_to_pixel(y1, oy),
                     self.x_to_pixel(x2, ox), self.y_to_pixel(y2, oy),
                     "-style", "pieslice", "-outline", fg, "-fill", fg,
                     "-start", start, "-extent", sweep, "-tags", tag)

    def draw_ring(self, fg, bg, ox, oy, x1a, y1a, x2a, y2a, x1b, y1b, x2b, y2b, tag):
        if not (x1a == x2a or y1a == y2a):
            tcl_eval(self, "create", "oval",
                     self.x_to_pixel(x1a, ox), self.y_to_pixel(y1a, oy),
                     self.x_to_pixel(x2a, ox), self.y_to_pixel(y2a, oy),
                     "-outline", fg, "-fill", fg, "-tags", tag)
        if not (x1b == x2b or y1b == y2b):
            tcl_eval(self, "create", "oval",
                     self.x_to_pixel(x1b, ox), self.y_to_pixel(y1b, oy),
                     self.x_to_pixel(x2b, ox), self.y_to_pixel(y2b, oy),
                     "-outline", bg, "-fill", bg, "-tags", tag)

    def draw_arc_ring(self, fg, bg, ox, oy, x1a, y1a, x2a, y2a, x1b, y1b, x2b, y2b,
                      start, sweep, tag):
        if not (x1a == x2a or y1a == y2a):
            tcl_eval(self, "create", "arc",
                     self.x_to_pixel(x1a, ox), self.y_to_pixel(y1a, oy),
                     self.x_to_pixel(x2a, ox), self.y_to_pixel(y2a, oy),
                     "-style", "pieslice", "-outline", bg, "-fill", fg,
                     "-start", start, "-extent", sweep, "-tags", tag)
        if not (x1b == x2b or y1b == y2b):
            tcl_eval(self, "create", "arc",
                     self.x_to_pixel(x1b, ox), self.y_to_pixel(y1b, oy),
                     self.x_to_pixel(x2b, ox), self.y_to_pixel(y2b, oy),
                     "-style", "pieslice", "-outline", bg, "-fill", bg,
                     "-start", start, "-extent", sweep, "-tags", tag)

    def draw_open_polygon(self, fg, bg, lw, ls, ox, oy, points, tag):
        tcl_eval(self, "create", "polygon",
                 *generate_polyline_coords(points, ox, oy, self.x_to_pixel, self.y_to_pixel),
                 "-outline", fg, "-fill", bg, "-width", lw, "-dash", ls, "-tags", tag)

    def draw_filled_polygon(self, fg, ox, oy, points, tag):
        tcl_eval(self, "create", "polygon",
                 *generate_polyline_coords(points, ox, oy, self.x_to_pixel, self.y_to_pixel),
                 "-outline", fg, "-fill", fg, "-tags", tag)

    def draw_polypoints(self, fg, ox, oy, points, dashed_p, tag):
        for point in points:
            x = self.x_to_pixel(point[0], ox)
            y = self.y_to_pixel(point[1], oy)
            if dashed_p is not False:
                tcl_eval(self, "create", "line", x, y, chez.add(x, 4), y, "-fill", fg, "-tags", tag)
            else:
                tcl_eval(self, "create", "line", x, y, chez.add(x, 1), y, "-fill", fg, "-tags", tag)

    def draw_text(self, fg, bg, text, ox, oy, relx, rely, justify, font, mode, tag):
        # a let*: each line in turn
        size = tell(font, "get-pixel-size", text)
        width = size[0]
        height = size[1]
        baseline = size[2]
        m_width = tell(font, "get-pixel-size", chez.String("M"))[0]
        text_relative_x_offset = chez.mul(relx, m_width)
        text_relative_y_offset = chez.mul(-1, rely, chez.sub(height, baseline))
        if justify == "center":
            justification_offset = 0
        elif justify == "left":
            justification_offset = chez.mul(_HALF, width)
        elif justify == "right":
            justification_offset = chez.mul(Fraction(-1, 2), width)
        else:
            justification_offset = None   # 1.2: case without else gives void
        center = chez.add(self.x_to_pixel(0, ox), text_relative_x_offset, justification_offset)
        left = chez.sub(center, chez.mul(_HALF, width))
        right = chez.add(center, chez.mul(_HALF, width))
        lower = chez.add(self.y_to_pixel(0, oy), baseline, text_relative_y_offset)
        upper = chez.sub(lower, height)
        if mode == "image":
            tcl_eval(self, "create", "rectangle",
                     chez.add(left, 1), chez.add(upper, 1), chez.sub(right, 1), chez.sub(lower, 1),
                     "-outline", bg, "-fill", bg, "-tags", tag)
        tcl_eval(self, "create", "text", center, lower,
                 "-text", text, "-anchor", "s", "-font", tell(font, "get-swl-font"),
                 "-fill", fg, "-tags", tag)

    def move(self, dx, dy, tag):
        xshift = chez.sub(self.x_to_pixel(dx, 0), self.x_to_pixel(0, 0))
        yshift = chez.sub(self.y_to_pixel(dy, 0), self.y_to_pixel(0, 0))
        tcl_eval(self, "move", tag, xshift, yshift)

    def move_pixels(self, dx, dy, tag):
        tcl_eval(self, "move", tag, dx, dy)

    def raise_(self, tag):
        tcl_eval(self, "raise", tag, "all")

    def unhide(self, tag):
        tcl_eval(self, "itemconfigure", tag, "-state", "normal")

    def retag(self, old, new):
        tcl_eval(self, "itemconfigure", old, "-tags", new)

    def rescale(self, tag, xfactor, yfactor):
        tcl_eval(self, "scale", tag, 0, 0, xfactor, yfactor)

    def delete(self, tag):
        tcl_eval(self, "delete", tag)


def generate_polyline_coords(points, ox, oy, x_to_pixel, y_to_pixel):
    """sgl-interpreter.ss: generate-polyline-coords"""
    out = []
    for point in points:
        out.append(x_to_pixel(point[0], ox))
        out.append(y_to_pixel(point[1], oy))
    return out


# -------------------------------------------------------------------------------

def draw_bang(vp, pexp, *tag):
    """sgl-interpreter.ss: draw!"""
    bg = vp.get_background_color()
    return draw_exp(vp, pexp, extend(init_env, "background-color", bg), 0, 0, bg,
                    "all" if not tag else tag[0])


def erase_bang(vp, pexp):
    """sgl-interpreter.ss: erase!"""
    return draw_bang(vp, ["erase", vp.get_background_color(), pexp])


def draw_exps(vp, pexps, env, ox, oy, erase_color, tag):
    """sgl-interpreter.ss: draw-exps"""
    for pexp in pexps:
        draw_exp(vp, pexp, env, ox, oy, erase_color, tag)


def draw_exp(vp, pexp, env, ox, oy, erase_color, tag):
    """sgl-interpreter.ss: draw-exp"""
    if not _null_p(pexp):
        key = pexp[0]
        args = pexp[1:]
        eraser = tag == "eraser"
        if key == "let-sgl":
            bindings, pexps = args[0], args[1:]
            origin_binding = chez.assq("origin", bindings)
            if origin_binding is not False:
                ox = chez.add(ox, origin_binding[1][0])
                oy = chez.add(oy, origin_binding[1][1])
            draw_exps(vp, pexps, extend_star(env, bindings), ox, oy, erase_color, tag)
        elif key == "rectangle":
            p1, p2 = args
            fg = erase_color if eraser else lookup(env, "foreground-color")
            lw = lookup(env, "line-width")
            ls = lookup(env, "line-style")
            vp.draw_open_rectangle(fg, lw, ls, ox, oy, p1[0], p1[1], p2[0], p2[1], tag)
        elif key == "filled-rectangle":
            p1, p2 = args
            fg = erase_color if eraser else lookup(env, "foreground-color")
            vp.draw_filled_rectangle(fg, ox, oy, p1[0], p1[1], p2[0], p2[1], tag)
        elif key == "line":
            fg = erase_color if eraser else lookup(env, "foreground-color")
            lw = lookup(env, "line-width")
            ls = lookup(env, "line-style")
            vp.draw_line_segments(fg, lw, ls, ox, oy, args, tag)
        elif key == "polyline":
            fg = erase_color if eraser else lookup(env, "foreground-color")
            lw = lookup(env, "line-width")
            ls = lookup(env, "line-style")
            vp.draw_polyline(fg, lw, ls, ox, oy, args, tag)
        elif key == "polygon":
            fg = erase_color if eraser else lookup(env, "foreground-color")
            bg = erase_color if eraser else lookup(env, "background-color")
            lw = lookup(env, "line-width")
            ls = lookup(env, "line-style")
            vp.draw_open_polygon(fg, bg, lw, ls, ox, oy, args, tag)
        elif key == "filled-polygon":
            fg = erase_color if eraser else lookup(env, "foreground-color")
            vp.draw_filled_polygon(fg, ox, oy, args, tag)
        elif key == "arc" or key == "filled-arc":
            center, size, start, sweep = args
            width = size[0]
            height = size[1]
            fg = erase_color if eraser else lookup(env, "foreground-color")
            if key == "arc":
                lw = lookup(env, "line-width")
                ls = lookup(env, "line-style")
            x1 = chez.sub(center[0], chez.div(width, 2))
            y1 = chez.add(center[1], chez.div(height, 2))
            x2 = chez.add(center[0], chez.div(width, 2))
            y2 = chez.sub(center[1], chez.div(height, 2))
            if key == "arc":
                if sweep >= 360:
                    vp.draw_open_oval(fg, lw, ls, ox, oy, x1, y1, x2, y2, tag)
                else:
                    vp.draw_open_arc(fg, lw, ls, ox, oy, x1, y1, x2, y2, start, sweep, tag)
            elif sweep >= 360:
                vp.draw_filled_oval(fg, ox, oy, x1, y1, x2, y2, tag)
            else:
                vp.draw_filled_arc(fg, ox, oy, x1, y1, x2, y2, start, sweep, tag)
        elif key == "ring":
            center, outer_diam, inner_diam = args[0], args[1], args[2]
            rest = args[3:]
            fg = erase_color if eraser else lookup(env, "foreground-color")
            bg = erase_color if eraser else lookup(env, "background-color")
            x1_outer = chez.sub(center[0], chez.div(outer_diam, 2))
            y1_outer = chez.add(center[1], chez.div(outer_diam, 2))
            x2_outer = chez.add(center[0], chez.div(outer_diam, 2))
            y2_outer = chez.sub(center[1], chez.div(outer_diam, 2))
            x1_inner = chez.sub(center[0], chez.div(inner_diam, 2))
            y1_inner = chez.add(center[1], chez.div(inner_diam, 2))
            x2_inner = chez.add(center[0], chez.div(inner_diam, 2))
            y2_inner = chez.sub(center[1], chez.div(inner_diam, 2))
            if not rest:
                vp.draw_ring(fg, bg, ox, oy, x1_outer, y1_outer, x2_outer, y2_outer,
                             x1_inner, y1_inner, x2_inner, y2_inner, tag)
            else:
                vp.draw_arc_ring(fg, bg, ox, oy, x1_outer, y1_outer, x2_outer, y2_outer,
                                 x1_inner, y1_inner, x2_inner, y2_inner, rest[0], rest[1], tag)
        elif key == "polypoints":
            fg = erase_color if eraser else lookup(env, "foreground-color")
            vp.draw_polypoints(fg, ox, oy, args, False, tag)
        elif key == "dashed-polypoints":
            fg = erase_color if eraser else lookup(env, "foreground-color")
            vp.draw_polypoints(fg, ox, oy, args, True, tag)
        elif key == "text":
            fg = erase_color if eraser else lookup(env, "foreground-color")
            bg = erase_color if eraser else lookup(env, "background-color")
            justify = lookup(env, "text-justification")
            mode = lookup(env, "text-mode")
            font = lookup(env, "font")
            if _string_p(args[0]):
                vp.draw_text(fg, bg, args[0], ox, oy, 0, 0, justify, font, mode, tag)
            elif args[0][0] == "text-relative":
                relative_offsets = args[0][1]
                relx = relative_offsets[0]
                rely = relative_offsets[1]
                vp.draw_text(fg, bg, args[1], ox, oy, relx, rely, justify, font, mode, tag)
            else:
                ox = chez.add(args[0][0], ox)
                oy = chez.add(args[0][1], oy)
                vp.draw_text(fg, bg, args[1], ox, oy, 0, 0, justify, font, mode, tag)
        elif key == "erase":
            c, sub_pexp = args
            draw_exp(vp, sub_pexp, env, ox, oy, colors.swl_color(c) if _string_p(c) else c,
                     "eraser")
        elif key == "clear":
            vp.delete("all")
            if not _null_p(args):
                vp.set_background_color_bang(args[0])
        # The following is a total hack.  When the workspace window is resized, we
        # need to recompute all rule pexps to reflect the new rule widths.  Many
        # of these pexps are embedded within larger pexps for answer and snag
        # descriptions.  Rule tags make it possible to replace the rule
        # subexpressions within these larger pexps.  The rule type and clause
        # information is needed to compute the new pexps.
        elif key == "rule":
            rule_type, clauses, sub_pexp = args
            draw_exp(vp, sub_pexp, env, ox, oy, erase_color, tag)
        else:
            chez.error("draw-exp", "invalid picture expression:~n~a", pexp)
    return "ok"


def graphics_dash_pattern():
    """sgl-interpreter.ss: graphics-dash-pattern"""
    if (g_platform == "windows" and g_tcl_or_tk_version_8_3_p
            and _metacat.setup.p_nice_graphics is not False):
        return chez.String(". ")
    return chez.String("- ")


def lookup(env, symbol):
    """sgl-interpreter.ss: lookup"""
    value = env(symbol)
    if symbol == "line-style":
        if value == "dotted":
            return chez.String(". ")
        if value == "dashed":
            return graphics_dash_pattern()
        return chez.String("")
    if symbol in ("foreground-color", "background-color", "erase-color"):
        return colors.swl_color(value) if _string_p(value) else value
    return value


def extend(env, sym, val):
    """sgl-interpreter.ss: extend"""
    if sym == "origin":
        return env
    return lambda symbol: val if symbol == sym else env(symbol)


def extend_star(env, bindings):
    """sgl-interpreter.ss: extend*"""
    for binding in bindings:
        env = extend(env, binding[0], binding[1])
    return env


def empty_env(symbol):
    """sgl-interpreter.ss: empty-env"""
    return False


init_env = False


def load():
    """port: sgl-interpreter.ss's init-env, made when the views start (after
    fonts.load(), which chooses sans-serif)"""
    global init_env
    init_env = extend_star(empty_env, [
        ["foreground-color", colors.c_black],
        ["font", fonts.swl_font(fonts.sans_serif, 10)],
        ["text-justification", "left"],
        ["text-mode", "normal"],
        ["line-width", 1],
        ["line-style", "solid"]])


g_flush_event_queue = swl.swl_sync_display
