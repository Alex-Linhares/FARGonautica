"""The windows part of general-graphics.ss: graphics windows and text windows.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from general-graphics.ss, with
racket/gui/general-graphics.rktl as a worked translation.

The pexp builders and text helpers of general-graphics.ss (circles, boxes,
dotted lines, arrowheads, break-into-lines, ...) are the engine's
(metacat/general_graphics.py), because the model calls them; this module has
the windows.  SWL's <toplevel> and its <frame> or <scrollframe> become one
window host (gui/hosts.py: offscreen by default, Tk once the GUI installs its
maker), whose canvas the sgl.Viewport draws on.  %default-fg-color% and
*fg-color* are engine globals (metacat/view_globals.py): `load()` installs
them there, and the windows read *fg-color* from there, as the model sets it.
SWL thread message queues become a queue.Queue and a thread, started only by
start-resize-listener.  Changes are marked "port:" (docs/porting-notes.md,
item 13).  This module does not import tkinter.
"""
from __future__ import annotations

import queue
import threading
from fractions import Fraction

from metacat import chez, setup, utilities, view_globals
from metacat.gui import colors, fonts, hosts, sgl
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.sugar import repeat_star_times
from metacat.utilities import base_object, exists_p

_HALF = Fraction(1, 2)

# (pi, pi/180 and 180/pi are the engine's: metacat/general_graphics.py)

p_default_fg_color = colors.c_black
p_default_bg_color = colors.c_white
# (swl-font serif 24): made by load(), port: fonts.ss's serif is chosen when the
# views start
p_default_scrollable_text_window_font = False

g_fg_color = p_default_fg_color


def toplevel_destroy_action(toplevel):
    """general-graphics.ss: toplevel-destroy-action"""
    tell(setup.g_control_panel, "hide-window", toplevel)
    return False


p_resize_listener_pause = 250
# port: SWL's (thread-make-msg-queue 'resizeq)
g_resize_message_queue = queue.Queue()
# port: SWL's critical-section (no other thread runs inside it)
_critical_section = threading.Lock()


def start_resize_listener():
    """general-graphics.ss: start-resize-listener"""
    # port: SWL's thread-fork on a Python daemon thread
    def loop():
        while True:
            resize = g_resize_message_queue.get()
            resize()
            utilities.pause(p_resize_listener_pause)
    threading.Thread(target=loop, name="resize-listener", daemon=True).start()
    return "done"


def make_scrollable_graphics_window(visible_w, visible_h, *color):
    """general-graphics.ss: make-scrollable-graphics-window"""
    bg_color = p_default_bg_color if not color else color[0]
    ymax = chez.div(visible_h, visible_w)
    return make_graphics_window(visible_w, visible_h, visible_w, visible_h,
                                0, 0, 1, ymax, bg_color, "both")


def make_unscrollable_graphics_window(visible_w, visible_h, *color):
    """general-graphics.ss: make-unscrollable-graphics-window"""
    bg_color = p_default_bg_color if not color else color[0]
    ymax = chez.div(visible_h, visible_w)
    return make_graphics_window(visible_w, visible_h, visible_w, visible_h,
                                0, 0, 1, ymax, bg_color, "none")


def make_horizontal_scrollable_graphics_window(visible_w, visible_h, canvas_w, *color):
    """general-graphics.ss: make-horizontal-scrollable-graphics-window"""
    bg_color = p_default_bg_color if not color else color[0]
    xmax = chez.div(canvas_w, visible_h)
    return make_graphics_window(visible_w, visible_h, canvas_w, visible_h,
                                0, 0, xmax, 1, bg_color, "horizontal")


def make_vertical_scrollable_graphics_window(visible_w, visible_h, canvas_h, *color):
    """general-graphics.ss: make-vertical-scrollable-graphics-window"""
    bg_color = p_default_bg_color if not color else color[0]
    ymax = chez.div(canvas_h, visible_w)
    return make_graphics_window(visible_w, visible_h, visible_w, canvas_h,
                                0, 0, 1, ymax, bg_color, "vertical")


# scrolling = {both | horizontal | vertical | none}
# determines which dimensions of a window will get rescaled when the window is
# resized.  scrolling = {both | none} maintains a fixed aspect ratio.

class GraphicsWindow(SchemeObject):
    """general-graphics.ss: make-graphics-window (the closure)"""
    __slots__ = ("visible_w", "visible_h", "canvas_w", "canvas_h", "xmin", "ymin",
                 "xmax", "ymax", "bg_color", "scrolling", "canvas_aspect_ratio",
                 "width_per_pixel", "height_per_pixel", "top", "frame", "vp",
                 "resizable_p", "position", "cache_mode_p", "cached_pexps",
                 "aspect_ratio_workaround_p")

    def __init__(this, visible_w, visible_h, canvas_w, canvas_h, xmin, ymin, xmax, ymax,
                 bg_color, scrolling):
        this.visible_w, this.visible_h = visible_w, visible_h
        this.canvas_w, this.canvas_h = canvas_w, canvas_h
        this.xmin, this.ymin, this.xmax, this.ymax = xmin, ymin, xmax, ymax
        this.bg_color = bg_color
        this.scrolling = scrolling
        this.canvas_aspect_ratio = chez.div(canvas_w, canvas_h)
        this.width_per_pixel = chez.div(chez.sub(xmax, xmin), canvas_w)
        this.height_per_pixel = chez.div(chez.sub(ymax, ymin), canvas_h)

        # pixel->x etc. read width-per-pixel at call time, so that they see the
        # values a resize sets
        def pixel_to_x(i):
            return chez.add(this.xmin, chez.mul(this.width_per_pixel, i))

        def pixel_to_y(j):
            return chez.sub(this.ymax, chez.mul(this.height_per_pixel, j))

        def x_to_pixel(x, offset):
            return utilities.floor(chez.div(chez.sub(chez.add(x, offset), this.xmin),
                                            this.width_per_pixel))

        def y_to_pixel(y, offset):
            return utilities.floor(chez.div(chez.sub(this.ymax, chez.add(y, offset)),
                                            this.height_per_pixel))

        # port: the toplevel and its (scroll)frame are one window host
        this.top = hosts.make_window_host(scrolling, toplevel_destroy_action)
        this.frame = this.top
        canvas = this.top.make_canvas(visible_w, visible_h, bg_color)
        this.vp = sgl.Viewport(canvas, pixel_to_x, pixel_to_y, x_to_pixel, y_to_pixel)
        this.top.show_viewport(this.vp)   # port: the host passes Tk's events to vp
        this.resizable_p = False
        this.position = False
        this.cache_mode_p = False
        this.cached_pexps = []   # port: oldest first (the original conses and reverses)
        this.aspect_ratio_workaround_p = False   # disabled 5/2015

        # ;; set-aspect-ratio-bounds! does not seem to work under Windows
        # ;; or Mac OS X, so we need a workaround to compensate for this
        # (and (or (eq? *platform* 'windows) (eq? *platform* 'macintosh))
        #      (or (eq? scrolling 'both) (eq? scrolling 'none)))))

        # port: (scroll-region: 0 0 canvas-w canvas-h), and Tk packing becomes
        # the host showing the canvas
        this.top.set_scroll_region_bang(0, 0, canvas_w, canvas_h)
        this.top.raise_()

    def _set_scroll_region(this, x1, y1, x2, y2):
        # port: the viewport's set-scroll-region! goes to the host
        this.top.set_scroll_region_bang(x1, y1, x2, y2)

    @message("object-type")
    def object_type(this, self):
        return "graphics-window"

    @message("get-vp")
    def get_vp(this, self):
        return this.vp

    @message("get-toplevel")
    def get_toplevel(this, self):
        return this.top

    @message("get-size")
    def get_size(this, self):
        return [this.visible_w, this.visible_h]

    @message("get-visible-w")
    def get_visible_w(this, self):
        return this.visible_w

    @message("get-visible-h")
    def get_visible_h(this, self):
        return this.visible_h

    @message("set-position")
    def set_position(this, self, x, y):
        return this.top.set_geometry_bang(chez.format_("+~a+~a", x, y))

    @message("remember-position")
    def remember_position(this, self):
        g = this.top.get_geometry()
        this.position = g[utilities.char_index(chez.Char("+"), g):]
        return "done"

    @message("restore-position")
    def restore_position(this, self):
        if exists_p(this.position):
            this.top.set_geometry_bang(this.position)
        return "done"

    @message("repair-aspect-ratio")
    def repair_aspect_ratio(this, self):
        w = this.top.get_width()
        h = this.top.get_height()
        ratio = chez.div(chez.sub(w, 2), chez.sub(h, 2))
        if ratio > this.canvas_aspect_ratio:
            return this.top.set_geometry_bang(chez.format_(
                "~ax~a", utilities.round_(chez.mul(h, this.canvas_aspect_ratio)), h))
        if ratio < this.canvas_aspect_ratio:
            return this.top.set_geometry_bang(chez.format_(
                "~ax~a", w, utilities.round_(chez.div(w, this.canvas_aspect_ratio))))
        return None

    @message("make-resizable")
    def make_resizable(this, self, *winname):
        if not (exists_p(fonts.g_scrollbar_width) and exists_p(fonts.g_scrollbar_height)):
            chez.error(False, "need to run (create-mcat-logo) first")
        if not this.resizable_p:
            this.top.set_resizable_bang(True, True)
            this.top.set_min_size_bang(10, 10)
            if this.scrolling in ("both", "none"):
                # For some reason, after creating an SWL window of size W x H
                # with (create <viewport> ...), the window thinks its width
                # and height are W+2 and H+2.  We need to use these values when
                # setting the aspect ratio bounds, otherwise the window may
                # resize itself slightly when made resizable.
                ratio = chez.div(chez.add(this.visible_w, 2), chez.add(this.visible_h, 2))
                this.top.set_aspect_ratio_bounds_bang(ratio, ratio)
            this.vp.set_resize_handler_bang(
                lambda win, w, h: this._resize_handler(self, w, h))
            this.resizable_p = True
        return "done"

    def _resize_handler(this, self, w, h):
        """port: make-resizable's resize handler (lambda (win w h) ...)"""
        if not (w > 2 and h > 2):
            return "done"
        # Recalculate and set visible-w, visible-h, canvas-w, canvas-h,
        # width-per-pixel, and height-per-pixel values.  pixel->x, x->pixel,
        # etc. procedures in vp will automatically see the new values.
        this.visible_w = chez.sub(w, 2)
        this.visible_h = chez.sub(h, 2)
        # Windows/Macintosh problem workaround
        if this.aspect_ratio_workaround_p:
            ratio = chez.div(this.visible_w, this.visible_h)
            if ratio > this.canvas_aspect_ratio:
                this.visible_w = utilities.round_(chez.mul(this.visible_h, this.canvas_aspect_ratio))
            else:
                this.visible_h = utilities.round_(chez.div(this.visible_w, this.canvas_aspect_ratio))
        sbw, sbh = fonts.g_scrollbar_width, fonts.g_scrollbar_height
        if this.scrolling == "none":
            this.canvas_w = this.visible_w
            this.canvas_h = this.visible_h
            this.width_per_pixel = chez.div(chez.sub(this.xmax, this.xmin), this.canvas_w)
            this.height_per_pixel = chez.div(chez.sub(this.ymax, this.ymin), this.canvas_h)
            this._set_scroll_region(0, 0, this.canvas_w, this.canvas_h)
        elif this.scrolling == "horizontal":
            this.canvas_w = utilities.round_(chez.mul(this.visible_h, this.canvas_aspect_ratio))
            this.canvas_h = this.visible_h
            this.width_per_pixel = chez.div(chez.sub(this.xmax, this.xmin), this.canvas_w)
            this.height_per_pixel = chez.div(chez.sub(this.ymax, this.ymin), this.canvas_h)
            if (this.canvas_w > this.visible_w
                    and not tell(self, "scrollbar-present?", "horizontal")
                    and utilities.round_(chez.mul(chez.sub(this.visible_h, sbh),
                                                  this.canvas_aspect_ratio)) <= this.visible_w):
                this._set_scroll_region(0, 0, this.visible_w, this.canvas_h)
            elif (this.canvas_w <= this.visible_w
                    and tell(self, "scrollbar-present?", "horizontal")
                    and utilities.round_(chez.mul(chez.add(this.visible_h, sbh),
                                                  this.canvas_aspect_ratio)) > this.visible_w):
                this._set_scroll_region(0, 0, this.visible_w, this.canvas_h)
            else:
                this._set_scroll_region(0, 0, this.canvas_w, this.canvas_h)
        elif this.scrolling == "vertical":
            this.canvas_w = this.visible_w
            this.canvas_h = utilities.round_(chez.div(this.visible_w, this.canvas_aspect_ratio))
            this.width_per_pixel = chez.div(chez.sub(this.xmax, this.xmin), this.canvas_w)
            this.height_per_pixel = chez.div(chez.sub(this.ymax, this.ymin), this.canvas_h)
            if (this.canvas_h > this.visible_h
                    and not tell(self, "scrollbar-present?", "vertical")
                    and utilities.round_(chez.div(chez.sub(this.visible_w, sbw),
                                                  this.canvas_aspect_ratio)) <= this.visible_h):
                this._set_scroll_region(0, 0, this.canvas_w, this.visible_h)
            elif (this.canvas_h <= this.visible_h
                    and tell(self, "scrollbar-present?", "vertical")
                    and utilities.round_(chez.div(chez.add(this.visible_w, sbw),
                                                  this.canvas_aspect_ratio)) > this.visible_h):
                this._set_scroll_region(0, 0, this.canvas_w, this.visible_h)
            else:
                this._set_scroll_region(0, 0, this.canvas_w, this.canvas_h)
        # (printf "~a now ~a~%" (car winname) (tell self 'get-info))
        visible_w, visible_h = this.visible_w, this.visible_h

        def resize():
            if this.aspect_ratio_workaround_p:
                tell(self, "repair-aspect-ratio")
            tell(self, "resize", visible_w, visible_h)
        with _critical_section:
            # a waiting resize is replaced by this one
            try:
                g_resize_message_queue.get_nowait()
            except queue.Empty:
                pass
            g_resize_message_queue.put(resize)
        return None

    # this method should be overridden by windows that can be resized
    @message("resize")
    def resize(this, self, w, h):
        chez.printf("warning: no resize method defined~%")

    @message("get-info")
    def get_info(this, self):
        hsb = "h" if tell(self, "scrollbar-present?", "horizontal") else False
        vsb = "v" if tell(self, "scrollbar-present?", "vertical") else False
        return [["visible:", this.visible_w, "x", this.visible_h],
                ["canvas:", this.canvas_w, "x", this.canvas_h],
                ["scroll:", *utilities.compress([hsb, vsb])]]

    @message("scrollbar-present?")
    def scrollbar_present_p(this, self, orientation):
        return (this.scrolling != "none"
                and exists_p(get_scrollbar_from_frame(this.frame, orientation, False)))

    @message("reposition-vertical-scrollbar")
    def reposition_vertical_scrollbar(this, self):
        if this.visible_h < this.canvas_h:
            # port: the host scrolls its canvas (and its scrollbar follows);
            # the original waited for the Tk scrollbar to appear
            hidden_h = chez.sub(this.canvas_h, this.visible_h)
            hidden_pct = chez.inexact(chez.div(hidden_h, this.canvas_h))
            this.top.set_vertical_view(hidden_pct)
        return "done"

    @message("set-mouse-handlers")
    def set_mouse_handlers(this, self, left_press, right_press):
        this.vp.set_mouse_handlers_bang(left_press, right_press)
        return "done"

    @message("set-window-title")
    def set_window_title(this, self, title):
        # port: the host is the viewport's toplevel
        this.top.set_title_bang(title)
        return "done"

    @message("set-icon-label")
    def set_icon_label(this, self, *args):
        return "ignored"

    @message("set-icon-image")
    def set_icon_image(this, self, *args):
        return "ignored"

    @message("set-background-color")
    def set_background_color(this, self, color):
        this.bg_color = color
        return "done"

    @message("cache-mode?")
    def cache_mode_p_(this, self):
        return this.cache_mode_p

    @message("caching-on")
    def caching_on(this, self):
        this.cache_mode_p = True
        return "done"

    @message("flush")
    def flush(this, self, *tag):
        if this.cached_pexps:
            if not tag:
                sgl.draw_bang(this.vp, tell(self, "get-cached-pexp"))
            else:
                sgl.draw_bang(this.vp, tell(self, "get-cached-pexp"), tag[0])
            this.cached_pexps = []
        # port: swl:sync-display is sgl's flush hook, as in the Racket port
        sgl.g_flush_event_queue()
        this.cache_mode_p = False
        return "done"

    @message("clear-pending-flush")
    def clear_pending_flush(this, self):
        this.cached_pexps = []
        this.cache_mode_p = False
        return "done"

    @message("get-cached-pexp")
    def get_cached_pexp(this, self):
        return ["let-sgl", [], *this.cached_pexps]

    @message("flash")
    def flash(this, self, pexp):
        flash_pause = view_globals.p_flash_pause
        if flash_pause > 0:
            sgl.draw_bang(this.vp, ["let-sgl", [["foreground-color", this.bg_color]], pexp],
                          "background")
            sgl.draw_bang(this.vp, pexp, "flash")
            utilities.pause(flash_pause)
            this.vp.delete("flash")

            def again():
                utilities.pause(flash_pause)
                sgl.draw_bang(this.vp, pexp, "flash")
                utilities.pause(flash_pause)
                this.vp.delete("flash")
            repeat_star_times(chez.sub(view_globals.p_num_of_flashes, 1), again)
            this.vp.delete("background")
        return "done"

    @message("draw")
    def draw(this, self, pexp, *tag):
        fg = view_globals.g_fg_color
        if exists_p(fg):
            pexp = ["let-sgl", [["foreground-color", fg]], pexp]
        if this.cache_mode_p:
            this.cached_pexps.append(pexp)
        elif not tag:
            sgl.draw_bang(this.vp, pexp)
        else:
            sgl.draw_bang(this.vp, pexp, tag[0])
        return "done"

    @message("erase")
    def erase(this, self, pexp):
        return tell(self, "erase-on-background", this.bg_color, pexp)

    @message("erase-on-background")
    def erase_on_background(this, self, color, pexp):
        if this.cache_mode_p:
            this.cached_pexps.append(["erase", color, pexp])
        else:
            sgl.draw_bang(this.vp, ["erase", color, pexp])
        return "done"

    @message("move")
    def move(this, self, dx, dy, tag):
        this.vp.move(dx, dy, tag)
        return "done"

    @message("move-pixels")
    def move_pixels(this, self, dx, dy, tag):
        this.vp.move_pixels(dx, dy, tag)
        return "done"

    @message("raise")
    def raise_(this, self, tag):
        this.vp.raise_(tag)
        return "done"

    @message("unhide")
    def unhide(this, self, tag):
        if sgl.g_tcl_or_tk_version_8_3_p:
            this.vp.unhide(tag)
        return "done"

    @message("retag")
    def retag(this, self, old, new):
        this.vp.retag(old, new)
        return "done"

    @message("rescale")
    def rescale(this, self, tag, xfactor, yfactor):
        this.vp.rescale(tag, xfactor, yfactor)
        return "done"

    @message("delete")
    def delete(this, self, tag):
        this.vp.delete(tag)
        return "done"

    @message("raise-window")
    def raise_window(this, self):
        return this.top.raise_()

    @message("lower-window")
    def lower_window(this, self):
        return this.top.lower()

    @message("clear")
    def clear(this, self):
        if this.cache_mode_p:
            this.cached_pexps.append(["clear", this.bg_color])
        else:
            sgl.draw_bang(this.vp, ["clear", this.bg_color])
        return "done"

    @message("get-center-coord")
    def get_center_coord(this, self):
        return [chez.div(chez.add(this.xmin, this.xmax), 2),
                chez.div(chez.add(this.ymin, this.ymax), 2)]

    @message("get-x-max")
    def get_x_max(this, self):
        return this.xmax

    @message("get-y-max")
    def get_y_max(this, self):
        return this.ymax

    @message("get-visible-x-max")
    def get_visible_x_max(this, self):
        return chez.add(this.xmin, chez.mul(this.width_per_pixel, this.visible_w))

    @message("get-visible-y-min")
    def get_visible_y_min(this, self):
        return chez.sub(this.ymax, chez.mul(this.height_per_pixel, this.visible_h))

    @message("get-width-per-pixel")
    def get_width_per_pixel(this, self):
        return this.width_per_pixel

    @message("get-height-per-pixel")
    def get_height_per_pixel(this, self):
        return this.height_per_pixel

    # For a character of size WIDTH x HEIGHT pixels, the bounding-box
    # coordinates relative to the text baseline offset point are given by
    #    lower left corner  = (-1, -OFFSET - 1)
    #    upper right corner = (WIDTH + 1, HEIGHT + 1 - OFFSET)
    # The baseline offset point is at position (0, OFFSET) in the
    # character's pixel matrix (for left text justification).
    @message("get-character-bounding-box")
    def get_character_bounding_box(this, self, char, font, text_origin):
        x = text_origin[0]
        y = text_origin[1]
        size = tell(font, "get-pixel-size", char)
        width = size[0]
        height = size[1]
        baseline = size[2]
        return [[chez.add(x, chez.mul(this.width_per_pixel, -1)),
                 chez.add(y, chez.mul(this.height_per_pixel, chez.sub(chez.sub(baseline), 1)))],
                [chez.add(x, chez.mul(this.width_per_pixel, chez.add(width, 1))),
                 chez.add(y, chez.mul(this.height_per_pixel,
                                      chez.sub(chez.add(height, 1), baseline)))]]

    # char is a one-character string
    @message("get-character-width")
    def get_character_width(this, self, char, font):
        return chez.mul(this.width_per_pixel, tell(font, "get-pixel-size", char)[0])

    @message("get-character-height")
    def get_character_height(this, self, char, font):
        return chez.mul(this.height_per_pixel, tell(font, "get-pixel-size", char)[1])

    @message("get-text-offset")
    def get_text_offset(this, self, font):
        return chez.mul(this.height_per_pixel,
                        tell(font, "get-pixel-size", chez.String("M"))[2])

    @message("get-string-width")
    def get_string_width(this, self, text_string, font):
        return chez.mul(this.width_per_pixel, tell(font, "get-pixel-width", text_string))

    @message("get-string-height")
    def get_string_height(this, self, font):
        return tell(self, "get-character-height", chez.String("M"), font)

    @message("destroy")
    def destroy(this, self):
        return this.top.destroy()

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_graphics_window(visible_w, visible_h, canvas_w, canvas_h, xmin, ymin, xmax, ymax,
                         bg_color, scrolling):
    """general-graphics.ss: make-graphics-window"""
    return GraphicsWindow(visible_w, visible_h, canvas_w, canvas_h, xmin, ymin, xmax, ymax,
                          bg_color, scrolling)


# For some reason, it takes some time for a scrollbar to show up among
# the children of a frame right after creating a graphics window, so we
# may need to wait for it if it's not yet there.  What a hack.

def get_scrollbar_from_frame(scrollframe, orientation, wait_for_scrollbar_p):
    """general-graphics.ss: get-scrollbar-from-frame"""
    # port: the window host knows its scrollbars (#f for none), so there is
    # nothing to wait for
    return scrollframe.get_scrollbar(orientation)


# ---------------------------------------------------------------------------

def _engine_gg():
    import metacat.general_graphics as gg
    return gg


class ScrollableTextWindow(SchemeObject):
    """general-graphics.ss: make-scrollable-text-window (the closure)"""
    __slots__ = ("visible_w", "visible_h", "bg_color", "font", "centering_p",
                 "paragraphs", "graphics_window")

    def __init__(this, visible_w, visible_h, canvas_h, bg_color):
        this.visible_w, this.visible_h = visible_w, visible_h
        this.bg_color = bg_color
        this.font = p_default_scrollable_text_window_font
        this.centering_p = False
        # <paragraphs> ::= ({<skip-num>+ | <paragraph-string>} ...)
        # paragraphs is a list of paragraph strings (in reverse order),
        # each one separated by one or more line skip numbers
        this.paragraphs = []
        this.graphics_window = make_vertical_scrollable_graphics_window(
            visible_w, visible_h, canvas_h, bg_color)
        tell(this.graphics_window, "reposition-vertical-scrollbar")

    @message("object-type")
    def object_type(this, self):
        return "scrollable-text-window"

    @message("get-font")
    def get_font(this, self):
        return this.font

    @message("get-paragraphs")
    def get_paragraphs(this, self):
        return this.paragraphs

    @message("set-paragraphs")
    def set_paragraphs(this, self, new_paragraphs):
        this.paragraphs = new_paragraphs
        return "done"

    @message("get-lines")
    def get_lines(this, self):
        gg = _engine_gg()
        out = []
        for p in reversed(this.paragraphs):
            if isinstance(p, chez.String):
                out.extend(chez.map_(gg.remove_leading_blanks,
                                     tell(self, "format-paragraph", p)))
            else:
                out.append(p)
        return out

    @message("new-font")
    def new_font(this, self, new_font):
        this.font = new_font
        tell(self, "redraw")
        return tell(this.graphics_window, "reposition-vertical-scrollbar")

    @message("default-font")
    def default_font(this, self):
        this.font = p_default_scrollable_text_window_font
        return tell(self, "redraw")

    @message("centering?")
    def centering_p_(this, self):
        return this.centering_p

    @message("centering-on")
    def centering_on(this, self):
        this.centering_p = True
        return tell(self, "redraw")

    @message("centering-off")
    def centering_off(this, self):
        this.centering_p = False
        return tell(self, "redraw")

    @message("clear")
    def clear(this, self):
        this.paragraphs = []
        return tell(this.graphics_window, "clear")

    @message("draw-paragraph")
    def draw_paragraph(this, self, text_string):
        this.paragraphs = [text_string] + this.paragraphs
        for line in tell(self, "format-paragraph", text_string):
            tell(self, "draw-line", line)
        return tell(self, "newline")

    @message("format-paragraph")
    def format_paragraph(this, self, text_string):
        max_line_length = chez.sub(
            tell(this.graphics_window, "get-x-max"),
            tell(this.graphics_window, "get-character-width", chez.String(" "), this.font))
        return _engine_gg().break_into_lines(this.graphics_window, this.font,
                                             max_line_length, text_string)

    def _centered(this, pexp):
        if this.centering_p:
            return ["let-sgl", [["text-justification", "center"], ["origin", [_HALF, 0]]],
                    pexp]
        return pexp

    @message("draw-line")
    def draw_line(this, self, line):
        line_height = tell(this.graphics_window, "get-string-height", this.font)
        line_pixel_height = tell(this.font, "get-pixel-height")
        line = chez.String(" " + str(_engine_gg().remove_leading_blanks(line)))
        tell(this.graphics_window, "move-pixels", 0, chez.sub(line_pixel_height), "all")
        if view_globals.p_text_scroll_pause > 0:
            utilities.pause(view_globals.p_text_scroll_pause)
        pexp = ["let-sgl", [["font", this.font]],
                ["text", [0, chez.mul(_HALF, line_height)], line]]
        return tell(this.graphics_window, "draw", this._centered(pexp))

    @message("skip")
    def skip(this, self, num_lines):
        this.paragraphs = [num_lines] + this.paragraphs
        skip_height = chez.mul(num_lines, tell(this.font, "get-pixel-height"))
        return tell(this.graphics_window, "move-pixels", 0, chez.sub(skip_height), "all")

    @message("newline")
    def newline(this, self):
        return tell(self, "skip", 1)

    @message("resize")
    def resize(this, self, new_width, new_height):
        this.visible_w = new_width
        this.visible_h = new_height
        tell(self, "redraw")
        return tell(this.graphics_window, "reposition-vertical-scrollbar")

    @message("redraw")
    def redraw(this, self):
        tell(this.graphics_window, "retag", "all", "garbage")
        line_height = tell(this.graphics_window, "get-string-height", this.font)
        tell(this.font, "get-pixel-height")   # line-pixel-height, unused
        line_num = 0
        for x in this.paragraphs:
            if chez.number_p(x):
                line_num = chez.add(line_num, x)
            else:
                for line in reversed(tell(self, "format-paragraph", x)):
                    pexp = ["let-sgl", [["font", this.font]],
                            ["text", [0, chez.mul(chez.add(line_num, _HALF), line_height)],
                             line]]
                    tell(this.graphics_window, "draw", this._centered(pexp))
                    line_num = chez.add(line_num, 1)
        return tell(this.graphics_window, "delete", "garbage")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


def make_scrollable_text_window(visible_w, visible_h, canvas_h, *color):
    """general-graphics.ss: make-scrollable-text-window"""
    bg_color = p_default_bg_color if not color else color[0]
    return ScrollableTextWindow(visible_w, visible_h, canvas_h, bg_color)


def load():
    """port: general-graphics.ss's top-level definitions that need the fonts or
    that the model reads: %default-scrollable-text-window-font% (fonts.ss's
    serif is chosen by fonts.load()), and %default-fg-color% and *fg-color*,
    installed in the engine."""
    global p_default_scrollable_text_window_font
    from metacat import engine
    p_default_scrollable_text_window_font = fonts.swl_font(fonts.serif, 24)
    engine.set_global("%default-fg-color%", p_default_fg_color)
    engine.set_global("*fg-color*", g_fg_color)
