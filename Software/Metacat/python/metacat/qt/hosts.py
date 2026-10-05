"""Qt window hosts: each graphics window of Metacat as a pane of the main window.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026), after metacat/gui/hosts.py's TkHost
(SWL's <toplevel> and <frame>/<scrollframe> of make-graphics-window), for
loop0003 item 04 (docs/qt-gui-plan.md 2.4).

`install()` makes make-graphics-window create `QtHost`s: a host is a `Pane`
widget holding a QGraphicsView of its QtCanvas's scene (metacat/qt/canvas.py).
The pane belongs to no window until the main window places it
(MainWindow.place_hosts), so a host never opens a window of its own.

The resize policy (docs/qt-gui-plan.md 2.4) keeps the original's protocol: a
pane's size goes to the viewport as Tk's <Configure> on the canvas did
(`viewport.configure(w+2, h+2)`), once the window is resizable
(make-resizable), and make-resizable's handler and the panel's own resize
method do the rest.  Tk's `wm aspect` has no pane equivalent, so an
unscrollable window's view is letterboxed: the largest rectangle of Tk's ratio
(w+2):(h+2) inside the pane, centred (or at the top), the margins in the
panel's background colour.  A scrolling window's view fills the pane beside
an always-shown scrollbar, as TkHost packs its scrollbars.

general-graphics.ss keeps one waiting resize for all windows (anomalies: "One
resize queue for every window"): when several panes change size at once, only
the last would redraw.  So the hosts hold each pane's latest size and feed the
configures one at a time, each only when the resize queue is empty; the queue
itself is unchanged.

Threads: make the hosts on the GUI thread.  The engine and the resize listener
may then call any host method from any thread: what touches widgets (scroll
region, scrolling, showing and hiding) is recorded and applied by `sync()`, on
the GUI thread, after the canvas's own sync.
"""
from __future__ import annotations

import threading
from fractions import Fraction

from PySide6.QtCore import QCoreApplication, QObject, QRectF, Qt, QThread, QTimer
from PySide6.QtGui import QPalette
from PySide6.QtWidgets import QFrame, QGraphicsView, QStyle, QWidget

from metacat.gui import hosts as ghosts
from metacat.gui import swl
from metacat.qt.canvas import PAINT_GATE, QtCanvas, _qcolor


def on_gui_thread():
    app = QCoreApplication.instance()
    return app is not None and QThread.currentThread() is app.thread()


_collector = None


def collect_on_gui_thread(interval_ms=100):
    """Python's cyclic garbage collector on the GUI thread only (call it on the
    GUI thread; again does nothing).

    The automatic collector runs on whichever thread happens to allocate: the
    engine or the resize listener, in the middle of a canvas command, holding
    PAINT_GATE.  What it frees there includes Qt objects that Python owns and
    that sit in reference cycles: a discarded host's pane (a top-level widget:
    ~QWidget closes its QWindow, which waits for the GUI thread to flush the
    window system events) and its canvas's scene (Qt timers stopped from the
    wrong thread fire later on a freed object).  The first deadlocked, as the
    GUI thread waited for the gate or joined the worker; the second crashed.
    So the automatic collector is turned off, and a timer on the GUI thread
    collects whenever the automatic one would have (gc.get_threshold)."""
    global _collector
    if _collector is not None:
        return
    import gc
    gc.disable()
    _collector = QTimer()
    _collector.setInterval(interval_ms)
    _collector.timeout.connect(_collect)
    _collector.start()


def _collect():
    import gc
    counts, thresholds = gc.get_count(), gc.get_threshold()
    for generation in (2, 1, 0):
        if thresholds[generation] and counts[generation] >= thresholds[generation]:
            gc.collect(generation)
            return


class PaneView(QGraphicsView):
    """the viewport's canvas widget: the scene at one pixel per unit, from
    its top-left corner"""

    def __init__(self, pane, scene):
        super().__init__(scene, pane)
        self.setFrameShape(QFrame.NoFrame)
        self.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setFocusPolicy(Qt.NoFocus)
        self.paints = 0

    def paintEvent(self, event):
        # Entered from Qt, this is one Python call that waits for the GIL; the
        # items' paint calls inside it then find the GIL held.  Without it,
        # each item's paint would wait for the engine thread to let go of the
        # GIL (anomalies: "Qt paints the panes slowly while the engine runs in
        # another thread").
        self.paints += 1
        with PAINT_GATE:
            super().paintEvent(event)

    # --- the mouse: TkHost's three bindings --------------------------------------

    def mousePressEvent(self, event):
        mods = press_modifiers(event.button(), event.modifiers())
        if mods is not None:
            p = event.position()
            # the pane, not an attribute: a Python cycle between the two
            # widgets crashed the cycle collector
            self.parentWidget().host.press(int(p.x()), int(p.y()), mods)
        event.accept()

    def mouseDoubleClickEvent(self, event):
        # Qt sends a double click in place of the second press; Tk sends the
        # second <ButtonPress> (TkHost binds no <Double-...>)
        self.mousePressEvent(event)

    def mouseMoveEvent(self, event):
        event.accept()          # no rubber band, no scene dragging

    def mouseReleaseEvent(self, event):
        event.accept()

    def wheelEvent(self, event):
        # Tk's canvas doesn't scroll on the wheel (TkHost binds none); the
        # scrollbars do, as Tk's do
        event.ignore()


def press_modifiers(button, modifiers):
    """the modifiers that TkHost's bindings give Viewport.mouse_press for a
    press: Tk picks the most specific binding that matches, and a binding
    ignores the modifiers it doesn't name, so <Shift-ButtonPress-1> takes any
    left press with Shift, <ButtonPress-1> any other left press (Control too),
    <ButtonPress-3> every right press; other buttons have no binding (None)"""
    if button == Qt.LeftButton:
        if modifiers & Qt.ShiftModifier:
            return ("shift", "left-button")
        return ("left-button",)
    if button == Qt.RightButton:
        return ("right-button",)
    return None


class Pane(QWidget):
    """port: SWL's <toplevel> and its frame, as a pane: the view of a host's
    canvas, laid out by the host's resize policy"""

    def __init__(self, host):
        super().__init__()
        self.host = host
        self.v_align = "center"      # "top" for the Temperature (docs/qt-gui-plan.md 2.4)
        self.view = PaneView(self, host.canvas.scene)
        self.setAutoFillBackground(True)
        self.background = None
        self.set_background()

    def set_background(self):
        """the letterbox margins, in the panel's background colour (painted by
        Qt itself: no Python paintEvent)"""
        background = swl.tcl_word(self.host.canvas.background)
        if background != self.background:
            self.background = background
            palette = self.palette()
            palette.setColor(QPalette.Window, _qcolor(background))
            self.setPalette(palette)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        self.host.pane_resized()


class QtHost(ghosts.OffscreenHost):
    """port: make-graphics-window's <toplevel> and frame, as a pane of the main
    window (gui/hosts.py's TkHost, in Qt)"""

    def __init__(self, scrolling, destroy_action):
        super().__init__(scrolling, destroy_action)
        self.pane = None
        self.resizable = False
        self.aspect = None
        self._resized = False         # the pane's size changed since the last configure
        self._lock = threading.Lock()
        self._scroll_region = None
        self._vertical_view = None
        self._visibility = None

    def make_canvas(self, visible_w, visible_h, bg_color):
        self.width, self.height = visible_w, visible_h
        # Tk's wm aspect, as make-resizable sets it: SWL's 2-pixel border counted
        self.aspect = Fraction(visible_w + 2, visible_h + 2)
        self.canvas = QtCanvas(bg_color)
        self.pane = Pane(self)
        view = self.pane.view
        horizontal = self.scrolling in ("horizontal", "both")
        vertical = self.scrolling in ("vertical", "both")
        view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOn if horizontal
                                          else Qt.ScrollBarAlwaysOff)
        view.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn if vertical
                                        else Qt.ScrollBarAlwaysOff)
        view.horizontalScrollBar().valueChanged.connect(self._scrolled)
        view.verticalScrollBar().valueChanged.connect(self._scrolled)
        view.setSceneRect(QRectF(0, 0, visible_w, visible_h))
        sbw, sbh = self._scrollbar_sizes()
        w, h = visible_w + (sbw if vertical else 0), visible_h + (sbh if horizontal else 0)
        self.pane.resize(w, h)
        view.setGeometry(0, 0, w, h)
        return self.canvas

    def _scrollbar_sizes(self):
        from metacat.gui import fonts as gfonts
        if not gfonts.g_scrollbar_width:
            install_scrollbar_sizes()
        return gfonts.g_scrollbar_width, gfonts.g_scrollbar_height

    def _scrolled(self, _value):
        view = self.pane.view
        self.canvas.set_origin(view.horizontalScrollBar().value(),
                               view.verticalScrollBar().value())

    # --- the resize policy -----------------------------------------------------

    def canvas_geometry(self, pw, ph):
        """the view's rectangle (x, y, w, h) in a pane pw x ph, and the canvas
        size the viewport is told (w, h)"""
        if self.scrolling == "none":
            r = self.aspect
            if pw * r.denominator >= ph * r.numerator:      # wider than the ratio
                w, h = min(pw, round(ph * r)), ph
            else:
                w, h = pw, min(ph, round(pw / r))
            x = (pw - w) // 2
            y = 0 if self.pane.v_align == "top" else (ph - h) // 2
            return (x, y, w, h), (w, h)
        sbw, sbh = self._scrollbar_sizes()
        w = pw - (sbw if self.scrolling in ("vertical", "both") else 0)
        h = ph - (sbh if self.scrolling in ("horizontal", "both") else 0)
        return (0, 0, pw, ph), (w, h)

    def pane_resized(self):
        """the pane's resizeEvent (GUI thread): lay out the view, and tell the
        viewport its new size as Tk's <Configure> did, through the feeder"""
        (x, y, w, h), size = self.canvas_geometry(self.pane.width(), self.pane.height())
        self.pane.view.setGeometry(x, y, w, h)
        self.pane.update()
        if size != (self.width, self.height):
            # even if it comes back to its size before the feeder's turn: Tk
            # would have sent both configures, and the view's scroll position
            # may have been clamped in between
            self._resized = True
        _feeder().request(self, size)

    def deliver_configure(self, size):
        """the feeder's turn for this host (GUI thread): Tk's <Configure> on
        SWL's viewport, as TkHost._configure sends it"""
        w, h = size
        if self.resizable and self.viewport is not None and w > 2 and h > 2 and self._resized:
            self._resized = False
            self.width, self.height = w, h
            self.viewport.configure(w + 2, h + 2)
            return True
        return False

    def press(self, x, y, mods):
        """a mouse press on the view (GUI thread), as TkHost._press: the
        viewport's handler runs here, on the GUI thread, as Tk ran it on Tk's"""
        if self.viewport is not None:
            try:
                self.viewport.mouse_press(x, y, mods)
            except Exception:   # noqa: BLE001 - SWL reported handler errors and went on
                import traceback
                traceback.print_exc()

    # --- SWL's toplevel and frame ----------------------------------------------

    def set_scroll_region_bang(self, x1, y1, x2, y2):
        with self._lock:
            self._scroll_region = tuple(float(swl.tcl_word(v)) for v in (x1, y1, x2, y2))

    def set_vertical_view(self, fraction):
        with self._lock:
            self._vertical_view = float(fraction)

    def show_window(self):
        self.visible = True
        with self._lock:
            self._visibility = True

    def hide_window(self):
        self.visible = False
        with self._lock:
            self._visibility = False

    def set_resizable_bang(self, w, h):
        self.resizable = bool(w and h)
        if self.resizable and self.pane is not None and on_gui_thread():
            self.pane_resized()

    def get_scrollbar(self, orientation):
        if self.pane is None:
            return False
        view = self.pane.view
        if orientation == "horizontal" and self.scrolling in ("horizontal", "both"):
            return view.horizontalScrollBar()
        if orientation == "vertical" and self.scrolling in ("vertical", "both"):
            return view.verticalScrollBar()
        return False

    def sync(self):
        """bring the scene and the view up to date (GUI thread only); true when
        the pane was shown or hidden"""
        self.canvas.sync()
        self.pane.set_background()
        with self._lock:
            region, self._scroll_region = self._scroll_region, None
            vview, self._vertical_view = self._vertical_view, None
            visibility, self._visibility = self._visibility, None
        view = self.pane.view
        if region is not None:
            x1, y1, x2, y2 = region
            view.setSceneRect(QRectF(x1, y1, x2 - x1, y2 - y1))
        if vview is not None:
            view.verticalScrollBar().setValue(round(vview * view.sceneRect().height()))
        if visibility is not None and self.pane.parent() is not None:
            self.pane.setVisible(visibility)
            return True     # the main window updates its splitters
        return False


class _Feeder(QObject):
    """the configures of the panes, one at a time: each host's latest size
    waits until general-graphics.ss's resize queue is empty"""

    def __init__(self):
        super().__init__()
        self.pending = {}             # host -> (w, h), in the order of the requests
        self.timer = QTimer(self)
        self.timer.setInterval(10)
        self.timer.timeout.connect(self.feed)

    def request(self, host, size):
        self.pending.pop(host, None)
        self.pending[host] = size
        if not self.timer.isActive():
            self.timer.start()

    def busy(self):
        return bool(self.pending)

    def feed(self):
        from metacat.gui import general_graphics as gg
        while self.pending and gg.g_resize_message_queue.empty():
            host = next(iter(self.pending))
            size = self.pending.pop(host)
            if host.deliver_configure(size):
                break                 # its resize is queued: the next one waits
        if not self.pending:
            self.timer.stop()


_the_feeder = None


def _feeder():
    global _the_feeder
    if _the_feeder is None:
        _the_feeder = _Feeder()
    return _the_feeder


def resize_listener_running():
    return any(t.name == "resize-listener" and t.is_alive() for t in threading.enumerate())


def ensure_resize_listener():
    """general-graphics.ss's start-resize-listener, unless one runs already"""
    from metacat.gui import general_graphics as gg
    if not resize_listener_running():
        gg.start_resize_listener()


def settle_resizes(process_events, timeout=30):
    """process events (on the GUI thread) until every pane's configure has been
    delivered and every queued resize has run.  True if settled in time."""
    import time
    from metacat.gui import general_graphics as gg
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        process_events()
        if _feeder().busy() or not gg.g_resize_message_queue.empty():
            time.sleep(0.005)
            continue
        if not resize_listener_running():
            return True
        # the listener runs requests in order: once this one has run, the
        # resizes before it have finished
        done = threading.Event()
        gg.g_resize_message_queue.put(done.set)
        while not done.wait(0.005):
            process_events()
            if time.monotonic() > deadline:
                return False
        process_events()
        if not _feeder().busy():
            return True
    return False


def install_scrollbar_sizes():
    """fonts.ss's scrollbar sizes (create-mcat-logo measured a Tk scrollbar):
    Qt's scroll bar extent"""
    from PySide6.QtWidgets import QApplication
    from metacat.gui import fonts as gfonts
    extent = QApplication.style().pixelMetric(QStyle.PM_ScrollBarExtent)
    gfonts.g_scrollbar_width = extent
    gfonts.g_scrollbar_height = extent


def qt_host_maker():
    """a window host maker for gui/hosts.py's set_window_host_maker"""
    return QtHost


def install():
    """the Qt GUI's part of setup.ss's setup before the windows are made (GUI
    thread): fonts.ss's faces and hidden canvas in Qt (metacat/qt/fonts.py),
    the scrollbar sizes, and Qt hosts for make-graphics-window; and the garbage
    collector on the GUI thread (collect_on_gui_thread)"""
    from metacat.qt import fonts
    collect_on_gui_thread()
    fonts.install()
    install_scrollbar_sizes()
    ghosts.set_window_host_maker(qt_host_maker())
