"""The Qt canvas: the panels' Tk canvas commands, drawn on a QGraphicsScene.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026), from Tk 8.6's X11 drawing of canvas items
(DisplayRectOval, DisplayArc, DisplayLine, DisplayPolygon, DisplayCanvText), for
loop0003 item 02.

``QtCanvas`` has the panel canvas interface of ``swl.TkCanvas``: ``tcl(*args)``
(the original's arguments: symbols, numbers, Scheme strings, colours, fonts),
``get_background_color()`` and ``set_background_color_bang(color)``.  It has two
layers (docs/qt-gui-plan.md 2.6):

1. the display list (metacat/qt/displaylist.py): Tk's semantics and answers,
   updated at once in the calling thread, whichever it is;
2. the scene: ``sync()``, on the GUI thread, brings the QGraphicsScene up to date
   with the display list's changes: one ``TkItem`` per canvas item, z values in
   stacking order, hidden items invisible, the background colour.

The scene's coordinates are Tk's canvas coordinates, one pixel per unit.  Items
are drawn as Tk draws them on X11: corners rounded to whole pixels, pens of
Tk's widths with butt caps (round joins for lines and polygons), Tk's dash
patterns, no antialiasing except for text.
"""
from __future__ import annotations

import threading

from PySide6.QtCore import QPointF, QRectF, Qt
from PySide6.QtGui import QBrush, QColor, QPainter, QPen, QPolygonF
from PySide6.QtWidgets import QGraphicsRectItem, QGraphicsScene

from metacat.gui import swl
from metacat.qt import fonts
from metacat.qt.displaylist import (DisplayList, TclError, bezier_points, color_rgb,  # noqa: F401
                                    dash_lengths)


# The paint gate.  The GUI thread holds it while it brings the scenes up to
# date (MainWindow.sync) and while a view paints (PaneView.paintEvent); a
# canvas command waits for it.  So the engine thread, which draws all the
# time, stops drawing for those moments: it waits on a lock, without the GIL,
# and the GUI thread's Python runs at full speed instead of sharing the GIL
# in 5 ms turns; and no item changes while it is painted.  The GUI thread
# waits at most for one canvas command, which never waits for the GUI thread
# (docs/qt-gui-plan.md 2.5, deadlock rules).
PAINT_GATE = threading.RLock()


class QtCanvas:
    """port: an SWL <canvas> on a QGraphicsScene.  measure: the text measurer
    (default: metacat.qt.fonts.metrics(), the fonts the scene draws with).
    Make it on the GUI thread; then any thread may call tcl."""

    def __init__(self, background=None, measure=None):
        from metacat.gui.colors import c_white
        self.display_list = DisplayList(measure or fonts.metrics())
        self.background = background if background is not None else c_white
        self.scene = QGraphicsScene()
        self.graphics = {}            # id -> TkItem
        self._background_dirty = True
        self._lock = threading.Lock()

    def tcl(self, *args):
        """port: the canvas's widget command, answering as swl.TkCanvas does"""
        words = [swl.tcl_word(a) for a in args]
        with PAINT_GATE:
            answer = self.display_list.tcl(*words)
        return swl._from_tcl(answer)

    def get_background_color(self):
        """port: SWL <canvas> get-background-color"""
        return self.background

    def set_background_color_bang(self, color):
        """port: SWL <canvas> set-background-color!"""
        self.background = color
        self._background_dirty = True

    def set_origin(self, x, y):
        """the view's scroll offset, for canvasx and canvasy (GUI thread)"""
        with self.display_list.lock:
            self.display_list.x_origin, self.display_list.y_origin = x, y

    def sync(self):
        """bring the scene up to date with the display list (GUI thread only)"""
        with self._lock:
            if self._background_dirty:
                self._background_dirty = False
                self.scene.setBackgroundBrush(QBrush(_qcolor(swl.tcl_word(self.background))))
            changed, deleted, z = self.display_list.take_changes()
            for i in deleted:
                g = self.graphics.pop(i, None)
                if g is not None:
                    self.scene.removeItem(g)
            measure = self.display_list.measure
            for item in changed:
                g = self.graphics.get(item.id)
                if g is None:
                    g = self.graphics[item.id] = TkItem(item, measure)
                    self.scene.addItem(g)
                else:
                    g.set_item(item, measure)
            if z is not None:
                for i, value in z.items():
                    g = self.graphics.get(i)
                    if g is not None:
                        g.setZValue(value)


class HiddenCanvas(QtCanvas):
    """port: fonts.ss's *hidden-canvas*, a canvas never shown that measures text
    by creating it and asking its bbox.  No scene follows it, so the changes
    are dropped at each delete instead of piling up for a sync.

    fonts.ss's get-pixel-size creates a text, asks its bbox and deletes all, in
    three commands: two threads measuring at once (the engine and the resize
    listener) delete each other's items, as they could in the original.  So
    each thread has a display list of its own; the answers are the same."""

    def __init__(self, background=None, measure=None):
        self._local = threading.local()
        self._measure = measure or fonts.metrics()
        super().__init__(background, self._measure)

    @property
    def display_list(self):
        d = getattr(self._local, "display_list", None)
        if d is None:
            d = self._local.display_list = DisplayList(self._measure)
        return d

    @display_list.setter
    def display_list(self, value):
        self._local.display_list = value

    def tcl(self, *args):
        answer = super().tcl(*args)
        if args and str(args[0]) == "delete":
            self.display_list.take_changes()
        return answer


def _qcolor(word):
    r, g, b = color_rgb(word)
    return QColor(r, g, b)


def _px(v):
    """port: Tk_CanvasDrawableCoords: a canvas coordinate as a whole pixel"""
    return int(v + 0.5) if v > 0 else int(v - 0.5)


class _Recorder:
    """a stand-in for a QPainter that records the calls made on it, to be
    replayed on a real one: (QPainter's method, its arguments)"""

    def __init__(self):
        self.ops = []

    def __getattr__(self, name):
        method = getattr(QPainter, name)

        def record(*args):
            self.ops.append((method, args))
        return record


class TkItem(QGraphicsRectItem):
    """One canvas item in the scene, painted as Tk paints it on X11.

    Its bounding rectangle is a QGraphicsRectItem's rect (with no pen), so
    that Qt's own boundingRect answers it: the scene asks every changed
    item's from C++, and a Python override would wait for the GIL each time,
    up to 5 ms while the engine thread runs (anomalies: "Qt paints the panes
    slowly while the engine runs in another thread").  paint is Python, but
    the views call it inside their paint pass, which PaneView.paintEvent
    enters from Python with the GIL held, and it replays the painter calls
    recorded at its first paint."""

    def __init__(self, item, measure):
        super().__init__()
        self.setPen(Qt.NoPen)
        self.set_item(item, measure)

    def set_item(self, item, measure):
        self.prepareGeometryChange()
        self.item_id = item.id
        self.item = item
        self.measure = measure
        if item.kind == "text":
            self.layout = item.text_layout(measure)
        box = item.bbox(measure, ignore_state=True) or [0, 0, 0, 0]
        self._rect = QRectF(box[0] - 2, box[1] - 2, box[2] - box[0] + 4, box[3] - box[1] + 4)
        self.setRect(self._rect)
        self.setZValue(item.z)
        self.setVisible(item.state != "hidden")
        self.ops = None               # recorded at the first paint
        self.update()

    # --- pens

    def _pen(self, color, cap=Qt.FlatCap, join=Qt.MiterJoin):
        o = self.item.opts
        width = int(o["-width"] + 0.5)
        pen = QPen(QColor(*color))
        pen.setWidth(width)
        pen.setCapStyle(cap)
        pen.setJoinStyle(join)
        dashes = dash_lengths(o["-dash"], o["-width"])
        if dashes:
            unit = max(width, 1)
            pen.setDashPattern([max(d, 1) / unit for d in dashes])
        return pen

    def paint(self, painter, option, widget=None):
        # the painter calls are made once, at the first paint after a change
        # (most items are deleted before they are ever painted), and replayed
        ops = self.ops
        if ops is None:
            recorder = _Recorder()
            self._paint_item(recorder)
            ops = self.ops = recorder.ops
        for method, args in ops:
            method(painter, *args)

    def _paint_item(self, painter):
        painter.setRenderHint(QPainter.Antialiasing, False)
        getattr(self, "_paint_" + self.item.kind)(painter)

    def _corners(self):
        c = self.item.coords
        x1, y1, x2, y2 = _px(c[0]), _px(c[1]), _px(c[2]), _px(c[3])
        return x1, y1, max(x2, x1 + 1), max(y2, y1 + 1)

    def _paint_rectangle(self, painter):
        x1, y1, x2, y2 = self._corners()
        fill, outline = self.item.color("-fill"), self.item.color("-outline")
        if fill:
            painter.fillRect(x1, y1, x2 - x1, y2 - y1, QColor(*fill))
        if outline:
            painter.setPen(self._pen(outline))
            painter.setBrush(Qt.NoBrush)
            painter.drawRect(QRectF(x1, y1, x2 - x1, y2 - y1))

    def _paint_oval(self, painter):
        x1, y1, x2, y2 = self._corners()
        fill, outline = self.item.color("-fill"), self.item.color("-outline")
        if fill:
            painter.setPen(Qt.NoPen)
            painter.setBrush(QColor(*fill))
            painter.drawEllipse(QRectF(x1, y1, x2 - x1, y2 - y1))
        if outline:
            painter.setPen(self._pen(outline))
            painter.setBrush(Qt.NoBrush)
            painter.drawEllipse(QRectF(x1, y1, x2 - x1, y2 - y1))

    def _paint_arc(self, painter):
        item = self.item
        x1, y1, x2, y2 = self._corners()
        rect = QRectF(x1, y1, x2 - x1, y2 - y1)
        start = int(16 * item.opts["-start"] + 0.5)
        extent = int(16 * item.opts["-extent"] + 0.5)
        style = item.opts["-style"]
        fill, outline = item.color("-fill"), item.color("-outline")
        if fill and extent and style != "arc":
            painter.setPen(Qt.NoPen)
            painter.setBrush(QColor(*fill))
            (painter.drawPie if style == "pieslice" else painter.drawChord)(rect, start, extent)
        if outline:
            painter.setPen(self._pen(outline))
            painter.setBrush(Qt.NoBrush)
            if extent:
                painter.drawArc(rect, start, extent)
            p1, p2, centre = item.arc_points()
            p1, p2, centre = (QPointF(_px(p[0]), _px(p[1])) for p in (p1, p2, centre))
            if style == "chord":
                painter.drawLine(p1, p2)
            elif style == "pieslice":
                painter.drawLine(p1, centre)
                painter.drawLine(p2, centre)

    def _paint_line(self, painter):
        item = self.item
        fill = item.color("-fill")
        if not fill:
            return
        pts, first, last = item.line_geometry()
        pts = [tuple(p) for p in pts]
        if item.smooth():
            pts = bezier_points(pts, closed=False)
        painter.setPen(self._pen(fill, Qt.FlatCap, Qt.RoundJoin))
        painter.setBrush(Qt.NoBrush)
        painter.drawPolyline(QPolygonF([QPointF(_px(x), _px(y)) for x, y in pts]))
        painter.setPen(Qt.NoPen)
        painter.setBrush(QColor(*fill))
        for poly in (first, last):
            if poly:
                painter.drawPolygon(QPolygonF([QPointF(_px(x), _px(y)) for x, y in poly]))

    def _paint_polygon(self, painter):
        item = self.item
        c = item.coords
        pts = [(c[i], c[i + 1]) for i in range(0, len(c), 2)]
        if item.smooth():
            pts = bezier_points(pts, closed=True)
        polygon = QPolygonF([QPointF(_px(x), _px(y)) for x, y in pts])
        fill, outline = item.color("-fill"), item.color("-outline")
        if fill:
            painter.setPen(Qt.NoPen)
            painter.setBrush(QColor(*fill))
            painter.drawPolygon(polygon, Qt.OddEvenFill)
        if outline:
            painter.setPen(self._pen(outline, Qt.FlatCap, Qt.RoundJoin))
            painter.setBrush(Qt.NoBrush)
            painter.drawPolygon(polygon, Qt.OddEvenFill)

    def _paint_text(self, painter):
        item = self.item
        fill = item.color("-fill")
        if not fill:
            return
        font = item.opts["-font"]
        left, top, width, _, lines = self.layout
        linespace, ascent = self.measure.linespace(font), self.measure.ascent(font)
        justify = item.opts["-justify"]
        painter.setRenderHint(QPainter.TextAntialiasing, True)
        painter.setFont(fonts.qfont(font))
        painter.setPen(QColor(*fill))
        for k, (line, w) in enumerate(lines):
            x = left
            if justify == "center":
                x += (width - w) // 2
            elif justify == "right":
                x += width - w
            painter.drawText(QPointF(x, top + k * linespace + ascent), line)
