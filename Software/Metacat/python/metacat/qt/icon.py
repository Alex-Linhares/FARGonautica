"""The window icon: the original's logo, drawn by fonts.ss's create-mcat-logo.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026).  Item 08 of loop0003: the Logo was a
110x80 window of its own (light sky blue, "Metacat" in %logo-font%, sans-serif
18 bold italic, anchored at the bottom of its centre point 55,50); in the Qt
GUI it is the window's and the application's icon, not a pane.  It is drawn
with the same Tk command on a Qt canvas (metacat/qt/canvas.py) and rendered
into images, scaled as vector text, so that the large icon sizes stay sharp.
"""
from __future__ import annotations

import sys

from PySide6.QtCore import QRectF
from PySide6.QtGui import QIcon, QImage, QPainter, QPixmap

LOGO_SIZE = (110, 80)                       # fonts.ss: the logo canvas
ICON_SIZES = (16, 24, 32, 48, 64, 128, 256)


def logo_canvas():
    """fonts.ss: create-mcat-logo's canvas, on a Qt canvas (GUI thread)"""
    from metacat.gui import fonts as gfonts
    from metacat.gui import swl
    from metacat.qt.canvas import QtCanvas
    K = sys.modules.get("metacat.gui.constants")
    background = (K.p_logo_background_color if K is not None
                  else swl.tcl_word("light sky blue"))
    if K is not None and K.p_logo_font:
        font = K.p_logo_font
    else:
        # %logo-font% before gui/constants.py's load(): the same face rule
        font = gfonts.swl_font(gfonts.sans_serif or "helvetica", 18, "bold", "italic")
    canvas = QtCanvas(background=background)
    canvas.tcl("create", "text", 55, 50, "-text", "Metacat", "-anchor", "s", "-font", font)
    canvas.sync()
    return canvas


def _render(canvas, image, target):
    image.fill(canvas.scene.backgroundBrush().color())
    painter = QPainter(image)
    painter.setRenderHint(QPainter.TextAntialiasing)
    canvas.scene.render(painter, target, QRectF(0, 0, *LOGO_SIZE))
    painter.end()
    return image


def logo_image(scale=1, canvas=None):
    """the logo, LOGO_SIZE times scale pixels"""
    canvas = canvas or logo_canvas()
    w, h = LOGO_SIZE
    image = QImage(round(w * scale), round(h * scale), QImage.Format_RGB32)
    return _render(canvas, image, QRectF(0, 0, image.width(), image.height()))


def app_icon():
    """the logo centred on its background in each square icon size"""
    canvas = logo_canvas()
    icon = QIcon()
    w, h = LOGO_SIZE
    for side in ICON_SIZES:
        image = QImage(side, side, QImage.Format_RGB32)
        th = side * h / w
        _render(canvas, image, QRectF(0, (side - th) / 2, side, th))
        icon.addPixmap(QPixmap.fromImage(image))
    return icon

