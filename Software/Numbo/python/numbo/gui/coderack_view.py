"""The coderack view: the urgency bins, their counts and the codelets waiting.

CoderackView is a QWidget and a GUI-thread observer (MainWindow.add_view).
It holds a coderack_model.CoderackModel:
  - on_event(event) updates the model (and notes whether the event was a
    choice, and the largest bin so far), nothing else;
  - redraw() computes the rows (coderack_rows, a pure function, so the
    tests can check them against the model) and paints now, so the run
    waits for the picture.

What it shows, top to bottom:
  - the codelet just chosen (chosen_text), on a magenta-tinted band when
    the latest event was that choice;
  - how many codelets wait, and the most this run has had;
  - one row per urgency bin, most urgent first: the urgency, a bar of the
    count (out of SCALE, the largest bin so far this run, at least
    MIN_SCALE, so the bars don't jump at every post), and the codelets
    waiting, grouped by name, newest first ("const-bl+ ×3 · decomp+").
    The chosen codelet's bin is outlined in magenta when the latest event
    was the choice; a bin the latest event changed is tinted yellow.
"""

import dataclasses

from PySide6.QtCore import QRectF, QSize, Qt
from PySide6.QtGui import QColor, QFont, QFontMetricsF, QPainter, QPen
from PySide6.QtWidgets import QSizePolicy, QWidget

from numbo.models.coderack_model import CoderackModel, bin_summary, codelet_text

__all__ = ["MIN_SCALE", "CoderackRow", "coderack_rows", "chosen_text", "CoderackView"]

MIN_SCALE = 10
CHOICE_KINDS = ("codelet-chosen", "setup-choose")
BACKGROUND = "#fbfbf8"
TEXT, MUTED = "#1a1a1a", "#6b6b6b"
BAR, BAR_TRACK = "#5b7fb8", "#e6e9ef"
CHOSEN, CHOSEN_TINT = "#e6007e", "#fbe3f0"
CHANGED_TINT = "#fff4c2"
PAD = 6
URGENCY_W, BAR_W = 40, 96
DEFAULT_ROWS = 7


@dataclasses.dataclass(frozen=True)
class CoderackRow:
    """A bin: URGENCY, COUNT, GROUPS ((codelet name, count), ...) newest
    first; CHANGED if the latest event changed it, CHOSEN if it was the
    choice of a codelet from this bin."""
    urgency: object
    count: int
    groups: tuple
    changed: bool
    chosen: bool


def coderack_rows(model, chosen_fresh):
    """MODEL's bins as CoderackRows; CHOSEN_FRESH: the latest event was
    the choice in model.chosen."""
    c = model.chosen
    chosen_urgency = (c.codelet.urgency if chosen_fresh and c is not None
                      and c.codelet is not None else None)
    return tuple(CoderackRow(urgency=u, count=len(cs), groups=bin_summary(cs),
                             changed=u in model.changed,
                             chosen=chosen_urgency is not None and u == chosen_urgency)
                 for u, cs in model.bins())


def chosen_text(choice):
    """A Choice as a line of text."""
    if choice is None:
        return "nothing chosen yet"
    when = "set-up" if choice.setup else f"iteration {choice.n}"
    if choice.codelet is None:
        return f"{when}: nothing (the rack was empty)"
    return f"{when}: {codelet_text(choice.codelet)}  (urgency {choice.codelet.urgency})"


def chosen_lines(choice):
    """A Choice as the view draws it: (when and how urgent, the form)."""
    if choice is None:
        return ("nothing chosen yet", "")
    when = "set-up" if choice.setup else f"iteration {choice.n}"
    if choice.codelet is None:
        return (when, "nothing (the rack was empty)")
    return (f"{when}, urgency {choice.codelet.urgency}", codelet_text(choice.codelet))


def groups_text(groups):
    return " · ".join(name.lower() + (f" ×{n}" if n > 1 else "") for name, n in groups)


class CoderackView(QWidget):
    """The coderack (see the module docstring).  ROWS: the CoderackRows
    drawn; CHOSEN_TEXT, CHOSEN_FRESH, TOTAL, SCALE; PAINTS: how many times
    it has been painted."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Preferred)
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(9)
        self.bold = QFont(self.font_)
        self.bold.setBold(True)
        self.metrics = QFontMetricsF(self.font_)
        self.line = self.metrics.height() + 4
        self.model = CoderackModel()
        self.chosen_fresh = False
        self.rows = ()
        self.chosen_text = chosen_text(None)
        self.total = 0
        self.scale = MIN_SCALE
        self.paints = 0
        self.redraw()

    def sizeHint(self):
        return QSize(380, round(self.line * (DEFAULT_ROWS + 3) + 3 * PAD))

    def minimumSizeHint(self):
        return QSize(260, round(self.line * (DEFAULT_ROWS + 3) + 3 * PAD))

    def on_event(self, event):
        if event.kind == "start":
            self.scale = MIN_SCALE
        self.model.on_event(event)
        self.chosen_fresh = event.kind in CHOICE_KINDS
        if event.kind == "post":
            # Kept here, not in redraw, so that a replay that jumps to an
            # event (and draws only there) has the live run's scale.
            self.scale = max(self.scale, len(self.model.bin(event.urgency)))

    def redraw(self, now=True):
        """Recompute the rows, paint now."""
        m = self.model
        self.rows = coderack_rows(m, self.chosen_fresh)
        self.chosen_text = chosen_text(m.chosen)
        self.total = m.total
        if self.isVisible():
            self.repaint() if now else self.update()

    def paintEvent(self, event):
        self.paints += 1
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.fillRect(self.rect(), QColor(BACKGROUND))
        width = self.width()
        line = self.line
        y = PAD
        room = width - 2 * PAD
        # The codelet just chosen: when, then its form on a line of its own.
        when, form = chosen_lines(self.model.chosen)
        if self.chosen_fresh:
            p.fillRect(QRectF(PAD / 2, y - 2, width - PAD, 2 * line + 2), QColor(CHOSEN_TINT))
        p.setFont(self.bold)
        p.setPen(QColor(CHOSEN if self.chosen_fresh else TEXT))
        label = "chosen  "
        lw = QFontMetricsF(self.bold).horizontalAdvance(label)
        p.drawText(QRectF(PAD, y, lw, line), Qt.AlignmentFlag.AlignVCenter, label)
        p.setFont(self.font_)
        p.setPen(QColor(MUTED))
        p.drawText(QRectF(PAD + lw, y, room - lw, line), Qt.AlignmentFlag.AlignVCenter,
                   self.metrics.elidedText(when, Qt.TextElideMode.ElideRight, room - lw))
        y += line
        p.setPen(QColor(TEXT))
        p.drawText(QRectF(PAD + lw, y, room - lw, line), Qt.AlignmentFlag.AlignVCenter,
                   self.metrics.elidedText(form, Qt.TextElideMode.ElideRight, room - lw))
        y += line
        p.setPen(QColor(MUTED))
        p.drawText(QRectF(PAD, y, width - 2 * PAD, line), Qt.AlignmentFlag.AlignVCenter,
                   f"{self.total} waiting (at most {self.model.max_total} this run)"
                   if self.model.name else "no coderack yet")
        y += line + PAD
        # The bins.
        text_x = PAD + URGENCY_W + PAD + BAR_W + PAD
        for r in self.rows:
            box = QRectF(PAD / 2, y, width - PAD, line)
            if r.changed:
                p.fillRect(box, QColor(CHANGED_TINT))
            if r.chosen:
                p.fillRect(box, QColor(CHOSEN_TINT))
                p.setPen(QPen(QColor(CHOSEN), 2))
                p.setBrush(Qt.BrushStyle.NoBrush)
                p.drawRoundedRect(box.adjusted(1, 1, -1, -1), 3, 3)
            p.setFont(self.bold)
            p.setPen(QColor(TEXT))
            p.drawText(QRectF(PAD, y, URGENCY_W, line),
                       Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter, str(r.urgency))
            track = QRectF(PAD + URGENCY_W + PAD, y + 3, BAR_W, line - 6)
            p.fillRect(track, QColor(BAR_TRACK))
            bar = BAR_W * r.count / self.scale
            if r.count:
                p.fillRect(QRectF(track.x(), track.y(), bar, track.height()), QColor(BAR))
            p.setFont(self.font_)
            # The count in white inside a long bar, else dark just past its end.
            count = str(r.count)
            if bar >= self.metrics.horizontalAdvance(count) + 8:
                p.setPen(QColor("#ffffff"))
                p.drawText(track.adjusted(4, 0, 0, 0), Qt.AlignmentFlag.AlignVCenter, count)
            else:
                p.setPen(QColor(TEXT))
                p.drawText(track.adjusted(bar + 4, 0, 0, 0), Qt.AlignmentFlag.AlignVCenter, count)
            p.setPen(QColor(TEXT if r.count else MUTED))
            room = width - PAD - text_x
            p.drawText(QRectF(text_x, y, room, line), Qt.AlignmentFlag.AlignVCenter,
                       self.metrics.elidedText(groups_text(r.groups) or "—",
                                               Qt.TextElideMode.ElideRight, room))
            y += line
        p.end()

    def render_png(self, path):
        """Grab the widget to a PNG at PATH."""
        image = self.grab()
        if not image.save(path):
            raise OSError(f"could not write {path}")
        return image
