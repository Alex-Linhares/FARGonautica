"""The Commentary dock (lib/Tk/SCommentary.pm): the message log and the answer buttons.

As in Perl: a read-only, word-wrapped text (10 × 60 characters, GUI_sparse.conf's font) with
the [SCommentary_tags] colours and fonts, and on its right four answer buttons (disabled and
blank until a question comes) above "Start Debug". ``insert`` is MessageRequiringNoResponse;
``ask`` shows a question (MessageRequiringAResponse / MessageRequiringBooleanResponse); a button
or the keys 1-4 answer it, ``answered(question, value)`` carries the answer (the chosen string;
1/0 for a yes/no question) and the response is logged in red. The pure part (Tk insert
arguments, tags, main::message) is ``seqsee.gui.commentary``.

The keys 1-4 work anywhere in the window while a question waits (Perl binds them on the Text,
which takes the focus and a grab while it waits).
"""
from PySide6.QtCore import QSize, Qt, Signal
from PySide6.QtGui import QFontMetrics, QKeySequence, QShortcut, QTextCharFormat, QTextCursor, \
    QTextOption
from PySide6.QtWidgets import QDockWidget, QHBoxLayout, QPushButton, QSizePolicy, QTextEdit, \
    QVBoxLayout, QWidget

from seqsee.gui import commentary as cm

from . import render

BUTTON_FONT = "Helvetica -12 bold"     # Tk's default button font on X11

class CommentaryText(QTextEdit):
    """The ROText: read-only, word wrap, sized for TEXT_HEIGHT lines of TEXT_WIDTH chars."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setReadOnly(True)
        self.setLineWrapMode(QTextEdit.WidgetWidth)
        self.setWordWrapMode(QTextOption.WrapAtWordBoundaryOrAnywhere)
        self.setFont(render.qfont(cm.TEXT_FONT))
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Preferred)

    def sizeHint(self):
        fm = QFontMetrics(self.font())
        margin = 2 * (self.frameWidth() + int(self.document().documentMargin()))
        return QSize(cm.TEXT_WIDTH * fm.averageCharWidth() + margin,
                     cm.TEXT_HEIGHT * fm.lineSpacing() + margin)


class Commentary(QWidget):
    """Tk::SCommentary. ``log`` mirrors the text as (text, tags) runs."""

    answered = Signal(object, object)   # the question, the answer
    debug_clicked = Signal()            # "Start Debug"

    def __init__(self, parent=None):
        super().__init__(parent)
        self.log = cm.Log()
        self._pending = None
        self.active_count = 0
        self.text = CommentaryText()
        layout = QHBoxLayout(self)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.addWidget(self.text)
        # The button frame is packed on the right, centred vertically: four buttons, then
        # Start Debug right below them.
        column = QVBoxLayout()
        column.setSpacing(2)
        layout.addLayout(column)
        column.addStretch(1)
        font = render.qfont(BUTTON_FONT)
        width = QFontMetrics(font).averageCharWidth() * cm.BUTTON_WIDTH + 12
        self.buttons = []
        for i in range(cm.BUTTON_COUNT):
            b = QPushButton("")
            b.setFont(font)
            b.setEnabled(False)
            b.setFixedWidth(width)
            b.setFocusPolicy(Qt.NoFocus)
            b.clicked.connect(lambda _=False, i=i: self.choose(i))
            column.addWidget(b)
            self.buttons.append(b)
        self.debug_button = QPushButton(cm.DEBUG_BUTTON_TEXT)
        self.debug_button.setFont(font)
        self.debug_button.setFixedWidth(width)
        self.debug_button.setFocusPolicy(Qt.NoFocus)
        self.debug_button.clicked.connect(self.debug_clicked)
        column.addWidget(self.debug_button)
        column.addStretch(1)
        self.shortcuts = []
        for key in range(1, cm.BUTTON_COUNT + 1):
            sc = QShortcut(QKeySequence(str(key)), self)
            sc.setContext(Qt.WindowShortcut)
            sc.activated.connect(lambda key=key: self.press_key(key))
            self.shortcuts.append(sc)
        self._formats = {}

    @property
    def pending(self):
        return self._pending

    # ---- the text -----------------------------------------------------------------------
    def _format(self, tags):
        if tags not in self._formats:
            style = cm.style_of(tags)
            fmt = QTextCharFormat()
            if style.foreground:
                fmt.setForeground(render.qcolor(style.foreground))
            fmt.setFont(render.qfont(style.font or cm.TEXT_FONT))
            self._formats[tags] = fmt
        return self._formats[tags]

    def insert(self, *parts):
        """MessageRequiringNoResponse: ``$Text->insert('end', @parts); $Text->see('end')``."""
        cursor = QTextCursor(self.text.document())
        cursor.movePosition(QTextCursor.End)
        for text, tags in self.log.insert(*parts):
            cursor.insertText(text, self._format(tuple(sorted(set(tags)))))
        bar = self.text.verticalScrollBar()
        bar.setValue(bar.maximum())

    def clear(self):
        self.log.clear()
        self.text.clear()

    # ---- questions ----------------------------------------------------------------------
    def ask(self, question):
        """Show a ``runner.Question`` (boolean or response) and enable its buttons."""
        choices = cm.BOOLEAN_CHOICES if question.kind == "boolean" else question.choices
        self.insert(*question.parts)
        self._pending = question
        self._choices = tuple(choices)
        self.active_count = min(len(choices), cm.BUTTON_COUNT)
        for i, b in enumerate(self.buttons):
            active = i < self.active_count
            b.setText(self._choices[i] if active else "")
            b.setEnabled(active)
        self.text.setFocus()

    def press_key(self, key):
        """<KeyPress-1..4>: the button, unless past the active ones."""
        index = cm.key_choice(key, self.active_count if self._pending is not None else 0)
        if index is not None:
            self.choose(index)

    def choose(self, index):
        """A button press: log the response, reset the buttons, emit ``answered``."""
        q = self._pending
        if q is None or index >= self.active_count:
            return
        response = self._choices[index]
        self._reset_buttons()
        self.insert(*cm.response_parts(response))
        value = cm.boolean_value(response) if q.kind == "boolean" else response
        self.answered.emit(q, value)

    def close_question(self, question):
        """The runner gave up on ``question`` (new sequence, quit): no answer."""
        if self._pending is not question:
            return
        self._reset_buttons()
        self.insert("  (cancelled)\n")

    def _reset_buttons(self):
        self._pending = None
        self.active_count = 0
        for b in self.buttons:
            b.setText("")
            b.setEnabled(False)


class CommentaryDock(QDockWidget):
    """The Commentary as a dock (below the canvas, where GUI_sparse.conf packs it)."""

    def __init__(self, parent=None):
        super().__init__("Commentary", parent)
        self.setObjectName("commentary")
        self.setFeatures(QDockWidget.DockWidgetMovable | QDockWidget.DockWidgetFloatable)
        self.setWidget(Commentary())
