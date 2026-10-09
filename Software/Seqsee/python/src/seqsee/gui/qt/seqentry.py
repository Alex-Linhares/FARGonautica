"""Sequence entry windows: lib/SGUI.pm's ask_seq and ask_for_more_terms as Qt dialogs.

- ``SequenceDialog`` ("Seqsee Sequence Entry"): as ask_seq packs it, the prompt on the left,
  an editable combo (config/sequence.list, sorted as Tk::ComboEntry shows it) at the top, Go
  under it on the right, the message label at the bottom. Return in the combo, Go, or picking
  a row from its list (ComboEntry's Select puts the row in the entry and invokes) runs
  ``seqentry.check_sequence``: "Illformed input: …" in the label, or ``accepted_terms(text,
  terms)`` and the dialog closes. Not modal, like the Toplevel: the model can keep running
  and the main window stays usable.
- ``MoreTermsDialog`` ("Request for more terms"): the prompt over an entry; Return sends
  ``entered(text)`` (unchecked, as Perl's <Return> handler) and closes; closing it otherwise
  sends ``dismissed`` (waitWindow returns, nothing is inserted).
"""
from PySide6.QtCore import Qt, Signal
from PySide6.QtWidgets import QComboBox, QDialog, QGridLayout, QLabel, QLineEdit, \
    QPushButton, QVBoxLayout

from seqsee.gui import seqentry


class SequenceDialog(QDialog):
    """SGUI::ask_seq's Toplevel."""

    accepted_terms = Signal(str, list)     # the text typed, its terms

    def __init__(self, sequences=None, parent=None):
        super().__init__(parent)
        self.setWindowTitle(seqentry.TITLE)
        self.setModal(False)
        self.setAttribute(Qt.WA_DeleteOnClose, False)
        if sequences is None:
            sequences = seqentry.read_sequence_list()
        self._done = False
        self.prompt_label = QLabel(seqentry.PROMPT)
        self.combo = QComboBox()
        self.combo.setEditable(True)
        self.combo.setInsertPolicy(QComboBox.NoInsert)
        self.combo.addItems(seqentry.combo_list(sequences))
        self.combo.setCurrentIndex(-1)
        self.combo.setEditText("")
        self.combo.setMinimumContentsLength(seqentry.COMBO_WIDTH)
        self.combo.setSizeAdjustPolicy(QComboBox.AdjustToMinimumContentsLengthWithIcon)
        edit = self.combo.lineEdit()
        self.go_button = QPushButton(seqentry.GO)
        self.go_button.setAutoDefault(False)
        self.message_label = QLabel("")
        self.message_label.setAlignment(Qt.AlignCenter)

        grid = QGridLayout(self)
        grid.addWidget(self.prompt_label, 0, 0, 3, 1, Qt.AlignVCenter)
        grid.addWidget(self.combo, 0, 1)
        grid.addWidget(self.go_button, 1, 1, Qt.AlignRight)
        grid.addWidget(self.message_label, 2, 1)
        grid.setColumnStretch(1, 1)

        edit.returnPressed.connect(self.invoke)
        self.go_button.clicked.connect(self.invoke)
        self.combo.activated.connect(self.select_row)
        edit.setFocus()

    def select_row(self, row):
        """ComboEntry's Select: the row's text goes into the entry, then -invoke."""
        if row < 0 or self._done:
            return
        self.combo.setEditText(self.combo.itemText(row))
        self.invoke()

    def invoke(self):
        """``$check_and_accept_input_sequence->($comboentry->get, $label)``."""
        if self._done:      # (an editable combo's Return also emits activated)
            return
        text = self.combo.currentText()
        check = seqentry.check_sequence(text)
        if check.message is not None:
            self.message_label.setText(check.message)
        if not check.accepted:
            return
        self._done = True
        self.close()
        self.accepted_terms.emit(text, list(check.terms))

    def keyPressEvent(self, event):
        # QDialog would close on Escape and swallow Return as "default button"; the
        # Toplevel has neither.
        if event.key() in (Qt.Key_Return, Qt.Key_Enter, Qt.Key_Escape):
            event.accept()
            return
        super().keyPressEvent(event)


class MoreTermsDialog(QDialog):
    """SGUI::ask_for_more_terms' Toplevel."""

    entered = Signal(str)       # the text, at <Return>
    dismissed = Signal()        # closed without <Return>

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle(seqentry.MORE_TERMS_TITLE)
        self.setModal(False)
        self._finished = False
        self.prompt_label = QLabel(seqentry.MORE_TERMS_PROMPT)
        self.entry = QLineEdit()
        self.entry.setFixedWidth(seqentry.MORE_TERMS_WIDTH
                                 * self.entry.fontMetrics().averageCharWidth() + 12)
        box = QVBoxLayout(self)
        box.addWidget(self.prompt_label, 0, Qt.AlignHCenter)
        box.addWidget(self.entry, 0, Qt.AlignHCenter)
        self.entry.returnPressed.connect(self._return)
        self.entry.setFocus()

    def _return(self):
        if self._finished:
            return
        self._finished = True
        text = self.entry.text()
        self.close()
        self.entered.emit(text)

    def close_quietly(self):
        """Close without ``dismissed`` (the runner gave up on the question)."""
        self._finished = True
        self.close()

    def closeEvent(self, event):
        super().closeEvent(event)
        if not self._finished:
            self._finished = True
            self.dismissed.emit()

    def keyPressEvent(self, event):
        if event.key() in (Qt.Key_Return, Qt.Key_Enter, Qt.Key_Escape):
            event.accept()
            return
        super().keyPressEvent(event)
