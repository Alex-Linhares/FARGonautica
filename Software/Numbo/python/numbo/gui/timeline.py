"""The timeline: where the views are in the run's history, and the controls
to move there.  A slider over the events (it seeks when released, or at
each click or key: a jump on a long run re-feeds the models, so it doesn't
seek at every pixel of a drag), one event back and forward, and a jump to
an iteration.  It only asks: seek_requested(index) and
iteration_requested(n) go to the window, which can refuse (a run is live).
"""

from PySide6.QtCore import Qt, Signal
from PySide6.QtWidgets import QHBoxLayout, QLabel, QPushButton, QSlider, QSpinBox, QWidget

__all__ = ["TimelineBar"]


class TimelineBar(QWidget):
    seek_requested = Signal(int)
    iteration_requested = Signal(int)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.count = 0
        self.position = -1
        self.back_button = QPushButton("◀")
        self.back_button.setToolTip("one event back")
        self.forward_button = QPushButton("▶")
        self.forward_button.setToolTip("one event forward (drawn as a step)")
        for b in (self.back_button, self.forward_button):
            b.setFixedWidth(32)
        self.slider = QSlider(Qt.Orientation.Horizontal)
        self.slider.setTracking(False)
        self.slider.setRange(0, 0)
        # The mouse's only: the window's left and right arrows move one event.
        self.slider.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.label = QLabel()
        # A fixed width: the text changes at every event, and a label whose
        # size can change lays out its parents again each time.
        self.label.setFixedWidth(self.label.fontMetrics().horizontalAdvance(
            "event 000000 of 000000, iteration 000000 …") + 8)
        self.label.setFixedHeight(self.label.sizeHint().height())
        self.iteration_spin = QSpinBox()
        self.iteration_spin.setRange(0, 0)
        self.iteration_spin.setToolTip("an iteration of the run")
        self.go_button = QPushButton("Go to iteration")
        layout = QHBoxLayout(self)
        layout.setContentsMargins(6, 2, 6, 2)
        layout.addWidget(self.back_button)
        layout.addWidget(self.forward_button)
        layout.addWidget(self.slider, 1)
        layout.addWidget(self.label)
        layout.addWidget(self.iteration_spin)
        layout.addWidget(self.go_button)

        self.slider.valueChanged.connect(self.seek_requested)
        self.slider.sliderMoved.connect(self._preview)
        self.back_button.clicked.connect(lambda: self.seek_requested.emit(self.position - 1))
        self.forward_button.clicked.connect(lambda: self.seek_requested.emit(self.position + 1))
        self.go_button.clicked.connect(
            lambda: self.iteration_requested.emit(self.iteration_spin.value()))
        self.set_position(-1, 0, None, None)

    def _text(self, index, iteration):
        if self.count == 0:
            return "no events"
        text = f"event {index} of {self.count}"
        if iteration is not None:
            text += f", iteration {iteration}"
        return text

    def _preview(self, value):
        self.label.setText(self._text(value, None) + " …")

    def set_position(self, index, count, iteration, iterations):
        """The views show event INDEX of COUNT, in ITERATION; ITERATIONS:
        (first, last) or None."""
        self.position, self.count = index, count
        s = self.slider
        s.blockSignals(True)
        if s.maximum() != max(count - 1, 0):
            s.setMaximum(max(count - 1, 0))
        if s.value() != max(index, 0):
            s.setValue(max(index, 0))
        s.blockSignals(False)
        lo, hi = iterations if iterations is not None else (0, 0)
        if (self.iteration_spin.minimum(), self.iteration_spin.maximum()) != (lo, hi):
            self.iteration_spin.setRange(lo, hi)
        text = self._text(index, iteration)
        if self.label.text() != text:
            self.label.setText(text)

    def set_scrubbable(self, on):
        """Whether the controls move the views (not while a run is live)."""
        for w in (self.slider, self.back_button, self.forward_button, self.iteration_spin,
                  self.go_button):
            w.setEnabled(on)
