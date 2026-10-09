"""An automatic answerer on the main window: it answers the run's questions as a user would,
by pressing the Commentary's buttons (lib/Tk/SCommentary.pm's MessageRequiringBooleanResponse
/ MessageRequiringAResponse) and typing into SGUI::ask_for_more_terms' window. The decisions
are ``seqsee.gui.autoanswer.Answerer``'s, from the question's text and the number of elements
in the snapshot on screen (the runner sends one just before each question).

The model ends a run command itself after each extension the user verifies
(DoInsertBookKeeping and SWorkspace's rule-app check set ``$Global::Break_Loop``); a Perl
user then presses Continue again, and ``cli.run_headless`` calls ``interaction_step_n``
again. With ``keep_going``, so does this answerer, until max steps. Before confirming
DescribeSolution's "Does this generate the sequence you had in mind?" it presses Pause
(``pause_on_solution``), so the run stops after the step that found the solution, as
``cli.run_headless`` stops on an accepted solution. Used by the end-to-end test.
"""
from PySide6.QtCore import QObject, QTimer

from seqsee.gui import autoanswer


class AutoAnswerer(QObject):
    """Answers ``window``'s questions. ``answers`` lists (kind, text, answer); ``accepted``
    tells whether a solution was confirmed; ``continues`` counts the Continue presses after
    a run command ended early; ``errors`` collects the runner's model errors."""

    def __init__(self, window, known, pause_on_solution=True, keep_going=True):
        super().__init__(window)
        self.window = window
        self.answerer = autoanswer.Answerer(known)
        self.pause_on_solution = pause_on_solution
        self.keep_going = keep_going
        self.answers = []
        self.errors = []
        self.accepted = False
        self.continues = 0
        runner = window.runner
        # Connected after the window's own slots: the question is on screen when it arrives.
        runner.question.connect(self._question)
        runner.error.connect(lambda message, tb="": self.errors.append(message))
        runner.command_done.connect(self._command_done)

    def _command_done(self, name, result):
        if name != "continue" or not self.keep_going or self.accepted or self.errors:
            return
        if self.window.run_state == "paused":   # the user pressed Pause: don't go on
            return
        snap = self.window.snapshot
        if snap is None or snap.steps >= self.window.max_steps():
            return
        self.continues += 1
        QTimer.singleShot(0, lambda: self.window.run_command("continue"))

    def _question(self, question):
        QTimer.singleShot(0, lambda: self._answer(question))

    def _answer(self, q):
        snap = self.window.snapshot
        count = snap.element_count if snap is not None else 0
        value = self.answerer.decide(q.kind, q.text, tuple(q.choices), count)
        if q.kind == "more_terms":
            dialog = self.window.more_terms_dialog
            if dialog is None or self.window._more_terms_question is not q:
                return
            self.answers.append((q.kind, q.text, value))
            if value is None:
                dialog.close()
            else:
                dialog.entry.setText(value)
                dialog.entry.returnPressed.emit()
            return
        commentary = self.window.commentary
        if commentary.pending is not q:
            return                      # cancelled meanwhile
        if q.kind == "response" and self.answerer.is_solution_question(q.text):
            if value == "Yes":
                self.accepted = True
                if self.pause_on_solution:
                    self.window.run_command("pause")
        self.answers.append((q.kind, q.text, value))
        index = [b.text() for b in commentary.buttons].index(value)
        commentary.buttons[index].click()
