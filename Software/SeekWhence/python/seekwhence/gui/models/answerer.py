"""The interactive answerer: the engine's guess answered by the user's click (G-008).

When the hypothesis has earned a guess the program shows "the user a synopsis of its
model" [M86 p. 7] and asks `enter no if wrong, ok if right` [M86 p. 166]. The engine asks
through its `answerer` hook (`filtering.make_guess`, DECISION D-064), synchronously, on
the thread that runs the engine: in the GUI, the run controller's worker. The `GuessMade`
event has already been delivered and drawn by then (the gate keeps the display in step,
PANES.md section 1), so an `InteractiveAnswerer` only has to tell the GUI that a guess is
waiting (`on_guess`, called on the worker; the window turns it into a queued signal) and
block the worker on a `threading.Event` until the GUI gives the answer (`give`) or stops
the run (`cancel`, which raises the controller's `RunStopped` on the worker, so the run
unwinds as a stop from any other wait does).

The answer is written after the prompt on the same line, as `Console.tell` writes a
scripted answer (`enter no if wrong, ok if right ok` [M86 p. 166]), so the transcript of
an interactive run reads as the transcript of the same run scripted, and the events are
the same (tested). Whatever its source, an answer is input (ENGINE-API.md section 3):
`given` keeps every answer for the session file. Qt-free.
"""

import dataclasses
import threading

from seekwhence.gui.models.run_controller import RunStopped
from seekwhence.session import ANSWER_PROMPT

__all__ = ["GuessPending", "InteractiveAnswerer"]


@dataclasses.dataclass(frozen=True)
class GuessPending:
    """A guess waiting for the user's answer: the hypothesis's form, the block the program
    predicts and its terms, as `GuessMade` carries them [M86 p. 166] (G-008)."""
    form: str
    block: str
    terms: tuple


class InteractiveAnswerer:
    """The `Answerer` (DECISION D-064) of an interactive run: `answer` blocks the engine's
    thread until `give(answer)` or `cancel()` is called from another thread (G-008).
    `console` is the session's console (the hook is called with it); `on_guess(pending)`
    is called on the engine's thread when a guess is waiting."""

    def __init__(self, console, on_guess):
        self.console = console
        self.on_guess = on_guess
        self.given: list = []
        self._event = threading.Event()
        self._lock = threading.Lock()
        self._answer = None
        self._cancelled = False
        self._pending = None

    @property
    def pending(self):
        """The guess waiting for an answer, as a `GuessPending`, or None once it is
        answered or cancelled (G-008)."""
        return self._pending

    @property
    def cancelled(self) -> bool:
        """Whether `cancel` was called: every later `answer` stops the run (G-008)."""
        return self._cancelled

    def answer(self, world, form: str, block: str, terms: tuple) -> str:
        """The engine's call, on its own thread: announce the guess, wait for the answer,
        write it after the prompt as a scripted answer is written [M86 p. 166]
        (DECISIONS D-064, D-069; G-008). Raises `RunStopped` when cancelled."""
        del world
        with self._lock:
            if self._cancelled:
                raise RunStopped()
            self._event.clear()
            self._answer = None
            pending = self._pending = GuessPending(form, block, tuple(terms))
        self.on_guess(pending)
        self._event.wait()
        with self._lock:
            self._pending = None
            if self._cancelled:
                raise RunStopped()
            answer = self._answer
        self.console.tell("\n" + ANSWER_PROMPT, answer)
        self.given.append(answer)
        return answer

    def give(self, answer: str) -> bool:
        """The user's answer (`ok`, `no`, ...), from any thread; False if no guess was
        waiting or the answerer was cancelled (G-008)."""
        with self._lock:
            if self._pending is None or self._cancelled:
                return False
            self._answer = str(answer)
            self._pending = None
            self._event.set()
            return True

    def cancel(self) -> None:
        """Stop: wake a waiting `answer`, which raises `RunStopped`, and make every later
        one raise too (G-008)."""
        with self._lock:
            self._cancelled = True
            self._pending = None
            self._event.set()
