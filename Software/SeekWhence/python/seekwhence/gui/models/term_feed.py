"""The term feed: the console lines of an interactive run, typed in the GUI and read by
the engine's `Console` on the worker thread (PANES.md P1; G-015). Qt-free.

"In response to a prompt, a user presents to the system numbers which presumably
follow some pattern the user has in mind" [M86 p. 37]. A `Session` without scripted
lines reads them from a stream, `inp`, with `readline()` (`seekwhence.session.Console`,
DECISION D-069). A `TermFeed` is that stream: the GUI puts lines in (`put`, or
`put_words` for several at once, `3 7 3 7`, queued one per prompt), the worker's
`readline` takes the next one or blocks on a `threading.Condition` until there is one,
`close` ends the input (`readline` returns the empty string, the end of the stream, so
the session's outcome is `eof`), and `cancel` makes `readline` raise the run
controller's `RunStopped`, so a stop or a close with the worker waiting for a term
unwinds the run as a stop from any other wait does (G-008's rule for the answerer).

The feed has no `isatty`, so the console echoes each line after the prompt (`please
enter a term: 3` [M86 p. 164]) and the transcript of an interactive run reads as the
transcript of the same run scripted. The lines read are the session's inputs (`read`)
for the session file (item 13).
"""

import threading

from seekwhence.gui.models.run_controller import RunStopped

__all__ = ["TermFeed", "split_words"]


def split_words(text: str) -> list:
    """The console lines in a typed text: words separated by spaces or commas, `3 7 3 7`
    giving four lines, one per prompt (PANES.md P1; G-015)."""
    return [word for word in text.replace(",", " ").split() if word]


class TermFeed:
    """A stream of console lines for `Session(inp=...)`: `readline` on the engine's thread,
    `put`, `put_words`, `close` and `cancel` from the GUI thread (G-015)."""

    def __init__(self):
        self._cond = threading.Condition()
        self._lines: list = []
        self._closed = False
        self._cancelled = False
        self._waiting = False
        self.read: list = []

    @property
    def queued(self) -> tuple:
        """The lines put and not yet read, in order (G-015)."""
        with self._cond:
            return tuple(self._lines)

    @property
    def waiting(self) -> bool:
        """Whether the engine's thread is blocked in `readline` now (G-015)."""
        with self._cond:
            return self._waiting

    @property
    def closed(self) -> bool:
        """Whether `close` was called: once the queue is empty, `readline` gives the end of
        the stream (G-015)."""
        with self._cond:
            return self._closed

    @property
    def cancelled(self) -> bool:
        """Whether `cancel` was called: every `readline` raises `RunStopped` (G-015)."""
        with self._cond:
            return self._cancelled

    def readline(self) -> str:
        """The engine's `Console.ask`: the next line with its newline, blocking until one
        is put; the empty string at the end of a closed feed; `RunStopped` once cancelled
        (DECISION D-069; G-015)."""
        with self._cond:
            while not self._lines and not self._closed and not self._cancelled:
                self._waiting = True
                self._cond.wait()
            self._waiting = False
            if self._cancelled:
                raise RunStopped()
            if self._lines:
                text = self._lines.pop(0)
                self.read.append(text)
                return text + "\n"
            return ""

    def put(self, text: str) -> None:
        """Queue one console line, a term or a command [M86 pp. 164-179] (G-015)."""
        with self._cond:
            self._lines.append(str(text).strip())
            self._cond.notify_all()

    def put_words(self, text: str) -> int:
        """Queue every word of the text as a line; returns how many (G-015)."""
        words = split_words(text)
        with self._cond:
            self._lines.extend(words)
            self._cond.notify_all()
        return len(words)

    def close(self) -> None:
        """End the input: once the queue is read out, `readline` returns the end of the
        stream and the session ends with the outcome `eof` (DECISION D-069; G-015)."""
        with self._cond:
            self._closed = True
            self._cond.notify_all()

    def cancel(self) -> None:
        """Stop: wake a waiting `readline`, which raises `RunStopped`, and make every later
        one raise too (G-015)."""
        with self._cond:
            self._cancelled = True
            self._cond.notify_all()
