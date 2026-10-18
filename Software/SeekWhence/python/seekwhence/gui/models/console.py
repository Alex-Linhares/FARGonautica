"""The console model (P8): the transcript as the CLI prints it, in both the verbose and
the terse mode at once, every line tagged with the index of the event it belongs to
(PANES.md P8; [M86 pp. 164-179]; DECISIONS D-054, D-055, D-069; G-018, G-019). Qt-free.

The transcript has two kinds of line. The lines the engine's events produce are written
by two `seekwhence.trace.Trace` instances, `verbose=True` (the `doing task` lines of runs
1 and 3 [M86 p. 164]) and `verbose=False` (run 2's happenings only [M86 p. 167]), both
with `prompts=True` so that `please enter a term: 3` and `enter no if wrong, ok if right
ok` are written at the `term` and `answered` events [M86 pp. 164, 166]. The other lines
the `Session` writes outside any event, at an *ask*: after the `start` event and after
every `prompt` event it reads one console line and, until that line is a term, echoes a
command with its output (`please enter a term: show-hypothesis` then `(Cycle 3 2 (3 7))`
[M86 p. 166]), `bye`, or an unknown word with the hint (DECISION D-054); when the lines
run out it writes an empty prompt and the run ends (DECISION D-069). The model knows the
run's lines (scripted: all of them from the start; interactive: each one as the GUI puts
it, `put`) and plays that protocol on them at each ask, printing a command from the
`Snapshot` in force at the ask (`models/commands.py`, equal to `Session.command_lines`
then), so the text equals the CLI's for the same seed and inputs, in both modes; the
engine is never asked (G-018). The summary line the CLI prints after a scripted run is
added at `finish` (DECISION D-069).

The lines are append-only: a seek moves the cursor (`position`) and the view greys the
lines after it; an event given again by a seek (its index at or before `seen`) adds
nothing (G-018). `begin(lines)` announces a new run, so that its `start` event clears the
transcript. A command printed when no run reads the GUI's lines (`print_command`: the
show buttons or the command line with no interactive run live) is appended at the cursor's
index and is not an input (G-019).
"""

import dataclasses

from seekwhence.gui.models.commands import command_lines
from seekwhence.session import HINT, PROMPT, parse_line
from seekwhence.trace import Trace

__all__ = ["MODES", "ConsoleLine", "ConsoleModel"]

# The two transcript modes (DECISION D-055).
MODES = ("verbose", "terse")


@dataclasses.dataclass(frozen=True)
class ConsoleLine:
    """One line of the transcript: the `index` of the event it belongs to, its `text`, and
    its `source`: `trace` (written by a `Trace` for the event), `ask` (a prompt echo, a
    command's output or the hint, written at an ask), `command` (printed at the cursor by
    the GUI, not an input) or `summary` (the CLI's last line) (G-018)."""
    index: int
    text: str
    source: str


class _Null:
    """A writer that drops what it is given: the model keeps the traces' lines itself
    (G-018)."""

    def write(self, text: str) -> None:
        """Drop the text (G-018)."""


class ConsoleModel:
    """The transcript of a run in both modes (see the module docstring; G-018, G-019).
    `history` is the window's `EventLog`, read for the snapshot in force at an ask (None:
    every command prints the empty world). `lines[mode]` are the `ConsoleLine`s; `inputs`
    every console line of the run in order, the session's inputs (`reads` pairs each with
    the index of the ask that read it)."""

    def __init__(self, history=None):
        self.history = history
        self.position = -1
        self.seen = -1
        self.fresh = False
        self.traces = {"verbose": Trace(_Null(), verbose=True, prompts=True),
                       "terse": Trace(_Null(), verbose=False, prompts=True)}
        self._clear_lines()
        self._take_inputs(None)

    def _clear_lines(self):
        self.lines = {mode: [] for mode in MODES}
        self.row_after = {mode: [] for mode in MODES}
        for trace in self.traces.values():
            trace.lines.clear()

    def _take_inputs(self, lines):
        self.inputs = [] if lines is None else [str(line).strip() for line in lines]
        self.queue = list(self.inputs)
        self.reads = []
        self.scripted = lines is not None
        self.closed = lines is not None
        self.at_ask = False
        self.ended = False
        self.outcome = None

    # ---- the run ----------------------------------------------------------------------------

    def begin(self, lines=None) -> None:
        """A run is about to start: its scripted console `lines` (terms and commands, as
        the `Session` gets them), or None for a run whose lines the GUI puts as the user
        types them, from now on (G-018). The transcript is cleared at the run's `start`
        event."""
        self.fresh = True
        self._take_inputs(lines)

    def set_position(self, index: int) -> None:
        """The index of the event about to be given, or of the cursor (G-016, G-018)."""
        self.position = index

    def on_event(self, event) -> None:
        """Write the event's lines in both modes, then, at the `start` and `prompt`
        events, play the ask on the queued lines (G-018). An event at or before `seen`
        (a seek giving it again) adds nothing; a `start` after `begin` clears."""
        index = self.position
        if event.kind == "start":
            if not self.fresh and index <= self.seen:
                return
            self._clear_lines()
            self.fresh = False
            self.seen = -1
        elif index <= self.seen:
            return
        self.seen = index
        for mode, trace in self.traces.items():
            trace.on_event(event)
            self._append(mode, trace.lines, "trace", index)
            trace.lines.clear()
        self._mark(index)
        if event.kind in ("start", "prompt"):
            self.at_ask = True
            self.consume()

    def put(self, line: str) -> None:
        """A console line the user typed (a term, a command, `bye` or anything else),
        recorded as an input and read at the ask it reaches, now if the session waits at
        one (G-018, G-019)."""
        text = str(line).strip()
        self.inputs.append(text)
        self.queue.append(text)
        self.consume()

    def close(self) -> None:
        """The input ends: at the next ask with no line left the empty prompt is written
        and the run is over (DECISION D-069; G-018)."""
        self.closed = True
        self.consume()

    def finish(self, outcome) -> None:
        """The run controller's `RunOutcome`: an interactive run that read the end of its
        input gets the empty prompt it ended on; a scripted run gets the summary line the
        CLI prints (DECISION D-069; G-018). A stopped run adds nothing."""
        result = outcome.result
        self.outcome = None if result is None else result["outcome"]
        if result is None:
            return
        if result["outcome"] == "eof":
            self.close()
        if self.scripted:
            self._append_both([result["summary"]], "summary", self.position, mark=True)

    # ---- the ask (DECISIONS D-054, D-069) --------------------------------------------------------

    def consume(self) -> None:
        """Play the console protocol on the queued lines while the session is at an ask:
        a term is read and the ask is over (its echo comes with the `term` event); a
        command is echoed and its output printed from the snapshot at the ask; `bye` ends
        the run; another word gets the hint; no line left and the input closed is the
        empty prompt and the end (DECISIONS D-054, D-069; G-018)."""
        while self.at_ask and not self.ended:
            if not self.queue:
                if self.closed:
                    self._append_both(["", PROMPT], "ask", self.position, mark=True)
                    self.ended = True
                    self.at_ask = False
                return
            line = self.queue.pop(0)
            self.reads.append((self.position, line))
            kind, value = parse_line(line)
            if kind == "term":
                self.at_ask = False
                return
            lines = ["", PROMPT + line]
            if kind == "command":
                lines.extend(command_lines(self.snapshot(), value))
            elif kind == "bye":
                self.ended = True
                self.at_ask = False
            else:
                lines.append(HINT)
            self._append_both(lines, "ask", self.position, mark=True)

    def print_command(self, name: str) -> list:
        """Print a command's lines for the snapshot in force at the cursor, echoed as if
        typed; not an input (G-019). Returns the command's lines."""
        lines = command_lines(self.snapshot(), name)
        self._append_both(["", PROMPT + name, *lines], "command", self.position, mark=False)
        return lines

    def snapshot(self):
        """The snapshot in force at the cursor: `history.state_at(position)`, or None
        (the empty world) before any (G-003, G-018)."""
        if self.history is None or self.position < 0:
            return None
        return self.history.state_at(self.position)

    # ---- the lines ----------------------------------------------------------------------------

    def _append(self, mode, texts, source, index):
        self.lines[mode].extend(ConsoleLine(index, text, source) for text in texts)

    def _append_both(self, texts, source, index, mark):
        for mode in MODES:
            self._append(mode, texts, source, index)
        if mark:
            self._mark(index)

    def _mark(self, index):
        """`row_after[mode][index]` is the number of rows once event `index` and its ask
        are written (G-018)."""
        for mode in MODES:
            rows = self.row_after[mode]
            count = len(self.lines[mode])
            while len(rows) <= index:
                rows.append(count)
            rows[index] = count

    def text(self, mode: str) -> str:
        """The transcript in the mode, one line per row, ending with a newline as the
        CLI's output does (DECISION D-055; G-018)."""
        lines = self.lines[mode]
        return "".join(line.text + "\n" for line in lines)

    def cursor_row(self, mode: str, position=None) -> int | None:
        """The row of the last line written at or before the event at `position` (the
        cursor by default), or None before any (G-018)."""
        position = self.position if position is None else position
        rows = self.row_after[mode]
        if position < 0 or not rows:
            return None
        count = rows[min(position, len(rows) - 1)]
        return count - 1 if count > 0 else None

    def is_future(self, mode: str, row: int, position=None) -> bool:
        """Whether the row's line belongs to an event after `position` (the cursor by
        default): greyed when not live (PANES.md P8; G-018)."""
        position = self.position if position is None else position
        return self.lines[mode][row].index > position

    def find(self, mode: str, needle: str, start: int = 0) -> int | None:
        """The first row at or after `start`, wrapping round, whose text contains the
        needle (case ignored), or None (PANES.md P8's find; G-019)."""
        lines = self.lines[mode]
        needle = needle.lower()
        if not needle or not lines:
            return None
        n = len(lines)
        start = max(0, min(start, n))
        for k in range(n):
            row = (start + k) % n
            if needle in lines[row].text.lower():
                return row
        return None
