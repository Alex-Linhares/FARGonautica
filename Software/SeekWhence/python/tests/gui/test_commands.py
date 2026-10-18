"""Loop0004 item 03: the console commands rebuilt from a snapshot
(`seekwhence/gui/models/commands.py`), Qt-free (G-017).

At every prompt, guess and end of the three appendix runs, `command_lines(snapshot,
name)` equals `Session.command_lines(name)` at that moment, for the four commands
[M86 pp. 166-179]; before any snapshot the lines are the empty world's.
"""

import io

import pytest

from seekwhence.gui.models.commands import COMMANDS, command_lines
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX


class Checker:
    """An observer, subscribed after the tap, that compares the tap's snapshot with what
    the session would print at the same moment."""

    def __init__(self, log):
        self.log = log
        self.session = None
        self.checked = 0
        self.mismatches = []

    def on_event(self, event):
        if self.session is None or event.kind not in ("prompt", "guess", "ended"):
            return
        snapshot = self.log.last_snapshot
        for name in COMMANDS:
            expected = self.session.command_lines(name)
            got = command_lines(snapshot, name)
            if got != expected:
                self.mismatches.append((event.kind, name, got, expected))
        self.checked += 1


@pytest.mark.parametrize("run_id,seed", APPENDIX)
def test_the_commands_equal_the_sessions_at_every_prompt(appendix_runs, run_id, seed):
    ref = appendix_runs[run_id]
    log = EventLog()
    tap = Tap(log)
    checker = Checker(log)
    session = Session(ref.config, seed, out=io.StringIO(), lines=list(ref.lines),
                      answers=list(ref.answers), observers=[tap, checker])
    tap.attach_session(session)
    checker.session = session
    session.run()
    assert checker.mismatches == []
    assert checker.checked == log.counts["prompt"] + log.counts["guess"] + 1
    assert command_lines(log.last_snapshot, "show") == ref.show == session.show_lines()


def test_the_empty_world_before_any_snapshot():
    assert command_lines(None, "show-hypothesis") == ["nil"]
    assert command_lines(None, "show-seq") == [""]
    assert command_lines(None, "show-parse") == ["()"]
    assert command_lines(None, "show") == ["terms of the sequence:", "", "", "bonds:", "",
                                           "gloms:", "", "gnoths:", ""]
    with pytest.raises(ValueError):
        command_lines(None, "show-everything")


def test_the_commands_are_the_sessions():
    assert COMMANDS == ("show", "show-hypothesis", "show-seq", "show-parse")
