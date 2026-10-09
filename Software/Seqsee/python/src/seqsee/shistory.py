"""Port of lib/SHistory.pm: a log of timestamped messages kept by workspace objects.

Messages look like ``"[<steps>]<runnable>\\t<msg>"``, using ``Global.Steps_Finished``
and ``Global.CurrentRunnableString`` at the time of the call.
"""
import re

from seqsee import global_ as Global
from seqsee.errors import Confess
from seqsee.util import perl_num, perl_str, perl_true

_STEP_RE = re.compile(r"^\[(\d+)\]")


def history_string(msg):
    """Perl: SHistory::history_string."""
    steps = Global.Steps_Finished if perl_true(Global.Steps_Finished) else 0
    return f"[{perl_str(steps)}]{perl_str(Global.CurrentRunnableString)}\t{msg}"


class SHistory:
    """Perl: SHistory (Class::Std)."""

    def __init__(self):
        self._messages = [history_string("created")]
        self._message_count = 0   # Perl: %message_count_of, never read.
        self._dob = Global.Steps_Finished

    def get_history(self):
        """Returns the live message list (Perl returns the array ref)."""
        return self._messages

    def add_history(self, msg):
        """Perl: AddHistory."""
        self._messages.append(history_string(msg))
        self._message_count += 1

    def search_history(self, pattern):
        """Perl: search_history. Returns the step numbers (strings) of messages.

        PERL-QUIRK: ``grep $re, @messages`` only tests ``$re`` for truth, so every
        message matches when the pattern is true (any compiled regex) and none
        when it is false ("" or "0").
        """
        if not perl_true(pattern):
            return []
        return [_STEP_RE.match(m).group(1) for m in self._messages]

    def unchanged_since(self, since):
        """Perl: UnchangedSince. 1 if the last message is from step <= since, else 0."""
        last = self._messages[-1]
        m = _STEP_RE.match(last)
        if not m:
            raise Confess(f"Huh '{last}'")
        return 0 if int(m.group(1)) > perl_num(since) else 1

    def get_age(self):
        """Perl: GetAge."""
        return perl_num(Global.Steps_Finished) - perl_num(self._dob)

    def history_as_text(self):
        return "\n".join(["History:", *self._messages])
