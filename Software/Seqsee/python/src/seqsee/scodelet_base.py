"""Port of lib/SCodeletBase.pm: the base of SCodelet and SAction.

A codelet has a ``family`` (Moose ``Str``), an ``urgency`` and ``arguments`` (normally a
dict). ``run`` sets ``$Global::CurrentCodelet``/``CurrentCodeletFamily`` and calls the
family's run sub (``Seqsee::SCF::<family>::run``), which lives in the registry of
``seqsee.codelets.family``.

``_message(msg)`` is main::message (the debugMAX "About to run" messages); it only logs.
"""
import logging

from seqsee import global_ as Global
from seqsee import util
from seqsee.errors import Confess

_log = logging.getLogger(__name__)


def _message(msg):
    """Perl: main::message($msg)."""
    _log.debug("%s", msg)


def _is_str(value):
    """Moose ``Str``: a defined non-reference."""
    return value is not None and util._is_scalar(value)


def check_attributes(params):
    """The Moose constructor checks on a params dict: required attributes (by key presence,
    in name order; undef is fine), then family's ``Str``."""
    from seqsee.objects.object import _type_error
    for attr in ("arguments", "family", "urgency"):
        if attr not in params:
            raise Confess(f"Attribute ({attr}) is required")
    if not _is_str(params["family"]):
        raise _type_error("family", "Str", params["family"])


class SCodeletBase:
    """Perl: package SCodeletBase (Moose)."""

    perl_name = "SCodeletBase"

    def __init__(self, family, urgency, arguments):
        check_attributes({"family": family, "urgency": urgency, "arguments": arguments})
        self.family = family
        self.urgency = urgency
        self.arguments = arguments

    def _base_run(self):
        """Perl: SCodeletBase::run (the method the subclasses wrap)."""
        Global.CurrentCodelet = self
        Global.CurrentCodeletFamily = self.family
        from seqsee.codelets import family as scf_family
        return scf_family.family_run(self.family, self, self.arguments)

    def run(self):
        return self._base_run()
