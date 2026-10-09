"""Port of lib/SAction.pm: a codelet that runs immediately (ACTION) instead of waiting on the
coderack. Unlike SCodelet there is no freshness check."""
from seqsee import global_ as Global
from seqsee import scodelet_base, util
from seqsee.scodelet_base import SCodeletBase, check_attributes


class SAction(SCodeletBase):
    """Perl: package SAction (Moose, extends SCodeletBase). ``SAction(params)`` is
    ``SAction->new({family => ..., urgency => ..., arguments => ...})``."""

    perl_name = "SAction"

    def __init__(self, params):
        check_attributes(params)
        super().__init__(params["family"], params["urgency"], params["arguments"])

    def conditionally_run(self):
        """Perl: conditionally_run. Runs with probability urgency/100 (one draw)."""
        if not util.toss(util.perl_num(self.urgency) / 100):
            return None
        return self.run()

    def run(self):
        """Perl: ``before 'run'`` (the debugMAX message), then SCodeletBase::run.

        SAction has no as_text, so StringifyForCarp gives "reftype=SAction".
        """
        if util.perl_true(Global.debugMAX):
            scodelet_base._message([self.family, "green",
                                    "About to run: " + util.stringify_for_carp(self)])
        return self._base_run()
