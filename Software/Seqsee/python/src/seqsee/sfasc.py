"""Port of lib/SFasc.pm: a "fascination" strength between 0 and 100."""
from seqsee.util import perl_true


class SFasc:
    """Perl: SFasc (Class::Std). ``SFasc({"strength": 40})`` or ``SFasc(strength=40)``."""

    def __init__(self, opts=None, **kwargs):
        opts = {**(opts or {}), **kwargs}
        strength = opts.get("strength")
        # BUILD: $opts_ref->{strength} || 0
        self._strength = strength if perl_true(strength) else 0

    def get_strength(self):
        return self._strength

    def set_strength(self, strength):
        self._strength = strength
