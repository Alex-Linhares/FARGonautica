"""Port of SNodeActivation.pm: the activation of an LTM node.

A Perl blessed array, so ``SNodeActivation`` subclasses ``list``::

    [raw activation, depth reciprocal, real activation]

The real activation is ``PRECALCULATED[raw activation]`` (SLinkActivation's table, indexed
the Perl way: see ``slink_activation.perl_index``).

PERL-QUIRK (not reproducible): Perl confesses "Load order issues" when SNodeActivation is
loaded before SLinkActivation. Here this module imports slink_activation itself.

Perl builds DecayManyTimes/SpikeSeveral/WeakenSeveral with string ``eval``; here they are
plain functions.
"""
from seqsee import util
from seqsee.errors import Confess
from seqsee.slink_activation import PRECALCULATED, perl_index

RAW_ACTIVATION = 0
DEPTH_RECIPROCAL = 1
REAL_ACTIVATION = 2

Initial_Raw_Activation = 2
Initial_Depth = 5
Initial_Depth_Reciprocal = 1 / Initial_Depth
_Initial_Real_Activation = PRECALCULATED[Initial_Raw_Activation]


class SNodeActivation(list):
    """Perl: SNodeActivation (blessed array)."""

    perl_name = "SNodeActivation"
    RAW_ACTIVATION = RAW_ACTIVATION
    DEPTH_RECIPROCAL = DEPTH_RECIPROCAL
    REAL_ACTIVATION = REAL_ACTIVATION

    def __init__(self, depth_reciprocal=None):
        """Perl: new($depth_reciprocal). A false depth reciprocal means 1/5; any other value
        is stored as given (a string stays a string)."""
        super().__init__([
            Initial_Raw_Activation,
            depth_reciprocal if util.perl_true(depth_reciprocal) else Initial_Depth_Reciprocal,
            _Initial_Real_Activation,
        ])


def _amount(x):
    """Perl ``$x ||= 1``."""
    return util.perl_num(x) if util.perl_true(x) else 1


def _last_real(activations):
    """Perl ``$_[-1][2]``; on an empty list the autovivification dies."""
    if not activations:
        raise Confess("Modification of non-creatable array value attempted, subscript -1")
    return activations[-1][REAL_ACTIVATION]


def _lower(act, amount):
    n = util.perl_num
    act[0] = n(act[0]) - n(act[1]) * amount
    if act[0] < 2:
        act[0] = 2
    act[2] = perl_index(PRECALCULATED, act[0])


def decay_many_times(times, *activations):
    """Perl: DecayManyTimes($times, @activations). ``$times ||= 1``. Returns nothing."""
    times = _amount(times)
    for act in activations:
        _lower(act, times)


def spike_several(spike, *activations):
    """Perl: SpikeSeveral($spike, @activations). ``$spike ||= 1``. Above 98 the raw
    activation drops to 90 and the node gets deeper. Returns the last one's real activation.

    PERL-QUIRK: there is no floor, so a negative spike can drive the raw activation below
    zero, and the real activation is then read from the end of the table (or is undef)."""
    spike = _amount(spike)
    n = util.perl_num
    for act in activations:
        act[0] = n(act[0]) + n(act[1]) * spike
        if act[0] > 98:
            act[0] = 90
            act[1] = 1 / (1 + 1 / n(act[1]))
        act[2] = perl_index(PRECALCULATED, act[0])
    return _last_real(activations)


def weaken_several(spike, *activations):
    """Perl: WeakenSeveral($spike, @activations). Same as decaying by ``$spike ||= 1``, but
    returns the last one's real activation."""
    spike = _amount(spike)
    for act in activations:
        _lower(act, spike)
    return _last_real(activations)
