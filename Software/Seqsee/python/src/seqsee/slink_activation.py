"""Port of SLinkActivation.pm: the activation of an LTM link, plus the lookup table.

An activation is a Perl blessed array, so ``SLinkActivation`` subclasses ``list`` and SLTM
can index it the way the Perl does (``act[RAW_SIGNIFICANCE]``)::

    [raw activation, raw significance, stability reciprocal, real activation, modifier index]

The real activation is ``PRECALCULATED[raw activation + raw significance]``. Indexing follows
Perl arrays (see ``perl_index``): fractions truncate, negative indices count from the end,
and anything out of range gives undef (None).

Perl builds Decay/DecayMany/Spike with string ``eval``; here they are plain functions.
"""
import math

from seqsee import util
from seqsee.errors import Confess

RAW_ACTIVATION = 0        # Index.
RAW_SIGNIFICANCE = 1      # Index.
STABILITY_RECIPROCAL = 2  # Index.
REAL_ACTIVATION = 3       # Index.
MODIFIER_NODE_INDEX = 4   # Index. *only* used for links.

PRECALCULATED = [0.4815 + 0.342 * math.atan2(12 * (i / 100 - 0.5), 1) for i in range(201)]

Initial_Raw_Activation = 5
Initial_Raw_Significance = 1
Initial_Stability = 50
Initial_Stability_Reciprocal = 1 / Initial_Stability
_Initial_Real_Activation = PRECALCULATED[Initial_Raw_Activation + Initial_Raw_Significance]


def perl_index(array, index):
    """Perl ``$array[$index]``: numify and truncate the index; negative indices count from
    the end; out of range is undef (None)."""
    i = util.perl_num(index)
    if isinstance(i, float):
        if math.isnan(i) or math.isinf(i):
            return None
        i = int(i)
    if -len(array) <= i < len(array):
        return array[i]
    return None


class SLinkActivation(list):
    """Perl: SLinkActivation (blessed array)."""

    perl_name = "SLinkActivation"
    RAW_ACTIVATION = RAW_ACTIVATION
    RAW_SIGNIFICANCE = RAW_SIGNIFICANCE
    STABILITY_RECIPROCAL = STABILITY_RECIPROCAL
    REAL_ACTIVATION = REAL_ACTIVATION
    MODIFIER_NODE_INDEX = MODIFIER_NODE_INDEX

    def __init__(self, modifier_index=None):
        """Perl: new($modifier_index)."""
        super().__init__([Initial_Raw_Activation, Initial_Raw_Significance,
                          Initial_Stability_Reciprocal, _Initial_Real_Activation, modifier_index])

    def get_raw_activation(self):
        """Perl: GetRawActivation."""
        return self[RAW_ACTIVATION]

    def get_raw_significance(self):
        """Perl: GetRawSignificance."""
        return self[RAW_SIGNIFICANCE]

    def get_stability_reciprocal(self):
        """Perl: GetStabilityReciprocal."""
        return self[STABILITY_RECIPROCAL]

    def amount_to_spread(self, original_amount):
        """Perl: AmountToSpread. A true modifier index multiplies by 3 × the real activation
        of ``$SLTM::ACTIVATIONS[$modifier]`` (``sltm.ACTIVATIONS``, looked up at call time)."""
        n = util.perl_num
        amt1 = (n(original_amount) * n(self[RAW_SIGNIFICANCE])
                / n(self[STABILITY_RECIPROCAL]))
        modifier = self[MODIFIER_NODE_INDEX]
        if util.perl_true(modifier):
            from seqsee import sltm
            node = perl_index(getattr(sltm, "ACTIVATIONS", []), modifier)
            modifier_activation = None if node is None else perl_index(node, 2)
            if modifier_activation is None:
                node_str = "" if node is None else f"SNodeActivation=ARRAY(0x{id(node):x})"
                raise Confess(f"AmountToSpread: <SLinkActivation=ARRAY(0x{id(self):x})>, "
                              f"<{util.perl_str(modifier)}> <{node_str}> <>")
            amt1 *= modifier_activation * 3
        amt1 /= 100
        amt1 += 1
        return amt1


def _decay_one(act):
    """Perl: $DECAY_CODE. Returns the new real activation."""
    n = util.perl_num
    if n(act[0]) > 1:
        act[0] = n(act[0]) - 1
    if n(act[0]) <= 1:
        act[0] = 5
        if n(act[1]) > 1:
            act[1] = n(act[1]) - n(act[2])
    act[3] = perl_index(PRECALCULATED, n(act[0]) + n(act[1]))
    return act[3]


def decay(act):
    """Perl: Decay. Returns the new real activation (the value of the last statement)."""
    return _decay_one(act)


def decay_many(*args):
    """Perl: DecayMany($arr_ref, $cnt): decay ``arr_ref[1 .. cnt]`` (1-based).

    Undef or missing slots are skipped (Perl autovivifies a temporary copy, so the array is
    untouched)."""
    if len(args) != 2:
        raise Confess("DecayMany needs 2 args")
    arr_ref, cnt = args
    for i in util.perl_range(1, cnt):
        act = arr_ref[i] if i < len(arr_ref) else None
        if act is not None:
            _decay_one(act)


def spike(act, amount=None):
    """Perl: Spike($act, $spike). ``$spike ||= 1``. Returns the new real activation."""
    n = util.perl_num
    amount = n(amount) if util.perl_true(amount) else 1
    act[0] = n(act[0]) + amount
    if act[0] > 99:
        act[1] = n(act[1]) + 2
        act[0] = 90
        if act[1] > 99:
            act[1] = 90
            stab = 1 / n(act[2])
            act[2] = 1 / (stab + 3)
    act[3] = perl_index(PRECALCULATED, act[0] + n(act[1]))
    return act[3]
