"""Port of Mapping.pm: the Mapping base class and the FindMapping/ApplyMapping multimethods.

``FIND_MAPPING``/``APPLY_MAPPING`` are ``multimethods.Multimethod`` tables, and
``find_mapping(*args)``/``apply_mapping(*args)`` call them. The variants from
Mapping.pm are registered here. Each submodule registers its own variants on import
(Mapping::Dir → mapping/dir.py, Mapping::Position → mapping/position.py; items
017–019 add numeric/meto_type/structural), and the imports are at the bottom of this
file.

Note: Mapping::Dir and Mapping::Position do **not** inherit from Mapping
(oracle-confirmed), so they have no ``check_sanity`` and fail MappingBased's isa check.

Hooks, looked up at call time (tests monkeypatch them):
- ``_spike_and_choose(amount, *concepts)``: SLTM::SpikeAndChoose;
- ``_message(text)``: main::message (GUI-only in Perl), logged at debug level.
Seqsee::Element->create goes through ``categories.numeric._element_create``.
"""
import logging

from seqsee import util
from seqsee.errors import Confess
from seqsee.multimethods import Multimethod, perl_isa

_log = logging.getLogger(__name__)


def _spike_and_choose(amount, *concepts):
    """Perl: SLTM::SpikeAndChoose($amount, @concepts)."""
    from seqsee import sltm
    return sltm.spike_and_choose(amount, *concepts)


def _message(text):
    """Perl: main::message($text)."""
    _log.debug("%s", text)


class Mapping:
    """Perl: package Mapping, the base of Mapping::Numeric/Structural/MetoType."""

    perl_name = "Mapping"

    def check_sanity(self):
        """Perl: CheckSanity. 1, or None (after a message) when a structural mapping's
        changed attributes aren't enough to build its category."""
        if not perl_isa(self, "Mapping::Structural"):
            return 1
        from seqsee.mapping.numeric import _method_target
        cat = _method_target(self.get_category(), "AreAttributesSufficientToBuild")
        atts = util.perl_keys(self.get_changed_bindings())
        if not util.perl_true(cat.are_attributes_sufficient_to_build(*atts)):
            _message(f"This transform is bogus! CAT={cat.as_text()} ATTS={' '.join(atts)}")
            return None
        return 1


FIND_MAPPING = Multimethod("FindMapping")
APPLY_MAPPING = Multimethod("ApplyMapping")


def find_mapping(*args):
    """Perl: FindMapping(...) multimethod. Perl's ``return;`` gives None."""
    return FIND_MAPPING.call(*args)


def apply_mapping(*args):
    """Perl: ApplyMapping(...) multimethod. Perl's ``return;`` gives None."""
    return APPLY_MAPPING.call(*args)


@FIND_MAPPING.variant("*", "*", "*")
def _find_with_cat(a, b, cat):
    return cat.find_mapping_for_cat(a, b)


@FIND_MAPPING.variant("SInt", "SInt")
@FIND_MAPPING.variant("Seqsee::Element", "Seqsee::Element")
def _numeric_find_transform(a, b):
    from seqsee import s as S
    common = a.get_common_categories(b)
    if not common:
        raise Confess("")
    if any(c is None for c in common):
        raise Confess(
            "undef in common_categories FindMapping SInt/Seqsee::Element SInt/Seqsee::Element:"
            + ", ".join("" if c is None else util.perl_ref_string(c) for c in common))
    cat = _spike_and_choose(0, *common)
    if cat is None:
        cat = S.NUMBER
    if util.perl_true(cat.is_numeric()):
        return cat.find_mapping_for_cat(a.get_mag(), b.get_mag())
    return cat.find_mapping_for_cat(a, b)


@FIND_MAPPING.variant("#", "#")
def _find_numbers(a, b):
    from seqsee import s as S
    return S.NUMBER.find_mapping_for_cat(a, b)


@FIND_MAPPING.variant("Seqsee::Anchored", "Seqsee::Anchored")
def _find_anchored(a, b):
    common = a.get_common_categories(b)
    if not common:
        return None
    cat = _spike_and_choose(10, *common)
    if not util.perl_true(cat):
        return None
    return cat.find_mapping_for_cat(a, b)


@APPLY_MAPPING.variant("Mapping::Numeric", "#")
def _apply_numeric_number(transform, num):
    return transform.get_category().apply_mapping_for_cat(transform, num)


@APPLY_MAPPING.variant("Mapping::Numeric", "SInt")
def _apply_numeric_sint(transform, num):
    from seqsee.sint import SInt
    new_mag = transform.get_category().apply_mapping_for_cat(transform, num.get_mag())
    if new_mag is None:
        return None
    return SInt(new_mag)


@APPLY_MAPPING.variant("Mapping::Numeric", "Seqsee::Element")
def _apply_numeric_element(transform, num):
    from seqsee.categories import numeric
    new_mag = transform.get_category().apply_mapping_for_cat(transform, num.get_mag())
    if new_mag is None:
        return None
    return numeric._element_create(new_mag, -1)


@APPLY_MAPPING.variant("Mapping::Structural", "Seqsee::Object")
def _apply_structural(transform, obj):
    return transform.get_category().apply_mapping_for_cat(transform, obj)


@FIND_MAPPING.variant("SInt", "Seqsee::Element")
@FIND_MAPPING.variant("Seqsee::Element", "SInt")
@FIND_MAPPING.variant("Seqsee::Anchored", "SInt")
@FIND_MAPPING.variant("SInt", "Seqsee::Anchored")
@APPLY_MAPPING.variant("Mapping::Numeric", "Seqsee::Anchored")
def _fail(*_args):
    return None


# Submodules register their variants (Perl: "More FindMapping in Mapping::Dir").
from seqsee.mapping import dir as _dir  # noqa: E402,F401
from seqsee.mapping import position as _position  # noqa: E402,F401
from seqsee.mapping import numeric as _numeric  # noqa: E402,F401
from seqsee.mapping import meto_type as _meto_type  # noqa: E402,F401
from seqsee.mapping import structural as _structural  # noqa: E402,F401
