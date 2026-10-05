"""Item 01: the Scheme -> Python name mapping (docs/python-translation-plan.md, "Names").

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The mapping is a decision, not a Chez behaviour, so these expected values come from the
plan.  What must hold for the original's ~1,300 names is checked on all of them: every
name maps to a valid, non-reserved Python identifier, and no two names collide.
"""
from __future__ import annotations

import keyword

import pytest

from name_mapping import EXCEPTIONS, RESERVED, original_names, scheme_to_python


@pytest.mark.parametrize("scheme, python", [
    ("foo-bar?", "foo_bar_p"),               # TASK.md's example
    ("make-bond", "make_bond"),
    ("*temperature*", "g_temperature"),
    ("*temperature-clamped?*", "g_temperature_clamped_p"),
    ("*EEG*", "g_EEG"),                      # case is kept
    ("%verbose%", "p_verbose"),
    ("%max-coderack-size%", "p_max_coderack_size"),
    ("=white=", "c_white"),
    ("vector-increment!", "vector_increment_bang"),
    ("bridge-type->theme-type", "bridge_type_to_theme_type"),
    ("CMs-equal?", "CMs_equal_p"),
    ("group-scout:whole-string", "group_scout__whole_string"),
    ("top-down-bond-scout:category", "top_down_bond_scout__category"),
    ("ObjCtgy/Length-change?", "ObjCtgy_or_Length_change_p"),
    ("fig5.10", "fig5_10"),
    ("plato-letter-category", "plato_letter_category"),
    ("stochastic-if*", "stochastic_if_star"),
    ("1st", "first"),
    ("100-", "hundred_minus"),
    ("^2", "square"),
    ("~", "rough"),
    ("break", "break_"),                     # a Python keyword (run.ss's break)
    ("print", "print_"),                     # a builtin (utilities.ss's print)
    ("round", "round_"),                     # utilities.ss's exact round
    ("filter", "filter_"),
])
def test_examples(scheme, python):
    assert scheme_to_python(scheme) == python


def test_the_scan_finds_the_original_names():
    names = original_names()
    assert len(names) > 1200
    for name in ("make-bond", "*temperature*", "%verbose%", "=white=", "for*",
                 "stochastic-if*", "group-scout:whole-string", "plato-a", "plato-letter-category",
                 "1st", "~", "tell", "delegate", "base-object"):
        assert name in names, name


def test_every_name_maps_to_a_valid_identifier():
    bad = {n: scheme_to_python(n) for n in original_names()
           if not scheme_to_python(n).isidentifier()
           or keyword.iskeyword(scheme_to_python(n))
           or scheme_to_python(n) in RESERVED}
    assert bad == {}


def test_the_mapping_is_injective():
    seen: dict[str, str] = {}
    collisions = []
    for name in original_names():
        py = scheme_to_python(name)
        if py in seen:
            collisions.append((seen[py], name, py))
        seen[py] = name
    assert collisions == []


def test_exceptions_are_used_and_distinct():
    names = original_names()
    assert set(EXCEPTIONS) <= set(names)
    assert len(set(EXCEPTIONS.values())) == len(EXCEPTIONS)
    rule_made = {scheme_to_python(n) for n in names if n not in EXCEPTIONS}
    assert not rule_made & set(EXCEPTIONS.values())
