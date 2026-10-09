"""Tests for seqsee.constants, mirroring the constant packages of lib/S.pm
(DIR, POS_MODE, METO_MODE, EXTENDIBILE, RELN_SCHEME, DISTANCE_MODE, DISTANCE).

Golden data: oracle/constants.pl -> golden/constants.json.
"""
import pytest

import golden
from seqsee import util
from seqsee.errors import Confess
from seqsee.constants import (
    DIR,
    DISTANCE,
    DISTANCE_MODE,
    EXTENDIBILE,
    METO_MODE,
    POS_MODE,
    RELN_SCHEME,
)

CASES = golden.load("constants")


def cases(pkg, **match):
    out = [c for c in CASES if c["pkg"] == pkg and all(c.get(k) == v for k, v in match.items())]
    assert out, (pkg, match)
    return out


# --- DIR -----------------------------------------------------------------

@pytest.mark.parametrize("case", cases("DIR", op=None), ids=lambda c: c["name"])
def test_dir_golden(case):
    d = getattr(DIR, case["name"])
    assert d.as_text() == case["as_text"]
    assert int(d.potentially_extendible()) == case["potentially_extendible"]
    assert int(d.is_left_or_right()) == case["is_left_or_right"]
    if case["flip_dies"]:
        with pytest.raises(Confess, match="weird direction"):
            d.flip()
    else:
        assert d.flip().as_text() == case["flip"]


@pytest.mark.parametrize("case", cases("DIR", op="eq"), ids=lambda c: f"{c['a']}-{c['b']}")
def test_dir_identity_equality(case):
    a, b = getattr(DIR, case["a"]), getattr(DIR, case["b"])
    assert int(a == b) == case["eq"] == case["smartmatch"]


def test_dir_flip_is_involution_and_hashable():
    assert DIR.LEFT.flip() is DIR.RIGHT
    assert DIR.LEFT.flip().flip() is DIR.LEFT
    assert len({DIR.LEFT, DIR.RIGHT, DIR.UNKNOWN, DIR.NEITHER, DIR.LEFT}) == 4


# --- POS_MODE --------------------------------------------------------------

@pytest.mark.parametrize("case", cases("POS_MODE", op=None), ids=lambda c: c["name"])
def test_pos_mode_golden(case):
    m = getattr(POS_MODE, case["name"])
    assert m.as_text() == case["as_text"]
    assert m.serialize() == case["serialize"]
    assert (POS_MODE.deserialize(m.serialize()) is m) == bool(case["roundtrip_same"])
    assert m.get_memory_dependencies() == case["memory_dependencies"]


@pytest.mark.parametrize("case", cases("POS_MODE", op="deserialize"), ids=lambda c: c["arg"])
def test_pos_mode_deserialize_unknown(case):
    assert (POS_MODE.deserialize(case["arg"]) is not None) == bool(case["defined"])


# --- METO_MODE -------------------------------------------------------------

@pytest.mark.parametrize("case", cases("METO_MODE", op=None), ids=lambda c: c["name"])
def test_meto_mode_golden(case):
    m = getattr(METO_MODE, case["name"])
    assert m.as_text() == case["as_text"]
    assert m.serialize() == case["serialize"]
    assert (METO_MODE.deserialize(m.serialize()) is m) == bool(case["roundtrip_same"])
    assert m.is_position_relevant() == case["is_position_relevant"]
    assert m.is_metonymy_present() == case["is_metonymy_present"]
    assert (m.get_pure() is m) == bool(case["get_pure_same"])
    assert m.get_memory_dependencies() == case["memory_dependencies"]


@pytest.mark.parametrize("case", cases("METO_MODE", op="deserialize"), ids=lambda c: c["arg"])
def test_meto_mode_deserialize_unknown_dies(case):
    assert case["dies"] == 1
    with pytest.raises(Confess, match="Unknown!"):
        METO_MODE.deserialize(case["arg"])


# --- EXTENDIBILE, RELN_SCHEME ---------------------------------------------

@pytest.mark.parametrize("case", cases("EXTENDIBILE"), ids=lambda c: c["name"])
def test_extendibile_golden(case):
    e = getattr(EXTENDIBILE, case["name"])
    assert e.mode == case["mode"]
    assert int(bool(e)) == case["bool"]


def test_reln_scheme_golden():
    (none,) = cases("RELN_SCHEME", name="NONE")
    (chain,) = cases("RELN_SCHEME", name="CHAIN")
    assert RELN_SCHEME.NONE == none["value"]
    assert int(bool(RELN_SCHEME.NONE)) == none["bool"]
    assert RELN_SCHEME.CHAIN.type == chain["type"]
    assert int(bool(RELN_SCHEME.CHAIN)) == chain["bool"]
    assert int(RELN_SCHEME.CHAIN == RELN_SCHEME.NONE) == chain["eq_none"]
    assert int(RELN_SCHEME.CHAIN == RELN_SCHEME.CHAIN) == chain["eq_chain"]


# --- DISTANCE_MODE, DISTANCE ----------------------------------------------

@pytest.mark.parametrize("case", cases("DISTANCE_MODE", op=None), ids=lambda c: c["name"])
def test_distance_mode_golden(case):
    m = getattr(DISTANCE_MODE, case["name"])
    assert m.mode == case["mode"]
    assert m.is_unit_groups() == case["is_unit_groups"]


def test_distance_mode_pick_one_seeded():
    (case,) = cases("DISTANCE_MODE", op="PickOne")
    util.srand(case["seed"])
    picks = [DISTANCE_MODE.pick_one().mode for _ in case["picks"]]
    assert picks == case["picks"]


@pytest.mark.parametrize("case", cases("DISTANCE"), ids=lambda c: f"{c['ctor']}({c['arg']})")
def test_distance_golden(case):
    ctor = {"InElements": DISTANCE.in_elements, "InGroups": DISTANCE.in_groups,
            "Zero": DISTANCE.zero}[case["ctor"]]
    d = ctor() if case["arg"] is None else ctor(case["arg"])
    assert d.get_magnitude() == case["magnitude"]
    assert d.is_non_zero() == case["is_non_zero"]
    assert d.is_unit_groups() == case["is_unit_groups"]
    assert d.as_text() == case["as_text"]
