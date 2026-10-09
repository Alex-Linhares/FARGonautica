"""Tests for seqsee.spos.

Mirrors Perl: lib/SPos.pm and t/spos.t (translated verbatim below).
Golden data: oracle/spos.pl -> golden/spos.json.
"""
import pytest

import golden
from seqsee.errors import SErr, Confess
from seqsee.spos import SPos

CASES = golden.load("spos")


def cases(op):
    return [c for c in CASES if c["op"] == op]


class FakeObj:
    """Stand-in for a Seqsee object (FakeObj in oracle/spos.pl)."""

    def __init__(self, n, s):
        self.n, self.s = n, s

    def get_parts_count(self):
        return self.n

    def get_structure_string(self):
        return self.s


def arg(a):
    """Golden args are Perl strings; integers go in as ints, the rest as given."""
    if a is None:
        return None
    try:
        return int(a) if str(int(a)) == a else a
    except ValueError:
        return a


# --- t/spos.t, verbatim -------------------------------------------------------

def test_spos_t():
    p1 = SPos(position=2)
    p2 = SPos(position=2)
    p3 = SPos(position=3)

    assert p1 == p2                      # ok( $p1 eq $p2 );
    assert p1 == p2                      # ok( $p1 ~~ $p2 );

    assert p1 != p3                      # ok( $p1 ne $p3 );
    assert not (p1 == p3)                # ok( not( $p1 ~~ $p3 ) );

    with pytest.raises(Confess):         # dies_ok { SPos->new( position => 0 ) }
        SPos(position=0)

    # Current code uses ->new(3) syntax as well...
    p4 = SPos(3)
    assert p4 == p3                      # ok( $p4 ~~ $p3 );

    with pytest.raises(Confess):         # dies_ok { SPos->new(-3) }
        SPos(-3)

    # A special exception for position -1 is made since it is widely used currently:
    SPos(-1)                             # lives_ok { SPos->new(-1) }


# --- golden -------------------------------------------------------------------

def test_new_golden():
    for c in cases("new"):
        assert SPos(arg(c["arg"])).position == int(c["position"])
    for c in cases("new_named"):
        assert SPos(position=arg(c["arg"])).position == int(c["position"])


def test_new_dies_golden():
    assert cases("new_dies") and cases("new_named_dies")
    for c in cases("new_dies"):
        assert c["dies"] == 1
        with pytest.raises(Confess):
            SPos(arg(c["arg"]))
    for c in cases("new_named_dies"):
        with pytest.raises(Confess):
            SPos(position=arg(c["arg"]))
    (c,) = cases("new_no_args_dies")
    assert c["dies"] == 1
    with pytest.raises(Confess):
        SPos()


def test_new_rejects_floats_and_bools():
    for bad in (2.5, 3.0, True, [3]):
        with pytest.raises(Confess):
            SPos(bad)


def test_eq_golden():
    for c in cases("eq"):
        pa, pb = SPos(c["a"]), SPos(c["b"])
        assert int(pa == pb) == c["eq"]
        assert int(pa.__eq__(pb)) == c["smartmatch"]
        # PERL-QUIRK: `ne` is not overloaded; it compares refs.
        assert int(pa != pb) == c["ne"]


def test_eq_same_object_golden():
    (c,) = cases("eq_same_object")
    p = SPos(2)
    assert int(p == p) == c["eq"]
    assert int(p != p) == c["ne"]


def test_eq_non_spos_dies_golden():
    (c,) = cases("eq_non_spos_dies")
    assert c["dies"] == 1
    with pytest.raises(Confess):
        SPos(2) == "2"


def test_hash_is_identity():
    a, b = SPos(2), SPos(2)
    assert len({a, b}) == 2


def test_setter_golden():
    p = SPos(2)
    for c in cases("set"):
        p.position = c["value"]
        assert p.position == c["position"]
    (c,) = cases("set_dies")
    with pytest.raises(Confess):
        p.position = 2.5


def test_find_range_golden():
    cs = cases("find_range")
    assert len(cs) == 17
    for c in cs:
        p = SPos(1)
        p.position = c["position"]
        obj = FakeObj(c["size"], "[4, 5, 6]" if c["position"] in (0, -7) else "[1, 2, 3]")
        if c["error_class"]:
            with pytest.raises(SErr) as ei:
                p.find_range(obj)
            assert ei.value.message == c["error"]
        else:
            assert p.find_range(obj) == c["result"]
