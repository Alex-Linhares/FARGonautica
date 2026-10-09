"""Tests for the simple numeric categories.

Mirrors lib/SCategory/Number.pm, lib/SCategory/Prime.pm, lib/SCategory/Odd.pm,
lib/SCategory/Even.pm and lib/SCategory/Numeric.pm (plus the bits of
LTMStorable/Independent.pm and SCategory/MetonymySpec/NotMetonyable.pm they use).
Golden data: oracle/numeric_categories.pl.
"""
import pytest

import golden
from seqsee import s as S
from seqsee.categorizable import Categorizable
from seqsee.categories import numeric, prime
from seqsee.categories.base import SCategory
from seqsee.categories.even import Even
from seqsee.categories.number import Number
from seqsee.categories.odd import Odd
from seqsee.categories.prime import Prime
from seqsee.errors import Confess
from seqsee.sbindings import SBindings
from seqsee.sint import SInt
from seqsee.util import perl_str

CASES = golden.load("numeric_categories")


def cases(kind):
    return [c for c in CASES if c["kind"] == kind]


def cat_of(key):
    return {"number": S.NUMBER, "prime": S.PRIME, "odd": S.ODD, "even": S.EVEN}[key]


def show(v):
    """The oracle's show(): defined values stringified, undef as None."""
    return None if v is None else perl_str(v)


class FakeTransform:
    def __init__(self, name):
        self._name = name

    def get_name(self):
        return self._name


class FakeMagObj:
    def __init__(self, mag):
        self._mag = mag

    def get_mag(self):
        return self._mag


class FakeElement(Categorizable):
    """Stands in for Seqsee::Element->create($mag, $pos) (item 023)."""

    def __init__(self, mag, pos):
        self.mag = mag
        self.pos = pos
        self.history = []

    def get_mag(self):
        return self.mag

    def add_history(self, msg):
        self.history.append(msg)


@pytest.fixture
def mapping_recorder(monkeypatch):
    """Mapping::Numeric->create replaced as in the oracle: "name/catname"."""
    monkeypatch.setattr(numeric, "_mapping_numeric_create",
                        lambda name, cat: f"{name}/{cat.get_name()}")


@pytest.fixture
def fake_elements(monkeypatch):
    created = []

    def create(mag, pos):
        el = FakeElement(mag, pos)
        created.append(el)
        return el

    monkeypatch.setattr(numeric, "_element_create", create)
    return created


# --- golden ----------------------------------------------------------------------

def test_golden_covers_every_kind():
    kinds = {c["kind"] for c in CASES}
    assert kinds == {"basics", "sufficient", "is_prime", "next_prime", "previous_prime",
                     "instancer", "find_mapping", "apply_mapping", "build", "build_dies"}
    assert len(cases("basics")) == 4


@pytest.mark.parametrize("case", cases("basics"), ids=lambda c: c["cat"])
def test_basics_golden(case):
    cat = cat_of(case["cat"])
    assert isinstance(cat, SCategory)
    assert cat.perl_name == case["class"]
    assert cat.get_name() == case["get_name"]
    assert cat.as_text() == case["as_text"]
    assert cat.serialize() == case["serialize"]
    assert cat.string_to_recreate() == case["string_to_recreate"]
    copy = type(cat).deserialize(cat.serialize())
    assert copy.perl_name == case["deserialized_class"]
    assert (copy is cat) == bool(case["deserialized_is_same"])
    assert cat.is_pure() == case["is_pure"]
    assert (cat.get_pure() is cat) == bool(case["get_pure_is_self"])
    assert len(cat.get_meto_types()) == case["meto_types_count"]
    assert len(cat.get_memory_dependencies()) == case["memory_deps_count"]
    assert cat.is_metonyable() == case["is_metonyable"]
    assert int(cat.is_numeric()) == case["is_numeric"] == case["does_numeric"]
    assert isinstance(cat, numeric.Numeric)
    assert (cat == cat) == bool(case["smartmatch_self"])


@pytest.mark.parametrize("case", cases("sufficient"), ids=lambda c: f"{c['cat']}-{c['atts']}")
def test_are_attributes_sufficient_to_build_golden(case):
    # PERL-QUIRK: `my ($self, @atts) = 1;` throws the arguments away, so always 0.
    assert cat_of(case["cat"]).are_attributes_sufficient_to_build(*case["atts"]) == case["result"]


@pytest.mark.parametrize("case", cases("is_prime"), ids=lambda c: repr(c["input"]))
def test_is_prime_golden(case):
    assert prime.is_prime(case["input"]) == case["result"]


def test_is_prime_float_two_is_prime():
    # The oracle's 2.0 literal arrives as JSON 2; check the float form explicitly.
    assert prime.is_prime(2.0) == 1
    assert prime.is_prime("2.0") == 0


@pytest.mark.parametrize("case", cases("next_prime"), ids=lambda c: repr(c["input"]))
def test_next_prime_golden(case):
    assert show(prime.next_prime(case["input"])) == case["result"]


@pytest.mark.parametrize("case", cases("previous_prime"), ids=lambda c: repr(c["input"]))
def test_previous_prime_golden(case):
    assert show(prime.previous_prime(case["input"])) == case["result"]


@pytest.mark.parametrize("bad", [2.5, -1.5, "abc", "a5", "2.5"])
def test_next_prime_refuses_inputs_perl_loops_on(bad):
    # PERL-QUIRK: Perl loops forever on these (non-integers never hit %Primes, and
    # "abc"++ is a magic string increment). The port raises instead of hanging.
    with pytest.raises(Confess):
        prime.next_prime(bad)


@pytest.mark.parametrize("bad", [2.5, 10.5, "2.5"])
def test_previous_prime_refuses_inputs_perl_loops_on(bad):
    with pytest.raises(Confess):
        prime.previous_prime(bad)


def test_prime_helpers_early_returns_come_first():
    # These return before Perl would start looping.
    assert prime.next_prime(100.5) is None
    assert prime.previous_prime(1.5) is None
    assert prime.previous_prime("abc") is None
    assert prime.next_prime("5abc") == 7   # not a magic-increment string: numified


def test_previous_prime_of_huge_number_is_fast():
    assert prime.previous_prime(10 ** 18) == 97


@pytest.mark.parametrize("case", cases("instancer"), ids=lambda c: f"{c['cat']}-{c['mag']!r}")
def test_instancer_golden(case):
    cat = cat_of(case["cat"])
    b = cat.numeric_instancer(case["mag"])
    assert (b is not None) == bool(case["defined"])
    if b is not None:
        assert case["ref"] == "SBindings"
        assert isinstance(b, SBindings)
        assert b.get_bindings_ref() == case["bindings"]
        assert b.get_squinting_raw() == case["slippages"]
    b2 = cat.instancer(FakeMagObj(case["mag"]))
    assert (b2 is not None) == bool(case["via_object_defined"])


def test_instancer_returns_fresh_bindings():
    assert S.NUMBER.numeric_instancer(3) is not S.NUMBER.numeric_instancer(3)


@pytest.mark.parametrize("case", cases("find_mapping"),
                         ids=lambda c: f"{c['cat']}-{c['a']!r}-{c['b']!r}")
def test_find_mapping_for_cat_golden(case, mapping_recorder):
    result = cat_of(case["cat"]).find_mapping_for_cat(case["a"], case["b"])
    assert result == case["result"]


def test_prime_find_mapping_undef_quirk(mapping_recorder):
    # PERL-QUIRK: NextPrime(97)/PreviousPrime(2) are undef, and undef == 0.
    assert S.PRIME.find_mapping_for_cat(97, 0) == "succ/Prime"
    assert S.PRIME.find_mapping_for_cat(2, 0) == "pred/Prime"


@pytest.mark.parametrize("case", cases("apply_mapping"),
                         ids=lambda c: f"{c['cat']}-{c['name']}-{c['mag']!r}")
def test_apply_mapping_for_cat_golden(case):
    result = cat_of(case["cat"]).apply_mapping_for_cat(FakeTransform(case["name"]), case["mag"])
    assert show(result) == case["result"]


def test_apply_mapping_same_returns_object_unchanged():
    obj = "6"
    assert S.NUMBER.apply_mapping_for_cat(FakeTransform("same"), obj) is obj
    assert S.NUMBER.apply_mapping_for_cat(FakeTransform("succ"), "6") == 7


@pytest.mark.parametrize("case", cases("build"), ids=lambda c: f"{c['cat']}-{sorted(c['args'])}")
def test_build_golden(case, fake_elements):
    cat = cat_of(case["cat"])
    args = dict(case["args"])
    ret = cat.build(args)
    assert ret is fake_elements[-1]
    assert ret.pos == -1
    assert ret.get_mag() == case["mag"]
    assert bool(ret.is_of_category_p(cat)) == bool(case["has_cat"])
    b = ret.get_binding_for_category(cat)
    assert b.get_bindings_ref() == case["bindings"]
    assert (b.get_bindings_ref() is args) == bool(case["bindings_shared"])
    assert b.get_squinting_raw() == case["slippages"]


@pytest.mark.parametrize("case", cases("build_dies"), ids=lambda c: f"{c['cat']}-{sorted(c['args'])}")
def test_build_dies_golden(case, fake_elements):
    assert case["dies"] == 1
    cat = cat_of(case["cat"])
    if "mag" in case["args"]:
        # The undef mag is rejected by Seqsee::Element->create (item 023), not the role:
        # the role passes it straight through.
        cat.build(dict(case["args"]))
        assert fake_elements[-1].get_mag() is None
        return
    with pytest.raises(Confess, match="Need mag"):
        cat.build(dict(case["args"]))
    assert fake_elements == []


def test_element_create_hook_is_wired():
    from seqsee.objects.element import Element
    e = numeric._element_create(1, -1)
    assert isinstance(e, Element) and e.get_mag() == 1 and e.get_edges() == (-1, -1)


def test_mapping_numeric_create_hook_is_wired():
    from seqsee.mapping.numeric import MappingNumeric
    m = numeric._mapping_numeric_create("succ", S.NUMBER)
    assert isinstance(m, MappingNumeric)
    assert (m.get_name(), m.get_category()) == ("succ", S.NUMBER)


# --- wiring ----------------------------------------------------------------------

def test_s_singletons_are_the_real_categories():
    assert type(S.NUMBER) is Number
    assert type(S.PRIME) is Prime
    assert type(S.ODD) is Odd
    assert type(S.EVEN) is Even


def test_deserialize_creates_new_registered_instances():
    from seqsee import categorizable
    copy = Number.deserialize("SCategory::Number->new()")
    assert type(copy) is Number and copy is not S.NUMBER
    assert categorizable._registered(copy) is copy
    # Perl eval of a bad string returns undef.
    assert Number.deserialize("Nonsense->new()") is None


def test_odd_deserializes_as_even():
    # PERL-QUIRK: Odd's string_to_recreate names SCategory::Even.
    assert type(Odd.deserialize(S.ODD.serialize())) is Even


def test_is_instance_adds_category():
    el = FakeElement(7, -1)
    b = S.PRIME.is_instance(el)
    assert isinstance(b, SBindings)
    assert el.is_of_category_p(S.PRIME)
    assert S.EVEN.is_instance(el) is None


def test_sint_uses_prime_is_prime(monkeypatch):
    from seqsee import global_ as Global
    Global.Feature["Primes"] = 1
    calls = []
    real = prime.is_prime
    monkeypatch.setattr(prime, "is_prime", lambda n: calls.append(n) or real(n))
    assert S.PRIME in SInt(7).get_categories()
    assert calls == [7]
