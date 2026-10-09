"""Tests for the Alternating category.

Mirrors lib/SCategory/Alternating.pm. Golden data: oracle/alternating.pl.

As in the oracle, Seqsee::Object->create, Mapping::Numeric->create,
Mapping::Structural->create, FindMapping and main::message are replaced by
recorders. Pure objects are FakePlatonics (stand-ins for SLTM::Platonic, which
item 028 ported), memoized by their string like the real ones.

Perl's Create sorts the two pure objects by their stringified address, so which
one becomes object1 is arbitrary. The golden categories are rebuilt here with
``Alternating(object1=..., object2=...)`` in the order Perl got (the ``deps`` of
the basics cases), which makes every which/flip result comparable.
"""
import re

import pytest

import golden
from seqsee import categorizable
from seqsee import s as S
from seqsee import sint as sint_module
from seqsee.categories import alternating, base, numeric
from seqsee.categories.alternating import Alternating
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.constants import METO_MODE
from seqsee.errors import Confess
from seqsee.sbindings import SBindings
from seqsee.sint import SInt
from seqsee.util import perl_ref_string, perl_str

CASES = golden.load("alternating")


def cases(kind):
    return [c for c in CASES if c["kind"] == kind]


def case(kind, label):
    (c,) = [c for c in CASES if c["kind"] == kind and c.get("label") == label]
    return c


def show(v):
    return None if v is None else perl_str(v)


# --- fakes (same names as in the oracle) ---------------------------------------------

def _parse_structure(string):
    """SLTM::Platonic::structure_from_string, for the simple forms used here."""
    string = re.sub(r"\s+", "", string)
    if not string.startswith("["):
        return string
    stack = [[]]
    for token in re.split(r"([\[\]])", string):
        if token == "[":
            stack.append([])
        elif token == "]":
            top = stack.pop()
            stack[-1].append(top)
        else:
            stack[-1].extend(t for t in token.split(",") if re.search(r"\d", t))
    return stack[0][0]


class FakePlatonic:
    _memo = {}

    def __init__(self, string):
        self.string = string
        self.structure = _parse_structure(string)

    @classmethod
    def create(cls, string):
        key = perl_str(string)
        if key not in cls._memo:
            cls._memo[key] = cls(key)
        return cls._memo[key]

    def get_structure(self):
        return self.structure

    def get_pure(self):
        return self

    def as_text(self):
        return "plat" + self.string


P = FakePlatonic.create


class FakeBuilt:
    def __init__(self, *items):
        self.items = list(items)
        self.described = []

    def describe_as(self, cat):
        self.described.append(cat)
        return "BINDINGS"


class FakeMapping:
    def __init__(self, name, cat):
        self.name = name
        self.cat = cat


class FakeTransform:
    def __init__(self, name):
        self.name = name

    def get_name(self):
        return self.name

    def as_text(self):
        return "T<" + ("undef" if self.name is None else self.name) + ">"


class FakeBindings:
    def __init__(self, h):
        self.h = h

    def get_bindings_ref(self):
        return self.h


class FakeCat:
    def __init__(self, name, numeric=0, sufficient=1):
        self.name = name
        self.numeric = numeric
        self.sufficient = sufficient
        self.asked = []

    def is_numeric(self):
        return self.numeric

    def are_attributes_sufficient_to_build(self, *atts):
        self.asked.append(sorted(atts))
        return self.sufficient

    def get_name(self):
        return self.name


class FakeObj:
    def __init__(self, pure, text, cats=(), b=None):
        self.pure = pure
        self.text = text
        self.cats = list(cats)
        self.b = b or {}
        self.described = []

    def get_pure(self):
        return self.pure

    def as_text(self):
        return self.text

    def describe_as(self, cat):
        self.described.append(cat)
        return 1

    def is_of_category_p(self, cat):
        h = self.b.get(cat)
        return None if h is None else FakeBindings(h)

    def get_common_categories(self, *others):
        return [c for c in self.cats if all(any(x is c for x in o.cats) for o in others)]


class FakeVal:
    def __init__(self, n):
        self.n = n
        self.described = []

    def as_text(self):
        return f"V{self.n}"

    def get_pure(self):
        return P(str(self.n))

    def describe_as(self, cat):
        self.described.append(cat)
        return 1


def fake_find_mapping(a, b):
    if not (isinstance(a, FakeVal) and isinstance(b, FakeVal)):
        return None
    d = b.n - a.n
    return {0: "same", 1: "succ", -1: "pred"}.get(d)


MESSAGES = []
DIR_SAME = object()


@pytest.fixture(autouse=True)
def recorders(monkeypatch):
    MESSAGES.clear()
    monkeypatch.setattr(base, "_object_create", lambda *items: FakeBuilt(*items))
    monkeypatch.setattr(numeric, "_mapping_numeric_create", lambda name, cat: FakeMapping(name, cat))
    monkeypatch.setattr(base, "_structural_create", lambda opts: {"STRUCTURAL": 1, **opts})
    monkeypatch.setattr(base, "_mapping_dir_same", lambda: DIR_SAME)
    monkeypatch.setattr(base, "_find_mapping", fake_find_mapping)
    monkeypatch.setattr(alternating, "_platonic_create", FakePlatonic.create)
    monkeypatch.setattr(alternating, "_message", lambda text, level: MESSAGES.append(f"{text}|{level}"))
    monkeypatch.setattr(sint_module.SInt, "get_pure", lambda self: P(perl_str(self.get_mag())))


def alt_desc(c):
    return "Alt(" + "|".join(sorted(o.as_text() for o in (c.object1(), c.object2()))) + ")"


def desc(v):
    if v is None:
        return None
    if isinstance(v, (str, int, float)):
        return perl_str(v)
    if isinstance(v, SInt):
        return v.as_text()
    if isinstance(v, list):
        return "[" + ",".join(desc(x) or "undef" for x in v) + "]"
    if isinstance(v, FakeBuilt):
        return "Built(" + ",".join(desc(x) or "undef" for x in v.items) + ")"
    if isinstance(v, (FakePlatonic, FakeObj, FakeVal)):
        return v.as_text()
    if isinstance(v, FakeMapping):
        return f"Num({v.name},{alt_desc(v.cat)})"
    if isinstance(v, Alternating):
        return alt_desc(v)
    if isinstance(v, FakeCat):
        return f"Cat({v.name})"
    if isinstance(v, dict) and v.get("STRUCTURAL"):
        cb = v["changed_bindings"]
        return (f"Structural(cat={desc(v['category'])},meto={v['meto_mode'].as_text()},"
                f"dir_same={1 if v['direction_reln'] is DIR_SAME else 0},"
                f"slippages={len(v['slippages'])},"
                "changed={" + ",".join(f"{k}:{desc(cb[k])}" for k in sorted(cb)) + "})")
    return f"OBJ:{type(v).__name__}"


def norm(name, *objs):
    """Replace the ref strings of ``objs`` in ``name`` by their as_text (oracle: norm)."""
    for o in objs:
        name = name.replace(perl_ref_string(o), o.as_text())
    return name


def O(p):
    return FakeObj(P(p), f"O{p}")


CAT_OBJECTS = {"alt": ("3", "5"), "alt12": ("[1,2]", "3"), "alt33": ("3", "3")}


def golden_cat(key):
    """The category in Perl's object order (basics deps), made with new()."""
    (basics,) = [c for c in cases("basics") if c["cat"] == key]
    o1, o2 = (P(t[len("plat"):]) for t in basics["deps"])
    return Alternating(object1=o1, object2=o2)


# --- golden tests ----------------------------------------------------------------------

def test_golden_covers_every_kind():
    kinds = {c["kind"] for c in CASES}
    assert kinds == {"basics", "create", "create_dies", "memoize", "sufficient", "instancer",
                     "find_mapping", "apply", "build", "flipping", "check"}


@pytest.mark.parametrize("case", cases("basics"), ids=lambda c: c["cat"])
def test_basics_golden(case):
    a, b = CAT_OBJECTS[case["cat"]]
    cat = Alternating.create(O(a), O(b))
    o1, o2 = cat.object1(), cat.object2()
    assert cat.perl_name == case["class"]
    assert sorted(o.as_text() for o in (o1, o2)) == case["objects"]
    assert sorted(cat.get_memory_dependencies(), key=id) == sorted([o1, o2], key=id)
    assert [x.as_text() for x in cat.get_memory_dependencies()] == [o1.as_text(), o2.as_text()]
    assert case["deps_are_objects"] == 1
    assert sorted(case["deps"]) == sorted(x.as_text() for x in (o1, o2))
    assert case["name_is_o1_or_o2"] == 1
    assert cat.get_name() == f"{perl_ref_string(o1)} or {perl_ref_string(o2)}"
    assert norm(cat.get_name(), o1, o2) in (case["name"], " or ".join(reversed(case["name"].split(" or "))))
    assert case["as_text_is_name"] == 1 and cat.as_text() == cat.get_name()
    # Perl sorts by the stringified refs; the port sorts by util.perl_ref_string.
    assert case["sorted_by_string"] == 1
    assert perl_ref_string(o1) <= perl_ref_string(o2)
    assert cat.is_pure() == case["is_pure"]
    assert (cat.get_pure() is cat) == bool(case["get_pure_is_self"])
    assert cat.get_meto_types() == case["meto_types"]
    assert show(cat.is_metonyable()) == case["is_metonyable"]
    assert int(bool(cat.is_numeric())) == case["is_numeric"]


def test_create_memo_golden():
    g = case("create", "memo")
    alt = Alternating.create(O("3"), O("5"))
    assert (Alternating.create(O("5"), O("3")) is alt) == bool(g["reversed_same"])
    assert (Alternating.create(P("3"), P("5")) is alt) == bool(g["platonics_same"])
    assert (Alternating.create(FakeObj(P("5"), "x"), P("3")) is alt) == bool(g["other_objs_same"])
    assert (Alternating(object1=P("3"), object2=P("5")) is alt) == bool(g["new_is_not_memo"])
    assert (Alternating.create(P("3"), P("7")) is alt) == bool(g["different"])
    alt33 = Alternating.create(O("3"), O("3"))
    assert [o.as_text() for o in (alt33.object1(), alt33.object2())] == g["self_alt_objects"]
    assert (alt33.object1() is alt33.object2()) == bool(g["self_alt_same_obj"])
    assert norm(alt33.get_name(), P("3")) == g["self_alt_name"]


@pytest.mark.parametrize("label", ["missing object1", "missing object2", "empty"])
def test_create_dies_golden(label):
    g = case("create_dies", label)
    kwargs = {"missing object1": {"object2": P("3")}, "missing object2": {"object1": P("3")},
              "empty": {}}[label]
    assert g["died"] == 1
    with pytest.raises(Confess) as e:
        Alternating(**kwargs)
    assert str(e.value).startswith(g["error"].split(" at constructor")[0])


def test_memoize_golden():
    # PERL-QUIRK: get_name/as_text are memoized per object (scalar-context cache).
    (g,) = cases("memoize")
    p3, p5, p7 = P("3"), P("5"), P("7")
    x = Alternating(object1=p3, object2=p5)
    assert norm(x.get_name(), p3, p5) == g["name_before"]
    x.set_object1(p7)
    assert norm(x.get_name(), p3, p5, p7) == g["name_after"]
    assert norm(x.as_text(), p3, p5, p7) == g["as_text_after"]
    assert x.object1().as_text() == g["object1_after"]
    assert [d.as_text() for d in x.get_memory_dependencies()] == g["deps_after"]
    y = Alternating(object1=p3, object2=p5)
    y.set_object2(p7)
    assert norm(y.get_name(), p3, p7) == g["unnamed_set_name"]
    assert norm(y.as_text(), p3, p7) == g["unnamed_set_as_text"]


@pytest.mark.parametrize("case", cases("sufficient"), ids=lambda c: repr(c["atts"]))
def test_are_attributes_sufficient_to_build_golden(case):
    cat = Alternating(object1=P("3"), object2=P("5"))
    assert show(cat.are_attributes_sufficient_to_build(*case["atts"])) == case["result"]


@pytest.mark.parametrize("case", cases("instancer"), ids=lambda c: f"{c['cat']}-{c['pure']}")
def test_instancer_golden(case):
    cat = golden_cat(case["cat"])
    b = cat.instancer(O(case["pure"]))
    assert int(b is not None) == case["defined"]
    if b is not None:
        assert isinstance(b, SBindings) and case["ref"] == "SBindings"
        assert {k: desc(v) for k, v in b.get_bindings_ref().items()} == case["bindings"]
        assert b.slippages_count() == case["slippages_count"]
        assert b.get_metonymy_mode().as_text() == case["metonymy_mode"]


@pytest.mark.parametrize("case", cases("find_mapping"), ids=lambda c: f"{c['cat']}-{c['a']}-{c['b']}")
def test_find_mapping_for_cat_golden(case):
    cat = golden_cat(case["cat"])
    m = cat.find_mapping_for_cat(O(case["a"]), O(case["b"]))
    if m is None:
        assert case["result"] is None
        return
    assert f"Num({m.name},{alt_desc(m.cat)})" == case["result"]
    assert (m.cat is cat) == bool(case["cat_is_self"])


ORIGINALS = {
    "obj 3": lambda: O("3"), "obj 5": lambda: O("5"), "obj 7": lambda: O("7"),
    "obj [1,2]": lambda: O("[1,2]"), "plain 3": lambda: 3, "plain 5": lambda: "5",
    "plain 7": lambda: 7, "plain [1,2]": lambda: "[1,2]", "plain [1, 2]": lambda: "[1, 2]",
}


@pytest.mark.parametrize("case", cases("apply"),
                         ids=lambda c: f"{c['cat']}-{c['transform']}-{c['original']}")
def test_apply_mapping_for_cat_golden(case):
    cat = golden_cat(case["cat"])
    transform = FakeTransform(case["transform"])
    original = ORIGINALS[case["original"]]()
    if case["died"]:
        with pytest.raises(Confess) as e:
            cat.apply_mapping_for_cat(transform, original)
        assert str(e.value) == case["error"]
        return
    ret = cat.apply_mapping_for_cat(transform, original)
    assert desc(ret) == case["result"]
    assert isinstance(ret, FakeBuilt) == bool(case["is_built"])


BUILDS = {
    "sint 0": lambda: {"which": SInt(0)}, "sint 1": lambda: {"which": SInt(1)},
    "sint 2": lambda: {"which": SInt(2)}, "sint -1": lambda: {"which": SInt(-1)},
    'sint "0.0"': lambda: {"which": SInt("0.0")}, 'sint "1.0"': lambda: {"which": SInt("1.0")},
    "sint 0.5": lambda: {"which": SInt(0.5)}, "sint undef": lambda: {"which": SInt(None)},
    'sint "abc"': lambda: {"which": SInt("abc")}, 'sint "1abc"': lambda: {"which": SInt("1abc")},
    'sint ""': lambda: {"which": SInt("")},
    "plain 0": lambda: {"which": 0}, "plain 1": lambda: {"which": 1},
    "undef": lambda: {"which": None}, "missing": lambda: {},
    "extra key": lambda: {"which": SInt(1), "x": 5},
}


@pytest.mark.parametrize("case", cases("build"), ids=lambda c: f"{c['cat']}-{c['label']}")
def test_build_golden(case):
    cat = golden_cat(case["cat"])
    args = BUILDS[case["label"]]()
    if case["died"]:
        # Perl dies with a message (Confess) or calls a method on a plain scalar.
        with pytest.raises((Confess, AttributeError)) as e:
            cat.build(args)
        if e.type is Confess:
            assert str(e.value) == case["error"]
        else:
            assert "get_mag" in case["error"]
        return
    ret = cat.build(args)
    assert desc(ret) == case["result"]
    assert [int(c is cat) for c in ret.described] == case["described_self"]


@pytest.mark.parametrize("case", cases("flipping"), ids=lambda c: c["cat"])
def test_flipping_mapping_golden(case):
    cat = golden_cat(case["cat"])
    m = cat.flipping_mapping()
    assert f"Num({m.name},{alt_desc(m.cat)})" == case["result"]
    assert (m.cat is cat) == bool(case["cat_is_self"])


# --- CheckForAlternation ------------------------------------------------------------------

def F(p, **kw):
    return FakeObj(P(p), f"O{p}", **kw)


NUMCAT = FakeCat("numcat", numeric=1)
CAT = FakeCat("cat")
OTHER = FakeCat("other")
INSUFF = FakeCat("insufficient", sufficient=0)


def FC(p, b):
    return F(p, cats=[CAT], b=({CAT: b} if b is not None else {}))


CHECKS = {
    "objects alternate": lambda: [F("3"), F("5"), F("3")],
    "objects alternate, reversed": lambda: [F("5"), F("3"), F("5")],
    "objects all same": lambda: [F("3"), F("3"), F("3")],
    "sints alternate": lambda: [SInt(3), SInt(5), SInt(3)],
    "sints all same": lambda: [SInt(4), SInt(4), SInt(4)],
    "sints no alternation": lambda: [SInt(3), SInt(5), SInt(7)],
    "sints first eq second": lambda: [SInt(3), SInt(3), SInt(5)],
    "objects no common category": lambda: [F("3"), F("5"), F("7")],
    "numeric common category": lambda: [F(p, cats=[NUMCAT]) for p in ("3", "5", "7")],
    "not of category (second)": lambda: [FC("3", {"x": FakeVal(1)}), FC("5", None),
                                         FC("7", {"x": FakeVal(3)})],
    "not of category (first)": lambda: [FC("3", None), FC("5", {"x": FakeVal(2)}),
                                        FC("7", {"x": FakeVal(3)})],
    "common category only in some": lambda: [
        F("3", cats=[OTHER, CAT], b={CAT: {"x": FakeVal(1)}}),
        F("5", cats=[CAT], b={CAT: {"x": FakeVal(2)}}),
        F("7", cats=[CAT, OTHER], b={CAT: {"x": FakeVal(3)}})],
    "attributes insufficient": lambda: [F(p, cats=[INSUFF], b={INSUFF: {"x": FakeVal(n)}})
                                        for p, n in (("3", 1), ("5", 2), ("7", 3))],
    "same mapping succ": lambda: [FC("3", {"x": FakeVal(1)}), FC("5", {"x": FakeVal(2)}),
                                  FC("7", {"x": FakeVal(3)})],
    "same mapping same": lambda: [FC("3", {"x": FakeVal(4)}), FC("5", {"x": FakeVal(4)}),
                                  FC("7", {"x": FakeVal(4)})],
    "recurse into alternation": lambda: [FC("3", {"x": FakeVal(1)}), FC("5", {"x": FakeVal(4)}),
                                         FC("7", {"x": FakeVal(1)})],
    "recurse, mappings differ (succ, pred)": lambda: [FC("3", {"x": FakeVal(1)}),
                                                      FC("5", {"x": FakeVal(2)}),
                                                      FC("7", {"x": FakeVal(1)})],
    "recurse fails": lambda: [FC("3", {"x": FakeVal(1)}), FC("5", {"x": FakeVal(4)}),
                              FC("7", {"x": FakeVal(9)})],
    "recurse on sints": lambda: [FC("3", {"x": SInt(1)}), FC("5", {"x": SInt(2)}),
                                 FC("7", {"x": SInt(1)})],
    "recurse on sints, numeric stop": lambda: [FC("3", {"x": SInt(1)}), FC("5", {"x": SInt(2)}),
                                               FC("7", {"x": SInt(6)})],
    "two keys, one recursion": lambda: [FC("3", {"x": FakeVal(1), "y": FakeVal(1)}),
                                        FC("5", {"x": FakeVal(2), "y": FakeVal(4)}),
                                        FC("7", {"x": FakeVal(3), "y": FakeVal(1)})],
    "no keys": lambda: [FC("3", {}), FC("5", {}), FC("7", {})],
    "missing key in later bindings": lambda: [FC("3", {"x": FakeVal(1)}), FC("5", {}), FC("7", {})],
}


@pytest.mark.parametrize("case", cases("check"), ids=lambda c: c["label"])
def test_check_for_alternation_golden(case):
    objs = CHECKS[case["label"]]()
    for c in (CAT, INSUFF):
        c.asked = []
    if case["died"]:
        # Perl dies calling a missing method (on a FakeVal or on undef).
        with pytest.raises(AttributeError):
            Alternating.check_for_alternation(*objs)
    else:
        ret = Alternating.check_for_alternation(*objs)
        assert desc(ret) == case["result"]
        if isinstance(ret, FakeMapping):
            m = ret.cat
            assert (Alternating.create(m.object1(), m.object2()) is m) == bool(case["result_cat_is_memo"])
    assert MESSAGES == case["messages"]
    assert [None if isinstance(o, SInt) else [desc(c) for c in o.described]
            for o in objs] == case["described"]
    assert [[alt_desc(c) if isinstance(c, Alternating) else c.get_name() for c in o.get_categories()]
            if isinstance(o, SInt) else None for o in objs] == case["sint_cats"]
    if "asked" in case:
        cat = INSUFF if case["label"] == "attributes insufficient" else CAT
        assert cat.asked[-1] == case["asked"][-1]


# --- direct tests --------------------------------------------------------------------------

def test_classes_and_roles():
    assert Alternating.__mro__[1:3] == (NotMetonyable, SCategory)
    assert Alternating.perl_name == "SCategory::Alternating"
    cat = Alternating(object1=P("3"), object2=P("5"))
    assert categorizable._registered(cat) is cat


def test_create_sorts_by_ref_string():
    a, b = P("3"), P("5")
    cat = Alternating.create(a, b)
    expected = sorted([a, b], key=perl_ref_string)
    assert [cat.object1(), cat.object2()] == expected


def test_instancer_bindings_are_sints():
    cat = Alternating(object1=P("3"), object2=P("5"))
    b = cat.instancer(O("5"))
    which = b.get_binding_for_attribute("which")
    assert isinstance(which, SInt) and which.get_mag() == 1
    assert b.get_squinting_raw() == {}
    assert b.get_metonymy_mode() is METO_MODE.NONE


def test_check_for_alternation_adds_category_to_sints():
    objs = [SInt(3), SInt(5), SInt(3)]
    ret = Alternating.check_for_alternation(*objs)
    for o in objs:
        assert o.get_categories()[-1] is ret.cat
    assert o.get_categories()[0] is S.NUMBER


def test_check_for_alternation_builds_structural_opts():
    objs = CHECKS["same mapping succ"]()
    ret = Alternating.check_for_alternation(*objs)
    assert ret["category"] is CAT
    assert ret["meto_mode"] is METO_MODE.NONE
    assert ret["direction_reln"] is DIR_SAME
    assert ret["slippages"] == {} and ret["changed_bindings"] == {"x": "succ"}


def test_serialize_and_deserialize_use_sltm_hooks(monkeypatch):
    cat = Alternating(object1=P("3"), object2=P("5"))
    seen = []
    monkeypatch.setattr(alternating, "_sltm_encode", lambda *objs: seen.append(objs) or "ENC")
    monkeypatch.setattr(alternating, "_sltm_decode", lambda s: [P("5"), P("3")])
    assert cat.serialize() == "ENC"
    assert seen == [(cat.object1(), cat.object2())]
    assert Alternating.deserialize("ENC") is Alternating.create(P("3"), P("5"))


def test_hooks_are_stubs_until_later_items(monkeypatch):
    monkeypatch.undo()
    # Item 028: SLTM::Platonic->create is real now.
    assert alternating._platonic_create("3").as_text() == "plat3"
    # Item 029: SLTM::encode/decode are real now.
    assert alternating._sltm_decode(alternating._sltm_encode("x")) == ["x"]
    alternating._message("headless: logged, not shown", 1)
