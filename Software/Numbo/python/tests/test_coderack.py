"""Tests for numbo.coderack, the port of lisp/src/coderack.lisp.

Two kinds of checks:
  - the exact cases of python/fixtures/coderack.json (lisp/tests/oracle/coderack.lisp):
    a scripted sequence of cr- calls, and every cr- call of three real oracle
    runs, replayed with the shared RNG.  Results, (random n) draws, RNG states
    and bins must match the oracle's exactly.
  - a port of lisp/tests/coderack-tests.lisp (its 65 checks), using the shared RNG
    where the Lisp test uses CL RANDOM.
"""

import json
import math

import pytest

from conftest import lisp_data, lisp_data_decode, load_fixture
from numbo import coderack
from numbo.franz import intern
from numbo.rng import Rng
from numbo.world import World

CR = load_fixture("coderack.json")

LEVELS = [600, 300, 150, 7, 4, 1, 0]
TEST_RACK = intern("TEST-RACK")
CODERACK = intern("*CODERACK*")


def S(name):
    return intern(name)


def form(*names):
    """A codelet form: symbols from names, other values as they are."""
    return [intern(x) if isinstance(x, str) else x for x in names]


class Obj:
    """Stands for an object inside a codelet form (the real runs post
    cyto-nodes as arguments); the coderack only moves it around."""

    def __init__(self, encoded):
        self.encoded = encoded


def decode(x):
    """lisp_data_decode, with an encoded object ({"obj", "name"}) as an Obj."""
    if isinstance(x, dict) and "obj" in x:
        return Obj(x)
    if isinstance(x, list):
        return [decode(v) for v in x]
    return lisp_data_decode(x)


def encode(x):
    """lisp_data, with an Obj as its oracle encoding."""
    if isinstance(x, Obj):
        return x.encoded
    if isinstance(x, list) and x:
        return [encode(v) for v in x]
    return lisp_data(x)


def same(x, expected_encoded):
    """x (Python Lisp data) encodes to the oracle's value, 3 and 3.0 apart."""
    return json.dumps(encode(x)) == json.dumps(expected_encoded)


def bins_data(world, name):
    rack = world.get_prop(name, coderack.CODERACK)
    return None if rack is None else encode(rack.bins)


# --- exact cases ---------------------------------------------------------------

def run_op(world, record):
    """Replay one fixture op on WORLD; return (result, error, draws)."""
    kind = record["op"]
    args = decode(record["args"])
    draws = []
    world.rng.sink = lambda n, v: draws.append([n, v])
    try:
        fn = {"make": coderack.cr_make_coderack,
              "hang": coderack.cr_hang,
              "choose": coderack.cr_choose,
              "choose-full": lambda w, name: coderack.cr_choose(w, name, True),
              "count": coderack.cr_count,
              "empty?": coderack.cr_empty_p,
              "empty": coderack.cr_empty_coderack}[kind]
        return fn(world, *args), False, draws
    except coderack.CoderackError:
        return None, True, draws
    except ValueError:            # the RNG refuses a float total, as oracle RANDOM does
        return None, True, draws
    finally:
        world.rng.sink = None


def test_levels_from_create_coderack():
    """(create-coderack) after (init-chiffre) makes these levels; cr-make-coderack
    keeps them in order."""
    world = World()
    name = S("MY-CODERACK")
    assert coderack.cr_make_coderack(
        world, name, [600, 300, 150, 7, 4, 1, 0]) is name
    assert json.dumps([b[0] for b in world.get_prop(name, coderack.CODERACK).bins]) \
        == json.dumps(CR["levels"])


def test_scripted_ops():
    world = World()
    for i, record in enumerate(CR["ops"]):
        where = f"op {i}: {record['op']} {record['args']}"
        if record["op"] == "seed":
            world.rng.seed(record["args"][0])
            assert world.rng.state == record["state"], where
            continue
        result, error, draws = run_op(world, record)
        assert error == bool(record["error"]), where
        if not error:
            assert same(result, record["result"]), (where, result)
        assert draws == (record["draws"] or []), where
        assert world.rng.state == record["state"], where
        name = lisp_data_decode(record["args"][0]) if record["args"][0] is not None else None
        expected_bins = record["bins"]
        got = bins_data(world, name) if name is not None else None
        assert json.dumps(got) == json.dumps(expected_bins), where


def test_scripted_ops_cover_the_cases():
    kinds = {r["op"] for r in CR["ops"]}
    assert kinds == {"seed", "make", "hang", "choose", "choose-full", "count",
                     "empty?", "empty"}
    assert sum(1 for r in CR["ops"] if r.get("error")) >= 12
    # rejection: some choose draws more than once? at least, every choose of a
    # non-empty rack with positive urgency draws twice
    # a choose draws twice (bin, then codelet), or once from an urgency-0 bin
    chooses = [r for r in CR["ops"] if r["op"].startswith("choose") and not r["error"]]
    assert any(len(r["draws"] or []) == 2 for r in chooses)
    assert any(len(r["draws"] or []) == 1 for r in chooses)


@pytest.mark.parametrize("run", CR["runs"], ids=lambda r: f"{r['problem'][0]}-seed{r['seed']}")
def test_real_run_calls(run):
    """Every cr- call of a real oracle run: the same picks, draws and bins.
    Codelets draw from the RNG between coderack calls, so each choose starts
    from the oracle's RNG state before it."""
    world = World()
    chooses = 0
    for i, record in enumerate(run["calls"]):
        where = f"call {i}: {record['op']} {record['args']}"
        if record["op"].startswith("choose"):
            world.rng.state = record["state_before"]
            chooses += 1
        result, error, draws = run_op(world, record)
        assert not error, where
        if "result" in record:
            assert same(result, record["result"]), (where, result)
        if "draws" in record:
            assert draws == (record["draws"] or []), where
            assert world.rng.state == record["state"], where
        name = lisp_data_decode(record["args"][0])
        assert coderack.cr_count(world, name) == record["count"], where
        if "bins" in record:
            assert json.dumps(bins_data(world, name)) == json.dumps(record["bins"]), where
    assert chooses > 40
    assert "bins" in run["calls"][-1]


def test_real_runs_cover_the_operations():
    for run in CR["runs"]:
        assert {c["op"] for c in run["calls"]} == {"make", "hang", "choose", "empty"}
    assert [r["outcome"] for r in CR["runs"]] == [":SOLVED", ":CAPPED", ":GAVE-UP"]


# --- the port of lisp/tests/coderack-tests.lisp --------------------------------------

@pytest.fixture
def world():
    w = World()
    w.rng.seed(1987)
    coderack.cr_make_coderack(w, TEST_RACK, LEVELS)
    return w


def test_making_hanging_emptiness_clearing(world):
    w = world
    assert coderack.cr_make_coderack(w, TEST_RACK, LEVELS) is TEST_RACK
    assert coderack.cr_empty_p(w, TEST_RACK) is True
    assert coderack.cr_count(w, TEST_RACK) == 0
    assert coderack.cr_choose(w, TEST_RACK) is None
    assert coderack.cr_choose(w, TEST_RACK, True) is None
    f = form("LOOK-FOR-NEW-BLOCK")
    assert coderack.cr_hang(w, TEST_RACK, f, 4) is f
    assert coderack.cr_empty_p(w, TEST_RACK) is False
    assert coderack.cr_count(w, TEST_RACK) == 1
    assert coderack.cr_choose(w, TEST_RACK) == form("LOOK-FOR-NEW-BLOCK")
    assert coderack.cr_empty_p(w, TEST_RACK) is True
    # (cr-choose rack t): (form urgency)
    coderack.cr_hang(w, TEST_RACK, form("KILL-NODE", "X"), 300)
    assert coderack.cr_choose(w, TEST_RACK, True) == [form("KILL-NODE", "X"), 300]
    # the rack is named by a symbol held in a global, as in the source
    w[CODERACK] = TEST_RACK
    coderack.cr_hang(w, w[CODERACK], form("C", 0), 150)
    assert coderack.cr_choose(w, w[CODERACK]) == form("C", 0)
    # clearing
    for i in range(10):
        coderack.cr_hang(w, TEST_RACK, form("C", i), LEVELS[i % 7])
    assert coderack.cr_count(w, TEST_RACK) == 10
    assert coderack.cr_empty_coderack(w, TEST_RACK) is TEST_RACK
    assert coderack.cr_empty_p(w, TEST_RACK) is True
    assert coderack.cr_choose(w, TEST_RACK) is None
    assert coderack.cr_empty_coderack(w, TEST_RACK) is TEST_RACK
    coderack.cr_hang(w, TEST_RACK, form("AGAIN"), 1)
    assert coderack.cr_choose(w, TEST_RACK) == form("AGAIN")
    # remaking a rack starts it empty
    coderack.cr_hang(w, TEST_RACK, form("OLD"), 1)
    coderack.cr_make_coderack(w, TEST_RACK, LEVELS)
    assert coderack.cr_empty_p(w, TEST_RACK) is True
    # two racks are independent
    other = S("OTHER-RACK")
    coderack.cr_make_coderack(w, other, [10, 1])
    coderack.cr_hang(w, other, form("O"), 10)
    assert coderack.cr_empty_p(w, TEST_RACK) is True
    assert coderack.cr_count(w, other) == 1


def test_draining(world):
    w = world
    posted = [form("CODELET", i) for i in range(60)]
    for i, f in enumerate(posted):
        coderack.cr_hang(w, TEST_RACK, f, LEVELS[i % 7])
    assert coderack.cr_count(w, TEST_RACK) == 60
    got = []
    while not coderack.cr_empty_p(w, TEST_RACK):
        got.append(coderack.cr_choose(w, TEST_RACK))
    assert len(got) == 60
    assert sorted(f[1] for f in got) == list(range(60))
    assert coderack.cr_choose(w, TEST_RACK) is None
    # the same codelet posted twice is two codelets
    coderack.cr_hang(w, TEST_RACK, form("TWICE"), 4)
    coderack.cr_hang(w, TEST_RACK, form("TWICE"), 4)
    assert coderack.cr_count(w, TEST_RACK) == 2
    assert [coderack.cr_choose(w, TEST_RACK), coderack.cr_choose(w, TEST_RACK)] \
        == [form("TWICE"), form("TWICE")]
    assert coderack.cr_empty_p(w, TEST_RACK) is True


def test_urgency_zero(world):
    w = world
    for _ in range(200):
        coderack.cr_empty_coderack(w, TEST_RACK)
        coderack.cr_hang(w, TEST_RACK, form("ZERO"), 0)
        coderack.cr_hang(w, TEST_RACK, form("ONE"), 1)
        assert coderack.cr_choose(w, TEST_RACK) == form("ONE")
        assert coderack.cr_choose(w, TEST_RACK) == form("ZERO")
        assert coderack.cr_empty_p(w, TEST_RACK) is True
    coderack.cr_empty_coderack(w, TEST_RACK)
    coderack.cr_hang(w, TEST_RACK, form("Z1"), 0)
    coderack.cr_hang(w, TEST_RACK, form("Z2"), 0)
    assert coderack.cr_empty_p(w, TEST_RACK) is False
    got = [coderack.cr_choose(w, TEST_RACK), coderack.cr_choose(w, TEST_RACK)]
    assert sorted(f[0].name for f in got) == ["Z1", "Z2"]
    assert coderack.cr_empty_p(w, TEST_RACK) is True


def test_errors(world):
    w = world
    for call in (lambda: coderack.cr_hang(w, TEST_RACK, form("X"), 5),
                 lambda: coderack.cr_hang(w, TEST_RACK, form("X"), None),
                 lambda: coderack.cr_hang(w, S("NO-SUCH-RACK"), form("X"), 4),
                 lambda: coderack.cr_choose(w, S("NO-SUCH-RACK")),
                 lambda: coderack.cr_empty_p(w, S("NO-SUCH-RACK")),
                 lambda: coderack.cr_make_coderack(w, S("BAD-RACK"), [10, -1]),
                 lambda: coderack.cr_make_coderack(w, S("BAD-RACK"), [10, S("X")])):
        with pytest.raises(coderack.CoderackError):
            call()
    assert coderack.cr_empty_p(w, TEST_RACK) is True


def tally_first_choice(w, postings, trials):
    counts = {id(p[0]): 0 for p in postings}
    for _ in range(trials):
        coderack.cr_empty_coderack(w, TEST_RACK)
        for f, u in postings:
            coderack.cr_hang(w, TEST_RACK, f, u)
        counts[id(coderack.cr_choose(w, TEST_RACK))] += 1
    return counts


@pytest.mark.parametrize("postings,trials", [
    ([(form("A"), 300), (form("B"), 150), (form("C"), 7)], 20000),
    ([(form("FOUR", 1), 4), (form("FOUR", 2), 4), (form("FOUR", 3), 4), (form("SEVEN"), 7)], 20000),
    ([(form("U"), 600), (form("F1"), 300), (form("S"), 150), (form("T3"), 7),
      (form("F4"), 4), (form("F5"), 1)], 40000),
    ([(form("X"), 0), (form("Y"), 4), (form("Z"), 1)], 5000),
])
def test_weighted_selection(world, postings, trials):
    """P(codelet) = urgency / total urgency (chapter p.143), within 4.5 sd."""
    total = sum(u for _, u in postings)
    counts = tally_first_choice(world, postings, trials)
    for f, u in postings:
        prob = u / total
        expected = trials * prob
        sd = math.sqrt(trials * prob * (1 - prob))
        assert abs(counts[id(f)] - expected) <= max(1, 4.5 * sd), (f, counts[id(f)], expected)


def test_without_replacement(world):
    w = world
    second_b, trials = 0, 2000
    for _ in range(trials):
        coderack.cr_empty_coderack(w, TEST_RACK)
        coderack.cr_hang(w, TEST_RACK, form("A"), 300)
        coderack.cr_hang(w, TEST_RACK, form("B"), 150)
        if coderack.cr_choose(w, TEST_RACK) == form("A"):
            if coderack.cr_choose(w, TEST_RACK) == form("B"):
                second_b += 1
    assert abs(second_b - 2 / 3 * trials) < 4.5 * math.sqrt(trials * 2 / 9)


def choice_sequence(w, seed):
    w.rng.seed(seed)
    coderack.cr_empty_coderack(w, TEST_RACK)
    for i in range(40):
        coderack.cr_hang(w, TEST_RACK, form("C", i), LEVELS[i % 6])
    out = []
    while not coderack.cr_empty_p(w, TEST_RACK):
        out.append(coderack.cr_choose(w, TEST_RACK)[1])
    return out


def test_reproducible(world):
    assert choice_sequence(world, 31) == choice_sequence(world, 31)
    assert choice_sequence(world, 31) != choice_sequence(world, 32)


def test_source_shaped_calls():
    """codelets.lisp's create-coderack shape (create-coderack itself is
    ported with codelets.py, item 10)."""
    w = World()
    name = S("MY-CODERACK")
    w[CODERACK] = coderack.cr_make_coderack(w, name, LEVELS)
    assert w[CODERACK] is name
    assert [b[0] for b in coderack.cr_get(w, w[CODERACK]).bins] == LEVELS
    assert coderack.cr_empty_p(w, w[CODERACK]) is True
    coderack.cr_hang(w, w[CODERACK], form("LOOK-FOR-NEW-BLOCK"), 4)
    coderack.cr_hang(w, w[CODERACK], form("COMPARE-B-TO-T", "CYTO-BRICK1"), 600)
    assert coderack.cr_count(w, w[CODERACK]) == 2
    coderack.cr_empty_coderack(w, w[CODERACK])
    assert coderack.cr_empty_p(w, w[CODERACK]) is True


def test_cr_get_accepts_a_rack(world):
    rack = coderack.cr_get(world, TEST_RACK)
    assert coderack.cr_get(world, rack) is rack


def test_world_cr_hang_is_the_coderack(world):
    """populate-coderack posts through world.cr_hang: by default the real one."""
    f = form("LOOK-FOR-NEW-BLOCK")
    world.cr_hang(TEST_RACK, f, 4)
    assert coderack.cr_count(world, TEST_RACK) == 1


def test_world_rng_starts_like_the_oracle():
    """oracle.lisp: (defvar *oracle-rng-state* 0)."""
    assert World().rng.state == Rng(0).state == 0


def test_docstrings_name_the_lisp_origin():
    for name in ("cr_get", "cr_make_coderack", "cr_hang", "cr_count", "cr_empty_p",
                 "cr_empty_coderack", "cr_choose"):
        lisp = name.replace("_p", "?").replace("_", "-")
        assert f"coderack.lisp: {lisp}" in getattr(coderack, name).__doc__
