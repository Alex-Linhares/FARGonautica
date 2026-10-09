"""Tests for the coderack (item 036).

Mirrors lib/SCoderack.pm (clear, init, add_codelet, _choose_codelet, get_urgencies_sum,
get_codelet_count, get_next_runnable, expunge_codelet, AttentionDistribution) and
SUtil::clear_all_but_workspace. Golden data: oracle/scoderack.pl; each case is rebuilt
here by name.

Perl never returns when the random number exceeds the urgency mass (fractional urgencies,
a negative remaining sum): ``_choose_codelet`` walks off the end of @CODELETS forever. Those
cases are not in the golden file; the port raises Confess instead (tested from the source).
"""
import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scoderack, scodelet, srule_app, sworkspace, util
from seqsee.errors import Confess
from seqsee.mapping.numeric import MappingNumeric
from seqsee.objects.anchored import Anchored
from seqsee.saction import SAction
from seqsee.scodelet import SCodelet
from seqsee.srelation import SRelation

CASES = {c["case"]: c for c in golden.load("scoderack")}


@pytest.fixture(autouse=True)
def _reset_coderack():
    scoderack.reset()
    yield
    scoderack.reset()


def reset_state():
    Global.Steps_Finished = 0
    sworkspace.init({"seq": [1, 2, 3, 4, 5, 6]})
    Global.Feature.clear()
    scoderack.clear()
    scoderack.LastSelectedRunnable = None
    util.srand(1)


def cl(family, urgency, tag=None):
    return SCodelet(family, urgency, {"t": tag} if tag is not None else {})


def view(c):
    if c is None:
        return None
    if isinstance(c, str):
        return "str:" + c
    if not isinstance(c, SCodelet):
        return type(c).__name__
    return [c[0], c[1], c[3].get("t")]


def state():
    return {
        "codelets": [view(c) for c in scoderack.CODELETS],
        "count": scoderack.get_codelet_count(),
        "sum": scoderack.get_urgencies_sum(),
        "history": dict(scoderack.HistoryOfRunnable),
        "last": view(scoderack.LastSelectedRunnable),
    }


def check_state(expected, got=None):
    got = state() if got is None else got
    for k in ("codelets", "count", "history", "last"):
        assert got[k] == expected[k], k
    assert got["sum"] == pytest.approx(expected["sum"])


def attempt(fn):
    try:
        return fn(), None
    except Confess as e:
        return None, str(e)


# ---- clear / init -----------------------------------------------------------------------------


def test_empty():
    reset_state()
    check_state(CASES["empty"])


def test_init(capsys):
    reset_state()
    scoderack.init()
    assert capsys.readouterr().out == CASES["init"]["output"]
    check_state(CASES["init"])
    scoderack.init({"foo": 1})
    assert capsys.readouterr().out == CASES["init_twice"]["output"]
    check_state(CASES["init_twice"])
    scoderack.clear()
    check_state(CASES["init_cleared"])


def test_init_steps_undef(capsys):
    reset_state()
    Global.Steps_Finished = None
    scoderack.init()
    c = CASES["init_steps_undef"]
    assert Global.Steps_Finished == c["steps"]
    assert [x[2] for x in scoderack.CODELETS] == c["creation"]


def test_init_codelets_are_scodelets_with_empty_args(capsys):
    reset_state()
    scoderack.init()
    assert all(isinstance(c, SCodelet) and c.arguments == {} for c in scoderack.CODELETS)


# ---- add_codelet ------------------------------------------------------------------------------


@pytest.mark.parametrize("name,make", [
    ("add_undef", lambda e: None),
    ("add_string", lambda e: "SCodelet"),
    ("add_string2", lambda e: "foo"),
    ("add_hash", lambda e: {}),
    ("add_action", lambda e: SAction({"family": "FocusOn", "urgency": 5, "arguments": {}})),
    ("add_element", lambda e: e[0]),
])
def test_add_bad(name, make):
    reset_state()
    e = sworkspace.get_elements()
    scoderack.clear()
    _, error = attempt(lambda: scoderack.add_codelet(make(e)))
    c = CASES[name]
    assert error == c["error"]
    check_state(c)


def test_add_many():
    reset_state()
    urg = [5, 3, 7, 3, 9, 1, 4, 4, 8, 2, 6, 3, 1, 10, 2, 2, 5, 7, 1, 9, 3, 3, 6, 1, 2, 0.5, 1, 4, 3, 1]
    steps = CASES["add_many"]["steps"]
    for i, u in enumerate(urg):
        scoderack.add_codelet(cl(f"F{i}", u, f"c{i}"))
        check_state(steps[i])


def test_add_ties():
    reset_state()
    for i in range(26):
        scoderack.add_codelet(cl("A", 5, f"x{i}"))
    check_state(CASES["add_ties"])


def test_add_ties2():
    reset_state()
    for i in range(28):
        scoderack.add_codelet(cl("A", 10 if i % 3 else 2, f"x{i}"))
    check_state(CASES["add_ties2"])


def test_add_odd_urgency():
    reset_state()
    scoderack.add_codelet(cl("A", "7", "s"))
    scoderack.add_codelet(cl("B", None, "u"))
    check_state(CASES["add_odd_urgency"])


# ---- get_next_runnable ------------------------------------------------------------------------


def test_next_empty():
    reset_state()
    Global.LogString = "xx"
    r = scoderack.get_next_runnable()
    after = util.rand()
    c = CASES["next_empty"]
    assert view(r) == c["runnable"]
    assert type(r).__name__ == c["isa"]
    assert r[2] == c["creation"]
    assert Global.LogString == c["logstring"]
    assert after == pytest.approx(c["after"])
    check_state(c)


def test_next_empty_keeps_last():
    reset_state()
    scoderack.add_codelet(cl("Old", 5, "old"))
    scoderack.get_next_runnable()
    r = scoderack.get_next_runnable()
    c = CASES["next_empty_keeps_last"]
    assert view(r) == c["runnable"]
    check_state(c)


@pytest.mark.parametrize("seed", [1, 7, 42])
def test_next_seeded(seed):
    reset_state()
    util.srand(seed)
    urg = [5, 30, 7, 1, 12, 50, 3, 3, 20, 9]
    for i, u in enumerate(urg):
        scoderack.add_codelet(cl(f"F{i % 3}", u, f"c{i}"))
    picked = [view(scoderack.get_next_runnable()) for _ in range(12)]
    after = util.rand()
    c = CASES[f"next_seeded_{seed}"]
    assert picked == c["picked"]
    assert after == pytest.approx(c["after"])
    check_state(c)


def test_next_interleaved():
    reset_state()
    util.srand(3)
    picked = []
    for rnd in range(31):
        scoderack.add_codelet(cl(f"G{rnd % 4}", 1 + (rnd * 7) % 13, f"r{rnd}"))
        if rnd % 3 == 2:
            picked.append(view(scoderack.get_next_runnable()))
    after = util.rand()
    c = CASES["next_interleaved"]
    assert picked == c["picked"]
    assert after == pytest.approx(c["after"])
    check_state(c)


def test_next_zero_sum():
    reset_state()
    scoderack.add_codelet(cl("Z", 0, "z"))
    _, error = attempt(scoderack.get_next_runnable)
    after = util.rand()
    c = CASES["next_zero_sum"]
    assert error == c["error"]
    assert after == pytest.approx(c["after"])
    check_state(c)


def test_next_fractional():
    for run in CASES["next_fractional"]["runs"]:
        reset_state()
        util.srand(run["seed"])
        scoderack.add_codelet(cl("H", 0.5, "a"))
        scoderack.add_codelet(cl("H", 1, "b"))
        r, error = attempt(lambda: view(scoderack.get_next_runnable()))
        assert (r, error) == (run["runnable"], run["error"])
        check_state(run)


@pytest.mark.parametrize("seed", [2, 3, 10, 11])
def test_next_fractional_walk_off_raises(seed):
    """PERL-QUIRK: Perl loops forever here (autovivifying @CODELETS); the port raises."""
    reset_state()
    util.srand(seed)
    scoderack.add_codelet(cl("H", 0.5, "a"))
    scoderack.add_codelet(cl("H", 1, "b"))
    with pytest.raises(Confess, match="Perl loops forever"):
        scoderack.get_next_runnable()
    assert len(scoderack.CODELETS) == 2


def test_next_negative():
    reset_state()
    scoderack.add_codelet(cl("Neg", -5, "n"))
    scoderack.add_codelet(cl("Pos", 10, "p"))
    r, error = attempt(lambda: view(scoderack.get_next_runnable()))
    c = CASES["next_negative"]
    assert (r, error) == (c["runnable"], c["error"])
    check_state(c)
    with pytest.raises(Confess, match="Perl loops forever"):
        scoderack.get_next_runnable()


# ---- _choose_codelet --------------------------------------------------------------------------


def test_choose_seeded():
    reset_state()
    util.srand(5)
    for u in [3, 1, 4, 1, 5, 9, 2, 6]:
        scoderack.add_codelet(cl("C", u, "u"))
    idx = [scoderack.choose_codelet() for _ in range(40)]
    c = CASES["choose_seeded"]
    assert idx == c["indices"]
    check_state(c)
    scoderack.clear()
    assert scoderack.choose_codelet() == CASES["choose_empty"]["index"]


# ---- AttentionDistribution --------------------------------------------------------------------


def named(dist, names):
    return {names.get(id(k), k): v for k, v in dist.items()}


def check_dist(got, expected):
    assert set(got) == set(expected)
    for k, v in expected.items():
        assert got[k] == pytest.approx(v)


def test_attention_empty():
    reset_state()
    assert scoderack.attention_distribution() == CASES["attention_empty"]["dist"]


def test_attention_no_reader():
    reset_state()
    e = sworkspace.get_elements()
    names = {id(x): f"e{i}" for i, x in enumerate(e)}
    scoderack.add_codelet(SCodelet("X", 10, {"a": e[0], "b": e[1]}))
    scoderack.add_codelet(SCodelet("Y", 30, {"a": e[0], "n": 7}))
    scoderack.add_codelet(SCodelet("Z", 20, {}))
    check_dist(named(scoderack.attention_distribution(), names), CASES["attention_no_reader"]["dist"])


def test_attention_reader():
    reset_state()
    e = sworkspace.get_elements()
    names = {id(x): f"e{i}" for i, x in enumerate(e)}
    g = Anchored.create(e[2], e[3])
    sworkspace.add_group(g)
    names[id(g)] = "g23"
    r = SRelation({"first": e[4], "second": e[5], "type": MappingNumeric.create("succ", S.NUMBER)})
    r.insert()
    names[id(r)] = "r45"
    sworkspace.update_object_strengths()
    scoderack.add_codelet(SCodelet("X", 10, {"a": e[0], "b": g}))
    scoderack.add_codelet(SCodelet("FocusOn", 40, {}))
    scoderack.add_codelet(SCodelet("FocusOn", 10, {"what": e[1]}))
    p, o = sworkspace.get_object_or_relation_choice_probability_distribution()
    c = CASES["attention_reader"]
    check_dist({names[id(x)]: v for x, v in zip(o, p)}, c["reader"])
    check_dist(named(scoderack.attention_distribution(), names), c["dist"])


# ---- clear_all_but_workspace / hooks ----------------------------------------------------------


def test_clear_all_but_workspace():
    reset_state()
    scoderack.add_codelet(cl("A", 5, "a"))
    scoderack.get_next_runnable()
    scoderack.add_codelet(cl("B", 6, "b"))
    util.clear_all_but_workspace()
    c = CASES["clear_all_but_workspace"]
    check_state(c)
    assert len(sworkspace.get_elements()) == c["elements"]


def test_schedule_and_hooks_add_to_coderack():
    reset_state()
    cl("S", 3, "s").schedule()
    scodelet._coderack_add_codelet(cl("T", 4, "t"))
    sworkspace._coderack_add_codelet(cl("U", 5, "u"))
    srule_app._coderack_add_codelet(cl("V", 6, "v"))
    assert [view(c) for c in scoderack.CODELETS] == [
        ["S", 3, "s"], ["T", 4, "t"], ["U", 5, "u"], ["V", 6, "v"]]
    assert scoderack.get_urgencies_sum() == 18


def test_codelet_tree_log(tmp_path, monkeypatch, capsys):
    reset_state()
    log = tmp_path / "tree.log"
    monkeypatch.setattr(Global, "CodeletTreeLogfile", str(log))
    Global.Feature["CodeletTree"] = 1
    scoderack.init()
    scoderack.get_next_runnable()
    scoderack.clear()
    scoderack.get_next_runnable()
    for _ in range(26):
        scoderack.add_codelet(cl("A", 5))
    Global.CodeletTreeLogHandle.close()
    lines = log.read_text().splitlines()
    assert lines[0] == "Initial"
    assert lines[1].startswith("\tSCodelet=HASH(0x") and lines[1].endswith("\tFocusOn\t100")
    assert lines[3] == "Background"
    assert lines[4].endswith("\tFocusOn\t100")
    assert lines[-1].startswith("Expunge SCodelet=HASH(0x")
