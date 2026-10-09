"""Tests for SWorkspace.pm, part I (item 031): clear/init/insert_elements, GetElements,
the bar lines and the magnitude scanners.

Mirrors lib/SWorkspace.pm (__Clear, clear, init, insert_elements, _insert_element,
__InsertElement, GetElements, GetSuperGroups, __CheckLiveness,
__CheckLivenessAtSomePoint, __ScanRightwardForElements, __ScanLeftwardForElements,
__CheckMagnitudesRightwards, __ClearBarLines, __AddBarLines, GetBarLines,
__ClosestBarLineToLeftGivenIndex, __ClosestBarLineToRightGivenIndex,
__CheckIfCrossesBarLinesInappropriately). Golden data: oracle/sworkspace_basic.pl.
"""
import pytest

import golden
from seqsee import global_ as Global
from seqsee import sltm, sworkspace, util
from seqsee.errors import Confess, ElementsBeyondKnownSought, ExceptionClassBase
from seqsee.objects import anchored
from seqsee.objects.element import Element

CASES = golden.load("sworkspace_basic")


def cases(kind):
    found = [c for c in CASES if c["kind"] == kind]
    assert found, kind
    return found


def err(e):
    """The oracle's err(): a string for plain dies, {class, message} for SErr objects."""
    if isinstance(e, ExceptionClassBase):
        return {"class": e.perl_name, "message": e.message or ""}
    return str(e)


def golden_err(e):
    """Moose exceptions are objects in Perl; the port raises Confess with the message."""
    if isinstance(e, dict) and e["class"].startswith("Moose::Exception::"):
        return e["message"]
    return e


def state():
    """The oracle's state()."""
    es = sworkspace.get_elements()
    return {
        "count": sworkspace.ElementCount,
        "mags": [e.get_mag() for e in es],
        "edges": [list(e.get_edges()) for e in es],
        "left_edge_of": [sworkspace.LEFT_EDGE_OF.get(e) for e in es],
        "live": [1 if sworkspace.check_liveness(e) else 0 for e in es],
        "supergroups": [len(sworkspace.get_super_groups(e)) for e in es],
        "strengths": [e.get_strength() for e in es],
        "as_text": [e.as_text() for e in es],
        "ltm_nodes": sltm.get_node_count(),
        "real_sequence": list(Global.RealSequence),
        "initial_terms": Global.InitialTermCount,
        "read_head": sworkspace.ReadHead,
        "relations": len(sworkspace.relations),
        "elements_var": len(sworkspace.elements),
    }


def attempt(f):
    try:
        f()
        return 1, None
    except (Confess, ExceptionClassBase) as e:
        return 0, err(e)


# --- the sequential part of the oracle, replayed in order --------------------------------
def test_golden_sequence():
    """init/clear/insert cases share LTM state (node counts), so replay them in order."""
    seq_cases = [c for c in CASES if c["kind"] not in ("scan", "closest", "crosses", "barlines",
                                                      "barlines_cleared")]
    it = iter(seq_cases)

    def check(c, **extra):
        assert c["state"] == state(), c
        for k, v in extra.items():
            assert c[k] == v, (c, k)

    sltm.clear()
    for seq in ([1, 1, 2, 1, 2, 3], [7, 8], [], ["4", "5.0", " 6"]):
        sworkspace.init({"seq": seq})
        c = next(it)
        assert c["kind"] == "init" and c["seq"] == seq
        check(c)

    Global.RealSequence[:] = [99]
    Global.InitialTermCount = 42
    ok, e = attempt(lambda: sworkspace.init({"seq": [3, "x", 4]}))
    c = next(it)
    assert c["kind"] == "init_dies"
    check(c, ok=ok, error=e)

    sworkspace.init({"seq": [1, 2, 3]})
    old = sworkspace.get_elements()
    sworkspace.ReadHead = 5
    sworkspace.relations.update(a=1, b=2)
    sworkspace.elements[:] = [1, 2]
    sworkspace.clear()
    c = next(it)
    assert c["kind"] == "clear"
    check(c,
          old_live=[1 if sworkspace.check_liveness(e) else 0 for e in old],
          old_live_once=[1 if sworkspace.check_liveness_at_some_point(e) else 0 for e in old],
          old_left_edge=[sworkspace.LEFT_EDGE_OF.get(e) for e in old],
          old_edges=[list(e.get_edges()) for e in old])

    sworkspace.clear()
    Global.Steps_Finished = 17
    Global.TimeOfLastNewElement = 3
    Global.TimeOfNewStructure = 4
    sworkspace.insert_elements(5, 6)
    check(next(it), last_new_element=17, new_structure=17)
    Global.Steps_Finished = 20
    sworkspace.insert_elements()
    check(next(it), last_new_element=20, new_structure=20)
    Global.Steps_Finished = 25
    ok, e = attempt(lambda: sworkspace.insert_elements(7, "y", 8))
    check(next(it), ok=ok, error=e, last_new_element=20, new_structure=20)
    Global.Steps_Finished = 0

    c = next(it)
    while c["kind"] == "insert_value":
        sworkspace.clear()
        value = [1] if c["type"] == "array" else c["value"]
        if c["type"] == "num" and c["value"] == 1e20:
            value = 1e20
        ok, e = attempt(lambda: sworkspace.insert_elements(value))
        assert (ok, e) == (c["ok"], golden_err(c["error"])), c
        assert c["state"] == state(), c
        c = next(it)

    assert c["kind"] == "insert_objects"
    sworkspace.clear()
    el = Element.create(9, 5)
    el2 = Element.create(4, 0)
    el2.set_edges(7, 8)
    sworkspace.insert_elements(1, el, el2, 2)
    es = sworkspace.get_elements()
    check(c, same_object=[1 if es[1] is el else 0, 1 if es[2] is el2 else 0])

    sworkspace.clear()
    sworkspace.insert_elements(el, el)
    c = next(it)
    assert c["kind"] == "insert_same_twice"
    check(c)

    sworkspace.clear()
    Global.ExtensionRejectedByUser.update({"3, 4": 1, "5": 1})
    sworkspace.insert_elements(3)
    c = next(it)
    assert c["kind"] == "insert_clears_rejected"
    assert c["rejected"] == sorted(Global.ExtensionRejectedByUser)

    assert next(it, None) is None


def test_insert_value_cases_cover_types():
    types = {c["type"] for c in cases("insert_value")}
    assert types == {"num", "str", "undef", "array"}


# --- scanning --------------------------------------------------------------------------------
@pytest.fixture
def seq_1_to_4():
    sworkspace.init({"seq": [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]})


@pytest.mark.parametrize("c", cases("scan"), ids=lambda c: f"{c['start']}:{c['mags']}")
def test_golden_scan(c, seq_1_to_4):
    assert sworkspace.scan_rightward_for_elements(c["start"], c["mags"]) == c["right"]
    assert sworkspace.scan_leftward_for_elements(c["start"], c["mags"]) == c["left"]
    try:
        result, error, nxt = sworkspace.check_magnitudes_rightwards(c["start"], c["mags"]), None, None
    except ElementsBeyondKnownSought as e:
        result, error, nxt = None, err(e), list(e.next_elements)
    assert (result, error, nxt) == (c["check"], c["check_error"], c["check_next"])


def test_check_magnitudes_does_not_modify_argument(seq_1_to_4):
    mags = [3, 4, 5]
    with pytest.raises(ElementsBeyondKnownSought):
        sworkspace.check_magnitudes_rightwards(8, mags)
    assert mags == [3, 4, 5]


# --- bar lines ---------------------------------------------------------------------------------
def test_golden_add_bar_lines():
    sworkspace.clear_bar_lines()
    for c in cases("barlines"):
        sworkspace.clear_bar_lines()
        for op in c["ops"]:
            sworkspace.add_bar_lines(*op)
        # JSON::PP prints "3" as 3 once the sort has used it as a number.
        assert [util.perl_num(x) for x in sworkspace.get_bar_lines()] == c["barlines"]
        assert sworkspace.BarLineCount == len(c["barlines"])
    sworkspace.clear_bar_lines()
    assert sworkspace.get_bar_lines() == cases("barlines_cleared")[0]["barlines"]
    assert sworkspace.BarLineCount == 0


def test_bar_lines_keep_given_values():
    """Perl sorts numerically but keeps the scalars ("3" stays a string)."""
    sworkspace.add_bar_lines(5, "3", 0)
    assert sworkspace.get_bar_lines() == [0, "3", 5]


@pytest.mark.parametrize("c", cases("closest"), ids=lambda c: f"{c['barlines']}@{c['index']}")
def test_golden_closest_bar_line(c):
    sworkspace.clear_bar_lines()
    sworkspace.add_bar_lines(*c["barlines"])
    left = sworkspace.closest_bar_line_to_left_given_index(c["index"])
    assert left == c["left"]
    assert sworkspace.closest_bar_line_to_right_given_index(c["index"]) == c["right"]
    # Perl returns an empty list (not undef) in list context when there is none.
    assert (0 if left is None else 1) == c["left_list_len"]


class FakeGroup:
    pass


@pytest.mark.parametrize("c", cases("crosses"),
                         ids=lambda c: f"<{c['left']},{c['right']}>{c['barlines']}")
def test_golden_crosses_bar_lines(c):
    gp = FakeGroup()
    sworkspace.LEFT_EDGE_OF[gp] = c["left"]
    sworkspace.RIGHT_EDGE_OF[gp] = c["right"]
    sworkspace.clear_bar_lines()
    sworkspace.add_bar_lines(*c["barlines"])
    assert sworkspace.check_if_crosses_bar_lines_inappropriately(gp) == c["result"]


# --- hooks and misc ----------------------------------------------------------------------------
def test_anchored_element_count_hook():
    sworkspace.init({"seq": [4, 5, 6]})
    assert anchored._element_count() == 3
    sworkspace.insert_elements(7)
    assert anchored._element_count() == 4
    es = sworkspace.get_elements()
    assert es[-1].is_flush_right() == 1 and es[0].is_flush_right() == 0


def test_get_elements_returns_a_copy():
    sworkspace.init({"seq": [1, 2]})
    sworkspace.get_elements().append("x")
    assert len(sworkspace.get_elements()) == 2


def test_clear_all_clears_workspace_first():
    sworkspace.init({"seq": [1, 2]})
    util.clear_all()
    assert sworkspace.ElementCount == 0 and sworkspace.get_elements() == []


def test_reset_forgets_everything():
    sworkspace.init({"seq": [1, 2]})
    old = sworkspace.get_elements()
    sworkspace.add_bar_lines(1)
    sworkspace.reset()
    assert not sworkspace.check_liveness_at_some_point(*old)
    assert sworkspace.get_bar_lines() == [] and sworkspace.BarLineCount == 0


def test_insert_registers_in_ltm():
    sworkspace.init({"seq": [5, 5, 6]})
    assert sltm.get_node_count() == 2
    assert sltm.memory_index(sworkspace.get_elements()[0].get_pure())
