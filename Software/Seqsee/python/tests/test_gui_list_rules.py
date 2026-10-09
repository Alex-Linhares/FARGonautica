"""Rules list drawing (loop0002 item 012): seqsee/gui/draw/lists/rules.py.

Mirrors lib/SGUI/List/Rules.pm (HeightPerRow 15; GetItemList calls
SRule->GetListOfSimpleRules and SRule->GetListOfCompoundRules, which lib/SRule.pm does not
define, so the real list dies before drawing anything; DrawOneItem writes as_text) on top of
lib/SGUI/List.pm. The golden's 'fake' cases install the two methods in the oracle to check
DrawOneItem and the paging with rule texts.
Golden: tests/golden/gui_list_rules.json from oracle/gui_list_rules.pl.
"""
import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee import srule
from seqsee.gui import snapshot
from seqsee.gui.draw import lists, ops
from seqsee.gui.draw.lists import rules

CASES = load("gui_list_rules")
NAME = "SGUI::List::Rules"


def _id(case):
    return "{}-{}-{}-p{}{}".format(case["recipe"], "fake" if case["fake"] else "real",
                                   "x".join(str(v) for v in case["rect"]), case["page"],
                                   "-redraw" if case["redraw"] else "")


def draw_or_partial(snap, rect, page, texts):
    try:
        lowered, out, state = rules.draw_layers(snap, *rect, page=page, rules=texts)
        return lowered + out, None, state
    except ops.DrawDied as e:
        return e.ops, str(e), None


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_golden(case):
    gui_recipes.build(case["recipe"])
    snap = snapshot.take()
    texts = case["rules"]                        # None: the real SRule, which has no list
    if case["redraw"]:
        lowered, out, state = rules.draw_layers(snap, *case["rect"], page=0, rules=texts)
        assert lists.survivors(lowered + out, NAME) == []
        page = state.page_number + 1
    else:
        page = case["page"]
    out, died, state = draw_or_partial(snap, case["rect"], page, texts)
    assert died == case["died"]
    assert_ops_match(out, case["items"])
    if died is None:
        assert state.page_number == case["page_after"]
        assert state.shown_from == case["shown_from"]
        assert state.shown_to == case["shown_to"]
        assert state.entries_count == case["entries_count"]


def test_golden_covers_states():
    real = [c for c in CASES if not c["fake"]]
    assert real and all(c["died"] == rules.NO_RULES and c["items"] == [] for c in real)
    # Perl dies inside GetItemList, before GetEntriesOnCurrentPage sets EntriesCount.
    assert all(c["entries_count"] is None for c in real)
    fake = [c for c in CASES if c["fake"]]
    assert any(c["redraw"] for c in fake) and any(c["page_after"] < 0 for c in fake)
    assert None in fake[0]["rules"] and "" in fake[0]["rules"]


def test_the_model_has_no_rule_lists():
    """The reason the real list dies: neither SRule nor its port has the two methods."""
    assert not hasattr(srule.SRule, "GetListOfSimpleRules")
    assert not hasattr(srule.SRule, "get_list_of_simple_rules")
    assert not hasattr(srule, "get_list_of_simple_rules")


def test_real_list_dies_with_nothing_drawn():
    gui_recipes.build("groups_list")
    with pytest.raises(ops.DrawDied) as e:
        rules.draw(snapshot.take(), 0, 0, 780, 450)
    assert str(e.value) == ('Can\'t locate class method "GetListOfSimpleRules" via package '
                            '"SRule"')
    assert e.value.ops == []


def test_one_row():
    gui_recipes.build("empty")
    lowered, out, st = rules.draw_layers(snapshot.take(), 10, 100, 300, 150,
                                         rules=["r0", None])
    assert [b.coords for b in lowered] == [(30, 135, 290, 150), (30, 120, 290, 135)]
    r0, r1 = out[:2]
    assert (r0.coords, r0.text, r0.anchor, r0.font) == ((30, 120), "r0", "nw", ops.DEFAULT_FONT)
    assert (r1.coords, r1.text) == ((30, 135), "")
    assert set(r0.tags) == {NAME, "rule0", NAME + "-Clickable-Item"}
    assert set(r1.tags) == {NAME, "rule1", NAME + "-Clickable-Item"}
    assert st.entries_count == 2
