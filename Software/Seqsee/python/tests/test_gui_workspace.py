"""Workspace drawing (loop0002 items 004 and 005): seqsee/gui/draw/workspace.py.

Mirrors lib/SGUI/Workspace.pm (Setup geometry, DrawIt, Seqsee::Element::draw_ws3 and
find_element_style, Seqsee::Anchored::draw_ws3 with find_group_style and
find_group_border_style, DrawGroups and raise('hilit'), DrawMetonym, SRelation::draw_ws3
with %RelationsToHide and %AnchorsForRelations, DrawBarLines, DrawLastRunnable) and
SGUI::Coderack::family_to_name (lib/SGUI/Coderack.pm), with the layout of
config/GUI_ws3.conf. Golden: tests/golden/gui_workspace.json from oracle/gui_workspace.pl,
which draws the oracle/GuiRecipes.pm states (the twins of tests/gui_recipes.py) in several
rectangles, and records the X server's metrics of every text it drew.
"""
import re
from pathlib import Path

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee.gui import snapshot
from seqsee.gui.draw import ops, workspace
from seqsee.gui.draw.theme import FONT_20, FONT_28

CASES = load("gui_workspace")
ROOT = Path(__file__).resolve().parents[2]


def _id(case):
    return "{}-{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]))


def golden_measure(metrics):
    """A text measure backed by the oracle's metrics: (text, font) -> (width, linespace)."""
    table = {(font, text): (w, ls) for font, text, w, ls in metrics}

    def measure(text, font):
        return table[(font, text)]
    return measure


ALL_METRICS = golden_measure([m for c in CASES for m in c["metrics"]])


def draw_recipe(name, rect, measure=ALL_METRICS):
    gui_recipes.build(name)
    return workspace.draw(snapshot.take(), *rect, measure=measure)


def _is_relation(item):
    d = item.to_dict() if isinstance(item, ops.Op) else item
    return d["type"] == "line" and (d.get("opts") or {}).get("arrow") == "last"


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_matches_perl_canvas(case):
    gui_recipes.build(case["recipe"])
    snap = snapshot.take()
    out = workspace.draw(snap, *case["rect"], measure=golden_measure(case["metrics"]))
    expected = case["items"]
    # Relations are drawn in hash order (values %SWorkspace::relations), and groups of equal
    # span in hash order too (GetGroups' rikeysort is stable over values %NonEltObjects).
    spans = [g.span for g in snap.groups]
    ordered = len(set(spans)) == len(spans)
    assert_ops_match([o for o in out if not _is_relation(o)],
                     [i for i in expected if not _is_relation(i)], ordered=ordered)
    assert_ops_match([o for o in out if _is_relation(o)],
                     [i for i in expected if _is_relation(i)], ordered=False)


def test_golden_covers_the_item_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "one_element", "six_elements", "twenty_elements", "bar_lines",
            "hilit_debug", "runnable_thought", "groups_relations", "large", "nested_groups",
            "metonyms", "overlapping_relations", "squint_no_groups"} <= recipes
    assert recipes <= set(gui_recipes.RECIPES)
    rects = {tuple(c["rect"]) for c in CASES}
    assert len(rects) >= 4 and any(x or y for x, y, _, _ in rects)


def test_layout_matches_gui_ws3_conf():
    text = (ROOT / "config" / "GUI_ws3.conf").read_text()

    def section(name):
        body = re.search(r"^\[%s\]\n(.*?)(?=^\[|\Z)" % name, text, re.M | re.S).group(1)
        return {k: float(v) for k, v in re.findall(r"^(\w+)\s*=\s*(\S+)", body, re.M)}

    wl = section("WorkspaceLayout")
    lay = workspace.LAYOUT
    assert lay.margin == section("Layout")["Margin"]
    assert (lay.elements_y_fraction, lay.min_gp_height_fraction, lay.max_gp_height_fraction,
            lay.meto_y_fraction, lay.reln_zenith_fraction, lay.barline_height_fraction) == (
        wl["ElementsYFraction"], wl["MinGpHeightFraction"], wl["MaxGpHeightFraction"],
        wl["MetoYFraction"], wl["RelnZenithFraction"], wl["BarlineHeightFraction"])


def test_setup_geometry():
    g = workspace.setup(50, 30, 600, 300)
    assert (g.effective_width, g.effective_height) == (560, 260)
    assert g.elements_y == 30 + 20 + 130
    assert g.min_gp_height == pytest.approx(5.2)
    assert g.max_gp_height == pytest.approx(52)
    assert g.meto_y == pytest.approx(30 + 20 + 195)
    assert g.barline_top == pytest.approx(180 - 39)
    assert g.barline_bottom == pytest.approx(180 + 39)


def test_elements_ignore_x_offset():
    # PERL-QUIRK: elements and bar lines are placed from $Margin, not $XOffset + $Margin.
    plain = draw_recipe("bar_lines", (0, 0, 600, 300))
    shifted = draw_recipe("bar_lines", (50, 0, 600, 300))
    assert [o.coords for o in plain] == [o.coords for o in shifted]


def test_last_runnable_ignores_offsets():
    # PERL-QUIRK: the label sits at ($Margin, $Height - $Margin), without either offset.
    label = draw_recipe("empty", (50, 30, 600, 300))[-1]
    assert (label.TYPE, label.coords, label.anchor) == ("text", (20, 280), "sw")


def test_element_ops_carry_object_tags():
    gui_recipes.build("six_elements")
    snap = snapshot.take()
    texts = [o for o in workspace.draw(snap, 0, 0, 780, 450) if "element" in o.tags]
    assert [o.tags for o in texts] == [("obj%d" % e.oid, "element", str(e.index))
                                       for e in snap.elements]
    assert [o.text for o in texts] == ["1", "1", "2", "1", "2", "3"]


def test_hilit_and_debug():
    out = draw_recipe("hilit_debug", (0, 0, 780, 450))
    elements = [o for o in out if "element" in o.tags]
    assert [o.font for o in elements] == [FONT_20, FONT_28, FONT_20, FONT_28, FONT_20, FONT_28]
    labels = [o for o in out if o.TYPE == "text" and not o.tags]
    assert [o.text for o in labels[:-1]] == ["0", "1", "2", "3", "4", "5"]
    assert labels[-1].text == ""          # Seqsee::SCF::FocusOn has no $NAME


@pytest.mark.parametrize("family, name", [
    ("Seqsee::SCF::FocusOn", None),
    ("", None),
    (None, None),
    ("SThought::Seqsee::Anchored", "Focusing on a Group"),
    ("SThought::Seqsee::Element", "Focusing on a Single Element"),
    ("SThought::SRelation", "Focusing on an Analogy"),
    ("SThought::SCat", "Focusing on a Category"),
])
def test_family_to_name(family, name):
    assert workspace.family_to_name(family) == name


@pytest.mark.perl_source
def test_family_names_match_perl_and_port():
    # Every `our $NAME` in lib/ and every NAME attribute of the ported SThought classes.
    perl = {}
    for path in (ROOT / "lib").rglob("*.pm"):
        package = None
        for line in path.read_text(errors="replace").splitlines():
            m = re.match(r"\s*package\s+([\w:]+)\s*;", line)
            if m:
                package = m.group(1)
            m = re.match(r"\s*our\s+\$NAME\s*=\s*'([^']*)'", line)
            if m:
                perl[package] = m.group(1)
    assert workspace.FAMILY_NAMES == perl

    from seqsee import sthought
    from seqsee.sthought import relations, scat, sobject
    port = {cls.NAME for mod in (relations, scat, sobject) for cls in vars(mod).values()
            if isinstance(cls, type) and issubclass(cls, sthought.SThought) and cls.NAME}
    assert port == set(perl.values())


def test_draw_returns_ops_and_leaves_snapshot_alone():
    gui_recipes.build("hilit_debug")
    snap = snapshot.take()
    before = hash(snap)
    out = workspace.draw(snap, 0, 0, 780, 450)
    assert all(isinstance(o, ops.Op) for o in out)
    assert hash(snap) == before
    assert workspace.draw(snap, 0, 0, 780, 450) == out


# ---- item 005: groups, metonyms, relations ------------------------------------------------

def _kinds(out):
    """Op kinds in draw order: 'fill'/'border' ovals, 'element', 'relation', 'bar', else type."""
    kinds = []
    for o in out:
        if o.TYPE == "oval":
            kinds.append("fill" if o.fill else "border")
        elif "element" in o.tags:
            kinds.append("element")
        elif _is_relation(o):
            kinds.append("relation")
        elif o.TYPE == "line" and o.width == 3:
            kinds.append("bar")
        else:
            kinds.append(o.TYPE)
    return kinds


def test_draw_order_groups_elements_relations_bars_label():
    kinds = _kinds(draw_recipe("groups_relations", (0, 0, 780, 450)))
    first = {k: kinds.index(k) for k in ("fill", "border", "element", "relation", "bar")}
    last = {k: len(kinds) - 1 - kinds[::-1].index(k) for k in first}
    assert max(last["fill"], last["border"]) < first["element"]
    assert last["element"] < first["relation"] and last["relation"] < first["bar"]
    assert kinds[-1] == "text"


def test_nested_groups_largest_first_and_hilit_raised():
    out = draw_recipe("nested_groups", (0, 0, 780, 450))
    ovals = [o for o in out if o.TYPE == "oval"]
    assert len(ovals) == 2 * 5
    fills = [o for o in ovals if o.fill]
    widths = [o.coords[2] - o.coords[0] for o in fills]
    assert widths == sorted(widths, reverse=True)
    # S2 is the largest group (no category: no stipple), S1 has no category (stipple).
    assert fills[0].stipple is None and fills[1].stipple == "gray75"
    # raise('hilit'): the borders of the highlighted groups (S2: Hilit 3, B: Hilit 1) end up
    # after all the other group ops, in their draw order.
    hilit = [o for o in ovals if "hilit" in o.tags]
    assert ovals[-2:] == hilit
    assert [o.width for o in hilit] == [8, 4] and hilit[1].dash == "---"
    assert all("hilit" not in o.tags for o in fills)


def test_relations_hidden_between_neighbours_unless_hilit():
    rels = [o for o in draw_recipe("nested_groups", (0, 0, 780, 450)) if _is_relation(o)]
    # A->B, e0->e1 hidden; S1->C hidden but highlighted; B->C, A->C, e1->e2 drawn.
    assert len(rels) == 4
    assert sorted(o.width for o in rels) == [3, 3, 3, 5]
    zenith = 20 + 0.15 * 410
    assert all(o.coords[3] == pytest.approx(zenith) for o in rels)
    assert all(o.arrowshape == (8, 12, 10) and o.smooth for o in rels)


def test_relation_to_outside_element_not_drawn():
    rels = [o for o in draw_recipe("overlapping_relations", (0, 0, 780, 450))
            if _is_relation(o)]
    assert len(rels) == 6     # of 9: two hidden inside G, one to an outside element


def test_squinted_elements_need_a_group():
    # PERL-QUIRK: DrawGroups returns early when there are no groups, so squinted elements and
    # element metonyms are not drawn; relations then use the elements' own anchors.
    out = draw_recipe("squint_no_groups", (0, 0, 780, 450))
    assert _kinds(out) == ["element"] * 3 + ["relation", "text"]
    assert out[3].coords[1] == 225 - 10


def test_squinted_element_anchor_is_its_oval_top():
    out = draw_recipe("metonyms", (0, 0, 780, 450))
    e5_oval = [o for o in out if o.TYPE == "oval" and o.fill][3]
    mid = (e5_oval.coords[0] + e5_oval.coords[2]) / 2
    e5_e6 = next(o for o in out if _is_relation(o) and o.coords[0] == pytest.approx(mid))
    assert e5_e6.coords[1] == pytest.approx(e5_oval.coords[1])


def test_metonym_texts_and_cross():
    out = draw_recipe("metonyms", (0, 0, 780, 450))
    actual, cross1, cross2, starred = out[:4]
    assert (actual.text, starred.text) == ("[2, 2, 2, 2]", "2")
    assert (actual.font, starred.font) == (FONT_20, FONT_20)
    assert actual.coords == pytest.approx((154.5, 347.5), abs=0.05)
    assert starred.coords == pytest.approx((154.5, 327.5), abs=0.05)
    # The two lines cross out Tk's bbox of the actual text.
    assert cross1.coords == (107, 338, 203, 359) and cross2.coords == (203, 338, 107, 359)


def test_ovals_and_rectangles_are_normalised_like_tk():
    # Tk swaps reversed corners (the 30x30 golden cases have a negative effective size).
    assert ops.oval(10, 20, 5, 2).coords == (5, 2, 10, 20)
    assert ops.rectangle((3, 1), (1, 3)).coords == (1, 1, 3, 3)
    assert ops.line(3, 1, 1, 3).coords == (3, 1, 1, 3)


@pytest.mark.parametrize("x, y, text, w, ls, bbox", [
    # Probed on the oracle's X server: fontMeasure, linespace and $c->bbox of a centred text.
    (100, 100, "2", 11, 21, (94, 90, 107, 111)),
    (100.3, 100.5, "2", 11, 21, (94, 91, 107, 112)),
    (100.5, 100.49, "[2, 2]", 48, 21, (76, 90, 126, 111)),
    (101.5, 100.5, "17", 22, 21, (90, 91, 114, 112)),
])
def test_text_bbox_is_tks(x, y, text, w, ls, bbox):
    assert workspace.text_bbox(x, y, text, "f", lambda t, f: (w, ls)) == bbox


def test_default_measure_is_plausible():
    w, ls = workspace.approx_measure("[2, 2, 2, 2]", FONT_20)
    assert 60 <= w <= 130 and 18 <= ls <= 26
    assert workspace.approx_measure("", FONT_20)[0] == 0
    gui_recipes.build("metonyms")
    assert workspace.draw(snapshot.take(), 0, 0, 780, 450)   # no measure: the default
