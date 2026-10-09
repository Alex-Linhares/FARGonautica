"""Composite views (loop0002 item 013): seqsee/gui/draw/views.py.

Mirrors lib/Tk/Seqsee.pm: the 11 @ViewOptions (parts as percentages of the canvas),
SetupParts, Update (delete('all'), each part's DrawIt in order on one shared canvas, an
uncaught die leaving Update), AttentionNeeded / AttentionNoLongerNeeded and
DrawAttentionDirectingArrows (with Style::Element(0) from lib/Themes/Std2.pm), and the shared
list viewers whose page numbers outlive a view change. The parts are lib/SGUI/Workspace.pm,
Workspace_Attention.pm, Slipnet.pm, Coderack.pm, Stream.pm, Relations.pm, List.pm and
List/{Groups,Categories,Rules,Stream}.pm; on a shared canvas their `$Canvas->lower` and
`$Canvas->raise('hilit')` act on every item, not just the part's own.
Golden: tests/golden/gui_views.json from oracle/gui_views.pl, which builds the real
Tk::Seqsee widget and switches views as its View menu does.
"""
import dataclasses
import re
from pathlib import Path

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee.gui import snapshot
from seqsee.gui.draw import lists, ops, views, workspace
from seqsee.gui.draw.theme import Style

CASES = load("gui_views")
ROOT = Path(__file__).resolve().parents[2]
REF = re.compile(r"=(?:HASH|SCALAR|ARRAY|CODE)\((?:ADDR|0x[0-9a-f]+)\)")


def _id(case):
    return "{}-v{}-{}{}{}".format(
        case["recipe"], case["view"], "x".join(str(v) for v in case["size"]),
        "-attn" if case["attention"] else "",
        "".join("-%s%d" % (k.rsplit(":", 1)[-1], p) for k, p in sorted(case["pages"].items()))
        + ("-before-setup" if case["before_setup"] else ""))


def _norm_text(text):
    """Texts without addresses; a list of refs (the Groups list's categories column, in hash
    order) is sorted."""
    if text is None or not REF.search(text):
        return text
    text = REF.sub("=REF", text)
    return ", ".join(sorted(text.split(", ")))


def _norm_ops(out):
    return [dataclasses.replace(o, text=_norm_text(o.text)) if isinstance(o, ops.Text) else o
            for o in out]


def _norm_items(items, tag_map):
    norm = []
    for item in items:
        item = dict(item, tags=[tag_map.get(t, t) for t in item["tags"]])
        if "text" in item["opts"]:
            item["opts"] = dict(item["opts"], text=_norm_text(item["opts"]["text"]))
        norm.append(item)
    return norm


def golden_measure(metrics):
    table = {(font, text): (w, ls) for font, text, w, ls in metrics}
    return lambda text, font: table[(font, text)]


def _group_key(snap, g):
    return (g.bounds_string, g.structure_string,
            tuple(sorted(snap.category(cid).name for cid in g.category_ids)))


def in_perl_order(snap, case):
    """The snapshot with Perl's hash orders (GetGroups' equal spans, the relations, the
    coderack history, the Categories list's categories), and Perl's group<K> / cat<K> tags
    mapped to the snapshot's obj<oid> / cat<cid>."""
    tag_map = {}
    pool = list(snap.groups)
    groups = []
    for row in case["groups"]:
        key = (row["bounds"], row["structure"], tuple(row["categories"]))
        i = next(i for i, g in enumerate(pool) if _group_key(snap, g) == key)
        g = pool.pop(i)
        groups.append(g)
        tag_map[row["tag"]] = "obj%d" % g.oid
    assert not pool, "groups Perl didn't list"
    pool = list(snap.relations)
    relations = []
    for row in case["relations"]:
        key = (tuple(row["ends"]), row["type"], row["class"])
        i = next(i for i, r in enumerate(pool)
                 if (tuple(r.end_bounds), r.type_text, r.perl_class) == key)
        relations.append(pool.pop(i))
    assert not pool, "relations Perl didn't list"
    cats = list(snap.categories)
    if case["categories"]:
        by_name = {c.name: c for c in snap.categories}
        cats = [by_name[row["name"]] for row in case["categories"]]
        for row in case["categories"]:
            tag_map[row["tag"]] = "cat%d" % by_name[row["name"]].cid
    snap = dataclasses.replace(
        snap, groups=tuple(groups), relations=tuple(relations), categories=tuple(cats),
        coderack=dataclasses.replace(snap.coderack,
                                     history=tuple((k, n) for k, n in case["history"])))
    return snap, tag_map


def _compose(case, snap):
    # A page turned before the view was chosen is lost: each list's Setup (SetupParts) sets
    # PageNumber back to 0.
    pages = {} if case["before_setup"] else case["pages"]
    return views.compose(case["view"] if case["view"] is not None else views.initial_view({}),
                         snap, *case["size"], attention_needed=bool(case["attention"]),
                         pages=pages, measure=golden_measure(case["metrics"]))


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_golden(case):
    gui_recipes.build(case["recipe"])
    snap, tag_map = in_perl_order(snapshot.take(), case)
    result = _compose(case, snap)
    assert result.died == case["died"]
    assert_ops_match(_norm_ops(result.ops), _norm_items(case["items"], tag_map))
    # A list whose DrawIt was not reached (an earlier part died) keeps the values of the
    # oracle's previous case: not compared.
    assert set(result.lists) <= set(case["lists"])
    for name, state in result.lists.items():
        perl = case["lists"][name]
        assert state.page_number == perl["page_after"], name
        if result.died_part == name:
            continue                    # GetEntriesOnCurrentPage died: nothing else was set
        assert state.shown_from == perl["shown_from"], name
        assert state.shown_to == perl["shown_to"], name
        assert state.entries_count == perl["entries_count"], name
    if result.died is None:
        assert set(result.lists) == set(case["lists"])


def test_golden_covers_views_and_states():
    keys = {(c["view"], tuple(c["size"]), c["attention"]) for c in CASES}
    for v in range(11):
        assert (v, (780, 450), 0) in keys and (v, (780, 450), 1) in keys
        assert (v, (1000, 700), 0) in keys and (v, (333, 211), 0) in keys
    assert any(c["view"] is None for c in CASES)
    died = {c["died"] for c in CASES if c["died"]}
    assert len(died) == 3        # the Rules list, Workspace_Attention, a Mapping::Dir node
    assert any(c["pages"] for c in CASES)
    assert any(c["attention"] and c["died"] for c in CASES)


@pytest.mark.perl_source
def test_view_options_match_perl_source():
    src = (ROOT / "lib" / "Tk" / "Seqsee.pm").read_text()
    body = src[src.index("our @ViewOptions"):src.index("my @Parts")]
    titles = re.findall(r"'([^']+)',\s*\[", body)
    parts = re.findall(r"\[\s*('SGUI::\w+'|\$\w+),\s*(\d+),\s*(\d+),\s*(\d+),\s*(\d+)\s*\]",
                       body)
    viewers = {"$ListGroupsViewer": "SGUI::List::Groups",
               "$ListCatViewer": "SGUI::List::Categories",
               "$ListRulesViewer": "SGUI::List::Rules",
               "$ListStreamViewer": "SGUI::List::Stream"}
    flat = [(viewers.get(p, p.strip("'")), int(l), int(t), int(w), int(h))
            for p, l, t, w, h in parts]
    assert [v.title for v in views.VIEW_OPTIONS] == titles
    assert [tuple(p) for v in views.VIEW_OPTIONS for p in v.parts] == flat
    golden_titles = {c["view"]: c["title"] for c in CASES if c["view"] is not None}
    assert golden_titles == {i: v.title for i, v in enumerate(views.VIEW_OPTIONS)}


def test_view_lookup_and_initial_view():
    assert views.view_index(3) == 3
    assert views.view_index("Workspace + Coderack") == 5
    with pytest.raises((KeyError, IndexError)):
        views.view_index("No such view")
    assert views.initial_view({}) == 0
    assert views.initial_view({"view": None}) == 0
    assert views.initial_view({"view": 0}) == 0
    assert views.initial_view({"view": "7"}) == 7


def test_part_rects_are_percentages_of_the_canvas():
    rects = views.part_rects(0, 780, 450)
    assert [r[0] for r in rects] == ["SGUI::Slipnet", "SGUI::Workspace", "SGUI::List::Groups",
                                     "SGUI::Relations"]
    # SetupParts: $l * 0.01 * $Width, ... (the same float expression as Perl)
    assert rects[0][1:] == (65 * 0.01 * 780, 0 * 0.01 * 450, 35 * 0.01 * 780, 50 * 0.01 * 450)
    assert rects[3][1:] == (35 * 0.01 * 780, 50 * 0.01 * 450, 65 * 0.01 * 780, 50 * 0.01 * 450)
    assert views.part_rects(1, 100, 80) == [("SGUI::Workspace", 0.0, 0.0, 100.0, 80.0)]


def test_attention_arrows():
    arrow, text = views.attention_arrows(780, 450)
    assert arrow == ops.line(390, 0.92 * 450, 390, 0.99 * 450, arrow="last", width=15,
                             fill="#FF0000")
    # Style::Element(0)'s own -anchor (centre) overrides the 'n' given before it.
    assert Style.Element(0)["anchor"] == "center"
    assert text == ops.text(390, 450 * 0.88, text="PLEASE SEE BELOW",
                            **dict(Style.Element(0), fill="#FF0000"))


def test_arrows_drawn_last_and_only_when_needed():
    gui_recipes.build("groups_relations")
    snap = snapshot.take()
    plain = views.compose(1, snap, 780, 450)
    attn = views.compose(1, snap, 780, 450, attention_needed=True)
    assert attn.ops[:len(plain.ops)] == plain.ops
    assert list(attn.ops[len(plain.ops):]) == views.attention_arrows(780, 450)


def test_a_die_stops_the_later_parts_and_the_arrows():
    gui_recipes.build("groups_relations")
    snap = snapshot.take()
    result = views.compose("Workspace + Attention", snap, 780, 450, attention_needed=True)
    assert result.died.startswith("Can't locate object method \"draw_attention\"")
    assert result.died_part == "SGUI::Workspace_Attention"
    _, *rect = views.part_rects("Workspace + Attention", 780, 450)[0]
    alone = workspace.draw(snap, *rect)
    # The Workspace stays drawn (its hilit borders lifted by the attention part's raise).
    plain = [o for o in alone if "hilit" not in o.tags]
    assert list(result.ops[:len(plain)]) == plain
    assert all(o in result.ops[len(plain):] for o in alone if "hilit" in o.tags)
    assert not any(o.coords == views.attention_arrows(780, 450)[0].coords for o in result.ops)
    # A part that dies first: nothing after it (view 0 starts with the Slipnet).
    gui_recipes.build("slipnet_dir")
    result = views.compose(0, snapshot.take(), 780, 450)
    assert result.died_part == "SGUI::Slipnet"
    assert result.lists == {}            # the Groups list's DrawIt never ran


def test_lowered_items_go_below_every_part():
    gui_recipes.build("groups_list")
    snap = snapshot.take()
    result = views.compose("Workspace + Groups", snap, 780, 450)
    bars = [o for o in result.ops if isinstance(o, ops.Rectangle)
            and o.fill in lists.BAR_COLOURS]
    assert bars and list(result.ops[:len(bars)]) == bars  # below the Workspace drawn before
    assert bars == sorted(bars, key=lambda o: -o.coords[1])   # latest lowered at the bottom


def test_hilit_raise_lifts_the_other_parts_items():
    gui_recipes.build("attention_groups")
    snap = snapshot.take()
    result = views.compose("Workspace + Attention", snap, 780, 450)
    hilit = [i for i, o in enumerate(result.ops) if "hilit" in o.tags]
    assert len(hilit) == 4               # two highlighted groups, in both parts
    # Workspace_Attention's raise('hilit') moves the Workspace's hilit borders up too: all
    # four sit together, after the attention part's black rectangle.
    assert hilit == list(range(hilit[0], hilit[0] + 4))
    black = next(i for i, o in enumerate(result.ops) if isinstance(o, ops.Rectangle)
                 and o.fill == "#000000")
    assert black < hilit[0]


def test_pages_and_list_states():
    gui_recipes.build("groups_many")
    snap = snapshot.take()
    # 12 rows per page in view 8: page 99 is clamped to the last page, 2.
    r = views.compose(8, snap, 780, 450, pages={"SGUI::List::Groups": 99})
    state = r.lists["SGUI::List::Groups"]
    assert (state.page_number, state.shown_from, state.shown_to, state.entries_count) == (
        2, 24, 29, 30)
    # Paging on from there (the page-down square), then a redraw.
    r = views.compose(8, snap, 780, 450, pages={"SGUI::List::Groups": state.page_number - 1})
    assert r.lists["SGUI::List::Groups"].shown_from == 12
    # Lists not in the view are not drawn and have no state.
    assert set(r.lists) == {"SGUI::List::Groups"}
    assert views.compose(1, snap, 780, 450).lists == {}


def test_compose_is_pure():
    gui_recipes.build("coderack_small")
    snap = snapshot.take()
    before = snapshot.take()
    a = views.compose(5, snap, 780, 450)
    b = views.compose(5, snap, 780, 450)
    assert a == b
    assert snap == before == snapshot.take()
