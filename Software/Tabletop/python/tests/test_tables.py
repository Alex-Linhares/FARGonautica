"""The realistic random tables of `tabletop.tables` (D-068; [F95 p. 38])."""

from tabletop import objects as O
from tabletop import tables as T
from tabletop.config import PROFILES
from tabletop.problems import Problem, Provenance, TableObject, load_all, load_problem
from tabletop.rng import Rng
from tabletop.run import run_problem

SEEDS = range(1, 101)


def obj(id_, type_, x, y, orientation=None):
    return TableObject(id_, type_, x, y, orientation, (0, 0, 0, 0),
                       Provenance("synthetic", 0, "test"))


# ---- the rule a set table obeys, on French's own layouts ---------------------------------

def test_every_digitized_layout_passes_the_realism_rule():
    for problem in load_all():
        assert T.realism_faults(problem.objects) == [], problem.name


def test_the_realism_rule_catches_each_kind_of_fault():
    assert "not inside" in T.realism_faults([obj("cup1", "cup", 2.0, 15.0)])[0]
    assert "overlap" in T.realism_faults([obj("cup1", "cup", 30.0, 15.0),
                                          obj("cup2", "cup", 32.0, 15.0)])[0]
    assert "between the rows" in T.realism_faults([obj("cup1", "cup", 30.0, 32.0)])[0]
    spread = T.realism_faults([obj("cup1", "cup", 30.0, 8.0), obj("cup2", "cup", 60.0, 24.0)])
    assert len(spread) == 1 and "spread" in spread[0]
    assert T.realism_faults([obj("cup1", "cup", 30.0, 15.0), obj("cup2", "cup", 60.0, 16.0)]) == []


def test_overlap_depth_is_negative_apart_and_positive_when_overlapping():
    a, b = obj("cup1", "cup", 30.0, 15.0), obj("cup2", "cup", 40.0, 15.0)
    assert T.overlap_depth(a, b) < 0
    assert T.overlap_depth(a, obj("cup3", "cup", 33.0, 15.0)) > 0
    assert abs(T.overlap_depth(a, obj("cup3", "cup", 30.0 + O.footprint("cup")[0], 15.0))) < 1e-9


# ---- the generator ------------------------------------------------------------------------

def test_a_table_is_a_problem_with_synthetic_provenance_and_no_answers():
    p = T.random_table(1)
    assert isinstance(p, Problem)
    assert p.name == "random-1" and p.family == T.FAMILY and p.answers == {}
    assert p.source["tag"] == T.SYNTHETIC and p.expected is None
    assert p.touched in p.ids() and p.touched_object().id == p.touched
    assert all(o.provenance.figure == T.SYNTHETIC for o in p.objects)
    assert p.notes[0].startswith(f"{len(p.objects)} objects;")


def test_same_seed_same_table_and_seeds_differ():
    assert T.random_table(7) == T.random_table(7)
    assert T.random_table(7) != T.random_table(8)
    assert len({T.random_table(s).notes[0] for s in range(1, 21)}) > 15


def test_every_table_is_realistic_sized_and_well_named():
    for s in SEEDS:
        p = T.random_table(s)
        assert T.realism_faults(p.objects) == [], s
        assert 4 <= len(p.objects) <= 40, s
        ids = p.ids()
        assert len(set(ids)) == len(ids)
        for o in p.objects:
            assert o.type in O.TYPES
            assert o.id.startswith(O.id_stem(o.type))
            assert (o.orientation == "up") == (o.type in O.SILVERWARE)
            assert o.x == round(o.x, 1) and o.y == round(o.y, 1)


def test_both_rows_are_set_and_every_object_lies_near_its_rows_line():
    centres = {"henry": T.ROW_Y[0][1], "eliza": T.ROW_Y[1][1], "middle": T.MIDDLE_Y}
    seen_middle = 0
    for s in SEEDS:
        p = T.random_table(s)
        bands = {name: [o for o in p.objects if T.band_of(o.y) == name]
                 for name in ("henry", "eliza", "middle")}
        assert len(bands["henry"]) >= 2 and len(bands["eliza"]) >= 2, s
        seen_middle += bool(bands["middle"])
        for name, members in bands.items():
            for o in members:
                assert abs(o.y - centres[name]) <= T.ROW_JITTER + T.OBJECT_JITTER + 0.05, (s, o)
            xs = [o.x for o in members]
            assert xs == sorted(xs), (s, name)       # placed left to right
    assert 25 <= seen_middle <= 75                   # half the tables, by a coin


def test_the_eleven_types_all_appear_and_silverware_dominates():
    counts = {}
    for s in SEEDS:
        for o in T.random_table(s).objects:
            counts[o.type] = counts.get(o.type, 0) + 1
    assert set(counts) == set(O.TYPES)
    assert counts["fork"] > counts["cup"] > counts["salt shaker"]


def test_a_setting_has_the_order_of_frenchs_rows():
    # forks, usually a knife, the centre, spoons, sometimes a glass [F95 p. 118 (img)]
    rank = {"fork": 0, "knife": 1, "plate": 2, "soup bowl": 2, "cup": 2, "saucer": 2,
            "spoon": 3, "small glass": 4, "big glass": 4}
    rng = Rng(3)
    for _ in range(50):
        slots = T.setting(rng)
        types = [t for t, _ in slots]
        assert types[0] == "fork" and 1 <= types.count("fork") <= 2
        assert [rank[t] for t in types] == sorted(rank[t] for t in types)
        assert any(t in ("plate", "soup bowl", "cup") for t in types)
        if "saucer" in types:
            assert types[types.index("saucer") - 1] == "cup"


def test_the_touched_object_is_drawn_by_type_weight():
    placed = [("cup", None, 30.0, 15.0), ("fork", "up", 40.0, 15.0)]
    weights = dict(T.TOUCH_WEIGHTS)
    cups = sum(T.touched_index(Rng(s), placed) == 0 for s in range(1, 401))
    p = weights["cup"] / (weights["cup"] + weights["fork"])
    sd = (400 * p * (1 - p)) ** 0.5
    assert abs(cups - 400 * p) <= 4 * sd


def test_a_row_too_wide_loses_slots_then_settings_and_still_fits():
    rng = Rng(5)
    clusters = [[T._slot("plate")] * 6, [T._slot("plate")] * 6, [T._slot("plate")] * 6]
    placed = T.place_row(rng, clusters, 15.0)
    assert 0 < len(placed) < 18
    boxes = [O.box(obj(f"plate{i}", t, x, y)) for i, (t, _, x, y) in enumerate(placed)]
    assert all(b[0] >= 0 and b[2] <= T.TABLE_WIDTH for b in boxes)
    for a, b in zip(boxes, boxes[1:]):
        assert b[0] - a[2] >= T.GAP[0] - 0.11


def test_a_random_table_runs_to_an_answer_under_every_profile():
    for s in (1, 2, 3):
        p = T.random_table(s)
        for profile in PROFILES.values():
            r = run_problem(p, profile, s)
            assert r.answer in p.ids() and not r.capped


def test_describe_names_the_bands():
    p = load_problem("surround-10")
    text = T.describe(p.objects)
    assert text.startswith("18 objects; henry: fork fork knife cup spoon spoon; middle:")
    assert "eliza: spoon spoon small glass fork fork knife cup" in text
