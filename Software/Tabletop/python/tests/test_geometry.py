"""The table, the nine blurry regions, the places opposite the touched object, the halves
and the edge gaps (D-001, D-002, D-004, D-005, D-022, D-032): the regions and places on the
digitized layouts, the blur's frequencies (seeded, with bounds set from the binomial
distribution), and the Blockage gaps recomputed from the footprints.
"""

import json
import math

import pytest

from tabletop import geometry as G
from tabletop import objects as O
from tabletop import problems as P
from tabletop.config import PROFILES
from tabletop.rng import Rng
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]


@pytest.fixture(scope="module")
def problems():
    return {p.name: p for p in P.load_all()}


# ---------------------------------------------------------------------------------------------
# The table and the nine regions (D-001, D-004).


def test_the_table_is_110_by_100_and_distances_are_isotropic():
    """D-001: the frames' aspect of 1.10 at 100 units of height."""
    assert (G.TABLE_WIDTH, G.TABLE_HEIGHT) == (110.0, 100.0)
    assert G.distance(0, 0, 3, 4) == 5.0
    assert G.distance(10, 20, 13, 20) == G.distance(10, 20, 10, 23)


def test_the_nine_regions_in_reading_order_with_their_names():
    """'nine regions like a tic-tac-toe board' [F95 p. 65]; the names the trace prints."""
    assert len(G.REGIONS) == 9
    assert [r.name for r in G.REGIONS] == [
        "upper-left", "upper-middle", "upper-right",
        "middle-left", "middle", "middle-right",
        "lower-left", "lower-middle", "lower-right"]
    for region in G.REGIONS:
        assert G.region_named(region.name) == region
    assert G.REGIONS[0] == G.Region(0, 0) and G.REGIONS[8] == G.Region(2, 2)
    assert G.column_boundaries() == (110 / 3, 220 / 3)
    assert G.row_boundaries() == (100 / 3, 200 / 3)


@pytest.mark.parametrize("x, y, name", [
    (0, 0, "upper-left"), (110, 100, "lower-right"), (55, 50, "middle"),
    (36.6, 10, "upper-left"), (110 / 3, 10, "upper-middle"), (73.4, 10, "upper-right"),
    (10, 33.3, "upper-left"), (10, 100 / 3, "middle-left"), (10, 66.7, "lower-left"),
    (-5, 120, "lower-left"), (200, -1, "upper-right"),
])
def test_exact_region_cuts_the_table_into_equal_thirds(x, y, name):
    """D-004: the region holding the centre; a point on a line belongs to the far third;
    points off the table are clamped."""
    assert G.exact_region(x, y).name == name


def test_adjacency_is_the_eight_neighbours():
    centre = G.region_named("middle")
    assert all(centre.is_adjacent(r) for r in G.REGIONS if r != centre)
    assert not centre.is_adjacent(centre)
    corner = G.region_named("upper-left")
    assert [r.name for r in G.REGIONS if corner.is_adjacent(r)] == [
        "upper-middle", "middle-left", "middle"]


# ---------------------------------------------------------------------------------------------
# Regions and places on the layouts (D-004, D-032).


def test_surround_layouts_touched_cup_upper_left_answers_lower_left_and_lower_right(problems):
    """Henry's cup is in the upper left and Eliza's glass and the isolated cup in the lower
    left and lower right in every Surround figure [F95 pp. 115-121]."""
    for n in range(1, 11):
        p = problems[f"surround-{n:02d}"]
        t = p.touched_object()
        assert G.exact_region(t.x, t.y).name == "upper-left", p.name
        e1, e2 = p.answer_object("E1"), p.answer_object("E2")
        assert G.exact_region(e1.x, e1.y).name == "lower-left", p.name
        assert G.exact_region(e2.x, e2.y).name == "lower-right", p.name


def test_surround_5_regions_object_by_object(problems):
    """The chapter 4 configuration: Henry's group in the upper left with its last spoons at
    the upper-middle line, Eliza's in the lower left with the knife at the lower-middle
    line, the isolated cup in the lower right [F95 p. 98 (img)]."""
    p = problems["surround-05"]
    regions = {o.id: G.exact_region(o.x, o.y).name for o in p.objects}
    assert regions == {
        "fork1": "upper-left", "fork2": "upper-left", "knife1": "upper-left",
        "cup1": "upper-left", "spoon1": "upper-middle", "spoon2": "upper-middle",
        "spoon3": "lower-left", "spoon4": "lower-left", "small-glass1": "lower-left",
        "fork3": "lower-left", "fork4": "lower-left", "knife2": "lower-middle",
        "cup2": "lower-right"}


def test_places_opposite_the_surround_touched_cup(problems):
    """'the most favored region is diagonally opposite the touched object, and the next most
    preferred region is directly opposite the touched object' [F95 p. 66]: for Henry's cup
    in the upper left these are the lower right and the lower left (D-032), which hold
    Surround 5's two answers."""
    p = problems["surround-05"]
    t = p.touched_object()
    touched = G.exact_region(t.x, t.y)
    assert G.diagonal_region(touched).name == "lower-right"
    assert G.opposite_region(touched).name == "lower-left"
    e1, e2 = p.answer_object("E1"), p.answer_object("E2")
    assert G.exact_region(e2.x, e2.y) == G.diagonal_region(touched)
    assert G.exact_region(e1.x, e1.y) == G.opposite_region(touched)
    relations = {r.name: G.region_relation(r, touched) for r in G.REGIONS}
    assert relations == {
        "upper-left": "near", "upper-middle": "near", "upper-right": "other",
        "middle-left": "near", "middle": "near", "middle-right": "other",
        "lower-left": "opposite", "lower-middle": "other", "lower-right": "diagonal"}
    weights = {r.name: G.region_weight(r, touched, CFG) for r in G.REGIONS}
    assert weights["lower-right"] == 1.0 and weights["lower-left"] == 0.7
    assert weights["upper-left"] == 0.4 and weights["upper-right"] == 0.15
    assert max(weights.values()) == weights["lower-right"]


def test_places_opposite_the_buridan_touched_spoon(problems):
    """Buridan's touched spoon is at the top centre [F95 p. 129 (img)]: the diagonal and the
    directly opposite regions coincide at the lower middle, and the two far corners, where
    the two candidate answers sit, take the opposite weight (D-032)."""
    p = problems["buridan-01"]
    t = p.touched_object()
    touched = G.exact_region(t.x, t.y)
    assert touched.name == "upper-middle"
    assert G.diagonal_region(touched) == G.opposite_region(touched) == G.region_named("lower-middle")
    assert G.region_relation(G.region_named("lower-middle"), touched) == "diagonal"
    assert G.region_relation(G.region_named("lower-left"), touched) == "opposite"
    assert G.region_relation(G.region_named("lower-right"), touched) == "opposite"
    assert G.region_relation(G.region_named("upper-right"), touched) == "near"
    e1, e2 = p.answer_object("E1"), p.answer_object("E2")
    assert G.region_weight(G.exact_region(e1.x, e1.y), touched, CFG) == 0.7
    assert G.region_weight(G.exact_region(e2.x, e2.y), touched, CFG) == 0.7


def test_a_touched_object_in_the_middle_row_or_the_centre_is_opposite_to_nothing():
    """D-032: a reflection that lands on the touched region itself is not 'opposite'."""
    middle_left = G.region_named("middle-left")
    assert G.opposite_region(middle_left) == middle_left
    assert G.region_relation(middle_left, middle_left) == "near"
    assert G.region_relation(G.region_named("middle-right"), middle_left) == "diagonal"
    centre = G.region_named("middle")
    assert G.diagonal_region(centre) == G.opposite_region(centre) == centre
    assert all(G.region_relation(r, centre) == "near" for r in G.REGIONS)


def test_every_region_has_one_diagonal_and_the_relations_partition_the_board():
    for touched in G.REGIONS:
        relations = [G.region_relation(r, touched) for r in G.REGIONS]
        assert set(relations) <= set(G.RELATIONS)
        assert relations.count("diagonal") == (0 if touched.name == "middle" else 1)
        assert relations.count("near") >= 4
        for r in G.REGIONS:
            assert G.region_weight(r, touched, CFG) in dict(CFG.region_preference).values()


def test_diagonal_and_opposite_points():
    """D-032: the reflections of a point through the centre and across the middle line."""
    assert G.diagonal_point(30.3, 15.3) == pytest.approx((79.7, 84.7))
    assert G.opposite_point(30.3, 15.3) == pytest.approx((30.3, 84.7))
    assert G.diagonal_point(55, 50) == (55.0, 50.0)
    assert G.opposite_point(10, 50) == (10.0, 50.0)
    dx, dy = G.diagonal_point(30.3, 15.3)
    assert G.exact_region(dx, dy).name == "lower-right"


def test_halves_are_the_upper_and_lower_rows_only():
    """D-005: Henry's and Eliza's halves exist only as regions; the middle row is nobody's."""
    assert [G.half(r) for r in G.REGIONS] == (
        [G.HENRY] * 3 + [None] * 3 + [G.ELIZA] * 3)


# ---------------------------------------------------------------------------------------------
# The blur (D-004): seeded, with bounds set from the binomial distribution. For n draws at
# probability p the count has standard deviation sqrt(n p (1 - p)); the bounds below are
# four standard deviations wide, so a correct implementation fails about once in 16,000
# seeds, and the seeds are fixed.


def crossings(x, y, n, seed, config=CFG):
    rng = Rng(seed)
    exact = G.exact_region(x, y)
    seen = [G.drawn_region(x, y, rng, config) for _ in range(n)]
    return sum(r != exact for r in seen), seen, rng.draws


def bounds(n, p, sigmas=4):
    sd = math.sqrt(n * p * (1 - p))
    return n * p - sigmas * sd, n * p + sigmas * sd


def test_blur_probability_is_half_on_the_line_and_vanishes_beyond_the_width():
    """D-004: region_blur_max x exp(-d / width); 'very close to a region boundary may
    sometimes be considered ... to be in the adjacent region' [F95 p. 65]."""
    assert G.blur_probability(0, CFG) == 0.5
    assert G.blur_probability(CFG.region_blur_width, CFG) == pytest.approx(0.5 / math.e)
    assert G.blur_probability(CFG.region_blur_width + 0.01, CFG) == 0
    assert G.blur_probability(2, CFG) == pytest.approx(0.5 * math.exp(-0.5))
    assert CFG.region_blur_width == 4.0 and CFG.region_blur_max == 0.5


def test_no_draw_when_no_boundary_is_within_the_blur_width():
    """An object well inside its region is never moved and costs no random number."""
    crossed, seen, draws = crossings(10, 10, 500, 1)
    assert crossed == 0 and draws == 0
    assert all(r.name == "upper-left" for r in seen)
    crossed, seen, draws = crossings(55, 50, 500, 1)
    assert crossed == 0 and draws == 0


def test_same_seed_same_regions():
    _, a, draws_a = crossings(37, 20, 200, 42)
    _, b, draws_b = crossings(37, 20, 200, 42)
    assert a == b and draws_a == draws_b == 200


def test_an_object_on_the_line_crosses_half_the_time():
    n = 4000
    crossed, seen, draws = crossings(110 / 3, 20, n, 3)
    assert draws == n
    low, high = bounds(n, 0.5)
    assert low <= crossed <= high, crossed
    assert {r.name for r in seen} == {"upper-left", "upper-middle"}


def test_an_object_a_glass_width_from_the_line_crosses_about_a_fifth_of_the_time():
    n = 4000
    d = CFG.region_blur_width  # 4.0 units, about one glass width (D-004)
    crossed, seen, draws = crossings(110 / 3 - d, 20, n, 5)
    assert draws == n
    low, high = bounds(n, 0.5 / math.e)
    assert low <= crossed <= high, crossed
    assert {r.name for r in seen} == {"upper-left", "upper-middle"}


def test_an_object_just_beyond_the_width_never_crosses():
    crossed, _, draws = crossings(110 / 3 - CFG.region_blur_width - 0.1, 20, 1000, 5)
    assert crossed == 0 and draws == 0


def test_both_axes_blur_independently_at_a_corner_of_four_regions():
    """At the crossing of two lines both coins are at 0.5, so the four regions around the
    corner each come up about a quarter of the time (two draws per call)."""
    n = 4000
    x, y = 110 / 3, 100 / 3
    rng = Rng(9)
    counts = {}
    for _ in range(n):
        name = G.drawn_region(x, y, rng, CFG).name
        counts[name] = counts.get(name, 0) + 1
    assert rng.draws == 2 * n
    assert set(counts) == {"upper-left", "upper-middle", "middle-left", "middle"}
    low, high = bounds(n, 0.25)
    assert all(low <= c <= high for c in counts.values()), counts


def test_surround_5_spoon_at_the_upper_middle_line_is_sometimes_seen_on_the_left(problems):
    """Surround 5's first spoon stands 2.1 units right of the line between the upper-left
    and upper-middle regions, so it is read in the upper left about 29 percent of the
    time; its neighbour, 8.4 units from the line, never (D-004)."""
    p = problems["surround-05"]
    spoon1, spoon2 = p.object("spoon1"), p.object("spoon2")
    d = spoon1.x - 110 / 3
    assert 0 < d < CFG.region_blur_width
    n = 4000
    crossed, seen, draws = crossings(spoon1.x, spoon1.y, n, 11)
    assert draws == n
    low, high = bounds(n, G.blur_probability(d, CFG))
    assert low <= crossed <= high, crossed
    assert {r.name for r in seen} == {"upper-middle", "upper-left"}
    assert crossings(spoon2.x, spoon2.y, 200, 11)[0] == 0


def test_the_blur_is_off_with_a_zero_maximum():
    config = v2(region_blur_max=0.0)
    crossed, _, draws = crossings(110 / 3, 20, 300, 1, config)
    assert crossed == 0 and draws == 0


def test_the_fault_profile_does_not_change_the_blur():
    """The blur is a decision, not one of French's faults (D-004)."""
    faults = V2_PROFILES["french-faults"]
    assert (faults.region_blur_width, faults.region_blur_max) == (
        CFG.region_blur_width, CFG.region_blur_max)


def test_fcca_regions_agree_with_the_exact_or_blurred_reading(problems):
    """FCCA's redrawn figures place each object in a ninth of its square table
    [FCCA pp. 394-399 (img)]; every one of the 130 is the region our centre gives or one the
    blur can draw (D-004), including the two disagreeing drawings (`docs/problems.md` 6)."""
    data = json.loads((P.PROBLEMS_DIR.parent / "tests" / "data" / "fcca_crosscheck.json")
                      .read_text())
    names = {"centre": "middle"}
    checked = 0
    for figure in data["figures"]:
        p = problems[figure["problem"]]
        for record in figure["objects"]:
            if record["id"] not in p.ids():
                continue
            row, column = record["region"].split()
            column = names.get(column, column)
            fcca = "middle" if (row, column) == ("middle", "middle") else f"{row}-{column}"
            o = p.object(record["id"])
            rng = Rng(7)
            seen = {G.drawn_region(o.x, o.y, rng, CFG).name for _ in range(300)}
            assert fcca in seen, (figure["figure"], record["id"], fcca, seen)
            checked += 1
    assert checked == 130


# ---------------------------------------------------------------------------------------------
# Footprints and edge gaps (D-002, D-022).


def test_gap_between_boxes():
    """D-022: 0 when boxes overlap or touch; the gap along the free axis; the corner distance."""
    a = (0, 0, 10, 10)
    assert G.gap_between(a, (5, 5, 15, 15)) == 0
    assert G.gap_between(a, (10, 0, 20, 10)) == 0
    assert G.gap_between(a, (13, 2, 20, 8)) == 3
    assert G.gap_between(a, (2, 14, 8, 20)) == 4
    assert G.gap_between(a, (13, 14, 20, 20)) == 5
    assert G.gap_between((13, 14, 20, 20), a) == 5


def test_boxes_and_glass_width():
    """D-002: the footprint centred on the position; a small glass is 4.5 units wide."""
    class Obj:
        type, orientation, x, y = "small glass", None, 50.0, 60.0
    assert O.box(Obj) == pytest.approx((47.75, 57.15, 52.25, 62.85))
    assert O.glass_width() == 4.5
    assert O.gap_in_glass_widths(9.0) == 2.0

    class Fork:
        type, orientation, x, y = "fork", "left", 10.0, 10.0
    left, top, right, bottom = O.box(Fork)
    assert right - left == pytest.approx(9.6) and bottom - top == pytest.approx(3.4)


def test_blockage_gaps_from_the_footprints(problems):
    """Blockage 8, 9 and 10 pull the two upper-right glasses apart so that one, two and
    three glasses would fit between them [F95 p. 128]. The engine's gap between the
    glasses' footprint boxes (D-002, D-022) reproduces the measured gaps of the files (1.3,
    1.8 and 2.6 glass widths; `docs/problems.md` 8) and keeps their order; French's whole
    numbers lie within the files' uncertainty of each."""
    glass = O.glass_width()
    widths = {}
    for n in (6, 8, 9, 10):
        p = problems[f"blockage-{n:02d}"]
        recorded = json.loads(P.problem_path(p.name).read_text())["gap"]
        a, b = p.object("small-glass2"), p.object("small-glass4")
        gap = O.edge_gap(a, b)
        widths[n] = O.gap_in_glass_widths(gap)
        # The footprint gap is the ink gap less the difference between the drawn boxes and
        # the 8 px footprint: Blockage 6 draws both glasses 7 px wide, so its gap comes out
        # one pixel (0.6 units) narrower than the file's; Blockage 8-10 draw them 8 px wide.
        drawn = sum(o.box_px[2] - o.box_px[0] + 1 for o in (a, b))
        allowance = (16 - drawn) * O.UNITS_PER_PIXEL + 0.15
        assert abs(gap - recorded["units"]) < allowance, (n, gap, recorded["units"])
        assert abs(widths[n] - recorded["glass_widths"]) < allowance / glass
    assert abs(O.edge_gap(*[problems["blockage-06"].object(i) for i in ("small-glass2", "small-glass4")]) - 2.2) < 0.05
    assert widths[6] < 1 < widths[8] < widths[9] < widths[10]
    uncertainty = problems["blockage-08"].uncertainty["units"] / glass
    for n, french in ((8, 1), (9, 2), (10, 3)):
        assert abs(widths[n] - french) <= uncertainty, (n, widths[n])
    assert (round(widths[8], 1), round(widths[9], 1), round(widths[10], 1)) == (1.3, 1.8, 2.7)


def test_neighbours_in_a_row_have_small_gaps_and_the_isolated_cup_a_large_one(problems):
    """Surround 5: the objects of Henry's row stand within a glass width of each other,
    and the isolated cup is many widths from everything [F95 p. 98 (img)] (D-022)."""
    p = problems["surround-05"]
    row = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
    for a, b in zip(row, row[1:]):
        assert O.gap_in_glass_widths(O.edge_gap(p.object(a), p.object(b))) < 1.0, (a, b)
    cup2 = p.object("cup2")
    assert all(O.gap_in_glass_widths(O.edge_gap(cup2, o)) > 8 for o in p.objects if o is not cup2)
