"""The problem library (D-001 to D-003): every file validates, its provenance is complete, its
labels name real objects and match French's result sets, the touched object is Henry's cup in
the upper left in every Surround and Blockage problem [F95 pp. 115-129 (img)] and his spoon at
the top centre in every Buridan problem [F95 pp. 129-139 (img)], Surround 6-9 differ from
Surround 5 only where French says [F95 pp. 119-121], Surround 10 is Surround 5 plus junk [F95
p. 121], the Blockage series grows as the text says and its last three gaps are one, two and
three glass widths within the uncertainty [F95 p. 128], the Buridan variations differ from
Buridan 1 only where French says [F95 pp. 131-139], Figure/Ground 1 is as described [F95
p. 139], and the Surround 5 file agrees with the chapter 4 screen dump of the same
configuration (`tests/data/ch4.json`).
"""

import copy
import json
from pathlib import Path

import pytest

from tabletop import problems as P
from tabletop import reported
from tabletop.geometry import TABLE_HEIGHT, TABLE_WIDTH, distance
from tabletop.objects import FOOTPRINTS_PX, ORIENTATIONS, SILVERWARE, TYPES, footprint, id_stem

PYTHON_DIR = Path(__file__).resolve().parents[1]
SURROUND = [f"surround-{n:02d}" for n in range(1, 11)]
BLOCKAGE = [f"blockage-{n:02d}" for n in range(1, 11)]
BURIDAN = [f"buridan-{n:02d}" for n in range(1, 14)]
FIGURE_GROUND = ["figure-ground-01"]
CH2 = [f"ch2-fig{n:02d}" for n in range(2, 21)]  # chapter 2's configurations (tests/test_ch2.py)
CHAPTER5 = SURROUND + BLOCKAGE + BURIDAN + FIGURE_GROUND
TOLERANCE = 5.0  # units: the silverware beside a replaced object shifts up to 3.4 (D-003)


@pytest.fixture(scope="module")
def problems():
    return {name: P.load_problem(name) for name in P.catalogue()}


def test_the_catalogue_holds_the_five_families():
    """Chapter 5's four families: Surround 1-10, Blockage 1-10, Buridan 1-13 and
    Figure/Ground 1 [F95 pp. 115-139]; Buridan 1 is one file for both program settings; and
    chapter 2's nineteen configurations, Figures 2.2-2.20 [F95 pp. 27-35]."""
    assert P.by_family("surround") == SURROUND
    assert P.by_family("blockage") == BLOCKAGE
    assert P.by_family("buridan") == BURIDAN
    assert P.by_family("figure-ground") == FIGURE_GROUND
    assert P.by_family("ch2") == CH2
    assert P.catalogue() == sorted(CHAPTER5 + CH2)
    assert len(P.catalogue()) == 53


def test_every_file_validates(problems):
    for name in P.catalogue():
        data = json.loads(P.problem_path(name).read_text())
        assert P.validate(data) == [], name
        assert data["name"] == name


def test_provenance_is_complete(problems):
    """Every object names its figure, printed page and extracted image; chapter 5's pages
    115-139 are PDF page + 10 and chapter 2's pages 27-35 are + 12 (SPEC.md's offsets). In
    chapter 5 the figures are the even-numbered images and in chapter 2 the odd ones (the
    others are the JPEGs' masks)."""
    for p in problems.values():
        assert p.source["tag"] == "F95" and p.source["figure"] == p.title
        assert p.method and p.uncertainty["units"] == 2 and p.uncertainty["note"]
        assert p.review.startswith("Reviewed")
        ch2 = p.family == "ch2"
        offset, parity = (12, 1) if ch2 else (10, 0)
        for o in p.objects:
            assert o.provenance.figure == p.title
            if ch2:
                assert 27 <= o.provenance.page <= 35, o.id  # the chapter 2 figures
            else:
                assert 115 <= o.provenance.page <= 139, o.id  # the chapter 5 figures
            assert o.provenance.page == p.source["page"]
            assert f"PDF page {o.provenance.page + offset}" in o.provenance.image
            assert "pdfimages" in o.provenance.image
        assert p.source["pdf_page"] == p.source["page"] + offset
        assert p.source["image_index"] % 2 == parity


def test_labels_match_every_reported_set(problems):
    """Every chapter 5 result set names labels the figure draws [F95 pp. 115-139]: the file's
    labels are exactly the set's (a bar for 'E1 or E2' [F95 p. 123] is `E1+E2` in
    `reported.py` and two labels in the file), and the touched object is E2 in Blockage 1-2
    and E3 in Blockage 3-10 (the cup Henry touched is an answer there [F95 p. 122])."""
    for rs in reported.CHAPTER5:
        p = problems[rs.problem]
        labels = {part for a in rs.answers for part in a.label.split("+")}
        assert set(p.answers) == labels, rs.problem
    for name in BLOCKAGE[:2]:
        assert problems[name].answers["E2"] == problems[name].touched == "cup1"
    for name in BLOCKAGE[2:]:
        p = problems[name]
        assert p.answers == {"E1": "small-glass1", "E2": "small-glass3", "E3": "cup1"}
    for name in BURIDAN:
        assert problems[name].answers["E1"] == "spoon2"
    assert problems["figure-ground-01"].answers == {"E1": "fork1", "E2": "fork2",
                                                    "E3": "small-glass1"}


def test_labels_name_real_ids_and_match_the_reported_sets(problems):
    for rs in reported.SURROUND:
        p = problems[rs.problem]
        assert set(p.answers) == {a.label for a in rs.answers}, rs.problem
        for label, id_ in p.answers.items():
            assert p.answer_object(label).id == id_
        assert p.answers["E2"] == "cup2" or rs.problem == "surround-06"


def test_the_touched_object_is_henrys_cup_upper_left(problems):
    """Surround and Blockage: Henry's cup in the upper left ninth [F95 pp. 115-129 (img)]."""
    for name in SURROUND + BLOCKAGE:
        p = problems[name]
        t = p.touched_object()
        assert t.id == "cup1" and t.type == "cup"
        assert t.x < TABLE_WIDTH / 3 and t.y < TABLE_HEIGHT / 3, (name, t.x, t.y)


def test_the_touched_object_is_henrys_spoon_top_centre_in_buridan(problems):
    """Buridan: Henry's spoon at the top centre, beside its sidekicks [F95 pp. 129-139
    (img)]; Eliza's spoon (E1) lower left and her E2 object lower right."""
    for name in BURIDAN:
        p = problems[name]
        t = p.touched_object()
        assert t.id == "spoon1" and t.type == "spoon" and t.orientation == "up"
        assert TABLE_WIDTH / 3 < t.x < 2 * TABLE_WIDTH / 3 and t.y < TABLE_HEIGHT / 4, name
        e1, e2 = p.answer_object("E1"), p.answer_object("E2")
        assert e1.type == "spoon" and e1.x < TABLE_WIDTH / 3 and e1.y > 2 * TABLE_HEIGHT / 3
        assert e2.x > 2 * TABLE_WIDTH / 3 and e2.y > 2 * TABLE_HEIGHT / 3, name


def test_ids_follow_the_convention(problems):
    for p in problems.values():
        for o in p.objects:
            assert o.id.startswith(id_stem(o.type))
            assert (o.orientation in ORIENTATIONS) == (o.type in SILVERWARE)
            assert 0 <= o.x <= TABLE_WIDTH and 0 <= o.y <= TABLE_HEIGHT
        assert len(set(p.ids())) == len(p.ids())


def test_positions_are_the_centres_of_the_boxes(problems):
    p = problems["surround-05"]
    o = p.object("fork1")
    assert o.box_px == (29, 33, 33, 49) and p.frame_px == (9, 14, 203, 190)
    assert (o.x, o.y) == (12.5, 15.3)
    assert P.position_from_box(o.box_px, p.frame_px, 110, 100) == (12.5, 15.3)


def test_every_frame_has_the_same_scale(problems):
    """All 34 chapter 5 figures draw the table as a frame of 191-195 x 174-176 px, and the
    19 chapter 2 figures, a tenth larger, as 212-215 x 193-196 (D-001; both 1.10 wide)."""
    for name, p in problems.items():
        left, top, right, bottom = p.frame_px
        w, h = right - left - 1, bottom - top - 1
        if p.family == "ch2":
            assert 211 <= w <= 216 and 192 <= h <= 197, name
        else:
            assert 190 <= w <= 196 and 174 <= h <= 177, name
        assert 1.07 <= (right - left) / (bottom - top) <= 1.13, name


def shared_shift(base: P.Problem, other: P.Problem) -> dict[str, float]:
    return {o.id: distance(o.x, o.y, base.object(o.id).x, base.object(o.id).y)
            for o in other.objects if o.id in base.ids()}


def test_surround_6_swaps_the_cup_and_the_glass(problems):
    """'In Surround 6, the isolated cup and the small glass are switched.' [F95 p. 119]"""
    s5, s6 = problems["surround-05"], problems["surround-06"]
    assert s6.based_on == "surround-05" and sorted(s6.ids()) == sorted(s5.ids())
    assert s6.answers == {"E1": "cup2", "E2": "small-glass1"}
    glass5, cup5 = s5.object("small-glass1"), s5.object("cup2")
    assert distance(s6.object("cup2").x, s6.object("cup2").y, glass5.x, glass5.y) < TOLERANCE
    assert distance(s6.object("small-glass1").x, s6.object("small-glass1").y,
                    cup5.x, cup5.y) < TOLERANCE
    shifts = shared_shift(s5, s6)
    assert all(d < TOLERANCE for i, d in shifts.items() if i not in ("cup2", "small-glass1"))


@pytest.mark.parametrize("name, replacement, type_", [
    ("surround-07", "saucer1", "saucer"),        # "replaced by a saucer" [F95 p. 120]
    ("surround-08", "plate1", "plate"),          # "replaced in Surround 8 by a plate" [p. 120]
    ("surround-09", "salt-shaker1", "salt shaker"),  # "replaced by a saltshaker" [p. 121]
])
def test_surround_7_to_9_replace_the_glass_only(problems, name, replacement, type_):
    s5, p = problems["surround-05"], problems[name]
    assert p.based_on == "surround-05"
    assert sorted(set(s5.ids()) - set(p.ids())) == ["small-glass1"]
    assert sorted(set(p.ids()) - set(s5.ids())) == [replacement]
    new = p.object(replacement)
    glass = s5.object("small-glass1")
    assert new.type == type_ and distance(new.x, new.y, glass.x, glass.y) < TOLERANCE
    assert p.answers == {"E1": replacement, "E2": "cup2"}
    assert max(shared_shift(s5, p).values()) < TOLERANCE


def test_surround_10_is_surround_5_plus_five_junk_objects(problems):
    """'adding superfluous "junk" to a particular table configuration, in this case to
    Surround 5' [F95 p. 121]; the drawing shows five added objects [F95 p. 121 (img)]."""
    s5, s10 = problems["surround-05"], problems["surround-10"]
    assert s10.based_on == "surround-05" and set(s5.ids()) <= set(s10.ids())
    junk = [o for o in s10.objects if o.id not in s5.ids()]
    assert sorted(o.type for o in junk) == ["fork", "knife", "pepper shaker", "salt shaker",
                                            "small glass"]
    assert all(25 < o.y < 60 for o in junk), "the junk lies between the two rows"
    assert max(shared_shift(s5, s10).values()) < TOLERANCE
    assert s10.answers == s5.answers


def test_surround_1_to_4_grow_towards_5(problems):
    """Surround 1 has the three key objects; 2 adds a spoon beside each; 3 adds a knife and
    a fork; 4 has a pair of spoons beside each [F95 pp. 115-117]."""
    counts = {n: len(problems[f"surround-0{n}"].objects) for n in range(1, 6)}
    assert counts == {1: 3, 2: 7, 3: 8, 4: 7, 5: 13}
    s3 = problems["surround-03"]
    assert s3.object("knife1").x < s3.object("cup1").x < s3.object("spoon1").x
    s4 = problems["surround-04"]
    assert s4.object("cup1").x < s4.object("spoon1").x < s4.object("spoon2").x
    assert s4.object("spoon3").x < s4.object("spoon4").x < s4.object("small-glass1").x


def test_surround_5_agrees_with_the_chapter_4_screen_dump(problems):
    """The same thirteen objects on an independent image (Fig. 4.2 [F95 p. 98 (img)]),
    within the stated uncertainty (D-003: 1.6 units in x, 2.2 in y)."""
    ch4 = json.loads((PYTHON_DIR / "tests" / "data" / "ch4.json").read_text())["problem"]
    s5 = problems["surround-05"]
    assert sorted(o["id"] for o in ch4["objects"]) == sorted(s5.ids())
    assert ch4["touched"] == s5.touched and ch4["expected_answers"] == s5.answers
    for o in ch4["objects"]:
        mine = s5.object(o["id"])
        assert mine.type == o["type"]
        assert abs(o["x"] * TABLE_WIDTH - mine.x) <= 2.5, o["id"]
        assert abs(o["y"] * TABLE_HEIGHT - mine.y) <= 2.5, o["id"]


# ---------------------------------------------------------------------------------------------
# Blockage [F95 pp. 122-129].


def test_blockage_grows_as_the_text_says(problems):
    """Blockage 1: the cup and Eliza's glass; 2 adds a glass 'in the upper right-hand corner'
    [F95 p. 123]; 3 'one more small glass ... in the lower left-hand corner' [F95 p. 123];
    4 'a spoon ... next to the small glass in the upper right-hand corner' [F95 p. 125];
    5 the spoon 'replaced by a cup' [F95 p. 126]; 6 'two glasses in the upper right-hand
    corner' [F95 p. 126]; 7 adds 'the silverware' [F95 p. 127]; 8-10 are Blockage 6 with the
    upper-right glasses 'pulled further apart' [F95 p. 128]."""
    b = {n: problems[f"blockage-{n:02d}"] for n in range(1, 11)}
    assert {n: len(p.objects) for n, p in b.items()} == {1: 2, 2: 3, 3: 4, 4: 5, 5: 5, 6: 5,
                                                         7: 9, 8: 5, 9: 5, 10: 5}
    assert b[1].based_on is None and b[2].based_on == "blockage-01"
    assert sorted(set(b[2].ids()) - set(b[1].ids())) == ["small-glass2"]
    g2 = b[2].object("small-glass2")
    assert g2.x > 2 * TABLE_WIDTH / 3 and g2.y < TABLE_HEIGHT / 3
    assert sorted(set(b[3].ids()) - set(b[2].ids())) == ["small-glass3"]
    g1, g3 = b[3].object("small-glass1"), b[3].object("small-glass3")
    assert g1.x < g3.x < TABLE_WIDTH / 3 and abs(g1.y - g3.y) < 1 and g3.y > 2 * TABLE_HEIGHT / 3
    assert sorted(set(b[4].ids()) - set(b[3].ids())) == ["spoon1"]
    assert b[4].object("spoon1").x > b[4].object("small-glass2").x
    assert sorted(set(b[4].ids()) ^ set(b[5].ids())) == ["cup2", "spoon1"]
    assert b[5].object("cup2").x > b[5].object("small-glass2").x
    assert sorted(set(b[5].ids()) ^ set(b[6].ids())) == ["cup2", "small-glass4"]
    assert sorted(set(b[7].ids()) - set(b[6].ids())) == ["fork1", "fork2", "knife1", "knife2"]
    assert b[7].object("fork1").x < b[7].object("knife1").x < b[7].object("small-glass2").x
    assert b[7].object("small-glass3").x < b[7].object("fork2").x < b[7].object("knife2").x
    for n in (8, 9, 10):
        assert sorted(b[n].ids()) == sorted(b[6].ids())
    chain = {2: 1, 3: 2, 4: 3, 5: 4, 6: 5, 7: 6, 8: 6, 9: 8, 10: 9}
    for n, base in chain.items():
        assert b[n].based_on == f"blockage-{base:02d}"
        shifts = shared_shift(b[base], b[n])
        moved = {"small-glass2", "small-glass4"} if n >= 8 else set()
        assert all(d < TOLERANCE for i, d in shifts.items() if i not in moved), (n, shifts)


def gap_px(p: P.Problem) -> int:
    """The white space between the upper-right glasses' boxes, in pixels (D-003)."""
    a, b = p.object("small-glass2"), p.object("small-glass4")
    return b.box_px[0] - a.box_px[2] - 1


def test_blockage_gaps_in_order_and_near_french(problems):
    """'in Blockage 8, a third glass could be placed between the two glasses; in Blockage 9,
    two glasses could be placed between the original two glasses; and in Blockage 10, three
    glasses would fit in the gap' [F95 p. 128]. Measured: 1.3, 1.8 and 2.6 glass widths of
    8 px (D-002), each within the stated uncertainty of 2 units (0.44 widths) of French's
    one, two and three; the gaps grow by 4 and 7 px, not by a glass width of 8 each
    (`docs/problems.md` 8)."""
    files = {n: json.loads(P.problem_path(f"blockage-{n:02d}").read_text()) for n in (6, 8, 9, 10)}
    glass_width = footprint("small glass")[0]
    assert glass_width == 4.5
    widths = {}
    for n, data in files.items():
        gap = data["gap"]
        p = problems[f"blockage-{n:02d}"]
        assert gap["between"] == ["small-glass2", "small-glass4"]
        assert gap["px"] == gap_px(p)
        assert gap["units"] == round(gap["px"] * 110 / 194, 1)
        assert gap["glass_width_units"] == glass_width
        assert gap["glass_widths"] == round(gap["units"] / glass_width, 2)
        widths[n] = gap["glass_widths"]
    assert files[6]["gap"]["french"] is None and widths[6] < 1
    assert widths[6] < widths[8] < widths[9] < widths[10]
    uncertainty = problems["blockage-08"].uncertainty["units"] / glass_width
    for n, french in ((8, 1), (9, 2), (10, 3)):
        assert files[n]["gap"]["french"] == french
        assert abs(widths[n] - french) <= uncertainty, (n, widths[n])
    assert (widths[8], widths[9], widths[10]) == (1.27, 1.76, 2.64)


# ---------------------------------------------------------------------------------------------
# Buridan [F95 pp. 129-139] and Figure/Ground [F95 pp. 139-142].

BURIDAN_1_IDS = ["cup1", "cup2", "spoon1", "spoon2", "small-glass1", "small-glass2",
                 "cup3", "cup4", "fork1"]


def test_buridan_1_is_one_file_for_both_programs(problems):
    """'This configuration, identical to the previous one' [F95 p. 131]: the figure printed
    for the modified program [F95 p. 130 (img)] is the one of p. 129, so one file serves the
    two result sets `reported.BURIDAN` gives for `buridan-01`."""
    b1 = problems["buridan-01"]
    assert list(b1.ids()) == BURIDAN_1_IDS and b1.based_on is None
    assert [rs.program for rs in reported.BURIDAN if rs.problem == "buridan-01"] == \
        ["original", "modified"]
    assert any("p. 130" in note for note in b1.notes)
    cups_h = [b1.object("cup1"), b1.object("cup2")]
    assert cups_h[0].x < cups_h[1].x < b1.object("spoon1").x
    assert b1.object("spoon2").x < b1.object("small-glass1").x < b1.object("small-glass2").x
    assert b1.object("cup3").x < b1.object("cup4").x < b1.object("fork1").x


@pytest.mark.parametrize("name, removed, added, page", [
    # Buridan 2: the sidekicks 'not doubled' [F95 p. 131]: one of each kept.
    ("buridan-02", ["cup1", "cup3", "small-glass2"], {}, 131),
    # Buridan 3: 'two paired-cup groups' back, one glass [F95 p. 132].
    ("buridan-03", ["small-glass2"], {}, 132),
    # Buridan 4: 'one of the glasses ... replaced by a cup' [F95 p. 132].
    ("buridan-04", ["small-glass2"], {"cup5": "cup"}, 132),
    # Buridan 6: a glass 'replaced by a saltshaker' [F95 p. 134].
    ("buridan-06", ["small-glass1"], {"salt-shaker1": "salt shaker"}, 134),
    # Buridan 7: 'no objects at all next to the rival spoon' [F95 p. 135].
    ("buridan-07", ["small-glass1", "small-glass2"], {}, 135),
    # Buridan 8: two saltshakers, where Buridan 6 had 'a saltshaker and a small glass, instead
    # of two saltshakers' [F95 p. 136];
    # its figure is on p. 135, its table on p. 136.
    ("buridan-08", ["small-glass1", "small-glass2"],
     {"salt-shaker1": "salt shaker", "salt-shaker2": "salt shaker"}, 135),
    # Buridan 9: the fork a saltshaker [F95 p. 136].
    ("buridan-09", ["fork1"], {"salt-shaker1": "salt shaker"}, 136),
    # Buridan 13: plates beside the spoon, a saltshaker for the fork [F95 p. 139].
    ("buridan-13", ["small-glass1", "small-glass2", "fork1"],
     {"plate1": "plate", "plate2": "plate", "salt-shaker1": "salt shaker"}, 139),
])
def test_buridan_variations_differ_from_buridan_1_only_where_french_says(problems, name, removed,
                                                                         added, page):
    b1, p = problems["buridan-01"], problems[name]
    assert p.based_on == "buridan-01" and p.source["page"] == page
    assert sorted(set(b1.ids()) - set(p.ids())) == sorted(removed)
    assert {o.id: o.type for o in p.objects if o.id not in b1.ids()} == added
    shifts = shared_shift(b1, p)
    assert max(shifts.values()) < TOLERANCE, shifts
    for id_ in added:
        new = p.object(id_)
        assert new.y > 2 * TABLE_HEIGHT / 3, "every replacement is on Eliza's side"
        assert (new.x < TABLE_WIDTH / 3) == (id_ != p.answers["E2"])


def test_buridan_5_has_spoons_only_and_a_glass_pair(problems):
    """'spoons as the sole type of silverware' and 'the cup-cup and cup-glass groups' against
    'the cup-cup and glass-glass groups' [F95 p. 134]: Buridan 4 with the lower-right cups
    two glasses and the fork a spoon."""
    b4, b5 = problems["buridan-04"], problems["buridan-05"]
    assert b5.based_on == "buridan-04"
    assert sorted(set(b4.ids()) - set(b5.ids())) == ["cup3", "cup4", "fork1"]
    assert {o.id: o.type for o in b5.objects if o.id not in b4.ids()} == \
        {"small-glass3": "small glass", "small-glass4": "small glass", "spoon3": "spoon"}
    assert {o.type for o in b5.objects if o.type in SILVERWARE} == {"spoon"}
    assert b5.answers == {"E1": "spoon2", "E2": "spoon3"}
    assert max(shared_shift(b4, b5).values()) < TOLERANCE
    assert b5.object("small-glass3").x < b5.object("small-glass4").x < b5.object("spoon3").x


def test_buridan_10_to_12_build_up_from_the_bare_silverware(problems):
    """Buridan 10: 'The complete lack of any positional or grouping factors' [F95 p. 137];
    11 adds 'cups next to the touched spoon and the fork' [F95 p. 138]; 12 adds 'pairs of
    cups ... next to each piece of silverware' and keeps 'the small glass in the lower
    left-hand group' [F95 p. 138]."""
    b1, b10, b11, b12 = (problems[n] for n in ("buridan-01", "buridan-10", "buridan-11",
                                                "buridan-12"))
    assert b10.based_on == "buridan-01" and list(b10.ids()) == ["spoon1", "spoon2", "fork1"]
    assert {o.type for o in b10.objects} == {"spoon", "fork"}
    assert b11.based_on == "buridan-10"
    assert sorted(set(b11.ids()) - set(b10.ids())) == ["cup2", "cup4"]
    assert b11.object("cup2").x < b11.object("spoon1").x
    assert b11.object("cup4").x < b11.object("fork1").x
    assert max(shared_shift(b10, b11).values()) < TOLERANCE
    assert b12.based_on == "buridan-10"
    assert sorted(set(b12.ids()) - set(b10.ids())) == \
        ["cup1", "cup2", "cup3", "cup4", "cup5", "cup6", "small-glass1"]
    assert b12.object("spoon2").x < b12.object("cup5").x < b12.object("cup6").x \
        < b12.object("small-glass1").x < TABLE_WIDTH / 3
    # Henry's and the lower-right objects stand where Buridan 1 has them; the touched spoon
    # of Buridan 10 and 11 is drawn at the table's centre line instead (docs/problems.md 5).
    for id_ in ("cup1", "cup2", "spoon1", "cup3", "cup4", "fork1"):
        o, base = b12.object(id_), b1.object(id_)
        assert distance(o.x, o.y, base.x, base.y) < TOLERANCE, id_
    for p in (b10, b11):
        assert abs(p.object("spoon1").x - TABLE_WIDTH / 2) < 2


def test_figure_ground_1(problems):
    """'Henry reaches across the table and touches the "figure" object on Eliza's side of
    the table that echoes the "background material" on his side of the table—namely, the
    fork' [F95 p. 139]: two forks and a glass on Henry's side, two glasses and the touched
    fork on Eliza's, both groups at the centre of their sides [F95 p. 139 (img)]."""
    fg = problems["figure-ground-01"]
    assert list(fg.ids()) == ["fork1", "fork2", "small-glass1", "small-glass2", "small-glass3", "fork3"]
    t = fg.touched_object()
    assert t.id == "fork3" and t.y > 2 * TABLE_HEIGHT / 3, "the touched object is Eliza's"
    henry = [fg.object(i) for i in ("fork1", "fork2", "small-glass1")]
    eliza = [fg.object(i) for i in ("small-glass2", "small-glass3", "fork3")]
    assert all(o.y < TABLE_HEIGHT / 4 for o in henry) and all(o.y > 3 * TABLE_HEIGHT / 4 for o in eliza)
    assert henry[0].x < henry[1].x < henry[2].x and eliza[0].x < eliza[1].x < eliza[2].x
    for group in (henry, eliza):
        assert TABLE_WIDTH / 3 < sum(o.x for o in group) / 3 < 2 * TABLE_WIDTH / 3
    assert fg.answers == {"E1": "fork1", "E2": "fork2", "E3": "small-glass1"}
    assert all(o.orientation == "up" for o in fg.objects if o.type == "fork")


def test_the_footprints(problems):
    assert set(FOOTPRINTS_PX) == set(TYPES) and len(TYPES) == 11
    assert footprint("fork") == (3.4, 9.6) and footprint("fork", "left") == (9.6, 3.4)
    assert footprint("cup") == (7.9, 4.0) and footprint("cup", "left") == (7.9, 4.0)
    assert footprint("plate")[0] == footprint("plate")[1]
    # Every box in the problem files is within two pixels (three in height) of its type's
    # footprint scaled to the figure's frame (chapter 2's figures are a tenth larger than
    # chapter 5's, D-001); the chapter 5 figures draw a glass 6-8 px wide and a shaker
    # 12-13 x 15-16, the chapter 2 figures a cup 15-16 x 7-9 and a shaker 13-15 x 17-18.
    for p in problems.values():
        scale = (p.frame_px[2] - p.frame_px[0]) / 194 if p.family == "ch2" else 1.0
        for o in p.objects:
            w, h = o.box_px[2] - o.box_px[0] + 1, o.box_px[3] - o.box_px[1] + 1
            fw, fh = FOOTPRINTS_PX[o.type]
            assert abs(w - fw * scale) <= 2 and abs(h - fh * scale) <= 3, (p.name, o.id, w, h)


def test_the_table(problems):
    assert (TABLE_WIDTH, TABLE_HEIGHT) == (110.0, 100.0)
    assert distance(0, 0, 3, 4) == 5.0
    for p in problems.values():
        assert (p.width, p.height) == (110.0, 100.0)


def good() -> dict:
    return json.loads(P.problem_path("surround-01").read_text())


def test_validate_catches_faults():
    assert P.validate(good()) == []
    bad = good()
    bad["objects"][0]["type"] = "teapot"
    assert any("unknown type" in e for e in P.validate(bad))
    bad = good()
    bad["objects"][0]["orientation"] = "up"  # a cup
    assert any("only silverware" in e for e in P.validate(bad))
    bad = good()
    bad["objects"][1]["id"] = "cup1"
    assert any("duplicate id" in e for e in P.validate(bad))
    bad = good()
    bad["answers"]["E3"] = "spoon9"
    assert any("not an object" in e for e in P.validate(bad))
    bad = good()
    bad["objects"][0]["x"] = 150
    assert any("off the table" in e for e in P.validate(bad))
    bad = good()
    bad["objects"][0]["x"] += 1
    assert any("not the centre of its box" in e for e in P.validate(bad))
    bad = good()
    del bad["objects"][0]["page"]
    assert any("missing key 'page'" in e for e in P.validate(bad))
    bad = good()
    bad["table"]["width"] = 100
    assert any("D-001" in e for e in P.validate(bad))
    bad = good()
    bad["touched"] = "cup9"
    assert any("touched" in e for e in P.validate(bad))
    bad = good()
    del bad["review"]
    assert P.validate(bad) == ["missing key 'review'"]
    with pytest.raises(FileNotFoundError):
        P.load_problem("no-such-problem")


def test_load_problem_refuses_a_faulty_file(tmp_path):
    bad = good()
    bad["objects"][0]["type"] = "teapot"
    (tmp_path / "surround-01.json").write_text(json.dumps(bad))
    with pytest.raises(ValueError, match="unknown type"):
        P.load_problem("surround-01", tmp_path)
    renamed = copy.deepcopy(good())
    (tmp_path / "other.json").write_text(json.dumps(renamed))
    with pytest.raises(ValueError, match="file's name"):
        P.load_problem("other", tmp_path)
    assert P.catalogue(tmp_path) == ["other", "surround-01"]
