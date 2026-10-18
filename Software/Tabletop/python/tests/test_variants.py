"""The variants bench (D-102): the eight variants as transformations of a problem file (a
mirror of a mirror is the identity, the side swap and the rotation are involutions, the
substitution keeps D-009's categories and takes every object of a type, the decoupled
variant keeps the row's composition and moves it off the mirror image, the saucer holds
the cup's centre, every variant passes `problems.validate`), the seeds of D-072 shared by
a problem and its variants, the measures on known inputs, a small fast sample of the
bench, the v1 records' completeness, and `tools/run_on_baseline.py` against HEAD. The
full run is bench-only (`results/v1/variants.json` was made by the tool from the baseline
commit)."""

import importlib.util
import json
import re
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import objects as O
from tabletop import slipnet_def as SD
from tabletop.bench import __main__ as CLI
from tabletop.bench import harness as H
from tabletop.bench import results as RS
from tabletop.bench import variants as V
from tabletop.config import PROFILES
from tabletop.geometry import opposite_point
from tabletop.problems import catalogue, load_problem, validate
from tabletop.rng import Rng, run_seeds

PYTHON_DIR = Path(__file__).resolve().parents[1]
TOOL = PYTHON_DIR / "tools" / "run_on_baseline.py"
V1_FILE = PYTHON_DIR / "results" / "v1" / "variants.json"
CFG = PROFILES["standard"]


def normalised(data: dict) -> list:
    """The objects with their positions recomputed from their boxes, as every variant
    writes them."""
    return V._placed(data, data["objects"])["objects"]


# --- the transformations ------------------------------------------------------------------

@pytest.mark.parametrize("name", ["surround-05", "blockage-02", "buridan-01", "ch2-fig13"])
def test_a_mirror_of_a_mirror_is_the_identity(name):
    data = V.problem_data(name)
    once = V.mirrored(data)[0]
    twice = V.mirrored(once)[0]
    assert twice["objects"] == normalised(data)
    assert once["objects"] != normalised(data)
    o, m = data["objects"][0], once["objects"][0]
    assert m["x"] == pytest.approx(data["table"]["width"] - o["x"], abs=0.1) and m["y"] == o["y"]


@pytest.mark.parametrize("name", ["surround-05", "blockage-02", "buridan-01", "ch2-fig13"])
def test_the_side_swap_and_the_rotation_are_involutions(name):
    data = V.problem_data(name)
    for maker in (V.side_swapped, V.rotated):
        once = maker(data)[0]
        assert maker(once)[0]["objects"] == normalised(data)
        assert once["objects"] != normalised(data)
    swapped = V.side_swapped(data)[0]["objects"][0]
    assert swapped["y"] == pytest.approx(data["table"]["height"] - data["objects"][0]["y"], abs=0.1)
    # the rotation is the mirror of the side swap
    assert V.rotated(data)[0]["objects"] == V.mirrored(V.side_swapped(data)[0])[0]["objects"]
    # silverware turns round with its axis
    fork = next(o for o in V.side_swapped(V.problem_data("surround-05"))[0]["objects"]
                if o["type"] == "fork")
    assert fork["orientation"] == "down"


def test_substitutions_keep_d009_categories_and_apply_to_every_object_of_a_type():
    for a, b in V.PARTNERS:
        ca, cb = SD.categories_of(SD.node_for_type(a)), SD.categories_of(SD.node_for_type(b))
        assert ca[0] == cb[0], (a, b)                      # the nearest category is shared
        assert len(set(ca) & set(cb)) >= 2, (a, b)
    partners = V.partner_types()
    assert partners["fork"] == "spoon" and partners["spoon"] == "fork"
    v = V.variant_of("surround-05", "type-substituted")
    original = {o["id"]: o for o in V.problem_data("surround-05")["objects"]}
    assert set(v.mapping) == set(original)
    for old, new in v.mapping.items():
        o = v.data["objects"][[x["id"] for x in v.data["objects"]].index(new)]
        assert o["type"] == partners.get(original[old]["type"], original[old]["type"])
        assert (o["x"], o["y"], o["box_px"]) == (original[old]["x"], original[old]["y"],
                                                 original[old]["box_px"])
    assert v.mapping["small-glass1"] == "big-glass1" and v.mapping["fork3"] == "spoon3"
    assert v.mapping["knife1"] == "knife1"
    assert v.data["answers"] == {"E1": "big-glass1", "E2": "soup-bowl2"}
    assert v.data["touched"] == "soup-bowl1" and v.back("soup-bowl1") == "cup1"
    assert v.params["pairs"] == [["big glass", "small glass"], ["cup", "soup bowl"], ["fork", "spoon"]]
    named = next(n for n in catalogue() if n.startswith("ch2")
                 and V.problem_data(n)["expected"]["answer"] is not None)
    ch2 = V.variant_of(named, "type-substituted")
    exp, before = ch2.data["expected"], V.problem_data(named)["expected"]
    assert exp["answer"] == ch2.mapping[before["answer"]]
    assert exp["plausible"] == [ch2.mapping[i] for i in before["plausible"]]
    assert [c["toward"] for c in exp["comparisons"]] == [ch2.mapping[c["toward"]]
                                                        for c in before["comparisons"]]


def test_the_decoupled_variant_keeps_the_rows_composition_and_moves_it_off_the_mirror_image():
    data = V.problem_data("surround-05")
    v = V.variant_of("surround-05", "decoupled")
    moved = v.params["moved"]
    assert sorted(moved) == ["fork3", "fork4", "knife2", "small-glass1", "spoon3", "spoon4"]
    assert "cup2" not in moved, "the isolated cup is not part of the structured row"
    before = {o["id"]: o for o in data["objects"]}
    after = {o["id"]: o for o in v.data["objects"]}
    shift = v.params["shift_units"]
    assert abs(shift) == pytest.approx(data["table"]["width"] / 3, abs=0.6)
    for id_ in moved:
        assert after[id_]["x"] - before[id_]["x"] == pytest.approx(shift, abs=0.15)
        assert after[id_]["y"] == pytest.approx(before[id_]["y"], abs=0.1)
    for id_ in before:
        if id_ not in moved:
            assert (after[id_]["x"], after[id_]["y"]) == (before[id_]["x"], before[id_]["y"])
    touched = before[data["touched"]]
    mx, my = opposite_point(touched["x"], touched["y"])
    nearest_before = min(moved, key=lambda i: (before[i]["x"] - mx) ** 2 + (before[i]["y"] - my) ** 2)
    assert nearest_before == "small-glass1"
    assert ((before["small-glass1"]["x"] - mx) ** 2 + (before["small-glass1"]["y"] - my) ** 2) ** 0.5 < 15
    assert ((after["small-glass1"]["x"] - mx) ** 2 + (after["small-glass1"]["y"] - my) ** 2) ** 0.5 > 30
    assert V.variant_of("ch2-fig02", "decoupled") is None     # one object: no other row


def test_on_saucer_puts_a_cups_centre_inside_a_saucers_footprint():
    v = V.variant_of("surround-05", "on-saucer")
    assert v.params == {"cup": "cup2", "saucer": "saucer1"} and v.added == ("saucer1",)
    ids = [o["id"] for o in v.data["objects"]]
    assert ids.index("saucer1") == ids.index("cup2") + 1
    cup = next(o for o in v.data["objects"] if o["id"] == "cup2")
    saucer = next(o for o in v.data["objects"] if o["id"] == "saucer1")
    left, top, right, bottom = O.box(V._Obj(saucer))
    assert left < cup["x"] < right and top < cup["y"] < bottom
    assert v.data["touched"] == "cup1" and v.back("saucer1") == "saucer1"
    assert "added by the variants bench" in saucer["image"]
    assert V.variant_of("figure-ground-01", "on-saucer") is None
    assert V.variant_of("buridan-10", "on-saucer") is None


def test_translated_shifts_by_half_a_glass_width_where_there_is_room():
    v = V.variant_of("surround-05", "translated")
    assert abs(v.params["dx_units"]) == pytest.approx(O.glass_width() / 2, abs=0.4)
    assert abs(v.params["dy_units"]) == pytest.approx(O.glass_width() / 2, abs=0.4)
    before = {o["id"]: o for o in V.problem_data("surround-05")["objects"]}
    for o in v.data["objects"]:
        assert o["x"] - before[o["id"]]["x"] == pytest.approx(v.params["dx_units"], abs=0.15)
        assert o["y"] - before[o["id"]]["y"] == pytest.approx(v.params["dy_units"], abs=0.15)


def test_junk_added_stands_clear_in_the_band_or_a_corner():
    data = V.problem_data("surround-05")
    v = V.variant_of("surround-05", "junk-added")
    assert v.added == ("salt-shaker1",) and v.params["where"] == "band"
    stray = v.data["objects"][-1]
    assert stray["type"] == V.JUNK_TYPE and stray["y"] == pytest.approx(50.0, abs=1.0)
    assert V._clearance(stray["type"], stray["x"], stray["y"], data) >= V.JUNK_CLEARANCE
    assert [o["id"] for o in v.data["objects"][:-1]] == [o["id"] for o in data["objects"]]
    assert v.mapping == {o["id"]: o["id"] for o in data["objects"]}


def test_every_variant_passes_validate_and_is_named_after_its_problem():
    count = 0
    for name in catalogue():
        data = V.problem_data(name)
        for variant in V.VARIANTS:
            v = V.variant_of(name, variant, data)
            if v is None:
                continue
            count += 1
            assert validate(v.data) == [], (name, variant)
            problem = v.to_problem()
            assert problem.name == f"{name}+{variant}" and problem.family == data["family"]
            assert problem.touched == v.mapping[data["touched"]]
            assert set(v.mapping) == {o["id"] for o in data["objects"]}
            assert len(problem.objects) == len(data["objects"]) + len(v.added)
            assert all(a in problem.ids() for a in v.added)
    assert count == 53 * 8 - 3
    assert V.planned() == 53 + count


# --- the seeds and the measures -------------------------------------------------------------

def test_the_seeds_follow_d072_and_a_variant_is_paired_with_its_original():
    [pv] = V.run(CFG, 5, 2, ["blockage-01"])
    batch = H.batch_seed_for(5, "blockage-01")
    assert pv.survey.batch_seed == batch and pv.survey.seed == 5
    seeds = run_seeds(batch, 2)
    assert [r[0] for r in pv.survey.results] == seeds
    for vr in pv.variants:
        assert vr.survey.batch_seed == batch and [r[0] for r in vr.survey.results] == seeds
        assert vr.survey.problem == f"blockage-01+{vr.variant.name}"
    again = V.run(CFG, 5, 2, ["blockage-01"])[0]
    assert V.records([again]) == V.records([pv])


def test_the_measures_on_known_inputs():
    assert V.total_variation({"a": 3, "b": 1}, {"a": 1, "b": 3}) == pytest.approx(0.5)
    assert V.total_variation({"a": 2}, {"a": 5}) == 0.0
    assert V.total_variation({"a": 2}, {"b": 5}) == 1.0
    lo, hi = V.bootstrap_interval(["a"] * 10, ["a"] * 10, Rng(1), samples=50)
    assert lo == hi == 0.0
    lo, hi = V.bootstrap_interval(["a", "b"] * 10, ["a", "b", "b", "b"] * 5, Rng(1), samples=100)
    assert 0.0 <= lo <= hi <= 1.0 and hi > 0.0
    labels = {"E1": "small-glass1", "E2": "cup2"}
    assert V.role_of("cup2", labels) == "E2" and V.role_of("fork1", labels) == "fork1"
    assert V.role_kept(["cup2", "fork1", "cup1"], ["cup2", "fork1", "cup2"], labels) == pytest.approx(66.7)
    assert V.answer_counts(["b", "a", "b"]) == {"b": 2, "a": 1}
    assert list(V.answer_counts(["b", "a", "b"])) == ["b", "a"]


def test_a_small_sample_of_the_bench_reports_and_records():
    results = V.run(CFG, 1, 2, ["blockage-01", "ch2-fig06"])
    assert [pv.problem.name for pv in results] == ["blockage-01", "ch2-fig06"]
    for pv in results:
        assert sum(pv.counts.values()) == 2 and pv.survey.runs == 2
        assert set(pv.mechanism) == {"wv_mean", "wv2", "coherent", "incoherent", "tags"}
        assert pv.mechanism["tags"] >= 0.0          # COLUMN tags at first sight (D-085)
        for vr in pv.variants:
            assert 0.0 <= vr.tv <= 1.0 and 0.0 <= vr.interval[0] <= vr.interval[1] <= 1.0
            assert 0.0 <= vr.p <= 1.0 and 0.0 <= vr.kept <= 100.0
            assert sum(vr.counts.values()) == 2
            assert all(vr.variant.back(a) == a for a in vr.counts)   # written back to the original's ids
    lines = V.report(results)
    assert lines[0].startswith("## The variants bench: eight variants of every problem")
    assert any(l.startswith("| mirrored | 2 |") for l in lines)
    assert any(l.startswith("**blockage-01**: 2 runs, main answer") for l in lines)
    recs = V.records(results)
    json.dumps(recs)
    assert [r["kind"] for r in recs][:9] == ["original"] + ["variant"] * 8
    assert all(r["applies"] for r in recs if r["kind"] == "variant")
    stripped = [RS.strip_record(r) for r in recs]
    assert "results" not in stripped[0] and stripped[0]["results_count"] == 2
    # the same schema as the committed v1 records
    committed = RS.read_records(V1_FILE)
    assert set(stripped[0]) == set(next(r for r in committed if r["kind"] == "original"))
    assert set(stripped[1]) == set(next(r for r in committed if r["kind"] == "variant" and r["applies"]))


def test_the_v1_and_roles_records_are_complete_at_500_runs():
    for path in (V1_FILE, PYTHON_DIR / "results" / "variants.json"):
        records_complete(path)


def records_complete(path):
    records = RS.read_records(path)
    originals = [r for r in records if r["kind"] == "original"]
    variants = [r for r in records if r["kind"] == "variant"]
    assert [r["problem"] for r in originals] == catalogue()
    assert all(r["runs"] == 500 and r["seed"] == 1 and r["profile"] == "standard" for r in originals)
    assert all(r["batch_seed"] == H.batch_seed_for(1, r["problem"]) for r in originals + variants
               if r.get("applies", True))
    assert len(variants) == 53 * 8
    skipped = sorted((r["problem"], r["variant"]) for r in variants if not r["applies"])
    assert skipped == [("buridan-10", "on-saucer"), ("ch2-fig02", "decoupled"),
                       ("figure-ground-01", "on-saucer")]
    assert all(r["runs"] == 500 and "results" not in r and r["results_count"] == 500
               for r in variants if r["applies"])
    if path == V1_FILE:
        assert all(r["mechanism"]["tags"] == 0.0 for r in originals), "v1 has no role tags"
    else:
        assert all(r["mechanism"]["tags"] > 0.0 for r in originals), "the roles engine tags"
    assert path.stat().st_size < 290_000


def test_the_bench_is_registered_and_the_results_block_reads_the_v1_column():
    assert "variants" in CLI.BENCHES and "variants" in RS.BENCHES and "variants" in RS.BLOCKS
    assert RS.V1_FIRST == () and "variants" in RS.V1_BLOCKS
    res = RS.Results(PYTHON_DIR / "results")
    lines = RS.variants_tables(res)
    assert not lines[0].startswith("(no ")
    assert any(re.match(r"\| mirrored \| 53 \| 53 \| [0-9.]+ \| [0-9.]+ \|", l) for l in lines)
    assert any(l.startswith("| decoupled | surround | 10 | ") for l in lines)
    assert any(l.startswith("| v1 | 53 | 500 |") for l in lines)
    assert any(l.startswith("| roles | 53 | 500 |") for l in lines)
    assert RS.variants_tables(RS.Results(PYTHON_DIR / "results", with_v1=False))[0] == "(no v1 variants records yet)"


# --- tools/run_on_baseline.py ---------------------------------------------------------------

def load_tool():
    spec = importlib.util.spec_from_file_location("run_on_baseline", TOOL)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_the_tool_refuses_a_missing_bench_and_a_worktree_inside_the_repository(tmp_path):
    tool = load_tool()
    assert tool.main(["HEAD", str(tmp_path / "wt"), "no-such-bench"]) == tool.USAGE_ERROR
    assert tool.main(["HEAD", str(tmp_path / "wt"), "variants", "--runs", "0"]) == tool.USAGE_ERROR
    top = tool.top_level()
    if top is None:
        pytest.skip("not inside a git repository (the standalone copy)")
    assert tool.main(["HEAD", str(top / "inside"), "variants"]) == tool.USAGE_ERROR
    assert tool.package_dir(Path("/tmp/wt"), top) == Path("/tmp/wt") / PYTHON_DIR.relative_to(top)


def worktrees_named(top: Path, text: str) -> list:
    out = subprocess.run(["git", "-C", str(top), "worktree", "list", "--porcelain"],
                         capture_output=True, text=True).stdout
    return [l for l in out.splitlines() if l.startswith("worktree ") and text in l]
