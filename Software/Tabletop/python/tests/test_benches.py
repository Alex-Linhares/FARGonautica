"""The eight benches of item 23 (D-073) and the `all` command, at a fast sample size (one
to three runs on one or two problems): the formats, the readings they make of French's
data, and the probe variants. No bench is run at its full size here (ANSWERS.md 1: before
the freeze the benches are run only in these small format checks)."""

import dataclasses
import json
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel

from tabletop import events as EV
from tabletop import reported as RP
from tabletop.bench import __main__ as CLI
from tabletop.bench import ch2, ch4, churn, faults, focus, humans, lesions, rho, temperature
from tabletop.bench import robustness
from tabletop.bench import harness as H
from tabletop.bench import potelbat as PBB
from tabletop.bench import probes as P
from tabletop.config import FAULT_KNOBS, PROFILES
from tabletop.objects import edge_gap
from tabletop.problems import by_family, catalogue, load_problem, validate
from tabletop.run import run_problem
from v2_defaults import V2

PYTHON_DIR = Path(__file__).resolve().parent.parent
CFG = V2              # the benches' mechanics at the decisions' values (D-114)
ALL_BENCHES = (ch2, ch4, faults, focus, humans, lesions, rho, PBB, temperature, robustness, churn)


def run_cli(*args):
    return subprocess.run([sys.executable, "-m", "tabletop.bench", *args], cwd=PYTHON_DIR,
                          capture_output=True, text=True)


# --- the protocol every bench follows ---------------------------------------------------

def test_every_bench_offers_the_same_names_and_is_listed():
    for module in ALL_BENCHES:
        for name in ("SETS", "problems_of", "planned", "run", "report", "records"):
            assert hasattr(module, name), (module.__name__, name)
    listed = [m for m, _ in CLI.BENCHES.values()]
    for module in ALL_BENCHES:
        assert module in listed
    assert list(CLI.BENCHES) == ["families", "fcca", "humans", "ch2", "ch4", "focus", "rho",
                                 "lesions", "faults", "potelbat", "stated", "temperature",
                                 "robustness", "variants", "churn"]


# --- probes: variants, readings, Spearman ------------------------------------------------

def test_a_variant_keeps_the_profile_name_and_records_its_changes():
    v = P.variant(CFG, "rho 2", "a note", rho=2.0)
    assert v.config.profile == "standard" and v.config.rho == 2.0
    assert v.changes == (("rho", "2.0"),) and v.note == "a note"
    assert P.intact(CFG).config is CFG and P.intact(CFG).changes == ()


def test_survey_readings_sum_as_they_should():
    pb = load_problem("surround-05")
    sv = H.survey(pb, CFG, 1, 3)
    total = sum(t.frequency for t in sv.tallies)
    assert total == pytest.approx(100.0)
    assert P.frequency(sv, pb.ids()) == pytest.approx(100.0)
    assert P.label_frequency(sv, pb, "E1+E2") == pytest.approx(
        P.label_frequency(sv, pb, "E1") + P.label_frequency(sv, pb, "E2"))
    assert 0.0 <= P.literal_frequency(sv, pb) <= 100.0
    assert P.main_tally(sv).count == max(t.count for t in sv.tallies)
    assert P.mean_structure(sv) == pytest.approx(sum(r[2] for r in sv.results) / 3)


def test_ranks_and_spearman_on_known_inputs():
    assert P.ranks([10, 30, 20]) == [1.0, 3.0, 2.0]
    assert P.ranks([5, 5, 1]) == [2.5, 2.5, 1.0]
    assert P.spearman([1, 2, 3, 4], [10, 20, 30, 40]) == pytest.approx(1.0)
    assert P.spearman([1, 2, 3, 4], [40, 30, 20, 10]) == pytest.approx(-1.0)
    assert P.spearman([1, 2, 3], [2, 2, 2]) is None
    assert P.spearman([1], [1]) is None
    # one tie on each side, by hand: ranks x (1, 2.5, 2.5, 4), y (1, 2, 3, 4)
    assert P.spearman([1, 2, 2, 3], [1, 2, 3, 4]) == pytest.approx(0.9486832980505138)
    with pytest.raises(ValueError):
        P.spearman([1, 2], [1])


def test_french_main_reads_the_first_result_set():
    assert P.french_main("surround-05").label == "E1"
    assert P.french_main("blockage-06").label == "E3"
    assert P.french_main("ch2-fig05") is None


# --- rho, lesions, faults: the variants and one table --------------------------------------

def test_rho_variants_cover_french_values_and_mark_the_profile_own():
    vs = rho.variants(CFG)
    assert [v.config.rho for v in vs] == list(rho.RHOS) == [0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0]
    assert sum("own value" in v.note for v in vs) == 1
    assert rho.planned(rho.SETS, ["surround-05"]) == 7


def test_lesions_are_fcca_criterion_11_each_changing_only_its_knobs():
    vs = lesions.variants(CFG)
    assert vs[0].name == "intact" and vs[0].config == CFG
    names = [v.name for v in vs[1:]]
    assert names == ["temperature-high", "temperature-low", "rho-1", "losers-destroyed",
                     "flat-depths", "fixed-lengths", "context-off"]
    for v, (_, changes, what, _) in zip(vs[1:], lesions.LESIONS):
        diff = CFG.differences(v.config)
        assert set(diff) == set(changes), v.name
        assert '[FCCA p. 365]' in what or '[F95' in what
    assert vs[1].config.temperature_fixed == 100.0 and vs[2].config.temperature_fixed == 0.0
    assert vs[5].config.depth_by_level == (50, 50, 50) and vs[6].config.label_shrink == 0.0
    assert vs[7].config.context_mechanism is False


def test_faults_variants_restore_one_fault_each_then_all():
    vs = faults.variants(CFG)
    assert vs[0].name == "fixed" and vs[0].config == CFG
    assert len(vs) == 2 + len(FAULT_KNOBS) == 9
    for v, (field, value) in zip(vs[1:-1], FAULT_KNOBS):
        assert CFG.differences(v.config) == {field: getattr(CFG, field)}
        assert getattr(v.config, field) == value
        assert field in faults.FAULT_NOTES and "D-0" in v.note
    assert vs[-1].name == "all-faults" and faults.all_faults_is_french_faults()
    assert set(faults.SETS) <= set(catalogue())


def test_a_variant_table_has_a_row_per_variant_and_the_json_replays():
    vs = [P.intact(CFG), P.variant(CFG, "rho 1", "", rho=1.0)]
    surveys = P.survey_variants(vs, ["surround-05"], 1, 2)
    assert set(surveys) == {("intact", "surround-05"), ("rho 1", "surround-05")}
    lines = P.variant_table("surround-05", vs, surveys)
    assert lines[0].startswith("### surround-05 (French's main answer E1, 86 percent [F95 p. 118])")
    rows = [l for l in lines if l.startswith("| intact") or l.startswith("| rho 1")]
    assert len(rows) == 2 and all(l.count("|") == 9 for l in rows)
    records = P.variant_records("rho", vs, ["surround-05"], surveys)
    assert [r["variant"] for r in records] == ["intact", "rho 1"]
    assert records[1]["changes"] == [["rho", "1.0"]]
    seed, answer, structure, codelets, capped = records[0]["survey"]["results"][0]
    from tabletop.run import run_problem
    r = run_problem(load_problem("surround-05"), CFG, seed)
    assert (r.answer, r.structure_value, r.codelets, r.capped) == (answer, structure, codelets, capped)


def test_lesion_verdicts_read_the_expectations_on_known_numbers():
    intact = {"literal": 10.0, "french_main": 70.0, "codelets": 50.0, "capped": 0}
    low = {"literal": 100.0, "french_main": 0.0, "codelets": 2.0, "capped": 0}
    assert lesions.verdict("temperature-low", intact, low, 500) == "as predicted"
    assert lesions.verdict("temperature-low", intact, dict(intact), 500) == "no change"
    # the literal share going down where the expectation says up
    assert lesions.verdict("temperature-low", intact, {**low, "literal": 2.0}, 500) == "against"
    high = {"literal": 12.0, "french_main": 60.0, "codelets": 3000.0, "capped": 500}
    assert lesions.verdict("temperature-high", intact, high, 500) == "as predicted"
    # a lesion expected to change nothing: within noise is as predicted, a move is named
    assert lesions.verdict("context-off", intact, {**intact, "literal": 11.0}, 500) == "as predicted"
    assert lesions.verdict("context-off", intact, {**intact, "literal": 20.0}, 500) == "moved: literal"
    assert set(lesions.READINGS) == {name for name, _, _, _ in lesions.LESIONS}
    assert lesions.verdict("rho-1", intact, {**intact, "literal": 3.0}, 500) == "as predicted"


# --- temperature: the curves -------------------------------------------------------------------

class _Result:
    def __init__(self, codelets):
        self.codelets = codelets


def test_a_temperature_path_reads_its_updates():
    updates = ((0, 80.0, True, "start", 0.05, 0.0, 0, 0), (5, 80.0, True, "schedule", 0.05, 0.0, 0, 0),
               (7, 96.0, False, "change", 0.6, 0.09, 1, 1), (10, 70.0, False, "schedule", 0.6, 0.06, 1, 1))
    path = temperature.Path(3, updates, _Result(12))
    assert [path.at(c) for c in (0, 4, 5, 6, 7, 9, 11)] == [80.0, 80.0, 80.0, 80.0, 96.0, 96.0, 70.0]
    assert path.at(12) is None and path.at(100) is None
    assert path.first_change() == (7, 96.0) and path.end() == 70.0 and path.lowest() == 70.0
    assert path.counts() == {"start": 1, "schedule": 2, "change": 1}
    terms = path.terms(CFG)
    assert len(terms) == 2 and terms[0] == (CFG.temperature_alpha * 0.09 / 0.6, CFG.temperature_beta, CFG.temperature_gamma)
    held = temperature.Path(4, updates[:2], _Result(6))
    assert held.first_change() is None and held.end() == 80.0
    rows = temperature.curve([path, held], (0, 7, 10))
    assert rows[0] == (0, 80.0, 80.0, 80.0, 80.0, 100.0)
    assert rows[1][1] == 96.0 and rows[1][5] == 50.0        # the held run ended at 6
    s = temperature.summary([path, held], CFG)
    assert s["no_change_share"] == 50.0 and s["after_first_mean"] == 96.0
    assert s["end_mean"] == 75.0 and s["first_change_median"] == 7


def test_temperature_at_sample_size_shows_the_dumps_beside_the_curve():
    results = temperature.run(CFG, 1, 3, ["surround-05"])
    assert temperature.planned(temperature.SETS, ["surround-05"]) == 3
    assert [v.name for v in results["variants"]] == ["intact", "dq-signed", "alpha-100"]
    paths = results["paths"][("intact", "surround-05")]
    assert len(paths) == 3 and all(p.updates[0][:4] == (0, 80.0, True, "start") for p in paths)
    assert [p.result.seed for p in paths] == [r[0] for r in results["surveys"][("intact", "surround-05")].results]
    lines = temperature.report(results)
    assert lines[0].startswith("## The course of the temperature")
    assert any(l.startswith("| T, French's run") and "59.0" in l and "2.0" in l for l in lines)
    assert any(l.startswith("| Codelets run | 2 | 12 | 22 | 33 | 36 | 47 | 55 |") for l in lines)
    records = temperature.records(results)
    assert len(records) == 3 and all(r["bench"] == "temperature" for r in records)
    assert all(len(r["paths"]) == 3 and len(r["curve"]) == len(temperature.CLOCKS) for r in records)
    assert all("dumps" in r and r["dumps"][2]["french_t"] == 59.0 for r in records)
    assert records[0]["curve"][0][1:] == [80.0, 80.0, 80.0, 80.0, 100.0]
    other = temperature.run(CFG, 1, 1, ["blockage-02"])
    assert "dumps" not in temperature.records(other)[0]
    assert not any(l.startswith("| T, French's run") for l in temperature.report(other))


# --- focus: Surround 5 against 10 and the junk ---------------------------------------------

def test_junk_variants_add_the_count_asked_in_the_band_without_overlap():
    base = load_problem("surround-05")
    assert focus.junk_variant(base, 0, 1) is base
    for count in focus.JUNK_COUNTS:
        v = focus.junk_variant(base, count, 1)
        assert len(v.objects) == len(base.objects) + count
        assert v.name == (f"surround-05+junk{count}" if count else "surround-05")
        junk = v.objects[len(base.objects):]
        for j in junk:
            assert focus.BAND[0] - focus.JITTER <= j.y <= focus.BAND[1] + focus.JITTER
            assert j.provenance.figure == "synthetic"
            assert j.orientation == ("up" if j.type in ("fork", "knife", "spoon") else None)
        others = list(v.objects)
        for j in junk:
            for o in others:
                if o is not j:
                    assert edge_gap(j, o) >= focus.MARGIN
    assert [j.type for j in focus.junk_variant(base, 5, 1).objects[13:]] == list(focus.JUNK_TYPES[:5])
    # the same seed, the same table; another seed, another jitter
    assert focus.junk_variant(base, 4, 1) == focus.junk_variant(base, 4, 1)
    assert focus.junk_variant(base, 4, 1) != focus.junk_variant(base, 4, 2)


def test_surround_10_junk_types_lead_the_list():
    """Surround 10's junk: a pepper shaker, a salt shaker, a fork, a knife and a glass
    [F95 p. 121 (img)]."""
    junk = [o.type for o in load_problem("surround-10").objects[13:]]
    assert sorted(junk) == sorted(focus.JUNK_TYPES[:5])


def test_focus_at_sample_size_reports_french_ratios():
    results = focus.run(CFG, 1, 1)
    lines = focus.report(results)
    assert lines[0] == "## Surround 5 against Surround 10: the junk [F95 p. 121]"
    assert any("| Surround 5 [F95 p. 118] | 86 |" in l for l in lines)
    assert any("| Surround 10 [F95 p. 121] | 73 |" in l for l in lines)
    assert any(l.startswith("E1 drop from Surround 5 to 10: French 13 points") for l in lines)
    assert any("Run length ratio, Surround 10 over 5: French 0.85" in l for l in lines)
    scaling = [l for l in lines if l.startswith("| ") and l.split("|")[1].strip().isdigit()]
    assert [int(l.split("|")[1]) for l in scaling] == list(focus.JUNK_COUNTS)
    assert [int(l.split("|")[2]) for l in scaling] == [13 + k for k in focus.JUNK_COUNTS]
    records = focus.records(results)
    assert len(records) == focus.planned() == 2 + len(focus.JUNK_COUNTS)
    assert [r.get("junk") for r in records[2:]] == list(focus.JUNK_COUNTS)
    # only Surround 10: no scaling
    partial = focus.run(CFG, 1, 1, ["surround-10"])
    assert partial["scaling"] == {} and focus.planned(focus.SETS, ["surround-10"]) == 1


# --- ch2 -------------------------------------------------------------------------------------

def test_ch2_checks_read_french_expected_block():
    names = ["ch2-fig12", "ch2-fig13"]
    comps = ch2.run(CFG, 1, 1, names)
    assert [c.name for c in comps] == names and comps[1].page == 32
    texts = [ch.text for ch in comps[1].checks]
    assert "plausible saucer1 appears" in texts and "plausible cup2 appears" in texts
    assert "toward cup2 more than ch2-fig12" in texts
    toward = next(ch for ch in comps[1].checks if ch.text.startswith("toward"))
    assert toward.verdict in (True, False) and "against" in toward.numbers
    # the comparison's base not run: not a verdict
    alone = ch2.run(CFG, 1, 1, ["ch2-fig13"])
    toward = next(ch for ch in alone[0].checks if ch.text.startswith("toward"))
    assert toward.verdict is None and toward.numbers == "not run"
    yes, no, skipped = ch2.tallied(alone)
    assert yes + no + skipped == len(alone[0].checks) and skipped == 1


def test_ch2_covers_the_nineteen_and_named_answers_are_checked():
    assert list(ch2.SETS) == by_family("ch2") and len(ch2.SETS) == 19
    comps = ch2.run(CFG, 1, 1, ["ch2-fig02", "ch2-fig03"])
    assert comps[0].checks[0].text == "French's answer cup1 is our most frequent"
    assert comps[0].checks[0].verdict is True      # the touched cup alone on the table
    lines = ch2.report(comps)
    assert lines[0] == "## Chapter 2's configurations against French's words [F95 pp. 26-35]"
    assert any(l.startswith("| ch2-fig03 | 27 | cup2 |") for l in lines)
    assert lines[-2].endswith("checks agree with French's words.")
    assert ch2.records(comps)[0]["checks"][0]["verdict"] is True


# --- ch4 -------------------------------------------------------------------------------------

def test_french_dumps_and_milestones_are_read_from_the_transcription():
    data = ch4.french_data()
    dumps = ch4.french_dumps(data)
    assert [d[2] for d in dumps] == [0, 2, 12, 22, 33, 36, 47, 47, 55]
    assert dumps[1][3] == 80.0 and dumps[-1][3] == 2.0 and dumps[-1][4]["FND-ANS"] == 30
    his = ch4.french_milestones(data)
    assert his == {"first gestalt group": 2, "first bottom-up group": 33,
                   "first correspondence": 12, "first Worldview entry": 22,
                   "first group correspondence in the Worldview": 47, "first fight": 47,
                   "answer": 55}


def test_a_run_path_records_the_dumps_it_reaches_and_its_milestones():
    pb = load_problem(ch4.PROBLEM)
    path = ch4.run_path(pb, CFG, 7, (2, 12, 22))
    assert set(path.dumps) <= {2, 12, 22} and 2 in path.dumps
    t, shares = path.dumps[2]
    assert t == 80.0 and sum(shares.values()) == pytest.approx(100.0)
    assert path.milestones["answer"] == path.result.codelets
    assert path.milestones["first gestalt group"] is not None
    for name, clock in path.milestones.items():
        assert clock is None or 1 <= clock <= path.result.codelets
    # the milestones agree with the events of a plain run
    col = EV.Collector()
    from tabletop.run import run_problem
    run_problem(pb, CFG, 7, [col])
    assert ch4.milestones_of(col, pb.ids()) == path.milestones


def test_ch4_at_sample_size():
    results = ch4.run(CFG, 1, 2)
    lines = ch4.report(results)
    assert lines[0] == "## The chapter 4 run against the screen dumps [F95 pp. 96-107]"
    assert any(l.startswith("| Fig. 4.2 | 98 | 2 | 80.0 | 80.0 | 2 |") for l in lines)
    assert any(l.startswith("| Fig. 4.9 | 107 | 55 | 2.0 |") for l in lines)
    assert any(l.startswith("| first gestalt group | 2 |") for l in lines)
    assert any("French's run: small-glass1 at 55 codelets [F95 p. 107]" in l for l in lines)
    rec = ch4.records(results)
    assert len(rec) == 1 and len(rec[0]["paths"]) == 2 and len(rec[0]["dumps"]) == 8
    assert results["survey"].runs == 2 and results["survey"].problem == "surround-05"
    assert ch4.run(CFG, 1, 1, ["blockage-01"])["paths"] == []


# --- humans ----------------------------------------------------------------------------------

def test_answer_ids_follow_the_tables_columns():
    """Tables 5.1 and 5.2 name E1 and E2; Table 5.3 names "Either small glass" and
    "Originally touched cup" [F95 pp. 143-144]."""
    row = RP.subject_row("buridan-01")
    pb = load_problem("buridan-01")
    assert humans.answer_ids(row, pb) == ((pb.answers["E1"],), (pb.answers["E2"],))
    row = RP.subject_row("blockage-06")
    pb = load_problem("blockage-06")
    glasses, cup = humans.answer_ids(row, pb)
    assert set(glasses) == {pb.answers["E1"], pb.answers["E2"]} and cup == (pb.touched,)
    row = RP.subject_row("blockage-01")
    pb = load_problem("blockage-01")
    assert humans.answer_ids(row, pb) == ((pb.answers["E1"],), (pb.touched,))
    assert pb.answers["E2"] == pb.touched


def test_our_fit_mirrors_french_fit():
    for row in RP.SUBJECT_ROWS:
        assert humans.our_fit(row, row.tabletop) == RP.fit(row)
    row = RP.subject_row("blockage-05")
    assert humans.our_fit(row, (16, 34)) == 0 and humans.our_fit(row, (0, 0)) == 50


def test_human_data_objects_and_problems():
    assert humans.human_data_problems() == ["blockage-01", "surround-05", "surround-05",
                                            "blockage-02", "blockage-06", "blockage-06",
                                            "blockage-01"]
    for d in RP.HUMAN_DATA:
        if d.problem in catalogue():
            pb = load_problem(d.problem)
            obj = humans.datum_object(d, pb)
            assert obj == (pb.answers["E2"] if d.percent is None else pb.touched)


def test_humans_runs_two_profiles_and_reports_the_tables():
    names = ["buridan-09", "blockage-05"]
    comps = humans.run(CFG, 1, 1, names)
    assert [c.row.problem for c in comps] == names
    assert list(comps[0].surveys) == ["standard", "human"]
    assert humans.profiles_for(PROFILES["human"])[1].profile == "standard"
    for c in comps:
        for prof, (p0, p1) in c.ours.items():
            assert 0 <= p0 + p1 <= 100.0 + 1e-9
            assert c.fits[prof] == humans.our_fit(c.row, (p0, p1))
        assert c.french_fit == RP.fit(c.row)
    lines = humans.report(comps)
    assert lines[0] == "## Table 5.1, Buridan [F95 p. 143]"
    assert any(l.startswith("| buridan-09 | 100 | 100 |") for l in lines)
    assert any(l.startswith("| Bur18 | buridan-09 | 0 |") for l in lines)
    assert any(l.startswith("Within ten points of the subjects: French 1 of 2") for l in lines)
    assert any(l.startswith("| standard | ") for l in lines) and any(l.startswith("| human | ") for l in lines)
    co = humans.correlations(comps, "standard")
    assert set(co) == {"with_subjects", "with_tabletop", "fit_with_french"}
    assert humans.within_ten(comps, None) == 1
    rec = humans.records(comps)
    assert rec[0]["subjects"] == [100, 0] and set(rec[0]["surveys"]) == {"standard", "human"}
    assert humans.planned(humans.SETS, names) == 4


# --- potelbat ---------------------------------------------------------------------------------

def test_potelbat_bench_covers_the_catalogue_and_counts_agreements():
    assert list(PBB.SETS) == catalogue()
    pb = load_problem("blockage-06")
    assert PBB.french_answer(pb) == (pb.answers["E3"], "E3", 126)
    pb = load_problem("blockage-03")
    assert PBB.french_answer(pb) == (pb.answers["E1"], "E1+E2", 123)
    assert PBB.french_answer(load_problem("ch2-fig03")) == ("cup2", "cup2", 27)
    assert PBB.french_answer(load_problem("ch2-fig13")) == (None, "", None)
    comps = PBB.run(CFG, 1, 1, ["surround-05", "blockage-06", "ch2-fig13"])
    n, pot, tab = PBB.agreements(comps)
    assert n == 2 and pot == 0          # the baseline misses both, as FCCA says [FCCA p. 328]
    lines = PBB.report(comps)
    assert any(l.startswith("| surround-05 | small-glass1 (E1) | 118 | cup2 (E2) |") for l in lines)
    assert lines[-2].startswith("Of 2 problems with an answer of French's, the Potelbat gives it on 0")
    rec = {r["problem"]: r for r in PBB.records(comps)}
    assert list(rec) == ["blockage-06", "ch2-fig13", "surround-05"]   # the catalogue's order
    assert rec["surround-05"]["potelbat"]["answer"] == "cup2" and rec["ch2-fig13"]["french"] is None


# --- the command line and `all` -------------------------------------------------------------

def test_cli_runs_a_new_bench_and_refuses_a_problem_outside_it(tmp_path):
    proc = run_cli("rho", "--problems", "surround-05", "--runs", "1", "--quiet",
                   "--json", str(tmp_path / "rho.json"))
    assert proc.returncode == 0 and proc.stderr == ""
    assert proc.stdout.splitlines()[0] == f"engine: {accel.info()['engine']}"      # D-107
    assert proc.stdout.splitlines()[1] == "## The urgency parameter rho, 0.5 to 6 [F95 pp. 81-82]"
    records = [json.loads(l) for l in (tmp_path / "rho.json").read_text().splitlines()]
    assert len(records) == 7 and {r["variant"] for r in records} == {f"rho {r:g}" for r in rho.RHOS}
    proc = run_cli("ch4", "--problems", "blockage-01", "--runs", "1")
    assert proc.returncode == CLI.USAGE_ERROR and "not in the ch4 bench" in proc.stderr
    proc = run_cli("humans", "--problems", "buridan-09", "--runs", "1")
    assert proc.returncode == 0 and "[1/2] buridan-09 (standard)" in proc.stderr
    assert "[2/2] buridan-09 (human)" in proc.stderr


def every_table_and_its_json(out: Path, printed: str, names: list) -> None:
    """`all` printed one line per bench, in the benches' order, and wrote NAME.md (a
    table) and NAME.json (one record per line) for each under `out` (D-073)."""
    assert [l.split(":")[0] for l in printed.splitlines()] == ["engine"] + names
    for name in names:
        md, js = out / f"{name}.md", out / f"{name}.json"
        assert md.exists() and js.exists(), name
        assert md.read_text().startswith("## ")
        for line in js.read_text().splitlines():
            json.loads(line)
    assert CLI.DEFAULT_OUT == "bench-results"
    assert "bench-results/" in (PYTHON_DIR / ".gitignore").read_text()


QUICK_BENCHES = ("ch4", "focus", "temperature", "robustness", "churn")   # about a second at one run


def test_all_writes_every_table_and_its_json_on_the_quick_benches(tmp_path, monkeypatch, capsys):
    """The `all` path in this process, over the five benches that take about a second at
    one run: the fifteen at full size are `test_all_writes_every_table_and_its_json`
    (a survey of tens of seconds, marked slow)."""
    monkeypatch.setattr(CLI, "BENCHES", {n: CLI.BENCHES[n] for n in QUICK_BENCHES})
    out = tmp_path / "results"
    assert CLI.main(["all", "--runs", "1", "--out", str(out), "--quiet"]) == 0
    every_table_and_its_json(out, capsys.readouterr().out, list(QUICK_BENCHES))
    assert len((out / "ch4.json").read_text().splitlines()) == 1
    assert len((out / "robustness.json").read_text().splitlines()) >= 1
    assert len((out / "churn.json").read_text().splitlines()) == 4


# --- robustness: random tables checked, and the speed profile (D-068, D-077) --------------

def test_robustness_at_sample_size_checks_tables_and_times_runs():
    seen = []
    results = robustness.run(PROFILES["standard"], 1, 3, only=["surround-05"],
                             progress=seen.append)
    s = results["summary"]
    assert s["tables"] == 3 and s["faults"] == 0 and s["capped"] == 0
    assert len(s["results"]) == 3 and len(s["per_table"]) == 3
    roles = s["roles"]                       # the role statistics (D-106)
    assert roles["tags_per_run"] > 0 and roles["standing_per_run"] <= roles["tags_per_run"]
    assert sum(roles["by_role"].values()) == pytest.approx(   # each entry rounded to 0.01
        roles["tags_per_run"], abs=0.005 * len(roles["by_role"]) + 0.01)
    assert all(len(row) == 6 for row in s["per_table"]) and sum(row[5] for row in s["per_table"]) > 0
    assert 0 <= roles["recast_runs"] <= 3 and 0 <= roles["tie_runs"] <= roles["recast_runs"]
    assert s["seed"] == 1 and s["batch_seed"] == robustness.H.batch_seed_for(1, "random tables")
    assert sum(s["touched"].values()) == 3 and sum(s["types"].values()) >= 12
    assert s["answers"]["literal"] + s["answers"]["other_side"] >= 0
    assert [r["problem"] for r in results["speed"]] == ["surround-05"]
    row = results["speed"][0]
    assert row["plain"]["runs"] == 3 and row["checked"]["runs"] == 3
    # the rates come from the wall clock, so only their presence and type are asserted
    assert all(isinstance(row[k]["runs_per_second"], (int, float, type(None))) for k in ("plain", "checked"))
    assert "overhead" in row
    assert [sv.problem for sv in seen] == ["3 random tables", "surround-05"]
    lines = robustness.report(results)
    assert lines[0].startswith("## Robustness: 3 random tables")
    assert any("| Runs capped (D-061) | 0 of 3" in l for l in lines)
    assert any(l.startswith("| Tags read | ") for l in lines), "the role statistics (D-106)"
    assert any(l.startswith("| surround-05 |") for l in lines)
    recs = robustness.records(results)
    assert [r["kind"] for r in recs] == ["tables", "speed"]
    json.dumps(recs)
    # the tables' runs are replayable from their seeds
    seed, answer, structure, codelets, capped = s["results"][0]
    r = run_problem(robustness.TB.random_table(seed), PROFILES["standard"], seed)
    assert (r.answer, r.structure_value, r.codelets, r.capped) == (answer, structure, codelets, capped)
    assert robustness.planned(robustness.SETS, ["surround-05"]) == 2


# --- churn: roles over time and the role codelets before the answer (D-106) --------------

def test_churn_at_sample_size_reads_the_changes_and_the_role_codelets():
    seen = []
    results = churn.run(CFG, 1, 4, only=["surround-05", "blockage-02"], progress=seen.append)
    assert [sv.problem for sv in seen] == ["surround-05", "blockage-02"]
    recs = churn.records(results)
    assert [r["problem"] for r in recs] == ["surround-05", "blockage-02"]
    r = recs[0]
    assert r["bench"] == "churn" and r["runs"] == 4 and len(r["results"]) == 4
    assert r["batch_seed"] == H.batch_seed_for(1, "surround-05")
    assert r["changes"] == sum(r["reasons"].values()) == sum(r["changes_by_role"].values())
    assert 0 <= r["changed_runs"] <= 4 and r["last_before_worldview"] <= r["changed_runs"]
    assert set(r["bands"]) == {"hot", "warm", "cold"}
    assert sum(b["codelets"] for b in r["bands"].values()) == sum(row[3] for row in r["results"])
    assert sum(b["changes"] for b in r["bands"].values()) == r["changes"]
    assert set(r["role_codelets"]) == set(churn.ROLE_CODELETS)
    assert r["find_answer_chosen"] == 1.0, "every uncapped run ends by one find-answer"
    assert r["find_answer_hung"] >= 1.0 and r["runs_with_role_codelet"] <= 4
    assert r["tags_read"] >= r["standing"] > 0
    assert sum(r["answers_touching"].values()) + sum(r["answers_untouched"].values()) == 4
    json.dumps(recs)
    lines = churn.report(results)
    assert lines[0].startswith("## Roles over time: 4 runs per carrier")
    assert any(l.startswith("| surround-05 | 4 | ") for l in lines)
    assert any("propose-role-cp" in l for l in lines)
    assert churn.planned(churn.SETS, ["surround-05"]) == 1
    assert churn.reason_of("lost to r7") == "lost" and churn.band_of(66.7) == "hot"
    assert churn.band_of(50.0) == "warm" and churn.band_of(1.0) == "cold"
    # a run's facts are replayable from the seed, and the timeline draws nothing
    seed, answer, structure, codelets, capped = r["results"][0]
    facts = churn.run_facts(load_problem("surround-05"), CFG, seed)
    assert (facts["result"].answer, facts["result"].codelets) == (answer, codelets)
    assert facts["result"] == run_problem(load_problem("surround-05"), CFG, seed)
