"""The oracle and lesion probes' tool (`tools/role_probes.py`; D-104, RESULTS.md §11.3 and
§11.4): a measurement, never a gate, and test-only. The tool is a list of independent jobs
(`plan`, `job`, `assemble`), so that a driver may farm them over processes and still write
what the serial run writes; these tests run it small."""

import importlib.util
import json
from pathlib import Path

PYTHON_DIR = Path(__file__).resolve().parents[1]
TOOL = PYTHON_DIR / "tools" / "role_probes.py"


def load_tool():
    spec = importlib.util.spec_from_file_location("role_probes_tool", TOOL)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_the_plan_names_every_lesion_but_the_no_break_probe_and_its_extra_sets():
    T = load_tool()
    jobs = T.plan(["blockage-02"], 1, 1, True)
    kinds = [j["kind"] for j in jobs]
    assert kinds[:4] == ["plain", "oracle", "no_break", "roles_read"]
    assert "scan" not in kinds                      # the penalty scan is the Buridan carrier's
    lesions = {j["lesion"] for j in jobs if j["kind"] == "lesion"}
    assert lesions == set(T.lesion_names()) and T.NO_BREAK not in lesions
    assert set(T.LESION_EXTRA) == set(T.lesion_names())
    extra = {j["problem"] for j in jobs if j["kind"] == "plain"} - {"blockage-02"}
    assert extra == {p for ps in T.LESION_EXTRA.values() for p in ps}
    scan = [j for j in T.plan(["buridan-01"], 1, 1, False) if j["kind"].startswith("scan")]
    assert [(j["kind"], j["penalty"]) for j in scan] == [("scan", 40), ("scan_oracle", 40),
                                                         ("scan", 300), ("scan_oracle", 300)]


def test_a_small_run_assembles_the_record_and_pairs_the_lesions(tmp_path):
    T = load_tool()
    names = ["blockage-02"]
    jobs = [j for j in T.plan(names, 2, 1, True)
            if j["kind"] != "lesion" or j["lesion"] in ("C1", "L2")]
    jobs = [j for j in jobs if j["problem"] == "blockage-02"]
    outputs = [T.job(j) for j in jobs]
    out = T.assemble(jobs, outputs, names, 2, 1, 0.0)
    record = out["records"][0]
    assert record["problem"] == "blockage-02" and out["extra_records"] == []
    for key in ("perceived", "oracle", "no_break", "roles_read", "oracle_tv", "lesions"):
        assert key in record
    assert "labels" not in record["perceived"] and "labels" not in record["lesions"]["C1"]
    assert record["perceived"]["runs"] == 2 and sum(record["perceived"]["answers"].values()) == 2
    assert set(record["perceived"]["through"]) <= {"correspondence", "default", "blocked", "group"}
    assert 0.0 <= record["oracle_tv"] <= 1.0
    rr = record["roles_read"]
    assert rr["hand_read"] == 14 and 0.0 <= rr["missed"] <= 1.0 and 0.0 <= rr["extra"] <= 1.0
    assert all(0.0 <= v <= 100.0 for v in rr["present"].values())
    for lesion in ("C1", "L2"):
        assert 0 <= record["lesions"][lesion]["paired_differ"] <= 2
        assert sum(record["lesions"][lesion]["moved"].values()) == record["lesions"][lesion]["paired_differ"]
    assert out["lesions"] == ["C1", "L2"]
    text = json.dumps(out)
    assert "labels" not in text


def test_the_recorded_file_has_every_section_at_two_hundred_runs():
    data = json.loads((PYTHON_DIR / "results" / "role-probes.json").read_text())
    T = load_tool()
    assert data["runs"] == 200 and data["lesions"] == T.lesion_names()
    assert [r["problem"] for r in data["records"]] == T.RP.oracle_problems()
    for record in data["records"]:
        assert set(record["lesions"]) == set(T.lesion_names())
        assert record["roles_read"]["runs"] == 200 and record["perceived"]["runs"] == 200
    extra = {p for ps in T.LESION_EXTRA.values() for p in ps}
    assert {r["problem"] for r in data["extra_records"]} == extra
    for record in data["extra_records"]:
        wanted = {l for l, ps in T.LESION_EXTRA.items() if record["problem"] in ps}
        assert set(record["lesions"]) == wanted
