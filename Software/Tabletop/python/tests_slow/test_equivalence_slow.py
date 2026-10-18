"""The statistical equivalence of the pure engine at size (D-107, D-113): every families
problem at 200 runs on the compiled engine and 200 on the pure engine (two processes, the
pure one on the explicit request), through `tools/equivalence.py` at its defaults; every
problem's total variation within the tolerance 0.25, and the simulated null still under
it. About two minutes on two cores. Skips, with a reason, when the python engine was asked
for explicitly. The quick version is `tests/test_equivalence.py`.
"""

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel
from test_equivalence import TOOL, load_tool

PYTHON_DIR = Path(__file__).resolve().parents[1]

pytestmark = [
    pytest.mark.slow,
    pytest.mark.skipif(
        accel.info()["engine"] != "compiled",
        reason="the compiled engine is not the one running (TABLETOP_ENGINE=python was "
               "requested explicitly)"),
]


def test_every_families_problem_is_within_the_tolerance_at_200_runs(tmp_path):
    tool = load_tool()
    out = tmp_path / "equivalence.json"
    env = {k: v for k, v in os.environ.items() if k != "TABLETOP_ENGINE"}
    proc = subprocess.run([sys.executable, "-I", str(TOOL), "--out", str(out)],
                          capture_output=True, text=True, cwd=str(PYTHON_DIR.parent), env=env)
    assert proc.returncode == 0, proc.stderr + proc.stdout
    assert proc.stdout.splitlines()[0] == "engine: compiled against python"
    full = json.loads(out.read_text())
    meta = full["meta"]
    assert meta["runs"] == tool.RUNS == 200 and meta["tolerance"] == tool.TOLERANCE == 0.25
    assert meta["engines"]["compiled"]["info"]["engine"] == "compiled"
    assert meta["engines"]["python"]["info"]["engine"] == "python"
    rows = full["rows"]
    assert len(rows) == 34
    beyond = [(r["problem"], r["tv"]) for r in rows if r["tv"] > tool.TOLERANCE]
    assert beyond == [], beyond
    assert full["summary"]["beyond_tolerance"] == []
    assert all(sum(r["python"].values()) == 200 and sum(r["compiled"].values()) == 200
               for r in rows)
    # the tolerance still covers the null at this N (D-113's derivation holds for this tree)
    assert max(r["null_q999"] for r in rows) < tool.TOLERANCE
