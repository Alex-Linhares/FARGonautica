"""Self-determinism of the compiled engine at size (D-107, D-112): the families problems x 5
seeds through `tools/workload.py` in two fresh processes with different `PYTHONHASHSEED`s
give the same rows digest, and the four golden cases of
`tests/data/golden/compiled/fingerprints.json` give their recorded fingerprints in both
processes. About a minute. Skips, with a reason, when the python engine was asked for
explicitly. The quick version is `tests/test_determinism.py`.
"""

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel

PYTHON_DIR = Path(__file__).resolve().parents[1]
GOLDEN = PYTHON_DIR / "tests" / "data" / "golden" / "compiled" / "fingerprints.json"
WORKLOAD = PYTHON_DIR / "tools" / "workload.py"
HASH_SEEDS = ("0", "31337")

pytestmark = [
    pytest.mark.slow,
    pytest.mark.skipif(
        accel.info()["engine"] != "compiled",
        reason="the compiled engine is not the one running (TABLETOP_ENGINE=python was "
               "requested explicitly)"),
]

FINGERPRINTS = """
import json
from tabletop import accel
from tabletop.run import fingerprint
assert accel.info()["engine"] == "compiled", accel.info()
recorded = json.loads(open(%r).read())
print(json.dumps({key: list(fingerprint(*key.split()[:1], int(key.split()[1])))
                  for key in recorded}))
"""


def environment(hash_seed: str) -> dict:
    env = {k: v for k, v in os.environ.items() if k != "TABLETOP_ENGINE"}
    env["PYTHONHASHSEED"] = hash_seed
    return env


def in_process(args, hash_seed: str) -> str:
    proc = subprocess.run([sys.executable, *args], cwd=PYTHON_DIR, text=True,
                          capture_output=True, env=environment(hash_seed), check=True)
    return proc.stdout.strip().splitlines()[-1]


def test_the_families_workload_x5_digests_the_same_in_two_processes():
    args = [str(WORKLOAD), "--engine", str(PYTHON_DIR), "--runs", "5"]
    records = [json.loads(in_process(args, h)) for h in HASH_SEEDS]
    assert all(r["engine"] == "compiled" and r["problems"] == 34 and r["count"] == 170
               for r in records)
    assert records[0]["digest"] == records[1]["digest"]
    assert records[0]["codelets"] == records[1]["codelets"]


def test_the_four_golden_cases_in_two_processes():
    recorded = json.loads(GOLDEN.read_text())
    assert len(recorded) == 4
    for h in HASH_SEEDS:
        got = json.loads(in_process(["-c", FINGERPRINTS % str(GOLDEN)], h))
        assert got == {key: list(value) for key, value in recorded.items()}, h
