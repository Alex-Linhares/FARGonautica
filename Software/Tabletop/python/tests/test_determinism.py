"""Self-determinism of the compiled engine, the quick version (D-107, D-112): the same seed
gives the same run in two fresh processes with different `PYTHONHASHSEED`s, checked on one
golden case against `tests/data/golden/compiled/fingerprints.json` and on a two-problem
workload through `tools/workload.py`. The families problems x 5 seeds are
`tests_slow/test_determinism_slow.py`'s. These skip, with a reason, when the python engine
was asked for explicitly. The pure engine's own determinism (D-113): one of its golden cases
(`tests/data/golden/python/fingerprints.json`) in two processes started with
`TABLETOP_ENGINE=python`, under two hash seeds; it needs no build and runs on either engine.
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
GOLDEN_PYTHON = PYTHON_DIR / "tests" / "data" / "golden" / "python" / "fingerprints.json"
WORKLOAD = PYTHON_DIR / "tools" / "workload.py"

compiled_only = pytest.mark.skipif(
    accel.info()["engine"] != "compiled",
    reason="the compiled engine is not the one running (TABLETOP_ENGINE=python was requested "
           "explicitly)")

FINGERPRINT = """
import json, sys
from tabletop import accel
from tabletop.run import fingerprint
assert accel.info()["engine"] == sys.argv[3], accel.info()
print(json.dumps(fingerprint(sys.argv[1], int(sys.argv[2]))))
"""


def environment(hash_seed: str, engine: str = "compiled") -> dict:
    env = {k: v for k, v in os.environ.items() if k != "TABLETOP_ENGINE"}
    if engine == "python":
        env["TABLETOP_ENGINE"] = "python"    # the explicit request, and only that (D-107)
    env["PYTHONHASHSEED"] = hash_seed
    return env


def in_process(args, hash_seed: str, engine: str = "compiled") -> str:
    proc = subprocess.run([sys.executable, *args], cwd=PYTHON_DIR, text=True,
                          capture_output=True, env=environment(hash_seed, engine), check=True)
    return proc.stdout.strip().splitlines()[-1]


def test_the_python_engine_golden_case_in_two_processes_with_two_hash_seeds():
    """The pure engine's own determinism (D-113): its golden, on explicit request."""
    recorded = json.loads(GOLDEN_PYTHON.read_text())
    key = "surround-05 1"
    digest, draws = recorded[key]
    for hash_seed in ("0", "4242"):
        got = json.loads(in_process(["-c", FINGERPRINT, "surround-05", "1", "python"],
                                    hash_seed, "python"))
        assert got == [digest, draws], hash_seed


@compiled_only
def test_one_golden_case_in_two_processes_with_two_hash_seeds():
    recorded = json.loads(GOLDEN.read_text())
    key = "surround-05 1"
    digest, draws = recorded[key]
    for hash_seed in ("0", "4242"):
        got = json.loads(in_process(["-c", FINGERPRINT, "surround-05", "1", "compiled"],
                                    hash_seed))
        assert got == [digest, draws], hash_seed


@compiled_only
def test_a_small_workload_digests_the_same_in_two_processes():
    args = [str(WORKLOAD), "--engine", str(PYTHON_DIR), "--runs", "2", "--only", "2"]
    records = [json.loads(in_process(args, hash_seed)) for hash_seed in ("1", "9999")]
    assert records[0]["engine"] == records[1]["engine"] == "compiled"
    assert records[0]["digest"] == records[1]["digest"]
    assert records[0]["count"] == 4 and records[0]["codelets"] == records[1]["codelets"]
