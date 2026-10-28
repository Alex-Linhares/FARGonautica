"""The Lisp emitter: SBCL runs Mitchell's Copycat and writes valid JSON lines. Skipped
without sbcl or the melanies-copycat folder."""
import json
import shutil
import subprocess

import pytest

from melanies_opengl import MELANIES_COPYCAT, lisprun

pytestmark = pytest.mark.skipif(not shutil.which("sbcl"), reason="needs sbcl")


def test_trace_is_json_lines_ending_with_an_answer():
    cmd = lisprun.sbcl_command("abc", "abd", "ijk", seed=1, every=1)
    out = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
    assert out.returncode == 0, out.stderr[-2000:]
    assert out.stderr.strip() == ""          # Mitchell's own printing is silenced
    lines = [json.loads(l) for l in out.stdout.splitlines() if l.strip()]
    assert lines[0]["k"] == "init"
    assert lines[-1]["k"] == "final"
    assert all(l["k"] == "step" for l in lines[1:-1])
    # one line per codelet: n climbs by one
    assert [l["n"] for l in lines[1:-1]] == list(range(1, len(lines) - 1))
    assert lines[-1]["n"] == len(lines) - 2
    final = lines[-1]
    assert final["found"] is True
    assert "".join(x["p"] for x in final["strings"]["a"]) == "ijl"     # the pinned run
    assert final["rule"].startswith("Replace ")
    assert final["trule"].startswith("Replace ")
    assert len(lines[0]["nodes"]) == 59 == len(final["act"]) == len(final["clamp"])
    assert lines[0]["timestep"] == 15
    assert lines[0]["problem"] == ["abc", "abd", "ijk"] and lines[0]["seed"] == 1


def test_every_thins_the_steps():
    cmd = lisprun.sbcl_command("abc", "abd", "ijk", seed=1, every=50)
    out = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
    lines = [json.loads(l) for l in out.stdout.splitlines() if l.strip()]
    steps = [l["n"] for l in lines if l["k"] == "step"]
    assert steps and all(n % 50 == 0 for n in steps)
    assert lines[-1]["k"] == "final" and lines[-1]["n"] == 389


def test_blocking_run_reads_the_live_trace():
    run = lisprun.blocking_run("abc", "abd", "ijk", seed=1, every=20).run_to_end()
    assert run.answer == "ijl"
    assert run.coderack.time == 389
    assert run.workspace.temperature == 13
    assert run.finished


def test_melanies_copycat_is_where_we_expect():
    import os
    assert os.path.exists(os.path.join(MELANIES_COPYCAT, "load.lisp"))


def test_a_recording_is_complete_up_to_the_final_line(tmp_path):
    """The record file must hold every line, the `final` one included, even though the
    program may exit as soon as it has read the final line (the reader thread is a daemon
    and the file was block-buffered: three of six recordings lost their tail before the
    per-line flush and `wait()`)."""
    path = tmp_path / "ijk.jsonl"
    run = lisprun.blocking_run("abc", "abd", "ijk", seed=1, every=1, record=str(path))
    run.run_to_end()
    run.lisp.wait()
    lines = [json.loads(t) for t in path.read_text().splitlines() if t.strip()]
    assert lines[0]["k"] == "init"
    assert lines[-1]["k"] == "final"
    assert lines[-1]["n"] == 389
    assert len(lines) == 389 + 2
