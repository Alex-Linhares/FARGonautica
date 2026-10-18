"""The slow tests of `tests/test_variants.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import importlib.util
import os
import shutil

import pytest

from test_variants import (
    CFG,
    PYTHON_DIR,
    Path,
    TOOL,
    V,
    json,
    load_tool,
    pytest,
    subprocess,
    sys,
    worktrees_named,
)

pytestmark = pytest.mark.slow


def test_the_tool_runs_the_current_bench_on_a_commit_and_removes_the_worktree(tmp_path):
    tool = load_tool()
    top = tool.top_level()
    if top is None:
        pytest.skip("not inside a git repository (the standalone copy)")
    worktree = Path("/tmp") / f"tabletop-v1-test-{tmp_path.name}"
    if not cython_findable():
        pytest.skip("no Cython here (the module, TABLETOP_CYTHON or `cython` on PATH): the "
                    "tool cannot build the compiled engine in a worktree of HEAD (D-107)")
    args = ["HEAD", str(worktree), "variants", "--problems", "blockage-01", "--runs", "1",
            "--seed", "3", "--json", str(tmp_path / "wt.json"),
            "--report", str(tmp_path / "wt.md")]
    env = dict(os.environ)
    env.pop("TABLETOP_ENGINE", None)          # the compiled engine, built in the worktree
    proc = subprocess.run([sys.executable, str(TOOL), *args], capture_output=True, text=True,
                          env=env)
    assert proc.returncode == 0, proc.stderr
    assert "engine: the compiled engine, built in the worktree" in proc.stderr
    written_records = [json.loads(l) for l in (tmp_path / "wt.json").read_text().splitlines()]
    assert written_records and all(r["engine"]["engine"] == "compiled" for r in written_records)
    assert not worktree.exists() and not worktrees_named(top, worktree.name)
    results = V.run(CFG, 3, 1, ["blockage-01"])
    written = (tmp_path / "wt.md").read_text().splitlines()
    assert written[0] == V.report(results)[0] and (tmp_path / "wt.json").read_text().strip()
    engine_at_head = subprocess.run(["git", "-C", str(top), "diff", "--quiet", "HEAD", "--",
                                     str(PYTHON_DIR / "tabletop")]).returncode == 0
    if not engine_at_head:
        pytest.skip("the engine differs from HEAD: the tool's run on HEAD is another engine's")
    assert written == V.report(results)
    # the tool's records carry the engine that ran them (D-107), as the bench CLI's do; the
    # module's own do not: the worktree's build of HEAD has this tree's stamp, so the field
    # is this process's `accel.info()`
    from tabletop import accel
    expected = [dict(record, engine=accel.info()) for record in V.records(results)]
    assert [json.loads(l) for l in (tmp_path / "wt.json").read_text().splitlines()] == \
        json.loads(json.dumps(expected))
    # the worktree's own bench, for a bench the commit already had
    proc = subprocess.run([sys.executable, str(TOOL), "HEAD", str(worktree), "families",
                           "--worktree-bench", "--problems", "blockage-01", "--runs", "1",
                           "--quiet", "--json", str(tmp_path / "fam.json")],
                          capture_output=True, text=True)
    assert proc.returncode == 0, proc.stderr
    [record] = [json.loads(l) for l in (tmp_path / "fam.json").read_text().splitlines()]
    assert record["problem"] == "blockage-01" and record["runs"] == 1
    assert not worktrees_named(top, worktree.name)
    # the pure engine on the explicit request alone: no build, the records say python
    proc = subprocess.run([sys.executable, str(TOOL), "HEAD", str(worktree), "families",
                           "--engine", "python", "--worktree-bench", "--problems",
                           "blockage-01", "--runs", "1", "--json", str(tmp_path / "py.json")],
                          capture_output=True, text=True, env=env)
    assert proc.returncode == 0, proc.stderr
    assert "engine: the pure engine, by explicit request" in proc.stderr
    [record] = [json.loads(l) for l in (tmp_path / "py.json").read_text().splitlines()]
    assert record["engine"]["engine"] == "python"
    assert not worktrees_named(top, worktree.name)


def cython_findable() -> bool:
    """Whether `tools/build_fast.py` would find Cython here: the module, the executable
    named by TABLETOP_CYTHON, or `cython` on PATH (D-110)."""
    given = os.environ.get("TABLETOP_CYTHON")
    return (importlib.util.find_spec("Cython") is not None
            or bool(given and os.access(given, os.X_OK))
            or shutil.which("cython") is not None)
    # a bad commit: no worktree is left either
    proc = subprocess.run([sys.executable, str(TOOL), "no-such-commit", str(worktree), "variants"],
                          capture_output=True, text=True)
    assert proc.returncode == 1 and not worktrees_named(top, worktree.name)
