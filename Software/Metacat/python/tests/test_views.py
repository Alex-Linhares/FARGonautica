"""The views attached to runs (loop0002 item 14).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
The counterpart of racket/tests/views-test.rkt.

1. Watching changes nothing: golden runs with every window attached
   (metacat.gui.views.attach_views: all graphics switches on, as in the
   original program; offscreen hosts, metacat/gui/hosts.py) give traces
   identical to tests/golden/, and every window was drawn into.  The fast tier
   runs one short golden; the slow tier all 109, and the original's crash run,
   which must crash at the same point with the views attached.
2. Pictures (slow): render_views.py draws eight scenes (run7 = abc abd xyz
   3852097033 at 300 and 800 codelets, its snag event's view, its answer, the
   answer's description, a click on its last clamp event in the Trace; a
   justify run; two answers compared in the Memory) on real Tk canvases under
   Xvfb.  Every picture must have been drawn into (several colours, Tk items).
   python/tests/snapshots/views/ keeps one rendering for inspection, next to
   racket/tests/snapshots/ and docs/screenshots/.
"""
from __future__ import annotations

import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

import golden_harness as g

HERE = Path(__file__).resolve().parent


def assert_same_trace(file_name, got):
    expected = g.golden_text(file_name)
    if got != expected:
        line, e, a = g.first_difference(expected, got or "")
        pytest.fail(f"{file_name}: with the views attached, first difference at line {line}\n"
                    f"  golden: {e}\n  python: {a}", pytrace=False)


def window_items(stdout):
    return {m.group(1): int(m.group(2))
            for m in re.finditer(r"^(\S+) view: (\d+) items$", stdout, re.M)}


# the least a window draws in a golden run (views-test.rkt's thresholds): the
# Memory, the Trace and the vertical themes only once there are answers, events
# or themes, the bottom themes only in justify runs
MINIMUM = {"workspace": 11, "slipnet": 11, "coderack": 11, "top-themes": 11,
           "vertical-themes": 0, "memory": 0, "commentary": 2, "trace": 0,
           "temperature": 11, "EEG": 11}


def test_a_short_golden_with_views():
    """a b z, seed 1, 1000 codelets with keep-going, every window attached."""
    (status, reason, text, stdout), = g.run_in_fresh_process(
        [(["a", "b", "z"], 1, 1000, True)], 1, digest="views")
    assert status == "ok", reason
    assert_same_trace("a-b-z_1.jsonl", text)
    items = window_items(stdout)
    assert sorted(items) == sorted(list(MINIMUM) + ["bottom-themes"])
    for window, n in MINIMUM.items():
        assert items[window] >= n, (window, items)


@pytest.fixture(scope="module")
def views_results():
    runs = g.golden_runs()
    runs.sort(key=lambda r: -os.path.getsize(os.path.join(g.GOLDEN_DIR, r[0])))
    results = g.run_in_fresh_process([(s, seed, cap, keep) for _, s, seed, cap, keep in runs],
                                     digest="views")
    return {r[0]: res for r, res in zip(runs, results)}


@pytest.mark.slow
@pytest.mark.parametrize("file_name", [r[0] for r in g.golden_runs()])
def test_golden_with_views(file_name, views_results):
    status, reason, text, stdout = views_results[file_name]
    if file_name == "abc-ccbbaa-ijk_3.jsonl":
        pytest.skip("the crash run: test_crash_run_with_views")
    assert status == "ok", f"{file_name}: the run raised with the views attached: {reason}"
    assert_same_trace(file_name, text)
    items = window_items(stdout)
    for window, n in MINIMUM.items():
        assert items.get(window, -1) >= n, f"{file_name}: the {window} window drew {items}"


@pytest.mark.slow
def test_every_window_drew_in_the_goldens(views_results):
    totals = {}
    for status, reason, text, stdout in views_results.values():
        for window, n in window_items(stdout).items():
            totals[window] = totals.get(window, 0) + n
    for window in MINIMUM:
        assert totals.get(window, 0) > 1000, (window, totals)
    assert totals.get("bottom-themes", 0) > 0, totals


@pytest.mark.slow
def test_crash_run_with_views():
    """The original's crash (abc ccbbaa ijk seed 3, a caddr of #f) happens at the
    same point, with the same trace, with the views attached."""
    job = [(["abc", "ccbbaa", "ijk"], 3, 10000, False)]
    (plain_status, plain_reason, plain_text, _), = g.run_in_fresh_process(job, 1)
    (status, reason, text, _), = g.run_in_fresh_process(job, 1, digest="views")
    assert plain_status == status == "error"
    assert "caddr" in reason and reason == plain_reason
    assert text == plain_text and text.count("\n") > 1000


@pytest.mark.slow
def test_render_scenes(tmp_path):
    """render_views.py under Xvfb: every scene's windows are drawn into."""
    import render_views
    proc = subprocess.run(
        ["xvfb-run", "-a", "-s", "-screen 0 3000x2000x24", sys.executable,
         str(HERE / "render_views.py"), str(tmp_path)],
        capture_output=True, text=True, timeout=900,
        env={k: v for k, v in os.environ.items() if k != "WAYLAND_DISPLAY"})
    assert proc.returncode == 0, proc.stdout[-3000:] + proc.stderr[-3000:]
    lines = {line.split()[0]: line.split()[1:] for line in proc.stdout.splitlines()}
    want = ["%s-%s.png" % (w, scene) for scene, spec in render_views.SCENES.items()
            for w in spec[-1]]
    assert sorted(lines) == sorted(want)
    for name, (w, h, ncolours, items) in lines.items():
        assert (tmp_path / name).exists()
        assert int(ncolours) >= 2, name + ": blank"
        assert int(items) >= 1, name + ": no items"
        if name.startswith("workspace-"):
            assert (int(w), int(h)) == (800, 600), name
