"""The command line, `python3 -m seekwhence`, run in subprocesses [M86 pp. 164-179]
(DECISION D-069: the same seed in two processes gives the same output)."""

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

PKG = Path(__file__).resolve().parent.parent
FENCE_SEED = 4


def run_cli(*args, stdin=None, hashseed=None):
    env = dict(os.environ)
    if hashseed is not None:
        env["PYTHONHASHSEED"] = str(hashseed)
    return subprocess.run([sys.executable, "-m", "seekwhence", *args], cwd=PKG, env=env,
                          input=stdin, capture_output=True, text=True, timeout=120)


def test_help():
    proc = run_cli("--help")
    assert proc.returncode == 0
    for option in ("--seed", "--terms", "--answers", "--trace", "--json", "--max-steps"):
        assert option in proc.stdout
    assert "show-seq" in proc.stdout and "interactive" in proc.stdout


def test_scripted_run_1_ends_with_bye_and_an_outcome_line():
    """Run 1 scripted: `show-hypothesis`, the fourth term, the guess, `ok`, `bye`
    [M86 pp. 164-166]."""
    proc = run_cli("--seed", str(FENCE_SEED), "--terms", "3", "7", "3", "show-hypothesis", "7",
                   "--answers", "ok")
    assert proc.returncode == 0, proc.stderr
    lines = proc.stdout.splitlines()
    assert lines[:3] == ["-> (startup)", "", "please enter a term: 3"]
    at = lines.index("please enter a term: show-hypothesis")
    assert lines[at + 1] == "(Cycle 3 2 (3 7))"
    assert lines[-6:-1] == ["hypothesis: (Cycle 3 2 (3 7))", "3 7 3 7", "",
                            "enter no if wrong, ok if right ok", "bye"]
    assert lines[-1].startswith("outcome: ok after 4 terms, ")
    assert lines[-1].endswith(f"(seed {FENCE_SEED}): (Cycle 3 2 (3 7))")


def test_same_seed_same_output_in_two_processes():
    """Two fresh processes with their own hash seeds."""
    a = run_cli("--seed", "7", "--terms", "1", "2", "1", "2", "3", "1", "2", "3", "4",
                hashseed=1)
    b = run_cli("--seed", "7", "--terms", "1", "2", "1", "2", "3", "1", "2", "3", "4",
                hashseed=4242)
    assert a.returncode == b.returncode
    assert a.stdout == b.stdout and len(a.stdout.splitlines()) > 20
    c = run_cli("--seed", "8", "--terms", "1", "2", "1", "2", "3", "1", "2", "3", "4")
    assert c.stdout != a.stdout


def test_a_judged_guess_and_the_end_of_the_terms():
    """A guess with no scripted answer is judged against the scripted terms (DECISION
    D-033); the script running out gives exit status 1."""
    proc = run_cli("--seed", str(FENCE_SEED), "--terms", "3", "7", "3", "7", "3", "7",
                   "--trace", "terse")
    assert proc.returncode == 0
    assert "enter no if wrong, ok if right ok" in proc.stdout
    assert not any(line.startswith("doing task") for line in proc.stdout.splitlines())
    proc = run_cli("--seed", str(FENCE_SEED), "--terms", "3", "7", "--trace", "terse")
    assert proc.returncode == 1
    lines = proc.stdout.splitlines()
    assert lines[-2] == "please enter a term: "
    assert lines[-1].startswith("outcome: no-terms after 2 terms, ")


def test_interactive_input_from_a_pipe_with_commands_and_bye():
    """The user protocol on a pipe: the terms, `show`, `show-seq`, `show-parse`, a line that
    is neither, and `bye` [M86 pp. 167-169, 174] (DECISION D-054)."""
    proc = run_cli("--seed", str(FENCE_SEED), "--trace", "terse",
                   stdin="3\n7\n3\nshow\nshow-seq\nshow-parse\nwhat\nbye\n")
    assert proc.returncode == 0, proc.stderr
    lines = proc.stdout.splitlines()
    at = lines.index("please enter a term: show")
    assert lines[at + 1:at + 10] == [
        "terms of the sequence:", "3 7 3", "", "bonds:",
        "bond1 Same print-value (remote) -- (glint1 glint3)", "", "gloms:",
        "glom1 (Same print-value fence) --> (3 7) terms 1 to 2",
        "glom3 pseudo --> ((3 7))  terms 1 to 2"]
    assert lines[lines.index("please enter a term: show-seq") + 1] == "3 7 3"
    assert lines[lines.index("please enter a term: show-parse") + 1] == "(((3 7)))"
    assert lines[lines.index("please enter a term: what") + 1].startswith("(enter a nonneg")
    assert lines[-1] == "please enter a term: bye"
    assert not any(line.startswith("outcome:") for line in lines)


def test_interactive_guess_answered_on_the_pipe_and_eof():
    proc = run_cli("--seed", str(FENCE_SEED), "--trace", "terse", stdin="3\n7\n3\n7\nok\n")
    assert proc.returncode == 0
    assert proc.stdout.splitlines()[-2:] == ["enter no if wrong, ok if right ok", "bye"]
    proc = run_cli("--seed", str(FENCE_SEED), "--trace", "terse", stdin="3\n")
    assert proc.returncode == 0
    assert proc.stdout.splitlines()[-1] == "please enter a term: "


def test_max_steps_caps_the_run():
    """DECISION D-011: the whole-run cap in tasks."""
    proc = run_cli("--seed", str(FENCE_SEED), "--terms", "3", "7", "3", "7", "--max-steps",
                   "20", "--trace", "terse")
    assert proc.returncode == 1
    assert proc.stdout.splitlines()[-1].startswith("outcome: capped after 3 terms, 20 tasks")


def test_json_writes_one_object_per_event(tmp_path):
    path = tmp_path / "events.jsonl"
    proc = run_cli("--seed", str(FENCE_SEED), "--terms", "3", "7", "3", "7", "--answers", "ok",
                   "--json", str(path))
    assert proc.returncode == 0
    records = [json.loads(line) for line in path.read_text().splitlines()]
    assert records[0] == {"kind": "start", "seed": FENCE_SEED, "config": records[0]["config"]}
    kinds = [r["kind"] for r in records]
    assert kinds.count("term") == 4 and "guess" in kinds and kinds[-1] == "ended"
    chosen = [r for r in records if r["kind"] == "chosen"]
    assert len(chosen) == int(proc.stdout.splitlines()[-1].split(" tasks")[0].split()[-1])


@pytest.mark.parametrize("args", [["--terms", "3", "x"], ["--terms", "-1"], ["--max-steps", "0"],
                                  ["--trace", "loud"], ["--seed", "one"], ["--seed", "-1"]])
def test_usage_errors_exit_2(args):
    proc = run_cli(*args)
    assert proc.returncode == 2 and "usage:" in proc.stderr
