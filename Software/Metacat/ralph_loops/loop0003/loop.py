#!/usr/bin/env python3
"""Ralph loop driver: one fresh `claude -p` session per work item.

Each iteration: read TASK.md + PROGRESS.md, take the first unchecked item in
iterations.md, run a fresh Claude session on it under a wall-clock cap, run the
regression tests, ask Claude to fix failures (up to N attempts), and commit if
green. If tests still fail, the code is reverted and PROGRESS.md
is kept. Each commit is pushed to origin. Stops when PROGRESS.md contains LOOP_COMPLETE, the item list is
exhausted, or knobs.json says stop.

    python3 loop.py              # run up to knobs["max_iterations"] iterations
    python3 loop.py 3            # run at most 3 iterations
    python3 loop.py --fresh      # reset PROGRESS.md first
    python3 loop.py --dry-run    # print the prompt for the next item and exit
    python3 loop.py --resume-wip # start on top of uncommitted work (after a crash)

Peeking while it runs (all files live in this folder):

    tail -f loop.log                  # one line per event: start, tool calls, tests, commits
    tail -f session_it03.log          # the running session, readable (text + tool calls)
    cat status.json                   # what is running now, since when, phase
    session_it03.jsonl                # the raw stream, if you need everything

The knobs (knobs.json) are re-read at the start of every iteration and every
fix attempt, so they can be turned while the loop runs:

    iteration_cap_hours   wall-clock cap per Claude session (default 3.0)
    fix_cap_hours         cap per test-fix session (default 0.5)
    max_fix_attempts      fix sessions before reverting code (default 3)
    max_iterations        stop after this many in one run (default 10)
    model                 passed to `claude --model`; null = the default
    pause                 true = finish the current iteration, then wait, polling
                          every 60 s until it is false again
    stop                  true = finish the current iteration, commit, exit
    sleep_between_s       pause between iterations (default 0)
    run_tests             false = skip the test gate (not recommended)
    push                  false = commit only; true = `git push origin` after each commit
    gate_timeout_min      wall-clock limit on one gate run; a hung gate counts as failed
                          (default 30)
"""
from __future__ import annotations

import datetime as dt
import json
import os
import re
import signal
import subprocess
import sys
import time
from pathlib import Path

# GUI code must never open on the owner's screen. The desktop is Wayland, where GTK and
# Qt ignore xvfb-run's DISPLAY unless WAYLAND_DISPLAY is gone, so every session and
# gate run inherits a headless environment; Tk still needs `xvfb-run -a`.
os.environ.pop("WAYLAND_DISPLAY", None)
os.environ["GDK_BACKEND"] = "x11"
os.environ["QT_QPA_PLATFORM"] = "offscreen"

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
TASK = HERE / "TASK.md"
PROGRESS = HERE / "PROGRESS.md"
ITEMS = HERE / "iterations.md"
KNOBS = HERE / "knobs.json"
LOG = HERE / "loop.log"
STATUS = HERE / "status.json"
LOOP_NAME = HERE.name
TEST_CMD = ["python3", f"ralph_loops/{LOOP_NAME}/gate.py"]
# Kept when a failing iteration's code is reverted: the loop folder and the notes.
KEEP_ON_REVERT = (str(HERE.relative_to(REPO)) + "/", "docs/")
# Directories that must be clean before the loop starts.
CODE_DIRS = ("python", "racket", "tests", "docs")
UNCHECKED = re.compile(r"^- \[ \] (.*)$", re.M)

DEFAULT_KNOBS = {
    "iteration_cap_hours": 3.0,
    "fix_cap_hours": 0.5,
    "max_fix_attempts": 3,
    "max_iterations": 10,
    "model": None,
    "pause": False,
    "stop": False,
    "sleep_between_s": 0,
    "run_tests": True,
    "push": True,
    "gate_timeout_min": 30,
}


# ----------------------------------------------------------------- plumbing

def now() -> str:
    return f"{dt.datetime.now():%Y-%m-%d %H:%M:%S}"


def log(msg: str) -> None:
    line = f"[{now()}] {msg}"
    print(line, flush=True)
    with LOG.open("a") as fh:
        fh.write(line + "\n")


def knobs() -> dict:
    k = dict(DEFAULT_KNOBS)
    if KNOBS.exists():
        try:
            k.update(json.loads(KNOBS.read_text()))
        except json.JSONDecodeError as exc:
            log(f"knobs.json unreadable ({exc}); using defaults")
    else:
        KNOBS.write_text(json.dumps(k, indent=2) + "\n")
    return k


def status(**fields) -> None:
    data = {"updated": now(), **fields}
    STATUS.write_text(json.dumps(data, indent=2) + "\n")


def git(*args: str) -> str:
    return subprocess.run(["git", *args], cwd=REPO, capture_output=True,
                          text=True).stdout.strip()


def n_items() -> int:
    return len(re.findall(r"^- \[[ x!]\] ", ITEMS.read_text(), re.M))


def next_item() -> str | None:
    text = ITEMS.read_text()
    m = UNCHECKED.search(text)
    if not m:
        return None
    # continuation: indented lines, and blank lines that are followed by an indented line
    cont = re.match(r"(\n {4,}.*|\n(?=\n {4,}))*", text[m.end():])
    return text[m.start():m.end() + (cont.end() if cont else 0)]


# ---------------------------------------------------------- claude session

def _summarise_tool(name: str, inp: dict) -> str:
    if name == "Bash":
        return f"$ {inp.get('command', '')[:160]}"
    if name in ("Read", "Write", "Edit", "MultiEdit"):
        return f"{name} {inp.get('file_path', '')}"
    if name in ("Grep", "Glob"):
        return f"{name} {inp.get('pattern', '')} {inp.get('path', '')}"
    return f"{name} {json.dumps(inp)[:120]}"


def claude(prompt: str, label: str, cap_hours: float, model: str | None) -> str:
    """Run one session, streaming a readable log. Returns 'ok', 'timeout' or 'error'."""
    env = {k: v for k, v in os.environ.items() if k != "CLAUDECODE"}
    cmd = ["claude", "-p", prompt, "--dangerously-skip-permissions",
           "--output-format", "stream-json", "--verbose"]
    if model:
        cmd += ["--model", model]
    readable = HERE / f"session_{label}.log"
    raw = HERE / f"session_{label}.jsonl"
    log(f"claude session {label} (cap {cap_hours:.2f} h) -> {readable.name}")
    deadline = time.time() + cap_hours * 3600
    proc = subprocess.Popen(cmd, cwd=REPO, env=env, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True,
                            start_new_session=True)
    outcome = "ok"
    tools = 0
    with readable.open("w") as rd, raw.open("w") as rw:
        rd.write(f"# session {label} started {now()}\n\n")
        rd.flush()
        for line in proc.stdout:
            rw.write(line)
            rw.flush()
            if time.time() > deadline:
                outcome = "timeout"
                break
            try:
                ev = json.loads(line)
            except json.JSONDecodeError:
                rd.write(line)
                rd.flush()
                continue
            t = ev.get("type")
            if t == "assistant":
                for block in ev.get("message", {}).get("content", []):
                    if block.get("type") == "text" and block["text"].strip():
                        rd.write(f"\n{block['text'].rstrip()}\n")
                    elif block.get("type") == "tool_use":
                        tools += 1
                        rd.write(f"  > {_summarise_tool(block['name'], block.get('input', {}))}\n")
                        if tools % 25 == 0:
                            status(phase="session", session=label, tool_calls=tools,
                                   started=_started, item=_item_title)
                rd.flush()
            elif t == "result":
                rd.write(f"\n# result: {ev.get('subtype')} in {ev.get('duration_ms', 0)/1000:.0f}s, "
                         f"{ev.get('num_turns')} turns, cost ${ev.get('total_cost_usd', 0):.2f}\n")
                rd.flush()
                if ev.get("is_error"):
                    outcome = "error"
    if outcome == "timeout":
        log(f"session {label} hit the {cap_hours} h cap; killing it")
        try:
            os.killpg(proc.pid, signal.SIGTERM)
            proc.wait(timeout=30)
        except Exception:
            os.killpg(proc.pid, signal.SIGKILL)
    rc = proc.wait()
    if rc != 0 and outcome == "ok":
        outcome = "error"
    log(f"session {label}: {outcome} (exit {rc}, {tools} tool calls)")
    return outcome


# ------------------------------------------------------------------ prompts

def iteration_prompt(item: str, n: int, cap_hours: float) -> str:
    return f"""You are iteration {n} of a Ralph loop in the repository at {REPO}.
You have a hard wall-clock cap of {cap_hours:g} hours for this session; plan the work
so that PROGRESS.md is written well before that. If the item would not fit, do a
coherent part of it, mark it blocked, and say exactly what remains.

Read these two files first, in full:

--- TASK.md ---
{TASK.read_text()}

--- PROGRESS.md (most recent iterations at the bottom) ---
{PROGRESS.read_text()}

--- YOUR ONE ITEM for this iteration (from {ITEMS}) ---
{item}

Do exactly this item and nothing else. When finished:
1. Run `timeout 1800 python3 ralph_loops/{LOOP_NAME}/gate.py` and make it pass. A gate
   that hits the timeout has a hung test (a deadlock): find and fix it.
2. Append an `## Iteration {n} — <date time>` section to {PROGRESS} with
   `### Completed`, `### Blockers`, `### Next`, and update the
   `Current: k/{n_items()} SOLVED` line.
3. Mark the item in {ITEMS} as `- [x]` (done) or `- [!]` (blocked).
4. Do not commit and do not push; the driver commits and pushes. Never edit
   chez_scheme/, tests/golden/, tests/diff/, python/fixtures/ or the Python engine
   modules python/metacat/*.py (the gate checks them). racket/ may change only in the
   optional Racket item, and then the gate also runs the Racket suite.
5. If every item in {ITEMS} is now checked, add the exact line LOOP_COMPLETE at the
   end of PROGRESS.md.

This is a non-interactive `claude -p` session: it ends the moment you stop and wait.
Never put a command in the background, or schedule a wake-up, to wait for its result;
run it in the foreground (with a timeout long enough for it to finish) and do steps
1-5 yourself before your final message. Nothing will resume you later.
"""


def fix_prompt(tail: str, attempt: int, max_attempts: int) -> str:
    return f"""The regression gate in the repository at {REPO} fails after the last
Ralph-loop iteration (fix attempt {attempt} of {max_attempts}). Run
`python3 ralph_loops/{LOOP_NAME}/gate.py` (the frozen references and the Python engine
must be unchanged, then python/run-tests.sh, and tests/run-tests.sh if racket/ changed), find the cause, and fix it without removing, skipping or
weakening tests, and without regenerating golden traces or python/fixtures/ to match
a broken port.
Never edit chez_scheme/ or the Python engine modules. If you cannot fix it, say so in PROGRESS.md. The tail of
the failing run:

{tail}

Do not commit and do not push."""


# ------------------------------------------------------------ test + commit

def tests_pass() -> tuple[bool, str]:
    """Run the gate under a wall-clock limit (knob gate_timeout_min). A hung test (a
    deadlock) kills the gate's whole process group and counts as a failure, so the fix
    sessions see it instead of the loop waiting forever."""
    status(phase="tests", started=_started, item=_item_title)
    limit = float(knobs()["gate_timeout_min"]) * 60
    proc = subprocess.Popen(TEST_CMD, cwd=REPO, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, start_new_session=True)
    try:
        out, _ = proc.communicate(timeout=limit)
        timed_out = False
    except subprocess.TimeoutExpired:
        for sig in (signal.SIGTERM, signal.SIGKILL):
            try:
                os.killpg(proc.pid, sig)
                out, _ = proc.communicate(timeout=30)
                break
            except (ProcessLookupError, subprocess.TimeoutExpired):
                out = ""
        timed_out = True
    tail = "\n".join((out or "").splitlines()[-40:])
    if timed_out:
        tail += (f"\nGATE TIMED OUT after {limit / 60:g} min: a test hung (a deadlock?). "
                 "Find it: count the finished tests in the pytest progress line, and map "
                 "the count onto `pytest --collect-only -q`.")
    ok = proc.returncode == 0 and not timed_out
    log("tests " + ("passed" if ok else "FAILED") + f": {tail.splitlines()[-1] if tail else ''}")
    return ok, tail


def commit(n: int, item: str, suffix: str = "") -> None:
    title = re.sub(r"^- \[.\] ", "", item.splitlines()[0])
    title = re.sub(r"\*\*", "", title)[:70]
    git("add", "-A", "--", ".")
    if not git("status", "--porcelain"):
        log("nothing to commit")
        return
    msg = (f"Ralph {LOOP_NAME} iteration {n}: {title}{suffix}\n\n"
           f"Driver: ralph_loops/{LOOP_NAME}/loop.py\n\n"
           f"Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>")
    subprocess.run(["git", "commit", "-q", "-m", msg], cwd=REPO)
    log(f"committed {git('rev-parse', '--short', 'HEAD')}: {title}")


def push() -> None:
    """Push the current branch; a failure is logged and retried after the next commit."""
    if not knobs()["push"]:
        return
    try:
        proc = subprocess.run(["git", "push", "-q", "-u", "origin", "HEAD"], cwd=REPO,
                              capture_output=True, text=True, timeout=300)
    except subprocess.TimeoutExpired:
        log("push timed out; will retry after the next commit")
        return
    if proc.returncode == 0:
        log(f"pushed {git('rev-parse', '--short', 'HEAD')} to origin")
    else:
        log(f"push FAILED (will retry after the next commit): {proc.stderr.strip()[-300:]}")


def revert_code_keep_notes() -> None:
    keep_prefixes = KEEP_ON_REVERT
    for line in git("status", "--porcelain").splitlines():
        path = line[3:].strip()
        if path.startswith(keep_prefixes):
            continue
        if "?" in line[:2]:
            subprocess.run(["git", "clean", "-fdq", "--", path], cwd=REPO)
        else:
            subprocess.run(["git", "checkout", "-q", "--", path], cwd=REPO)
    log("reverted code; kept the loop folder and docs/")


# --------------------------------------------------------------------- main

_started = ""
_item_title = ""


def wait_while_paused() -> None:
    announced = False
    while knobs()["pause"]:
        if not announced:
            log("paused by knobs.json; polling every 60 s")
            status(phase="paused", started=_started, item="")
            announced = True
        time.sleep(60)
    if announced:
        log("unpaused")


def main() -> None:
    global _started, _item_title
    argv = sys.argv[1:]
    positional = [a for a in argv if not a.startswith("--")]
    if "--fresh" in argv:
        PROGRESS.write_text(PROGRESS.read_text().split("---")[0] + "---\n")
        log("PROGRESS.md reset")
    if "--dry-run" in argv:
        item = next_item()
        print(iteration_prompt(item, 1, knobs()["iteration_cap_hours"]) if item else "no items left")
        return
    dirty = git("status", "--porcelain", "--", "Metacat", *CODE_DIRS)
    if dirty and "--resume-wip" in argv:
        log("--resume-wip: starting on top of uncommitted work:\n" + dirty)
    elif dirty:
        log("uncommitted changes in code directories; commit or stash first:\n" + dirty)
        sys.exit(1)
    k = knobs()
    limit = int(positional[0]) if positional else int(k["max_iterations"])
    log(f"loop start: up to {limit} iterations, cap {k['iteration_cap_hours']} h each, "
        f"model {k['model'] or 'default'}")

    n_done = 0
    while n_done < limit:
        wait_while_paused()
        k = knobs()
        if k["stop"]:
            log("stop requested by knobs.json; exiting")
            break
        if "LOOP_COMPLETE" in PROGRESS.read_text():
            log("LOOP_COMPLETE found; stopping")
            break
        item = next_item()
        if item is None:
            log("no unchecked items left; stopping")
            break
        n = len(re.findall(r"^## Iteration ", PROGRESS.read_text(), re.M)) + 1
        _started = now()
        _item_title = re.sub(r"\*\*", "", item.splitlines()[0][6:])[:90]
        log(f"=== iteration {n}: {_item_title}")
        status(phase="session", session=f"it{n:02d}", started=_started,
               item=_item_title, tool_calls=0)
        before = ITEMS.read_text()
        outcome = claude(iteration_prompt(item, n, k["iteration_cap_hours"]),
                         f"it{n:02d}", k["iteration_cap_hours"], k["model"])
        if ITEMS.read_text() == before:
            mark = "- [!]"
            log(f"item not marked by the agent ({outcome}); marking {mark} so the loop advances")
            ITEMS.write_text(before.replace(item, item.replace("- [ ]", mark, 1), 1))
            with PROGRESS.open("a") as fh:
                fh.write(f"\n## Iteration {n} — {now()}\n### Completed\n- (driver) session ended "
                         f"with outcome `{outcome}` without marking the item\n### Blockers\n"
                         f"- see session_it{n:02d}.log\n### Next\n- revisit or re-open this item\n\n---\n")
        ok, tail = (True, "") if not k["run_tests"] else tests_pass()
        attempt = 0
        while not ok and attempt < int(k["max_fix_attempts"]):
            attempt += 1
            k = knobs()
            claude(fix_prompt(tail, attempt, int(k["max_fix_attempts"])),
                   f"it{n:02d}_fix{attempt}", k["fix_cap_hours"], k["model"])
            ok, tail = tests_pass()
        if ok:
            commit(n, item, "" if outcome == "ok" else f" ({outcome})")
        else:
            log("tests still failing after fixes:\n" + tail)
            revert_code_keep_notes()
            commit(n, item, " (code reverted, notes kept)")
        push()
        n_done += 1
        status(phase="between", started="", item="", last_iteration=n, outcome=outcome)
        if k["sleep_between_s"]:
            time.sleep(float(k["sleep_between_s"]))
    log(f"loop end: {n_done} iteration(s) this run")
    status(phase="idle", started="", item="")


if __name__ == "__main__":
    main()
