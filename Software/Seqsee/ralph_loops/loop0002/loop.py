#!/usr/bin/env python3
"""Ralph loop runner (fresh-context pattern). See ../RALPH_LOOP_GUIDE.md.

Usage (from this folder):
    python3 loop.py                 # 10 iterations, resume from PROGRESS.md
    python3 loop.py 60              # 60 iterations
    python3 loop.py 60 --fresh      # reset PROGRESS.md first
    python3 loop.py 20 TASK.md PROGRESS.md
    python3 loop.py 1 --dry-run     # print the prompt, run nothing
    python3 loop.py 60 --no-push    # commit locally only

Each iteration: pick the next `[ ]` item from iterations.md (falls back to
near_misses.md), run a fresh `claude -p` session, run the regression tests,
commit if green; otherwise ask Claude to fix (up to 3 attempts), then revert
code but keep PROGRESS.md findings. Exits when PROGRESS.md contains a line
reading exactly LOOP_COMPLETE.
"""

import argparse
import os
import datetime
import re
import subprocess
import sys
import threading
from pathlib import Path

LOOP_DIR = Path(__file__).resolve().parent
REPO = Path(
    subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        cwd=LOOP_DIR, capture_output=True, text=True, check=True,
    ).stdout.strip()
)
PY_DIR = REPO / "python"
LOG_DIR = LOOP_DIR / "logs"
# Paths (relative to REPO) the loop may stage and revert.
CODE_PATHS = ["python"]
STATE_PATHS = [str(LOOP_DIR.relative_to(REPO)), str(LOOP_DIR.parent.relative_to(REPO))]
TEST_CMD = [sys.executable, "-m", "pytest", "-q", "-p", "no:cacheprovider"]
MAX_FIX_ATTEMPTS = 3
MAX_REVERTS_PER_ITEM = 2
PROGRESS_TAIL_CHARS = 15000
TEST_OUTPUT_TAIL_CHARS = 6000
COAUTHOR = "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"

ALLOWED_TOOLS = [
    "Read", "Edit", "Write", "Glob", "Grep",
    "Bash(python3 -m pytest:*)", "Bash(python3 -c:*)", "Bash(python3 -m seqsee:*)",
    "Bash(python3 oracle/regen.py:*)", "Bash(python3 python/oracle/regen.py:*)",
    "Bash(python3 oracle/baseline.py:*)", "Bash(python3 python/oracle/baseline.py:*)",
    "Bash(oracle/run_perl.sh:*)", "Bash(python/oracle/run_perl.sh:*)",
    "Bash(./oracle/run_perl.sh:*)", "Bash(cd:*)",
    "Bash(oracle/run_perl_gui.sh:*)", "Bash(python/oracle/run_perl_gui.sh:*)",
    "Bash(./oracle/run_perl_gui.sh:*)", "Bash(gs:*)", "Bash(QT_QPA_PLATFORM=offscreen python3:*)",
    "Bash(ls:*)", "Bash(grep:*)", "Bash(find:*)", "Bash(cat:*)", "Bash(head:*)",
    "Bash(tail:*)", "Bash(wc:*)", "Bash(sed -n:*)", "Bash(mkdir:*)",
    "Bash(git status:*)", "Bash(git diff:*)", "Bash(git log:*)", "Bash(git show:*)",
]

ITEM_RE = re.compile(r"^\s*-\s*\[(?P<mark>[ xX!])\]\s*(?P<text>.+?)\s*$", re.M)
COMPLETE_RE = re.compile(r"^LOOP_COMPLETE\s*$", re.M)


# ---------------------------------------------------------------- helpers

def git(*args, check=True):
    return subprocess.run(
        ["git", *args], cwd=REPO, capture_output=True, text=True, check=check
    ).stdout


def now():
    return datetime.datetime.now().strftime("%Y-%m-%d %H:%M")


def iterations_file():
    for name in ("iterations.md", "near_misses.md"):
        p = LOOP_DIR / name
        if p.exists():
            return p
    sys.exit("No iterations.md (or near_misses.md) in " + str(LOOP_DIR))


def items(path):
    return [(m["mark"], m["text"]) for m in ITEM_RE.finditer(path.read_text())]


def next_item(path):
    for mark, text in items(path):
        if mark == " ":
            return text
    return None


def is_complete(progress):
    return progress.exists() and bool(COMPLETE_RE.search(progress.read_text()))


def iteration_count(progress):
    nums = re.findall(r"^## Iteration (\d+)", progress.read_text(), re.M)
    return max(map(int, nums), default=0)


def update_status_line(progress, iters):
    its = items(iters)
    done = sum(1 for m, _ in its if m in "xX")
    blocked = sum(1 for m, _ in its if m == "!")
    text = progress.read_text()
    new = re.sub(
        r"^- \*\*Current\*\*:.*$",
        f"- **Current**: {done}/{len(its)} SOLVED, {blocked} blocked",
        text, count=1, flags=re.M,
    )
    if new != text:
        progress.write_text(new)


def mark_item(iters, item_text, mark):
    text = iters.read_text()
    pat = re.compile(r"^(\s*-\s*)\[ \](\s*" + re.escape(item_text) + r")", re.M)
    iters.write_text(pat.sub(lambda m: f"{m[1]}[{mark}]{m[2]}", text, count=1))


def tail(s, n):
    return s if len(s) <= n else "…(truncated)…\n" + s[-n:]


# ---------------------------------------------------------------- claude

def run_claude(prompt, log_path, args):
    cmd = ["claude", "-p", "--output-format", "text"]
    if args.model:
        cmd += ["--model", args.model]
    if args.yolo:
        cmd += ["--dangerously-skip-permissions"]
    else:
        cmd += ["--permission-mode", "acceptEdits", "--allowedTools", ",".join(ALLOWED_TOOLS)]

    LOG_DIR.mkdir(exist_ok=True)
    print(f"  → claude session (log: {log_path.relative_to(LOOP_DIR)})", flush=True)
    with open(log_path, "a") as log:
        log.write(f"\n===== {now()} =====\n--- PROMPT ---\n{prompt}\n--- OUTPUT ---\n")
        log.flush()
        proc = subprocess.Popen(
            cmd, cwd=REPO, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, text=True,
        )
        timer = threading.Timer(args.timeout * 60, proc.kill)
        timer.start()
        try:
            proc.stdin.write(prompt)
            proc.stdin.close()
            for line in proc.stdout:
                log.write(line)
                log.flush()
                if not args.quiet:
                    print("    " + line, end="", flush=True)
            proc.wait()
        except KeyboardInterrupt:
            proc.kill()
            raise
        finally:
            timer.cancel()
    if proc.returncode != 0:
        print(f"  ! claude exited with code {proc.returncode}", flush=True)
    return proc.returncode


def build_prompt(task_path, progress_path, iters_path, item, iter_no):
    progress = progress_path.read_text()
    item_line = (
        f"Your item for this iteration:\n\n    {item}\n"
        if item else
        "All items in iterations.md are checked. Verify the completion conditions "
        "in TASK.md; if they are met, append LOOP_COMPLETE to PROGRESS.md. Otherwise "
        "add the missing work as new items.\n"
    )
    return f"""You are one iteration of a Ralph loop (fresh-context pattern). You have no
memory of earlier iterations; all state lives in files.

Repository root (cwd): {REPO}
TASK (immutable):      {task_path}
PROGRESS (append):     {progress_path}
Work items:            {iters_path}
Regression tests:      cd {PY_DIR} && python3 -m pytest

This is iteration {iter_no}. {item_line}
Steps:
1. Read PROGRESS.md (whole file if the tail below is truncated) for earlier decisions.
2. Do ONLY this item, following TASK.md: write the tests first, then implement until the full suite passes.
3. Mark the item [x] (done) or [!] (blocked) in {iters_path.name}.
4. Append a section "## Iteration {iter_no} — <YYYY-MM-DD HH:MM>" to PROGRESS.md with
   ### Completed / ### Decisions / ### Blockers / ### Next.
5. Do NOT run git commands that change state (no add/commit/checkout/reset); the runner commits.

===== TASK.md =====
{task_path.read_text()}

===== PROGRESS.md (tail) =====
{tail(progress, PROGRESS_TAIL_CHARS)}
"""


def build_fix_prompt(task_path, progress_path, item, test_output, attempt):
    return f"""You are fixing a failing regression suite in a Ralph loop iteration
(attempt {attempt}/{MAX_FIX_ATTEMPTS}). Repository root (cwd): {REPO}.
The previous session worked on this item: {item or '(completion check)'}
Read {task_path} for conventions, and the last section of {progress_path} for what was attempted.

The test suite (cd {PY_DIR} && python3 -m pytest) is failing:

{tail(test_output, TEST_OUTPUT_TAIL_CHARS)}

Fix the code (or the tests if they are wrong about the Perl behaviour) so the WHOLE
suite passes. Do not delete or weaken tests just to make them pass. Add a short note
under the current iteration section in PROGRESS.md about what you fixed.
Do NOT run git commands that change state.
"""


# ---------------------------------------------------------------- tests & git

def run_tests():
    env = {**os.environ, "QT_QPA_PLATFORM": "offscreen"}
    r = subprocess.run(TEST_CMD, cwd=PY_DIR, capture_output=True, text=True, env=env)
    out = r.stdout + r.stderr
    ok = r.returncode in (0, 5)  # 5 = no tests collected
    return ok, out


def dirty(paths):
    return git("status", "--porcelain", "--", *paths).strip()


PUSH = True


def push():
    if not PUSH or not git("remote").strip():
        return
    r = subprocess.run(["git", "push", "-q"], cwd=REPO, capture_output=True, text=True)
    if r.returncode == 0:
        print("  ✓ pushed")
    else:
        print(f"  ! push failed (will retry after the next commit): {r.stderr.strip()}")


def commit(message, paths):
    git("add", "-A", "--", *paths)
    if not git("diff", "--cached", "--name-only").strip():
        print("  (nothing to commit)")
        return False
    git("commit", "-q", "-m", f"{message}\n\n{COAUTHOR}")
    print(f"  ✓ committed: {message}")
    push()
    return True


def warn_out_of_scope():
    allowed = tuple(p.rstrip("/") + "/" for p in CODE_PATHS + STATE_PATHS)
    stray = []
    for line in git("status", "--porcelain").splitlines():
        path = line[3:].split(" -> ")[-1]
        if not path.startswith(allowed):
            stray.append(line)
    if stray:
        print("  ! changes outside the loop's scope were left unstaged:\n    "
              + "\n    ".join(stray))


def revert_code(base, keep_files):
    saved = {p: p.read_text() for p in keep_files if p.exists()}
    git("checkout", base, "--", *CODE_PATHS, check=False)
    git("clean", "-fdq", "--", *CODE_PATHS)
    for p, text in saved.items():
        p.write_text(text)


# ---------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("iterations", nargs="?", type=int, default=10)
    ap.add_argument("task", nargs="?", default=str(LOOP_DIR / "TASK.md"))
    ap.add_argument("progress", nargs="?", default=str(LOOP_DIR / "PROGRESS.md"))
    ap.add_argument("--fresh", action="store_true", help="reset PROGRESS.md before starting")
    ap.add_argument("--yolo", action="store_true", help="run claude with --dangerously-skip-permissions")
    ap.add_argument("--model", help="model for the claude sessions")
    ap.add_argument("--timeout", type=float, default=60, help="minutes per claude session (default 60)")
    ap.add_argument("--quiet", action="store_true", help="don't echo claude output (still logged)")
    ap.add_argument("--no-push", action="store_true", help="don't git push after each commit")
    ap.add_argument("--dry-run", action="store_true", help="print the next prompt and exit")
    args = ap.parse_args()
    global PUSH
    PUSH = not args.no_push

    task_path = Path(args.task).resolve()
    progress_path = Path(args.progress).resolve()
    iters_path = iterations_file()
    keep_files = [progress_path, iters_path]

    if args.fresh:
        n = len(items(iters_path))
        progress_path.write_text(
            f"# Progress Log\n\n## Ralph Loop {LOOP_DIR.name.removeprefix('loop')} Status\n"
            f"- **Started**: {datetime.date.today()}\n- **Target**: {n} items\n"
            f"- **Current**: 0/{n} SOLVED\n\n---\n"
        )

    if args.dry_run:
        print(build_prompt(task_path, progress_path, iters_path,
                           next_item(iters_path), iteration_count(progress_path) + 1))
        return

    if dirty(CODE_PATHS + STATE_PATHS):
        ok, out = run_tests()
        if not ok:
            sys.exit("Uncommitted changes and failing tests; fix or commit first.\n" + tail(out, 3000))
        commit(f"{LOOP_DIR.name}: baseline before loop run", CODE_PATHS + STATE_PATHS)

    reverts = {}
    for i in range(1, args.iterations + 1):
        if is_complete(progress_path):
            print("LOOP_COMPLETE found in PROGRESS.md; done.")
            return
        item = next_item(iters_path)
        iter_no = iteration_count(progress_path) + 1
        print(f"\n[{now()}] run {i}/{args.iterations} — iteration {iter_no}: {item or 'completion check'}", flush=True)

        base = git("rev-parse", "HEAD").strip()
        iters_before = iters_path.read_text()
        log_path = LOG_DIR / f"iter{iter_no:04d}.log"

        run_claude(build_prompt(task_path, progress_path, iters_path, item, iter_no), log_path, args)
        warn_out_of_scope()

        ok, out = run_tests()
        attempt = 0
        while not ok and attempt < MAX_FIX_ATTEMPTS:
            attempt += 1
            print(f"  ✗ tests failing — fix attempt {attempt}/{MAX_FIX_ATTEMPTS}", flush=True)
            run_claude(build_fix_prompt(task_path, progress_path, item, out, attempt), log_path, args)
            ok, out = run_tests()

        label = (item or "completion check")[:70]
        if ok:
            print("  ✓ tests pass")
            update_status_line(progress_path, iters_path)
            commit(f"{LOOP_DIR.name} iter {iter_no}: {label}", CODE_PATHS + STATE_PATHS)
            continue

        # Still red: revert code, keep the findings, un-check the item.
        print("  ✗ tests still failing — reverting code, keeping PROGRESS.md", flush=True)
        revert_code(base, keep_files)
        iters_path.write_text(iters_before)
        reverts[item] = reverts.get(item, 0) + 1
        note = f"\n### Runner: iteration reverted ({now()})\nTests still failing after {MAX_FIX_ATTEMPTS} fix attempts; code changes reverted.\n```\n{tail(out, 2000)}\n```\n"
        if item and reverts[item] >= MAX_REVERTS_PER_ITEM:
            mark_item(iters_path, item, "!")
            note += f"Item marked [!] after {reverts[item]} reverted attempts.\n"
        with open(progress_path, "a") as f:
            f.write(note)
        update_status_line(progress_path, iters_path)
        commit(f"{LOOP_DIR.name} iter {iter_no}: REVERTED {label}", STATE_PATHS)

    print(f"\nFinished {args.iterations} runs. Next item: {next_item(iters_path)}")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit("\nInterrupted; working tree left as-is (nothing committed for this iteration).")
