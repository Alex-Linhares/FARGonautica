# ralph_loops/

Both ports of Metacat in this repository were written by **Ralph loops**. A Ralph loop is a
small Python driver that works through a fixed list of work items. For each item it starts
one fresh, non-interactive Claude Code session (`claude -p`), then runs a regression gate,
lets Claude repair a failing gate a limited number of times, and finally commits and pushes.
No session carries memory to the next one. State lives in files: an immutable goal
(`TASK.md`), the item list (`iterations.md`) and a log that every session appends to
(`PROGRESS.md`). [`loop0001/`](loop0001/README.md) built the Racket port in 18 iterations
(2026-10-02/03). [`loop0002/`](loop0002/README.md) built the Python port in 18 iterations
(2026-10-03/04). This folder keeps both loops complete: driver, gate, plan, progress log,
driver log and the full transcript of every session.

## Why "fresh context"

[`ralph_loop_guide.md`](ralph_loop_guide.md) is the general method this repository follows.
As a session goes on, its context fills up and the model gets slower and more confused
about earlier decisions. A Ralph loop starts each item at an empty context and passes
knowledge forward only through files. Each session reads `TASK.md` and the whole
`PROGRESS.md`, works on exactly **one** item, records what it did, what blocked it and what
the next session should know, and marks the item done (`[x]`) or blocked (`[!]`). The loop
ends when the agent writes the sentinel line `LOOP_COMPLETE` into `PROGRESS.md`, or when no
unchecked item is left.

## Folder convention

Each loop gets its own numbered folder, `loopNNNN/`, holding:

| File | Written by | Purpose |
| --- | --- | --- |
| `TASK.md` | the owner, before the loop | The goal, philosophy, acceptance criteria, constraints and toolchain. Not changed while the loop runs. |
| `iterations.md` | the owner; sessions tick items | The ordered work items, each `- [ ]`, `- [x]` (done) or `- [!]` (blocked), with that item's tests. |
| `PROGRESS.md` | each session appends | One `## Iteration N` section per session (`### Completed`, `### Blockers`, `### Next`) and a `Current: k/N SOLVED` line. |
| `gate.py` | the owner | The regression gate. Exit 0 means green. |
| `loop.py` | the owner | The driver (below). |
| `knobs.json` | the owner, live | Runtime settings, re-read every iteration. |
| `loop.log` | the driver | One line per event: session start/end, tool-call count, gate result, commit, push. |
| `nohup.out` | `nohup` | The driver's stdout. In both loops it is identical to `loop.log`. |
| `status.json` | the driver | What is running right now (phase, session, item, start time). Both loops end with `"phase": "idle"`. |
| `session_itNN.log` | the driver | A readable transcript of session NN: the agent's text and a one-line summary of each tool call, ending with a `# result:` line (duration, turns, reported cost). |
| `session_itNN.jsonl` | the driver | The raw `stream-json` output of the same session. |
| `session_itNN_fixK.*` | the driver | The same, for fix attempt K after a failed gate. |

## The driver, `loop.py`

```bash
cd ralph_loops/loop0002
python3 loop.py              # up to knobs["max_iterations"] iterations
python3 loop.py 3            # at most 3 iterations
python3 loop.py --dry-run    # print the prompt for the next item and exit
python3 loop.py --fresh      # reset PROGRESS.md first
```

Each iteration does the following:

1. **Pick the item.** The first `- [ ]` item in `iterations.md`, with its indented lines.
2. **Run one session.** It runs `claude -p <prompt> --dangerously-skip-permissions
   --output-format stream-json --verbose` with a wall-clock cap. The prompt contains
   `TASK.md`, all of `PROGRESS.md` and the one item, and tells the agent to run the gate,
   append to `PROGRESS.md`, tick the item, never commit or push, and write `LOOP_COMPLETE`
   when every item is checked. The stream is written to `session_itNN.jsonl` and
   summarised in `session_itNN.log`. A session that exceeds the cap is killed.
3. **Enforce marking.** If the session ended without changing `iterations.md`, the driver
   marks the item `[!]` and appends a `(driver)` entry to `PROGRESS.md`, so the loop
   cannot get stuck on one item.
4. **Gate.** It runs `python3 ralph_loops/loopNNNN/gate.py`.
5. **Fix attempts.** If the gate fails, it starts up to `max_fix_attempts` short fix
   sessions (each capped at `fix_cap_hours`). Each one gets the tail of the failing output
   and is told not to remove, skip or weaken tests, and not to regenerate goldens or
   fixtures to match a broken port. The gate runs again after each one.
6. **Commit.** If the gate is green, `git add -A` and commit as `Ralph loopNNNN iteration N:
   <item title>`. If it is still red, the driver reverts the code but keeps the loop folder
   and `docs/` (the notes), and commits with "(code reverted, notes kept)". Neither loop
   ever needed this path.
7. **Push.** `git push origin HEAD`, when `push` is on. A failed push is retried after the
   next commit.
8. Stop on `LOOP_COMPLETE`, when no items are left, or when `stop` is set.

Before starting, the driver refuses to run if the code directories have uncommitted
changes.

### `knobs.json`

The driver re-reads these at the start of every iteration and every fix attempt, so they
can be changed while a loop runs:

| Knob | Both loops used | Meaning |
| --- | --- | --- |
| `iteration_cap_hours` | 3.0 | Wall-clock cap per session. |
| `fix_cap_hours` | 0.5 | Cap per fix session. |
| `max_fix_attempts` | 3 | Fix sessions before the code is reverted. |
| `max_iterations` | 18 (loop0001's file now), 30 (loop0002) | Iterations per run of the driver. Both `loop.log`s start with "up to 30 iterations". |
| `model` | `null` | Passed to `claude --model`. `null` means the default model. |
| `pause` | false | Finish the current iteration, then poll every 60 s until it is false again. |
| `stop` | false | Finish the current iteration, commit, exit. |
| `sleep_between_s` | 0 | Pause between iterations. |
| `run_tests` | true | `false` skips the gate (not recommended). |
| `push` | true | Push after each commit (to `git@github.com:fargonauts/metacat.git`). |

### Watching a running loop

```bash
tail -f ralph_loops/loop0002/loop.log           # events
tail -f ralph_loops/loop0002/session_it07.log   # the running session, readable
cat ralph_loops/loop0002/status.json            # phase, item, tool calls so far
```

### The gates

| | `loop0001/gate.py` | `loop0002/gate.py` |
| --- | --- | --- |
| Original untouched | The git tree of `chez_scheme/original/` equals the import commit `9f072c0:Metacat`, with no uncommitted edits and no untracked files. | Same tree-hash check. |
| Frozen references | — | `chez_scheme/`, `racket/`, `tests/golden/` and `tests/diff/` must equal commit `9684b16` (the end of loop0001), committed or not, and have no untracked files. `README.md` files are excluded, so documentation can be added later. |
| Tests | `bash tests/run-tests.sh`: the Racket tests, GUI tests under Xvfb and the Chez oracle checks. About 10 minutes by the end. | `bash python/run-tests.sh`: 1412 tests in about 7 minutes at the final audit. |

## The two loops

| | [loop0001](loop0001/README.md): Racket | [loop0002](loop0002/README.md): Python |
| --- | --- | --- |
| Goal | A faithful Racket port with a racket/gui interface, checked against the original running headless under Chez Scheme 10 | A test-driven translation to Python 3.12 (standard library only) with a tkinter GUI, checked against the same oracle |
| Items | 18 (00–17) | 18 (00–17) |
| Started | 2026-10-02 20:29:43 | 2026-10-03 18:20:07 |
| Ended | 2026-10-03 05:05:48 | 2026-10-04 03:41:27 |
| Wall-clock | about 8 h 36 min | about 9 h 21 min |
| Sessions | 18, no fix sessions | 18, plus 1 fix session (item 14) |
| Tool calls (sum over sessions) | 1,575 | 2,032 (plus 11 in the fix session) |
| Reported cost (sum of `# result:` lines) | about $98 | about $152 |
| Outcome | 17/18 SOLVED. Item 17 (final audit) was marked `[!]` by the driver (see lessons), but its work passed the gate and was committed. All 109 goldens and 720 extra-seed runs match the oracle. | 18/18 SOLVED, `LOOP_COMPLETE`. All 109 goldens and 720 extra-seed runs match the oracle byte for byte. |
| Ended because | "no unchecked items left" | "LOOP_COMPLETE found" |

Every one of the 36 iterations committed and pushed on the first gate run, except loop0002
iteration 15 (item 14), which needed one fix session.

## Lessons learned

These come from the loops' logs and from the changes made to the driver between the two
loops (`diff loop0001/loop.py loop0002/loop.py`).

- **A `claude -p` session must not put work in the background and wait for it.** In
  loop0001's final audit (item 17), the session started the 10-minute gate in the
  background, scheduled a wake-up "in case the gate notification doesn't arrive", wrote
  "waiting for the notification" and stopped (`session_it18.log`). A non-interactive
  session ends when the agent stops, so nothing resumed it. `PROGRESS.md` and
  `iterations.md` were never updated, and the driver marked the item `[!]`, even though the
  audit's changes passed the gate when the driver ran it. For loop0002, the prompt says:
  "Never put a command in the background, or schedule a wake-up, to wait for its result;
  run it in the foreground … Nothing will resume you later." Loop0002's own item 17 adds
  "Do this all in the foreground and finish the PROGRESS entry yourself", and it did.
  (Loop0002's fix session waited with a foreground `until grep …; do sleep 5; done` loop,
  which is fine.)
- **GUI tests must never reach the owner's screen.** The owner's desktop is Wayland. In
  loop0001 item 15, `xvfb-run racket …` still opened windows **on the owner's screen**,
  three times for a few seconds each, because GTK prefers `WAYLAND_DISPLAY` over Xvfb's
  `DISPLAY`. The fix is `env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a …` everywhere,
  and a GUI test that refuses to run while `WAYLAND_DISPLAY` is set (see
  `docs/anomalies_and_quirks.md`). Between loop0001 iterations 12 and 13 there is also a
  hand commit, "loop0001: GUI tests run under xvfb-run, never on the owner's screen". The
  loop0002 driver removes `WAYLAND_DISPLAY` and sets `GDK_BACKEND=x11` and
  `QT_QPA_PLATFORM=offscreen` for itself and every session and gate it starts. Every GUI
  script must also exit by itself, or it hangs the gate.
- **Freeze what the loop must not touch, and check it in the gate.** Loop0001's gate
  protected only the original. Loop0002 translated *against* the finished Racket port, the
  oracle, the goldens and the Chez batteries, so its gate checks all of them against
  `9684b16`. The prompts repeat the rule, and the fix prompt forbids regenerating goldens or
  fixtures to make a broken port pass.
- **Fix sessions work for harness bugs.** Loop0002's one gate failure was in the test
  harness, not the port. `render_views.py` grabbed windows from a shared Xvfb screen in
  parallel processes, and another scene's window was sometimes raised over the one being
  grabbed. A 7.5-minute fix session added a file lock around the grab and logged the race
  in the anomalies file.
- **Make sessions record evidence, not claims.** Both `TASK.md`s require tests written
  before the code, and every iteration reports whether that really happened, including
  "Tests first, honestly: no" (loop0001 item 16). Sessions also ran mutation checks (break
  the code on purpose, see a test fail, restore it) and wrote down what they *saw* in each
  rendered PNG. Stale compiled code fooled mutation checks in both ports: Racket's `.zo`
  files ignore edits to included files, and a same-size restore can run a stale `.pyc`.
  Both cases are in `docs/anomalies_and_quirks.md`.
- **Hand forward through `### Next`.** Notes like "item 11 must not skip the golden
  first-answer check" (loop0001 item 09) or "rules.ss must register `format-slipnode`"
  (loop0001 item 03) are how one session warns the next. The second was missed until the
  final audit found and fixed it, so a final audit item is worth having.
- **Sessions can use subagents.** In loop0002, items 06–10 and 14 had subagents translate
  files, in parallel and often into a staging directory, while the main session wrote the
  tests. The main session then integrated and reviewed their work. The audits of both
  loops used read-only subagents to check the docs against the code.
- **Shared machines distort timings.** Loop0002's speed measurements (item 12) and its audit ran
  alongside another user's jobs (a load average of about 30 during the audit). Re-running `docs/python-run-times.md` on an
  idle machine was left as a follow-up.

## Related

- [`../docs/follow-ups.md`](../docs/follow-ups.md): what each loop's final audit left for a
  next loop.
- [`../docs/README.md`](../docs/README.md): the documents the loops wrote.
- [`../CLAUDE.md`](../CLAUDE.md): the repository rules that every session reads.
