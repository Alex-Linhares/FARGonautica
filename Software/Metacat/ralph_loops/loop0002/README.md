# loop0002: Metacat 1.2 to Python

This Ralph loop wrote the Python port in [`python/`](../../python/README.md): Python 3.12,
standard library only, with a tkinter GUI. It ran 18 fresh `claude -p` sessions, one per
item, plus one fix session, from **2026-10-03 18:20** to **2026-10-04 03:41** (about
9 h 21 min). It translated against references that the gate kept frozen: the original, the
Chez oracle, the finished Racket port from [loop0001](../loop0001/README.md), the 109 golden
traces and the Chez differential batteries. Every expected value in its tests came from
Chez. The loop ended with `LOOP_COMPLETE` and 18/18 items solved. The Python port
reproduces the oracle byte for byte on the 109 goldens, on 720 extra-seed runs, on the
CLI's output and on every battery, and also when every GUI window is attached. See
[`../README.md`](../README.md) for how Ralph loops and the driver work in general.

## The goal (`TASK.md`)

"Metacat in Python — a test-driven translation, with a Tk GUI." The principles:
- **The original is the specification; the oracle decides.** "Roughly like Metacat" is not
  the goal. The goal is the same codelets, structures, temperatures, answers and commentary
  for the same seed.
- **Test-driven translation.** For each Chez battery in `tests/diff/`, Chez's output is
  frozen per test into `python/fixtures/`. Each test is translated to pytest, and only then
  is the code translated until the test passes.
- **Faithful, not improved.** Bugs, crashes and halts of the original are reproduced. Each
  place where Python reproduces a quirk is marked with a `# chez:` or `# 1.2:` comment.
- **Readable Python, Scheme-shaped structure.** One module per `.ss` file, one function per
  definition, a fixed name mapping (`foo-bar?` → `foo_bar_p`), and docstrings that name
  the origin.
- **The Racket port is a worked translation, not the oracle.** When the two disagree, Chez
  wins.
- **The engine never imports tkinter. Look at what you draw. Log the strange.**

`TASK.md` also lists the frozen references, the target layout of `python/`, the expected
speed (20–100× slower than Chez), the need to run golden runs in parallel to keep the gate
under about 15 minutes, and the rule that Tk windows only ever open under `xvfb-run`.

## Items and outcomes

Timings come from [`loop.log`](loop.log). "Session" is the Claude session's wall-clock
time. "Total" runs from session start to commit, so it includes the gate (and, for
iteration 15, the fix session). Cost is the figure on the last line of each
`session_itNN.log`.

| Iter. | Item | Outcome | Started | Session | Total | Tool calls | Cost |
| ---: | --- | --- | --- | ---: | ---: | ---: | ---: |
| 1 | 00 Skeleton and the fixture pipeline | solved | 10-03 18:20 | 7 min | 7 min | 22 | $0.86 |
| 2 | 01 The translation plan | solved | 18:27 | 17 min | 17 min | 70 | $5.08 |
| 3 | 02 Chez semantics: `chez.py` | solved | 18:44 | 21 min | 22 min | 63 | $5.46 |
| 4 | 03 Objects, sugar and utilities | solved | 19:06 | 24 min | 25 min | 71 | $5.58 |
| 5 | 04 Constants, setup, coderack, descriptions | solved | 19:32 | 49 min | 50 min | 72 | $8.00 |
| 6 | 05 Slipnet and images | solved | 20:22 | 18 min | 18 min | 65 | $4.77 |
| 7 | 06 Workspace | solved | 20:40 | 25 min | 25 min | 126 | $10.33 |
| 8 | 07 Bonds, groups, concept mappings | solved | 21:05 | 18 min | 19 min | 159 | $8.90 |
| 9 | 08 Bridges and breakers | solved | 21:24 | 33 min | 34 min | 107 | $10.04 |
| 10 | 09 Rules and answers | solved | 21:58 | 32 min | 34 min | 174 | $13.66 |
| 11 | 10 Themes, justification, trace, jootsing, memory | solved: **109/109 goldens match** | 22:33 | 32 min | 34 min | 278 | $18.77 |
| 12 | 11 Full runs, the trace writer and the CLI | solved | 23:07 | 28 min | 31 min | 62 | $4.46 |
| 13 | 12 Extra seeds and speed | solved: **720/720 extra seeds match** | 23:38 | 55 min | 61 min | 76 | $5.07 |
| 14 | 13 The SGL interpreter on tkinter | solved | 10-04 00:39 | 23 min | 29 min | 68 | $6.90 |
| 15 | 14 The panels | solved after **1 fix session** | 01:08 | 41 min | 61 min | 388 (+11) | $26.57 (+$0.47) |
| 16 | 15 The control panel and windows | solved | 02:09 | 31 min | 38 min | 90 | $9.33 |
| 17 | 16 Packaging and docs | solved | 02:47 | 11 min | 18 min | 28 | $1.99 |
| 18 | 17 Final audit | solved, `LOOP_COMPLETE` | 03:05 | 28 min | 36 min | 113 | $5.79 |

In total there were 2,032 tool calls in the item sessions plus 11 in the fix session. The
reported cost was about $152.

**The one gate failure** (iteration 15, item 14). The gate failed after the panels session.
The fix session (`session_it15_fix1.log`, about 7.5 minutes) found that the failure was in
the test harness, not the port. `python/tests/render_views.py` renders its scenes as
parallel processes on one Xvfb display and grabs windows from the screen. Under load,
another scene's window was sometimes raised over the Temperature window before the grab,
so the picture came out blank. The fix added a file lock around the raise-and-grab step,
without weakening any test, and logged the race in `docs/anomalies_and_quirks.md`.

## Notable findings (from `PROGRESS.md`)

- **The fixture pipeline** (item 00). `python/oracle/capture.py` runs each Chez battery
  through the unedited `chez_scheme/oracle/diff-eval.ss` and splits the output per test.
  The test names come from Chez's own reader. It checks that joining the pieces gives
  Chez's output back byte for byte. The first capture held 10 batteries, 604 tests and
  168 MB of plain text (about 13 MB gzipped), with a freshness test that re-captures them.
- **The plan** (item 01, [`docs/python-translation-plan.md`](../../docs/python-translation-plan.md)).
  Four object representations were prototyped and benchmarked against the Chez fixtures.
  **C3** was chosen: a class per object, a message dictionary and the original
  `(self, msg, *args)` protocol, at about 173 ns per message.
- **`chez.py`** (item 02). A new battery of 63 tests captured the vectors the frozen
  batteries lacked. They cover the random-number generator (every draw and state over 15
  seeds × 54 arguments), arithmetic over exact, inexact, signed-zero and infinite operands,
  rounding, `tanh`/`expt`, `number->string` on thousands of doubles and near ties, the
  printer on every character below 256, and `map`/`sort` call orders. Chez's printer
  rounds a halfway last digit up, while Python's `repr` rounds it to even.
- **Whole runs match before the run loop existed** (item 10). With a test-side driver, all
  109 goldens matched byte for byte. Item 11 moved the run loop into the package
  (`metacat/run.py`, `__main__.py`, `trace_writer.py`). There, all 109 match in about 35 s
  on 32 forks, and a run stopped and resumed with `go` is the same run.
- **Speed** (item 12, [`docs/python-run-times.md`](../../docs/python-run-times.md)). The
  port is about 9× slower than Chez per codelet, well inside the 20–100× that `TASK.md`
  expected. Seven speed-ups, each keeping every trace identical, cut CPU time by 25–37% and
  Python function calls by about a third.
- **The SGL interpreter is checked against the original's Tcl** (item 13). New oracle
  scripts record the Tk commands that the unedited `sgl-interpreter.ss` sends for a fixture
  (318 commands per viewport). `metacat/gui/sgl.py` must make the same canvas operations.
- **The panels** (item 14). Four subagents translated the `*-graphics.ss` files in
  parallel against a host contract that the main session wrote first. `render_views.py`
  renders 8 scenes (56 pictures) on real Tk canvases. They match the Racket screenshots item
  for item, except that Tk's fonts are a little larger. With all views attached, every
  golden still matches.
- **The GUI** (item 15). The engine runs in a worker thread with a queue, because a
  suspend breaks out from *inside* a codelet and only a parked thread can resume there, so
  `after`-driven stepping wouldn't work. tkinter's cross-thread calls segfaulted in
  `mainloop`, so all Tk calls from other threads go through `swl.ThreadSafeTk`. The GUI is
  driven through its own widgets under Xvfb (`python/tests/drive_gui.py`), and every GUI
  run's trace equals its golden.
- **Packaging** (item 16). `python3 -m metacat`, `python3 -m metacat.gui` and
  `pip install -e python` were tested in a fresh venv.
- **The audit** (item 17). It re-ran everything in the foreground. It found one leak:
  `engine.load()` imported the `metacat.gui` package, because `"gui"` (gui.ss) is in the
  load order. That was fixed after a test that failed first. Nine stale statements in the
  plan were corrected, and the 242 quirk sites (167 `# chez:`, 75 `# 1.2:`) are now listed
  in the plan by a script whose test fails when the list drifts. The gate passed with 1412
  tests in 6 min 57 s.

## Files in this folder

| File | What |
| --- | --- |
| [`TASK.md`](TASK.md) | The goal, philosophy, frozen references, target layout and constraints. |
| [`iterations.md`](iterations.md) | The 18 items with their tests, all `[x]`. |
| [`PROGRESS.md`](PROGRESS.md) | One section per iteration (tests written first and seen failing, mutation checks, what was seen in renders, blockers, notes for the next item), ending with `LOOP_COMPLETE`. |
| [`loop.py`](loop.py) | The driver. Compared with loop0001's, it keeps sessions off the owner's Wayland screen (`WAYLAND_DISPLAY` removed, `GDK_BACKEND=x11`), forbids background-and-wait in the prompt, and names the frozen references in the prompts. |
| [`gate.py`](gate.py) | The gate: the original equals `9f072c0:Metacat`; `chez_scheme/`, `racket/`, `tests/golden/` and `tests/diff/` equal `9684b16` (README files excepted); then `bash python/run-tests.sh` must pass. |
| [`knobs.json`](knobs.json) | The knobs: 3 h per session, 0.5 h per fix, 3 fix attempts, up to 30 iterations, push on. |
| [`loop.log`](loop.log), `nohup.out` | The driver's log (the two files are identical). |
| `status.json` | The final state: `"phase": "idle"`. |
| `session_it01.log` … `session_it18.log`, `session_it15_fix1.log` | A readable transcript of each session. |
| `session_*.jsonl` | The raw `stream-json` of each session (about 34 MB with the logs). |
| `__pycache__/` | Python's bytecode cache of `gate.py`. It is not part of the loop. |

## Re-running it

The loop is complete (`LOOP_COMPLETE` is in `PROGRESS.md`), so `python3 loop.py` would
stop at once. To continue with new work, start a new folder (`loop0003/`) from these files,
or remove the sentinel and add items:

```bash
cd ralph_loops/loop0002
python3 loop.py --dry-run     # print the prompt the next session would get
python3 loop.py 1             # run one iteration (gate, commit and push included)
python3 ralph_loops/loop0002/gate.py   # the gate alone, from the repo root (about 7 min)
```

- The driver refuses to start if `python/`, `tests/` or `docs/` has uncommitted changes.
- Set `"push": false` in `knobs.json` to commit without pushing.
- The gate needs Python 3.12 with pytest and tkinter, Chez Scheme 10 (for the live oracle
  comparisons) and `xvfb-run`.
- Not re-done at the audit: re-measuring `docs/python-run-times.md` on an idle machine. It
  is listed in [`docs/follow-ups.md`](../../docs/follow-ups.md) under "Python (loop0002)",
  together with ideas for a next loop.
