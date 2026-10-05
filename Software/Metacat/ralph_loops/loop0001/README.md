# loop0001: Metacat 1.2 to Racket

This Ralph loop wrote the Racket port in [`racket/`](../../racket/). It also built
everything the port is checked against: the headless Chez Scheme 10 oracle in
[`chez_scheme/oracle/`](../../chez_scheme/oracle/README.md), the 109 golden traces in
`tests/golden/`, and the differential test batteries in `tests/diff/`. It ran 18 fresh
`claude -p` sessions, one per item, from **2026-10-02 20:29** to **2026-10-03 05:05**
(about 8 h 36 min). Every session passed the gate on the first try, and each was committed
and pushed. At the end, all 109 golden runs and 720 runs with other seeds match the
original event for event and byte for byte. The GUI, the CLI and a standalone executable
work. See [`../README.md`](../README.md) for how Ralph loops and the driver work in general.

## The goal (`TASK.md`)

"Metacat in modern Scheme — a faithful Racket port with a racket/gui interface." The
principles:
- **The original is the specification.** `chez_scheme/original/` is never edited. The gate
  checks this.
- **Faithful first, idiomatic later.** Keep the same names, the same `tell` objects, the
  same order of random draws and the same arithmetic. Every forced change goes in
  `docs/porting-notes.md`, and every deliberate difference in `docs/divergences.md`.
- **An oracle, not eyeballing.** Golden traces come only from the original running under
  Chez. They are never edited and never regenerated to make the port pass.
- **The engine knows nothing about the GUI.** Watching a run must not change it.
- **Tests first. Look at what you draw. Log the strange** (in
  `docs/anomalies_and_quirks.md`).

`TASK.md` expected the oracle to need a portable random-number generator swapped in. Item
01 found that Chez 10's own generator could be reproduced exactly instead, so nothing was
swapped (see `docs/trace-format.md`).

## Items and outcomes

Timings come from [`loop.log`](loop.log). "Session" is the Claude session's wall-clock
time. "Total" runs from session start to commit, so it includes the gate. Cost is the
figure on the last line of each `session_itNN.log`.

| Iter. | Item | Outcome | Started | Session | Total | Tool calls | Cost |
| ---: | --- | --- | --- | ---: | ---: | ---: | ---: |
| 1 | 00 Toolchain and skeleton | solved | 10-02 20:29 | 5 min | 5 min | 22 | $1.06 |
| 2 | 01 The original, headless, under Chez 10 | solved | 20:34 | 19 min | 20 min | 91 | $4.24 |
| 3 | 02 Traces and golden files | solved | 20:53 | 16 min | 17 min | 60 | $3.57 |
| 4 | 03 The compatibility layer | solved | 21:11 | 32 min | 33 min | 78 | $6.03 |
| 5 | 04 Constants, setup, coderack, descriptions | solved | 21:43 | 16 min | 16 min | 57 | $3.31 |
| 6 | 05 Slipnet and images | solved | 22:00 | 11 min | 12 min | 55 | $2.66 |
| 7 | 06 Workspace objects and strings | solved | 22:12 | 18 min | 19 min | 64 | $3.95 |
| 8 | 07 Bonds, groups, concept mappings | solved | 22:31 | 26 min | 27 min | 95 | $5.44 |
| 9 | 08 Bridges and breakers | solved | 22:58 | 34 min | 36 min | 78 | $4.36 |
| 10 | 09 Rules and answers | solved (one criterion moved to item 11) | 23:34 | 35 min | 40 min | 96 | $5.98 |
| 11 | 10 Themes, justification, trace, jootsing, memory | solved: **109/109 goldens match** | 10-03 00:15 | 41 min | 47 min | 110 | $6.37 |
| 12 | 11 Full runs and the CLI | solved | 01:02 | 30 min | 36 min | 61 | $4.29 |
| 13 | 12 The SGL interpreter on racket/draw | solved | 01:38 | 24 min | 30 min | 80 | $5.96 |
| 14 | 13 Workspace, bridge, group and rule graphics | solved | 02:08 | 38 min | 44 min | 134 | $13.14 |
| 15 | 14 The other panels | solved | 02:52 | 29 min | 36 min | 120 | $6.64 |
| 16 | 15 The control panel and windows | solved | 03:28 | 40 min | 48 min | 107 | $9.40 |
| 17 | 16 Demos, packaging, README | solved | 04:16 | 21 min | 28 min | 90 | $4.87 |
| 18 | 17 Final audit | `[!]` set by the driver; work committed | 04:44 | 14 min | 22 min | 177 | $6.39 |

In total there were 1,575 tool calls, and the reported cost was about $98. `PROGRESS.md`
ends at "Current: 17/18 SOLVED". The loop stopped with "no unchecked items left", so it
never wrote `LOOP_COMPLETE`.

**Item 17 in detail.** The audit session started the gate in the background, scheduled a
wake-up and stopped while "waiting for the notification" (end of `session_it18.log`).
Because a `claude -p` session ends when the agent stops, it never wrote its `PROGRESS.md`
entry or ticked the item. The driver then marked it `[!]` and added a `(driver)` entry. The
audit's changes passed the gate and were committed as `1033517`. What it did is recorded
in `docs/porting-notes.md` ("Final audit (item 17)"), `docs/extra-seeds.md` and
`docs/follow-ups.md`. Loop0002's prompt was changed because of this (see
[`../README.md`](../README.md#lessons-learned)).

## Notable findings (from `PROGRESS.md`)

- **The original runs headless, unmodified** (item 01). `chez_scheme/oracle/prelude.ss`
  loads all 44 files under `scheme --script`. It stubs out SWL, provides `extend-syntax` as
  a `syntax-case` macro, and defines the three settings `metacat.ss` needs.
- **Chez 10's `random` can be reproduced exactly** (item 01). It is a 32-bit LCG
  (`S := S*72931 + 90763387 mod 2^32`), read from the Chez C source. A Chez check compares
  the specification with the built-in draw for draw. So no generator was swapped in.
- **Most demo seeds still replay**, contrary to `TASK.md`'s expectation (items 01, 16):
  Runs 1–5 and 7, Figs. 5.7/5.8, misc1/2/4/5/9. Run 6, Run 8, fig5.5-bottom, fig5.11,
  eqe-qeeeq and misc3/6–8 don't (`docs/demos.md`).
- **Chez does not evaluate arguments left to right** (items 01, 03). `(f (show 1) (show 2)
  (show 3))` prints `312`, `let` goes right to left, and inlined primitives go left to
  right. Chez's `map` applies its procedure in an order of its own, its `sort` sorts the
  second half first, and `for-each` returns the last value. Every one of these was
  reproduced in `racket/compat.rkt` and checked by a 197-test differential battery.
- **The original itself fails on some runs** (item 02). `eqe qeq abbba aaabaaa` seed 3 calls
  `report-error-and-halt` at codelet 4004 (kept in the golden set as a "halt" run).
  `abc ccbbaa ijk` seed 3 raises a Chez error (`caddr` of `#f`), so that seed is left out.
- **Exact arithmetic matters**: 3,601 codelet lines in the goldens have rational urgencies.
- **One module for the engine** (item 04). The 44 files are mutually recursive and `set!`
  each other's globals, so `racket/engine.rkt` `include`s one `.rktl` per original file in
  load order.
- **Stale `.zo` files hid mutations** (item 06). Loading a compiled module ignores edits to
  included `.rktl` files, so at first no mutation made a test fail. The test runner now
  loads through the compilation manager.
- **Codelet-level harness** (items 07–09). Restricted runs of the oracle and the port were
  compared codelet by codelet before the self-watching half existed: 2000 codelets on all
  109 runs (item 07) and 3000 with bridges (item 08), all byte-identical.
- **109/109 goldens identical** from item 10 on: 272,957 codelets, 12,952 theme lines,
  1,326 Temporal Trace events, 115 answers. They stayed identical with every window attached
  (items 13–14). A Slipnet view that draws one random number makes all 109 differ.
- **The windows opened on the owner's screen** (item 15). GTK ignores Xvfb while
  `WAYLAND_DISPLAY` is set. Every GUI run since uses `env -u WAYLAND_DISPLAY
  GDK_BACKEND=x11 xvfb-run -a …`.
- **The final audit** (item 17) found 720/720 extra-seed runs identical, and 1.55 million
  lines of verbose output identical once one bug was fixed (`format-slipnode` was not
  registered as a top-level value). It also found no `racket/gui` in the engine's
  transitive imports.

## Files in this folder

| File | What |
| --- | --- |
| [`TASK.md`](TASK.md) | The goal, philosophy, acceptance criteria and constraints (unchanged during the loop). |
| [`iterations.md`](iterations.md) | The 18 items with their tests. 17 are `[x]` and item 17 is `[!]`. |
| [`PROGRESS.md`](PROGRESS.md) | One section per iteration: what was done, tests written first or not, mutation checks, what was seen in the PNGs, blockers, and notes for the next item. |
| [`loop.py`](loop.py) | The driver used for this loop. |
| [`gate.py`](gate.py) | The gate: `chez_scheme/original/` must equal the import commit `9f072c0`, then `bash tests/run-tests.sh` must pass. |
| [`knobs.json`](knobs.json) | The knobs: 3 h per session, 0.5 h per fix, 3 fix attempts, push on. |
| [`loop.log`](loop.log), `nohup.out` | The driver's log (the two files are identical). |
| `status.json` | The final state: `"phase": "idle"`. |
| `session_it01.log` … `session_it18.log` | A readable transcript of each session. |
| `session_it01.jsonl` … `session_it18.jsonl` | The raw `stream-json` of each session (about 26 MB in all). |

## Re-running it

The loop is finished: every item is checked, so `python3 loop.py` would stop at once with
"no unchecked items left". To repeat an item or add new ones:

```bash
cd ralph_loops/loop0001
# re-open or add items as "- [ ] ..." in iterations.md
python3 loop.py --dry-run     # print the prompt the next session would get
python3 loop.py 1             # run one iteration (gate, commit and push included)
```

- The driver refuses to start if `racket/`, `chez_scheme/oracle/`, `tests/` or `docs/` has
  uncommitted changes.
- Set `"push": false` in `knobs.json` to commit without pushing to `origin`.
- The gate alone is `python3 ralph_loops/loop0001/gate.py`. It needs Racket 8.x CS, Chez
  Scheme 10, `xvfb-run`, `xwininfo` and optionally `bwrap`. It takes about 10 minutes.
- This driver predates the lessons of this loop. Its prompt does not forbid waiting on
  background jobs, and it does not remove `WAYLAND_DISPLAY` from the sessions' environment.
  For new work, start from [`../loop0002/loop.py`](../loop0002/loop.py), which does both.
