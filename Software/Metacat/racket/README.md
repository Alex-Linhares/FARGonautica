# racket/: Metacat 1.2 in Racket

This folder is the Racket port of James B. Marshall's Metacat 1.2. The original is
Chez Scheme with a GUI on SWL/Tk, kept unchanged in
[`../chez_scheme/original/`](../chez_scheme/original/). The model is a line-for-line copy
of the original, run as one Racket module (the **engine**). It runs headless from a
command line or with the original's windows, redrawn on `racket/draw` and `racket/gui`.
Given the same problem and seed, it makes the same run as the original: the same
codelets in the same order, the same structures, temperature, answers and commentary.
This was checked event for event against the original running headless under Chez
Scheme 10, on 109 golden traces and 720 more seeded runs. It needs Racket 8.x CS with
`racket/gui` (it was built with Racket 8.18).

![The Racket port after answering wyz to "abc → abd; xyz → ?" (Run 7 of Marshall's dissertation, seed 3852097033)](../docs/screenshots/run7-wyz.png)

## What's here

| Path | What it is |
| --- | --- |
| [`engine.rkt`](engine.rkt) | The engine: one module that `include`s [`engine/*.rktl`](engine/README.md) in the load order of the original's `metacat.ss`. It exports every definition, plus `set-global!` for the globals that are set from outside. |
| [`engine/`](engine/README.md) | The ported model files, one per original `.ss` file. |
| [`engine-lang.rkt`](engine-lang.rkt) | The engine's module language: racket/base, except that the values of module-level expressions are discarded (Chez's top level drops them). |
| [`compat.rkt`](compat.rkt) | The compatibility layer: syntactic-sugar.ss's 22 `extend-syntax` macros as `syntax-rules`, and the Chez Scheme 10 built-ins Metacat relies on where Racket lacks them or differs. |
| [`utilities.rkt`](utilities.rkt) | utilities.ss: the object system (`tell`, `delegate`, ...), the random helpers, lists and tables. |
| [`headless.rkt`](headless.rkt) | Headless runs: `run-problem`, with null windows, the JSON-lines trace and the run driver. The counterpart of the oracle's `run.ss` and `trace.ss`. |
| [`cli.rkt`](cli.rkt) | The command-line program around `headless.rkt`. |
| [`main.rkt`](main.rkt) | The GUI entry point: opens the control panel and the windows, as `(setup)` did. |
| [`metacat.rkt`](metacat.rkt) | The entry point of the standalone executable: the GUI with no arguments, otherwise the CLI. |
| [`gui/`](gui/README.md) | The SGL drawing language on racket/draw, the windows, and the control panel on racket/gui. |
| [`tests/`](tests/README.md) | Display-free tests: differential batteries against Chez, golden traces, pixel snapshots. |
| [`gui-tests/`](gui-tests/README.md) | Tests that need a (virtual) display: the control panel and the standalone program. |
| [`info.rkt`](info.rkt) | Package info: collection `metacat`, dependencies `base`, `gui-lib`, `rackunit-lib`, license GPL-2.0-or-later; keeps `gui-tests` out of plain `raco test`. |

`compiled/` directories are created by `raco make` and are not in git.

## How it is built

The original `load`s 44 files into one global top level. Its definitions refer to each
other across files in both directions, and files `set!` each other's globals. Racket
modules can't be mutually recursive and can't `set!` an import. So the port keeps the
original's shape:

```
compat.rkt          syntactic-sugar.ss + Chez built-ins     (module)
utilities.rkt       utilities.ss                            (module, requires compat)
engine.rkt          every model file, in load order         (module, requires both)
  └─ include engine/constants.rktl, setup.rktl, coderack.rktl, ... demos.rktl
headless.rkt        run-problem: headless windows + trace   (requires the engine)
cli.rkt             racket racket/cli.rkt ...               (requires headless)
gui/views.rkt       the windows, on racket/draw             (requires the engine)
gui/gui.rkt         the control panel, on racket/gui        (requires views)
main.rkt, metacat.rkt   lazy-require gui/gui.rkt and cli.rkt
```

- **The engine never requires `racket/gui` or `racket/draw`.** The model talks to its
  windows through globals (`*workspace-window*`, ...), as in the original. Headless
  they hold null objects; with the GUI they hold the real windows. Watching changes
  nothing: the 109 golden runs give the same traces with every window attached.
  `main.rkt` and `metacat.rkt` load the GUI with `lazy-require`, so requiring them
  stays headless, and `raco exe` still embeds the GUI.
- **compat.rkt** reproduces what equivalence depends on: Chez's random-number
  generator, bit for bit; Chez's order of evaluating `map`'s calls; Chez's `sort`
  (algorithm and argument order); `remq`/`remv`/`remove`; top-level values; Chez's
  number printing, `format` and `error`; `record-case`, `reset`; and one-armed `if` and
  Chez-style `case`. utilities.ss's `round`/`floor`/`ceiling`/`truncate` return exact
  integers, as in the original.
- Every change Racket forced is marked `port:` in the code and explained in
  [`docs/porting-notes.md`](../docs/porting-notes.md) (one section per work item).
  What deliberately differs (drawing, the control panel's widgets, entry points) is in
  [`docs/divergences.md`](../docs/divergences.md).

[`engine/README.md`](engine/README.md) explains the include mechanism and lists every
file. [`gui/README.md`](gui/README.md) explains the graphics.

## Running

### The GUI

```bash
racket racket/main.rkt          # the control panel and every window
racket racket/main.rkt 1.5      # the same, windows scaled by 1.5
racket racket/one-window.rkt    # the same GUI in one window: menus, control strip, panes
```

`racket/one-window.rkt` ([`gui/one-window.rkt`](gui/one-window.rkt)) puts the control
panel and every graphics window into one frame of the screen's size, laid out as the Python
Qt GUI lays them out: Temperature, Workspace, Coderack, Vertical Themes and Commentary on
top; Slipnet, Top and Bottom Themes and Episodic Memory in the middle; the Temporal Trace
(and the EEG, when shown) at the bottom. The panes follow the window's size; the Windows
menu hides and shows them. There are no splitters.

![Run 7 at its answer in the one-window GUI](../docs/screenshots/racket-one-window-run7.png)

Type a problem in the control panel's command line and press Enter: `abc abd xyz` asks
what `xyz` changes to, `abc abd xyz 7` uses seed 7, and `abc abd xyz wyz` asks Metacat
to justify `wyz`. Then press Go or Step. The Demos menu loads the dissertation's runs
with their seeds (see [`docs/demos.md`](../docs/demos.md) for which ones replay).

### Headless

```bash
racket racket/cli.rkt INITIAL MODIFIED TARGET [ANSWER] [--seed N] [--max-codelets K] [--keep-going] [--trace FILE] [--verbose]
```

For example (under a second):

```
$ racket racket/cli.rkt abc abd xyz --seed 3852097033
Problem: abc -> abd; xyz -> ?  seed 3852097033
Comment: Okay, if "abc" changes to "abd", what does "xyz" change to?  Hmm...
Comment: Uh-oh, I seem to have run into a little problem.  Changing the letter-category of the letter z to its successor is not possible in xyz.
...
Comment: The answer "wyz" occurs to me.  I think this answer is great!
Answer: wyz  quality 91  codelet 2170  temperature 15
Type (go) or click on the Workspace to continue...
Stopped: suspend
Codelets: 2170
Temperature: 15
Answers: (wyz)
```

- With a fourth string the run is a justify run.
- Without `--seed`, the seed comes from the clock and is printed on the Problem line;
  pass it back with `--seed` to replay the run. Seeds run from 1 to 4294967295.
- The run ends where the original would stop and wait for Go: at the first answer, or
  when it gives up. `--keep-going` continues as if Go were pressed. `--max-codelets K`
  is the original's breakpoint: the run stops after codelet K. "Stopped:" gives the
  reason: `suspend`, `cap`, or `halt` (the original's `report-error-and-halt`).
- `--trace FILE` writes the run's JSON-lines trace ([`docs/trace-format.md`](../docs/trace-format.md)).
  For the runs of `tests/problems.txt` it equals the file in `tests/golden/`.
- `--verbose` turns on the original's verbose mode and prints the model's own running
  commentary on each codelet.
- Exit codes: 0 after a run, 2 for bad arguments, 1 if the run raises an error (the
  original itself crashes on some runs, such as `abc ccbbaa ijk` with seed 3; see
  [`docs/anomalies_and_quirks.md`](../docs/anomalies_and_quirks.md)).

The output is byte for byte what the oracle prints for the same arguments:
`scheme --script chez_scheme/oracle/run.ss abc abd xyz --seed 3852097033`.

Run one problem per process. The engine is a single module instance, and some state
outlives a run (the Episodic Memory keeps its answers), whereas the oracle starts a
fresh Chez process for each run. The tests use a fresh namespace per run.

### The standalone program

```bash
bash make-dist.sh                      # builds build/metacat/ (raco exe --gui + raco distribute)
build/metacat/bin/metacat              # the GUI
build/metacat/bin/metacat 1.5          # the GUI, scaled
build/metacat/bin/metacat abc abd xyz --seed 7   # headless, the CLI's arguments and output
build/metacat/bin/metacat --help
```

`make-dist.sh` (at the repository root) takes an optional destination directory instead
of `build/metacat`. The result holds `bin/metacat`, the Racket runtime it needs, the
top-level README and the license, and runs without Racket installed (about 70 MB). It
was built and tested on Linux only.

## Testing

```bash
raco test racket/                                   # the display-free tests (racket/tests/)
bash tests/run-tests.sh                             # everything, about 9-10 minutes
```

`tests/run-tests.sh` compiles every module with `raco make`, runs `raco test racket/`,
runs [`gui-tests/`](gui-tests/README.md) on a virtual display with
`env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run ...`, then the Chez oracle checks in
`chez_scheme/oracle/tests/`. It needs Chez Scheme 10 (`scheme` or `chezscheme`),
`xvfb-run` and `xwininfo`; `bwrap` is optional. Never run the GUI tests on your real
screen.

## How it is verified

1. [`chez_scheme/oracle/`](../chez_scheme/oracle/README.md) runs the unmodified
   original headless under Chez Scheme 10 and writes JSON-lines traces of every codelet,
   structure, temperature update, theme, Temporal Trace event, answer and comment.
2. `tests/golden/` holds 109 such traces (36 problems, every demo of the dissertation
   included), recorded by the oracle only.
3. [`tests/golden-test.rkt`](tests/golden-test.rkt) runs all 109 with the port: every
   trace must be byte-identical, and every printed output must equal the oracle's, run
   live. [`tests/views-test.rkt`](tests/views-test.rkt) does it again with every window
   attached.
4. Differential batteries compare the port with the original file by file, under Chez
   and Racket in the same test: utilities, coderack, slipnet, workspace, bonds and
   groups codelet by codelet, bridges, rules and answers, the graphics builders, the
   SGL interpreter and the panels. See [`tests/README.md`](tests/README.md).
5. Beyond the goldens, `python3 tests/extra-seeds.py` ran 20 more seeds per problem,
   oracle against port: 720 of 720 identical ([`docs/extra-seeds.md`](../docs/extra-seeds.md)).

Speed: the port is about 2× slower than Chez per codelet but starts faster, so most
single runs finish sooner ([`docs/run-times.md`](../docs/run-times.md)).

## Further reading

- [`docs/code-map.md`](../docs/code-map.md): the original, one paragraph per file.
- [`docs/porting-notes.md`](../docs/porting-notes.md): how each part was ported, and why.
- [`docs/divergences.md`](../docs/divergences.md): where the port differs on purpose.
- [`docs/anomalies_and_quirks.md`](../docs/anomalies_and_quirks.md): bugs and oddities
  found in the original, Chez, Racket and the port.
- [`docs/trace-format.md`](../docs/trace-format.md): the trace format and the random
  generator plan.
- [`docs/demos.md`](../docs/demos.md): the dissertation's demos and which seeds replay.
- [`ralph_loops/loop0001/PROGRESS.md`](../ralph_loops/loop0001/PROGRESS.md): the log of
  the work, item by item.
- [`../python/`](../python/README.md): the second port, to Python.

Metacat is © 1999, 2003 James B. Marshall, based on Copycat by Melanie Mitchell. The
original and this port are free software under the GNU GPL, version 2 or later
([`chez_scheme/original/LICENSE`](../chez_scheme/original/LICENSE)).
