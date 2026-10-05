# racket/tests/: the Racket port's tests

These are the display-free tests of the Racket port. Most compare the port with the
original Metacat 1.2. Some run the original live under Chez Scheme 10 (through
[`chez_scheme/oracle/`](../../chez_scheme/oracle/README.md)) and compare outputs line by
line. Others compare with the golden traces in [`tests/golden/`](../../tests/golden/),
which the oracle recorded. The rest check the port's own structure (no `racket/gui` in
the engine, stop-and-resume, the SGL viewport) and the pixels of the windows. The tests
that need a display are in [`../gui-tests/`](../gui-tests/README.md).

## Running

```bash
raco test racket/                         # every test here (gui-tests is skipped by info.rkt)
raco test racket/tests/golden-test.rkt    # one file
bash tests/run-tests.sh                   # everything: raco make, raco test racket/,
                                          # the GUI tests under xvfb-run, the Chez checks
```

`tests/run-tests.sh` runs `raco make` on every module first. `raco test` and plain
`racket` load a module's `.zo` without checking its dependencies, so a changed module
could leave stale `.zo` files behind (see
[`docs/anomalies_and_quirks.md`](../../docs/anomalies_and_quirks.md)). Run `raco make`
yourself before running single files after a change.

Requirements: Chez Scheme 10 as `scheme` or `chezscheme` for the tests marked "Chez,
live" below. The golden and views tests run the 109 runs in parallel on Racket places
(up to 16, or one per core on smaller machines); golden-test also runs the oracle on
half the cores at the same time.

## The test files

"Compares with" says what the port is checked against:

- **Chez, live**: the original, run under Chez Scheme during the test.
- **golden**: the files in `tests/golden/`.
- **snapshots**: the PNGs in [`snapshots/`](#snapshots).

Times are wall-clock seconds for `raco test racket/tests/FILE` with compiled code, measured
one file at a time on a 32-core Linux machine. They are a guide, not a promise: the
golden and views tests depend on the number of cores.

| File | What it checks | Compares with | Time |
| --- | --- | --- | ---: |
| `skeleton-test.rkt` | `racket/main.rkt` loads without a display; `metacat-version` is `"1.2"`. | (none) | <1 s |
| `compat-test.rkt` | What `compat.rkt` and `utilities.rkt` do that a battery can't express: the random generator against known Chez values, one-armed `if`, Chez's `map` order, `sort`, `remq`, `record-case` (hygiene, extra and missing arguments), cross-module mutation, `reset`. | values recorded from Chez | <1 s |
| `engine-test.rkt` | The structure of `racket/engine.rkt`: loading prints nothing; `set-global!` sets only the listed globals; the 27 codelet types and 59 slipnodes are module variables and top-level values; run.ss's procedures, the graphics builders and view globals. | (none) | <1 s |
| `utilities-diff-test.rkt` | `tests/diff/utilities-battery.scm`: `compat.rkt` and `utilities.rkt`, test by test. | Chez, live | ~1 s |
| `coderack-diff-test.rkt` | `tests/diff/coderack-battery.scm`: constants, setup, the Coderack (bins, urgencies, posting, deletion, choosing, draw for draw), descriptions. | Chez, live | ~1.5 s |
| `slipnet-diff-test.rkt` | `tests/diff/slipnet-battery.scm`: the initial Slipnet, activation spreading and decay, slippages, top-down codelets, images. | Chez, live | ~1.5 s |
| `workspace-diff-test.rkt` | `tests/diff/workspace-battery.scm`: the Workspace, objects, strings and formulas; the initial Workspace of every problem × seed. | Chez, live | ~1.5 s |
| `codelet-diff-test.rkt` | `tests/diff/codelet-battery.scm`: bonds, groups and concept mappings codelet by codelet (bond and group codelets only) for every problem × seed of `tests/problems.txt`. | Chez, live | ~20 s |
| `bridge-diff-test.rkt` | `tests/diff/bridge-battery.scm`: the same with bridges, descriptions and the breaker added. | Chez, live | ~55 s |
| `rule-diff-test.rkt` | `tests/diff/rule-battery.scm`: the same with rules and answers, up to the first answer of every problem × seed. | Chez, live | ~160 s |
| `graphics-diff-test.rkt` | `tests/diff/graphics-battery.scm`: the engine's pexp builders (general-, group-, bridge-, rule-graphics), every coordinate, and the messages to the Workspace window. | Chez, live | ~1 s |
| `sgl-diff-test.rkt` | `tests/diff/sgl-battery.scm`: the messages `racket/gui/sgl.rkt` sends a viewport, one per Tk canvas item the original creates. | Chez, live | ~1 s |
| `panels-diff-test.rkt` | `tests/diff/panels-battery.scm`: the panels' pexp builders, Themespace layout, Trace and Memory mouse handlers, the EEG object (engine plus `racket/gui/views.rkt`). | Chez, live | ~1.5 s |
| `golden-test.rkt` | All 109 golden runs, each in a fresh engine: the JSON-lines trace must equal `tests/golden/<run>.jsonl` byte for byte, and the printed output must equal what the oracle's `run.ss` prints for the same run. Also the original's crash on `abc ccbbaa ijk` seed 3: the port crashes in `caddr` too, with the same trace up to the crash. | golden, and Chez, live | ~30 s |
| `cli-test.rkt` | `racket/cli.rkt` run as a program next to `chez_scheme/oracle/run.ss`: stdout and exit code for an answer, a cap, a justify run, `--keep-going`, a halt, verbose runs; `--trace` writes the golden file; a clock seed replays in the oracle; the crash; bad arguments exit 2. | Chez, live, and golden | ~16 s |
| `run-test.rkt` | run.ss's own `break` and `go`, as the GUI uses them: a run stopped at codelets 150, 300 and 450 and resumed equals a run never stopped (count, generator state, temperature, output); step mode; `go` without a break. | (none) | <1 s |
| `demos-test.rkt` | The engine's demo problems equal those of the original `demos.ss`; every demo problem and seed is a golden run; the demo runs that [`docs/demos.md`](../../docs/demos.md) lists as replaying give their documented answers and codelet counts through the CLI. | the original's `demos.ss` source | ~2.5 s |
| `no-gui-test.rkt` | Walks the compiled imports of every module: nothing outside `racket/gui/` reaches `racket/gui`; the engine and headless driver don't reach `racket/draw`; the views modules reach `racket/draw` only. | (none) | ~1 s |
| `sgl-test.rkt` | `racket/gui/sgl.rkt`, `fonts.rkt` and `colors.rkt`: no `racket/gui`, colours, Tk dash patterns, tag operations, text placement, painted pixels, and the fixture of every SGL form, pixel for pixel. | snapshots | <1 s |
| `views-test.rkt` | All 109 golden runs with every window attached (`attach-views!`) give the golden traces, and every window is drawn into; the crash run crashes the same way with the views; the windows at eight scenes of golden runs, pixel for pixel. | golden, snapshots | ~65 s |

Helper modules (they contain no tests):

| File | Used by | What it is |
| --- | --- | --- |
| `diff-runner.rkt` | the `*-diff-test.rkt` files | Evaluates a battery from `tests/diff/` twice: under Chez with the original loaded (`chez_scheme/oracle/diff-eval.ss`) and in a fresh Racket namespace with the port's modules. `check-battery` requires every output line to be identical and reports the first difference. |
| `golden-harness.rkt` | golden-test, views-test, cli-test | `golden-run`/`golden-trace`: one run through [`../headless.rkt`](../headless.rkt), returning its trace and printed output. |
| `golden-pool.rkt` | golden-test, views-test | Runs the golden runs in parallel on places, each in a fresh engine; `first-difference` between traces. |
| `views-harness.rkt` | views-test | `views-run` (a golden run with every window attached) and `render-scene` (the scenes for the snapshots). |
| `sgl-fixture.rkt` | sgl-test | The fixture of every SGL form. |
| `sgl-recorder.rkt` | sgl-diff-test | A viewport that records every message it gets, for the SGL battery. |

The batteries themselves (`*.scm`) are in [`tests/diff/`](../../tests/diff/). Both sides
load `tests/diff/helpers.scm` first.

## Snapshots

`snapshots/` holds 49 PNG files that the pixel tests compare against:

- `sgl-fixture.png`: the SGL fixture, for `sgl-test.rkt`. It is the same image as
  [`docs/screenshots/panels/sgl-fixture-racket.png`](../../docs/screenshots/panels/sgl-fixture-racket.png).
- 48 files named `WINDOW-SCENE.png`, for `views-test.rkt`. A WINDOW is one of `workspace`,
  `slipnet`, `coderack`, `temperature`, `top-themes`, `bottom-themes`,
  `vertical-themes`, `memory`, `commentary`, `trace` or `EEG`. The scenes are defined in
  `views-harness.rkt`. Windows left blank in a scene aren't pictured.

| Scene | Run | Picture taken |
| --- | --- | --- |
| `mrrjjj-513` | `abc abd mrrjjj`, seed 1 | at codelet 513, mid-run |
| `mrrjjj-answer` | `abc abd mrrjjj`, seed 1 | at the first answer (Workspace only) |
| `xyz-snag-event` | `abc abd xyz`, seed 3852097033 (Run 7) | the last snag event's Workspace view, at codelet 800 |
| `xyz-answer` | Run 7 | at the answer `wyz` |
| `xyz-answer-description` | Run 7 | the answer's description from the Episodic Memory |
| `xyd-justify` | `abc abd xyz xyd`, seed 1760747975 | a justify run, where it stops after justifying `xyd` (every window) |
| `xyz-clamp-click` | Run 7 | after a click on the last clamp event in the Trace window |
| `glz-compare` | `abc abd glz`, seed 1108779034, 1800 codelets, `--keep-going` | after clicks on two answers in the Memory window, which compares them |

A mismatch writes the actual rendering to `/tmp/WINDOW-SCENE-actual.png` (or
`/tmp/sgl-fixture-actual.png`). To accept a deliberate change of the rendering, run the
test with `METACAT_UPDATE_SNAPSHOTS=1`, look at the new PNGs, and say why in the progress
log. To render a scene by hand:

```bash
racket racket/tests/views-harness.rkt xyz-answer /tmp/out    # writes /tmp/out/WINDOW-xyz-answer.png
racket racket/tests/sgl-fixture.rkt /tmp/fixture.png
```

The rendering uses whatever faces fontconfig gives for `times` and `helvetica`
([`docs/divergences.md`](../../docs/divergences.md)), so on a machine with other fonts
installed the pixel comparisons may fail even though the runs match.
