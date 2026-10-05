# Metacat 1.2 in Python

A translation of James B. Marshall's **Metacat 1.2** from Chez Scheme to Python 3.12. It
uses the standard library only, with a tkinter GUI. An optional second GUI puts every
window in one Qt window ([`metacat/qt/`](metacat/qt/README.md), needs PySide6). It was translated test-first, and every
expected value in its tests came from the original running under Chez Scheme 10. Given the
same seed, it makes the same run as the original, event for event: all 109 golden traces in
[`tests/golden/`](../tests/golden/) match byte for byte (also with every window attached),
and so do 720 more seeded runs. It is GPL v2 or later, like Metacat itself. Metacat is
© 1999, 2003 James B. Marshall and is based on Copycat by Melanie Mitchell. What Metacat is,
and how the ports are checked against the original, is in the
[top-level README](../README.md). The other port, to Racket, is in [`racket/`](../racket/).

![The Python GUI after answering wyz to "abc → abd; xyz → ?" (Run 7 of Marshall's
dissertation)](../docs/screenshots/python-run7-wyz.png)

*Run 7 of the dissertation (`abc abd xyz`, seed 3852097033) in the Python GUI, captured
under Xvfb at the answer `wyz`. Top row: the control panel and the Temperature, the
Workspace (both rules and the crossed bridges that map `a`–`z` and `c`–`x`), the Coderack
and the Commentary. Below: the Slipnet, the Top, Bottom and Vertical Themes, the Episodic
Memory (a snag and the answer) and the Temporal Trace. Known flaw: the Coderack's tiny
labels drop letters ("Bond bu ders" for "Bond builders"). This Tk draws X core fonts as
1-bit bitmaps, which lose the `i`s and `l`s at 8–10 px; the Qt GUI keeps them (see
[`docs/anomalies_and_quirks.md`](../docs/anomalies_and_quirks.md)).*

## Contents

- [Running it](#running-it)
- [Layout](#layout)
- [How it was translated](#how-it-was-translated)
- [Verification](#verification)
- [Speed](#speed)
- [Tests](#tests)
- [Further reading](#further-reading)

## Running it

Requirements: Python 3.12 or later, plus tkinter (Tk 8.6) for the GUI. Nothing else is
needed: `pyproject.toml` declares no dependencies, and pytest is only a test extra.

From a checkout, without installing anything:

```bash
cd python
python3 -m metacat abc abd xyz --seed 3852097033   # headless: the commentary and the answers
python3 -m metacat.gui                             # the control panel and the windows
python3 -m metacat.gui 1.5                         # the same, with the windows scaled
```

Or install it, which gives you two commands:

```bash
pip install -e python          # editable; or: pip install python  (a regular install)
metacat abc abd xyz --seed 7
metacat-gui
```

With the `qt` extra it also installs PySide6 and a third command, the one-window GUI:

```bash
pip install -e 'python[qt]'
metacat-qt                     # or, from python/: python3 -m metacat.qt
```

### Headless

`python3 -m metacat` (or `metacat`) takes the same arguments as the original's headless
run, [`chez_scheme/oracle/run.ss`](../chez_scheme/oracle/), and prints exactly what it
prints:

```
metacat INITIAL MODIFIED TARGET [ANSWER] [--seed N] [--max-codelets K] [--keep-going]
        [--trace FILE] [--verbose]
```

- A run stops at its first answer, as the GUI does, unless you pass `--keep-going`.
  `--max-codelets K` stops it after K codelets. Without that option there is no cap.
- With an ANSWER, Metacat tries to *justify* that answer.
- Without `--seed`, the seed comes from the clock and is printed on the Problem line, so you
  can replay the run.
- `--trace FILE` writes the JSON-lines trace of
  [`docs/trace-format.md`](../docs/trace-format.md), the format of the golden traces.
- `--verbose` prints the model's own running commentary on each codelet.
- The exit code is 0 normally, 2 for bad arguments, and 1 when the original itself crashes.
  It does crash on a few runs, such as `abc ccbbaa ijk --seed 3`; see
  [`docs/anomalies_and_quirks.md`](../docs/anomalies_and_quirks.md).

Run 7 takes about 1.5 s and ends like this:

```
Comment: The answer "wyz" occurs to me.  I think this answer is great!
Answer: wyz  quality 91  codelet 2170  temperature 15
Type (go) or click on the Workspace to continue...
Stopped: suspend
Codelets: 2170
Temperature: 15
Answers: (wyz)
```

### The GUI

`python3 -m metacat.gui [SCALE]` (or `metacat-gui`) is the original's SWL interface,
redone in tkinter. Type a problem in the control panel's command line and press Enter:

- `abc abd xyz` asks what `xyz` changes to;
- `abc abd xyz 7` does the same with seed 7;
- `abc abd xyz wyz` asks Metacat to justify the answer `wyz`.

The buttons:

- **Go** runs.
- **Step** runs one step. The step interval is under Options.
- **Stop** interrupts the run.
- **Reset** starts the problem again.

When Metacat finds an answer, it stops. Go, or a click on the Workspace, makes it look for
another.

The menus:

- **Demos** has the runs of Marshall's dissertation, with their seeds.
- **Windows** hides and shows the windows.
- **Options** has breakpoints, Eliza mode, the graphics switches, verbose mode, clamping a
  theme or codelet pattern by hand (and undoing it), and saving the commentary.
- **Clear Memory** empties the Episodic Memory.
- **Help** shows the original's help text, shipped as `metacat/gui/help.txt`.

How the windows are drawn is in [`metacat/gui/`](metacat/gui/README.md).

### The one-window GUI (Qt)

`python3 -m metacat.qt` (or `metacat-qt`) shows the same panels, drawn by the same code,
as panes of one window: a control strip and a menu bar at the top (View replaces Windows),
and every panel below, in splitters you can drag. The panes can be hidden and shown from
View, the layout is saved between sessions, and View > Reset layout restores the default.
A run in it gives exactly its golden trace. It needs PySide6 (`pip install -e
'python[qt]'`) and a screen of at least 1920×1080. See
[`metacat/qt/README.md`](metacat/qt/README.md).

![The Qt GUI after answering wyz to Run 7, at 1920×1080](../docs/screenshots/qt-run7-wyz.png)

*Run 7 at its answer `wyz` in the Qt GUI, at 1920×1080 (grabbed offscreen).*

> *"The answer "wyz" occurs to me. I think this answer is great!"*
>
> — Metacat, Run 7

![The Qt GUI justifying ijk → abd, with every pane shown](../docs/screenshots/qt-justify-ijk-abd.png)

> *"Aha! I see why this answer makes sense. I think it's a pretty dumb answer."*
>
> — Metacat, asked to justify `ijk → abd` for `abc → abd`

*Asked to justify the literal-minded answer `abd` (seed 1, codelet 1835), Metacat builds rules that turn each letter of `ijk` into `a`, `b` and `d`, sees why they work, and isn't impressed. Every pane is shown here, including the Bottom Themes and the EEG (bottom), which justify runs bring into play.*

![abc → abd; ijk → ? with a user clamp](../docs/screenshots/python-ijk-clamp.png)

*`abc abd ijk`, seed 1, after the answer `ijd`. A pattern was clamped by hand from the
Options menu (a manual clamp), so the Commentary says "Thank you for that interesting suggestion!" and, later,
that it "resulted in zero progress". The Trace shows the Clamp event. The EEG window at the
bottom plots the average Workspace activity (yellow) and the temperature (red).*

## Layout

| Path | What it is |
|---|---|
| [`metacat/`](metacat/README.md) | The package: one module per original `.ss` file (the model), plus `chez.py` (Chez semantics), `objects.py`, `headless.py`, `trace_writer.py` and the CLI in `__main__.py` |
| [`metacat/gui/`](metacat/gui/README.md) | The views: the SGL interpreter on a tkinter Canvas, the panels, the control panel and the GUI program. The engine never imports this package |
| [`metacat/qt/`](metacat/qt/README.md) | The one-window GUI on PySide6 (optional): the Tk canvas commands executed on a `QGraphicsScene`, the panes, the control strip and menus. It reuses `metacat/gui/`'s panels; neither the engine nor the tkinter GUI imports it |
| [`oracle/`](oracle/README.md) | Scripts that run the unedited original under Chez and freeze its output into `fixtures/`, the port's own Chez batteries (`oracle/batteries/`) and the benchmarks |
| [`fixtures/`](fixtures/README.md) | The frozen Chez outputs every test compares against (171 MB, committed) |
| [`tests/`](tests/README.md) | pytest: 36 test files, their helpers, and `snapshots/` (renderings to inspect) |
| `pyproject.toml` | Package `metacat` 1.2.0. It ships the packages `metacat`, `metacat.gui` and `metacat.qt` and the help text, declares the commands `metacat` (`metacat.__main__:main`), `metacat-gui` (`metacat.gui.app:main`) and `metacat-qt` (`metacat.qt.app:main`), the extras `test` (pytest) and `qt` (PySide6), and sets up pytest (`slow` marker) |
| `run-tests.sh` | The single test entry point (full tier, `--fast`, or `--qt` for the Qt tests only) |

The package reads nothing outside itself: `test_install.py` runs a clean copy of `python/`
without the fixtures and tests. Of the
original's 44 files, the model is about 14,600 non-blank, non-comment lines of Scheme. The
translation is about 26,800 lines in `metacat/` and 9,400 in `metacat/gui/`, docstrings
and comments included.

## How it was translated

The port was built by a "Ralph loop", [`ralph_loops/loop0002/`](../ralph_loops/loop0002/),
the same method that built the Racket port: one fresh Claude Code session per work item.
After each session, a gate checks that the original and the references are unchanged and
that `run-tests.sh` passes. The goal and rules are in
[`TASK.md`](../ralph_loops/loop0002/TASK.md), the 18 items (00–17) in
[`iterations.md`](../ralph_loops/loop0002/iterations.md), and each session's work in
[`PROGRESS.md`](../ralph_loops/loop0002/PROGRESS.md).

The rules that shaped the code:

- **The original is the specification; the oracle decides.** The original runs unedited
  under Chez Scheme 10 ([`chez_scheme/oracle/`](../chez_scheme/oracle/)). The Racket port
  was used as a worked translation, never as the source of expected values.
- **Test first.** For each layer, the Chez differential batteries in
  [`tests/diff/`](../tests/diff/) (written for the Racket port), plus the port's own
  batteries in `oracle/batteries/`, were run under Chez. Their output was frozen per test
  into `fixtures/`, and each test was translated into a pytest case. Only then was the code
  translated, until the case passed. PROGRESS.md records that the tests failed first.
- **Faithful, not improved.** The port keeps Metacat 1.2's bugs, crashes and halts, and
  Chez's semantics: the 32-bit LCG behind `random`, exact rationals, its argument
  evaluation order, `map`'s order, its `sort`, and its printer. A `# chez:` or `# 1.2:`
  comment marks each place where this matters. There are 242 such sites, listed in the plan.
- **Scheme-shaped Python.** There is one module per `.ss` file and one function per
  definition, with a fixed name mapping (`foo-bar?` → `foo_bar_p`, `*x*` → `g_x`). Each
  docstring names its origin (`"""bonds.ss: bond-builder"""`). See
  [`metacat/README.md`](metacat/README.md).
- **The engine knows nothing about the GUI.** Model modules never import tkinter, and
  watching a run never changes it.

[`docs/python-translation-plan.md`](../docs/python-translation-plan.md) explains how each
Chez semantic is reproduced, how objects were chosen (a micro-benchmark of six variants),
the name mapping, the module and load-order scheme, and the risks. Each item added an "As
built" section, and item 17 added an audit.

## Verification

| Check | What must match | Where |
|---|---|---|
| 109 golden traces (36 problems, `tests/problems.txt`) | Every trace line, byte for byte | `tests/test_golden.py` |
| The same 109 with every window attached (offscreen) | The same traces: watching changes nothing | `tests/test_views.py` |
| 720 extra seeds (20 non-golden seeds per problem line, the seeds the Racket port was audited on) | Exit code, all of stdout, the first line of stderr, the trace's sha256 and line count | `tests/test_extra_seeds.py` |
| The CLI next to the live oracle (an answer, caps, justify, keep-going, verbose, the halt run, the crash run, bad arguments) | stdout and exit code | `tests/test_cli.py` |
| Every Chez differential battery, test by test (utilities, coderack, slipnet, workspace, codelets, bridges, rules, SGL, graphics, panels, GUI) | Chez's printed value, to the last bit of every flonum | `tests/test_*.py` against `fixtures/` |
| The SGL interpreter's Tcl command stream | Command for command with the original's `swl:tcl-eval` | `tests/test_sgl.py` |
| The GUI driven through its own widgets under Xvfb | Each run's trace equals its golden | `tests/test_gui.py` (`drive_gui.py`) |
| Installs (clean copy, `pip install -e`, regular install into fresh venvs) | Run 7's stdout and golden trace | `tests/test_install.py` |
| The Qt GUI, offscreen (loop0003): its canvas, fonts, panes, run controls, menus, clicks and layout | Display lists equal to tkinter's; every GUI run's trace equals its golden; menus, states and clicks equal to the tkinter GUI's | `tests/test_qt_*.py` |
| Installs with and without the `qt` extra (fresh venvs) | `metacat-qt` opens and closes headlessly; without PySide6 the engine and the tkinter GUI still run | `tests/test_install.py` |

In all there were **1412 tests** at the end of loop0002: 990 in the fast tier and 422
marked `slow`. Its final audit (item 17) ran the gate green in 6 min 57 s on 32 cores.
With the Qt GUI's tests (loop0003) there are 1640: 1198 fast and 442 slow, about 10 min.

## Speed

From [`docs/python-run-times.md`](../docs/python-run-times.md) (all 109 golden runs as
processes, Python 3.12.13 against Chez Scheme 10.0.0, measured before the speed-ups of
item 12):

- **Per codelet**, Python was about 9× slower than Chez: 1.23 ms against 0.13 ms. The
  Racket port takes about 0.2 ms ([`docs/run-times.md`](../docs/run-times.md)).
- **Per process**, the gap is smaller: 340 s against 112 s for the 109 runs (3.0×). The
  oracle compiles the 44 original files at every start (0.71 s). Python loads cached
  bytecode and builds the Slipnet in 0.05 s, so on short runs Python finishes first.
- **Item 12's seven speed-ups** keep every run identical. They are marked `speed (item 12)`
  in the code: arithmetic fast paths, identity `memq`/`remq`, `weighted-index`, the trace
  wrappers, `tell-all`, and `sort-by-method`. Together they cut function calls by about a
  third and CPU time by 25–37%. After them, the benchmark runs take 0.43–1.3 ms per
  codelet.

The per-problem table wasn't re-measured after the speed-ups, because the machine was never
idle (listed in [`docs/follow-ups.md`](../docs/follow-ups.md)).

## Tests

```bash
bash python/run-tests.sh          # full tier, what the gate runs: about 10 min on 32 idle cores
bash python/run-tests.sh --fast   # fast tier: skips tests marked slow (about 10 s)
bash python/run-tests.sh -k golden   # any other arguments go to pytest
```

`run-tests.sh` runs `python3 -m pytest -x -q` from `python/` and stops at the first
failure. The full tier needs Chez Scheme 10 (`scheme` or `chezscheme`) for the live-oracle
checks and re-captures, and `xvfb-run` for the GUI tests, which never open windows on the
real screen. The fast tier needs no display, and needs Chez only
for `scheme --version` in two `SOURCES` checks. Each test file is described in
[`tests/README.md`](tests/README.md).

## Further reading

- [`docs/python-translation-plan.md`](../docs/python-translation-plan.md): the plan, the
  Chez semantics, objects, names, modules, the audit and the list of `# chez:`/`# 1.2:`
  sites.
- [`docs/anomalies_and_quirks.md`](../docs/anomalies_and_quirks.md): bugs and oddities of
  the original, Chez, Racket and Python, including the Python-specific traps.
- [`docs/trace-format.md`](../docs/trace-format.md): the trace format, the PRNG
  specification and the number formatting.
- [`docs/python-run-times.md`](../docs/python-run-times.md): timings and speed-ups.
- [`docs/follow-ups.md`](../docs/follow-ups.md), section "Python (loop0002)": what a next
  loop could do.
- [`docs/code-map.md`](../docs/code-map.md): one paragraph per original file.
