# Follow-ups for a second loop

Written by the final audit of loop0001 (item 17, 2026-10-03). Loop0001 made a faithful,
line-for-line port: it reproduces the oracle event for event on the 109 golden runs and
on 720 runs with other seeds (docs/extra-seeds.md). Nothing below is needed for that.
These are the things loop0001 deliberately left alone ("faithful first, idiomatic later"),
plus what the audit noticed. Each item should keep the golden and extra-seed equivalence
unless it says otherwise; anything that changes a run is a divergence and goes in
docs/divergences.md.

## Safety net to keep first

- **Keep the oracle and the goldens as the regression net for every clean-up.** Before
  a refactor, run `bash tests/run-tests.sh` and `python3 tests/extra-seeds.py`. Consider
  putting a short extra-seed run (`--seeds 2`, about 15 s) in the gate, so that
  refactoring has more than the 109 golden runs behind it.
- **Gate time** is about 10 minutes, most of it the differential batteries
  (bridge-diff-test.rkt about 85 s, rule-diff-test.rkt about 2.7 min), views-test.rkt
  (about 70 s) and dist-test.rkt (a 70 MB build). Now that the goldens cover full runs,
  the codelet-level harness batteries (items 07–09) could run with smaller caps, or
  move to a slower nightly tier.

## Idiomatic clean-up

- **Module structure.** The engine is one module that `include`s 37 `.rktl` files
  (porting-notes.md, item 04), because the original's files are mutually recursive and
  `set!` each other's globals. A second loop could split it into real modules along the
  dependency graph in docs/code-map.md, with explicit exports, and replace `set-global!`
  (a whitelist of globals that outside code may set) with parameters or explicit hooks.
- **Objects.** About 4,000 `(tell obj 'msg ...)` call sites dispatch on closures through
  `record-case` (utilities.ss). Racket structs or `racket/class` would give arity
  checking and error messages that name the method. Do this file by file, keeping the
  goldens green.
- **Chez evaluation order.** Each call site where two arguments draw random numbers was
  ported as an explicit `let*` in Chez's order (`port:` marks, porting-notes.md tables).
  Idiomatic code should keep that order explicit, never relying on argument order.
  compat's right-to-left `map` and Chez's `sort` algorithm are needed for equivalence and
  should stay in one place, with comments.
- **engine/pending.rktl** now holds only the four names the original never defines
  (`*temperature-clamped?*`, `*initial-slipnode-unclamp-time*`, `same-direction?`,
  `complement-codelet-pattern`). The first two belong with run.ss's globals and the last
  two can go, along with their dead callers. Then delete the file.
- **Graphics split.** Parts of the graphics files live in the engine (pexp builders,
  `group-event-pexp-text-string`, `relation-name`, the EEG) because the model calls them
  (porting-notes.md, items 13–14). A cleaner boundary is a view-observer interface: the
  model emits events, views subscribe. That would also remove view-globals.rktl's
  `#f` colours and fonts from the engine.
- **compat.rkt** reimplements Chez's printer (`format`, `number->string` for flonums),
  `random`, `sort` and `record-case`. Keep the parts that matter for equivalence (RNG,
  sort, flonum printing in traces). The rest could become plain Racket once the callers
  stop depending on Chez's exact output.
- **Dead and latent code in the original** (docs/anomalies_and_quirks.md): the
  never-called singleton-group proposers, `bonds-equal?`, `get-complement-codelet-pattern`,
  `init-env`'s untellable default font, the Memory's dead first-icon spacing.
- **Divergence candidates, each needing a decision and a divergences.md entry.** These
  are suspected bugs in the original, ported faithfully. Fixing any of them changes runs
  and needs new goldens produced from a deliberately patched oracle, never by hand:
  - `letter-category-mappable-objects?` compares object1's group category with itself
    (always true);
  - a string image's `new-alpha-position-category` sends `new-start-letter`;
  - `answer-justifier` sends `get-constituent-objects` to a letter (the
    `report-error-and-halt` runs, e.g. `eqe qeq abbba aaabaaa` seeds 3 and 3401132640);
  - `transcribe-to-english` crashes on `abc ccbbaa ijk` seed 3 (`caddr` of `#f`).

## Performance

- Per codelet the port is about **2× slower** than Chez (docs/run-times.md: ≈200 vs
  ≈97 ms per 1000 codelets). Nothing was optimised. Profile first (`raco profile`
  on `racket/cli.rkt`). Likely costs:
  - closure-and-`record-case` message dispatch on every `tell`;
  - exact rational arithmetic, which equivalence requires;
  - the compat wrappers (`map` in Chez's order, `sort`);
  - list-based tables (the original has no hash tables).
- Splitting the engine into real modules (see above) is a readability change first. One
  big module does not stop Racket's compiler from inlining (it may even help), so
  measure before restructuring for speed.
- The GUI repaints a whole window's display list on change (racket/gui/gui.rkt's 50 ms
  tick). Long runs make long display lists, because erasing adds items
  (anomalies). Pruning covered items, or a backing bitmap per window, would keep long
  GUI runs fast.
- Each golden or extra-seed run needs a fresh engine because the Memory outlives a run
  (anomalies). A `reset-engine!` that rebuilds the global state would make batch runs
  cheaper than one process or namespace per run.

## New features

- **Batch statistics**: run a problem over N seeds and report the answer distribution,
  mean quality, codelets and final temperature (as in the dissertation's Chapter 5
  tables and Copycat's answer bar charts). tests/extra-seeds.py already computes most of
  this from traces; a `cli.rkt --batch N` would make it a feature.
- **Save and load runs**: the trace format (docs/trace-format.md) is complete enough to
  replay a run's events in the GUI without re-running the model, and a problem plus
  seed reproduces a run exactly.
- **Packaging** for macOS and Windows (only Linux is built and tested;
  make-dist.sh and dist-test.rkt are Linux-specific), and HiDPI scaling of the windows
  (fonts are sized at a fixed 96 dpi).
- **GUI**: native widget colours (racket/gui cannot set them), window placement
  remembered across runs, an automated test of the theme edit mode (Clamp theme pattern,
  ported but only compiled), and a single-window layout as an alternative to eleven
  frames.
- **Problems beyond the letter-string domain** belong to a different project; the
  docs/robotone_numbo_metacat_* notes compare Metacat with its FARG relatives.

## Python (loop0002)

Written by the final audit of loop0002 (item 17, 2026-10-04). The Python port in
`python/` reproduces the oracle byte for byte on the 109 goldens, on the 720 extra-seed
runs (exit code, stdout and trace hash), on the CLI's output and on every differential
battery. The GUI runs give their goldens' traces too. The Racket items above still
apply to `racket/`. These are the Python port's own items. As above, an item must keep
the goldens and extra seeds identical, or else become a divergence in docs/divergences.md.

### Safety net

- `bash python/run-tests.sh` is the gate's tier: about 7 min on 32 cores, half of it the
  720 extra seeds and their oracle re-capture (test_extra_seeds.py), which only matter
  when the engine changes. The `--fast` tier (about 10 s) runs one short golden (a b z seed 1). A middle
  tier would help a refactoring loop: the 109 goldens (35 s in forks) plus the batteries'
  fast cases.
- `python/tests/quirk_sites.py --write` keeps the plan's list of `# chez:` and `# 1.2:`
  sites, and test_quirk_sites.py fails when it drifts. A clean-up that moves a site
  must keep its comment. Removing a site's comment should mean the site is gone, and
  the list's diff says so.
- `docs/python-run-times.md`'s main table predates item 12's speed-ups, and it and the
  speed-up timings were measured on a shared, loaded machine. Rerun
  `python3 python/oracle/bench_runs.py docs/python-run-times.md` on an idle machine.
  It rewrites the file, so add the Speed-ups section back afterwards.

### Idiomatic clean-up

- **Objects.** Every model object is a `SchemeObject` subclass answering `tell(obj,
  "msg", ...)` through a per-class message dict (candidate C3 of the plan), with
  `delegate` to parent objects. Plain methods (`obj.get_strength()`) would be
  faster and readable, but `tell`'s dispatch, `delegate`'s receiver and `INVALID` answers
  are where the original's halts come from (anomalies: `report-error-and-halt`). Convert
  per message family, keeping the halt runs and goldens green.
- **Module boundaries.** Cross-module references are written `module.name` and are
  resolved at call time, because the original's files are mutually recursive and `set!`
  each other's globals. `engine.set_global` takes Scheme names. As for Racket: real
  dependencies along docs/code-map.md, then explicit hooks instead of global writes.
- **Model/graphics coupling.** Seven engine modules hold graphics code that the model
  calls: `general_graphics`, `group_graphics`, `bridge_graphics`, `rule_graphics`,
  `trace_graphics`, `theme_graphics` and `eeg_graphics` (pexp builders, the EEG, ungated
  `group-graphics 'erase`), plus `view_globals`' colours and fonts set to `#f`. An
  event interface from model to views would remove them, as for Racket.
- **Test-side translations.** The batteries' fakes and set-ups are translated in
  `python/tests/` (codelet_harness.py's `STAND_INS`, test_rules.py's fake Trace and
  Memory, engine_stubs.py). They are faithful to `tests/diff/`, so they are verbose.
  They could share one fixture module per battery family.
- **Speed-up assumptions** (plan, "As built (item 12)"): `sort-by-method` caches pure
  keys, and `memq` compares by identity only where `eq?` is identity. A new caller with an
  effectful sort key needs the uncached form.
- **Latent code paths** that are translated but never run headless or in a test:
  `apply-transforms`' `cadr` of `#f` on a GroupCtgy transform without a BondFacet
  transform, and the REPL's `ask` (minimal reader). `Breakpoint` resumes once, where a
  Chez continuation could be re-entered. Nothing in the original does that.
- **Divergence candidates** are the original's, listed above. The Python port
  reproduces each of them (the `caddr` crash with exit code 1, the halts).

### Performance

- About 9× slower than Chez per codelet before item 12 (1.23 vs 0.13 ms), about 25% less
  CPU after it. `tell` dominates (36 M calls in a 14,000-codelet run), then Chez arithmetic's
  type checks, `get-removal-weight` (every post to a full coderack weighs every codelet)
  and `memq`. Not yet tried: method calls in place of `tell` on the hottest messages,
  caching the highest bin's urgency in `delete-codelets`, and PyPy. PyPy is untested: the
  printer's tie correction and `float(Fraction)` would need the vectors of the chez
  fixtures re-run there.
- A golden run needs a fresh process because the Memory outlives a run, and the test
  session's engine is shared. A `reset` of every module's globals would make batch runs
  and the test suite cheaper.

### Features

- A batch mode (`python3 -m metacat ... --seeds N`) with answer statistics, as for Racket.
- GUI: HiDPI (fonts are fixed at 96 dpi with `tk scaling` 96/72); the theme edit dialog
  is only tested through Cancel; window placement isn't remembered across sessions.
  On a model error, a GUI run goes to input mode with an "Error: ..." line. The original
  dropped into the REPL.
- Only Linux with Tk 8.6 under Xvfb is tested; macOS and Windows Tk are untried.

## Python Qt GUI (loop0003)

Written by the final audit of loop0003 (item 11, 2026-10-05). `python3 -m metacat.qt`
(`metacat-qt`) shows every panel of the original in one window, with the control strip,
the menus, the dialogs, the mouse and the keys of the tkinter GUI. Each element of the
tkinter inventory (`python/tests/data/tk-gui-inventory.json`) is mapped to the Qt tests
that check it (`python/tests/test_qt_audit.py`'s `COVERAGE`), and the GUI runs give their
goldens' traces. Watching must keep never changing a run: the engine is frozen, and every
item below must keep the goldens and the GUI scenarios' traces identical.

### Safety net

- The Qt tests are about 3 minutes (209 tests) of the gate's 10-minute Python tier (`bash
  python/run-tests.sh --qt` runs only them). They need `QT_QPA_PLATFORM=offscreen`, and
  `conftest.py` runs them last because the offscreen `QApplication` starts a thread and
  other tests fork. Keep both.
- The rule from the deadlock (docs/anomalies_and_quirks.md, "Python's cyclic garbage
  collector freed a Qt widget on a worker thread"): a Python-owned Qt object must never be
  freed off the GUI thread. Keep `hosts.collect_on_gui_thread()` installed, never call
  `gc.collect()` from a worker, and give a new dialog `deleteLater` in its `closeEvent`
  as `SwlDialog` does.
- The tkinter references (`tk-display-lists.json`, `tk-fonts-colors.json`,
  `tk-clicks.json`, `tk-gui-inventory.json`) were taken with Anaconda's Tk 8.6 under
  Xvfb. Their slow tests regenerate them; a different Tk (with Xft, say) would change
  the font metrics and need a new reference, not a looser test.

### Idiomatic clean-up

- `QtControlPanel` (`controls.py`, about 1,300 lines) copies gui.py's control panel
  message by message, with its own copies of the menu helpers. Both GUIs could share the
  model side (`gui.py`'s actions already are shared) and keep only the widgets apart.
- `drive_qt_*.py` share a pattern (a driver thread, `on_main`, a watchdog, a JSON report)
  with duplicated helpers; `drive_qt_audit.py` already imports `drive_qt_menus.py`'s. A
  small driver module would remove the copies.
- `HiddenCanvas` keeps one display list per thread to dodge the original's race on its
  one hidden canvas (anomalies, "Two threads measuring text on the one hidden canvas").
  A measuring API that doesn't draw would be simpler, but fonts.ss's `get-pixel-size` is
  the original's protocol.

### Performance and polish

- Run 7 at the slider's fast end: 6.5–7.1 s in the Qt GUI against 6.2–6.6 s in the
  tkinter GUI (item 05, on a loaded machine). The paint gate costs most of the
  difference; it buys a GUI that answers within 0.1 s. Not tried: drawing each pane into
  a `QPixmap` cache, and replacing the per-item recorded painter calls with one
  `QPicture` per pane.
- At start the panels draw at their original sizes inside their panes, then redraw at
  the pane sizes over about two seconds, one panel per 250 ms pause of the original's
  resize listener. A first layout that told each panel its pane size before its first
  draw would avoid the visible jump.
- A hidden pane still gets configures (anomalies, "A hidden pane still gets a
  configure"); the EEG redraws once at start while hidden.
- The mouse wheel doesn't scroll the canvases (only the scroll bars), as in the tkinter
  GUI. A press in a letterbox margin does nothing (divergences.md).
- Only Qt's offscreen platform is tested. The window has never been opened on a real X11
  or Wayland screen by the loop (the owner's rule), nor on macOS or Windows.
- The tkinter GUI still has the port bug in which two errors within 700 ms leave the
  first on the control panel (anomalies); the Qt GUI has the fix, which gui.py could
  copy.

### Racket's one window

- `racket racket/one-window.rkt` (loop0003 item 10) places the panes by fixed
  proportions. It has no draggable borders (racket/gui has no splitter) and no saved
  layout. A hand-made splitter on `layout-panel%` and a preferences file would bring it
  level with the Qt GUI.
