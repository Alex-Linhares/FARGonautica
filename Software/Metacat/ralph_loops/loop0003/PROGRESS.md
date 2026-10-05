# Progress Log

## Ralph Loop 0003 Status
- **Started**: (not started)
- **Target**: 12 items
- **Current**: 12/12 SOLVED

---

## Iteration 1 — 2026-10-04 15:29:35
### Completed
- (driver) session ended with outcome `error` without marking the item
### Blockers
- see session_it01.log
### Next
- revisit or re-open this item

---

## Iteration 2 — 2026-10-04 16:02:02
Item **01 Skeleton and test harness**: SOLVED.

### Completed
- **Tests first, and they failed first.** `python/tests/test_qt_skeleton.py` and
  `python/tests/test_tk_canvas_inventory.py` were written before any code. First run:
  the inventory test failed at collection (`ModuleNotFoundError: tk_canvas_inventory`);
  the skeleton test gave 7 failed, 1 error, 2 passed (no `metacat.qt`, no `qt` extra,
  `run-tests.sh` not wired; the skip test failed with a pytest-qt `INTERNALERROR`, see
  below). `test_qt_tests_run_after_the_others` was added later, also failing first.
- **`python/metacat/qt/`**: `__init__.py` (`has_pyside6()`, no Qt import),
  `__main__.py` and `app.py` (`python3 -m metacat.qt [--quit-after MS]`), `mainwindow.py`
  (an empty `QMainWindow` titled Metacat, 1366×768), `grab.py` (`grab_png(widget, path)`:
  `QWidget.grab()` to a PNG). Screenshot of the empty window inspected (plain grey, the
  right size).
- **Harness**: `pyproject.toml` has the `qt = ["PySide6>=6.5"]` extra and the
  `metacat.qt` package (test_install.py updated); conftest.py has the session `qapp`
  fixture (forces `QT_QPA_PLATFORM=offscreen`, closes all windows and quits at the end)
  and `grab(widget, name)` (PNGs in `python/tests/screenshots-qt/`, git-ignored). Every
  `test_qt_*.py` starts with `pytest.importorskip("PySide6...")`; a test runs a fresh
  pytest with PySide6 hidden and checks that every Qt file is skipped, and another checks
  the guard convention. `run-tests.sh` exports `QT_QPA_PLATFORM=offscreen`, unsets
  `WAYLAND_DISPLAY`, and has `--qt` (Qt tests only). Qt tests sit in the normal tiers
  (fast unless marked slow).
- **Two environment fixes, both logged in `docs/anomalies_and_quirks.md`:**
  - pytest-qt (installed on this machine) aborts all of pytest when no Qt binding
    imports. `addopts = "-p no:pytest-qt"` in pyproject.toml; `-p no:pytestqt` silently
    does nothing;
  - the offscreen `QApplication` starts a thread, and later test files fork (32
    "fork() may lead to deadlocks" warnings in the first gate run). conftest.py now runs
    the `test_qt_*.py` files last; the second gate run had no warnings.
- **The Tk canvas command inventory**, `python/tests/data/tk-canvas-commands.json`, made
  by `python/tests/tk_canvas_inventory.py` from three parts:
  - code: `ast` scan of every `tcl_eval`, `swl_tcl_eval` and `.tcl` call in
    `python/metacat/`. A computed command outside the three known forwarders is an error;
  - fixtures: both `sgl-tcl` streams (636 lines);
  - runs: recording canvases (and a recording hidden canvas) in run7 and
    `abc abd xyz wyz` seed 1 with every view attached (120,344 commands, 3.3 s).

  It lists 14 commands: create rectangle, line, oval, arc, polygon and text with their
  options; delete, move, itemconfigure (`-state`, `-tags`), bbox, raise and scale (only
  in the fixtures, never in a run), and canvasx/canvasy (code only). It also lists the
  enumerated values (anchor s/nw, style arc/pieslice, state hidden/normal, dash
  ""/"- "/". "), the target forms, the 22 tag words (always single words) and the
  canvas methods. The fast tests compare the code and fixture parts with the committed
  list, and a synthetic panel module that sends `create window`, `itemconfigure -fill`
  or `lower` is caught. The slow tests compare the runs part and the whole list. Summary
  table added to `docs/qt-gui-plan.md` 2.6.
- `python/tests/README.md`: rows for both test files and the helper, the Qt fixtures,
  `--qt`, and the new totals (1439 tests: 1014 fast, 425 slow).
- Gate: `python3 ralph_loops/loop0003/gate.py`: GATE PASSED (1439 passed in 8:01, no
  warnings; racket/ unchanged).

### Blockers
- None for item 01.
- Item 00 is still marked `[!]` because session 1 lost its API connection while running
  the gate. Its deliverables were committed (`docs/qt-gui-plan.md`,
  `tk-gui-inventory.json`, `tk_gui_inventory.py`, `test_tk_gui_inventory.py`), and their
  tests pass in this iteration's full gate. A later session (or the owner) can re-check
  it and mark it `[x]`; this session left it alone.

### Next
- Item 02, the Qt canvas: implement exactly the commands in `tk-canvas-commands.json`
  (display list first, then the scene), and test it against tkinter by replaying the
  `sgl-tcl` streams.

---

## Iteration 3 — 2026-10-04 16:41:46
Item **02 The Qt canvas**: SOLVED.

### Completed
- **Tests first, and they failed first.** `python/tests/test_qt_canvas.py` and its helper
  `python/tests/canvas_streams.py` were written before any code. The tkinter reference
  `python/tests/data/tk-display-lists.json` was generated, then the first run gave
  `12 failed, 2 passed` (`ModuleNotFoundError: metacat.qt.canvas`). After the first
  implementation, the exact comparison failed on Tk's polygon bbox rule (see below), and
  then on a wrong expectation in my own scene test (it forgot the paint margin).
- **The reference** (`canvas_streams.py`, under `xvfb-run`, under a second): the two
  `sgl-tcl` fixture streams (parsed directly, because the test reader can't read
  `\x2D;`), and a synthetic stream that reaches what the fixtures don't. That covers
  arrows (first, last, both), `-smooth`, `-justify` with multi-line text, all nine
  anchors, `lower`, `raise` above an item, ids as targets, tag lists, `scale` (negative
  too), `move all`, hidden, disabled and fill-less items, colour forms (`#abc`,
  `#123456789`, CamelCase names), and 160 random arcs, rectangles, ovals, lines and
  polygons at fractional coordinates. Each stream is replayed into tkinter Canvases. The
  dump records the display list through `find all`, `type`, `coords`, `itemcget` (every
  option of the item's kind), `gettags` and `bbox`, plus the answers of every `create`,
  `bbox` and `canvasx`/`canvasy`, and Tk's own text metrics (`font measure`, `font
  metrics -linespace`).
- **`python/metacat/qt/displaylist.py`** (no Qt): Tk 8.6's display list, with ids that
  are never reused, the stacking order, tag/id/`all` searches, `create` with Tk's
  defaults and option checks (errors as `TclError`), `delete`, `move`, `scale`,
  `raise`/`lower` (Tk's RelinkItems), `itemconfigure`, `bbox` with Tk's per-kind rules
  (rect/oval bloat, arc end points and quadrants, line and polygon fudge, arrowheads,
  text anchors with ROUND and the cursor fudge), `canvasx`/`canvasy`, and the queries
  above. Colour words are read as Tk 8.6 reads them. Arc angles are normalised, and
  rectangle/oval/arc corners are sorted. One `RLock` covers each command, so any thread
  may draw. The display list records dirty ids and a "restacked" flag for the scene.
- **`python/metacat/qt/canvas.py`**: `QtCanvas`, with `tcl` (answers shaped like
  `swl.TkCanvas`'s), `get_background_color`, `set_background_color_bang`, `set_origin`,
  and `sync()`, which runs on the GUI thread and applies the changes to the
  `QGraphicsScene`. `TkItem` paints each kind as X11 does: rounded corners, Tk's pen
  widths and DashConvert patterns, butt caps, round joins on lines and polygons, pies and
  chords, arrowheads, Tk's spline for `-smooth`, and justified multi-line text. There
  is no antialiasing except on text. **`python/metacat/qt/fonts.py`**: Tk font word →
  `QFont` (negative size = pixels, positive = points at 96 dpi, styles), and
  `QtMetrics`, which measures with the same `QFont`. This is a first mapping; item 03
  completes it.
- **Results:**
  - with Tk's text metrics, the Qt display list is **identical** to Tk's for all three
    streams (ids, order, kinds, coordinates, every option, tags, every bbox, `bbox all`,
    every answer);
  - with Qt's fonts, only text extents differ: widths by 1 px at most, heights by 5 px
    at most (tolerance 6);
  - every inventoried command and option is accepted, and the Tk errors are raised;
  - 8 threads drawing at once produce 1600 unique ids;
  - the scene follows the display list: z order after `raise`, hidden items, delete,
    move, background.
- **Pictures, inspected:** `sgl-fixture.scm` drawn through `metacat/gui/sgl.py` onto the Qt
  canvas, with text measured on a Qt hidden canvas, passes all 16 of
  `render_sgl_fixture.py`'s pixel checks. 97.7% of its pixels match
  `docs/screenshots/panels/sgl-fixture-python.png` within 24 levels. I looked at it next
  to the tkinter one, at 3× zoom on rectangles, arcs, dashes and the "erase" cell. The
  differences are antialiased text a pixel lower (Qt's taller metrics), dash phase,
  slightly rounder 2-px circles, and X11's jog in the 5-px diagonal line, which Qt draws
  straight. Committed as `docs/screenshots/panels/sgl-fixture-qt.png`, with a section in
  `docs/screenshots/README.md`. The frozen v1 stream replayed into the Qt canvas passes
  the same pixel checks.
- **Logged** in `docs/anomalies_and_quirks.md`:
  - Tk 8.6 gives five colour names web values (TIP 403), and reads `#abc` as `#aabbcc`;
  - Tk 8.6.13's measured bbox rules: a polygon's outline adds `(int)(width+1)/2`;
    `canvasx` rounds to a whole pixel; a text without fill has no bbox;
  - Qt's font metrics are taller than Xft's for the same faces (open, item 03);
  - X11's jog in wide lines.

  `docs/divergences.md` has a new "Python Qt GUI" section, and `docs/qt-gui-plan.md` 2.6
  an "As built" paragraph: dirty ids instead of a journal, so one update per changed
  item per sync.
- `tk_canvas_inventory.py`: `metacat/qt/canvas.py`'s `tcl` added to `FORWARDERS`. It is a
  canvas that passes on what it's given. `python/tests/README.md`: rows for
  `test_qt_canvas.py` and `canvas_streams.py`, and new totals (1454: 1028 fast, 426
  slow).
- Gate: `python3 ralph_loops/loop0003/gate.py`: GATE PASSED (1454 passed in 7:37, no
  warnings; racket/ unchanged).

### Blockers
- None for item 02. Item 00 is still `[!]` from session 1 (its deliverables exist and
  their tests pass; see iteration 2).

### Next
- Item 03, fonts and colours: make Qt's text metrics match Tk's (the heights: Qt
  `ascent+descent` 15 against Tk's linespace 12 for 11-px Helvetica), then tighten
  `TEXT_TOLERANCE` in `test_qt_canvas.py`. Check the tiny Coderack labels. The colour
  reading is already in `displaylist.color_rgb`.
- Item 04 will need `QtCanvas.sync()` called on a timer (item 05 batches it) and
  `set_origin` from the view's scroll position.


---

## Iteration 4 — 2026-10-04 17:08:35
Item **03 Fonts and colours**: SOLVED.

### Completed
- **Tests first, and they failed first.** `python/tests/test_qt_fonts.py` and its Tk
  reference were written before any code. First run: `40 failed, 24 passed` (no
  `metacat.qt.fontspec`, no `families`/`install`/`descent`, Qt's heights too tall, the
  canvas accepting `-font {helvetica -11 heavy}`). Three of my own expectations were then
  wrong and were corrected, not loosened silently: `bbox` answers a list; Tk and Qt widths
  differ by up to 6 pixels for 22-pixel bold Helvetica (now 97% exact, 98% within 1, all
  within 6); antialiased 1-pixel stems straddle two columns, so the thin-letter test
  counts local minima.
- **The reference**: `python/tests/tk_fonts_colors.py` under Xvfb writes
  `python/tests/data/tk-fonts-colors.json`: Tk's ascent, descent, linespace, actual family
  and the widths of 8 samples for 252 font words (Helvetica, Times, Courier; −5…−30 pixels
  and 8…24 points; 4 styles), and `winfo rgb` of all 752 colour names of `colors.py`. Its
  slow test regenerates it and compares.
- **Finding: the tkinter GUI's Tk has no Xft.** Anaconda's `libtk8.6.so` links libX11
  only; Tk draws X core fonts, which the X server rasterises from
  `/usr/share/fonts/X11/Type1` as 1-bit bitmaps, and their ascent/descent are glyph-ink
  extents.
- **`python/metacat/qt/fontspec.py`** (no Qt): Tk 8.6's ParseFontNameObj: list form with
  any style words or style lists (later weight/slant wins, case-sensitive), the `-family
  -size -weight -slant -underline -overstrike` form, Tk's named fonts as Helvetica −12,
  size 0 = default, points → `(int)(pt·96/72+0.5)` pixels, and Tk's error messages. The
  display list now checks `-font` with it (an item with a bad font isn't created).
- **`python/metacat/qt/fonts.py`** completed: `qfont` from the spec; `QtMetrics` measures
  widths with `horizontalAdvance` and takes ascent/descent from the ink of the font's
  printable Latin-1 glyphs (`QRawFont.boundingRect`, rounded up), as X's core fonts do.
  Linespace is now within 3 px of Tk's for all 252 fonts (within 1 for 79%), ascent within
  1 for all; before, Qt's OS/2 win ascent made it up to 7 px taller. `families()` (Qt's
  families lower-cased, ` [foundry]` stripped, plus fonts.ss's preferred faces that
  fontconfig maps onto a real face rather than its fallback) and `install()` (fonts.ss
  picks faces from them and measures on a `HiddenCanvas`, a `QtCanvas` that drops its
  change records at each `delete`). Here: serif `times new roman` (Liberation Serif),
  sans-serif `helvetica` (Nimbus Sans), fancy `palatino linotype` (P052).
- **Measurement consistency** tested: a text item's `bbox` = measured width + Tk's cursor
  pixel by the linespace; `horizontalAdvance` of the drawing `QFont` equals the
  measurer's width; the drawn ink lies inside the bbox and fills it but the side
  bearings; `FixedFont get-pixel-size` after `install()` gives the Qt numbers.
- **Colours**: all 752 names read as Tk reads them (the 5 TIP 403 names differ from
  `colors.py`, as logged in item 02), upper case too; every `Rgb` of `colors.py`,
  `constants.py` and `*color-names*` round-trips through `swl.tcl_word`; the scene paints
  the exact RGB.
- **`TEXT_TOLERANCE` in `test_qt_canvas.py` tightened from 6 to 2** (fails at 1).
- **The UFO answered.** `python/tests/render_small_text.py` draws the Coderack's labels at
  5–11 px with Tk and with Qt: `docs/screenshots/panels/small-text-tk.png` and
  `small-text-qt.png` (inspected). The default Coderack is 598 px high, so the labels are
  8 px; Tk's core fonts drop the `i`s and `l`s at 8, 9 and 10 px (not 7 or 11); Qt draws
  every letter at the same widths. Test:
  `test_tiny_coderack_labels_keep_their_thin_letters`. The SGL fixture picture
  re-rendered and inspected: 97.8% of pixels match the tkinter one (was 97.7%);
  `docs/screenshots/panels/sgl-fixture-qt.png` updated.
- **Docs**: `docs/anomalies_and_quirks.md`: the UFO entry explained; "Qt's metrics taller"
  explained and worked around; new entries for the 22-px width differences (two hinters)
  and Qt's foundry-suffixed family names and alias resolution (Anaconda's `fc-match`
  answers KaTeX_AMS for everything). `docs/divergences.md` (Python Qt GUI): ink metrics,
  antialiased text, and the faces (Liberation Serif for serif where tkinter uses Nimbus
  Roman). `docs/qt-gui-plan.md` 2.6 "As built (item 03)". `docs/screenshots/README.md`:
  small-text section. `python/tests/README.md`: rows for the new test, reference script
  and render script; totals 1518 (1091 fast, 427 slow).
- Gate: `python3 ralph_loops/loop0003/gate.py`: GATE PASSED (1518 passed in 7:37; racket/
  unchanged).

### Blockers
- None for item 03. Item 00 is still `[!]` from session 1 (deliverables exist and pass;
  see iteration 2).

### Next
- Item 04 (the Qt host): call `fonts.install()` when the Qt GUI starts (before the
  panels select their fonts), call `QtCanvas.sync()` on a timer, and set `set_origin`
  from each view's scroll position. The Logo's scrollbar sizes still need Qt values
  (`QStyle.PM_ScrollBarExtent`).

---

## Iteration 5 — 2026-10-04 18:03:30
Item **04 Panels in panes**: SOLVED.

### Completed
- **Tests first, and they failed first.** I wrote `python/tests/test_qt_panes.py` and its
  scene script `python/tests/render_qt_panes.py` before any code. First run: `18 failed`
  (no `metacat.qt.hosts`, no `default_sizes`, no splitter tree). Then two of my own
  expectations were wrong and I corrected them: the Temperature test used a pane too
  narrow for its ratio, and the colour comparison counted black, the text colour (Tk's
  1-bit core fonts against Qt's antialiased text). Three tests came from bugs found while
  looking at the results, and each failed before its fix:
  - `test_a_pane_that_comes_back_to_its_size_still_gets_a_configure`, from a screenshot;
  - `test_fonts_measure_from_several_threads_at_once`, from a crash in one run in six;
  - the `--screenshot` timer order of `test_the_program_opens_every_pane`, a flake in the
    fast tier.
- **`python/metacat/qt/hosts.py`**:
  - `QtHost`, a `gui/hosts.py` host, makes a `Pane` (a `QWidget` with a `QGraphicsView` of
    the `QtCanvas` scene). It has no parent until it is placed, so it never opens a
    window of its own;
  - the resize policy of `docs/qt-gui-plan.md` 2.4, the same for every panel.
    Unscrollable windows are letterboxed at Tk's ratio (w+2):(h+2), centred (the
    Temperature at the top), with the margins in the panel's background. Scrolling ones
    fill the pane beside an always-shown scroll bar, whose size is Qt's
    `PM_ScrollBarExtent` (given to fonts.ss). A pane's size reaches the panel through the
    original's own protocol (`viewport.configure(w+2, h+2)`, make-resizable's handler,
    the resize listener), and only once the window is resizable;
  - a configure feeder: each pane's latest size waits until the single resize queue is
    empty, so every panel redraws (anomalies: "One resize queue for every window",
    updated);
  - scroll regions, `set_vertical_view` and show/hide from any thread are applied by
    `sync()` on the GUI thread. `canvasx` follows the scroll bars. `settle_resizes()` and
    `install()` (Qt fonts, scroll bar sizes, host maker).
- **`python/metacat/qt/mainwindow.py`**: the splitter tree of 2.2 (rows / top / middle /
  themes / bottom), not collapsible, with stretch on the Commentary, the Memory and the
  Trace, the EEG hidden, and minimum pane sizes. `default_sizes(w, h)` computes 2.2's
  proportions; they follow the window until the user drags a handle. A 50 ms sync timer.
  `DEFAULT_SIZE` is now 1920×1010 (1080p is the minimum).
- **`python/metacat/qt/app.py`**: `setup(window)` attaches every window on Qt hosts,
  places them and runs enable-resizing. `python3 -m metacat.qt` now shows every panel
  (no run yet), and `--screenshot PNG` grabs it before `--quit-after` quits.
- **`HiddenCanvas` has one display list per thread** (`qt/canvas.py`). fonts.ss's
  `get-pixel-size` is `create`/`bbox`/`delete all` on the one hidden canvas, and the
  engine and the resize listener measuring at once deleted each other's items. The
  original and the tkinter GUI share this race. Logged in anomalies.
- **Results:**
  - offscreen, run7 driven directly with every window in one window gives exactly its
    golden at the answer. At 300 and 800 codelets the trace is the golden's prefix;
  - `a b z` seed 1 (fast tier) also gives its golden;
  - every pane has items: at the answer Workspace 486, Slipnet 112, Coderack 156,
    Themes 25/12, Memory 46, Commentary 14, Trace 98, Temperature 24, EEG 297. The
    Bottom Themes are empty outside justify runs, as in the tkinter snapshots;
  - run7 with the window resized 14 times during the run gives its golden. Each panel
    redraws on the resize listener while the run goes on, and ends at its pane's size,
    with the Commentary at its last line.
- **Pictures, inspected**: I grabbed the window at codelets 300 and 800 and at the answer
  and compared each pane with `python/tests/snapshots/views/`, side by side for the
  Vertical and Top Themes, Commentary, Memory, Temperature and Trace. They are the same
  pictures at the panes' sizes, with antialiased text and wider Commentary paragraphs.
  The slow test checks that each pane keeps the snapshot's main colours (backgrounds and
  fills exact, black excluded). I also looked at the start screen of `python3 -m
  metacat.qt`.
- **Measured:**
  - run7 to the answer with every pane takes 6.8 s (engine in the GUI thread, offscreen;
    7.3 s for the whole process);
  - each panel's `resize` takes 1–26 ms at its 1080p pane size, so live dragging keeps
    the original protocol and no panel needs the `fitInView` fallback;
  - the startup redraw takes about 3 s (one panel per 250 ms listener pause).
- **Found, for item 05** (logged in anomalies, open): with the engine in a worker thread,
  Qt painting stalls for seconds. Each `TkItem.paint` and `boundingRect` is a Python call
  that waits for the GIL. The resize test therefore resizes at the run's
  `update-everything`, the seam run.py leaves to drivers.
- **Docs**:
  - `docs/divergences.md`: a new section, Python Qt GUI: windows as panes;
  - `docs/anomalies_and_quirks.md`: the GIL entry, the clamped-scroll bug (fixed), the
    hidden-canvas race, and the resize-queue update;
  - `docs/qt-gui-plan.md` 2.4: "As built (item 04)";
  - `python/tests/README.md`: rows for `test_qt_panes.py` and `render_qt_panes.py`, and
    new totals (1547: 1112 fast, 435 slow; fast tier about 20 s).
- Gate: `python3 ralph_loops/loop0003/gate.py`: GATE PASSED (1547 passed in 8:02; racket/
  unchanged).

### Blockers
- None for item 04. Item 00 is still `[!]` from session 1 (its deliverables exist and
  pass; see iteration 2).

### Next
- Item 05 (the engine thread):
  - solve the GIL stall first. Measure the GUI thread's Python calls per refresh while a
    worker runs, and paint with fewer Python calls (for example a `QPicture` or
    `QPainterPath` per item made at sync, or a scene index that doesn't ask Python for
    bounding rectangles), then time run 7 against the tkinter GUI;
  - `MainWindow.sync` already runs on a 50 ms timer, and `QtHost.sync` applies the
    deferred scroll and visibility changes;
  - mouse presses aren't wired to `viewport.mouse_press` yet (item 07).


---

## Iteration 6 — 2026-10-04 19:03:59
Item **05 Engine thread and run control**: SOLVED.

### Completed
- **Tests first, and they failed first.** I wrote `python/tests/test_qt_engine.py` and the
  driver `python/tests/drive_qt_gui.py` before any code. First run of the fast tier:
  `7 failed, 9 errors` (no `metacat.qt.engine_bridge`, no `metacat.qt.controls`, no
  `PaneView.paints`; the scene items still had a Python `boundingRect`). The first version
  of the test run also hung at exit; I fixed two test bugs that caused it (the busy thread
  wasn't a daemon; the driver didn't `os._exit` when building the GUI failed, so the
  resize listener kept the process up). Three more changes came from what the tests found,
  each failing before its fix:
  - the 50 toggles: run 7 went on to its answer during the toggling. A Stop can be
    lost: `go` switches to run mode before it clears `*interrupt?*` (also true of the
    original). The scenario now presses Go only in input mode, Stop once the run shows,
    and stops toggling at the answer;
  - responsiveness: the GUI thread took up to 0.41 s to answer during a run (limit 0.5 s),
    because the Workspace's paints (130 ms each at half the GIL) filled it. That led to
    the paint gate and the recorded painter calls (below);
  - the repaint test was flaky in the full suite (13 and then 7 ticks, where earlier tests'
    windows and timers share the process), so it now runs `qt_paint_probe.py` in a fresh
    process.
- **`python/metacat/qt/engine_bridge.py`**: `GuiInvoker.post` (a queued signal; at once on
  the GUI thread) and `call(fn, timeout)` (waits for the GUI thread's value; directly on
  the GUI thread, and `TimeoutError` instead of a hang). `EngineBridge` reuses
  `metacat.gui.app.EngineThread` as the REPL thread and sets `setup.g_repl_thread` and the
  Workspace's thread-break handler.
- **`python/metacat/qt/controls.py`**: `QtControlPanel` is gui.ss's control panel object,
  with the same messages and model effects as gui.py's: init/run/resume/reset problem,
  the run, input and disabled modes (`_enable_all`'s table), breakpoint messages, display
  and display-error (700 ms), engine-error, verbose mode, plus `run-demo` (the demo menu
  item's action). Its widgets are a strip at the top of the window: info label, command
  line (Enter), the speed slider, Step, Go, Stop, Reset, the breakpoint label and the
  self-watching warning. Enter, the buttons and the slider run gui.py's own actions. An
  Options menu has Set breakpoint, Clear breakpoint and Step mode interval, with gui.ss's
  input dialog in Qt. A widget change from another thread is posted to the GUI thread.
  When the engine parks, the window syncs at once.
- **`app.setup`** now loads the engine with `engine.load()` (not `headless.prepare()`,
  which replaced `break`), makes the bridge and the control panel, and places the strip in
  a fixed tool bar (`MainWindow.place_control_panel`). At quit with a run going,
  `main` leaves with `os._exit`.
- **Painting while the engine runs (the GIL stall that item 04 found), fixed:**
  - `TkItem` is a `QGraphicsRectItem` with no Python `boundingRect`;
  - `PaneView.paintEvent` enters the paint pass from Python, and the panes' margins
    are painted by Qt (`autoFillBackground`);
  - `canvas.PAINT_GATE`: the GUI thread holds it while it syncs and paints, and canvas
    commands wait for it;
  - PySide6's constructors release the GIL (a `QColor` costs 0.5 ms while another
    thread runs Python), so each item records its painter calls at its first paint after
    a change and replays them. A Workspace paint went from 22 ms to 6 ms; most items are
    deleted before they're ever painted;
  - colour words are cached in `displaylist.color_rgb`.

  `qt_paint_probe.py`: 38–42 ticks and paints in 2 s; with `--without-fix`, 2 ticks.
- **Results** (`drive_qt_gui.py`, offscreen, about 28 s): each of these scenarios gives
  exactly its golden trace:
  - start (invalid input, the slider at Fast);
  - a full run (`abc abd ijk 1`, Enter, Go);
  - step mode (interval 40 through the dialog: steps at 40, 80, 120, then Go);
  - a demo (Run 7 via `run-demo`) stopped mid-run and restarted;
  - a breakpoint at 100, then Go;
  - Reset with an empty line;
  - a justify run (`abc abd ijk abd 1`);
  - 50 rapid Go/Stop toggles, then the run to its answer (no deadlock; worst toggle
    0.16–0.22 s).

  While run 7 runs, the GUI thread answers within 0.045–0.094 s and the panes paint
  about 150 times in the measured second or two. I grabbed the window at run 7's answer
  and looked at it: every panel is animated in its pane, and the strip is on top.
- **Timing, run 7 from Go to its answer, slider at Fast** (interleaved, 32 cores at a
  load average of about 22): **tkinter GUI 6.15 / 6.56 / 6.34 s** (`drive_gui.py
  invalid_input timing` under xvfb-run); **Qt GUI 6.46 / 7.14 / 7.01 s**. Before the paint
  gate the Qt run took 5.3–5.7 s, but the GUI then answered in up to 0.41 s and the
  Workspace repainted 4 times a second (now about 17). Recorded in
  `docs/qt-gui-plan.md` 2.5.
- `drive_gui.py` gained a `timing` scenario that runs only when named (the default run
  and `test_gui.py` are unchanged).
- **Docs**:
  - `docs/anomalies_and_quirks.md`: the GIL entry is updated to fixed, with the
    constructor finding and the probe's numbers; new entries for the lost Stop
    (original) and Qt offscreen's `propagateSizeHints()` warning;
  - `docs/divergences.md`: a new section, Python Qt GUI: the engine thread and the run
    controls;
  - `docs/qt-gui-plan.md` 2.5: "As built (item 05)";
  - `python/tests/README.md`: rows for `test_qt_engine.py`, `drive_qt_gui.py` and
    `qt_paint_probe.py`, and new totals (1564: 1128 fast, 436 slow).
- Gate: `python3 ralph_loops/loop0003/gate.py`: GATE PASSED (1564 passed in 8:53; racket/ unchanged).

### Blockers
- None for item 05. Item 00 is still `[!]` from session 1 (its deliverables exist and
  their tests pass; see iteration 2).

### Next
- Item 06 (control strip and menus):
  - add Demos (with highlighting, via `run-demo`), View, the rest of Options, Memory,
    Help and Save commentary to the menu bar;
  - add the Clear Memory and theme-edit dialogs (`ready-to-edit?` answers #f until
    then);
  - extend `QtControlPanel.menus` so that run mode disables them all;
  - the input dialog in `controls.py` can be reused.
- Mouse presses on panes aren't wired yet (item 07). The breakpoint scenario resumes
  with Go, not with a click on the Workspace.

---

## Iteration 7 — 2026-10-04 19:32:43
Item **06 Control strip and menus**: SOLVED.

### Completed
- **Tests first, and they failed first.** I wrote `python/tests/test_qt_menus.py` and its
  driver `python/tests/drive_qt_menus.py` before any code. First run: `20 failed` (the
  driver stopped at the first state: no Demos, View, Memory or Help menus, no
  `command_line_look`). The extra test `test_the_strip_fits_a_1080p_window_with_every_message`
  came from a screenshot (the window grew to 2015 px once the self-watching warning
  showed) and failed before its fix. While the tests went green, five of my own
  expectations were wrong and I corrected them rather than the code: problems as
  `update-current-problem` stores them (5 elements); Tk's help line count (`end-1c` counts
  the empty last line, as Qt's `blockCount` does); the theme state is taken before edit
  mode deletes the themes; the self-watching inventory also lists the EEG and Logo
  (hidden from the start); the commentary faces are Qt's (item 03).
- **`python/metacat/qt/controls.py`**:
  - the menu bar in the order Help, Demos, View, Options, Memory. Every item of
    gui.ss/gui.py is there, with its label, SWL font (the action's font and its
    `swl-font` property), kind and action;
  - **Demos**: all 35 demo items in their submenus (`gui.DEMO_ITEMS`, `gui.figure`).
    Each calls `init-new-problem` and is highlighted (checked) until the next problem;
  - **Options**: Set/Clear breakpoint and Step mode interval (item 05), the seven check
    items with gui.py's actions (Eliza, Slipnet graphics, Coderack graphics, codelet
    counts, last codelet type, self-watching, verbose), Clamp theme pattern, Clamp codelet
    pattern ▸ (5), Undo last clamp, Commentary font face ▸ (12, each in its own font) and
    size ▸ (6), with gui.ss's update-menu-fonts and highlighting, and Save commentary to
    file (Qt's file dialog; `set_file_dialog` for tests). Self-watching off shows the
    warning, disables the three clamp items, undoes the last clamp, deletes the themes and
    hides the Themes panes;
  - **View** (`attach_windows`): gui.ss's window controllers for the 11 panes (checkable
    actions), Show/Hide all panes and Reset layout;
  - **Memory > Clear Memory** and **Help > Metacat help**;
  - the dialogs: `SwlDialog` (SWL's toplevel: the destroy handler runs once on close,
    Escape or `destroy`), `confirm_dialog` (Clear Memory; the yellow theme-edit dialog
    with gui.ss's instructions) and the Help window (help.txt, Courier on bisque,
    read-only, word wrap, one window only);
  - the control panel's remaining messages: `ready-to-edit?`, `edit-theme-type`,
    `raise-theme-edit-dialog`, `theme-edit-mode-on/off`, `clear-memory`, `hide-window`,
    `raise`, plus the rest of select-control-panel-fonts. Run and disabled modes disable
    the Demos, Options and Memory cascades only, as gui.ss does.
- **`mainwindow.py`**: `place_control_panel` fills the menu bar; `set_pane_visible`,
  `update_splitters` (an empty nested splitter hides itself) and `reset_layout`.
  `QtHost.sync` reports a visibility change, so the splitters follow. The info label is
  wide enough for a problem and its seed (it was clipped). The breakpoint message and the
  warning are stacked.
- **Results** (`drive_qt_menus.py`, offscreen on a 1920×1200 screen like the inventory's,
  about 5 s): the Qt menu tree equals the inventory's, mapped as `docs/divergences.md`
  says, and so does every item state and control-panel widget (enabled state, text,
  colours, font, justification, Enter action) in the five states: initial, input,
  disabled, run, self-watching off. Every scenario passes: 35 demos; View toggles, an
  empty splitter, show/hide all, Reset layout; the Options switches; Slipnet graphics
  blanking; the 5 codelet clamps and Undo; the commentary fonts; Help; Clear Memory
  (Cancel, close, Yes after a real run); theme edit (Cancel restores, Clamp Themes clamps,
  "No current problem!"); Save commentary; a real run with only Stop enabled.
- **Pictures, inspected**: the window with the menu bar and strip, each menu (Options,
  Demos, View, the font face menu in its own fonts), the Help window, and the Clear
  Memory and theme-edit dialogs. The driver grabs them into
  `python/tests/screenshots-qt/menus/`.
- **Two bugs found and fixed, both logged in `docs/anomalies_and_quirks.md`:**
  - two errors within 700 ms left the first on the panel. This is a port bug: the timer
    replaced the original's blocking pause. Fixed in Qt; the tkinter GUI still has it;
  - a segfault when Python's cycle collector freed a closed dialog in the engine thread.
    Fixed with `deleteLater` in `SwlDialog.closeEvent`.
- **Docs**: `docs/divergences.md` has a new section, "Python Qt GUI: the menus and dialogs";
  `docs/qt-gui-plan.md` has "As built (item 06)"; `python/tests/README.md` has rows for
  `test_qt_menus.py` and `drive_qt_menus.py`, and the new totals (1585: 1149 fast, 436
  slow). `test_qt_engine.py`'s `enabled()` now reads the Options cascade's action.
- Gate: GATE_PLACEHOLDER (filled in by iteration 10: the driver's gate passed on this work
  after fix session it09_fix2, and committed it as be3c001)

### Blockers
- None for item 06. Item 00 is still `[!]` from session 1 (its deliverables exist and
  their tests pass; see iteration 2).

### Next
- Item 07: wire mouse presses on the panes to `viewport.mouse_press`. The theme-edit
  dialog is ready for clicks on the Themes panes (`ready-to-edit?` and `edit-theme-type`
  work; the driver calls them directly).
- Item 08: save the splitter sizes and hidden panes with `QSettings`. Reset layout
  already restores the default panes and sizes. Add the About/logo item to Help.
- The panes are still blank for the first few seconds after start: the startup redraw
  goes one panel per 250 ms listener pause (item 04).

## Iteration 7 — 2026-10-04 21:04:04
### Completed
- (driver) session ended with outcome `error` without marking the item
### Blockers
- see session_it07.log
### Next
- revisit or re-open this item

---

## Owner's note — 2026-10-04 22:45 (between iterations, written for the owner)
- **Item 00 is DONE.** Session 1 finished it, but the API connection dropped (EAI_AGAIN)
  before it could mark it. The driver's gate passed on its work, which is committed in
  6ce8de2: `docs/qt-gui-plan.md`, `python/tests/data/tk-gui-inventory.json` and
  `test_tk_gui_inventory.py`.
- **Item 06 is re-opened, and only the hang remains.** Session 7's report above lists
  everything item 06 built, and that work is on disk, uncommitted. The session then died
  of an API timeout while chasing a **deadlock that only shows in the full suite**:
  `tests/test_qt_panes.py::test_fonts_measure_from_several_threads_at_once` passes alone
  and hangs after the other Qt tests. The driver's gate (with no time limit then) hung on
  it for 1 h 37 min, and the owner stopped it.
- **Evidence:** `ralph_loops/loop0003/hang-it07-faulthandler.log` (pytest with
  `-o faulthandler_timeout=60`). Two worker threads are blocked in the Qt canvas's `tcl`
  (canvas.py:67/68 → displaylist.py:450/487/500), both measuring fonts through
  `fonts.py:211 get_pixel_size` → `swl.py:109`. Find what the threads are waiting on (a
  lock held across a blocking call to the GUI thread, or state left by an earlier test),
  fix it, and make the whole gate pass. Then finish item 06's PROGRESS entry (its gate line
  says GATE_PLACEHOLDER) and mark the item.
- **Driver changes:** each gate run is now limited by `gate_timeout_min` (30); a timeout
  counts as a failure. Sessions are told to run the gate under `timeout 1800`.

---

## Iteration 9 — 2026-10-04 23:22:50
### Completed
- (driver) session ended with outcome `error` without marking the item
### Blockers
- see session_it09.log
### Next
- revisit or re-open this item

---

---

## Iteration 10 — 2026-10-05 02:20:00
Item **07 Mouse and keyboard parity**: SOLVED.

### Completed
- **Tests first, and they failed first.** I wrote `python/tests/test_qt_clicks.py`, the
  shared scenario `python/tests/click_scenario.py` and the two adapters (a `clicks`
  scenario in `drive_gui.py` and in `drive_qt_gui.py`, both run only when named) before
  the code was in place. With `metacat/qt/hosts.py` and `controls.py` reverted to HEAD,
  the file gave `10 failed, 2 passed`. The two that passed check the inventory. The Qt
  driver stopped at "the resumed run differs from its golden": the Workspace click did
  nothing.
- **The bindings, from `tk-gui-inventory.json`:** on every canvas, `<Button-1>`,
  `<Shift-Button-1>` and `<Button-3>`; press handlers on the Workspace, Temporal Trace,
  Episodic Memory and the three Themes windows. The Slipnet and Coderack have none, so
  clicks there do nothing in both GUIs. `<Key-Return>` on the command line and the input
  dialog. There are no other key bindings.
- **`metacat/qt/hosts.py`**: `PaneView.mousePressEvent` and `mouseDoubleClickEvent` call
  `QtHost.press`, which calls `viewport.mouse_press(x, y, mods)` on the GUI thread, as
  `TkHost._press` does, and prints a handler's error and goes on. `press_modifiers` applies
  Tk's binding rules: Shift-left gives `(shift left-button)`, any other left
  `(left-button)` (Control too), any right `(right-button)`, other buttons nothing. A
  double click is a second press, as in Tk. Move, release and wheel events on the canvas
  do nothing; the scrollbars still take the wheel.
- **`metacat/qt/controls.py`**: `bind_return`, an event filter for Tk's `<Key-Return>`
  (Return with any modifiers, not the keypad's Enter). It replaces `returnPressed` on the
  command line and the input dialog.
- **The scenario** (27 steps, the same code in both GUIs):
  - keypad Enter (nothing happens), Return, Shift-Return;
  - a breakpoint at 100 through the dialog's Enter;
  - right, shift, middle and shift-right clicks on the Workspace (nothing happens);
  - a left click that resumes the run, whose trace equals the golden `abc-abd-ijk_1`;
  - a second run, then Trace selections (left, again to unselect, Control-left; right and
    shift do nothing) and the Workspace click that restores the state;
  - Memory selections: one answer, then a second, which compares them (the commentary
    runs); a double click; right and shift do nothing;
  - a Workspace click outside display mode continues the run;
  - left, shift and right clicks on the five panes with no press handler;
  - theme edit mode: Workspace, Trace and Memory clicks raise the dialog. The first click
    on the Top and Vertical Themes edits their type (not the Bottom's outside justify
    mode). Then left, right and shift clicks select themes, and second clicks unselect
    them;
  - Clamp Themes, and the run for 150 codelets with the clamp.

  Each click aims at a model object through the model's own hit test, on the visible
  pixels as `mouse_press` converts them, so the different pane sizes don't matter.
- **Results:** the Qt GUI (QTest's events, sent through the window as a real mouse sends
  them) and the tkinter GUI (Tk's `event_generate`, under xvfb-run) give **identical**
  model snapshots in all 27 steps: codelet count, RNG state, modes, highlighted events and
  answers, every theme's activation, and the clamp's pattern. They also give identical
  traces, 504 + 911 + 721 + 176 lines, including the answer comparison's commentary and the
  run after the manual clamp. The tkinter result is committed as
  `python/tests/data/tk-clicks.json` (the steps, and each trace's length, SHA-256 and last
  line). The slow test regenerates it and compares. The Qt scenario takes about 26 s and
  the tkinter one about 30 s.
- **Pictures, inspected:** the scenario grabs the window after the Trace selection (event
  1's Workspace, Slipnet and Temperature redrawn as they were then), after the answer
  comparison (the answer description with the highlighted answer and the comparison
  commentary) and in theme edit mode. I compared the theme edit grab with the tkinter
  screen grab of the same step: the same two −100 themes are marked red (Top: Object Type
  iden; Vertical: Bond Facet diff), and the Bottom Themes pane is blank in both.
- **Found and logged** in `docs/anomalies_and_quirks.md`:
  - collecting fake `QtHost`s one after another crashes the cycle collector. This happens
    with HEAD's `hosts.py` too. The tests keep one host per kind;
  - `QTest.mouseDClick` on a widget sends only the double click; on the window it sends
    press, release, double click, release. That's why the panes treat a double click as a
    press and the tests send clicks through the window.
- **Docs**:
  - `docs/divergences.md` has a new section, "Python Qt GUI: the mouse and the keys": a
    margin press does nothing, and the wheel doesn't scroll the canvas;
  - `docs/qt-gui-plan.md` has "As built (item 07)";
  - `python/tests/README.md` has rows for `test_qt_clicks.py` and `click_scenario.py`, and
    new totals (1597: 1159 fast, 438 slow).
- Item 06's "Gate: GATE_PLACEHOLDER" line is annotated: the driver's gate passed on that
  work and committed it as be3c001.
- Gate: `timeout 1800 python3 ralph_loops/loop0003/gate.py`: GATE PASSED (1597 passed in
  10:25; racket/ unchanged).

### Blockers
- None for item 07.
- Item 06 is still marked `[!]` in iterations.md: the driver marked it because session 9
  died, but its work passed the gate and is committed (be3c001). The owner can mark it `[x]`.

### Next
- Item 08 (layout polish): save the splitter sizes and hidden panes with `QSettings`, and
  add the About/logo item. The panes are still blank for the first seconds after start
  (one panel per 250 ms listener pause).


---

## Iteration 11 — 2026-10-05 02:57:00
Item **08 Layout polish**: SOLVED.

### Completed
- **Tests first, and they failed first.** I wrote `python/tests/test_qt_layout.py` and its
  driver `python/tests/drive_qt_layout.py` before any code. First run of the fast tier:
  `5 failed, 9 errors` (no `app.make_application` or `open_window`, no `metacat.qt.icon`,
  no `SETTINGS_VERSION`). Then three of my own expectations, or the driver, were wrong, and
  I corrected them rather than the code:
  - the driver's handle drag moved 300 px for 120, because each move's global position was
    computed from the handle after it had moved;
  - a hidden pane takes its splitter handle with it, so the middle row's visible sizes
    gain 4 px;
  - Reset layout keeps `layout/version`.

  Three more tests came from what the screenshots and measurements showed, and each failed
  before its change:
  - the strip clipped at the window's minimum: the strip needs 1684 px, the tool bar
    doesn't pass that on, and 1600 was too narrow;
  - the high-DPI test reported a pixel ratio of 1, because the offscreen screen's `dpr` key
    doesn't reach its windows (logged);
  - with the EEG shown, the Trace was 40 px high. I wrote
    `test_the_eeg_doubles_the_bottom_row` together with its change, and the run test's
    "Trace and EEG ≥ 80 px" check failed before that change.
- **`metacat/qt/mainwindow.py`**:
  - `MainWindow(settings)` saves the layout in a QSettings: `layout/version` (1),
    `window/geometry`, `view/hidden`, `layout/custom`, and each splitter's `saveState()`
    once a handle has been dragged;
  - it saves 500 ms after the last drag or View change (a single-shot timer), and at
    close. `restore_geometry()` runs before the window is shown, and `restore_layout()`
    after, through the View menu's window controllers, so the checkmarks follow. An older
    version is ignored;
  - Reset layout removes the saved layout. A layout that follows the window is saved as
    not custom, so the next start lays it out for its own size;
  - the minimum size is 1600×800, widened to the control strip's minimum (1692 px with the
    1080p fonts);
  - with the EEG shown in the default layout, `default_sizes(..., eeg=True)` doubles the
    bottom row (18 %), and the other rows keep 60:31.
- **`metacat/qt/app.py`**:
  - `make_application()`: high DPI with the `PassThrough` rounding policy, plus the app
    and organisation names;
  - `open_settings()`: `--settings INI`, `$METACAT_QT_SETTINGS`, or
    `QSettings("fargonauts", "metacat-qt")`;
  - `open_window(settings)`: the saved geometry, or maximised on a screen larger than
    1920×1010, then the saved panes and sizes;
  - `setup()` sets the window icon.

  `conftest.py` points `$METACAT_QT_SETTINGS` at a throw-away file, so the tests never
  touch the owner's settings.
- **`metacat/qt/icon.py`**: the original's Logo (fonts.ss's `create-mcat-logo`: light sky
  blue, "Metacat" in `%logo-font%` at 55,50 anchor s). It is drawn by that Tk command on a
  Qt canvas and rendered as vector text into square icons from 16 to 256 px, for the window
  and the application. There is no About item: Help keeps gui.ss's one item.
- **Results** (`drive_qt_layout.py`, fresh offscreen processes):
  - a drag resizes the panes exactly and is saved after the delay;
  - save and restore round-trip the geometry, the hidden panes (Slipnet hidden, EEG shown),
    the View checkmarks and every splitter's sizes;
  - Reset layout gives exactly `default_sizes`, the default panes and checkmarks, and the
    sizes then follow a resized window;
  - the window fills a 1920×1080 screen (maximised);
  - shrunk to 1000×500, the window stops at its minimum, with the strip whole and every
    pane at or above its own minimum;
  - at 200 % (a 3840×2160 screen with `QT_SCALE_FACTOR=2`), the window is 1920×1080
    logical and the grab is 3840×2160. Every pane's grab is twice its size;
  - after `abc abd ijk abd 1` (a justify run, so the Bottom Themes draw) with every pane
    shown, at 1920×1080 and 2560×1440, every pane is visible, has items and pixels, and is
    at least its minimum. The run ends at its golden's codelet count and random state
    (1835, 4230205117) and takes 5.5–5.9 s at the slider's fast end.
- **Pictures, inspected:**
  - the whole window after the run at 1920×1080 and at 2560×1440. At 1440p every pane is
    larger; the Trace's labels and the EEG are readable;
  - the start screen at 200 %, with a full-resolution crop of the Coderack: its 8-px
    labels are sharp and keep every `i` and `l`;
  - the window at its minimum size, after the panels redrew at their pane sizes;
  - the 256-px icon.
- **Docs**:
  - `docs/qt-gui-plan.md`: the 1366×768 layout is removed. Its place has a table of the pane
    sizes at 1920×1080 and 2560×1440, and the 4K note. The minimum size and the EEG rule
    are in 2.2, and "As built (item 08)" is in 2.3. The About item is gone from 2.3 and 2.7;
  - `docs/divergences.md`: a new section, "Python Qt GUI: the layout, its saving, and the
    icon";
  - `docs/anomalies_and_quirks.md`: the offscreen `dpr` key, and the hidden splitter
    handle and the tool bar that hides what doesn't fit;
  - `python/tests/README.md`: rows for `test_qt_layout.py` and `drive_qt_layout.py`, and
    new totals (1614: 1174 fast, 440 slow).
- **Other test files**: `test_qt_skeleton.py`'s grab test resized the window to 400×300,
  below the new minimum, so it now uses `MINIMUM_SIZE` + 100. `render_qt_panes.py`'s third
  resize size went from 1300 to 1650 wide.
- Gate: `timeout 1800 python3 ralph_loops/loop0003/gate.py`: GATE PASSED (1614 passed in
  10:13; racket/ unchanged). The first gate run failed on the skeleton grab test above;
  the second passed.

### Blockers
- None for item 08. Item 06 is still marked `[!]` in iterations.md, but its work passed
  the gate and is committed (be3c001); the owner can mark it `[x]`.

### Next
- Item 09 (packaging and docs): a `metacat-qt` console script; `python/metacat/qt/README.md`
  with screenshots. `drive_qt_layout.py OUT run --screen 1920x1080` (and `2560x1440`) makes
  good ones, in `OUT/window-run-*.png`. Mention `--settings` and `$METACAT_QT_SETTINGS`.
- The panes are still blank for the first seconds after start (one panel per 250 ms
  listener pause).

## Owner's note — 2026-10-05 03:20 (between iterations, loop paused)
- **Item 06 is DONE.** Its work (session 7's report above) was committed in be3c001,
  after a gate that passed by luck at 01:32. The deadlock was intermittent and still in
  the code.
- **Deadlock fixed (root cause found by a separate debugging agent in a git worktree).**
  Python's automatic cyclic garbage collector ran on a test's worker thread and freed a
  Qt top-level widget there. Destroying it waits for the GUI thread, which was waiting for
  the worker. The fix is `metacat.qt.hosts.collect_on_gui_thread()`: automatic GC off,
  collections from a `QTimer` on the GUI thread. It comes with a regression test
  (`test_the_garbage_collector_runs_on_the_gui_thread_only`). Details are in
  `docs/anomalies_and_quirks.md`. **Rule for later items:** a Python-owned Qt object must
  never be freed off the GUI thread. Keep `collect_on_gui_thread()` installed, and
  never call `gc.collect()` from a worker thread.
- **Housekeeping:** commit be3c001 had picked up the debugging agent's worktree as an
  empty gitlink (`.claude/worktrees/…`). It is removed, and `.claude/worktrees/` is now
  in `.gitignore`.

---

## Iteration 12 — 2026-10-05 03:36:45
Item **09 Packaging and docs**: SOLVED.

### Completed
- **Tests first, and they failed first.** In `python/tests/test_install.py` I first
  changed `test_commands_declared` (now expects `metacat-qt = metacat.qt.app:main`),
  added `test_qt_extra_declared` and `metacat.qt.app.main` to the no-argument entry
  point test, and wrote two slow tests. Before the code: the fast tier gave
  `1 failed, 5 passed`, and the two new slow tests both failed (no `metacat-qt` in the
  venv's `bin/`).
  - `test_editable_install_with_qt_extra`: a fresh venv (system site-packages visible,
    so PySide6 resolves offline), `pip install --no-build-isolation --no-index -e
    'COPY[qt]'` *without* `--no-deps`, so the extra really resolves. Then
    `venv/bin/metacat-qt --quit-after 2500 --settings INI --screenshot PNG` from another
    directory with `QT_QPA_PLATFORM=offscreen`: exit 0, prints `Metacat` and its panes,
    writes a PNG, and saves the layout at close;
  - `test_install_without_qt_extra`: a wheel built by this Python's setuptools,
    installed into a venv **without** system site-packages (`import PySide6` fails
    there). Run 7 through `metacat` gives the live oracle's stdout and the golden trace,
    `metacat-gui` opens under xvfb-run, and `metacat-qt` exits 1 with `The Qt GUI needs
    PySide6: pip install -e 'python[qt]'`.

  Both pass in about 20 s together.
- **`python/pyproject.toml`**: `metacat-qt = "metacat.qt.app:main"` in
  `[project.scripts]` (the `qt` extra and the `metacat.qt` package were already there
  from item 01).
- **Screenshots**: `drive_qt_layout.py` has a new `run7` scenario (Run 7 in the default
  layout; `run` and `run7` now share `run_to_answer`). At 1920×1080 and 2560×1440 it
  ends at the golden's codelet count and random state (2170, 4089168737), in 6.2–6.4 s.
  I inspected both grabs. They show the same picture as the Racket and tkinter Run 7
  screenshots: the crossed bridges, both rules, three snags and `wyz`, Temperature 15, and
  the Coderack labels with their `i`s and `l`s. They are committed as
  `docs/screenshots/qt-run7-wyz.png` and `qt-run7-wyz-1440p.png`. At 1440p the Trace is
  scrolled to its end, so its first icon is cut at the left edge; that is the Trace's own
  scrolling.
- **Docs**:
  - new `python/metacat/qt/README.md`: running and installing, the options
    (`--settings`, `$METACAT_QT_SETTINGS`, `--quit-after`, `--screenshot`), the
    layout, a table of the modules, how the canvas, threads, speed and fonts work, the
    tests, and both screenshots. I checked the threading paragraph against `canvas.py`
    (canvas commands wait for `PAINT_GATE`, never for a call into the GUI thread);
  - `python/README.md`: the Qt GUI in the intro, in the install commands and in a new
    section "The one-window GUI (Qt)" with the screenshot. The layout table has a
    `metacat/qt/` row; the pyproject row lists the third command and both extras. The
    verification table has two Qt rows. `--qt` is noted, and the test counts are updated
    (35 files, 1618 tests). The Xvfb label-flaw caption now gives item 03's explanation;
  - top-level `README.md`: the intro, a quick-start row and the install cell, and a
    paragraph "Everything in one window" with the commands and the screenshot;
  - `docs/screenshots/README.md`: how the Qt images were made (a table row), a section
    "Run 7 in one window" for both images, the intro (Qt offscreen as well as Xvfb), and the
    Known rendering issue note updated with item 03's finding;
  - `docs/divergences.md`: "Python Qt GUI: packaging and a third command";
  - `python/tests/README.md`: the `test_install.py` row (6 + 6), the `run7` scenario,
    and the totals: **1618 tests, 1176 fast and 442 slow** (HEAD had 1615, with the owner's
    GC test).
- Gate: `timeout 1800 python3 ralph_loops/loop0003/gate.py`: **GATE PASSED** (1618
  passed in 10:25; racket/ unchanged). After it, only Markdown changed (test counts and
  wording); `run-tests.sh --fast` was green again (1176 passed).

### Blockers
- None.

### Next
- Item 10 (optional, Racket single window) or item 11 (final audit).
- For the audit: the top-level README's "Using the GUI" says "Enter starts the run; Go
  stays greyed out until a run is under way". In the code (gui.ss's
  `go-button-action` → `init-new-problem`), Enter sets up the problem and parks it in
  input mode with Go enabled, and Go runs it, as `drive_qt_layout.py` does. This wording
  predates loop0003, and I left it alone.

---

## Iteration 13 — 2026-10-05 04:33:00
Item **10 (Optional) A single window for the Racket port too**: SOLVED.

### Completed
- **Tests and order, honestly:** I wrote a first draft of the code (the pane mode and
  `one-window.rkt`) before `racket/gui-tests/one-window-test.rkt`, so the test did not
  come strictly first. Without the new code, the test fails: with gui.rkt, gui.rktl and
  headless.rkt reverted to HEAD and one-window.rkt removed, it stops at once
  (`only-in: identifier 'trace-gui-runs!' not included`). Its first run against the draft
  gave **10 failures in 917 checks**, which found four bugs. Each failed before its fix:
  - a custom `place-children` gets one entry per child, hidden ones included (an
    exception at setup);
  - showing or hiding a pane didn't lay the panel out again: "the Coderack moved left"
    and "EEG has room: (4 98 46 8)" failed. The fix is `container-flow-modified`;
  - `reparent` showed the hidden self-watching warning. I saw it in the screenshot and
    wrote the check before the fix;
  - the Temperature's letterbox margin was unpainted (white). Also seen in the
    screenshot.
- **`racket/gui/gui.rkt`** (the multi-window GUI behaves as before):
  - `screen-host%` has an optional `pane-parent`, set through `make-pane-host-maker`. The
    canvas goes into that panel instead of a frame, with no min sizes. Show and hide act
    on the canvas and re-lay the panel out. `set-geometry!` and `raise` do nothing;
  - a window with `none` scrolling is letterboxed at its (w+2):(h+2) ratio, at the top
    centre of its pane. `paint` fills the margins in the viewport's background, and
    presses subtract the offset;
  - a pane's resize waits while the original listener's queue holds a resize, because
    the listener keeps only the last of simultaneous resizes;
  - `settled?` and `get-offset` are for the tests. `make-engine-thread` is exported, and
    `set-control-panel-frame-maker!` is a hook that `make-control-panel` (gui.rktl, a
    5-line change) asks for its frame.
- **`racket/gui/one-window.rkt`**: `setup-one-window` is setup.rktl's `setup` with
  the windows as panes and the control panel built in the same frame, without
  `arrange-windows!`. Its widgets are then moved into a strip: problem and command line;
  slider and Step/Go/Stop/Reset; breakpoint and warning. `layout-panel%` places the panes
  by `pane-rects`, which is the Qt layout of `docs/qt-gui-plan.md` 2.1–2.2:
  - rows of 60/31/9 % of the height, 18 % when the EEG is shown;
  - fixed-aspect widths from the row height, the Temperature at max(60, 6 %), and the
    Commentary and Memory taking the rest (at least 200 px);
  - the Themes stacked in one column, and empty rows closed up.

  There are no splitters (racket/gui has none) and no saved layout. The Windows menu
  hides and shows panes.
- **`racket/one-window.rkt`**: the entry point, `racket racket/one-window.rkt [SCALE]`.
  It uses `lazy-require`, as main.rkt does, and is added to `no-gui-test.rkt`'s headless
  modules.
- **`racket/headless.rkt`**: `trace-gui-runs!` installs the trace's wrappers and
  recorders around the GUI's own windows and writes to a port. It adds no headless
  windows and no `break`.
- **Results** (`one-window-test.rkt`, 1103 checks, about 10 s on Xvfb):
  - Run 7 is typed into the strip and run with Go at Fast, and the window is resized
    twice during the run. Its trace equals the golden `abc-abd-xyz_3852097033.jsonl`
    **line for line** (all 2,667 events between the start and end lines), and the run ends
    at the golden's (2170, rng). The run took 2.0 s;
  - `pane-rects` checked at 3 sizes × 5 pane sets: every pane inside the area, none
    overlapping;
  - the built window has one shown frame, every window is a pane of it, and the EEG
    starts hidden. Each pane takes its own size through the original's protocol, and
    each panel redraws at that size;
  - hiding and showing the Workspace and showing the EEG lay the panes out again;
  - self-watching off hides the three Themes panes and shows the warning; on undoes both;
  - the screen grab (python3 + PIL, skipped if missing) has, for each pane, its
    background and its drawings where the pane is. The Bottom Themes draws only in
    justify runs, so it is checked for its background only.
- **Screenshot, inspected:** Run 7 at `wyz` in one 1920×1040 frame. The picture matches
  the multi-window screenshots: crossed bridges, both rules, the Coderack, Temperature 15,
  the Slipnet, the Themes, the Memory's two answers, the Commentary and the Trace's icons.
  Committed as `docs/screenshots/racket-one-window-run7.png`, with a section and a table
  row in `docs/screenshots/README.md`.
- **Docs:**
  - `docs/divergences.md`: "Racket: one window";
  - `docs/anomalies_and_quirks.md`: the racket/gui entry (place-children, re-placing,
    reparent);
  - `docs/porting-notes.md`: "One window (loop0003 item 10)";
  - `racket/README.md` (command and screenshot), `racket/gui/README.md` (rows),
    `racket/gui-tests/README.md` (row and section);
  - the top-level README's quick-start row, and CLAUDE.md's command list.
- `racket/gui-tests/*.rkt` all pass on Xvfb: 1317 checks with control-panel-test and
  dist-test, in 72 s.
- Gate: `timeout 1800 python3 ralph_loops/loop0003/gate.py`: **GATE PASSED**. The Python
  suite was green, and so was the Racket suite (racket/ changed: `raco test racket/`, the
  gui-tests under xvfb-run, and the Chez checks).

### Blockers
- None.

### Next
- Item 11 (final audit). Remember that `racket/` changed in this item, so the audit must
  run `tests/run-tests.sh` too. The audit note from iteration 12 still stands: the
  README's "Enter starts the run" wording.
- The Racket one-window GUI could later get draggable pane borders (a hand-made splitter
  on `layout-panel%`) and a saved layout, as the Qt GUI has.

## Iteration 13, fix 2: the gate "hang" was the time limit, not a deadlock
- Re-ran `python3 ralph_loops/loop0003/gate.py` with timestamps: **GATE PASSED in 17 min 52 s**.
  The Python suite took 10 min 38 s (1618 passed). Because racket/ changed, the gate also
  ran tests/run-tests.sh: raco make 2 s, `raco test racket/` 5 min 53 s, gui-tests 31 s,
  Chez checks 47 s. No test hangs. The failing runs were killed at 15 min while
  `racket/tests/rule-diff-test.rkt` was running, which this run reached at 13 min.
- Cause: knobs.json had `gate_timeout_min` at 15, below the gate's real length whenever
  racket/ changes. Fix: put it back to 30, the driver's default. No test was changed.

---

## Iteration 15 — 2026-10-05 06:10
Item **11 Final audit**: SOLVED.

### Completed
- **Everything re-run in the foreground of the gate:** the Python suite (**1640 passed** in
  10:21, Qt tests included) and, because `racket/` differs from the freeze point (item 10),
  the Racket suite: `raco test racket/` (2640 tests), the gui-tests under xvfb-run (1317
  checks) and the Chez oracle checks. The Qt tier alone (`run-tests.sh --qt`) gave 209
  passed in 2:58.
- **The Qt GUI against the item 00 inventory, entry by entry.** The menus, the five
  control-panel states, the Help, Clear Memory and theme-edit dialogs, Save commentary and
  the bindings were already compared by `test_qt_menus.py` and `test_qt_clicks.py`. The
  rest had no entry-by-entry check, so I wrote `python/tests/test_qt_audit.py` (22 tests,
  fast tier, about 6 s) and its driver `python/tests/drive_qt_audit.py` (it imports
  `drive_qt_menus.py`'s window and helpers). It checks:
  - every graphics window is a pane in a splitter with a View item, and has the
    inventory's title, global, panel class, drawing module, scrolling, scroll bars,
    resize method, visibility at start, left/right/shift press handlers and background;
    the unscrollable ones keep Tk's aspect ratio;
  - the Logo is the window icon (16–256 px);
  - the speed slider's range, initial value, labels, and the five speed settings at each
    of the inventory's seven recorded values;
  - both Input dialogs: their offset from the control panel ([20, 80] and [80, 80]), a
    second trigger raises the open dialog, "x", "0" and "-3" give the red "Invalid
    input!" and it goes back after 700 ms, an empty field closes without a change, "123"
    sets the value and closes (and the breakpoint label says so);
  - Step, Go, Stop and Reset run gui.py's four actions, Enter is Go in input mode, and
    closing the window quits (the control panel's close was `gui._exit`);
  - `COVERAGE`: every top-level inventory key and every dialog maps to the tests that
    check it, and each named test exists. A new inventory key fails this test.

  **Honest order:** the audit compares the existing GUI with the inventory, so most of
  these tests passed at their first run. The one code change, `b.action = action` on the
  strip's buttons in `controls.py` (so a test can read which action a button runs), was
  made for the buttons test, and with `controls.py` reverted that file gives `3 failed, 19
  passed` (the driver stops at the buttons step).
  **Result:** every compared entry is equal. Two differences remain, both visual, now in
  `docs/divergences.md` ("Python Qt GUI: found by the final audit"): the buttons lack Tk's
  green/red active colours, and panes tell their panels the pane's size (already
  recorded), including the hidden EEG once at start.
- **No engine module imports PySide6 or tkinter.** A static grep of `python/metacat/*.py`
  finds no such import (the existing `test_no_engine_module_imports_qt` and
  `test_engine_never_imports_gui` check this). The new runtime test
  `test_a_run_imports_neither_pyside6_nor_tkinter` imports all 42 engine modules, runs
  `abc abd xyz` seed 7 for 200 codelets through `metacat.__main__.main`, and finds no
  PySide6, shiboken6, tkinter, `metacat.gui` or `metacat.qt` in `sys.modules`.
- **`docs/qt-gui-plan.md` re-read against the code.** A read-only agent listed 24
  discrepancies; I checked the important ones in the code and corrected the doc:
  - the 1080p wireframe: the strip as built (info label, command line, slider over its
    labels, the buttons, the stacked messages), and aspect ratios in place of the old
    estimated sizes (the measured table stays);
  - the QSettings keys (`layout/custom`, splitter states only when custom, `view/hidden`
    space-separated, saved after a drag *or* a View change), and what Reset removes (it
    keeps `layout/version` and `window/geometry`);
  - 2.5: no journal (dirty ids synced every 50 ms under `PAINT_GATE`), no blocking
    measurement fallback, the deadlock rules as built (the paint gate, `os._exit` at quit),
    `parked_hook` for the final picture, `horizontalAdvance`, the 4-thread test;
  - 2.6: one `TkItem` class with recorded painter calls; butt caps and round joins;
    antialiasing on text only, with no option; `install()`/`qt_host_maker()`; panes placed
    by name; `HiddenCanvas`;
  - 2.7: the strip order, plain buttons, the menu order Help, Demos, View, Options, Memory,
    "Metacat help" in a `QTextEdit`, "Clear Memory";
  - 2.8: the module plan rewritten as built (app, displaylist, fontspec, icon, grab; the
    drivers);
  - "There is no run yet" and "Item 05 must solve this" marked as resolved;
    `get-relative-position` noted as replaced; the strip font rule as followed.
  The stale `MINIMUM_SIZE` comment in `mainwindow.py` ("the control strip is 1600 wide")
  now gives the strip's real 1684 px.
- **Startup, inspected:** grabs of `python3 -m metacat.qt` at 0.7 s and 2.5 s. The panels
  aren't blank (an earlier note said so): they draw at once at their original sizes inside
  their panes, and reach their pane sizes within about 2.5 s. Recorded in the plan and in
  follow-ups.
- **`docs/anomalies_and_quirks.md`:** the GIL entry's Status now says fixed (item 05); the
  conftest-GC entry points to the later program fix (`collect_on_gui_thread`); a new entry,
  "A hidden pane still gets a configure (Qt GUI)", from the audit (a probe showed one
  delivery of 640×26 to the hidden EEG); status not a bug.
- **`docs/follow-ups.md`:** a new section, "Python Qt GUI (loop0003)": safety net (the Qt
  tier, the GC rule, the tkinter references), clean-up (the duplicated control panel,
  the driver helpers, `HiddenCanvas`), performance and polish (run 7's times, the startup
  jump, the hidden pane, the wheel, real screens untested, the tkinter 700 ms bug), and the
  Racket one window (no splitters, no saved layout).
- **Also fixed:** the top-level README's "Enter starts the run; Go stays greyed out" (the
  note from iteration 12). Enter sets the problem up and waits, and Go starts the run
  (gui.py's `go_button_action` → `init-new-problem` → `quiet_break`).
- **Test docs:** rows for `test_qt_audit.py` and `drive_qt_audit.py` in
  `python/tests/README.md` and `python/metacat/qt/README.md`; totals 1640 (1198 fast, 442
  slow; checked with `--collect-only`) and 36 test files in `python/README.md`.
- Gate: `timeout 1800 python3 ralph_loops/loop0003/gate.py`: **GATE PASSED** (Python 1640
  passed in 10:21; the Racket suite ran because racket/ changed and passed). The 10-minute
  tool limit moved the gate command into the background; I waited for it in the
  foreground with a polling loop and read its log.

### Blockers
- None.

### Next
- Every item of loop0003 is done. What a later loop could take up is in
  `docs/follow-ups.md`, "Python Qt GUI (loop0003)": the button active colours, a first
  layout without the startup jump, skipping configures for hidden panes, and opening the
  window on a real screen (never done by the loop, by the owner's rule).

LOOP_COMPLETE
