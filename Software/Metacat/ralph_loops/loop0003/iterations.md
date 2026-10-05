# loop0003 — one window for all of Metacat (PySide6): items

- [x] **00 Inventory and design.** Write `docs/qt-gui-plan.md`.
    (1) **Inventory every element of today's tkinter GUI:**
    - each graphics window: its title, default size, whether it can be resized, which
      file draws it, and its mouse bindings (left, shift-left and right click);
    - every control-panel widget and its states: enabled or disabled in each run state,
      the command line's Enter behaviour, Go, Step, Stop, Reset, and the speed slider;
    - every menu and item: Demos, Windows, Options, Memory, Help and Save commentary,
      plus the dialogs they open;
    - every keyboard binding.

    Generate the inventory with a script that walks the live tkinter GUI under
    `xvfb-run`, and commit it as `python/tests/data/tk-gui-inventory.json`.
    (2) **The single-window design:**
    - an ASCII wireframe of the default layout at 1920×1080 and at 1366×768;
    - the splitter tree: nested horizontal and vertical `QSplitter`s, with which panes go
      where and their default proportions (fixed splitters, not docks: the owner's
      decision);
    - how panes are hidden and shown (the View menu), how splitter sizes are saved and
      restored (`QSettings`), and "reset layout";
    - the resize policy per panel;
    - the threading bridge;
    - the canvas backend.

    Tests: the inventory script runs and its JSON lists every window and menu item that
    `gui.py` defines. This is a check of the inventory itself.

- [x] **01 Skeleton and test harness.** Create `python/metacat/qt/`:
    - `python3 -m metacat.qt` opens an empty `QMainWindow` titled Metacat;
    - add a `qt` extra in `pyproject.toml`;
    - make `python/tests/test_qt_*.py` skip when PySide6 is missing;
    - add a pytest fixture for a headless `QApplication`;
    - add a helper that grabs a widget to PNG;
    - wire `run-tests.sh` in the right tier.

    Also inventory exactly which Tk canvas commands and options the panels send. Use the
    code and every stream in `python/fixtures/sgl-tcl/`, and commit the list as test
    data.
    Tests: the window opens and closes headlessly; the screenshot helper writes a PNG;
    the command inventory is complete. A test fails if any panel module sends a command
    that is missing from the list.

- [x] **02 The Qt canvas.** `qt/canvas.py`: an object with the panel canvas interface
    (`tcl`, background getters and setters) that executes the inventoried Tk canvas
    commands on a `QGraphicsScene`:
    - item kinds and their options (fill, outline, width, dash, arrow, smooth, anchor,
      justify, font, text, state);
    - tags and tag searches;
    - `move`, `delete`, `raise`/`lower`, `itemconfigure` and `scale`;
    - `bbox` with Tk's semantics.

    Tests, written first:
    - replay every `python/fixtures/sgl-tcl` stream into the Qt canvas and into a real
      tkinter Canvas (under `xvfb-run`), then compare normalised display lists: item
      order, kinds, coordinates, options, tags, hidden state, and `bbox` answers;
    - render the SGL fixture with Qt, inspect it, and compare it with
      `docs/screenshots/panels/sgl-fixture-python.png`.

- [x] **03 Fonts and colours.** Map Tk font specs (family lists, negative pixel sizes,
    points, weight, slant) and Tk colour names onto Qt. Text measurement for the panels
    must use the same fonts the Qt canvas draws with. Check the tiny Coderack label case:
    do the `i`s and `l`s render in Qt? Update the anomalies entry with the answer.
    Tests:
    - font mapping table tests;
    - measurement consistency (a text item's `bbox` equals the measured width);
    - colour names against `colors.py`.

- [x] **04 Panels in panes.** `qt/hosts.py`: a host maker that puts each graphics window in a
    pane of the main window (`QGraphicsView` over the Qt canvas), and the main window that
    holds all the panes in the default layout from item 00. Apply the resize policy per
    panel. Tests:
    - offscreen, run run7 (`abc abd xyz` 3852097033) with the Qt hosts, driving the
      engine directly. The trace must equal the golden, and every pane must have items;
    - grab the window at codelets 300 and 800 and at the answer. Inspect the images and
      compare each pane with the tkinter panel snapshots in
      `python/tests/snapshots/views/`.

- [x] **05 Engine thread and run control.** `qt/engine_bridge.py`:
    - the engine in a worker thread;
    - canvas commands batched onto the GUI thread;
    - blocking queries (`bbox`, measurement) without deadlock;
    - Go, Step, Stop, Reset, step interval, breakpoints and the speed slider, with the
      same semantics as `gui.py`.

    Tests:
    - offscreen GUI runs of every scenario `python/tests/drive_gui.py` covers (full run,
      step, breakpoint, stop/restart, reset, a demo, a justify run) give their golden
      traces;
    - no deadlock across 50 rapid Go/Stop toggles;
    - record run 7's wall-clock time in the Qt GUI and in the tkinter GUI.

- [x] **06 Control strip and menus.** Translate the control panel and every menu from the
    item 00 inventory into the main window:
    - a control strip with the command line, Go, Step, Stop, Reset and speed, showing
      the problem and seed;
    - the menu bar: Demos with all of `demos.ss`'s runs; View, replacing Windows, to
      show or hide or reset panes; Options; Memory; Help with the original's help text;
      Save commentary;
    - the original's dialogs.

    Tests: every inventory entry exists in the Qt GUI and does what the tkinter one does,
    driven offscreen. Enabled and disabled states match the tkinter GUI's in each run
    state.

- [x] **07 Mouse and keyboard parity.** Every binding from the inventory:
    - a click on the Workspace continues a run;
    - clamp clicks on the Slipnet, Coderack, Themes, Trace and Memory panes;
    - right clicks;
    - shift clicks;
    - keyboard shortcuts (Enter and others).

    Tests: drive each one offscreen with `QTest`. The resulting engine state and trace
    equal those of the same action in the tkinter GUI under `xvfb-run`, and the clamp
    scenario of `drive_gui.py` gives its expected trace.

- [x] **08 Layout polish.**
    - the default layout for 1920×1080 from item 00 (1080p is the minimum screen, the
      owner's decision; remove the 1366×768 layout from `docs/qt-gui-plan.md`), and how it
      grows on 2560×1440 and 4K;
    - splitter handles that resize panes, with sensible minimum pane sizes;
    - hiding and showing panes from the View menu, with the splitters closing up the gap;
    - layout saved and restored with `QSettings`, plus a View → Reset layout item;
    - high-DPI scaling;
    - the window's minimum size;
    - an app icon, built from the original's logo if `fonts.ss`'s `create-mcat-logo` can
      be drawn.

    Tests: splitter-size save and restore round-trips; reset layout restores the default; every pane is visible and non-empty at
    1920×1080 and 2560×1440 after a run. Inspect screenshots at both sizes.

- [x] **09 Packaging and docs.**
    - `pip install -e 'python[qt]'` installs a `metacat-qt` command;
    - write `python/metacat/qt/README.md` with screenshots;
    - add the Qt GUI to `python/README.md`, to the top-level `README.md` (quick start and
      a screenshot), to `docs/screenshots/README.md` and to `docs/divergences.md`.

    Tests: install into a fresh venv with the qt extra and open and close the window
    headlessly; without the extra, the engine and the tkinter GUI still install and run.

- [x] **10 (Optional) A single window for the Racket port too.** In `racket/gui/`, add a
    single-frame layout that hosts the existing views as panes in one `frame%`. Use new
    files and a new entry point, leaving the current multi-window GUI as it is. Tests,
    following the patterns in `racket/gui-tests/`:
    - a GUI run's trace equals its golden;
    - a screenshot, inspected;
    - the full Racket suite stays green (the gate runs it when `racket/` changes).

    This item is a plus, not a requirement. If it doesn't fit the 3-hour cap, do a
    coherent part, mark it blocked and say exactly what remains.

- [x] **11 Final audit.** Re-run everything in the foreground: the Python suite and the Qt
    tests, plus the Racket suite if `racket/` changed. Then:
    - check the Qt GUI against the item 00 inventory, entry by entry;
    - confirm that no engine module imports PySide6 or tkinter;
    - re-read `docs/qt-gui-plan.md` against the code and correct it;
    - update `docs/anomalies_and_quirks.md` and `docs/follow-ups.md`;
    - finish the PROGRESS entry yourself.
