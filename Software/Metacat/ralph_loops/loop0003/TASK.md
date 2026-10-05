# TASK: One window for all of Metacat — a PySide6 (Qt) GUI for the Python port

## Philosophy
- **One window, everything in it.** Today's GUIs, the original's and both ports', scatter
  about ten top-level windows across the screen: the control panel, Workspace, Slipnet,
  Coderack, Temperature, the three Themes windows, Temporal Trace, Episodic Memory,
  Commentary and the EEG. The goal is a single main window that shows all of them at once
  as panes, with every control and menu of today's GUI. **1920×1080 is the minimum screen**
  (the owner's decision): the default layout is designed for 1080p, and larger screens
  (2560×1440, 4K) use the extra space. Smaller screens aren't a target, so drop the
  1366×768 layout from `docs/qt-gui-plan.md`.
- **Same pictures, new frame.** The panels' drawing code already exists and is faithful:
  `python/metacat/gui/*_graphics.py`, through `sgl.py`. Every panel draws by sending the
  original's Tk canvas commands to a canvas object (`tcl(*args)`; see `swl.py`, `hosts.py`).
  The Qt GUI reuses all of it unchanged. What's new is a **Qt canvas that executes those Tk
  commands on a `QGraphicsScene`**, a Qt host that puts each panel in the main window, and
  a Qt control panel and menu bar. Don't redraw the panels by hand.
- **Watching never changes a run.** The engine (`python/metacat/*.py`) is frozen; the gate
  checks it. A run in the Qt GUI must produce exactly its golden trace, as the tkinter GUI's
  runs do.
- **The tkinter GUI stays.** `python3 -m metacat.gui` keeps working and its tests stay
  green. The Qt GUI is a new package, `python/metacat/qt/`, started with
  `python3 -m metacat.qt`. PySide6 is an optional dependency (`pip install -e
  'python[qt]'`). The engine and the tkinter GUI still need nothing beyond the standard
  library, and the Qt tests skip cleanly when PySide6 is missing.
- **Tests first, from the existing references.** The Tk command streams in
  `python/fixtures/sgl-tcl/` and the tkinter GUI itself are the oracle for the Qt canvas.
  The same commands must give the same display list: items, coordinates, options, tags and
  stacking order. Write each test before the code. Record in PROGRESS.md that it failed
  first.
- **Look at what you draw.** Render the main window with `QWidget.grab()` and inspect every
  screenshot with the Read tool. Compare panel by panel with `docs/screenshots/` and fix
  what looks wrong. Check the small-text case in particular: the tkinter screenshots lose
  the `i`s and `l`s of the Coderack labels (`docs/anomalies_and_quirks.md`, UFO section).
- **Log the strange** in `docs/anomalies_and_quirks.md`, in its entry format, and record any
  deliberate difference from the original's GUI in `docs/divergences.md`, in a Python Qt
  section.
- **Small, verifiable steps.** If something can't be verified, it isn't done.
- **Fixed splitters, not docks (the owner's decision).** The panes sit in a fixed
  arrangement of nested `QSplitter`s: no floating, tabbing or dragging panes around. The
  existing tkinter GUI already offers separate windows, so the Qt GUI is the
  everything-in-one-place dashboard. The user can resize panes by dragging the splitter
  handles and hide or show panes from the View menu. The splitter sizes are saved, and
  View → Reset layout restores the default.

## Current Focus
`python3 -m metacat.qt` opens one window. A control strip and menu bar are at the top, and
every panel is visible below. You type `abc abd xyz 3852097033` and press Enter, and the
Workspace, Slipnet, Coderack, Temperature, Themes, Trace, Memory, Commentary and EEG all
animate in the same window, exactly as the separate windows do today. Panels can be
resized with splitters, hidden and restored, and the splitter sizes persist.

## Target Problems (in order)
See `iterations.md`. Work on the first item not marked `[x]` (done) or `[!]` (blocked).

## Acceptance Criteria (per item)
- [ ] The item's own criteria in `iterations.md` are met
- [ ] Tests were written before the code and failed without it
- [ ] `python3 ralph_loops/loop0003/gate.py` passes: the original, the references, the
      fixtures and the Python engine are unchanged, and `python/run-tests.sh` is green
      (plus `tests/run-tests.sh` if `racket/` changed)
- [ ] No regressions in the tkinter GUI or the engine

## Completion Conditions
An item is DONE when either:
1. **Solved**: Meets all criteria, OR
2. **Blocked**: Documented in PROGRESS.md with specific blockers (what was tried, exact
   error output, what is needed to proceed)

The loop is complete when every item in `iterations.md` is DONE or BLOCKED. Only then add
`LOOP_COMPLETE` to PROGRESS.md.

## Context
- **Read first:**
  - `python/README.md` and `python/metacat/gui/README.md`: the module map, hosts, the SGL
    interpreter, `ThreadSafeTk`, and the engine thread;
  - `python/tests/README.md`;
  - `docs/screenshots/README.md`;
  - `python/metacat/gui/gui.py` (the control panel and menus, about 1,700 lines);
  - `hosts.py`, `swl.py` and `sgl.py`;
  - `docs/anomalies_and_quirks.md` (the Tk and Xvfb entries).
- **The canvas interface.** A panel's canvas is any object with `tcl(*args)`,
  `get_background_color()` and `set_background_color_bang(color)`. The panels send only a
  few Tk canvas commands: `create` (line, rectangle, oval, arc, polygon, text, window?),
  `delete`, `move`, `raise`, `itemconfigure`, `scale` and `bbox`, with tags. Item 01
  inventories them exactly from the code and the `sgl-tcl` fixtures.
- **Hosts.** `hosts.set_window_host_maker(maker)` decides what `make-graphics-window`
  creates. There are offscreen hosts and Tk hosts today. A Qt host maker is the natural
  seam.
- **Toolchain** (installed): Python 3.12.13 with PySide6 6.11.2 and pytest 7.4. Qt runs
  headless with `QT_QPA_PLATFORM=offscreen`, which the loop driver sets for every session,
  so Qt tests need no X server. tkinter comparisons still need `xvfb-run -a`.
- **Layout** (create as needed):
  ```
  python/metacat/qt/        __init__.py, __main__.py, canvas.py (Tk commands → QGraphicsScene),
                            fonts.py, hosts.py, mainwindow.py, controls.py, engine_bridge.py
  python/tests/test_qt_*.py
  docs/qt-gui-plan.md       the inventory and the design (item 00)
  ```
- **Constraints:**
  - never edit the frozen paths (see `gate.py`);
  - PySide6 only (no other new dependency);
  - keep the GPL headers and the "translated" lines style.

## Important Notes
- **Threading.** The engine runs in a worker thread. The Tk GUI marshals canvas calls to the
  Tk thread with `ThreadSafeTk`. In Qt, every scene change must happen on the GUI thread:
  use queued signals or `QMetaObject.invokeMethod`, never touch widgets from the worker.
  Some panel code asks the canvas for answers (`bbox`, text widths), so the bridge needs a
  blocking call for those. Avoid deadlocks: the GUI thread must never wait on the worker
  while the worker waits on it.
- **Speed.** A run sends a very large number of canvas commands. Batch them per refresh
  (for example a 50 ms timer, as racket/gui does), and keep the slider's "fast" end fast.
  Measure the run time of run 7 with the Qt GUI against the tkinter GUI and record both.
- **Resizing.** The original's windows can be resized (`make-resizable`, the resize
  listener, panels redrawn at the new size). In one window the panes change size all the
  time. Prefer the original's own resize protocol where a panel has one, and otherwise
  scale the view. Record the choice per panel.
- **Fonts.** Tk font specs (family, negative = pixel size, weight, slant) become `QFont`s.
  Text measurement feeds layout, so measure with the same fonts you draw with. Only the
  pictures may depend on fonts, never the model.
- **Headless always.** The driver unsets `WAYLAND_DISPLAY` and sets
  `QT_QPA_PLATFORM=offscreen`. Never open a window on the owner's screen. Every Qt test
  and script must quit its `QApplication` by itself.
- **No background waiting.** Run tests in the foreground and finish PROGRESS.md before
  your last message.
- Do not commit or push. The driver commits after the gate passes and pushes to `origin`.
