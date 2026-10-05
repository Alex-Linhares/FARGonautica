# Divergences from the original

Where the Racket port deliberately behaves differently from Metacat 1.2 in
`chez_scheme/original/`. Without an entry here, the original is right.
Each entry: what differs, where, why, and how the oracle/tests account for it.

## Drawing: Tk's canvas is emulated on racket/draw (item 12)
- **What:** the original drew its SGL pictures as Tk 8.x canvas items, measured text
  with Tk on the logo window's hidden canvas, and sized fonts at the screen's resolution.
  racket/gui/sgl.rkt records the same canvas items (checked against the original by
  tests/diff/sgl-battery.scm) but paints them itself on a racket/draw `dc<%>`, and
  racket/gui/fonts.rkt measures text with racket/draw on a private bitmap dc, with
  point sizes converted at a fixed 96 dpi. So:
  - pixels differ from a Tk screenshot: the faces are whatever fontconfig gives for
    `times`/`helvetica`, text widths and heights come from Pango (Tk's text bbox may be
    a pixel or two larger), arcs and dashed curves are drawn by Cairo (dashed curves
    are flattened first), and line ends follow X11's rule only approximately;
  - `get-pixel-size` works without `(create-mcat-logo)`, where the original raised
    "need to run (create-mcat-logo) first";
  - a font's `get-actual-values` reports the requested face (not the face fontconfig
    picks), and a pixel size as points at 96 dpi.
- **Why:** Tk is not available to Racket; racket/draw is the target the item names.
  Fixed 96 dpi keeps offscreen renderings (and the snapshot test) independent of the
  display.
- **Tests:** none of this reaches the model: the engine never measures text (the golden
  runs are headless). racket/tests/sgl-test.rkt pins the rendering with a pixel snapshot.

## Windows: offscreen hosts, aliased text, full-speed settings (item 13)
- **What:**
  - general-graphics.ss's `make-graphics-window` made a Tk toplevel with a frame or
    scrollframe around the viewport. racket/gui/views.rkt makes a *window host* instead
    (`make-window-host`): offscreen by default (it keeps the title and geometry and has
    no scrollbars), replaced by on-screen hosts once the control panel exists (item 15,
    `set-window-host-maker!`). `reposition-vertical-scrollbar` scrolls the viewport
    itself, rather than waiting for a Tk scrollbar to appear.
  - Text is drawn aliased (`'unsmoothed`). Item 12 used greyscale antialiasing.
  - `attach-views!` (and item 13's `attach-workspace-view!`) set gui.ss's speed settings
    as at full speed with no flashing (`%num-of-flashes%` 1, `%flash-pause%` 0,
    `%snag-pause%` 0, `%codelet-highlight-pause%` 0, `%text-scroll-pause%` 0). The
    original set them from the speed slider when the control panel was made; with the
    control panel (item 15) the slider sets them as in the original.
- **Why:**
  - Hosts: racket/gui is not available to engine-side tests, and pictures of the views
    must be possible without a display.
  - Aliased text: the graphics erase by drawing the same text again in the background
    colour. Over antialiased text that leaves grey fringes (seen in the first renders
    of item 13: a ghost "?" where the answer is drawn, smears after concept mappings
    that switched font). X11's core fonts, which the dissertation's screenshots show,
    were aliased, so overpainting erased them exactly.
  - Speed settings: no control panel yet. The pauses only make the program wait, and
    with `%flash-pause%` 0 a flash draws nothing.
- **Tests:** racket/tests/views-test.rkt (item 13's workspace-view-test.rkt) runs all 109
  golden runs with every window attached (item 14) and requires identical traces; its pixel snapshots and
  racket/tests/sgl-test.rkt's pin the rendering.

## The control panel and windows on racket/gui (item 15)
- **What:**
  - gui.ss's widgets are racket/gui's (racket/gui/gui.rktl). The actions, the control
    panel object's messages and their effects, and the window controllers are the
    original's. These differ:
    - racket/gui gives no colours to panels, buttons or menu items, and no fonts to
      menu items. The control panel keeps its fonts and the labels' colours. The
      command line's run mode is "running..." on a green field (the original:
      green bold italic on black). Highlighted menu items (the last demo, the
      commentary font) are checked items.
    - Help and Clear Memory were commands in SWL's menu bar; racket/gui's menu bar only
      holds menus, so they are in a Help menu and a Memory menu.
    - `display-error` and the input dialogs' "Invalid input!" turn red for 700 ms on a
      timer instead of `(pause 700)` in the event thread.
    - Dialogs are frames, not modal; their `destroy` runs the destroy-request handler,
      as SWL's did.
    - The speed slider's initial value sets the speed when the panel is made (Tk's scale
      command did it).
  - An error in the model, which stopped the original at the REPL with the panel still
    in run mode, returns the panel to input mode and shows "Error: ..." in it.
  - Windows are tiled on the screen (racket/gui/gui.rkt's `arrange-windows!`); the
    original left placement to the window manager. Aspect-ratio bounds are not enforced
    (racket/gui has none); resizing still goes through the original resize handler and
    listener.
  - `create-mcat-logo` measures nothing: scrollbars are Tk's X11 size (15 pixels), and
    fonts measure on a private bitmap (item 12).
  - The REPL thread is an engine thread that runs the thunks the control panel hands it
    with `thread-break`. There is no REPL: `racket racket/main.rkt` opens the windows as
    `(setup)` did.
- **Why:** racket/gui's widget set; a display-free test suite for everything else.
- **Tests:** racket/gui-tests/control-panel-test.rkt drives the panel's own widgets on
  Xvfb. Its full, stepped, stopped and resumed, breakpointed and reset runs all end at
  the golden's codelet count and generator state.

## Racket: one window (loop0003 item 10)
- **What:** `racket racket/one-window.rkt` (racket/gui/one-window.rkt) is a second GUI,
  next to the multi-window one (`racket/main.rkt`, unchanged). The control panel and the
  eleven graphics windows are in one frame, the screen's size less 40 pixels:
  - the menu bar is the control panel's (Help, Demos, Windows, Options, Memory); its widgets
    are moved into a strip under it (problem and command line; speed slider and Step, Go,
    Stop, Reset; breakpoint and self-watching messages);
  - the windows are panes in the Qt GUI's arrangement (docs/qt-gui-plan.md 2.1-2.2):
    rows of 60 %, 31 % and 9 % of the height (18 % when the EEG is shown), each pane of
    fixed aspect ratio as wide as its ratio gives at the row's height, the Temperature
    max(60, 6 % of the width), the Commentary and the Memory the rest;
  - a window that doesn't scroll is letterboxed at the top centre of its pane at its
    window's (w+2):(h+2) ratio, the margins in its background colour; the scrolling ones
    (Commentary, Memory, Trace, EEG) fill their panes. Every pane reaches its size through
    the original's resize handler and listener, one pane at a time (a pane waits until the
    listener's queue is empty, since the listener keeps only the last of simultaneous
    resizes);
  - racket/gui has no splitters, so the panes can't be dragged to other sizes, and no
    layout is saved. The Windows menu hides and shows panes, and the others close up;
  - the Logo window (Windows > Show Logo) is still a separate small window.
- **Why:** loop0003's goal is one window for everything; the Python Qt GUI is the full
  version (fixed splitters, saved layout), and this is the optional Racket counterpart
  (iterations.md item 10).
- **Tests:** racket/gui-tests/one-window-test.rkt: Run 7 typed into the strip and run with
  Go, with the window resized during the run, writes exactly its golden trace (2,667
  events between the start and end lines) and ends at 2170 codelets with the golden's
  generator state. It also checks the layout, the hidden and shown panes, self-watching
  off and on, and a screenshot's pixels pane by pane.

## Entry points: a command line, a headless CLI and a standalone program (items 11, 15–16)
- **What:** the original was used from the Chez/SWL REPL: `(setup)` opened the windows,
  and `(run ...)`/`(mcat ...)` or the control panel ran problems. The port has no REPL.
  - `racket racket/main.rkt [SCALE]` does what `(setup)` did.
  - `racket racket/cli.rkt INITIAL MODIFIED TARGET [ANSWER] [--seed N] [--max-codelets K]
    [--keep-going] [--trace FILE] [--verbose]` runs a problem headless. It is the port's
    counterpart of the oracle's `chez_scheme/oracle/run.ss`, which is not part of the
    original either. It prints the commentary, answers and a summary, and the run ends
    where the original would wait for Go, unless `--keep-going`.
  - The standalone `metacat` program (make-dist.sh) opens the GUI with no arguments or a
    scale, and is the CLI otherwise.
- **Why:** there is no SWL REPL to type into; batch runs and tests need a program.
- **Tests:** racket/tests/cli-test.rkt (the CLI against the oracle's run.ss, output and
  exit codes, including `--verbose`), racket/gui-tests/dist-test.rkt (the standalone
  program), racket/gui-tests/control-panel-test.rkt (the GUI).

## Python Qt GUI: the Tk canvas on a QGraphicsScene (loop0003 item 02)
- **What:** in the Qt GUI (`python3 -m metacat.qt`), the panels' Tk canvas commands are not
  run by Tk. `python/metacat/qt/displaylist.py` keeps Tk 8.6's display list itself: ids,
  stacking, tags, options, `move`, `scale`, `raise`/`lower`, `itemconfigure`, and `bbox`
  with Tk's rules. `python/metacat/qt/canvas.py` paints each item on a `QGraphicsScene`
  as X11 would: corners rounded to whole pixels, Tk's pen widths and dash patterns,
  butt caps, no antialiasing except for text. So:
  - pixels differ from the tkinter GUI's: Qt rasterises wide lines, arcs and dash phases
    a little differently (X11's jog in wide diagonal lines is gone), and text is drawn
    by Qt;
  - text extents come from Qt's metrics of the same fontconfig faces. Widths are Tk's
    for 97% of the reference samples; ascent and descent are the ink of the font's
    Latin-1 glyphs, as X's core fonts compute them, so heights are within 3 pixels of
    Tk's (item 03). All text items' bboxes are within 2 pixels of Tk's;
  - text is antialiased, where the tkinter GUI's Tk draws X core fonts in bitmaps.
    Thin letters of small text no longer vanish: the Coderack's 8-pixel labels show
    their `i`s and `l`s;
  - the faces: fonts.ss picks from `qt/fonts.families()`, fontconfig's families plus
    the preferred faces fontconfig maps onto a real face. On this machine that gives
    `times new roman` (Liberation Serif) for serif and `palatino linotype` (P052) for
    fancy, where the tkinter GUI, listing X's core font names, picks `times` (Nimbus
    Roman) and `palatino` (also P052's design). Liberation Serif has Times New Roman's
    metrics, so serif text is laid out a little differently from the tkinter GUI's;
  - colour words are read as Tk 8.6 reads them (`gray` is `#808080`), but the panels
    only send `#rrggbb`.
- **Why:** the single-window GUI is Qt; Tk can't draw into it.
- **Tests:** `python/tests/test_qt_canvas.py` replays every `sgl-tcl` stream and a
  synthetic one into the Qt canvas and into a tkinter Canvas (reference
  `python/tests/data/tk-display-lists.json`). With Tk's text metrics, every item, option,
  tag, coordinate and `bbox` answer is identical; with Qt's fonts, only text extents
  differ, by 5 pixels at most. The SGL fixture drawn on the Qt canvas passes
  `render_sgl_fixture.py`'s pixel checks, and 97.7% of its pixels match the tkinter
  picture.

## Python Qt GUI: windows as panes (loop0003 item 04)
- **What:** in the Qt GUI each graphics window is a pane of one main window
  (`python/metacat/qt/hosts.py`, `mainwindow.py`), in the fixed splitter tree of
  `docs/qt-gui-plan.md` 2.2. So:
  - a pane has no title bar: window titles are kept but not shown;
  - Tk's `wm aspect` has no pane equivalent. An unscrollable window's view is the
    largest rectangle of Tk's ratio, (w+2):(h+2), inside its pane, centred (the
    Temperature at the top), and the margins are painted in the panel's background
    colour (letterboxing);
  - the scrolling windows (Trace, EEG, Commentary, Memory) always show their scroll bar,
    as `TkHost` packs it. The scroll bar extent comes from Qt's style
    (`PM_ScrollBarExtent`) and goes to fonts.ss's `*scrollbar-width*`/`-height*`;
  - a pane's size reaches the panel through the original's own protocol
    (`viewport.configure(w+2, h+2)`, make-resizable's handler, the resize listener), but
    the configures of several panes are sent one at a time, so that general-graphics.ss's
    single resize queue drops none (anomalies: "One resize queue for every window");
  - the EEG pane is hidden at start, as the EEG window is.
- **Why:** one window for everything, with fixed splitters (the owner's decision).
- **Tests:** `python/tests/test_qt_panes.py`: the layout, letterboxing, scroll bars,
  feeding, and golden runs (run7 at 300 and 800 codelets and at the answer, `a b z`
  seed 1, and run7 with 14 window resizes) giving their golden traces in the panes.

## Python Qt GUI: the engine thread and the run controls (loop0003 item 05)
- **What:** the Qt GUI runs the engine in `metacat.gui.app.EngineThread`, as the tkinter
  GUI does, with the control panel as a strip above the panes
  (`python/metacat/qt/controls.py`, `engine_bridge.py`). It answers gui.ss's messages
  and runs gui.py's own button and slider actions. Differences in how it does so:
  - the messages the engine sends from its thread that change widgets
    (`switch-to-run-mode`, `switch-to-input-mode`, the breakpoint messages,
    `engine-error`) are posted to the GUI thread and return at once. SWL and
    `ThreadSafeTk` made the engine wait until the widget had changed. No caller uses
    their value;
  - the panes show the canvases as of the last 50 ms refresh (and at once when the engine
    parks), so a flash shorter than 50 ms may not show, as in the Racket port;
  - the paint gate (`canvas.PAINT_GATE`): while the GUI thread brings the scenes up to
    date or paints a pane, the engine thread waits at its next canvas command. This
    changes timing, never a run (anomalies: "Qt paints the panes slowly while the engine
    runs in another thread");
  - a demo starts through the panel's `run-demo` message (a port addition: gui.ss's
    demo menu item action, `init-new-problem` with step mode off);
  - the input dialog is a Qt dialog titled "Input": Enter reads the field as gui.ss's
    `input-dialog` does; Escape also closes it.
- **Why:** Qt's widgets belong to the GUI thread; the engine must never wait for it while
  it paints (docs/qt-gui-plan.md 2.5, deadlock rules).
- **Tests:** `python/tests/test_qt_engine.py`: the bridge, the panel's modes and actions,
  the dialogs, painting while another thread runs, and `drive_qt_gui.py` (a full run,
  step mode, a demo stopped and restarted, a breakpoint, Reset, a justify run, 50 Go/Stop
  toggles, each trace equal to its golden).

## Python Qt GUI: the menus and dialogs (loop0003 item 06)
- **What:** gui.ss's menus are in the main window's menu bar, in the order Help, Demos,
  View, Options, Memory (`python/metacat/qt/controls.py`). Each item has the original's
  label, font, kind and action, and the same enabled and disabled states in every run
  state. The differences:
  - **View replaces Windows.** One checkable action per pane, in the Windows menu's
    order. A check mark replaces the "Hide X" / "Show X" titles and their on/off colours.
    "Show all windows" and "Hide all windows" are "Show all panes" and "Hide all panes".
    There is a new **Reset layout** item, which shows the default panes and restores the
    default splitter sizes. There is no Logo item, because the Logo is not a pane.
    Hiding both Themes panes also hides their splitter, so no gap is left;
  - **Help and Clear Memory are menus**, "Help > Metacat help" and "Memory > Clear
    Memory", rather than commands on the menu bar. There is no blank spacer between
    Options and Clear Memory. Clear Memory is not drawn red when active;
  - **highlighted items are checked.** The current demo (until the next problem) and the
    current commentary face and size have a check mark. The original gave them a white
    background, which a QMenu can't give one item;
  - the commentary font items use the faces the Qt GUI picked (item 03), Liberation Serif
    and P052 rather than Tk's Times and Palatino;
  - disabling Demos, Options or Memory (run and disabled modes) disables the menu bar's
    item only, as gui.ss's `set-enabled!` did. The items inside keep their own states;
  - the dialogs are Qt dialogs with the original's titles, texts, fonts, colours, buttons
    and offsets from the control strip: Help (help.txt, read-only, word-wrapped, Courier
    on bisque), Confirm (Clear Memory; the yellow theme-edit dialog), Input, and Qt's
    file dialog for "Save Commentary to File". Escape closes a dialog as the window
    manager's close does, which runs its destroy handler (Cancel);
  - the clamp-codelets items look their pattern up when chosen, not when the menu is
    made, so that the panel can be made before the engine is loaded;
  - `display-error` puts back the last displayed message, not the label's text (anomalies:
    "Two errors within 700 ms leave the first one on the control panel");
  - the breakpoint message and the self-watching warning sit one above the other at the
    end of the strip, so that the strip fits a 1920-pixel window with both showing.
- **Why:** one window (TASK.md), with Qt's menu conventions where Tk's look can't be had.
- **Tests:** `python/tests/test_qt_menus.py` compares the Qt menu bar and control panel
  with `python/tests/data/tk-gui-inventory.json` in its five states, and
  `drive_qt_menus.py` drives every item and dialog.

## Python Qt GUI: the mouse and the keys (loop0003 item 07)
- **What:** the panes take the bindings of `gui/hosts.py`'s `TkHost`: a left press gives
  the viewport `(left-button)`, a left press with Shift `(shift left-button)`, a right
  press `(right-button)`, whatever other modifiers are held, as Tk's binding rules give
  them. The handler runs on the GUI thread, as it ran on Tk's. A double click is two
  presses, as in Tk. The command line and the input dialog take Return with any
  modifiers and not the keypad's Enter, as Tk's `<Key-Return>` does
  (`metacat/qt/controls.py`'s `bind_return`; QLineEdit's `returnPressed` would also take
  the keypad's Enter). The differences:
  - a press in a pane's letterbox margin does nothing: it is outside the canvas, and Tk's
    windows, sized by `wm aspect`, had no margin;
  - the wheel over a pane's canvas does nothing (Tk's canvas has no wheel binding); over a
    pane's scrollbar it scrolls, as it does over Tk's.
- **Why:** parity. `python/tests/test_qt_clicks.py` drives the same mouse and key
  scenario through QTest's events in the Qt GUI and Tk's events in the tkinter GUI and
  finds the same model states and traces.

## Python Qt GUI: the layout, its saving, and the icon (loop0003 item 08)
- **What:** things the original's separate windows didn't have, or had differently:
  - the layout is remembered: the splitter sizes (once a handle has been dragged), the
    hidden panes and the window's geometry are saved in `QSettings` and restored at the next
    start; View > Reset layout forgets them. The original's windows always opened at
    their default places and sizes;
  - on a screen larger than 1920×1010 the window opens maximised and every pane grows with
    it (the default proportions of `docs/qt-gui-plan.md` 2.2 follow the window);
  - when the EEG is shown in the default layout, the bottom row doubles in height for it
    (the EEG was a window of its own);
  - the window has a minimum size, 1600×800 or the control strip's width (1692 px with
    the 1080p fonts); the original's control panel could be shrunk until it clipped;
  - the Logo window is the window's and the application's icon, drawn by the original's own
    Tk command (`metacat/qt/icon.py`), not a window, and there is no Show/Hide Logo item;
  - high DPI is Qt 6's scaling: at 200 % the panels are drawn at twice the resolution in
    the same logical sizes. Tk 8.6 under X11 doesn't scale.
- **Why:** one window on a 1080p-or-larger screen (TASK.md). Watching still changes no run:
  `python/tests/test_qt_layout.py` runs `abc abd ijk abd 1` in the window at 1920×1080 and
  2560×1440 and checks the codelet count and random state against the golden.


## Python Qt GUI: packaging and a third command (loop0003 item 09)
- **What:** the original had one program, started from Chez Scheme's REPL. The Python port
  installs three commands: `metacat` (headless), `metacat-gui` (the tkinter GUI, the
  original's windows) and, with the optional `qt` extra (`pip install -e 'python[qt]'`,
  which adds PySide6), `metacat-qt` (every window as a pane of one Qt window). Without
  PySide6, `metacat-qt` prints `The Qt GUI needs PySide6: pip install -e 'python[qt]'`
  and exits with status 1; the engine and the tkinter GUI never import the Qt package.
  `metacat-qt` also takes `--settings INI` (where the layout is saved; or
  `$METACAT_QT_SETTINGS`), and `--quit-after MS` and `--screenshot PNG` for scripts.
- **Why:** PySide6 is large, and the engine and the tkinter GUI need only the standard
  library (TASK.md), so the Qt GUI is opt-in. `python/tests/test_install.py` installs
  both ways into fresh venvs: with the extra, `metacat-qt` opens and closes headlessly;
  without it (a venv that can't see PySide6), Run 7 still gives its golden trace and
  `metacat-gui` opens.

## Python Qt GUI: found by the final audit (loop0003 item 11)
- **What:** `python/tests/test_qt_audit.py` compares the Qt GUI with every entry of the
  tkinter inventory. Two differences remain, both in how things look, never in what they do:
  - the Step, Go, Stop and Reset buttons are plain `QPushButton`s. gui.ss gives them an
    active (mouse-over) background, green for Step, Go and Reset and red for Stop, which
    the Qt strip doesn't reproduce;
  - a pane's panel is told the pane's size, so the windows' sizes differ from the
    inventory's default sizes (already in "windows as panes" above). The hidden EEG is
    told its pane's size once at start too, where the tkinter GUI's hidden EEG keeps its
    default size (`docs/anomalies_and_quirks.md`, "A hidden pane still gets a configure").
- **Why:** the button colours are a hover effect of Tk's, and Qt's style draws its own; the
  audit found it at the end of the loop and recorded it (`docs/follow-ups.md`). The panels,
  scrolling, aspect ratios, resize methods, press handlers, backgrounds, the slider's
  settings, the Input dialogs' places and rules, the buttons' actions and the close all
  equal the inventory's.

## Python Qt GUI: the command line starts with Run 7 (2026-10-05, owner's request)
- **What:** `python3 -m metacat.qt` (and `metacat-qt`) starts with `abc abd xyz 3852097033`
  (`metacat.qt.app.DEFAULT_PROBLEM`, Run 7 of the dissertation) in the control strip's
  command line, with the focus on it, so that Enter starts the run. Words on the command
  line replace it (`python3 -m metacat.qt abc abd ijk 7`). `--empty` starts with an empty
  box, as the original's control panel does.
- **Why:** the owner asked for it, so the program shows something at once.
- **Scope:** only the launcher (`app.main`) fills the box. `app.setup` and `open_window`,
  which the parity tests and drivers use, leave it empty, so those tests still compare
  with the original's initial state. The engine and the tkinter GUI are unchanged.
  Tests: `python/tests/test_qt_launcher.py`.

