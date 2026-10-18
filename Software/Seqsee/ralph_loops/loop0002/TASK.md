# TASK: A Qt (PySide6) GUI for the Python port of Seqsee

## Philosophy
- **Faithful drawings, modern shell.** Each view the Perl/Tk GUI draws (Workspace,
  Workspace_Attention, Slipnet, Coderack, Stream, Relations, the Groups/Categories/
  Rules/Stream lists, and the composite views of `Tk/Seqsee.pm`) is reproduced: the
  same shapes, positions, colours and text for the same model state. The window
  around them is modern Qt: menus, a toolbar, dockable panes, a speed slider,
  PNG/SVG export and remembered layout.
- **Tests first, against the real Perl GUI.** Perl/Tk runs here on a virtual X
  display. For each drawing item: write a Perl oracle that builds a deterministic
  model state, draws it on a Tk canvas and dumps every canvas item as golden JSON.
  Write pytest tests that compare the Python drawing against that golden data.
  The tests must fail first; then implement until green.
- **Perl is the source of truth for drawings.** If your reading of the source
  disagrees with the dumped canvas, the dump wins. A Perl drawing bug is ported
  faithfully, with a `# PERL-QUIRK:` comment and a note in PROGRESS.md.
- **Never change model behaviour.** The headless port in `python/src/seqsee/`
  (loop0001) is done and verified against Perl. The GUI only reads the model and
  drives it through the existing entry points (`seqsee_main.interaction_step_n`,
  `Global.Break_Loop`, `Global.InterstepSleep`, `user_interaction.install`, …).
  If the GUI needs a hook the model lacks, add the smallest possible hook, with
  no change in headless behaviour, and say so in PROGRESS.md.
- **Small, green steps.** ONE item from `iterations.md` per iteration. The full
  test suite must pass when you finish.

## Architecture (follow it; record any deviation in PROGRESS.md)
- `seqsee/gui/` is a new subpackage. The core `seqsee` package must never import
  it, and must still import and pass all tests without PySide6.
- `seqsee/gui/draw/` is **pure Python, no Qt imports**. Each Perl drawing module
  becomes a function from a *snapshot* plus a rectangle `(x, y, w, h)` to a list
  of draw ops: a small dataclass mirroring Tk canvas items (`line`, `oval`,
  `rectangle`, `polygon`, `arc`, `text`, with coords and options such as fill,
  outline, width, arrow, smooth, dash, anchor, font and tags). These ops are what
  is compared with the Perl canvas dumps.
- `seqsee/gui/snapshot.py` builds an immutable, plain-data **snapshot** of everything
  the views read. Drawing code reads only snapshots, never live model objects. Each
  drawing item extends the snapshot with what its view needs.
- `seqsee/gui/qt/` holds the PySide6 code: a renderer from ops to a `QGraphicsScene`
  (or `QPainter`), the widgets, and the main window.
- **Threading.** The model runs on its own worker thread, never on the GUI thread
  (`interaction_step_n` already runs batches on a deep-stack thread). At each display
  update, the worker builds a snapshot and sends it to the GUI with a queued signal.
  The GUI never touches model objects while the worker is running. Model questions
  ("Is the next term …?", `MessageRequiringAResponse`, more-terms requests) are
  installed through `user_interaction.install`. They send the question to the GUI
  thread and block the worker until the user answers. Pause sets
  `Global.Break_Loop`. Model exceptions are shown in the GUI and must not kill it.
- Tk → Qt: Tk canvas y grows downward, like Qt's. Tk X11 font names
  (`-adobe-helvetica-bold-r-normal--9-…`) map to the nearest QFont. Compare text
  content, anchor and position, not glyph metrics.

## Current Focus
Work through `iterations.md` top to bottom. The next unchecked item is the current
one (the loop runner tells you which).

## Acceptance Criteria (per item)
- [ ] Drawing items: an oracle `python/oracle/gui_<name>.pl` and its golden file
      `python/tests/golden/gui_<name>.json` (generated, never hand-edited) that cover
      several deterministic states (empty, small, nested/edge cases)
- [ ] New/updated tests in `python/tests/` (GUI tests in files named `test_gui_*.py`);
      each test module's docstring names the Perl source file(s) it mirrors
- [ ] Implementation in `python/src/seqsee/gui/`
- [ ] Items that draw or show something: a screenshot saved under
      `python/docs/gui/screens/` (Qt) and, where Perl draws the same thing, a Perl one
      under `python/docs/gui/perl/`. **Look at both images** with the Read tool and
      write the visible differences in PROGRESS.md.
- [ ] `cd python && python3 -m pytest` passes (whole suite, no regressions)
- [ ] `python/PORTING_MAP.md` row(s) updated (the single "SGUI*, Tk/*, Themes/*,
      SColor.pm, UI/Graphical.pm — skipped" row is split into one row per module by
      item 000)
- [ ] Item marked in `iterations.md`: `[x]` done, or `[!]` blocked (with reason)
- [ ] An iteration section appended to `PROGRESS.md`

## Completion Conditions
An item is DONE when either:
1. **Solved**: meets all criteria, OR
2. **Blocked**: marked `[!]` with the specific blocker written in PROGRESS.md

When every item is `[x]` or `[!]` and the final audit item is done, append
`LOOP_COMPLETE` on its own line at the end of PROGRESS.md.

## Context
- **Perl GUI source**: `Seqsee/lib/` (~2.7k lines): `SGUI.pm`, `SGUI/*.pm`,
  `SGUI/List/*.pm`, `Tk/Seqsee.pm` (the canvas and its 11 `@ViewOptions`), `Tk/SCommentary.pm`,
  `Tk/SCodeletCount.pm`, `Themes/Std2.pm` (the `Style::*` functions), `SColor.pm`,
  and `UI/Graphical.pm`. GUI wiring is in `Seqsee.pl` (`init_display`, `Interaction_*`).
  The layout config files are `config/GUI_ws3.conf` and `config/GUI_sparse.conf`
  (frames, buttons, key bindings). The sequence menu is `config/sequence.list`.
- **Python model**: `python/src/seqsee/`, which is complete. Read `python/README.md`
  and `python/PORTING_MAP.md` to find the port of each Perl module.
- **Python**: the base anaconda `python3` (3.12). Allowed dependencies: the standard library,
  **PySide6** (runtime, GUI only) and **pytest, pytest-qt, hypothesis** (tests).
  All of them are installed. Ask before adding any other dependency (record the
  need as a blocker instead).
- **Testing**: `cd /home/al/dev/SeqSee/Seqsee/python && python3 -m pytest`. Qt tests must
  run headless: set `QT_QPA_PLATFORM=offscreen` in `tests/conftest.py`, use
  pytest-qt's `qtbot`, and never open a real window or block on `exec()`. Keep the
  GUI tests fast (seconds, not minutes); mark long ones `@pytest.mark.slow`.
- **Constraints**: never modify anything outside `python/` and this loop folder.
  Never edit the Perl code. Don't run git commands that change state; the loop
  runner commits.

## Perl GUI oracle
- `python/oracle/run_perl_gui.sh script.pl` runs Perl/Tk on a private Xvfb display.
  It goes through `run_perl.sh`, so it gets the fixed hash seed, ASLR off, `lib/`
  and `oracle/` on `@INC`, and a 4 GB memory cap. `run_perl.sh` alone is still fine
  for non-Tk code. `cd python && python3 oracle/regen.py gui_<name>` writes the
  golden file (check that regen.py handles the GUI wrapper; extend it in item 000
  if not).
- **Working prototype**: `ralph_loops/loop0002/reference/ws_dump_prototype.pl` ran a 400-step
  Perl model, drew `SGUI::Workspace` on a 780×450 canvas, and dumped the canvas items.
  It also saved the canvas as EPS. Result: `reference/perl_workspace_400steps.png`. Lessons:
  - `use Themes::Std2;` must come before any `SGUI::*` module.
  - Each drawing module is set up with `SGUI::X->Setup($canvas, $x, $y, $w, $h)` and
    drawn with `SGUI::X->DrawIt()`.
  - Dump with `$c->find('all')`, `$c->type($id)`, `$c->coords($id)` and
    `$c->itemcget($id, '-fill')` etc. Round coordinates (e.g. to 0.1).
  - For a screenshot, call `$mw->update` first, then `$c->postscript(-file => 'x.eps')`,
    then `gs -q -dSAFER -dBATCH -dNOPAUSE -sDEVICE=png16m -dEPSCrop -r96 -sOutputFile=x.png x.eps`.
- **Goldens come from deterministic states, not stochastic runs.** Python can't
  reproduce a Perl run's trajectory. So each drawing oracle builds its states
  directly, by inserting elements, creating groups, relations, metonyms,
  activations, codelets and thoughts, the way the loop0001 oracles do (e.g.
  `oracle/sworkspace_groups.pl`, `oracle/scoderack.pl`). The Python test builds the
  same states through the port, takes a snapshot and draws it. Use the same named
  recipe in both languages. Never record stringified refs or addresses. Where the
  Perl draw order depends on hash order, compare as multisets.
- Comparison helper (item 000): match items by type, compare coords within a small
  tolerance (±0.5 px), and compare options exactly after normalising (e.g. Tk
  colour names to `#RRGGBB`).

## Important Notes
- Read PROGRESS.md first. Earlier iterations record decisions you must follow.
- Keep the iteration focused. If an item turns out too big, finish a coherent part,
  then split the rest into a new item inserted directly after it (e.g. `005b`) and
  mark the original `[x]`.
- If you find missing work that belongs earlier, add a new item rather than
  expanding the current one.
- Look at what you draw. A test that passes while the screenshot looks wrong is a
  failing test you haven't written yet.
