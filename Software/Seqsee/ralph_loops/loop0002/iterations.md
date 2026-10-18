# Iterations: loop0002, the Qt (PySide6) GUI

One item per iteration, top to bottom. `[x]` done, `[!]` blocked (reason in PROGRESS.md).

## A. Foundations
- [x] 000 GUI test infrastructure — `oracle/CanvasDump.pm` (dumps every Tk canvas item: type, coords, options, tags; can also save an EPS and convert it to PNG); regen.py support for `run_perl_gui.sh`; `seqsee/gui/draw/ops.py` (the draw-op dataclasses); `tests/gui_compare.py` (tolerant op comparison and colour normalisation); conftest with `QT_QPA_PLATFORM=offscreen`, a `gui` marker and PySide6 importorskip; split the GUI row in PORTING_MAP.md into one `todo` row per Perl GUI module; one smoke golden (an empty canvas plus one hand-made item of each type)
- [x] 001 Snapshot I — `seqsee/gui/snapshot.py`: list every model attribute that the SGUI/Tk modules read (write the list in PROGRESS.md); an immutable snapshot of the workspace (elements, groups with spans/strength/categories/metonyms, relations, bar lines, last runnable, steps); tests: it matches the live state, it doesn't change when the model changes later, it's fast enough
- [x] 002 Colours & theme — SColor.pm (`HSV2RGB`, `HSV2Color`) and Themes/Std2.pm (every `Style::*`) → `seqsee/gui/draw/theme.py`; goldens from calling each Style function in Perl over a grid of arguments (exact match)
- [x] 003 Model runner — `seqsee/gui/runner.py`: worker thread; commands step / step_n / crawl(ms) / continue / pause / new_sequence / quit; snapshots at every `update_after` steps (throttled to ~30 per second); model exceptions sent to the GUI as a signal; questions bridged through `user_interaction.install` (the worker blocks until answered); tests with qtbot: snapshots arrive, pause stops the run, a question round-trip works, no deadlock, and the Alternating `1 1 1 2 2 3 …` sequence (deep recursion) doesn't crash

## B. Drawings (pure draw ops, checked against Perl canvas dumps)
- [x] 004 Workspace I — SGUI/Workspace.pm: `Setup` geometry, elements (`draw_ws3`), bar lines, last runnable; states: empty, 1 element, 6 elements, 20 elements, several rectangle sizes
- [x] 005 Workspace II — SGUI/Workspace.pm: groups (nested heights, `Style::Group`/`GroupBorder`), relations (arcs, hidden relations, anchors), metonyms, starred/squinted items; states with nested groups, overlapping relations and metonyms
- [x] 006 Workspace attention — SGUI/Workspace_Attention.pm (`draw_attention` for elements, groups and relations; legend; attention styles), with attention values taken from the snapshot
- [x] 007 Slipnet — SGUI/Slipnet.pm (node ovals sized by activation, columns, `MinActivationForDisplay`, text width) and the SlipnetLayout config
- [x] 008 Coderack — SGUI/Coderack.pm (`family_to_name`, columns and rows, counts, urgencies, historical fractions)
- [x] 009 Stream — SGUI/Stream.pm (`DrawThought`, ThoughtBox/ThoughtComponent styles, fringe display)
- [x] 010 Relations pane — SGUI/Relations.pm (rows: strength, simplicity, ends, type)
- [x] 011 List base + Groups list — SGUI/List.pm (paging, bookkeeping, `GetEntriesOnCurrentPage`) and SGUI/List/Groups.pm
- [x] 012 Categories, Rules & Stream lists — SGUI/List/Categories.pm (`MarkDescendentsToKeep`), List/Rules.pm, List/Stream.pm
- [x] 013 Composite views — Tk/Seqsee.pm: all 11 `@ViewOptions` (sub-rectangles as fractions of the canvas), `AttentionNeeded` and the attention-directing arrows; a `compose(view, snapshot, w, h)` function; the Perl oracle draws the full Tk::Seqsee widget for each view and state

## C. The Qt application
- [x] 014 Qt renderer — `seqsee/gui/qt/render.py`: ops → QGraphicsScene items, for every op type and option (arrows, smooth lines, dashes, anchors, fonts mapped from X11 names); tests check the item properties; render every Perl golden from B and compare with the Perl PNGs by eye
- [x] 015 Main window & views — QMainWindow with the canvas, a View menu offering the 11 views, a layout that redraws on resize, Save as PNG/SVG (replacing "Save as EPS"), and Help → About
- [x] 016 Controls — a toolbar and the key bindings from GUI_sparse.conf (s step; h/j/k/l step 5/25/50/100; d/f/g crawl; c continue; p pause; x new sequence; q quit; m toggles debugMAX); an InterstepSleep slider (0–40 ms); the codelet count (Tk/SCodeletCount.pm); a status bar (steps, elements, groups, run state); all of it drives the runner from 003
- [x] 017 Commentary — Tk/SCommentary.pm as a dock: the message log with tag colours from GUI_sparse.conf; MessageRequiringNoResponse, MessageRequiringAResponse (buttons) and MessageRequiringBooleanResponse wired to the runner's question bridge; UI/Graphical.pm's `ask_user_extension` and its messageBox
- [x] 018 Sequence entry — SGUI::ask_seq (an editable combo filled from config/sequence.list, the "Illformed input" check, resetting the workspace, coderack and stream) and SGUI::ask_for_more_terms; a new sequence can be started at any time, including while a run is going
- [x] 019 Modern shell — every pane (Workspace, Attention, Slipnet, Coderack, Stream, Relations, the lists, Commentary) also available as a QDockWidget, as an alternative to the fixed views; seed and max-steps fields; window and dock layout remembered with QSettings (a test checks the round-trip with a temporary settings file)
- [x] 020 List interaction — SGUI::List `ProcessClickOnItem` / `CreatePopupWidget` (clicking an item shows its details), paging controls, and hover tooltips on workspace objects
- [x] 021 Entry point — `python3 -m seqsee --gui` (and `python3 -m seqsee.gui`) with Seqsee.pl's options (sequence, seed, gui_config, update_interval, max_steps); a clear error message if PySide6 is missing; headless CLI behaviour unchanged (the existing CLI tests still pass)

## D. Whole-application checks
- [x] 022 End-to-end GUI run — a qtbot test drives the main window through a full run on `1 1 2 1 2 3` with an automatic answerer; it reaches the solution; screenshots of all 11 views at the end go to `python/docs/gui/screens/`; Perl screenshots of the same views (from a deterministic state) for comparison; differences recorded
- [x] 023 Responsiveness — the event loop stays responsive while the model runs `continue` (a measured latency bound); snapshot + render stays within budget (e.g. ≤ 50 ms per frame for a 30-term workspace); Pause takes effect within one batch
- [x] 024 Final audit — every Perl GUI module has a PORTING_MAP row marked `done` or `skipped` (with reason); no `NotImplementedError("TODO` stubs in `seqsee/gui`; a GUI section in python/README.md (how to launch it, the controls, screenshots); the full suite passes; then LOOP_COMPLETE
