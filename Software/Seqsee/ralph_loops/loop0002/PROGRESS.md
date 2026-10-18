# Progress Log

## Ralph Loop 0002 Status
- **Started**: 2026-10-07
- **Target**: 25 items (iterations.md)
- **Current**: 25/25 SOLVED, 0 blocked

## Setup notes (written before the loop started)
- loop0001 finished the headless port: 5124 tests pass, and end-to-end parity with Perl is
  documented in its PROGRESS.md (iterations 51–52). The model is the fixed base for this loop.
- Perl/Tk 804.036 works, and so do Tk::ComboEntry, Tk::Menustrip and Tk::ROText. Xvfb and gs are
  installed. PySide6 6.11.2 and pytest-qt are installed in the base anaconda Python.
- `python/oracle/run_perl_gui.sh` (new) wraps `run_perl.sh` in `xvfb-run`. `run_perl.sh` caps
  virtual memory at 4 GB (ORACLE_MEM_KB), because an unbounded Perl loop once OOM-killed the
  loop0001 runner.
- `reference/ws_dump_prototype.pl` is a working canvas dump of `SGUI::Workspace` after a 400-step
  Perl run (4 lines, 6 ovals, 7 texts). `reference/perl_workspace_400steps.png` is its rendering.
  Stochastic runs are only for screenshots; goldens must come from deterministic, directly built
  states (see TASK.md).
- Port facts the GUI relies on: `seqsee_main.interaction_step_n` (it already runs on a 256 MiB
  deep-stack thread via `util.call_with_deep_stack`), `Global.Break_Loop`, `Global.InterstepSleep`,
  `user_interaction.install(boolean_response=…, ask_user_extension=…, response=…)`,
  `user_interaction._update_display` and `sworkspace`'s update_display hook, and
  `cli.py` (the headless options and the interactive loop).

---

## Iteration 1 — 2026-10-07 00:50
### Completed
- Item 000, GUI test infrastructure:
  - `python/oracle/CanvasDump.pm`: `dump_canvas($c)` gives one `{type, coords, opts, tags}` per item,
    in draw order. `save_png($mw, $c, $path)` converts the canvas to EPS and then to PNG with gs.
    `perl_screen_path(NAME)` returns `python/docs/gui/perl/NAME.png`. `normalise_colour($w, $c)`
    returns `#RRGGBB`.
  - `oracle/regen.py`: `gui_*.pl` oracles now run through `run_perl_gui.sh` (Xvfb). Other oracles
    still use `run_perl.sh`.
  - `oracle/gui_smoke.pl` → `tests/golden/gui_smoke.json`. It has 4 cases: `empty`; `defaults` (one
    item of each type with no options); `styled` (one or two items of each type, with the options the
    Seqsee GUI uses: fill/outline, width, smooth, arrow + arrowshape, dash string and list, stipple,
    arc start/extent/style, text anchor/font/justify, tags); `colours` (Tk's `rgb` for 250
    names/specs). It also writes `python/docs/gui/perl/smoke.png`.
  - `src/seqsee/gui/__init__.py`, `gui/draw/__init__.py`, `gui/draw/ops.py` (frozen dataclasses
    `Line, Oval, Rectangle, Polygon, Arc, Text`, whose field defaults are Tk's defaults;
    `create(type, *coords, **opts)` takes Perl-style arguments, so `-fill` keys, nested coords,
    `**Style::X(...)` results and tags given as a string all work; lowercase helpers `line()`,
    `oval()`, …), and `gui/draw/colors.py` (`to_hex`, an X11 colour table, grayN/greyN).
  - `tests/gui_compare.py`: `normalize`, `item_diff`, `compare_ops(actual, expected, tol=0.5,
    ordered=True)` and `assert_ops_match`.
  - `tests/conftest.py`: sets `QT_QPA_PLATFORM=offscreen` before anything imports Qt, and skips
    tests marked `gui` when PySide6 is missing (it works like importorskip). The `gui` marker is
    registered in pyproject.toml.
  - `tests/test_gui_infra.py` has 17 tests: the goldens, the colour table, ops, the comparison helper,
    the architecture rules (draw/ has no Qt imports, the core never imports gui, the core imports and
    runs without PySide6), and one `gui` Qt smoke test with qtbot.
  - PORTING_MAP.md: the single GUI row is now 18 `todo` rows, one per Perl GUI module, each naming
    the loop0002 item(s) that port it. I also added a `done` row for the infrastructure.
    `test_porting_audit.py` now lets GUI rows be `todo`/`partial` (other rows must still be done or
    skipped), and a new test checks that each GUI module has exactly one row. **Item 024 must
    restore the strict check** (no todo/partial GUI rows).
- Full suite: passes (about 2 minutes).
### Decisions
- **Dump format**: `opts` lists only the options whose value differs from Tk's default, as given
  by `itemconfigure`'s default column. So the defaults of the Python op dataclasses must equal Tk's;
  the `defaults` golden case checks this with a raw `to_dict()` equality. `null` in `opts` means
  "explicitly unset", e.g. `outline => ''` on a rectangle or `fill => ''` on a polygon. Numbers
  are rounded to 0.1. `smooth` is dumped as 1. A font is dumped as its name (Tk's default is
  `"Helvetica -12"`, which is `ops.DEFAULT_FONT`). Arc `-style` has no Tk default, so `pieslice` is
  treated as the default. Active*/disabled* options, `-updatecommand` and the `current` tag are
  skipped.
- **Perl/Tk quirk: `-dash`**. Perl/Tk 804 reads string dash patterns (`'---'`, `'.'`) back as
  garbage bytes, from both `itemcget` and `itemconfigure`; list patterns read back fine. So
  CanvasDump wraps `Tk::Canvas::create` and `itemconfigure` (load CanvasDump before drawing), keeps
  the `-dash` value that was passed in, and dumps that. `Style::GroupBorder` uses `'---'`.
- **Colours**: X11 names are not SVG/Qt names (X11 `gray` = #BEBEBE, `green` = #00FF00, `maroon` =
  #B03060, `purple` = #A020F0). `gui/draw/colors.py` resolves names itself. The Qt renderer (014)
  must use `colors.to_hex` and never `QColor(name)`. On the 24-bit Xvfb visual, `#abc` resolves to
  `#A0B0C0`, and `#aaaabbbbcccc` keeps the top byte of each channel. All 250 golden names match.
- The comparison normalises both sides: colours become hex, numbers become floats, options equal
  to the type's Tk default are dropped, and tags are compared as a set. So `outline='#000000'`
  matches an unset (default black) outline.
- `colors.py` lives in `seqsee/gui/draw/`, not in the test helper, because the Qt renderer needs it
  too. `gui_compare.py` uses it.
- Tests and implementation were written together in this item (it is test infrastructure). I
  checked that the comparison fails on changed ops (e.g. a wrong arrow or anchor shows up as an
  `opts` difference).
### Screenshots
- Perl: `python/docs/gui/perl/smoke.png`. I looked at it: the rendering is right (smooth red arrow,
  dashed and arrowed lines, a pale-green oval, a stippled grey rectangle with a thick border, a
  smooth green polygon outline, an `arc`-style arc, a yellow chord, the nw-anchored 20 px red label,
  and two centred lines of 10 px text). Note: a Tk outline of width 0 still draws 1 px.
- Qt: none yet. There is no ops→Qt renderer until item 014, which must render the `styled` case and
  compare it with this PNG.
### Blockers
- None.
### Next
- Item 001 (Snapshot I). Oracle conventions for drawing items: `use CanvasDump;` (before drawing),
  `use Themes::Std2;` before any `SGUI::*`, `record(name => ..., items => dump_canvas($c))`, and
  `save_png($mw, $c, perl_screen_path('name'))`. Python tests should use
  `gui_compare.assert_ops_match(ops, case["items"])`, with `ordered=False` where Perl draws in
  hash order.

## Iteration 2 — 2026-10-07 01:01
### Completed
- Item 001, Snapshot I:
  - `python/src/seqsee/gui/snapshot.py`: `take(object_ids=None)` returns a frozen `Snapshot` with
    `steps`, `current_runnable`, `debug`, `element_count`, `elements` and `groups` (frozen
    `ObjectSnap`s), `relations` (frozen `RelationSnap`s) and `bar_lines`, plus `obj(oid)` and
    `largest_group`. `ObjectSnap` has oid, is_element, index, mag, left, right, span, items (oids
    of `@$self`), strength, group_p, metonym_active, structure_string, starred_structure_string
    (only when the metonym is active), categories (names), category_kind (find_group_style's
    ascending → descending → sameness chain), categories_as_string, is_locked, bounds_string and
    hilit. `RelationSnap` has oid, ends (oids), strength, type_text, complexity, perl_class and hilit.
  - `python/tests/gui_recipes.py`: named deterministic workspace states: `empty`, `one_element`,
    `six_elements`, `twenty_elements`, `groups_relations` (a nested group, an ascending group, a
    sameness group with an active "each" metonym, relations inside/outside groups, a highlighted
    group and relation, two bar lines, 42 steps, a current runnable) and `large` (30 elements, 10
    groups, 20 relations). The drawing oracles of items 004–006 must build the same states
    under the same names in Perl.
  - `python/tests/test_gui_snapshot.py`, 14 tests: it matches the live model (elements, groups,
    metonyms, relations, globals, bar lines, oids, relation ends outside the workspace); it is
    frozen plain data all the way down, and hashable; it doesn't change when the model changes
    afterwards (strengths, metonym, locks, Hilit, steps, runnable, debug, bar lines, a removed
    relation, a deleted group, a new element); equal states give equal snapshots; taking it
    doesn't change the model; it is fast (I measured 0.34 ms for `large`; the test's bound is a
    5 ms median).
  - PORTING_MAP.md: a `done` row for gui/snapshot.py (no Perl module).
- I wrote the tests first, and they failed (ImportError) before snapshot.py existed. The full suite passes.

### Model reads of the Perl GUI (the list item 001 asks for)
Python names are in brackets. ✓ = in Snapshot I; the others belong to the item named.
- **SGUI/Workspace.pm** (004, 005) ✓: `$SWorkspace::ElementCount` [sworkspace.ElementCount],
  `SWorkspace->GetElements` [get_elements()], `SWorkspace->GetGroups` [get_groups(), longest span
  first; the first is drawn as "largest"], `values %SWorkspace::relations` [sworkspace.relations],
  `SWorkspace->GetBarLines` [get_bar_lines()]; element `get_mag`, `get_group_p`,
  `get_metonym_activeness`; group/element `@$self` (items), `get_edges`, `get_span`,
  `get_metonym_activeness`, `get_structure_string`, `GetEffectiveObject->get_structure_string`
  [get_effective_object()], `get_strength` (read by find_group_style, unused by Style::Group2),
  `is_of_category_p($S::ASCENDING/DESCENDING/SAMENESS)`; relation `get_ends`, `get_strength`;
  `%Global::Hilit` [Global.Hilit], `$Global::Feature{debug}`, `$Global::CurrentRunnableString`
  (through `SGUI::Coderack::family_to_name`, i.e. the package variable `$<family>::NAME`).
- **SGUI/Workspace_Attention.pm** (006): everything above ✓, plus `SCoderack->AttentionDistribution`
  [scoderack.attention_distribution()], keyed by element/group/relation. Its last-runnable text is
  the raw `$Global::CurrentRunnableString` ✓ (no family_to_name).
- **SGUI/Slipnet.pm** (007): `SLTM::GetTopConcepts(10)` [sltm.get_top_concepts()] →
  `[concept, activation, raw_activation, raw_significance]`, `$concept->as_text`.
- **SGUI/Coderack.pm** (008): `@SCoderack::CODELETS` (`[family, urgency, …]`) [scoderack.CODELETS],
  `$SCoderack::URGENCIES_SUM`, `%SCoderack::HistoryOfRunnable`, `$Seqsee::SCF::<family>::NAME`.
  NB: DrawIt *writes* `$SCoderack::HistoryOfRunnable{$_} ||= 0` for every family on the rack.
  That is a model mutation from the GUI side; the Python draw code must do it only on its
  snapshot copy.
- **SGUI/Stream.pm** (009): `$Global::MainStream->{CurrentThought}` [current_thought],
  `{OlderThoughts}` [older_thoughts], `{thought_hit_intensity}{$tht}`, `{hit_intensity}{$component}`;
  `$tht->as_text`, `$tht->stored_fringe` (`[component, activation]`).
- **SGUI/Relations.pm** (010): `values %SWorkspace::relations`, `UNIVERSAL::isa` Mapping::Numeric /
  Mapping::Structural on each relation, `get_ends`, `get_strength`, `get_type->get_complexity`,
  `get_type->as_text`, `$end->get_bounds_string`. All are ✓ (perl_class stands in for isa; 010 may
  need the ancestry).
- **SGUI/List/Groups.pm** (011): `SWorkspace->GetGroups`, `get_is_locked_against_deletion`,
  `get_strength`, `get_bounds_string`, `get_categories_as_string` ✓. The popup actions (020) call
  `SWorkspace::__DeleteGroup`, `set_is_locked_against_deletion`, `SLTM::FindActiveFollowers`,
  `history_as_text`, `get_fringe_for` and `SThought->create($group)->get_actions`.
- **SGUI/List/Categories.pm** (012): `GetGroups` sorted by span (rikeysort), `GetElements`,
  `get_edges`, `get_categories`, `$cat->get_name` ✓, `$SWorkspace::ElementCount` ✓. Actions:
  `is_of_category_p`, `__DeleteGroup`, `__ClearBarLines`, `__AddBarLines(get_left_edge)`,
  `__RemoveGroupsCrossingBarLines`. The list is keyed by category object, so 012 must extend
  the snapshot with category identities (two categories can share a name).
- **SGUI/List/Rules.pm** (012): `SRule->GetListOfSimpleRules`, `SRule->GetListOfCompoundRules`,
  `$rule->as_text`. Neither method exists in lib/SRule.pm (or in the port), so the Perl list
  should die when drawn. 012 must check this with the oracle.
- **SGUI/List/Stream.pm** (012): the MainStream fields above, `stored_fringe`, component `as_text`.
- **SGUI/List.pm** (011, 020): no model reads; a click sets `$Global::Break_Loop = 1`.
- **Tk/Seqsee.pm** (013): `$Global::Options_ref->{view}`; no other model reads.
- **Tk/SCodeletCount.pm** (016): `$Global::Steps_Finished` ✓.
- **Tk/SCommentary.pm** (017): toggles `$Global::debugMAX`.
- **UI/Graphical.pm** (017): `Seqsee::already_rejected_by_user`, `$Global::Feature{debug}` ✓; writes
  `$Global::AtLeastOneUserVerification` and `$Global::Break_Loop`.
- **SGUI.pm** (018): only writes: `SWorkspace->clear`, `SCoderack->clear`, `$Global::MainStream->clear`,
  `SWorkspace->insert_elements`.
- **Themes/Std2.pm, SColor.pm** (002): no model reads (pure functions of their arguments).

### Decisions
- Objects are identified by integer `oid`s, unique within one snapshot: elements (left to right),
  then groups (GetGroups order), then relations, then any other object something refers to (a
  group item or a relation end that is not in the workspace). `obj(oid)` is None for such outside
  objects. This reproduces Perl's failed `%AnchorsForRelations` lookup (the relation isn't drawn).
  `take(object_ids=d)` fills `d` with `{id(live): oid}` (used by tests; later, to map clicks back
  to model objects).
- `relations` keep `sworkspace.relations` insertion order, and `groups` keep get_groups() order.
  Perl uses hash order there, so drawing tests must compare as multisets where the order matters.
- `current_runnable` stores the raw `CurrentRunnableString`. SGUI::Workspace passes it through
  `family_to_name`, which reads `$Seqsee::SCF::<family>::NAME`. No codelet family in lib/ defines
  `$NAME` (only the SThought classes do), so the Workspace view's last-runnable text is probably
  always empty in Perl. Item 004 must confirm this with the oracle and port it as a PERL-QUIRK.
- The snapshot copies the values the model gives (perl_str'd strings, raw numbers). It contains no
  view logic except `category_kind`, a plain model query kept so the snapshot doesn't store
  category objects.
- Taking a snapshot only calls getters. One caveat: `MappingNumeric.get_complexity` fills its memo
  (as it does in Perl); no value changes. No model hook was needed.
- Noticed for item 006: Workspace_Attention.pm defines `SReln::draw_attention`, but relations are
  `SRelation` objects, so `$rel->draw_attention` should die in Perl ("Can't locate object method").
  The oracle must show what really happens.
- Noticed for item 007: the port's `get_top_concepts` returns `[concept, real, raw]` (3 fields,
  like Perl's dummy), so SGUI::Slipnet's `$raw_significance` is undef and `int(undef)` is 0.
- No screenshots: this item draws nothing.

### Blockers
- None.

### Next
- Item 002 (Colours & theme): SColor.pm and every `Style::*` in Themes/Std2.pm →
  `seqsee/gui/draw/theme.py`, with goldens from calling each Style function in Perl over a grid
  of arguments.

## Iteration 3 — 2026-10-07 01:12
### Completed
- Item 002, Colours & theme:
  - `python/oracle/gui_theme.pl` → `tests/golden/gui_theme.json`. Case `hsv`: 4960 calls to
    `SColor::HSV2RGB`/`HSV2Color` over a grid (32 h × 10 s × 16 v), with out-of-range values (h ≥ 360,
    negative h, s ≤ 0, v ≥ 100, negative v). Case `styles`: 172 calls covering all 15 `Style::*`
    functions, each result stored as ordered `[key, value]` pairs (undef → null). Case `arity`: wrong
    argument counts and an unknown style die. The oracle needs no canvas; it is named `gui_*` because
    it belongs to the GUI port, so it runs under Xvfb anyway.
  - `python/src/seqsee/gui/draw/theme.py`: `hsv2rgb`, `hsv2color` (alias `HSV`, lru-cached), a
    `Style` class with one static method per Perl function (`Style.Element(hilit)`,
    `Style.Group2(meto, category_name, is_largest)`, …), and `style(name, *args)`, which raises
    AttributeError("… Have you defined this style?") like Style::AUTOLOAD. Font constants
    `FONT_10/14/20/28`.
  - `python/tests/test_gui_theme.py` (21 tests): exact match of every HSV and Style call, the quirky
    colours, a check that every `sub Style::` in Std2.pm is ported, arity errors, styles splat into
    ops, and results are fresh dicts.
  - PORTING_MAP: the SColor.pm and Themes/Std2.pm rows are now `done`.
- I wrote the tests first and they failed (ImportError). Afterwards the only mismatch was Perl
  printing -0.0 as "0", a stringification difference in the test, not a value difference. The full
  suite passes.
### Decisions
- **API**: a Style result is a dict with plain keys (`fill`, not `-fill`), in Perl's order, with
  None for undef. Use it as `ops.text(x, y, text=t, **Style.Element(h))`; `ops.create` treats None
  as unset, the same as Tk does with undef. Every call returns a fresh dict (Perl memoizes, but a
  cached mutable dict would be shared); only `hsv2color` is cached.
- Truthiness follows Perl (`util.perl_true`): `Group2(meto, '0', 0)` gets the stipple, like undef
  and ''. `category_name` None is compared as ''.
- **PERL-QUIRKs** (confirmed by the oracle, ported in theme.py):
  - A channel `int(x*2.56)` outside 0..255 loses its high hex digit, because `$D2H{16+}` and
    `$D2H{-n}` are undef. So `HSV(…, 100)` = `#000`, v=120 gives `#333`, and NetActivation(110)
    gives `#0404F`. A negative channel whose `int(n/16)` is -0 keeps the digit "0".
  - h ≥ 360: `int(h/60)` = 6 matches no branch, so HSV2RGB returns `('')` and HSV2Color returns `'#00'`.
  - In the real GUI these cases need NetActivation raw_significance ≥ ~101 or < 0. The other styles
    stay in range for the arguments the SGUI modules pass.
- Std2.pm's `Style::Group` (strength-based colour) is unused by the drawings (Workspace uses
  `Group2`), except for SGUI::Workspace's colour-legend lists (lines 276–280). It is ported anyway.
### Screenshots
- None. This item draws nothing on its own, and there is no Qt renderer until 014. Items 004–009
  show the styles in use.
### Blockers
- None.
### Next
- Item 003 (Model runner): `seqsee/gui/runner.py` worker thread with snapshots, pause, and the
  question bridge.

## Iteration 4 — 2026-10-07 01:22
### Completed
- Item 003, Model runner:
  - `python/src/seqsee/gui/runner.py`: `Runner(QObject)` and `Question`. Commands, queued from the
    GUI thread and run one at a time on a daemon worker thread: `step()`, `step_n(n)`, `crawl(ms)`,
    `continue_()` (Seqsee.pl's Interaction_step / _step_n / _crawl / _continue, with the same `n`,
    `update_after`, `InterstepSleep` and `max_steps`), `new_sequence(seq, seed=, max_steps=,
    update_interval=)`, `call(fn, *args)` (any model action on the worker thread, run with deep
    stack; its result comes back in `command_done`), `pause()`, `answer(q, value)`, `quit(timeout)`.
    Signals: `snapshot(Snapshot)`, `question(Question)`, `question_closed(Question)`,
    `error(message, traceback)`, `command_done(name, result)`, `state_changed("idle" | "running" |
    "waiting" | "stopped")`. Properties: `options`, `pending_question`, `state`.
  - `python/tests/test_gui_runner.py` has 30 qtbot tests (about 3 s): snapshots arrive, on the GUI
    thread; step/step_n/crawl/continue; throttling (≤ 30/s + final + one per question); the model
    runs off the GUI thread; pause stops a crawl and a continue within one step and drops queued
    runs; a run after a pause goes on; boolean/response/more-terms round trips; the worker stays
    blocked until answered; a snapshot is sent before each question (it shows the hilit); a real
    `1 1 2 1 2 3` run with a yes-answerer asks and gets answers with no deadlock; model and `call`
    exceptions are signalled and the runner survives; quit while a question is pending or while
    running; hooks are installed and restored; new_sequence during a run replaces it and cancels a
    pending question; a bad sequence is an error; the same seed gives the same run; recursion
    headroom (20000 frames via `call`); Alternating `1 1 1 2 2 3 3 3 4 4 5 5 5 6 6` with seed 1 runs
    450 steps without error, and the test measures that FindMappingForCat reached a frame depth
    above 1000 (deeper than Python's default limit).
  - PORTING_MAP: a `done` row for gui/runner.py.
- I wrote the tests first, and they failed (ImportError) before runner.py existed. The full suite
  passes (5206 tests, ~2 min).
### Decisions
- **Model hooks, no model change**: the runner replaces module attributes the model already looks
  up at call time: `seqsee_main.seqsee_step` (wrapped), the `_update_display` of seqsee_main,
  sworkspace, all_mx and user_interaction, `all_mx._ask_for_more_terms`, and (through
  `user_interaction.install`) `boolean_response` and `response`. `start()` installs them and
  `quit()` restores the saved originals (only after the worker has stopped). `ask_user_extension`
  keeps its default (`gui_ask_user_extension`, UI/Graphical.pm's), which asks through
  `boolean_response`. Headless behaviour is unchanged.
- **Pause**: `Interaction_step_n` sets `Break_Loop = 0` before every step, so a GUI write of
  Break_Loop between two steps would be lost. `pause()` sets Break_Loop and a pause Event, and the
  `seqsee_step` wrapper sets Break_Loop = 1 again after each step while the Event is set. The Event
  is cleared (under the queue lock) when the next run command is taken, so pause can't be lost.
  Pause also drops queued run commands (not `call`/`new_sequence`). A pause waits for the current
  step to end: a single very deep step (the 90 s step 4005 of Alternating, seed 1) can't be
  interrupted.
- **Snapshots** are taken on the worker thread in the `update_display` hooks, at most one per
  `min_interval` (1/30 s by default). Every command ends with a forced snapshot, and one is forced
  before every question and after inserting more terms. A snapshot failure is reported with
  `error` and does not reach the model.
- **Questions**: `Question.kind` is `boolean` (text = the question; `extra` = the other arguments
  of MessageRequiringBooleanResponse, e.g. `('', msg_suffix, ['debug'])`), `response` (`choices`)
  or `more_terms` (text = SGUI::ask_for_more_terms' label). The worker blocks on an Event until
  `answer()`. A more-terms answer is the typed text: it is trimmed and split on `[,\s]+`, then
  `insert_elements` is called, as in the Perl Entry's <Return> handler. None means the window was
  closed (Perl's waitWindow returns and nothing is inserted). Empty fields are dropped (Perl's
  split would keep a leading empty field for input like ",4"). `new_sequence` and `quit` cancel a
  pending question: it is answered None, `cancelled` is set, and `question_closed` is emitted.
  After `quit`, new questions are answered None at once.
- **new_sequence** does the same setup as `cli.run_headless`: `s.load()`, `s.reset_all()`, hooks
  reinstalled (reset_all restores the default answer callbacks), `read_config(seq, seed?,
  max_steps?, update_interval?)` with stdout swallowed, `Global.Options_ref`, `util.srand(seed)`,
  and `cli._initialize` (Seqsee.pl's INITIALIZE without the display). So the GUI run with seed N
  follows the same trajectory as `python3 -m seqsee --seed N` with the same answers. Item 018 can
  add the lighter Perl SGUI::ask_seq reset (clear + insert) if it must be exact.
- Run commands before any sequence report `error("No sequence: …")`. `options` values are
  read_config's raw values (e.g. `max_steps` may be a string), as interaction_step_n expects.
- The worker is a plain `threading.Thread`, not a QThread. PySide6 queues signals emitted from a
  foreign thread to receivers on the GUI thread (the tests check this). Steps get their deep stack
  from `interaction_step_n`; `call` and INITIALIZE go through `util.call_with_deep_stack` too.
- Not bridged yet: the many `_message` hooks (main::message; with `$no_break` false Perl asks
  MessageRequiringAResponse(['continue'], …)). That belongs to item 017 (Commentary).
- No screenshots: this item draws nothing.
### Blockers
- None.
### Next
- Item 004 (Workspace I): the SGUI/Workspace.pm oracle `gui_workspace.pl` building the
  `gui_recipes.py` states (empty, one_element, six_elements, twenty_elements, several rectangle
  sizes) and `gui/draw/workspace.py`. Confirm with the oracle whether the last-runnable text is
  always empty (family_to_name, see iteration 2).

## Iteration 5 — 2026-10-07 01:35
### Completed
- Item 004, Workspace I:
  - `python/oracle/GuiRecipes.pm` (new): the Perl twin of `tests/gui_recipes.py`.
    `build_recipe($name)` builds the named states and `object_tags()` maps each stringified
    element to `obj<oid>`. It has empty, one_element, six_elements and twenty_elements, plus three
    new recipes that are also in gui_recipes.py: `bar_lines` (1 1 2 1 2 3; bar lines added as
    (3, 0) then 6), `hilit_debug` (Hilit 1/2/3 on elements 1/3/5, `Feature{debug}`, 42 steps,
    runnable `Seqsee::SCF::FocusOn`) and `runnable_thought` (4 5, runnable
    `SThought::Seqsee::Element`, bar line 1). groups_relations and large are not in the Perl
    module yet; item 005 adds them.
  - `oracle/CanvasDump.pm`: `dump_canvas($c, \%tag_map)` replaces mapped tags. It dies on any
    tag that still looks like a ref (`=HASH(0x…)`), so no address can reach a golden.
  - `python/oracle/gui_workspace.pl` → `tests/golden/gui_workspace.json`: 35 cases (7 recipes ×
    rects (0,0,780,450), (0,0,400,200), (50,30,600,300), (10,100,300,150), (0,0,30,30); the last
    has a negative effective size). It also writes three Perl screenshots.
  - `python/src/seqsee/gui/draw/workspace.py`: `Layout`/`LAYOUT` (GUI_ws3.conf), `setup(x, y, w,
    h)` → `Geometry` (Setup's lexicals), and `draw(snap, x, y, w, h)` → ops, in DrawIt's order
    (groups, elements, relations, bar lines, last runnable).
    `gui/draw/family.py`: `family_to_name` + `FAMILY_NAMES` (SGUI::Coderack's helper; item 008
    reuses it).
  - `python/tests/test_gui_workspace.py` (51 tests): every golden case matches. Other tests: the
    golden covers the item's states; the layout equals GUI_ws3.conf (parsed); setup geometry; the
    offset quirks; element tags; hilit fonts and debug labels; family_to_name; FAMILY_NAMES equals
    every `our $NAME` in lib/ and the port's SThought NAMEs; draw is pure and repeatable.
  - PORTING_MAP: the SGUI/Workspace.pm row is now `partial` (005 completes it).
- I wrote the tests first, and they failed (ImportError). The full suite passes (5257 tests,
  ~2 min).
### Decisions
- **Last-runnable label** (the question from iteration 2): the oracle confirms it. For
  `Seqsee::SCF::FocusOn` (and for '') the text is empty. Only an SThought package name gives a
  label (`SThought::Seqsee::Element` → "Focusing on a Single Element"). In a real run
  `CurrentRunnableString` is always `"Seqsee::SCF::" . family` (Seqsee.pm:103), and no codelet
  family defines `$NAME`, so the Workspace view's label is always empty. It is ported as is, with
  a PERL-QUIRK note on `draw_last_runnable`. family_to_name uses a static table, so the draw code
  never imports model classes; a test keeps the table in sync with lib/ and with the port.
- **PERL-QUIRKs** (confirmed by the golden offset cases): elements are at `$Margin + (0.5+i) *
  $SpacePerElement` and bar lines at `$Margin + index * $SpacePerElement`, both without
  `$XOffset` (groups do add it, item 005). The y positions do include `$YOffset`. The label is at
  `($Margin, $Height - $Margin)`, without either offset. This matters for the composite views
  (013), where the workspace isn't at (0, 0).
- **Object tags**: Perl tags an element's text with `[$self, 'element', $idx]`. The Python op
  tags are `("obj<oid>", "element", "<idx>")`, and the oracle maps each ref to the same
  `obj<oid>`. Tk keeps the integer-looking tag "0" as is. Items 015/020 can use these tags to
  map items back to snapshot objects.
- An undef or '' text leaves Tk's default empty text (`_text` helper), because `ops.create`
  turns '' into None, which is not the Text default "".
- `draw` raises NotImplementedError if the snapshot has groups or relations, until item 005.
  That is louder than drawing a half picture. Note for 005: with no groups, Perl's DrawGroups
  returns before its loop over elements with `get_group_p`/metonyms (`my @groups = … or return`),
  and so before `$Canvas->raise('hilit')` too.
### Screenshots
- Perl: `python/docs/gui/perl/workspace_hilit_debug.png`, `workspace_bar_lines.png` and
  `workspace_twenty.png` (780×450). I looked at the first two. The elements sit on the mid line,
  evenly spaced. Hilit 1 is green, Hilit 2 blue, and Hilit 1/2/3 are larger. The debug index
  labels are small, below and right of each element. The bar lines are thick black lines at
  index 0 (on the left margin, before the first element), between elements 3 and 4, and after
  the last element. The label is empty. Note that Xvfb has no 20/28 px adobe-helvetica X font,
  so Tk falls back to a smaller font (the elements look ~12 px). Qt will use the real sizes, so
  Qt text will look larger than these Perl PNGs.
- Qt: none yet. As in items 000–002, there is no ops→Qt renderer until item 014. 014 must render
  the gui_workspace goldens and compare them with these PNGs. The Python ops equal the Perl dump
  (±0.5 px, exact options) for all 35 cases.
### Blockers
- None.
### Next
- Item 005 (Workspace II): add `groups_relations` and `large` (and any nested/metonym states)
  to GuiRecipes.pm; port `Seqsee::Anchored::draw_ws3`, `find_group_style`, DrawMetonym
  (`$Canvas->bbox` of the text: needs text metrics, or golden bbox values), SRelation::draw_ws3 with
  `%RelationsToHide`/anchors, and `raise('hilit')`. Compare as multisets where Perl uses hash
  order (relations).

## Iteration 6 — 2026-10-07 01:52
### Completed
- Item 005, Workspace II:
  - `oracle/GuiRecipes.pm`: Perl twins of `groups_relations` and `large`, plus four new recipes
    (also added to `tests/gui_recipes.py`): `nested_groups` (three levels, A/B/C in S1 = (A B) in
    S2 = (S1 C), all with distinct spans; Hilit 1 on B, 3 on S2, 2 on the hidden relation S1→C;
    hidden and visible relations between groups and elements), `metonyms` (the largest group with
    an active "each" metonym, a group with an inactive one, an element with an active metonym that
    is also squinted (group_p), a squinted element, a descending group), `overlapping_relations`
    (overlapping arcs, a leftward one, group→element, one to an element outside the workspace,
    bar lines) and `squint_no_groups` (squinted/metonym elements but no groups). Helpers `_group`
    and `_reln` mirror the Python ones.
  - `oracle/gui_workspace.pl` now draws all 13 recipes × 5 rects (65 cases). Each case also has
    `metrics`: `[font, text, fontMeasure, linespace]` for each distinct text on the canvas (the font
    taken by name, as dump_canvas does). New Perl screenshots: `workspace_groups_relations`,
    `_nested_groups`, `_metonyms`, `_overlapping_relations`, `_large`.
  - `gui/draw/workspace.py`: `draw_groups` (largest with is_largest, the others, then squinted /
    metonym elements; then raise('hilit')), `draw_anchored` (Seqsee::Anchored::draw_ws3 with Group2
    and GroupBorder, RelationsToHide from neighbouring items, anchors), `draw_metonym`,
    `draw_relations`/`draw_relation` (hidden unless hilit, missing or x = 0 anchors skipped, the
    zenith point, arrowshape (8, 12, 10)). `text_bbox(x, y, text, font, measure)` reproduces Tk's
    ComputeTextBbox, and `approx_measure` is a pure-Python default measure. `draw(…, measure=)`.
    The NotImplementedError guards are gone.
  - `gui/draw/ops.py`: `create` swaps reversed oval/rectangle corners, like Tk does.
  - `tests/test_gui_workspace.py`: 65 golden cases (non-relation ops compared in order when
    every group span is distinct, otherwise as a multiset; relation lines always as a multiset),
    plus tests for draw order, the largest group and the hilit raise, hidden relations, the
    relation to an outside element, the no-groups quirk, a squinted element's anchor, the metonym
    texts and cross, `text_bbox` against probed Tk bboxes, the default measure, and the
    normalised ovals.
  - PORTING_MAP: the SGUI/Workspace.pm row is now `done`.
- I wrote the tests first, and they failed (missing `text_bbox`/`measure`, then the 30×30 cases).
  The full suite passes (5300 tests, ~2 min).
### Decisions
- **Text metrics**: DrawMetonym's cross lines come from `$Canvas->bbox` of a text, so the draw
  code takes a `measure(text, font) -> (width, linespace)` callback. Tk's bbox of a centred
  single-line text is `(R(x) - w//2 - 1, R(y) - ls//2, R(x) - w//2 + w + 1, R(y) - ls//2 + ls)`
  with R = floor(v + 0.5); the ±1 is the insert-cursor fudge. The golden test measures with the
  oracle's recorded X metrics (FONT_20 is 21 px linespace, '2' = 11 px wide on Xvfb). Item 014/015
  should pass a QFontMetrics-based measure, so the cross fits the Qt text. Note that this Tk rounds
  a few exact .5 positions down (99.5 → 99, 3.5 → 3, in runs of 5; probably a float error in its
  coordinate conversion). I couldn't reverse-engineer the rule. No golden case hits one, and the
  error would be 1 px.
- Measure fonts by name: `itemcget(-font)` returns a Tk::Font object whose `fontMeasure` gave
  different numbers (74 vs 94 px for "[2, 2, 2, 2]", linespace 23 vs 21). The by-name numbers
  match the drawn bbox.
- **Tk normalises oval/rectangle corners** (ComputeRectOvalBbox). With the 30×30 rect, the
  effective size is negative and the group ovals come out reversed; Tk's coords are swapped. This
  is now done in `ops.create`, for every view. Arcs are not normalised (not checked yet).
- **Hash order**: Perl's GetGroups is `rikeysort` by span over hash values, so equal-span groups
  (and the "largest" one among them) are in hash order. The recipes avoid ties where it matters,
  except `groups_relations` (asc and same both have span 3) and `large` (all span 3; is_largest
  makes no difference for ascending groups). Those are compared as multisets.
- `raise('hilit')` is applied to the ops drawn by DrawGroups (it moves them to the end of the list
  so far). On a shared canvas (item 013) Tk raises every 'hilit' item, also those of views drawn
  earlier; 013 must check whether any other view tags items 'hilit' (Workspace_Attention?).
- Relations hidden by `%RelationsToHide` are keyed by (oid, oid) pairs, not Perl's concatenated
  ref strings (which can't collide in practice).
- **PERL-QUIRK** (confirmed by `squint_no_groups`): with no groups, DrawGroups returns before the
  element loop, so squinted elements and element metonyms are not drawn. Relations then use the
  elements' own anchors (y − 10). With groups, a squinted element's anchor is the top of its oval,
  because the group pass sets it first and the element pass uses `||=`.
- DrawLegend is not ported (its call is commented out in DrawIt).
### Screenshots
- Perl: I looked at `workspace_metonyms.png`, `workspace_nested_groups.png` and
  `workspace_groups_relations.png`. They show the nested ovals with the largest at the back (green,
  no stipple), the stippled uncategorised S1, cyan ascending groups and blue sameness/metonym ovals.
  B has a dashed black border (Hilit 1) and S2 a thick blue one (Hilit 3), both on top. The relation
  arcs peak at the zenith line, and the highlighted relation is thicker and green. The relations
  between neighbours in a group are not drawn. Squinted elements have small ovals, and the arcs
  start at their tops. Each metonym shows the starred text (blue) above the crossed-out actual text.
  Visible quirk of the PNG pipeline: the PostScript text is smaller than the X font that sized the
  bbox, so the cross is larger than the text (e.g. around "5"). Qt will use one font for both.
- Qt: none yet, as in 000–004: there is no ops→Qt renderer until item 014, which must render these
  goldens (with a QFontMetrics measure) and compare them with the Perl PNGs.
### Blockers
- None.
### Next
- Item 006 (Workspace attention): SGUI/Workspace_Attention.pm. Extend the snapshot with
  `SCoderack->AttentionDistribution` values, and add attention to the GuiRecipes states (Perl
  and Python). Check with the oracle whether `$rel->draw_attention` dies (iteration 2: it is
  defined on SReln, not SRelation) and port what happens. Reuse `text_bbox`/`measure` if the view
  needs text sizes.

## Iteration 7 — 2026-10-07 02:00
### Completed
- Item 006, Workspace attention:
  - `oracle/GuiRecipes.pm` / `tests/gui_recipes.py`: `_init` now also clears the coderack
    (`SCoderack::clear` / `scoderack.clear`). A `_codelet(family, urgency, %args)` helper and three
    new recipes: `attention_elements` (codelets on elements and a string argument, urgencies sum to
    100: e0 0.8 so v is clamped to 99, e1 0.2, e2 0.05, e4 0.1, e3/e5 0; Hilit, debug, bar lines, a
    runnable), `attention_groups` (the nested groups of `nested_groups` plus a squinted metonym
    element, no relations; a FocusOn codelet spreads attention through the reader's distribution)
    and `attention_relations` (groups_relations + codelets on the reader, elements and a relation).
    Perl's `groups_relations` now returns its named objects, as the Python one did.
  - `oracle/gui_workspace_attention.pl` → `tests/golden/gui_workspace_attention.json`: 13 recipes ×
    the 5 rects (65 cases). Each case has `items`, `metrics`, `died` (DrawIt's die message without
    " at FILE line N.", or null) and `attention` (AttentionDistribution as `[label, value]`, labels
    `e<idx>` / `g<bounds><structure>` / `r<bounds>-><bounds>`). Perl screenshots
    `attention_elements`, `attention_groups`, `attention_relations`.
  - `gui/snapshot.py`: `ObjectSnap.attention` and `RelationSnap.attention` (default 0), from
    `scoderack.attention_distribution()`, looked up by `id()` (scalar keys dropped).
  - `gui/draw/workspace.py`: the drawer now has SGUI::Workspace's `find_element_style`,
    `find_group_style`, `find_group_border_style`, `find_relation_style`, `BAR_LINE_OPTIONS` and
    `last_runnable_text` as overridable methods (no behaviour change; all workspace goldens pass).
  - `gui/draw/workspace_attention.py` (new): `LAYOUT` ([Workspace_AttentionLayout]: bar lines 0.2),
    `draw(snap, x, y, w, h, layout, measure, relations_die=True)`. A subclass of the workspace drawer:
    black rectangle first, `Style::ElementAttention`/`GroupAttention`/`GroupBorderAttention`/
    `RelationAttention`, bar lines of width 1, the raw CurrentRunnableString as label.
  - `gui/draw/ops.py`: `DrawDied(message, ops)`.
  - `tests/test_gui_workspace_attention.py` (100 tests): all 65 golden cases (ops and die message);
    the snapshot's attention equals Perl's AttentionDistribution for every recipe (rel 1e-12); layout
    vs GUI_ws3.conf; zero attention without codelets; attention frozen in the snapshot; the black
    rectangle with offsets; element styles follow attention, not Hilit; thin bar lines and the raw
    label; hilit borders raised; the die and its partial ops; the `relations_die=False` path; purity.
  - PORTING_MAP: SGUI/Workspace_Attention.pm row `done`; the snapshot row mentions attention.
- I wrote the tests first; they failed (ImportError). The full suite passes.
### Decisions
- **PERL-QUIRK (confirmed by the oracle)**: `$rel->draw_attention` dies with `Can't locate object
  method "draw_attention" via package "SRelation"` (the method is SReln's). DrawRelations comes after
  DrawGroups and DrawElements, so the canvas keeps the black rectangle, groups and elements, and has no
  bar lines and no label. Every recipe with a relation dies (groups_relations, nested_groups,
  metonyms, squint_no_groups, attention_relations). It dies at the first relation, even a hidden one
  (the method lookup fails before the body). `draw` raises `ops.DrawDied`, whose `.ops` are exactly
  the Perl canvas. In Tk::Seqsee::Update the die is not caught, so the parts after this one are not
  drawn and the die leaves update_display. **Item 013 must port that** (in 'Workspace + Attention'
  the Workspace part above stays drawn), e.g. by catching DrawDied in `compose`, keeping the partial
  ops and stopping there. The modern shell (015/019) can offer `relations_die=False`, which runs
  SReln::draw_attention's body (the Workspace relation code with Style::RelationAttention, Tk's
  default arrowshape; hidden between neighbours unless hilit). It has no golden, since Perl never
  reaches it.
- Attention 0 is drawn black on the black rectangle (HSV v = 0): such elements, the debug labels,
  the bar lines (default black), the metonym cross lines and the label are invisible in Perl. Ported
  as is.
- The snapshot stores `AttentionDistribution->{$obj} || 0`. Taking it calls
  `scoderack.attention_distribution()`, which only reads the coderack and the workspace (it reads
  ReadHead but doesn't change it). No model hook was needed.
- `raise('hilit')` (see iteration 6's note for item 013): Workspace_Attention also tags highlighted
  group borders 'hilit', so on a shared canvas Tk's raise in one view also raises the other view's
  hilit items. Item 013 must handle this.
- Regenerating `gui_workspace.json` with the new GuiRecipes.pm only changed the order of hash-ordered
  items (equal-span groups, relations), which that test compares as multisets.
### Screenshots
- Perl: I looked at `attention_elements.png`, `attention_groups.png` and `attention_relations.png`.
  Each shows a black rectangle inside a white margin. Elements are pink/purple, brighter with more
  attention (e0 and e1 bright, e2 and e4 dim). e3 and e5 are invisible (attention 0), and so are the
  debug labels, bar lines and label. In attention_groups the largest group S2 (0.43, clamped to
  v 99) is a bright mint oval, S1 a dark green one, B (0.3) bright mint, and A and C nearly black;
  their element texts are black on mint. The squinted element 9 is a small mint oval with a pink
  "5". Its metonym shows a blue starred "5" above a crossed-out "5" (black cross lines, invisible).
  attention_relations shows only groups and elements (the die), and the metonym text "[2, 2, 2]" in
  dark grey.
- Qt: none yet, as in 000–005 (no ops→Qt renderer until item 014, which must render these goldens).
### Blockers
- None.
### Next
- Item 007 (Slipnet): SGUI/Slipnet.pm with `[SlipnetLayout]`. Extend the snapshot with
  `sltm.get_top_concepts(10)` (3 fields: raw_significance is undef, so 0; see iteration 2), add
  slipnet states (activations) to GuiRecipes, and use the `measure` callback if text widths matter.

## Iteration 8 — 2026-10-07 02:24
### Completed
- Item 007, Slipnet:
  - `oracle/GuiRecipes.pm` / `tests/gui_recipes.py`: an `_activate(index, value)` helper (sets the
    node's REAL_ACTIVATION directly in `@SLTM::ACTIVATIONS` / `sltm.ACTIVATIONS`) and four recipes:
    `slipnet_small` (elements, categories, the succ mapping and its dependencies; one node at exactly
    0.01, one left at the initial 0.003, one spiked by SpikeBy), `slipnet_full` (40 nodes, 32 above
    the threshold), `slipnet_long_text` (nested group platonics, one longer than 30 characters) and
    `slipnet_dir` (Mapping::Dir nodes, one hidden and one shown).
  - `oracle/gui_slipnet.pl` → `tests/golden/gui_slipnet.json`: 7 recipes (empty, one_element,
    groups_relations, and the 4 above) × the 5 rects = 35 cases. Each case has `items`, `died`, and
    `concepts` (GetTopConcepts as `[as_text or null, activation, raw, ref]`). Perl screenshots
    `slipnet_small`, `slipnet_full`, `slipnet_long_text`, `slipnet_dir`.
  - `gui/snapshot.py`: `ConceptSnap(text, activation, raw_activation, raw_significance=None,
    perl_class="")` and `Snapshot.slipnet` (default `()`), one per `sltm.get_top_concepts(10)` entry.
  - `gui/draw/slipnet.py` (new): `Layout`/`LAYOUT` ([Layout] Margin + [SlipnetLayout]) and
    `draw(snap, x, y, w, h, layout)`.
  - `tests/test_gui_slipnet.py` (46 tests): all 35 golden cases (ops and die message), the snapshot
    concepts equal Perl's for every recipe, the layout matches GUI_ws3.conf, the oval/text geometry,
    the strict threshold (skipped nodes take no row; undef activation), the columns and the fourth
    column, truncation, colour from raw_significance, the negative geometry of a tiny rect, the
    Mapping::Dir die, the snapshot stays frozen, and draw is pure.
  - PORTING_MAP: the SGUI/Slipnet.pm row is now `done`, and the snapshot row mentions the slipnet.
- I wrote the tests first; they failed (ImportError). The full suite passes (5442 tests, ~1:46).
### Decisions
- **PERL-QUIRK (confirmed by the oracle)**: DrawIt checks `last if $col >= $ColumnCount` at the
  top of the loop, before the row overflow moves to the next column. So when more than 30 nodes
  are shown, a 31st is drawn in a fourth column, outside the rectangle (at x ≈ 767 in the 780-wide
  canvas, so it is clipped), and then the loop stops.
- **PERL-QUIRK (found by the runner test, then confirmed by the oracle)**: Mapping::Dir has no
  `as_text`. When a Dir node is above the threshold, DrawNode dies (`Can't locate object method
  "as_text" via package "Mapping::Dir"`) after it creates the oval and before the text. `draw` raises
  `ops.DrawDied` with the ops drawn so far, as Workspace_Attention does. Hidden Dir nodes are never
  asked for their text. The snapshot stores `text=None` for a concept without `as_text`, plus
  `perl_class`. Before that fix, `snapshot.take()` raised AttributeError in real runs (the
  runner's throttling test failed after ~190 steps of `1 1 2 1 2 3`). **Item 013** must handle this
  die like the attention one (it is uncaught in Tk::Seqsee::Update). So in Perl, any view with the
  Slipnet stops drawing once a Dir mapping is active above 0.01.
- GetTopConcepts is a dummy: it returns every node in memory-index order (not the top 10 by
  activation) with 3 fields. raw_significance is undef, so `int(undef)` = 0 and every oval is
  `Style::NetActivation(0)` = `#A1A1E6`. The draw code still reads `raw_significance` (int() truncates)
  in case a model gives it.
- Setup's `int($EffectiveWidth / $ColumnCount)` truncates toward zero; Python's `int()` matches,
  including the negative sizes of the 30×30 rect (`//` would not).
- Text widths don't matter here (the text is cut by characters, not pixels), so no `measure`.
- The snapshot stores every node, not just those above the threshold, so the view's config stays in
  the drawing code. `take()` now calls `as_text` on every LTM node. The runner and snapshot speed
  tests still pass.
### Screenshots
- Perl: I looked at all four. `slipnet_small`: plat1, plat3, ascending (the largest), tiny dots for
  number and succ (activation ≈ 0.01, radius 0.2 px), and descending, in one column, all lavender with
  black outlines. plat2 (0.01) and sameness are absent. `slipnet_full`: three columns of 10, the
  ovals sized by activation, and the clipped 31st oval at the right edge (plat38, its label is off
  the canvas). `slipnet_long_text`: two columns; the last label is cut at
  "plat[[1, 2, 3, 4, 5, 6], [7, 8". `slipnet_dir`: plat4 and plat5 with labels, then a large oval
  with no label (the die).
- Qt: none yet, as in 000–006 (there is no ops→Qt renderer until item 014, which must render the
  gui_slipnet goldens and compare them with these PNGs). Expected difference: the PostScript text is
  small (the Xvfb fallback font); Qt will use the default Helvetica 12.
### Blockers
- None.
### Next
- Item 008 (Coderack): SGUI/Coderack.pm (`family_to_name` is already in gui/draw/family.py). Extend
  the snapshot with `scoderack.CODELETS` (family, urgency), `URGENCIES_SUM` and `HistoryOfRunnable`.
  DrawIt writes `HistoryOfRunnable{$_} ||= 0`; do that on a copy, never on the model. Add coderack
  recipes (the `_codelet` helper exists) and check whether the view dies anywhere, as Slipnet and
  Workspace_Attention do.

## Iteration 9 — 2026-10-07 02:34
### Completed
- Item 008, Coderack:
  - `oracle/GuiRecipes.pm` / `tests/gui_recipes.py`: a `_history(family => count, …)` helper (it
    writes `HistoryOfRunnable{"Seqsee::SCF::<family>"}` directly) and four recipes:
    `coderack_small` (5 codelets of 4 families, urgencies sum 120; history Reader 12, FocusOn 6,
    AttemptExtension 2 (not on the rack); AreRelated and Bigger are on the rack but not in the
    history), `coderack_zero_urgency` (urgencies all 0, no history), `coderack_history_only` (no rack,
    a history with one 0 count) and `coderack_many` (25 codelets, the rack's maximum, plus history:
    55 families).
  - `oracle/gui_coderack.pl` → `tests/golden/gui_coderack.json`: 7 recipes (empty, one_element,
    attention_elements and the 4 above) × the 5 rects, plus 2 `redraw` cases (37 cases in all). A
    redraw case draws coderack_small once, replaces the rack with one Mid codelet, then dumps a
    second draw. Each case records `items`, `died` (always null), `codelets`, `urgencies_sum`,
    `history` (before the draw, sorted) and `drawn` (%HistoryOfRunnable after the draw, in hash
    order, which is the row order). Perl screenshots: `coderack_small`, `coderack_many` and
    `coderack_zero_urgency`.
  - `gui/snapshot.py`: `CoderackSnap(codelets=((family, urgency), …), urgencies_sum, history=((key,
    count), …))`, stored as `Snapshot.coderack` (default empty).
  - `gui/draw/coderack.py` (new): `Layout`/`LAYOUT` ([Layout] Margin + [CoderackLayout]),
    `draw(snap, x, y, w, h, layout, known_families=())`, `draw_layers` (returns the lowered stripes
    and the other ops separately), `rows(coderack, known_families)` and `rack_families(snap)`. It
    reuses `family.family_to_name`.
  - `tests/test_gui_coderack.py` (85 tests): all 37 golden cases (ops compared in order, with the
    snapshot's history put in Perl's hash order); for every case, the snapshot equals Perl's
    CODELETS / URGENCIES_SUM / history, and Python's row set equals Perl's drawn keys. Other tests:
    the layout vs GUI_ws3.conf, the headers above the rect, the geometry of one row, the zero
    urgency sum without history, the row/column overflow, the stripe order, the rows added from the
    rack and from known families, that draw never writes the model, a frozen snapshot, and purity.
  - PORTING_MAP: the SGUI/Coderack.pm row is now `done`; the snapshot row mentions the coderack.
- I wrote the tests first; they failed (ImportError). The full suite passes.
### Decisions
- **Hash order**: DrawIt walks `%HistoryOfRunnable` with `each`, so row positions follow Perl's
  hash order. Python can't reproduce it, so the golden records the walk order (`drawn`). The golden
  test puts the snapshot's history in that order (with `dataclasses.replace`) and compares the ops
  in order. A separate check shows that Python's own rows (history order, then the known families,
  then the rack's) are the same set of (key, count) pairs. `each` can stop early (`last`), but that
  does not affect the next draw: `values %HistoryOfRunnable` (the sum) resets the iterator first.
- **No model write**: DrawIt's `$HistoryOfRunnable{$_} ||= 0` stays in the Perl model, so a family
  that was ever on the rack keeps a 0-run row until SCoderack->clear (the redraw golden shows
  this). The port adds those rows only to its own row list. A widget that wants Perl's persistent
  rows collects `rack_families(snap)` at each draw and passes them as `known_families` (items
  015/016/019 should do this, and reset the set on new_sequence). No model hook was needed, and
  nothing else in the model reads HistoryOfRunnable.
- **PERL-QUIRKs** (all confirmed by the dump and the PNGs):
  - The name column is always empty (no codelet family defines `$NAME`; see iteration 5).
  - The headers are at `YOffset - 10`, so at y = 0 they are off the canvas.
  - `$rows_displayed > $MaxRows` gives 16 rows per column, and `$current_column > $MaxColumns`
    draws a third column right of the rectangle (off a 780-wide canvas). That is at most 48 rows.
  - The stripes ignore the Margin: a stripe for row r spans YOffset + (2+r)·RowHeight − 3 to
    YOffset + (4+r)·RowHeight − 3, so it sits about one row below its rows.
  - With URGENCIES_SUM 0 the sums become `'---'`, which is 0 as a number: zero-width blue bars.
- **Item 013**: `$Canvas->lower($id)` puts each stripe below every item on the whole canvas, not just
  below this view's items. `draw_layers` returns the stripes apart (latest first, as Tk stacks them),
  so `compose` can put them at the bottom of the composite list.
- The view never dies (`died` is null in all 37 cases).
### Screenshots
- Perl: I looked at all three. `coderack_small`: five rows. Counts 2/1/1/1/(empty), blue urgency bars
  (Bigger the widest), tick marks at 50 px, red run-share bars for Reader (12), FocusOn (6) and
  AttemptExtension (2), and run counts. Pale green stripes cover rows 1–2 and the empty area below
  row 4. No headers and no names are visible. `coderack_many`: two visible columns of 16 rows
  each (the third is off the canvas), striped every four rows, with thin bars.
  `coderack_zero_urgency`: two rows with counts 2 and 1, zero-width blue bars, no red bars, and
  run counts 0.
- Qt: none yet, as in 000–007 (no ops→Qt renderer until item 014, which must render the
  gui_coderack goldens and compare them with these PNGs). Expected difference: the PostScript text
  is small (the Xvfb fallback font); Qt will use Helvetica 12.
### Blockers
- None.
### Next
- Item 009 (Stream): SGUI/Stream.pm (`DrawThought`, ThoughtBox/ThoughtComponent styles, fringe).
  Extend the snapshot with `Global.MainStream` (current_thought, older_thoughts,
  thought_hit_intensity, hit_intensity, each thought's as_text and stored_fringe). Add stream recipes
  (thoughts with fringes) to GuiRecipes, and check with the oracle whether the view dies anywhere.

## Iteration 10 — 2026-10-07 02:50
### Completed
- Item 009, Stream:
  - `oracle/GuiRecipes.pm` / `tests/gui_recipes.py`: `_init` now also clears the main stream
    (`clear`, plus hit_intensity and thought_hit_intensity, which `clear` leaves alone). Helpers
    `_stream(current, @older)` (sets CurrentThought / OlderThoughts / OlderThoughtCount / ThoughtsSet
    directly), `_thought_hits` and `_component_hits`. Four recipes: `stream_current` (one current
    element thought with its real get_fringe, hit undef), `stream_small` (a current thought and four
    older ones of the four thought classes; fringes of 4 (3 drawn: a string, a number, an SInt, then
    a category), 1, `[]`, undef and 2 (a float, a string) components; thought hits 100, 500, 2500,
    undef, 0; two component hits), `stream_full` (no current; 12 older thoughts plus a '' entry) and
    `stream_real` (real fringes of a group, a relation, a category and an element). Thoughts are
    built with `->new(core => …)`, not the memoized `SThought->create` (a memo hit on a reused
    address would give a thought whose weak core is gone).
  - `oracle/gui_stream.pl` → `tests/golden/gui_stream.json`: 6 recipes (empty, groups_relations and
    the 4 above) × the 5 rects = 30 cases. Each case has `items`, `died` (always null) and `stream`
    (the current and older thoughts: as_text, thought hit, fringe as `[text, activation, hit]`).
    Every address in a text is replaced by `ADDR`. Perl screenshots `stream_small`, `stream_full`,
    `stream_real`.
  - `gui/snapshot.py`: `ComponentSnap(text, activation, hit_intensity)`, `ThoughtSnap(text,
    hit_intensity, fringe, perl_class)`, `StreamSnap(current, older)` as `Snapshot.stream`, and
    `perl_string(x)` (Perl's `"$x"`). False stream entries / fringe entries are stored as None; a
    false stored_fringe as `fringe=None`; the whole fringe is kept (the view draws 3).
  - `gui/draw/stream.py` (new): `Layout`/`LAYOUT` ([Layout] Margin + [StreamLayout]) and
    `draw(snap, x, y, w, h, layout)`.
  - `gui/draw/theme.py`: `Style.ThoughtBox` numifies its hit intensity (`perl_num`), so undef is 0
    as in Perl (it raised TypeError on None before). The theme goldens still pass.
  - `tests/test_gui_stream.py` (49 tests): all 30 golden cases (ops in order, ref texts normalised);
    the snapshot's stream equals Perl's for every recipe; the layout vs GUI_ws3.conf; the geometry of
    one thought; the older-thought grid (from row 1, past ColumnCount); false entries take no row;
    box colours (undef, clamp); fringe undef / [] / a false entry; empty texts; perl_string; hit
    intensities; the snapshot is frozen; draw doesn't touch the model; purity.
  - PORTING_MAP: the SGUI/Stream.pm row is now `done`; the snapshot row mentions the stream.
- I wrote the tests first; they failed (ImportError). The full suite passes (5576 tests, ~2 min).
### Decisions
- **Ref strings (PERL-QUIRK, confirmed by the oracle)**: DrawThought draws `-text => $component`,
  and no fringe component class overloads `""` except SInt (`SInt(5)`). So Perl shows
  `SLTM::Platonic=SCALAR(0x…)`, `SCategory::Ascending=HASH(0x…)`, `Mapping::Numeric=HASH(0x…)`,
  `Seqsee::Element=HASH(0x…)`; only strings (absolute_position_N) and numbers are readable. Ported:
  `snapshot.perl_string` gives `Class=HASH(0x<id>)` (perl_name + Python id). The port always says
  HASH (Class::Std objects such as SLTM::Platonic are SCALAR refs in Perl), so the tests compare
  `Class=REF` on both sides. The modern shell (019) could offer as_text instead; no golden for that.
- **Geometry (PERL-QUIRKs, in the dump and the PNGs)**: the current thought is at (XOffset +
  Margin/2, YOffset), i.e. on the top edge and half outside the margin, and its box is drawn first,
  so the older boxes overlap it. The older grid starts at row 1 (row++ before the first draw) and
  is not bounded by ColumnCount: the 12th older thought of `stream_full` is in a fifth column at
  x = 760, clipped by the 780-wide canvas. In a real run MaxOlderThoughts is 10 (column 3, row 1:
  inside), so this needs '' entries.
- Style::ThoughtComponent ignores both arguments: every component is `#3314CC`, font 10. The
  snapshot still records each component's hit intensity (`hit_intensity{_key(comp)}`), which a
  later view could use.
- thought_hit_intensity is only filled for thoughts hit by the last `_is_there_a_hit` (cleared on
  every add_thought), so most boxes have undef (v 90, the palest colour).
- **Known divergence (not testable deterministically)**: SThought's core is a Moose `weak_ref`. In
  Perl, a thought whose group was deleted and freed has an undef core, and DrawThought's
  `$tht->as_text` would die. The port's thoughts hold strong refs, so it draws "Group …" for such a
  thought. No hook was added.
- The view never dies in the 30 cases (`died` null), unlike Slipnet and Workspace_Attention.
- No model hook was needed; taking the snapshot only calls `as_text`, `stored_fringe` and dict gets.
### Screenshots
- Perl: I looked at all three. `stream_small`: the current thought's pale-green box with a thick
  black border at the top left, with "Element Seqsee::Element:[3,3] 4" and the blue components
  absolute_position_3, 4, SInt(5) (the 4th not drawn). Column 0 rows 1–2: the group thought (mid
  green, "SCategory::Ascending=HASH(0x…)") and the relation thought (dark green: hit 2500 clamped
  to v 50, no components). Column 1 rows 0–1: "Category ascending" (pale, undef hit, no fringe) and
  the element thought with 0.5 and x. Long heads are covered by the next column's box ("Group
  Seqsee::Anchored <0, 2> [1, 2,"). `stream_full`: an empty top-left cell (no current thought), then
  12 boxes darkening with the hit intensity, column-wise from row 1; the 12th is a sliver at the
  right edge. `stream_real`: every drawn component is a ref string (platonics, categories, the mapping,
  elements; the element thought's absolute positions are components 4–6, not drawn); the element
  thought (hit 1999) is dark green.
- Qt: none yet, as in 000–008 (no ops→Qt renderer until item 014, which must render the
  gui_stream goldens and compare them with these PNGs). Expected differences: the Xvfb fallback
  font makes the PostScript text small; Qt will use Helvetica 14/10. The Perl PNGs show real
  addresses, Qt will show Python ids.
### Blockers
- None.
### Next
- Item 010 (Relations pane): SGUI/Relations.pm (rows: strength, simplicity, ends, type). The
  snapshot's RelationSnap has ends, strength, type_text, complexity and perl_class; check whether
  the view needs the isa ancestry (Mapping::Numeric / Mapping::Structural) beyond `perl_class`, and
  whether it dies anywhere (oracle). Relations are in hash order: compare as multisets, or record
  Perl's walk order as the coderack golden does.

## Iteration 11 — 2026-10-07 02:57
### Completed
- Item 010, Relations pane:
  - `oracle/GuiRecipes.pm` / `tests/gui_recipes.py`: `_reln` takes an optional category (default
    NUMBER); a new `_struct_reln(first, second)` helper (an SRelation::Structural whose type is the
    `[ascending] start => succ` Mapping::Structural of the srelation oracle). Two recipes:
    `relations_pane` (8 elements, groups A (1 2 3) and B (4 5 6); succ/pred/same on NUMBER, succ on
    EVEN, the structural relation A→B, A→e6 and e7→an element outside the workspace; strengths set
    after insert: 0, 5.555, 100, 123456.789, -3.14159, 0.005, 42) and `relations_many` (27
    elements, 26 relations, more than RowCount = 22 rows, strength 3.7 i).
  - `oracle/gui_relations.pl` → `tests/golden/gui_relations.json`: 7 recipes (empty, one_element,
    groups_relations, overlapping_relations, large and the two above) × the 5 rects = 35 cases.
    Each case has `items`, `died` (always null), `compound` (always 0) and `relations`: the rows in
    DrawIt's walk order (ends' bounds strings, strength, complexity, type as_text, class,
    isa_numeric, isa_structural). Perl screenshots `relations_pane`, `relations_many`,
    `relations_groups_relations`.
  - `gui/snapshot.py`: `RelationSnap.end_bounds` (both ends' get_bounds_string, also for ends
    outside the workspace, which have no ObjectSnap), `isa_numeric` / `isa_structural`
    (`multimethods.perl_isa`, i.e. UNIVERSAL::isa by Perl class name). The defaults keep old
    constructors working.
  - `gui/draw/relations.py` (new): `Layout`/`LAYOUT`, `fmt` (`%5.2f`, undef → 0, Perl's Inf/NaN),
    `rows(snap)` (`@compound, @simple`), `draw_layers` (lowered stripes, texts) and `draw`.
  - `tests/test_gui_relations.py` (54 tests): all 35 golden cases (ops in order, after putting the
    snapshot's relations in Perl's walk order); the snapshot's rows equal Perl's for every recipe
    (strength, complexity, bounds, type text, class, isa flags); golden coverage; the layout vs
    GUI_ws3.conf; one row's geometry with offsets; stripe order; rows overflowing the rect; fmt;
    the compound/structural lists; no relations; outside-end bounds; purity; frozen snapshot.
  - PORTING_MAP: the SGUI/Relations.pm row is `done`; the snapshot row mentions item 010.
- I wrote the tests first; they failed (ImportError). The full suite passes.
### Decisions
- **PERL-QUIRK (confirmed by the oracle: `compound` is 0 and both isa flags are 0 in every case)**:
  `UNIVERSAL::isa($_, 'Mapping::Numeric' / 'Mapping::Structural')` is tested on the relations,
  which are SRelation / SRelation::Structural objects, never Mappings (it was presumably meant for
  their types). So the compound list is always empty and every relation, structural ones included,
  is drawn once. The port keeps both lists (`rows`), and the snapshot records the real isa
  answers, so it would still behave like Perl if that changed.
- **PERL-QUIRK**: no row limit. relations_many's rows 23–26 are drawn below the 450 px canvas.
  `row_height` keeps its fraction (no int() in Setup, unlike the other views); with the 30×30 rect
  it is negative, so rows go upwards and Tk swaps the stripe corners (ops.create already does).
- **Hash order**: rows follow `values %SWorkspace::relations`. As with the coderack, the golden
  records the walk order and the test reorders the snapshot's relations to match it (rows are
  matched by the ends' bounds, type text and class), then compares the ops in order.
- `ystart` is YOffset + Margin + 10. The stripes run from x + Margin to x + w − Margin, starting
  at the row's top. Every even row (0, 2, …) gets one, each lowered below the whole canvas
  (`draw_layers` returns them apart for item 013, latest first).
- Complexities agree between Perl and the port, including the EVEN succ mapping, which is 0.1
  (not the 0.4 the source suggests); the snapshot just reads get_complexity.
- An element outside the workspace still has a bounds string (" <9, 9> " for
  Element->create(9, 9)), so its row is drawn, unlike its arc in the Workspace view.
- No model hook was needed. The view never dies.
- GuiRecipes.pm changed (new optional `_reln` argument, new recipes); the other goldens were not
  regenerated (the existing recipes build the same states).
### Screenshots
- Perl: I looked at `relations_pane.png` and `relations_many.png`. relations_pane: seven rows of
  small text in five columns (strength, complexity, end 1, end 2, type), e.g. "100.00 0.00 <6, 6>
  <7, 7> same", "-3.14 0.20 <0, 2> <3, 5> [ascending] start => succ", "123456.79", " 5.55" (5.555
  rounds down in binary) and " 0.01". Pale green stripes sit behind rows 1, 3, 5 and 7, across the
  inside of the margins. relations_many: 23 rows visible down to the canvas bottom, the rest cut
  off, in hash order (18.50, 88.80, 48.10, …).
- Qt: none yet, as in 000–009 (there is no ops→Qt renderer until item 014, which must render the
  gui_relations goldens and compare them with these PNGs). Expected difference: Qt will draw the
  9 px font at its real size; the PostScript text looks smaller.
### Blockers
- None.
### Next
- Item 011 (List base + Groups list): SGUI/List.pm (paging, bookkeeping, `GetEntriesOnCurrentPage`)
  and SGUI/List/Groups.pm. The snapshot already has each group's is_locked, strength,
  bounds_string and categories_as_string. GetGroups' rikeysort order puts equal-span groups in hash
  order (record Perl's order in the golden, as here). Check with the oracle whether the list
  draws on a canvas and whether it dies anywhere.

## Iteration 12 — 2026-10-07 03:17
### Completed
- Item 011, List base + Groups list:
  - `oracle/GuiRecipes.pm` / `tests/gui_recipes.py`: two recipes. `groups_list` (A ascending
    (1 2 3), locked, strength 55.555; B ascending (4 5 6) plus EVEN with empty bindings, so two
    categories, strength 0; C sameness (7 7 7), locked, 100; D = (A B), no category, 99.999) and
    `groups_many` (60 elements, 30 disjoint pair groups, strength 3.3 i, every 7th locked).
  - `oracle/gui_list_groups.pl` → `tests/golden/gui_list_groups.json`: 61 cases. 7 recipes (empty,
    one_element, groups_relations, large, nested_groups and the 2 above) × 6 rects (the usual 5 plus
    200×10) on page 0; groups_many on pages 1, 2, 3 and 99 and as a page-up redraw (PageNumber++,
    Clear, DrawIt) in three rects; large on pages 1 and 5; empty on page 3; a groups_list redraw.
    Each case records `items`, `died`, the list's PageNumber / EntriesShownFrom / EntriesShownTo /
    EntriesCount after DrawIt, and the groups in GetGroups' order (tag `group<K>`, bounds,
    strength, categories, locked). The list object's tags are mapped to `SGUI::List::Groups[-…]`.
    Perl screenshots: `list_groups`, `list_groups_many`, `list_groups_many_page3`.
  - `gui/draw/lists/__init__.py` (new; the SGUI::List base): `MARGIN`, `ListState(page_number,
    shown_from, shown_to, entries_count, entries)`, `entries_per_page`, `perl_slice`,
    `current_page` (GetEntriesOnCurrentPage), `bookkeeping_text`, `draw_bookkeeping` and
    `draw_list(entries, draw_one, x, y, w, h, name, height_per_row, page, tag_of)` → (lowered bars,
    others, state).
  - `gui/draw/lists/groups.py` (new): `Layout`/`LAYOUT` (new's fields), `draw_one`, `draw_layers`
    (→ lowered, others, ListState) and `draw(snap, x, y, w, h, page=0)`.
  - `tests/test_gui_list_groups.py` (84 tests): all 61 golden cases (ops in order and the paging
    state, after putting the snapshot's groups in Perl's order and mapping `group<K>` to
    `obj<oid>`); the snapshot's groups equal Perl's; golden coverage; the layout; entries_per_page
    truncation; normal / clamped / empty / zero / negative page sizes; perl_slice; bookkeeping
    texts; one row's geometry and tags; alternating, lowered bars; empty texts; undef strength; an
    undef slot dies after its bar; the page round trip; draw doesn't touch the model.
  - PORTING_MAP: SGUI/List.pm and SGUI/List/Groups.pm are `partial` (item 020 adds clicks, paging
    controls and the popup ActionButtons). The other list rows now point at `gui/draw/lists/`.
- I wrote the tests first; they failed (ImportError). The full suite passes (5714 tests, ~2 min).
### Decisions
- **Page state**: the page number is the widget's state. `draw_layers(…, page=n)` returns a
  `ListState` with the page Perl leaves in `$self->{PageNumber}` (a page past the last is clamped
  to the last; with no entries the page is left as it is). Page up = `state.page_number + 1`, page
  down = `- 1` unless 0, and both redraw (item 020). The top red square is `-pagedown` (back), the
  bottom one `-pageup` (forward), as in Perl.
- **Tags**: Perl tags with the stringified list (`"$self"`) and entry. The port uses `name`
  (default `SGUI::List::Groups`; the oracle maps `"$self"` to it) and `obj<oid>` for a group.
  Every item of a row has `(name, obj<oid>, name-Clickable-Item)`; the bookkeeping text has
  `(name)`, the squares `(name-pagedown|pageup, name)`.
- **PERL-QUIRKs (all confirmed by the oracle)**:
  - EntriesPerPage = `int((h − 40)/15)`. With h < 55 it is 0 and GetEntriesOnCurrentPage dies
    "Illegal division by zero" before anything is drawn (the 30×30 cases: empty canvas). With
    h < 40 it is negative: 200×10 gives −2, and int() plus negative slice bounds pick odd pages
    (groups_many: PageNumber −14, entries 28–29, "Page #-13. Entries 29-30 of 30 Shown.").
  - For groups_relations / groups_list in 200×10, page_count is 0, so EntriesShownFrom/To are
    `'--'` but EntriesCount is not 0: the text is "Page #1. Entries 1-1 of 3 Shown." ('--' + 1).
  - The categories column shows stringified category refs (`SCategory::Ascending=HASH(0x…)`, joined
    by ', ' in hash order), since `category_list_as_strings` is the hash's keys. The snapshot keeps
    the port's `perl_ref_string` text; the tests compare without addresses and with the parts
    sorted.
  - An undef slot (a slice past the end, not reached by any golden case) dies in DrawOneItem after
    its bar is drawn; `draw_one(None)` raises DrawDied, and `draw_list` passes the canvas's ops.
- `fmt` (`%5.2f`, undef → 0) is reused from `relations.py`. 55.555 gives "55.55" in both.
- GetGroups puts equal spans in hash order, and every rebuild changes it, so each case records
  its own order (as for relations).
- No model hook was needed. No list ever dies except the two quirks above.
### Screenshots
- Perl: I looked at all three. `list_groups`: four rows on alternating pale yellow / pale peach
  bars across the margins: "100.00 <0, 5>" (D, no categories), "L 55.55 <0, 2>
  SCategory::Ascending=HASH(0x…)", "L 100.00 <6, 8> SCategory::Sameness=HASH(0x…)", " 0.00 <3, 5>
  SCategory::Ascending=HASH(0x…), SCategory::Even=HASH(0x…)"; the "L" is in the left margin.
  "Page #1. Entries 1-4 of 4 Shown." at the bottom left and red squares at the top and bottom
  right. `list_groups_many`: 27 striped rows (shuffled strengths, locked ones with "L"), "Page #1.
  Entries 1-27 of 30 Shown.". `list_groups_many_page3` (400×200, page 2): 10 rows, "Page #3.
  Entries 21-30 of 30 Shown.". The bottom square sits on the bookkeeping text's line.
- Qt: none yet, as in 000–010 (no ops→Qt renderer until item 014, which must render the
  gui_list_groups goldens and compare them with these PNGs). Expected differences: Qt will draw
  the 9 px and Helvetica 12 fonts at their real sizes (the PostScript text looks smaller), and
  show Python ids in the category refs.
### Blockers
- None.
### Next
- Item 012 (Categories, Rules & Stream lists): SGUI/List/Categories.pm (`MarkDescendentsToKeep`),
  List/Rules.pm, List/Stream.pm, as `gui/draw/lists/categories.py`, `rules.py`, `stream.py` on top
  of `lists.draw_list`. Categories needs category identities in the snapshot (iteration 2). Rules
  calls SRule->GetListOfSimpleRules / GetListOfCompoundRules, which don't exist: confirm with the
  oracle that it dies (and where: GetItemList runs inside GetEntriesOnCurrentPage, before anything
  is drawn). Reuse the `group<K>`-style tag mapping in the oracle.

## Iteration 13 — 2026-10-07 03:30
### Completed
- Item 012, Categories, Rules & Stream lists:
  - `oracle/GuiRecipes.pm` / `tests/gui_recipes.py`: three recipes. `categories_many` (28
    elements, 14 pair groups, each with one of ascending, descending, mountain, sameness, prime,
    odd, even, interlaced 2..8 added with empty bindings, G0 also descending, BIG = (G0 G1)
    ascending: 15 categories, more than one 780×450 page), `stream_list_many` (a current thought
    with 4 components and 14 older thoughts: no fringe / empty fringe / two components, hit
    intensities with ties, undef and 2.5) and `stream_hole` (a '' in OlderThoughts).
  - Three oracles and goldens: `gui_list_categories` (59 cases: 8 recipes × 6 rects, paging on
    pages 1/3/99 and redraws, a 1-per-page rect), `gui_list_stream` (50 cases: 7 recipes × 6
    rects, paging and redraws) and `gui_list_rules` (26 cases: the real list for 2 recipes × 6
    rects, then fake rules installed in the oracle × 6 rects plus paging and redraws). The
    categories oracle wraps GetItemList and SWorkspace::GetGroups to record the order DrawIt used,
    and %Cat2Objects. Perl screenshots: `list_categories`, `list_categories_many`,
    `list_stream_small`, `list_stream_many`, `list_stream_real`, `list_rules_fake`.
  - `gui/snapshot.py`: `CategorySnap(cid, name)`, `Snapshot.categories` (first seen over the
    groups, then the elements) and `Snapshot.category(cid)`, `ObjectSnap.category_ids`,
    `ComponentSnap.label` (as_text if the component has one, else "$component"). Defaults keep
    old constructors working. Groups are now snapshotted before elements (oids are unchanged:
    they are assigned up front).
  - `gui/draw/lists/__init__.py`: `Untagged` (a DrawOneItem result whose items get no tags) and
    `survivors(ops, name)` (what Clear leaves).
  - `gui/draw/lists/categories.py` (new): `Layout`, `CategoryEntry`, `item_list`, `draw_one`,
    `draw_layers`, `draw`, `mark_descendents_to_keep(snap, oid)`, `groups_to_keep(snap, cid)`
    (DeleteAllOther's %Keep, for item 020).
  - `gui/draw/lists/stream.py` (new): `Layout`, `item_list`, `tag_of`, `fringe_string`,
    `draw_one`, `draw_layers`, `draw`.
  - `gui/draw/lists/rules.py` (new): `NO_RULES`, `item_list` (dies), `draw_one`,
    `draw_layers(..., rules=None)`, `draw`.
  - Tests: `test_gui_list_categories.py`, `test_gui_list_stream.py`, `test_gui_list_rules.py`
    (all golden cases, in order, after putting the snapshot in Perl's order; the item lists and
    the snapshot match Perl; coverage; layouts; geometry; edge cases such as an undef category,
    no elements, a hole, untagged rows, undef hits/activations; MarkDescendentsToKeep).
  - PORTING_MAP: List/Categories.pm `partial` (item 020 adds the actions), List/Rules.pm and
    List/Stream.pm `done`; the List.pm and snapshot rows mention item 012.
- I wrote the tests first; they failed (ImportError). Every golden case passed on the first run.
  The full suite passes (`python3 -m pytest` exit code 0, ~2 min).
### Decisions
- **Category identity**: the snapshot gives categories their own `cid`s (separate from `oid`s,
  so existing oids don't move); rows are tagged `cat<cid>`. An unregistered (undef) category gets
  one shared CategorySnap with name None; its row dies on `get_name` after its bar.
- **Orders**: categories come in Perl hash order (`values %VivifyCats`), and it changes from case
  to case even for the same recipe, so each case records it. The port lists them in snapshot
  order; the test reorders `snap.categories`, and `snap.groups` to the GetGroups order that
  GetItemList used. `rikeysort` and `rnkeysort` are stable (stream_list_many's ties confirm it).
- **PERL-QUIRKs (confirmed by the oracle)**:
  - Rules: `Can't locate class method "GetListOfSimpleRules" via package "SRule"`, inside
    GetEntriesOnCurrentPage before EntriesCount is set: nothing drawn, entries_count null.
  - Stream: a thought with no stored fringe makes DrawOneItem `return` before its item list, so
    its intensity and name texts get no tags; Clear on a page redraw leaves them, and the next
    page is drawn over them (the redraw goldens have 4 such leftovers). The Qt widget (item 020)
    must compose a redraw as `lowered + survivors(previous) + others`.
  - Stream: a '' older entry sorts as 0 (undef hit) and dies on `as_text` after its bar is drawn.
  - Stream: the intensity text is undef (empty) for older thoughts with no hit; numbers are
    perl_str'd ("2.5", "2500", "0").
  - HeightPerRow 40 means rects under 80 px high (e.g. 30×30) die "Illegal division by zero".
  - The categories picture: element squares are drawn after (over) the ovals; ovals start at
    l + 0.8 and end at r + 1.2 SpacePerElements, element k sits at k + 1.
- Category names come from the model: "Prime" is capitalised, interlaced ones are
  "Interlaced_<n>". Fringe labels agree with Perl (platonics "plat…", categories, mappings,
  elements, "SInt(5)", plain strings).
- No model hook was needed.
### Screenshots
- Perl: I looked at all six. `list_stream_real`: unlike the Stream view, components show their
  as_text, not refs ("[100] plat[1, 2, 3]; [100] ascending;", "[50] Seqsee::Element:[0,0] 1"); the
  element thought (1999) shows all six components, absolute positions included; the category
  thought has no intensity. `list_categories`: four rows on
  alternating bars, each a grey 300 px box with blue ovals and small element squares and the name
  on the right: ascending (two 3-wide ovals), number (ten small circles), even (one oval over
  elements 4–6), sameness (7–9). `list_categories_many`: 10 rows in hash order (Interlaced_6,
  sameness, …, number with 28 tight ovals), "Page #1. Entries 1-10 of 15 Shown.".
  `list_stream_small`: "-" and the current element thought with its sorted fringe "[100] 4;
  [80] absolute_position_3; [50] ascending; [30] SInt(5);", then 2500 relation (empty fringe
  line), 500 group, the fringe-less "Category ascending" with no intensity, 0 element.
  `list_stream_many`: hit order 40, 40, 30, 30, 20, 10, 10, 2.5, then the undef/0 ones; rows
  without fringe have only the top line. `list_rules_fake`: 27 rows of rule text, two empty rows
  (the '' and undef rules), then the compound rules.
- Qt: none yet, as in 000–011 (no ops→Qt renderer until item 014, which must render the
  gui_list_* goldens and compare them with these PNGs). Expected differences: Qt will draw
  Helvetica 12 at its real size (the PostScript text looks smaller) and show no addresses.
### Blockers
- None.
### Next
- Item 013 (Composite views): Tk/Seqsee.pm's 11 `@ViewOptions`, `AttentionNeeded` and the arrows,
  as `compose(view, snapshot, w, h)`. Every draw module returns its lowered items apart
  (`draw_layers`); composition must lower them below the whole canvas in drawing order. Which
  views include the lists (and with which page) must come from the oracle.

## Iteration 14 — 2026-10-07 03:57
### Completed
- Item 013, Composite views (lib/Tk/Seqsee.pm):
  - `oracle/gui_views.pl` → `tests/golden/gui_views.json` (220 cases, 2 MB, ~3 s). The oracle
    builds the real `Tk::Seqsee` widget (`$mw->Seqsee(-width, -height)`: Populate, Menustrip,
    canvas) and, for each case, runs the View menu callback's statements (`@Parts = @{ $vo->[1]
    }; SetupParts(); Update()`). `@Parts` and `$Canvas` are file lexicals; PadWalker's
    `closed_over(\&Tk::Seqsee::SetupParts)` reaches them (PadWalker is already installed). Cases:
    12 recipes (empty, groups_relations, attention_groups, metonyms, slipnet_small, slipnet_dir,
    coderack_small, stream_small, relations_pane, groups_list, categories_many,
    stream_list_many) × the 11 views at 780×450; AttentionNeeded × 11 views × 3 recipes; the
    view Populate sets up (view 0); groups_many with the Groups list on pages 1 and 9 in views 0
    and 8; Categories and Stream2 on page 1; pages set *before* SetupParts (views 0, 8); 3
    recipes × 11 views at 1000×700; groups_relations × 11 views at 333×211; arrows at both
    other sizes. Each case records items, died, metrics (non-default fonts), the list states,
    and the hash orders (GetGroups, relations, %HistoryOfRunnable, the Categories list's
    categories) with `group<K>` / `cat<K>` tags, as the per-view oracles do. Addresses → ADDR.
    10 Perl screenshots `docs/gui/perl/views_<view>_<recipe>.png`.
  - `gui/draw/views.py` (new): `View`, `VIEW_OPTIONS`, `view_index` (index or title),
    `initial_view(options)` (`$Options_ref->{view} || 0`), `part_rects` (SetupParts, the same
    `l * 0.01 * W` float expressions), `attention_arrows(w, h)`, `Composition(ops, died,
    died_part, lists)` and `compose(view, snap, w, h, attention_needed=False, pages=None,
    measure=, known_families=(), rules=None)`. A small `_Canvas` display list implements draw,
    lower (lowered items latest first at the bottom of the whole canvas) and raise by tag.
  - Hooks in existing draw modules (no behaviour change; all their goldens pass):
    `workspace._Drawer.raised_at` (len(out) after DrawGroups' raise('hilit')),
    `workspace.draw_raised` / `workspace_attention.draw_raised` → `(ops, raised_at)`;
    `ops.DrawDied(message, ops, lowered=0, raised_at=None)`; `lists.draw_list`'s DrawDied now
    says how many of its ops are lowered bars.
  - `tests/test_gui_views.py` (231 tests, 0.7 s): all 220 golden cases (ops in order after putting the
    snapshot in Perl's hash orders and mapping the tags, die messages, list states); view
    options parsed from Tk/Seqsee.pm; golden coverage; view lookup and initial view; part
    rectangles; arrow geometry; arrows last and only when needed; a die stops later parts and the
    arrows; bars below every part; the cross-part hilit raise; paging; purity.
  - PORTING_MAP: Tk/Seqsee.pm is `partial` (item 015 adds the menus and widget).
- I wrote the tests first; they failed (ImportError). Full suite: 6121 passed (~1:45).
### Decisions
- **Part names** are the Perl module names (`SGUI::Workspace`, `SGUI::List::Groups`, …), so
  `pages` keys and `Composition.lists` keys equal the list tags.
- **Shared canvas**: `lower` and `raise('hilit')` act on every item, so `compose` replays
  them on one display list rather than concatenating views. Confirmed by the golden: in
  'Workspace + Attention', Workspace_Attention's raise lifts the Workspace's hilit borders above
  the attention part's black rectangle (visible in `views_2_attention_groups.png`).
- **PERL-QUIRKs (all confirmed by the oracle)**:
  - Update doesn't catch a part's die: the partial items stay, later parts and the arrows are not
    drawn. 'Workspace + Rules' always dies after the Workspace; 'Workspace + Attention' dies with
    any relation; view 0 draws the Slipnet first, so an active Mapping::Dir node leaves only the
    Slipnet's partial ovals. `compose` returns `died`/`died_part`. A list whose DrawIt never ran
    keeps stale fields in Perl; `Composition.lists` simply omits it.
  - DrawAttentionDirectingArrows passes `-anchor => 'n', Style::Element(0), -fill => red`;
    Style::Element's own `-anchor => 'center'` comes later and wins, so "PLEASE SEE BELOW" is
    centred on 0.88 H. The arrow is width 15, from 0.92 H to 0.99 H at W/2.
  - SGUI::List::Setup sets PageNumber = 0, and SetupParts calls it on every view change, so a
    page doesn't survive a view switch even though the list objects are shared (the
    'before_setup' cases). The widget (015/020) must reset `pages` to {} on a view change and
    feed back `Composition.lists[name].page_number` (±1 for the page squares) within a view.
  - Parts aren't clipped: the Coderack's headers (YOffset − 10) are visible at y = 215 in view 5,
    the Stream's current thought sits on the part's top edge, and overflowing rows run into the
    other parts / off the canvas, as in Perl.
- Initial view: Test::Seqsee sets no `view` option, so Populate shows view 0 (the golden's
  `view: null` case); `initial_view` mirrors `$Options_ref->{view} || 0`.
- No model hook was needed.
### Screenshots
- Perl: I looked at `views_0_groups_relations`, `views_2_attention_groups`,
  `views_2_groups_relations`, `views_5_coderack_small`, `views_9_stream_small` and
  `views_4_categories_many`. View 0: the workspace in the top-left 65 %, the (empty) slipnet on
  the right, the Groups list bottom-left (3 rows, its bookkeeping text and red squares at x ≈
  243) and the Relations pane bottom-right with green stripes. View 2 (attention_groups): the
  workspace on top with the dashed and thick blue hilit borders, the black attention area below
  with mint/dark-green groups; view 2 (groups_relations): the black area has only the groups'
  outlines and elements, no bar lines or label (the die). View 5: headers at y ≈ 210, five rows
  with blue and red bars. View 9: the stream boxes in the lower half, the current one with its
  thick border on the halfway line. View 4: four category rows per page ("Page #1. Entries 1-4 of
  15 Shown."), the workspace's 14 stippled pair groups above.
- Qt: none yet, as in 000–012 (no ops→Qt renderer until item 014, which must render the
  gui_views goldens and compare them with these PNGs). Expected differences: Qt draws fonts at
  their real sizes (the Xvfb fallback makes the PostScript text small) and shows no addresses.
### Blockers
- None.
### Next
- Item 014 (Qt renderer): `seqsee/gui/qt/render.py`, ops → QGraphicsScene items for every op
  type and option (arrow + arrowshape, smooth, dash strings/lists, stipple, anchors, X11 font
  names → QFont, `colors.to_hex` never `QColor(name)`); a QFontMetrics-based `measure` for the
  workspace metonym cross. Render the goldens of 000–013 (e.g. the gui_views cases) and save Qt
  screenshots next to the Perl PNGs; look at both and record the differences.

## Iteration 15 — 2026-10-07 04:12
### Completed
- Item 014, the Qt renderer:
  - `gui/qt/__init__.py`, `gui/qt/render.py` (new): `render(ops, scene=None, width, height,
    background)`, `add_ops`, `make_item`, `op_items(scene)`, `render_image`, `save_png`;
    `TextItem` (a QGraphicsItem: lines, font, colour, anchor, justify, wrap width);
    `QtMeasure` (`(width, linespace)` from QFontMetrics, for the draw functions' `measure`);
    `qcolor` (through `colors.to_hex`), `parse_font` → `FontSpec`, `qfont` (cached);
    pure Tk geometry helpers `dash_lengths` (DashConvert), `arrow_geometry` (ConfigureArrows),
    `bezier_segments` (TkMakeBezierCurve), `text_origin` (ComputeTextBbox).
  - `gui/draw/ops.py`: `from_dict(item)`, the inverse of `to_dict` (golden items → ops).
  - `tests/gui_screens.py` (new): `SCREENS` maps each of the 44 Perl PNGs to the golden case it
    was saved from (the same conditions as the oracles), plus `canvas_size` and `screen_case`.
  - `tests/conftest.py`: a `--write-screens` option (env-var prefixes need approval in this
    sandbox; pytest options don't).
  - `tests/test_gui_render.py` (new, 89 tests, ~1.7 s): font parsing (XLFD, Tk strings, default),
    dashes, arrow geometry (hand-computed), Bézier control points, all nine anchors and Tk's
    rounding; the item for each op type and option of the gui_smoke 'styled' recipe (pen, brush,
    stipple texture of exactly 75 %, cap/join, dash pattern in pen widths, arrowhead children,
    shortened line ends, smooth paths, arc start/end and styles, pieslice centre, text box and
    justification); X11 colours (gray, green, maroon, purple) not Qt's; unset fill/outline;
    QFont mapping and caching; QtMeasure; pixels of a rendered image; from_dict round trip;
    every item of every drawing golden (13 files) renders one top-level item per op; a composed
    view (compose + QtMeasure) renders; every Perl PNG has a screen case; the Qt screenshots
    exist.
  - 44 Qt screenshots in `python/docs/gui/screens/` (same names as `docs/gui/perl/`), rendered
    from the golden items of the case each Perl PNG came from.
  - PORTING_MAP: a `done` row for the Qt renderer (no Perl module).
- I wrote the tests first; they failed (ImportError). Full suite: 6210 passed (~1:51).
### Decisions
- **Items**: one top-level item per op, `zValue` = op index; the op is in `data(OP_ROLE=0)` and
  its tags (a list) in `data(TAGS_ROLE=1)`, for click mapping later (020). Lines (and smooth
  polygons, arcs) are QGraphicsPathItems, with arrowheads as child QGraphicsPolygonItems filled
  in the line colour; rectangles/ovals are Rect/EllipseItems (miter joins); plain polygons are
  PolygonItems (odd-even fill, like X).
- **PySide6 6.11 bug**: calling `parentItem()` on a top-level item removes it from its scene (it
  happens with plain QGraphicsRectItems too). Use `render.op_items(scene)` (filters on the op
  data) and never call `parentItem()` in the widgets (015–020).
- **Tk semantics kept**: width 0 draws 1 px; a dash string scales with the rounded width and
  Qt's dash pattern is divided by the pen width; the line end backs off to the arrow's neck before
  smoothing; arrow direction comes from the raw neighbour point; `-smooth` polygons are closed
  curves; Tk's text box = ROUND(point) − integer halves of (max line width, lines × linespace).
- **Fonts**: XLFD family/weight/slant/pixel size (point size/10 if the pixel field is `*`) and Tk
  strings ("Helvetica -12", "{Times} 12 italic"). Always set in pixels; Tk points at 96 dpi (like
  `approx_measure`). Style hints: sans (helvetica…), serif (times…), typewriter (courier, fixed).
- **Stipples**: gray12/25/50/75 as texture brushes in the item's colour (gray75 = rows 1011 /
  1110); unknown bitmaps fall back to solid. Text stipple is applied to the text pen.
- **Screenshots come from the golden items**, i.e. what Perl drew (the drawing tests already prove
  the Python ops equal them); one test also renders the Python `compose` output with QtMeasure.
  Background white (the Perl PNGs come from PostScript, which has no canvas background; the Tk
  widget's own background is the system grey — item 015 decides).
- No model hook was needed.
### Screenshots (looked at both for smoke, workspace_groups_relations, views_0_groups_relations,
views_2_attention_groups, list_groups_many, coderack_many, slipnet_full, stream_real)
- Shapes, colours, positions and stacking match in all of them: ovals, stipples (the uncategorised
  group's dotted fill), dashed hilit borders, thick borders, smooth relation arrows, striped list
  rows, red page squares, slipnet circle sizes, stream boxes, the black attention area with mint
  and dark-green groups and pink "5".
- Visible differences:
  - Text is bigger in Qt: Qt draws the 9/10/14/20 px fonts at their real size, the Perl PNGs use
    the Xvfb fallback font scaled by PostScript. So Qt's 9 px rows in the Relations pane and
    Groups list touch each other, the Groups list's long category text runs into the Relations
    pane in view 0, and stream texts overflow their boxes (all at Perl's positions).
  - The metonym cross (from Perl's recorded bbox) is narrower than Qt's "[2, 2, 2]"; with
    QtMeasure (the live widget) it will fit Qt's text.
  - The coderack's column headers (drawn at YOffset − 10, partly above y = 0) show their lower
    half in Qt; the Perl PNG (EPS crop) cuts them off entirely.
  - The Perl PNGs are slightly smaller (EPS crop at 96 dpi: e.g. 385 vs 400 px for smoke) and
    Perl's outlines look a little heavier; Qt antialiases. The "---" dashes look a bit longer in
    Qt than in the PostScript rendering.
  - Addresses: the Qt images show "HASH(ADDR)" (the goldens' scrubbed text) where Perl shows real
    addresses.
### Blockers
- None.
### Next
- Item 015 (Main window & views): a QMainWindow with a QGraphicsView on a scene filled by
  `render.render(compose(view, snap, w, h, measure=render.QtMeasure()).ops, scene=…)`; the View
  menu with the 11 `VIEW_OPTIONS` (reset `pages` on a view change, as SetupParts does); redraw on
  resize; Save as PNG/SVG (`render_image` / QSvgGenerator — check QtSvg ships with PySide6);
  Help → About. Use `op_items`, not `parentItem()`.

## Iteration 16 — 2026-10-07 04:26
### Completed
- Item 015, the main window (lib/Tk/Seqsee.pm's Populate, as SGUI.pm's CreateWidgets builds it
  with config/GUI_sparse.conf):
  - `oracle/gui_window.pl` → `tests/golden/gui_window.json` (1 case). It builds the real
    `Tk::Seqsee` widget with GUI_sparse.conf's options and dumps: the Menustrip's menus (View,
    Save, Help; Help packed on the right; entries and the Help separator in order), the
    @Parts each View entry sets (entries invoked like clicks; PadWalker as in gui_views.pl),
    the canvas (780 × 450, background #D9D9D9, highlight 1, border 0), the frame background,
    and what the Help entries do (they print "[About...]" / "[Help On...]": no action).
    Tk::Menustrip is not Tk::Menu: each label is a Frame with a 'Label' Button and a 'Popup'
    Toplevel whose packSlaves are the entries; the oracle walks `$strip->{m_MenuList}`.
    It also saves `docs/gui/perl/window_groups_relations.png`, a grab of the whole live widget
    (Tk::WinPhoto → PPM → pnmtopng), so it shows real X fonts, not the PostScript rendering.
  - `gui/qt/mainwindow.py` (new): `MainWindow(options, width, height)` with `view_actions`
    (11 checkable, exclusive), `save_actions` ("as PNG...", "as SVG..."), `about_action`;
    `set_view` (index or title; empties `pages`), `set_page`, `set_attention_needed`,
    `show_snapshot`, `redraw`, `reset_run`, `attach_runner(runner)` (snapshot → show,
    error → status bar), `save_image(path)` (.png via `render.save_png`, .svg via
    QSvgGenerator; anything else → ValueError), `about_text()`. `Canvas` is a QGraphicsView
    (no frame, no scroll bars, top-left aligned, identity transform, sizeHint 780 × 450) that
    schedules a redraw on resize.
  - `tests/test_gui_mainwindow.py` (25 tests, ~1 s): menus and View entries against the golden,
    initial view (`options["view"]`), canvas size/background, the window's composition equals
    `compose(view, snap, w, h, measure=QtMeasure())` and the scene holds those ops, every view,
    pages reset on view change, attention arrows, a died part in the status bar, a drawing bug
    caught, Coderack rows remembered, resize redraws at the new size (and coalesces 5 resizes into
    one redraw), PNG/SVG saving (SVG holds every text and the viewBox), the save dialog adds the
    chosen suffix and a cancel does nothing, About box, a runner's signals, drawing reads only
    the snapshot, and the window screenshot.
  - `tests/gui_screens.py`: `WINDOW_SCREENS` (whole-window screenshots, not golden cases);
    `test_gui_render.py` accepts them and skips gui_window.json in its drawing-golden loop.
  - PORTING_MAP: Tk/Seqsee.pm is `done`.
- I wrote the tests first; they failed (ImportError). Full suite: 6235 passed (~1:26).
### Decisions
- **Canvas background #D9D9D9**: GUI_sparse.conf's `background = white` (no dash) doesn't reach
  the canvas; the oracle shows both the frame and the canvas keep Tk's default grey. PNG and SVG
  exports use it too (what you see), unlike the item-014 screenshots (white, like PostScript).
- **Resize**: the Perl canvas is fixed; here the canvas follows the window, and each resize
  recomposes the view at the new size (SetupParts' fractions) via a 0 ms single-shot timer, so a
  burst of resizes gives one redraw. `set_view`/`show_snapshot`/`set_page` redraw at once
  (snapshots come at most ~30/s from the runner).
- The window is resized to its sizeHint in `__init__`: Qt caps a new top-level window at 2/3 of
  the screen (533 px on the 800 × 600 offscreen screen).
- **Menus**: View / Save / Help as in Perl. "as EPS" became "as PNG..." and "as SVG..." (a
  QFileDialog with both filters; a name without .png/.svg gets the chosen filter's suffix).
  Help → "About..." opens a QMessageBox (Perl's does nothing); "Help On..." (no action in Perl)
  is left out; Help isn't right-aligned (QMenuBar). No view shortcuts: single keys belong to
  item 016's bindings.
- **Coderack rows**: SGUI::Coderack's `$HistoryOfRunnable{$_} ||= 0` persists in the model; the
  window keeps `known_families` (rack families added after every draw where the Coderack ran)
  and `reset_run()` forgets them (to be called on a new sequence, item 018).
- Errors: a part's die (PERL-QUIRK, e.g. 'Workspace + Rules') shows "<part> died: <first line>"
  in the status bar; any other exception from compose/render keeps the last picture and shows
  "Drawing failed: …"; runner errors show "Model error: …" (the commentary dock, 017, will log
  them properly).
- `Canvas` creates its QGraphicsScene with itself as parent: a parentless scene passed to the
  QGraphicsView constructor was garbage-collected by PySide6.
- No model hook was needed. Noticed: `python/oracle/_tmp_gg.pl` is a scratch file committed in
  iteration 14; harmless, left alone (item 024 can delete it).
### Screenshots (looked at both `window_groups_relations.png`)
- Same layout: workspace top-left (bar lines, the mint and blue groups inside the green one,
  the dashed hilit border, the green relation arrow 1→3, the grey 4→5 arrow, the "2"/"[2, 2, 2]"
  metonym with its cross), the empty slipnet on the right, the Groups list bottom-left with its
  three striped rows, red page squares and "Page #1. Entries 1-3 of 3 Shown.", the Relations pane
  with green stripes; same grey background. Fonts look alike here (both are real screen fonts).
- Differences: Perl's Help menu is on the right, Qt's on the left; Qt adds a status bar below
  the canvas; the Groups list and Relations rows come in a different order (Perl's hash order vs
  the port's; the golden tests already cover the order); the metonym cross fits the text in both
  (QtMeasure sizes it); both show live addresses ("HASH(0x…)"), which differ; Qt antialiases.
### Blockers
- None.
### Next
- Item 016 (Controls): a toolbar and GUI_sparse.conf's key bindings driving the runner (s, h/j/k/l,
  d/f/g, c, p, x, q, m), the InterstepSleep slider (0–40), Tk/SCodeletCount.pm, a status bar
  (steps, elements, groups, run state). Build on `MainWindow.attach_runner`; the status bar
  already carries died/error messages, so give the run state its own permanent widget.

## Iteration 17 — 2026-10-07 04:38
### Completed
- Item 016, the controls (config/GUI_sparse.conf's [buttons], [bindings], [Scale], [SCodeletCount]
  as lib/SGUI.pm's CreateWidgets / SetupButtons / SetupBindings install them; lib/Tk/SCodeletCount.pm):
  - `oracle/gui_controls.pl` → `tests/golden/gui_controls.json` (1 case). It runs the real
    SGUI::CreateWidgets / SetupButtons / SetupBindings on GUI_sparse.conf, with Seqsee.pl's
    Interaction_* subs, SGUI::ask_seq, Tk::exit and CORE::GLOBAL::exit replaced by recorders, and
    dumps: the frames (class, pack side), the button frame's slaves (Start / Pause / Quit on the
    left, the SCodeletCount on the right), each button's and key binding's effect (the call it
    makes, or Break_Loop / debugMAX before and after; each binding pressed twice, so m's 1 then 0
    is recorded), the count's font (XLFD + fontActual: Nimbus Sans bold 20 px) and its text after
    SGUI::Update with Steps_Finished undef / 0 / 17 ('0', '0', '17'; '' before any Update), and the
    [Scale] config. Screenshot: `docs/gui/perl/controls_buttons.png` (WinPhoto of the button frame).
    Gotchas: Tk.pm does `*CORE::GLOBAL::exit = \&Tk::exit`, so the override must come after
    `use Tk` (with the `(;$)` prototype); `$MW->bind` lists `<KeyPress-c>` as "c", and a lone "c"
    passed back to bind is read as a tag, so ask for `"<KeyPress-c>"`.
  - `gui/qt/controls.py` (new): `BINDINGS` (key, command, arg, menu text, toolbar text, Perl
    command), `BUTTONS`, `COUNT_FONT`, `SLEEP_MIN/MAX/TICK`, `toggled_debug_max`, `count_text`,
    `CodeletCount` (QLabel), `SleepSlider` (label + 0–40 slider + "N ms"; `moved` only for user
    changes, `show_value` is quiet), `RunStatus` (permanent status-bar labels), `make_actions`,
    `fill_run_menu`, `fill_toolbar`.
  - `gui/qt/mainwindow.py`: a Run menu (after View: all 13 bindings with their keys as shortcuts),
    a "controls" toolbar (Start / Pause / Quit, Step +5 +25 +50 +100, a Crawl drop-down (30 ms; 300
    and 700 in its menu), New..., debugMAX (checkable), the count on the right), the sleep slider
    in the menu bar's right corner, status labels "Steps: N  Elements: N  Groups: N  <state>";
    `run_command(command, arg)`, `ask_seq` / `start_sequence`, `interstep_sleep`, `debug_max`,
    `run_state`, closeEvent → `runner.quit(5)`. `attach_runner` now also connects `state_changed`
    and `command_done`.
  - `gui/runner.py`: `set_interstep_sleep(ms)` and `set_debug_max(value)` (single Global writes;
    GUI code, not a model change).
  - `tests/test_gui_controls.py` (40 tests, ~3 s): bindings, buttons, count font/text and slider
    range against the golden; toolbar contents; the count, slider and key actions are really
    visible at the initial 780 px width; Run menu shortcuts; status labels from snapshots; every
    binding and button drives a fake runner; real key presses (qtbot.keyClick) trigger the
    bindings; debugMAX toggles; slider → set_interstep_sleep; crawl/continue move the slider
    quietly; a new sequence re-applies sleep and debugMAX; state label incl. "Paused"; no-runner
    message; ask_seq; q closes and quits the runner; the runner setters; with a real Runner: h
    steps 5 (count "5", Elements 6, Idle), and a crawl whose sleep the slider changes live, then
    paused ("Paused"); the toolbar screenshot.
  - Updated: test_gui_mainwindow.py (menus are View, Run, Save, Help; the fake runner has the new
    signals and quit), test_gui_render.py (gui_controls isn't a drawing golden; CONTROL_SCREENS),
    gui_screens.py (`CONTROL_SCREENS`), the window screenshot (now with toolbar and status labels).
  - PORTING_MAP: Tk/SCodeletCount.pm `done`; a `done` row for the controls; the SGUI.pm row notes
    SetupButtons/SetupBindings (still `todo` for ask_seq / ask_for_more_terms, item 018).
- I wrote the tests first; they failed (ImportError on `controls`). Full suite: 6275 passed
  (~1:51). (PORTING_MAP cells can't contain `\|\|`: the audit's table split breaks on it.)
### Decisions
- **Bindings as QActions** with single-key shortcuts (Qt.WindowShortcut), in the Run menu, so
  they work anywhere in the window like `$MW->bind`; Qt text fields still get their typed
  letters (ShortcutOverride). The toolbar reuses the same actions (iconText = short label).
  Start / Pause / Quit are separate actions without shortcuts (c, p, q belong to the Run actions).
- **The slider**: GUI_sparse.conf configures a [Scale] but no frame packs one, so Perl sparse
  shows none; this one is added. It writes `Global.InterstepSleep` through the runner at once, so
  it changes a running crawl's sleep (Interaction_step_n reads it every step). crawl(ms) / continue
  show their sleep on it without emitting (Interaction_crawl sets ms, Interaction_continue 0); a
  700 ms crawl shows the slider at 40 and the label "700 ms" — no write-back (a Tk Scale bound to
  the variable would clamp it to 40; moot, since sparse has no Scale).
- **Layout**: with everything on one toolbar, the toolbar was 1158 px wide and pushed the window
  past the 780 px canvas; then (after compacting) the count landed behind the overflow "»". The
  slider went to the menu bar's right corner, and the step/crawl texts got short (Crawl is one
  drop-down button). A test checks the count, slider and actions are visible at the initial size.
- **debugMAX**: `1 - $debugMAX` (undef → 1); the m action is checkable and the window keeps the
  value. **New sequence**: SGUI::ask_seq keeps InterstepSleep and debugMAX, but the runner's fresh
  run (reset_all) clears them, so on `command_done("new_sequence")` the window sets both again.
- **x** opens a plain QInputDialog ("Seqsee Sequence Entry", Perl's label) and calls
  `start_sequence` (reset_run + runner.new_sequence); item 018 replaces it with the
  sequence.list combo and the "Illformed input" check. **q** / Quit close the window; closeEvent
  quits the runner (5 s timeout). Perl's Quit also prints "ATTEMPTING TO QUIT"; not ported.
- **Run state** label: Idle / Running / Waiting for an answer / Stopped / "No model" (no runner);
  "Paused" when the runner goes idle after a pause (cleared by the next run command). Commands
  without a runner show "No model attached" in the status bar.
- The count shows "0" from the start (init_display calls SGUI::Update right after creating it).
- No model hook was needed (the two runner setters are GUI code writing Global scalars, as Perl's
  Tk callbacks do).
### Screenshots (looked at both `controls_buttons.png`, and the new `window_groups_relations.png`)
- Same: Start, Pause, Quit on the left, in that order; the count "17" on the right in a big bold
  sans font.
- Differences: Perl's buttons are raised Tk buttons packed tightly, with the count right after
  Quit (the frame is only as wide as its contents); Qt's are flat toolbar buttons across the
  window's width, with the step/crawl/New/debugMAX actions between Quit and the count, which sits
  at the far right. The sleep slider is in the menu bar's corner (no Perl counterpart); the status
  bar shows Steps / Elements / Groups / state.
### Blockers
- None.
### Next
- Item 017 (Commentary): Tk/SCommentary.pm as a dock (message log with GUI_sparse.conf's
  [SCommentary_tags] colours and font), MessageRequiringNoResponse / AResponse (buttons) /
  BooleanResponse wired to the runner's `question` signal, UI/Graphical.pm's ask_user_extension.
  Model errors currently go to the status bar ("Model error: …"); log them in the commentary too.
  SCommentary's own m binding also prints "debugMAX=N" (`main::message`): add that to the log
  when the m action fires.

## Iteration 18 — 2026-10-07 05:10
### Completed
- Item 017, the Commentary (lib/Tk/SCommentary.pm as SGUI::CreateWidgets builds it from
  config/GUI_sparse.conf's [SCommentary] / [SCommentary_tags]; lib/UI/Graphical.pm's message,
  ask_user_extension, update_display, default_error_handler):
  - `oracle/gui_commentary.pl` → `tests/golden/gui_commentary.json` (6 cases): the widget
    (ROText height 10, width 60, wrap word, font XLFD + fontActual; each tag's foreground/font;
    the tag priority from `tagNames`; the 4 blank disabled buttons, width 15, and Start Debug
    packed bottom), and 5 scripted sessions (no_response, response, boolean, message,
    ask_user_extension) on the real widget. Questions are answered by a `$MW->after` callback
    that first records the buttons and Tk::Seqsee's AttentionNeeded (PadWalker closed_over),
    then invokes a button or calls the Text's `<KeyPress-N>` binding (keys past the active
    buttons are ignored). After each step: return value, text as [text, [tags]] runs
    (`$Text->dump`), globals. Screenshot `docs/gui/perl/commentary.png` (WinPhoto).
  - `gui/commentary.py` (new, pure): TEXT_FONT/HEIGHT/WIDTH/WRAP, TAG_CONFIG → TAGS,
    TAG_PRIORITY, `style_of`, `tag_list`/`insert_runs` (Tk insert's chars/tagList pairs; a
    string tagList is a Tcl list, '' none), `response_parts`, `boolean_value`, `key_choice`,
    `message_request` (main::message), `debug_message`, `parts_text`, `Log` (runs merged as
    `dump` reads them).
  - `gui/qt/commentary.py` (new): `Commentary` (QTextEdit with per-tag QTextCharFormats, the
    button column, window-wide QShortcuts 1-4, `insert`, `ask`, `choose`, `press_key`,
    `close_question`, `pending`, signals `answered(question, value)`, `debug_clicked`),
    `CommentaryDock` (objectName "commentary", bottom area).
  - `gui/runner.py`: `Question.parts` (the Tk insert arguments: boolean → (question, *rest),
    response → (question,)); a `message` signal; `MESSAGE_HOOK_MODULES` (all_mx, large_gp,
    codelets.ui, scripts.describe_solution, sanity, mapping, categories.alternating,
    scodelet_base — sstream2 calls scodelet_base's at call time): their `_message` becomes the
    runner's main::message (no_break → `message`; else a 'continue' response question that
    blocks the worker, as Perl waits). Installed with the other hooks, restored by quit.
  - `gui/qt/mainwindow.py`: the dock, `show_question` (boolean/response only; sets
    AttentionNeeded), `question_closed`, answers → `runner.answer`, Start Debug →
    `run_command("debug_max")` + "debugMAX=N\n", model errors logged ("Model error: …" in the
    `error` tag) as well as in the status bar.
  - `tests/test_gui_commentary.py` (27 tests, ~3 s): config vs golden; each session replayed
    on the window's dock (button clicks and real key presses; waiting buttons, AttentionNeeded,
    return values, runs, plain text, globals; ask_user_extension runs the port's
    gui_ask_user_extension); tag colours/fonts in the QTextDocument; sizing; scroll to end;
    cancelled question; more_terms ignored; Start Debug vs m; the runner's message hooks,
    question parts, hook restore; a real Runner answered through the dock; errors logged;
    screenshot. Fake runners in test_gui_controls/test_gui_mainwindow got the new signals;
    gui_screens.CONTROL_SCREENS has "commentary"; test_gui_render skips gui_commentary.
  - PORTING_MAP: Tk/SCommentary.pm and UI/Graphical.pm `done`; the runner row lists the hooks.
- Tests written first; they failed (ImportError on `seqsee.gui.qt.commentary`). (The pure
  `gui/commentary.py` was drafted just before the tests.) Full suite: 6302 passed (~1:53).
### Decisions
- **main::message blocks without no_break**, as in Perl: e.g. with debugMAX on, every codelet's
  "About to run" waits for 'continue' (that is what Perl's debugMAX does). Messages with
  no_break (e.g. DescribeSolution's) just log. No model change: the `_message` functions were
  already the port's UI hooks; the runner only replaces them.
- **Keys 1-4** are window-wide while a question waits (Perl binds them on the Text, which takes
  focus and a grab). No grab: the toolbar/keys stay usable; run commands queue behind the
  question; Pause/new sequence work as before (a new sequence cancels the question, which the
  dock logs as "  (cancelled)").
- **more_terms** questions are not the Commentary's (SGUI::ask_for_more_terms is a separate
  Toplevel): `show_question` ignores them; item 018 must handle them, or a run that asks for
  more terms waits forever in the GUI.
- **Tag overlap**: Tk priority = creation order (tags_to_aref's hash walk under the fixed
  seed: codelet_family, debug, green, user_response, from the oracle); `style_of` takes each
  option from the highest-priority tag that sets it.
- The text font maps through `render.qfont` (Lucida Bright bold 17 px; Tk resolved it to Noto
  Sans bold, Qt falls back similarly). The buttons use Tk's default button font (Helvetica
  -12 bold); the button column is centred vertically with Start Debug right below, as packed.
- Not ported: the Text's q binding (the window's q already quits), `pop_message` (no caller in
  lib/), `main::debug_message` (called by SLTM/DescribeSolution but defined nowhere in Perl).
- The m binding doesn't log (GUI_sparse.conf's m only toggles); only Start Debug logs
  "debugMAX=N" (iteration 17's note mixed the two up).
- The status bar's "Model error" is cleared by the redraw of the snapshot that follows the
  error (pre-existing); the commentary keeps it now.
### Screenshots (looked at both `commentary.png`; also the new `window_groups_relations.png`)
- Same: the same five lines (bold sans 17 px; "Reader" in green; the empty line), the question
  "Is the next term 4?" at the end, buttons yes / no / two blank / Start Debug in a column on
  the right, bold labels.
- Differences: Perl always shows both scroll bars ('se'); Qt shows a vertical one only when
  needed (none here). Perl's text area is grey (#D9D9D9) with a sunken border, Qt's is white.
  Perl's button column sits higher (its frame is centred against the text plus the horizontal
  scroll bar); Qt's buttons are flatter. The window screenshot now has the Commentary dock
  (title "Commentary") under the canvas; the canvas is still 780 × 450.
### Blockers
- None.
### Next
- Item 018 (Sequence entry): SGUI::ask_seq (sequence.list combo, "Illformed input" check,
  reset) replacing the plain QInputDialog, and SGUI::ask_for_more_terms for `more_terms`
  questions (the runner already sends them; `MainWindow.show_question` ignores them now).
  Perl's ask_seq logs 'New Sequence Started: ', [], "$seq\n" when the dialog opens, with $seq
  still undef (so just "New Sequence Started: \n"): port it as a PERL-QUIRK through
  `self.commentary.insert`.

## Iteration 19 — 2026-10-07 05:32
### Completed
- Item 018, sequence entry (lib/SGUI.pm's ask_seq, `$check_and_accept_input_sequence`, the
  BEGIN block reading config/sequence.list, ask_for_more_terms; UI/Graphical.pm's
  ask_for_more_terms + waitWindow):
  - `oracle/gui_seqentry.pl` → `tests/golden/gui_seqentry.json` (19 cases). It runs the real
    SGUI::ask_seq / ask_for_more_terms with SWorkspace->clear / insert_elements, SCoderack->clear,
    `$Global::MainStream->clear`, SGUI::Update and `$SGUI::Commentary` replaced by recorders, and
    dumps: the widgets (title; packed slaves with class/side/text; the ComboEntry's width 40, its
    listbox contents, initial text, bindtags; the Entry's width 20), the message logged at opening,
    10 ask_seq sessions (typed text + the Entry's <Return> callback, Go's invoke, or a listbox row
    + ComboEntry::Select; after each step: label text, whether the Toplevel still exists, every
    recorded call with its arguments, what was printed) and 7 more-terms inputs. Screenshots
    `docs/gui/perl/seqentry.png` (after "1 x 2") and `docs/gui/perl/more_terms.png` (WinPhoto).
  - `gui/seqentry.py` (new, pure): texts and widths, `read_sequence_list`, `combo_list`
    (sorted), `strip`, `split_terms` (Perl split: a leading empty field stays), `check_sequence`
    → `Check(accepted, terms, message)`, `NEW_SEQUENCE_MESSAGE`.
  - `gui/qt/seqentry.py` (new): `SequenceDialog` (prompt left, editable combo top, Go right,
    message bottom, as packed; `accepted_terms(text, terms)`; `select_row` = ComboEntry's Select)
    and `MoreTermsDialog` (`entered(text)`, `dismissed`, `close_quietly`).
  - `gui/qt/mainwindow.py`: `ask_seq` opens the dialog (or raises the open one) and logs
    "New Sequence Started: \n"; `accept_sequence(text, terms)` → `runner.accept_sequence`;
    `show_question` opens `MoreTermsDialog` for more_terms questions (`ask_for_more_terms`),
    whose Return/close answer the question; `question_closed` closes it quietly; closeEvent closes
    both dialogs; command_done("accept_sequence") re-applies sleep/debugMAX too. QInputDialog gone.
  - `gui/runner.py`: `accept_sequence(terms)` (interrupt, then on the worker: INITIALIZE without a
    sequence if no run yet (`_do_new_sequence(..., allow_empty=True)`), then sworkspace.clear,
    scoderack.clear, MainStream.clear, insert_elements); `_ask_for_more_terms` now uses
    `seqentry.split_terms`, always calls insert_elements (even with no terms) and re-asks after
    an insert error.
  - `tests/test_gui_seqentry.py` (48 tests, ~3 s): sequence list / sorted combo list / texts /
    opening message vs golden; every session replayed through `check_sequence` and through the Qt
    dialog (real typing, Return, Go clicks, list activation); Return on a listed sequence accepts
    once; the more-terms dialog; the window's x / accept / no-runner / more-terms paths with a fake
    runner; real Runner: first accept (INITIALIZE, coderack empty, 20 steps run), accept during a
    crawl (steps go on), a "1-2" model error after the clear, empty more-terms answer still sets
    TimeOfLastNewElement, a bad more-terms answer re-asks; the screenshots.
  - Updated: test_gui_controls.py (the QInputDialog test → `test_ask_seq_accepts_a_sequence` and
    `test_start_sequence_is_a_fresh_run`), test_gui_render.py (gui_seqentry isn't a drawing
    golden), gui_screens.CONTROL_SCREENS (seqentry, more_terms). PORTING_MAP: SGUI.pm `done`;
    the controls and runner rows mention ask_seq / accept_sequence.
- Tests written first; they failed (ImportError on `seqsee.gui.seqentry`). Full suite: 6351 passed
  (~1:53).
### Decisions
- **Accept = Perl's closure, not a restart**: the dialog's accepted input clears the workspace,
  coderack and stream and inserts the terms; Steps_Finished, the random state, the slipnet, LTM
  and RealSequence stay (Perl never sets RealSequence/InitialTermCount on this path; only the
  auto answerer reads them). So after an accept the coderack is empty and the next step runs a
  background FocusOn, as in Perl (INITIALIZE's SCoderack->init is undone by the closure's clear).
  Before the first run, INITIALIZE runs with an empty seq first (Perl: INITIALIZE, then ask_seq).
  `Runner.new_sequence` / `MainWindow.start_sequence` stay as the fresh-run path (seeds, item 019).
  No model change.
- **Terms are not checked beyond Perl's check**: "1-2", or a leading comma's "" field, reach
  insert_elements, which dies "Huh? Trying to insert …" after the clear (Perl: the same confess,
  as a Tk background error). Here it's a model error (status bar + commentary). Perl's dialog
  stays open in that case (destroy comes after the insert); ours has closed already (the check
  runs on the GUI thread, the insert on the worker) — press x again.
- **More terms**: Perl inserts even an empty list (TimeOfLastNewElement/TimeOfNewStructure =
  Steps_Finished, which feeds the desperation computation), so the runner no longer skips it.
  An insert error leaves Perl's window open: the runner sends the error and asks again. No
  AttentionNeeded for this window (Perl doesn't set it).
- **One sequence dialog at a time**: x raises the open dialog instead of opening another and
  logging again (Perl opens one Toplevel per press). Both dialogs are non-modal top-level windows
  (Qt.Window), so the run goes on and the main window's single-key shortcuts don't fire while
  typing in them. Escape/Return don't close a QDialog here (the Toplevels have no such keys).
- **ComboEntry details**: SelectionList sorts the list (string sort), so the combo shows it
  sorted; picking a row puts it in the entry and invokes at once (Select → DoInvokeCallback).
  The `$f->bind('<Return>')` on the composite only fires if the frame itself has focus (Perl's
  `$f->focus()` gives it to the frame, so typing needs a click into the entry first); the Entry's
  own <Return> → -invoke is what works, and that is what Qt's Return does (focus starts in the
  edit). An editable QComboBox also emits activated on Return for a listed text: a `_done` guard
  keeps it to one accept (NoInsert keeps the list unchanged).
- Not ported: `print "Return pressed; Seq is: @seq"` (stdout noise; recorded in the golden).
- \s / \d are ASCII here (Perl on Tk's strings could match Unicode digits; not meaningful input).
### Screenshots (looked at all four: perl/ and screens/ `seqentry.png`, `more_terms.png`)
- Same: the prompt on the left, centred vertically; the combo across the top right with its
  drop-down arrow; Go under it at the right; "Illformed input: 1 x 2" centred at the bottom; the
  more-terms prompt over a centred entry holding "7 8".
- Differences: Perl's fonts are bold Helvetica, Qt's the regular system font; Perl's combo is
  white with a Tk arrow button, Qt's a styled combo (my first Qt shot had a too-narrow combo
  without a visible arrow — fixed with setMinimumContentsLength(40)); Qt has more padding; Perl's
  Go is a raised Tk button.
### Blockers
- None.
### Next
- Item 019 (Modern shell): docks for every pane, seed and max-steps fields (they can drive
  `MainWindow.start_sequence` / `Runner.new_sequence(seq, seed=, max_steps=)`, the fresh-run
  path, while x keeps Perl's accept), QSettings layout round-trip.

## Iteration 20 — 2026-10-07 05:49
### Completed
- Item 019, the modern shell (no Perl counterpart; the panes are the drawing modules of
  lib/Tk/Seqsee.pm's @ViewOptions, plus lib/Tk/SCommentary.pm; the fields are Seqsee.pl's
  --seed / --max_steps):
  - `gui/draw/views.py`: `pane_view(part)` (a View whose only part covers 100% of the canvas);
    `compose` / `part_rects` accept a `View` as well as an index or title.
  - `gui/qt/panes.py` (new): `PANES` (Workspace, Attention, Slipnet, Coderack, Stream, Relations,
    Groups, Categories, Rules, Stream list; each with an objectName for saveState), `PaneDock`
    (its own Canvas; draws the window's snapshot when visible: on each snapshot, when shown, after
    a resize (coalesced); a list pane keeps its own page, `set_page`; a die → title
    "<title> (died)" + tooltip; feeds the window's Coderack rows), `make_docks` (right area,
    hidden at first).
  - `gui/qt/controls.py`: `RunSettings` (Seed QLineEdit, digits only, empty = "random"; Max steps
    QSpinBox 1..2^31-1, default from config/seqsee.conf = 100000, `default_max_steps`).
  - `gui/qt/mainwindow.py`: `MainWindow(options, w, h, settings=None)`; a Panes menu (the 10
    docks' toggle actions, a separator, the Commentary's); a second toolbar row "Run settings"
    (objectName run_settings) with the fields and Restart (also at the end of the Run menu);
    `seed()`, `max_steps()`, `restart()` (fresh run of `last_sequence` with the fields; no
    sequence → ask_seq), `start_sequence(seq, seed=None, max_steps=None)` (defaults: the
    fields), `accept_sequence` passes `seed=` / `max_steps=`; a user change of max steps →
    `runner.set_max_steps`; options `seed` / `max_steps` fill the fields; `save_settings` (on
    close: geometry, saveState(1), view, seed, max_steps; sync) / `restore_settings` (in
    __init__; unreadable values ignored; options win), `default_settings()` =
    QSettings("Seqsee", "Seqsee"); `remember_coderack_rows(comp, view)` (public, used by panes).
  - `gui/runner.py`: `max_steps` / `set_max_steps(n)` (overrides options["max_steps"] for the
    following run commands; a running command keeps its limit), `accept_sequence(terms, seed=None,
    max_steps=None)` (seed for the first INITIALIZE), `new_sequence` drops the override.
    GUI runner only; no model change.
  - `tests/test_gui_shell.py` (21 tests, ~2 s): panes = the views' modules; pane_view; Panes
    menu; visible panes draw `compose(pane_view(part), …)` and hidden ones don't (until shown);
    every pane draws; the Rules pane dies visibly; pane resize; pane Coderack rows; independent
    list pages; the fields (typing through the validator), options → fields, Restart / Restart
    without a sequence, accept passes seed/max_steps, the box → set_max_steps; a real Runner
    (accept with seed 5 → options seed 5; set_max_steps(7) + continue → 7 steps); no settings →
    nothing saved; **round-trip through a temporary IniFormat file** (view, seed, max steps,
    size, two docks shown, one moved to the left area, Commentary hidden → all restored);
    options beat settings; bad settings ignored; default_settings names; screenshot.
  - Updated: test_gui_controls.py / test_gui_mainwindow.py menu titles (+ "Panes").
    PORTING_MAP: a "Modern shell" row.
- Tests written first; they failed (ImportError on `seqsee.gui.qt.panes`). Full suite: 6372 passed (~2:02).
### Decisions
- **Panes draw a module over the whole dock**, through `compose` (so the shared-canvas rules,
  lower/raise and the die PERL-QUIRKs apply as in a one-part view). The Workspace pane equals the
  "Workspace" view (tested). Panes start hidden (the fixed views stay the default, as in Perl)
  and are not tabbed (a tabbed-but-not-raised dock never gets visibilityChanged(True), so it
  wouldn't draw); shown docks stack in the right area.
- **List pages**: Perl's lists are shared by the views; a pane's page is its own (paging
  controls are item 020).
- **Seed/max steps**: the seed applies when a run starts (Restart / start_sequence, and the
  INITIALIZE of the first accept); later x accepts are not restarts (Perl's closure), so the seed
  doesn't apply to them. Max steps applies from the next run command (Runner.set_max_steps, one
  attribute write like set_interstep_sleep); accept passes the box's value; new_sequence uses
  its `max_steps` option instead.
- **Settings are opt-in**: MainWindow(settings=None) remembers nothing, so the tests never touch
  the user's settings; item 021's entry point should pass `mainwindow.default_settings()`.
  Command-line options (view, seed, max_steps) beat the remembered values.
- The run settings get their own toolbar row (addToolBarBreak): the controls toolbar already
  fills the 780 px canvas width. The canvas stays 780 × 450.
- Offscreen restoreGeometry clamps to the 800 × 600 virtual screen; the round-trip test uses
  760 × 700.
### Screenshots (looked at `screens/window_docks.png` (new, no Perl counterpart),
  `screens/window_groups_relations.png` and `perl/window_groups_relations.png`)
- window_docks: Workspace view on the left, Slipnet (empty in this recipe), Coderack and Groups
  panes stacked on the right, each with its own title bar and float/close buttons; the Groups
  pane shows the three striped rows, page squares and "Page #1. Entries 1-3 of 3 Shown." at the
  pane's size. The Coderack pane is short (≈ 80 px), so its header row is cut at the top, as
  SGUI::Coderack draws it in so small a rectangle. My first shot had the Max steps box stretched
  across the row (fixed: fixed size policy).
- window_groups_relations: same canvas as Perl's (workspace, groups list, relations; the list
  order and HASH addresses differ as before); the Qt window now has a second toolbar row
  (Seed / Max steps / Restart) and the Panes menu.
### Blockers
- None.
### Next
- Item 020 (List interaction): SGUI::List ProcessClickOnItem / CreatePopupWidget, the page
  squares (the window's `set_page` and the panes' `PaneDock.set_page` are ready), hover tooltips
  on workspace objects. Item 021 should create the window with `settings=default_settings()` and
  pass Seqsee.pl's seed / max_steps / view in `options`.

## Iteration 21 — 2026-10-07 06:20
### Completed
- Item 020, list interaction (lib/SGUI/List.pm's Setup '<1>' bindings, ProcessClickOnItem,
  CreatePopupWidget; the ActionButtons of lib/SGUI/List/Groups.pm and List/Categories.pm) and
  hover tooltips on workspace objects (no Perl counterpart):
  - `oracle/gui_list_interaction.pl` → `tests/golden/gui_list_interaction.json` (38 cases):
    the 4 popups (title, buttons in pack order = hash order, withdrawn); 5 scripted click
    sequences (Motion + ButtonPress/Release-1 via eventGenerate on rows, the "L", the page text,
    both squares, empty space, row boundaries; after each: page, SelectedItem, Break_Loop, popup
    state, and the GetItemList order of the drawing clicked on); 32 actions (every Groups and
    Categories button on several items, through the real button with Tk::Seqsee::Update and
    main::message recorded; groups before/after, bar lines, messages with refs → REF, `srand(20)`
    before each). Screenshot `docs/gui/perl/list_popup_groups.png` (WinPhoto).
  - `gui/draw/lists/__init__.py`: `Click`, `click_of_tags` (the bindings on the 'current' item's
    tags), `page_after`, `entry_for` (ItemVivify).
  - `gui/listactions.py` (new, model side, no Qt): `ACTIONS` per list in Perl's order
    (Groups: ActionFringe, ShowFollowers, Unlock, History, Fringe, Lock, Delete; Categories:
    AddBarlinesBefore, DeleteAllOther; Rules/Stream: none), `run(part, action, item, message)`,
    `popup_title`, `fringe_string`, `mark_descendents_to_keep` (live objects).
  - `gui/hover.py` (new, pure): `targets` (replays the Workspace / Workspace_Attention drawer with
    recording overrides: element text bbox, group oval, relation curve), `target_at` (element, then
    relation within 4 px of the quadratic curve, then smallest group oval), `tooltip(snap, view,
    w, h, x, y)`, `describe_object`, `describe_entry` (the popup's details).
  - `gui/snapshot.py`: `take(live=)` fills tag → live object ("obj<oid>", "cat<cid>").
  - `gui/runner.py`: `list_action(snap, part, action, key)` (worker: resolves the tag in that
    snapshot's live map — kept for the last `LIVE_SNAPSHOTS` = 16 sent — and runs the action with
    the runner's main::message hook; an unknown tag → error "…no longer in the workspace").
  - `gui/qt/listpopup.py` (new): `list_click(scene, x, y)` (topmost item within Tk's
    -closeenough 1 px), `ListPopup` ("Actions for <class>", details label, centred buttons; a
    button emits and withdraws; close withdraws), `ListPopups` (one per list, shared by views and
    panes).
  - `gui/qt/mainwindow.py`: `Canvas(on_click=, tooltip_at=)` (mouse tracking; sets the view's and
    viewport's tooltip); `canvas_clicked`, `handle_list_click`, `process_click_on_item` (popup +
    `run_command("pause")`), `selected_items`, `_list_action` (→ runner.list_action, or "No model
    attached"), `tooltip_at`; closeEvent hides popups. `gui/qt/panes.py`: `PaneDock.canvas_clicked`
    (own page), `PaneDock.tooltip_at`.
  - `tests/test_gui_list_interaction.py` (61 tests, ~1 s): bindings/paging units; popups, clicks
    (through the rendered Qt scene) and every action vs golden; window and pane clicks (popup,
    pause, button → list_action, page squares incl. clamping, close withdraws, Stream list popup
    without buttons, no runner); a real Runner (Unlock, Delete, History question, unknown tag
    error); `take(live=)`; entry details; hover targets/priorities on Workspace, Workspace +
    Groups, Workspace + Attention, a Slipnet pane; window/pane tooltips; screenshots
    `screens/list_popup_groups.png` and `screens/hover_tooltip.png`.
  - Updated: test_gui_render.py (gui_list_interaction isn't a drawing golden), gui_screens
    CONTROL_SCREENS (+ list_popup_groups), docstrings (lists, groups, categories, views, runner,
    mainwindow). PORTING_MAP: SGUI/List.pm, List/Groups.pm, List/Categories.pm → done; runner and
    Modern shell rows mention list_action / hover.
- Tests written first; they failed (ImportError on `seqsee.gui.hover`). Full suite: 6433 passed (~2:09).
### Decisions
- **Hit test = Tk's 'current'**: only the topmost item's tags count, within a 1 px halo; a click
  on the line between two rows goes to the earlier row (its bar is stacked higher, as each bar is
  lowered when drawn) — the golden has such a click (y = 380 in the Categories list).
- **Page click** = `page_after` on the composition's (clamped) page, then redraw, which clamps
  again (page up on the last page stays there; on an empty list Perl's PageNumber becomes 1 and
  stays — no page count to clamp to — harmless).
- **Actions run on the worker** on the live object of the snapshot that was clicked
  (`take(live=)`), as Perl acts on SelectedItem even if the model moved on; the closing snapshot
  is Tk::Seqsee::Update. Smallest hook: the GUI runner only; no model change.
- **Pause on click**: ProcessClickOnItem sets Break_Loop; here `run_command("pause")` (shows
  "paused"). No runner → only the popup.
- PERL-QUIRK (not reproduced, recorded): a row click while the popup is visible blocks the Tk
  callback in waitVisibility until the popup's visibility changes (the oracle withdraws the popup
  between clicks); the Qt popup just shows the new entry.
- GetItemList order is hash order and can differ between calls (Categories builds a new hash each
  time), so each click records the order of the drawing on screen and the test orders the
  snapshot the same way.
- SCodelet::as_text lists args in hash order (`keys %args`); the port in insertion order (model
  behaviour, unchanged): the ActionFringe test compares each codelet's args as a sorted set.
  ActionFringe is stochastic (ConvulseEnd); with `srand(20)` on both sides the port matches.
- Details text in the popup and hover tooltips are modern additions (Perl shows neither); the
  Workspace_Attention part never draws relations (it dies), so they have no hover target there.
### Screenshots (looked at perl/ and screens/ `list_popup_groups.png`, and screens/`hover_tooltip.png`)
- Popup — same: the seven buttons in the same order, stacked, centred at natural width.
  Differences: Qt adds the details lines on top ("Group <0, 2> [1, 2, 3]", span/strength,
  categories, "locked against deletion"); Perl's buttons are raised bold Tk buttons packed tight,
  Qt's are styled and spaced; my first Qt shot had full-width buttons (fixed: AlignHCenter).
- hover_tooltip (no Perl counterpart): the Workspace view of groups_relations with the tooltip of
  the largest group ("Group <0, 5> [[1, 2, 3], [2, 2, 2]]", "Span 6, strength 32.80",
  "Categories: none") drawn next to the pointer. (A real QToolTip can't be grabbed offscreen; the
  test paints a label with the same text into the window grab.)
### Blockers
- None.
### Next
- Item 021 (Entry point): `python3 -m seqsee --gui` / `python3 -m seqsee.gui` with Seqsee.pl's
  options; create the window with `settings=mainwindow.default_settings()` and pass seed /
  max_steps / view in `options`; clear error if PySide6 is missing.

## Iteration 22 — 2026-10-07 06:45
### Completed
- Item 021, the entry point (Seqsee.pl with Tk: `_read_config(_read_commandline())`,
  INITIALIZE's init_display = lib/SGUI.pm `setup` with config/<gui_config>.conf, SGUI::ask_seq
  without --seq, MainLoop; lib/Seqsee.pm's GetOptions `gui_config=s`, `gui=s`):
  - `oracle/gui_entry.pl` → `tests/golden/gui_entry.json` (16 cases): `launch` — the real
    Seqsee.pl (`do './Seqsee.pl'`) with `*Tk::MainLoop` replaced before Seqsee.pl's `use Tk`
    imports it: $Global::Options_ref, the elements, whether ask_seq's window opened, steps;
    screenshot `docs/gui/perl/entry_window.png` (WinPhoto of $SGUI::MW); `launch_no_seq`
    (INITIALIZE again with `--seed 7` only; PadWalker sets Seqsee.pl's file-level
    $OPTIONS_ref); 11 command lines parsed (`options`: seq, max_steps, update_interval, view,
    gui_config, the seed if given, features, non-options left); `setup` per gui_config
    (GUI_ws3 dies — no [frames]; nosuch dies "Can't open config file"; GUI_sparse ok).
  - `gui/app.py` (new; no Qt at import): `parse_args(argv, check_config=True)` → `GuiArgs`
    (options, window_options, runner_defaults, seq, rest), `check_gui_config`,
    `missing_pyside6`, `build(args, settings=, runner=)`, `main(argv, err, settings, run)`.
    `gui/__main__.py`: `python3 -m seqsee.gui`.
  - `cli.py`: `split_gui_flag` (a bare `--gui` is taken out; `--gui X` / `--gui=X` stay for
    Getopt = gui_config, as in Perl; both ask for the GUI; case-insensitive; stops at `--`);
    `main` hands such a command line to `importlib.import_module("seqsee.gui.app").main`.
    Headless runs unchanged (test_cli passes; a subprocess test checks a headless run loads
    neither seqsee.gui nor PySide6).
  - `gui/runner.py`: `Runner(defaults=)` — read_config options for fresh runs that don't give
    them (update_interval, gui_config, a given view) and the -f features turned on again after
    `reset_all` (which clears Global.Feature), with debugMAX as _read_commandline does.
  - `tests/test_gui_entry.py` (46 tests, ~5 s): split_gui_flag cases; cli dispatch; headless
    unchanged; core/app import without Qt (subprocesses); missing PySide6 through both
    `python3 -m seqsee --gui` and `python3 -m seqsee.gui` (subprocess with
    `sys.modules['PySide6'] = None`: exit 1, message, no traceback) and in-process; every
    `options` golden case vs parse_args; window_options; gui_config vs the `setup` cases; usage
    errors (bad --seq, GUI_ws3, `--gui nosuch`, --json/--continuation/--answer, unknown -f) →
    exit 2 "seqsee: …"; a real launch vs the `launch` case (run options, fields, view,
    elements, no steps, no ask_seq) and `launch_no_seq` (ask_seq window, seed field, no run);
    a launched run stops at --max-steps with --update-interval; -f features survive the fresh
    run and -f debugMAX checks m; `main` shows the window, runs a stand-in event loop, saves a
    temporary IniFormat settings file, and uses `mainwindow.default_settings()` by default;
    screenshot `screens/entry_window.png`.
  - Updated: test_gui_infra.py `test_core_never_imports_gui` is now AST-based (import
    statements and import_module string literals) and allows exactly one GUI import in the
    core: `import_module("seqsee.gui.app")` inside `cli.main`; test_gui_render.py
    (gui_entry isn't a drawing golden); gui_screens CONTROL_SCREENS (+ entry_window).
    PORTING_MAP: a "Seqsee.pl with Tk" row.
  - Checked by hand: `QT_QPA_PLATFORM=offscreen python3 -m seqsee --gui --seq "1 1 2 1 2 3"`
    stays in the event loop (killed after 6 s); `python3 -m seqsee.gui --gui_config GUI_ws3`
    → exit 2 with the message; `python3 -m seqsee --seq "1 2 3" --max-steps 5` → the headless
    report as before.
- Tests written first; they failed (ImportError on `seqsee.gui.app`). Full suite: 6479 passed (~2:17).
### Decisions
- **Architecture deviation (the only one)**: the core package mentions the GUI in one place,
  a lazy `importlib.import_module("seqsee.gui.app")` in `cli.main`, reached only with `--gui`
  (the item asks for `python3 -m seqsee --gui`). The core still imports and runs without
  PySide6; the architecture test enforces that nothing else imports the GUI.
- **`--gui` spelling**: Perl's `--gui` takes a value (it is `--gui_config`). A bare `--gui`
  (last, or followed by an option) is our switch and is removed before Getopt; `--gui X`
  keeps Perl's meaning (gui_config X, with `||=`: `--gui_config GUI_ws3 --gui GUI_sparse`
  gives GUI_ws3, as in the golden) and also launches the GUI.
- **gui_config**: only a config/<name>.conf with a [frames] `frames` list is a window layout
  (only GUI_sparse; GUI_ws3.conf is the drawing layout and SGUI::setup dies on it). Others are
  usage errors; the window's layout is always GUI_sparse's (its buttons/bindings are built in,
  items 015–017), so a hypothetical other [frames] file would not change the window.
- **Window options**: only what the command line gives (seed, max_steps or -n, view) goes to
  the window, so it beats the remembered QSettings, and no `--seed` leaves the seed field
  empty (random per run; read_config's random load-time default is not shown in the field).
  The sequence, update_interval and gui_config also go along (informational).
- **Launch with a sequence** = `MainWindow.start_sequence` (Runner.new_sequence: reset,
  srand(seed), INITIALIZE), no steps — Seqsee.pl waits in MainLoop for Start too. Without a
  sequence: ask_seq's window; the run is set up at accept (Perl ran INITIALIZE with an empty
  sequence at once; same visible state: an empty workspace, as item 018 recorded).
- Exit statuses: 2 usage error (as the CLI), 1 PySide6 missing, else the event loop's.
  The default event loop sets SIGINT to SIG_DFL so Ctrl+C in the terminal quits (without
  saving the layout).
- `parse_args` calls `s.load(); s.reset_all()` before Getopt, as `cli.run_headless` does
  (PossibleFeatures for -f); an unknown -f (Perl prints "No feature X. Typo?" and exits) is a
  usage error with that line.
### Screenshots (looked at perl/ and screens/ `entry_window.png`)
- Same: the six elements 1 1 2 1 2 3 across the upper workspace, the last-runnable red
  square in the middle, "No entries to show." with a second red square at the bottom left
  (view 0, the Groups list empty), the Commentary below with four blank buttons and
  Start Debug, count 0.
- Differences: Perl's Start/Pause/Quit button row sits above its in-widget View/Save/Help
  menustrip; Qt has a menu bar (View, Panes, Run, Save, Help, Sleep slider), a toolbar with
  more actions, the Seed 7 / Max steps 40 / Restart row and a status bar (Steps 0,
  Elements 6, Groups 0, Idle). Perl's window is 810 × 700 (GUI_sparse geometry) and its
  canvas 780 × 450 with a margin; Qt's canvas spans the window width. Commentary: Perl's has
  scroll bars and a grey text area, Qt's a white text area with a dock title.
### Blockers
- None.
### Next
- Item 022 (End-to-end GUI run): drive the window (e.g. `app.build(app.parse_args([...]))`)
  through a full run on `1 1 2 1 2 3` with an automatic answerer; screenshots of all 11
  views; Perl screenshots of the same views from a deterministic state.

## Iteration 23 — 2026-10-07 07:03
### Completed
- Item 022, the end-to-end GUI run (Seqsee.pl with Tk: Interaction_continue; the questions of
  lib/Seqsee.pm's ask and UI/Graphical.pm's ask_user_extension in Tk/SCommentary.pm;
  DescribeSolution's confirmation; Tk/Seqsee.pm's 11 @ViewOptions at the end):
  - `gui/autoanswer.py` (new, pure): `asked_terms(text)` (the integers after the last
    "term"/"terms"), `Answerer(known).decide(kind, text, choices, element_count)` (terms vs the
    known terms after the elements on screen, as cli's `--continuation`; beyond them → "no",
    remembered in `beyond_known`; other boolean questions "yes"; a response its first choice /
    "Yes" to the solution question; more terms = the next known terms up to the next one that
    doesn't go up), `SOLUTION_QUESTION`.
  - `gui/qt/autoanswer.py` (new): `AutoAnswerer(window, known, pause_on_solution=True,
    keep_going=True)`: on each runner question (connected after the window's slots, answered
    from a 0 ms timer) it clicks the Commentary button with the answer's text, or types into
    the more-terms window and presses Return; presses Pause before confirming the solution;
    presses Continue again when a `continue` command ends before the solution (until max
    steps); records `answers`, `accepted`, `continues`, `errors`.
  - The `solution` recipe (tests/gui_recipes.py + oracle/GuiRecipes.pm), modelled on the
    run's end state: 1 1 2 1 2 3 1 2 3 4 1 2 3 4 5, ascending blocks (1 2), (1 2 3),
    (1 2 3 4), (1 2 3 4 5), e0 squinted, BIG = (e0 + the 4 blocks) with a MappingBased category,
    strengths, Hilit 2 on the last two blocks, no relations, slipnet (succ, ascending, number,
    mountain), 7 codelets + a history of 8 families, a stream of 4 thoughts, 677 steps,
    DescribeSolution as last runnable. Added to oracle/gui_views.pl's @RECIPES (golden
    regenerated: 231 cases, +11, all pass in test_gui_views.py the first time) with the 11
    Perl screenshots `docs/gui/perl/views_<v>_solution.png`; gui_screens.SCREENS has them,
    so test_gui_render renders their golden items to `docs/gui/screens/views_<v>_solution.png`.
  - `tests/test_gui_e2e.py` (13 tests, ~1 s): the answerer's parsing and decisions; the
    answerer on a window with a fake runner (buttons pressed, more terms typed, Pause before
    "Yes"); **the full run**: `app.build(app.parse_args(["--seq", "1 1 2 1 2 3", "--seed",
    "7", "--max-steps", "3000"]))`, `run_command("continue")`, the answerer answers through
    the Commentary: "Is the next term 4?" no, "Are the next 4 terms 1, 2, 3, and 4?" yes,
    "Are the next 5 terms …" yes, the solution confirmed, Paused; the largest group spans all
    15 elements with structure `[1, [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]`;
    count/status labels/commentary show it; each of the 11 views composes what
    `views.compose` gives for the final snapshot (only 'Workspace + Rules' dies) and is saved
    (`docs/gui/screens/e2e_run_<v>.png`, plus `e2e_run_window.png`); then the same run
    headless (`cli.run_headless(... --continuation ...)`) gives the same elements, steps (677)
    and questions; and the `solution` recipe has the run's elements, groups (bounds +
    structure), steps, last runnable and (no) relations. A test checks the run screenshots
    exist.
  - PORTING_MAP: an "End-to-end GUI run" row (done).
- Tests written first; they failed (ImportError on `seqsee.gui.autoanswer`). Full suite passes
  (~2:05).
### Decisions
- **Continue after each verified extension**: the model ends the run command itself after a
  "yes" (DoInsertBookKeeping and SWorkspace's rule-app check set Break_Loop); Perl's user
  presses c again, cli.run_headless loops on interaction_step_n. So the answerer presses
  Continue again (`keep_going`); the first run stopped at step 490 without it.
- **Pause before "Yes"**: with Pause pressed while the confirmation waits, the step that asked
  ends the run (the runner's pause re-sets Break_Loop after the step), exactly where the
  headless run stops (cli's accept hook sets Break_Loop) — so the GUI run and the headless run
  agree step for step (677).
- The answerer judges from the question's text and the snapshot's element count only (as a
  user sees them), never from model objects. It is a test helper: no command-line option uses
  it (`--continuation` stays headless-only).
- The Perl side is a deterministic recipe, not a replay (Perl can't follow the Python
  trajectory); it copies the run's end state where the drawings show it. Its stream fringes are
  strings (the run's are objects, drawn as refs).
- No model hook was needed.
### Screenshots (looked at perl/ `views_0/2/4/5/9_solution.png`, screens/ `views_0_solution.png`,
  `e2e_run_0/2/4/5/9.png` and `e2e_run_window.png`)
- Same in all: the big green group across the workspace holding the four cyan ascending
  blocks and the squinted first element, thick blue hilit borders, the element numbers;
  view 0's slipnet circles (number small, succ and ascending large, mountain a dot), the Groups
  list (74.40 <0, 14> MappingBased, then the four blocks by strength) with its red squares and
  "Page #1. Entries 1-5 of 5 Shown."; view 2's "Seqsee::SCF::DescribeSolution" label and the
  black attention area with mint groups; view 4's three category rows (Gp based on [ascending]…,
  ascending, number) with blue ovals; view 5's coderack with blank family names (no family
  defines $NAME, the known PERL-QUIRK), blue urgency and red history bars; view 9's stream
  boxes with the thick-bordered current thought on BIG.
- Differences between the Perl recipe and the run (state, not drawing): the run highlights all
  four blocks and the first element (Hilit 2), the recipe the last two; the run's slipnet
  shows two more nodes ("[ascending] start => same;end", "Gp based on [ascending] start"); the
  run's category name is "…start => same;end => succ;length => succ" (the recipe's mapping
  only changes `end`); the run's attention is on BIG and the block (1 2 3), the recipe's on BIG
  and D; the run's coderack has 19 families (a second column) and its stream 10 older thoughts
  whose fringe components are objects (drawn as `HASH(0x…)` refs, as Perl would).
- Rendering differences, as recorded before: Qt draws the fonts at their real sizes (Perl's
  PostScript PNGs use a small fallback font), so long texts run further right (the Groups
  list's category text crosses the red square; stream titles overflow their boxes); Qt's
  canvas background is Tk's grey in the run shots (white in the golden renderings, like
  PostScript); real addresses in the run shots, ADDR in the golden renderings.
- e2e_run_window: the whole window at the end: menus, toolbar, Seed 7 / Max steps 3000,
  count 677, view 0, and the Commentary log ending "Are the next 5 terms 1, 2, 3, 4, and 5?
  yes / I believe I got it / I will describe the solution now! / The sequence consists of the
  blocks 1; [1, 2]; [1, 2, 3]; [1, 2, 3, 4]; [1, 2, 3, 4, 5] / RULE DESCRIPTION CURRENTLY
  BROKEN!! / That finishes the description! / Does this generate the sequence you had in
  mind? Yes", status "Steps: 677 Elements: 15 Groups: 5 Paused".
### Blockers
- None.
### Next
- Item 023 (Responsiveness): event-loop latency while `continue` runs, snapshot + render
  ≤ 50 ms per frame for a 30-term workspace, Pause within one batch. The e2e run (seed 7,
  ~1 s for 677 steps) is a ready-made workload; `AutoAnswerer(keep_going=True)` keeps it going.

## Iteration 24 — 2026-10-07 07:20
### Completed
- Item 023, responsiveness (no Perl counterpart: in Seqsee.pl the model runs inside Tk's event
  loop, Interaction_step_n redraws through update_display every update_after steps and Pause's
  Break_Loop is read after each step):
  - `tests/test_gui_responsive.py` (9 tests, ~3.5 s), written first; they failed
    (AttributeError: no `FRAME_BUDGET`, `queue_snapshot`, `snapshot_times`; the latency and
    pause tests failed on the measurements, see Decisions):
    - frame cost: a real 30-term run (`1 1 2 1 2 3 … 1 2`, seed 3, 300 steps, answers "no":
      30 elements, 8 groups, 27 relations); each of the 11 views, median of 5 frames of
      snapshot + compose + render + paint ≤ `mainwindow.FRAME_BUDGET` (50 ms); then view 0 with
      every pane dock shown as well. Measured: snapshot ~0.9 ms, frames 2.6–5.6 ms per view.
    - the runner's `snapshot_times` (median within budget).
    - coalescing: three queued snapshots → one frame, `frames_dropped` 2; `show_snapshot`
      drops a queued older one; a question, and a state change, draw the pending snapshot
      first; the runner's snapshots go through the queue.
    - event-loop latency: `continue` on the 30 terms, 1500 steps, `--update-interval 1` (a
      display every step, ~30 frames/s, the heaviest load), AutoAnswerer answering; a 10 ms
      probe timer and an event posted every 50 ms. Bounds: longest stall ≤ 150 ms, 95th
      percentile lateness ≤ 50 ms, posted events ≤ 150 ms, ≥ half the expected ticks, frames
      drawn during the run, median frame ≤ budget. Measured (3 runs): longest stall 34–39 ms,
      p95 14–15 ms, posted ≤ 10.5 ms, frames median ~11 ms (max ~36), 0 dropped.
    - Pause during `continue` with update_interval 1 and 25: the run stops ≤ 2 steps after
      the click (the running step, plus one that may end between reading the step and the
      click — within one batch), "Paused" within 1 s, the window shows the step where it
      stopped, nothing runs afterwards.
  - `gui/qt/mainwindow.py`: `FRAME_BUDGET`, `frame_times` (last 100 frame costs),
    `frames_dropped`, `queue_snapshot` (newest only, drawn by a 0 ms single-shot timer),
    `flush_snapshot` (called by `show_question`, `question_closed`, `_state_changed`,
    `_command_done`); `attach_runner` connects `snapshot` to `queue_snapshot` and sets
    `runner.pace_frames`; `show_snapshot` paints the visible canvases at once
    (`viewport().repaint()`), times the frame, and calls the runner's `frame_drawn` from a 0 ms
    timer (after the scene's own queued work).
  - `gui/runner.py`: `pace_frames` / `frame_drawn()` / `FRAME_WAIT` (0.25 s): with pacing, the
    step after a snapshot waits until the GUI has drawn it; `_interrupt` releases the wait;
    `SWITCH_INTERVAL` (sys.setswitchinterval 0.001 while the runner runs; `quit` restores it);
    `snapshot_times`.
  - `gui/qt/autoanswer.py`: no Continue after a command that the user's Pause ended
    (`run_state == "paused"`) — the pause test found it pressing Continue again.
  - test_gui_mainwindow.py `test_attach_runner_shows_its_snapshots_and_errors`: a snapshot
    from the runner is now drawn at the next event-loop turn (the test flushes it).
  - PORTING_MAP: a "Responsiveness" row (done).
- Full suite: 6522 passed (~1:35), twice in a row. The first full run failed the latency
  test once: one 171 ms stall (all other ticks ≤ 28 ms), not seen when the file runs alone.
  It is a full garbage collection over the heap that thousands of earlier tests leave behind
  (a gen-2 pass holds the GIL; even in isolation one took 18 ms). The test now calls
  `gc.collect(); gc.freeze()` before the measured run and `gc.unfreeze()` after it. The GUI
  code does not tune GC; a long session with a large model heap can still see such a pause.
### Decisions
- **The cause of the stalls was the GIL, not drawing cost.** Before the change, a frame that
  costs 4 ms alone took 77–411 ms during a run and the event loop stalled for 100–700 ms. A
  bisection (scratch tests, deleted): the model stepping on a thread with the window idle, or
  with snapshots only stored, leaves 10 ms ticks on time; rendering into the scene (even
  without our Python TextItem) freezes the loop. Every Qt virtual call on a Python-created
  scene/item (paint, boundingRect, event, …) takes the GIL, so the GUI thread hands the GIL
  back and forth with the worker for each one (a convoy). Remedy: **frame pacing** — the
  worker waits at the next step boundary until the frame it sent is drawn — plus a 1 ms GIL
  switch interval while the runner runs. Calling `frame_drawn` from a 0 ms timer (rather
  than directly) lets the scene's queued follow-up work run while the worker still waits:
  that took the longest stall from ~34 ms per frame period to ≤ ~24 ms in the bisection.
- Cost: with a display every step the model runs slower while the window draws (1500 steps:
  ~0.45 s without a window, ~1.9 s with frames at 30/s). With the default update_interval
  the frames are few and the cost small. Perl drew synchronously in the same thread, so this
  is no worse than Perl. The steps themselves are unchanged (same trajectory: the e2e test
  still matches the headless run step for step).
- `FRAME_WAIT` bounds the wait so a GUI that never draws (no window attached, a fake runner)
  can't stall the model for long; only `attach_runner` turns pacing on, so plain-runner tests
  and headless runs never wait.
- "Pause within one batch": the runner already stopped after the running step (item 003);
  the test pins it for update_interval 1 and 25. A single very long step still can't be
  interrupted (item 003's note).
- No screenshot: this item draws nothing new.
### Blockers
- None.
### Next
- Item 024 (Final audit): every Perl GUI module has a PORTING_MAP row `done`/`skipped`; no
  `NotImplementedError("TODO` in seqsee/gui; a GUI section in python/README.md (launching,
  controls, screenshots); full suite; then LOOP_COMPLETE.

## Iteration 25 — 2026-10-07 07:34
### Completed
- Item 024, the final audit:
  - `tests/test_gui_audit.py` (new, 9 tests), written first; 6 failed before the changes:
    every file under lib/ of the Perl GUI (SGUI.pm, SGUI/**, Tk/**, Themes/**, SColor.pm,
    UI/Graphical.pm, **including non-.pm files**) has a PORTING_MAP row marked done/skipped;
    skipped rows give a reason; every GUI row (also the rows with no Perl module: renderer,
    runner, snapshot, shell, …) is done/skipped; no `NotImplementedError("TODO` in
    seqsee/gui; README.md no longer says the GUI is not ported and has a `## GUI` section with
    the launch commands (`python3 -m seqsee --gui`, `python3 -m seqsee.gui`), PySide6, the main
    options, `QT_QPA_PLATFORM=offscreen`, every key of `controls.BINDINGS`, the five menus,
    the 11 view titles, Sleep / Restart / PNG / SVG / Commentary, and screenshots from both
    docs/gui/screens/ and docs/gui/perl/ that exist; pyproject.toml has a `gui` extra and
    pytest-qt in `test`.
  - `tests/test_porting_audit.py`: `test_every_row_is_done_or_skipped` no longer lets GUI rows
    be todo/partial (its docstring said item 024 would remove the allowance).
  - The audit found one Perl GUI file without a row: `lib/Tk/images/foo.xbm` (a 10 × 10 X
    bitmap). Nothing uses it (no Perl file, config or GUI module names it, an `images/` path
    or a `-bitmap` option). It now has its own PORTING_MAP row, `skipped`, with that reason.
    All the other GUI rows (SGUI.pm … UI/Graphical.pm and the module-less GUI rows) were
    already `done`. There were no TODO stubs in seqsee/gui.
  - README.md: the intro now points to the GUI; requirements (PySide6 via
    `pip install -e .[gui]`, pytest-qt; GUI tests skipped without PySide6); a new `## GUI`
    section: what it is, how to launch it and its options, the window screenshot
    (`e2e_run_window.png`), Controls (the key table, View with the 11 views, Panes, Run with
    Restart, Save PNG/SVG, Help, Sleep, the count and status bar, the Commentary and its 1–4
    keys and Start Debug, more terms, list clicks, hover, remembered layout, the worker
    thread), Screenshots (Perl vs Qt views 0 and 2 of the `solution` recipe, the docked panes),
    how the GUI tests and oracles run, and the code layout. The Layout table has
    `src/seqsee/gui/` and `docs/gui/` rows.
  - pyproject.toml: `gui = ["PySide6"]`; `test` gains `pytest-qt` (metadata only: no new
    dependency, both were already allowed and installed).
- Full suite: 6531 passed (2:32).
### Decisions
- The GUI audit walks every file under the GUI directories, not only `*.pm`, so a resource
  (the bitmap) needs a row too; test_porting_audit's general check stays on `*.pm`.
- README screenshots: Perl and Qt images of the same deterministic state (views_0/2_solution)
  side by side, as the most direct comparison; looked at e2e_run_window.png,
  perl/views_0_solution.png and window_docks.png again: as recorded in iterations 18 and 23
  (same shapes and colours; Perl's PostScript fallback font is smaller; Qt shows real
  addresses in run shots).
- No model or GUI code changed in this item.
### Blockers
- None.
### Next
- All items of loop0002 are `[x]`. The loop is complete.

LOOP_COMPLETE
