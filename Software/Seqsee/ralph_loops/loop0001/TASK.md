# TASK: Test-driven translation of Seqsee (Perl) to Python

## Philosophy
- **Faithful port, not a redesign.** Reproduce the behaviour of the Perl code in
  `Seqsee/lib/`. Python idioms are welcome; changed semantics are not. If the
  Perl looks buggy, port it faithfully and note it in PROGRESS.md (and in a
  `# PERL-QUIRK:` comment).
- **Tests first, against the real Perl.** The original Perl runs (see "Perl
  oracle" below). For each item: write an oracle script that exercises the
  Perl module and records its outputs as golden JSON. Write pytest tests that
  compare the Python port against that golden data, plus tests from reading
  the source. The tests must fail first; then implement until green.
- **Perl is the source of truth.** If your reading of the source disagrees with
  what the oracle reports, the oracle wins. Investigate and note it.
- **Small, green steps.** ONE item from `iterations.md` per iteration. The
  full test suite must pass when you finish.
- **Headless.** No Tk/GUI. GUI hooks become no-ops or logging calls.

## Current Focus
Work through `iterations.md` top to bottom. The next unchecked item is the
current one (the loop runner tells you which).

## Acceptance Criteria (per item)
- [ ] An oracle script `python/oracle/<name>.pl` and its golden file
      `python/tests/golden/<name>.json` (generated, never hand-edited) cover the
      module's deterministic behaviour, where it can be exercised in isolation
- [ ] New/updated tests in `python/tests/` cover the ported behaviour, including
      the golden cases; each test module's docstring names the Perl source
      file(s) it mirrors
- [ ] Implementation in `python/src/seqsee/`
- [ ] `cd python && python3 -m pytest` passes (whole suite, no regressions)
- [ ] `python/PORTING_MAP.md` row(s) updated
- [ ] Item marked in `iterations.md`: `[x]` done, or `[!]` blocked (with reason)
- [ ] An iteration section appended to `PROGRESS.md`

## Completion Conditions
An item is DONE when either:
1. **Solved**: meets all criteria, OR
2. **Blocked**: marked `[!]` with the specific blocker written in PROGRESS.md

When every item is `[x]` or `[!]` and the final audit item has been done, append
`LOOP_COMPLETE` on its own line at the end of PROGRESS.md.

## Context
- **Perl source**: `Seqsee/lib/` (~17k lines, 125 modules). `S.pm` imports
  everything and defines constant packages (`DIR`, `POS_MODE`, `METO_MODE`,
  `EXTENDIBILE`, `RELN_SCHEME`, `DISTANCE_MODE`, `DISTANCE`). Entry point:
  `Seqsee.pl` → `lib/Seqsee.pm`. Config: `Seqsee/config/`.
- **Python target**: `Seqsee/python/` with `src/seqsee/` (package) and `tests/`.
  Use the **base anaconda Python** (`python3`, 3.12). Use only pytest and
  hypothesis for tests and the standard library for runtime code. Ask before
  adding any other dependency (record the need as a blocker instead).
- **Testing**: `cd /home/al/dev/SeqSee/Seqsee/python && python3 -m pytest`
- **Constraints**: never modify anything outside `python/` and this loop
  folder. Never edit the Perl code. Don't run git commands that change state;
  the loop runner commits.

## Perl oracle
The Perl dependencies are installed (apt packages plus CPAN modules in
`~/perl5`). Always run Perl through the wrapper, which sets `PERL5LIB`, adds
`lib/` and `oracle/` to `@INC`, fixes the hash seed, disables ASLR, and runs
from the repo root:
- `python/oracle/run_perl.sh -e 'use S; ...'`: quick experiments
- `python/oracle/run_perl.sh python/oracle/foo.pl`: run a script
- `python/oracle/<name>.pl` uses `Oracle.pm` (`record(k => v, ...)`,
  `dies(sub {...})`, `emit()`) and prints a JSON list of cases.
  `cd python && python3 oracle/regen.py <name>` writes
  `tests/golden/<name>.json`. Tests read it with `golden.load("<name>")`
  (`tests/golden.py`). See `oracle/spos.pl` for an example.
- Record **observable values only** (numbers, strings, booleans, lists of
  those, attribute values, and the `get_name`/`as_text`-style descriptions the
  Perl provides). Never record stringified refs (`Foo=HASH(0x…)`); addresses
  differ between Perl and Python.
- Some modules need global setup first. Look at how `lib/Test/Seqsee.pm`
  (`INITIALIZE_for_testing`, `RunSeqsee`) and `t/spos.t` load things;
  `use S;` loads almost everything.
- **Randomness matches exactly.** Perl's `rand()` is drand48 (since 5.20):
  `srand(s)` sets `x = (s << 16) | 0x330E`, and each draw is
  `x = (0x5DEECE66D*x + 0xB) mod 2**48` returning `x / 2**48`. Port this as the
  central RNG in `seqsee/util.py`. Then seeded oracles (`srand(42); ...`) can
  be matched draw for draw, as long as no Perl hash-iteration order is
  involved. Where hash order matters, compare as sets or distributions.
- **End-to-end runs**: `python/oracle/run_perl.sh python/oracle/e2e_run.perl
  SEED "SEQ" "CONTINUATION" MAX_STEPS` prints one JSON result line (status,
  steps). It is deterministic for a given seed. Python can't reproduce the
  exact trajectory (hash order and object addresses differ), so compare whole
  runs statistically: success rate and step distributions over many seeds.
- Never modify `lib/`. If Perl itself crashes on something, record that as a
  PERL-QUIRK.

## Porting conventions
- Module naming: snake_case mirroring the Perl hierarchy, e.g.
  `SCategory::Ascending` → `seqsee/categories/ascending.py`,
  `Mapping::Numeric` → `seqsee/mapping/numeric.py`,
  `Seqsee::SCF_MX::General` → `seqsee/codelets/general.py`. Record each choice
  in PORTING_MAP.md and keep it consistent.
- Keep Perl method names recognisable: `get_strength` stays `get_strength`;
  CamelCase methods like `FindExtension` become `find_extension`. Keep the
  Perl name in the docstring so people can grep for it.
- Moose / Class::Std classes → plain classes or `dataclasses`. Required
  attributes → required constructor args. Validation failures → exceptions.
- `Class::Multimethods` → `functools.singledispatch` or an explicit
  type-dispatch table.
- `Memoize` → `functools.cache`, plus a reset hook if Perl clears it.
- `Exception::Class` / `SErr::*` → one exception hierarchy in `seqsee/errors.py`.
- Randomness: ALL random draws go through one central, seedable RNG in
  `seqsee/util.py` (port of SUtil). Never call the `random` module elsewhere.
- Global state (`$Global::…`, `S::` singletons, the workspace, the coderack,
  LTM) → module-level singletons with an explicit `reset()`/`init()` so tests
  can isolate state. Add an autouse fixture in `conftest.py` once there is
  global state to reset.
- `Codelet_Family(...)` (MooseX::SCF) → a registry plus a decorator, e.g.
  `@codelet_family("AttemptExtensionOfGroup", attributes=("object", "direction"))`.
- Circular dependencies: if the module you're porting needs something not
  yet ported, port the minimal piece needed, or add a stub that raises
  `NotImplementedError("TODO: iteration NNN")`. Use lazy imports to break
  import cycles.
- Stochastic behaviour: match seeded Perl draws exactly where possible;
  otherwise test statistically (many trials, generous tolerance). Look at
  `Test::Stochastic` usage in the Perl.

## Important Notes
- Read PROGRESS.md first. Earlier iterations record decisions you must follow.
- Keep the iteration focused. If an item turns out too big, finish a coherent
  part, then split the rest into a new item inserted directly after it in
  `iterations.md` (e.g. `031b`) and mark the original `[x]`.
- If you find missing work that belongs earlier, add a new item rather than
  expanding the current one.
