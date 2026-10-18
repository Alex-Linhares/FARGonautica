# Iterations — Seqsee Perl → Python

Format: `- [ ] NNN Title — Perl sources — notes`. `[x]` = done, `[!]` = blocked.
The loop runner picks the first `[ ]` item. Paths are relative to `Seqsee/lib/`.

## Phase 0 — Perl reference
- [x] 000 Perl end-to-end baseline — `python/oracle/baseline.py`: run oracle/e2e_run.perl for every line of config/sequence_list_for_testing × seeds 1..10 (MAX_STEPS 10000, in parallel with a process pool; strip `Label:` prefixes) → `python/tests/golden/e2e_baseline.json` (per sequence: statuses, steps, success rate). Add a small pytest that checks the file's shape. Record the summary table in PROGRESS.md

## Phase 1 — Foundations
- [x] 001 Constants & enums — S.pm packages DIR, POS_MODE, METO_MODE, EXTENDIBILE, RELN_SCHEME, DISTANCE_MODE, DISTANCE (S.pm:86–end) — `seqsee/constants.py`; category/metonym singletons in S.pm come later (item 046)
- [x] 002 Errors — SErr.pm, Carp/Seqsee.pm, plus every `SErr::…` class referenced anywhere (grep) — `seqsee/errors.py`
- [x] 003 Utilities + central RNG — SUtil.pm — `seqsee/util.py`: drand48 RNG identical to Perl's rand/srand (see TASK.md), then SUtil functions; seeded oracle must match draw for draw (toss, choose, etc.); update conftest `seeded` fixture to seed it
- [x] 004 Weighted choice — SChoose.pm, Set/Weighted.pm — seeded oracle, exact match
- [x] 005 Positions — SPos.pm — translate t/spos.t verbatim into pytest
- [x] 006 Small value types — SInt.pm, SFasc.pm, SHistory.pm
- [x] 007 Global state — Global.pm (SStream2 dependency → stub until 039) — global state with reset()
- [x] 008 Bindings — SBindings.pm

## Phase 2 — Categories & metonyms
- [x] 009 Category base — Categorizable.pm, SCategory.pm
- [x] 010 Simple numeric categories — SCategory/Number.pm, Prime.pm, Odd.pm, Even.pm, Numeric.pm
- [x] 011 Sequence categories I — SCategory/Sameness.pm, Ascending.pm, Descending.pm
- [x] 012 Sequence categories II — SCategory/Mountain.pm, Interlaced.pm
- [x] 013 Alternating category — SCategory/Alternating.pm
- [x] 014 Mapping-based categories & metonymy spec — SCategory/MappingBased.pm, MetonymySpec*.pm
- [x] 015 Metonyms — SMetonymType.pm, SMetonym.pm

## Phase 3 — Mappings & relations
- [x] 016 Mapping base — Mapping.pm, Mapping/Dir.pm, Mapping/Position.pm
- [x] 017 Numeric mapping — Mapping/Numeric.pm
- [x] 018 Metonym-type mapping — Mapping/MetoType.pm
- [x] 019 Structural mapping — Mapping/Structural.pm
- [x] 020 Position structure — PositionStructure.pm

## Phase 4 — Workspace objects
- [x] 021 Seqsee::Object I — Seqsee/Object.pm (construction, items, categories, bindings)
- [x] 022 Seqsee::Object II — Seqsee/Object.pm (metonyms, structure, strength, remaining methods)
- [x] 023 Elements & anchored groups — Seqsee/Element.pm, Seqsee/Anchored.pm
- [x] 024 Relations — SRelation.pm, SRelation/Structural.pm
- [x] 025 Rules — SRule.pm, SRuleApp.pm
- [x] 026 Result objects — Seqsee/ResultOf*.pm

## Phase 5 — Long-term memory
- [x] 027 Activations — SNodeActivation.pm, SLinkActivation.pm
- [x] 028 Memory plumbing — SLTM/Platonic.pm, Memory/*.pm, LTMStorable*.pm
- [x] 029 SLTM I — SLTM.pm (nodes, links, activation, spreading, SpikeAndChoose)
- [x] 030 SLTM II — SLTM.pm (persistence: storing/loading memory, remaining methods)

## Phase 6 — Workspace
- [x] 031 SWorkspace I — SWorkspace.pm: clear/init/insert_elements, GetElements, bar lines, scanning
- [x] 032 SWorkspace II — SWorkspace.pm: add_group/remove_gp, conflicts, bookkeeping, supergroups, liveness
- [x] 033 SWorkspace III — SWorkspace.pm: distance/position helpers, sameness groups, check_at_location, rapid_create_gp, GetSomethingLike/LookForSomethingLike
- [x] 034 SWorkspace IV — SWorkspace.pm: choice distributions, strengths, saccade, FightUntoDeath, relations, remaining subs

## Phase 7 — Codelets, thoughts, stream
- [x] 035 Codelet machinery — SAction.pm, SCodelet.pm, SCodeletBase.pm, MooseX/SCF.pm, Seqsee/SCF.pm — family registry + decorator
- [x] 036 Coderack — SCoderack.pm
- [x] 037 Thoughts I — SThought.pm, SThought/SCat.pm, SThought/Relations.pm
- [x] 038 Thoughts II — SThought/SObject.pm
- [x] 039 Stream — SStream2.pm (replace the Global.pm stub)
- [x] 040 Codelet families: General I — Seqsee/SCF_MX/General.pm (first half of families)
- [x] 041 Codelet families: General II — Seqsee/SCF_MX/General.pm (second half)
- [x] 042 Codelet families: AllMX — Seqsee/SCF_MX/AllMX.pm
- [x] 043 Codelet families: AllMX2 & LargeGp — Seqsee/SCF_MX/AllMX2.pm, LargeGp.pm
- [x] 044 UI codelets & user interaction (headless) — Seqsee/SCF_MX/UI.pm, UserInteraction.pm — answers come from a pluggable callback (default: auto/no-op)
- [x] 045 Scripts — Seqsee/Scripts.pm, Seqsee/Scripts/DescribeSolution*.pm

## Phase 8 — Integration
- [x] 046 Global wiring — S.pm singletons (ASCENDING, DOUBLE, …), Sanity.pm, initialization order
- [x] 047 Main loop & config — Seqsee.pm (run, Seqsee_Step, Interaction_step_n, config/commandline), config/seqsee.conf, config/start_codelets.conf
- [x] 048 CLI — `python3 -m seqsee --seq "1 1 2 1 2 3" --seed N --max-steps M` (headless port of Seqsee.pl), reports the extension found
- [x] 049 Test harness — Test/Seqsee.pm, Seqsee/ResultOfTestRun.pm: stochastic helpers + RegHarness equivalent
- [x] 050 End-to-end parity — run config/sequence_list_for_testing in Python with the same seeds/steps as item 000; compare per-sequence success rate and step distributions with tests/golden/e2e_baseline.json; add pytest e2e tests (mark slow) asserting parity within tolerance; for significant gaps, log diagnoses in PROGRESS.md and add fix items after this one
- [x] 050b Deep-recursion headroom — Alternating `1 1 1 2 2 3 3 3 4 4 5 5 5 6 6` crashes with RecursionError in 10/10 Python runs (Perl: 0/10, 2/40 with an unrelated "Too few params"). FindMapping(Element, Element) → sameness FindMappingForCat → CalculateBindingsChange → FindMapping on `each` (an element is a length-1 sameness of itself) recurses until `number` gets spiked enough to win SpikeAndChoose; Perl goes up to ~90–490 levels, but Python's limit (~12 frames per level) stops it at ~80. Give Seqsee's main loop/RunSeqsee/CLI enough headroom (e.g. run steps in a thread with a big stack and a raised `sys.setrecursionlimit`, and/or cut C-level frames per level such as `Multimethod.__call__`; with only a raised limit, 3.12's C recursion limit still hit at depth 3330, step 4005). Don't change the semantics. Then drop the KNOWN_GAPS xfail in tests/test_e2e_parity.py and re-check with `oracle/e2e_compare.py --seeds 40`
- [x] 051 Final audit — every non-GUI Perl module has a PORTING_MAP row marked done/skipped; no leftover `NotImplementedError("TODO` stubs (grep); full suite green; write python/README.md usage; then LOOP_COMPLETE
