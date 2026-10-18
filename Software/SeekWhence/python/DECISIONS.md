# Decisions: where the sources are silent

Every place where the thesis (and FCCA chapter 1, the historical notes) does not dictate what
the program does, and the choice this reconstruction makes. Code that implements a decision
says `DECISION D-NNN` in a comment or docstring; a test checks that every `D-NNN` in the code
has an entry here. All knobs live in one frozen dataclass, `seekwhence.config.Config`, each
field commented with its tag or decision number.

Each entry has five parts, in this order: **Sources** (what the sources say, with tags, and
where nothing was found), **Choice** (what the code does), **Reason** (why: the transcripts,
the thesis's words, FCCA's design intent, Seqsee, an engineering default, or the simplest
option, in that order of preference), **Knob** (the `Config` field, or "none") and
**Alternatives** (what else would fit the sources). Numbers are given in order of creation
and never reused. A decision made while a question for the author is open is marked "pending
the author".

The entries are grouped by the part of the program they concern: forms and boxes (D-001 to
D-006), the taskrack and the run (D-007 to D-014), the cytoplasm (D-015 to D-025), templates
(D-026 to D-029), gnoths and the hypothesis (D-030 to D-037), reformulation (D-038 to D-052),
and the user protocol (D-053 to D-055); decisions made while building the engine follow
under "Later additions" (D-056 to D-070, in the order the items met them). SPEC.md sections
8.8, 14.6 and 15.5 list the open points these answer, and each SPEC section names the modules
that implement it. The index below gives every entry's kind, knob and the modules that cite
it; `README.md` ("The knobs") lists the `Config` fields with their defaults.

## Index

Every decision, with its **kind**: *transcript-inferred* (read off the printouts of the three
appendix runs [M86 pp. 164-179], which are the program's own output; the entry gives the page),
*Meredith-silent* (the thesis's prose does not decide the point; the entry takes the simplest
reading of her words, or of the diagrams), *engineering choice* (the value or rule is a
default of the reconstruction's own, taken where the thesis is silent and nothing in the
transcripts decides it: a starting value, or the simplest rule that keeps the code testable),
or *extension* (machinery the reconstruction needs and the original program had no
need of: the typed events, the scripted user, the command line, the bench; each is off the
engine's path or reproduces the transcripts' protocol, and none changes what the engine does
for a given seed). The **knob** column names the `Config` field, and the last column the
modules of `seekwhence/` whose code says `DECISION D-NNN`. `tests/test_docs.py` checks that
the index lists every entry, every `Config` field and every decision the code cites, and that
every kind is one of the four. The kind is a judgement of where the deciding evidence came
from; entries amended by a later item (marked **Amended** in the entry) keep the kind of their
final reading.

| Decision | Title | Kind | Knob | Cited by |
|---|---|---|---|---|
| D-001 | The internal form has four slots; a fence form's start and length come from its elements | transcript-inferred | none | `forms`, `gloms`, `platoplasm` |
| D-002 | `shared` is written as the word, referring to the earliest identical structure to its left | Meredith-silent | none | `boxes`, `forms`, `platoplasm` |
| D-003 | A box reports its stored value while `ready`; a Cycle hits one element per hit | engineering choice | none | `boxes`, `gnoths`, `platoplasm` |
| D-004 | One frame of a Cycle hypothesis is one full turn | transcript-inferred | none | `hypothesis`, `slipping`, `templates` |
| D-005 | The repeater types have no process of their own on gloms | transcript-inferred | none | `platoplasm` |
| D-006 | A P-group that would go below 0 is an error, and a candidate that raises one does not postdict | engineering choice | none | `bench`, `boxes`, `hypothesis`, `platoplasm` |
| D-007 | Taskrack sampling rule | engineering choice | `urgency_power` | `config`, `events`, `rng`, `run`, `taskrack` |
| D-008 | The urgency of every task type | engineering choice | `urgencies` | `config`, `world` |
| D-009 | Stale tasks check their structures and do nothing; only a Dissolver says so | transcript-inferred | none | `bonds`, `casting`, `events`, `filtering`, `operations`, `reforms`, `run`, `slipping`, `world` |
| D-010 | Cleaning the taskrack | Meredith-silent | `cleaned_on_term`, `cleaned_on_reform`, `cleaned_on_hypothesis` | `bonds`, `casting`, `config`, `events`, `gnoths`, `slipping`, `taskrack`, `world` |
| D-011 | Run limits | engineering choice | `max_tasks_per_term`, `max_tasks` | `__main__`, `bench`, `config`, `events`, `run`, `session` |
| D-012 | When the program asks for the next term | transcript-inferred | none | `casting`, `filtering`, `gnoths`, `run`, `session`, `taskrack`, `world` |
| D-013 | `empty-task` | transcript-inferred | none | `events`, `run` |
| D-014 | Numbering and naming of structures | transcript-inferred | none | `cytoplasm`, `milestones`, `world` |
| D-015 | Sparkler-plus and its printed number | transcript-inferred | `sparkler_plus_urgencies` | `bonds`, `config`, `cytoplasm`, `gloms`, `world` |
| D-016 | How many Testers, Sparklers and Glom-scouts are released | transcript-inferred | `testers_per_spark`, `sparklers_per_bond`, `glom_scouts_per_bond`, `sparklers_per_term` | `bonds`, `config` |
| D-017 | Sparks on refutation | transcript-inferred | `sparklers_on_refutation` | `config`, `filtering` |
| D-018 | Bond types tested, bond families and modifiers | transcript-inferred | none | `bonds`, `cytoplasm` |
| D-019 | Bond strengths at the cyto level | Meredith-silent | `bond_type_weights`, `bond_modifier_weights`, `bond_field_weights`, `glom_bond_minimum` | `bonds`, `config`, `cytoplasm`, `events`, `gloms` |
| D-020 | The Glom-scout's choice, its three tests, the family label, and the Glomtester's urgency | transcript-inferred | `glomtester_test_weights`, `glomtester_family_weights`, `glom_bond_minimum` | `config`, `cytoplasm`, `events`, `gloms`, `rng`, `world` |
| D-021 | The Glomtester's extent, the Glommer's urgency, refinement and failure | Meredith-silent | none | `config`, `cytoplasm`, `events`, `gloms` |
| D-022 | Which plato-classes find a glom interesting | transcript-inferred | none | `gloms`, `operations` |
| D-023 | The Plato-evaluator's search for pure subcollections and secondary purity | Meredith-silent | none | `gloms`, `platoplasm` |
| D-024 | Bursters and Dissolvers: who posts them, and what a Dissolver may dissolve | transcript-inferred | none | `bonds`, `cytoplasm`, `gloms` |
| D-025 | The Sparkler's draw | transcript-inferred | none | `bonds`, `cytoplasm`, `milestones`, `rng` |
| D-026 | The Template-applier's fit and its loosening order | transcript-inferred | none | `templates` |
| D-027 | What the Template-evaluator checks | Meredith-silent | none | `events`, `templates` |
| D-028 | The Template-reviewer's "very simple fixes" | Meredith-silent | none | `templates` |
| D-029 | The Template-resolver's blast | Meredith-silent | none | `templates` |
| D-030 | The catchall gnoth is a gnoth object that `show` does not list | transcript-inferred | none | `events`, `filtering`, `gnoths`, `operations`, `reforms` |
| D-031 | How the Hypothesizer turns a template into a form | transcript-inferred | none | `events`, `gnoths` |
| D-032 | Validity growth and the guess trigger | transcript-inferred | `guess_validity` | `config`, `filtering`, `hypothesis` |
| D-033 | The guess block | transcript-inferred | `guess_min_hits` | `__main__`, `bench`, `config`, `events`, `filtering`, `hypothesis`, `session` |
| D-034 | Postdiction and the validity of a new candidate | transcript-inferred | none | `casting`, `events`, `gnoths`, `hypothesis` |
| D-035 | Freeze-dried hypotheses: the repeat check and who destroys the box | Meredith-silent | `reject_freeze_dried_repeats` | `casting`, `config`, `hypothesis`, `slipping` |
| D-036 | The Gnoth-setter's layout: frames, the cut-short last frame, reuse of gnoths, pseudo-gloms and numbering | transcript-inferred | none | `events`, `milestones`, `slipping` |
| D-037 | Casting under a fence class | transcript-inferred | none | `casting` |
| D-038 | The s-link table | engineering choice | `s_links` | `config`, `platoplasm` |
| D-039 | The pull and push tables of the eight classes | Meredith-silent | `bond_values` | `config`, `platoplasm` |
| D-040 | The strict classes | transcript-inferred | `strict_classes` | `casting`, `config`, `platoplasm`, `slipping` |
| D-041 | How "recent tries" enter the slip decision | transcript-inferred | `stay_factor_per_try` | `config`, `slipping` |
| D-042 | Which task makes and prints the slip check, and `best: nil` | transcript-inferred | none | `events`, `milestones`, `slipping`, `trace` |
| D-043 | The Reformulator's pull between gloms, "with each in the list", and the direction of a move | transcript-inferred | none | `events`, `reforms` |
| D-044 | PROGRAM construction for a deep move | Meredith-silent | none | `events`, `operations`, `reforms` |
| D-045 | When NO-OP is proposed and the Capper | Meredith-silent | none | `events`, `operations`, `reforms` |
| D-046 | When the threshold is cleared | engineering choice | `threshold_fraction` | `config`, `events`, `operations`, `reforms`, `slipping` |
| D-047 | The Bond-assessor's test | transcript-inferred | none | `events`, `reforms` |
| D-048 | Casting a gnoth and generalising the casts (group classes) | engineering choice | none | `casting`, `events`, `gnoths` |
| D-049 | The Slip-scout's effect | engineering choice | `slip_scout_factor` | `casting`, `config`, `events`, `slipping` |
| D-050 | "No progress for quite some time" and the blast | engineering choice | `max_cast_failures` | `casting`, `config`, `events`, `slipping`, `trace`, `world` |
| D-051 | Equivalence-type marking | Meredith-silent | none | `gnoths`, `session`, `slipping` |
| D-052 | The Hfilter line after a refused guess | Meredith-silent | none | none (a recorded divergence) |
| D-053 | Answers to a guess | transcript-inferred | none | `__main__`, `events`, `filtering`, `session` |
| D-054 | The console commands and the `show` format | transcript-inferred | none | `__main__`, `session` |
| D-055 | The trace imitates the transcripts; counts and order are not milestones | extension | `verbose` | `__main__`, `bench`, `config`, `events`, `filtering`, `milestones`, `session`, `slipping`, `trace` |
| D-056 | The box tree's nodes and their numbering | Meredith-silent | none | `boxes`, `milestones` |
| D-057 | The arena of each task type | Meredith-silent | none | `taskrack` |
| D-058 | Which spark a Tester tests, and the spark's life | Meredith-silent | none | `bonds`, `rng` |
| D-059 | Destroying a glom removes its bonds and sparks | Meredith-silent | none | `casting`, `cytoplasm`, `platoplasm` |
| D-060 | When the Template-scout applies the form itself, and one application per dubbing | transcript-inferred | none | `cytoplasm`, `events`, `templates`, `trace` |
| D-061 | What a template makes and what its acceptance sets off | transcript-inferred | `sparklers_on_acceptance` | `config`, `gloms`, `templates` |
| D-062 | What the Gnoth-maker does with the template glom, the frames and the gnoth's form | transcript-inferred | none | `gnoths`, `templates` |
| D-063 | What the Hfilter does with a term: the catchall, the frame gloms, more than one term | transcript-inferred | none | `filtering`, `gnoths`, `hypothesis` |
| D-064 | Who answers a guess: the `Answerer` | extension | none | `bench`, `filtering`, `run`, `session`, `world` |
| D-065 | A shift carries what lies between the listed gloms and the gnoth border | transcript-inferred | none | `operations` |
| D-066 | What the operations leave behind: caps, ranges, frames, forms and the new gloms | Meredith-silent | none | `events`, `operations`, `slipping` |
| D-067 | What the Gnoth-operator checks, how a PROGRAM runs, and what follows it | Meredith-silent | none | `events`, `operations` |
| D-068 | The Reformulator's order over the parse, what it sets out, and when it does nothing | transcript-inferred | none | `reforms` |
| D-069 | The session and the command line | extension | none (`--trace` and `--max-steps` set `verbose` and `max_tasks`) | `__main__`, `bench`, `rng`, `session`, `trace`, `world` |
| D-070 | The bench's `dozen`: the sequences, the terms fed, and the knowing user's judgement | extension | none | `bench`, `sequences`, `session` |

## Forms and boxes

### D-001: The internal form has four slots; a fence form's start and length come from its elements
- **Sources:** the eight formats are "(typename start-value length actual-value)" [M86 p. 49];
  the prose writes `(Cycle (2 1 8))` [M86 p. 53] but the program prints `(Cycle 3 2 (3 7))`
  [M86 p. 166], `(Cycle 3 3 (3 7 3))` [M86 p. 169] and the appendix builds
  `(Tuple 1 3 (...))`, `(Cycle 1 3 (...))` [M86 pp. 162-163]; the dubbing structure is
  "(platotype [start-val] [length] [value])" [M86 p. 69]. How the start of a Cycle is chosen
  is not said (SPEC 4.3, 8.8 item 1).
- **Choice:** every form is `Form(kind, start, length, actual)`; for Constant and Countup only
  `start` is set; for C-group, S-group and P-group `start` and `length`; for Y-group, Cycle and
  Tuple `actual` is the list of elements, `start` is the first element's value (or form) and
  `length` the number of elements, both derived and printed, so the program's own forms print
  as the transcripts do (`(Cycle 3 2 (3 7))`). The prose forms without start and length
  (`(Cycle (2 1 8))`) are accepted by the parser and printed with them.
- **Reason:** the transcripts and the appendix are the program's own output; in every example
  the start of a fence form is its first element and the length its element count.
- **Knob:** none.
- **Alternatives:** two kinds of form (prose and program); a Cycle start chosen as the smallest
  element. Nothing in the sources supports either.

### D-002: `shared` is written as the word, referring to the earliest identical structure to its left
- **Sources:** shared substructure "is indicated by the word 'shared', with an arrow pointing to
  the first instance of the structure to be shared" [M86 p. 56]; the program's own notation is
  `(same pstrucN)` at every place the structure occurs, with a binding list [M86 pp. 161-163].
  The arrow cannot be printed (SPEC 4.5, 8.8 item 1).
- **Choice:** internally a `Shared(index)` marker names the argument it shares:
  the index into the element list of a Y-group, Cycle or Tuple, or into (start,
  length) for a group, so a shared length is `Shared(0)`, the start. In the printed form a bare
  `shared` refers to the nearest form (a non-integer argument) to its left in the same argument
  list, or, for a group's length, to the group's start: `(C-group (Countup 1) shared)`
  [M86 p. 161], `(Cycle (8 (Countup 1) shared))` [M86 p. 57], `(Cycle (8 (Countup 1) shared))`
  [M86 p. 84]. Where the arrow points elsewhere, to the bracketing integer of
  `(Tuple (2 (Countup 1) shared))` [M86 p. 58] (the arrow on the page runs from `shared` to the
  2, and the hits `2 1 2`, `2 2 2` confirm it), the printed form names the argument by its
  0-based index, `(Tuple 2 3 (2 (Countup 1) (shared 0)))`; the parser reads `(shared N)` in any
  position. An arrow into a nested structure is a path of positions: on p. 84 the length of
  `(C-group (S-group 2 3) shared)` shares the 2 inside the S-group ("so that the sameness of
  the 2's were to be modeled explicitly" [M86 p. 84]), written `(C-group (S-group 2 3)
  (shared 0 0))` and held as `Shared((0, 0))`. `build-box`'s `(same pstrucN)` notation with its binding list [M86 pp. 161-163] is
  accepted by the parser as a third spelling: the first `(same X)` stands for the bound form
  and each later one for a `Shared` of it.
- **Reason:** the arrow cannot be printed; on pp. 57, 84 and 161-163 it points to the nearest
  Countup to the left, so the bare word reproduces those forms; on p. 58 it points to an
  integer, which no rule on the bare word can tell from the Countup case (the rule "the first
  non-constant structure to the left" gives `2 1 1` there, not the page's `2 1 2`), so that
  case is written explicitly. The marker is tested.
- **Knob:** none.
- **Alternatives:** print `(same pstrucN)` everywhere (the appendix's internal notation, never
  shown to the user in the transcripts); print the index always.

### D-003: A box reports its stored value while `ready`; a Cycle hits one element per hit
- **Sources:** a shared printstructure, when it fires, "feeds both Countup boxes, making both
  boxes 'ready'" [M86 p. 84]; boxes "set their 'ready' fields to 'true' -- so that they can
  report this value until the next time they are hit" [M86 p. 85]. The outputs to reconcile:
  `(Cycle (8 (Countup 1) shared))` gives `8 1 2` [M86 p. 84], `(Tuple 1 3 ((Countup 1) 8 shared))`
  gives `(1 8 1)` [M86 p. 162], `(C-group (Countup 1) shared)` gives `(2 2)`, `(3 3 3)`
  [M86 p. 161] (SPEC 8.5, 8.8 item 8).
- **Choice:** a hit on the top box stamps an epoch; a box hit twice within one epoch returns the
  value it computed at the first hit (it is `ready`); a Cycle advances one element per top-level
  hit, so its shared Countup fires once per element (`8 1 2`), while a group, Tuple or Y-group
  reads all its boxes in one epoch (`(1 8 1)`, `(2 2)`).
- **Reason:** the only reading that gives all four printed outputs (SPEC 8.5); the tests
  confirm it on pp. 58, 84 and 161-163.
- **Knob:** none.
- **Alternatives:** none found that gives both `8 1 2` and `(1 8 1)`.

### D-004: One frame of a Cycle hypothesis is one full turn
- **Sources:** a Cycle returns "one top-level element each time queried" [M86 p. 53]; p. 81
  groups the Cycle's hits one turn per parenthesis; the transcript's Cycle gnoth has
  `range: (1 2)` for `(Cycle 3 2 (3 7))` [M86 p. 168] (SPEC 4.6, 7.4, 8.8 item 7).
- **Choice:** the Gnoth-maker, the Gnoth-setter and the guess treat one turn of a Cycle (one
  pass over its element list) as one frame; `hit` on a Cycle box still returns one element, as
  `show-box` does [M86 p. 163]; the frame is the list of one turn's hits.
- **Reason:** the transcript's gnoth range; p. 81's parenthesisation.
- **Knob:** none.
- **Alternatives:** one element per frame (then run 2's `gnoths: (gnoth1 gnoth2 gnoth3)` for four
  terms could not hold).

### D-005: The repeater types have no process of their own on gloms
- **Sources:** "all but the fence-type plato-classes (Tuple and Cycle) possess a 'process'"
  [M86 p. 68], which includes Constant and Countup, but what their processes accept is not said
  (SPEC 4.4, 8.8 item 2); in the transcripts a glom is scouted for `(Cycle)`,
  `(S-group P-group)` or all six group and fence classes `(C-group S-group P-group Y-group Cycle
  Tuple)`, never for Constant or Countup [M86 pp. 165, 170, 173]; `(Countup 1)` arises only as a
  hypothesis for the single S-group hit `1 2` [M86 p. 171].
- **Choice:** Constant's process accepts a number string of all one value and Countup's a run of
  successive integers (of any length, one included), but neither repeater is ever a candidate
  class for a glom (D-022): the processes serve the Hypothesizer (D-031) and the Gnoth-caster
  (D-048) only.
- **Reason:** the transcripts' candidate lists; a glint is a manifestation of an ideal atom
  [M86 p. 61], not of a repeater.
- **Knob:** none.
- **Alternatives:** let a single glint be dubbed Constant or Countup (never seen in the runs).

### D-006: A P-group that would go below 0 is an error, and a candidate that raises one does not postdict
- **Sources:** `(P-group 2 4)` is undefined, "would run to negative numbers" [M86 p. 52]; the
  domain has only nonnegative integers [FCCA p. 48].
- **Choice:** hitting such a form raises `ValueError`; the postdiction test and the Template-applier
  treat the error as failure; the parser accepts the form.
- **Reason:** it keeps the box code simple.
- **Knob:** none.
- **Alternatives:** stop the run at 0, or produce negative terms.

## The taskrack and the run

### D-007: Taskrack sampling rule
- **Sources:** "All tasks have 'urgencies' (integer weights), and more urgent tasks will have a
  greater chance of being chosen than less urgent ones do" [M86 p. 42]; tasks are "chosen at
  random to perform their duties, the choice being affected (but not determined) by the weights"
  [M86 p. 26]. No formula, no decay, no bins (SPEC 2.3).
- **Choice:** one draw from the run's `Rng` chooses a task with probability proportional to its
  urgency raised to `urgency_power` (default 1, so to the urgency itself); the chosen task is
  removed; urgencies do not change with age. `Rng.weighted_index` does the draw with integer
  weights (item 00).
- **Reason:** the simplest reading of p. 42, an integer-weighted choice. Copycat's seven bins and temperature scaling are not in the
  thesis.
- **Knob:** `Config.urgency_power` (1).
- **Alternatives:** urgencies decaying with age; Copycat-style bins.

### D-008: The urgency of every task type
- **Sources:** the only urgency the thesis describes is the Glomtester's, "dependent on the test
  ('groupable' being strongest) and the bond-type involved (sameness being stronger than
  successorship, and so on)" [M86 p. 66], so that "a Glomtester for a sameness group is given a
  higher weight than a predecessor fence Glomtester" [M86 p. 67]; Sparkler-plus prints its
  urgency (D-015). Nothing else (SPEC 2.3, 8.8 item 4).
- **Choice:** the table `Config.urgencies`, one integer per task type: Sparkler 5,
  Sparkler-plus 10 or 2 (D-015), Tester 10, Bonder 10, Glom-scout 6, Glomtester by D-020
  (2 to 9), Glommer 8, Glom-inspector 8, Plato-scout 8, Plato-evaluator 6, Burster 3,
  Dissolver 1, Template-scout 6, Template-applier 8, Template-resolver 3, Template-evaluator 8,
  Template-reviewer 8, Gnoth-maker 8, Hypothesizer 8, Call-term 4, Hfilter 10, Gnoth-setter 2,
  Reformulator 5, Gnoth-operator 5, Capper 2, Bond-assessor 5, Gnoth-caster 5, Slip-scout 5.
- **Reason:** the stage-two values are starting values (bond-finding at 10 against a
  Gnoth-setter at 2, so that bond-finding tasks usually, but not always, run before the
  Gnoth-setter, which is what lets the slip-check see three bonds as the transcript's does
  [M86 p. 176]); Dissolvers lowest because the transcripts show them run late and finding
  their gloms gone [M86 pp. 170, 172]; Call-term below the bonding tasks so that the term is
  asked for after the new structures have been tried, as in the transcripts where `Call-term`
  follows many tasks [M86 p. 165]; the rest are ordered by their level, higher levels
  (template, socrato) a little below the cyto tasks that feed them, following FCCA's intent of
  bottom-up perception "invaded by top-down" [FCCA p. 63].
- **Knob:** `Config.urgencies`.
- **Alternatives:** all equal (the thesis's words forbid it for Glomtesters only); urgencies
  derived from bond strengths.
- **Amended (item 10):** a Glommer set out by a Glomtester carries the Glomtester's urgency
  (D-021); the table's 8 is the urgency of a Glommer set out by a Plato-evaluator (D-023).
- **Amended (item 20):** the Gnoth-setter's urgency is 10, the Tester's and Bonder's, not the
  starting value 2. The reason for 2 (so that bond-finding tasks usually, but not always, run
  before the Gnoth-setter, to let p. 176's slip-check see three bonds) does not hold: the
  three bonds of p. 176's check existed before the refuting term (p. 175's `show` lists them).
  In run 2 the Gnoth-setter ran before any Tester of the five new sparks (`slip-check:
  stayval: 0.0` / `best: nil` is computed with `bond1` alone; `bond2` and `bond3` follow it
  [M86 p. 168]) and in run 3's second refutation before any new bond [M86 p. 176]; only run
  3's first refutation, under the strict Countup, shows it late [M86 pp. 172-173]. At 2 the
  adjacent sameness bond `3`-`3` of run 2 was made before the Gnoth-setter on 16 of 23 Cycle
  seeds, the slip-check slipped Cycle to C-group and the episode blasted; at 10, 30 of 35 stay
  with Cycle as p. 168 does (`docs/appendix-runs.md`).

### D-009: Stale tasks check their structures and do nothing; only a Dissolver says so
- **Sources:** an old task "will probably do nothing, because the structures on which it was
  designed to operate no longer exist or are inaccessible to it" [M86 p. 42]; Testers, Bonders,
  Glomtesters, Plato-scouts and Gnoth-operators are each said to check [M86 pp. 64, 67, 68, 124];
  the transcripts print `glom3 is not in cytoplasm` after a Dissolver [M86 p. 172] and
  `Glomtester failed glint2` [M86 p. 170], `failed to glom (...)` [M86 p. 165].
- **Choice:** every task begins by checking that its arguments still exist (a glom in the
  cytoplasm and not under a pseudo-glom when it must be free; a gnoth with a non-empty glom
  collection; the parse version for a Gnoth-operator) and returns silently when they do not,
  except that a Dissolver prints `glomN is not in cytoplasm`, a Glomtester `Glomtester failed
  glintN` and a Glommer `failed to glom (...)`, as printed.
- **Reason:** the thesis's rule and the transcripts' three printed failures.
- **Knob:** none.
- **Alternatives:** print a line for every stale task.

### D-010: Cleaning the taskrack
- **Sources:** "At certain times some cleaning is done (removing old tasks of certain types)"
  [M86 p. 42]; when and which types is not said (SPEC 2.3).
- **Choice:** when a new term is entered, every Sparkler, Sparkler-plus, Tester and Dissolver
  task created before the previous term is removed; when a Gnoth-setter runs (a reformulation
  begins), every waiting Reformulator, Gnoth-operator, Bond-assessor, Gnoth-caster, Slip-scout
  and Capper is removed; when a hypothesis is accepted, every Template-scout, Template-applier,
  Template-evaluator and Template-reviewer is removed.
- **Reason:** the three moments are the ones where the thesis says the world changes level
  ("the template level has taken control" [M86 p. 74]; the old hypothesis is freeze-dried and
  its box destroyed [M86 p. 99]); the cyto types chosen are the ones that multiply (the
  "horde" of Testers [M86 p. 64]).
- **Knob:** `Config.cleaned_on_term`, `Config.cleaned_on_reform`, `Config.cleaned_on_hypothesis`.
- **Alternatives:** no cleaning (the rack then grows without bound on long runs); cleaning by
  age for every type.

### D-011: Run limits
- **Sources:** "successful runs are generally completed in under ten minutes of real time.
  Unsuccessful runs take a bit longer (potentially forever), as the program thrashes about for a
  solution" [M86 p. 145]; on Blackburn #5 "We stopped it after a fifteen-minute attempt"
  [M86 p. 146]. The system may quit [M86 p. 37] but no transcript shows it (SPEC 1.4).
- **Choice:** two caps, counted in tasks run: `max_tasks_per_term` (the tasks run between one
  prompt and the next; when reached, the run asks for the next term as if a Call-term had run)
  and `max_tasks` for the whole run (when reached, the outcome is "capped"). The program never
  quits on its own.
- **Reason:** a cap counted in tasks, as the Numbo port's cap in codelets,
  `--max-iterations`; the per-term cap stands in for the user's patience in the thesis's runs.
- **Knob:** `Config.max_tasks_per_term` (2000), `Config.max_tasks` (200000).
- **Alternatives:** a wall-clock limit (not reproducible); a quit when the taskrack empties with
  no hypothesis (the thesis's "quits" [M86 p. 37], never seen).

### D-012: When the program asks for the next term
- **Sources:** `doing task Call-term` precedes every `please enter a term:` once a hypothesis
  exists [M86 pp. 165, 171, 174, 178]; "At this point, the system typically calls for the next
  term in order to test its new hypothesis" [M86 p. 125]; before a hypothesis the prompt returns
  after a few tasks (two after the first term) [M86 pp. 164, 170] with no Call-term printed.
- **Choice:** a Call-term task is posted by the Hypothesizer, by a Gnoth-caster that makes a new
  hypothesis candidate and by the guess-handling after a refusal; the run loop runs tasks
  until a Call-term runs, or the taskrack is empty, or the per-term cap (D-011) is reached, and
  then prompts. The Hfilter of a confirming term posts a Call-term after the guess machinery
  (D-032) has had its say. *Amended (item 13):* in every transcript the `Call-term` follows
  both the Hypothesizer and the Gnoth-maker, whichever ran first [M86 pp. 165, 171]; so the
  Hypothesizer posts the Call-term only when no Gnoth-maker is waiting on the rack, and a
  Gnoth-maker that runs after the hypothesis was made posts it itself (stale or not). Without
  this the Call-term, at urgency 4 against the Gnoth-maker's 8, ran first in about a third of
  the runs and the prompt came back with no gnoths. *Amended (item 14):* once a hypothesis
  exists, entering a term sets out an `Hfilter` task (`doing task Hfilter` is the first task
  line after the term in every transcript [M86 pp. 166, 168, 171, 175]); the Hfilter of a
  confirming term that does not reach the guess posts the Call-term, a refused guess posts it,
  an accepted guess ends the run (`run_until_prompt` returns `done` and runs nothing more), and
  a refutation posts the Gnoth-setter instead (the Gnoth-caster's candidate posts the next
  Call-term, item 18).
- **Reason:** the transcripts' placement of `Call-term`; an empty rack must return control to
  the user or the run would hang.
- **Knob:** none.
- **Alternatives:** a Call-term posted after every term with low urgency (then the prompt would
  come back at random moments in stage one).
- **Amended (item 20):** the Hfilter of a new term is set out and run at once, ahead of any
  draw (`World.run_now`): `doing task Hfilter` is the first task line after the prompt in all
  six places where a term is entered under a hypothesis [M86 pp. 166, 168, 172, 175, 176,
  179], which an urgency of 10 against the term's Sparkler-plus tasks at 10 gave only about
  half the time. The term's cytoplasm tasks stay on the rack and run after it, as run 2's
  sparks 13-17 and run 3's Sparkler-plus lines follow the filtering line [M86 pp. 168, 172].
  Alternative recorded: a very high urgency for the Hfilter (first with high probability
  rather than certainty).

### D-013: `empty-task`
- **Sources:** `doing task empty-task` is printed once, after the first term of run 3, and never
  described [M86 p. 170].
- **Choice:** when the run loop finds the taskrack empty it prints `doing task empty-task` and
  returns to the prompt; nothing else happens.
- **Reason:** the simplest mechanism that prints the line where run 3 prints it (the rack is
  empty after the first term's two tasks). Run 1 does not print it at the same point; the
  difference is not reproducible and not a milestone.
- **Knob:** none.
- **Alternatives:** a real task posted at startup.

### D-014: Numbering and naming of structures
- **Sources:** the transcripts number glints, sparks, bonds, gloms and gnoths in order of
  creation, never reusing a number (`gnoth5` after `gnoth3` [M86 p. 176]; `glom14` after burst
  gloms [M86 p. 178]); the thesis's examples do the same [M86 pp. 114-121].
- **Choice:** one counter per class per run; names are `glintN`, `sparkN`, `bondN`, `glomN`,
  `gnothN`, `boxN`, `pstrucN`, counted from 1; a destroyed structure's number is never reused.
- **Reason:** the transcripts.
- **Knob:** none.
- **Alternatives:** none.

## The cytoplasm

### D-015: Sparkler-plus and its printed number
- **Sources:** the transcripts print `doing task Sparkler-plus on (glint2 glint1 10)`,
  `(glint2 glint2 2)`, `(glom1 glom1 10)`, `(glint3 glom3 2)`, always with the newest element
  first and the number 2 or 10, for a new glint against every existing element and itself and
  for a new glom likewise [M86 pp. 164-165, 170-173, 176]; the prose never describes the task
  (SPEC 2.3, 15.5 item 2). New structures "are immediately made centers of interest called
  active foci, and heightened activity takes place around them" [M86 p. 47].
- **Choice:** when a glint or a real glom is created (becomes an active focus), one
  Sparkler-plus is posted for it against every top-level cyto-element, itself included; the
  task's third argument is its urgency, 10 when the two elements are distinct and neither is a
  pseudo-glom, 2 otherwise; when run it makes a spark if the two elements are distinct, both
  still exist and neither is a subglom of the other, else nothing.
- **Reason:** of the 57 Sparkler-plus lines printed in runs 1 and 3, 47 fit the rule; the ten
  exceptions go both ways (10 for `(glom1 glom1)` three times and for a glint against a
  pseudo-glom three times; 2 for two distinct glints four times), so the printed number may
  have had a random or hidden component. The rule is the simplest deterministic reading, and it
  makes a new element the centre of activity, as p. 47 says. The number is not a milestone.
- **Knob:** `Config.sparkler_plus_urgencies` ((10, 2)).
- **Alternatives:** the number as something other than an urgency (nothing in the prose
  suggests what).
- **Amended (item 20):** the Sparkler-plus tasks of a new glint or glom target every element
  of the cytoplasm at any level, pseudo-gloms included, not only the top-level ones: run 3
  prints `Sparkler-plus on (glint3 glint1 10)`, `(glint3 glint2 10)` with both under `glom1`
  under the gnoth-glom `glom3`, `(glom1 glint3 10)`, `(glom1 glint1 10)` for a glom against
  its own subglom, and `(glint3 glom3 2)` against the pseudo-glom [M86 p. 172]; run 2's
  sparks 13-17 are the new `glint4` against `glint2`, `glint3`, `glint1` with `glint1` and
  `glint2` nested [M86 p. 168]. A task against a pseudo-glom, a subglom or the element itself
  makes no spark (no spark line follows those tasks on p. 172); the thesis forbids only the
  spark between a glom and its own subglom [M86 p. 63]. With this, run 2's `bond3`
  (glint1-glint4) is made on every Cycle seed (2 of 100 before).

### D-016: How many Testers, Sparklers and Glom-scouts are released
- **Sources:** a spark releases "a horde of 'Testers'" [M86 p. 64]; "Creation of a bond causes
  the release of more Sparklers ... and causes the release of some Glom-scouts" [M86 p. 65]. In
  the transcripts the counts are: run 1, 12 sparks, 12 Testers, 9 Sparklers, 2 Glom-scouts, 1
  bond; run 3, 30 sparks, 30 Testers, 28 Sparklers, 8 Glom-scouts, 8 bonds [M86 pp. 164-179].
- **Choice:** one Tester per spark (`testers_per_spark` 1), two Sparklers per bond
  (`sparklers_per_bond` 2) and two Glom-scouts per bond (`glom_scouts_per_bond` 2), and three
  plain Sparklers per new term (`sparklers_per_term` 3), since run 1 has nine Sparklers for one
  bond. The term's plain Sparklers are posted only when the cytoplasm holds at least two
  top-level real elements to draw from (a Sparkler needs a pair [M86 p. 63]); after the first
  term the transcripts print no Sparkler [M86 pp. 164, 170].
- **Reason:** the counts above: Testers equal sparks in both runs; Sparklers exceed what one
  bond can explain, so terms must release some; Glom-scouts are 2 and 8 for 1 and 8 bonds.
- **Knob:** `Config.testers_per_spark`, `Config.sparklers_per_bond`, `Config.glom_scouts_per_bond`, `Config.sparklers_per_term`.
- **Alternatives:** a larger horde with cleaning (D-010) removing most of it.

### D-017: Sparks on refutation
- **Sources:** when a term is unexpected the system "releases sparks to encourage low-level
  activity" [M86 p. 98]; in run 2 five spark lines (`spark13` to `spark17`, the new glint4
  against glint2, glint3, glint3, glint1, glint3) are printed directly after the filtering line
  and before `set-out -- validity: 0` [M86 p. 168]; in run 3, which prints its task lines, the
  sparks after the refuting term all come from the new glint's Sparkler-plus tasks
  (`doing task Sparkler-plus on (glint3 glint1 10)`, `spark6 --- between glint3glint1`, ...)
  and ten plain `Sparkler` tasks run between the Hfilter and the Gnoth-setter [M86 pp. 172-173].
- **Choice:** *(rewritten, item 14)* the refuting glint gets its ordinary Sparkler-plus tasks
  when it is entered (D-015), and the Hfilter, on the refutation, sets out
  `Config.sparklers_on_refutation` (3) plain Sparklers (D-025's draw) before the Gnoth-setter;
  no spark is made directly.
- **Reason:** run 2 prints no task lines at all, so its five sparks are what its unprinted
  Sparkler-plus tasks made (the same partners, with repeats, as run 3's Sparkler-plus lines
  show); the earlier reading ("the sparks appear before any task line could have made them")
  mistook the terse mode for immediacy. Plain Sparklers are the thesis's own way of
  "encouraging low-level activity" (p. 63) and run 3 shows them running there.
- **Knob:** `Config.sparklers_on_refutation` (3).
- **Alternatives:** direct sparks between the refuting glint and every other element (the
  first reading); no extra Sparklers at all (run 3's ten are then the term's three and the
  bonds' two each).

### D-018: Bond types tested, bond families and modifiers
- **Sources:** "sameness, successorship, predecessorship, adjacency, and meeting ... grouped into
  families" [M86 p. 64]; bond-fields are print-value and span [M86 p. 65]; gloms `(1 2)` and
  `(1 2)` "might engender both 'same print-value' and 'same span' Bonders" [M86 p. 64]; a
  same-length bond is "not considered strong enough to cause glomming" [M86 p. 62]; the
  transcripts print only `Same`, `Succ`, `Pred` on `print-value`, `(adjacent)` or `(remote)`
  [M86 p. 174]; "the 'pred-succ' bond family" [M86 p. 67] (SPEC 5.5, 8.8 item 3).
- **Choice:** a Tester tests print-value for sameness, successorship and predecessorship (a glom's
  print-value compared as a whole: two gloms are `Same` when their print-values are equal, and a
  glom and a glint, or two gloms of equal span, are `Succ`/`Pred` when the elements are
  successive term by term; for a glint against a glom the glint's value is compared with the
  end of the glom that faces it, which is how the glint `1` is bonded to the glom `(2 3)` "by
  reason of adjacent successorship" [M86 p. 63]; gloms of unequal span pass neither) and span
  for sameness between two non-glint gloms only (a glint's span "is really unnecessary"
  [M86 p. 61], and the transcripts show no span bond between glints); the modifier is `adjacent` when the two
  elements are neighbours and `remote` otherwise; the families are `Same` (sameness on either
  field) and `pred-succ` (`Succ`, `Pred`); adjacency and meeting bonds are not implemented;
  span bonds never cause glomming (D-019).
- **Reason:** the transcripts show only these; "meeting" and "adjacency" are named once and
  never used in an example; FCCA's microworld has only sameness, successorship and
  predecessorship [FCCA p. 48].
- **Knob:** none.
- **Alternatives:** implement meeting bonds (`(8 1 4)` and `(4 7)`) as "facts of note".

### D-019: Bond strengths at the cyto level
- **Sources:** "Bond strength is derived from the bond type (e.g., 'sameness'), any bond modifiers
  (adjacent terms are more strongly bonded than non-adjacent ones, for example) and the glom
  characteristic (e.g., 'print-value') that is the subject of the bond" [M86 pp. 64-65]; no
  numbers; the pull and push strengths of ch. 3 are "from 1 to 10" [M86 p. 91] (SPEC 5.6).
- **Choice:** a bond's own strength is the product of a type weight (Same 10, Succ 8, Pred 8), a
  modifier weight (adjacent 1, remote 1/2) and a field weight (print-value 1, span 1/4),
  rounded down to an integer; a Glom-scout and a Glomtester consider only bonds of strength at
  least `glom_bond_minimum` (3), which excludes span bonds; the strength is otherwise unused
  (the values a class gives a bond in its environment are D-039).
- **Reason:** p. 64-65's three factors, with the thesis's orderings (sameness strongest, adjacent
  above remote, print-value the only glom-field) and p. 62's rule that span bonds do not glom.
- **Knob:** `Config.bond_type_weights`, `Config.bond_modifier_weights`, `Config.bond_field_weights`,
  `Config.glom_bond_minimum`.
- **Alternatives:** use the reigning class's environment values at the cyto level (there is no
  reigning class in stage one).

### D-020: The Glom-scout's choice, its three tests, the family label, and the Glomtester's urgency
- **Sources:** "a Glom-scout chooses a cyto-element (a glint or glom which is not a subglom of
  any other glom) and attempts to group it with its neighbors. Actually, three quicktests tests
  are made for any bond-family in which the glom is involved: 1) is it groupable? (bonded to
  any neighbors in this way?) 2) is it coverable? (bonded into a symmetry group?) 3) is it
  fenceable? (are there remote gloms to which it is bonded?)" [M86 p. 66]; a glint's neighbours
  are its `left-nbr` and `right-nbr` fields, glints [M86 p. 61]; the printed proposals
  `Same -cover proposed --> glint1`, `Same -fence proposed --> glint1` for `3 7 3` [M86 p. 165],
  `Pred -group proposed --> glint2` for the successor pair `1 2` [M86 p. 170], and
  `Same -cover proposed --> glint3`, `Same -fence proposed --> glint3` for `1 2 1` with
  `glint1` and `glint2` under `glom1` [M86 p. 173], printed twice, the second time after
  `bond4`, adjacent predecessorship between `glom1` and `glint3`, with no group proposed; the
  6 of `5 3 1 6 1 3 4` is a Glomtester's target "with 'symmetry' the reason" [M86 p. 67]
  though bonded to nothing; the urgency ordering [M86 pp. 66-67]; palindromes are "somewhat
  subtler, since recognition of a palindrome requires noticing long-distance relationships"
  [FCCA p. 59] (SPEC 5.8, 15.5 item 4).
- **Choice:** the scout draws one element, through the run's `Rng`, uniformly from the
  top-level real elements (pseudo-gloms excluded, as D-025). For each family in which it has a
  bond of at least `glom_bond_minimum` (D-019), sameness first: *groupable* when it has an
  adjacent bond of the family to a neighbour that is free to be grouped, where a glint's
  neighbours are its `left-nbr`/`right-nbr` glints (which must still be top-level) and a
  glom's are the top-level elements beside it; *coverable* (sameness only) when it is an end
  of a remote sameness bond; *fenceable* when it has a remote bond of the family, on either
  side. An element whose two top-level neighbours are bonded to each other by remote sameness
  is also proposed for a cover, as the centre. The family in the proposal is named from the
  scouted element's side: a `Succ` bond seen from its right end is `Pred` (so `1 2` scouted at
  the 2 is `Pred -group`), and the first qualifying bond in the element's `bonds-in` names a
  group. The Glomtester's urgency is `test_weight * family_weight` with group 3, fence 2,
  cover 1 and Same 3, pred-succ 2 (`Config.glomtester_test_weights`,
  `Config.glomtester_family_weights`), so a sameness group has 9, a sameness fence 6, a
  predecessor fence 4 and a sameness cover 3.
- **Reason:** p. 66's definitions and the glint's neighbour fields of p. 61, which are what
  make the second scout of p. 173 propose no group for `glint3` (its left neighbour glint is
  taken) although the adjacent `glom1` is bonded to it: the simplest reading that fits all
  three transcript scouts; the transcript's label on the `1 2` glom; p. 67's ordering for group
  above fence, and Hofstadter's "subtler" palindromes [FCCA p. 59] together with the fence glom
  winning in both of runs 1 and 2 [M86 pp. 165, 167] for fence above cover. The ends of a
  remote bond are both proposed for a fence because p. 173 shows the right end proposed; the
  Glomtester then rejects it (D-021). The thesis calls the `1 8 8 5 2` fence a "successorship
  fence" on p. 66 and "a predecessor fence Glomtester" on p. 67, so the label rule cannot be
  settled from that example.
- **Knob:** the two weight tables; `Config.glom_bond_minimum`.
- **Alternatives:** a sum instead of a product; a scout that starts from a bond drawn at random
  and scouts one of its members (measured: it raises the fence's share for `3 7 3` from about a
  third of seeds to about a half, but it can never target the unbonded 6 of p. 67); a glom's
  bond to a glint neighbour counting as groupable from the glint's side (contradicted by the
  second scout of p. 173).

### D-021: The Glomtester's extent, the Glommer's urgency, refinement and failure
- **Sources:** the Glomtester "must first be certain that the glom it is supposed to test is
  still in the cytoplasm. If so, it must then determine the extent of the evolving glom. The
  thrust here is to get maximally-sized gloms" [M86 p. 67], with the four examples of p. 67;
  "The Glomtester either rejects the group as a glom or creates a Glommer task to refine the
  group and perform the final glomming" [M86 p. 67]; the transcripts print `Glomtester failed
  glint2` when the target had gone into `glom1` [M86 p. 170], nothing for `Glomtester on (Same
  fence glint3)` and `(Same cover glint3)` of `1 2 1` with `glint1` and `glint2` under `glom1`
  [M86 p. 173], and `failed to glom (glint1 glint2 glint3)` for the cover proposed before the
  fence glom `(3 7)` had taken `glint1` and `glint2` [M86 p. 165]; the fence glom came first in
  both of runs 1 and 2 [M86 pp. 165, 167] (SPEC 5.8, 8.8 item 5).
- **Choice:** the Glomtester prints `Glomtester failed X` and does nothing when its target is
  gone, under another glom or a pseudo-glom; otherwise it reads the lineup (the top-level
  elements in sequence order) and takes the extent: *group* is the maximal chain of lineup
  neighbours linked by adjacent bonds of the family of at least `glom_bond_minimum`
  (`1 2 3 2 3`); *fence* runs from the element up to, not including, its nearest remote
  partner of the family to the right, which must itself be in the lineup (`9 1 2 5` of
  `9 1 2 5 9`; the right end of a bond, `glint3` of `1 2 1`, cuts no fence); *cover* from an
  end of a remote sameness bond is the window between the two ends, both of which must be in
  the lineup and whose print-values must read the same both ways, widened outward while the
  mirror elements stay equal, and from a centre the widest such window around it (`3 1 6 1 3`).
  An extent of fewer than two elements, or containing a pseudo-glom, is rejected silently. The
  Glommer is set out with the members and **with the Glomtester's own urgency** (D-020), so
  the scan's bias carries up; a Glommer set out by a Plato-evaluator (D-023) has the table's
  urgency (D-008). The Glommer drops from each end any member that is no longer top-level and
  fails, printing `failed to glom (...)`, when fewer than two remain, when the rest are not
  all top-level, adjacent and real; otherwise it creates the glom, makes it an active focus
  with its Sparkler-plus tasks (D-015) and posts a Glom-inspector.
- **Reason:** p. 67's four examples give the three extents; the p. 173 silences are explained
  by the window's ends being taken; the transcript's failure by members already taken. The
  inherited urgency is the terraced-scan pattern (each stage's urgency derives from the
  last) and is what lets the fence glom of runs 1 and 2 beat the cover more
  often than not once both are proposed from `glint1`. **A finding:** even so, for `3 7 3` the
  fence `(3 7)` wins in about a third of seeds and the cover `(3 7 3)`, dubbed Y-group, in the
  rest, because the scout's draw (D-020) reaches the fence only from `glint1` while the cover
  is reachable from all three glints; see RESULTS.md.
- **Knob:** none beyond D-020's weights.
- **Alternatives:** a Glommer that bursts competing gloms to take their members; a flat Glommer
  urgency (then the race from `glint1` is even); a cover window required to be at least as
  long as a competing fence (no source).

### D-022: Which plato-classes find a glom interesting
- **Sources:** the Glom-inspector names "the 'interested' (candidate) plato-classes" [M86 p. 68];
  the transcripts show `(Cycle glom1)` for the sameness fence `(3 7)`, `((S-group P-group)
  glom1)` for the pred-succ group `(1 2)`, and `((C-group S-group P-group Y-group Cycle Tuple)
  glom6)` for a top-down glom [M86 pp. 165, 170, 173]; `(1 1 1)` may be dubbed C-group and
  Y-group [M86 p. 69] (SPEC 5.8, 15.5 item 5).
- **Choice:** by the glom's `type`: a Same group interests C-group and Y-group; a pred-succ group
  S-group and P-group; a cover Y-group; a fence Cycle; a top-down glom (made by a Gnoth-setter,
  a gnoth operation or a Capper) all six group and fence classes; never the repeaters (D-005).
  Tuple is reached only through the all-six list and the Hypothesizer's fall-back (D-031).
- **Reason:** the three transcript lists; p. 69's double dubbing.
- **Knob:** none.
- **Alternatives:** every glom scouted for all six classes.

### D-023: The Plato-evaluator's search for pure subcollections and secondary purity
- **Sources:** the Plato-evaluator looks "for dubbable gloms of secondary purity (non-flat) and
  for pure subcollections of gloms within the proposed target glom" [M86 p. 69], breaking
  `1 2 3 2 3` into `1 2 3` and `2 3` [M86 p. 69]; secondary purity is "not flat; e.g.,
  ((1 5) (1 5) (1 5))" [M86 p. 69]; "pure <--> an exact manifestation; flat" [M86 p. 69]; the
  transcript runs `Plato-evaluator on (P-group glom1)` after the Plato-scout dubbed `(1 2)` as
  S-group, and nothing follows [M86 pp. 170-171]; "If the Plato-evaluator has no success and
  the glom has not already been dubbed ... A Burster task is created" [M86 p. 69] (SPEC 5.9,
  8.8 item 5).
- **Choice:** a Plato-scout dubs only a flat glom, as pure, and sets out one Plato-evaluator
  per candidate class whose test fails (so a non-flat glom goes to the evaluator for every
  class). The evaluator, for a non-flat glom, applies its class's process to the units (the
  subgloms' print-values) and dubs the glom with secondary purity when they pass (`((1 5) (1 5)
  (1 5))` as a C-group; the Y-group process likewise; S-group and P-group never pass on units,
  and the fence classes have no process). For a flat, undubbed glom it cuts the members
  wherever the type of the adjacent print-value bond between neighbours changes or is missing,
  and keeps each piece shorter than the whole, of two or more members, whose values pass the
  process of a class interested in a group of its family (D-022): `1 2 3 2 3` gives `1 2 3`
  (Succ), `3 2` (Pred) and `2 3` (Succ); if there is any such piece it dissolves the glom and
  sets out a Glommer for each piece (the Glommers' race decides between the overlapping
  pieces). A glom that has meanwhile been glommed into another is left alone (p. 70's escape).
  When it has had no success and the glom is undubbed, it posts a Burster. The structure a
  dubbing records is `(kind start length)` for a group class and `(kind start length (units))`
  for Y-group, Cycle and Tuple, where a unit that is a glom is its own first dubbed form, else
  a Tuple of its print-value.
- **Reason:** the two examples; p. 69's "pure <--> flat", which makes the evaluator the only
  dubber of non-flat gloms; the transcript's evaluator on a class that failed while another
  passed; the simplest cut rule that produces the thesis's pieces. The dub form follows the
  program's printed forms `(S-group 1 2)` and `(Cycle 3 2 (3 7))` [M86 pp. 165, 171]; the thesis
  prints no secondary form, so the unit's form is the simplest reading of "secondary".
- **Knob:** none.
- **Alternatives:** try every contiguous sub-run against every class (more gloms, slower); one
  Plato-evaluator per glom rather than per failed class (the transcript prints the class).

### D-024: Bursters and Dissolvers: who posts them, and what a Dissolver may dissolve
- **Sources:** the Plato-evaluator posts a Burster [M86 p. 69]; "A Dissolver is a task that
  destroys a glom, leaving its highest-level subgloms to float independently" [M86 p. 70]; the
  transcripts show a Dissolver posted on each new term (`Dissolver on (glint1)` after the second
  term [M86 p. 164]) and old Dissolvers finding their gloms gone [M86 pp. 170, 172] (SPEC 5.10).
- **Choice:** an element is an active focus from its creation (a glint when its term is entered,
  a glom when the Glommer makes it [M86 p. 67]) until the next term is entered; a new term
  posts one Dissolver (urgency 1) on each element whose focus ends with it (the previous
  term's glint and the gloms made since: `Dissolver on (glint1)` after the second term,
  `Dissolver on (glom3)` [M86 pp. 164, 172]), or a bare Dissolver that does nothing when
  there is none (`doing task Dissolver` after the first term [M86 pp. 164, 170]); a Dissolver
  dissolves its glom only if it still exists, is free (not a subglom of any glom, so not
  capped by a pseudo-glom), not dubbed, not a pseudo-glom and not an active focus; on a glint
  it does nothing. A Burster bursts its glom to glints unless it has since been dubbed or
  capped.
- **Reason:** the transcripts' Dissolvers on glints after a term; the thesis's "soup bubbling"
  [M86 p. 62]: undubbed gloms decay unless they rise.
- **Knob:** none beyond the urgency (D-008).
- **Alternatives:** no Dissolvers but the Plato-evaluator's (then the trace would not show them
  on term entry); a Plato-evaluator that posts a Dissolver instead of a Burster when a subglom
  of the failed glom is dubbed, so that dubbed work survives.
- **Note (item 10):** the Burster is implemented as p. 69 says, "all its non-glint subgloms":
  a dubbed glom absorbed into a larger undubbed glom (as `(1 2)` into `((1 2) 1)` when the
  third term of `1 2 1` arrives before a template has capped it) is destroyed with it. Before
  the template stage exists this is what ends most `1 2 1` runs with no glom at all; whether
  the window between dubbing and capping matters once templates exist is for item 12 to see.
- **Amended (item 21):** a term posts one Dissolver, on the top-level element holding the
  previous term's glint (`Dissolver on (glint1)` after the second term [M86 pp. 164, 170];
  `Dissolver on (glom3)` after the third, glint2 lying under the bottom-up glom1 under the
  gnoth-glom glom3 [M86 p. 172]; `Dissolver on (glom8)` and `on (glom10)`, the caps over
  glint3 and glint4, both run after the fifth term because the fourth term's was never drawn
  before the guess [M86 p. 176]), or a bare one after the first term. The earlier rule (every
  element whose focus ended) could not post a Dissolver on a pseudo-glom. The `active` flag
  is kept for what the Dissolver may dissolve. `cytoplasm.top_of`, `bonds.new_term`.

### D-025: The Sparkler's draw
- **Sources:** "A spark is created between two gloms when a Sparkler task pulls those gloms at
  random from the cytoplasm" and "simply looks for gloms that are not subgloms of each other"
  [M86 p. 63]; the same pair is sparked repeatedly in the transcripts [M86 p. 167].
- **Choice:** a Sparkler draws two elements, through the run's `Rng`, from the glints and real
  gloms that are not subgloms of any other glom (top-level, pseudo-gloms excluded); a draw of
  the same element twice makes no spark; duplicates of existing sparks are allowed.
- **Reason:** p. 63; the repeated sparks of the transcripts.
- **Knob:** none.
- **Alternatives:** draw from every glom at any level.
- **Amended (item 20):** the Sparkler draws from the glints and real gloms at every level, not
  only the top-level ones: "The Sparkler simply looks for gloms that are not subgloms of each
  other" [M86 p. 63] forbids a glom with its own subglom and nothing else, and the transcripts
  spark nested elements (`spark11 --- between glint1glint2` with both under `glom1`,
  `spark12 --- between glom1glint3` with `glom1` under the template glom [M86 p. 165]).
  Two elements nested in each other make no spark; pseudo-gloms are still left out (no spark
  with one is printed anywhere).

## Templates

### D-026: The Template-applier's fit and its loosening order
- **Sources:** the applier tries "to view the sequence as a repetition" of the dubbed form, then
  "checks to see if loosening a parameter or two in its representation would help", giving
  `(S-group 1 n)` [M86 p. 74]; the template's `coverage` is `(1 2)` for the three terms `3 7 3`
  [M86 p. 167]; which parameter is loosened first is not said (SPEC 6.4, 8.8 item 6).
- **Choice:** (amended in item 12) the applier reads the *lineup* (the top-level cyto-elements
  in order) as repetitions of one *unit* of the form: one hit of a group, Y-group or Tuple, one
  full turn of a Cycle (D-004). A glom must be exactly one complete unit; a run of free glints
  is cut into units from the left, each as long as it can be ("maximally-sized", the
  Glomtester's thrust [M86 p. 67]); the last element or glints may be a proper prefix of a
  unit (the `3` after `(3 7)` [M86 p. 167]). `coverage` is the positions of the complete
  units. On failure the parameters are loosened: the length to `n`, then the start, then both;
  the first loosening that fits is the template. Only a group form with integer parameters can
  be loosened (a fence form's elements are its structure; nothing in it is "a parameter");
  under a wildcard "n means any nonnegative integer" [M86 p. 74], so a unit must pass the
  class's process and match the parameters that are not wild, and with both parameters wild
  a unit needs two terms (else any single term would be a unit and the template would
  explain anything). A fit with no complete unit is no template. The applier's lineup reading
  is what makes it "fail because of the first glom" of `(1 1) 2 (1 2 3)` where the resolver,
  reading the glints, succeeds [M86 p. 75].
- **Reason:** p. 74's example loosens the length; the transcript's coverage counts complete
  units; the applier/resolver distinction of p. 75 requires the applier to respect gloms.
- **Knob:** none.
- **Alternatives:** loosen the start first; read the terms rather than the lineup (then the
  resolver has nothing to add); allow a one-term unit under a doubly loosened form.

### D-027: What the Template-evaluator checks
- **Sources:** the template "will be checked by a Template-evaluator task and either be passed,
  or rejected and abandoned" [M86 p. 74]; the transcript prints `check-cycle template
  (Cycle 3 2 ((3 7)))` [M86 p. 165] and `Template-evaluator on nil` [M86 p. 171]; nothing says
  what is checked (SPEC 6.3, 8.8 item 6).
- **Choice:** (amended in item 12) the evaluator requires that the template glom still exists
  and that the lineup still fits the template's form (D-026's fit, reading the template glom
  as its units); it passes when both hold and otherwise abandons the template (the template
  glom is dissolved and the unit gloms the template made are burst, D-061; the engendering
  glom stays). For a fence-class template it prints the check line named after the class with
  the template glom's print-value, `check-cycle template (Cycle 3 2 ((3 7)))` [M86 p. 167];
  for a group-class template nothing is printed, as run 3 prints nothing for `(S-group 1 2)`
  after `Template-evaluator on nil` [M86 p. 171]. Passing is acceptance: the evaluator sets
  out a Gnoth-maker and a Hypothesizer [M86 pp. 75-76] and the Sparklers of D-061.
- **Reason:** the simplest reading of p. 73, a template explaining at least roughly all the
  terms seen so far [M86 p. 73]; the check
  between creation and acceptance exists because a term may arrive in between; the two
  transcripts' check lines.
- **Knob:** none.
- **Alternatives:** an evaluator that always passes; a check line for every class.

### D-028: The Template-reviewer's "very simple fixes"
- **Sources:** the reviewer "can try some very simple fixes and can either: 1) call for
  modification of the template and restart the creation and evaluation processes; 2) leave the
  template alone; 3) target it for abandoning" [M86 p. 75]; the fixes are not described.
- **Choice:** on a term that fails the template: if a loosening of the template's form
  (D-026's order) fits the lineup, with the template glom read as its units and the unit gloms
  the template itself made read as their glints (so that the looser form can cut the units
  afresh), the template is modified: the template glom is dissolved, those unit gloms are
  burst, and the template is re-made with the looser form (its lines printed again) and
  re-evaluated ("restart the creation and evaluation processes" [M86 p. 75]); else if the new
  term could begin a new unit (it is a complete unit or a prefix of one by itself), the
  template is left alone; else it is abandoned (as D-027's abandonment).
- **Reason:** the three outcomes p. 75 lists, each given the one trigger the thesis's examples
  suggest (loosening is the applier's own fix); re-cutting only the gloms the template made
  keeps the reviewer from the resolver's blast, which the thesis reserves for the resolver.
- **Knob:** none.
- **Alternatives:** always abandon; let the reviewer blast like the resolver.

### D-029: The Template-resolver's blast
- **Sources:** "If the Template-resolver is successful, it 'blasts' (does an immediate burst of)
  all gloms and has the glints reglommed to fit the template" [M86 p. 75]; if not, "it is
  forgotten and the engendering glom dissolved" [M86 p. 75].
- **Choice:** the resolver fits the form and then its loosenings (D-026) to the glints alone;
  on success it bursts every free real glom (pseudo-gloms are not touched; the bonds of a
  burst glom go with it, D-059, bonds between glints survive), reads the glints as units and
  makes the template over them as D-061 says (one top-down unit glom per complete unit); on
  failure it dissolves the engendering glom when it is still free, "forgotten and the
  engendering glom dissolved" [M86 p. 75], and nothing else. Note the consequence, taken
  literally from p. 75: a dubbed glom whose form (loosened or not) does not fit the whole
  sequence at term level is dissolved once its scout, applier and resolver have run; the
  thesis says the resolver "is seldom invoked" because a template usually comes early
  [M86 p. 75].
- **Reason:** p. 75; bond survival follows the blast of p. 92 ("leaving only the glints and
  their bonds", D-050).
- **Knob:** none.
- **Alternatives:** burst the bonds too; keep the engendering glom (not what p. 75 says).

## Gnoths and the hypothesis

### D-030: The catchall gnoth is a gnoth object that `show` does not list
- **Sources:** "a catchall gnoth, a special gnoth that simply serves as a repository for
  non-troublesome terms" [M86 p. 98]; no transcript's `show` lists it [M86 pp. 168, 175, 179]
  (SPEC 7.4, 8.8 item 7).
- **Choice:** the catchall is a `Gnoth` flagged `catchall`, with no plato-class, no frame and
  no gnoth-glom; confirming terms are appended to it; `show` and `show-parse` omit it; the
  Gnoth-setter splits it (D-036) and destroys it. *Amended (item 13):* the Gnoth-maker makes
  it, holding the glints beyond the template's coverage (the trailing `3` of `3 7 3`, which
  the template filter let pass as the beginning of a unit [M86 p. 167]): run 2's Gnoth-setter
  makes gnoths `(3 7)`, `(3)` and `(3)` from the parse [M86 p. 168], so the trailing term
  was held somewhere the Gnoth-setter could reach, and p. 98 says that place is the catchall.
- **Reason:** the thesis calls it a gnoth; the transcripts never print it.
- **Knob:** none.
- **Alternatives:** a bare list of unparsed glints.
- **Amended (item 15):** the catchall is made by the Hfilter at the first confirming term,
  not by the Gnoth-maker (D-062, D-063): when made it takes in every glint the hypothesis
  postdicted beyond the listed gnoths (the trailing `3` of `3 7 3`), so a Gnoth-setter can
  still reach them, and it is destroyed by the Gnoth-setter ("the catchall is destroyed"
  [M86 p. 98]). Reason: the gnoth numbers of the transcripts. Run 3's second Gnoth-setter
  prints `gnoths: (gnoth2 gnoth3 gnoth5)` [M86 p. 176] after its first printed
  `gnoths: (gnoth1 gnoth2 gnoth3)` [M86 p. 173]: `gnoth4` was made between the two, after
  the first episode's three gnoths, which is where a catchall numbered at the confirming `2`
  of p. 175 falls; and the first episode's `gnoth1 gnoth2 gnoth3` leaves no number for a
  catchall made before it. Both lists, and run 2's `gnoths: (gnoth1 gnoth2 gnoth3)`
  [M86 p. 168], come out exactly under this rule (D-036 gives the rest).

### D-031: How the Hypothesizer turns a template into a form
- **Sources:** the Hypothesizer devises a hypothesis "based on the template and the existing
  gnoths (if any) and gloms" [M86 p. 79], with the Tuple fall-back for a faulty template
  [M86 p. 79]; the transcripts give `(Cycle 3 2 (3 7))` from the template `(Cycle 3 2 (3 7))`
  [M86 p. 165] and `(Countup 1)` from the template `(S-group 1 2)` with one gnoth [M86 p. 171];
  `1 2` "causes the system to hypothesize (Countup 1)" [M86 p. 98]; the imperative is "the
  strictest appropriate class" [M86 p. 59] (SPEC 8.1, 8.8 item 8, 15.5 item 7).
- **Choice:** (1) if the template has repeated (two or more complete hits) and no wildcard, the
  form is the template's form; (2) if it has a wildcard, the gnoths' values of that parameter
  are generalised as the Gnoth-caster does (D-048): equal values stay, values counting up by one
  become `(Countup first)`, else the fall-back; (3) if the template has only one complete hit, a
  generate-class form is replaced by its repeater: an S-group hit by `(Countup start)`, a
  C-group hit by `(Constant start)`, a P-group hit stays (there is no counting-down repeater);
  a fence form with one hit stays; (4) the fall-back is `(Tuple 1 k (all the terms))`. The form
  must postdict the terms (D-034) or the fall-back is used.
- **Reason:** rule (3) is the only one that gives run 3's `(Countup 1)` from `(S-group 1 2)` and
  p. 98's statement, and it is what the strictness imperative asks for: a single run of
  successive integers that has not repeated is a Countup; rules (1), (2), (4) are p. 79 and the
  transcripts.
- **Knob:** none.
- **Alternatives:** always copy the template's form (contradicts p. 171).

### D-032: Validity growth and the guess trigger
- **Sources:** validity is "a number that grows as new, correctly-predicted terms are
  encountered" [M86 p. 81]; "Consistent verification, in the form of terms which support the
  hypothesis, will lead the system to a confirmation of the hypothesis and the venturing of a
  public guess" [M86 p. 39]; every transcript guesses after exactly one confirming term
  [M86 pp. 166, 169, 175, 179] (SPEC 1.4, 8.3, 15.5 item 15).
- **Choice:** each confirming term adds 1 to validity; when validity reaches `guess_validity`
  (1) the Hfilter prints the guess (D-033) and asks the user; after a refusal validity is set to
  0 and the hypothesis stays until a term refutes it; after `ok` the run ends.
- **Reason:** the four transcripts.
- **Knob:** `Config.guess_validity` (1).
- **Alternatives:** a threshold of 2 or more (never seen).

### D-033: The guess block
- **Sources:** the guesses print `3 7 3 7` after the terms `3 7 3 7`, `3 7 3 3 7` after
  `3 7 3 3 7`, `(1 2)(1 2)(1 2)` after `1 2 1 2`, `(1 2)(1 2 3)(1 2 3 4)` after `1 2 1 2 3 1`
  [M86 pp. 166, 169, 175, 179]; the box "can be reset to start again, asked to list a number of
  terms" [M86 p. 82] (SPEC 8.3, 15.5 item 14).
- **Choice:** the guess resets a copy of the box and fires it from the first term for at least
  `guess_min_hits` (3) hits and until every entered term is covered; the hits are printed as
  the transcripts print them (a Cycle's single elements separated by spaces, a group's lists
  run together). In the non-interactive mode a guess is judged right when its terms agree
  with the scripted sequence as far as both go.
- **Reason:** the one rule that fits all four printed guesses (SPEC 15.5 item 14).
- **Knob:** `Config.guess_min_hits` (3).
- **Alternatives:** the next three hits from the current box state (gives `3 7 3` and
  `(1 2 3)(1 2 3 4)(1 2 3 4 5)`, not what is printed).

### D-034: Postdiction and the validity of a new candidate
- **Sources:** a candidate's box "is tested to see whether it can accurately 'postdict' the
  known terms of the sequence" [M86 p. 125]; after a new candidate the first confirming term
  produces the guess [M86 pp. 174-175] (SPEC 14.4).
- **Choice:** a form postdicts when a fresh box, fired from the first term, produces exactly the
  entered terms (the last hit may be cut short); the postdicted terms do not count as
  confirmations: a new hypothesis starts with validity 0.
- **Reason:** p. 125; the transcripts' one confirming term after a reformulation.
- **Knob:** none.
- **Alternatives:** count the postdicted terms (then the candidate would guess at once).

### D-035: Freeze-dried hypotheses: the repeat check and who destroys the box
- **Sources:** a failed hypothesis's form "is kept on a list of old hypotheses, along with the
  number of terms of the sequence it explained. The old hypotheses serve as a check against
  cycling" [M86 p. 91]; "saving the form of the old hypothesis and destroying its box" is the
  first step of reconsideration [M86 p. 99]; what the check does with a repeat is not said
  (SPEC 10.2, 14.6 item 2).
- **Choice:** the Gnoth-setter freeze-dries the form with the count of terms it explained and
  destroys the box; a Gnoth-caster candidate whose form equals a freeze-dried form that
  explained at least as many terms as are now entered is rejected as if it had failed to
  postdict (a Slip-scout follows, D-048); a repeat that would explain more terms is allowed.
- **Reason:** "flag cyclic behavior" [M86 p. 92] read as the minimum that prevents the same
  failure twice, while letting a form return once it can explain more.
- **Knob:** `Config.reject_freeze_dried_repeats` (True).
- **Alternatives:** only report the repeat; reject every repeat.
- **Note (item 15):** the Gnoth-setter does the freeze-drying (`FreezeDried(form, terms
  explained)`, the count being the terms its frames matched) and the destruction of the
  box: `world.hypothesis` is None afterwards, so nothing filters terms until a candidate is
  made [M86 p. 99]. The repeat check waits for the Gnoth-caster (item 17).
- **Note (item 18):** built in `casting.repeats_freeze_dried`: a candidate whose form equals
  a freeze-dried form that explained at least as many terms as are now entered is rejected
  (the Gnoth-caster counts it as a failure and sets out a Slip-scout, as for a form that does
  not postdict [M86 p. 126]); a form that could explain more is allowed. The check applies to
  the Gnoth-caster's candidates only; the Hypothesizer's forms (D-031), made in stage one after
  a blast, are not checked against the list, so a cover seed of `3 7 3 7 ...` can propose a
  larger palindrome after each blast (RESULTS.md). *Alternative, pending nothing:* check the
  Hypothesizer's form too.


### D-036: The Gnoth-setter's layout: frames, the cut-short last frame, reuse of gnoths, pseudo-gloms and numbering
- **Sources:** the Gnoth-setter "carefully fills out gnoths in accordance with the old
  hypothesis", splitting the catchall so that the predicted terms get gnoths and "Any
  non-fitting terms are collected together in a final gnoth" [M86 p. 98]; "a Gnoth-setter
  puts out three gnoths, one for each term" after `1 2` and `1` [M86 pp. 98-99]; the
  transcripts print five `top-down glom` lines for three gnoths in the first refutations and
  three for the second of run 3 [M86 pp. 168, 173, 176], `groups: ((glint1 glint2))` and
  `glom: (glint1 glint2)` under the first in run 2 [M86 p. 168], the lists `gnoths: (gnoth1
  gnoth2 gnoth3)` [M86 pp. 168, 173] and `gnoths: (gnoth2 gnoth3 gnoth5)` [M86 p. 176], the
  frames `0`, `1`, `2` after the first Gnoth-setter [M86 pp. 168, 175], `gnoth2` still
  `frame: 1` and the new `gnoth5` `frame: 2` after the second [M86 p. 179], `glom9` (gnoth2's
  glom) surviving the second Gnoth-setter [M86 pp. 175, 178], and bonds between glints only
  after the first (`bond4`, between the bottom-up `glom1` and `glint3` [M86 p. 173], is not in
  `show` [M86 p. 174]) (SPEC 10.1, 14.6 item 3, 15.5 item 8).
- **Choice:** *(rewritten, item 15)* the old box is fired from the first term, frame by frame
  (one turn for a Cycle, D-004); every complete frame that matches gets a gnoth, a frame cut
  short by the failing term gets a gnoth over the terms that matched, and the non-fitting
  terms (the failing term and anything after it) form the final gnoth; the frames are
  numbered from 0 in the old hypothesis's order and the final gnoth takes the number after
  the last frame's. The catchall is destroyed. The existing gnoths are reused by position:
  for each frame, a gnoth whose range *begins* where the frame begins is reused, kept intact
  when its range is the frame's and its gnoth-glom is present, filled afresh otherwise (its
  old gnoth-glom dissolved, any element straddling the frame's boundaries burst to glints,
  one new gnoth-glom, a pseudo-glom printed `top-down glom glomN`, put over the elements
  there); every other gnoth overlapping the frame is destroyed; a frame with no gnoth gets a
  new one numbered from the run's counter; a reused gnoth keeps its name and frame number.
  Each gnoth gets its equivalence mark (D-051); the final gnoth none. For a Cycle, `3 7 3 3`
  under `(Cycle 3 2 (3 7))` gives `(3 7)`, `(3)`, `(3)`.
- **Reason:** p. 98's example; the three printed `gnoths:` lists come out exactly (with
  D-030's catchall numbering): run 3's `gnoth1` (the Gnoth-maker's gnoth over `(1 2)`,
  whose range begins at the first frame `1`) is filled afresh and `(1 2)` burst, which also
  removes `bond4` as p. 174 shows, `gnoth2` and `gnoth3` are new; in the second episode
  `gnoth2` is kept with `glom9`, `gnoth3` (range `(3 3)`, the second frame being `(3 4)`) is
  filled afresh and `gnoth5` is new with frame 2, as p. 179 prints. The top-down glom counts
  (5, 5, 3 against the reconstruction's 2, 3, 2) and p. 168's `groups:` and `glom:` lines are
  not reproduced: the thesis does not say which gloms were made (not milestones, D-055).
- **Knob:** none.
- **Alternatives:** one gnoth per hit (for a Cycle, one per element); destroy and remake every
  gnoth (gives `gnoth2 gnoth3 gnoth4` where p. 173 prints `gnoth1 gnoth2 gnoth3`); renumber
  frames from 0 after every Gnoth-setter (gives `gnoth2` frame 0 where p. 179 prints 1).
- **Note (item 21):** with D-063's amendment the cap over the previous frame is gone before
  the Gnoth-setter runs, so a gnoth over that frame is filled afresh (a new cap over its
  elements, its `form` the one member's structure field as a recap gives it, D-066), and run
  2's gnoth1 is no longer kept intact: the Gnoth-setter prints three `top-down glom` lines
  there. The printed counts (five for three gnoths at both first refutations, three for the
  two gnoths filled at run 3's second, 2n - 1 for n gnoths filled [M86 pp. 168, 173, 176])
  say her Gnoth-setter filled all three gnoths of run 2 too, which agrees.

### D-037: Casting under a fence class
- **Sources:** run 2's Gnoth-caster prints `casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1
  (3)))` and `(Cycle 3 3 (3 7 3))` for the terms `3 7 3 3` [M86 p. 168], with no task lines
  printed in that run; the caster generalises "if all the gnoths can be cast, or if all but the
  last can and it shows promise" [M86 p. 125] (SPEC 14.3, 15.3).
- **Choice:** when the reigning class is Cycle or Tuple, each top-level item (glint or glom) of
  every gnoth but a promising last one is cast as a one-element form of the class, `(Cycle v 1
  (v))`, and the casts are concatenated into one form over all the items, `(Cycle first k
  (items))`; the last gnoth "shows promise" when its terms are a prefix of the concatenated
  list; the candidate must postdict (D-034).
- **Reason:** with D-036's gnoths `(3 7)`, `(3)`, `(3)`, this is the only reading that prints
  exactly three casts and the printed form without any gnoth operation, which the terse run 2
  does not show.
- **Knob:** none.
- **Alternatives:** cast each gnoth as one Cycle and generalise element by element (gives a
  different cast list).
- **Note (item 18):** built in `casting.cast_terms` and `casting.cast_gnoths`. Stated
  exactly: under Cycle or Tuple every *term* of a gnoth is cast as a one-element form of the
  class (the gnoths of D-036, `(3 7)`, `(3)`, `(3)`, give the three casts `(Cycle 3 1 (3))`
  `(Cycle 7 1 (7))` `(Cycle 3 1 (3))` only if the first gnoth is cast term by term); the
  promise check comes *before* casting, since a concatenation always generalises: when there
  are at least two gnoths and the last gnoth's terms are a prefix of (or equal to) the
  concatenation of the earlier gnoths' terms, the last gnoth is left uncast and the head is
  the form, `(Cycle 3 3 (3 7 3))` [M86 p. 168]; otherwise every gnoth is cast and the Cycle
  covers every term. The consequence, recorded as a finding (RESULTS.md): a fence hypothesis
  refuted by a term that does not begin a repetition grows to a Cycle (or Tuple) of all the
  terms, which postdicts trivially and is adopted (`3 7 3 7 3 7 5` gives `(Cycle 3 7 (3 7 3 7
  3 7 5))`), the degenerate reading of p. 125's rule "if all the gnoths can be cast" for a
  class that can cast anything. *Alternatives:* reject a fence candidate that covers every
  term (an invention); or gnoths of one hit each under Cycle with the troublesome term in the
  catchall (then the three casts need no promise rule).


## Reformulation

### D-038: The s-link table
- **Sources:** "S-group has s-links to Countup, C-group, Y-group, Cycle, and Tuple" [M86 p. 90];
  S-group to Countup 0.1, to Tuple 0.4 [M86 pp. 90-91], to Y-group 0.4 [M86 p. 104]; the
  transcript slips `Countup --> S-group` [M86 p. 174]; no other class's links are given
  (SPEC 11.1, 14.6 item 4).
- **Choice:** `Config.s_links`, directed, with the thesis's four values and starting values
  for the rest: S-group to Countup 0.1, C-group 0.3, Y-group 0.4, Cycle 0.3, Tuple 0.4;
  Countup to S-group 0.5, C-group 0.3, Tuple 0.2; Constant to C-group 0.5, Countup 0.3, Tuple 0.2;
  C-group to Constant 0.1, S-group 0.3, Y-group 0.3, Tuple 0.4; P-group to S-group 0.3, Y-group
  0.4, Tuple 0.4; Y-group to S-group 0.3, C-group 0.3, Tuple 0.4; Cycle to Tuple 0.4, S-group 0.2,
  C-group 0.2; Tuple to Cycle 0.4, S-group 0.2, C-group 0.2, Y-group 0.2.
- **Reason:** the thesis's two principles, "it is difficult to slip to a stricter class" and
  Tuple as "a generic grouping mechanism" [M86 pp. 90-91]; the other values are an
  engineering choice that follows those two principles.
- **Knob:** `Config.s_links`.
- **Alternatives:** symmetric links; a uniform 0.3.
- **Note (item 11):** the `Platoplasm` reads the table once when the run starts into each
  `IdealType`'s `s_links` (neighbour -> slipperiness, in the table's order), since the values
  "could be, but in practice are not, changed by the system" [M86 p. 91]; a pair with no
  s-link has slipperiness 0, and a class's `neighbours` are the keys of its table.
- **Amended (item 15):** Cycle's s-links go to Tuple (0.4) and C-group (0.2) only; the
  starting table's link to S-group is dropped. Reason: run 2 prints `best: nil` at a moment when the
  remote sameness bond `bond1` between glint1 and glint3 exists [M86 pp. 167-168]; S-group
  values that bond -5, and a push counts by its absolute value in a neighbour's pressure
  [M86 p. 104], so with S-group a neighbour of Cycle `best` would be `(S-group 1.0)`.
  C-group and Tuple value a remote sameness bond 0 (D-039), so `nil` comes out under D-042's
  rule. Which neighbours Cycle really had is not in the thesis; any neighbour that values
  remote sameness 0 would do.

### D-039: The pull and push tables of the eight classes
- **Sources:** under S-group adjacent successor +10, remote sameness -5 [M86 p. 101], adjacent
  predecessor 0 and remote successor 0 [M86 pp. 175, 179]; under Y-group remote sameness 10
  [M86 p. 104]; C-group "favors adjacent sameness bonds most strongly, but also likes to see
  gloms having the same span" [M86 p. 91], "sameness bonds become strong whereas successorship
  bonds are made negative" [M86 p. 100]; strengths "from 1 to 10" [M86 p. 91]; a bond type a
  class lists neither way is worth 0 (SPEC 11.1, 11.6, 14.6 item 5).
- **Choice:** `Config.bond_values`, keyed by class, bond type and modifier: S-group {succ
  adjacent 10, same remote -5, same adjacent -5}; P-group {pred adjacent 10, same remote -5,
  same adjacent -5}; C-group {same adjacent 10, same-span adjacent 3, succ adjacent -5, pred
  adjacent -5}; Y-group {same remote 10}; Constant {same adjacent 10}; Countup {succ adjacent
  10}; Cycle {same remote 5}; Tuple {same remote 5}. Unlisted combinations are 0. A sameness
  bond on the field span is keyed `Same-span`; the other entries are on print-value.
- **Reason:** the thesis's values where given; starting values for the rest, with
  P-group as S-group's mirror, C-group's span pull from p. 91 and the fence classes' remote
  sameness from "the fact that the group repeats" [M86 p. 55].
- **Knob:** `Config.bond_values`.
- **Alternatives:** stronger fence pulls; pushes for the fence classes.
- **Amended (item 15):** Cycle and Tuple list no pull or push bonds at all (the starting table's remote
  sameness pull of 5 for both is dropped). Reason: run 2's `slip-check: stayval: 0.0`
  [M86 p. 168] is printed with the remote sameness bond `bond1` (glint1-glint3 [M86 p. 167])
  lying between the gnoths `(3 7)` and `(3)`; a pull of 5 there would be an external pull
  halved into each gnoth, a staying pressure of -5.0, and a push would give +5.0; only 0
  gives 0.0. Consequences, recorded for item 17: Cycle's "strongest pull-bond strength"
  [M86 p. 105] is 0, so a Cycle reformulation starts with a threshold of 0 and no bond is
  ever "sufficiently strong" [M86 p. 124], which is consistent with run 2, where the
  Gnoth-caster casts without any gnoth operation [M86 p. 168]; and a reigning Cycle or
  Tuple can never slip on bond pressure (its staying pressure is always 0 and D-042 needs a
  strictly greater pressure), only the blast (D-050) leaves it.

### D-040: The strict classes
- **Sources:** "if a reigning class -- such as Constant -- is very strict ... slipping is chosen
  immediately" [M86 p. 99]; the transcript slips from Countup at the first Reformulator with no
  `slip-check` printed [M86 pp. 173-174] (SPEC 11.2, 14.6 item 4).
- **Choice:** `Config.strict_classes` = (Constant, Countup); a strict reigning class slips at
  once to its most slippery s-link neighbour, which for Countup is S-group
  and for Constant is C-group; a tie between two links goes to the one listed first in
  `Config.s_links` (`IdealType.most_slippery`, item 11).
- **Reason:** p. 99 and the transcript.
- **Knob:** `Config.strict_classes`.
- **Alternatives:** Constant only.

### D-041: How "recent tries" enter the slip decision
- **Sources:** "The old hypotheses are checked to determine the number of recent hypotheses of
  this class -- how many 'tries' the class has had since it seized power" [M86 p. 99]; how the
  count is used is not said; the transcript's second S-group refutation prints `stayval: 15.0`,
  the raw stability sum [M86 p. 176] (SPEC 11.2, 11.6, 14.6 item 4).
- **Choice:** the staying pressure is multiplied by `stay_factor_per_try` (0.5) once for every
  freeze-dried hypothesis of the reigning class since it seized power, not counting the
  hypothesis just refuted; the printed `stayval` is the multiplied value.
- **Reason:** the transcript's 15.0 requires the first try to count for nothing; halving on each
  later try is the simplest penalty that makes a class that keeps failing lose the throne.
- **Knob:** `Config.stay_factor_per_try` (0.5).
- **Alternatives:** a hard limit on tries; no effect (then the count would be pointless).
- **Note (item 15):** implemented as stated; `recent_tries` counts the freeze-dried
  hypotheses of the reigning class at the end of the list, less one, and the test of
  p. 176's state gives 15.0 with the list `(Countup 1)`, `(S-group 1 2)`.

### D-042: Which task makes and prints the slip check, and `best: nil`
- **Sources:** `slip-check: stayval: 15.0 best: (Y-group 4.0)` follows the Gnoth-setter's
  `gnoths:` line directly [M86 pp. 168, 176]; no slip-check is printed before the strict
  Countup's slip [M86 pp. 173-174]; `best: nil` is printed with `stayval: 0.0` when no bonds
  exist [M86 p. 168] (SPEC 11.6, 14.6 items 4-5).
- **Choice:** the Gnoth-setter ends by making the decision: for a strict class it slips silently
  (D-040); otherwise it computes the staying pressure and every neighbour's slipping pressure,
  prints `slip-check: stayval: S best: (class p)` with the largest pressure, or `best: nil` when
  the largest is 0, slips when the largest is strictly greater than the staying pressure, and
  posts a Reformulator. A slip prints `Changing to a new reigning class. X --> Y` (Meredith's
  annotation wording, which the program never printed; marked as the reconstruction's line).
- **Reason:** the transcripts' placement; "strictly greater" is p. 104's "greater than".
- **Knob:** none.
- **Alternatives:** a separate slip-check task; `nil` only when a class has no
  s-links.
- **Amended (item 15):** the slip is announced by the task that makes it, the Gnoth-setter,
  for a strict class too: `Changing to a new reigning class. Countup --> S-group` follows
  `gnoths: (gnoth1 gnoth2 gnoth3)` directly, where the transcript has Meredith's annotation
  under the first `doing task Reformulator` [M86 pp. 173-174]; the line is the
  reconstruction's, not the program's, and item 20's comparison must not count it. The
  printed check is two lines, `slip-check: stayval: 15.0` and `        best: (Y-group 4.0)`,
  as the printout has them [M86 pp. 168, 176]. A slip clears the threshold (D-046) and
  re-assesses the bonds under the new class, which is why p. 175's `show` prints S-group's
  values after the slip from Countup.

### D-043: The Reformulator's pull between gloms, "with each in the list", and the direction of a move
- **Sources:** the selection order of p. 107 pairs the new element of one chain with a list of
  the other's ("assess pull with each in the list") [M86 p. 107]; "the strength of the operation
  ... the difference between the gloms' mutual attraction and the pull exerted by other gloms
  to hold them in place" [M86 pp. 108-109]; "Single gloms are the most likely to move, leaving
  an empty gnoth behind" [M86 p. 108]; the transcript's first move is `SHIFT-RIGHT gnoth1 gnoth2
  (glint1)` for two single glints `1`, `2` [M86 p. 174] (SPEC 12.2, 12.3, 14.6 item 6).
- **Choice:** the pull between two gloms is the sum of the assessed values (positive only) of the
  bonds between any glint of one and any glint of the other; "with each in the list" takes the
  best pair in the list; the holding pull of a glom is the sum of the positive values of its
  bonds to its siblings in its own gnoth; the operation's strength is the pull minus the larger
  of the two holdings and must be strictly greater than the threshold; the glom with the
  smaller holding moves, a tie going to the one with fewer glints, then to the left one (moving
  right); the first candidate in p. 107's order that is strong enough wins and the
  Reformulator stops.
- **Reason:** p. 108-109's definition; the transcript's SHIFT-RIGHT on a tie; p. 107's stopping
  rule ("will not suggest any further moves").
- **Knob:** none.
- **Alternatives:** a weighted random draw among the strong-enough moves (the thesis's "weight
  competing alternatives" could mean that).
- **Amended (item 16):** the strength is the pull minus the *mover's* holding, not the larger
  of the two holdings. With the larger, the "real move" of p. 122 could never be proposed: in
  Figure 1 the 3 (holding 0) is pulled by the 2 of `(1 2)`, whose holding is the adjacent
  successor bond to its 1, worth 10 under S-group [M86 pp. 101-102], so the strength would be
  10 - 10 = 0 against a threshold of 8 [M86 p. 105], yet the thesis says "the 2 <--> 3 pull is
  resolved via" the PROGRAM [M86 p. 122]. With the mover's holding it is 10 - 0 = 10. The
  transcript's `SHIFT-RIGHT gnoth1 gnoth2 (glint1)` (two single glints, both holdings 0) is
  unchanged [M86 p. 174]. The Reformulator of item 17 implements it.

- **Amended (item 17):** the pull between two gloms is the sum of the positive values of
  the bonds between the two *themselves* (a bond's members are exactly those two elements),
  not of the bonds between their parts: "the decisive pull on it may be coming from a
  deeply-nested glom ... In Figure 2 for example, glint4 may be pulled toward glom3"
  [M86 p. 108], and p. 107's order reaches that pull at the nested glom's own level. Counting
  the parts' bonds for the containing glom would make level 1 of Figure 1 (`(1 2) <--> 3`)
  find the 2-3 bond and shift the 3 beside `(1 2)` instead of into it, which is not p. 122's
  "real move". The holding of a glom is the sum of the positive values of the bonds from
  anything within it to the other elements of its gnoth (what keeps it where it is). "With
  each in the list" takes the move of greatest strength in the group, ties to the first
  listed. The strength must be strictly greater than the threshold ("exceed", p. 107). One
  finding: the transcript's second move, `(SHIFT-RIGHT gnoth3 gnoth5 (glint3))` on the gnoths
  `(1 2) (1 2) (3)` [M86 p. 177], is not derivable by the rule of p. 106 from the bonds the
  transcript prints at that moment (bond1-bond3, bond5, bond6, bond7): no bond joins the last
  of gnoth3 (the 2, glint4) to the first of gnoth5 (the 3, glint5) until `bond8` is made after
  the Gnoth-operator ran [M86 p. 177]; the reconstruction finds that move once bond8 exists
  (RESULTS.md). The order over the parse is D-068.

### D-044: PROGRAM construction for a deep move
- **Sources:** the PROGRAMs of p. 122 and the rule that "the remainder of the PROGRAM is designed
  simply and mechanically to repair any concomitant structural damage" by "re-enclosing the
  remaining gloms at each intermediate level" and setting out Plato-scouts [M86 pp. 122-123]
  (SPEC 13.5).
- **Choice:** a move of glom *m* (depth *d* in its gnoth) toward glom *t* (depth *e* in the
  neighbour) is the shift of *m* (bursting the *d* gloms above it), a CAPTURE of *m* by *t*'s
  parent when *t* is nested (bursting the *e* gloms above *t*), then one ENCLOSE per burst glom
  on each side, innermost first, over that glom's remaining subgloms (`nil` when none), and a
  Plato-scout for each new glom; when *m* is a top-level glom the PROGRAM is a single shift plus
  `(ENCLOSE gnoth nil)` for each gnoth touched, as the transcript prints [M86 p. 174].
- **Reason:** p. 122's three PROGRAMs follow this pattern exactly.
- **Knob:** none.
- **Alternatives:** no repair (the thesis forbids it, p. 108).
- **Amended (item 16):** the deep move's later ENCLOSEs name gloms that only exist once the
  earlier steps have run (the thesis's `newglom3`, `newglom10` [M86 p. 122]); a PROGRAM step
  refers to the glom made by an earlier step as `New(k)` (printed `(new k)`), resolved by the
  Gnoth-operator when it reaches the step (`operations.New`). The bursting that "all gloms
  containing it must be destroyed" [M86 p. 109] calls for is a *dissolving* of each containing
  glom, outermost first, so that the siblings at every level survive (p. 114's glom5, p. 116's
  glom4, glom6 and glom8 are all still there afterwards); a glom is never burst to its glints
  by an operation. Every new glom (CAPTURE, ENCLOSE, MERGE) gets one Plato-scout for all six
  classes (D-022), "Plato-scout tasks are placed on the taskrack to peruse the gnoth's
  newly-created gloms" [M86 pp. 109-110]; whether every one does ("Often") is not said.

- **Amended (item 17):** a pull between two top-level gloms is one bare operation, not a
  PROGRAM: "When more than a straightforward top-level move is to be required, a Reformulator
  must create a PROGRAM" [M86 p. 121], and p. 122's high-level move is "translated into the
  move: (SHIFT-LEFT gnoth3 gnoth4 (glom8))"; the transcript's second move is printed bare,
  `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` [M86 p. 177]. The first move's wrapper on p. 174,
  `((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil))))`, is therefore not
  reproduced for a top-level glint (the reconstruction prints `((SHIFT-RIGHT gnoth1 gnoth2
  (glint1)))`); it would follow from glint1 having been nested under a glom of its own in
  gnoth1, which p. 173's five `top-down glom` lines for three gnoths may mean. When the mover
  or the target is nested, the PROGRAM is: the shift; a CAPTURE of the mover by the target's
  parent glom when the target is nested (`(CAPTURE-RIGHT gnoth2 glom2 glint5)` [M86 p. 122];
  no capture when the target is top-level, even if it is a glom); then one ENCLOSE per glom
  burst on the mover's side, innermost first, then per glom burst on the target's side above
  the captor, each over the burst glom's remaining subgloms plus the glom the previous step
  made (`New`), or `(ENCLOSE gnoth nil)` when nothing remains (p. 122's "siblings-of-glom1"
  is empty in Figure 2). No ENCLOSE is written for a gnoth in which nothing was burst, so the
  two `nil` ENCLOSEs of p. 122's real move, "no repair necessary", are not printed. The
  mover's old gnoth may be gone when a `nil` ENCLOSE names it; that is not a failure (D-067).
- **Note (item 21):** over 100 seeds of run 3 the first move is always the bare
  `((SHIFT-RIGHT gnoth1 gnoth2 (glint1)))`, so p. 174's PROGRAM wrapper is never printed
  (milestone 28 of `docs/appendix-runs.md`); the second move prints p. 177's own
  `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` on 6 seeds, as the second of two shifts (the 2
  first, then the 1 carried after it, D-065), and `((SHIFT-LEFT gnoth3 gnoth5 (glint5)))`,
  the 3 moved left to the same parse, on 79 (D-043's holding rule).

### D-045: When NO-OP is proposed and the Capper
- **Sources:** NO-OP uncaps a gnoth until "the Capper task it sets out is invoked and recaps the
  gnoth" [M86 p. 113], permitting natural glomming inside and between uncapped gnoths; when a
  Reformulator proposes it is not said (SPEC 13.2, 14.6 item 6).
- **Choice:** a Reformulator that finds no neighbour pull above the threshold proposes
  `(NO-OP gnoth)` for the gnoth whose internal pushes (absolute sum, assessed) exceed the
  threshold, if any; the Gnoth-operator suspends the gnoth-glom and posts a Capper (urgency 2,
  D-008); the Capper claims every current glom whose range overlaps the gnoth's original range,
  the first Capper to run winning a shared glom, and recaps.
- **Reason:** p. 113's description; internal pushes are the one pressure the thesis names that
  no other operation relieves (divesting pushes being unimplemented, p. 123).
- **Knob:** none.
- **Alternatives:** never propose NO-OP (then the Capper is dead code).
- **Amended (item 16):** the Capper's claim is every top-level element overlapping the
  original range that is not another gnoth's cap; the cap goes over the longest run of them
  (two uncapped neighbours whose gloms combined across the border: the first Capper to run
  takes the combined glom and its range extends, the second takes what is left, "the gnoth
  that recaps first will get the glom and the extended range" [M86 p. 113]); a gnoth with
  nothing left to claim disappears. The Capper's arguments are the gnoth's name and its
  original range, `(gnoth1 (1 3))`.

- **Amended (item 17):** when no neighbour pull is strong enough, the NO-OP goes to the
  gnoth whose internal pushes (by absolute value, from the lists `assess_bonds` filled) are
  the largest and exceed the threshold; a Reformulator never proposes both a move and a
  NO-OP (`reforms.find_no_op`).

### D-046: When the threshold is cleared
- **Sources:** the threshold is "1 more than the existing threshold value or, if none exists, 80%
  of the strongest pull-bond strength for the reigning type" [M86 p. 105]; -1 per Gnoth-operator
  [M86 p. 124]; both reformulations of run 3 start again at 8.0 [M86 pp. 174, 177] (SPEC 12.1,
  14.1, 14.6 item 6).
- **Choice:** the threshold is cleared (set to none) by the Gnoth-setter and by a slip, so each
  reformulation episode and each reigning class starts at `threshold_fraction` (0.8) of the
  strongest pull; the Bond-assessor's printed number is the current threshold.
- **Reason:** run 3's two episodes both begin at 8.0; kept from one term to the next, the
  threshold would only grow.
- **Knob:** `Config.threshold_fraction` (0.8, [M86 p. 105]).
- **Alternatives:** never clear (contradicts p. 177).

### D-047: The Bond-assessor's test
- **Sources:** "If there are sufficiently strong bonds to warrant further reforms, the
  Bond-assessor places a Reformulator on the taskrack. If not, it creates a Gnoth-caster"
  [M86 p. 124]; on p. 178 a Bond-assessor at 10.0 yields to the Gnoth-caster while bonds of
  value 10 exist [M86 pp. 178-179] (SPEC 14.2, 14.6 item 7).
- **Choice:** the Bond-assessor posts a Reformulator when some bond's assessed value in the
  reigning environment, in absolute value, is strictly greater than the threshold, and a
  Gnoth-caster otherwise.
- **Reason:** the transcript's 10.0 giving way with a 10 present; "exceed" on p. 107.
- **Knob:** none.
- **Alternatives:** `>=` (contradicts p. 178); pulls only.

- **Amended (item 17):** the Bond-assessor is set out with the reigning class and the
  threshold of the moment, `(S-group 8.0)`, and tests against the threshold it was given,
  not the one current when it runs (D-068): on p. 177 the Gnoth-operator has lowered the
  threshold to 7 before `Bond-assessor on (S-group 8.0)` runs, and either reading gives the
  same decision there (10 > 8 and 10 > 7). Set out without arguments it reads the world's
  threshold, 0 when none is set.

### D-048: Casting a gnoth and generalising the casts (group classes)
- **Sources:** "a Gnoth-caster attempts to describe each gnoth in terms of the reigning class"
  [M86 p. 124]; `(3 4) (3 4) (3)` gives `(S-group 3 2)`, `(1 2) (1 2 3)` gives
  `(S-group 1 (Countup 2))` [M86 p. 125]; `((S-group 1 2) (S-group 1 1))` gives `(S-group 1 2)`
  [M86 p. 174]; "errors or poor castings are possible" [M86 p. 125] (SPEC 14.3, 14.6 item 8).
- **Choice:** a gnoth is cast from its flattened terms: for C-group, S-group and P-group as
  `(class first length)` when the terms pass the class's process (length 1 included); for
  Y-group as `(Y-group first length (terms))` when symmetric; for a repeater as its single term;
  for the fence classes by D-037. Generalisation is parameter by parameter over the casts:
  equal values stay, values counting up by one become `(Countup first)`, anything else fails;
  if all casts generalise, that is the candidate; else if all but the last do and the last
  "shows promise", the head's generalisation is the candidate, "promise" meaning that the last
  gnoth's terms are a prefix of the head form's next frame. A cast that is a nested form is not
  attempted (the thesis's "(C-group (S-group m n) ...)" castings are future work, p. 125).
- **Reason:** the four examples; parameter-by-parameter generalisation and this "promise"
  reading reproduce all of them in the tests.
- **Knob:** none.
- **Alternatives:** generalise two parameters jointly (`(S-group (Countup 1) (Countup 2))`).
- **Note (item 18):** built in `casting.cast_terms`, `generalise_casts`, `promise_frame` and
  `cast_gnoths`. Details settled by the transcripts: the printed `casts:` list holds every
  gnoth's cast, the promising last one included (`casts: ((S-group 1 2) (S-group 1 1))` then
  `(S-group 1 2)` [M86 p. 174]), and a gnoth that cannot be cast prints as `nil`; the
  generalised form is printed on its own line, as run 3 does (run 2's printout runs the list
  and the form together on one wrapped line [M86 p. 168]); the Y-group's elements are
  generalised position by position when every cast has the same length, with the same
  rule (`(Y-group 1 3 (1 (Countup 2) 1))`); a repeater class's casts generalise to `(Constant
  v)` or `(Countup first)`; "shows promise" is tested on the frame after the frames the other
  gnoths stand for (`promise_frame(head, k)`), one turn for a Cycle (D-004). The Gnoth-caster
  does not re-mark the gnoths' `equivalence-type`: p. 175's `parse` on gnoth2 and the absence
  of a mark on gnoth5 at p. 179 (which is parse-equivalent to its frame) are both the
  Gnoth-setter's marks surviving the operations, so the caster leaves the fields alone.
  Nothing is printed for a failure (no transcript shows one).
- **Amended (item 21):** the Gnoth-caster writes each gnoth's cast into its `form` field
  (`casting.record_casts`): after `casts: ((S-group 1 2) (S-group 1 1))` `show` prints
  `form: (S-group 1 2)` and `form: (S-group 1 1)` [M86 pp. 174-175], after `((S-group 1 2)
  (S-group 1 3))` `form: (S-group 1 3)` [M86 pp. 178-179]. A gnoth with exactly one cast
  gets it; one with several (every term under a fence class, D-037) or none keeps its
  field, a case no transcript prints.

### D-049: The Slip-scout's effect
- **Sources:** "Slip-scouts are only skeletally implemented in the current system" and "the
  probability of a class change will increase" [M86 p. 126]; the rest is described as intention
  (SPEC 14.5, 14.6 item 9).
- **Choice:** a Slip-scout repeats the slip-or-stay check with the staying pressure multiplied
  by `slip_scout_factor` (0.5), prints a `slip-check` line, and posts a Reformulator; nothing
  more (the frequency study and the interleaving sensitivity are not built; any later version is
  an extension, off by default).
- **Reason:** the one effect the thesis states as fact.
- **Knob:** `Config.slip_scout_factor` (0.5).
- **Alternatives:** force a slip to the best neighbour.
- **Note (item 18):** built in `casting.slip_scout`: the task repeats `slipping.slip_or_stay`
  with the staying pressure multiplied by `Config.slip_scout_factor` (a strict class slips at
  once, D-040; otherwise the check is printed as the Gnoth-setter prints it, D-042), then sets
  out a Reformulator so that the episode continues under whatever class reigns; the threshold
  is cleared only when the class changes (D-046), so a stay keeps "cooling". Nothing of the
  frequency study or the interleaving sensitivity of p. 126 is built; any later version is an
  extension, off by default.


### D-050: "No progress for quite some time" and the blast
- **Sources:** "when no progress has been made for quite some time, to blast away all gnoths and
  gloms, leaving only the glints and their bonds to push up an inspiration" [M86 p. 92]; "a
  return to the term level, with all perceived groupings eradicated but with accumulated
  knowledge of term samenesses and other primitive relations maintained" [M86 p. 39] (SPEC 9.5,
  14.6 item 1).
- **Choice:** progress is measured in Gnoth-caster failures within one reformulation episode
  (a candidate made resets the count); at the `max_cast_failures`th (3) failure the Gnoth-caster
  blasts: every gnoth, pseudo-glom and glom is destroyed, bonds attached to destroyed gloms go,
  bonds between glints stay, the hypothesis and reigning class are cleared, the template is
  forgotten, and stage one resumes with a Sparkler-plus for every glint. Scrapping (p. 39) is
  this same action.
- **Reason:** the two passages; 3 is an engineering choice.
- **Knob:** `Config.max_cast_failures` (3).
- **Alternatives:** count tasks or terms without a new hypothesis.
- **Note (item 18):** built in `casting.fail` and `casting.blast`. The count lives in
  `World.cast_failures`; the Gnoth-setter sets it to 0 (a new episode) and so does an adopted
  candidate. The blast: every gnoth (the catchall included) is destroyed with its gnoth-glom,
  every glom in the cytoplasm is burst (the bonds attached to a destroyed glom go with it,
  D-059; bonds between glints stay), the reigning class, the threshold and the template are
  forgotten, the waiting reformulation tasks are removed (the `reform` cleaning of D-010), and
  "their bonds ... push up an inspiration" [M86 p. 92] is read through p. 65: each surviving
  bond releases what a new bond releases, `Config.sparklers_per_bond` Sparklers and
  `Config.glom_scouts_per_bond` Glom-scouts (D-016), instead of the "Sparkler-plus for every
  glint" the entry had said, which D-015's Sparkler-plus (a new element against the others)
  did not fit. The reconstruction prints its own line, `blasting away all gnoths and gloms`
  (the printouts show no blast). Stage one then runs on the glints: on `3 7 3 7` a cover seed
  blasts after three failed casts and, with the sequence continued, gloms the fence and
  guesses `(Cycle 3 2 (3 7))` on most seeds (RESULTS.md). *Alternatives:* more Glom-scouts
  (one per glint) to give the scan more draws after a blast; a Sparkler-plus per glint.


### D-051: Equivalence-type marking
- **Sources:** each gnoth is "marked with the frame and equivalence type (term or parse,
  depending on agreement with the hypothesis' parenthesization)" [M86 p. 98]; structural
  equivalence and form polishing are unimplemented [M86 p. 97] (SPEC 7.5, 10.1).
- **Choice:** a gnoth is `parse`-equivalent when its parse-print (the parenthesised print of its
  glom collection) equals the hypothesis frame's print, `term`-equivalent otherwise; the mark is
  recorded, shown by `show`, and never used to change a gnoth. *Detailed (item 13):* the
  parse-print is the list of the collection's print-values, `((1 2) 3)` for a gnoth over
  `glom1 (1 2)` and `glint3`, `(1 2 3)` for one over three glints [M86 p. 95]; so a gnoth the
  Gnoth-maker puts over one dubbed glom `(1 2)` has the parse-print `((1 2))` and is only
  term-equivalent to the frame `(1 2)`, and `show-parse` before any reformulation prints
  `(((3 7)))` for run 1's parse (no transcript shows `show-parse` at that point; after a
  reformulation the gnoth-gloms stand over glints and it prints `((1 2) (1))` [M86 p. 174]).
  Structural equivalence is a check (`structural_equivalent`, with `deep_frame` and
  `shallow_frame` reproducing p. 96's frames) that nothing calls in a run.
- **Reason:** p. 98; p. 95's two examples; the limitation of p. 97 kept.
- **Knob:** none.
- **Alternatives:** read a collection of one glom through to that glom's print-value (then a
  Gnoth-maker gnoth would be parse-equivalent at once, which is what p. 98's "the two
  pre-existing gnoths ... would be marked as parse-equivalent" suggests if those gnoths stood
  over gloms rather than glints; p. 98 does not say which).

### D-052: The Hfilter line after a refused guess
- **Sources:** after `nope`, the next term is filtered "through (1 2)1", the parse rather than
  the form [M86 p. 176] (SPEC 15.5 item 13).
- **Choice:** the Hfilter always prints the hypothesis form; the parse-print variant is not
  reproduced (recorded in RESULTS.md as a format difference).
- **Reason:** nothing explains the variant; one format is simpler to read beside the transcripts.
- **Knob:** none.
- **Alternatives:** print the parse after a refusal.

## The user protocol

### D-053: Answers to a guess
- **Sources:** `enter no if wrong, ok if right`; `ok` ends the run with `bye`; the transcript's
  user types `nope` and the program asks for the next term [M86 pp. 166, 175-176] (SPEC 1.4).
- **Choice:** `ok` ends the run; any other answer counts as a refusal; in the non-interactive
  mode the scripted answers are used in order and, when none is left, the guess is judged by
  D-033 against the remaining scripted terms. *Amended (item 14):* `ok` is read in any case
  and spacing; a refusal sets the validity to 0 and posts a Call-term (D-032); after `ok` the
  world is `done`, nothing runs, and a term entered afterwards sets out no Hfilter. The
  answering object is D-064's.
- **Reason:** the transcripts.
- **Knob:** none.
- **Alternatives:** treat an unknown answer as an error.
- *Noted (item 19):* at the console the answer is read from the user on the prompt line, as
  the transcripts show; the end of the input while a guess waits counts as a refusal (the
  session then ends at the next prompt); scripted answers are used before the user is asked
  (D-069).

### D-054: The console commands and the `show` format
- **Sources:** `show`, `show-hypothesis`, `show-seq`, `show-parse` and their printed output
  [M86 pp. 167-168, 171, 174-179] (SPEC 1.4, 7.2).
- **Choice:** the four commands print as the transcripts do, with the gnoth fields in the order
  of p. 168 and, after a reformulation, the four bond and equivalence fields first as on p. 175;
  `bye` quits; any other input is read as a term when it parses as a nonnegative integer and is
  otherwise re-prompted.
- **Reason:** the transcripts.
- **Knob:** none.
- **Alternatives:** none.
- *Noted (item 19):* an input that is neither a term, a command nor `bye` is re-prompted
  after one line of the reconstruction's own (`(enter a nonnegative integer, show,
  show-hypothesis, show-seq, show-parse or bye)`), since a silent re-prompt would leave a
  user guessing; `show-hypothesis` before a hypothesis exists prints `nil` (Lisp's empty
  answer; no transcript asks then); `show` lists every glom in creation order, nested ones
  included and glints left out, and keeps a heading whose section is empty (p. 167 lists
  `glom1` under its gnoth-glom `glom3`; no transcript shows an empty section); a pseudo-glom's
  line has two spaces before `terms`, a bottom-up glom's one, as every glom line of the
  transcripts has [M86 pp. 167, 171, 175, 178]; the commands are read in any case.

### D-055: The trace imitates the transcripts; counts and order are not milestones
- **Sources:** "no assumptions can be made about which of two competing tasks will run first"
  [M86 p. 41]; no seed is printed (SPEC 15.5 item 16).
- **Choice:** the text trace prints the transcripts' lines in their formats (`doing task X on
  (...)`, `sparkN --- between ...`, `top-down glom glomN`, `I have a guess!`); the engine emits
  typed events and the trace is one observer; RESULTS.md compares milestones in order, formats
  and mechanism-derived numbers, never task counts or spark numbers.
- **Reason:** the task order is random and the seed unknown; this is what SPEC 15.8 says is
  measurable.
- **Knob:** `Config.verbose` (True: print `doing task` lines, as runs 1 and 3; False: run 2's
  terse mode).
- **Alternatives:** none.
- **Amended (item 20):** the comparison is made by `seekwhence/milestones.py` and
  `seekwhence/bench.py` (`python3 -m seekwhence.bench appendix`): every milestone of
  `expectations.json` is turned into the lines it requires and looked for within the turn it
  was printed in (the k-th input opens the k-th turn, the inputs being hers), in two readings:
  *exact* (the lines as printed, in the printout's order) and *reached* (in its turn, in any
  order, every structure number but a glint's a wildcard). The first-appearance order of task
  types is summarised as the share of the printout's consecutive pairs that hold. One seed's
  whole trace per run is kept as a golden file (`tests/data/golden_run*_seed54.txt`), never
  regenerated to make a failing test pass. The Gnoth-setter's
  milestone is its task line, its first `top-down glom` and the `gnoths:` list; the number of
  its top-down gloms and p. 168's `groups:`/`glom:` lines are not reproduced (D-036).
  `docs/appendix-runs.md` has the rates and every divergence with its cause.

- **Amended (item 21):** `set-out -- validity: 0` is printed by the terse trace only: run
  2, without task lines, prints it after the filtering line [M86 p. 168], and run 3, with
  every task line, prints it at neither of its refutations [M86 pp. 171-172, 176]; the one
  rule that fits all three printouts is that the mode without task lines announces the
  refutation itself. The bench replays each run in its printout's mode (run 2 terse) and
  drops the `doing task` needles for a terse trace; it also judges every guess against the
  whole sequence (D-033), since Meredith's answers are replayed blindly. The assessed bond
  lists are built by the gnoth's elements from right to left, a bond's creation order within
  one element, which is the order of every printed list (`((bond2 0) (bond5 -5) (bond6 0)
  (bond3 -5))`, `((bond6 0) (bond5 -5) (bond2 0) (bond3 -5))`, `((bond8 10) (bond7 10))`
  [M86 pp. 175, 179]; `slipping.assess_bonds`). Run 3's golden file is
  `tests/data/golden_run3_seed30.txt`; run 2's was regenerated in item 21 (PROGRESS.md).

## Later additions

### D-056: The box tree's nodes and their numbering
- **Sources:** Figure 3 [M86 p. 83] draws `(C-group (S-group 2 3) 2)` as one box and one
  printstructure per structure, the top box first (`Box1`, then `Box2` and its subtree `Box3`,
  `Box4`, then `Box5`), each integer argument a Constant printstructure with a `value`; the
  shared variant has "(box3 box5)" in Pstruc3's `boxes` field "and there would be no need for
  Pstruc5" [M86 p. 84]; `(Cycle (8 (Countup 1) shared))` "requires the creation of three
  boxes: one for the 'Constant 8', one for the first 'Countup 1', and one for the second
  'Countup 1'" [M86 p. 84]. The appendix's `build-box` returns `box5`, `box12` and `box19` for
  its three forms [M86 pp. 161-163], so the original made 5, 7 and 7 boxes for them, which
  Figure 3's scheme does not give (4, 5 and 5: the shared structure's second box, but no
  box for a fence form's start or length, and none for the bound structure itself).
- **Choice:** the tree is Figure 3's: one box per argument structure (an integer, a form or
  a `Shared`), one printstructure per distinct structure, a `Shared` adding a box to the
  printstructure of the structure it shares, no box for a fence form's derived start and
  length; boxes and printstructures are named `boxN` and `pstrucN` from one counter each
  (D-014), the top box first and each subtree before its right-hand sibling, as Figure 3
  numbers them. The appendix's box numbers are not reproduced and are not milestones; the
  tests check the tree's shape and its values, not the numbers.
- **Reason:** Figure 3 is the thesis's one explicit picture of the tree; the appendix's
  numbers need two assumptions (the binding list builds a box for the bound structure, and a
  fence form has one more box, for its length or its start, with the top box numbered last)
  that the text nowhere states.
- **Knob:** none.
- **Alternatives:** the reading just given, which does reproduce `box5`, `box12`, `box19`
  (post-order naming, a box for each bound structure, a box for a fence form's length).

### D-057: The arena of each task type
- **Sources:** "Each task operates in one of three arenas: conceptual (platoplasm), short-term
  analytical (socratoplasm), or perceptual (cytoplasm)" [MHN p. 3]. The thesis places each task
  by level in its prose (SPEC 2.4's "Level" column): the cyto tasks [M86 pp. 63-70], the
  template tasks at "the template level" [M86 p. 74], the Plato-scout and Plato-evaluator
  between the plato and cyto levels [M86 pp. 68-69], the gnoth tasks in the socratoplasm
  [M86 pp. 76-79, 105-125], the Slip-scout in the platoplasm [M86 p. 126]; it never names an
  arena for Call-term, Hfilter or `empty-task`.
- **Choice:** the table `taskrack.ARENAS`: the cytoplasm for Sparkler, Sparkler-plus, Tester,
  Bonder, Glom-scout, Glomtester, Glommer, Glom-inspector, Burster, Dissolver and the five
  template tasks (the template is a pseudo-glom in the cytoplasm, Figure 1 [M86 p. 44]); the
  platoplasm for Plato-scout, Plato-evaluator and Slip-scout (they apply the classes' tests
  and study slipping); the socratoplasm for Gnoth-maker, Hypothesizer, Hfilter (the
  hypothesis sits beside the socratoplasm, Figure 1), Gnoth-setter, Reformulator,
  Gnoth-operator, Capper, Bond-assessor and Gnoth-caster; `user` for Call-term and
  `empty-task`, which talk to the user and touch no structure. The arena is a field of every
  task record and of the `TaskPosted` and `TaskChosen` events; nothing in the engine's
  behaviour depends on it.
- **Reason:** the notes' statement and the thesis's placement of each task; the arena is kept
  because the notes make it a property of every task, and a trace or a view may group tasks
  by it.
- **Knob:** none.
- **Alternatives:** the template tasks as a fourth "template" level, as the thesis's prose has
  it [M86 p. 74]; Plato-scouts in the cytoplasm, since they act on gloms.

### D-058: Which spark a Tester tests, and the spark's life
- **Sources:** "When a spark is created between two gloms, a horde of 'Testers' is placed on the
  taskrack. When invoked (chosen to run) at some later time, each Tester chooses some spark, not
  necessarily the one whose creation caused the tester's creation" [M86 p. 64]; the Tester is
  printed without arguments (`doing task Tester` [M86 p. 164]); nothing says what becomes of a
  spark once tested, and no spark is printed twice. In run 1 twelve sparks meet twelve Testers,
  in run 3 thirty meet thirty [M86 pp. 164-179] (D-016).
- **Choice:** a Tester draws one spark, through the run's `Rng`, uniformly from the sparks now in
  the cytoplasm, removes it (a spark is tested once and then gone) and tests its members; with
  no spark it does nothing. A spark whose member is gone, or whose members have since become
  nested, is used up without a Bonder.
- **Reason:** p. 64's "chooses some spark"; consuming the spark keeps the cytoplasm from filling
  with sparks that were tested already, and with one Tester per spark (D-016) every spark is
  tested once in a run where every task runs.
- **Knob:** none.
- **Alternatives:** sparks that stay until a new term and may be tested several times; the Tester
  prefers the newest spark.

### D-059: Destroying a glom removes its bonds and sparks
- **Sources:** a bond "links the two gloms" [M86 p. 64]; a Tester and a Bonder check that their
  elements "still exist" [M86 p. 64]; the thesis does not say what becomes of the bonds of a
  glom that is dissolved or burst [M86 pp. 62, 70]. In the transcripts a bond attached to a
  glom that was later burst is never shown again.
- **Choice:** when a glom is dissolved or burst, every bond and every spark with that glom as a
  member is removed from the cytoplasm and from the other member's `bonds-in`; the bonds among
  its subgloms survive with the subgloms. The glom is also dropped from the manifestation
  lists of the ideals it was dubbed as (`Platoplasm.forget`, item 11).
- **Reason:** a bond with one end gone links nothing; keeping it would let a Glom-scout follow
  a bond to a structure that is not there (D-009). A manifestation is a cyto-level structure
  "which [has] been dubbed" [M86 p. 90], and a destroyed glom is no structure.
- **Knob:** none.
- **Alternatives:** keep the bonds as "facts of note" [M86 p. 62] with a dead member.

### D-060: When the Template-scout applies the form itself, and one application per dubbing
- **Sources:** "Whenever a glom is dubbed as a manifestation of some plato-type, a
  Template-scout process is placed on the taskrack" [M86 p. 73]; "the scout would set out a
  Template-applier task to determine if the entire sequence seen is of that form" [M86 p. 74].
  Run 1 prints `Template-scout on (glom1)` then `Template-applier on (glom1 (Cycle 3 2
  (3 7)))` for `3 7 3` with `(3 7)` glommed [M86 p. 165]; run 3 prints `Template-scout on
  (glom1)` followed at once by `create-template-glom (S-group 1 2)(glom1)` for `1 2` with
  `(1 2)` glommed, no applier [M86 pp. 170-171]. "A glom can be dubbed more than once"
  [M86 p. 69], each dubbing setting out a scout.
- **Choice:** the scout takes up the dubbings of its glom that no scout has taken up yet (a
  counter on the glom); when the glom covers the entire sequence entered, it applies each form
  at once (the fit cannot fail, the glom being one complete unit); otherwise it sets out one
  Template-applier per form with the glom and the form as arguments. While a template is
  alive (working or accepted) the scout does nothing: "no new templates are considered for the
  lifetime of the given template" [M86 p. 74]; the applier and the resolver check the same.
- **Reason:** the one difference between the two transcripts' scouts is whether the glom is
  the whole sequence; the counter keeps a doubly dubbed glom from being applied twice.
- **Knob:** none.
- **Alternatives:** always set out an applier (then run 3's trace has a line the page lacks).
- **Amended (item 21):** the scout's own application prints `template: (form (S-group 1 2)
  state working coverage (1 2) glom glom2)` and posts the evaluator with a nil argument,
  `doing task Template-evaluator on nil` [M86 pp. 170-171], where the applier's path prints
  `template made : (form ...` and a bare evaluator [M86 pp. 165, 167]; the printout's line
  stops after the form (as p. 165's does), which is not reproduced (`TemplateMade.immediate`).

### D-061: What a template makes and what its acceptance sets off
- **Sources:** the template glom is put "over the highest-level gloms in the cytoplasm"
  [M86 p. 73]; "What is added is direction from above: instructions to make or dissolve gloms,
  to create units of a particular form" [M86 p. 74]; the Gnoth-maker example has gloms `(1)`,
  `(1 2)` and `(1 2 3)` under the template `(S-group 1 n)` for `1 1 2 1 2 3` [M86 p. 78] and
  makes "one gnoth for each subglom of the template-glom" [M86 p. 77]; `show` prints the
  Gnoth-setter's top-down gloms as `pseudo`, one of them over a single term [M86 p. 175];
  acceptance "causes increased activity in the system, in effect 'raising the temperature'
  ... it sets off two tasks, a Gnoth-maker and a Hypothesizer" [M86 pp. 75-76]; runs 1 and 2
  print two sparks between the check line and `gnoths constructed` [M86 pp. 165, 167].
- **Choice:** every complete unit of the fit that is not already one glom (a run of free
  glints, or a single glint) gets a top-down pseudo-glom, printed `top-down glom glomN`, dubbed
  with the unit's own form (`(S-group 1 1)` for `(1)`) without setting out a Template-scout;
  the template glom, a pseudo-glom, is put over the unit gloms and printed as
  `create-template-glom <form>(<units>)` and `top-down glom glomN`; a term filtered past the
  template that completes a unit extends the template glom over the unit in place (coverage
  follows). The unit gloms a template made go with it when it is abandoned or modified
  (burst); bottom-up gloms stay. Acceptance sets out one Gnoth-maker, one Hypothesizer and
  `Config.sparklers_on_acceptance` plain Sparklers.
- **Reason:** one subglom per unit is what the Gnoth-maker needs (p. 77-78); the pseudo type
  follows p. 175's top-down gloms; the sparks of pp. 165 and 167 are the visible "increased
  activity".
- **Knob:** `Config.sparklers_on_acceptance` (2).
- **Alternatives:** the template glom directly over glints (then a gnoth per glint); real
  (non-pseudo) unit gloms; no Sparklers.

### D-062: What the Gnoth-maker does with the template glom, the frames and the gnoth's form
- **Sources:** "it creates one gnoth for each subglom of the template-glom and notifies the
  associated plato-classes of their existence" [M86 p. 77]; "Each gnoth places a pseudo-glom
  (called a 'gnoth-glom') over its glom collection (which contains just one glom initially)"
  [M86 p. 78]; the example's gnoth-gloms `glom10`, `glom11`, `glom12` each have one of the
  template's gloms as subglom, with frames 1, 2, 3 and ranges `(1 1)`, `(2 3)`, `(4 6)`
  [M86 p. 78]; the transcripts print one `top-down glom glom3` then `gnoths constructed`
  [M86 pp. 167, 171], and `show` afterwards lists `glom1` and `glom3 pseudo --> ((3 7))` but
  not the template glom `glom2` [M86 pp. 167-168, 171]; the gnoth's fields print `frame: 0`,
  `plato-class: Cycle`, `glom: glom3`, `notes: nil`, `form: (((Cycle 3 2 (3 7)) pure))`,
  `state: stable`, `range: (1 2)` [M86 p. 168] (SPEC 7.2-7.3). Whether the template glom
  survives, what `form` and `notes` hold and how frames are numbered is not said in the prose.
- **Choice:** the Gnoth-maker dissolves the template glom (its units float free for a
  moment), then puts one gnoth-glom, a pseudo-glom printed `top-down glom glomN`, over each
  unit in order, and makes the gnoth with `plato-class` the template's class, `frame` counted
  from 0 left to right, `range` the unit's first and last position, `form` the unit glom's
  structure field (its list of `(form purity)` pairs) and `notes` nil, `state` `stable`; the
  gnoth is listed as an actualization of its class; the glints beyond the template's coverage
  go to the catchall (D-030); then `gnoths constructed`. The template object stays `accepted`
  with no glom, for the Hypothesizer; the template filter does nothing once its glom is gone.
  A second Gnoth-maker, or one without an accepted template with its glom, does nothing.
- **Reason:** the `show` outputs of pp. 167-168 and 171 (no `glom2`, `glom3` over `glom1`,
  the structure field as the `form`); the program's `frame: 0` against the prose's 1.
- **Knob:** none.
- **Alternatives:** keep the template glom over the gnoth-gloms (then `show` would list it);
  the gnoth's `form` as the template's form.
- **Amended (item 15):** the Gnoth-maker makes no catchall; the glints beyond the template's
  coverage float free until the Hfilter makes the catchall at the first confirming term
  (D-030, D-063), so that the catchall's number falls after the gnoths of the parse
  (`gnoth4` in run 3 [M86 pp. 173, 176]).

### D-063: What the Hfilter does with a term: the catchall, the frame gloms, more than one term
- **Sources:** "The system stores hypothesis-confirming terms in a catchall gnoth, a special
  gnoth that simply serves as a repository for non-troublesome terms" [M86 p. 98]; in the
  example there the catchall holds "1 2 3 4", the first three terms "because they were
  predicted by the hypothesis; the '4' is the last term entered -- the troublesome one"
  [M86 p. 98]. A `top-down glom` is printed after every confirming term: `glom4` after the
  `7` that completes the turn `(3 7)` at terms 3-4 [M86 p. 166], `glom9` after a `7` that
  only begins the turn `(3 7 3)` at terms 4-6 [M86 p. 169], `glom10` after the `2` that
  completes `(1 2)` at terms 3-4 [M86 p. 175], `glom15` after a `1` that only begins
  `(1 2 3 4)` [M86 p. 179]. `glom10` is gone before the Gnoth-setter runs (`doing task
  Dissolver on (glom10)`, `glom10 is not in cytoplasm` [M86 p. 176]) while `glom9` of the first
  reformulation is still listed after the second [M86 pp. 175, 178]. What the glom covers, what
  a refutation destroys, and what the Hfilter does when two terms await it, the prose does not
  say (SPEC 8.8, 15.5 item 13).
- **Choice:** a filtered term's glint goes into the catchall, confirming and troublesome alike
  (the catchall's collection is the top-level elements that hold its glints). On a confirming
  term the Hfilter finds the frame of the hypothesis that holds the term (one hit, or one turn
  of a Cycle, D-004, from a fresh box: `Hypothesis.frame_bounds`) and puts a top-down
  pseudo-glom, type `frame`, over the free elements of the lineup from the frame's start (or
  from the first free element after whatever is not free) up to the new glint, printed
  `top-down glom glomN`; a frame glom over a shorter prefix of the same frame is dissolved
  first, so a frame has one glom, growing as its terms arrive (`(3)` then `(3 7)`); a glom
  already lying inside the frame is taken as a member. On a refutation the frame gloms are
  dissolved (their glints float free, for the cytoplasm and the Gnoth-setter), the glint
  joins the catchall, and D-017's Sparklers and the Gnoth-setter are set out; the gnoth-gloms
  are left to the Gnoth-setter (item 15). An Hfilter filters every term entered since the
  hypothesis last predicted one, in order, stopping at a refutation or when a guess ends the
  run; the other Hfilter then finds nothing to filter (D-009). A hypothesis made without a
  Gnoth-maker (the Tuple fall-back) gets its catchall from the first Hfilter.
- **Reason:** p. 98 for the catchall; the four `top-down glom` lines (two after complete frames,
  two after a frame's first term) are all produced by "a glom over the frame so far"; the
  one-term pseudo-glom is a thing the program prints (`glom8 pseudo --> (1)  terms 3 to 3`
  [M86 p. 175]); a frame glom in the catchall is what D-036's Gnoth-setter needs for "a last
  frame cut short by the failing term"; `glom10`'s disappearance before the Gnoth-setter and
  `glom9`'s survival are both reproduced by dissolving the frame gloms and nothing else.
- **Knob:** none.
- **Alternatives:** a glom over the new glint only (then a complete frame would be several
  gloms); extending a gnoth whose frame was cut short (gnoth3's `(1)` at p. 175 becoming
  `(1 2)`, which would also explain why `glom8` is gone at p. 176); a Dissolver that
  dissolves pseudo-gloms and announces it (every `is not in cytoplasm` line of the
  transcripts is on a pseudo-glom after a refutation [M86 pp. 172, 176], so that reading fits
  as well as "gone already"); dissolving the gnoth-gloms too on refutation (contradicted by
  `glom9`). *Open for item 15:* `glom3` [M86 p. 172] and `glom8` [M86 p. 176], both
  gnoth-gloms, are also gone before their Gnoth-setter runs; the Gnoth-setter (D-036) or the
  Dissolver reading above must account for them.
- **Amended (item 15):** the catchall is made at the first confirming term, taking in the
  glints the hypothesis postdicted beyond the listed gnoths (D-030); a refutation puts the
  troublesome glint into the catchall only when one exists (p. 98's example has one holding
  confirmed terms) and makes none for it, since a catchall made before the Gnoth-setter
  would take the number the transcript gives to the Gnoth-setter's gnoths [M86 p. 173].
  *Still open:* `glom3` [M86 p. 172] and `glom8` [M86 p. 176] are gone before their
  Gnoth-setter runs; the Gnoth-setter of D-036 dissolves a gnoth-glom only when it fills
  the gnoth afresh, which is after those lines. Two readings remain: the Dissolver
  dissolving a pseudo-glom and announcing it (D-024's alternative), which the Sparkler-plus
  on `(glom1 glint3 10)` and `bond4` between `glom1` and `glint3` on pp. 172-173 (the
  bottom-up glom under the gnoth-glom treated as a free element) make more likely; or a
  refutation dissolving the gnoth-gloms of the gnoths it cuts into (contradicted for
  `glom9`, kept on p. 178). For item 18 or 20, with D-015 and D-024.
- **Amended (item 21), closing the open point:** on a refutation the Hfilter also
  dissolves every top-level pseudo-glom (a gnoth-glom or a frame glom) holding a glint of
  the frame before the one the troublesome term falls in, the frame that term may belong
  with (`filtering.dissolve_previous_frame`); what lies beneath floats free. This is what
  all three `is not in cytoplasm` lines show: `glom3`, the gnoth-glom over `(1 2)`, gone
  before the Gnoth-setter at run 3's first refutation, with the bottom-up `glom1` free again
  (`Sparkler-plus on (glom1 glint3 10)`, `bond4 --- between glom1glint3` [M86 pp. 172-173]);
  `glom8` (the cap over the `1` of term 3) and `glom10` (the frame glom of term 4) gone at the
  second, while `glom9`, two frames back, is kept [M86 pp. 176, 178]. Under `(Countup 1)`
  the previous frame is the one term `2`; under `(Cycle 3 2 (3 7))` at run 2's refutation it
  is `(3 7)`, so gnoth1's cap goes there too, which the Gnoth-setter's five top-down gloms
  for three gnoths confirm (D-036's note). A hypothesis refuted inside its first frame
  dissolves the frame gloms only. The Dissolver reading (a Dissolver dissolving a
  pseudo-glom) is dropped: `is not in cytoplasm` is what the Dissolver prints when its glom
  is gone (D-024).

### D-064: Who answers a guess: the `Answerer`
- **Sources:** `enter no if wrong, ok if right ok` then `bye` [M86 p. 166]; `nope` and then
  `please enter a term:` [M86 pp. 175-176]; the task's non-interactive mode (answers scripted;
  a guess judged against the remaining scripted terms when no answer is given).
- **Choice:** the guess is answered through an `Answerer` object held by the world, with one
  method, `answer(world, form, block, terms)`, returning the user's words.
  `ScriptedAnswerer(answers, terms, default)` gives the scripted answers in order, then, when
  the whole sequence is known, judges the guess by D-033, then answers `default`, which is
  `ok`. `new_world` installs a scriptless `ScriptedAnswerer` when none is given, so an engine
  run without a user accepts its first guess. The interactive answerer is the session's
  (item 19). The trace prints the prompt with the answer on it, `enter no if wrong, ok if
  right ok`, once the answer is given, and `bye` after `ok`.
- **Reason:** the transcripts' protocol; the tests need a scripted user; accepting by default
  ends a run at its first guess, the natural stopping point for an engine test, and the
  judgement against the known sequence is what the non-interactive mode needs.
- **Knob:** none (the answerer is an argument of `new_world`, not a `Config` field: it is not
  about Seek-Whence's behaviour).
- **Alternatives:** raise when no answerer is given; a default of `no`.

### D-065: A shift carries what lies between the listed gloms and the gnoth border
- **Sources:** SHIFT-LEFT's glomlist "is a list of the gloms (which must be neighbors in
  order) to be transferred from right-gnoth to left-gnoth" [M86 p. 110], and SHIFT-RIGHT's
  "serves an analogous purpose" [M86 p. 110]; the second Gnoth-operator of run 3 is
  `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` [M86 p. 177] when gnoth3 holds `(1 2)` (terms 3
  and 4) and gnoth5 holds `(3)` (term 5), and afterwards `show` lists `glom14 pseudo --> (1 2
  3)  terms 3 to 5` under gnoth5 with `range: (3 5)`, and no gnoth3 [M86 pp. 178-179]; the
  series of p. 120 moves `glint6` out of `(3 4 5)` and leaves `[4 5]` behind [M86 p. 120]
  (SPEC 13.2, 13.4, 15.5).
- **Choice:** a shift uncovers each listed glom (dissolving the gloms containing it) and moves
  the border between the two gnoths past the listed gloms: for SHIFT-LEFT the left gnoth's
  range extends to the last listed glom's end, for SHIFT-RIGHT the right gnoth's range extends
  back to the first listed glom's start. Whatever lies between the listed gloms and the old
  border goes with them, so `(SHIFT-RIGHT gnoth3 gnoth5 (glint3))` moves glint3 *and* glint4
  into gnoth5, which is what p. 178 shows. A gnoth left with nothing disappears [M86 p. 120].
- **Reason:** a gnoth covers a contiguous run of terms (`range` [M86 p. 78]); moving glint3
  alone past glint4 would break that, and pp. 177-178 show glint4 went too.
- **Knob:** none.
- **Alternatives:** refuse a glomlist that does not reach the border (then p. 177's move
  would do nothing); read the glomlist as naming only the first glom to move.

### D-066: What the operations leave behind: caps, ranges, frames, forms and the new gloms
- **Sources:** "When a gnoth operation is completed, a capping procedure puts a new
  pseudo-glom in place above the gnoth's gloms" [M86 p. 109]; a SPLIT's new gnoth is drawn
  as `gnoth` with no number [M86 p. 112] and "(emerges)" [M86 p. 120]; ENCLOSE's glom is
  "dubbed to be of type 'enclose'", MERGE's is "a 'merge'-type glom" [M86 pp. 112-113];
  CAPTURE's "new one" has no stated type [M86 p. 112]; a shift's gloms "must be neighbors in
  order", ENCLOSE's "neighboring gloms", MERGE's "neighbor gloms in order" [M86 pp. 110,
  112-113]; SPLIT's splitlist is only "a list of gloms currently under the given gnoth"
  [M86 p. 110]. Nothing on the frame, class or form of a gnoth after an operation (the
  `form: (S-group 1 2)` of p. 175 is the Gnoth-caster's, item 18).
- **Choice:** every operation but NO-OP recaps each gnoth it touched at once with a fresh
  pseudo-glom (printed `top-down glom glomN` [M86 pp. 174, 177]); the gnoth's `range` follows
  its cap, its `form` is the one member's structure field when a single dubbed glom remains
  and nil otherwise, and its frame, plato-class and the four reformulation fields are left as
  they were. A gnoth that emerges from a SPLIT takes a fresh name and the frame, class and
  state of the gnoth it was split from. The gloms an operation makes are of type `capture`,
  `enclose` or `merge` (a one-word `type` field, as `pseudo` is), not pseudo, undubbed. The
  listed gloms of an ENCLOSE or MERGE must be neighbours in order (none nested in another,
  covering their extent without a gap, checked on positions before anything is destroyed);
  a SPLIT's must be such a run at one end of the gnoth and not the whole of it; a listed
  glint in a MERGE is kept as it is.
- **Reason:** p. 109's capping procedure; the diagrams of pp. 111-112 show every gnoth
  recapped after the operation; a SPLIT in the middle would leave a gnoth in two pieces,
  which no example shows; the one-word types follow the thesis's two names.
- **Knob:** none.
- **Alternatives:** recap only once at the end of a PROGRAM (fewer `top-down glom` lines; the
  counts are not milestones, D-055); give the capture glom the type of the captor.
- **Amended (item 21):** a recapped gnoth's `groups` field is set to the member lists of
  the gloms operations made under it (`capture`, `enclose`, `merge`), empty for a shift;
  `show` then prints `groups: nil` for the gnoth that received glint1 and nothing for the
  untouched gnoth3 [M86 p. 175], and `groups: nil` for gnoth5 after the second shift
  [M86 p. 179]. What a non-empty `groups` would print is unseen; p. 168's `groups: ((glint1
  glint2))` under a Gnoth-setter glom is the same format (D-036).

### D-067: What the Gnoth-operator checks, how a PROGRAM runs, and what follows it
- **Sources:** "It must first check to see that all the structures relevant to its operation
  are still in existence ... If the relevant structures do still exist, the Gnoth-operator
  carries out the operations; if not, it will simply terminate. When a Gnoth-operator does in
  fact operate, its last action is to decrease the system's bond-strength threshold by 1"
  [M86 p. 124]; the transcript's two forms, a PROGRAM `((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2
  (glint1)) (ENCLOSE gnoth1 nil))))` [M86 p. 174] and a bare operation `((SHIFT-RIGHT gnoth3
  gnoth5 (glint3)))` [M86 p. 177]; the PROGRAM's `(ENCLOSE gnoth1 nil)` names the gnoth the
  shift before it emptied [M86 p. 174]; bonds are assessed per gnoth as internal or external
  [M86 p. 100] (SPEC 14.1).
- **Choice:** the Gnoth-operator's one argument is an operation or a PROGRAM; it first checks
  every gnoth and glom named by every step (a `New` reference must point to an earlier step)
  and does nothing, reporting itself stale, when any is gone or a step is malformed. It then
  runs the steps in order; each step re-checks its own conditions (the gnoths are neighbours,
  the gloms lie under the right gnoth, the captive is a neighbour, the list is a run) and the
  PROGRAM stops at the first step that cannot be done, keeping what the earlier steps did. An
  ENCLOSE of `nil` does nothing and never fails, even when its gnoth has since disappeared.
  If any step acted, every gnoth's bonds are assessed afresh in the reigning environment
  (the collections changed) and the threshold goes down by 1 when a Reformulator has set one
  (none is set until then, D-046).
- **Reason:** p. 124's two duties; a PROGRAM's later steps may legitimately name a gnoth an
  earlier step empties (p. 174), so the check is on existence at the start and the harmless
  `nil` repair is let through; stopping a PROGRAM short rather than skipping a step keeps the
  repairs from being applied to a structure the move never made.
- **Knob:** none.
- **Alternatives:** skip a failed step and go on; check only the first step's structures.

### D-068: The Reformulator's order over the parse, what it sets out, and when it does nothing
- **Sources:** "The first moves considered are those at the highest level, under the theory
  that if a glom wants to move, its subgloms should follow" and the selection order of p. 107
  for one pair of neighbouring gnoths [M86 p. 107]; "The Reformulator will not suggest any
  further moves, since any others would occur at a lower structural level" [M86 pp. 107-108];
  "If the Reformulator finds no actions to be taken or if it has completed its
  recommendations, it hangs a Bond-assessor task on the taskrack" [M86 p. 105]; on p. 174 the
  Bond-assessor `(S-group 8.0)` runs *between* the Reformulator and the Gnoth-operator it set
  out, and on p. 177 after it [M86 pp. 174, 177]; the threshold numbers 8.0, 8.0, 9.0, 10.0
  [M86 pp. 177-178]. Nothing says how the pairs of neighbouring gnoths are ordered against
  each other, nor what a Reformulator does once a new hypothesis exists (SPEC 12.2, 14.2).
- **Choice:** the levels of p. 107's order are taken one at a time over the whole parse: level
  1 of every pair of neighbouring gnoths, left to right, then level 2 of every pair, and so
  on; the first group whose best move is strictly stronger than the threshold wins and the
  Reformulator stops. A Reformulator sets out *both* a Gnoth-operator (when it found a move
  or a NO-OP) and a Bond-assessor, the latter with the reigning class and the threshold as
  its arguments; so a second Reformulator may run before the Gnoth-operator and propose the
  same move again, and the second Gnoth-operator then finds its gnoth gone and terminates,
  as p. 124 describes. A Reformulator or Bond-assessor that runs without a reigning class or
  after a new hypothesis has been made (the Gnoth-caster's, item 18) does nothing.
- **Reason:** the thesis's words put the level before anything else, and "any others would
  occur at a lower structural level" only holds if a Reformulator's candidates are ordered by
  level over the whole parse; left to right within a level is the sequential domain's order
  (p. 106). The Bond-assessor set out together with the Gnoth-operator is
  what p. 174's order requires. The run-3
  numbers come out as 8.0, 8.0, 9.0, 10.0 on seeds where the Gnoth-operator runs before the
  second Reformulator and as 8.0, 9.0, 9.0, 10.0 on the others; both are the mechanism.
- **Knob:** none.
- **Alternatives:** pair-major order (every level of the leftmost pair before the next
  pair); the Bond-assessor only when no move was found; a Reformulator that keeps
  the threshold rising after the new hypothesis (harmless but pointless).

### D-069: The session and the command line
- **Sources:** the console protocol of the transcripts: `-> (startup)`, `please enter a term:`
  with the user's input printed on the prompt line [M86 p. 164], the four console commands
  [M86 pp. 166-179], `enter no if wrong, ok if right` with the answer on the same line and
  `bye` [M86 p. 166], `nope` and the next prompt [M86 pp. 175-176] (SPEC 1.4). The thesis says
  nothing about running without a user, about what happens when the input ends, about a
  machine-readable trace or about exit statuses; the task asks for a non-interactive mode
  (terms fed on request, answers scripted, a guess judged against the remaining scripted terms
  when no answer is given, D-033). The package's conventions: a run is made from its seed and
  its configuration; the command line run in two processes must agree; drivers and displays
  only observe; a trace should record enough to place a divergence.
- **Choice:** `seekwhence.session.Session` is one run seen through the protocol. It owns the
  `World`, a `Trace` with its prompt lines turned off (`prompts=False`), a `Recorder`, and a
  `Console` that writes the prompts and reads the user's lines from a script (`lines`) or a
  stream (standard input by default). The console echoes the line it read after the prompt
  when the input is a script or a stream that is not a terminal, so that the output reads as
  the transcripts do (`please enter a term: 3`); a terminal echoes by itself. A term is fed
  with `run.enter_term` and the tasks run with `run.run_until_prompt` until the program asks
  again; a per-term cap prompts again (D-011). A guess is answered by a `ConsoleAnswerer`
  (D-064): the next scripted answer, else, when the whole sequence is scripted, the judgement
  of D-033 (`ok` or `no`, written after the prompt), else the user is asked. The session ends
  with one of five outcomes: `ok` (a guess accepted, `bye`), `bye` (the user quit), `eof` (the
  stream ended), `no-terms` (the script ran out: the prompt is printed and left empty),
  `capped` (D-011's whole-run cap). `python3 -m seekwhence` is interactive without `--terms`
  and scripted with it; `--terms` may hold console commands among the terms (`--terms 3 7 3
  show-hypothesis 7`), so a transcript can be replayed as a script; `--answers` gives the
  answers in order; `--seed` the seed (default 0); `--trace verbose|terse` the two printouts
  of D-055 (it sets `Config.verbose`); `--json FILE` attaches a `JsonLines` observer that
  writes every event as one JSON object per line, the `kind` first and the fields in order,
  forms and structures as the text the trace prints for them; `--max-steps N` sets
  `Config.max_tasks`. In the scripted mode one line of the reconstruction's own closes the
  output, `outcome: ok after 4 terms, 50 tasks (seed 4): (Cycle 3 2 (3 7))`; the interactive
  mode prints nothing after `bye`, as the transcripts. The exit status is 0 when a guess was
  accepted or the user quit (`bye`, the end of the input), 1 when the scripted terms ran out
  or the run was capped, 2 for a usage error.
- **Reason:** the transcripts fix every printed line; the session prints the prompts itself
  because a prompt must be on the screen before the user types, which the trace's
  after-the-fact prompt line (right for the engine's tests) cannot do; the echo keeps a piped
  or scripted run readable as a transcript and makes the two-process repeatability test a
  comparison of whole outputs; the judged answer in the scripted mode
  is the task's rule; the outcome line and the exit statuses follow the Numbo port's command
  line (`outcome: solved, 45 iterations (seed 1)`; 0, 1, 2), so scripts can tell the endings
  apart; the JSON lines are the repository's trace idea, with the printed text standing in for
  objects so that the file needs no schema beyond the events' fields.
- **Knob:** none (`--trace` and `--max-steps` set the existing knobs `Config.verbose` and
  `Config.max_tasks`; the rest is not about Seek-Whence's behaviour).
- **Alternatives:** no echo on a pipe (the output would show bare prompts); a quit when the
  rack is empty with no hypothesis (the thesis's "quits" [M86 p. 37], never seen); a summary
  line in the interactive mode too; `--json -` to standard output in place of the text trace;
  a `RunEnded`-like event for the session's outcome in the JSON file (the outcome is on the
  text line only).

### D-070: The bench's `dozen`: the sequences, the terms fed, and the knowing user's judgement
- **Sources:** the Blackburn dozen, raw and parsed [M86 pp. 6-7], and the program's performance
  on each [M86 pp. 145-147]: "the current Seek-Whence program can solve only three of the
  Blackburn dozen -- problems 2, 8, and 12" [M86 p. 147], with the three related sequences it
  "can solve" [M86 pp. 145-146]; the nine sequences solved [M86 p. 88]; the typical problems
  [M86 p. 38]; the unconquered sequences [M86 p. 150]; Hofstadter's sampler [FCCA pp. 53-54].
  What a solution is: "the solver must complete the current template and fill out the next
  one" [M86 p. 7]; a guess is "a synopsis of its model" plus the next template [M86 p. 7]. The
  thesis gives run times ("under one minute" [M86 p. 145], "within three minutes" [M86 p. 146],
  "up to twenty minutes" [M86 p. 147], "stopped it after a fifteen-minute attempt" [M86 p. 146])
  but never how many terms a run was given, how many runs were made, or how the user judged a
  guess beyond `ok`/`no`. TASK.md asks that the user's answers be "scripted from the true
  continuation". A stochastic program is measured over many seeds, each run reproducible
  from its seed; drivers only observe.
- **Choice:** `seekwhence/sequences.py` catalogues the 45 sequences above (12 + 3 + 9 + 7 + 2 +
  12, each distinct sequence once, with the other places it is listed in `also`; the pre-parsed
  "spliceroo" of FCCA p. 54 is left out, its continuation not being given), each with the rule
  its parsed form prints, continued to `LENGTH` = 40 terms (`16 15 14 15 14 13 ...` to 0, since
  the domain has no negative integers [M86 p. 38]), the number of terms the source prints, its
  citation, the reported result (`solved`, `unsolved`, `not attempted`, or none) and the
  remarks in brief. `python3 -m seekwhence.bench dozen` runs each sequence over `--seeds` (20)
  seeds, feeding at most `--terms` (16) terms of the continuation one at a time through a
  `Session`, with the per-term cap of D-011 (`--per-term-cap`, 2000 tasks). Every guess is
  answered by a `ContinuationAnswerer`, a user who knows the rule: `ok` when the hypothesis's
  own terms, listed afresh from its box ("asked to list a number of terms" [M86 p. 82]), equal
  the continuation over its whole known length, `no` otherwise (a form whose box cannot give
  the terms, a P-group below 0, D-006, is wrong). A sequence is *solved* on a seed when a guess
  is accepted within the terms fed; the comparison with the thesis's verdicts calls a sequence
  solved here when at least half the seeds solve it. The report gives, per sequence, the seeds
  solved, the terms and tasks of a solution (min, median, max), the forms accepted and refused,
  the final hypotheses of the unsolved seeds, the blasts, and how many refused guesses the
  judgement of the printed block alone (D-033) would have passed. `RESULTS.md` keeps the tables
  of 20 seeds and 16 terms beside her verdicts.
- **Reason:** the continuation is what the parsed forms determine, and it is the only thing a
  knowing user can judge by; judging the printed block alone (D-033, the session's rule for a
  user who sees only the block) accepts wrong hypotheses that fit the terms so far, such as
  `(Countup 1)` on Blackburn 9 after `1 2 3`, `(Cycle 1 4 (1 8 1 2))` on Blackburn 11 after
  eight terms, or `(Constant 1)` on `1 1 1 2 2 2`. Sixteen terms cover the terms the thesis
  prints for every sequence but Blackburn 7 (18) and 11 (24), and every solution found takes
  at most 16 (median 12 at the most); the per-term cap stands in for her patience as D-011
  says; half the seeds is the plain reading of a verdict given for a program run a few times.
  Forty terms of continuation is far beyond any form's period in the catalogue, so a wrong
  form cannot pass by chance.
- **Knob:** none in `Config`; the bench's `--terms`, `--seeds`, `--first-seed`, `--per-term-cap`,
  `--groups` and `--names` (`seekwhence.sequences.LENGTH` for the continuation).
- **Alternatives:** judge by D-033 alone (the `python3 -m seekwhence --terms` mode does, since
  its user has only the block); feed exactly the terms printed and no more; a wall-clock
  budget as the thesis's (not reproducible); count a sequence solved on any seed (Blackburn 3
  would then be "solved", on 1 seed of 20); require the `ok` form to be the parsed form of
  pp. 6-7 rather than any form that predicts the continuation (Blackburn 12's Y-group and
  `(Cycle 1 5 (1 8 5 8 1))` on p. 146's sequence would then be judged wrong although they
  predict every term).
