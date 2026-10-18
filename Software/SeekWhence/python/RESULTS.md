# Results: the reconstruction against Meredith's reported runs

This file compares the reconstruction with the three program runs in the thesis's appendix
(`3 7 3 7`, `3 7 3 3 7 3`, `1 2 1 2 3 1 2 3 4`) and with Meredith's reported results on the
Blackburn dozen and the other sequences the sources name, honestly: where the reconstruction solves a sequence she reports hers could
not, or fails one hers solved, that is a finding, recorded here, not a bug to hide.

## Appendix runs 1 and 2 (item 20, tables regenerated in item 21)

Runs 1 (`3 7 3 7`, pp. 164-166) and 2 (`3 7 3 3 7 3`, pp. 167-169) were replayed over 100
seeds with Meredith's console inputs and her answer, and compared milestone by milestone
(`python3 -m seekwhence.bench appendix`; the method, the tables and every divergence with its
cause are in `docs/appendix-runs.md`). In short:

| | Meredith's program | The reconstruction, 100 seeds |
|---|---|---|
| Run 1: the fence `(3 7)` dubbed Cycle at the third term, the template, hypothesis `(Cycle 3 2 (3 7))`, `gnoths constructed` | pp. 164-165 | 35 seeds; on 65 the cover `(3 7 3)` is glommed first and dubbed Y-group (the scout's uniform draw, D-020) |
| Run 1: the fourth term confirms, the guess `3 7 3 7`, `ok`, `bye` | p. 166 | 35 seeds, every one of the fence seeds; on the cover seeds the `7` refutes the Y-group, the gnoths cannot be cast, the episode blasts and the four terms run out (with more terms 92 of 100 guess the Cycle, below) |
| Run 2: `show` after three terms (the two glom lines, the nine gnoth fields), `show-hypothesis` | pp. 167-168 | 35 seeds, field for field |
| Run 2: the fourth term `3` refutes, `set-out -- validity: 0`, `gnoths: (gnoth1 gnoth2 gnoth3)`, `slip-check: stayval: 0.0` / `best: nil` | p. 168 | 35 refute and set out the gnoths; 30 stay with Cycle as she does; 5 see the `3`-`3` sameness bond first and slip to C-group (p. 104's rule), then blast |
| Run 2: `casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3))) (Cycle 3 3 (3 7 3))`, the candidate, `show-seq`, the fifth term confirms, the guess `3 7 3 3 7`, `ok` | pp. 168-169 | 30 seeds, line for line |
| Run 2: an accepted guess at all | 1 run | 97 seeds: 30 by her route; 65 guess `(Y-group 3 3 (3 7 3))` at the *fourth* term, which predicts her sequence; 2 the Y-group after a blast |
| Task-type first-appearance order (run 1) | pp. 164-166 | 90% of her consecutive pairs hold; every type she prints runs on some seed |

Seed 54 reaches every milestone of both runs (two inexact: the Plato-scout's place among the
third term's tasks, and the frame glom numbered `glom7` for her `glom9`); its traces are the
golden files `tests/data/golden_run1_seed54.txt` and `golden_run2_seed54.txt` (the latter
regenerated in item 21: the terse mode's `set-out` line and a third Gnoth-setter glom). Three readings
were corrected by these replays, none a special case: the Hfilter runs first after a term
(D-012), sparks are made between any two elements not nested in each other (D-015, D-025,
p. 63's rule as the transcripts show it), and the Gnoth-setter's urgency is the Tester's
(D-008). The one divergence that matters, the cover's 65%, is the uniform scout draw of
item 10 (D-020), left as the simplest reading of "chooses a cyto-element" [M86 p. 66].

## Appendix run 3 (item 21)

Run 3 (`1 2 1 2 3 1 2 3 4`, pp. 170-179), the thesis's running example with the wrong guess
and the "nope", replayed over 100 seeds with her sixteen console inputs and her two answers
(`docs/appendix-runs.md` has the method, the 67-milestone table, seed 30 beside the printout
and every divergence with its cause):

| | Meredith's program | The reconstruction, 100 seeds |
|---|---|---|
| `(1 2)` glommed, dubbed S-group, the template `(S-group 1 2)` applied by the scout itself (`template: (form ...`, `Template-evaluator on nil`), `gnoths constructed`, the hypothesis `(Countup 1)` | pp. 170-171 | 100 seeds (`Pred` label and p. 171's `show` block on 43, `Succ` from the other end on 57) |
| The third term `1` refutes it (no `set-out` line); `glom3 is not in cytoplasm`; the bonds `2`-`3` and `1`-`3` | pp. 171-172 | 100 refute; the Dissolver line on 91; both bonds on 100 |
| Her proposals for `glint3` and `bond4` between `glom1` and `glint3`, before a late Gnoth-setter | pp. 172-173 | 6 and 14 (ours usually runs the Gnoth-setter first) |
| `gnoths: (gnoth1 gnoth2 gnoth3)`, `Countup --> S-group` | pp. 173-174 | 100 |
| The PROGRAM `((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil))` | p. 174 | the bare `((SHIFT-RIGHT gnoth1 gnoth2 (glint1)))` on 100 (D-044) |
| Bond-assessor 8.0, 8.0, 9.0, 10.0; `casts: ((S-group 1 2) (S-group 1 1))`, `(S-group 1 2)`, the candidate; `show-parse` `((1 2) (1))` | p. 174 | 100 |
| `show` with the reformulation fields (`external-bonds`, `internal-bonds`, `equivalence-type: parse`, `groups: nil`, `form: (S-group 1 2)`) | pp. 174-175 | field for field on the 43 seeds whose two bonds are numbered as hers |
| The fourth term `2` confirms; the guess `(1 2)(1 2)(1 2)`; `nope` | p. 175 | 100 |
| The fifth term `3` refutes `(S-group 1 2)` (printed `(1 2)1`); `glom8` and `glom10` not in cytoplasm; `gnoths: (gnoth2 gnoth3 gnoth5)` | p. 176 | 100 refute (the form printed, not `(1 2)1`, D-052); both Dissolver lines on 98; the gnoths on 100 |
| `slip-check: stayval: 15.0` / `best: (Y-group 4.0)`, staying with S-group | p. 176 | her numbers on 31 (the seeds whose check sees her three bonds); larger staying values on the rest; every seed stays |
| `(SHIFT-RIGHT gnoth3 gnoth5 (glint3))`, Bond-assessor 8.0, 8.0, 9.0, 10.0, `casts: ((S-group 1 2) (S-group 1 3))`, `(S-group 1 (Countup 2))`, `show-parse` `((1 2) (1 2 3))` | pp. 177-178 | the parse and the candidate on 85: the 3 moved left (`SHIFT-LEFT gnoth3 gnoth5 (glint5)`) on 79, her own line after a shift of the 2 on 6; 15 blast instead |
| The sixth term confirms; the guess `(1 2)(1 2 3)(1 2 3 4)`; `ok`; `bye` | p. 179 | 95 (85 by her route, 10 from stage one after the blast); 4 guess `(Tuple 1 5 (1 2 1 2 3))`, wrong but accepted by her replayed `ok`; 1 runs out of terms |
| Task-type first-appearance order | pp. 170-179 | 91% of her consecutive pairs hold; every type she prints runs |

Seed 30 reaches 59 of the 67 milestones (46 exactly); its trace is the golden file
`tests/data/golden_run3_seed30.txt`. Seven readings were corrected by this replay, none a
special case (D-024, D-048, D-055, D-060, D-063, D-066): the Dissolver's target, what a
refutation dissolves (the caps over the previous frame, which is what every `is not in
cytoplasm` line of the printouts shows), the caster's `form` field, `groups` after an
operation, the order of the assessed bond lists, the terse mode's `set-out` line and the
scout's `template:` line. Four milestones no seed reaches are recorded divergences: the
Plato-scout on one of her Gnoth-setter's gloms, the PROGRAM wrapper (D-044), `(1 2)1`
(D-052) and p. 178's `show` (ours makes a bond hers never did). The one behavioural finding:
on 15 seeds the second episode blasts because the move the parse needs is not found before
the threshold reaches 10.0, the bonds it needs not having been made in time; 10 of them still
reach her final hypothesis through stage one.

## The Blackburn dozen and the other sequences (item 22)

Every sequence the sources name was run over 20 seeds, fed one term at a time from its true
continuation, at most 16 terms, with every guess answered by a user who knows the rule: `ok`
only when the hypothesis predicts the whole known continuation (`python3 -m seekwhence.bench
dozen`; the catalogue and the protocol are `seekwhence/sequences.py` and D-070; the full
reports and the notable seeds' routes are in `docs/dozen.md`). The thesis's verdict: "the
current Seek-Whence program can solve only three of the Blackburn dozen -- problems 2, 8, and
12" [M86 p. 147]; beside them it names three related sequences it can solve [M86 pp. 145-146]
and nine solved sequences [M86 p. 88]. A sequence counts as solved here when at least half the
seeds end in an accepted guess (D-070). **Agreement with her verdicts: 23 of the 26 sequences
that have one.** The three disagreements are findings, not bugs to hide (TASK.md), and no
sequence, term or seed is special-cased anywhere.

### The dozen and the related sequences, 20 seeds, up to 16 terms

Columns: Meredith's reported result [M86 pp. 145-147]; the seeds solved; the terms a solution
took (median, min-max); the median tasks of a solution; the forms accepted; the guesses
refused by the knowing user (with the number of times); what the unsolved seeds end with
(`none` is no hypothesis, the terms having run out after a blast); the blasts over the seeds.

| sequence | terms as printed | Meredith | solved | terms | tasks | forms accepted | guesses refused | unsolved seeds end with | blasts |
|---|---|---|---|---|---|---|---|---|---|
| blackburn-1 | `1 1 2 1 2 3 1 2 3 4 1 2 3 4 5` | unsolved | 20/20 | 4 (4-16) | 165 | `(S-group 1 (Countup 1))` 20 | `(Tuple 1 3 (1 1 2))` 8 | none | 46 |
| blackburn-2 | `1 2 3 4 5 6 7` | solved | 20/20 | 3 (3-3) | 37 | `(Countup 1)` 20 | none | none | 0 |
| blackburn-3 | `2 1 2 2 2 2 2 3 2 2 4 2 2 5 2` | not attempted | 1/20 | 9 (9-9) | 682 | `(Y-group 2 3 (2 (Countup 1) 2))` 1 | `(P-group 2 2)` 20, `(Tuple 2 6 (2 1 2 2 2 2))` 11, `(Tuple 2 4 (2 1 2 2))` 10, `(Tuple 2 5 (2 1 2 2 2))` 4, `(Y-group 2 3 (2 (Cycle 1 2 (1 2)) 2))` 3, `(Y-group 2 3 (2 (S-group 1 2) 2))` 1 | `none` 19 | 54 |
| blackburn-4 | `1 2 2 3 3 3 4 4 4 4` | unsolved | 0/20 | - | - | none | `(S-group (Countup 1) 2)` 19 | `none` 20 | 41 |
| blackburn-5 | `1 8 5 8 1 8 5 8 1 8 5 8` | unsolved | 19/20 | 7 (6-15) | 259 | `(Cycle 1 4 (1 8 5 8))` 19 | none | `(Cycle 1 4 (1 8 5 8))` 1 | 13 |
| blackburn-6 | `2 1 2 2 2 3 2 4 2 5` | unsolved | 0/20 | - | - | none | `(P-group 2 2)` 20, `(Tuple 2 4 (2 1 2 2))` 10, `(Y-group 2 3 (2 (Cycle 1 2 (1 2)) 2))` 1, `(Y-group 2 3 (2 (S-group 1 2) 2))` 1 | `none` 20 | 43 |
| blackburn-7 | `2 3 1 2 3 2 2 2 3 3 3 3 2 3 4 4 4 4` | unsolved | 0/20 | - | - | none | `(Tuple 2 3 (2 3 1))` 34, `(Tuple 2 5 (2 3 1 2 3))` 2 | `none` 20 | 43 |
| blackburn-8 | `1 2 2 3 3 4 4 5 5 6` | solved | 20/20 | 5 (5-6) | 248 | `(S-group (Countup 1) 2)` 20 | none | none | 21 |
| blackburn-9 | `1 2 3 3 4 4 5 5 5 6 6 6` | unsolved | 0/20 | - | - | none | `(Countup 1)` 20 | `none` 20 | 68 |
| blackburn-10 | `9 1 9 2 9 3 9 4` | unsolved | 0/20 | - | - | none | `(Cycle 9 4 (9 1 9 2))` 7, `(Cycle 9 6 (9 1 9 2 9 3))` 7, `(Cycle 9 8 (9 1 9 2 9 3 9 4))` 7, `(Cycle 9 10 (9 1 9 2 9 3 9 4 9 5))` 6, `(Cycle 9 12 (9 1 9 2 9 3 9 4 9 5 9 6))` 6, `(Cycle 9 14 (9 1 9 2 9 3 9 4 9 5 9 6 9 7))` 6 | `none` 14, `(Cycle 9 16 (9 1 9 2 9 3 9 4 9 5 9 6 9 7 9 8))` 6 | 14 |
| blackburn-11 | `1 8 1 2 1 8 1 2 3 2 1 8 1 2 3 4 3 2 1 8 1 2 3 4` | unsolved | 0/20 | - | - | none | `(Cycle 1 4 (1 8 1 2))` 20 | `none` 20 | 25 |
| blackburn-12 | `1 8 5 5 8 1 1 8 5 5 8 1` | solved | 20/20 | 8 (7-13) | 220 | `(Y-group 1 6 (1 8 5 5 8 1))` 18, `(Y-group 1 5 (1 8 (C-group 5 2) 8 1))` 2 | none | none | 2 |
| p146-1 | `2 2 3 3 3 4 4 4 4` | solved | 20/20 | 6 (6-10) | 451 | `(C-group (Countup 2) (Countup 2))` 20 | none | none | 40 |
| p146-2 | `1 1 2 2 2 3 3 3 3` | solved | 20/20 | 6 (6-10) | 451 | `(C-group (Countup 1) (Countup 2))` 20 | none | none | 40 |
| p146-3 | `1 8 5 8 1 1 8 5 8 1` | solved | 20/20 | 6 (6-7) | 118 | `(Y-group 1 5 (1 8 5 8 1))` 15, `(Cycle 1 5 (1 8 5 8 1))` 4, `(Y-group 1 3 (1 (Y-group 8 3 (8 5 8)) 1))` 1 | none | none | 0 |

### The other sequences the sources name, 20 seeds, up to 16 terms

| sequence | terms as printed | Meredith | solved | terms | tasks | forms accepted | guesses refused | unsolved seeds end with | blasts |
|---|---|---|---|---|---|---|---|---|---|
| p88-1 | `1 1 1` | solved | 20/20 | 3 (3-3) | 36 | `(Constant 1)` 20 | none | none | 0 |
| p88-2 | `1 2 3 4` | solved | 20/20 | 3 (3-3) | 37 | `(Countup 1)` 20 | none | none | 0 |
| p88-3 | `1 1 1 2 2 2 3 3 3` | solved | 20/20 | 7 (7-8) | 947 | `(C-group (Countup 1) 3)` 20 | `(Constant 1)` 20 | none | 56 |
| p88-4 | `3 4 3 4 3 4` | solved | 20/20 | 4 (4-4) | 89 | `(S-group 3 2)` 20 | none | none | 0 |
| p88-5 | `3 7 3 7 3 7` | solved | 20/20 | 6 (4-9) | 188 | `(Cycle 3 2 (3 7))` 20 | none | none | 15 |
| p88-6 | `3 7 3 3 7 3 3 7 3` | solved | 20/20 | 4 (4-8) | 50 | `(Y-group 3 3 (3 7 3))` 14, `(Cycle 3 3 (3 7 3))` 5, `(Constant (Y-group 3 3 (3 7 3)))` 1 | none | none | 2 |
| p88-7 | `16 15 14 17 16 15 18 17 16` | solved | 20/20 | 8 (7-9) | 565 | `(P-group (Countup 16) 3)` 20 | `(Tuple 16 4 (16 15 14 17))` 36 | none | 40 |
| p88-8 | `16 15 14 15 14 13 14 13 12` | solved | 0/20 | - | - | none | none | `none` 12, `(Tuple 16 16 (16 15 14 15 14 13 14 13 12 13 12 11 12 11 10 11))` 8 | 223 |
| p88-9 | `1 2 1 2 3 1 2 3 4` | solved | 20/20 | 6 (6-10) | 193 | `(S-group 1 (Countup 2))` 20 | `(S-group 1 2)` 20, `(Tuple 1 5 (1 2 1 2 3))` 2 | none | 6 |
| p38-1 | `2 2 2` | no verdict | 20/20 | 3 (3-3) | 36 | `(Constant 2)` 20 | none | none | 0 |
| p38-2 | `1 1 2 2 3 3` | no verdict | 20/20 | 5 (5-6) | 352 | `(C-group (Countup 1) 2)` 20 | none | none | 37 |
| p38-5 | `1 0 1 0 0 1 0 0 0 1` | no verdict | 0/20 | - | - | none | `(P-group 1 2)` 40, `(Tuple 1 5 (1 0 1 0 0))` 18 | `none` 20 | 29 |
| p38-6 | `1 2 8 3 4 8 5 6 8` | no verdict | 0/20 | - | - | none | none | `none` 20 | 37 |
| p38-7 | `1 2 8 3 4 5 8 6 7 8 9 8` | no verdict | 0/20 | - | - | none | none | `none` 20 | 37 |
| p38-9 | `8 0 8 8 0 8 8 0 8` | no verdict | 20/20 | 4 (4-8) | 50 | `(Y-group 8 3 (8 0 8))` 14, `(Cycle 8 3 (8 0 8))` 5, `(Constant (Y-group 8 3 (8 0 8)))` 1 | none | none | 2 |
| p38-10 | `1 1 2 1 2 2 1 3 2` | no verdict | 0/20 | - | - | none | `(S-group 1 (Countup 1))` 22, `(Tuple 1 3 (1 1 2))` 8 | `none` 20 | 48 |
| p150-1 | `1 2 10 3 4 10 5 6 10` | unsolved | 0/20 | - | - | none | none | `none` 20 | 37 |
| p150-3 | `1 1 1 1 2 1 1 3 1` | unsolved | 0/20 | - | - | none | `(Constant 1)` 40, `(Tuple 1 5 (1 1 1 1 2))` 36, `(Y-group 1 3 (1 (Tuple 1 2 (1 (Tuple 1 5 (1 1 2 (C-group 1 2) 3)))) 1))` 1, `(Y-group 1 3 (1 (Tuple 1 8 (1 (Tuple 1 3 (1 1 2)) (C-group 1 2) 3 (C-group 1 2) 4 (C-group 1 2) 5)) 1))` 1 | `none` 19, `(Y-group 1 3 (1 (Tuple 1 8 (1 (Tuple 1 3 (1 1 2)) (C-group 1 2) 3 (C-group 1 2) 4 (C-group 1 2) 5)) 1))` 1 | 40 |
| fcca-1 | `2 2 2 3 3 2 2 2 3 3 2 2 2 3 3` | no verdict | 20/20 | 6 (6-6) | 541 | `(Tuple 2 5 (2 2 2 3 3))` 20 | `(Constant 2)` 20 | none | 36 |
| fcca-2 | `1 0 0 1 1 1 0 0 0 0 1 1 1 1 1` | no verdict | 0/20 | - | - | none | `(Tuple 1 3 (1 0 0))` 9, `(Y-group 1 3 (1 (C-group 0 2) 1))` 4, `(Y-group 1 3 (1 (Tuple 0 2 ((C-group 0 2) 1)) 1))` 1, `(Y-group 1 4 (1 0 0 1))` 1 | `none` 20 | 43 |
| fcca-3 | `1 0 0 1 5 1 0 0 0 5 1 1 1 1 5 0 0 0 0 5` | no verdict | 0/20 | - | - | none | `(Tuple 1 3 (1 0 0))` 9 | `none` 20 | 35 |
| fcca-4 | `1 2 0 2 0 2 1 3 1 3 1 2 0 2 0 2 0 3 0 3` | no verdict | 0/20 | - | - | none | none | `none` 20 | 52 |
| fcca-7 | `2 1 3 1 2 4 1 2 3 5 1 2 3 4 6` | no verdict | 0/20 | - | - | none | none | `none` 20 | 48 |
| fcca-8 | `1 2 1 3 2 1 4 3 2 1 5 4 3 2 1` | no verdict | 0/20 | - | - | none | none | `none` 20 | 20 |
| fcca-9 | `3 2 1 0 1 2 3 2 1 0 1 2 3` | no verdict | 10/20 | 12 (10-16) | 2607 | `(Cycle 3 6 (3 2 1 0 1 2))` 10 | none | `none` 10 | 47 |
| fcca-10 | `0 0 0 1 1 2 4 5 5 6 6 6 7 7 8 10 11 11` | no verdict | 0/20 | - | - | none | `(Constant 0)` 20 | `none` 20 | 56 |
| fcca-11 | `1 1 2 1 1 2 3 2 1 1 2 3 4 3 2 1` | no verdict | 0/20 | - | - | none | `(Tuple 1 3 (1 1 2))` 24, `(S-group 1 (Countup 1))` 11, `(Cycle 1 3 (1 1 2))` 2 | `none` 19, `(Y-group 1 3 (1 (Tuple 1 6 (1 (Tuple 2 4 (2 1 (S-group 1 3) (Tuple 2 2 (2 (C-group 1 2))))) 2 3 4 (P-group 3 2))) 1))` 1 | 75 |
| fcca-12 | `1 1 2 3 1 2 2 3 1 2 3 3 1 1 2 3` | no verdict | 0/20 | - | - | none | `(Tuple 1 4 (1 1 2 3))` 12 | `none` 20 | 74 |
| fcca-13 | `1 1 2 3 1 2 2 3 1 2 3 3 1 2 2 3 1 1 2 3` | no verdict | 0/20 | - | - | none | `(Tuple 1 4 (1 1 2 3))` 12 | `none` 20 | 74 |
| fcca-14 | `1 2 2 3 3 1 1 2 3 3 1 1 2 2 3 1 2 2 3 3` | no verdict | 0/20 | - | - | none | `(S-group (Countup 1) 2)` 19, `(Y-group 1 3 (1 (Tuple 2 4 (2 (Tuple 2 4 (2 3 3 (C-group 1 2))) 2 (C-group 3 2))) 1))` 4, `(Y-group 1 3 (1 (Tuple 2 2 (2 (Tuple 2 2 (2 (C-group 3 2))))) 1))` 2, `(Y-group 1 3 (1 (Tuple 2 2 ((C-group 2 2) (C-group 3 2))) 1))` 1 | `none` 20 | 48 |

### Agreements

- **Blackburn 2, 8 and 12**, the three she solved, are solved on every seed: `(Countup 1)` at
  the third term; `(S-group (Countup 1) 2)` at the fifth or sixth, by reformulation (on seed 0:
  `(Countup 1)` from `1 2` is refuted by the second `2`, `Countup --> S-group`, the casts
  `((S-group 1 2) (S-group 2 1))` fail three times and the episode blasts; stage one's
  fall-back `(Tuple 1 3 (1 2 2))` is refuted by the `3`, `Tuple --> S-group`, and the
  Gnoth-caster's casts `((S-group 1 2) (S-group 2 2))` generalise to the candidate
  `(S-group (Countup 1) 2)`, confirmed by the next term; 21 blasts over 20 seeds); the Y-group
  `(Y-group 1 6 (1 8 5 5 8 1))` at the eighth term (seventh to thirteenth) on 18 seeds and the
  nested `(Y-group 1 5 (1 8 (C-group 5 2) 8 1))` on 2. Her program on 12 "finds a Y-group, but
  often it is the Y-group" `(1 8 (5 (5 8 1 1 8 5) 5) 8 1)` [M86 p. 147], "up to twenty
  minutes" [M86 p. 147]; ours finds the plain one in a few hundred tasks and never hers (why
  her program's cover ran across the join is not stated; here the cover between the two 1's
  is made as soon as the sixth term arrives).
- **The related sequences of pp. 145-146** are solved on every seed: `2 2 3 3 3 4 4 4 4` as
  `(C-group (Countup 2) (Countup 2))` and `1 1 2 2 2 3 3 3 3` as `(C-group (Countup 1)
  (Countup 2))` at the sixth term (the two runs are the same run on the same seed, the
  mechanisms seeing only sameness and successorship), and `1 8 5 8 1 1 8 5 8 1` at the sixth
  or seventh as the Y-group `(1 8 5 8 1)` on 15 seeds, the Cycle of the same five terms on 4,
  a nested Y-group on 1.
- **The p. 88 list** is solved on every seed for eight of its nine sequences: `1 1 1`
  (`(Constant 1)`), `1 2 3 4`, `1 1 1 2 2 2 3 3 3` (`(C-group (Countup 1) 3)` at the seventh
  term, after `(Constant 1)` is refused), `3 4 3 4` (`(S-group 3 2)`), `3 7 3 7`
  (`(Cycle 3 2 (3 7))`, at the fourth term on the fence seeds and after a blast on the rest,
  see runs 1 and 2 above), `3 7 3 3 7 3` (the Y-group `(3 7 3)` on 14 seeds, the Cycle on 5,
  `(Constant (Y-group 3 3 (3 7 3)))` on 1), `16 15 14 17 16 15 18 17 16`
  (`(P-group (Countup 16) 3)` at the eighth term, after the Tuple fall-back `(Tuple 16 4 (16 15
  14 17))` is refused, 36 times over 20 seeds) and `1 2 1 2 3 1 2 3 4` (run 3's
  `(S-group 1 (Countup 2))`, its wrong guess `(S-group 1 2)` refused first as on p. 175).
- **Blackburn 4, 6, 7, 9, 10, 11** are unsolved here as there, and the thesis's remark fits
  what the seeds show. On 4, "the initial S-groups -- (1 2) and (2 3) -- interfere with the
  budding C-group notion" [M86 p. 146]: 19 of 20 seeds guess `(S-group (Countup 1) 2)` after
  `1 2 2 3 3`, are refused, and the C-group never breaks through within 16 terms. On 9, every
  seed guesses `(Countup 1)` after `1 2 3`, refused (the printed block `1 2 3` would have
  passed D-033's judgement; the knowing user does not), and the growth of the group lengths
  "will tax the system's representation scheme" [M86 p. 147]. On 10, "The only problem here is
  the alternation of terms of which at least one is non-constant" [M86 p. 147]: the fence class
  casts the whole sequence as one lengthening Cycle, `(Cycle 9 4 (9 1 9 2))`,
  `(Cycle 9 6 (9 1 9 2 9 3))`, ..., refused at every length (D-037's reading of casting under a
  fence class; the Cycle's elements are never generalised to a Countup, which is the "slight
  extension" she foresaw). On 11, `(Cycle 1 4 (1 8 1 2))` is guessed after the eighth term on
  every seed and refused, then "the system gets bogged down in spurious relationships"
  [M86 p. 147] and blasts (25 blasts over 20 seeds). On 6 and 7 the Tuple fall-back (D-031)
  is guessed and refused, then the terms run out with no hypothesis.
- **Blackburn 3**, the theme song, "This is hopeless as yet; we have not even attempted it"
  [M86 p. 145]: unsolved on 19 seeds (the P-group `(2 1)`, the Tuples and the Y-groups with a
  Cycle or an S-group centre all refused). **On one seed of 20 (seed 17) it is solved at the
  ninth term, as `(Y-group 2 3 (2 (Countup 1) 2))`**: the Y-group casts `(2 1 2)`, `(2 2 2)`,
  `(2 3 2)` generalised position by position (D-048) give the centre `(Countup 1)`. A
  reasonable form of the sequence, reached by the mechanisms once; by D-070's rule the
  sequence is not solved.
- **The unconquered sequences of p. 150** are unsolved: `1 2 10 3 4 10 5 6 10` ("It is unable
  to handle independent interleaved sequences when the components are any more complicated
  than constants" [M86 p. 150]) never makes a hypothesis that survives, and `1 1 1 1 2 1 1 3 1`
  guesses `(Constant 1)` and Tuples, refused.

### Disagreements

- **Blackburn 1, `1 1 2 1 2 3 1 2 3 4 ...`: she unsolved, we solved on 20 of 20 seeds**
  (`(S-group 1 (Countup 1))` at the fourth term on 11 seeds, the seventh on 5, the eleventh on
  3, the sixteenth on 1). "The program thrashes hopelessly on this one" [M86 p. 145], the
  initial C-group interfering with the lengthening S-groups, which reach the template level
  but not a hypothesis [M86 p. 145]; the summary says the program "will have to cling less
  forcefully to its original formulation in order for it to solve problems 1, 4, 5, and 6"
  [M86 p. 147]. The reconstruction clings less because of the blast (D-050): `1 1` is glommed
  and dubbed C-group, its template gives `(Constant 1)` (D-031), the third term `2` refutes
  it, the strict Constant slips to C-group (D-040), the casts `((C-group 1 2) (C-group 2 1))`
  do not generalise, and the third failure blasts every gnoth and glom away (D-050's three
  failures, an engineering choice). Stage one then runs again over the three
  terms: on the seeds that glom `(1 2)` the applier loosens its template to `(S-group 1 n)`
  over `(1)` and `(1 2)` and the Hypothesizer's wildcard rule (D-031) gives
  `(S-group 1 (Countup 1))` at once, confirmed by the fourth term; on the seeds that re-glom
  `1 1` the fall-back `(Tuple 1 3 (1 1 2))` is guessed and refused, the Tuples lengthen with
  the terms (`(Tuple 1 5 ...)`, `(Tuple 1 7 ...)`, ...) and the S-group is found after a later
  blast. The likely cause of the disagreement is therefore the blast's short fuse and the
  Tuple fall-back, the two mechanisms the thesis describes least (SPEC 9.6, 8.1; D-031, D-050),
  together with the scan's uniform draw, which gives the S-groups their chance against the
  "low-level rediscovery of C-groups" [M86 p. 145] that defeated hers.
- **Blackburn 5, `1 8 5 8 1 8 5 8 ...`: she unsolved, we solved on 19 of 20 seeds**
  (`(Cycle 1 4 (1 8 5 8))` at the seventh term on the median seed, the sixth to the
  fifteenth). Her program finds the Y-group `1 8 5 8 1` "and doggedly clings to it"
  [M86 p. 146], stopped after fifteen minutes. The reconstruction finds the same Y-group (the
  cover between the two 1's, as on `3 7 3`) at the fifth term; the sixth term `8` refutes it
  before any guess, the casts under Y-group fail (`((Y-group 1 5 (1 8 5 8 1)) (Y-group 8 1
  (8)))`, with a slip to S-group and back), and the blast (13 over 20 seeds) lets stage one
  glom the fence `(1 8 5 8)` and dub it Cycle, confirmed by the seventh term. The same cause
  as for Blackburn 1: the blast bounds the clinging.
- **`16 15 14 15 14 13 14 13 12 ...` [M86 p. 88]: she solved "as well as possible, given a
  non-infinite pattern" [M86 p. 88], we never** (0 of 20; 243 refutations and 223 blasts over
  the seeds; 8 seeds end holding a Tuple of the sixteen terms, 12 none). The likely cause is
  the form language: no form of the eight types gives a start that falls by one per hit (a
  Countup counts up; a P-group's start is a value, not a falling one; D-001, D-006), so no
  hypothesis can predict the continuation and the knowing user refuses every guess; the
  thesis's own qualification suggests her program's solution covered a finite stretch, which
  D-070's judgement does not accept. What form her program produced is not stated.

### The sequences without a verdict

Of the typical problems of p. 38 not among the above, `2 2 2`, `1 1 2 2 3 3`
(`(C-group (Countup 1) 2)` at the fifth term) and `8 0 8 8 0 8` (the Y-group `(8 0 8)`, or its
Cycle) are solved on every seed; `1 0 1 0 0 1 0 0 0 1`, `1 2 8 3 4 8 5 6 8`, `1 2 8 3 4 5 8 6 7
8 9 8` and `1 1 2 1 2 2 1 3 2` on none (the growing groups and the interleaved marker, the
thesis's two hard kinds [M86 pp. 146, 150]). Of Hofstadter's sampler, the hemiolic bi-cycle
`2 2 2 3 3` is solved on every seed as `(Tuple 2 5 (2 2 2 3 3))` (after `(Constant 2)` is
refused), the sawtooth `3 2 1 0 1 2` on 10 of 20 as `(Cycle 3 6 (3 2 1 0 1 2))` (D-037's
Cycle of the whole period, right this once, at the twelfth term), and the rest on none, as
SPEC 3.4 expected of the 1986 design: the alternating copies, the interleavings, the scales
with a note omitted, the descending scales, the sawtooth plateaus, the mountain chain and
the three doublers and singlers all end with their Tuples or Y-groups refused and the terms
run out.

### The judgement of the printed block alone

On seven sequences the knowing user refused guesses that D-033's judgement of the printed
block would have accepted (the `python3 -m seekwhence --terms` mode's rule, for a user who
sees only the block): `(Countup 1)` on Blackburn 9 (20 guesses), the lengthening Cycles on
Blackburn 10 (39), `(Cycle 1 4 (1 8 1 2))` on Blackburn 11 (20), `(Constant 1)` on
`1 1 1 2 2 2 3 3 3` (20) and on `1 1 1 1 2 1 1 3 1` (40), `(Constant 2)` on the hemiolic
bi-cycle (20) and `(Constant 0)` on the sawtooth plateaus (20). A solved rate measured by the
block alone would be higher and wrong.

## Findings from the stages as they were built (items 10-19)

Findings recorded while the engine was built bottom-up, kept so that they are not lost:

- **`3 7 3` (runs 1 and 2, [M86 pp. 165, 167]):** Meredith's program glommed the fence `(3 7)`
  and dubbed it Cycle in both runs, the cover `(glint1 glint2 glint3)` failing afterwards. In
  the reconstruction the fence and the cover race, with the mechanisms as decided (D-020,
  D-021): over 200 seeds the fence `(3 7)` dubbed `(Cycle 3 2 (3 7))` wins 68 times and the
  cover `(3 7 3)` dubbed Y-group 132 times. The reason is the Glom-scout's uniform draw: the
  fence can only be cut from `glint1`, the cover from any of the three glints. Reordering the
  Glomtester weights (fence above cover, with Hofstadter's "subtler" palindromes [FCCA p. 59]
  as the reason) and letting the Glommer inherit the Glomtester's urgency raised the fence's
  share from 13 to 34 of 100 seeds; a scout that starts from a bond would raise it to about a
  half but could not target the bondless centre of p. 67's symmetry example. No further
  tuning was done: the item's expectation that the fence wins "for most seeds" is not met,
  and what the later stages make of a Y-group `(3 7 3)` when the fourth term `7` arrives is
  for items 12 and 20 to measure.
- **The template level on `3 7 3` (item 12):** a fence seed reproduces run 1's template lines
  in order (`Template-scout on (glom1)`, `Template-applier on (glom1 (Cycle 3 2 (3 7)))`,
  `create-template-glom (Cycle 3 2 (3 7))(glom1)`, `top-down glom glom2`, `template made :
  (form (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)`, `check-cycle template
  (Cycle 3 2 ((3 7)))` [M86 pp. 165, 167]). A cover seed makes the template `(Y-group 3 3
  (3 7 3))` instead, which a fourth term `7` fails (a Y-group has nothing to loosen and `7`
  begins no unit), so the Template-reviewer abandons it at the fourth term and the cytoplasm
  starts again with the glints and `(3 7 3)` free; no hypothesis has been made from it by then
  only because the Hypothesizer is not built (item 13). What stage two does with a refuted
  `(Y-group 3 3 (3 7 3))` hypothesis is for item 20.
- **`1 2` and `1 2 1 2 3` (item 12):** `1 2` makes its template at once from the scout, as
  run 3 prints [M86 pp. 170-171]; `1 2 1 2 3` from scratch ends with the p. 73 template
  `(S-group 1 n)` over `(1 2)` and a top-down `(1 2 3)` for every one of 10 seeds, the
  reviewer having loosened `(S-group 1 2)` at the fifth term.
- **`1 1 2 1 2 3` (Blackburn #1) at the template level (item 12):** from scratch, the initial
  `1 1` is glommed and dubbed C-group before any S-group exists, its template `(C-group 1 2)`
  is accepted at the second term, loosened by the reviewer to `(C-group n 2)` at the third,
  and then left alone (every later term could begin a unit) for the rest of the terms, so that
  the S-groups the cytoplasm later gloms and dubs never reach the template level: "no new
  templates are considered for the lifetime of the given template" [M86 p. 74]. The p. 73
  template `(S-group 1 n)` arises when the situation of p. 74 is set up by hand (the first
  `1 2` glommed and dubbed, the rest free: `tests/test_templates.py`). This is the thesis's own
  diagnosis of this very sequence, which her program could not solve: "the initial C-group
  interferes with the system's ability to find the lengthening S-groups" [M86 p. 145], and
  when it does find them "it seems unable to push the correct notion beyond the template
  level" [M86 p. 145]; whether stage two's refutation of the `(Constant 1)` hypothesis that
  `(C-group 1 2)` will give (D-031) frees the template level is for items 14-15 and 20 to see.
- **The end of stage one (item 13):** on a fence seed `3 7 3` reaches run 1's milestones in
  order: `top-down glom glom3`, `gnoths constructed`, `(Cycle 3 2 (3 7))` after `doing task
  Hypothesizer`, `doing task Call-term` last [M86 pp. 165-166], and `show` lists `glom1`,
  `glom3 pseudo --> ((3 7))` and the gnoth's nine fields exactly as p. 168 prints them. Over
  60 seeds the hypothesis is `(Cycle 3 2 (3 7))` on the fence seeds (about a third, as above)
  and `(Y-group 3 3 (3 7 3))` on the cover seeds, one gnoth either way. `1 2` ends with
  `(Countup 1)` and run 3's gnoth (p. 171) for all 30 seeds tried. The transcripts' order,
  with the `Call-term` after both the Gnoth-maker and the Hypothesizer whichever ran first,
  needed a rule (D-012, amended): a Call-term set out by the Hypothesizer alone came first in
  about a third of the runs. Consequences for the later items: once a hypothesis exists, the
  template level is quiet, so `1 2 1 2 3` entered term by term now ends stage one with
  `(Countup 1)` after the second term, as run 3 did [M86 p. 171], and reaches `(S-group 1 n)`
  only when the template level is run without the gnoth tasks (`tests/test_templates.py`);
  `1 1 2 1 2 3` likewise ends with `(Constant 1)` after `1 1` (D-031's repeater rule on the
  C-group template), which is the hypothesis stage two will have to refute. The Hypothesizer's
  wildcard rule (D-031) gives `(S-group 1 (Countup 1))` from p. 78's three gnoths and
  `(S-group 1 (Countup 2))` from units `(1 2)`, `(1 2 3)`: run 3's final hypothesis
  [M86 p. 178], reached there through two reformulations, comes straight from the template
  when the template `(S-group 1 n)` is in place before any hypothesis is made.
- **A literal p. 75 (item 12):** "If the proposed template does not work at term level, it is
  forgotten and the engendering glom dissolved" [M86 p. 75] is implemented as written
  (D-029), so a dubbed glom whose form, loosened or not, does not fit the whole sequence is
  dissolved once its scout, applier and resolver have run; p. 67's example gloms (`1 1 1` in
  `2 1 1 1 3`, `9 1 2 5` in `9 1 2 5 9`) go that way when the run continues to the template
  level. The thesis says the resolver "is seldom invoked" because a template usually comes
  early [M86 p. 75]; the early template of the first glom then blocks the scouts. Item 20 will
  measure how often the resolver runs on the appendix sequences.
- **Filtering and the guess (item 14):** on a fence seed the whole `3 7 3 7` run prints
  p. 166 from `doing task Hfilter` to `bye` line for line: `new term being hypothesis-filtered
  through (Cycle 3 2 (3 7))`, `top-down glom glom4` (over `glint3 glint4`, the turn the
  fourth term completes), `I have a guess!`, `hypothesis: (Cycle 3 2 (3 7))`, `3 7 3 7`,
  `enter no if wrong, ok if right ok`, `bye` [M86 p. 166]; in the terse mode the same without
  the task lines, as run 2 prints its guess [M86 p. 169]. Over the seeds 0-99 the run reaches
  that guess on 35; on the other 65 the cover `(3 7 3)` won the scan (the finding of item 10),
  the hypothesis was `(Y-group 3 3 (3 7 3))`, and the fourth term refuted it with run 2's
  `set-out -- validity: 0` and a Gnoth-setter that item 15 builds. The item asked for "most
  seeds"; the share is the scan's, not the filter's, and no special case was added. `3 7 3 3`
  on a fence seed prints run 2's two lines [M86 p. 168] and `1 2` then `1` prints run 3's
  `new term being hypothesis-filtered through (Countup 1)` [M86 pp. 171-172] on every seed
  tried. A refused guess (`nope`) resets the validity and asks for the next term, and the next
  confirming term guesses again with a longer block (`3 7 3 7 3`), as D-033's rule predicts
  for a case the transcripts do not show. One format difference is kept: the block is printed
  flush left, as runs 2 and 3 print it, where run 1 indents it by five spaces [M86 p. 166].
- **The Gnoth-setter and the slip check (item 15):** `3 7 3 3` on a fence seed prints run 2's
  `gnoths: (gnoth1 gnoth2 gnoth3)` and `slip-check: stayval: 0.0` / `best: nil`
  [M86 p. 168], and `1 2 1` prints run 3's `gnoths: (gnoth1 gnoth2 gnoth3)` [M86 p. 173] and
  slips `Countup --> S-group` [M86 p. 174] on every seed tried, with the gnoths' frames 0, 1,
  2 and the first two parse-equivalent [M86 pp. 99, 175]. The gnoth numbers of all three
  printed lists, the frame numbers of p. 175 and p. 179 (`gnoth2` keeps `frame: 1`, the new
  `gnoth5` gets `frame: 2`) and the survival of `glom9` [M86 p. 178] follow from two
  readings: the catchall is made at the first confirming term (D-030), and the Gnoth-setter
  reuses a gnoth whose range begins where a frame begins (D-036). Not reproduced: the
  number of `top-down glom` lines (5, 5, 3 against 2, 3, 2) and p. 168's `groups:` and
  `glom:` lines; and `glom3` and `glom8` are still present when the Gnoth-setter runs
  (D-063). Two numbers of the transcripts fixed two tables: `stayval: 0.0` with the remote
  sameness bond `bond1` lying between two gnoths means Cycle lists no pull or push bonds
  (D-039), and `best: nil` means no neighbour of Cycle values that bond (D-038); the starting
  values were changed accordingly. Figure 1's `15`, `10`, `-5`, `20` and Y-group's `8`
  [M86 pp. 102-104] and the transcript's `stayval: 15.0` / `best: (Y-group 4.0)` [M86 p. 176]
  (from p. 174's three bonds, the gnoths `(1 2) (1 2) (3)` and a first try that counts for
  nothing, D-041) come out of the one stability rule of SPEC 11.4; with `bond5` added, as
  p. 176 does right after the check, the same state gives p. 104's 20 and 8. The second
  episode of run 3 (`stayval: 15.0` in a full run) waits for the Reformulator and the
  Gnoth-caster (items 16-17), which make the second hypothesis.

- **The gnoth operations (item 16):** every before/after example of pp. 110-118, both series
  of pp. 120-121 and the three PROGRAMs of p. 122 come out as printed, on real gloms, with
  the surviving gloms keeping their identity. On a `1 2 1` run brought to the Gnoth-setter
  (`gnoths: (gnoth1 gnoth2 gnoth3)`, `Countup --> S-group`), the transcript's first PROGRAM
  `((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil))))`, posted by hand
  since the Reformulator is item 17's, prints its task line and one `top-down glom` line and
  leaves `show-parse` at `((1 2) (1))` with gnoth1 gone, as p. 174 shows; the bare second
  move `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` on the gnoths of p. 176 leaves gnoth5 capped
  over `(1 2 3)`, terms 3 to 5, with gnoth3 gone, as pp. 178-179 show, which required reading
  the shift as moving the border past the listed glom (D-065). One finding for the
  Reformulator (item 17): D-043's strength, the pull minus the larger of the two holdings,
  gives 0 for the "real move" of p. 122 (the 3 pulled by the 2 of `(1 2)`, whose holding is
  10), so that move could never pass the threshold of 8; the strength is now the pull minus
  the mover's holding (D-043 amended), which gives 10 there and leaves the transcript's
  `SHIFT-RIGHT gnoth1 gnoth2 (glint1)` at 10 as before.

- **The Reformulator and the Bond-assessor (item 17):** on `1 2 1`, every seed of 0-9 runs
  from `Countup --> S-group` through Reformulators, Bond-assessors and one Gnoth-operator to
  the Gnoth-caster, with the Bond-assessor's numbers starting at 8.0 and ending at 10.0, the
  one shift of the 1 into the 2's gnoth carried out once (a second Gnoth-operator, set out by
  a Reformulator that ran before the first had acted, finds gnoth1 gone and terminates, as
  p. 124 says) and `show-parse` at `((1 2) (1))` [M86 pp. 174, 178]; the transcript's exact
  numbers 8.0, 8.0, 9.0, 10.0 appear on the seeds where the Gnoth-operator runs before the
  second Reformulator, and 8.0, 9.0, 9.0, 10.0 on the others. `3 7 3 3` under Cycle prints
  `Bond-assessor on (Cycle 0.0)` and goes straight to the Gnoth-caster, as run 2's lack of
  any reformulation requires (D-039). Figure 1's "real move" of p. 122 is found at level 2
  of p. 107's order with strength 10 and built as the SHIFT-LEFT and CAPTURE-RIGHT the
  thesis gives; the deeper move of p. 122 builds with its three repairs, the first `nil`
  because Figure 2's glom3 holds glom1 alone. Two differences from the printout, both
  recorded: the first move is printed bare, `((SHIFT-RIGHT gnoth1 gnoth2 (glint1)))`, where
  p. 174 wraps it in a PROGRAM with `(ENCLOSE gnoth1 nil)` (D-044: the thesis's own rule
  makes a top-level pull a single move, and the wrapper would follow only from glint1 being
  nested under a glom of its own); and the second move of p. 177, `(SHIFT-RIGHT gnoth3
  gnoth5 (glint3))`, is not derivable by the rule of p. 106 from the bonds printed at that
  moment, since nothing bonds the 2 of the second `(1 2)` to the 3 until `bond8` is made
  after the Gnoth-operator ran (D-043); given bond8 the reconstruction moves the 3 into the
  second gnoth and captures it beside the 2, which gives the same parse `((1 2) (1 2 3))` as
  p. 178. No special case was added. The second hypothesis itself waits for the
  Gnoth-caster (item 18).

- **The Gnoth-caster, the Slip-scout and the blast (item 18):** the four casts and
  generalisations the sources give are reproduced from hand-set gnoths (`(3 4) (3 4) (3)` to
  `(S-group 3 2)` and `(1 2) (1 2 3)` to `(S-group 1 (Countup 2))` [M86 p. 125]; `(1 2) (1)`
  to `casts: ((S-group 1 2) (S-group 1 1))`, `(S-group 1 2)` [M86 p. 174]; `(3 7) (3) (3)`
  under Cycle to `casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3)))`, `(Cycle 3 3 (3 7
  3))` [M86 p. 168]), and from whole runs: `1 2 1` on every seed of 0-9 prints run 3's
  `casts: ((S-group 1 2) (S-group 1 1))`, `(S-group 1 2)`, `new hypoth candidate (S-group 1
  2)`, `doing task Call-term` in order and, with `2` entered, the guess `(1 2)(1 2)(1 2)`
  [M86 pp. 174-175]; `3 7 3 3` on the fence seed prints run 2's three casts and form after
  `best: nil` and, with `7` entered, guesses `3 7 3 3 7` [M86 pp. 168-169]. Run 3's third
  candidate, `casts: ((S-group 1 2) (S-group 1 3))` to `(S-group 1 (Countup 2))` [M86
  p. 178], is reproduced by the Gnoth-caster on p. 178's gnoths `(1 2) (1 2 3)`, but **not
  from a run of the whole sequence**: the shift of the 3 into the second `(1 2)` is not
  found by the Reformulator from the bonds present (item 17's finding on p. 177's move),
  the casts of `(1 2) (1 2) (3)` do not generalise and the `3` does not begin `(1 2)`, so
  the third episode fails three times (two Slip-scouts, which stay or slip to C-group and
  back) and blasts; stage one then runs again over `1 2 1 2 3` and the Hypothesizer gives
  `(S-group 1 (Countup 2))` on 17 of 40 seeds (the guess `(1 2)(1 2 3)(1 2 3 4)` follows,
  as p. 179) and the Tuple fall-back `(Tuple 1 5 (1 2 1 2 3))` on 21 (a template `(S-group
  n 2)` accepted with the `3` as a prefix of a unit, whose generalised form does not
  postdict: D-031's fall-back, a stage-one matter). So the reconstruction reaches the
  thesis's final hypothesis for `1 2 1 2 3 1 2 3 4` on about two seeds in five, by a
  different route than the printout's. On `3 7 3 7` continued to eight terms, the 35 fence
  seeds of 100 guess `(Cycle 3 2 (3 7))` at the fourth term as before; of the 65 cover seeds
  (hypothesis `(Y-group 3 3 (3 7 3))` refuted at the fourth term, the gnoths `(3 7 3)` and
  `(7)` uncastable under Y-group and under the classes the Slip-scouts try), every one
  blasts after three failures, and 57 then glom the fence in the renewed stage one and guess
  the Cycle at a later term (some after a second or third blast, each time with a longer
  palindrome `(Y-group 3 5 (3 7 3 7 3))` refuted first); 6 never glom the fence again before
  the terms run out (the uniform scout draw of item 10's finding, now over eight glints) and
  2 guess `(Constant (Tuple 3 2 (3 7)))`, D-031's reading of a C-group template over a Tuple
  unit, which predicts the right terms. Two consequences of the decisions are recorded with
  them: a fence hypothesis refuted by a term that begins no repetition grows to a Cycle of
  all the terms (D-037), and the Hypothesizer's forms are not checked against the
  freeze-dried list (D-035). No sequence, term or seed is special-cased.

What it will be measured against is
already in place: the three runs transcribed from the page images (`tests/data/run1_3737.txt`,
`run2_373373.txt`, `run3_121231234.txt`), their milestones and the reported results on the
dozen (`tests/data/expectations.json`), and SPEC.md section 15, which says what must match
(the milestones in order for the same terms, the printed formats, the numbers that follow
from the mechanisms) and what cannot (the exact task order, which depended on an unrecorded
random seed).
