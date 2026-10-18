# The appendix runs, replayed: the three printouts against the reconstruction

Meredith's thesis prints three runs of her program [M86 pp. 164-179]. This note compares the
reconstruction with all three, `3 7 3 7` (run 1, pp. 164-166), `3 7 3 3 7 3` (run 2,
pp. 167-169) and `1 2 1 2 3 1 2 3 4` (run 3, pp. 170-179, with the wrong guess and the
"nope"), by replaying each over 100 seeds with exactly the console inputs she typed
(`inputs_in_order` of `tests/data/expectations.json`: the terms and the `show`,
`show-hypothesis`, `show-parse`, `show-seq` commands, in her order) and the answers she gave
to the guesses (`ok`; `nope` then `ok` in run 3). Everything here is produced by

    python3 -m seekwhence.bench appendix --seeds 100 --markdown
    python3 -m seekwhence.bench appendix --runs run1_3737 run2_373373 --side-by-side 54 --markdown
    python3 -m seekwhence.bench appendix --runs run3_121231234 --side-by-side 30 --markdown

and the thresholds of `tests/test_appendix.py` are set from the rates measured here. Runs 1
and 2 were done in item 20, run 3 in item 21; the item-21 changes to the engine (below)
altered a few of the run-2 numbers, and the tables are regenerated.

## Method

The original's random seed is not recorded, and "no assumptions can be made about which of two
competing tasks will run first" [M86 p. 41], so an event-for-event replay is impossible
(SPEC 15.5 item 16; DECISION D-055). What is compared is the **milestones** of each printout in
order: the terms, the first spark and bond, the proposals, the glom and the failed glom, the
Plato-scout, the template with its lines, the hypothesis, `gnoths constructed`, the `show`
printouts field for field, the filtering lines, `set-out -- validity: 0`, the Gnoth-setter's
output, the slip-check with its numbers, the bonds made during reformulation, the casts and
the generalised form, the candidate, the guess with its block and the `bye`. Each milestone
is turned into the printed lines it requires (`seekwhence/milestones.py`) and looked for in
our session's output **within the turn it was printed in** (the k-th input opens the k-th
turn of ours, since the inputs are hers), in two readings:

- **exact**: the lines as printed, structure names included, in the printout's order;
- **reached**: the milestone happened in its turn, in any order, with every structure number
  but a glint's (`glom4`, `spark6`, `bond1`) a wildcard. A glint's number is its term's
  position [M86 p. 61]; a glom's, spark's or bond's depends on the run's history (D-014).

Each run is replayed in its printout's own mode: verbose (`doing task` lines) for runs 1
and 3, terse for run 2, whose printout has no task lines but does print `set-out --
validity: 0` (D-055; item 21). The matcher is checked against the transcriptions themselves:
applied to Meredith's own printout, it finds every milestone of all three runs exactly
(`test_every_milestone_is_found_exactly_in_its_own_transcript`). The order of **task types**
is compared too, as a summary and not a milestone: the share of the printout's consecutive
first-appearance pairs (type a first ran before type b) that hold in ours.

Three things are not compared, by design (D-055): the number of `doing task` lines, the
spark and bond numbers, and the number of `top-down glom` lines the Gnoth-setter prints
(five on p. 168 against three of ours, D-036) with p. 168's `groups:`/`glom:` lines.

## What item 20 changed in the engine

Three readings were corrected on the evidence of the replays of runs 1 and 2; none
special-cases a sequence, a term or a seed.

1. **The Hfilter runs first after a term (D-012).** In all six places where a term is
   entered under a hypothesis, `doing task Hfilter` is the first task line after the prompt
   [M86 pp. 166, 168, 172, 175, 176, 179]. The Hfilter was posted at urgency 10 and often
   lost the first draw to the term's Sparkler-plus tasks; it is now run as soon as the term
   is entered (`World.run_now`), ahead of any draw, with the term's cytoplasm tasks left on
   the rack to run after it, as run 2's sparks 13-17 and run 3's Sparkler-plus lines follow
   the filtering line [M86 pp. 168, 172].
2. **Sparks are made between any two elements that are not nested in each other (D-015,
   D-025).** The thesis forbids only a spark between a glom and its own subglom ("looks for
   gloms that are not subgloms of each other" [M86 p. 63]); the reconstruction had restricted
   sparks to top-level elements, which contradicts `spark11 --- between glint1glint2` with
   both under `glom1` [M86 p. 165], run 2's sparks 13-17 and `bond3` between `glint1`, under
   the gnoth-glom, and the new `glint4` [M86 p. 168], and run 3's `Sparkler-plus on (glint3
   glint1 10)`, `(glom1 glint3 10)`, `bond2`, `bond3`, `bond4` with nested members [M86
   p. 172]. A new element's Sparkler-plus tasks now target every element of the cytoplasm at
   any level (pseudo-gloms included, printed `(glint3 glom3 2)` and never sparked, as
   p. 172 shows), and the Sparkler draws from every real element. This is what lets run 2's
   `bond3` (glint1-glint4) appear on every Cycle seed (it appeared on 2 of 100 before).
3. **The Gnoth-setter's urgency is 10, not 2 (D-008).** The starting value 2 was justified by the
   p. 176 slip-check "seeing three bonds", but those bonds existed before the refuting term
   (p. 175's `show`); in run 2 the Gnoth-setter ran before any Tester of the five new sparks
   (its `slip-check: stayval: 0.0 / best: nil` is computed with `bond1` alone, and `bond2`,
   `bond3` follow it [M86 p. 168]), and in run 3's second refutation before any new bond
   [M86 p. 176]; only run 3's first refutation, under the strict Countup, shows it late
   [M86 pp. 172-173]. At 2 the adjacent sameness bond `3`-`3` and even the group `(3 3)` were
   made before the Gnoth-setter on 16 of 23 Cycle seeds, and the slip-check then slipped
   from Cycle to C-group (pressure 2.0 against a staying pressure of 0.0, the thesis's own
   rule [M86 p. 104]), after which no gnoth could be cast and the episode blasted. At 10,
   the same as the Tester's and Bonder's, 30 of 35 Cycle seeds stay with Cycle as p. 168
   does.

## What item 21 changed in the engine

Run 3's printout settled seven more points, none of them a special case for a sequence, a
term or a seed; each is recorded under its decision.

1. **The Dissolver of a term goes on the top-level element holding the previous term's
   glint (D-024).** `Dissolver on (glint1)` after the second term [M86 pp. 164, 170],
   `Dissolver on (glom3)` after the third, with glint2 under the bottom-up `glom1` under the
   gnoth-glom `glom3` [M86 p. 172], and `Dissolver on (glom8)` and `on (glom10)`, the caps
   over glint3 and glint4, both run after the fifth term (the fourth term's Dissolver was
   never drawn before the guess) [M86 p. 176]. The earlier rule (every element whose focus
   ended) gave a bare `Dissolver` after the third term.
2. **A refutation dissolves the caps over the previous frame (D-063).** All three
   `is not in cytoplasm` lines of the printout are Dissolvers finding a pseudo-glom gone
   *before* the Gnoth-setter ran: `glom3`, the gnoth-glom over `(1 2)` at the first
   refutation (the bottom-up `glom1` is free again: `Sparkler-plus on (glom1 glint3 10)`,
   `bond4 --- between glom1glint3` [M86 pp. 172-173]), and `glom8` (gnoth3's cap over the
   `1` of term 3) with `glom10` (the frame glom of term 4) at the second, while `glom9`, the
   cap over terms 1-2, is still listed after the next Gnoth-setter [M86 pp. 176, 178]. One
   rule gives all of it: on a refutation the Hfilter dissolves every top-level pseudo-glom
   holding a glint of the frame before the one the troublesome term falls in (the frame
   that term may belong with), besides the frame gloms. It also fits the Gnoth-setter's
   glom counts: five `top-down glom` lines for three gnoths at both first refutations and
   three for the two gnoths it fills at the second (2n - 1 for n gnoths filled), which says
   run 2's gnoth1 was filled afresh too [M86 pp. 168, 173, 176]; the reconstruction had
   kept it intact.
3. **The Gnoth-caster writes each gnoth's cast into its `form` field (D-048).** After
   `casts: ((S-group 1 2) (S-group 1 1))`, `show` prints `form: (S-group 1 2)` and
   `form: (S-group 1 1)` [M86 pp. 174-175]; after `((S-group 1 2) (S-group 1 3))`,
   `form: (S-group 1 3)` [M86 pp. 178-179]. Ours printed `form: nil`.
4. **A gnoth operation sets `groups` on the gnoth it recaps (D-066).** `groups: nil` is
   printed for gnoth2, which received glint1 in the first PROGRAM, and not for the
   untouched gnoth3 [M86 p. 175]; for gnoth5 after the second shift [M86 p. 179]. The field
   holds the gloms operations made under the gnoth, none for a shift.
5. **The assessed bond lists are printed by element from right to left (D-055).** Gnoth2's
   `external-bonds: ((bond2 0) (bond5 -5) (bond6 0) (bond3 -5))` is glint2's bonds in
   creation order and then glint1's; gnoth5's `((bond6 0) (bond5 -5) (bond2 0) (bond3 -5))`
   and `internal-bonds: ((bond8 10) (bond7 10))` the same way [M86 p. 179]. The values are
   unchanged.
6. **`set-out -- validity: 0` is the terse mode's line (D-055).** Run 2, which prints no
   task lines, prints it after the filtering line [M86 p. 168]; run 3, which prints every
   task line, prints it at neither of its refutations [M86 pp. 171-172, 176]. The trace
   now prints it only without the task lines, and the bench replays run 2 in that mode.
7. **The Template-scout's own application prints `template:` and an evaluator `on nil`
   (D-060).** `template: (form (S-group 1 2)` and `doing task Template-evaluator on nil`
   [M86 pp. 170-171] where the applier's path prints `template made : (form ...` and a
   bare evaluator [M86 pp. 165, 167].

Run 2's golden file was regenerated for 2 and 6 (its only differences: the `set-out` line
now in the terse trace, a third `top-down glom` from the Gnoth-setter and the numbers after
it); run 1's is unchanged.

## The rates over 100 seeds

### run1_3737 (The Sequence  3 7  3 7  3 7), pp. 164-166, 100 seeds

outcomes: no-terms: 65, ok: 35
guesses: (Cycle 3 2 (3 7)): 35
guesses judged against the sequence: right: 35
seeds with every milestone found exactly: 1 of 100 (first: 33)
task-type first-appearance order: 90% of the transcript's consecutive pairs hold (1438 of 1600 over the seeds); types never run: none; types run that the transcript never printed: empty-task: 100, Bond-assessor: 65, Gnoth-caster: 65, Gnoth-setter: 65, Reformulator: 65, Slip-scout: 65, Template-resolver: 51, Plato-evaluator: 3

| # | milestone | p. | exact | reached |
|---|---|---|---|---|
| 1 | term 1: 3 | 164 | 100/100 | 100/100 |
| 2 | term 2: 7 | 164 | 100/100 | 100/100 |
| 3 | spark spark1 | 164 | 24/100 | 100/100 |
| 4 | term 3: 3 | 164 | 100/100 | 100/100 |
| 5 | bond bond1 | 164 | 100/100 | 100/100 |
| 6 | glom-proposal Same -cover --> glint1 | 165 | 57/100 | 57/100 |
| 7 | glom-proposal Same -fence --> glint1 | 165 | 57/100 | 57/100 |
| 8 | glom glom1 (Same print-value fence) | 165 | 35/100 | 35/100 |
| 9 | glom-failed (glint1 glint2 glint3) | 165 | 51/100 | 51/100 |
| 10 | plato-scout (Cycle) glom1 | 165 | 27/100 | 35/100 |
| 11 | template (Cycle 3 2 (3 7)) | 165 | 31/100 | 35/100 |
| 12 | hypothesis (Cycle 3 2 (3 7)) | 165 | 33/100 | 35/100 |
| 13 | gnoths-constructed glom3 | 165 | 80/100 | 100/100 |
| 14 | command show-hypothesis | 166 | 35/100 | 35/100 |
| 15 | term 4: 7 | 166 | 100/100 | 100/100 |
| 16 | hfilter (Cycle 3 2 (3 7)) (confirmed) | 166 | 35/100 | 35/100 |
| 17 | guess (Cycle 3 2 (3 7)): 3 7 3 7 (ok) | 166 | 35/100 | 35/100 |
| 18 | bye | 166 | 35/100 | 35/100 |

### run2_373373 (The Sequence 3 7 3  3 7 3), pp. 167-169, 100 seeds

outcomes: ok: 97, no-terms: 3
guesses: (Y-group 3 3 (3 7 3)): 67, (Cycle 3 3 (3 7 3)): 30
guesses judged against the sequence: right: 97
seeds with every milestone found exactly: 0 of 100
task-type order: the printout has no task lines

| # | milestone | p. | exact | reached |
|---|---|---|---|---|
| 1 | term 1: 3 | 167 | 100/100 | 100/100 |
| 2 | term 2: 7 | 167 | 100/100 | 100/100 |
| 3 | spark spark1 | 167 | 24/100 | 100/100 |
| 4 | term 3: 3 | 167 | 100/100 | 100/100 |
| 5 | bond bond1 | 167 | 100/100 | 100/100 |
| 6 | glom-proposal Same -cover --> glint1 | 167 | 57/100 | 57/100 |
| 7 | glom-proposal Same -fence --> glint1 | 167 | 57/100 | 57/100 |
| 8 | glom glom1 (Same print-value fence) | 167 | 35/100 | 35/100 |
| 9 | glom-failed (glint1 glint2 glint3) | 167 | 51/100 | 51/100 |
| 10 | template (Cycle 3 2 (3 7)) | 167 | 31/100 | 35/100 |
| 11 | gnoths-constructed glom3 | 167 | 97/100 | 100/100 |
| 12 | command show | 167 | 35/100 | 35/100 |
| 13 | command show-hypothesis | 168 | 35/100 | 35/100 |
| 14 | hypothesis (Cycle 3 2 (3 7)) | 168 | 35/100 | 35/100 |
| 15 | term 4: 3 | 168 | 100/100 | 100/100 |
| 16 | hfilter (Cycle 3 2 (3 7)) (refuted) | 168 | 35/100 | 35/100 |
| 17 | refutation set-out -- validity: 0 | 168 | 35/100 | 35/100 |
| 18 | gnoth-setter gnoths: (gnoth1 gnoth2 gnoth3) | 168 | 35/100 | 35/100 |
| 19 | slip-check stayval 0.0, best None | 168 | 30/100 | 30/100 |
| 20 | bond bond2 | 168 | 11/100 | 33/100 |
| 21 | bond bond3 | 168 | 16/100 | 35/100 |
| 22 | casts (Cycle 3 3 (3 7 3)) | 168 | 28/100 | 30/100 |
| 23 | new-hypothesis-candidate (Cycle 3 3 (3 7 3)) | 169 | 28/100 | 30/100 |
| 24 | hypothesis (Cycle 3 3 (3 7 3)) | 169 | 28/100 | 30/100 |
| 25 | command show-seq | 169 | 35/100 | 35/100 |
| 26 | term 5: 7 | 169 | 35/100 | 35/100 |
| 27 | hfilter (Cycle 3 3 (3 7 3)) (confirmed) | 169 | 0/100 | 30/100 |
| 28 | guess (Cycle 3 3 (3 7 3)): 3 7 3 3 7 (ok) | 169 | 30/100 | 30/100 |
| 29 | bye | 169 | 32/100 | 32/100 |

### run3_121231234 (The Sequence  1 2  1 2 3  1 2 3 4), pp. 170-179, 100 seeds

outcomes: ok: 99, no-terms: 1
guesses: (S-group 1 2): 100, (S-group 1 (Countup 2)): 95, (Tuple 1 5 (1 2 1 2 3)): 4
guesses judged against the sequence: wrong: 104, right: 95
seeds with every milestone found exactly: 0 of 100
task-type first-appearance order: 91% of the transcript's consecutive pairs hold (1996 of 2200 over the seeds); types never run: none; types run that the transcript never printed: Slip-scout: 16, Template-applier: 15, Template-resolver: 7, Burster: 4

| # | milestone | p. | exact | reached |
|---|---|---|---|---|
| 1 | term 1: 1 | 170 | 100/100 | 100/100 |
| 2 | task empty-task | 170 | 100/100 | 100/100 |
| 3 | term 2: 2 | 170 | 100/100 | 100/100 |
| 4 | spark spark1 | 170 | 24/100 | 100/100 |
| 5 | bond bond1 | 170 | 100/100 | 100/100 |
| 6 | glom-proposal Pred -group --> glint2 | 170 | 64/100 | 64/100 |
| 7 | glom glom1 (Pred print-value group) | 170 | 43/100 | 43/100 |
| 8 | glomtester-failed Glomtester failed glint2 | 170 | 18/100 | 18/100 |
| 9 | plato-scout (S-group P-group) glom1 | 170 | 93/100 | 100/100 |
| 10 | plato-evaluator (P-group) glom1 | 170 | 89/100 | 92/100 |
| 11 | template (S-group 1 2) | 170 | 56/100 | 100/100 |
| 12 | gnoths-constructed glom3 | 171 | 89/100 | 100/100 |
| 13 | hypothesis (Countup 1) | 171 | 41/100 | 100/100 |
| 14 | command show-hypothesis | 171 | 100/100 | 100/100 |
| 15 | command show | 171 | 43/100 | 43/100 |
| 16 | term 3: 1 | 171 | 100/100 | 100/100 |
| 17 | hfilter (Countup 1) (refuted) | 171 | 100/100 | 100/100 |
| 18 | bond bond2 | 172 | 41/100 | 100/100 |
| 19 | glom-dissolved glom3 | 172 | 83/100 | 91/100 |
| 20 | bond bond3 | 172 | 18/100 | 100/100 |
| 21 | glom-proposal Same -cover --> glint3 | 172 | 2/100 | 6/100 |
| 22 | glom-proposal Same -fence --> glint3 | 172 | 2/100 | 6/100 |
| 23 | bond bond4 | 173 | 0/100 | 14/100 |
| 24 | gnoth-setter gnoths: (gnoth1 gnoth2 gnoth3) | 173 | 7/100 | 100/100 |
| 25 | plato-scout (C-group S-group P-group Y-group Cycle Tuple) glom6 | 173 | 0/100 | 0/100 |
| 26 | reigning-class-change Countup --> S-group | 174 | 7/100 | 100/100 |
| 27 | bond-assessor S-group 8.0 | 174 | 65/100 | 100/100 |
| 28 | gnoth-operation ((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil)))) | 174 | 0/100 | 0/100 |
| 29 | bond-assessor S-group 8.0 | 174 | 65/100 | 100/100 |
| 30 | bond-assessor S-group 9.0 | 174 | 83/100 | 100/100 |
| 31 | bond-assessor S-group 10.0 | 174 | 93/100 | 100/100 |
| 32 | casts (S-group 1 2) | 174 | 99/100 | 100/100 |
| 33 | new-hypothesis-candidate (S-group 1 2) | 174 | 99/100 | 100/100 |
| 34 | hypothesis (S-group 1 2) | 174 | 99/100 | 100/100 |
| 35 | command show-hypothesis | 174 | 100/100 | 100/100 |
| 36 | command show-parse | 174 | 100/100 | 100/100 |
| 37 | command show | 174 | 0/100 | 43/100 |
| 38 | term 4: 2 | 175 | 100/100 | 100/100 |
| 39 | hfilter (S-group 1 2) (confirmed) | 175 | 0/100 | 100/100 |
| 40 | guess (S-group 1 2): (1 2)(1 2)(1 2) (nope) | 175 | 100/100 | 100/100 |
| 41 | command show-seq | 176 | 100/100 | 100/100 |
| 42 | term 5: 3 | 176 | 100/100 | 100/100 |
| 43 | hfilter (1 2)1 (refuted) | 176 | 0/100 | 0/100 |
| 44 | glom-dissolved glom8 | 176 | 91/100 | 98/100 |
| 45 | glom-dissolved glom10 | 176 | 0/100 | 98/100 |
| 46 | gnoth-setter gnoths: (gnoth2 gnoth3 gnoth5) | 176 | 4/100 | 100/100 |
| 47 | slip-check stayval 15.0, best ['Y-group', 4.0] | 176 | 3/100 | 31/100 |
| 48 | bond bond5 | 176 | 2/100 | 61/100 |
| 49 | bond bond6 | 177 | 2/100 | 100/100 |
| 50 | bond bond7 | 177 | 7/100 | 58/100 |
| 51 | gnoth-operation ((SHIFT-RIGHT gnoth3 gnoth5 (glint3))) | 177 | 0/100 | 6/100 |
| 52 | bond-assessor S-group 8.0 | 177 | 62/100 | 100/100 |
| 53 | bond bond8 | 177 | 5/100 | 100/100 |
| 54 | bond-assessor S-group 8.0 | 178 | 60/100 | 100/100 |
| 55 | bond-assessor S-group 9.0 | 178 | 74/100 | 100/100 |
| 56 | bond-assessor S-group 10.0 | 178 | 85/100 | 100/100 |
| 57 | casts (S-group 1 (Countup 2)) | 178 | 82/100 | 85/100 |
| 58 | new-hypothesis-candidate (S-group 1 (Countup 2)) | 178 | 82/100 | 85/100 |
| 59 | hypothesis (S-group 1 (Countup 2)) | 178 | 92/100 | 95/100 |
| 60 | command show-hypothesis | 178 | 95/100 | 95/100 |
| 61 | command show-parse | 178 | 85/100 | 85/100 |
| 62 | command show | 178 | 0/100 | 0/100 |
| 63 | command show-seq | 179 | 100/100 | 100/100 |
| 64 | term 6: 1 | 179 | 100/100 | 100/100 |
| 65 | hfilter (S-group 1 (Countup 2)) (confirmed) | 179 | 0/100 | 95/100 |
| 66 | guess (S-group 1 (Countup 2)): (1 2)(1 2 3)(1 2 3 4) (ok) | 179 | 95/100 | 95/100 |
| 67 | bye | 179 | 99/100 | 99/100 |

## Reading the tables

The two runs share their first three terms, and the seeds of the two replays are the same
seeds, so milestones 1-13 of run 1 and 1-11 of run 2 have the same counts.

**The third term decides everything.** On 35 seeds of 100 the Glom-scout gloms the fence
`(3 7)` from `glint1`, as both printouts do, and from there the Cycle template, hypothesis
and gnoth follow on every one of them (milestones 10-14: 35 reached). On the other 65 the
cover `(3 7 3)` is glommed first and dubbed Y-group, and the hypothesis is
`(Y-group 3 3 (3 7 3))`. This is item 10's finding (D-020, D-021): the Glom-scout "chooses
a cyto-element" [M86 p. 66], read as a uniform draw; the fence is proposed only from
`glint1` (`Same -fence proposed --> glint1` on 57 seeds, the cover also from `glint2` and
`glint3`), and when a cover Glomtester from another scout runs first the fence loses (the
fence was proposed but lost on 22 seeds). No weight was changed in this item; the
reasons for leaving the draw uniform are in D-020.

**Run 1 after the third term.** On the 35 fence seeds the fourth term `7` is filtered first
(`doing task Hfilter`), confirms the hypothesis, and the guess `3 7 3 7` is made and accepted
exactly as p. 166 prints it. On the 65 cover seeds the `7` refutes `(Y-group 3 3 (3 7 3))`,
the Gnoth-setter, the slip check, the Reformulator and the Gnoth-caster run (the task types
the printout never shows: Gnoth-setter, Reformulator, Bond-assessor, Gnoth-caster,
Slip-scout, each on 65 seeds), the gnoths `(3 7 3)` and `(7)` cannot be cast as one form,
the episode blasts after three failures, and the terms of the script run out (`no-terms`):
with Meredith's four terms there is no fifth to recover with. On `3 7 3 7` continued to eight
terms, 92 of 100 seeds guess the Cycle (RESULTS.md, item 18).

**Run 2 after the third term.** On the 35 fence seeds `show` prints p. 167-168's block field
for field (the glom lines, the nine gnoth fields), `show-hypothesis` prints the form, and the
fourth term `3` is filtered and refutes it with `set-out -- validity: 0`; the Gnoth-setter
prints `gnoths: (gnoth1 gnoth2 gnoth3)`. On 30 of the 35 the slip-check prints `stayval: 0.0`
/ `best: nil` as p. 168 does, the bonds `3`-`3` (adjacent sameness, her `bond2`) and `3`-`3`
remote (her `bond3`) are made, and the Gnoth-caster prints p. 168's three one-element casts,
the generalised `(Cycle 3 3 (3 7 3))` and `new hypoth candidate (Cycle 3 3 (3 7 3))`;
`show-seq` prints `3 7 3 3`, the fifth term `7` confirms the candidate and the guess
`3 7 3 3 7` is made and accepted (milestone 28: 30 seeds). On the other 5 fence seeds the
adjacent sameness bond is made before the Gnoth-setter, the slip-check sees it and slips
`Cycle --> C-group` (the thesis's rule, p. 104, with Cycle valuing no bond, D-039), the
C-group casts fail and the episode blasts; 2 of them then glom the cover and guess the
Y-group at the fifth term, 3 run out of terms. On the 65 cover seeds the fourth term `3`
*confirms* `(Y-group 3 3 (3 7 3))`, and the guess `(3 7 3)(3 7 3)(3 7 3)` is made at the
fourth term and accepted (the scripted `ok`, and rightly: the Y-group and her Cycle predict
the same terms), so the session ends one term before Meredith's and never reaches
`show-seq` or the fifth term. In all, 97 of 100 seeds end with an accepted guess, 30 by
her route.

**Exact against reached.** The gaps between the two columns have three causes. (1) Task
order within a turn: the Plato-scout and the failed cover Glommer run in either order
(milestone 10: 27 exact, 35 reached), and a spark names its two elements in the order the
Sparkler drew them (milestone 3). (2) Structure numbers: the frame glom of the fifth term is
her `glom9` after the Gnoth-setter's five top-down gloms and our `glom7` after its three
(milestone 27: 0 exact, 30 reached); the bonds of reformulation are numbered in the order the
Testers happen to run (milestones 20-21). (3) The order of the template lines against the
failed cover Glommer (milestone 11: 31 exact, 35 reached), again the rack's draw. Milestone
13 (`gnoths constructed`, 80 exact, 100 reached) is found on the cover seeds too, where the
gnoth-glom is also `glom3`, which is why it exceeds the fence's 35; the template line is
always printed with its four fields, as p. 167 does, where p. 165 prints the form alone.
In run 2 the frame glom of the fifth term is now her `glom9` against our `glom7` (the
Gnoth-setter recaps gnoth1 too, item 21).

**Task-type order.** In run 1, 90% of the printout's consecutive first-appearance pairs
(Sparkler-plus before Dissolver before Sparkler before Tester ...) hold over the seeds;
every type the printout prints runs on some seed (the cover seeds' Y-group template needs no
Template-applier, the thesis's `Template-scout` applying at once, D-060), and the types ours
runs that she never printed are stage two's on the cover seeds, `empty-task` after the first
term (printed by run 3, not by run 1, D-013), the Template-resolver (51 seeds) and a
Plato-evaluator on 3.

**Run 3, the first hypothesis.** `1 2` is glommed on every seed, labelled `Pred` (scouted
from the 2, as her two proposals are) on 43 and `Succ` (from the 1) on 57 (D-020 labels the
family from the scouted end); the Plato-scout dubs it S-group, the Plato-evaluator takes
P-group away, the Template-scout applies `(S-group 1 2)` itself (`template: (form (S-group 1
2) ...`, `Template-evaluator on nil`), the Gnoth-maker builds gnoth1 and the Hypothesizer gives
`(Countup 1)` (D-031's single-hit rule) on 100 seeds; `show` prints p. 171's block on the 43
`Pred` seeds. Her `Glomtester failed glint2` (a second proposal from the same end) occurs on
18; the other seeds' second proposal is from the other end or none.

**Run 3, the first refutation and the second hypothesis.** The third term `1` is filtered
and refutes `(Countup 1)` on 100 seeds (no `set-out` line, as in her verbose printout). The
term's Dissolver, on the gnoth-glom `glom3`, finds it gone (`glom3 is not in cytoplasm`) on
91 seeds (on the other 9 the turn ends at the candidate's `Call-term` before the Dissolver,
urgency 1, is drawn); the bonds `2`-`3` (Pred adjacent) and `1`-`3` (Same remote) are made on
100, in her order on about 40. Her `Same -cover` and `Same -fence proposed --> glint3` (6
seeds) and `bond4` between the freed `glom1` and `glint3` (14) need the Gnoth-setter to run
late in the turn, as hers did (about sixty tasks after the refutation, p. 173); ours, at
urgency 10, usually caps the glints first. The Gnoth-setter prints `gnoths: (gnoth1 gnoth2
gnoth3)` and the strict Countup slips to S-group on 100; the Reformulator's move is the bare
`((SHIFT-RIGHT gnoth1 gnoth2 (glint1)))` on 100 (her PROGRAM wrapper with `(ENCLOSE gnoth1
nil)` is D-044's known divergence); the Bond-assessor at 8.0, 9.0 and 10.0, the casts
`((S-group 1 2) (S-group 1 1))`, the candidate `(S-group 1 2)`, `show-parse` `((1 2) (1))`
and the wrong guess `(1 2)(1 2)(1 2)` refused with `nope` follow on 100. P. 174's `show` is
reached on 43 (the two bonds numbered as hers), its gnoth blocks now field for field
(`form: (S-group 1 2)`, `groups: nil`, item 21).

**Run 3, the second refutation and the third hypothesis.** The fifth term `3` refutes
`(S-group 1 2)` on 100 seeds (her `(1 2)1` after the filtering line is not reproduced,
below). The two Dissolvers, on gnoth3's cap and on the frame glom of term 4, find both gone
on 98 seeds; the Gnoth-setter prints `gnoths: (gnoth2 gnoth3 gnoth5)` on 100 and every seed
stays with S-group at the slip check. Her numbers, `slip-check: stayval: 15.0` / `best:
(Y-group 4.0)`, come out on 31 seeds, those on which the check sees exactly her three bonds;
the others have made one or two more of the bonds she makes later (`2`-`4`, `3`-`4`,
`2`-`5`, `4`-`5`) and print 20.0, 25.0 or 30.0 with the Y-group or the C-group as the best
(8.0, 4.5), and stay all the same. The second episode then moves the 3 in beside the second
`(1 2)` on 85 seeds: as `((SHIFT-LEFT gnoth3 gnoth5 (glint5)))` on 79 (the 3 pulled left by
the `4`-`5` successorship bond, D-043's holding rule) and as two SHIFT-RIGHTs, the 2 and
then the 1, ending in her own `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))`, on 6; the casts
`((S-group 1 2) (S-group 1 3))`, the candidate `(S-group 1 (Countup 2))` and `show-parse`
`((1 2) (1 2 3))` follow on those 85. On the other 15 the Reformulator finds no move above
the threshold before it reaches 10.0, the Gnoth-caster fails three times and the episode
blasts; stage one then rebuilds and gives `(S-group 1 (Countup 2))` through the template
`(S-group 1 n)` on 10 of them (item 18's finding), the Tuple fall-back `(Tuple 1 5 (1 2 1
2 3))` on 4, and nothing in time on 1. P. 178's `show` is never reached: ours has made a bond
hers never did (`1`-`4`, Succ remote) or lacks one, and the lists are in another order.

**Run 3, the end.** The sixth term `1` confirms and the guess `(1 2)(1 2 3)(1 2 3 4)` is made
and accepted on 95 seeds, Meredith's outcome. The 4 Tuple seeds also end in `ok`, because the
replay gives her second answer to whatever is guessed second; judged against the sequence
their guess `1 2 1 2 3 1 2 1 2 3 ...` is wrong, and the bench's `guesses judged` line counts
it so (95 right, 104 wrong: the 100 refused `(S-group 1 2)` guesses and the 4 Tuples). The
task-type order agrees on 91% of her consecutive pairs; every type she prints runs.

## The representative seeds: 54 (runs 1 and 2) and 30 (run 3)

Seed 54 reaches every milestone of both runs 1 and 2, with two inexact ones: the Plato-scout
runs before the cover Glommer fails (run 1, milestone 10) and the frame glom of the fifth
term is `glom7`, not `glom9` (run 2, milestone 27). Seed 30 reaches 59 of run 3's 67
milestones, 46 of them exactly; the eight it misses are the four no seed reaches (25, 28,
43, 62) and four of the rack's making (8, 23, 37, 51), all in the divergence table below.
Their whole traces are the golden files `tests/data/golden_run1_seed54.txt`,
`golden_run2_seed54.txt` (terse, the printout's mode) and `golden_run3_seed30.txt`, which
`tests/test_appendix.py` requires line for line (never regenerated to make a failing test
pass). Below, each milestone's lines in the printout and in the seed.

### run1_3737, seed 54: the transcript beside the reconstruction

| # | milestone (p.) | Meredith's printout | seed 54 | found |
|---|---|---|---|---|
| 1 | term 1: 3 (164) | `please enter a term: 3` | `please enter a term: 3` | yes |
| 2 | term 2: 7 (164) | `please enter a term: 7` | `please enter a term: 7` | yes |
| 3 | spark spark1 (164) | `spark1 --- between glint1glint2` | `spark1 --- between glint1glint2` | yes |
| 4 | term 3: 3 (164) | `please enter a term: 3` | `please enter a term: 3` | yes |
| 5 | bond bond1 (164) | `bond1 --- between glint1glint3` | `bond1 --- between glint1glint3` | yes |
| 6 | glom-proposal Same -cover --> glint1 (165) | `Same -cover proposed --> glint1` | `Same -cover proposed --> glint1` | yes |
| 7 | glom-proposal Same -fence --> glint1 (165) | `Same -fence proposed --> glint1` | `Same -fence proposed --> glint1` | yes |
| 8 | glom glom1 (Same print-value fence) (165) | `Glommer for Same print-value fence members: (glint1 glint2)` | `Glommer for Same print-value fence members: (glint1 glint2)` | yes |
| 9 | glom-failed (glint1 glint2 glint3) (165) | `failed to glom (glint1 glint2 glint3)` | `failed to glom (glint1 glint2 glint3)` | yes |
| 10 | plato-scout (Cycle) glom1 (165) | `doing task Plato-scout on (Cycle glom1)` | `doing task Plato-scout on (Cycle glom1)` | reached |
| 11 | template (Cycle 3 2 (3 7)) (165) | `create-template-glom (Cycle 3 2 (3 7))(glom1)`<br>`top-down glom glom2`<br>`template made : (form (Cycle 3 2 (3 7))`<br>`check-cycle template (Cycle 3 2 ((3 7)))` | `create-template-glom (Cycle 3 2 (3 7))(glom1)`<br>`top-down glom glom2`<br>`template made : (form (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)`<br>`check-cycle template (Cycle 3 2 ((3 7)))` | yes |
| 12 | hypothesis (Cycle 3 2 (3 7)) (165) | `doing task Hypothesizer`<br>`(Cycle 3 2 (3 7))` | `doing task Hypothesizer`<br>`(Cycle 3 2 (3 7))` | yes |
| 13 | gnoths-constructed glom3 (165) | `top-down glom glom3`<br>`gnoths constructed` | `top-down glom glom3`<br>`gnoths constructed` | yes |
| 14 | command show-hypothesis (166) | `please enter a term: show-hypothesis`<br>`(Cycle 3 2 (3 7))` | `please enter a term: show-hypothesis`<br>`(Cycle 3 2 (3 7))` | yes |
| 15 | term 4: 7 (166) | `please enter a term: 7` | `please enter a term: 7` | yes |
| 16 | hfilter (Cycle 3 2 (3 7)) (confirmed) (166) | `doing task Hfilter`<br>`new term being hypothesis-filtered through (Cycle 3 2 (3 7))`<br>`top-down glom glom4` | `doing task Hfilter`<br>`new term being hypothesis-filtered through (Cycle 3 2 (3 7))`<br>`top-down glom glom4` | yes |
| 17 | guess (Cycle 3 2 (3 7)): 3 7 3 7 (ok) (166) | `I have a guess!`<br>`hypothesis: (Cycle 3 2 (3 7)) 3 7 3 7`<br>`enter no if wrong, ok if right ok` | `I have a guess!`<br>`hypothesis: (Cycle 3 2 (3 7))`<br>`3 7 3 7`<br>`enter no if wrong, ok if right ok` | yes |
| 18 | bye (166) | `bye` | `bye` | yes |

### run2_373373, seed 54: the transcript beside the reconstruction

| # | milestone (p.) | Meredith's printout | seed 54 | found |
|---|---|---|---|---|
| 1 | term 1: 3 (167) | `please enter a term: 3` | `please enter a term: 3` | yes |
| 2 | term 2: 7 (167) | `please enter a term: 7` | `please enter a term: 7` | yes |
| 3 | spark spark1 (167) | `spark1 --- between glint1glint2` | `spark1 --- between glint1glint2` | yes |
| 4 | term 3: 3 (167) | `please enter a term: 3` | `please enter a term: 3` | yes |
| 5 | bond bond1 (167) | `bond1 --- between glint1glint3` | `bond1 --- between glint1glint3` | yes |
| 6 | glom-proposal Same -cover --> glint1 (167) | `Same -cover proposed --> glint1` | `Same -cover proposed --> glint1` | yes |
| 7 | glom-proposal Same -fence --> glint1 (167) | `Same -fence proposed --> glint1` | `Same -fence proposed --> glint1` | yes |
| 8 | glom glom1 (Same print-value fence) (167) | `Glommer for Same print-value fence members: (glint1 glint2)` | `Glommer for Same print-value fence members: (glint1 glint2)` | yes |
| 9 | glom-failed (glint1 glint2 glint3) (167) | `failed to glom (glint1 glint2 glint3)` | `failed to glom (glint1 glint2 glint3)` | yes |
| 10 | template (Cycle 3 2 (3 7)) (167) | `create-template-glom (Cycle 3 2 (3 7))(glom1)`<br>`top-down glom glom2`<br>`template made : (form (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)`<br>`check-cycle template (Cycle 3 2 ((3 7)))` | `create-template-glom (Cycle 3 2 (3 7))(glom1)`<br>`top-down glom glom2`<br>`template made : (form (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)`<br>`check-cycle template (Cycle 3 2 ((3 7)))` | yes |
| 11 | gnoths-constructed glom3 (167) | `top-down glom glom3`<br>`gnoths constructed` | `top-down glom glom3`<br>`gnoths constructed` | yes |
| 12 | command show (167) | `please enter a term: show`<br>`terms of the sequence:`<br>`3 7 3`<br>`bonds:`<br>`bond1 Same print-value (remote) -- (glint1 glint3)`<br>`gloms:`<br>`glom1 (Same print-value fence) --> (3 7) terms 1 to 2`<br>`glom3 pseudo --> ((3 7)) terms 1 to 2`<br>`gnoths:`<br>`class: Gnoths`<br>`name: gnoth1`<br>`frame: 0`<br>`plato-class: Cycle`<br>`glom: glom3`<br>`notes: nil`<br>`form: (((Cycle 3 2 (3 7)) pure))`<br>`state: stable`<br>`range: (1 2)` | `please enter a term: show`<br>`terms of the sequence:`<br>`3 7 3`<br>`bonds:`<br>`bond1 Same print-value (remote) -- (glint1 glint3)`<br>`gloms:`<br>`glom1 (Same print-value fence) --> (3 7) terms 1 to 2`<br>`glom3 pseudo --> ((3 7)) terms 1 to 2`<br>`gnoths:`<br>`class: Gnoths`<br>`name: gnoth1`<br>`frame: 0`<br>`plato-class: Cycle`<br>`glom: glom3`<br>`notes: nil`<br>`form: (((Cycle 3 2 (3 7)) pure))`<br>`state: stable`<br>`range: (1 2)` | yes |
| 13 | command show-hypothesis (168) | `please enter a term: show-hypothesis`<br>`(Cycle 3 2 (3 7))` | `please enter a term: show-hypothesis`<br>`(Cycle 3 2 (3 7))` | yes |
| 14 | hypothesis (Cycle 3 2 (3 7)) (168) | `(Cycle 3 2 (3 7))` | `(Cycle 3 2 (3 7))` | yes |
| 15 | term 4: 3 (168) | `please enter a term: 3` | `please enter a term: 3` | yes |
| 16 | hfilter (Cycle 3 2 (3 7)) (refuted) (168) | `new term being hypothesis-filtered through (Cycle 3 2 (3 7))` | `new term being hypothesis-filtered through (Cycle 3 2 (3 7))` | yes |
| 17 | refutation set-out -- validity: 0 (168) | `set-out -- validity: 0` | `set-out -- validity: 0` | yes |
| 18 | gnoth-setter gnoths: (gnoth1 gnoth2 gnoth3) (168) | `top-down glom glom4`<br>`gnoths: (gnoth1 gnoth2 gnoth3)` | `top-down glom glom4`<br>`gnoths: (gnoth1 gnoth2 gnoth3)` | yes |
| 19 | slip-check stayval 0.0, best None (168) | `slip-check: stayval: 0.0 best: nil` | `slip-check: stayval: 0.0 best: nil` | yes |
| 20 | bond bond2 (168) | `bond2 --- between glint3glint4` | `bond2 --- between glint3glint4` | yes |
| 21 | bond bond3 (168) | `bond3 --- between glint1glint4` | `bond3 --- between glint1glint4` | yes |
| 22 | casts (Cycle 3 3 (3 7 3)) (168) | `casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3))) (Cycle 3 3 (3 7 3))` | `casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3)))` | yes |
| 23 | new-hypothesis-candidate (Cycle 3 3 (3 7 3)) (169) | `new hypoth candidate (Cycle 3 3 (3 7 3))` | `new hypoth candidate (Cycle 3 3 (3 7 3))` | yes |
| 24 | hypothesis (Cycle 3 3 (3 7 3)) (169) | `new hypoth candidate (Cycle 3 3 (3 7 3))` | `new hypoth candidate (Cycle 3 3 (3 7 3))` | yes |
| 25 | command show-seq (169) | `please enter a term: show-seq`<br>`3 7 3 3` | `please enter a term: show-seq`<br>`3 7 3 3` | yes |
| 26 | term 5: 7 (169) | `please enter a term: 7` | `please enter a term: 7` | yes |
| 27 | hfilter (Cycle 3 3 (3 7 3)) (confirmed) (169) | `new term being hypothesis-filtered through (Cycle 3 3 (3 7 3))`<br>`top-down glom glom9` | `new term being hypothesis-filtered through (Cycle 3 3 (3 7 3))`<br>`top-down glom glom7` | reached |
| 28 | guess (Cycle 3 3 (3 7 3)): 3 7 3 3 7 (ok) (169) | `I have a guess!`<br>`hypothesis: (Cycle 3 3 (3 7 3))`<br>`3 7 3 3 7`<br>`enter no if wrong, ok if right ok` | `I have a guess!`<br>`hypothesis: (Cycle 3 3 (3 7 3))`<br>`3 7 3 3 7`<br>`enter no if wrong, ok if right ok` | yes |
| 29 | bye (169) | `bye` | `bye` | yes |

### run3_121231234, seed 30: the transcript beside the reconstruction

| # | milestone (p.) | Meredith's printout | seed 30 | found |
|---|---|---|---|---|
| 1 | term 1: 1 (170) | `please enter a term: 1` | `please enter a term: 1` | yes |
| 2 | task empty-task (170) | `doing task empty-task` | `doing task empty-task` | yes |
| 3 | term 2: 2 (170) | `please enter a term: 2` | `please enter a term: 2` | yes |
| 4 | spark spark1 (170) | `spark1 --- between glint1glint2` | `spark1 --- between glint2glint1` | reached |
| 5 | bond bond1 (170) | `bond1 --- between glint1glint2` | `bond1 --- between glint1glint2` | yes |
| 6 | glom-proposal Pred -group --> glint2 (170) | `Pred -group proposed --> glint2` | `Pred -group proposed --> glint2` | yes |
| 7 | glom glom1 (Pred print-value group) (170) | `Glommer for Pred print-value group members: (glint1 glint2)` | `Glommer for Pred print-value group members: (glint1 glint2)` | yes |
| 8 | glomtester-failed Glomtester failed glint2 (170) | `Glomtester failed glint2` | not found: `Glomtester failed glint2` | no |
| 9 | plato-scout (S-group P-group) glom1 (170) | `doing task Plato-scout on ((S-group P-group) glom1)` | `doing task Plato-scout on ((S-group P-group) glom1)` | yes |
| 10 | plato-evaluator (P-group) glom1 (170) | `doing task Plato-evaluator on (P-group glom1)` | `doing task Plato-evaluator on (P-group glom1)` | yes |
| 11 | template (S-group 1 2) (170) | `create-template-glom (S-group 1 2)(glom1)`<br>`top-down glom glom2`<br>`template: (form (S-group 1 2)`<br>`doing task Template-evaluator on nil` | `create-template-glom (S-group 1 2)(glom1)`<br>`top-down glom glom2`<br>`template: (form (S-group 1 2) state working coverage (1 2) glom glom2)`<br>`doing task Template-evaluator on nil` | yes |
| 12 | gnoths-constructed glom3 (171) | `top-down glom glom3`<br>`gnoths constructed` | `top-down glom glom3`<br>`gnoths constructed` | yes |
| 13 | hypothesis (Countup 1) (171) | `(Countup 1)` | `(Countup 1)` | reached |
| 14 | command show-hypothesis (171) | `please enter a term: show-hypothesis`<br>`(Countup 1)` | `please enter a term: show-hypothesis`<br>`(Countup 1)` | yes |
| 15 | command show (171) | `please enter a term: show`<br>`terms of the sequence:`<br>`1 2`<br>`bonds:`<br>`bond1 Succ print-value (adjacent) -- (glint1 glint2)`<br>`gloms:`<br>`glom1 (Pred print-value group) --> (1 2) terms 1 to 2`<br>`glom3 pseudo --> ((1 2)) terms 1 to 2`<br>`gnoths:`<br>`class: Gnoths`<br>`name: gnoth1`<br>`frame: 0`<br>`plato-class: S-group`<br>`glom: glom3`<br>`notes: nil`<br>`form: (((S-group 1 2) pure))`<br>`state: stable`<br>`range: (1 2)` | `please enter a term: show`<br>`terms of the sequence:`<br>`1 2`<br>`bonds:`<br>`bond1 Succ print-value (adjacent) -- (glint1 glint2)`<br>`gloms:`<br>`glom1 (Pred print-value group) --> (1 2) terms 1 to 2`<br>`glom3 pseudo --> ((1 2)) terms 1 to 2`<br>`gnoths:`<br>`class: Gnoths`<br>`name: gnoth1`<br>`frame: 0`<br>`plato-class: S-group`<br>`glom: glom3`<br>`notes: nil`<br>`form: (((S-group 1 2) pure))`<br>`state: stable`<br>`range: (1 2)` | yes |
| 16 | term 3: 1 (171) | `please enter a term: 1` | `please enter a term: 1` | yes |
| 17 | hfilter (Countup 1) (refuted) (171) | `doing task Hfilter`<br>`new term being hypothesis-filtered through (Countup 1)` | `doing task Hfilter`<br>`new term being hypothesis-filtered through (Countup 1)` | yes |
| 18 | bond bond2 (172) | `bond2 --- between glint2glint3` | `bond3 --- between glint2glint3` | reached |
| 19 | glom-dissolved glom3 (172) | `glom3 is not in cytoplasm` | `glom3 is not in cytoplasm` | yes |
| 20 | bond bond3 (172) | `bond3 --- between glint1glint3` | `bond2 --- between glint1glint3` | reached |
| 21 | glom-proposal Same -cover --> glint3 (172) | `Same -cover proposed --> glint3` | `Same -cover proposed --> glint3` | yes |
| 22 | glom-proposal Same -fence --> glint3 (172) | `Same -fence proposed --> glint3` | `Same -fence proposed --> glint3` | yes |
| 23 | bond bond4 (173) | `bond4 --- between glom1glint3` | not found: `bond4 --- between glom1glint3` | no |
| 24 | gnoth-setter gnoths: (gnoth1 gnoth2 gnoth3) (173) | `doing task Gnoth-setter`<br>`top-down glom glom4`<br>`gnoths: (gnoth1 gnoth2 gnoth3)` | `doing task Gnoth-setter`<br>`top-down glom glom4`<br>`gnoths: (gnoth1 gnoth2 gnoth3)` | yes |
| 25 | plato-scout (C-group S-group P-group Y-group Cycle Tuple) glom6 (173) | `doing task Plato-scout on ((C-group S-group P-group Y-group Cycle Tuple) glom6)` | not found: `doing task Plato-scout on ((C-group S-group P-group Y-group Cycle Tuple) glom6)` | no |
| 26 | reigning-class-change Countup --> S-group (174) |  | `Changing to a new reigning class. Countup --> S-group` | yes |
| 27 | bond-assessor S-group 8.0 (174) | `doing task Bond-assessor on (S-group 8.0)` | `doing task Bond-assessor on (S-group 8.0)` | yes |
| 28 | gnoth-operation ((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil)))) (174) | `doing task Gnoth-operator`<br>`((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil))))`<br>`top-down glom glom9` | `doing task Gnoth-operator on ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)))`<br>not found: `doing task Gnoth-operator` | no |
| 29 | bond-assessor S-group 8.0 (174) | `doing task Bond-assessor on (S-group 8.0)` | `doing task Bond-assessor on (S-group 8.0)` | yes |
| 30 | bond-assessor S-group 9.0 (174) | `doing task Bond-assessor on (S-group 9.0)` | `doing task Bond-assessor on (S-group 9.0)` | yes |
| 31 | bond-assessor S-group 10.0 (174) | `doing task Bond-assessor on (S-group 10.0)` | `doing task Bond-assessor on (S-group 10.0)` | yes |
| 32 | casts (S-group 1 2) (174) | `casts: ((S-group 1 2) (S-group 1 1))` | `casts: ((S-group 1 2) (S-group 1 1))` | yes |
| 33 | new-hypothesis-candidate (S-group 1 2) (174) | `new hypoth candidate (S-group 1 2)` | `new hypoth candidate (S-group 1 2)` | yes |
| 34 | hypothesis (S-group 1 2) (174) | `new hypoth candidate (S-group 1 2)` | `new hypoth candidate (S-group 1 2)` | yes |
| 35 | command show-hypothesis (174) | `please enter a term: show-hypothesis`<br>`(S-group 1 2)` | `please enter a term: show-hypothesis`<br>`(S-group 1 2)` | yes |
| 36 | command show-parse (174) | `please enter a term: show-parse`<br>`((1 2) (1))` | `please enter a term: show-parse`<br>`((1 2) (1))` | yes |
| 37 | command show (174) | `please enter a term: show`<br>`terms of the sequence:`<br>`1 2 1`<br>`bonds:`<br>`bond1 Succ print-value (adjacent) -- (glint1 glint2)`<br>`bond2 Pred print-value (adjacent) -- (glint2 glint3)`<br>`bond3 Same print-value (remote) -- (glint1 glint3)`<br>`gloms:`<br>`glom8 pseudo --> (1) terms 3 to 3`<br>`glom9 pseudo --> (1 2) terms 1 to 2`<br>`gnoths:`<br>`external-bonds: ((bond2 0) (bond3 -5))`<br>`internal-bonds: ((bond1 10))`<br>`equivalence-type: parse`<br>`groups: nil`<br>`class: Gnoths`<br>`name: gnoth2`<br>`frame: 1`<br>`plato-class: S-group`<br>`glom: glom9`<br>`notes: nil`<br>`form: (S-group 1 2)`<br>`state: stable`<br>`range: (1 2)`<br>`internal-bonds: nil`<br>`name: gnoth3`<br>`frame: 2`<br>`glom: glom8`<br>`form: (S-group 1 1)`<br>`range: (3 3)` | `please enter a term: show`<br>`terms of the sequence:`<br>`1 2 1`<br>`bonds:`<br>`bond1 Succ print-value (adjacent) -- (glint1 glint2)`<br>`bond3 Pred print-value (adjacent) -- (glint2 glint3)`<br>not found: `bond2 Pred print-value (adjacent) -- (glint2 glint3)` | no |
| 38 | term 4: 2 (175) | `please enter a term: 2` | `please enter a term: 2` | yes |
| 39 | hfilter (S-group 1 2) (confirmed) (175) | `doing task Hfilter`<br>`new term being hypothesis-filtered through (S-group 1 2)`<br>`top-down glom glom10` | `doing task Hfilter`<br>`new term being hypothesis-filtered through (S-group 1 2)`<br>`top-down glom glom8` | reached |
| 40 | guess (S-group 1 2): (1 2)(1 2)(1 2) (nope) (175) | `I have a guess!`<br>`hypothesis: (S-group 1 2)`<br>`(1 2)(1 2)(1 2)`<br>`enter no if wrong, ok if right nope` | `I have a guess!`<br>`hypothesis: (S-group 1 2)`<br>`(1 2)(1 2)(1 2)`<br>`enter no if wrong, ok if right nope` | yes |
| 41 | command show-seq (176) | `please enter a term: show-seq`<br>`1 2 1 2` | `please enter a term: show-seq`<br>`1 2 1 2` | yes |
| 42 | term 5: 3 (176) | `please enter a term: 3` | `please enter a term: 3` | yes |
| 43 | hfilter (1 2)1 (refuted) (176) | `doing task Hfilter`<br>`new term being hypothesis-filtered through` | `doing task Hfilter`<br>not found: `new term being hypothesis-filtered through (1 2)1` | no |
| 44 | glom-dissolved glom8 (176) | `glom8 is not in cytoplasm` | `glom8 is not in cytoplasm` | yes |
| 45 | glom-dissolved glom10 (176) | `glom10 is not in cytoplasm` | `glom6 is not in cytoplasm` | reached |
| 46 | gnoth-setter gnoths: (gnoth2 gnoth3 gnoth5) (176) | `doing task Gnoth-setter`<br>`top-down glom glom11`<br>`gnoths: (gnoth2 gnoth3 gnoth5)` | `doing task Gnoth-setter`<br>`top-down glom glom9`<br>`gnoths: (gnoth2 gnoth3 gnoth5)` | reached |
| 47 | slip-check stayval 15.0, best ['Y-group', 4.0] (176) | `slip-check: stayval: 15.0 best: (Y-group 4.0)` | `slip-check: stayval: 15.0 best: (Y-group 4.0)` | reached |
| 48 | bond bond5 (176) | `bond5 --- between glint2glint4` | `bond4 --- between glint2glint4` | reached |
| 49 | bond bond6 (177) | `bond6 --- between glint2glint5` | `bond6 --- between glint2glint5` | reached |
| 50 | bond bond7 (177) | `bond7 --- between glint3glint4` | `bond5 --- between glint3glint4` | reached |
| 51 | gnoth-operation ((SHIFT-RIGHT gnoth3 gnoth5 (glint3))) (177) | `doing task Gnoth-operator on ((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))`<br>`top-down glom glom14` | `doing task Gnoth-operator on ((SHIFT-LEFT gnoth3 gnoth5 (glint5)))`<br>not found: `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` | no |
| 52 | bond-assessor S-group 8.0 (177) | `doing task Bond-assessor on (S-group 8.0)` | `doing task Bond-assessor on (S-group 8.0)` | yes |
| 53 | bond bond8 (177) | `bond8 --- between glint4glint5` | `bond8 --- between glint4glint5` | reached |
| 54 | bond-assessor S-group 8.0 (178) | `doing task Bond-assessor on (S-group 8.0)` | `doing task Bond-assessor on (S-group 8.0)` | yes |
| 55 | bond-assessor S-group 9.0 (178) | `doing task Bond-assessor on (S-group 9.0)` | `doing task Bond-assessor on (S-group 9.0)` | yes |
| 56 | bond-assessor S-group 10.0 (178) | `doing task Bond-assessor on (S-group 10.0)` | `doing task Bond-assessor on (S-group 10.0)` | yes |
| 57 | casts (S-group 1 (Countup 2)) (178) | `casts: ((S-group 1 2) (S-group 1 3))` | `casts: ((S-group 1 2) (S-group 1 3))` | yes |
| 58 | new-hypothesis-candidate (S-group 1 (Countup 2)) (178) | `new hypoth candidate (S-group 1 (Countup 2))` | `new hypoth candidate (S-group 1 (Countup 2))` | yes |
| 59 | hypothesis (S-group 1 (Countup 2)) (178) | `new hypoth candidate (S-group 1 (Countup 2))` | `new hypoth candidate (S-group 1 (Countup 2))` | yes |
| 60 | command show-hypothesis (178) | `please enter a term: show-hypothesis`<br>`(S-group 1 (Countup 2))` | `please enter a term: show-hypothesis`<br>`(S-group 1 (Countup 2))` | yes |
| 61 | command show-parse (178) | `please enter a term: show-parse`<br>`((1 2) (1 2 3))` | `please enter a term: show-parse`<br>`((1 2) (1 2 3))` | yes |
| 62 | command show (178) | `please enter a term: show`<br>`terms of the sequence:`<br>`1 2 1 2 3`<br>`bonds:`<br>`bond1 Succ print-value (adjacent) -- (glint1 glint2)`<br>`bond2 Pred print-value (adjacent) -- (glint2 glint3)`<br>`bond3 Same print-value (remote) -- (glint1 glint3)`<br>`bond5 Same print-value (remote) -- (glint2 glint4)`<br>`bond6 Succ print-value (remote) -- (glint2 glint5)`<br>`bond7 Succ print-value (adjacent) -- (glint3 glint4)`<br>`bond8 Succ print-value (adjacent) -- (glint4 glint5)`<br>`gloms:`<br>`glom9 pseudo --> (1 2) terms 1 to 2`<br>`glom14 pseudo --> (1 2 3) terms 3 to 5`<br>`gnoths:`<br>`external-bonds: ((bond2 0) (bond5 -5) (bond6 0) (bond3 -5))`<br>`internal-bonds: ((bond1 10))`<br>`equivalence-type: parse`<br>`groups: nil`<br>`class: Gnoths`<br>`name: gnoth2`<br>`frame: 1`<br>`plato-class: S-group`<br>`glom: glom9`<br>`notes: nil`<br>`form: (S-group 1 2)`<br>`state: stable`<br>`range: (1 2)`<br>`external-bonds: ((bond6 0) (bond5 -5) (bond2 0) (bond3 -5))`<br>`internal-bonds: ((bond8 10) (bond7 10))`<br>`name: gnoth5`<br>`frame: 2`<br>`glom: glom14`<br>`form: (S-group 1 3)`<br>`range: (3 5)` | `please enter a term: show`<br>`terms of the sequence:`<br>`1 2 1 2 3`<br>`bonds:`<br>`bond1 Succ print-value (adjacent) -- (glint1 glint2)`<br>`bond3 Pred print-value (adjacent) -- (glint2 glint3)`<br>not found: `bond2 Pred print-value (adjacent) -- (glint2 glint3)` | no |
| 63 | command show-seq (179) | `please enter a term: show-seq`<br>`1 2 1 2 3` | `please enter a term: show-seq`<br>`1 2 1 2 3` | yes |
| 64 | term 6: 1 (179) | `please enter a term: 1` | `please enter a term: 1` | yes |
| 65 | hfilter (S-group 1 (Countup 2)) (confirmed) (179) | `doing task Hfilter`<br>`new term being hypothesis-filtered through (S-group 1 (Countup 2))`<br>`top-down glom glom15` | `doing task Hfilter`<br>`new term being hypothesis-filtered through (S-group 1 (Countup 2))`<br>`top-down glom glom12` | reached |
| 66 | guess (S-group 1 (Countup 2)): (1 2)(1 2 3)(1 2 3 4) (ok) (179) | `I have a guess!`<br>`hypothesis: (S-group 1 (Countup 2))`<br>`(1 2)(1 2 3)(1 2 3 4)`<br>`enter no if wrong, ok if right ok` | `I have a guess!`<br>`hypothesis: (S-group 1 (Countup 2))`<br>`(1 2)(1 2 3)(1 2 3 4)`<br>`enter no if wrong, ok if right ok` | yes |
| 67 | bye (179) | `bye` | `bye` | yes |

## Every divergence and its cause

| Divergence | Where | Cause | Status |
|---|---|---|---|
| The cover `(3 7 3)` is glommed before the fence `(3 7)` on 65 seeds of 100; the hypothesis is then `(Y-group 3 3 (3 7 3))` | both runs, third term | The Glom-scout's uniform draw over the three glints (D-020): the fence can be cut only from `glint1`, the cover from any of the three, and the cover Glomtester that runs first wins. The thesis says only that the scout "chooses a cyto-element" [M86 p. 66]. | A free choice, left as it is (D-020 records the alternative, a scout that starts from a bond, which cannot target p. 67's bondless centre). |
| Run 1 ends with no guess on the cover seeds | run 1, fourth term | The `7` refutes the Y-group; the gnoths `(3 7 3)` and `(7)` cast as no one form; after three failures the blast, and no fifth term in Meredith's script. | A consequence of the above; with more terms 92 of 100 seeds guess the Cycle (RESULTS.md). |
| Run 2's cover seeds guess the Y-group at the fourth term, one term before Meredith's program | run 2, fourth term | The `3` confirms `(Y-group 3 3 (3 7 3))`; its block `(3 7 3)(3 7 3)(3 7 3)` predicts her sequence. | A finding: the Y-group reading of `3 7 3 3 7 3` is as right as the Cycle's. |
| `slip-check` slips `Cycle --> C-group` on 5 of 35 fence seeds | run 2, fourth term | The adjacent sameness bond `3`-`3` is made before the Gnoth-setter runs and C-group's pressure (0.2 × 10 = 2.0) exceeds Cycle's staying pressure of 0.0 (Cycle values no bond, D-039; the slip rule of p. 104). | The Gnoth-setter's urgency raised to 10 (D-008); the residue is the rack's randomness. |
| The reconstruction prints `Changing to a new reigning class. ...` and a blast line | wherever a slip or blast happens | The printout has no line for a slip (p. 174's is Meredith's annotation) nor for a blast. | Reconstruction's own lines (D-042, D-050). |
| `set-out -- validity: 0` is printed by run 2 and not by run 3 | run 2 | The terse mode's line: run 2 prints no task lines; run 3 prints them and no `set-out` at either refutation. | Printed by the terse trace only (D-055, item 21). |
| Three `top-down glom` lines from the Gnoth-setter against five, and no `groups:`/`glom:` lines | run 2, fourth term | The Gnoth-setter makes one gnoth-glom per frame (D-036); the printout's extra gloms (2n - 1 for n gnoths filled) and the two lines under the first are not described. | Not a milestone (D-055). |
| Structure numbers after the first turns (`glom7` for `glom9`; `bond2`/`bond3` in either order) | run 2, fourth and fifth terms | The counts above and the Testers' order. | Not a milestone (D-055, D-014). |
| The second Glomtester fails on `glint1`, hers on `glint2` (milestone 8, 18 seeds as hers) | run 3, second term | Two scouts propose the group from either end (`Pred` from the 2, `Succ` from the 1); hers happened to propose from the 2 twice. | The rack's draw (D-020). |
| The proposals for `glint3` (6 seeds) and `bond4` between `glom1` and `glint3` (14) before the Gnoth-setter | run 3, third term | Her Gnoth-setter ran about sixty tasks after the refutation (pp. 172-173); ours, at the Tester's urgency (D-008), usually runs first and caps the glints, after which `glint3` is no longer top-level and `glom1` is burst. | Noted; D-008's urgency was argued from runs 2 and 3's second refutation, where hers also ran early. |
| No `Plato-scout on ((C-group ... Tuple) glom6)` and no `Template-scout on (glom6)` lines (milestone 25) | run 3, third term | One of her Gnoth-setter's five gloms was scouted for all six classes and dubbed six times (six Template-scouts, none of which did anything visible); the reconstruction's gnoth-gloms are pseudo-gloms that no scout visits (D-036). | A known divergence; which glom she scouted is not described. |
| `((SHIFT-RIGHT gnoth1 gnoth2 (glint1)))` printed bare where hers is wrapped in a PROGRAM with `(ENCLOSE gnoth1 nil)` (milestone 28) | run 3, third term | D-044: a top-level pull is one bare operation; her wrapper would follow from glint1 lying under a glom of its own inside gnoth1, which her five top-down gloms for three gnoths may mean. | A known divergence (D-044). |
| `(1 2)1` after `new term being hypothesis-filtered through` (milestone 43) | run 3, fifth term | Her program printed something other than the form after the refused guess; what it is (the block of the last guess cut to the terms?) is not described. Ours prints the form. | A known divergence (D-052). |
| `slip-check: stayval: 15.0 / best: (Y-group 4.0)` on 31 seeds; 20.0, 25.0 or 30.0 on the rest | run 3, fifth term | The check sees the bonds made so far; hers saw exactly three (p. 174's `show`); ours has often made one or two more of the bonds she makes after the check. Every seed stays with S-group. | The rack's draw; the arithmetic is the thesis's (D-041). |
| `((SHIFT-LEFT gnoth3 gnoth5 (glint5)))` on 79 seeds for her `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` (milestone 51); her own line on 6, after a SHIFT-RIGHT of the 2 | run 3, fifth term | Item 17's finding: her move is not derivable from the bonds printed at that moment; with the `4`-`5` bond the 3 is the mover (less holding) and goes left. The parse is hers, `((1 2) (1 2 3))`, on 85 seeds. | A known divergence (D-043, D-065). |
| 15 seeds blast in the second episode; 10 recover the final form from stage one, 4 guess a Tuple, 1 runs out of terms | run 3, fifth term | No move above the threshold before 10.0 (the bonds the move needs were not made in time), three casting failures, the blast (D-050); afterwards the template `(S-group 1 n)` or the Tuple fall-back (D-031). | A finding (RESULTS.md); the Tuple guesses are accepted only because her `ok` is replayed blindly. |
| P. 178's `show` never reached (milestone 62) | run 3, fifth term | Ours makes the Succ remote bond `1`-`4`, which hers never made, or lacks one of hers; the bond lines and the assessed lists then differ. | Not a milestone's worth (D-055). |
| Task order within a turn (Plato-scout before or after the failed cover Glommer; a spark's two names in either order) | both runs, third term | The rack's random draw [M86 p. 41]. | Not a milestone; the task-type first-appearance order agrees 90%. |
| `template made :` always printed with its four fields | run 1 | p. 165 prints the form only and p. 167 all four fields; ours follows p. 167. | By design (SPEC 6.2). |
