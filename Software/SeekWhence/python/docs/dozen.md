# The Blackburn dozen and the other sequences, over 20 seeds

Item 22's survey (DECISION D-070): every sequence of `seekwhence/sequences.py` run over
seeds 0-19 with at most 16 terms, every guess judged by a user who knows the rule.
`RESULTS.md` has the tables and the analysis beside Meredith's verdicts [M86 pp. 88,
145-147]; this file keeps the method, the full text reports and the routes of the notable
seeds. Regenerate the reports with

```sh
python3 -m seekwhence.bench dozen --groups blackburn p146 --seeds 20 --terms 16
python3 -m seekwhence.bench dozen --groups p88 p38 p150 fcca --seeds 20 --terms 16
```

(about one and a half minutes for the first, three and a half for the second) and the
markdown tables with `--markdown`; `--json FILE` writes every seed's record (outcome, terms,
tasks, the forms made, the guesses and their answers, refutations, blasts).

## Method

- **The sequences.** The Blackburn dozen [M86 pp. 6-7] with the reported results
  [M86 pp. 145-147]; the three sequences the thesis says it can solve beside them
  [M86 pp. 145-146]; the nine of p. 88; the typical problems of p. 38; the unconquered
  sequences of p. 150; Hofstadter's sampler [FCCA pp. 53-54] (the pre-parsed "spliceroo"
  left out). Each distinct sequence once, continued by the rule its parsed form prints, to
  40 terms (`16 15 14 15 14 13 ...` to 0).
- **The run.** A `Session` with the first 16 terms of the continuation as its script, in the
  terse trace; the per-term cap of D-011 (2000 tasks) bounds a thrashing episode; the whole
  run is bounded by the terms. Nothing else is changed: the same `Config` as the appendix
  replays.
- **The judgement.** A guess is answered `ok` when the hypothesis's own terms, listed afresh
  from its box, equal the continuation over its whole length; `no` otherwise. The report also
  counts how many refused guesses the judgement of the printed block alone (D-033) would have
  passed.
- **Solved** on a seed: a guess accepted within the terms fed. **Solved here**, for the
  comparison with the thesis's verdicts: on at least half the seeds.

## The dozen and the related sequences

```
blackburn-1: 1 1 2 1 2 3 1 2 3 4 1 2 3 4 5 ...   [M86 pp. 6-7, 145]   Meredith: unsolved
    solved 20/20 seeds; terms 4 (4-16); tasks (median) 165; outcomes ok: 20; refutations 46, blasts 46, refused guesses the block alone would pass 0
    accepted: (S-group 1 (Countup 1)) x20
    refused: (Tuple 1 3 (1 1 2)) x8
    unsolved seeds end with: none
blackburn-2: 1 2 3 4 5 6 7 ...   [M86 pp. 6-7, 145]   Meredith: solved
    solved 20/20 seeds; terms 3 (3-3); tasks (median) 37; outcomes ok: 20; refutations 0, blasts 0, refused guesses the block alone would pass 0
    accepted: (Countup 1) x20
    refused: none
    unsolved seeds end with: none
blackburn-3: 2 1 2 2 2 2 2 3 2 2 4 2 2 5 2 ...   [M86 pp. 6-7, 145-146]   Meredith: not attempted
    solved 1/20 seeds; terms 9 (9-9); tasks (median) 682; outcomes no-terms: 19, ok: 1; refutations 55, blasts 54, refused guesses the block alone would pass 0
    accepted: (Y-group 2 3 (2 (Countup 1) 2)) x1
    refused: (P-group 2 2) x20, (Tuple 2 6 (2 1 2 2 2 2)) x11, (Tuple 2 4 (2 1 2 2)) x10, (Tuple 2 5 (2 1 2 2 2)) x4, (Y-group 2 3 (2 (Cycle 1 2 (1 2)) 2)) x3, (Y-group 2 3 (2 (S-group 1 2) 2)) x1
    unsolved seeds end with: none x19
blackburn-4: 1 2 2 3 3 3 4 4 4 4 ...   [M86 pp. 6-7, 146]   Meredith: unsolved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 49, blasts 41, refused guesses the block alone would pass 0
    accepted: none
    refused: (S-group (Countup 1) 2) x19
    unsolved seeds end with: none x20
blackburn-5: 1 8 5 8 1 8 5 8 1 8 5 8 ...   [M86 pp. 6-7, 146]   Meredith: unsolved
    solved 19/20 seeds; terms 7 (6-15); tasks (median) 259; outcomes ok: 19, no-terms: 1; refutations 13, blasts 13, refused guesses the block alone would pass 0
    accepted: (Cycle 1 4 (1 8 5 8)) x19
    refused: none
    unsolved seeds end with: (Cycle 1 4 (1 8 5 8)) x1
blackburn-6: 2 1 2 2 2 3 2 4 2 5 ...   [M86 pp. 6-7, 146]   Meredith: unsolved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 43, blasts 43, refused guesses the block alone would pass 0
    accepted: none
    refused: (P-group 2 2) x20, (Tuple 2 4 (2 1 2 2)) x10, (Y-group 2 3 (2 (Cycle 1 2 (1 2)) 2)) x1, (Y-group 2 3 (2 (S-group 1 2) 2)) x1
    unsolved seeds end with: none x20
blackburn-7: 2 3 1 2 3 2 2 2 3 3 3 3 2 3 4 4 4 4 ...   [M86 pp. 6-7, 146]   Meredith: unsolved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 43, blasts 43, refused guesses the block alone would pass 0
    accepted: none
    refused: (Tuple 2 3 (2 3 1)) x34, (Tuple 2 5 (2 3 1 2 3)) x2
    unsolved seeds end with: none x20
blackburn-8: 1 2 2 3 3 4 4 5 5 6 ...   [M86 pp. 6-7, 146]   Meredith: solved
    solved 20/20 seeds; terms 5 (5-6); tasks (median) 248; outcomes ok: 20; refutations 29, blasts 21, refused guesses the block alone would pass 0
    accepted: (S-group (Countup 1) 2) x20
    refused: none
    unsolved seeds end with: none
blackburn-9: 1 2 3 3 4 4 5 5 5 6 6 6 ...   [M86 pp. 6-7, 146-147]   Meredith: unsolved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 68, blasts 68, refused guesses the block alone would pass 20
    accepted: none
    refused: (Countup 1) x20
    unsolved seeds end with: none x20
blackburn-10: 9 1 9 2 9 3 9 4 ...   [M86 pp. 6-7, 147]   Meredith: unsolved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 59, blasts 14, refused guesses the block alone would pass 39
    accepted: none
    refused: (Cycle 9 4 (9 1 9 2)) x7, (Cycle 9 6 (9 1 9 2 9 3)) x7, (Cycle 9 8 (9 1 9 2 9 3 9 4)) x7, (Cycle 9 10 (9 1 9 2 9 3 9 4 9 5)) x6, (Cycle 9 12 (9 1 9 2 9 3 9 4 9 5 9 6)) x6, (Cycle 9 14 (9 1 9 2 9 3 9 4 9 5 9 6 9 7)) x6
    unsolved seeds end with: none x14, (Cycle 9 16 (9 1 9 2 9 3 9 4 9 5 9 6 9 7 9 8)) x6
blackburn-11: 1 8 1 2 1 8 1 2 3 2 1 8 1 2 3 4 3 2 1 8 1 2 3 4 ...   [M86 pp. 6-7, 147]   Meredith: unsolved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 30, blasts 25, refused guesses the block alone would pass 20
    accepted: none
    refused: (Cycle 1 4 (1 8 1 2)) x20
    unsolved seeds end with: none x20
blackburn-12: 1 8 5 5 8 1 1 8 5 5 8 1 ...   [M86 pp. 6-7, 147]   Meredith: solved
    solved 20/20 seeds; terms 8 (7-13); tasks (median) 220; outcomes ok: 20; refutations 2, blasts 2, refused guesses the block alone would pass 0
    accepted: (Y-group 1 6 (1 8 5 5 8 1)) x18, (Y-group 1 5 (1 8 (C-group 5 2) 8 1)) x2
    refused: none
    unsolved seeds end with: none
p146-1: 2 2 3 3 3 4 4 4 4 ...   [M86 p. 146]   Meredith: solved
    solved 20/20 seeds; terms 6 (6-10); tasks (median) 451; outcomes ok: 20; refutations 60, blasts 40, refused guesses the block alone would pass 0
    accepted: (C-group (Countup 2) (Countup 2)) x20
    refused: none
    unsolved seeds end with: none
p146-2: 1 1 2 2 2 3 3 3 3 ...   [M86 p. 146]   Meredith: solved
    solved 20/20 seeds; terms 6 (6-10); tasks (median) 451; outcomes ok: 20; refutations 60, blasts 40, refused guesses the block alone would pass 0
    accepted: (C-group (Countup 1) (Countup 2)) x20
    refused: none
    unsolved seeds end with: none
p146-3: 1 8 5 8 1 1 8 5 8 1 ...   [M86 p. 146]   Meredith: solved
    solved 20/20 seeds; terms 6 (6-7); tasks (median) 118; outcomes ok: 20; refutations 4, blasts 0, refused guesses the block alone would pass 0
    accepted: (Y-group 1 5 (1 8 5 8 1)) x15, (Cycle 1 5 (1 8 5 8 1)) x4, (Y-group 1 3 (1 (Y-group 8 3 (8 5 8)) 1)) x1
    refused: none
    unsolved seeds end with: none

against the thesis's verdicts: agree: 13, we solve, she did not: 2
```

## The other sequences

```
p88-1: 1 1 1 ...   [M86 p. 88]   Meredith: solved
    solved 20/20 seeds; terms 3 (3-3); tasks (median) 36; outcomes ok: 20; refutations 0, blasts 0, refused guesses the block alone would pass 0
    accepted: (Constant 1) x20
    refused: none
    unsolved seeds end with: none
p88-2: 1 2 3 4 ...   [M86 p. 88]   Meredith: solved
    solved 20/20 seeds; terms 3 (3-3); tasks (median) 37; outcomes ok: 20; refutations 0, blasts 0, refused guesses the block alone would pass 0
    accepted: (Countup 1) x20
    refused: none
    unsolved seeds end with: none
p88-3: 1 1 1 2 2 2 3 3 3 ...   [M86 p. 88]   Meredith: solved
    solved 20/20 seeds; terms 7 (7-8); tasks (median) 947; outcomes ok: 20; refutations 56, blasts 56, refused guesses the block alone would pass 20
    accepted: (C-group (Countup 1) 3) x20
    refused: (Constant 1) x20
    unsolved seeds end with: none
p88-4: 3 4 3 4 3 4 ...   [M86 p. 88]   Meredith: solved
    solved 20/20 seeds; terms 4 (4-4); tasks (median) 89; outcomes ok: 20; refutations 20, blasts 0, refused guesses the block alone would pass 0
    accepted: (S-group 3 2) x20
    refused: none
    unsolved seeds end with: none
p88-5: 3 7 3 7 3 7 ...   [M86 p. 88]   Meredith: solved
    solved 20/20 seeds; terms 6 (4-9); tasks (median) 188; outcomes ok: 20; refutations 15, blasts 15, refused guesses the block alone would pass 0
    accepted: (Cycle 3 2 (3 7)) x20
    refused: none
    unsolved seeds end with: none
p88-6: 3 7 3 3 7 3 3 7 3 ...   [M86 p. 88]   Meredith: solved
    solved 20/20 seeds; terms 4 (4-8); tasks (median) 50; outcomes ok: 20; refutations 7, blasts 2, refused guesses the block alone would pass 0
    accepted: (Y-group 3 3 (3 7 3)) x14, (Cycle 3 3 (3 7 3)) x5, (Constant (Y-group 3 3 (3 7 3))) x1
    refused: none
    unsolved seeds end with: none
p88-7: 16 15 14 17 16 15 18 17 16 ...   [M86 p. 88]   Meredith: solved
    solved 20/20 seeds; terms 8 (7-9); tasks (median) 565; outcomes ok: 20; refutations 60, blasts 40, refused guesses the block alone would pass 0
    accepted: (P-group (Countup 16) 3) x20
    refused: (Tuple 16 4 (16 15 14 17)) x36
    unsolved seeds end with: none
p88-8: 16 15 14 15 14 13 14 13 12 ...   [M86 p. 88]   Meredith: solved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 243, blasts 223, refused guesses the block alone would pass 0
    accepted: none
    refused: none
    unsolved seeds end with: none x12, (Tuple 16 16 (16 15 14 15 14 13 14 13 12 13 12 11 12 11 10 11)) x8
p88-9: 1 2 1 2 3 1 2 3 4 ...   [M86 p. 88]   Meredith: solved
    solved 20/20 seeds; terms 6 (6-10); tasks (median) 193; outcomes ok: 20; refutations 42, blasts 6, refused guesses the block alone would pass 0
    accepted: (S-group 1 (Countup 2)) x20
    refused: (S-group 1 2) x20, (Tuple 1 5 (1 2 1 2 3)) x2
    unsolved seeds end with: none
p38-1: 2 2 2 ...   [M86 p. 38]   Meredith: no verdict
    solved 20/20 seeds; terms 3 (3-3); tasks (median) 36; outcomes ok: 20; refutations 0, blasts 0, refused guesses the block alone would pass 0
    accepted: (Constant 2) x20
    refused: none
    unsolved seeds end with: none
p38-2: 1 1 2 2 3 3 ...   [M86 p. 38]   Meredith: no verdict
    solved 20/20 seeds; terms 5 (5-6); tasks (median) 352; outcomes ok: 20; refutations 39, blasts 37, refused guesses the block alone would pass 0
    accepted: (C-group (Countup 1) 2) x20
    refused: none
    unsolved seeds end with: none
p38-5: 1 0 1 0 0 1 0 0 0 1 ...   [M86 p. 38]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 29, blasts 29, refused guesses the block alone would pass 0
    accepted: none
    refused: (P-group 1 2) x40, (Tuple 1 5 (1 0 1 0 0)) x18
    unsolved seeds end with: none x20
p38-6: 1 2 8 3 4 8 5 6 8 ...   [M86 p. 38]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 37, blasts 37, refused guesses the block alone would pass 0
    accepted: none
    refused: none
    unsolved seeds end with: none x20
p38-7: 1 2 8 3 4 5 8 6 7 8 9 8 ...   [M86 p. 38]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 37, blasts 37, refused guesses the block alone would pass 0
    accepted: none
    refused: none
    unsolved seeds end with: none x20
p38-9: 8 0 8 8 0 8 8 0 8 ...   [M86 p. 38]   Meredith: no verdict
    solved 20/20 seeds; terms 4 (4-8); tasks (median) 50; outcomes ok: 20; refutations 7, blasts 2, refused guesses the block alone would pass 0
    accepted: (Y-group 8 3 (8 0 8)) x14, (Cycle 8 3 (8 0 8)) x5, (Constant (Y-group 8 3 (8 0 8))) x1
    refused: none
    unsolved seeds end with: none
p38-10: 1 1 2 1 2 2 1 3 2 ...   [M86 p. 38]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 48, blasts 48, refused guesses the block alone would pass 0
    accepted: none
    refused: (S-group 1 (Countup 1)) x22, (Tuple 1 3 (1 1 2)) x8
    unsolved seeds end with: none x20
p150-1: 1 2 10 3 4 10 5 6 10 ...   [M86 p. 150]   Meredith: unsolved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 37, blasts 37, refused guesses the block alone would pass 0
    accepted: none
    refused: none
    unsolved seeds end with: none x20
p150-3: 1 1 1 1 2 1 1 3 1 ...   [M86 p. 150]   Meredith: unsolved
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 40, blasts 40, refused guesses the block alone would pass 40
    accepted: none
    refused: (Constant 1) x40, (Tuple 1 5 (1 1 1 1 2)) x36, (Y-group 1 3 (1 (Tuple 1 2 (1 (Tuple 1 5 (1 1 2 (C-group 1 2) 3)))) 1)) x1, (Y-group 1 3 (1 (Tuple 1 8 (1 (Tuple 1 3 (1 1 2)) (C-group 1 2) 3 (C-group 1 2) 4 (C-group 1 2) 5)) 1)) x1
    unsolved seeds end with: none x19, (Y-group 1 3 (1 (Tuple 1 8 (1 (Tuple 1 3 (1 1 2)) (C-group 1 2) 3 (C-group 1 2) 4 (C-group 1 2) 5)) 1)) x1
fcca-1: 2 2 2 3 3 2 2 2 3 3 2 2 2 3 3 ...   [FCCA p. 53]   Meredith: no verdict
    solved 20/20 seeds; terms 6 (6-6); tasks (median) 541; outcomes ok: 20; refutations 36, blasts 36, refused guesses the block alone would pass 20
    accepted: (Tuple 2 5 (2 2 2 3 3)) x20
    refused: (Constant 2) x20
    unsolved seeds end with: none
fcca-2: 1 0 0 1 1 1 0 0 0 0 1 1 1 1 1 ...   [FCCA p. 53]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 43, blasts 43, refused guesses the block alone would pass 0
    accepted: none
    refused: (Tuple 1 3 (1 0 0)) x9, (Y-group 1 3 (1 (C-group 0 2) 1)) x4, (Y-group 1 3 (1 (Tuple 0 2 ((C-group 0 2) 1)) 1)) x1, (Y-group 1 4 (1 0 0 1)) x1
    unsolved seeds end with: none x20
fcca-3: 1 0 0 1 5 1 0 0 0 5 1 1 1 1 5 0 0 0 0 5 ...   [FCCA p. 53]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 35, blasts 35, refused guesses the block alone would pass 0
    accepted: none
    refused: (Tuple 1 3 (1 0 0)) x9
    unsolved seeds end with: none x20
fcca-4: 1 2 0 2 0 2 1 3 1 3 1 2 0 2 0 2 0 3 0 3 ...   [FCCA p. 53]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 52, blasts 52, refused guesses the block alone would pass 0
    accepted: none
    refused: none
    unsolved seeds end with: none x20
fcca-7: 2 1 3 1 2 4 1 2 3 5 1 2 3 4 6 ...   [FCCA p. 54]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 48, blasts 48, refused guesses the block alone would pass 0
    accepted: none
    refused: none
    unsolved seeds end with: none x20
fcca-8: 1 2 1 3 2 1 4 3 2 1 5 4 3 2 1 ...   [FCCA p. 54]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 40, blasts 20, refused guesses the block alone would pass 0
    accepted: none
    refused: none
    unsolved seeds end with: none x20
fcca-9: 3 2 1 0 1 2 3 2 1 0 1 2 3 ...   [FCCA p. 54]   Meredith: no verdict
    solved 10/20 seeds; terms 12 (10-16); tasks (median) 2607; outcomes no-terms: 10, ok: 10; refutations 85, blasts 47, refused guesses the block alone would pass 0
    accepted: (Cycle 3 6 (3 2 1 0 1 2)) x10
    refused: none
    unsolved seeds end with: none x10
fcca-10: 0 0 0 1 1 2 4 5 5 6 6 6 7 7 8 10 11 11 ...   [FCCA p. 54]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 56, blasts 56, refused guesses the block alone would pass 20
    accepted: none
    refused: (Constant 0) x20
    unsolved seeds end with: none x20
fcca-11: 1 1 2 1 1 2 3 2 1 1 2 3 4 3 2 1 ...   [FCCA p. 54]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 75, blasts 75, refused guesses the block alone would pass 2
    accepted: none
    refused: (Tuple 1 3 (1 1 2)) x24, (S-group 1 (Countup 1)) x11, (Cycle 1 3 (1 1 2)) x2
    unsolved seeds end with: none x19, (Y-group 1 3 (1 (Tuple 1 6 (1 (Tuple 2 4 (2 1 (S-group 1 3) (Tuple 2 2 (2 (C-group 1 2))))) 2 3 4 (P-group 3 2))) 1)) x1
fcca-12: 1 1 2 3 1 2 2 3 1 2 3 3 1 1 2 3 ...   [FCCA p. 54]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 74, blasts 74, refused guesses the block alone would pass 0
    accepted: none
    refused: (Tuple 1 4 (1 1 2 3)) x12
    unsolved seeds end with: none x20
fcca-13: 1 1 2 3 1 2 2 3 1 2 3 3 1 2 2 3 1 1 2 3 ...   [FCCA p. 54]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 74, blasts 74, refused guesses the block alone would pass 0
    accepted: none
    refused: (Tuple 1 4 (1 1 2 3)) x12
    unsolved seeds end with: none x20
fcca-14: 1 2 2 3 3 1 1 2 3 3 1 1 2 2 3 1 2 2 3 3 ...   [FCCA p. 54]   Meredith: no verdict
    solved 0/20 seeds; terms -; tasks (median) -; outcomes no-terms: 20; refutations 56, blasts 48, refused guesses the block alone would pass 0
    accepted: none
    refused: (S-group (Countup 1) 2) x19, (Y-group 1 3 (1 (Tuple 2 4 (2 (Tuple 2 4 (2 3 3 (C-group 1 2))) 2 (C-group 3 2))) 1)) x4, (Y-group 1 3 (1 (Tuple 2 2 (2 (Tuple 2 2 (2 (C-group 3 2))))) 1)) x2, (Y-group 1 3 (1 (Tuple 2 2 ((C-group 2 2) (C-group 3 2))) 1)) x1
    unsolved seeds end with: none x20

against the thesis's verdicts: no verdict: 19, agree: 10, she solved, we do not: 1
```

## Notable seeds

- **Blackburn 1, seed 2, solved at the fourth term** (`python3 -m seekwhence.bench dozen
  --names blackburn-1 --trace 2 --terms 4`): `1 1` glommed and dubbed C-group, its template
  `(C-group 1 2)`, the hypothesis `(Constant 1)`; the `2` refutes it, `gnoths: (gnoth1 gnoth2
  gnoth3)`, `Changing to a new reigning class. Constant --> C-group`, three times `casts:
  ((C-group 1 2) (C-group 2 1))` with `slip-check: stayval: 7.5 / best: (S-group 4.5)` (a
  stay), `blasting away all gnoths and gloms`; then `(1 2)` glommed by successorship,
  `create-template-glom (S-group 1 n)(glom9 glom8)` over a top-down `(1)` and the `(1 2)`,
  `gnoths constructed`, `(S-group 1 (Countup 1))`; the fourth term `1` confirms, the guess
  `(1)(1 2)(1 2 3)` is accepted.
- **Blackburn 1, seed 0, solved at the sixteenth term**: the same first episode, then the
  blast leaves `1 1` to be re-glommed, the template `(C-group n 2)` gives the fall-back
  `(Tuple 1 3 (1 1 2))`, guessed and refused at the fourth term; the Tuples lengthen with the
  terms (`(Tuple 1 5 (1 1 2 1 2))`, `(Tuple 1 7 ...)`, `(Tuple 1 8 ...)`, `(Tuple 1 9 ...)`),
  each refuted and blasted, until stage one finds the S-groups and `(S-group 1 (Countup 1))`
  is guessed and accepted.
- **Blackburn 3, seed 17, the theme song solved at the ninth term**: `(P-group 2 2)` guessed
  after `2 1 2` and refused; `(Y-group 2 3 (2 (Cycle 1 2 (1 2)) 2))` guessed three times and
  refused; one blast; then the Y-group casts `(2 1 2)`, `(2 2 2)`, `(2 3 2)` generalised
  position by position give `(Y-group 2 3 (2 (Countup 1) 2))`, confirmed and accepted
  (`--names blackburn-3 --trace 17`).
- **Blackburn 5, seed 0, solved at the seventh term**: the Y-group `(1 8 5 8 1)` is made
  from the cover between the two 1's at the fifth term, as the thesis says hers was
  [M86 p. 146]; the sixth term `8` refutes it before any guess, the casts under Y-group fail
  (`((Y-group 1 5 (1 8 5 8 1)) (Y-group 8 1 (8)))`, with `Y-group --> S-group` and back),
  the episode blasts, the fence `(1 8 5 8)` is glommed and dubbed Cycle, and the seventh term
  confirms `(Cycle 1 4 (1 8 5 8))`.
- **Blackburn 8, seed 0, solved at the fifth term by reformulation**: `(Countup 1)` from
  `1 2`, refuted by the second `2`; `Countup --> S-group`; the casts `((S-group 1 2) (S-group
  2 1))` fail three times and the episode blasts; the fall-back `(Tuple 1 3 (1 2 2))` from
  the template `(S-group n 2)`, refuted by the `3`; `Tuple --> S-group`; the Gnoth-caster's
  casts `((S-group 1 2) (S-group 2 2))` generalise to `new hypoth candidate (S-group (Countup
  1) 2)`, confirmed by the next `3` and accepted.
- **`16 15 14 15 14 13 ...`**: no form of the eight types has a start that falls by one per
  hit, so every hypothesis is a Tuple of the terms so far or a P-group that the next triple
  refutes; 223 blasts over 20 seeds, the most of any sequence.
