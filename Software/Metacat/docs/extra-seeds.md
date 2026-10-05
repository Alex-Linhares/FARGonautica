# Extra-seed equivalence runs (item 17)

The final audit of loop0001 ran every problem of `tests/problems.txt` with **20 seeds
that are not golden seeds**, in both the oracle and the port, and compared the results.

```bash
python3 tests/extra-seeds.py              # 720 runs, about 2.5 min on 32 cores
python3 tests/extra-seeds.py --seeds 1    # 36 runs, about 10 s
python3 tests/extra-seeds.py --keep DIR   # keep the traces in DIR
```

## What it does

- For each of the 36 problem lines (same strings, codelet cap and keep-going flag as the
  goldens), it draws 20 seeds in [1, 2^32) from a fixed Python generator
  (`random.Random(20261003)`), skipping the line's golden seeds. A rerun repeats the same
  720 runs.
- Each run is two fresh processes: `scheme --script chez_scheme/oracle/run.ss ... --trace`
  (the original, unmodified, through the prelude) and `racket racket/cli.rkt ... --trace`
  (the port).
- It requires byte-identical traces (docs/trace-format.md), byte-identical printed output
  (commentary, answers, summary) and equal exit codes. On a difference it prints the first
  differing line of each.
- Nothing is written to `tests/golden/`. The script is not part of `tests/run-tests.sh`,
  because the gate already compares all 109 golden runs, and these 720 runs take another
  2–3 minutes of 32 cores.

## Result (2026-10-03)

**720 of 720 runs identical**: traces, printed output and exit codes.

- 2,151,725 codelets in all (the goldens have 272,957), 651 answers.
- 618 runs stopped at an answer or a give-up (suspend), 101 at their codelet cap, and 1
  in the original's `report-error-and-halt`: `eqe qeq abbba aaabaaa` seed 3401132640 halts
  at codelet 4239 (`get-constituent-objects` sent to a letter), the same error as the
  golden seed 3 at codelet 4004 (docs/anomalies_and_quirks.md). The port halts at the same
  codelet with the same message.
- No run crashed with a Chez error (the golden set avoids `abc ccbbaa ijk` seed 3, which
  does; none of its 20 extra seeds does).
- Check that the script catches a divergence: with bonds.rktl's number factor
  `(expt 0.6 ...)` changed to `0.5` in the port, `--seeds 1` reported differing runs from
  the first problem on. Restored afterwards, 36 of 36 identical again.

Per problem line (justify runs list the answer string; keep-going runs list every answer
found before the cap):

| Problem | Runs | Codelets | Ended by | Most frequent answers (count) |
| --- | ---: | ---: | --- | --- |
| `a b z` | 20 | 20000 | cap 20 | b (15), z (14), y (11) |
| `aabc aabd ijkk hjkk` | 20 | 102567 | cap 4, suspend 16 | hjkk (16) |
| `aabc aabd ijkk ijll` | 20 | 40552 | suspend 20 | ijll (20) |
| `abc aabbcc kkjjii` | 20 | 30000 | cap 20 | kkjjii (3), kkkjjjiii (2), kji (2) |
| `abc aabbdd ijkl` | 20 | 65291 | suspend 20 | iijjkkmm (18), iijkmm (2) |
| `abc abd glz` | 20 | 36000 | cap 20 | gld (6), hlz (5), glz (5), flz (3) |
| `abc abd iijjkk` | 20 | 16019 | suspend 20 | iijjll (12), iijjkl (7), iijjdd (1) |
| `abc abd ijk abd` | 20 | 93432 | cap 1, suspend 19 | abd (12) |
| `abc abd ijk` | 20 | 11249 | suspend 20 | ijl (19), ijk (1) |
| `abc abd kji` | 20 | 14298 | suspend 20 | kjj (15), kjh (5) |
| `abc abd kkjjii` | 20 | 30746 | cap 1, suspend 19 | kkjjij (15), kkjjii (2), kkjjjj (2) |
| `abc abd mrrjjj mrrjjjj` | 20 | 100217 | cap 1, suspend 19 | mrrjjjj (19) |
| `abc abd mrrjjj mrrkkk` | 20 | 37736 | suspend 20 | mrrkkk (20) |
| `abc abd mrrjjj` | 20 | 20601 | suspend 20 | mrrkkk (11), mrrjjk (6), mrrjjj (2), mrrjdd (1) |
| `abc abd xyz dyz` | 20 | 87574 | cap 1, suspend 19 | dyz (15) |
| `abc abd xyz wyz` | 20 | 26366 | suspend 20 | wyz (20) |
| `abc abd xyz xyd` | 20 | 18619 | suspend 20 | xyd (20) |
| `abc abd xyz` | 20 | 37782 | suspend 20 | xyd (6), wyz (6), xyz (5), abd (1) |
| `abc cba mrrjjj mmmrrj` | 20 | 166379 | cap 12, suspend 8 | mmmrrj (8) |
| `abc ccbbaa ijk` | 20 | 52882 | suspend 20 | kkjjii (16), ijk (3), kkjii (1) |
| `apc abc opc` | 20 | 19622 | suspend 20 | obc (19) |
| `eeqee qeeq xxixx` | 20 | 65980 | suspend 20 | qeeq (15), ixxi (4) |
| `eqe qeq abbba aaabaaa` | 20 | 264599 | cap 10, halt 1, suspend 9 | aaabaaa (8) |
| `eqe qeq abbba baaab` | 20 | 61530 | suspend 20 | baaab (20) |
| `eqe qeq abbba` | 20 | 24002 | suspend 20 | baaab (15), qeeeq (5) |
| `eqe qeq abbbc aaabccc` | 20 | 158548 | cap 6, suspend 14 | aaabccc (13) |
| `eqe qeq abbbc` | 20 | 48460 | suspend 20 | qeeeq (9), pqqeq (1), cbbba (1), qeq (1) |
| `eqe qeq bxxxb bbbxbbb` | 20 | 132340 | cap 2, suspend 18 | bbbxbbb (13) |
| `eqe qeq bxxxb xbbbx` | 20 | 47919 | suspend 20 | xbbbx (18) |
| `rst rsu xyz uyz` | 20 | 82856 | cap 1, suspend 19 | uyz (12) |
| `rst rsu xyz wyz` | 20 | 26325 | suspend 20 | wyz (20) |
| `rst rsu xyz xyu` | 20 | 20554 | suspend 20 | xyu (20) |
| `rst rsu xyz` | 20 | 42781 | suspend 20 | xyu (10), wyz (5), xyz (3), uyz (1) |
| `xqc xqd mrrjjj mrrjjjj` | 20 | 109656 | cap 2, suspend 18 | mrrjjjj (18) |
| `xqc xqd mrrjjj mrrkkk` | 20 | 25066 | suspend 20 | mrrkkk (20) |
| `xqc xqd mrrjjj` | 20 | 13177 | suspend 20 | mrrkkk (12), mrrjjk (4), mrrjkk (2), mrrjjj (1) |
