# The demo runs and their seeds

`chez_scheme/original/demos.ss` lists Marshall's demo problems, each with the
random seed of a run he described: Runs 1–8 of the dissertation's Chapter 5,
the answer-comparison runs of section 5.2.3, the runs behind Figures 5.4–5.11,
and five "other sample runs" (misc1–misc5) whose outcome the file's comments
record. The port has demos.ss verbatim (`racket/engine/demos.rktl`). The
control panel's **Demos** menu loads a demo's problem and seed, as gui.ss did,
and `(demo run7)` does the same from Racket.

## The seed caveat

A seed replays a run only if the generator and the order of every random draw
are unchanged. The port reproduces **Metacat 1.2 under Chez Scheme 10**
draw for draw (the oracle, `tests/golden/`). Marshall chose the seeds around
1999–2003, under the Chez Scheme of the time. Chez's global `random` is still
the same 32-bit LCG (`docs/trace-format.md`), and most documented runs do
come out exactly as written. A few don't. The likely causes are argument-order
changes in the Chez compiler, a code version other than 1.2, or a lost setting
(`docs/anomalies_and_quirks.md`). **Where the oracle and the
dissertation disagree, the port follows the oracle**, because it ports the
program and not the book.

Also, demos.ss says: *to reproduce Runs 1–8 exactly, clear the Episodic
Memory at the beginning of each run* (Memory → Clear Memory). The memory
keeps answers across runs, and a remembered answer changes later runs.

## What replays

The oracle's outcome is the first answer (or give-up) and the codelet at
which it happens. "Checked" says which test pins the documented outcome. Every
demo problem with its seed is also a golden run (`tests/problems.txt`), which
golden-test.rkt checks event for event.

| Demo | Documented (demos.ss comment, or dissertation page) | Oracle and port | Replays? |
| --- | --- | --- | --- |
| run1 `abc abd mrrjjj mrrjjjj` | justifies mrrjjjj "a few time steps" after a bridge built at 2688 (p. 213) | mrrjjjj at 2692 | yes |
| run2 `xqc xqd mrrjjj mrrkkk` | justifies mrrkkk at 1747 (p. 217) | mrrkkk at 1747 | yes, checked |
| run3 `rst rsu xyz uyz` | justifies uyz at 3163 (p. 220) | uyz at 3163 | yes, checked |
| run4 `abc abd xyz dyz` | never finds the rule; a Jootser ends the run at 3228 (pp. 224–226) | no answer, gives up at 3228 | yes, checked |
| run5 `xqc xqd mrrjjj mrrjjjj` | gives up at 4493 with an unjustified answer description for mrrjjjj (p. 229) | mrrjjjj at 4493 | yes, checked |
| run6 `eqe qeq abbbc aaabccc` | clamps at 2874, 3379, 3889; gives up at 6196 (p. 234) | aaabccc at 5976 | **no** |
| run7 `abc abd xyz` | wyz at 2170 (p. 240) | wyz at 2170 | yes, checked |
| run8 `eqe qeq abbbc` | no answer; snags, clamps; a Jootser ends the run at 5933 (p. 244) | qeeeq at 1013 | **no** |
| 5.2.3 runs (abc-xyd … eqe-aaabccc) | answers only, no time steps (pp. 247–255) | the documented answer, except eqe-qeeeq | can't tell; **eqe-qeeeq gives qcccb** |
| fig5.4-top / fig5.5-top `eeqee qeeq xxixx` | Fig. 5.4 shows qeeq; Fig. 5.5 (top) shows ixxq; no time steps (pp. 257–258) | qeeq at 1383 | answer of Fig. 5.4 yes |
| fig5.4-bottom | qeeq, no time step (p. 257) | qeeq at 2899 | answer yes |
| fig5.5-bottom | qeeq, no time step (p. 258) | qxeeq at 2899 | **no** |
| fig5.7 `aabc aabd ijkk ijll` | the figure shows "Codelets run: 1172" (p. 264) | ijll at 1172 | yes, checked |
| fig5.8 `aabc aabd ijkk hjkk` | justifies hjkk at 733 (p. 265) | hjkk at 733 | yes, checked |
| fig5.10 `abc abd xyz` | wyz, no time step (pp. 267–268) | wyz at 1489 | answer yes |
| fig5.11 `abc abd xyz` | yyz; "a continuation of the run shown in Figure 4.12" (pp. 268–269) | xyd at 1558 | **no** (cannot be reproduced by a seed alone) |
| misc1 `abc cba mrrjjj mmmrrj` | justifies mmmrrj, 7794 steps | mmmrrj at 7794 | yes, checked |
| misc2 `abc abd ijk abd` | justifies abd, 1126 steps | abd at 1126 | yes, checked |
| misc3 `abc aabbcc kkjjii` | kji 1240, kkkjjjiii 1470, kkjjii 1485 | kkjjii 1240, kkkjjjiii 1264, kkjjii 1280, kji 1317 | **no** |
| misc4 `a b z` | b, then y at 945 | b at 453, y at 945 | yes, checked |
| misc5 `abc abd glz` | flz, dlz, then hlz at 1721 | flz 1695, dlz 1710, hlz 1721 | yes, checked |
| misc6–misc8 (commented out) | qbbbq/qeeeq/qcccb at 2782; xbbbx 1945; bbbxbbb 5888 | different | **no** |
| misc9 (commented out) `abc abd xyz` | dyz at 2257 | dyz at 2257 | yes, checked |

"Checked" runs are pinned in the port by `racket/tests/demos-test.rkt` (each run
through `racket/cli.rkt`, from a fresh process). misc1–misc9 are also pinned in
the original by `chez_scheme/oracle/tests/demo-replay-check.ss`. Page numbers
are the dissertation's printed pages (`docs/reference/dissertation.pdf`; the
PDF page is the printed page + 16).
