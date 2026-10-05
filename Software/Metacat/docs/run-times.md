# Run times: the port against the oracle (item 11)

Produced by `racket tests/bench-runs.rkt docs/run-times.md` on 2026-10-03 (32-core Linux
machine, Racket 8.18 CS, Chez Scheme 10.0.0). It runs every run of `tests/problems.txt`
once as a process, one at a time: `racket racket/cli.rkt ...` (compiled `.zo`) and
`scheme --script chez_scheme/oracle/run.ss ...` with the same arguments. Times are
wall-clock seconds per process, summed over a problem's seeds, startup included.

Reading the table:
- The oracle loads the 44 original source files (compiling them) at every start, about
  0.7 s; the port loads compiled code in about 0.14 s. So on short runs the port's
  process finishes first.
- Per codelet, the port is about **2× slower** than Chez (≈200 vs ≈97 ms per 1000
  codelets over all runs, startup subtracted). On the longest runs (34,601 codelets for
  `eqe qeq abbba aaabaaa`) the port's process is 1.35× slower in total.
- The per-1000-codelet columns subtract the startup median from each run; where a run is
  shorter than that estimate's noise, the Chez column shows 0.00.
- No optimisation was attempted: the port is a line-for-line copy (item 11's job is
  equivalence). Clean-ups belong to a later loop.

Startup (load, set up a problem, run 1 codelet), median of 5: Chez 0.71 s, Racket 0.14 s.

| Problem | Runs | Codelets | Chez s | Racket s | Racket / Chez | Chez ms/1000 codelets | Racket ms/1000 codelets |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `abc abd mrrjjj mrrjjjj` | 3 | 8321 | 3.22 | 2.50 | 0.78 | 129.48 | 248.16 |
| `xqc xqd mrrjjj mrrkkk` | 3 | 4537 | 2.58 | 1.35 | 0.52 | 97.31 | 200.93 |
| `rst rsu xyz uyz` | 3 | 10699 | 3.08 | 2.26 | 0.73 | 87.69 | 170.49 |
| `abc abd xyz dyz` | 3 | 10157 | 2.99 | 2.06 | 0.69 | 83.69 | 159.87 |
| `xqc xqd mrrjjj mrrjjjj` | 3 | 18448 | 4.82 | 5.29 | 1.10 | 145.21 | 263.03 |
| `eqe qeq abbbc aaabccc` | 3 | 17939 | 5.02 | 5.66 | 1.13 | 160.51 | 291.35 |
| `abc abd xyz` | 4 | 7474 | 3.31 | 1.54 | 0.46 | 61.53 | 128.11 |
| `eqe qeq abbbc` | 3 | 7578 | 2.58 | 1.34 | 0.52 | 57.49 | 119.50 |
| `abc abd xyz xyd` | 3 | 2867 | 2.40 | 0.95 | 0.39 | 92.50 | 178.15 |
| `abc abd xyz wyz` | 3 | 2873 | 2.38 | 0.97 | 0.41 | 84.12 | 186.84 |
| `rst rsu xyz xyu` | 3 | 2049 | 2.33 | 0.79 | 0.34 | 93.64 | 172.31 |
| `rst rsu xyz wyz` | 3 | 3235 | 2.44 | 1.09 | 0.45 | 92.85 | 202.74 |
| `abc abd mrrjjj mrrkkk` | 3 | 3647 | 2.49 | 1.21 | 0.49 | 97.42 | 212.74 |
| `eqe qeq abbba baaab` | 3 | 13404 | 3.47 | 3.00 | 0.86 | 99.51 | 191.34 |
| `eqe qeq abbba aaabaaa` | 3 | 34601 | 7.37 | 9.93 | 1.35 | 151.29 | 274.36 |
| `eeqee qeeq xxixx` | 3 | 7181 | 2.83 | 1.81 | 0.64 | 96.11 | 190.90 |
| `aabc aabd ijkk ijll` | 3 | 4327 | 2.54 | 1.37 | 0.54 | 92.06 | 215.27 |
| `aabc aabd ijkk hjkk` | 3 | 10273 | 3.69 | 3.48 | 0.94 | 151.05 | 296.03 |
| `abc cba mrrjjj mmmrrj` | 3 | 13341 | 3.82 | 3.63 | 0.95 | 125.73 | 239.57 |
| `abc abd ijk abd` | 3 | 5234 | 2.38 | 1.20 | 0.50 | 45.50 | 146.13 |
| `abc aabbcc kkjjii` | 3 | 4500 | 2.39 | 1.21 | 0.51 | 56.66 | 172.88 |
| `a b z` | 3 | 3000 | 2.08 | 0.59 | 0.28 | 0.00 | 50.89 |
| `abc abd glz` | 3 | 5400 | 2.40 | 1.03 | 0.43 | 47.70 | 111.21 |
| `eqe qeq bxxxb xbbbx` | 3 | 7053 | 2.45 | 1.31 | 0.54 | 44.50 | 124.72 |
| `eqe qeq bxxxb bbbxbbb` | 3 | 14901 | 3.45 | 3.08 | 0.89 | 87.82 | 177.46 |
| `abc abd mrrjjj` | 3 | 3643 | 2.25 | 0.96 | 0.43 | 30.83 | 144.69 |
| `abc abd ijk` | 3 | 1840 | 2.05 | 0.65 | 0.32 | 0.00 | 118.52 |
| `abc abd iijjkk` | 3 | 6471 | 2.47 | 1.31 | 0.53 | 50.92 | 135.29 |
| `abc abd kji` | 3 | 1973 | 2.16 | 0.69 | 0.32 | 11.96 | 131.08 |
| `abc abd kkjjii` | 3 | 4121 | 2.24 | 0.97 | 0.43 | 25.18 | 130.33 |
| `rst rsu xyz` | 3 | 4648 | 2.29 | 0.96 | 0.42 | 31.68 | 114.14 |
| `xqc xqd mrrjjj` | 3 | 1857 | 2.11 | 0.69 | 0.33 | 0.00 | 136.86 |
| `eqe qeq abbba` | 3 | 4893 | 2.30 | 0.91 | 0.39 | 32.68 | 96.57 |
| `apc abc opc` | 3 | 2439 | 2.14 | 0.73 | 0.34 | 0.00 | 121.88 |
| `abc ccbbaa ijk` | 3 | 5959 | 2.58 | 1.35 | 0.52 | 73.65 | 154.11 |
| `abc aabbdd ijkl` | 3 | 11974 | 3.02 | 2.27 | 0.75 | 73.90 | 152.95 |
| **all** | 109 | 272857 | 104.12 | 70.12 | 0.67 | 96.72 | 199.14 |

Outputs identical: 109 of 109 runs.
