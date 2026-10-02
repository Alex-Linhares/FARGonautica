# lisp/tests/oracle

Fixture capture scripts. Each one loads the SBCL port in **oracle mode** and writes
a JSON fixture into `../python/fixtures/`, which the Python tests replay. So
every expected value in the Python tests comes from the Lisp.

Regenerate them all (they take a few seconds):

```sh
python/scripts/regen_fixtures.sh          # from Software/Numbo/
```

The script runs every `*.lisp` here except `common.lisp`, in name order.
`python/tests/test_fixtures_current.py` checks that the committed fixtures
are up to date and that regeneration is deterministic.

| Script | Writes | What it captures |
|---|---|---|
| `common.lisp` | — | Shared prologue: loads the port in oracle mode; `write-fixture`. |
| `rng-vectors.lisp` | `rng_vectors.json` | The shared splitmix64 RNG's outputs and `(random n)` draws. |
| `franz-cases.lisp` | `franz_cases.json` | The Franz built-ins on 1,090 cases (division, `*mod`, print names, ...). |
| `pnet.lisp` | `pnet.json` | The 88 pnodes and the 91 holder variables, every slot; the parameters. |
| `pnet-functions.lisp` | `pnet_functions.json` | Pnet states before and after spreading activation, and every pnode method. |
| `coderack.lisp` | `coderack.json` | Scripted coderack operations, and every coderack call of 3 real runs. |
| `cyto-def.lisp` | `cyto_def.json` | Cytoplasm states, and every cytoplasm and cyto-node method on them. |
| `codelets-a.lisp` | `codelets_a.json` | Node creation, linking, activation: whole-world states before and after each call. |
| `codelets-b.lisp` | `codelets_b.json` | Block search codelets and their helpers. |
| `codelets-c.lisp` | `codelets_c.json` | Decomposition, targets, temperature, including the kill-block-gap run. |
| `main-loop.lisp` | `main_loop.json` | 7 full runs, each in a fresh SBCL process: outcome, printed text, trace. |
| `solution-checker.lisp` | `solution_checker.json` | `check-solution` on 126 texts. |
| [`lib/`](lib/) | — | Helpers loaded by the scripts and tests, not capture scripts. |
