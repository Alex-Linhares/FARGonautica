# `python/fixtures/`: frozen Chez outputs

> **Not included in FARGonautica's copy.** Only this README is here. Regenerate the
> fixtures with the commands below (they need Chez Scheme 10), or get them from
> [github.com/fargonauts/metacat](https://github.com/fargonauts/metacat/tree/main/python/fixtures).

This folder holds what the **original Metacat 1.2, running under Chez Scheme 10**, printed
for every differential test, plus the outcomes of 720 whole runs and the Tcl command stream
of its SGL interpreter. The Python tests compare against these files, so every expected
value in [`python/tests/`](../tests/README.md) comes from Chez and not from the Python
code. The files are produced only by the capture scripts in
[`python/oracle/`](../oracle/README.md) and are committed (about 171 MB). Never edit them
by hand, and never re-capture them to make a failing port pass. Don't add files to the
subfolders either: the tests check their contents exactly, down to one file per test.

## Format of a battery folder

Every folder except `extra-seeds/` and `sgl-tcl/` is one battery, written by
`python/oracle/capture.py`:

| File | Content |
|---|---|
| `MANIFEST` | The battery's test names, one per line, in battery order (from Chez's reader) |
| `NNN-NAME.txt` | What Chez printed after `NAME => ` for test number NNN (from `000`), byte for byte, without the record's final newline. A value is in `b:canon` notation (`tests/diff/helpers.scm`), or `ERROR`. Characters of NAME outside `[A-Za-z0-9.+_-]` are written `%XX` (`prob?` → `012-prob%3F.txt`) |
| `SOURCES` | `chez 10.0.0`, then the sha256 and path of every input of the capture: `diff-eval.ss`, `prelude.ss`, `list-tests.ss`, every file of `tests/diff/`, and the battery itself if it is one of the port's own. `chez_scheme/original/` is not listed, because the gate pins it |

Joining `NAME => VALUE\n` for every test gives Chez's output again, byte for byte. In a
test:

```python
from chez_fixtures import chez        # python/tests/chez_fixtures.py
chez("utilities", "prob?")            # the text Chez printed for (test prob? ...)
```

## The folders

The tests column counts the lines in `MANIFEST`.

### From the frozen batteries in [`tests/diff/`](../../tests/diff/)

These batteries were written for the Racket port.

| Folder | Battery | Tests | Size | Checked by |
|---|---|---:|---:|---|
| `utilities/` | `utilities-battery.scm` | 197 | 0.9 MB | `test_utilities.py`, `test_chez.py`, `test_object_prototype.py` |
| `coderack/` | `coderack-battery.scm` | 43 | 1.1 MB | `test_coderack.py` |
| `slipnet/` | `slipnet-battery.scm` | 47 | 1.3 MB | `test_slipnet.py` |
| `workspace/` | `workspace-battery.scm` (loads `workspace-dump.scm`) | 75 | 0.9 MB | `test_workspace.py` |
| `codelet/` | `codelet-battery.scm` (through `codelet-harness.scm`) | 59 | 23 MB | `test_codelets.py` |
| `bridge/` | `bridge-battery.scm` | 46 | 47 MB | `test_bridges.py` |
| `rule/` | `rule-battery.scm` | 50 | 94 MB | `test_rules.py` |
| `sgl/` | `sgl-battery.scm` (after `sgl-chez-setup.ss`) | 48 | 0.2 MB | `test_sgl.py` |
| `graphics/` | `graphics-battery.scm` | 48 | 0.5 MB | `test_graphics.py` |
| `panels/` | `panels-battery.scm` | 36 | 0.2 MB | `test_panels.py`, `test_graphics.py` (the EEG) |

### From the port's own batteries in [`python/oracle/batteries/`](../oracle/README.md#the-ports-own-batteries-batteries)

| Folder | Battery | Tests | Checked by |
|---|---|---:|---|
| `chez/` | `chez-battery.scm` | 63 | `test_chez.py` |
| `utilities-extra/` | `utilities-extra-battery.scm` | 4 | `test_utilities.py` |
| `coderack-extra/` | `coderack-extra-battery.scm` | 2 | `test_coderack.py` |
| `slipnet-extra/` | `slipnet-extra-battery.scm` | 4 | `test_slipnet.py` |
| `workspace-extra/` | `workspace-extra-battery.scm` | 8 | `test_workspace.py` |
| `codelet-extra/` | `codelet-extra-battery.scm` | 13 | `test_codelets.py` |
| `bridge-extra/` | `bridge-extra-battery.scm` | 1 | `test_bridges.py` |
| `rule-extra/` | `rule-extra-battery.scm` | 17 | `test_rules.py` |
| `trace-extra/` | `trace-extra-battery.scm` | 3 | `test_golden.py` |
| `gui/` | `gui-battery.scm` | 8 | `test_gui.py` |

### From the other captures

| Folder | Written by | Content | Checked by |
|---|---|---|---|
| `extra-seeds/` | `python/oracle/capture_extra_seeds.py` | `runs.jsonl`: one JSON object per run for the 720 runs of `tests/extra-seeds.py` (20 non-golden seeds per line of `tests/problems.txt`): `strings`, `seed`, `cap`, `keep`, `exit`, `stdout`, `error` (first line of stderr), `trace_sha256`, `trace_lines`. Plus `SOURCES` | `test_extra_seeds.py` |
| `sgl-tcl/` | `python/oracle/capture_sgl_tcl.py` | `v1.txt`, `v2.txt`: the Tcl commands the original's SGL interpreter sends while drawing `python/oracle/sgl-fixture.scm` on each viewport, one written datum per line, in order. Plus `SOURCES` | `test_sgl.py` |

## Regenerating

You need Chez Scheme 10 (`scheme` or `chezscheme`). Run from the repository root:

```bash
python3 python/oracle/capture.py --all           # every battery folder (about a minute)
python3 python/oracle/capture.py rule            # one battery folder
python3 python/oracle/capture_extra_seeds.py     # extra-seeds/
python3 python/oracle/capture_sgl_tcl.py         # sgl-tcl/
```

A capture replaces its folder. The only legitimate reason to regenerate is a changed
input, which shows up as a changed `SOURCES`. With the same inputs, a capture writes the
same bytes.

## Freshness

The tests check two things: that the fixtures are complete, and that they still match
their inputs.

| Fixture | Fast tier (no Chez run) | Slow tier (runs Chez) |
|---|---|---|
| Battery folders | `test_fixtures.py`: every battery has a folder and no folder lacks a battery; one fixture per test, counted with an independent Scheme scanner (`scheme_forms.py`); the split/join round trip; `SOURCES` equal to the hashes of today's inputs | `test_fixtures.py::test_recapture_is_byte_identical`: every battery re-captured into a temporary directory, byte-identical |
| `extra-seeds/` | `test_extra_seeds.py`: `SOURCES` (including `scheme --version`) and the job list | The oracle's 720 runs re-captured, byte-identical (about 1.5 min) |
| `sgl-tcl/` | `test_sgl.py`: `SOURCES` unchanged (including `scheme --version`) | `test_sgl.py::test_tcl_stream_recapture_is_identical` |

So if someone edits a battery, `tests/diff/`, the oracle's `diff-eval.ss` or `prelude.ss`,
or a capture script, the fast tier fails until the fixtures are re-captured. If a re-capture
gives different bytes, the slow tier fails.
