# Numbo

Daniel Defays' Numbo (1987), a FARG model of how people solve number puzzles
("reach 114 from the bricks 11 20 7 1 6"). The original source is here as it
was digitized, together with two implementations that run today: a faithful
port of the original Lisp, and a Python implementation with a live GUI.

## Layout

| Path | What's there |
|---|---|
| [`numbo-digitized/`](numbo-digitized/) | The 1987 Franz Lisp + Flavors source, as digitized from the printout (read-only; the tests check it is unchanged). |
| [`numbo.Daniel.Defays.1987.pdf`](numbo.Daniel.Defays.1987.pdf) | The scanned 1987 printout. |
| [`lisp/`](lisp/) | The 1987 source ported to **SBCL**, with compatibility layers for Franz Lisp and Flavors, a coderack reconstructed from Defays' chapter, and an opt-in *oracle mode*. See [`lisp/src/README.md`](lisp/src/README.md). |
| [`python/`](python/) | The **Python** implementation: the engine, a CLI and a PySide6 GUI. In oracle mode it matches the SBCL port event for event, on all 11 chapter puzzles × 20 seeds. See [`python/README.md`](python/README.md). |
| [`docs/`](docs/) | The audit and plan the Python translation followed. |
| [`bricks.py`](bricks.py) | An earlier arithmetic-tree sketch. |

Every folder of the ports has a README.md describing what is in it.

## Quick start

```sh
# Lisp (SBCL)
cd lisp
sbcl --non-interactive --load src/load.lisp \
     --eval "(print (numbo::run-config '(114 11 20 7 1 6) :seed 1 :max-iterations 20000))" 2>/dev/null

# Python, CLI (standard library only)
cd python
python3 -m numbo 114 11 20 7 1 6 --seed 1

# Python, GUI (needs PySide6: pip install -e 'python[gui]')
cd python
python3 -m numbo.gui
```

## Tests

```sh
./run-tests.sh        # lisp/tests/run-tests.sh, then the Python tests (pytest)
```

The Python tests use the SBCL port as their oracle, so they need `sbcl` too.

Reference: Daniel Defays, "Numbo: A Study in Cognition and Recognition",
chapter 3 of Douglas Hofstadter and the Fluid Analogies Research Group,
*Fluid Concepts and Creative Analogies* (Basic Books, 1995).
