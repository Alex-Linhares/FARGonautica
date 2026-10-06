# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repository is

FARGonautica is an archive of Douglas Hofstadter's Fluid Analogies Research Group (FARG) architectures. It holds source code (`Software/`), literature (`Literature/`, mostly PDFs: theses, papers, CRCC reports) and material for a course on Fluid Concepts. It has three goals: collect every open-source FARG architecture together with instructions for running it, collect the literature, and make it possible for undergraduates to *see the principles in action* and for graduate students to experiment with the architectures. `README.md` is the public catalog. `repo-version.md` and `contributing.md` set out the roadmap: version 1.0.0 needs complete, running source code for every major project, plus exercises and challenges.

Two projects are actively maintained, with modern ports and test suites: **Numbo** and **Metacat** (see below). Everything else in `Software/` is archived as found: zips, the original sources, IDE build artifacts and `__MACOSX` folders. Don't clean up or reformat archived code unless asked.

## Projects and implementations

Every known implementation, in this repo or elsewhere, with its paradigm. When the README and these tables disagree, update both.

### Copycat (Mitchell & Hofstadter): letter-string analogies (`abc → abd; ijk → ?`). Project page: `copycat.md`

| Implementation | Language / paradigm | Where |
|---|---|---|
| Melanie Mitchell, original | Common Lisp (Lucid) + Flavors (`defflavor`) | [`Software/Copycat/Melanie-Mitchell-[LISP]/`](Software/Copycat/Melanie-Mitchell-[LISP]/) (`.l` files and `ccat.tar.gz`); [Mitchell's page](http://web.cecs.pdx.edu/~mm/how-to-get-copycat.html) |
| Scott Burson & Mitchell, revived | Common Lisp | https://github.com/slburson/copycat |
| Scott Boland | Java, OOP | [`Software/Copycat/Scott-Boland-implementation in JAVA/JavaCopycat.zip`](Software/Copycat/Scott-Boland-implementation%20in%20JAVA/JavaCopycat.zip); [archive.org](https://archive.org/details/JavaCopycat) |
| "speakeasy", fork of Boland | Java, OOP | https://github.com/speakeasy/CopyCat |
| Arthur O'Dwyer, J Alan Brogan et al. | Python 2, OOP (no graphics) | https://github.com/Quuxplusone/co.py.cat |
| Lucas Saldyt (the recommended version) | Python 3, OOP | https://github.com/fargonauts/copycat (and https://github.com/LSaldyt/copycat) |
| Joseph Aaron Hager | Python 3, OOP, OpenGL graphics | https://github.com/ajhager/copycat |
| Paul Geiger | JavaScript, OOP, runs in a browser | https://github.com/Paul-G2/copycat-js ([demo](https://paul-g2.github.io/copycat-js/)) |
| Greg Detre | Clojure, functional (untested) | https://github.com/gregdetre/copycat-clojure |

### Metacat (Marshall & Hofstadter): Copycat plus self-watching (Temporal Trace, Episodic Memory, justification)

| Implementation | Language / paradigm | Where |
|---|---|---|
| Classic Metacat 1.0 (2003) | Chez Scheme + SWL GUI, closures with message dispatch | [`Software/Metacat/Metacat/`](Software/Metacat/Metacat/), [`Metacat-1.0.zip`](Software/Metacat/Metacat-1.0.zip) |
| Metacat 1.2 (2020), the reference | Chez Scheme (read-only) | [`Software/Metacat/chez_scheme/original/`](Software/Metacat/chez_scheme/original/); headless oracle in [`chez_scheme/oracle/`](Software/Metacat/chez_scheme/oracle/) |
| **Python port** | Python 3, OOP; tkinter GUI and one-window Qt GUI | [`Software/Metacat/python/`](Software/Metacat/python/) |
| **Racket port** | Racket, functional/OO; `racket/gui` | [`Software/Metacat/racket/`](Software/Metacat/racket/) |
| Full repo with test data | — | https://github.com/fargonauts/metacat; Marshall's page: https://science.slc.edu/~jmarshall/metacat/ |

### Numbo (Defays, 1987): number puzzles ("reach 114 from 11 20 7 1 6")

| Implementation | Language / paradigm | Where |
|---|---|---|
| Original, digitized from the printout | Franz Lisp + Flavors (read-only) | [`Software/Numbo/numbo-digitized/`](Software/Numbo/numbo-digitized/), scan in [`numbo.Daniel.Defays.1987.pdf`](Software/Numbo/numbo.Daniel.Defays.1987.pdf) |
| SBCL port (the oracle for the Python implementation) | Common Lisp with Franz/Flavors compatibility layers | [`Software/Numbo/lisp/`](Software/Numbo/lisp/) |
| **Python implementation** | Python 3, OOP; PySide6 GUI | [`Software/Numbo/python/`](Software/Numbo/python/) |
| Early sketch | Python | [`Software/Numbo/bricks.py`](Software/Numbo/bricks.py) |
| Tom Hume | Clojure, functional (untested) | https://github.com/twhume/numbo |

### Musicat (Nichols & Hofstadter): real-time melody perception

| Implementation | Language / paradigm | Where |
|---|---|---|
| Eric Nichols (RhythmCat/Musicat) | C#, OOP (.NET / Visual Studio solution, with an MSTest project) | [`Software/Musicat/RhythmCat/`](Software/Musicat/RhythmCat/) (`RhythmCat/`, `RhythmCatTests/`, `GroundTruthParser/`, `Installer/`), [`musicat.zip`](Software/Musicat/musicat.zip); http://ericpnichols.com/musicat/ |

### Capyblanca (Linhares & Hofstadter): chess perception and decision-making with active symbols. Project page: `capyblanca.md`

| Implementation | Language / paradigm | Where |
|---|---|---|
| Alex Linhares | Delphi / Object Pascal, OOP (`.pas` units, `Fluid_Chess_Project.dpr`; an SVN checkout with compiled `.dcu` files) | [`Software/Capyblanca/capyblanca/trunk/`](Software/Capyblanca/capyblanca/trunk/), [source archive zip](Software/Capyblanca/capyblanca-source-archive-from-code.google.com.zip) |
| Planned Python 3 port | Python 3 | https://github.com/Alex-Linhares/Capyblanca |

### Letter Spirit (McGraw, then Rehling, with Hofstadter): recognizing and designing letters in a consistent style

| Implementation | Language / paradigm | Where |
|---|---|---|
| Rehling's version (Examiner, Adjudicator, Drafter) | Scheme (`.ss`) | [`Software/Letter-Spirit/lspirit/`](Software/Letter-Spirit/lspirit/) (read `README` and `BIGPICTURE`; the loadable code is in `clean-export/`, `ls-load.ss` looks like the loader), [`lspirit.zip`](Software/Letter-Spirit/lspirit.zip) |
| Paul Geiger, McGraw's Examiner | JavaScript, OOP, runs in a browser | https://github.com/Paul-G2/letter-spirit-examiner-js ([demo](https://paul-g2.github.io/letter-spirit-examiner-js/)) |
| Paul Geiger, Letter Spirit II (Rehling) | JavaScript, OOP, runs in a browser | https://github.com/Paul-G2/letter-spirit-2-js ([demo](https://paul-g2.github.io/letter-spirit-2-js/index.html)) |

### Seqsee (Mahabal & Hofstadter): extending integer sequences. Not stored here, maintained by its author

| Implementation | Language / paradigm | Where |
|---|---|---|
| Original | Perl, OOP | https://github.com/amahabal/Seqsee |
| PySeqsee, a rewrite and general Fluid Concepts framework | Python 3, OOP | https://github.com/amahabal/PySeqsee (the README's link to it is malformed). Install notes: `PySeqSee-install-log.md`, `pySeqsee-log.md` |

### Projects with no code available

- **Seek-Whence** (Meredith): Franz Lisp, "coming soon".
- **Tabletop** (French): code not obtained.
- **Phaeaco** (Foundalis): C++, closed source; the thesis is in `Literature/`.
- **George** (Lara-Dammer): Java, never released.

## Working on the maintained ports (Numbo, Metacat)

Every folder of these ports has a README.md. Read it before changing code there.

### The core rule: faithful ports, checked against an oracle

Both projects follow the same design. Keep it intact:

1. **The original source is read-only.** `Software/Numbo/numbo-digitized/` and `Software/Metacat/chez_scheme/original/` must never be edited. Tests check this (`git diff --quiet` for Numbo, a git tree-hash gate for Metacat).
2. **The original (or a faithful port of it) runs headless as an oracle.** For Numbo, the oracle is the SBCL port in `lisp/` with oracle mode on. For Metacat, it is the unmodified 1.2 loaded into Chez Scheme 10 through a prelude in `chez_scheme/oracle/`.
3. **The ports reproduce the oracle event for event** for a given seed: the same codelets in the same order, the same RNG draws, the same printed text, and the same JSON-lines trace. This includes reproducing Chez's RNG, evaluation order, `sort` and number printing, and the original programs' bugs and quirks. Don't "fix" an original bug in a port. Known quirks are documented and marked in the code:
   - Numbo: `# 1987:` comments citing `lisp/src/PORTING_NOTES.md`.
   - Metacat: `docs/anomalies_and_quirks.md` and `docs/divergences.md`.
4. **Structure mirrors the original.** There is one module per original file and one function per original function, with the same names (`look-for-new-block` → `look_for_new_block`). Docstrings name their origin. Marshall's GPL copyright headers stay on every translated Metacat file.
5. **The GUIs only observe.** The engines don't depend on the GUIs, and runs must still match the oracle with every view attached.

## Commands

### Numbo (run from `Software/Numbo/`)

```sh
./run-tests.sh                                   # all tests: lisp/tests/run-tests.sh, then pytest python/tests
bash lisp/tests/run-tests.sh                     # Lisp port only
cd python && python3 -m pytest tests/test_coderack.py -q   # a single Python test file
cd python && python3 -m numbo 114 11 20 7 1 6 --seed 1     # CLI run (stdlib only)
cd python && python3 -m numbo.gui                # GUI (pip install -e 'python[gui]')
cd python && python3 -m numbo.gui --smoke --puzzle 1 --seed 1   # offscreen GUI smoke run
cd lisp && sbcl --non-interactive --load src/load.lisp \
  --eval "(print (numbo::run-config '(114 11 20 7 1 6) :seed 1 :max-iterations 20000))"
```

The Python tests use SBCL as their oracle. Tests that need `sbcl` are skipped when it isn't on `PATH`.

### Metacat (run from `Software/Metacat/`)

```sh
bash python/run-tests.sh --fast                  # Python port, skips @slow tests (~10 s); use while iterating
bash python/run-tests.sh                         # Python port, full (~7 min)
bash python/run-tests.sh --qt                    # Qt GUI tests only
bash python/run-tests.sh tests/test_x.py -k name # extra args go to pytest (stops at first failure, -x)
bash tests/run-tests.sh                          # Racket port (raco make + raco test) and the Chez oracle checks (~10 min)
raco test racket/tests/some-test.rkt             # a single Racket test file (run `raco make` first: stale .zo files are a known trap)
racket racket/cli.rkt abc abd xyz --seed 3852097033      # headless run, Racket
cd python && python3 -m metacat abc abd xyz --seed 3852097033   # headless run, Python
racket racket/main.rkt  |  cd python && python3 -m metacat.gui  |  python3 -m metacat.qt
```

You need Chez Scheme 10 (`scheme`/`chezscheme`) to run the oracle and the equivalence tests. GUI tests run headless: Tk and Racket under `xvfb-run` (with `WAYLAND_DISPLAY` unset), Qt with `QT_QPA_PLATFORM=offscreen`. Never open test windows on the real screen.

**Large test data is not in this copy.** The 109 golden traces (`tests/golden/`) and the Python fixtures (`python/fixtures/`) were left out to keep the repo small, so tests that depend on them won't work here. Regenerate them with `scheme --script chez_scheme/oracle/make-golden.ss` and `python3 python/oracle/capture.py --all` (plus `capture_extra_seeds.py` and `capture_sgl_tcl.py`), or use the full repo at github.com/fargonauts/metacat.

## Architecture notes

- In both programs, codelets are chosen stochastically from a **coderack** by urgency. They build structures in a working area (Numbo's *cytoplasm*, Metacat's *workspace*) and change activations in a concept network (Numbo's *Pnet*, Metacat's *Slipnet*). **Temperature** measures how coherent the current structures are. All randomness goes through one seeded RNG, which is what makes runs reproducible.
- Numbo's Python package keeps the 1987 globals in a `World` object, created fresh for each run (`numbo.harness.run_config`). Observers get typed events, and the GUI is built on these events. The SBCL port has compatibility layers for Franz Lisp and Flavors (`lisp/src/franz-compat.lisp`, etc.).
- In the Metacat original, objects are closures that dispatch on messages (`tell`, with `delegate` for inheritance). `docs/code-map.md` describes every original file and how it depends on the others. `docs/trace-format.md` defines the JSON-lines trace that the ports are compared on.
- The Metacat ports were built by "Ralph loops", one Claude Code session per work item, each followed by a regression gate. See `ralph_loops/` and `ralph_loops/ralph_loop_guide.md`. The gates (`python3 ralph_loops/loop0001/gate.py` for Racket, `loop0002/gate.py` for Python) are the most complete checks.
