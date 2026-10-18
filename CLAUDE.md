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
| Scott Burson & Mitchell: Mitchell's original, revived in modern Common Lisp (runs) | Common Lisp | https://github.com/slburson/copycat |
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
| Same code, built on Linux (2026) | Mono 6.12 + libgdiplus from source; GUI and headless (`noGUI png=...`) | [`Software/Musicat/linux/`](Software/Musicat/linux/RUNNING-ON-LINUX.md): `build-linux.sh` (builds into `~/dev/musicat-build`, archive untouched), `run-musicat.sh`, `linux-paths.patch` |

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

### Seqsee (Mahabal & Hofstadter): extending integer sequences

| Implementation | Language / paradigm | Where |
|---|---|---|
| Original | Perl, OOP | https://github.com/amahabal/Seqsee |
| PySeqsee, a rewrite and general Fluid Concepts framework | Python 3, OOP | https://github.com/amahabal/PySeqsee (the README's link to it is malformed). Install notes: `PySeqSee-install-log.md`, `pySeqsee-log.md` |
| Perl original, archived here | Perl, OOP (Perl/Tk GUI) | [`Software/Seqsee/`](Software/Seqsee/) (`Seqsee.pl`, `lib/`, `config/`, `t/`), the source of the port below |
| **Python port** (2026) | Python 3.12, OOP, standard library; optional PySide6 GUI | [`Software/Seqsee/python/`](Software/Seqsee/python/): a faithful module-by-module port (`PORTING_MAP.md`), Perl's quirks kept and marked `# PERL-QUIRK:`, a drand48 RNG port, tests against golden data recorded from the real Perl (`oracle/`, `tests/golden/`), the GUI checked against Perl/Tk canvas dumps. Not event-identical: whole runs diverge from Perl because of hash order. Artistic-2.0. Built by Ralph loops in `Software/Seqsee/ralph_loops/`; history in Gitea `RootAI/SeqSee2python`. |

### Seek-Whence (Meredith & Hofstadter, 1986): extending integer sequences one term at a time

| Implementation | Language / paradigm | Where |
|---|---|---|
| Original | Franz Lisp (about 5,400 lines; lost, no code survives) | thesis: `Literature/Ph.D. Theses/Meredith-Seek.whence.pdf`; notes: `Literature/Meredith-Historical-Notes-on-Seek-Whence.pdf` |
| **Python reconstruction** (2026) | Python 3, OOP, standard library only | [`Software/SeekWhence/python/`](Software/SeekWhence/python/): a reconstruction from the specification (the thesis and FCCA ch. 1), not a port. `SPEC.md` (the specification with page citations), `DECISIONS.md` (every place the sources are silent), `RESULTS.md` (the three appendix runs and the Blackburn dozen against Meredith's reports). Built by `ralph_loops/loop0003/`. |

### Tabletop (French & Hofstadter, 1992): analogy-making in a microworld of table settings ("Do this!")

| Implementation | Language / paradigm | Where |
|---|---|---|
| Original | Lisp (over 26,000 lines; lost, no code survives) | the book *The Subtlety of Sameness* (MIT Press, 1995); FCCA chapters 8 and 9 |
| **Python reconstruction** (2026) | Python 3, OOP, standard library; optional Cython build of the Slipnet; PySide6 GUI | [`Software/Tabletop/`](Software/Tabletop/) (also on its own at https://github.com/fargonauts/Tabletop): a reconstruction from the specification with `SPEC.md`, `DECISIONS.md` and `RESULTS.md` (every result set French reports beside ours), 53 digitized tables, twelve benches, a CLI and a GUI (`python/docs/gui-window.png`). Built by `ralph_loops/loop0005` to `loop0007`. |

### Projects with no code available
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

### Seqsee (run from `Software/Seqsee/python/`)

```sh
PYTHONPATH=src QT_QPA_PLATFORM=offscreen python3 -m pytest -q     # all tests (the GUI tests need PySide6 and pytest-qt; offscreen or their timing tests fail)
PYTHONPATH=src python3 -m seqsee --seq "1 1 2 1 2 3 1 2 3 4" --seed 3 --max-steps 3000 --continuation "1 2 3 4 5"
PYTHONPATH=src python3 -m seqsee --gui --seq "1 1 2 1 2 3" --seed 7  # the Qt GUI
python3 oracle/regen.py <name>                                       # regenerate a golden file from the real Perl (needs Perl and its modules)
```

### Seek-Whence (run from `Software/SeekWhence/python/`)

```sh
python3 -m pytest -q --ignore=tests/gui              # the engine's tests (standard library only; about 30 s)
bash run-tests.sh                                      # engine and GUI, in parallel with pytest-xdist (~45 s; ~4 min without); --all adds the 4 slow tests
python3 -m seekwhence --seed 4                         # interactive: enter terms, answer its guesses ok/no
python3 -m seekwhence --seed 4 --terms 3 7 3 7 --answers ok   # scripted run, printing Meredith-style trace lines
python3 -m seekwhence.bench appendix                   # the thesis's three appendix runs over 100 seeds
python3 -m seekwhence.bench dozen                      # the Blackburn dozen and related sequences (about five minutes)
```

It is a reconstruction from the specification: every module cites the thesis pages it implements, and every choice the sources leave open is a numbered decision in `DECISIONS.md` with a knob in `seekwhence/config.py`. There is no original program to compare against event for event, unlike Numbo and Metacat; `RESULTS.md` compares behaviour with Meredith's transcripts and reported results instead.

## Architecture notes

- In both programs, codelets are chosen stochastically from a **coderack** by urgency. They build structures in a working area (Numbo's *cytoplasm*, Metacat's *workspace*) and change activations in a concept network (Numbo's *Pnet*, Metacat's *Slipnet*). **Temperature** measures how coherent the current structures are. All randomness goes through one seeded RNG, which is what makes runs reproducible.
- Numbo's Python package keeps the 1987 globals in a `World` object, created fresh for each run (`numbo.harness.run_config`). Observers get typed events, and the GUI is built on these events. The SBCL port has compatibility layers for Franz Lisp and Flavors (`lisp/src/franz-compat.lisp`, etc.).
- In the Metacat original, objects are closures that dispatch on messages (`tell`, with `delegate` for inheritance). `docs/code-map.md` describes every original file and how it depends on the others. `docs/trace-format.md` defines the JSON-lines trace that the ports are compared on.
- Ralph loops (one fresh Claude Code session per work item, each followed by a regression gate) live in the top-level `ralph_loops/`. Start one by copying `ralph_loops/loop_template/` (see `ralph_loops/README.md`).
