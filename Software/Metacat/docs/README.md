# docs/

This folder holds everything written about the two ports of Metacat 1.2 apart from the code.
That covers a map of the original Chez Scheme source, the decisions behind the Racket port
(`racket/`) and the Python port (`python/`), and where and why either port differs from the
original. It also has the trace format both ports must reproduce, logs of bugs and oddities,
timing tables, a list of follow-up work, Marshall's dissertation with its figures, and
screenshots. Most of these files were written and kept up to date by the Ralph loops that
built the ports ([`../ralph_loops/`](../ralph_loops/README.md)). Item numbers such as
"item 13" in them refer to the work items of [loop0001](../ralph_loops/loop0001/README.md)
(Racket) or [loop0002](../ralph_loops/loop0002/README.md) (Python).

## Suggested reading order for newcomers

1. The top-level [`README.md`](../README.md): what Metacat is, how to run it, how the oracle works.
2. [`code-map.md`](code-map.md): the original's 44 files in load order. Read it before
   opening `chez_scheme/original/`.
3. [`trace-format.md`](trace-format.md): Chez's random-number generator and the JSON-lines
   trace that "event for event" equivalence is measured with.
4. [`demos.md`](demos.md): which of Marshall's documented runs replay, and why some don't.
5. [`divergences.md`](divergences.md): the short list of places where the Racket port
   deliberately differs.
6. Depending on what you work on, use [`porting-notes.md`](porting-notes.md) (Racket) or
   [`python-translation-plan.md`](python-translation-plan.md) (Python) as a reference,
   not as a read-through.
7. Read [`anomalies_and_quirks.md`](anomalies_and_quirks.md) for the stories (bugs in the
   original, Chez's evaluation order, `map` order, Wayland vs. Xvfb …), and
   [`follow-ups.md`](follow-ups.md) to see what could come next.

## Index

### The original and how it is reproduced

| Document | Lines | What it is | Who should read it |
| --- | ---: | --- | --- |
| [`code-map.md`](code-map.md) | 497 | One paragraph per file of `chez_scheme/original/` in `metacat.ss`'s load order: what it defines, which files it depends on (worked out by matching identifiers), its line count and whether it calls the SWL toolkit. Starts with general facts for porting: the `tell`/`record-case` objects, every random-number entry point, `sort`, no hash tables, the exact `round`. Written by loop0001 item 00. | Anyone reading or porting the original. |
| [`trace-format.md`](trace-format.md) | 229 | Two specifications. **Randomness plan** (item 01): Chez 10's global `random`/`random-seed` is a 32-bit linear congruential generator, specified exactly so a port can reproduce it bit for bit, plus the evaluation-order caveat. **Trace format** (item 02): the JSON-lines events (codelet, build/break, temperature, slipnet, themes, event, answer, comment, halt, …), field order, number printing, and the golden set in `tests/problems.txt`. | Anyone who touches the engine, the oracle, or the goldens. |
| [`demos.md`](demos.md) | 65 | The demo runs of `demos.ss` (Runs 1–8 of Chapter 5, the figures, misc1–misc9) with their seeds, and the **seed caveat**: most replay as documented under Chez 10, a few don't. Where the oracle and the dissertation disagree, the ports follow the oracle. | Users of the Demos menu; anyone comparing with the dissertation. |
| [`extra-seeds.md`](extra-seeds.md) | 84 | Loop0001's final audit: every problem line run with 20 non-golden seeds (720 runs) in both the oracle and the Racket port, all identical. Explains `tests/extra-seeds.py` and has a per-problem table of answers. | Anyone who wants evidence beyond the 109 goldens. |

### The Racket port

| Document | Lines | What it is | Who should read it |
| --- | ---: | --- | --- |
| [`porting-notes.md`](porting-notes.md) | 1665 | The per-item log of every rename, restructuring and Chez-ism the Racket port had to deal with: the headless oracle, traces, `compat.rkt`, the one-module engine that `include`s the `.rktl` files, each group of model files, the SGL interpreter on racket/draw, the panels, the control panel, demos and packaging. Ends with the final audit (item 17) and a list of earlier statements that are now out of date. | Racket port maintainers. Use the section headings as an index. |
| [`divergences.md`](divergences.md) | 104 | Where the Racket port *deliberately* behaves differently from the original, and how the tests account for it: drawing on racket/draw instead of Tk, offscreen window hosts, the control panel's widgets on racket/gui, and the extra entry points (CLI, standalone program). If something isn't listed here, the original is right. | Anyone who sees the Racket port differ from the original. |
| [`run-times.md`](run-times.md) | 63 | Wall-clock time of every golden run, oracle against Racket port (item 11, 2026-10-03). Per codelet, the port takes about twice as long as Chez, but it starts faster (0.14 s against 0.71 s). | Anyone interested in performance. |

### The Python port

| Document | Lines | What it is | Who should read it |
| --- | ---: | --- | --- |
| [`python-translation-plan.md`](python-translation-plan.md) | 1427 | Loop0002 item 01's plan, kept up to date afterwards: every Chez behaviour the engine depends on and how Python reproduces it (numbers, evaluation order, `map`, `sort`, truthiness, printing, continuations). Also the object-representation benchmark and the choice of "C3", the name mapping, the module layout and the risks ranked. It then has an "As built" section for each item, and the final audit lists all 242 `# chez:`/`# 1.2:` quirk sites in the code. | Python port maintainers; anyone translating Scheme to Python. |
| [`python-run-times.md`](python-run-times.md) | 107 | Python port against the oracle, per problem (item 11): about 9× slower than Chez per codelet, but faster to start. Then item 12's seven speed-ups, each measured, which leave every trace unchanged. | Anyone interested in performance. |

### Logs that both ports share

| Document | Lines | What it is | Who should read it |
| --- | ---: | --- | --- |
| [`anomalies_and_quirks.md`](anomalies_and_quirks.md) | 1204 | A log of 82 surprises, each with a fixed entry format (Seen / What / Evidence / Status). They are grouped as bugs in the original (e.g. `report-error-and-halt` in `answer-justifier`, `caddr` of `#f` in `transcribe-to-english`), anomalies, Chez/Racket/Python quirks (argument evaluation order, `map` order, printer rounding, GTK ignoring Xvfb under Wayland), hidden couplings between model and graphics, and UFO sightings. | Everyone, eventually. The best place to learn why "faithful" was hard. |
| [`follow-ups.md`](follow-ups.md) | 180 | What the two loops deliberately left undone ("faithful first, idiomatic later"): safety nets to keep, idiomatic clean-up, performance and new features. It has a Racket section (loop0001 audit, 2026-10-03) and a "Python (loop0002)" section (2026-10-04). | Whoever plans the next loop. |

### Essays

| Document | Lines | What it is |
| --- | ---: | --- |
| [`robotone_numbo_metacat_similarities.md`](robotone_numbo_metacat_similarities.md) | 76 | A short comparison of what three programs have in common: Robotone (Ganesalingam & Gowers' human-style theorem prover), Numbo (Defays, 1987) and Metacat. It covers only the similarities; the differences are left out on purpose. |
| [`robotone_numbo_metacat_family_resemblance.md`](robotone_numbo_metacat_family_resemblance.md) | 134 | A philosophical companion piece. It uses Wittgenstein's "family resemblance" to argue that the three converge on shared commitments (the process is the result, relevance, cognition as recognition, introspection as evidence) without descending from one another. |

Neither essay is needed to work on the code. They are background for readers interested in
FARG-style cognitive models.

### Folders

| Folder | What it holds |
| --- | --- |
| [`reference/`](reference/README.md) | Marshall's dissertation (`dissertation.pdf`, 306 pages) and 197 figures extracted from it, indexed by the panel they show. The ports' drawings were compared against them. |
| [`screenshots/`](screenshots/README.md) | A gallery of both ports: full screens of the Racket and Python GUIs on several problems, Workspace crops, the Python port's individual panels, and the SGL drawing-language test fixture drawn by each port. |

## Conventions

- Every behaviour that the Racket port had to adapt is noted in `porting-notes.md`, and
  every deliberate difference in `divergences.md` (see [`../CLAUDE.md`](../CLAUDE.md)).
  `divergences.md` has entries for the Racket port only. The Python port's design decisions
  and quirk sites are in `python-translation-plan.md`.
- Anything surprising goes in `anomalies_and_quirks.md` in its entry format, even if it
  turns out to be nothing. When the cause is found, the entry's status is updated.
- The per-item logs (`porting-notes.md`, the "As built" sections of the plan) are left as
  they were written. When a statement is later found to be out of date, the final audits
  note it in a separate section or mark the correction in place, so read those sections too.
