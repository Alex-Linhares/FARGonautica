# Citation audit (item 24)

The reconstruction cites a page for every claim about Seek-Whence. Two checks guard the
citations by machine: `tools/check_quotes.py` (run by `tests/test_quotes.py`) finds every
quotation of six or more words on the page it cites, and `tools/audit_transcript_citations.py`
(this item) checks every citation of the appendix transcripts, pp. 164-179, against the
line-for-line transcriptions in `tests/data/`. Neither can check a *claim* that is not a
quotation, so this audit also re-verified a random sample of citations by eye on the page
images, and reviewed by hand every transcript citation the tool could not confirm.

Date: 2026-10-12. Everything below was done on the package as items 00-23 left it; the fixes
are listed in the sections that found them.

## 1. A random sample of 36 citations, checked on the page images

Method: every tag in `SPEC.md`, `DECISIONS.md` and the docstrings of `seekwhence/*.py` that
cites a page outside the transcripts was collected (690, 219 and 935 tags), and 12 were drawn
from each pool with `random.Random(20261012).sample`. For each, the cited page was rendered
with `pdftoppm -r 110` (thesis PDF page = printed + 12; FCCA PDF = printed + 10) and the
claim read against the page.

| Where | Tag | The claim, in brief | On the page |
|---|---|---|---|
| SPEC.md:82 | [M86 p. 7] | the guess shows "a synopsis of its model", not only the next template | yes |
| SPEC.md:295 | [M86 p. 43] | the three levels; the cytoplasm as the "real world" of input integers and their relations; the socratoplasm as the parse | yes |
| SPEC.md:600 | [FCCA p. 52] | the boundary-shift that cleaves between adjacent 2's, giving `(2 1 2) (2 2 2) ...` and the template `[2 n 2]` | yes |
| SPEC.md:1219 | [M86 p. 65] | glomming only when the bonds are "sufficiently strong" to view the chain as a unit | yes |
| SPEC.md:1279 | [M86 p. 67] | the Glomtester rejects or creates a Glommer "to refine the group" | yes |
| SPEC.md:1992 | [M86 p. 99] | reconsideration as a "homing in" from a tightening spiral | yes |
| SPEC.md:2213 | [M86 p. 101] | the stability definition: holding = internal pulls + external pushes, tearing = external pulls + internal pushes | yes |
| SPEC.md:12 | [M86 pp. 101-103] | the tag format's example (Figure 1 and its arithmetic are on these pages) | yes |
| SPEC.md:204 | [M86 p. 181] | the glossary's "hit" | yes |
| SPEC.md:260 | [M86 p. 145] | "approximately 5400 lines of Franz Lisp code", a VAX 11/750 | yes |
| SPEC.md:481 | [M86 p. 98] | on an unexpected term the validity goes to 0, sparks are released, a Gnoth-setter is posted | yes |
| SPEC.md:3222 | [M86 pp. 151-152] | low-level myopia "mitigated when divesting pushes, cosmetic reform, and 'form-polishing' are implemented"; high-level haughtiness | yes |
| DECISIONS.md:200 | [M86 p. 68] | "all but the fence-type plato-classes (Tuple and Cycle) possess a 'process'" | yes |
| DECISIONS.md:281 | [M86 pp. 64, 67, 68, 124] | Testers, Bonders, Glomtesters, Plato-scouts and Gnoth-operators each check existence first | yes (pp. 64, 67, 68, 124 each say so) |
| DECISIONS.md:505 | [M86 p. 67] | the 6 of `5 3 1 6 1 3 4` targeted "with 'symmetry' the reason" | yes |
| DECISIONS.md:506 | [M86 pp. 66-67] | the urgency ordering: groupable strongest, sameness above successorship, a sameness group above a predecessor fence | yes |
| DECISIONS.md:704 | [M86 p. 75] | the applier would "fail because of the first glom" of `(1 1) 2 (1 2 3)` | yes |
| DECISIONS.md:981 | [M86 pp. 90-91] | "difficult to slip to a stricter class"; Tuple "a generic grouping mechanism" | yes (p. 90 and the top of p. 91) |
| DECISIONS.md:1206 | [M86 p. 105] | the threshold: 1 more than the existing one or 80% of the strongest pull | yes |
| DECISIONS.md:1220 | [M86 p. 124] | the Bond-assessor posts a Reformulator or a Gnoth-caster | yes |
| DECISIONS.md:1238 | [M86 p. 124] | the Gnoth-caster describes each gnoth in terms of the reigning class | yes |
| DECISIONS.md:1442 | [M86 p. 84] | the shared variant of Figure 3 has `(box3 box5)` on Pstruc3 and no Pstruc5; the Cycle form needs three boxes | yes |
| DECISIONS.md:1507 | [M86 p. 64] | a bond "links the two gloms" | yes |
| DECISIONS.md:1703 | [M86 p. 120] | the series moves `glint6` out of `(3 4 5)` and leaves `[4 5]` | yes |
| boxes.py:293 | [M86 pp. 161, 162] | `show-box` prints `(2 2)` and `(1 8 1)` | yes |
| config.py:79 | [M86 p. 98] | sparks released on refutation | yes |
| forms.py:12 | [M86 p. 53] | the prose spelling `(Cycle (2 1 8))` | yes |
| gloms.py:120 | [M86 p. 66] | the three quicktests in the thesis's order, per bond family | yes |
| operations.py:37 | [M86 pp. 109, 112] | the two categories of operations; CAPTURE-LEFT and CAPTURE-RIGHT | yes |
| platoplasm.py:137 | [M86 pp. 54, 59] | the hierarchy of categories; strictness | yes |
| platoplasm.py:306 | [M86 p. 54] | the generating function's definition with `(C-group 1 3)` | yes |
| platoplasm.py:424 | [M86 p. 85] | a filled box reports its value "until the next time they are hit" | yes |
| reforms.py:336 | [M86 p. 113] | the "slow-recap" permits natural glomming within the gnoth | yes |
| sequences.py:265 | [M86 pp. 145-147] | the dozen's reported results | yes |
| templates.py:9 | [M86 pp. 73-74] | a template is a dubbed glom's form, loosened to `(S-group 1 n)` | yes |
| templates.py:296 | [M86 p. 74] | a template is "rejected and abandoned" | yes |

Result: 36 of 36 correct. One docstring was found wrong on the way, outside the sample: `platoplasm.Platoplasm` attributed p. 91's
"could be, but in practice are not, changed by the system" to the slipperiness values too,
where the page says that of the strengths and, of the slipperiness, that it "can be changed
during processing, although the current system does not do so" [M86 p. 91]. Fixed.

## 2. Every citation of the appendix transcripts

Method: `python3 tools/audit_transcript_citations.py` takes every tag citing a page in
164-179 in `SPEC.md`, `DECISIONS.md`, `README.md`, `RESULTS.md`, `docs/`, `seekwhence/` and
`tests/` (931 tags after the fixes), extracts the backticked and quoted snippets of the
claim before the tag (the paragraph in a `.md` file, the docstring in a `.py` file), and looks
each up, whitespace ignored, in the transcription of the cited pages:

| Verdict | Count | Meaning |
|---|---|---|
| EXACT | 458 | every snippet is printed on a cited page |
| NEAR | 28 | a snippet is printed only on the page before or after |
| ELSEWHERE | 13 | a snippet is printed in a transcript, but not on the cited pages or next to them |
| NOTFOUND | 157 | a snippet is in no transcript |
| NOSNIPPET | 275 | nothing checkable in the context (a claim in words) |

All 203 non-EXACT citations (before the fixes: 32 NEAR, 16 ELSEWHERE, 155 NOTFOUND) were
read by hand. Every one but eight is a false positive of the method, of five kinds:

- the snippet is the reconstruction's own wording, said to be so in the same sentence
  (`Changing to a new reigning class. ...`, `blasting away all gnoths and gloms`, the
  bench's `outcome:` line, `(S-group 1 n)` with the wildcard, `(Y-group 3 3 (3 7 3))`);
- the snippet is a sequence or a form written in prose form (`1 2 1`, `(3 7) (3) (3)`,
  `(1 2) (1 2) (3)`), or a code identifier (`Config.bond_values`, `World.run_now`);
- the claim is one of *absence* (`bond4` absent from the last `show` [M86 p. 178]; no
  `slip-check` printed at the slip from Countup [M86 pp. 173-174]; `gnoth4`, the catchall's
  number, which no transcript prints);
- the tag cites one page for one snippet while an earlier sentence of the same paragraph
  names another page's line (the README's results table, the family words `Same`, `Succ`,
  `Pred`, listed once for all three runs);
- Meredith's annotations, which the transcription keeps as `# note:` lines.

The eight that were wrong, all fixed:

| Where | Was | Now | Why |
|---|---|---|---|
| SPEC.md 5.6 (destroyers) | `glom3 is not in cytoplasm` [M86 p. 170] | p. 172 | the line is on p. 172 |
| SPEC.md 15.5 (2) | the Sparkler-plus lines [M86 pp. 164, 172, 176] | pp. 164-165, 171-173, 176 | `(glom1 glom1 10)` is printed on pp. 165, 171, 173 |
| DECISIONS.md D-015 | the same lines [M86 pp. 164, 170, 172, 176] | pp. 164-165, 170-173, 176 | the same |
| `trace.py` module docstring | the stage-one lines [M86 p. 165] | pp. 164-165 | `spark1 --- between glint1glint2` is on p. 164 |
| `slipping.set_class` | the slip moving every gnoth [M86 p. 174] | pp. 174-175 | the `plato-class: S-group` fields are on p. 175 |
| `casting.cast_terms` | `(S-group 1 1)` for `1` | adds [M86 p. 174] | the cast is printed there |
| `tests/test_cli.py` | the console commands [M86 pp. 167-169] | adds p. 174 | `show-parse` is in run 3 only |
| `platoplasm.Platoplasm` | the p. 91 quotation (above) | two quotations | misattribution |

The transcriptions themselves were made from the page images at 130 dpi (item 04) and are
guarded by `tests/test_data.py` (every milestone of `expectations.json` found in its run, in
order, on its page); this audit did not re-transcribe them. Two of the eight pages involved
in the fixes (172 and 176) were looked at again on the images.

## 3. The other checks of item 24

- **No claim of Meredith's without a page.** Every tag in the four documents and the code
  is of the three forms the conventions allow; `grep` for "Meredith used", "Meredith chose",
  "she used", "her program uses", "Meredith implemented" finds nothing. The 41 mentions of
  "Meredith's" in `README.md`, `RESULTS.md` and `docs/` are "Meredith's printout", "Meredith's
  inputs", "Meredith's reported result": references to the transcripts and to pp. 145-147,
  cited where the result is stated.
- **Every decision referenced, every reference defined.** `tests/test_conventions.py` checks
  that every `D-NNN` in the code is a heading of `DECISIONS.md`; `tests/test_config.py` that
  every `Config` field named in `DECISIONS.md` exists and every field is a knob or cited;
  `tests/test_docs.py` that the index names every entry, every knob and, for every decision
  the code cites, the citing module. The only decision no code cites is D-052 (a recorded
  divergence); the index says so.
- **Modules small and readable.** 27 modules, 9,600 lines in all, the longest `events.py`
  (796 lines of one-dataclass-per-event). Three functions are longer than 60 lines, and all
  three are tables rather than algorithms: `trace.Trace.on_event` (84 lines, one branch per
  event kind giving the printed line, in the order the printouts show them),
  `milestones._needles` (108 lines, one branch per milestone kind of `expectations.json`)
  and `bench.survey` (65 lines, one loop filling eight counters). Each is kept whole and
  says so in its docstring; splitting them would separate each printed line from its event,
  or each counter from the loop that fills it.
- **Standard library only.** `tests/test_package.py` parses every module with `ast` and
  allows only stdlib modules and `seekwhence`; `pyproject.toml` lists no dependency.
- **One RNG.** `import random` occurs in `seekwhence/rng.py` only; every draw is a method of
  the run's `Rng` (`World.rng`), and `tests/test_run.py` counts one draw per task chosen. A
  defect found by the audit: `random.Random(-n)` draws the same
  numbers as `random.Random(n)`, so two seeds could give one run. `Rng` now refuses a negative
  seed and `--seed` is a usage error below 0 (tests added).
- **No sequence-specific code.** With docstrings and comments stripped, the only literal
  sequences in `seekwhence/` are the catalogue of `sequences.py` (the data the bench runs);
  no conditional in the engine tests a term value, a sequence or a seed.
- **The test suite's run time.** 978 tests, 1 skipped, in 27 s (`python3 -m pytest -q`);
  the quotation check is 16 s of it, the appendix replays about 3 s. The gate's 20-minute
  limit is not approached.
- **Other small fixes.** All behaviour-preserving, every test passing: `Platoplasm.__init__`
  checks every name and value of the three tables as it reads them; `pseudo` is a plain
  attribute of every cyto element, so the six `getattr(x, "pseudo", False)` tests became
  `x.pseudo`; and the side effect of `slipping.staying_pressure` and `slip_check` on the
  gnoths' assessed bond lists is stated in their docstrings.
