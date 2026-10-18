# Seek-Whence: the specification, distilled from the sources

This file is the specification the package `seekwhence` implements: Meredith's thesis, read
page by page, with Hofstadter's account of the project for the design intent. Every statement
about Seek-Whence cites a page that has been looked at on the scan. Where the sources are
silent, the choice made is not here but in [`DECISIONS.md`](DECISIONS.md).

## Citation conventions

| Tag | Source | Page given as |
|---|---|---|
| `[M86 p. 101]`, `[M86 pp. 101-103]`, `[M86 p. viii]` | Meredith 1986, the thesis | the **printed** page |
| `[FCCA p. 52]` | Hofstadter & FARG 1995 | the **printed** page |
| `[MHN p. 3]` | Meredith's historical notes, 2017 | the PDF page (no printed numbers) |

Other sources are cited by author, year and page in words (e.g. "Mahabal 2009, p. 12").

Quotations are checked by machine: a quotation of six or more words in double quotes, directly
followed by a tag, is checked by `tools/check_quotes.py` against the text layer of the cited
page (plus or minus one page), with tolerance for the thesis's OCR errors. Quotations are word
for word, with OCR errors silently corrected; editorial insertions go in [square brackets];
"..." marks an omission; no quotation exceeds 80 words (the sources are read in place, never
copied into the repository). Where a true quotation cannot be found by the checker because the
OCR is too garbled, it is given as a paraphrase (no quotation marks) with a note.

## The sources

| Source | Where | Offsets |
|---|---|---|
| Meredith (1986), *Seek-Whence: A Model of Pattern Perception*, IU CS TR 214 | `Literature/Ph.D. Theses/Meredith-Seek.whence.pdf` (201 PDF pages) | body: PDF = printed + 12; front matter: PDF = roman value - 1 |
| Meredith (2017), historical notes on Seek-Whence | `Literature/Meredith-Historical-Notes-on-Seek-Whence.pdf` (7 pages) | PDF page |
| Hofstadter & FARG (1995), *Fluid Concepts and Creative Analogies*, chapter 1 | the author's copy, outside the repository (copyrighted; read in place) | PDF = printed + 10; chapter 1 is printed pp. 13-86 |

The thesis's contents, by printed page: ch. 1 Foundations pp. 1-34; ch. 2 Stage One:
Hypothesis Creation pp. 35-86; ch. 3 Stage Two: Reformulation pp. 87-126; ch. 4 comparisons
pp. 127-143; ch. 5 performance and problems pp. 144-158; appendix: box implementations
pp. 161-163, three program runs pp. 164-179; glossary pp. 180-182; bibliography pp. 183-189.

FCCA chapter 1 is Hofstadter's account of the *project*, not a description of Meredith's
*program*: "I will not go into the Seek-Whence notation in this article" [FCCA p. 56]. It is
used for motivation and design intent, kept apart from what the thesis says the program did.

## The specification

The sections below were written by the specification items (01-04) of the loop and are the
specification the package was built from. Under each section heading an *Implemented in* line
names the modules of `seekwhence/` that implement it, the decisions (DECISIONS.md) that fill
its gaps, and the test files that check it; `tests/test_docs.py` checks that every module and
test file named exists. In short:

| Section | Modules |
|---|---|
| 1 The domain and the task | `session`, `__main__`, `filtering` (the guess), `sequences`, `trace` |
| 2 The architecture | `world`, `taskrack`, `run`, `config`, `events` |
| 3 What FCCA chapter 1 adds | none (design intent behind decisions); the sampler in `sequences` |
| 4 The platoplasm | `forms`, `platoplasm` |
| 5 The cytoplasm | `cytoplasm`, `bonds`, `gloms` |
| 6 Templates | `templates` (and `forms` for the wildcard) |
| 7 The socratoplasm | `gnoths` |
| 8 The hypothesis | `hypothesis`, `boxes`, `gnoths` (the Hypothesizer), `filtering` (the Hfilter) |
| 9 Why and when to reformulate | `filtering` (refutation), `slipping`, `casting` (the blast) |
| 10 The Gnoth-setter | `slipping`, `hypothesis` (freeze-dried forms) |
| 11 Slip or stay | `platoplasm` (the tables), `slipping` (the arithmetic) |
| 12 The Reformulator | `reforms` |
| 13 The gnoth operations and PROGRAMs | `operations`, `reforms` (PROGRAM construction) |
| 14 Carrying out reforms | `operations` (Gnoth-operator, Capper), `reforms` (Bond-assessor), `casting` |
| 15 The appendix runs and the results | `tests/data/`, `milestones`, `bench`, `sequences` |

### 1. The domain and the task

*Implemented in:* `seekwhence/session.py` and `seekwhence/__main__.py` (the user protocol of 1.4: the prompts, `show`, `show-hypothesis`, `show-seq`, `show-parse`, the answers; D-053, D-054, D-069), `seekwhence/filtering.py` (the guess and the answer, D-032, D-033, D-064), `seekwhence/sequences.py` (the Blackburn dozen of 1.2 and the reported results of 1.6 as data, D-070), `seekwhence/trace.py` (the printed lines). Tests: `tests/test_session.py`, `tests/test_cli.py`, `tests/test_dozen.py`.

#### 1.1 What the program does

Seek-Whence is shown the terms of an integer sequence one at a time by a user (the
"presenter") and tries to guess the pattern the presenter has in mind [M86 p. 6]. The thesis
states the protocol in one sentence: "The system receives these terms one by one and, after
each one, either ventures a guess at the underlying pattern, quits, or asks for more
information (another term)" [M86 p. 37]. A guess is not just the next block of terms: the
system "will venture a guess -- not simply by supplying the next template (although it does
this), but by showing the user a synopsis of its model" [M86 p. 7]. If the guess is wrong
"the user will so indicate and the system will continue, probably by asking for more terms,
and then using those as a basis for reformulating its faulty perception" [M86 p. 37]. The
2017 notes say the same: "Once Seek-Whence has sufficient evidence in support of its model,
it will present a summary of that model's structure as a hypothesis and supply the next
block of terms predicted by it" [MHN p. 3], and "Seek-Whence will settle once it has created
and displayed a working hypothesis for the pattern" [MHN p. 3].

The test of the system's pattern perception is twofold: "1) a characterization of the
pattern; 2) an extrapolation of the sequence according to that characterization"
[M86 p. vii]. The program's three stated goals are to discover non-mathematically-sophisticated
patterns in sequences of nonnegative integers, to represent them as concepts built from the
eight primitives (Constant, Countup, C-group, S-group, P-group, Y-group, Cycle, Tuple), and to
reformulate the descriptions fluidly by slipping when a description is non-predictive or
non-optimal [M86 p. 148].

#### 1.2 What a "reasonable" sequence is

The domain is "non-mathematically-sophisticated rules that generate sequences of nonnegative
integers" [M86 p. 37]. The thesis was written because an earlier class project, a sequence
extrapolator that knew primes and Fibonacci numbers and could finite-difference its way to
`1 2 5 15 42 98` (second differences every third prime), was "mechanistic", applying the same
machinery to every sequence regardless of its form [M86 pp. 4-5]. Seek-Whence strips the
integers of all mathematical operations so that they "serve as atomic units -- structures
without internal pattern -- in our pattern domain" [M86 p. vii]; the patterns "are based on
little more than the successorship and sameness relations between pairs of integers -- yet
they can become arbitrarily complex, challenging even for humans" [M86 p. vi]. The guessed
rules must be reasonable, "acceptable as possible solutions to a human observer; they should
elegantly and economically explain the portion of the sequence already seen, as well as
predict an infinite continuation" [M86 p. 37]. The notes put it as a definition: "A
reasonable pattern is one that relies on nothing more mathematical than successorship and
predecessorship" [MHN p. 3].

The thesis's own list of typical problems [M86 p. 38], checked on the page image (the text
layer confuses the digit 8 with B):

| Typical sequence [M86 p. 38] |
|---|
| `2 2 2 ...` |
| `1 1 2 2 3 3 ...` |
| `1 2 2 3 3 3 ...` |
| `1 2 1 2 3 1 2 3 4 ...` |
| `1 0 1 0 0 1 0 0 0 1 ...` |
| `1 2 8 3 4 8 5 6 8 ...` |
| `1 2 8 3 4 5 8 6 7 8 9 8 ...` |
| `3 7 3 7 3 7 ...` |
| `8 0 8 8 0 8 8 0 8 ...` |
| `1 1 2 1 2 2 1 3 2 ...` |

**Excluded**, with the thesis's reasons [M86 p. 38]:

| Non-domain sequence | Why it is out |
|---|---|
| `2 3 5 7 11 ...` | "primes" is too mathematically sophisticated a notion |
| `-3 -2 -1 ...` | negative integers are "unknown" |
| `1 7 9 15 18 ...` | "get bigger" is too amorphous; there is no canonical next term |

The notes give the same two kinds of exclusion with `2 3 5 7 11 ...` and `5 12 89 147 ...`
[MHN p. 3]. The system "is like a small child who is able to count and notice samenesses but
who cannot do arithmetic, count by twos, recite primes, etc." [M86 p. 38]. Derived sequences
are banned on purpose, as a lesson from the class project, whose success "was directly
proportional to the number of tricks and special sequences the programmer could devise"
[M86 p. 8]: "we have not permitted Seek-Whence to work on derived sequences (e.g., first
differences, first ratios, even-numbered terms, etc.) of any kind" [M86 p. 8]. Separating
interleaved subsequences, pulling out group lengths and the like are "actions that can only
be employed after the initial noticing of patterns has taken place" [M86 p. 9]; the program
should pick such tricks from the bag by itself, based on its perceptions, and not from a
canned, pre-determined hierarchy of techniques (paraphrased from [M86 p. 9]).

The **Blackburn dozen** [M86 pp. 6-7] are the twelve sequences given, one term at a time, to
twenty-five students at Blackburn College [M86 p. 6]; they are the thesis's yardstick for
"correct" solutions and "reasonable" patterns, and chapter 5 reports the program on each of
them (section 1.6 below). Read on the page images (p. 6 unparsed, p. 7 parsed):

| # | Sequence [M86 p. 6] | Parsed [M86 p. 7] |
|---|---|---|
| 1 | `1 1 2 1 2 3 1 2 3 4 1 2 3 4 5 ...` | `1 * 1 2 * 1 2 3 * 1 2 3 4 * 1 2 3 4 5 * ...` |
| 2 | `1 2 3 4 5 6 7 ...` | `1 * 2 * 3 * 4 * 5 * 6 * 7 ...` |
| 3 | `2 1 2 2 2 2 2 3 2 2 4 2 2 5 2 ...` | `2 1 2 * 2 2 2 * 2 3 2 * 2 4 2 * 2 5 2 ...` |
| 4 | `1 2 2 3 3 3 4 4 4 4 ...` | `1 * 2 2 * 3 3 3 * 4 4 4 4 * ...` |
| 5 | `1 8 5 8 1 8 5 8 1 8 5 8 ...` | `1 8 5 8 * 1 8 5 8 * 1 8 5 8 * ...` |
| 6 | `2 1 2 2 2 3 2 4 2 5 ...` | `2 1 * 2 2 * 2 3 * 2 4 * 2 5 ...` |
| 7 | `2 3 1 2 3 2 2 2 3 3 3 3 2 3 4 4 4 4 ...` | `2 3 (1) * 2 3 (2 2) * 2 3 (3 3 3) * 2 3 (4 4 4 4) * ...` |
| 8 | `1 2 2 3 3 4 4 5 5 6 ...` | `1 2 * 2 3 * 3 4 * 4 5 * 5 6 ...` |
| 9 | `1 2 3 3 4 4 5 5 5 6 6 6 ...` | `1 * 2 * 3 3 * 4 4 * 5 5 5 * 6 6 6 ...` |
| 10 | `9 1 9 2 9 3 9 4 ...` | `9 1 * 9 2 * 9 3 * 9 4 ...` |
| 11 | `1 8 1 2 1 8 1 2 3 2 1 8 1 2 3 ...` | `1 8 1 * (2 1) 8 (1 2) * (3 2 1) 8 (1 2 3) * ...` |
| 12 | `1 8 5 5 8 1 1 8 5 5 8 1 ...` | `1 8 5 5 8 1 * 1 8 5 5 8 1 * ...` |

The thesis calls "a run of terms between asterisks (*) in the parsed versions" a template, and
to show understanding "the solver must complete the current template and fill out the next
one" [M86 p. 7]. (This is the informal sense of "template"; the program's template structure
of section 2 is a different, more specific thing. FCCA uses the word in a third way, section
3.3.) Note the thesis's own reading of #7 and #11: the parenthesised parts are the growing
groups and the outer `2 3` / `8` are fixed [M86 p. 7].

Chapter 1 also lists fourteen "reasonable extensions" of the initial segment `1 1 2`
[M86 pp. 18-19], for example `1 * 1 2 * 1 2 3 * 1 2 3 4 *`, `1 1 * 2 2 * 3 3 * 4 4 *`,
`1 1 * 2 2 * 1 1 * 2 2 *`, `1 1 * 2 * 1 1 * 2 *`, `1 * 1 2 * 1 * 1 2 *`, `1 1 * 2 1 * 3 1 * 4 1 *`,
`1 1 * 2 2 2 * 3 3 3 3 *`, `1 1 2 * 1 2 2 * 1 3 2 * 1 4 2 *`, `1 (1 2) * 2 (1 2) * 3 (1 2) *`,
`1 1 2 * 2 1 3 * 3 1 4 * 4 1 5 *`, `1 1 2 * 3 1 4 * 5 1 6 * 7 1 8 *`,
`1 * 1 2 1 * 1 2 3 2 1 * 1 2 3 4 3 2 1 *`, the "doubler" `(1 1) * 2 * 3 ** (1 1) * 2 * 3 **`
and Clossman's "marching doubler" `(1 1) * 2 * 3 ** 1 * (2 2) * 3 ** 1 * 2 * (3 3) **`
[M86 p. 18]; no one can say which generalization is the correct one, since that depends on
the presenter's pattern concept, but all are reasonable (paraphrased from [M86 p. 21]). The marching doubler needs the
rectangular "instruction boxes" of the diagrams (replace-by, jump-to-next-sibling)
[M86 pp. 23-24], which the program does not implement: "The most important omission is of the
rectangular instruction boxes seen in Figures 5 and 6 of the last chapter" [M86 p. 36].

#### 1.3 The roles an integer can play

A term is an atomic element: eliminating knowledge of mathematical operations avoids the
question whether a 4 should be read as 2*2, 5-1 or 100/25 (paraphrased from [M86 p. 36]). The same page
lists the roles a value can have: in the segment `2 3 4 5` the 4 is an element, it is the
length of the segment, it is the successor of 3 and the predecessor of 5, and "Beyond that,
it has very little structure" [M86 p. 36]. So an integer appears in the program as (a) a term
value, (b) a group length, (c) the successor of the previous value or (d) the predecessor of
the next [M86 p. 36]. In the implemented program group lengths are *not* registered as
manifestations of the ideal integers, which the thesis names as the obstacle to Blackburn
#9: "The solution of problem 9 will probably require that group lengths be used as
manifestations (they are not, currently)" [M86 p. 147]. Of the same vocabulary, the
glossary's "hit" is "a query of a Seek-Whence diagram or of a box for its next value -- a
term or grouping of terms" [M86 p. 181].

#### 1.4 The user protocol as seen in the transcripts

The appendix prints three complete console runs [M86 pp. 164-179]. Everything below was read
on the page images. A run begins with `-> (startup)` [M86 pp. 164, 170] (the second transcript
omits it [M86 p. 167]). The prompt is `please enter a term:`; the user types one integer, and
the program prints the task stream until it needs another term, then prompts again
[M86 p. 164]. Lines beginning with `;` are Meredith's annotations added to the printout
(`; A template is made.`, `; The system ventures a guess.`), not program output
[M86 p. 165].

At the prompt the user may instead type a console command:

| Command | What is printed | Seen at |
|---|---|---|
| `show` | `terms of the sequence:` and the terms; `bonds:` (one line per bond: name, type, field, modifier, members); `gloms:` (name, type or `pseudo`, print-value, `terms i to j`); `gnoths:` (the fields of every gnoth; see item 02) | [M86 pp. 167-168, 171, 174-175, 178-179] |
| `show-hypothesis` | the hypothesis form, e.g. `(Cycle 3 2 (3 7))` | [M86 pp. 166, 168, 171, 174, 178] |
| `show-seq` | the terms so far, e.g. `3 7 3 3` | [M86 pp. 169, 176, 179] |
| `show-parse` | the parenthesization, e.g. `((1 2) (1))` | [M86 pp. 174, 178] |

The `show` output lists bonds as `bond1 Same print-value (remote) -- (glint1 glint3)` and
gloms as `glom1 (Same print-value fence) --> (3 7) terms 1 to 2` or
`glom3 pseudo --> ((3 7)) terms 1 to 2` [M86 p. 167]. A gnoth is printed one field per line:
`class`, `name`, `frame`, `plato-class`, `glom`, `notes`, `form`, `state`, `range`
[M86 p. 168], and, after reformulation, also `external-bonds`, `internal-bonds`,
`equivalence-type` and `groups` [M86 p. 175].

**The guess.** When a new term has been filtered through the hypothesis and confirms it, the
program prints `I have a guess!`, then `hypothesis:` followed by the form, then the next block
of predicted terms, then `enter no if wrong, ok if right`; the user answers on the same line
[M86 p. 166]. After `ok` the program prints `bye` and the run ends [M86 pp. 166, 169, 179].
After a refusal (the transcript's user types `nope`) the program goes back to asking for terms
and reformulates [M86 pp. 175-176]. In all three runs the guess comes after exactly one
confirming term: `3 7 3` then `7` [M86 p. 166]; `3 7 3 3` then `7` [M86 p. 169]; `1 2 1`
then `2` [M86 p. 175] and `1 2 1 2 3` then `1` [M86 p. 179]. The predicted block printed with
the guess is the next few hits of the box: `3 7 3 7` for `(Cycle 3 2 (3 7))` [M86 p. 166],
`3 7 3 3 7` for `(Cycle 3 3 (3 7 3))` [M86 p. 169], `(1 2)(1 2)(1 2)` for `(S-group 1 2)`
[M86 p. 175] and `(1 2)(1 2 3)(1 2 3 4)` for `(S-group 1 (Countup 2))` [M86 p. 179]. The
prose gives no numeric confirmation rule, only "Consistent verification, in the form of terms
which support the hypothesis, will lead the system to a confirmation of the hypothesis and the
venturing of a public guess" [M86 p. 39], a validity count that grows with correct
predictions (item 02, [M86 pp. 81-82]), and the socrato-level tasks that say, in essence,
"Enough! I have a hypothesis. Let's have the next term to check it out" or "I think the answer
is .... Tell me if I'm wrong" [M86 p. 76]. How many confirmations the reconstruction requires
is therefore a decision (item 05), with the transcripts' "one" as the evidence.

**Quitting.** The thesis allows the system to quit [M86 p. 37], and reports that "successful
runs are generally completed in under ten minutes of real time. Unsuccessful runs take a bit
longer (potentially forever), as the program thrashes about for a solution" [M86 p. 145]; on
Blackburn #5 "We stopped it after a fifteen-minute attempt" [M86 p. 146]. No transcript shows
the program quitting on its own.

#### 1.5 Where the program lives, in the author's words

The program is "approximately 5400 lines of Franz Lisp code", run interpreted on a VAX 11/750
[M86 p. 145]; the notes name the Lisp as "Franz Lisp (Opus 38.69, June 1983)" [MHN p. 4] and
describe its organisation in four files, `bonds.l` (the startup function and bonding tasks),
`gloms.l` (glom-scouts, Plato-scouts, templates, Plato-evaluator), `gnoths.l` (template
evaluation, gnoths, the hypothesis, the guess) and `reforms.l` (reformulation)
[MHN pp. 4-5]. The code itself is not reproduced in the notes and is believed lost
(ANSWERS.md 1).

#### 1.6 The reported results

On the Blackburn dozen "the current Seek-Whence program can solve only three of the Blackburn
dozen -- problems 2, 8, and 12" [M86 p. 147]; #2 is solved "quite readily, in just under one
minute" [M86 p. 145], #8 "within three minutes" [M86 p. 146], #12 "can take up to twenty
minutes" and often yields the ugly Y-group `(1 8 (5 (5 8 1 1 8 5) 5) 8 1)` [M86 p. 147]. On
#1 the program "thrashes hopelessly", although it solves `1 2 1 2 3 1 2 3 4` (the third
transcript) [M86 p. 145]; #3 is "hopeless as yet; we have not even attempted it" [M86 p. 145];
on #4 it fails but solves `2 2 3 3 3 4 4 4 4` and `1 1 2 2 2 3 3 3 3` [M86 p. 146]; on #5 it
finds the Y-group `1 8 5 8 1` and clings to it, but solves `1 8 5 8 1 1 8 5 8 1`
[M86 p. 146]; #6 and #7 are beyond the system [M86 p. 146]; #9 needs group lengths as
manifestations [M86 pp. 146-147]; #10 "will probably be the next sequence solved"
[M86 p. 147]; #11 bogs down in spurious relationships [M86 p. 147]. The full table, with the
reconstruction's results beside it, is the job of item 04 and `RESULTS.md`.

### 2. The architecture

*Implemented in:* `seekwhence/world.py` (the three arenas of 2.1, the counters, the shared state), `seekwhence/taskrack.py` (the taskrack of 2.3: urgencies, the one-draw choice, cleaning; D-007 to D-010, D-057), `seekwhence/run.py` (the run loop and the prompt of 2.5; D-011 to D-013), `seekwhence/config.py` (`TASK_TYPES`, the table of 2.4, and the urgency of every type, D-008), `seekwhence/events.py` (the typed events the trace observes, D-055). Tests: `tests/test_world.py`, `tests/test_taskrack.py`, `tests/test_run.py`, `tests/test_events.py`, `tests/test_config.py`.

#### 2.1 The three levels and the two intermediate structures

The thesis views the Seek-Whence world as "three levels, with two potential intermediating
structures -- the hypothesis and the template" [M86 p. 43]. The levels follow the Socratic
picture of a real world, an ideal world and a perceived world between them [M86 p. 43]:

| Level | Name | Holds | Source |
|---|---|---|---|
| lowest, the "real world" | **cytoplasm** | representations of the input integers (glints) and relations among them: sparks, bonds, gloms, pseudo-gloms | [M86 p. 43], [M86 p. 44] (Figure 1) |
| middle, the "perceived world" | **socratoplasm** | gnoths (the parse), boxes and printstructures | [M86 p. 43], [M86 p. 44] (Figure 1) |
| highest, the "ideal world" | **platoplasm** | the ideal types: ideal repeaters, ideal groups, and the "ideals seen" (the ideal integers) | [M86 p. 43], [M86 p. 44] (Figure 1), [M86 p. 45] |

Figure 1 [M86 p. 44] draws the hypothesis as a box beside the socratoplasm and the template as
a box beside the cytoplasm, and labels the cytoplasm's contents "Gloms, Pseudo-gloms" above a
wavy line and "Gloms, Glints, Sparks, Bonds" below it. The 2017 notes name the same three
arenas as "conceptual (platoplasm), short-term analytical (socratoplasm), or perceptual
(cytoplasm)" [MHN p. 3].

The two intermediate structures: the **hypothesis** is "an active formulation of the system's
current view of the evolving pattern" [M86 p. 42], and the **template** is its lower-level
counterpart, "a transitory, weaker, less expressive, and less flexible structure used as a
first rough statement of an emerging formulation" [M86 p. 42]. Below them are the central
working structures: glints, gloms and gnoths (pronounced "knots", because they tie things
together) [M86 p. 42]. Glints are the representations of input terms, members of the
class Glints, a distinguished subclass of the class Gloms, atomic and undissolvable
(paraphrased from [M86 p. 42]); gloms represent collections of adjacent glints hierarchically
grouped, and glomming is the combination of two or more gloms into a new one [M86 p. 42].

The **platoplasm** houses the "ideals": idealised versions of the input integers, the grouping
structures known a priori, and relations among them; ideals connect to the cytoplasm through
*manifestation* links and to the socratoplasm through *actualization* links, so a glom of
three 2's becomes a manifestation of the C-group ideal and a gnoth devoted to it an
actualization [M86 p. 45 and Figure 2]. The glossary: a manifestation is "a glom that exhibits
the properties of some Platonic class" [M86 p. 181]; an actualization is "a gnoth that
exhibits the properties of some Platonic class" [M86 p. 180].

The **socratoplasm** exists because gloms cannot represent a parse: they combine only under
bottom-up pressure, their participants do not survive glomming, and they cannot change
structure without being destroyed; "Thus, gloms are too ephemeral and unpredictable to
represent a sequence parse" [M86 p. 43]. Gnoths are more permanent: a gnoth "can pass gloms to
its neighbors, can withstand reformulation of its subgloms, and can even become a
representative of concepts different from those associated with it at the time of its
creation" [M86 p. 44], and "A gnoth ceases to exist only when its subglom collection is empty"
[M86 p. 44]. "The socratoplasm is a working area and sometime battleground between the
cytoplasm and the platoplasm" [M86 p. 46]; the first gnoths are created with the first
hypothesis, and from then on "gnoths must always be in agreement with the hypothesis"
[M86 p. 46]; each gnoth derives its structure from its collection of subgloms, and any change
to a gnoth is realised by changing that collection [M86 p. 46]. It is "where slipping occurs"
[M86 p. 76].

The **cytoplasm** is the bottom level: all changes to higher structures filter down and are
reflected there, and "All processing here is automatic and myopic -- no global views of the
sequence are maintained" [M86 pp. 46-47]. New structures (a new glom, the glint of a new term)
"are immediately made centers of interest called active foci, and heightened activity takes
place around them" [M86 p. 47]. "The main goal of this level is to hit upon a pattern of gloms
which can be taken as a template for the sequence" [M86 p. 47]; once a template is in place a
"cap", a pseudo-glom (inert, unable to interact with other gloms), is put over the top-level
gloms to prevent their disappearance; new terms' glints still bond and glom but no change that
contradicts the template is allowed, and a term that does not fit starts a review [M86 p. 47].
Once a hypothesis exists, new terms are filtered through it rather than through the template,
which "is virtually abandoned in deference to the more malleable, more expressive structure"
[M86 p. 47].

The levels talk in both directions. Seek-Whence, like HEARSAY II, operates at several levels
at once, from the integers typed at the terminal through the hypothesis to the ideal
concepts; low-level structures support the creation of higher ones, and changes made by
high-level actions to lower structures may have unexpected effects upwards, reverberations
modelled on the ripplings among levels in HEARSAY II [M86 p. 39]. The notes insist on the
point: "while processing begins in the cytoplasm, Seek-Whence is not a purely bottom-up
system. All noticings, recombinations, and results associated with the higher levels percolate
down to drive or terminate further work in the cytoplasm" [MHN p. 4].

#### 2.2 The two stages

"There are two stages of processing in Seek-Whence" [M86 p. 38]. **Stage one** builds an
initial formulation, "culminating in the creation of a hypothesis for the underlying rule"
[M86 p. 38]; its structures matter later because all high-level actions affect and are affected
by them [M86 pp. 38-39]. **Stage two** begins once a hypothesis exists: "The major distinction
between the two stages of processing is the existence of the hypothesis in stage two. Without
it, the system has no model of the sequence and so cannot predict the next term to be
encountered" [M86 p. 39]. In stage two every new term is "filtered" through the hypothesis
(checked for agreement with it); confirming terms are handled rapidly, essentially just being
swallowed, while an unexpected term makes the system sit back and look things over
(paraphrased from [M86 p. 40]). Consistent verification leads to a public guess [M86 p. 39]; a
guess rejected by the user, or a refutation by new evidence, makes the system reformulate "or,
in rare instances, abandon the hypothesis" [M86 p. 39]. Abandonment ("scrapping") takes
processing back to the lower levels: "This is not a total restart with a clean slate as though
the sequence terms had never been seen, but rather a return to the term level, with all
perceived groupings eradicated but with accumulated knowledge of term samenesses and other
primitive relations maintained" [M86 p. 39].

Reformulation is modification of the form of the hypothesis, done by slipping from one concept
to another, guided by the evidence and by the system's slipping knowledge, which lives in the
Slipnet, a structure holding the relations among the ideals and pointers to salient structures
at various levels [M86 p. 40]. Reformulation causes related changes through all the levels, so
that all levels work with the same pattern structure in mind, and these changes can in turn
cause the noticing of new perceptions [M86 p. 40], [M86 p. ix]. Unlike HEARSAY II, Seek-Whence
keeps no collection of alternative hypotheses; it maintains one reigning hypothesis and the
ability to reformulate it as the pressure to change mounts [M86 p. 27]. Against loops it
"goes a step further by remembering encapsulations of previously-generated hypotheses in
order to prevent their re-use" [M86 p. 30] (the freeze-dried hypotheses of item 03).

The chapter-2 summary [M86 p. 47]: when the program succeeds, the complex but subliminal first
stage develops a hypothesis, an encapsulation of the perceived pattern, represented so as to
make the reformulation often needed in the second stage possible.

#### 2.3 The taskrack

**Simulated parallelism, no overseer.** "The system employs simulated parallel processing with
non-cooperating processes working independently and under no overseeing agent" [M86 pp. 25-26].
Triggered processes, those awakened by recent events, "are chosen at random to perform their
duties, the choice being affected (but not determined) by the weights" or urgencies of the
candidates [M86 p. 26]. In chapter 2: "all operations are carried out in task series, which run
in simulated parallel. The tasks comprising various series are chosen at random for
processing, so no assumptions can be made about which of two competing tasks will run first"
[M86 p. 41]; the tasks of a series may vary, because any task may alter the environment
[M86 p. 41]. "A particular task may create, access, or modify some data structure, may request
information from the user, or may set out other tasks -- place them on the taskrack where they
will stay until chosen and run" [M86 p. 41]. The glossary defines a task as "an uninterruptible
(and generally small) segment of a computational process. Tasks are capable of creating or
modifying structures, setting off other tasks, or querying the user" [M86 p. 182]. The notes:
"There is no centralized program controller; order must emerge from task effects and
interactions" [MHN p. 3]. The model is biological, credited to Hofstadter: enzymes act on
molecules in a cell, making a molecule more attractive to some enzymes and less to others;
"The gloms in Seek-Whence's cytoplasm serve as molecules and the tasks as enzymes in our
version of this biological model" [M86 p. 41].

**Urgency.** All tasks have urgencies, integer weights, "and more urgent tasks will have a
greater chance of being chosen than less urgent ones do" [M86 p. 42]. The only urgencies the
thesis describes concretely are the Glomtester's: its weight depends on the test, groupable
"being strongest) and the bond-type involved (sameness being stronger than successorship, and
so on)" [M86 p. 66], so "a Glomtester for a sameness group is given a higher weight than a
predecessor fence Glomtester. The system is thus biased toward noticing certain similarities
first, yet it is not compelled to do so" [M86 p. 67]. In the transcripts the task
`Sparkler-plus` carries a trailing number, always 2 or 10 (`doing task Sparkler-plus on
(glint2 glint1 10)`, `... (glint2 glint2 2)`) [M86 pp. 164, 170, 172]; the prose never
describes Sparkler-plus or the number. The thesis does not give the sampling formula, the
urgency of any other task type, or whether urgencies decay.

**Cleaning and stale tasks.** "At certain times some cleaning is done (removing old tasks of
certain types) but it is perfectly possible for an old task finally to be chosen and, when it
runs, for it to find the Seek-Whence world quite different than when it was created. Such a
task will probably do nothing, because the structures on which it was designed to operate no
longer exist or are inaccessible to it" [M86 p. 42]. The transcripts show such no-ops:
`doing task Dissolver on (glom3)` followed by `glom3 is not in cytoplasm` [M86 p. 172], a
`Glomtester failed glint2` [M86 p. 170], `failed to glom (glint1 glint2 glint3)` [M86 p. 165],
and a `doing task empty-task` [M86 p. 170]. Every task that acts first checks that its
structures still exist: Testers check that both gloms of the spark are still in existence
[M86 p. 64], a Bonder "checks to see that both gloms still exist" [M86 p. 64], a Glomtester
"must first be certain that the glom it is supposed to test is still in the cytoplasm"
[M86 p. 67], a Plato-scout begins only "If the glom still exists when the Plato-scout is
invoked" [M86 p. 68], a Gnoth-operator "must first check to see that all the structures
relevant to its operation are still in existence" [M86 p. 124]. When the cleaning happens and
which types it removes is not said.

**Temperature.** The thesis uses the word only by analogy with Jumbo [M86 p. 29]: "The
creation and acceptance of a template causes increased activity in the system"
[M86 pp. 75-76], in effect raising the temperature [M86 p. 76]; and in stage two a
Gnoth-operator's last act is to lower the bond-strength threshold by 1, "heating up" the
environment, while each Reformulator adds 1, cooling it [M86 p. 124] (item 03). There is no
global temperature variable in the description.

**Order from chaos.** "Seek-Whence, then, depends upon order to emerge from chaos" [M86 p. 42]:
small special-purpose tasks at different levels of abstraction, with no overseeing agent,
eventually guide the system to convergence on a working hypothesis [M86 p. 42], [M86 p. ix].

#### 2.4 The task types

Every task named in the thesis or printed in the transcripts, in the order of the processing
chain. "Level" is where the thesis places it. "Pages" are where it is described; transcript
pages are given when the prose is silent or when the printed form matters.

| Task | Level | What it does | Pages |
|---|---|---|---|
| **Call-term** | user | Asks the user for the next term (`please enter a term:`). Not described in the prose; printed after a Hypothesizer, a Gnoth-maker or a new hypothesis candidate. "At this point, the system typically calls for the next term in order to test its new hypothesis" [M86 p. 125]. | [M86 pp. 165, 171, 174, 178] |
| **Sparkler** | cyto | Pulls two gloms at random from the cytoplasm and, if they are not subgloms of each other, creates a *spark* between them: "A spark is created between two gloms when a Sparkler task pulls those gloms at random from the cytoplasm and determines in a very cursory way that the two structures might be amenable to bonding" [M86 p. 63]. It looks for no common features. Prints `sparkN --- between glintAglintB`. | [M86 p. 63], [M86 pp. 164-165] |
| **Sparkler-plus** | cyto | Transcript only: `doing task Sparkler-plus on (glint3 glint1 2)`, with a pair of cyto-elements (the newest glint or glom first) and a number 2 or 10; sometimes creates a spark, sometimes not. The prose does not describe it. | [M86 pp. 164-165, 170, 172, 176-177] |
| **Tester** | cyto | A spark releases a horde of Testers; each, when run, chooses some spark (not necessarily the one that created it), checks that both members still exist, intersects their bond-fields (print-value, span) and tests the values for similarities; for each bonding test passed it creates a Bonder. Bond types: sameness, successorship, predecessorship, adjacency and meeting, grouped into families; sameness and successor-predecessorship matter most. | [M86 p. 64] |
| **Bonder** | cyto | "1) checks to see that both gloms still exist, and 2) checks to see that they are not already bonded in this way" [M86 p. 64], then creates a bond with a strength derived from the bond type, modifiers (adjacent stronger than remote) and the glom characteristic bonded on [M86 pp. 64-65]. "Creation of a bond causes the release of more Sparklers, stimulating the system to carry out more low-level search and bond creation, and causes the release of some Glom-scouts" [M86 p. 65]. Prints `doing task Bonder on (Same print-value (remote) glint1 glint3)` and `bond1 --- between glint1glint3`. | [M86 pp. 64-65], [M86 p. 164] |
| **Glom-scout** | cyto | Chooses a cyto-element (a glint or a glom that is not a subglom) and makes three quick tests for each bond family it is in: groupable (bonded to a neighbour this way), coverable (bonded into a symmetry group), fenceable (bonded to remote gloms); each test passed creates a Glomtester. The tests prefigure the plato-level generate, process and fence classes. Prints `Same -cover proposed --> glint1`, `Same -fence proposed --> glint1`, `Pred -group proposed --> glint2`. | [M86 pp. 65-66], [M86 pp. 165, 170] |
| **Glomtester** | cyto | Checks the glom is still in the cytoplasm, then finds the extent of the evolving glom, aiming at maximal size (the four examples on p. 67: `2 1 1 1 3` sameness gives `1 1 1`; `9 1 2 5 9` fence gives `9 1 2 5`; `5 3 1 6 1 3 4` symmetry gives `3 1 6 1 3`; `1 2 3 2 3` pred-succ gives all of it); "The Glomtester either rejects the group as a glom or creates a Glommer task to refine the group and perform the final glomming" [M86 p. 67]. Its urgency depends on the test and the bond type (section 2.3). | [M86 pp. 66-67], [M86 p. 165] |
| **Glommer** | cyto | Bookkeeping: "It creates the new glom and makes it an active focus -- a site of increased system activity. It also creates a Glom-inspector task to continue pushing the glom up to higher levels" [M86 p. 67]. Prints `Glommer for Same print-value fence members: (glint1 glint2)`, or `failed to glom (...)`. | [M86 p. 67], [M86 p. 165] |
| **Glom-inspector** | cyto/plato | Determines which plato-classes, if any, might find the glom interesting, i.e. might consider it a manifestation; if there are any, creates a Plato-scout with the glom and the candidate classes. | [M86 p. 68], [M86 p. 165] |
| **Plato-scout** | plato-cyto | Given a glom and candidate classes, applies each class's *process* function to the glom's print-value; a glom that passes is pure and is *dubbed* a manifestation of the class (a glom may be dubbed more than once, e.g. `(1 1 1)` as C-group and Y-group); fence classes (Tuple, Cycle) have no process, and a flat glom (glints only) passes them by default. A glom that passes no test causes a Plato-evaluator. Prints `doing task Plato-scout on ((S-group P-group) glom1)`. | [M86 pp. 68-69], [M86 pp. 165, 170, 173] |
| **Plato-evaluator** | plato-cyto | "Any glom that does not pass even one of its process tests is cause for the creation of a Plato-evaluator task" [M86 p. 69]; it looks for dubbable gloms of secondary purity (non-flat) and for pure sub-collections (e.g. `1 2 3 2 3` broken into `1 2 3` and `2 3`); if it has no success and the glom was not already dubbed, a Burster is created. It creates no other tasks: "Our terraced scan has come to an end" [M86 p. 70]. | [M86 pp. 69-70], [M86 p. 170] |
| **Burster** | cyto | Destroys a glom and all its non-glint subgloms, leaving only glints: "A Burster task is even more destructive of glom structure" [M86 p. 70]. | [M86 pp. 69-70] |
| **Dissolver** | cyto | "A Dissolver is a task that destroys a glom, leaving its highest-level subgloms to float independently in the cytoplasm" [M86 p. 70]. Once a Burster or Dissolver is set on a glom, its only escape is to glom with another glom. Prints `doing task Dissolver on (glint1)` or `... (glom3)` then `glom3 is not in cytoplasm`. | [M86 p. 70], [M86 pp. 164, 170, 172, 176] |
| **Template-scout** | template | Created whenever a glom is dubbed; sets out a Template-applier to see if the whole sequence is of the dubbed glom's form. Prints `doing task Template-scout on (glom1)`. | [M86 pp. 69, 73-74], [M86 pp. 165, 170, 173-174] |
| **Template-applier** | template | Tries to view the whole sequence as repetitions of the dubbed structure; if that fails, loosens a parameter or two (`(S-group 1 2)` to `(S-group 1 n)`, where n means any nonnegative integer); if it still fails, creates a Template-resolver. On success prints `create-template-glom (Cycle 3 2 (3 7))(glom1)`, `top-down glom glom2`, `template made : (form (Cycle 3 2 (3 7))`. | [M86 pp. 74-75], [M86 p. 165] |
| **Template-resolver** | template | The last resort: looks at glints rather than gloms to see whether the sequence can be re-viewed to fit the mold; if successful it blasts (bursts) "all gloms and has the glints reglommed to fit the template" [M86 p. 75]; if not, the template is forgotten and the engendering glom dissolved. "In practice, the Template-resolver is seldom invoked" [M86 p. 75]. | [M86 p. 75] |
| **Template-evaluator** | template | "The template will be checked by a Template-evaluator task and either be passed, or rejected and abandoned" [M86 p. 74]; if passed it is the basis for hypothesis and gnoth creation. Prints `doing task Template-evaluator` then `check-cycle template (Cycle 3 2 ((3 7)))`. | [M86 p. 74], [M86 pp. 165, 171] |
| **Template-reviewer** | template | "is invoked after a template has been created (but no hypothesis exists) and when new terms fail to fit the template" [M86 p. 75]; it can call for modification of the template and a restart of creation and evaluation, leave it alone, or target it for abandoning. Not seen in the transcripts. | [M86 pp. 74-75] |
| **Gnoth-maker** | socrato | Set off, with a Hypothesizer, when a template is accepted; "it creates one gnoth for each subglom of the template-glom and notifies the associated plato-classes of their existence" [M86 p. 77] (the three-gnoth example for `1 1 2 1 2 3` with `(S-group 1 n)` is on p. 78). Prints `top-down glom glom3`, `gnoths constructed`. | [M86 pp. 76-78], [M86 pp. 165, 171] |
| **Hypothesizer** | socrato | "the Hypothesizer is responsible for devising a hypothesis for the sequence, based on the template and the existing gnoths (if any) and gloms" [M86 p. 79]; with a faulty template or none it can take "the fall-back position of declaring the sequence to be a Tuple, the weakest of all plato-classes" [M86 p. 79]. Prints the form, e.g. `(Countup 1)`. | [M86 pp. 76, 79], [M86 pp. 165, 171] |
| **Hfilter** | hypothesis | Hypothesis filtering of a new term: "a process whereby new input terms are checked for conformity with the reigning hypothesis. Should a new term not conform to the hypothesis, reformulation begins" [M86 p. 181]. Prints `new term being hypothesis-filtered through (Cycle 3 2 (3 7))` and, on agreement, `top-down glom glom4` and the guess; on disagreement `set-out -- validity: 0` (run 2's terse printout only). | [M86 pp. 40, 47, 181], [M86 pp. 166, 168, 171, 175] |
| **Gnoth-setter** | socrato | On an unexpected term "the system immediately sets the hypothesis' validity to 0, releases sparks to encourage low-level activity, and places a Gnoth-setter task on the taskrack" [M86 p. 98]; the Gnoth-setter fills out gnoths according to the old hypothesis (the catchall gnoth is split; non-fitting terms go in a final gnoth) and calls for reconsideration. Prints `top-down glom glomN` lines and `gnoths: (gnoth1 gnoth2 gnoth3)`, then the slip-or-stay check `slip-check: stayval: 15.0 best: (Y-group 4.0)`. | [M86 pp. 98-99], [M86 pp. 168, 173, 176] |
| **Reformulator** | socrato | Placed once a reigning class is established or reconfirmed: "this process will attempt to find salient bonds and will set out Gnoth-operator tasks designed to act upon the bond pulls or pushes in order to change the gnoths" [M86 p. 105]; sets the threshold (1 more than the existing one, or 80% of the strongest pull-bond strength for the reigning type), assesses all bond pulls, converts strong pulls into gnoth operations, and when done hangs a Bond-assessor on the taskrack. | [M86 pp. 105-108], [M86 pp. 174, 177-178] |
| **Gnoth-operator** | socrato | Carries out the operation or PROGRAM it was given; checks that the structures still exist, else terminates; "its last action is to decrease the system's bond-strength threshold by 1" [M86 p. 124]. Prints the program, e.g. `((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil))))`. | [M86 pp. 107, 124], [M86 pp. 174, 177] |
| **Capper** | socrato | Set out by a NO-OP operation: recaps an uncapped gnoth; "The Capper finds all current gloms whose ranges overlap with the original range of the gnoth (before it was uncapped) and claims them for the gnoth" [M86 p. 113]. | [M86 p. 113] |
| **Bond-assessor** | socrato | Created each time a Reformulator finishes at some level: "The Bond-assessor's job is to look at all current bonds and determine whether or not there is reason to continue reformulation" [M86 p. 124]; if so it places a Reformulator, if not a Gnoth-caster. Prints `doing task Bond-assessor on (S-group 8.0)`. | [M86 pp. 105, 124], [M86 pp. 174, 177-178] |
| **Gnoth-caster** | socrato | "a Gnoth-caster attempts to describe each gnoth in terms of the reigning class" [M86 p. 124]; if all gnoths (or all but a promising last one) can be cast it generalises the casts to a common form (`(3 4) (3 4) (3)` gives `(S-group 3 2)`; `(1 2) (1 2 3)` gives `(S-group 1 (Countup 2))`), which becomes a hypothesis candidate; a box is built and tested to see that it postdicts the known terms. Prints `casts: ((S-group 1 2) (S-group 1 3))`, the form, `new hypoth candidate (S-group 1 (Countup 2))`. | [M86 pp. 124-125], [M86 pp. 168-169, 174, 178] |
| **Slip-scout** | plato | Placed when the Gnoth-caster cannot cast every gnoth, cannot generalise, or the candidate fails to postdict; makes a more detailed study of slipping to another reigning class. "Slip-scouts are only skeletally implemented in the current system, a partial explanation for its floundering in many cases when initial reformulation fails" [M86 p. 126]. | [M86 p. 126] |
| **empty-task** | — | Printed once, at startup of the third run; not described. | [M86 p. 170] |

The chain from Glom-scout to Plato-evaluator is the thesis's example of a terraced scan: these
tasks "perform increasingly extensive tests on target gloms, screening the gloms as potential
representatives of various Platonic classes" [M86 p. 66]; a glom that passes a test is
targeted for further evaluation, one that fails may be re-evaluated by other tasks, and gloms
that are not discernably Platonic are ignored or destroyed [M86 p. 66].

#### 2.5 From a term to a guess

The flow, assembled from the prose and read against the transcripts:

1. **A term is entered.** The glint of the new term becomes an active focus [M86 p. 47]. The
   transcripts show `Sparkler-plus` tasks on the new glint against earlier elements at once
   [M86 p. 164].
2. **Sparks, then bonds.** Sparklers make sparks; Testers turn sparks into Bonders; Bonders
   make bonds; each bond releases more Sparklers and some Glom-scouts [M86 pp. 63-65].
3. **Gloms.** Glom-scouts propose groupings (group, cover, fence); Glomtesters size them;
   Glommers create them and make them active foci; a Glom-inspector sends a Plato-scout; the
   scout dubs pure gloms, or a Plato-evaluator looks for pure parts, or a Burster or Dissolver
   destroys the glom [M86 pp. 65-70]. In the first run `3 7 3` the remote sameness bond between
   the two 3's leads to the fence glom `(3 7)` and a Cycle dubbing [M86 pp. 164-165].
4. **A template.** Dubbing creates a Template-scout; a Template-applier tries the dubbed
   structure on the whole sequence, loosening parameters; a template glom (a pseudo-glom) caps
   the top-level gloms; a Template-evaluator passes or rejects it; until a hypothesis exists,
   new terms are filtered past the template and a Template-reviewer handles misfits: "The
   filtering process is the first major top-down action performed by the system; the template
   level has taken control" [M86 p. 74].
5. **Gnoths and a hypothesis.** Acceptance of a template sets off a Gnoth-maker and a
   Hypothesizer [M86 p. 76]; the gnoths are the parse, one per hit of the hypothesis
   [M86 p. 77]; the hypothesis is a form plus a box: "A hypothesis has two components: a form
   or encapsulation of a pattern, and a box or active realization of the pattern" [MHN p. 5].
   Then a Call-term asks for the next term [M86 p. 165].
6. **Filtering.** Each new term is hypothesis-filtered (Hfilter). Agreement: the term is
   absorbed (`top-down glom glom4`), and the program ventures its guess (`I have a guess!`)
   [M86 p. 166]. Disagreement: validity is set to 0, sparks are released, a Gnoth-setter lays
   out gnoths from the old hypothesis [M86 p. 98], the old form is freeze-dried and its box
   destroyed [M86 p. 99], the system decides whether to stay with the reigning class or slip
   [M86 pp. 99-105], and Reformulators, Gnoth-operators and Bond-assessors reshape the gnoths
   until a Gnoth-caster produces a candidate that postdicts the terms, which becomes the new
   hypothesis; the system then calls for the next term [M86 pp. 105-125]. If casting fails, a
   Slip-scout is placed [M86 p. 126].
7. **The answer.** `ok` ends the run (`bye`); a refusal returns to step 6's reformulation path
   with the next terms [M86 pp. 166, 175-176].

### 3. What FCCA chapter 1 adds: the design intent

*Implemented in:* no module: this section is design intent, used only to justify decisions (D-018's three bond types from the microworld of 3.1; D-020's weight of the cover from the speed of glue in 3.5). The sampler of 3.4 is data in `seekwhence/sequences.py` (group `fcca`, D-070). Tests: `tests/test_dozen.py`.

Hofstadter's chapter is a retrospective on the *project* (1977 onward), written in 1995. It
is explicit that it does not describe Meredith's program: "I will not go into the Seek-Whence
notation in this article" [FCCA p. 56], referring readers to her writings; it credits
"Marsha implementing the Seek-Whence program ... the first incarnation thereof" [FCCA p. 51];
and it sums up the themes of "the years of work by myself, Marsha Meredith, and Gray Clossman
on the Seek-Whence project" [FCCA p. 84]. Everything in this section is therefore *intent*,
usable to justify a decision where the thesis is silent, never as a statement of what the
program did. Each subsection ends with what the thesis says on the same point.

#### 3.1 The austere microworld

Hofstadter's starting conviction was that pattern-finding is "close to the core, if not the
core, of intelligence" [FCCA p. 13], and his model of the activity was his own oscillation
among calculation, guesswork and revision when extrapolating a sequence [FCCA p. 13]. He
pared the domain down step by step, dropping exponentiation, then multiplication and
division, then addition and subtraction, because each left sequences needing "great
math-savvy to decode" [FCCA pp. 48-49]. What remained: "I still had the integers themselves,
starting with 0 (with subtraction gone, negative numbers were totally banished). I still had
the notion of equality and the notion of immediate adjacency -- that is, successorship and
predecessorship" [FCCA p. 49]; "the possibility of clustering numbers together according to
shared properties, and the idea of clustering packets together into larger packets -- in other
words, the idea of hierarchical perceptual structures" [FCCA p. 49]; and "the ability to use
numbers to count -- to count, for instance, the number of numbers in a packet, or the number
of little packets in a larger packet, or even the number of hierarchical levels in a packet"
[FCCA p. 49]. Music, with scales, clusters, rhythms and a crude counterpoint of voices taking
turns, became the second source of sequences [FCCA pp. 49-51]; the fragment
`EAEAEBEBECECEDEDEEEEEFEF`, two voices colliding on E, shows one pitch playing several roles
[FCCA p. 50].

*In the thesis:* the same domain (nonnegative integers; sameness, successorship,
predecessorship; nested groups) [M86 pp. 36-38], with the integer's roles listed on p. 36
(section 1.3). The thesis adds the explicit ban on derived sequences [M86 p. 8].

#### 3.2 Packets, templates, segmentation and unification

In a sequence of small integers "the packets are not explicit, and therefore have to be
discovered (or created, if you prefer)" [FCCA p. 47]. There is no unique correct packeting:
`1 1 2 2 3 3 4 4` can be `(1 1) (2 2) (3 3) (4 4)` or `1 (1 2) (2 3) (3 4) (4 5)`, the latter
with "a glitch in front" but coherent, whereas `1 ((1 2) 2) (3 3) (4 4 5)` is incoherent and
cannot be extrapolated, i.e. made into a template [FCCA p. 47]. On esthetics: "people are more
inclined to go for packets built on sameness than ones built on successorship-leaps"
[FCCA p. 47]. Two activities are the crux of pattern-sensitive extrapolation: segmentation,
"figuring out where the boundaries of packets ought to lie" [FCCA p. 48], and unification,
"figuring out how the packets are related to one another" [FCCA p. 48], intertwined and hard
to tease apart [FCCA p. 48]. A template in FCCA is the bracket notation `[n n]`, `[2 n 2]`
[FCCA pp. 46, 52].

*In the thesis:* packets are gloms (bottom-up, ephemeral) and gnoths (the parse,
"parenthesization" in the glossary: `(1 2) (1 2 3)` is two gnoths [M86 p. 181]); segmentation
is glomming and the gnoth operations; unification is dubbing, templates and the hypothesis
form. The thesis's "template" is the program's proto-hypothesis [M86 p. 182], and its parsed
Blackburn list uses asterisks where FCCA uses brackets [M86 p. 7].

#### 3.3 The theme song

The project's "theme song" is `2, 1, 2, 2, 2, 2, 2, 3, 2, 2, 4, 2, 2, 5, 2, 2, ...`, built from
the two-voice musical example and the continued fraction for e, "not intended to be enormously
hard -- just hard enough to bring up the most central issues" [FCCA pp. 51-52]. Presented one
term at a time, the run of 2's produces "a mounting sense of strain and discomfort"; the 3 and
the 4 reveal the different 2 among its siblings, a primitive polyphony of a voice singing
"2 2" against an ascending scale [FCCA p. 52]. Packets that keep "2 2" intact leave an initial
glitch; "the only remedy is a systematic boundary-shift that repeatedly cleaves the sequence
right between adjacent 2's, violating what seem like the most natural-seeming chunks"
[FCCA p. 52], giving `(2 1 2) (2 2 2) (2 3 2) (2 4 2) (2 5 2)` and the symmetric template
`[2 n 2]` [FCCA p. 52]. A formulaic extrapolator with an interleaving operator would solve it
trivially from ten terms, and teach nothing about perception [FCCA pp. 52-53]. The sequence is
also the argument against a deterministic plateaus-first strategy, which would "fall into the
trap of building the perceptually immediate and appealing" `22222` island, "a hallucination,
a decoy" [FCCA p. 59].

*In the thesis:* this is Blackburn #3 [M86 p. 6]; the program has not attempted it, "There is
far too much interference -- terms having multiple potential roles" [M86 pp. 145-146]; it is
"one of our favorite examples. It has been, and continues to be, a distant goal" [M86 p. 146].
Chapter 1 already makes the point that the `(C-group 2 5)` "must not be viewed as such. It
has to be torn apart, and its pieces recombined with other sequence fragments" [M86 p. 24].

#### 3.4 The sampler

Hofstadter's sampler of domain sequences [FCCA pp. 53-54], read on the page images; the
descriptions are his:

| Sequence [FCCA pp. 53-54] | His description |
|---|---|
| `2 2 2 3 3 2 2 2 3 3 2 2 2 3 3` | a "hemiolic bi-cycle": a trio of 2's alternating with a duo of 3's |
| `1 0 0 1 1 1 0 0 0 0 1 1 1 1 1` | n copies of either 1 or 0, alternately |
| `1 0 0 1 5 1 0 0 0 5 1 1 1 1 5 0 0 0 0 5 ...` | the same, with every fifth term replaced by 5 |
| `1 2 0 2 0 2 1 3 1 3 1 2 0 2 0 2 0 3 0 3 ...` | interleaving the first two sequences |
| `1 2 2 3 3 4 4 5 5 6 6 7 7 8` | template `[n n+1]`; or the more appealing `[n n]` with the initial 1 as a glitch |
| `1 1 2 1 2 3 1 2 3 4 1 2 3 4 5 ...` | template `[1 2 ... n-1 n]`, ascending scales of length n |
| `2 1 3 1 2 4 1 2 3 5 1 2 3 4 6 ...` | ascending scales with the next-to-last note omitted |
| `1 2 1 3 2 1 4 3 2 1 5 4 3 2 1 ...` | template `[n n-1 ... 2 1]`, descending scales |
| `3 2 1 0 1 2 3 2 1 0 1 2 3 ...` | "sawtooth sequence", periodic with template `[3 2 1 0 1 2]` |
| `0 0 0 1 1 2 4 5 5 6 6 6 7 7 8 10 11 11 ...` | "sawtooth plateaus": each pitch repeated as many times as the sawtooth dictates |
| `1 1 2 1 1 2 3 2 1 1 2 3 4 3 2 1 ...` | a "mountain chain", template `[1 2 ... n ... 2 1]` |
| `1 1 2 3 1 2 2 3 1 2 3 3 1 1 2 3 ...` | the "marching doubler" |
| `1 1 2 3 1 2 2 3 1 2 3 3 1 2 2 3 1 1 2 3 ...` | the "bouncing doubler" |
| `1 2 2 3 3 1 1 2 3 3 1 1 2 2 3 1 2 2 3 3 ...` | the "marching singler", a figure/ground reversal of the marching doubler |
| `(1-22-33 1-22-3 1-2-33) (11-2-3 11-2-33 1-2-33) ...` | the "doubler/singler spliceroo", given pre-parsed |

These are "the haiku, or perhaps the bonsai, of sequence-extrapolation puzzles" [FCCA p. 55].
Many need "moving parts" that simple templates cannot express [FCCA p. 55]; representing the
bouncing doubler as one long period misses its internal structure [FCCA p. 55]; and a
representation must show what is central and what is "slippable" if variations are to be
made [FCCA p. 55].

*In the thesis:* the ascending scales are Blackburn #1 [M86 p. 6]; `1 2 2 3 3 4 4 5 5` is #8
[M86 p. 6], and the thesis solves it [M86 p. 146]; the marching doubler is Clossman's and
needs the unimplemented instruction boxes [M86 pp. 19, 23, 36]; sequences with growing group
lengths such as #9 strain the representation [M86 pp. 146-147]. The reconstruction's sampler
of test sequences (item 20) can draw on this list, with the expectation that most are beyond
the 1986 program.

#### 3.5 Islands of order, glue, and the speed of glue

To organise `1 1 2 1 1 1 2 3 2 2 1 1`, the first twelve terms of a lopsided mountain chain
[FCCA p. 57], one locates "islands of order", stretches with their own internal logic
[FCCA p. 58]:

| Island [FCCA p. 58] | Examples | Held together by |
|---|---|---|
| plateaus | `44`, `111` | "sameness glue" |
| up-runs | `12`, `456` | "successorship glue" |
| down-runs | `21`, `654` | "predecessorship glue" |
| palindromes | `5885`, `71617` | mirror symmetry (no local glue) |

Islands can sprout in many ways, most of them mixing parts of two mountains, "But a priori,
how could one know that?" [FCCA p. 58]: the search must start locally. On salience: "plateaus
are the most salient type of island since sameness is such an elemental perceptual fact"
[FCCA p. 58]; "Runs come next, with palindromes being somewhat subtler, since recognition of a
palindrome requires noticing long-distance relationships" [FCCA p. 59]. But a model that let
plateaus always emerge first, then runs, then palindromes "would be very rigid and inefficient"
and fatal for the theme song [FCCA p. 59]. The balance between chaos and rigidity "can be
struck by employing parallel processing with probabilistic biases. The way this works is to
let perceptual glue of various sorts bubble up in parallel in different regions of the
sequence, with a tendency but not a rule for sameness glue to emerge the fastest, successorship
and predecessorship glue a bit more slowly, and so forth" [FCCA pp. 59-60]. Each dab of glue is
a small local pressure toward an island of a type at a place; palindromes, lacking local glue,
need a more complex treatment [FCCA p. 60]. "Glue alone does not make an island come into
existence; it merely serves as a hint or suggestion to build an island of a certain sort in a
certain region" [FCCA p. 60]; an island is a commitment, yet even a fully built one "can be
sacrificed, under pressure, for the greater good" [FCCA p. 60].

*In the thesis:* glue is the bond (sameness, successorship, predecessorship, adjacency,
meeting, in families [M86 p. 64]); the island is the glom; the probabilistic bias is the
taskrack's random choice by urgency [M86 p. 42], and the speed order is implemented at least
in the Glomtester's weights, sameness above successorship, "biased toward noticing certain
similarities first, yet it is not compelled to do so" [M86 p. 67]. The Glom-scout's three
quick tests, groupable, coverable and fenceable [M86 p. 66], are the thesis's plateau-or-run,
palindrome, and remote-bond islands; the thesis's "cover" (symmetry) grouping is its treatment
of Hofstadter's glueless palindromes. The thesis gives no urgency for Sparklers, Testers or
Bonders by bond type; where the reconstruction needs one, Hofstadter's order (sameness
fastest, successorship and predecessorship slower) is the design intent to cite, as a
decision.

#### 3.6 Perceiving islands, affinities, mini-analogies

A second tier of perception works on the islands themselves, leading to multi-level packets and
templates [FCCA p. 60]. An island has more to it than its category: `1111111` is a plateau of
1's, possibly of length 7; `23456` an up-run beginning on 2 and ending on 6, possibly of length
5, less likely centred on 4; "an island of order is a little structure that can be
characterized by a name and one or more parameters" [FCCA p. 60], the parameters having
different probabilities of being perceived [FCCA pp. 60-61]. Noticing a parameter in one island
raises the chance of noticing it in others, and a successorship connection between central
elements reinforces attention to central elements and to successorship [FCCA p. 61]. The two
tiers are not serial: "many little agents, each one searching in its own small area for
appealing samenesses or other types of connections, none knowing about any of the others"
[FCCA p. 61], like an ant colony. Islands are attracted to one another; in `11 34 22 567` the
affinity between `11` and `22` is strong, between `11` and `567` nil; the affinities come from
"being close to each other in the sequence, belonging to the same category, and sharing
parameters" [FCCA p. 62]. Linking two islands is a mini-analogy with a salience and a strength,
the strength a collective function of the affinities; mating `11` with `22` pushes for mating
`34` with `567` and sets up the expectation of length-2 plateaus elsewhere; "variabilizations"
(n+1 copies of n, or n copies of n) are plausible guesses about which constants are variables
[FCCA p. 62].

*In the thesis:* the island's name and parameters are the glom's `type` and `print-value`,
`span`, `start-position` [M86 p. 62, item 02] and, after dubbing, its `structure`
`(platotype [start-val] [length] [value])` [M86 p. 69]; affinities are the bond-fields,
"Typically, the print-value and span are useful bond-fields" [M86 p. 65], and the bond
modifiers (adjacent stronger than remote) [M86 pp. 64-65]; the inter-island analogy is the
remote bond, and the template is the thesis's unification of the gloms. The thesis does not
let discoveries raise the salience of a parameter ("in practice this ability is not yet used"
[M86 p. 65]).

#### 3.7 Bottom-up invaded by top-down

A perception should exert influence by raising the likelihood of similar perceptions and
weakening the commitment to dissimilar structures (paraphrased from [FCCA p. 63]): "probabilistic biases guiding the search for regularities should be altered on
the basis of discoveries already made" [FCCA p. 63]; finding two length-6 palindromes should
bias the search toward more of that length, a bias that would be absurd at the start
[FCCA p. 63]. So the process is "a perceptual process that begins in a pure bottom-up manner
but that is gradually invaded by increasing amounts of top-down influence" [FCCA p. 63];
bottom-up is data-driven, top-down is theory-driven [FCCA p. 63]. The mountain chain is then
solved by the up-runs `(1 2 3)` and `(1 2 3 4)` begetting a search for `(1 2)` to their left,
by bottom-up plateaus `(1 1)` and `(2 2)` reinforcing each other, by a theme of length-2
plateaus related by successorship, and by a tentative template `[[1 2 ... n] [n-1 n-1] ???]`
whose question marks focus attention on `(1)` and `(2 1)` until the abstraction "a down-run
ending on 1, whose length is the predecessor of the number in the plateau to its left" is
found, giving `[[1 ... n] [n-1 n-1] [n-2 ... 1]]` (paraphrased from [FCCA pp. 64-65]).

*In the thesis:* top-down starts with template filtering, "the first major top-down action
performed by the system; the template level has taken control" [M86 p. 74], and continues with
hypothesis filtering [M86 p. 47], the Gnoth-setter and the Reformulator [M86 pp. 98, 105].
Chapter 5's self-criticism of the balance between the levels (its sections on low-level
myopia and high-level haughtiness) is read in item 04. Whether the probabilistic biases
themselves (bond strengths, slipperiness) change during a run is settled in item 03 from
pp. 90-91.

#### 3.8 Polishing, generalisation and variations

After the template fits, the first term `1` remains unexplained; patching the template by
moving an `n-1` from the plateau into the down-run, and then both of them, removes the glitch
only by stretching the meaning of a down-run to start on a pair, a notation that only a being
with very fluid concepts "could gracefully handle this type of informal, almost sloppy,
notation-stretching" [FCCA p. 66]. Generalisation starts from one event or
structure: "generalization outwards from a single event or structure plays a key role"
[FCCA p. 77], the simplest case being the variabilization of `(3 4)` into `[n n+1]`
[FCCA p. 77]. Beyond substitution of variables, generalisation reconfigures an idea by moving
internal boundaries, swapping components or shifting substructures between levels, merging two
substructures or breaking one, lengthening or shortening a component, adding components or
levels, replacing a concept by a related one, and trying reversals [FCCA p. 77]. Such remedial
actions are called for "when one's current theory clashes with a new term, and thus is proven
to be wrong" [FCCA p. 78], when it cannot account for the sequence's beginning, and "when
one's current theory is judged esthetically unappealing" [FCCA p. 78]. Making variations on a
theme (the Chopin theme `1 2 2 3 3 4 4 5 5 6`, with its A-parsing on plateaus with a glitch and
its B-parsing on up-runs without one [FCCA p. 78], and the variants A1-A5, B1-B5, C1-C2, X, Y
and the last one [FCCA pp. 79-81]) was a goal that "always remained just a tantalizing dream"
[FCCA p. 82].

*In the thesis:* the reconfigurations are the gnoth operations, which the glossary lists as
SHIFT-LEFT, SHIFT-RIGHT, SPLIT, CAPTURE, ENCLOSE, FRACTURE, MERGE and NO-OP [M86 p. 181]
(item 03); the three triggers
are the thesis's medical reform (a hypothesis "fails to be predictive" [M86 p. 181]) and
cosmetic reform (reformulation "for aesthetic reasons -- to give it a cleaner form"
[M86 p. 180]), of which only medical reform is implemented and the need for cosmetic reform is
"evident in cases such as" Blackburn #12's ugly Y-group [M86 p. 147]. The A/B trade-off is
Blackburn #8 [M86 p. 6]. Variations and analogies between sequences are future work in the
thesis [M86 pp. 40-41].

#### 3.9 The eight themes

Hofstadter's list of the project's recurrent themes [FCCA pp. 84-85], paraphrased: (1) the
inseparability of perception and high-level cognition, a perceptual architecture at the heart
of cognition; (2) easily reconfigurable multi-level representations held loosely together by
bonds of different types and strengths; (3) subcognitive pressures, the more important a
concept the greater its probabilistic influence; (4) many pressures commingling in a
nondeterministic parallel architecture where bottom-up and top-down coexist; (5) the
simultaneous feeling-out of many pathways at rates set by quick estimates of promise;
(6) analogies and variations on a theme at the centre of high-level cognition; (7) deeper and
shallower aspects of representations, the shallower ones yielding under pressure, slipping;
(8) the inner structure of concepts and conceptual neighbourhoods. He adds that Jumbo and Numbo
took up themes (1) to (5) and that Copycat is the deeper model of concepts [FCCA p. 85].

*In the thesis:* (2) is the glom/gnoth/bond design and the bond strengths [M86 pp. 64-65];
(3) and (4) are the taskrack with integer urgencies and no overseer [M86 pp. 41-42]; (5) is
the terraced scan from Glom-scout to Plato-evaluator [M86 p. 66]; (7) and (8) are the Slipnet,
its s-links and their slipperiness (item 03). The thesis's own list of the Hofstadter notions it tried to realise
is on pp. 27-28: active symbols made of groupings of lower-level units, reformulation,
conceptual skeletons, slipping, fluid concepts, focusing and filtering, the terraced scan,
salience, roles, categories made on the fly, "non-cooperating processes and randomness in lieu
of an overseeing all-powerful agent" [M86 p. 27], and toy worlds [M86 pp. 27-28].

#### 3.10 How the reconstruction uses this chapter

Only as the source of design intent for decisions the thesis leaves open, each cited as
Hofstadter's intent in `DECISIONS.md`: the speed order of glue types where a task urgency by
bond type is needed (3.5); the affinity features (proximity, same category, shared parameters)
where a bond strength or spark preference is needed (3.6); the principle that glue is a hint
and an island a commitment, for the relative urgencies of Sparklers, Bonders and Glommers
(3.5); and the intent that discoveries alter biases (3.7), which is *not* implemented in the
thesis and, if ever added, is an extension off by default. The theme song (3.3) and the
sampler (3.4) are test material, with the thesis's own verdicts recorded beside them.

### 4. The platoplasm: ideal atoms and the eight ideal types

*Implemented in:* `seekwhence/forms.py` (the form language of 4.3 and 4.5: `Form`, `Shared`, parse and print; D-001, D-002), `seekwhence/platoplasm.py` (ideal atoms and manifestations of 4.2, the eight types, the hierarchy of 4.4, the generating and process functions, membership and purity, and `hits`/`terms`, the reference evaluation of every form of 4.6; D-003 to D-006). Tests: `tests/test_forms.py`, `tests/test_platoplasm.py`.

All of this section was read on the page images of pp. 48-59, 80-81, 84 and 161-163, except
where another page is cited.

#### 4.1 What the platoplasm holds

The platoplasm is the most abstract of the three levels. "Currently, the platoplasm houses the
ideal types -- the primitive, built-in notions available to the system for use in constructing
its view of a sequence" [M86 p. 48]; they are "its vocabulary for the well-structured 'phrases'
it constructs" [M86 p. 48]. Two things the thesis says are not there yet: new notions, "the
newly-constructed phrases, may eventually come to be housed in the platoplasm as first-class
citizens" [M86 p. 48], and "We are developing a network of relations among the ideal types to
aid the system's reformulation efforts" [M86 p. 48] (what exists of that network, the s-links
and the pull and push lists, is in ch. 3; section 11). So the platoplasm of the program is a
fixed vocabulary: the ideal atoms and the eight non-atomic ideal types, each with its lists of
manifestations (gloms) and, from section 7 on, actualizations (gnoths).

#### 4.2 Ideal atoms

"The ideal-atoms are Seek-Whence analogues to the integers entered at the keyboard. An
ideal-atom has predecessor and successor values, its own value, and manifestations in the
cytoplasm" [M86 p. 48]. The example: ideal5 has predecessor ideal4 and successor ideal6, "while
'ideal0' has no predecessor but has 'ideal1' as successor" [M86 pp. 48-49]. Glints are the
manifestations: in the fragment `5 0 5` "the first and last terms are manifestations of ideal5,
while the middle term is a manifestation of ideal0" [M86 p. 49]. When a term is entered the
system creates a glint for it "and then lists that glint as a manifestation, or cyto-level
analogue, of the appropriate ideal-atom in the platoplasm" [M86 p. 61].

Other integer-valued quantities, in particular group lengths, could also be manifestations
(the length of `(4 4 4 4)` "might prove to be an important manifestation of ideal5"
[M86 p. 49]), but "In the current version of the system, however, only glints are referenced
as manifestations of ideal atoms -- a simplifying (and weakening) design decision"
[M86 p. 49]. The reconstruction keeps the limitation (see also section 1.3 and [M86 p. 147]).

#### 4.3 The eight non-atomic ideal types

"There are eight non-atomic ideal types, each of which is associated with a format having one
or more active parameters: (typename start-value length actual-value)" [M86 p. 49]. Optional
parameters are shown in square brackets. A *form* is an instantiation of a format; "When such
a form is queried -- or 'hit', as we say -- it returns a value" [M86 p. 49], and the thesis
shows the results of successive hits on separate lines. The types "correspond quite closely
(but not exactly) to the Seek-Whence diagram primitives introduced in Chapter One"
[M86 p. 49] (section 1.2).

| Type | Described as | Format | Pages |
|---|---|---|---|
| **Constant** | "a structure that always returns one value, its argument, when queried" [M86 p. 50] | `(Constant arg)` | 50, 56 |
| **Countup** | "a structure that returns nonnegative integers in succession, starting with its argument, when queried" [M86 p. 50] | `(Countup n)` | 50, 56 |
| **C-group** (Copy-group) | "a structure that returns a number of copies of a given argument" [M86 p. 51] | `(C-group start length)` | 51, 56 |
| **S-group** (Successor-group) | "a structure that returns a given-length run of successive integers, starting with a given value" [M86 p. 51] | `(S-group start length)` | 51, 56 |
| **P-group** (Predecessor group) | "a structure that returns a given-length downward progression of nonnegative integers, starting with a given value" [M86 p. 52]; a run that would go below 0 is undefined | `(P-group start length)` | 52, 56 |
| **Y-group** (Symmetry group) | "a structure that returns a given group of nonnegative integers, symmetric about the center" [M86 p. 52] | `(Y-group [start] [length] actual)` | 52, 55 |
| **Cycle** | "a structure that cycles through its actual parameter's value, returning one top-level element each time queried" [M86 p. 53] | `(Cycle actual)` | 53, 55 |
| **Tuple** | "a structure that returns its actual parameter's value each time queried" [M86 p. 53] | `(Tuple actual)` | 53-55 |

Three facts about the formats that the later code must respect:

- **Arguments may be forms.** `(C-group (Countup 1) 2)` gives `(1 1)`, `(2 2)`, `(3 3)`
  [M86 p. 51]; `(S-group (Countup 1) 2)` gives `(1 2)`, `(2 3)`, `(3 4)` [M86 p. 51];
  `(Y-group ((Countup 1) 8 (Countup 1)))` gives `(1 8 1)`, `(2 8 2)`, `(3 8 3)` [M86 p. 52];
  `(Cycle (3 (Countup 1)))` gives `3 1 3 2 3 3 3 4` one term per hit [M86 p. 53]. A nested
  form is hit each time the enclosing structure needs its value, so the Countup inside a
  group advances once per hit of the group, and the Countup inside a Cycle advances once per
  turn of the Cycle.
- **Fence forms carry the start and length too, in the program.** The prose writes
  `(Cycle (2 1 8))` and `(Tuple (1 8 4))` [M86 p. 53], but the program prints
  `(Cycle 3 2 (3 7))` and `(Cycle 3 3 (3 7 3))` in the transcripts [M86 pp. 166, 169] and
  `(Tuple 1 3 (...))`, `(Cycle 1 3 (...))` in the appendix [M86 pp. 162-163], and the
  dubbing structure is `(platotype [start-val] [length] [value])` [M86 p. 69]. The
  reconstruction's internal form therefore has all four slots, with start and length optional
  for the fence and symmetry types; how the start of a Cycle is chosen (the first element, in
  every example) is a decision (item 05).
- **Hits are the unit of prediction.** Every example lists one value per hit: a single integer
  for the repeater types and Cycle, a list for the group types and Tuple. A gnoth "represents
  one 'hit' of the current hypothesis" [M86 p. 77] (section 7) and the guess prints the next
  hits (section 8.3).

#### 4.4 The hierarchy of ideal types

The eight types "fall into a hierarchy of categories, each of which captures an important
organizing notion for the Seek-Whence world" [M86 p. 54]. From the top:

| Category | Members | What distinguishes it | Page |
|---|---|---|---|
| **repeater** | Constant, Countup | "one-parameter generate types; given the single parameter (and the state), the next value can be generated" [M86 p. 54] | 54, 56 |
| **generate** | C-group, S-group, P-group | from the start and length "the actual value (e.g., (1 1 1)) can be generated by the associated 'generating function'" [M86 p. 54]; every generate type also has a process | 54, 55-56 |
| **process** | Y-group | "Possesses a 'process', a method of determining whether or not some actual group is a representative of the class without reference to any information external to the group and the class in question" [M86 p. 54]; it has no generator | 54, 55 |
| **fence** | Cycle, Tuple | "Has no generator, no process; virtually any collection of neighboring terms can be called a group by virtue of these types" [M86 p. 54]; "the terms are 'fenced off' into a group by their neighbors" [M86 p. 55] | 54-55 |

The fence types are "the least restrictive types" [M86 p. 55], used when the system "cannot
classify them as being higher-level types" and "can set up a 'fence', identifying the group as
a group" [M86 p. 55]. The example is `1 5 8 3 2 5 8 3 3 5 8 3 4 5 8 3 ...`, where `5 8 3` is
grouped "simply because of the interleaved 1, 2, 3,... and the fact that the group repeats"
[M86 p. 55], probably as `(Tuple (5 8 3))`; recognising the repetition requires first
identifying the group, and the repetition confirms the group. A whole sequence can be a fence
form: `4 7 4 7 ...` "can be represented as (Cycle (4 7)), with an understood repetition"
[M86 p. 55]. Tuple is "the weakest of all plato-classes" [M86 p. 79] and the Hypothesizer's
fall-back (section 8.1).

The process type's form `(Y-group 1 5)`, a Y-group of length 5 starting with 1, "is not
sufficient to generate a unique symmetry group, but is sufficient to determine that
(1 7 4 7 1) is such a Y-group whereas (1 8 2 5 1), (1 7 1), and (2 0 0 0 1) are not"
[M86 p. 55]. (Of the three rejected groups, `(1 7 1)` has the right start and symmetry but
the wrong length, `(2 0 0 0 1)` is asymmetric and starts wrong, `(1 8 2 5 1)` is asymmetric.)

The generate types "can use two parameters, the starting value and length, to generate a
particular grouping representative of the given type" [M86 pp. 55-56]: `(C-group 1 5)` is
`(1 1 1 1 1)`, `(S-group 2 3)` is `(2 3 4)`, `(P-group 9 4)` is `(9 8 7 6)` [M86 p. 56]. "Each
repeater type takes one parameter" [M86 p. 56]: `(Constant 2)` always returns 2, `(Countup 3)`
returns 3 on the first request, then 4, 5, and so on [M86 p. 56].

**Process functions.** The Plato-scout applies "the process function for each candidate class
to the glom's print-value" [M86 p. 68], and "all but the fence-type plato-classes (Tuple and
Cycle) possess a 'process' -- a function which, when given a number string, determines whether
or not the string is an instantiation of the class" [M86 p. 68]. The thesis does not print the
process functions; their content follows from the definitions of the types (all copies of one
value; successive integers; descending integers not below 0; symmetric about the centre), and
the four Glomtester examples and the `(Y-group 1 5)` example above are the test vectors. What
the processes of the repeater types accept (a single term?) is not stated; a decision (item 05).

**Strictness.** The thesis calls a class higher in this hierarchy *stricter*: slipping "to a
less strict class" is "vertical" slippage and to "a related class" is "lateral" slippage
[M86 p. 59]; `1 1 1` "can be viewed as a Tuple, it is also a C-group and should generally be so
characterized" [M86 p. 59], and the imperative to use "the strictest appropriate class" is
stated as future work [M86 p. 59]. The intended relations between classes, "Ideal-relations"
with lateral links (C-group to S-group for groups of length one) and vertical links (Countup
to S-group), "will be stored in the platoplasm" [M86 p. 59]; what the program has instead is
the s-links of ch. 3 (section 11).

#### 4.5 Combining ideals and `shared`

"Ideal types can be combined to create structures which encapsulate fairly intricate
patterns" [M86 p. 56]. Shared substructure "is indicated by the word 'shared', with an arrow
pointing to the first instance of the structure to be shared" [M86 p. 56]; the arrow is drawn
on the page (pp. 57, 58, 84, 161-163) and the reconstruction's printed form must carry the same
information without it (a decision, item 05: a `Shared(index)` marker, D-002). Each line of
the examples is one hit [M86 p. 56]:

- `(S-group 1 (Countup 2))` gives `1 2`, `1 2 3`, `1 2 3 4`, "thus giving the sequence:
  1 2 1 2 3 1 2 3 4 ..." [M86 p. 56].
- `(C-group (S-group 1 (Countup 1)) 2)` gives `(1 1)`, `((1 2) (1 2))`, `((1 2 3) (1 2 3))`,
  giving `1 1 1 2 1 2 1 2 3 1 2 3 ...` [M86 p. 57]. (The first hit is printed `(1 1)`, not
  `((1) (1))`: a group of length one is printed as its term.)
- `(Cycle (8 (Countup 1) shared))` gives `8 1 2 8 3 4`, giving `8 1 2 8 3 4 8 5 6 ...`
  [M86 p. 57]: the shared Countup is advanced at both of its places in the cycle.
- `(Tuple (2 (Countup 1) 2))` gives `2 1 2`, `2 2 2`, `2 3 2`, giving
  `2 1 2 2 2 2 2 3 2 2 4 2 2 5 2 ...` [M86 p. 57]; `(Tuple (2 (Countup 1) shared))` gives the
  same hits and the same sequence [M86 p. 58].

"The difference between the latter two representations is subtle but can be important. In the
last one, the sameness of the 'bracketing' 2's is made explicit" [M86 p. 58], which shows in
generalisation: `(Tuple (5 (Countup 1) 2))` is "a possible generalization of the first
representation because the bracketing integers are seen as distinct" [M86 p. 58] and gives
`5 1 2`, `5 2 2`, `5 3 2`; the shared form generalises to `(Tuple (5 (Countup 1) shared))`,
giving `5 1 5`, `5 2 5`, `5 3 5` [M86 p. 58]. The mechanism behind `shared` (one printstructure
feeding two boxes) is in section 8.2.

#### 4.6 Every example form in the thesis, with the values it produces

The table collects every form the thesis evaluates or pairs with terms, for the tests of the
form evaluator (item 06). "Hits" lists the successive values; a list value is one hit. "Terms"
is the flattened sequence where the thesis prints it. Forms that the thesis only names as the
description of a segment (pp. 59, 80-81) are included with the segment; their hits are
derived from the definitions and marked *derived*. All pages were checked on the images.

| Form | Hits | Terms | Page |
|---|---|---|---|
| `(Constant 3)` | 3; 3; 3; ... | | 50 |
| `(Countup 3)` | 3; 4; 5; ... | | 50, 56 |
| `(Countup 8)` | 8; 9; 10; ... | | 50 |
| `(Constant 2)` | 2; 2; ... | | 56 |
| `(C-group 2 3)` | (2 2 2); (2 2 2); ... | | 51 |
| `(C-group (Countup 1) 2)` | (1 1); (2 2); (3 3); ... | | 51 |
| `(S-group 2 3)` | (2 3 4); (2 3 4); ... | | 51, 56 |
| `(S-group 5 4)` | (5 6 7 8); (5 6 7 8); ... | | 51 |
| `(S-group (Countup 1) 2)` | (1 2); (2 3); (3 4); ... | | 51 |
| `(P-group 8 4)` | (8 7 6 5); (8 7 6 5); ... | | 52 |
| `(P-group 2 4)` | undefined: "would run to negative numbers" | | 52 |
| `(Y-group 1 5 (1 8 3 8 1))` | (1 8 3 8 1); (1 8 3 8 1); ... | | 52 |
| `(Y-group ((Countup 1) 8 (Countup 1)))` | (1 8 1); (2 8 2); (3 8 3); ... | | 52 |
| `(Cycle (2 1 8))` | 2; 1; 8; 2; 1; ... | | 53 |
| `(Cycle (3 (Countup 1)))` | 3; 1; 3; 2; 3; 3; 3; 4; ... | | 53 |
| `(Tuple (1 8 4))` | (1 8 4); (1 8 4); ... | | 53 |
| `(Tuple (2 2))` | (2 2); (2 2); ... | | 54 |
| `(C-group 1 3)` | (1 1 1) | `1 1 1` | 54, 80, 82 |
| `(C-group 1 5)` | (1 1 1 1 1) | | 56 |
| `(P-group 9 4)` | (9 8 7 6) | | 56 |
| `(Y-group 1 5)` | accepts (1 7 4 7 1); rejects (1 8 2 5 1), (1 7 1), (2 0 0 0 1) | | 55 |
| `(Tuple (5 8 3))` | (5 8 3) | the group in `1 5 8 3 2 5 8 3 3 5 8 3 4 5 8 3` | 55 |
| `(Cycle (4 7))` | 4; 7; 4; 7; ... | `4 7 4 7 ...` | 55 |
| `(S-group 1 (Countup 2))` | (1 2); (1 2 3); (1 2 3 4); ... | `1 2 1 2 3 1 2 3 4 ...` | 56, 179 |
| `(C-group (S-group 1 (Countup 1)) 2)` | (1 1); ((1 2) (1 2)); ((1 2 3) (1 2 3)); ... | `1 1 1 2 1 2 1 2 3 1 2 3 ...` | 57 |
| `(Cycle (8 (Countup 1) shared))` | 8; 1; 2; 8; 3; 4; ... | `8 1 2 8 3 4 8 5 6 ...` | 57, 84 |
| `(Cycle (8 (Countup 1) (Countup 1)))` | 8; 1; 1; 8; 2; 2; ... (*derived*) | `8 1 1 8 2 2 8 3 3 ...` | 84 |
| `(Tuple (2 (Countup 1) 2))` | 2 1 2; 2 2 2; 2 3 2; ... | `2 1 2 2 2 2 2 3 2 2 4 2 2 5 2 ...` | 57 |
| `(Tuple (2 (Countup 1) shared))` | 2 1 2; 2 2 2; 2 3 2; ... | same | 58 |
| `(Tuple (5 (Countup 1) 2))` | 5 1 2; 5 2 2; 5 3 2; ... | | 58 |
| `(Tuple (5 (Countup 1) shared))` | 5 1 5; 5 2 5; 5 3 5; ... | | 58 |
| `(S-group 1 (Countup 1))` | (1); (1 2); (1 2 3); (1 2 3 4) (*derived*) | `1 1 2 1 2 3 1 2 3 4` | 59 |
| `(Cycle (2 (Countup 1)))` | 2; 1; 2; 2; 2; 3; 2; 4 (*derived*) | `2 1 2 2 2 3 2 4 ...` | 59 |
| `(C-group (S-group 4 3) 2)` | ((4 5 6) (4 5 6)) (*derived*) | `4 5 6 4 5 6` | 80 |
| `(S-group (Countup 4) 3)` | (4 5 6); (5 6 7); ... "Each 'hit' or evaluation of this form would yield a length-3 successor group" [M86 p. 81] | `4 5 6 5 6 7` | 81 |
| `(Cycle ((Countup 1) (Tuple (5 8 4))))` | 1; (5 8 4); 2; (5 8 4); 3; (5 8 4) (*derived*) | `1 5 8 4 2 5 8 4 3 5 8 4` | 81 |
| `(Cycle ((Countup 1) 1))` | (1 1) (2 1) (3 1) ... as printed; one element per hit | `1 1 2 1` | 81 |
| `(Cycle (1 (S-group 1 2)))` | (1 (1 2)) (1 (1 2)) ... as printed | `1 1 2 1` | 81 |
| `(S-group 1 2)` | (1 2) (1 2) ... | `1 2 1` and the guess `(1 2)(1 2)(1 2)` | 81, 175 |
| `(Y-group [1] [3] (1 2 1))` | (1 2 1) (1 2 1) ... | `1 2 1` | 81 |
| `(C-group (S-group 2 3) 2)` | ((2 3 4) (2 3 4)) (*derived*; Figure 3) | | 82-84 |
| `(C-group (S-group 2 3) shared)` | the same, with the sameness of the 2's explicit | | 84 |
| `(C-group (S-group (Countup 1) 3) 2)` | ((1 2 3) (1 2 3)); ((2 3 4) (2 3 4)); ((3 4 5) (3 4 5)) | `1 2 3 1 2 3 2 3 4 2 3 4 3 4 5 3 4 5` | 96 |
| `(C-group (Countup 1) (Countup 1))` | (1); (2 2); (3 3 3) (*derived*: two independent Countups) | `1 2 2 3 3 3` | 97 |
| `(C-group (Countup 1) shared)` | (1); (2 2); (3 3 3); (4 4 4 4) | | 161 |
| `(Tuple 1 3 ((Countup 1) 8 shared))` | (1 8 1); (2 8 2); (3 8 3); (4 8 4) | | 162 |
| `(Cycle 1 3 ((Countup 1) 8 shared))` | 1; 8; 2; 3; 8; 4; 5; 8; 6 | | 163 |
| `(Cycle 3 2 (3 7))` | 3; 7; 3; 7 | the guess `3 7 3 7` after `3 7 3 7` | 166 |
| `(Cycle 3 3 (3 7 3))` | 3; 7; 3; 3; 7 | the guess `3 7 3 3 7` after the five terms `3 7 3 3 7` | 169 |
| `(Countup 1)` | 1; 2; ... | the first hypothesis for `1 2` | 171 |
| `(S-group 1 (Countup 2))` | (1 2); (1 2 3); (1 2 3 4) | the guess after `1 2 1 2 3 1` | 179 |

Three readings settled on the images: on p. 57 the page has no printed number but follows
p. 56 and precedes p. 58 (PDF page 69); on p. 84 the second form in the sentence about Figure 3
is printed `(C-group (S-group (2 3)) 2)`, with the inner parentheses, while Figure 3 itself is
captioned `(C-group (S-group 2 3) 2)`; and on p. 161 the program's form is
`(C-group (Countup 1) shared)` (the arrow runs from `shared` back to `(Countup 1)`), whose box
produces `(1)`, `(2 2)`, `(3 3 3)`, `(4 4 4 4)`, so a `shared` length means "the same Countup
printstructure as the start", advanced once per hit, not twice.

On p. 81 the thesis writes the Cycle's hits as `(1 1) (2 1) (3 1)` and `(1 (1 2)) (1 (1 2))`,
grouping one turn of the cycle per parenthesis, although by its definition a Cycle returns
"one top-level element each time queried" [M86 p. 53]; the parentheses show what one *frame*
of the hypothesis covers (section 7.4), not one hit. The transcripts' Cycle gnoths cover one
full turn each (`range: (1 2)` for `(Cycle 3 2 (3 7))` [M86 p. 168]). How many hits make a
frame of a Cycle is therefore a decision (item 05), with one full turn as the evidence.

### 5. The cytoplasm: glints, sparks, bonds and gloms

*Implemented in:* `seekwhence/cytoplasm.py` (glints, gloms, sparks and bonds with the fields of 5.2-5.4, dissolving and bursting of 5.10; D-014, D-059), `seekwhence/bonds.py` (what a term releases, the Sparkler, Sparkler-plus, Tester and Bonder of 5.4-5.6, the Dissolver; D-015 to D-019, D-024, D-025, D-058), `seekwhence/gloms.py` (the task chain of 5.8, the Plato-scout, dubbing and the Plato-evaluator of 5.9, the Burster; D-020 to D-024), `seekwhence/platoplasm.py` (the manifestation lists). Tests: `tests/test_cytoplasm.py`, `tests/test_bonds.py`, `tests/test_gloms.py`.

Read on the page images of pp. 60-70 and of the transcripts cited.

#### 5.1 The role of the cytoplasm

"The cytoplasm has the role of 'real world' in our 'Socratic model', in which the platoplasm
houses the analogues of Plato's Ideals and the socratoplasm is the analogue of Socrates'
'perceived world'" [M86 p. 60]. It "houses the lowest-level structures in the system -- the
sparks, bonds, glints and gloms -- and is the site of much upward-thrusting, relatively
uncritical activity" [M86 p. 60]. Its activity is "probabilistic, undirected" [M86 p. 60];
"Groupings are continually being generated and regenerated at this level" [M86 p. 60], and
looping is tolerated: "we will leave it to higher-level processes to notice and handle any
unproductive looping in cyto-level activity" [M86 p. 60]. "The cyto-level should bombard the
upper levels with suggestions, noted similarities, and groupings of terms. It is up to the
processes above to curb this enthusiasm and to consider the suggestions more carefully and
critically" [M86 p. 60].

"The four data types residing in the cytoplasm are Sparks, Bonds, Glints, and Gloms. The
former two classes are for finding, proposing, and later evaluating glom groupings. The latter
two classes, the Glints and Gloms, are used for representing the sequence terms and any term
groupings of current interest" [M86 pp. 60-61].

#### 5.2 Glints

"In Seek-Whence, glints are the cyto-level representations of the integers entered at the
keyboard" [M86 p. 61]. The fields, in the thesis's order [M86 p. 61]:

| Field | Meaning | glint3 of `1 2 2 3` (the second 2) |
|---|---|---|
| `class` | always `Glints` | `Glints` |
| `name` | `glintN`, numbered in order of entry | `glint3` |
| `print-value` | the integer | `2` |
| `position` | the term's position, counted from 1 | `3` |
| `span` | the number of terms covered, always 1 | `1` |
| `pred` (or `left-nbr`) | the glint to the left | `glint2` |
| `succ` (or `right-nbr`) | the glint to the right | `glint4` |
| `bonds-in` | optional: the bonds attached to the glint | (not shown) |

"The 'span' field is really unnecessary in glints, but is a consequence of the fact that the
Glints class is a subclass of the Gloms. It indicates that this glom is of length 1"
[M86 p. 61]. (The example lists `right-nbr: glint4` although only four terms have been entered
and glint4 is the 3; the neighbour fields name the glints to either side.) On entry the glint
is listed as a manifestation of its ideal atom: "glint3 would become a manifestation of ideal2
because its value is 2 and it represents an input integer" [M86 p. 61]; the glint "is then
examined by cyto-level tasks as described below to determine how it is related to other
cyto-level structures" [M86 p. 62].

"The class 'Glints' is a distinguished subclass of the class 'Gloms'. Each glint is
indestructible -- an 'atomic' glom" [M86 p. 62].

#### 5.3 Gloms

"Non-glint gloms are cohesive units, made of adjacent atoms bound by 'bonds' of one type or
another" [M86 p. 62]. Not every bonded chain becomes a glom: "some bond types (e.g., one
indicating that gloms (8 1 4) and (1 2 3) are of the same length) are generally not
considered strong enough to cause glomming, but are facts of note preserved for use by
higher-level processes" [M86 p. 62].

The fields [M86 p. 62], "The last two are optional, and are filled in when appropriate by
cyto-level processes" [M86 p. 62]:

| Field | Meaning | glom7 of `8 2 2 2 8` (the three 2's) |
|---|---|---|
| `class` | `Gloms` | `Gloms` |
| `name` | `glomN`, numbered in order of creation | `glom7` |
| `type` | the bond family, field and kind the glom was made on | `(Same print-value group)` |
| `print-value` | the list of the subgloms' print-values | `(2 2 2)` |
| `start-position` | position of its first term | `2` |
| `span` | number of terms covered | `3` |
| `positions-covered` | first and last position | `(2 4)` |
| `subgloms` | the gloms (here glints) it is made of | `(glint2 glint3 glint4)` |
| `structure` | optional: set by dubbing, `(platotype [start-val] [length] [value])` with the purity [M86 p. 69] | |
| `bonds-in` | optional: the bonds attached to the glom | |

The `type` field's three words are the bond family (`Same`, `Succ`, `Pred`), the glom-field
(`print-value`) and the kind of grouping proposed by the Glom-scout (`group`, `cover`, `fence`;
section 5.7): the transcripts print `glom1 (Same print-value fence) --> (3 7) terms 1 to 2`
[M86 p. 167] and `(Pred print-value group)` [M86 p. 171]. A pseudo-glom prints `pseudo` in
place of its type: `glom3 pseudo --> ((3 7)) terms 1 to 2` [M86 p. 167], `glom8 pseudo --> (1)
terms 3 to 3` [M86 p. 175].

Gloms are ephemeral. "Disappearance by dissolving (being destroyed as a unit, but with all
subgloms surviving intact), bursting (being destroyed as a unit and having all non-glint
subgloms burst as well -- leaving only the underlying glints), or glomming (being combined
with another glom) is fluid and continual" [M86 p. 62]. "The cytoplasm might be viewed as a
soup bubbling with gloms, the bubbles which rise to the top being the system's current view of
the sequence. If neighboring bubbles have enough mutual attraction (strong enough bonds) they
will combine; otherwise they will either exist independently or burst to permit new bubbles
to take their place" [M86 pp. 62-63]. The examples of destruction [M86 p. 70]: dissolving
`((1 1) (2 2))` leaves `(1 1)` and `(2 2)` floating free; bursting it leaves only the glints
`1`, `1`, `2`, `2`.

The glom's *print-value* nests as the subgloms nest (`((1 1) (2 2))`, `((3 7))` for the
pseudo-glom over `(3 7)`); a glom made only of glints is *flat* [M86 p. 69].

#### 5.4 Sparks

"Sparks and Bonds, two more cyto-level classes (the others being the Glints and Gloms discussed
above), are used during the early stages of group discovery" [M86 p. 63]. "A spark is created
between two gloms when a Sparkler task pulls those gloms at random from the cytoplasm and
determines in a very cursory way that the two structures might be amenable to bonding. The
Sparkler simply looks for gloms that are not subgloms of each other. It does not look for any
common features -- this is the work of other tasks" [M86 p. 63]. The examples of what later
tasks may find: glints `1` and `1` bondable by print-value; gloms `(1 2)` and `(1 2)` by
print-value or by span; the glint `1` and the glom `(2 3)` "by reason of adjacent
successorship" [M86 p. 63]; and "no glom can be bonded to one of its own subgloms, so the glom
'(1 (1 1))' cannot be bonded to the subglom '(1 1)' in any way" [M86 p. 63].

A spark's fields are not listed; it has a name (`sparkN`) and two members, printed as
`spark6 --- between glint3glint1` [M86 p. 167] (the newer element first in the transcripts;
`spark12 --- between glom1glint3` shows a glom as a member [M86 p. 167]). The same pair may be
sparked repeatedly (`spark1`, `spark2` both between glint1 and glint2 [M86 p. 167]). The
transcripts also show sparks released in a burst after a new term (five sparks after the term
7 [M86 p. 167]) and when a term denies the hypothesis (section 10).

#### 5.5 Testers and bond types

"When a spark is created between two gloms, a horde of 'Testers' is placed on the taskrack.
When invoked (chosen to run) at some later time, each Tester chooses some spark, not
necessarily the one whose creation caused the tester's creation" [M86 p. 64]. The spark's
members "are tested to ascertain if they are currently in existence (recall that gloms are
ephemeral). If both gloms exist, their bond-fields -- the characteristics such as print-value
or span which are important enough to be used as a rationale for bonding -- are intersected,
and these fields' values are tested for similarities" [M86 p. 64].

"The system uses several types of bonds -- sameness, successorship, predecessorship,
adjacency, and meeting (e.g., '(8 1 4)' and '(4 7)' 'meet' at 4) -- grouped into families, to
link gloms. The most important of these are, not surprisingly, sameness and
successor-predecessorship" [M86 p. 64]. The transcripts print three bond types, `Same`,
`Succ` and `Pred`, all on `print-value`, with the modifier `(adjacent)` or `(remote)`:
`bond1 Succ print-value (adjacent) -- (glint1 glint2)`, `bond2 Pred print-value (adjacent) --
(glint2 glint3)`, `bond3 Same print-value (remote) -- (glint1 glint3)` [M86 p. 174]. Which
bonds belong to which family is not spelled out beyond "successor-predecessorship" being one
family (the Glomtester example 4 names "the 'pred-succ' bond family" [M86 p. 67]) and sameness
another; adjacency and meeting bonds never appear in the transcripts. A decision (item 05).

"If a bonding test is passed, a 'Bonder' task is created with the intent of performing the
actual bonding. One Bonder task will be created for each bonding test passed by the two gloms,
so several Bonders might actually be created for any given glom pair. For example, gloms
'(1 2)' and '(1 2)' might engender both 'same print-value' and 'same span' Bonders"
[M86 p. 64].

#### 5.6 Bonding and bond strength

"When invoked, a Bonder 1) checks to see that both gloms still exist, and 2) checks to see
that they are not already bonded in this way" [M86 p. 64]. Then it "creates a Bond-class
structure, which we refer to simply as a bond. This bond, which exists in the cytoplasm, links
the two gloms and has a strength associated with it" [M86 p. 64]. "Bond strength is derived
from the bond type (e.g., 'sameness'), any bond modifiers (adjacent terms are more strongly
bonded than non-adjacent ones, for example) and the glom characteristic (e.g., 'print-value')
that is the subject of the bond" [M86 pp. 64-65]. No numbers are given here; ch. 3 gives the
pull and push strengths (1 to 10) that each Platonic class attaches to bond types, and the
transcripts print the bonds of a gnoth with values such as `(bond1 10)`, `(bond2 0)` and
`(bond3 -5)` [M86 p. 175] (section 11). So a bond's fields, as far as the thesis shows, are a
name, a type, the field it is on, a modifier, its two members and a strength. The bond is
printed when made: `bond1 --- between glint1glint3` [M86 p. 167], and the Bonder's task line
carries the type, field, modifier and members: `doing task Bonder on (Succ print-value
(adjacent) glint1 glint2)` [M86 p. 174].

"Creation of a bond causes the release of more Sparklers, stimulating the system to carry out
more low-level search and bond creation, and causes the release of some Glom-scouts -- tasks
designed to look for and push up glom groupings" [M86 p. 65]. How many of each is not stated.

#### 5.7 Glomming: bond-fields and glom-fields

"Bonds are created in order to provide some basis for the grouping of sequence terms (glints)
and term groups (gloms). The act of bonding simply reflects the fact that two gloms are related
in some way. Glomming, however, is performed only when the bonds among two or more gloms are
sufficiently strong that the system should view the items comprising the bond-chain as a unit"
[M86 p. 65].

"The system distinguishes between the 'bond-fields' of a glom and its 'glom-fields'.
Bond-fields are those characteristics of gloms that are to be compared for the purpose of
bonding. Typically, the print-value and span are useful bond-fields" [M86 p. 65]: `(1 2 3)` and
`(7 8 9)`, or `(1 2 3)` and `(8 1 4)`, "will generally be bonded" [M86 p. 65]. "Glom-fields are
the glom characteristics that are important enough to use for glomming purposes. Only
print-value is used as a glom-field in the current system" [M86 p. 65]. The reason is
historical: "In an early version of this system, we did allow such gloms. The result was a
plethora of uninteresting gloms that seemed to get in the way of the system's real work"
[M86 p. 65]. The system "can make chosen glom characteristics more salient by designating them
as bond-fields or glom-fields, or less salient by removing these designations; in practice
this ability is not yet used" [M86 p. 65]. The reconstruction keeps both lists fixed: bond-fields
print-value and span, glom-field print-value.

#### 5.8 The stage-one task chain: Glom-scout, Glomtester, Glommer, Glom-inspector

The chain from Glom-scout to Plato-evaluator is the thesis's example of a terraced scan
(section 2.4). Each task's checks, in the thesis's words:

**Glom-scout.** "When invoked, a Glom-scout chooses a cyto-element (a glint or glom which is
not a subglom of any other glom) and attempts to group it with its neighbors. Actually, three
quicktests tests are made for any bond-family in which the glom is involved: 1) is it
groupable? (bonded to any neighbors in this way?) 2) is it coverable? (bonded into a symmetry
group?) 3) is it fenceable? (are there remote gloms to which it is bonded?)" [M86 p. 66].
"Note that these tests are the precursors of the plato-level notions of generate, process, and
fence classes. Any tests passed cause creation of a Glomtester task to make a more extensive
test of the glom" [M86 p. 66]. The transcripts print the proposals as `Same -cover proposed
--> glint1`, `Same -fence proposed --> glint1` [M86 p. 167] and `Pred -group proposed -->
glint2` [M86 p. 170]: the bond family, the test and the element.

**The Glomtester's urgency** is the one urgency the thesis describes: "The Glomtester's weight
(urgency) is dependent on the test ('groupable' being strongest) and the bond-type involved
(sameness being stronger than successorship, and so on)" [M86 p. 66]. The example: in
`1 8 8 5 2` the two 8's "might be seen as a budding 'sameness group' because of their adjacent
sameness", while "the remote successorship between the 1 and the 2 might also be noted and
used to propose a 'successorship fence' group, one that would separate the given segment into
gloms '(1 8 8 5)' and '(2)'" [M86 p. 66], important if the sequence goes on
`1 8 8 5 2 8 8 5 3 8 8 5`, "but are not as immediately compelling as such groupings as the
pair of 8's. Therefore, a Glomtester for a sameness group is given a higher weight than a
predecessor fence Glomtester. The system is thus biased toward noticing certain similarities
first, yet it is not compelled to do so" [M86 p. 67]. The numbers are a decision (item 05).

**Glomtester.** "When invoked, a Glomtester task must first be certain that the glom it is
supposed to test is still in the cytoplasm. If so, it must then determine the extent of the
evolving glom. The thrust here is to get maximally-sized gloms" [M86 p. 67]. The four examples
[M86 p. 67], the test vectors for item 08:

| Terms | Target | Reason | Suggested group |
|---|---|---|---|
| `2 1 1 1 3` | the second 1 | sameness | `1 1 1` |
| `9 1 2 5 9` | the first 9 | fence | `9 1 2 5` |
| `5 3 1 6 1 3 4` | the 6 | symmetry | `3 1 6 1 3` |
| `1 2 3 2 3` | the first 2 | the pred-succ bond family | `1 2 3 2 3` |

The fence example groups the terms *between* the two bonded remote 9's together with the first
of them (the second 9 starts the next group); the symmetry example extends outward from the
centre as long as the mirror terms match; the pred-succ example takes the whole chain of
neighbours related by either successorship or predecessorship, which is why the Plato-scout
must later purify it (section 5.9). "The Glomtester either rejects the group as a glom or
creates a Glommer task to refine the group and perform the final glomming" [M86 p. 67].

**Glommer.** "When invoked, the Glommer will do a bit of 'bookkeeping'. It creates the new
glom and makes it an active focus -- a site of increased system activity. It also creates a
Glom-inspector task to continue pushing the glom up to higher levels" [M86 p. 67]. What "refine
the group" means between the Glomtester and the Glommer is not said; the transcripts show a
Glommer announced with its members, `Glommer for Same print-value fence members: (glint1
glint2)`, and failing: `failed to glom (glint1 glint2 glint3)` [M86 p. 167]. What makes a
Glommer fail is not said (the members in the example are not all bonded by sameness: the 7 is
between the two 3's); a decision (item 08).

**Glom-inspector.** "A Glom-inspector determines which, if any, plato-classes might find the
given glom 'interesting' -- which classes might possibly consider it a 'manifestation' of
themselves. If there are any such classes, the Glom-inspector then creates a Plato-scout task,
giving it the glom in question and the names of the 'interested' (candidate) plato-classes"
[M86 p. 68]. The transcript shows the candidates passed on: `doing task Plato-scout on
((S-group P-group) glom1)` [M86 p. 170] for a glom of type `(Pred print-value group)`. How a
class's interest is decided (from the glom's type, presumably: sameness groups interest
C-group, pred-succ groups interest S-group and P-group, covers interest Y-group, fences
interest Cycle and Tuple) is a decision (item 08).

#### 5.9 Plato-scouts, purity and dubbing

"The Seek-Whence system has an imperative to find analogues of its plato-classes" [M86 p. 68].
At the lower levels that drive is procedural; from the Plato-scout on "some manifest reference
is made directly to the ideal-types to begin, if not rejecting gloms, then favoring those that
seem the purest analogues of the ideals. When found, these special gloms will be 'dubbed'. All
others will be put on a track towards destruction" [M86 p. 68].

"During the glomming stage, maximally-sized groups of gloms -- all 'chains' that consist of
gloms related to their neighbors by some element of a bond family -- are collected. For
example, '1 2 3 2 3' could be a 'pred-succ-family' glom. The Plato-scout stage will now focus
on 'purifying' these groups" [M86 p. 68].

"A Plato-scout is given a glom and a list of candidate plato-classes. It applies the process
function for each candidate class to the glom's print-value. If the glom passes the test, then
the glom is pure and will be dubbed a manifestation of the candidate class. A glom can be
dubbed more than once. The glom '(1 1 1)', for example, might be dubbed as both a C-group
('copy-group') and a Y-group ('symmetry-group')" [M86 pp. 68-69]. "If the plato-class is a
fence type (and so has no process), and if the glom is 'flat' -- has only glints as subgloms
-- a 'pass by default' occurs. This permits flat potential Tuple and Cycle gloms to be dubbed
as such" [M86 p. 69].

"Any glom that does not pass even one of its process tests is cause for the creation of a
Plato-evaluator task. When invoked, the Plato-evaluator examines the glom a bit further, looking
for dubbable gloms of secondary purity (non-flat) and for pure subcollections of gloms within the
proposed target glom" [M86 p. 69]. "Thus '1 2 3 2 3' might well be sent to a Plato-evaluator,
which might break it into the two pure successorship groups '1 2 3' and '2 3', and send on the
gloms for these two groups to be dubbed" [M86 p. 69]. If it has no success "and the glom has not
already been dubbed as a manifestation of some other platotype, the scene is set for its
destruction. A Burster task is created to destroy the glom and all its non-glint subgloms" [M86 p.
69]. (How the Plato-evaluator finds the pure subcollections, by splitting the chain where the bond
type changes, is not said; a decision, item 09.)

**Dubbing** "is a two-step process: 1) the glom's structure field is modified to indicate
a) new structure: (platotype [start-val] [length] [value]) e.g., (C-group 1 3) or
(Cycle (2 9 8 1)) b) purity: pure <--> an exact manifestation; flat; e.g., (1 1 1)
secondary <--> not flat; e.g., ((1 5) (1 5) (1 5)) 2) the Platonic ideal has this glom added
to its list of manifestations" [M86 p. 69]. The transcript shows the structure with its purity
inside a gnoth: `form: (((Cycle 3 2 (3 7)) pure))` [M86 p. 168]. "Dubbing causes creation of a
Template-scout process, indicating that the glom is strong enough to warrant a check to
determine whether or not its structure, as described in the structure field added during
dubbing, might yield a pattern for the entire sequence" [M86 pp. 69-70].

#### 5.10 Bursters and Dissolvers: the end of the scan

"Those gloms that do not pass any of the tests leading to dubbing are targeted for destruction
by a Burster or a Dissolver task. A Dissolver is a task that destroys a glom, leaving its
highest-level subgloms to float independently in the cytoplasm" [M86 p. 70]; "A Burster task is
even more destructive of glom structure" [M86 p. 70], leaving only glints. "Once a Burster or
Dissolver has been created on a glom, the glom's only escape route is to become invisible in
the cytoplasm by glomming with another glom. Thus, after reaching the point of perusal by a
Plato-evaluator, a glom will either a) be dubbed; b) be destroyed and have some subgloms
dubbed; or c) be targeted for destruction. The Plato-evaluator creates no other tasks. Our
terraced scan has come to an end" [M86 p. 70]. Which of the two destroyers is chosen when is
not said beyond the Plato-evaluator's Burster; the transcripts show Dissolvers run on glints
(`doing task Dissolver on (glint1)` [M86 p. 164], a no-op since glints are indestructible) and
on gloms that are already gone (`glom3 is not in cytoplasm` [M86 p. 172]).

### 6. Templates

*Implemented in:* `seekwhence/templates.py` (the template and its glom, the applier, resolver, evaluator and reviewer of 6.3-6.5, filtering past a template; D-026 to D-029, D-060, D-061), `seekwhence/forms.py` (the wildcard `n` of 6.1). Tests: `tests/test_templates.py`.

Read on the page images of pp. 69-70 and 73-76 and of the transcript pages cited.

#### 6.1 What a template is

The template level sits "at an intermediate level between the 'real', data-driven cyto-level
and the 'perceived', perception-driven socrato-level" [M86 p. 73]; its human analogue is the
stage "during which a person's eyes move back and forth across the terms, as the person waits
for an idea to emerge" [M86 p. 73]. "In Seek-Whence, the template-level processes attempt to
come up with a template or descriptor of the sequence. This is a preliminary step to devising
a hypothesis -- that is, a predictive model of the sequence, an encapsulation of its structure.
Templates and hypotheses have similar forms, but templates are far less complete and exact,
lacking the predictive ability and expressive power of hypotheses. A good, working template
will eventually give rise to a hypothesis" [M86 p. 73].

"A template is formed when the structure of some particular dubbed glom is found to explain, at
least roughly, all the sequence terms seen thus far. For example, the template form '(S-group 1
n)' suffices to explain '1 1 2 1 2 3' since it 'fits' all the term groupings, even though there is
no built-in notion or even any recognition that 'n' means 'countup' here" [M86 p. 73]. The same
template fits `1 2 1 2 3 4 5 1 1 2 3` as well; "The ability to notice 'cross-glom' properties,
such as n <--> countup, is left to higher-level processes" [M86 p. 73]. So a template form is a
dubbed glom's structure with, possibly, a parameter replaced by the wildcard `n`, "where 'n' means
'any nonnegative integer'" [M86 p. 74].

The template as printed has four fields: `template made : (form (Cycle 3 2 (3 7)) state
working coverage (1 2) glom glom2)` [M86 p. 167]: its `form`, a `state` (`working`), the
`coverage` (the positions it accounts for, here the first two of three terms) and its `glom`,
the template glom.

#### 6.2 The template glom

"Once a template is created, it puts a pseudo-glom called the template glom over the highest-level
gloms in the cytoplasm (those that are not subgloms of any other glom) to prevent the
disappearance of the gloms that engendered and now reflect it" [M86 p. 73]. "A pseudo-glom cannot
combine with real gloms, and it prevents its subgloms from glomming activity as well. Some
cyto-level activity can still continue -- bonding being done as freely as ever, for example -- but
no new templates are considered for the lifetime of the given template" [M86 pp. 73-74]. The
transcript prints the creation as `create-template-glom (Cycle 3 2 (3 7))(glom1)` followed by
`top-down glom glom2` [M86 p. 167], and `show` lists the template glom as `glom3 pseudo --> ((3
7)) terms 1 to 2` [M86 p. 167] (the glossary: a pseudo-glom is an inert glom, "generally used as a
cap to prevent the disappearance of a given glom cluster" [M86 p. 181]).

#### 6.3 Evaluation and filtering

"The template will be checked by a Template-evaluator task and either be passed, or rejected and
abandoned. If it is passed, it will probably be the basis for hypothesis and gnoth creation" [M86
p. 74]. Until a hypothesis exists "all new terms will be 'filtered' past the template, checked for
agreement with it. Should a term not fit the template, a review is set up, with resulting
modification or rejection of the template. The filtering process is the first major top-down
action performed by the system; the template level has taken control" [M86 p. 74]. The cyto-level
goes on as before: "What is added is direction from above: instructions to make or dissolve gloms,
to create units of a particular form" [M86 p. 74]. The transcript prints the evaluator's check as
`check-cycle template (Cycle 3 2 ((3 7)))` [M86 p. 167] (the check is named after the class; what
it checks is not said). The thesis does not say what the evaluator tests; a decision (item 10),
with "fits all the terms seen so far, allowing for the wildcard" as the simplest reading of p. 73.

#### 6.4 Template difficulties: applier, resolver, reviewer

"The process of devising a template is not as easy as it might first appear" [M86 p. 74]. The
example: with `1 1 2 1 2 3` entered and the first `1 2` glommed and dubbed `(S-group 1 2)`, the
Template-scout "would set out a Template-applier task to determine if the entire sequence seen is
of that form. The applier would attempt to view the sequence as a repetition of '(S-group 1 2)'
and would, of course, fail because of the initial '1' and the trailing '3'" [M86 p. 74]. The
applier "checks to see if loosening a parameter or two in its representation would help. In this
case, changing the form from the original '(S-group 1 2)' to '(S-group 1 n)' -- where 'n' means
'any nonnegative integer' -- will do the trick. The accepted template will then be '(S-group 1
n)'" [M86 p. 74]. Which parameter is loosened first, and whether the start may be loosened too, is
not said; a decision (item 10).

"If the Template-applier fails, it creates a Template-resolver task for one final attempt.
When invoked, the resolver looks at glints rather than gloms to determine whether or not the
sequence can be re-viewed to fit into the given mold" [M86 p. 75]: if `1 1 2 1 2 3` were
glommed as `(1 1) 2 (1 2 3)` the applier fails on the first glom, but a resolver with
`(S-group 1 n)` can find the S-groups in the terms themselves. "If the Template-resolver is
successful, it 'blasts' (does an immediate burst of) all gloms and has the glints reglommed to
fit the template" [M86 p. 75], a radical action that can destroy "'locked-in' gloms, ones the
system created and can never seem to burst" [M86 p. 75]. "If the proposed template does not
work at term level, it is forgotten and the engendering glom dissolved" [M86 p. 75]. "In
practice, the Template-resolver is seldom invoked because the system can usually devise a
template early on which is good enough to push up a hypothesis" [M86 p. 75]; the design rule
is "to give each level a little more capability than it should need to use" [M86 p. 75].

"The Template-reviewer process is in this category. It is invoked after a template has been
created (but no hypothesis exists) and when new terms fail to fit the template. It can try
some very simple fixes and can either: 1) call for modification of the template and restart
the creation and evaluation processes; 2) leave the template alone; 3) target it for
abandoning" [M86 p. 75]. The fixes are not described.

#### 6.5 Acceptance

"The creation and acceptance of a template causes increased activity in the system, in effect
'raising the temperature' in the system. Most importantly, it sets off two tasks, a Gnoth-maker
and a Hypothesizer. This action pushes processing up into the next level, the socrato-level,
where more considered operations are performed on the fluid but less ephemeral structures of
the socratoplasm" [M86 pp. 75-76]. In the transcripts the acceptance is visible as a burst of
sparks, `top-down glom glom3` and `gnoths constructed` [M86 p. 167].

### 7. The socratoplasm: gnoths

*Implemented in:* `seekwhence/gnoths.py` (the `Gnoth` with the fields of 7.2 and of `show`, the Gnoth-maker of 7.3, the catchall of 7.4, the three equivalences of 7.5; D-030, D-051, D-062). Tests: `tests/test_gnoths.py`.

Read on the page images of pp. 76-79, 94-98 and the transcript pages cited.

#### 7.1 The middle level

"The socratoplasm is the 'perceived world' of Seek-Whence, the place where perceptions developed
at the cyto-level are noticed, catalogued, and dealt with. It can be viewed as a battleground
between the 'ideal' plato-notions and the 'real' cyto-glimmerings -- that is, between the semantic
and the syntactic -- or -- to put it one last way -- between the cognitive and the subcognitive"
[M86 p. 76]. It is "the system's playground, where perceptions can be modified and manipulated; in
short, it is where slipping occurs" [M86 p. 76]. Socrato-level operations "inevitably cause
cyto-level activity. This is very desirable. Such low-level activity may result in the noticing of
a special bond or the creation of a new glom which might eventually engender a better parse" [M86
p. 76]. Unlike a cyto-level task, "the socrato-level can and does support tasks which say, in
essence, 'Enough! I have a hypothesis. Let's have the next term to check it out', or better still,
'I think the answer is .... Tell me if I'm wrong'" [M86 p. 76].

Why gnoths exist: "At this point, malleable, manipulable, relatively non-ephemeral structures
are needed so that any necessary slipping can be noticed and carried out. Moreover, a
reformulation vocabulary must be developed so that the system can express clearly and
succinctly the actions it needs to take. Thus, the structures we call 'gnoths' are created"
[M86 p. 77]. Their place between the levels: "Cyto-level tasks are somewhat myopic, able to
view the sequence only in a restricted, localized way. They have no overview of the sequence"
[M86 p. 79]; "the hypothesis and platonic-level processes can be said to have no 'underview'
of the sequence, no direct contact with reality as it exists in the cytoplasm. Gnoths are
designed to bridge the gap between these levels, to provide a place where inconsistencies
between the high-level and low-level views can be worked out" [M86 p. 79].

#### 7.2 The gnoth: three views and its fields

"Each gnoth, a member of the class Gnoths, is viewed in three different ways: 1) it is an
actualization of a Platonic class; 2) it has an underlying glom collection from which it
derives its structure; 3) it represents one 'hit' of the current hypothesis (if there is one)"
[M86 p. 77].

The fields, from the Gnoth-maker example [M86 p. 78] and the `show` output of the transcripts
[M86 pp. 168, 175, 179]:

| Field | Meaning | Source |
|---|---|---|
| `class` | `Gnoths` | p. 78 |
| `name` | `gnothN` | p. 78 |
| `frame` | which hit of the hypothesis the gnoth holds: "this gnoth holds the first hypothesis 'hit'" [M86 p. 78]. The thesis numbers frames 1, 2, 3 [M86 p. 78]; the program numbers them from 0 (`frame: 0` for the first gnoth [M86 p. 168]; `frame: 1`, `frame: 2` for the second and third [M86 p. 175]) | pp. 78, 168, 175 |
| `plato-class` | the class the gnoth actualizes: `S-group`, `Cycle` | pp. 78, 168 |
| `glom` | the gnoth's pseudo-glom, the gnoth-glom, which has the underlying glom(s) as subgloms: `glom10 (where glom10 has glom2 as subglom)` [M86 p. 78] | pp. 78, 168 |
| `range` | "the sequence terms it 'covers'" [M86 p. 78], as first and last position: `(1 1)`, `(2 3)`, `(4 6)` | pp. 78, 168 |
| `notes` | `nil` in every transcript; not described | pp. 168, 175 |
| `form` | the gnoth's form: `(((Cycle 3 2 (3 7)) pure))` after the Gnoth-maker [M86 p. 168], `(S-group 1 2)`, `(S-group 1 1)`, `(S-group 1 3)` after reformulation [M86 pp. 175, 179] | pp. 168, 175, 179 |
| `state` | `stable` in every transcript | pp. 168, 175 |
| `external-bonds` | after reformulation: the bonds to other gnoths with their values, `((bond2 0) (bond3 -5))` [M86 p. 175] | pp. 175, 179 |
| `internal-bonds` | the bonds inside the gnoth with their values, `((bond1 10))`, `nil` [M86 p. 175] | pp. 175, 179 |
| `equivalence-type` | `parse` [M86 p. 175]; the three levels are in 7.5 | p. 175 |
| `groups` | `nil` in the `show` output [M86 p. 175]; the Gnoth-setter prints `groups: ((glint1 glint2))` and `glom: (glint1 glint2)` while filling a gnoth [M86 p. 168] | pp. 168, 175 |

The `show` output prints the first nine fields in the order class, name, frame, plato-class,
glom, notes, form, state, range [M86 p. 168], and after reformulation prints external-bonds,
internal-bonds, equivalence-type and groups *before* them [M86 p. 175]. The gnoths are listed
in order of their frames and a gnoth that has been emptied is not listed (p. 175 lists gnoth2
and gnoth3 only, with frames 1 and 2, after gnoth1 gave up its glint in the SHIFT-RIGHT of
p. 174). The meaning of the bond values (10, 0, -5) is the pull and push arithmetic of ch. 3
(section 11).

#### 7.3 The Gnoth-maker

"When a Gnoth-maker task, set off by the system after template creation, is invoked, it
creates one gnoth for each subglom of the template-glom and notifies the associated
plato-classes of their existence" [M86 p. 77]. The worked example [M86 pp. 77-78]: in
`1 1 2 1 2 3` with gloms `glom2: (1)`, `glom7: (1 2)`, `glom4: (1 2 3)` and the template
`(S-group 1 n)`, the Gnoth-maker creates:

| Gnoth | frame | plato-class | glom | range |
|---|---|---|---|---|
| gnoth1 | 1 | S-group | glom10 (has glom2 as subglom) | (1 1) |
| gnoth2 | 2 | S-group | glom11 (has glom7 as subglom) | (2 3) |
| gnoth3 | 3 | S-group | glom12 (has glom4 as subglom) | (4 6) |

"Each gnoth places a pseudo-glom (called a 'gnoth-glom') over its glom collection (which contains
just one glom initially). A gnoth-glom, like a template-glom, cannot glom with other cyto-elements
and serves to prevent the haphazard disappearance of glom structures important to the system" [M86
p. 78]. Since the glom collection gives the gnoth its character, "that collection must be
preserved until the gnoth itself must change. Cyto-level bonding activity can continue but now the
gnoth oversees the fate of its gloms" [M86 pp. 78-79]. The transcript shows the gnoth-gloms as
`top-down glom glom3` before `gnoths constructed` [M86 p. 167], and `show` lists them as
pseudo-gloms (`glom8 pseudo --> (1) terms 3 to 3`, `glom9 pseudo --> (1 2) terms 1 to 2` [M86 p.
175]).

The example shows a term `1` standing alone as an S-group of length 1 (gnoth1), so a template
glom may have a bare glint as a subglom and the gnoth over it is still an actualization of the
template's class.

#### 7.4 The catchall gnoth

"The system stores hypothesis-confirming terms in a catchall gnoth, a special gnoth that simply
serves as a repository for non-troublesome terms" [M86 p. 98]; the glossary calls it "a
rightmost or 'trailer' gnoth that simply holds input terms that agree with the hypothesis
without parenthesizing them" [M86 p. 180]. In the example of p. 98 the hypothesis is
`(S-group 1 3)`, two gnoths hold `1 2 3` each, and the catchall holds `1 2 3 4`: the first three
because the hypothesis predicted them, the 4 as the term that failed. The Gnoth-setter splits
the catchall into gnoths (section 10). So between a guess and a failure the parse has the
gnoths made by the Gnoth-maker (or the last reformulation) followed by one catchall; the
transcripts do not print the catchall in `show` (it is a decision, item 11, whether it is a
gnoth object or the list of unparsed terms).

#### 7.5 Gnoth-hypothesis equivalence: term, parse, structural

"We have stated that each gnoth is to represent one hit of the hypothesis. But is it
sufficient that the gnoth give the same terms as a hypothesis hit? Or do we want the same
terms with the same parenthesization? Or might we also want the gnoth to obey the same
underlying form (that is, have the same parenthesization for the same reason)?" [M86 p. 94].
"We use the term frame of a hypothesis to mean an abstractly-viewed hit of the hypothesis: the
collection of Seek-Whence forms that would produce the given hit" [M86 p. 94].

- **Term equivalence**, "the weakest of the three types of representation, requires that each
  gnoth govern precisely the same terms as one frame of the hypothesis" [M86 p. 95]. With the
  hypothesis `(S-group 1 3)`, a gnoth2 over `glom1 (1 2)` and `glint3 (3)`, whose value is
  `((1 2) 3)`, and a gnoth3 over three glints, whose value is `(1 2 3)`, are both
  term-equivalent [M86 p. 95].
- **Parse equivalence** "requires that the gnoth print its value with the same parenthesization
  as the corresponding hypothesis frame. In the above example, gnoth3 is parse-equivalent to
  the given hypothesis while gnoth2 is not" [M86 p. 95].
- **Structural equivalence**: "a gnoth must be parse-equivalent to it and the gnoth's form must
  be the same as the corresponding frame of the hypothesis" [M86 pp. 95-96], the frame being
  taken in its *deep-structure* form. For `(C-group (S-group (Countup 1) 3) 2)` over
  `1 2 3 1 2 3 2 3 4 2 3 4 3 4 5 3 4 5` the first three frames are, shallow:
  `(C-group (1 2 3) 2)`, `(C-group (2 3 4) 2)`, `(C-group (3 4 5) 2)`; deep:
  `(C-group (S-group 1 3) 2)`, `(C-group (S-group 2 3) 2)`, `(C-group (S-group 3 3) 2)`
  [M86 p. 96]. "More structural detail is retained in the deep-structure form, with only the
  lowest-level structures replaced by constants or runs. In the shallow-structure form, all but
  the top-level structures are so replaced" [M86 p. 96]; the deep form "represents a
  'one-step-down' view of the hypothesis, we chose it as our goal" [M86 p. 96]. "Once a
  hypothesis is made, the system gives each gnoth its target form, the deep-structure
  equivalent of the hypothesis frame to which it corresponds" [M86 pp. 96-97].

"In summary, all gnoths must always maintain term equivalence with the hypothesis. Their goal
will be to achieve structural equivalence by reforming into the deep-structure form of one
hypothesis frame. Along the way they will achieve the middle state of parse equivalence,
indicated by the fact that the gnoth's 'parse-print', the parenthesized printing of its value,
matches that of the hypothesis frame" [M86 p. 97]. That reforming, *form polishing*, "will be
essential to an ability to generalize sequences in reasonable ways and make analogies between
sequence descriptions" [M86 p. 97] but is "among the future high-level goals of the
Seek-Whence project, unimplemented as yet" [M86 p. 97]. So the program computes and records
`equivalence-type` (the transcripts show `parse` [M86 p. 175]) but never reforms a gnoth to
raise it; the reconstruction does the same.

The glossary's definition of the frame that goes with this: "an abstractly-viewed hit of a
hypothesis: the collection of Seek-Whence forms that would produce the given hit"
[M86 p. 180]; and of a gnoth: "a socratoplasm-level structure representing a logical grouping
of terms in the system's parenthesization or parse of a sequence" [M86 p. 180].

### 8. The hypothesis: forms, boxes and printstructures

*Implemented in:* `seekwhence/hypothesis.py` (the hypothesis of 8.2-8.3: form, box, validity, prediction, postdiction, the guess block, freeze-dried forms; D-004, D-032 to D-034), `seekwhence/boxes.py` (boxes, printstructures, firers and `ready` of 8.4-8.5, `build_box` and `show_box` of 8.6; D-003, D-056), `seekwhence/gnoths.py` (the Hypothesizer and the Tuple fall-back of 8.1, D-031), `seekwhence/filtering.py` (the Hfilter: validity and the guess of 8.3, D-063). The open points of 8.8 are decided in D-001 to D-037. Tests: `tests/test_hypothesis.py`, `tests/test_boxes.py`, `tests/test_filtering.py`.

Read on the page images of pp. 59, 79-85 and 161-163 and the transcript pages cited.

#### 8.1 Purpose, the Hypothesizer and the Tuple fall-back

"The major goal of the Seek-Whence system is to formulate a hypothesis -- a structure that
describes and can extrapolate the perceived pattern. The hypothesis, which is derived from the
information at hand -- the sequence terms seen and any relationships the program has been able
to establish among them -- is expressed in terms of the Platonic classes described above"
[M86 p. 59]: `1 1 2 1 2 3 1 2 3 4` as `(S-group 1 (Countup 1))`, `2 1 2 2 2 3 2 4` as
`(Cycle (2 (Countup 1)))` [M86 p. 59]. When a hypothesis fails "the tendency of Seek-Whence
will be: a) to generalize parameters, maintaining the Platonic class structure; b) to slip to a
less strict class ('vertical' slippage) or to a related class ('lateral' slippage)" [M86 p. 59].

"The overall purpose of the system is to develop a reasonable hypothesis: a clean, predictive
model of the rule underlying the sequence. When a template is accepted, a Hypothesizer task is set
off along with a Gnoth-maker, described above" [M86 p. 79]. "When invoked, the Hypothesizer is
responsible for devising a hypothesis for the sequence, based on the template and the existing
gnoths (if any) and gloms" [M86 p. 79]. "If, for some reason, there is a faulty template (or none
at all), the Hypothesizer can take the fall-back position of declaring the sequence to be a Tuple,
the weakest of all plato-classes" [M86 p. 79]. How the Hypothesizer turns a template with a
wildcard into a form is not said; the transcripts show it taking the template form as the
hypothesis form when the template has no wildcard (`(Cycle 3 2 (3 7))` [M86 pp. 167-168]) and
producing `(Countup 1)` for `1 2` [M86 p. 171] (section 15); a decision (item 12).

A hypothesis must be "not only predictive and clean, but also amenable to reformulation --
'slippable'" [M86 p. 79]. "The predictive nature of a hypothesis is a semantic rather than a
syntactic requirement, and so poses few constraints on hypothesis form" [M86 p. 80]; it "should
have a clean visual appearance so that it can be understood by humans" and "should be modular,
so that the reformulation so fluidly and naturally done by humans can be carried out equally
smoothly by the system" [M86 p. 80].

#### 8.2 Hypothesis form

"The form we have chosen for hypotheses is, not surprisingly, closely tied to the ideals in
the platoplasm" [M86 p. 80] and resembles the Seek-Whence diagrams of ch. 1. The fragment
`1 1 1` "may well be viewed as a C-group (Constant group). A hypothesis would express this in
the form '(C-group 1 3)', a list consisting of the Platonic class name, the start-value and the
(top-level) length of the grouping" [M86 p. 80]. The other examples of pp. 80-81 are in the
table of section 4.6: `(C-group (S-group 4 3) 2)` for `4 5 6 4 5 6`, `(S-group (Countup 4) 3)`
for `4 5 6 5 6 7`, `(Cycle ((Countup 1) (Tuple (5 8 4))))` for `1 5 8 4 2 5 8 4 3 5 8 4`, the
two forms for `1 1 2 1` and the two for `1 2 1`. "These forms are constructed by the system as
it attempts to build a hypothesis for the pattern presented. The Hypothesizer process will take
such a form and from it construct a Seek-Whence hypothesis -- a data structure with several
fields, capabilities, and functions" [M86 p. 81].

#### 8.3 Hypothesis features: form, prediction, validity, box

"First and apparently simplest, the hypothesis can display its form, much as was shown in the last
section. It can also predict the next term to be expected following that form. In addition, it has
a validity associated with it -- a number that grows as new, correctly-predicted terms are
encountered" [M86 p. 81]. The most crucial field, the one that supports the others, is the
hypothesis' box: "The box is the structure which, when 'hit', produces the next run of terms
predicted by the hypothesis. The box can be reset to start again, asked to list a number of terms,
or asked to predict the next term, given the sequence's currently known terms" [M86 pp. 81-82]. So
the hypothesis has at least the fields *form*, *validity* and *box*, and the operations *show
form*, *predict next term*, *hit*, *reset* and *list n terms*.

Validity starts at 0 or is reset to 0 when a term fails: the transcript prints `set-out --
validity: 0` when the term 3 denies `(Cycle 3 2 (3 7))` [M86 p. 168], and ch. 3 says "the
system immediately sets the hypothesis' validity to 0" [M86 p. 98]. How much it grows per
confirming term and what value triggers the guess is not said (section 1.4: every transcript
guesses after one confirming term); a decision (item 05).

**The guess** prints the form and the next hits of the box: `3 7 3 7` for `(Cycle 3 2 (3 7))`
[M86 p. 166], `3 7 3 3 7` for `(Cycle 3 3 (3 7 3))` [M86 p. 169], `(1 2)(1 2)(1 2)` for
`(S-group 1 2)` [M86 p. 175] and `(1 2)(1 2 3)(1 2 3 4)` for `(S-group 1 (Countup 2))`
[M86 p. 179]. The group hypotheses print three hits; the two Cycles print four and five single
terms, which is not three hits and not three turns (a turn of `(Cycle 3 3 (3 7 3))` is three
terms). The predicted terms begin with the next unseen term in the S-group runs and with the
*first* term of the sequence in the Cycle runs (`3 7 3 7` after the terms `3 7 3 7`; `3 7 3 3 7`
after the five terms `3 7 3 3 7`), which suggests the box was reset before the guess in those
runs. Section 15.5 states the one rule that fits all four guesses: the box is fired from the
start for at least three hits and until every entered term is covered. The rule is a decision
(item 12), recorded with this evidence.

"The hypothesis' box is a member of the class 'Boxes' and as such lives in the socratoplasm,
the middle level of the Seek-Whence world, along with members of the classes 'Printstructures'
and 'Gnoths'. Gnoths, as we have seen earlier, are the central representative structures in
the socratoplasm; boxes and the closely-related printstructures are not as visible, serving a
more private purpose" [M86 p. 82].

#### 8.4 Boxes and printstructures

"Each box is a repository of information about an underlying printstructure and through that
printstructure branches out, tree-like, to represent in an active way structures with such
forms as: (C-group 1 3) or (C-group (S-group 2 3) 2)" [M86 p. 82] (Figure 3, p. 83).

"Boxes can be 'hit', prodded for their next value. When implementing box hits, I wanted to be
sure that hit propagation down the box tree could be done in a fully parallel manner, with no
reliance on the return of any particular value before any other. The following implementation
will work in this fashion, although the current version of the program treats box hits as
indivisible operations, rather than as a task series" [M86 p. 82]. The reconstruction treats
hits as indivisible too.

"When a box is hit, it calls upon its underlying printstructure to feed it a value. Each
printstructure has a collection of fire-boxes, subboxes which must be hit to give it a value. When
the printstructure 'fires' -- that is, hits its fire-boxes, each box must return a value" [M86 p.
82]. "Thus, a hit on a top-level box propagates down through the tree of printstructures and boxes
below it until the most deeply-nested structures return their values. These are passed up and the
upwards-bubbling proceeds until the top-level answer appears in the top box's 'pstruc-val' field"
[M86 pp. 82-83].

**Figure 3** [M86 p. 83], the box tree for `(C-group (S-group 2 3) 2)`, read from the image:

| Node | Fields |
|---|---|
| Box1 | `printstruc: Pstruc1`, `pstruc-val`, `ready` |
| Pstruc1 | `type: C-group`, `boxes: (box1)`, `n-val: Box2`, `k-val: Box5` |
| Box2 | `printstruc: Pstruc2`, `pstruc-val`, `ready` |
| Pstruc2 | `type: S-group`, `boxes: (box2)`, `n-val: Box3`, `k-val: Box4` |
| Box3 | `printstruc: Pstruc3` |
| Pstruc3 | `type: Constant`, `boxes: (box3)`, `value: 2` |
| Box4 | `printstruc: Pstruc4` |
| Pstruc4 | `type: Constant`, `boxes: (box4)`, `value: 3` |
| Box5 | `printstruc: Pstruc5` |
| Pstruc5 | `type: Constant`, `boxes: (box5)`, `value: 2` |

So a box has the fields `printstruc`, `pstruc-val` and `ready`; a printstructure has `type`,
`boxes` (the boxes it feeds, the inverse of `printstruc`), and, for a group type, `n-val` (the
start) and `k-val` (the length), each a box; a Constant printstructure has `value` instead.
"In Figure 3, for example, the 'fire-boxes' for Pstruc1 -- a C-group printstructure -- are its
'n-val' and 'k-val' boxes, namely Box2 and Box5. In order for Box2 to fire, though, it must in
turn receive a value from its subordinate printstructure, Pstruc2 -- an S-group
printstructure. When Box2 is duly filled, its 'ready' field will be set to 'true', and it will
report the value in its 'pstruc-val' field to Pstruc1" [M86 pp. 83-84]. The fire-boxes of the
other types (the elements of a Cycle, Tuple or Y-group; the single argument of Countup) are not
drawn; the forms of pp. 162-163 show a Tuple or Cycle printstructure holding its elements as a
list in which a shared element is written `(same pstrucN)`.

"In order to handle the details of firing and box-filling, each printstructure type (C-group,
S-group, etc.) has a firer associated with it, a process which knows how to fire the relevant
fireboxes of the printstructure and what to do with the results" [M86 pp. 84-85]. (The text
layer reads "fint"; the image reads *firer*.)

#### 8.5 Shared printstructures and `ready`

"It is possible for two or more boxes to share the same underlying printstructure. This happens,
for example, in the sequence: '8 1 2 8 3 4 8 5 6...', which can be described by the form '(Cycle
(8 (Countup 1) shared))'. Modeling this form requires the creation of three boxes: one for the
'Constant 8', one for the first 'Countup 1', and one for the second 'Countup 1' (referred to as
'shared' in the given form)" [M86 p. 84]. "The 'shared' distinguishes this situation from the one
implied by the form '(Cycle (8 (Countup 1) (Countup 1)))', which also requires three boxes, and
which corresponds to the sequence '8 1 1 8 2 2 8 3 3...'" [M86 p. 84].

"In our first form, only one Countup printstructure is created. When that printstructure fires in
response to a hit on the first Countup box, it feeds both Countup boxes, making both boxes
'ready'. Later on, when the second Countup box is hit, the same printstructure will fire, again
feeding both boxes, but this time with the next value in sequence" [M86 p. 84]. "In contrast, the
second form causes creation of different printstructures for the two Countup boxes. Those
printstructures are hit independently, once each in a turn around the Cycle" [M86 p. 84].

"A simpler example of the same phenomenon can be seen using Figure 3. If the form modeled had
been '(C-group (S-group 2 3) shared)' -- rather than '(C-group (S-group (2 3)) 2)' -- so that
the sameness of the 2's were to be modeled explicitly, our diagram in Figure 3 would have been
slightly different. Pstruc3 would have '(box3 box5)' in its 'boxes' field, and there would be
no need for Pstruc5. Box5 would point to Pstruc3 as its 'printstruc'" [M86 p. 84].

"When a printstructure is shared by two or more boxes, each box must be filled whenever the
printstructure fires. Those boxes must then record the fact that they already have a value --
set their 'ready' fields to 'true' -- so that they can report this value until the next time
they are hit. Boxes can also be reset to start from the beginning of the pattern described, or
asked to show a number of terms" [M86 p. 85].

The two descriptions of a shared Countup must be reconciled by the implementation: in the
Cycle of p. 84 the second Countup box, when its turn comes, is *hit* and the printstructure
fires again with the next value (the hits give `8 1 2`), whereas in the Tuple of p. 162 both
Countup boxes report the *same* value in one hit (`(1 8 1)`), and in `(C-group (Countup 1)
shared)` of p. 161 the start and the length report the same value (`(2 2)`, `(3 3 3)`). The
consistent reading is that a box reports its stored value while `ready`, that `ready` is
cleared when the value has been reported upward (a group or Tuple hit fills its sister boxes in
one firing and both are read in the same hit), and that a Cycle hits one element box per hit,
so a `ready` flag set during an earlier hit has been cleared by the time the shared box's turn
comes. The reconstruction implements that reading and checks it against all four outputs
(pp. 57-58, 161-163); the reading is a decision (item 06).

**Changing a hypothesis.** "One proposed project for refinement of Seek-Whence is to create a
box-tree editor. We or the system could then change the box tree associated with a form. This
would make hypothesis modification cleaner and more sophisticated than the current technique,
which is to scrap the old box tree and make a new one" [M86 p. 85]. The reconstruction scraps
and rebuilds.

"We feel that the chosen implementation of hypotheses goes a long way toward meeting our goals.
It gives us an active structure capable of realizing any well-formed hypothesis form. It
accurately represents pattern structures, and shared substructures can be represented
explicitly in the box tree. Thus it is expressive. It is modular so that slipping --
reformulation -- is supported" [M86 p. 85].

#### 8.6 `build-box` and `show-box`: the appendix's box implementations

The appendix [M86 pp. 161-163] prints three console sessions of the box code, "The program's
equivalent of the form" followed by a `build-box` call and successive `show-box` calls. Read
from the images (the text layer is clean here):

```
(C-group (Countup 1) shared):                              ; p. 161
-> (build-box
        '(C-group (same pstruc1) (same pstruc1))
         '((pstruc1 (Countup 1))))
box5
-> (show-box 'box5)   (1)
-> (show-box 'box5)   (2 2)
-> (show-box 'box5)   (3 3 3)
-> (show-box 'box5)   (4 4 4 4)

(Tuple 1 3 ((Countup 1) 8 shared)):                        ; p. 162
-> (build-box
        '(Tuple 1 3 ((same pstruc2) 8 (same pstruc2)))
            '((pstruc2 (Countup 1))))
box12
-> (show-box 'box12)  (1 8 1)
-> (show-box 'box12)  (2 8 2)
-> (show-box 'box12)  (3 8 3)
-> (show-box 'box12)  (4 8 4)

(Cycle 1 3 ((Countup 1) 8 shared)):                        ; p. 163
-> (build-box
        '(Cycle 1 3 ((same pstruc3) 8 (same pstruc3)))
            '((pstruc3 (Countup 1))))
box19
-> (show-box 'box19)  1
-> (show-box 'box19)  8
-> (show-box 'box19)  2
-> (show-box 'box19)  3
-> (show-box 'box19)  8
-> (show-box 'box19)  4
-> (show-box 'box19)  5
-> (show-box 'box19)  8
-> (show-box 'box19)  6
```

(Each `show-box` result is printed on the line below the call in the original; they are joined
here.) Three things the sessions settle: `build-box` takes a form in which a shared
substructure is written `(same pstrucN)` at every place it occurs, plus a list binding each
such name to its form, and returns the name of the top box; the program's Tuple and Cycle
forms carry a start and a length before the actual list (`1 3`); and `show-box` of a Cycle
returns one element per call, while a Tuple or group returns a list. The box numbers (box5,
box12, box19) show that the first session made five boxes (the C-group, its two arguments,
the Countup's argument, and one more), consistent with Figure 3's one box per node.

#### 8.7 The end of stage one

"Once the hypothesis is in place and the gnoths corresponding to it are 'up' (created by the
Gnoth-maker), the system has reached the culmination of its stage-one processing. From now on,
activity will take place at all levels of the system simultaneously. The new goal will be
confirmation of a predictive model for the sequence" [M86 p. 85]. "Virtually all the
structures created before the gnoths and hypothesis operate at a level that we feel is
generally ignored by most AI systems" [M86 p. 85]; the gnoth operations, "a language in which
we can express several ways for gnoths to combine and split, to share terms, and generally to
interact with each other" [M86 pp. 85-86], are the subject of ch. 3 (sections 9-14).

#### 8.8 What sections 4-8 leave to decisions

For item 05 (and the items that implement each mechanism), the points on which pp. 48-86 and
161-163 are silent, collected:

1. The internal form: four slots for every type; how the start of a fence form is chosen; how
   `shared` is written without the arrow (`(same pstrucN)` is the program's own notation).
2. The process functions of the repeater types, and whether a single glint is a manifestation
   of Constant or Countup.
3. The bond families and which bond types the Tester actually tests (only `Same`, `Succ`,
   `Pred` on `print-value`, `adjacent` or `remote`, appear in the transcripts); the numeric
   bond strengths; how many Sparklers, Testers and Glom-scouts a spark or bond releases.
4. The urgencies of every task but the ordering given for the Glomtester.
5. What the Glommer's "refine" and "failed to glom" mean; what interests a plato-class in a
   glom; how the Plato-evaluator splits a chain into pure subcollections; when a Burster rather
   than a Dissolver is chosen.
6. What the Template-evaluator and `check-cycle` check; which parameter the Template-applier
   loosens; the Template-reviewer's "very simple fixes".
7. Whether the catchall is a gnoth object; how many hits of a Cycle make one frame.
8. How the Hypothesizer turns a template into a hypothesis form; how validity grows and when
   the guess is made; how many hits the guess prints and whether the box is reset first; the
   `ready` semantics across hits.

### 9. Why and when to reformulate

*Implemented in:* `seekwhence/filtering.py` (refutation, 9.4's first step; D-017), `seekwhence/slipping.py` (the Gnoth-setter and the freeze-dried hypotheses of 9.2 and 9.4; D-035, D-036), `seekwhence/casting.py` (the blast of 9.5 and the failure count; D-050). Cosmetic reform (9.3) is not implemented, as in the thesis. Tests: `tests/test_slipping.py`, `tests/test_casting.py`.

Read on the page images of pp. 87-99 (the chapter opens on p. 87, a title page). Sections 9-14
are chapter 3, "Seek-Whence: Stage Two -- Reformulation" [M86 p. 87].

#### 9.1 What the chapter covers, and the running example

The chapter's introduction states what the program had solved: "Several sequence problems have
been solved by the system" [M86 p. 88], and lists nine (transcribed from the image of p. 88):
`1 1 1 ...`, `1 2 3 4 ...`, `1 1 1 2 2 2 3 3 3 ...`, `3 4 3 4 3 4 ...`, `3 7 3 7 3 7 ...`,
`3 7 3 3 7 3 3 7 3 ...`, `16 15 14 17 16 15 18 17 16 ...`, `16 15 14 15 14 13 14 13 12 ...`
(annotated "as well as possible, given a non-infinite pattern" [M86 p. 88]) and
`1 2 1 2 3 1 2 3 4 ...`. "We will use the last of these in a running example of Seek-Whence
processing throughout the remainder of this dissertation" [M86 p. 88]; its transcript is the
third appendix run [M86 pp. 170-179].

The defining characteristic of stage two is "the looming presence of the hypothesis. Without
it, the system suffers from a 'blind men and elephants' problem -- trying to make global sense
from multiple local perspectives. With the hypothesis, the system has a 'point of view', a
predictive model of the sequence to which it can cling until contradictory evidence is
encountered" [M86 p. 88].

#### 9.2 The human approach: one hypothesis, a cloud of variants

The design is modelled on observed behaviour: "once they have developed a hypothesis they
insist on its correctness until it is proved incorrect by the production of a term that simply
will not fit" [M86 p. 89]. Shown `1 2 2 3`, "many people hypothesize" `(1) (2 2) (3 3 3)
(4 4 4 4)...`, "or, in Seek-Whence terminology: (C-group (Countup 1) shared)" [M86 p. 89]. Told
"Nope, not it" and shown another 3, "the usual reaction is 'Yeah?', uttered with an inflection
of challenge" [M86 p. 89]; only a 4, making `1 2 2 3 3 4`, convinces them "that they really
believe another formulation is required. Then follows a variable-length period of review and
reorganization, which is in turn followed by the generation of a new firmly-held hypothesis
(or, in difficult cases, resignation)" [M86 p. 89].

This is what the program models: "The system maintains one hypothesis at any given time, rather
than a list of possibilities. It is able to do this and still function reasonably well because of
its ability to 'slip' from an old hypothesis to a new one" [M86 p. 89]. "The hypothesis is, in
effect, surrounded by cloud of potential hypotheses, close variants into which it can be
transformed whenever appropriate. Underlying this ability are links among the Platonic concepts
and information about the cyto-level environment favored by each Platonic concept" [M86 p. 89].
(The page really prints "surrounded by cloud", without an article.) The "links" are the s-links
and the "information about the environment" the pull-push bond lists of section 11.1; the cloud is
not stored anywhere.

#### 9.3 Two reasons to change a hypothesis: medical and cosmetic

"There are actually two reasons for changing a hypothesis: 1) it fails to predict; 2) it is
predictive but its form is less than satisfactory. We term hypothesis changes made for the
former reason 'medical reformulations' to distinguish them from the 'cosmetic reformulations'
made in response to the latter. The current version of Seek-Whence supports the more critical
medical reformulations but has only made a beginning at handling the cosmetic ones"
[M86 pp. 92-93].

**Cosmetic reform** (pp. 93-97) is the refinement of a correct but "ugly" parse: "An 'ugly',
though correct, parse can be very dissatisfying to humans; there is generally strong agreement
on which of several candidate parses is 'best' in this heuristic sense" [M86 p. 93]. The
example is Blackburn #3, `2 1 2 2 2 2 2 3 2 2 4 2 2 5 2 ...`: "most successful solvers will
come up with the parse: (2 1 2) (2 2 2) (2 3 2) ...." [M86 p. 93], while "More than one
person has parsed it as: 2 (1 2 2) (2 2 2) (3 2 2)..., becoming annoyed at the presenter for
posing a problem with such a tricky, ugly parse, 'with that 2 sticking out in front'" [M86
p. 93]. A second kind of cosmetic question is that equally acceptable parses "will generalize
differently" [M86 p. 93]: `(5 1 5) (5 2 5) (5 3 5)...` and `(5 1 6) (5 2 6) (5 3 6)...` "are
both considered generalizations of the sequence (4 1 5) (4 2 5) (4 3 5)...." [M86 p. 93]; in
the first "the countup between the bracketing 4 and 5 in the original (last-listed) sequence
is either not noticed or not considered salient, while in the second generalization it is
maintained" [M86 p. 93].

What exists of cosmetic reform: "Hypothesis refinement is as yet only minimally supported in
Seek-Whence. It is to be carried out by internal gnoth reformers, processes that modify the
internal structure of the gnoths" [M86 pp. 93-94], for two reasons: "1) to relieve internal
pressure within a gnoth, pressure deriving from those bonds within the gnoth that would push
it apart; 2) to make the gnoth's structure conform more closely to the reigning hypothesis"
[M86 p. 94]. The first is "bottom-up" pressure (an unwieldy structure or poor
parenthesization, the lone 2 of the Blackburn #3 case), the second "top-down", "the goal of
structural equivalence between each gnoth and the hypothesis" [M86 p. 94]. The three
equivalence levels and the deep vs shallow frames (pp. 94-97) are in section 7.5; the goal is
stated as "all gnoths must always maintain term equivalence with the hypothesis. Their goal
will be to achieve structural equivalence by reforming into the deep-structure form of one
hypothesis frame" [M86 p. 97], and the closing verdict is that form polishing and the
noticing of structural samenesses "are among the future high-level goals of the Seek-Whence
project, unimplemented as yet" [M86 p. 97].

*What the reconstruction implements:* medical reformulation, in full, as sections 10-14
describe it. Of cosmetic reform only what the thesis says exists: the `equivalence-type` mark
on each gnoth (term or parse, set by the Gnoth-setter, section 10.1) and nothing that changes
a gnoth's form to polish it. Internal gnoth reformers are not built; any later attempt is an
extension, off by default.

#### 9.4 Medical reform: the outline

"Medical reformulation, which is supported in the current version of Seek-Whence, is done when the
hypothesis has been demonstrated to be invalid" [M86 p. 97]. It "involves a review of the old
hypothesis and the underlying structures supporting it, a decision as to which Platonic type
should hold sway, a re-evaluation of the bonds noticed by the system, the use of bonds in the
environment of the chosen Platonic type to engender gnoth reformulations, and finally (it is
hoped) the construction of a new, predictive hypothesis" [M86 pp. 97-98]. Those five steps are, in
order, sections 10 (the Gnoth-setter), 11 (reigning type: slip or stay), 12 (the Reformulator's
bond assessment and selection of pulls), 13 (the gnoth operations), and 14 (carrying out reforms,
casting, testing the candidate, failure).

The spirit of the process: "The goal of reconsideration is the construction of a new and valid
hypothesis. This is not a mechanical, program-directed reconstruction, however, but rather a
'homing in' on a new formulation from a tightening spiral of possibilities generated by
independent but interacting processes" [M86 p. 99].

#### 9.5 Cycling, thrashing, and the blast fallback

The program is allowed to cycle at low levels and only flags cycling at the level of
hypotheses (the freeze-dried list, section 10.3): "On the other hand, we do not want to
prevent cycles at low levels for several reasons. People experience them. Although we may
find them quite annoying at times, they are often quite useful in forcing us to consider once
again a correct notion which we had rejected for some 'high-level' but incorrect reason"
[M86 p. 92]. The cost is admitted: "Seek-Whence has thrashed about more than once, clinging to
some Platonic class or glom, while underlying layers push up another, correct, notion over and
over again. Knowing when to permit these notions to take over and when to squelch them is a
most difficult problem" [M86 p. 92].

The fallback: "Our current solution has been, when no progress has been made for quite some
time, to blast away all gnoths and gloms, leaving only the glints and their bonds to push up
an inspiration" [M86 p. 92]. The inspiration was a mathematics student who, after hours on a
problem set, would toss her papers away, walk around the room and greet the sheet afresh:
"Oh, look -- a problem set! I wonder what the questions are. Shall we try some? I bet they'll
be fun." [M86 p. 92]; "Sometimes it worked and sometimes...." [M86 p. 92]. The thesis does not
say how "no progress for quite some time" is measured, which task performs the blast, or
whether bonds between destroyed gloms survive (the sentence keeps "the glints and their
bonds"). The template-level analogue is the Template-resolver's blast (section 6). All of this
is left to a decision (item 05); see 14.6 for the list.

### 10. The Gnoth-setter and freeze-dried hypotheses

*Implemented in:* `seekwhence/slipping.py` (`layout`, the Gnoth-setter, the reuse and refilling of gnoths, `gnoths: (...)`; D-036, D-042), `seekwhence/hypothesis.py` (`FreezeDried`, D-035). Tests: `tests/test_slipping.py`.

Read on the page images of pp. 90-92 and 98-99, with the transcript pages 168, 173 and 176 for
what the program prints.

#### 10.1 The Gnoth-setter

The trigger and the task, in the thesis's words: "The system stores hypothesis-confirming
terms in a catchall gnoth, a special gnoth that simply serves as a repository for
non-troublesome terms. When an unexpected term is encountered, the system immediately sets the
hypothesis' validity to 0, releases sparks to encourage low-level activity, and places a
Gnoth-setter task on the taskrack. When invoked, the Gnoth-setter carefully fills out gnoths in
accordance with the old hypothesis and calls for the system to reconsider its parse"
[M86 p. 98].

The worked example: "if the old hypothesis were '(S-group 1 3)', and two gnoths, each holding '1 2
3', were already in existence, the catchall gnoth might be holding '1 2 3 4'" [M86 p. 98]. "The
first three terms in the segment '1 2 3 4' are in the catchall because they were predicted by the
hypothesis; the '4' is the last term entered -- the troublesome one. The Gnoth-setter would
therefore create two new gnoths, one to hold the initial '1 2 3' from the catchall and the other
to hold the trailing 4" [M86 p. 98].

The marking rule: "Each gnoth is marked with the frame and equivalence type (term or parse,
depending on agreement with the hypothesis' parenthesization) appropriate for it. Any
non-fitting terms are collected together in a final gnoth and the catchall is destroyed. In the
example above, the two pre-existing gnoths and the first of the newly-created ones would be
marked as parse-equivalent to the old hypothesis" [M86 p. 98].

The running example: "entry of the first two terms -- '1 2' -- causes the system to
hypothesize (Countup 1). When the next term entered is '1', a Gnoth-setter puts out three
gnoths, one for each term. The first two are in accord with the discredited hypothesis and are
in fact parse-equivalent to it. The last one simply holds the non-fitting term" [M86
pp. 98-99]. "Now, with 'all the cards on the table', the Gnoth-setter calls for reconsideration
to begin" [M86 p. 99].

What the transcripts show the Gnoth-setter doing [M86 pp. 168, 173, 176]: the line `set-out --
validity: 0` precedes it (p. 168, where the failing term's sparks are also listed); then one
`top-down glom glomN` line per pseudo-glom it makes (five on p. 168: glom4 to glom8; five on
p. 173; three on p. 176), with `groups: ((glint1 glint2))` and `glom: (glint1 glint2)` printed
under the first on p. 168, and a closing `gnoths: (gnoth1 gnoth2 gnoth3)` (p. 168, 173) or
`gnoths: (gnoth2 gnoth3 gnoth5)` (p. 176). The number of `top-down glom` lines exceeds the
number of gnoths, so a gnoth's filling can create more than one pseudo-glom (the thesis does
not say which: a reading for item 05). The gnoths it leaves are shown on p. 175 after
reformulation: `frame: 1` and `frame: 2`, `equivalence-type: parse`, with the hypothesis-form
`(S-group 1 2)` and `(S-group 1 1)` (section 7.2).

Note the two descriptions of the running example at the moment of the third term: p. 98 says
the hypothesis after `1 2` is `(Countup 1)` and the third term `1` makes three one-term
gnoths; p. 101 says that "just after the 3 is introduced, we should have (S-group 1 2) as the
now-discredited hypothesis and three gnoths as shown in Figure 1" [M86 p. 101], the gnoths
`(1 2)`, `(1 2)`, `(3)`. Both are true at different moments of the same run (the transcript on
pp. 173-176 shows first the Countup, then `(S-group 1 2)` refuted by the 3).

#### 10.2 Freeze-drying the old hypothesis and destroying its box

"When a hypothesis has been deemed inadequate, it is 'freeze-dried' -- its form is extracted and
is kept on a list of old hypotheses, along with the number of terms of the sequence it explained.
The old hypotheses serve as a check against cycling in the system" [M86 p. 91]. "When Seek-Whence
has trouble coming up with a hypothesis, it, like most humans, keeps coming back to the same
incorrect hypotheses again and again. This, we feel, is not a bad feature, since people are guilty
of the same 'foolish' behavior. It would be disastrous, however, should it go unnoticed" [M86 p.
91]. The thesis contrasts this small effort with the self-watching work of Gray Clossman and
others in FARG and the Copycat project [M86 pp. 91-92], and concludes: "In any case, freeze-dried
hypotheses at least flag cyclic behavior at this level of granularity" [M86 p. 92].

Where it happens: "The first step taken during reconsideration is a bookkeeping measure,
saving the form of the old hypothesis and destroying its box, the home of its active
representation. This leaves the system with no active structure to govern or filter
processing, only a 'freeze-dried' form to remind it of its most recent perspective"
[M86 p. 99]. Two uses of the list are named: the count of recent tries by the reigning class
(section 11.2) and the flagging of a repeat. The thesis does not say what the program does when
a candidate repeats a freeze-dried form (reject it, lower its chances, or only note it), nor
whether the box is destroyed by the Gnoth-setter or by the slip-or-stay step that follows; the
order of p. 99 (box destroyed, then the reigning-type decision) is the reading implemented.

### 11. Slip or stay: pulls, pushes, stability and the strict classes

*Implemented in:* `seekwhence/platoplasm.py` (the s-links, the pull and push lists and the strict classes of 11.1, a bond's value in a class's environment; D-038 to D-040), `seekwhence/slipping.py` (bond assessment, gnoth stability, the staying and slipping pressures and the slip check of 11.2-11.6; D-041, D-042). Tests: `tests/test_links.py`, `tests/test_slipping.py`.

Read on the page images of pp. 90-91 and 99-105, Figure 1 on p. 102 at 170 dpi, with pp. 175-176
of the third transcript for the program's own numbers.

#### 11.1 S-links and pull-push bonds: what the platoplasm holds for reformulation

"The Platonic concepts of Seek-Whence, C-group, Tuple, Countup, and the like, are to be
connected by a variety of links reflecting the concepts' interrelationships. This network of
connections, in conjunction with a philosophy for their use, constitutes the 'Slipnet' which is
so essential to the system's reformulation ability. In the current version, we have implemented
a small number of undifferentiated slipping links, called s-links, for this purpose"
[M86 p. 90]. A second, "level-spanning" network relates each concept to its manifestations and
actualizations [M86 p. 90] (section 4.1); "Also included in the level-spanning network are
lists of pulling and pushing bonds, bonds which the concept can use to group or separate
sequence terms. Level-spanning links are little used as yet" [M86 p. 90].

**S-links.** "As currently implemented, the s-links have direction and 'slipperiness'. For
example, S-group has s-links to Countup, C-group, Y-group, Cycle, and Tuple. Associated with each
s-link is a number between 0 (non-slippery) and 1 (perfectly slippery), which indicates my
estimate of the system's proclivity to move from the given concept to the neighbor" [M86 p. 90].
"The s-link from S-group to Countup has slipperiness 0.1, reflecting the fact that it is difficult
to slip to a stricter class. Slipperiness from S-group to Tuple is 0.4, since Tuple can serve as a
generic grouping mechanism if no satisfactory stricter class is appropriate. The slipperiness
values can be changed during processing, although the current system does not do so" [M86 pp.
90-91]. For the fuller notion she points to [Hofstadter 84] [M86 p. 91].

The s-link values the thesis gives, all from S-group:

| From | To | Slipperiness | Source |
|---|---|---|---|
| S-group | Countup | 0.1 ("difficult to slip to a stricter class") | p. 90 |
| S-group | Tuple | 0.4 ("a generic grouping mechanism") | p. 91 |
| S-group | Y-group | 0.4, from the worked example `0.4 [the s-link slipperiness] * (10 + 10) = 8` | p. 104 |
| S-group | C-group, Cycle | exist; value not given | p. 90 |

The transcript confirms 0.4 for S-group to Y-group independently (11.6). No other class's
s-links are described: the neighbours of Cycle, C-group, Countup, and the rest, and whether
links are symmetric, are decisions (item 05). The transcript shows one slip, `Countup -->
S-group` [M86 p. 174], so Countup has an s-link to S-group.

**Pull-push bonds.** "In addition to the s-links, each concept preserves information about the
types of bonds it finds most useful in grouping sequence terms. For example, the C-group concept,
because it involves copy or sameness groups, favors adjacent sameness bonds most strongly, but
also likes to see gloms having the same span (number of sequence terms covered)" [M86 p. 91].
"Bonds which a Platonic class might use to hold groups together are listed as 'pull-bonds'; those
it tends to use to separate groups are listed as 'push-bonds'. Each so-designated bond type is
given a strength from 1 to 10, strengths which again could be, but in practice are not, changed by
the system" [M86 p. 91].

The pull and push values the thesis gives or implies:

| Class (environment) | Bond type | Value | Source |
|---|---|---|---|
| S-group | adjacent successor | pull, +10 | p. 101 (Figure 1), p. 175 (`(bond1 10)`) |
| S-group | remote sameness | push, -5 | p. 101 (Figure 1), p. 175 (`(bond3 -5)`) |
| S-group | adjacent predecessor | 0 (neither) | p. 175 (`(bond2 0)` for `Pred print-value (adjacent)`) |
| C-group | adjacent sameness | pull, "most strongly" (no number) | p. 91 |
| C-group | same span | pull (no number) | p. 91 |
| C-group | successorship | push ("made negative", no number) | p. 100 |
| Y-group | remote sameness | pull, 10 | p. 104 (`where the 10's are the strengths, in a Y-group environment, of the "remote same" bonds`) |

The general rule: "If, for example, S-group is the reigning class, adjacent successor bonds are
given large positive values to indicate that they are strong pulling bonds while sameness bonds
are given negative values to indicate that they tend to push gloms apart" [M86 p. 100]. "Should
C-group be in ascendancy, sameness bonds become strong whereas successorship bonds are made
negative. The information required for the system to assign these values is in the platoplasm,
with each Platonic concept listing both pulling and pushing bond types and their strengths" [M86
p. 100]. A bond type a class lists neither way is worth 0 in its environment (the `(bond2 0)` of
p. 175 and the arithmetic of 11.6). The full tables for the eight classes are not in the thesis: a
decision (item 05), to be built from these values and the class definitions of section 4.

#### 11.2 The decision: strict classes, recent tries, bond tension

"The system then decides whether to stay with the reigning class -- the Platonic class at the
highest level of the (former) hypothesis -- or to slip to a new one. This decision is made on
several considerations" [M86 p. 99]:

1. **Strict classes slip at once.** "First, if a reigning class -- such as Constant -- is
   very strict in the sense that it is difficult to generalize without moving to a new class
   altogether, slipping is chosen immediately. Otherwise, some deeper investigation is made"
   [M86 p. 99]. Constant is the example; the transcript's `Changing to a new reigning class.
   Countup --> S-group` [M86 p. 174], printed under `doing task Reformulator` with no
   `slip-check` line before it, suggests Countup is treated as strict too (the reading
   implemented; which classes count as strict is a decision, item 05).
2. **Recent tries.** "The old hypotheses are checked to determine the number of recent
   hypotheses of this class -- how many 'tries' the class has had since it seized power"
   [M86 p. 99]. The thesis does not say how the count enters the decision (a limit, a penalty
   on the staying pressure, or only a report): a decision (item 05).
3. **Bond tension.** "All bonds are assessed in the environment of this class -- assigned a
   strength which depends on the class in question as well as on the type of the bond"
   [M86 p. 99]. "The result of this assessment is a rough measurement of the existing 'bond
   tension', the strength of the bonds favoring modification of the current gnoths. Strong bond
   tension implies strong pressure to change some aspect of the current parse -- either to
   abandon the current reigning class or to modify the gnoths' structure within the framework
   of that class" [M86 p. 100].

#### 11.3 Assessing bond pulls

"Bond assessment is a relatively straightforward procedure designed to assign strengths to all
existing bonds under the assumption that a particular Platonic class holds sway" [M86 p. 100] (the
values of 11.1). "Procedurally, each gnoth is processed in turn. Its internal bonds, those among
the gloms it covers, are noted, and their strengths in the current environment -- that of the
reigning class -- are assigned. Its external bonds, those between its elements and those of other
gnoths, are similarly assessed" [M86 p. 100]. "These values become instrumental in determining the
'happiness' of the gnoth -- its inclination to stand pat. The collective happiness of all the
gnoths is used as a measure of the success of the reigning class in organizing the system's
perception of the sequence" [M86 p. 100]. The assessed values are what the `show` command prints
per gnoth as `external-bonds: ((bond2 0) (bond3 -5))` and `internal-bonds: ((bond1 10))` [M86 p.
175] (section 7.2).

#### 11.4 Gnoth stability and the staying pressure

"The pressure to stay with the reigning class is the sum of what is termed
'gnoth-stabilities', a less anthropomorphic and more functionally defined term for the
'happiness' mentioned above. The stability of a gnoth is the difference between the bond
forces holding it together and those acting to tear it apart. 'Holding' bonds are internal
pulls and external pushes. 'Tearing' bonds are external pulls and internal pushes" [M86 p. 101].

Halving: "The 2.5's represent half the strength of the two external push bonds under gnoth1.
These are the remote sameness bond between glint1 and glint3, and the remote sameness bond
between glint2 and glint4. Strength-halving is done so that external bond values are not
counted twice, once for each gnoth involved" [M86 p. 102]. The same halving applies to external
pulls: gnoth2's tearing external pull "of strength 10" enters its stability as `(5)` and
gnoth3's as `(5)` [M86 p. 103].

The rule, as the code implements it, for a gnoth *g* with bonds valued in the reigning
class's environment (positive = pull, negative = push, 0 = indifferent):

- holding(g) = sum of the positive values of internal bonds + half the absolute values of the
  negative external bonds;
- tearing(g) = half the positive values of external bonds + the absolute values of the
  negative internal bonds;
- stability(g) = holding(g) - tearing(g);
- staying pressure = sum of stability(g) over all gnoths [M86 pp. 101, 103, 104].

A bond is *internal* to a gnoth when both its ends are under that gnoth, *external* when one
end is under it and the other under another gnoth [M86 pp. 100-101].

#### 11.5 The worked example: Figure 1 (p. 102) and p. 104 as numbers

The state: sequence `1 2 1 2 3`, "(S-group 1 2) as the now-discredited hypothesis and three
gnoths as shown in Figure 1" [M86 p. 101]. Figure 1, "Measuring gnoth stability", transcribed
from the image of p. 102:

| Gnoth | Pseudo-glom | Glints (position: value) |
|---|---|---|
| gnoth1 | glom1 | glint1: 1, glint2: 2 |
| gnoth2 | glom2 | glint3: 1, glint4: 2 |
| gnoth3 | (none drawn; the glint directly) | glint5: 3 |

The bonds drawn, with the values the figure prints (legend: "+10 -- adjacent successor value
bond", "-5 -- remote same value bond"):

| Bond | Between | Type | Value (S-group reigning) | Role |
|---|---|---|---|---|
| a | glint1, glint2 | adjacent successor | +10 | internal pull of gnoth1 |
| b | glint1, glint3 | remote same | -5 | external push, gnoth1 / gnoth2 |
| c | glint2, glint4 | remote same | -5 | external push, gnoth1 / gnoth2 |
| d | glint3, glint4 | adjacent successor | +10 | internal pull of gnoth2 |
| e | glint4, glint5 | adjacent successor | +10 | external pull, gnoth2 / gnoth3 |

The thesis's commentary [M86 p. 101]: bond a "functions as an 'internal pull' for gnoth1 since
it has a positive value and both members, glint1 and glint2, are within that gnoth"; bond e
"also has value +10, but functions as an 'external pull' between gnoth2 and gnoth3"; bond c
"functions as an 'external push', tending to keep the parent gnoths, gnoth1 and gnoth2, apart.
Therefore, it contributes to the stability or 'happiness' of both gnoths involved"; "In this
particular example, there are no 'internal push' bonds".

The arithmetic, as printed (test vectors):

| Gnoth | Holding | Tearing | Stability | Source |
|---|---|---|---|---|
| gnoth1 | +10 (a) + 2.5 (half of b) + 2.5 (half of c) = 15 | 0 | `(+10 +2.5 +2.5) -(0) = 15` | p. 102 |
| gnoth2 | +10 (d) + 2.5 (b) + 2.5 (c) = 15 | 5 (half of e) | `(+10 + 2.5 + 2.5) - (5) = 10` | p. 103 |
| gnoth3 | 0 | 5 (half of e) | `(0) - (5) = -5` | p. 103 |
| total | | | `15 + 10 - 5 = +20` | p. 103 |

"We then add the individual gnoth stabilities to find a total system stability, in this case,
of 15 + 10 - 5 = +20" [M86 p. 103]. This total is the staying pressure: "the reigning S-group,
whose staying pressure is 20" [M86 p. 104].

A caveat the thesis makes about its own measure: "We note that some of the tearing pressure is
due to unresolved bond pulls favoring the reigning type -- if not its specific realization in
the current hypothesis -- and so may be considered inappropriate for our purposes.
Nevertheless, we are tapping a measure of internal consistency. That is, if we assume an
environment of this class and still find much bond tension (much gnoth unhappiness), we may
quickly abandon the type, at least for a while" [M86 p. 103].

**Slipping pressure.** "In order for a reigning class to be abandoned, however, some other
class has to demonstrate strength in its own right. Those classes that 'neighbor' the current
reigning class -- those connected to it by s-links in the platoplasm -- are the primary
'pretenders to the throne'. If one of them can show sufficient strength (sufficient 'slipping
pressure', as described below), it may supplant the current 'monarch'" [M86 p. 103].

"The slipping pressure from the reigning class to a neighbor is evaluated by: 1) adding two
quantities -- the sum of all pulling-bond strengths, and the absolute value of the sum of all
pushing-bond strengths -- taken over all existing bonds, and assessed in the environment of the
neighboring class, and then 2) multiplying the sum by the slipperiness of the link between the
monarch and the neighbor -- the proclivity to slip in that direction" [M86 p. 104]. "In effect,
the system tries to estimate the gnoth stability in an 'alternative universe' -- the
environment dominated by the neighboring class -- as well as the likelihood of moving from the
current universe to the alternative one" [M86 p. 104]. Note that the slipping pressure ignores
the gnoth boundaries (every bond counts, pulls and pushes both positively), whereas the staying
pressure depends on them.

The decision: "Slipping-pressure estimates are calculated for each class that is an s-link
neighbor of the reigning class. If the largest of these values is greater than the 'staying
pressure' -- the current stability -- then a slip to the corresponding class will be made and
the system will have a new reigning class" [M86 p. 104]; "A similar value is calculated for each
neighboring class, and if the largest of these values is greater than the staying pressure, a
slip to that class will be made" [M86 p. 105]. Strictly greater, in both statements.

The worked number, settled on the image of p. 104 (the text layer reads "O.i ... + (10 + 10)
= 8"; the page prints a 0.4, an asterisk for the multiplication, and the square brackets):

> "the slipping pressure from S-group to Y-group ('symmetry' group -- for, say, a parse: 1 2 1
> 2 3 2) is: 0.4 [the s-link slipperiness] * (10 + 10) = 8, where the 10's are the strengths,
> in a Y-group environment, of the 'remote same' bonds between glint1 and glint3 and between
> glint2 and glint4. Thus Y-group, with a slipping pressure of 8, cannot seize the throne from
> the reigning S-group, whose staying pressure is 20" [M86 p. 104].

So in a Y-group environment the two remote-same bonds are pulls of 10 each and the three
adjacent-successor bonds contribute nothing (or the sum would not be 20): a test vector,
`slipping_pressure(S-group -> Y-group) == 0.4 * (10 + 10) == 8.0 < 20`.

#### 11.6 The program's own numbers: `slip-check` in the transcripts

The transcripts print, after the Gnoth-setter, `slip-check: stayval: 15.0 best: (Y-group 4.0)`
[M86 p. 176] for the same sequence `1 2 1 2 3`, and `slip-check: stayval: 0.0 best: nil` [M86
p. 168] for `3 7 3` under Cycle. The first can be reconciled with the prose exactly. At that
moment the bonds in existence are the three of p. 174's `show`: `bond1 Succ print-value
(adjacent) -- (glint1 glint2)`, `bond2 Pred print-value (adjacent) -- (glint2 glint3)`, `bond3
Same print-value (remote) -- (glint1 glint3)` (bond5, the remote sameness between glint2 and
glint4, is only made after the check, p. 176), and the gnoths are gnoth2 `(1 2)`, gnoth3
`(1 2)`, gnoth5 `(3)`. With the values of p. 175 (`(bond1 10)`, `(bond2 0)`, `(bond3 -5)`):

| Gnoth | Holding | Tearing | Stability |
|---|---|---|---|
| gnoth2 | 10 (bond1 internal) + 2.5 (half of bond3) | 0 | 12.5 |
| gnoth3 | 2.5 (half of bond3) | 0 | 2.5 |
| gnoth5 | 0 | 0 | 0 |
| total | | | **15.0** = the printed `stayval: 15.0` |

and the best pretender: Y-group, with the one remote-same bond worth 10 in its environment and
the adjacent bonds worth 0, `0.4 * 10 = 4.0` = the printed `best: (Y-group 4.0)`. So the
transcript confirms the halving, the S-group values +10 / -5 / 0, the Y-group remote-same pull
of 10, the slipperiness 0.4, and that `best` reports the largest slipping pressure with its
class. The thesis never states this reconciliation; it is a reading, and it is the arithmetic
the code implements (checked by a test with the page in its docstring). The `stayval: 0.0 best:
nil` of p. 168 is printed in a state with one bond, `bond1 Same print-value (remote) --
(glint1 glint3)` [M86 p. 167], which lies between the gnoths `(3 7)` and `(3)` the
Gnoth-setter has just made: under the stability rule the total is 0.0 only if Cycle values a
remote sameness bond 0 (a pull would give -5.0, a push +5.0), and `best: nil` only if no
s-link neighbour of Cycle values it either (D-038, D-039, D-042; item 15 corrected an earlier
reading of this sentence that supposed no bond existed). The printed `best` has one decimal, so the pressures
are floating-point in the program.

Which task prints `slip-check` is not stated; it follows the Gnoth-setter's `gnoths:` line
directly on pp. 168 and 176 and, on p. 174, the slip `Countup --> S-group` is printed under
`doing task Reformulator`. The reading implemented: the Gnoth-setter ends by making the
decision and printing `slip-check`, and the strict-class slip is announced when the
Reformulator first runs under the new class (item 05 may choose otherwise; it is a decision).

### 12. The Reformulator

*Implemented in:* `seekwhence/reforms.py` (the threshold of 12.1, the lasts and firsts and the selection order of 12.2, the pull, the holding, the mover and the strength of 12.3; D-043, D-046, D-068). Tests: `tests/test_reforms.py`.

Read on the page images of pp. 105-109.

#### 12.1 Threshold, assessment, and the hand-off to the Bond-assessor

"At this point, a reigning class has been established -- or reconfirmed -- and so a
Reformulator process is placed on the taskrack. When invoked, this process will attempt to
find salient bonds and will set out Gnoth-operator tasks designed to act upon the bond pulls
or pushes in order to change the gnoths" [M86 p. 105].

"The Reformulator's first act is to determine a threshold bond strength. Bonds or bond groups
exerting pressures below this threshold will be ignored. Currently, the new threshold is set to
either 1 more than the existing threshold value or, if none exists, 80% of the strongest pull-bond
strength for the reigning type" [M86 p. 105]. "(This value was chosen arbitrarily, with some vague
remembrance of Winston's grouping algorithm in his 'blocks-world' program [Winston 75]. It has
remained because it seems to have done no harm as yet.)" [M86 p. 105]. (The text layer reads
"80.,."; the image prints "80%".) For S-group, whose strongest pull is the adjacent successor at
10, the first threshold is 8.0, which is what the transcripts print on the Bond-assessor's line:
`doing task Bond-assessor on (S-group 8.0)`, then `(S-group 9.0)`, `(S-group 10.0)` [M86 pp. 174,
177-178], one more per Reformulator (section 14.2).

"Because new bonds may have been established since the Reformulator's creation time, its next
act is to assess all bond pulls in the environment of the reigning class, as described above"
[M86 p. 105] (section 11.3). "Then begins the process of finding strong pulls and/or pushes, and
turning them into gnoth operations -- actions that modify gnoths. If the Reformulator finds no
actions to be taken or if it has completed its recommendations, it hangs a Bond-assessor task
on the taskrack (to determine system 'happiness') and terminates" [M86 p. 105].

#### 12.2 Selection of neighbour pulls

"All inter-gnoth moves involve the rightmost (at some level) glom of some gnoth and the
leftmost (again, at some level) glom of the gnoth's neighbor to the right. This is a
consequence of the sequential nature of our domain. We obviously cannot rearrange the order of
sequence terms (even though such an operation might make a 'more interesting' sequence); we can
only readjust our groupings" [M86 p. 106]. (For a less restricted pulling environment she
points to [Hofstadter 83].)

Figure 2, "Neighboring gnoths" [M86 p. 106], transcribed from the image (children left to
right):

| Node | Children |
|---|---|
| gnoth3 | glom14, glom15 |
| glom15 | glom9, glom10 |
| glom10 | glom5, glom6, glom7 |
| glom7 | glint3, glint4 |
| gnoth4 | glom8, glom17 |
| glom8 | glom3, glom4, glom2 |
| glom3 | glom1 |
| glom1 | glint5, glint6 |
| glom4 | glint7, glint8 |

"our system will be interested in the 'lasts' of gnoth3: (glom15 glom10 glom7 glint4), and the
'firsts' of gnoth4: (glom8 glom3 glom1 glint5)" [M86 p. 106]. So the *lasts* of a gnoth are the
rightmost top-level glom, its rightmost subglom, and so on down to a glint; the *firsts* are
the leftmost chain of the right neighbour. (glom14, glom9, glom5, glom6, glom17, glom4 and
glom2 are off both chains.)

The precedence rule: "The first moves considered are those at the highest level, under the theory
that if a glom wants to move, its subgloms should follow. It is also possible that some glom feels
relatively content but one of its subgloms is attracted to a glom in the neighbor gnoth. In such a
case, the subglom should be popped out and over to the neighbor" [M86 p. 107]. "Should both glom
and subglom feel a pull, the glom move should take precedence since it is structurally more
important. Subsequently, internal gnoth operations -- actions which modify the internal structure
of a gnoth -- could be used to move the subglom if it still feels the need to leave its parent
glom" [M86 p. 107].

**Selection order** [M86 p. 107], transcribed from the image (a test vector for the order in
which candidate pairs are assessed; `<-->` is the thesis's arrow; a parenthesised list means
"assess pull with each in the list"):

| Level | Pairs, in order | The thesis's gloss |
|---|---|---|
| 1 | glom15 <--> glom8 | "Assess the pull between the topmost gloms, then between level two gloms and those at levels one and two." |
| 2 | glom10 <--> glom8; glom15 <--> glom3; glom10 <--> glom3 | |
| 3 | glom7 <--> (glom8 glom3); (glom15 glom10) <--> glom1; glom7 <--> glom1 | "(assess pull with each in the list)" |
| 4 | glint4 <--> (glom8 glom3 glom1); (glom15 glom10 glom7) <--> glint5; glint4 <--> glint5 | |

In words: at level *k* the new element of each chain (the *k*-th last, the *k*-th first) is
paired with every element of the other chain down to level *k*, the new last against the old
firsts first, then the old lasts against the new first, then the two new ones together. The
pattern for levels 3 and 4 (the "lasts" side listed as a group against the single new "first",
and the new "last" against the group of old "firsts") is as printed; how the pull "with each
in the list" is combined into one strength is not said (a decision, item 05).

**Stopping.** "As soon as some reformulation is strong enough -- the bond pulls and pushes
supporting it exceed the threshold -- the Reformulator creates an appropriate gnoth operation or
program of operations and sets a Gnoth-operator on the taskrack to carry it out" [M86 p. 107].
"The Reformulator will not suggest any further moves, since any others would occur at a lower
structural level and therefore would be less important to the system. Should any lower-level moves
be important, they will eventually be discovered by some future Reformulator" [M86 pp. 107-108].

#### 12.3 Conversion of bond pulls into gnoth operations

"When there is sufficient strength of pull between two gloms from neighboring gnoths, a gnoth
operation must be devised to bring the two gloms together. Simply shifting one glom into the
other's gnoth may not be sufficient, because the decisive pull on it may be coming from a
deeply-nested glom, one several levels down from the top" [M86 p. 108]. "In Figure 2 for example,
glint4 may be pulled toward glom3. In our '1 2 1 2 3' example, at the time described in Figure 1,
the last term -- the '3' -- is pulled by its predecessor -- a '2' -- which is nested within a glom
whose print-value is '(1 2)'" [M86 p. 108].

Direction: "The system must decide which of the two attracting gloms is to move and which is
to stay put. This is determined by an analysis of the bonds holding the gloms in their
respective gnoths. Single gloms are the most likely to move, leaving an empty gnoth behind, a
shell which the system destroys" [M86 p. 108]. (A gnoth left empty by a shift "disappears", as
the series on p. 120 shows: section 13.6.)

Repair: "Once the direction of the move is determined, the total move must be constructed. As
will be discussed below, gnoth operations can be quite destructive of a gnoth's internal
structure, bursting gloms until the target gloms below are reached. When a gnoth operation is
performed, at least some of this structural damage must be repaired; we do not want the
destruction of important nesting structures to be a side-effect of reformulation" [M86 p. 108].

Strength: "Finally, the strength of the operation is calculated. This strength -- the
difference between the gloms' mutual attraction and the pull exerted by other gloms to hold
them in place -- must exceed the system-determined threshold, or else the move would not have
been generated. The strength is used by Seek-Whence to weight competing alternatives when
necessary" [M86 pp. 108-109]. Which bonds count as "the pull exerted by other gloms to hold them
in place" (the mover's internal pulls in its own gnoth, read as the bonds between the mover and
its siblings) and how competing alternatives are weighted (the first strong enough wins, by the
stopping rule above; a weighted draw is the alternative) are decisions (item 05).

### 13. The nine gnoth operations and PROGRAMs

*Implemented in:* `seekwhence/operations.py` (the nine operations of 13.2-13.4 on the world's gnoths and gloms, the capping discipline of 13.1, the printed forms and PROGRAMs with `New` references; D-044, D-045, D-065, D-066), `seekwhence/reforms.py` (`build_program`, the PROGRAM construction of 13.5; D-044). Divesting pushes (13.6) are not implemented, as in the thesis. Tests: `tests/test_operations.py`, `tests/test_reforms.py`.

Read on the page images of pp. 109-123; the example pages 114, 116, 118, 120, 121 and 122 at 170
dpi. The glossary's entry "gnoth operation" lists SHIFT-LEFT, SHIFT-RIGHT, SPLIT, CAPTURE,
ENCLOSE, FRACTURE, MERGE and NO-OP [M86 p. 181] (a paraphrase: the text layer garbles the names);
with CAPTURE's two directions that is nine.

#### 13.1 The two categories and the capping discipline

"Gnoth operations fall into two categories: external or inter-gnoth operations, and internal
or intra-gnoth operations. The external operations are: SHIFT-LEFT, SHIFT-RIGHT, and SPLIT. The
internal operations are: CAPTURE, ENCLOSE, FRACTURE, MERGE, and NO-OP" [M86 p. 109].

Why they are delicate: "each gnoth has an associated 'pseudo-glom', a glom that cannot interact
with others, serving as a cap to prevent the disappearance, through natural glomming, of useful
gloms and glom groups. The pseudo-gloms of any gnoths involved in gnoth operations must be
destroyed to permit the true gloms below to interact with each other" [M86 p. 109]. "Similarly, if
a very deeply nested glom is to be involved in an operation, all gloms containing it must be
destroyed so that it can rise to the top of the cytoplasm and become available" [M86 p. 109].

"Naturally, all this glom-bursting destroys the encasing gnoth's structure. This is permitted
because neither we nor the system can know whether the destruction is the primary purpose of the
operation or just a side-effect of its real intent" [M86 p. 109]. "Any proposer of gnoth
operations that wishes to preserve some of the original structure must make the effort to do so.
The burst gloms cannot, of course, be brought back, but functionally similar (not identical,
because the gnoth operation did change something) ones can be created" [M86 p. 109].

Afterwards: "When a gnoth operation is completed, a capping procedure puts a new pseudo-glom in
place above the gnoth's gloms. Often, Plato-scout tasks are placed on the taskrack to peruse
the gnoth's newly-created gloms, searching among them for any new manifestations of the
Platonic concepts" [M86 pp. 109-110]. In the transcripts every Gnoth-operator is followed by a
`top-down glom glomN` line, the new cap [M86 pp. 174, 177].

The glom-naming convention in the examples: a new glom made by an operation gets a fresh
number (glom20 on pp. 114, 116, 118; glom2 on p. 114; glom3 on p. 118; glom5, glom6, glom7 in
the series), burst gloms are never reused, and glints keep their names. The package's test
vectors compare parenthesised print-values and the identity of surviving gloms, not the fresh
numbers.

#### 13.2 Formats

External [M86 p. 110]:

| Operation | Format | Meaning |
|---|---|---|
| SHIFT-LEFT | `(SHIFT-LEFT <left-gnoth> <right-gnoth> <glomlist>)` | "glomlist is a list of the gloms (which must be neighbors in order) to be transferred from right-gnoth to left-gnoth" |
| SHIFT-RIGHT | `(SHIFT-RIGHT <left-gnoth> <right-gnoth> <glomlist>)` | "glomlist serves an analogous purpose, this time from left-gnoth to right-gnoth" |
| SPLIT | `(SPLIT <gnoth> <splitlist>)` | "splitlist is a list of gloms currently under the given gnoth. A new gnoth is to be formed using the splitlist gloms as its top level" |

Internal [M86 pp. 112-113]:

| Operation | Format | Meaning |
|---|---|---|
| CAPTURE-LEFT | `(CAPTURE-LEFT <gnoth> <glom> <captive>)` | "the given glom within the given gnoth is to swallow its neighbor, captive, whole. Actually, the glom is destroyed and a new one created with the captive as its leftmost subglom and also containing all the original glom's subgloms" |
| CAPTURE-RIGHT | `(CAPTURE-RIGHT <gnoth> <glom> <captive>)` | "analogous to the operation above" (the captive becomes the rightmost subglom) |
| ENCLOSE | `(ENCLOSE <gnoth> <encloselist>)` | "encloselist is a list of neighboring gloms within the gnoth to be covered by a new glom, dubbed to be of type 'enclose'" |
| FRACTURE | `(FRACTURE <gnothname>)` | "the given gnoth is to have all of its top-level gloms (the direct subgloms of its pseudo-glom) dissolved, bringing their subgloms to the top-level" |
| MERGE | `(MERGE <gnoth> <glomlist>)` | "glomlist is a list of neighbor gloms in order within the given gnoth. The listed gloms are all uncovered and their subgloms glommed into a 'merge'-type glom, which becomes a top-level glom in the gnoth" |
| NO-OP | `(NO-OP <gnoth>)` | uncaps the gnoth until a Capper recaps it (below) |

NO-OP in full: "which causes the gnoth to be 'uncapped' -- have its pseudo-glom suspended -- and
remain that way until the Capper task it sets out is invoked and recaps the gnoth" [M86 p. 113].
"This 'slow-recap' permits natural glomming to occur within the gnoth, and between gnoths should
two neighboring gnoths be uncapped simultaneously. The Capper finds all current gloms whose ranges
overlap with the original range of the gnoth (before it was uncapped) and claims them for the
gnoth. Should two different gnoths claim the same glom -- one formed, perhaps, by combining gloms
from the two gnoths -- the gnoth that recaps first will get the glom and the extended range" [M86
p. 113]. (The `type` field of a glom, section 5.2, thus also takes the values `enclose` and
`merge`.) When a Reformulator proposes a NO-OP, and with what urgency the Capper is placed, is not
said (decisions, item 05).

#### 13.3 The before/after examples, as test vectors

Notation: `gnothN: [ ... ]` is the gnoth's parenthesised print-value as the thesis prints it
(square brackets = the gnoth, round = gloms, bare digits = glints at the top level); the glom
rows under it name the top-level gloms left to right and, with arrows, the gloms that span
them. Every example is transcribed from the image of its page.

**SHIFT-LEFT** [M86 p. 110], diagram on p. 111:

| | gnoth2 | gnoth3 |
|---|---|---|
| initial | `[(1 1) (2 2)]` = glom3 glom5 | `[(3 3) (4 4) (4 4)]` = glom7 glom10 glom15 |
| operation | `(SHIFT-LEFT gnoth2 gnoth3 (glom7 glom10))` | |
| final | `[(1 1) (2 2) (3 3) (4 4)]` = glom3 glom5 glom7 glom10 | `[(4 4)]` = glom15 |

The gloms keep their identity across the shift (glom7 and glom10 are the same gloms, now under
gnoth2); the diagram shows both gnoths re-capped.

**SPLIT** [M86 p. 111], diagram on p. 112:

| | gnoth2 | new gnoth |
|---|---|---|
| initial | `[(1 2) (2 3) (2 3)]` = glom2 glom6 glom10 | |
| operation | `(SPLIT gnoth2 (glom6 glom10))` | |
| final | `[(1 2)]` = glom2 | `gnoth(new): [(2 3) (2 3)]` = glom6 glom10 |

**CAPTURE-RIGHT** [M86 p. 114]:

| | gnoth1 |
|---|---|
| initial | `[(1 2) 3]` = glom1 glint3 |
| operation | `(CAPTURE-RIGHT gnoth1 glom1 glint3)` |
| final | `[(1 2 3)]` = glom2 (new; glom1 destroyed) |

**CAPTURE-LEFT** [M86 p. 114], diagram on p. 115 ("will cause glom10 to 'swallow' its
neighbor to the left, glom7, within gnoth1" [M86 p. 115]):

| | gnoth1 |
|---|---|
| initial | `[((1 2) (2 3) ((2 3) (2 3))) (1 2)]`; top level glom15 glom18; glom15 = (glom5 glom7 glom10); glom10 = (glom8 glom9); glom5 = (1 2), glom7 = (2 3), glom8 = (2 3), glom9 = (2 3), glom18 = (1 2) |
| operation | `(CAPTURE-LEFT gnoth1 glom10 glom7)` |
| final | `[(1 2) ((2 3) (2 3) (2 3)) (1 2)]`; top level glom5 glom20 glom18; glom20 = (glom7 glom8 glom9) (new) |

Two things to note, both confirmed by the diagram: glom15, which contained the operands, is
burst and *not* rebuilt (glom5 and glom20 sit at the top level beside glom18), and the
captive glom7 keeps its identity inside the new glom20 while glom10 is destroyed.

**ENCLOSE, simple** [M86 p. 115]:

| | gnoth1 |
|---|---|
| initial | `[1 2 3 1]` = glints 1, 2, 3, 4 |
| operation | `(ENCLOSE gnoth1 (glint1 glint2 glint3))` |
| final | `[(1 2 3) 1]` = glom1 glint4 |

**ENCLOSE, nested** [M86 p. 116], diagram on the same page:

| | gnoth3 |
|---|---|
| initial | `[(2 2) ((3) ((3 3) (4 4))) ((4 4) (5 5))]`; top level glom1 glom12 glom9; glom12 = (glom4 glom10); glom10 = (glom6 glom5); glom9 = (glom7 glom8); glom1 = (2 2), glom4 = (3), glom6 = (3 3), glom5 = (4 4), glom7 = (4 4), glom8 = (5 5) |
| operation | `(ENCLOSE gnoth3 (glom5 glom7))` |
| final | `[(2 2) (3) (3 3) ((4 4) (4 4)) (5 5)]`; top level glom1 glom4 glom6 glom20 glom8; glom20 = (glom5 glom7) (new) |

The two operands were under different parents (glom10 inside glom12, and glom9), so glom12,
glom10 and glom9 are all burst to bring glom5 and glom7 to the top, and nothing is rebuilt
around glom4, glom6 and glom8. (In the diagram glom10's children are drawn as glom6, glom5 in
that order.)

**FRACTURE** [M86 p. 117], diagram on the same page:

| | gnoth2 |
|---|---|
| initial | `[(1 2) ((3 3) (4 4))]`; top level glom1 glom5; glom1 = (glint1 glint2); glom5 = (glom4 glom3); glom4 = (3 3), glom3 = (4 4) |
| operation | `(FRACTURE gnoth2)` |
| final | `[1 2 (3 3) (4 4)]` = glint1 glint2 glom4 glom3 |

Only one level is dissolved: glom1 and glom5 go, their children rise.

**MERGE, simple** [M86 p. 118]:

| | gnoth1 |
|---|---|
| initial | `[(1 1) (1 1 1)]` = glom1 glom2 |
| operation | `(MERGE gnoth1 (glom1 glom2))` |
| final | `[(1 1 1 1 1)]` = glom3 (new) |

**MERGE, nested** [M86 p. 118], diagram on p. 119:

| | gnoth1 |
|---|---|
| initial | `[((1 2 3) (3 4)) (((5 6) (4 5)) (6 7 8))]`; top level glom4 glom19; glom4 = (glom1 glom2); glom19 = (glom7 glom10); glom7 = (glom5 glom6); glom1 = (1 2 3), glom2 = (3 4), glom5 = (5 6), glom6 = (4 5), glom10 = (6 7 8) |
| operation | `(MERGE gnoth1 (glom4 glom7))` |
| final | `[((1 2 3) (3 4) (5 6) (4 5)) (6 7 8)]`; top level glom20 glom10; glom20 = (glom1 glom2 glom5 glom6) (new) |

glom19 is burst to free glom7, glom10 rises to the top, and the merged glom holds the
*subgloms* of the listed gloms (glom4 and glom7 are "uncovered"), not the listed gloms
themselves.

#### 13.4 Operations in series: the two worked examples

"The gnoth operations described above can be used to reformulate the gnoth-based parse of the
sequence (as opposed to the hypothesis-based parse) when applied in series. Two examples
follow" [M86 p. 119].

**Series 1** [M86 pp. 119-120]: "we start out with three gnoths which parenthesize the sequence
segment '1 2 3 3 4 3 4 5' as shown initially. After several operations, a more 'reasonable'
final parenthesization emerges" [M86 p. 119]. Glints are glint1 to glint8 for the eight terms.
Transcribed from the image of p. 120:

| Step | Operation | gnoth1 | gnoth2 | gnoth3 |
|---|---|---|---|---|
| 0 | initial | `[(1 2 3)]` = glom1 | `[((3 4) (3 4))]` = glom4 over glom2 glom3 | `[5]` = glint8 |
| 1 | `(SHIFT-LEFT gnoth2 gnoth3 glint8)` | `[(1 2 3)]` | `[((3 4) (3 4)) 5]` | `[]` (disappears) |
| 2 | `(CAPTURE-RIGHT gnoth2 glom3 glint8)` | `[(1 2 3)]` | `[(3 4) (3 4 5)]` = glom2 glom5 | |
| 3 | `(FRACTURE gnoth1)` | `[1 2 3]` | `[(3 4) (3 4 5)]` | |
| 4 | `(ENCLOSE gnoth1 (glint1 glint2))` | `[(1 2) 3]` | `[(3 4) (3 4 5)]` | |
| 5 | `(SPLIT gnoth2 (glom5))` | `[(1 2) 3]` | `[(3 4)]` | `[(3 4 5)]` (emerges) |
| 6 | `(SHIFT-LEFT gnoth2 gnoth3 (glint6))` | `[(1 2) 3]` | `[(3 4) 3]` | `[4 5]` |
| 7 | `(ENCLOSE gnoth3 (glint7 glint8))` | `[(1 2) 3]` | `[(3 4) 3]` | `[(4 5)]` |

Readings settled on the image: step 1's glomlist is printed bare, `glint8`, not `(glint8)`
(step 6 prints `(glint6)`); step 2 bursts glom4 (the cap over glom2 glom3) so that glom3 can
be replaced by the new glom5 = (3 4 5), and glom4 is not rebuilt; step 6 moves glint6 (the
third 3) out of glom5, which must be burst to free it, leaving gnoth3 with the bare glints
`[4 5]`; step 7 repairs that. The final parenthesization is `[(1 2) 3] [(3 4) 3] [(4 5)]`, as
printed.

**Series 2** [M86 p. 121]: "three gnoths which exchange gloms and are reformulated internally
to come up with a new, more coherent parenthesization -- this time of the sequence segment
'1 2 3 3 4 5 6 4 5 6 7 8'" [M86 p. 121]. Glints glint1 to glint12. From the image of p. 121:

| Step | Operation | gnoth1 | gnoth2 | gnoth3 |
|---|---|---|---|---|
| 0 | initial | `[(1 2 3)]` = glom1 | `[(3 4) (5 6) (4 5)]` = glom2 glom3 glom4 | `[(6 7 8)]` = glom5 |
| 1 | `(SHIFT-RIGHT gnoth2 gnoth3 (glom4))` | `[(1 2 3)]` | `[(3 4) (5 6)]` | `[(4 5) (6 7 8)]` |
| 2 | `(MERGE gnoth3 (glom4 glom5))` | `[(1 2 3)]` | `[(3 4) (5 6)]` | `[(4 5 6 7 8)]` = glom6 |
| 3 | `(FRACTURE gnoth2)` | `[(1 2 3)]` | `[3 4 5 6]` | `[(4 5 6 7 8)]` |
| 4 | `(ENCLOSE gnoth2 (glint4 glint5 glint6 glint7))` | `[(1 2 3)]` | `[(3 4 5 6)]` = glom7 | `[(4 5 6 7 8)]` |

The initial gnoth2 of series 2, `(3 4) (5 6) (4 5)`, is not in sequence order (its print-value
reads 3 4 5 6 4 5), which is how the page prints it; the operations treat the gloms by name,
and the result is in order. Both series are test vectors for the operations composed, checked
on the print-values after every step.

#### 13.5 Bonds into gnoth operations: PROGRAMs

"The conversion of bond pulls and pushes into gnoth operations simply requires that care be
taken about who is attracting whom and how deeply nested each of the participants is in its
original gnoth. When more than a straightforward top-level move is to be required, a
Reformulator must create a PROGRAM or series of moves designed to put the proper glom in its
proper place and repair as much resulting gnoth-tearing as possible" [M86 p. 121].

**High-level move** [M86 p. 122]: in Figure 2, "if glom8 is to be pulled away from gnoth4 by
glom15, this high-level attraction is translated into the move: (SHIFT-LEFT gnoth3 gnoth4
(glom8))" [M86 p. 122]. A top-level glom needs one operation.

**Deeper move** [M86 p. 122]: "If, however, glom1 is to be pulled away by glom7, we have a
more complicated situation" [M86 p. 122]; "a translation of the result is" (transcribed from the
image, with the thesis's bracketed glosses):

```
(PROGRAM ((SHIFT-LEFT gnoth3 gnoth4 (glom1))            [move glom1 over]
          (CAPTURE-RIGHT gnoth3 glom10 glom1)           [swallow it]
          (ENCLOSE gnoth4 siblings-of-glom1)            [replace glom3]
          (ENCLOSE gnoth4 newglom3&sibs-of-glom3)       [replace glom8]
          (ENCLOSE gnoth3 sibs-of-glom10&newglom10)     [replace glom15]
        ))
```

Read against Figure 2: glom1 is two levels down in gnoth4 (glom8 > glom3 > glom1) and glom7 one
level down in gnoth3 (glom15 > glom10 > glom7; the pull is between glom7 and glom1, and the
captor named is glom10, glom7's parent, so that the moved glom joins glom7's group). The shift
bursts glom8 and glom3 on the right and the capture bursts glom15 and glom10 on the left; the
three ENCLOSEs rebuild, bottom up, a replacement for glom3 from glom1's former siblings, a
replacement for glom8 from that and glom3's former siblings, and a replacement for glom15 from
glom10's siblings and the new glom10. The symbolic arguments `siblings-of-glom1`, `newglom3`
and the like are the thesis's shorthand for lists that are only known when the earlier steps
have run: a PROGRAM's later steps refer to the results of its earlier ones.

**A real move** [M86 p. 122]: "In most cases, such deep nesting is not encountered. In the case
of '1 2 1 2 3' [Figure 1], the 2 <--> 3 pull is resolved via:" (from the image; the states are
the thesis's own annotations at the right):

```
(PROGRAM (    ((1 2)) ((1 2)) (3)
          (SHIFT-LEFT gnoth2 gnoth3 (glint5))     --> ((1 2)) ((1 2) 3)
          (CAPTURE-RIGHT gnoth2 glom2 glint5)     --> ((1 2)) ((1 2 3))
          (ENCLOSE gnoth2 nil)                    [no repair necessary]
          (ENCLOSE gnoth3 nil)                    [no repair necessary]
        ))
```

So a PROGRAM is the move, the capture that nests the moved glom where the pull came from, and
one ENCLOSE per gnoth touched, with `nil` when nothing was burst; and the single-glom gnoth3
empties (p. 108's "shell which the system destroys"). The transcript's Gnoth-operator lines
show the same shape with the current printed form: `((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2
(glint1)) (ENCLOSE gnoth1 nil))))` [M86 p. 174] and `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))`
[M86 p. 177] (a bare operation, no PROGRAM wrapper; the glomlist printed is `(glint3)` on the
image although the resulting cap glom14 covers terms 3 to 5 [M86 p. 178]; item 04 transcribes
it).

The rule for PROGRAM construction: "Reformulator processes are responsible for creating such
'PROGRAM's as described above. For the most part, once the initial move or two have been supplied,
the remainder of the PROGRAM is designed simply and mechanically to repair any concomitant
structural damage" [M86 p. 123]. "Such damage is usually caused by the need to burst a glom in
order to get at one of its subgloms, perhaps even one nested several levels below it. The damage
is repaired by re-enclosing the remaining gloms at each intermediate level -- those not directly
involved in the operation -- and setting out Plato-scouts on the newly-enclosed gloms. This last
step is taken to determine whether any 'interesting' new structures have been created" [M86 pp.
122-123]. And the thesis's insistence: "It should be emphasized that PROGRAM construction is a
mechanical action, performed by a task that exists at a high enough level to possess the necessary
vocabulary. The Reformulator's activity in writing a PROGRAM is no more intelligent than a
Glommer's or a Bonder's, or that of any other Seek-Whence task. Whatever 'intelligence' the
Seek-Whence system possesses is an emergent phenomenon arising from the performance of all of
these mechanical tasks in parallel" [M86 p. 123].

#### 13.6 Divesting pushes (not implemented)

"In addition to neighbor pulls, there is a second potentially strong agent for reformulation --
what we call a 'divesting push'. There may be no real pull between one glom in a gnoth and the
neighboring gnoth, but the glom's current home may not want it" [M86 p. 123]. "This sort of
unilateral decision to push out a glom and either foist it off on the neighbor or create a new
gnoth to hold it could be the foundation for much useful reformulation. Divesting pushes are not
implemented in the current system, causing some weakness in its performance when handling Tuples,
for example" [M86 p. 123]. The Problems chapter adds that such a push "will occur when a gnoth
contains a glom that causes it 'unhappiness' in the sense of decreasing its stability, but the
neighboring gnoth does not have any particular attraction for the glom either" [M86 p. 152], and
that implementing them would be "a safety valve for the current reigning class, by increasing
gnoth stability without calling for a new monarch" [M86 p. 152]. The reconstruction reproduces the
limitation: no divesting pushes; the Reformulator proposes only neighbour-pull moves (an
implementation would be an extension, off by default).

### 14. Carrying out reforms: Gnoth-operator, Bond-assessor, Gnoth-caster, failure and Slip-scouts

*Implemented in:* `seekwhence/operations.py` (the Gnoth-operator and the Capper of 14.1; D-067), `seekwhence/reforms.py` (the Bond-assessor of 14.2; D-047), `seekwhence/casting.py` (the Gnoth-caster, the casts and their generalisation of 14.3, postdiction of 14.4, failure, the Slip-scout and the blast of 14.5; D-037, D-048 to D-050). The open points of 14.6 are decided in D-038 to D-052. Tests: `tests/test_operations.py`, `tests/test_reforms.py`, `tests/test_casting.py`.

Read on the page images of pp. 124-126 and 92, with the transcript pages 168-169, 174 and 177-178.

#### 14.1 The Gnoth-operator

"A Gnoth-operator task is charged with carrying out the operation or PROGRAM given it by a
Reformulator at the time of its creation. It must first check to see that all the structures
relevant to its operation are still in existence, having survived the system's activity while
the Gnoth-operator was hanging on the taskrack. If the relevant structures do still exist, the
Gnoth-operator carries out the operations; if not, it will simply terminate. When a
Gnoth-operator does in fact operate, its last action is to decrease the system's
bond-strength threshold by 1. The effect of this threshold reduction is to encourage the system
to make more reformulations by allowing weaker bonds to be considered, in effect 'heating up'
the environment. Reformulators, by adding 1 to the threshold, have the opposite effect, cooling
things down. Eventually, the system will settle as the Reformulators find fewer and fewer
relevant operations to suggest to Gnoth-operators, reflecting the fact that the gnoths are
stabilizing" [M86 p. 124].

The threshold is therefore one global number per reformulation episode: set to 80% of the
reigning class's strongest pull when there is none (12.1), +1 by each Reformulator, -1 by each
Gnoth-operator that acts. When it is cleared (at a new hypothesis, at a slip to a new reigning
class, or never) is not said: a decision (item 05). The transcript's `(S-group 8.0)`, `9.0`,
`10.0` on p. 178 rise by one per Reformulator with no Gnoth-operator between, which fits.

The printed form: `doing task Gnoth-operator` then the operation or PROGRAM on the next line
[M86 pp. 174, 177], then `top-down glom glomN` for the new cap.

#### 14.2 The Bond-assessor

"A Bond-assessor task is created each time a Reformulator decides that it has finished
finding interesting gnoth operations at some particular level. The Bond-assessor's job is to
look at all current bonds and determine whether or not there is reason to continue
reformulation. If there are sufficiently strong bonds to warrant further reforms, the
Bond-assessor places a Reformulator on the taskrack. If not, it creates a Gnoth-caster task
instead and terminates" [M86 p. 124].

The printed form is `doing task Bond-assessor on (S-group 8.0)` [M86 pp. 174, 177, 178], the
reigning class and a number. The thesis does not say what the number is; it starts at 8.0 for
S-group and rises by 1 with each Reformulator, which is exactly the threshold of 12.1 (80% of
10, then +1, +1). The reading implemented: the Bond-assessor is created with the reigning class
and the current threshold as its arguments, and "sufficiently strong" means a bond whose
assessed pull in the reigning environment reaches the threshold. On pp. 174 and 178 the
sequence is Reformulator, Bond-assessor 8.0, Reformulator, Bond-assessor 9.0, Reformulator,
Bond-assessor 10.0, Gnoth-caster: the strongest pull is 10, so at 10.0 nothing is left above
it and the Gnoth-caster follows. Whether "reaches" is `>=` or `>` is a decision (item 05); the
transcript on p. 178 shows a Bond-assessor at 10.0 giving way to the Gnoth-caster while a
bond of strength 10 exists, so with `>=` the Bond-assessor at 10.0 would have placed another
Reformulator: the transcript favours strictly greater, which the Reformulator's own rule
("exceed the threshold", p. 107) also uses.

#### 14.3 The Gnoth-caster: casts and generalisation

"When invoked, a Gnoth-caster attempts to describe each gnoth in terms of the reigning class.
In more sophisticated versions of the program, there will be provision for casting gnoths in
terms of more complicated but still incomplete forms -- such as '(C-group (S-group m n) (Cycle
(1 4)))' or '(Y-group [3] ((C-group m n) 8 shared))', where 'm' and 'n' have no numerical
value. This will be necessary when more complicated Seek-Whence descriptions are required to
parse target sequences" [M86 pp. 124-125]. (Those two forms are future notation, read on the
image of p. 125: `[3]` in square brackets and `8 shared` are printed so; the reconstruction
does not implement them.)

"Since each gnoth is supposed to represent one frame of the hypothesis, such casting must be
possible if the class is right and the gnoths are correctly formed" [M86 p. 125].

Generalisation: "If all the gnoths can be cast, or if all but the last can and it shows
promise, the Gnoth-caster then attempts to create a more general form common to all the
castings. For example, if the term groupings generated by the gnoths are: (3 4) (3 4) (3), then
the form '(S-group 3 2)' would be generated. In our slightly more complicated running example,
given (1 2) (1 2 3), the form '(S-group 1 (Countup 2))' is generated" [M86 p. 125]. "The casting
process is quite mechanical, as currently implemented, and so errors or poor castings are
possible. A final test -- to weed out any surviving bad casts -- is made of a cast when it
becomes a hypothesis candidate" [M86 p. 125].

The casts and generalisations the sources give, as test vectors:

| Reigning class | Gnoth groupings | Casts (as printed) | Generalised form | Source |
|---|---|---|---|---|
| S-group | `(3 4) (3 4) (3)` | (not printed) | `(S-group 3 2)` (the last gnoth, `(3)`, "shows promise") | p. 125 |
| S-group | `(1 2) (1 2 3)` | `((S-group 1 2) (S-group 1 3))` | `(S-group 1 (Countup 2))` | p. 125; transcript p. 178 |
| S-group | `(1 2) (1)` | `((S-group 1 2) (S-group 1 1))` | `(S-group 1 2)` | transcript p. 174 |
| Cycle | `(3) (7) (3)` | `((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3)))` | `(Cycle 3 3 (3 7 3))` | transcript p. 168 |

Three generalisation rules are visible in these: a parameter constant across the casts stays
(the start 1, the start 3); a parameter that counts up by one across the casts becomes
`(Countup first)` (lengths 2, 3 give `(Countup 2)`); a last cast that is a prefix of the others
(`(3)` after `(3 4) (3 4)`; `(1)` after `(1 2)`) is "promising" and takes the others'
parameter (`(S-group 3 2)`, `(S-group 1 2)`). The Cycle case concatenates one-element Cycle
casts into one Cycle over all the terms. The thesis states none of these as rules; they are
the readings the package implements and tests, and the exact generalisation algorithm (what
happens when neither rule applies, how many casts must agree, what "shows promise" means) is a
decision (item 05).

The printed form [M86 pp. 168, 174, 178]: `doing task Gnoth-caster`, `casts: ((S-group 1 2)
(S-group 1 3))`, the generalised form on its own line, then `new hypoth candidate (S-group 1
(Countup 2))`; on p. 168 the casts and the form are printed without the `doing task` line and
without `new hypoth candidate` (the second run is in the terser mode, section 1).

#### 14.4 Testing the candidate: postdiction

"The casting form returned, if any, now becomes a hypothesis candidate. A 'box', or predictive
model, is made for it and is tested to see whether it can accurately 'postdict' the known terms of
the sequence" [M86 p. 125]. "If so, the candidate is instantiated as the new hypothesis for the
system, which can now sit back in the 'certainty' that its new model is the correct one for the
given sequence. At this point, the system typically calls for the next term in order to test its
new hypothesis" [M86 p. 125]. In the transcripts `new hypoth candidate ...` is followed at once by
`doing task Call-term` [M86 pp. 174, 178], and the hypothesis then has validity 0 again, since the
first confirming term after it produces the guess (section 8.5): the postdicted terms are not
counted as confirmations (a reading; item 05).

#### 14.5 Failure and the Slip-scout

"If the Gnoth-caster is unable to cast all the gnoths in terms of the reigning class, or if it
cannot generalize the casts to come up with a candidate, or if the candidate fails to postdict
the sequence properly, the reformulation effort has failed. In each such case, a 'Slip-scout'
process is placed on the taskrack" [M86 p. 126].

"Slip-scouts are only skeletally implemented in the current system, a partial explanation for its
floundering in many cases when initial reformulation fails. When invoked, a Slip-scout will make a
more detailed study of the potential for slipping to another reigning class, and the probability
of a class change will increase" [M86 p. 126]. "The Slip-scout will look at all existing bonds to
find frequently-occurring types and will be especially sensitive to the possibility of an
interleaving of two or more independent subsequences. It will use the knowledge of which classes
favor what bonds to help suggest a new reigning class, or perhaps a subclass within a reigning
Cycle or Tuple" [M86 p. 126]. "This seems to be the point where Simon and Kotovsky [Simon 63]
began their program -- looking for a cycle. If so, we have now almost completed the substrate
necessary for a system to support heuristics of their sort in a fluid, non-mechanistic way" [M86
p. 126].

What the reconstruction does: a Slip-scout exists and is placed on each of the three failures;
it does the one thing the thesis states as fact, "the probability of a class change will
increase", and no more (the detailed study, the frequency count and the interleaving
sensitivity are described as intentions, "will", not as what the program did). How the
probability is increased, and how a failed episode continues (another Gnoth-setter? the next
term? the blast of 9.5?), is not said: decisions (item 05), with the thesis's verdict on the
Blackburn dozen (section 1) as the measure of what the skeleton should and should not achieve.

#### 14.6 What chapter 3 leaves to decisions

For item 05, the points in pp. 87-126 that the thesis does not settle:

1. The blast fallback: what "no progress for quite some time" measures, which task blasts,
   and whether the glints' bonds survive (9.5).
2. Freeze-dried hypotheses: what happens when a candidate repeats one; whether "the number
   of terms it explained" is used; who destroys the box (10.2).
3. The Gnoth-setter: how many pseudo-gloms it makes per gnoth; the frame numbering of the
   gnoths it creates after the catchall (10.1).
4. S-links: the full table (neighbours and slipperiness for all eight classes; symmetry);
   which classes are "strict" (Constant and, by the transcript, Countup); how "recent tries"
   enters the decision; which task prints `slip-check` and announces a slip (11.1, 11.2, 11.6).
5. Pull-push tables for the eight classes: the thesis gives S-group (+10 adjacent successor,
   -5 remote sameness, 0 adjacent predecessor), Y-group (10 remote sameness), and C-group's
   preferences without numbers (11.1); whether `best: nil` means no neighbours or none
   scoring.
6. The Reformulator: how a pull "with each in the list" is combined; what "the pull exerted
   by other gloms to hold them in place" sums; whether competing moves are weighted by
   strength or the first strong one wins; when NO-OP is proposed; the Capper's urgency;
   when the threshold is cleared (12.2, 12.3, 13.2, 14.1).
7. The Bond-assessor's test (`>` versus `>=` against the threshold) (14.2).
8. The Gnoth-caster: the casting of one gnoth as a form of each class; the generalisation
   rules and "shows promise" (14.3); whether postdicted terms count as confirmations (14.4).
9. The Slip-scout's effect and what follows a failed episode (14.5).

### 15. The appendix runs and the reported results

*Implemented in:* `tests/data/` (the three transcriptions of 15.1-15.4 and `expectations.json`; `tests/test_data.py`), `seekwhence/milestones.py` (the milestones as lines, matched within their turn; D-055), `seekwhence/bench.py` (`appendix`: the runs replayed over many seeds; `dozen`: the sequences of 15.6 over many seeds; D-070), `seekwhence/sequences.py` (the catalogue of 15.6-15.7), `seekwhence/trace.py` (the formats of 15.5). The measures of 15.8 are reported in `RESULTS.md`, `docs/appendix-runs.md` and `docs/dozen.md`. Tests: `tests/test_appendix.py`, `tests/test_dozen.py`, `tests/test_data.py`.

Read on the page images of pp. 164-179 (the three runs), p. 88 (the solved list), pp. 145-153
(performance and problems) and pp. 6-7 (the dozen). The runs are transcribed line for line in
`tests/data/run1_3737.txt`, `run2_373373.txt` and `run3_121231234.txt`; their milestones, the
dozen and the reported results are in `tests/data/expectations.json`; `tests/test_data.py`
checks that the three files agree with each other. This section says what the data files hold,
what the runs show that the prose does not, and what the reconstruction is measured against.

#### 15.1 The three runs as data

The appendix prints three complete console sessions, each headed by its sequence
[M86 pp. 164, 167, 170]: `3 7 3 7 3 7` (pp. 164-166), `3 7 3 3 7 3` (pp. 167-169) and
`1 2 1 2 3 1 2 3 4` (pp. 170-179), the last being the running example of chapters 3-5
[M86 p. 88]. The transcription files keep every console line as printed: the user's input
stays on the prompt line, the program's own continuation lines keep their one leading space,
glint names in spark and bond lines are run together (`glint1glint2`), and the four places
where the 72-column printout wrapped a line are kept as two lines and listed in the file's
header. Meredith's annotations, printed with a leading semicolon (`;  A template is made.`)
[M86 p. 165], become `# note:` lines; `# page: N` marks where each printed page begins; the
header of each file lists every reading that had to be settled on the image. One reading is
not certain: p. 168 prints `top-down glom glom71` between `glom6` and `glom8`, read as `glom7`
with a stray character. The second run is printed without the `doing task` lines and without
`-> (startup)` [M86 p. 167], so the task stream can be read only from runs 1 and 3.

`expectations.json` gives, for each run, the terms entered and every console input in order,
the milestones in order with their printed page (terms; the first spark and bond; glom
proposals, gloms, failed gloms, dissolved gloms; Plato-scouts; the template; hypotheses;
`gnoths constructed`; the `show`, `show-hypothesis`, `show-seq` and `show-parse` outputs;
hypothesis filtering with its result; the refutation line; Gnoth-setter output; slip-checks;
the change of reigning class; Bond-assessor values; gnoth operations and PROGRAMs; casts and
hypothesis candidates; guesses with their answers), the set, first-appearance order and counts
of task types, and the spark, bond and glom numbering. The test walks each run's lines with a
cursor and requires every milestone in order on its page.

#### 15.2 Run 1: `3 7 3 7` [M86 pp. 164-166]

| Milestone | Printed | Page |
|---|---|---|
| terms 1-3 | `3`, `7`, `3`; `Sparkler-plus` tasks on the new glint at once, Dissolvers on old glints, sparks 1-10 | 164 |
| first bond | `bond1 --- between glint1glint3`, from `Bonder on (Same print-value (remote) glint1 glint3)` | 164 |
| proposals | `Same -cover proposed --> glint1`, `Same -fence proposed --> glint1`, two Glomtesters | 165 |
| first glom | `Glommer for Same print-value fence members: (glint1 glint2)`: glom1 = `(3 7)`; the cover `(glint1 glint2 glint3)` fails | 165 |
| dubbing | `Plato-scout on (Cycle glom1)` | 165 |
| template | `Template-applier on (glom1 (Cycle 3 2 (3 7)))`, `create-template-glom (Cycle 3 2 (3 7))(glom1)`, `top-down glom glom2`, `template made : (form (Cycle 3 2 (3 7))`, `check-cycle template (Cycle 3 2 ((3 7)))` | 165 |
| hypothesis | `Hypothesizer` prints `(Cycle 3 2 (3 7))`; `Gnoth-maker`, `top-down glom glom3`, `gnoths constructed`; `Call-term` | 165 |
| term 4 | `7`; `Hfilter`, `new term being hypothesis-filtered through (Cycle 3 2 (3 7))`, `top-down glom glom4` | 166 |
| guess | `I have a guess!`, `hypothesis: (Cycle 3 2 (3 7))`, `3 7 3 7`, `enter no if wrong, ok if right ok`, `bye` | 166 |

Seventeen task types appear, in this order of first appearance: Sparkler-plus, Dissolver,
Sparkler, Tester, Bonder, Glom-scout, Glomtester, Glommer, Glom-inspector, Plato-scout,
Template-scout, Template-applier, Template-evaluator, Hypothesizer, Gnoth-maker, Call-term,
Hfilter; 53 task lines in all, 12 sparks, 1 bond, gloms up to glom4 [M86 pp. 164-166].

#### 15.3 Run 2: `3 7 3 3 7 3` [M86 pp. 167-169]

The first three terms produce exactly the lines of run 1 (sparks 1-12, bond1, the two
proposals, glom1, the failed cover, the template, glom2, glom3) [M86 p. 167]; here the template
is printed with all four fields: `template made : (form (Cycle 3 2 (3 7)) state working
coverage (1 2) glom glom2)` [M86 p. 167]. `show` prints the terms, `bond1 Same print-value
(remote) -- (glint1 glint3)`, `glom1 (Same print-value fence) --> (3 7) terms 1 to 2`,
`glom3 pseudo --> ((3 7))  terms 1 to 2`, and gnoth1 with `plato-class: Cycle`, `glom: glom3`,
`form: (((Cycle 3 2 (3 7)) pure))`, `state: stable`, `range: (1 2)` [M86 pp. 167-168].

| Milestone | Printed | Page |
|---|---|---|
| term 4 | `3`; `new term being hypothesis-filtered through (Cycle 3 2 (3 7))`; sparks 13-17 | 168 |
| refutation | `set-out -- validity: 0`; `top-down glom glom4`, `groups: ((glint1 glint2))`, `glom: (glint1 glint2)`, gloms 5-8, `gnoths: (gnoth1 gnoth2 gnoth3)` | 168 |
| slip-check | `slip-check: stayval: 0.0  best: nil`: Cycle stays (annotation: "The system will continue to let 'Cycle' reign.") | 168 |
| bonds | `bond2 --- between glint3glint4`, `bond3 --- between glint1glint4` | 168 |
| casts | `casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3))) (Cycle 3 3 (3 7 3))`, `new hypoth candidate (Cycle 3 3 (3 7 3))` | 168-169 |
| term 5 | after `show-seq` (`3 7 3 3`): `7`; filtered through `(Cycle 3 3 (3 7 3))`, `top-down glom glom9` | 169 |
| guess | `hypothesis: (Cycle 3 3 (3 7 3))`, `3 7 3 3 7`, `ok`, `bye` | 169 |

No Reformulator, Bond-assessor or Gnoth-operator line is visible (none is printed in this run),
and the casts come straight after two new bonds: the reformulation of a Cycle that stays a
Cycle recast the three one-term gnoths `(3) (7) (3)` as the three-term cycle [M86 p. 168].

#### 15.4 Run 3: `1 2 1 2 3 1 2 3 4` [M86 pp. 170-179]

| Milestone | Printed | Page |
|---|---|---|
| terms 1-2 | `1` (then `empty-task`), `2`; `bond1` from `(Succ print-value (adjacent) glint1 glint2)` | 170 |
| first glom | `Pred -group proposed --> glint2` (twice), `Glommer for Pred print-value group members: (glint1 glint2)`: glom1 = `(1 2)`; then `Glomtester failed glint2` | 170 |
| dubbing | `Plato-scout on ((S-group P-group) glom1)`, `Plato-evaluator on (P-group glom1)` | 170 |
| template | `create-template-glom (S-group 1 2)(glom1)`, `top-down glom glom2`, `template: (form (S-group 1 2)`, `Template-evaluator on nil` | 170-171 |
| hypothesis 1 | `Gnoth-maker`, `top-down glom glom3`, `gnoths constructed`; `Hypothesizer` prints `(Countup 1)`; `Call-term` | 171 |
| show | gnoth1: `plato-class: S-group`, `glom: glom3`, `form: (((S-group 1 2) pure))`, `range: (1 2)` | 171 |
| term 3 | `1`; filtered through `(Countup 1)`; no `set-out` line; `bond2` (Pred adjacent glint2 glint3), `glom3 is not in cytoplasm`, `bond3` (Same remote glint1 glint3), `Same -cover/-fence proposed --> glint3`, `bond4` (Pred adjacent glom1 glint3) | 171-173 |
| Gnoth-setter | `top-down glom glom4` to `glom8`, `gnoths: (gnoth1 gnoth2 gnoth3)`; no slip-check line | 173 |
| scouting | `Plato-scout on ((C-group S-group P-group Y-group Cycle Tuple) glom6)`, seven `Template-scout on (glom6)` | 173-174 |
| reigning class | `Reformulator`; annotation "Changing to a new reigning class. Countup --> S-group" (nothing printed by the program); `Bond-assessor on (S-group 8.0)` | 174 |
| operation | `Gnoth-operator` `((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil))))`, `top-down glom glom9` | 174 |
| assessors | `Bond-assessor on (S-group 8.0)`, `9.0`, `10.0`, each after a `Reformulator` | 174 |
| hypothesis 2 | `Gnoth-caster`, `casts: ((S-group 1 2) (S-group 1 1))`, `(S-group 1 2)`, `new hypoth candidate (S-group 1 2)`, `Call-term` | 174 |
| show | `show-parse` `((1 2) (1))`; bonds 1-3; `glom8 pseudo --> (1)  terms 3 to 3`, `glom9 pseudo --> (1 2)  terms 1 to 2`; gnoth2 (`frame: 1`, `glom: glom9`, `form: (S-group 1 2)`, `range: (1 2)`, `external-bonds: ((bond2 0) (bond3 -5))`, `internal-bonds: ((bond1 10))`, `equivalence-type: parse`) and gnoth3 (`frame: 2`, `glom: glom8`, `form: (S-group 1 1)`, `range: (3 3)`, `internal-bonds: nil`) | 174-175 |
| term 4 | `2`; filtered through `(S-group 1 2)`, `top-down glom glom10` | 175 |
| guess 1 | `hypothesis: (S-group 1 2)`, `(1 2)(1 2)(1 2)`, answer `nope` | 175 |
| term 5 | after `show-seq` (`1 2 1 2`): `3`; `new term being hypothesis-filtered through (1 2)1`; `glom8 is not in cytoplasm`, `glom10 is not in cytoplasm` | 176 |
| Gnoth-setter | `glom11`, `glom12`, `glom13`, `gnoths: (gnoth2 gnoth3 gnoth5)` | 176 |
| slip-check | `slip-check: stayval: 15.0  best: (Y-group 4.0)`: S-group stays | 176 |
| bonds | `bond5` (Same remote glint2 glint4), `bond6` (Succ remote glint2 glint5), `bond7` (Succ adjacent glint3 glint4) | 176-177 |
| operation | `Reformulator`; `Gnoth-operator on ((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))`, `top-down glom glom14`; `Bond-assessor on (S-group 8.0)`; `bond8` (Succ adjacent glint4 glint5) | 177 |
| assessors | `Bond-assessor on (S-group 8.0)`, `9.0`, `10.0` | 178 |
| hypothesis 3 | `casts: ((S-group 1 2) (S-group 1 3))`, `(S-group 1 (Countup 2))`, `new hypoth candidate (S-group 1 (Countup 2))` | 178 |
| show | `show-parse` `((1 2) (1 2 3))`; bonds 1, 2, 3, 5, 6, 7, 8 (bond4 gone); `glom9 pseudo --> (1 2)  terms 1 to 2`, `glom14 pseudo --> (1 2 3)  terms 3 to 5`; gnoth2 with `external-bonds: ((bond2 0) (bond5 -5) (bond6 0) (bond3 -5))`, gnoth5 (`frame: 2`, `glom: glom14`, `form: (S-group 1 3)`, `range: (3 5)`, `external-bonds: ((bond6 0) (bond5 -5) (bond2 0) (bond3 -5))`, `internal-bonds: ((bond8 10) (bond7 10))`) | 178-179 |
| term 6 | after `show-seq` (`1 2 1 2 3`): `1`; filtered through `(S-group 1 (Countup 2))`, `top-down glom glom15` | 179 |
| guess 2 | `hypothesis: (S-group 1 (Countup 2))`, `(1 2)(1 2 3)(1 2 3 4)`, `ok`, `bye` | 179 |

Twenty-three task types appear, in this order of first appearance: Sparkler-plus, Dissolver,
empty-task, Sparkler, Tester, Bonder, Glom-scout, Glomtester, Glommer, Glom-inspector,
Plato-scout, Plato-evaluator, Template-scout, Template-evaluator, Gnoth-maker, Hypothesizer,
Call-term, Hfilter, Gnoth-setter, Reformulator, Bond-assessor, Gnoth-operator, Gnoth-caster;
192 task lines, 30 sparks, 8 bonds, gloms up to glom15 [M86 pp. 170-179]. Six of the 29 task
types of section 2.4 are never printed: Template-resolver, Template-reviewer, Burster, Capper,
Slip-scout and, in run 3, Template-applier (run 1 prints it).

#### 15.5 What the runs show that the prose does not

Each of these is an observation on the printed pages, listed here so that a decision (item 05
and later) can cite it; none is stated in the prose.

1. **Startup.** The first term produces only `Sparkler-plus on (glint1 glint1 2)` and a
   `Dissolver` (run 3 adds `empty-task`); the second term starts Sparklers, Testers and a
   `Dissolver on (glint1)` [M86 pp. 164, 170]. Dissolvers on bare glints do nothing visible.
2. **Sparkler-plus.** Its argument is a pair of cyto-elements, the newer first, and a number
   that is always 2 or 10 (`(glint2 glint1 10)`, `(glint2 glint2 2)`, `(glom1 glom1 10)`,
   `(glint3 glom3 2)`); some of these lines are followed by a spark line and some are not, and
   the same pair can be tried several times [M86 pp. 164-165, 171-173, 176]. Sparks are also made
   between an element and itself (`(glint3 glint3 2)` makes no spark) [M86 p. 164].
3. **Sparks before bonds.** Nine sparks and many Testers precede the first bond in run 1, and
   the only bond in runs 1-2 before the template is the remote sameness `glint1`-`glint3`
   [M86 p. 164]; the successor bond `glint1`-`glint2` is the first bond of run 3 and its
   Bonder is re-run several times without a new bond ("checks to see that they are not
   already bonded in this way" [M86 p. 64]) [M86 pp. 170, 172].
4. **The group label.** The successor pair `1 2` is proposed by `Pred -group proposed -->
   glint2` and glommed as `(Pred print-value group)`, although its bond is `Succ print-value
   (adjacent)` [M86 pp. 170-171]: the Glom-scout names the family from the newer glint's
   point of view, which is the predecessor side.
5. **Which classes a glom is scouted for.** `(3 7)` goes to `(Cycle glom1)` alone; `(1 2)` to
   `((S-group P-group) glom1)`, and the Plato-evaluator is then run on `(P-group glom1)`, the
   class that failed; after the first refutation a top-down glom is scouted for all six
   classes `((C-group S-group P-group Y-group Cycle Tuple) glom6)` [M86 pp. 165, 170, 173].
6. **Template lines.** Run 1 prints `template made : (form (Cycle 3 2 (3 7))`, run 2 the
   same with `state working coverage (1 2) glom glom2)`, run 3 `template: (form (S-group 1
   2)` and `Template-evaluator on nil`; the Cycle template is checked by `check-cycle
   template (Cycle 3 2 ((3 7)))` with the turn doubly parenthesised [M86 pp. 165, 167, 171].
7. **The first hypothesis of run 3 is `(Countup 1)`** while the template and gnoth1's form
   are `(S-group 1 2)` [M86 p. 171]: the Hypothesizer does not simply copy the template form.
   Its refutation by the third term `1` prints no `set-out -- validity: 0` line, whereas run
   2's refutation does [M86 pp. 168, 171].
8. **The Gnoth-setter** prints one `top-down glom` per gnoth and catchall part (five in both
   first refutations, three in run 3's second) and the gnoth list; in run 2 it also prints
   `groups: ((glint1 glint2))` and `glom: (glint1 glint2)` after the first glom
   [M86 pp. 168, 173, 176]. The gnoth numbering continues (`gnoth5` after `gnoth3`) and the
   list after the second refutation is `(gnoth2 gnoth3 gnoth5)` [M86 p. 176].
9. **The slip-check** is printed after the Gnoth-setter in run 2 (`stayval: 0.0 best: nil`)
   and after the second Gnoth-setter of run 3 (`stayval: 15.0 best: (Y-group 4.0)`,
   reconciled in section 11.5), but not after the first Gnoth-setter of run 3, where the
   reigning class changes from Countup to S-group and the change is announced only by
   Meredith's annotation [M86 pp. 168, 173-174, 176]. Both printed checks end in staying.
10. **Reformulation at the top.** In both reformulations of run 3 the Bond-assessor is
    printed with `8.0`, `8.0`, `9.0`, `10.0`, each preceded by a Reformulator, and the first
    Gnoth-operator comes between the first two [M86 pp. 174, 177-178]: the threshold starts at
    80% of the strongest pull (10) and rises by 1 per Reformulator as section 12.1 says, and
    the Gnoth-caster runs once the Bond-assessor sees nothing above 10. Run 2's Cycle
    reformulation prints none of these.
11. **Bond values in a gnoth's environment** (S-group reigning): adjacent successor `10`,
    remote sameness `-5`, adjacent predecessor and remote successor `0` [M86 pp. 175, 179];
    the external-bond lists of neighbouring gnoths list the same bonds in different orders.
12. **The casts and the generalisation**: `(3) (7) (3)` cast as three one-term Cycles and
    generalised to `(Cycle 3 3 (3 7 3))`; `(1 2) (1)` as `(S-group 1 2) (S-group 1 1)` to
    `(S-group 1 2)`; `(1 2) (1 2 3)` as `(S-group 1 2) (S-group 1 3)` to `(S-group 1 (Countup
    2))` [M86 pp. 168, 174, 178] (section 14.3).
13. **After a refused guess** the program asks for the next term; the following Hfilter
    prints `new term being hypothesis-filtered through (1 2)1` rather than a form, and the
    two gloms of the refused parse are dissolved before the Gnoth-setter runs
    [M86 pp. 175-176]. `bond4`, the Pred bond from `glom1` to `glint3`, is absent from the
    last `show` [M86 p. 178]: a bond dies with its glom.
14. **The guess block.** The four guesses print `3 7 3 7` after the four terms `3 7 3 7`,
    `3 7 3 3 7` after the five terms `3 7 3 3 7`, `(1 2)(1 2)(1 2)` after `1 2 1 2`, and
    `(1 2)(1 2 3)(1 2 3 4)` after `1 2 1 2 3 1` [M86 pp. 166, 169, 175, 179]. One rule fits all
    four: the box is fired from the first term for at least three hits and until every term
    entered so far is covered (four and five one-term hits for the Cycles; three hits for the
    groups, the third of `(S-group 1 (Countup 2))` completing the term `1` just entered). The
    prose gives no rule (section 8.5); this reading is the evidence for the decision of item 12.
15. **One confirming term.** Every guess follows the first term that passes hypothesis
    filtering after the hypothesis was made [M86 pp. 166, 169, 175, 179] (section 1.4).
16. **Exact replay is impossible.** The task order depends on random choice [M86 p. 42] and no
    seed is printed, so the reconstruction cannot reproduce these runs line for line as the
    Numbo and Metacat ports reproduce their oracles. What it is measured against is the
    milestones in order (the same gloms, templates, hypotheses, refutations, casts and guesses
    for the same terms), the printed formats, and the numbers that follow from the mechanisms
    (the bond values, the stay value, the thresholds), not the sparks and task counts.

#### 15.6 The reported results

**Implementation.** "The Seek-Whence program currently consists of approximately 5400 lines of
Franz Lisp code" [M86 p. 145], run interpreted on a shared VAX 11/750; "successful runs are
generally completed in under ten minutes of real time. Unsuccessful runs take a bit longer
(potentially forever), as the program thrashes about for a solution" [M86 p. 145].

**The solved sequences** [M86 p. 88], transcribed from the image: `1 1 1 ...`, `1 2 3 4 ...`,
`1 1 1 2 2 2 3 3 3 ...`, `3 4 3 4 3 4 ...`, `3 7 3 7 3 7 ...`, `3 7 3 3 7 3 3 7 3 ...`,
`16 15 14 17 16 15 18 17 16 ...`, `16 15 14 15 14 13 14 13 12 ...` ("as well as possible,
given a non-infinite pattern" [M86 p. 88]) and `1 2 1 2 3 1 2 3 4 ...`. Chapter 5 adds, next to
the Blackburn problems the program fails, four more it solves: `2 2 3 3 3 4 4 4 4`,
`1 1 2 2 2 3 3 3 3` [M86 p. 146], `1 8 5 8 1 1 8 5 8 1` [M86 p. 146] and the third run's
`1 2 1 2 3 1 2 3 4` [M86 p. 145].

**The Blackburn dozen** [M86 pp. 6-7], with the program's result [M86 pp. 145-147]:

| # | Sequence | Result | What the thesis says | Page |
|---|---|---|---|---|
| 1 | `1 1 2 1 2 3 1 2 3 4 ...` | unsolved | "The program thrashes hopelessly on this one, although it readily solves" `1 2 1 2 3 1 2 3 4`; the initial C-group interferes with finding the lengthening S-groups, which reach the template level but cannot be pushed beyond it | 145 |
| 2 | `1 2 3 4 ...` | **solved** | "quite readily, in just under one minute" | 145 |
| 3 | `2 1 2 2 2 2 2 3 2 2 4 2 ...` | not attempted | "This is hopeless as yet; we have not even attempted it. There is far too much interference -- terms having multiple potential roles"; "a distant goal" | 145-146 |
| 4 | `1 2 2 3 3 3 4 4 4 4 ...` | unsolved | the initial S-groups `(1 2)` and `(2 3)` interfere with the budding C-group notion; "the correct notion is discovered, but cannot seem to break through into a hypothesis"; but it solves `2 2 3 3 3 4 4 4 4` and `1 1 2 2 2 3 3 3 3` | 146 |
| 5 | `1 8 5 8 1 8 5 8 ...` | unsolved | finds the Y-group `1 8 5 8 1` "and doggedly clings to it. We stopped it after a fifteen-minute attempt"; but it solves `1 8 5 8 1 1 8 5 8 1` | 146 |
| 6 | `2 1 2 2 2 3 2 4 2 5 ...` | unsolved | "too much interference here, combined with an alternation of terms. This is beyond the current system" | 146 |
| 7 | `2 3 1 2 3 2 2 2 3 3 3 3 2 3 4 4 4 4 ...` | unsolved | "far beyond the current system. It combines interference, interleaving, and growing group lengths" | 146 |
| 8 | `1 2 2 3 3 4 4 5 ...` | **solved** | "The system solves this within three minutes" | 146 |
| 9 | `1 2 3 3 4 4 5 5 5 6 6 6 ...` | unsolved | "This will prove difficult for a while yet. The subtle pattern of growth in group lengths will tax the system's representation scheme" | 146-147 |
| 10 | `9 1 9 2 9 3 9 4 ...` | unsolved | "The only problem here is the alternation of terms of which at least one is non-constant. This will probably be the next sequence solved by the system" | 147 |
| 11 | `1 8 1 2 1 8 1 2 3 2 1 8 1 2 3 4 3 2 1 8 1 2 3 4 ...` | unsolved | "This sequence is hard. There is interference between the groups, which grow at both ends. The expressive power is available, but the system gets bogged down in spurious relationships" | 147 |
| 12 | `1 8 5 5 8 1 1 8 5 5 8 1 ...` | **solved** | "The system solves this, but can take up to twenty minutes to do so. It finds a Y-group, but often it is the Y-group" `(1 8 (5 (5 8 1 1 8 5) 5) 8 1)` "rather than the one we would like" | 147 |

The summary: "the current Seek-Whence program can solve only three of the Blackburn dozen --
problems 2, 8, and 12. With slight extension, it should solve problem 10 as well. It will have
to cling less forcefully to its original formulation in order for it to solve problems 1, 4,
5, and 6" [M86 p. 147]; "The system's interference handling will need improvement before it
can handle problems 3, 7, and 11. The solution of problem 9 will probably require that group
lengths be used as manifestations (they are not, currently)" [M86 p. 147].

**Human difficulty** [M86 p. 148], from the twenty-five subjects: problem 7 was the hardest
(seven people passed on it, no more than two on any other), 11 was also clearly more
time-consuming; "The 'easiest' problems were (2) and (10), followed by (8), then (4) and (5),
then (6) and (12), and then (1), (3), and (9)" [M86 p. 148]; the problems the program solves
or is closest to solving are among the easier ones for people [M86 p. 148].

#### 15.7 The stated failures and problems [M86 pp. 148-153]

The three summary faults: the program "fails to notice interleaved sequences of any
complexity", "is unable to handle interference well" and "clings too tenaciously to its first
organizing notion" [M86 p. 149]. In detail:

- **Implementation faux pas** [M86 p. 149]: a one-person, "expr-based" Lisp program with
  "some disregard for 'neatness' in cleaning up old, unwanted structures"; "unabashedly 'ad
  hoc'", with no separation of domain-dependent from domain-independent processing.
- **Unconquered sequences** [M86 p. 150]: interleaved sequences whose components are more
  than constants: it can do `3 7 3 7 3 7` "but it cannot as yet do" `1 2 10 3 4 10 5 6 10`
  (read on the image; the text layer garbles it). The reason given: "Currently, bonds are used
  only in a bottom-up fashion, to push up gloms", while the platoplasm's knowledge of the bond
  types each class favours (many adjacent samenesses suggest C-group) is not used, and
  "must be used cautiously, since it may lead to false conclusions" (Blackburn #3 has many
  adjacent samenesses but no C-group) [M86 p. 150]. Knowledge about manifestations and
  actualizations "is virtually unused by the current system" [M86 p. 150]. Sequences with much
  interference, `2 1 2  2 2 2  2 3 2  2 4 2` and `1 1 1  1 2 1  1 3 1`, cannot be analysed;
  people find a place of least confusion, a strategy "suitable at the Slip-scout level and
  beyond" [M86 pp. 150-151]. The clinging to early notions "probably stems from the same root
  causes" [M86 p. 151].
- **Low-level myopia** [M86 p. 151]: favourite early gloms can push up `((1 2) 3)` instead of
  `(1 2 3)`; "Its effects will be mitigated when divesting pushes, cosmetic reform, and
  'form-polishing' are implemented" [M86 p. 151], none of which is.
- **High-level haughtiness** [M86 pp. 151-152]: once the high levels take over there is too
  little room for low-level coercion, so the system sticks with a formulation type or Platonic
  class too long; the "Little Prince Problem": a good notion "has come up repeatedly to become
  a template, and then to disappear, never to reach hypothesis status" [M86 p. 152].
  **Divesting pushes** are proposed as the remedy and are not implemented [M86 p. 152]
  (section 13.6).
- **Coordination problems** [M86 pp. 152-153]: the hypothesis and the gnoths must agree on the
  parse; the equivalence levels (section 7.4) "give us something of a handle on the problem"
  but no mechanism keeps the two in step automatically [M86 p. 153].
- **Herky-jerky** [M86 p. 153]: reforms at the highest level are rough rather than fluid;
  differentiated plato-links (instead of the undifferentiated s-links) are proposed.

#### 15.8 What the reconstruction is measured against

`RESULTS.md` reports, for the three runs, whether the reconstruction reaches the same
milestones in the same order for the same terms (15.5, items 20-21) and, for the Blackburn dozen
and the p. 88 list, which it solves and which it does not, beside the thesis's results above
(item 22: `python3 -m seekwhence.bench dozen`, every sequence over many seeds, each guess
judged against the true continuation, D-070).
A sequence solved that Meredith reports unsolved, or the reverse, is a finding to report, not
a bug to hide (TASK.md). The task-type set printed in the runs (15.2, 15.4) is the minimum the
trace must be able to print; the six types never printed are implemented from the prose only.
