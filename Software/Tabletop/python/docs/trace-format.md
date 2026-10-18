# The traces of a run (final, item 21; the `RL` lines and the `blocked` answer added by the roles loop, D-105)

A run of the reconstruction can be watched in two forms, both produced by observers of the
engine's typed events (`docs/engine-api.md`, D-065) and neither read by the engine: the **text
trace**, one line per codelet for a person (`tabletop.observe.TextTrace`), and the **JSON-lines
trace**, one record per event for machines (`tabletop.observe.JsonLines`), which
`tabletop.run.fingerprint` hashes. The command line prints the first and writes the second
(`python3 -m tabletop PROBLEM --seed N [--json FILE]`, `python/README.md`).

## The text trace

The first line names the problem, the seed, the profile (and every field changed from
`standard`, as `field=value`), the engine that runs it (`compiled` or `python`, D-107) and
the touched object; then one line per codelet run; the last
line imitates French's report of a run [F95 p. 145; FCCA p. 392]. From a real run
(`python3 -m tabletop surround-05 --seed 3`):

```text
tabletop surround-05  seed 3  profile standard  engine compiled  Henry touches cup1
   1  T=81.9  a=1.5  FND-GP()  -> found g1 {spoon3 spoon4 small-glass1 fork3 fork4 knife2} gestalt horizontal; jolt knife +19; jolt group +14; jolt horizontal +19; posted 9 (FND-GP SAM FD-NNBR x2 FD-END FD-MID PK-SALO x2 FND-RL); jolt column +19; jolt group +19
      slipnet posts FND-GP x2 PK-SALO x2 PK-SALC RC-RL
      RL tag r1 knife2 COLUMN:middle w1 s5 by find-group
      RL tag r2 knife2 MEMBER w1 s6 by find-group
...
 211  T=81.9  a=1.5  CT-CP(small-glass1 cup1)/1  -> passed cp30 small-glass1=cup1 static 56 p=0.56; posted 1 (CP-COMP)
...
 217  T=81.9  a=1.5  CP-COMP(cp29)/3  -> entered cp29 spoon3=cup1 entered the Worldview without a fight, rating 53; jolt mirror symmetry +6; jolt sameness +6; jolt liquid holder +6; jolt sameness +6; posted 4 (BLD-CP CP-CLN CHK-WV RD-WVR)
...
 239  T=30.4  a=5.2  CP-COMP(cp30)/2  -> fight cp30 small-glass1=cup1 vs cp29: old 53 vs rival 68, p=0.78, rival wins; jolt mirror symmetry +6; jolt sameness +6; jolt smaller than +6; jolt bigger than +6; jolt sameness +6; posted 4 (BLD-CP CP-CLN CHK-WV RD-WVR)
...
 346  T=38.3  a=4.6  CP-COMP(cp30)/1  -> fight cp30 small-glass1=cup1 vs cp33: old 71 vs rival 60, p=0.32, old holds; posted 2 (CP-CLN CHK-WV)
      T 38.3 -> 36.6 (dQ 0.02 Q 0.72 dWV 4 Tcp 16) by competition
...
 384  T=28.8  a=5.3  FND-ANS()  -> answer Eliza touches cup2 (E2)
Eliza touches cup2 (E2). Structure 72. Codelets 384. T 28.3.
```

(The lines are the run's own, kept current by `tests/test_readme.py`; the `...` marks lines
left out. The `RL tag` lines are role tags built, here the knife's column and its
membership of Eliza's row, read at first sight when find-group brought the row into view,
D-085; the `T 38.3 -> 36.6` line a temperature update after a competition, the glass's
correspondence coming back to fight the lone cup's and losing. `slipnet posts ... RC-RL`
shows a role node posting a consumer's codelet top down, D-081. The run is v5's, the engine
of the structural Blockage fix (D-116 to D-120) on the defaults of D-115: it starts at
T = 81.9 and its jolts are 19 for a find, 6 for a slippage; v4's run of the same seed took
the lone cup after 305 codelets, v3's the glass, E1, after 144.)

A codelet line has these columns, separated by two spaces:

1. the codelet's ordinal: 1 for the first codelet run, as the screen dumps count codelets
   [F95 p. 96 (img)]; it is the `clock` of the `CodeletChosen` event;
2. `T=` the temperature at the start of the codelet, one decimal, as the thermometer prints it
   [F95 p. 98 (img)];
3. `a=` the exponent a(T) used to choose this codelet, one decimal [F95 p. 92];
4. the codelet: the Coderack strip's label [F95 p. 107 (img)] (D-051) and, in parentheses,
   its arguments as ids and structure names (`cup1`, `g4`, `cp2`), with its generation after
   a slash when it is not 0 (`FD-NNBR(fork2)/2`) [F95 p. 81];
5. `->` and what the codelet did (`CodeletRan`): an action from a fixed vocabulary, `found`,
   `built`, `proposed`, `passed`, `failed`, `picked`, `described`, `entered`, `fight`,
   `removed`, `answer`, followed by the codelet's own words (`built cp3 spoon4=cup1 static
   49`, `fight: old 274 vs rival 285, p=0.61, rival wins`), or `fizzled (reason)` for a
   codelet that did nothing [F95 p. 73]; then, after semicolons, the side effects in order:
   `jolt NODE +40` (every `NodeJolted` the codelet sent), `posted 6 (SAM FD-NNBR x2 FD-END
   FD-MID PK-SALO)` (its follow-ups, by label, consecutive repeats counted), `pop cp3` and
   `push cp3` (the context left and entered before it ran, D-021).

Lines that are not codelets, indented by six spaces, come after the codelet's line:

- `slipnet posts PK-SALO PK-SALC x4` when the Slipnet update after the codelet posts
  top-down codelets (D-020);
- `T 63.1 -> 55.1 (dQ 0.05 Q 0.78 dWV 4 Tcp 4) by schedule` when a temperature update changes
  T (`by` is `schedule`, `competition` or `change`, D-050); updates that leave T where it was
  print nothing;
- `aged out: PK-SALO(cup1) posted at 3` and `pushed off: ...` when a codelet leaves the rack
  unrun (D-053);
- `answer schedule: FND-ANS x2 at urgency 12.0` when the schedule hangs find-answer codelets
  (D-056);
- `rack reloaded with the starting set` when an empty rack was reloaded (D-055);
- the role events (D-105), one `RL` line each: `RL tag r1 cup2 LONER w1 s42 by perceived`
  (a tag built: its name, item, role, the arguments after a colon when it has any, its
  weight, its strength at tagging and who tagged it), `RL drop r1 cup2 LONER (lost to r2)`
  (a tag discarded, with the reason: `lost to rN`, `broken`, `structure gone`), `RL used
  cup2 LONER by C1: create-cp cup2 small-glass1` (a consumer read the tag), and `RL recast
  spoon1 [group:liquid holder,liquid holder,silverware member] tiers 3:spoon2:2 4:fork1:1`
  (the re-cast of the touched object's signature onto the other row, D-093: the signature
  in brackets, then each candidate as tier:candidate:weight, and `ambiguous` when the first
  non-empty tier is a tie).

With `--quiet` only the first and last lines are printed. In the last line, `(E1)` is the
problem file's label for the answer, `(none)` when no label names it, and the word `literal`
follows when the answer is the touched object (D-063). A capped run (D-061) ends `Codelets
3000 (capped).` In the full trace, the find-answer line of a default answer given while
a CLAIMED tag stands on an obvious counterpart (consumer C4, D-101) names the claim after
a semicolon, `literal; small-glass1 is claimed by small-glass2 through cp3`, and the
`AnswerGiven` record's `through` is `blocked`. An answer read from group position (D-058,
D-097) says so after the semicolon too: `the member of g2 with the touched object's roles
anchor middle` when consumer C3 matched the touched object's roles (and `RL used` lines
record the tags consumed), `the member of g2 in the touched object's position` when the
group-frame words decided, `the touched object is mapped only through g1` when neither found
a member.

Structures are named in the order they were built: `g1`, `g2`, ... for groups, `p1`, `p2`,
... for bottom-up proposals and `cp1`, `cp2`, ... for correspondences, with the members in
braces and the ends joined by `=` (`cp2 cup1=small-glass3`, `cp5 g2=g7`); names are never
reused. The drawing conventions of the screen (SPEC.md 6c.14) are for loop0006, not for the
text.

## The JSON-lines trace

One JSON object per line, one line per event, in the order the events were published. Every
record has these keys first, in this order:

| Key | Type | Meaning |
|---|---|---|
| `event` | string | the event's class name (`CodeletChosen`, `GroupBuilt`, ...) |
| `clock` | integer | codelets run when the event was published |
| `draws` | integer | the generator's draw count when the event was published (D-064) |

then the event's own fields, in the order of its dataclass (`docs/engine-api.md` lists them;
`tabletop.events.record_of`). Values are JSON numbers, strings, booleans, null, and lists of
these; engine objects never appear: a structure is its name (`"g4"`), an object its id, a node
its name, a codelet its type and arguments. Tuples are written as lists; floats are written
with `repr`, so two runs that differ in the last bit differ in the trace (the point of the
fingerprint). Keys are never sorted and no whitespace is added:
`json.dumps(record, separators=(",", ":"))`.

The first record is `RunStarted` (the problem's name, the seed, the profile, the engine,
`compiled` or `python` (D-107), every field that differs from `standard` as `[field, value]`
pairs, the touched object's id, the object ids in
the file's order) and the last is `RunEnded` (the answer's id and label, `literal`, `capped`,
the structure value, the codelet count, the final temperature, the Worldview as a list of
`[name, end, end]`). A run with the same problem, seed and profile produces byte-identical
lines in any process, under any `PYTHONHASHSEED` (the determinism check of the gate;
`tests/test_trace.py`). The first four records of `python3 -m tabletop surround-05 --seed 3
--json -` (the tests check that these lines are real):

```text
{"event":"RunStarted","clock":0,"draws":0,"problem":"surround-05","seed":3,"profile":"standard","engine":"compiled","changed":[],"touched":"cup1","objects":["fork1","fork2","knife1","cup1","spoon1","spoon2","spoon3","spoon4","small-glass1","fork3","fork4","knife2","cup2"]}
{"event":"CodeletPosted","clock":0,"draws":0,"codelet":"find-group","args":[],"raw":21,"generation":0,"urgency":21.0,"by":"start"}
{"event":"CodeletPosted","clock":0,"draws":0,"codelet":"find-group","args":[],"raw":21,"generation":0,"urgency":21.0,"by":"start"}
{"event":"CodeletPosted","clock":0,"draws":0,"codelet":"find-group","args":[],"raw":21,"generation":0,"urgency":21.0,"by":"start"}
```

The order of a run's records: `RunStarted`; the starting rack's 25 `CodeletPosted` (D-055; French's 24 and one find-roles, D-081);
`SlipnetUpdated` (the first update, D-017); `ObjectNoticed` and `DescriptionAdded` for the
touched object; `TemperatureUpdated` by `start`; then, per codelet, `CodeletRemoved` for the
codelets aged out, `CodeletChosen`, `ContextPopped` and `ContextPushed` if any, the events
of the codelet's own work (`NodeJolted`, `ObjectNoticed`, `GroupBuilt`, `CompetitionHeld`,
...), `CodeletRan` (and `CodeletFizzled` when it did nothing), `CodeletPosted` for its
follow-ups (with `CodeletRemoved` for what the capacity pushed off), `SlipnetUpdated` and the
`CodeletPosted` of its top-down postings, `TemperatureUpdated` when due, and
`AnswerCodeletPosted` with its `CodeletPosted` for each find-answer hung; last, `AnswerGiven`
and `RunEnded`.

## What the fingerprint hashes

`fingerprint(problem, seed, profile="standard")` (`tabletop.run`) runs the problem with the
hashing observer (`tabletop.observe.Hasher`) attached and no other, and returns the SHA-256
hex digest of the JSON lines (each ending in `\n`, fed to the hash one by one) and the
generator's draw count at `RunEnded`. The gate compares two problems and two seeds across two
processes with different `PYTHONHASHSEED`; each engine has its own set (D-107):
`tests/data/golden/compiled/fingerprints.json` and `tests/data/golden/python/fingerprints.json`
record the four digests, and the `*.txt` beside them the text traces of two of them, so that
any change of an engine's behaviour is seen at once and made on purpose. The two sets differ
in the `engine` field of `RunStarted` (and so in every digest) and, from the native Slipnet
on, wherever the compiled arithmetic differs; the draw counts are compared across engines
only statistically (RESULTS.md, "Engines and speed").
