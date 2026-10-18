"""Milestones of the appendix runs, matched against a session's output [M86 pp. 164-179]
(DECISION D-055: comparing a stochastic program with a published run).

The three appendix runs are transcribed in `tests/data/run*.txt`, and `expectations.json`
lists their milestones in order: the terms, the first spark and bond, the proposals, gloms,
templates, hypotheses, the `show` printouts, the filtering lines, the Gnoth-setter's output,
slip-checks, Bond-assessor thresholds, operations, casts, candidates and guesses (SPEC 15).
Because the original's seed is unknown and "no assumptions can be made about which of two
competing tasks will run first" [M86 p. 41], the reconstruction is compared on milestones in
order, formats and mechanism-derived numbers, never on task counts or spark numbers
(DECISION D-055). This module turns each milestone into the printed lines it requires
(`needles`), matches them in order against the lines of one of our sessions (`match`), and
compares the first-appearance order of task types (`task_order_agreement`). Two readings are
reported for every milestone: `exact`, the lines as printed, structure names included, in
the printout's order, and `reached`, the milestone found in its turn in any order with every
structure number but a glint's (`glom4`, `spark6`, `bond1`) treated as a wildcard, since the
numbering depends on the run's history (DECISION D-014, D-056) and independent tasks of one
turn run in any order [M86 p. 41].
"""

import re
from dataclasses import dataclass, field

# The structures whose numbers are not milestones (DECISION D-055): a loose needle matches
# any number after these names.
NUMBERED = ("glom", "spark", "bond", "gnoth", "pstruc", "box")

TASK_LINE = re.compile(r"^doing task (\S+)")


# ---- lines ---------------------------------------------------------------------------------------

def logical_lines(lines) -> list:
    """A session's output lines with the continuation lines joined to the line they continue
    (the trace indents `best:` under `slip-check:` as the printout does [M86 p. 168]; a
    transcription's line that closes the parentheses the previous one left open is one of
    the printout's 72-column wraps, SPEC 15.1) and whitespace collapsed; blank lines
    dropped. The same joining as the transcriptions' reader."""
    out = []
    for raw in lines:
        if not raw.strip():
            continue
        wrapped = (out and out[-1].count("(") > out[-1].count(")")
                   and raw.count(")") > raw.count("("))
        if out and (raw.startswith(" ") or wrapped):
            out[-1] = " ".join((out[-1] + " " + raw).split())
        else:
            out.append(" ".join(raw.split()))
    return out


def sexp(items) -> str:
    """A list printed in the transcripts' way: `(a b c)` [M86 p. 165]."""
    return "(" + " ".join(items) + ")"


# ---- needles ---------------------------------------------------------------------------------------

@dataclass(frozen=True)
class Needle:
    """One printed line a milestone requires: `text` as printed, matched as a whole line
    when `exact`, as a substring otherwise; `window` lines may be joined for the search
    (the casts and their generalised form are on one line in run 2 and on two in run 3
    [M86 pp. 168, 174]); `alternatives`, other spellings accepted in the loose reading
    only (a spark line names its two elements in the order the Sparkler drew them,
    DECISION D-025). (DECISION D-055)"""
    text: str
    exact: bool = True
    window: int = 1
    alternatives: tuple = ()

    def pattern(self, loose: bool) -> re.Pattern:
        """The regular expression for this needle: literal when strict; with every
        structure number but a glint's a wildcard when loose (a glint's number is its
        term's position [M86 p. 61], a glom's, spark's or bond's depends on the run's
        history, DECISIONS D-014, D-055), and with the alternatives allowed."""
        texts = [self.text] + (list(self.alternatives) if loose else [])
        parts = []
        for text in texts:
            pattern = re.escape(" ".join(text.split()))
            if loose:
                pattern = re.sub(r"(" + "|".join(NUMBERED) + r")\d+", r"\1\\d+", pattern)
            parts.append(pattern)
        return re.compile("|".join(f"(?:{p})" for p in parts))

    def found_in(self, text: str, loose: bool) -> bool:
        """Whether the needle matches `text`, a logical line or a joined window of lines
        (DECISION D-055)."""
        pattern = self.pattern(loose)
        return bool(pattern.fullmatch(text)) if self.exact else bool(pattern.search(text))


def gnoth_lines(g: dict) -> list:
    """The printed lines of a gnoth under `show`, in the JSON's key order [M86 p. 168]."""
    out = []
    for key, value in g.items():
        if key in ("external-bonds", "internal-bonds"):
            text = "nil" if value == [] else sexp(f"({b} {v})" for b, v in value)
        elif key == "range":
            text = sexp(str(v) for v in value)
        else:
            text = str(value)
        out.append(f"{key}: {text}")
    return out


def needles(m: dict, task_lines: bool = True) -> list:
    """The needles of one milestone of `expectations.json`, in the order they must be found
    (SPEC 15.2-15.4; the kinds of SPEC 15.1; the lines as printed [M86 pp. 164-179]).
    Every kind of the three runs is covered; an unknown kind is an error. With
    `task_lines=False` the `doing task` needles are left out, for a printout without task
    lines (run 2 [M86 p. 167])."""
    out = _needles(m)
    if not task_lines:
        out = [n for n in out if not n.text.startswith("doing task")]
    return out


def _needles(m: dict) -> list:
    # One branch per milestone kind of `expectations.json`, each giving the printed lines
    # the kind requires; a table kept whole on purpose (`docs/citation-audit.md`, item 24).
    k = m["kind"]
    if k == "term":
        return [Needle(f"please enter a term: {m['value']}")]
    if k == "task":
        return [Needle(f"doing task {m['name']}")]
    if k == "spark":
        a, b = m["between"]
        return [Needle(f"{m['name']} --- between {a}{b}",
                       alternatives=(f"{m['name']} --- between {b}{a}",))]
    if k == "bond":
        a, b = m["between"]
        return [Needle(f"{m['name']} --- between {a}{b}")]
    if k == "glom-proposal":
        return [Needle(f"{m['family']} -{m['kind_proposed']} proposed --> {m['target']}")]
    if k == "glom":
        return [Needle(f"Glommer for {m['type'][1:-1]} members: {sexp(m['members'])}")]
    if k == "glom-failed":
        return [Needle(f"failed to glom {sexp(m['members'])}")]
    if k == "glomtester-failed":
        return [Needle(m["line"])]
    if k == "glom-dissolved":
        return [Needle(f"{m['glom']} is not in cytoplasm")]
    if k in ("plato-scout", "plato-evaluator"):
        classes = m["classes"][0] if len(m["classes"]) == 1 else sexp(m["classes"])
        task = "Plato-scout" if k == "plato-scout" else "Plato-evaluator"
        return [Needle(f"doing task {task} on ({classes} {m['glom']})")]
    if k == "template":
        out = [Needle(f"create-template-glom {m['form']}({m['from_glom']})"),
               Needle(f"top-down glom {m['template_glom']}")]
        if "line_as_printed" in m:
            out.append(Needle(m["line_as_printed"], exact=False))
        else:
            out.append(Needle(f"template made : (form {m['form']}", exact=False))
        if "coverage" in m:
            out.append(Needle(f"state {m['state']} coverage {sexp(map(str, m['coverage']))} "
                              f"glom {m['template_glom']})", exact=False))
        if "check" in m:
            out.append(Needle(m["check"]))
        if "evaluator_line" in m:
            out.append(Needle(m["evaluator_line"]))
        return out
    if k == "hypothesis":
        if m["ordinal"] == 1 and "note" not in m:
            return [Needle("doing task Hypothesizer"), Needle(m["form"])]
        return [Needle(m["form"], exact=False)]
    if k == "gnoths-constructed":
        return [Needle(f"top-down glom {m['gnoth_glom']}"), Needle("gnoths constructed")]
    if k == "command":
        out = [Needle(f"please enter a term: {m['name']}")]
        out += [Needle(line) for line in m.get("output", [])]
        if m["name"] == "show":
            out += [Needle("terms of the sequence:"), Needle(" ".join(map(str, m["terms"]))),
                    Needle("bonds:")]
            out += [Needle(line) for line in m["bonds"]]
            out.append(Needle("gloms:"))
            out += [Needle(line) for line in m["gloms"]]
            out.append(Needle("gnoths:"))
            for g in m["gnoths"]:
                out += [Needle(line) for line in gnoth_lines(g)]
        return out
    if k == "hfilter":
        through = m.get("through") or m["through_as_printed"]
        # p. 176 wraps the line before `(1 2)1`, so two lines may be joined
        out = [Needle("doing task Hfilter"),
               Needle(f"new term being hypothesis-filtered through {through}", exact=False,
                      window=2)]
        if "top_down_glom" in m:
            out.append(Needle(f"top-down glom {m['top_down_glom']}"))
        return out
    if k == "refutation":
        return [Needle(m["line"])]
    if k == "gnoth-setter":
        # the task line, its first top-down glom and the gnoths listed; the number of
        # `top-down glom` lines and p. 168's `groups:`/`glom:` lines are not reproduced
        # (DECISION D-036, D-055)
        gloms = m["top_down_gloms"]
        return [Needle("doing task Gnoth-setter"), Needle(f"top-down glom {gloms[0]}"),
                Needle(f"gnoths: {sexp(m['gnoths'])}")]
    if k == "slip-check":
        best = "nil" if m["best"] is None else f"({m['best'][0]} {m['best'][1]:.1f})"
        return [Needle(f"slip-check: stayval: {m['stayval']:.1f} best: {best}")]
    if k == "reigning-class-change":
        # the reconstruction prints its own line (DECISION D-042); the transcript has
        # Meredith's annotation only
        return [Needle(f"Changing to a new reigning class. {m['from_class']} --> {m['to_class']}")]
    if k == "bond-assessor":
        return [Needle(f"doing task Bond-assessor on ({m['reigning_class']} {m['threshold']:.1f})")]
    if k == "gnoth-operation":
        # p. 174 prints the PROGRAM on the lines after `doing task Gnoth-operator`, p. 177
        # after `on` as a continuation; ours prints `doing task Gnoth-operator on (...)`
        return [Needle("doing task Gnoth-operator", exact=False),
                Needle(m["program"], exact=False, window=2),
                Needle(f"top-down glom {m['top_down_glom']}")]
    if k == "casts":
        return [Needle(f"casts: {sexp(m['casts'])} {m['generalised']}", window=2)]
    if k == "new-hypothesis-candidate":
        return [Needle(f"new hypoth candidate {m['form']}")]
    if k == "guess":
        # run 1 indents the predicted terms, which join the hypothesis line [M86 p. 166]
        return [Needle("I have a guess!"), Needle(f"hypothesis: {m['hypothesis']}", exact=False),
                Needle(m["predicted_as_printed"], exact=False),
                Needle(f"enter no if wrong, ok if right {m['answer']}")]
    if k == "bye":
        return [Needle("bye")]
    raise ValueError(f"unknown milestone kind {k!r}")


def label(m: dict) -> str:
    """A short name for a milestone in a table: its kind and what identifies it
    (SPEC 15.2-15.4; DECISION D-055)."""
    k = m["kind"]
    extra = {
        "term": lambda: f"{m['index']}: {m['value']}",
        "task": lambda: m["name"],
        "spark": lambda: m["name"], "bond": lambda: m["name"],
        "glom-proposal": lambda: f"{m['family']} -{m['kind_proposed']} --> {m['target']}",
        "glom": lambda: f"{m['name']} {m['type']}",
        "glom-failed": lambda: sexp(m["members"]),
        "glomtester-failed": lambda: m["line"],
        "glom-dissolved": lambda: m["glom"],
        "plato-scout": lambda: f"{sexp(m['classes'])} {m['glom']}",
        "plato-evaluator": lambda: f"{sexp(m['classes'])} {m['glom']}",
        "template": lambda: m["form"],
        "hypothesis": lambda: m["form"],
        "gnoths-constructed": lambda: m["gnoth_glom"],
        "command": lambda: m["name"],
        "hfilter": lambda: f"{m.get('through') or m['through_as_printed']} ({m['result']})",
        "refutation": lambda: m["line"],
        "gnoth-setter": lambda: f"gnoths: {sexp(m['gnoths'])}",
        "slip-check": lambda: f"stayval {m['stayval']}, best {m['best']}",
        "reigning-class-change": lambda: f"{m['from_class']} --> {m['to_class']}",
        "bond-assessor": lambda: f"{m['reigning_class']} {m['threshold']}",
        "gnoth-operation": lambda: m["program"],
        "casts": lambda: m["generalised"],
        "new-hypothesis-candidate": lambda: m["form"],
        "guess": lambda: f"{m['hypothesis']}: {m['predicted_as_printed']} ({m['answer']})",
        "bye": lambda: "",
    }
    text = extra[k]() if k in extra else ""
    return f"{k} {text}".strip()


# ---- matching ---------------------------------------------------------------------------------------

@dataclass
class Match:
    """The result for one milestone: `found` when every needle was found in order after the
    previous milestone's match; `line`, the index of the last needle's line; `missing`, the
    first needle not found (DECISION D-055)."""
    milestone: dict
    found: bool
    line: int | None = None
    missing: Needle | None = None
    lines_found: list = field(default_factory=list)


PROMPT = "please enter a term: "


def turns(logical) -> list:
    """The start index of every turn of a session's logical lines: a turn begins at a
    `please enter a term:` line and runs to the next one [M86 p. 164]. The scripted inputs
    are the transcript's own, so the k-th turn of ours is the k-th turn of the printout."""
    return [i for i, line in enumerate(logical) if line.startswith(PROMPT)]


def match(lines, milestones, loose: bool = False, ordered: bool = True,
          task_lines: bool = True) -> list:
    """Match the milestones against a session's output (`logical_lines` of the text the
    session wrote), turn by turn: a `term` or `command` milestone is the k-th input and
    opens the k-th turn of ours; every other milestone is looked for inside the turn it was
    printed in, so a line printed only after a later term (say the fence glommed again after
    a blast) does not count for an earlier milestone. When `ordered`, a milestone must also
    come at or after the previous found milestone's line, the printout's order; otherwise
    anywhere in its turn, since independent tasks of one turn run in any order [M86 p. 41].
    The two readings reported are `exact` (`loose=False, ordered=True`: the lines as printed,
    in the printout's order) and `reached` (`loose=True, ordered=False`: the milestone
    happened in its turn, structure numbers aside). A milestone that is not found leaves the
    cursor where it was, and the later milestones are still looked for (DECISION D-055;
    the first divergence). Returns one `Match` per milestone."""
    logical = logical_lines(lines)
    starts = turns(logical) + [len(logical)]
    turn = -1               # the index of the current turn; -1 before the first input
    pos = 0
    out = []
    for m in milestones:
        if m["kind"] in ("term", "command"):
            turn += 1
        if turn >= len(starts) - 1:         # our session never reached this turn
            out.append(Match(m, False, None, needles(m, task_lines)[0], []))
            continue
        low, high = (starts[turn], starts[turn + 1]) if turn >= 0 else (0, starts[0])
        cursor = max(pos, low) if ordered else low
        found = True
        got = []
        missing = None
        for needle in needles(m, task_lines):
            hit = None
            for width in range(1, needle.window + 1):     # a single line before a window
                for i in range(cursor, high):
                    text = " ".join(logical[i:min(i + width, high)])
                    if needle.found_in(text, loose):
                        hit = i
                        break
                if hit is not None:
                    break
            if hit is None:
                found = False
                missing = needle
                break
            cursor = hit
            if logical[hit] not in got:
                got.append(logical[hit])
        if found:
            pos = cursor
        out.append(Match(m, found, cursor if found else None, missing, got))
    return out


def exact(lines, milestones) -> list:
    """The exact reading: the milestones' lines as printed, in the printout's order
    (DECISION D-055)."""
    return match(lines, milestones, loose=False, ordered=True)


def reached(lines, milestones) -> list:
    """The reached reading: each milestone found in its turn, structure numbers as
    wildcards, in any order (DECISION D-055)."""
    return match(lines, milestones, loose=True, ordered=False)


def task_types(lines) -> list:
    """The task types in the order they ran, from the `doing task` lines (SPEC 2.4;
    [M86 p. 164])."""
    return [m.group(1) for line in lines if (m := TASK_LINE.match(line))]


def first_appearances(types) -> list:
    """Each task type once, in the order of its first appearance (SPEC 15.1; DECISION
    D-055)."""
    seen = []
    for t in types:
        if t not in seen:
            seen.append(t)
    return seen


def task_order_agreement(ours, theirs) -> dict:
    """How the first-appearance order of task types in one of our runs agrees with the
    transcript's: the types they print that we never ran (`missing`), the ones we ran that
    they never printed (`extra`), and the share of the transcript's consecutive pairs
    (a before b) that hold in ours, over the pairs whose two types both ran (`pairs`,
    `agreeing`). Task order is not a milestone (DECISION D-055); this is a summary of how
    far the two racks agree on which kind of work comes first."""
    ours_first = first_appearances(ours)
    theirs_first = list(theirs)
    rank = {t: i for i, t in enumerate(ours_first)}
    pairs = agreeing = 0
    for a, b in zip(theirs_first, theirs_first[1:]):
        if a in rank and b in rank:
            pairs += 1
            agreeing += rank[a] < rank[b]
    return {
        "missing": [t for t in theirs_first if t not in rank],
        "extra": [t for t in ours_first if t not in theirs_first],
        "pairs": pairs,
        "agreeing": agreeing,
    }
