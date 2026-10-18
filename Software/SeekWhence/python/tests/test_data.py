"""The transcribed appendix runs and the expectations file agree with each other.

`tests/data/run1_3737.txt`, `run2_373373.txt` and `run3_121231234.txt` are Meredith's three
program runs [M86 pp. 164-179], transcribed from the page images; `tests/data/expectations.json`
lists, for each run, the milestones in order (terms, sparks, bonds, gloms, templates,
hypotheses, refutations, slip-checks, gnoth operations, Bond-assessor values, casts, guesses
and answers) and the task types, plus the Blackburn dozen [M86 pp. 6-7] with the reported
results [M86 pp. 145-147], the solved sequences [M86 p. 88] and the stated failures
[M86 pp. 148-153]. These tests check that the files parse, that every milestone is found in
its run in the stated order on the stated page, that the task types, numbering and terms agree,
and that the results tables are consistent. They say nothing about the engine (not built yet).
"""

import json
import re
from pathlib import Path

import pytest

DATA = Path(__file__).resolve().parent / "data"
SPEC = Path(__file__).resolve().parents[1] / "SPEC.md"
RUN_FILES = ["run1_3737.txt", "run2_373373.txt", "run3_121231234.txt"]


# --- parsing the run files -----------------------------------------------------------------

class Run:
    """A transcribed run: console lines with their printed page, notes, and logical lines
    (continuation lines joined to the line they continue)."""

    def __init__(self, path: Path):
        self.path = path
        self.header = []      # the leading "# ..." lines before the first "# page:"
        self.lines = []       # (page, text) for every console line, blank lines dropped
        self.notes = []       # (page, text) for every "# note:" line
        self.page_marks = []  # pages in the order of their "# page:" markers
        page = None
        for raw in path.read_text().splitlines():
            if raw.startswith("# page:"):
                page = int(raw.split(":")[1])
                self.page_marks.append(page)
            elif raw.startswith("# note:"):
                self.notes.append((page, raw[len("# note:"):].strip()))
            elif raw.startswith("#"):
                assert page is None, f"{path.name}: comment after the first page marker: {raw!r}"
                self.header.append(raw)
            elif raw.strip():
                assert page is not None, f"{path.name}: console line before any page marker"
                self.lines.append((page, raw))
        self.logical = self._join_continuations()

    def _join_continuations(self):
        """Join a line with the lines that continue it: lines beginning with a space (the
        program's own continuation lines) and lines needed to balance its parentheses (the
        printout's wraps). Returns (page, text) with single spaces."""
        out = []
        for page, text in self.lines:
            wrapped = (out and out[-1][1].count("(") > out[-1][1].count(")")
                       and text.count(")") > text.count("("))
            if out and (text.startswith(" ") or wrapped):
                prev_page, prev = out[-1]
                out[-1] = (prev_page, " ".join((prev + " " + text).split()))
            else:
                out.append((page, " ".join(text.split())))
        return out

    def terms_entered(self):
        return [int(t) for _, t in self.lines
                if (m := re.fullmatch(r"please enter a term: (\S+)", t)) for t in [m.group(1)]
                if t.lstrip("-").isdigit()]

    def inputs(self):
        return [m.group(1) for _, t in self.lines
                if (m := re.fullmatch(r"please enter a term: (\S+)", t))]

    def tasks(self):
        return [m.group(1) for _, t in self.lines if (m := re.match(r"doing task (\S+)", t))]

    def numbered(self, prefix):
        """The numbers of the 'sparkN --- between' or 'bondN --- between' lines, in order."""
        return [int(m.group(1)) for _, t in self.lines
                if (m := re.match(rf"{prefix}(\d+) --- between", t))]

    def top_down_gloms(self):
        return [m.group(1) for _, t in self.lines if (m := re.match(r"top-down glom (glom\d+)", t))]


class Cursor:
    """Sequential search through a run's logical lines: each needle must occur at or after
    the previous match, and the match's page must be the milestone's page."""

    def __init__(self, run: Run):
        self.run = run
        self.pos = 0

    def find(self, needle: str, page, exact: bool = False, window: int = 1):
        """`page` None: the line may be on any page (a `show` printout runs over a page)."""
        needle = " ".join(needle.split())
        lines = self.run.logical
        for i in range(self.pos, len(lines)):
            text = " ".join(t for _, t in lines[i:i + window])
            hit = (text == needle) if exact else (needle in text)
            if hit:
                assert page is None or lines[i][0] == page, (
                    f"{self.run.path.name}: {needle!r} found on p. {lines[i][0]}, expected p. {page}")
                self.pos = i
                return i
        raise AssertionError(f"{self.run.path.name}: {needle!r} not found after line {self.pos} "
                             f"({lines[self.pos][1] if self.pos < len(lines) else 'end'!r})")

    def expect_next(self, needle: str, page: int):
        """The very next logical line must be `needle`."""
        self.pos += 1
        got = self.run.logical[self.pos]
        assert got == (page, " ".join(needle.split())), f"{self.run.path.name}: expected {needle!r} next, got {got!r}"


@pytest.fixture(scope="module")
def expectations():
    return json.loads((DATA / "expectations.json").read_text())


@pytest.fixture(scope="module")
def runs():
    return {name: Run(DATA / name) for name in RUN_FILES}


# --- the files themselves --------------------------------------------------------------------

def test_run_files_have_headers_and_consecutive_pages(runs, expectations):
    for spec in expectations["runs"]:
        run = runs[spec["file"]]
        first, last = spec["pages"]
        assert run.page_marks == list(range(first, last + 1)), run.path.name
        assert any("[M86 pp." in h for h in run.header), "the header cites the pages"
        assert any("Doubtful readings" in h for h in run.header), "the header lists doubtful readings"
        assert run.lines[0][1].strip().startswith("The Sequence"), "the first line is the printed title"
        assert spec["title_as_printed"] == run.lines[0][1].strip()
        assert run.lines[-1][1] == "bye"


def test_expectations_json_shape(expectations):
    assert [r["id"] for r in expectations["runs"]] == [f[:-4] for f in RUN_FILES]
    kinds = set(expectations["milestone_kinds"])
    for spec in expectations["runs"]:
        assert spec["file"] in RUN_FILES
        for m in spec["milestones"]:
            assert m["kind"] in kinds, m
            assert spec["pages"][0] <= m["page"] <= spec["pages"][1], m
    assert len(expectations["blackburn_dozen"]) == 12


# --- the runs against their expectations -----------------------------------------------------

def test_terms_and_inputs(runs, expectations):
    for spec in expectations["runs"]:
        run = runs[spec["file"]]
        terms = run.terms_entered()
        assert terms == spec["terms_entered"], run.path.name
        assert terms == spec["sequence"][:len(terms)], "every run's terms are a prefix of its sequence"
        assert run.inputs() == spec["inputs_in_order"], run.path.name
        title_terms = [int(t) for t in run.lines[0][1].split()[2:]]
        assert title_terms == spec["sequence"], "the printed title is the sequence"
        assert (run.lines[1][1] == "-> (startup)") == spec["has_startup_line"]


def test_sparks_bonds_and_gloms_are_numbered_consecutively(runs, expectations):
    for spec in expectations["runs"]:
        run = runs[spec["file"]]
        sparks, bonds = run.numbered("spark"), run.numbered("bond")
        assert sparks == list(range(1, spec["spark_count"] + 1)), run.path.name
        assert bonds == list(range(1, spec["bond_count"] + 1)), run.path.name
        gloms = run.top_down_gloms()
        assert gloms[-1] == spec["last_glom"]
        numbers = [int(g[4:]) for g in gloms]
        assert numbers == sorted(numbers) and len(set(numbers)) == len(numbers)


def test_task_types(runs, expectations):
    for spec in expectations["runs"]:
        run = runs[spec["file"]]
        tasks = run.tasks()
        if spec["task_types"] is None:
            assert not spec["prints_task_lines"] and tasks == [], run.path.name
            continue
        assert spec["prints_task_lines"]
        order = []
        for t in tasks:
            if t not in order:
                order.append(t)
        assert order == spec["task_types"]["first_appearance_order"], run.path.name
        assert sorted(set(tasks)) == spec["task_types"]["set"]
        counts = {t: tasks.count(t) for t in set(tasks)}
        assert counts == spec["task_types"]["counts"]
        assert len(tasks) == spec["task_types"]["total"]


def test_task_types_are_in_the_spec_table(expectations):
    """Every task type printed in the transcripts is a row of SPEC.md section 2.4."""
    rows = re.findall(r"^\| \*\*([A-Za-z-]+)\*\* \|", SPEC.read_text(), flags=re.M)
    assert len(rows) >= 29
    for spec in expectations["runs"]:
        if spec["task_types"]:
            missing = set(spec["task_types"]["set"]) - set(rows)
            assert not missing, missing


def sexp(items):
    return "(" + " ".join(items) + ")"


def gnoth_lines(g):
    """The printed lines of a gnoth under `show`, in the order of the JSON's keys."""
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


def check_milestone(c: Cursor, run: Run, m: dict):
    k, p = m["kind"], m["page"]
    if k == "term":
        c.find(f"please enter a term: {m['value']}", p, exact=True)
        assert run.terms_entered()[m["index"] - 1] == m["value"]
    elif k == "task":
        c.find(f"doing task {m['name']}", p, exact=True)
    elif k in ("spark", "bond"):
        a, b = m["between"]
        if "type" in m and run.tasks():   # the Bonder line precedes the bond line
            c.find(f"doing task Bonder on ({m['type']} {m['field']} ({m['modifier']}) {a} {b})", p, exact=True)
            c.expect_next(f"{m['name']} --- between {a}{b}", p)
        else:
            c.find(f"{m['name']} --- between {a}{b}", p, exact=True)
    elif k == "glom-proposal":
        c.find(f"{m['family']} -{m['kind_proposed']} proposed --> {m['target']}", p, exact=True)
    elif k == "glom":
        c.find(f"Glommer for {m['type'][1:-1]}", p)
        c.find(f"members: {sexp(m['members'])}", p)
    elif k == "glom-failed":
        c.find(f"failed to glom {sexp(m['members'])}", p, exact=True)
    elif k == "glomtester-failed":
        c.find(m["line"], p, exact=True)
    elif k == "glom-dissolved":
        c.find(f"doing task Dissolver on ({m['glom']})", p, exact=True)
        c.expect_next(f"{m['glom']} is not in cytoplasm", p)
    elif k in ("plato-scout", "plato-evaluator"):
        classes = m["classes"][0] if len(m["classes"]) == 1 else sexp(m["classes"])
        task = "Plato-scout" if k == "plato-scout" else "Plato-evaluator"
        c.find(f"doing task {task} on ({classes} {m['glom']})", p, exact=True)
    elif k == "template":
        c.find(f"create-template-glom {m['form']}({m['from_glom']})", p, exact=True)
        c.expect_next(f"top-down glom {m['template_glom']}", p)
        # the template line may be on the next page (run 3: pp. 170-171)
        if "line_as_printed" in m:
            c.find(m["line_as_printed"], None, exact=True)
        else:
            c.find(f"template made : (form {m['form']}", None)
        if "coverage" in m:
            c.find(f"state {m['state']} coverage {sexp(map(str, m['coverage']))} glom {m['template_glom']})", None)
        if "check" in m:
            c.find(m["check"], None, exact=True)
        if "evaluator_line" in m:
            c.find(m["evaluator_line"], None, exact=True)
    elif k == "hypothesis":
        if m["ordinal"] == 1 and run.tasks():
            c.find("doing task Hypothesizer", p, exact=True)
            c.expect_next(m["form"], p)
        else:
            c.find(m["form"], p)
    elif k == "gnoths-constructed":
        c.find(f"top-down glom {m['gnoth_glom']}", p, exact=True)
        c.expect_next("gnoths constructed", p)
    elif k == "command":
        c.find(f"please enter a term: {m['name']}", p, exact=True)
        for line in m.get("output", []):
            c.expect_next(line, p)
        if m["name"] == "show":
            c.expect_next("terms of the sequence:", p)
            c.expect_next(" ".join(map(str, m["terms"])), p)
            c.expect_next("bonds:", p)
            for line in m["bonds"]:
                c.expect_next(line, p)
            c.find("gloms:", None, exact=True)       # a show printout may run over the page
            for line in m["gloms"]:
                c.expect_next(line, run.logical[c.pos + 1][0])
            c.find("gnoths:", None, exact=True)
            for g in m["gnoths"]:
                for line in gnoth_lines(g):
                    c.expect_next(line, run.logical[c.pos + 1][0])
    elif k == "hfilter":
        c.find("doing task Hfilter", p, exact=True) if run.tasks() else None
        through = m.get("through") or m["through_as_printed"]
        c.find(f"new term being hypothesis-filtered through {through}", p, window=2)
        if "top_down_glom" in m:
            c.find(f"top-down glom {m['top_down_glom']}", p, exact=True)
    elif k == "refutation":
        c.find(m["line"], p, exact=True)
    elif k == "gnoth-setter":
        if run.tasks():
            c.find("doing task Gnoth-setter", p, exact=True)
        gloms = m["top_down_gloms"]
        c.find(f"top-down glom {gloms[0]}", p, exact=True)
        for line in m.get("extra_lines", []):   # p. 168 prints them after the first glom
            c.find(line, p, exact=True)
        for g in gloms[1:]:
            c.find(f"top-down glom {g}", p, exact=True)
        c.find(f"gnoths: {sexp(m['gnoths'])}", p, exact=True)
    elif k == "slip-check":
        best = "nil" if m["best"] is None else f"({m['best'][0]} {m['best'][1]:.1f})"
        # "best:" is printed indented on the next line, so it joins the stayval line
        c.find(f"slip-check: stayval: {m['stayval']:.1f} best: {best}", p, exact=True)
    elif k == "reigning-class-change":
        assert not m["printed_by_program"]
        joined = " ".join(" ".join(t.split()) for pg, t in run.notes if pg == p)
        assert m["note_by_meredith"] in joined, "the change is only in Meredith's annotation"
        c.find(f"doing task {m['after_task']}", p, exact=True)
    elif k == "bond-assessor":
        c.find(f"doing task Bond-assessor on ({m['reigning_class']} {m['threshold']:.1f})", p, exact=True)
    elif k == "gnoth-operation":
        i = c.find("doing task Gnoth-operator", p)
        if m["program"] not in run.logical[i][1]:   # p. 177 prints "on" and the program as a continuation
            c.expect_next(m["program"], p)
        for op in m["operations"]:
            args = " ".join(sexp(a) if isinstance(a, list) else a for a in op["args"])
            assert f"({op['op']} {args})" in m["program"]
        c.find(f"top-down glom {m['top_down_glom']}", p, exact=True)
    elif k == "casts":
        # the generalised form follows the casts, on the same logical line (run 2) or the next
        c.find(f"casts: {sexp(m['casts'])} {m['generalised']}", p, window=2)
    elif k == "new-hypothesis-candidate":
        c.find(f"new hypoth candidate {m['form']}", p, exact=True)
    elif k == "guess":
        c.find("I have a guess!", p, exact=True)
        i = c.find(f"hypothesis: {m['hypothesis']}", p)
        if run.logical[i][1] == f"hypothesis: {m['hypothesis']}":
            c.expect_next(m["predicted_as_printed"], p)
        else:   # run 1 indents the predicted terms, so they join the hypothesis line
            assert run.logical[i][1] == f"hypothesis: {m['hypothesis']} {m['predicted_as_printed']}"
        printed = [int(t) for t in re.findall(r"\d+", m["predicted_as_printed"])]
        assert printed == m["predicted_terms"]
        c.find(f"enter no if wrong, ok if right {m['answer']}", p, exact=True)
    elif k == "bye":
        c.find("bye", p, exact=True)
    else:
        raise AssertionError(f"unknown milestone kind {k}")


def test_milestones_in_order_on_their_pages(runs, expectations):
    for spec in expectations["runs"]:
        run = runs[spec["file"]]
        c = Cursor(run)
        for m in spec["milestones"]:
            check_milestone(c, run, m)


def test_every_guess_follows_one_confirming_term(expectations):
    """In all four guesses the program guesses after one term that confirms the hypothesis
    [M86 pp. 166, 169, 175, 179]."""
    guesses = 0
    for spec in expectations["runs"]:
        ms = spec["milestones"]
        for i, m in enumerate(ms):
            if m["kind"] == "guess":
                guesses += 1
                assert m["after_confirming_terms"] == expectations["guess_rule_observed"]["after_confirming_terms"]
                assert ms[i - 1]["kind"] == "hfilter" and ms[i - 1]["result"] == "confirmed"
                assert ms[i - 2]["kind"] == "term"
                # the printed block is at least three hits and covers every term entered so far
                entered = spec["sequence"][:ms[i - 2]["index"]]
                assert m["predicted_terms"][:len(entered)] == entered
                if m["answer"] == "ok":
                    assert m["predicted_terms"] == spec["sequence"][:len(m["predicted_terms"])]
                else:
                    assert m["predicted_terms"] != spec["sequence"][:len(m["predicted_terms"])]
    assert guesses == 4
    assert sorted(expectations["guess_rule_observed"]["evidence_pages"]) == [166, 169, 175, 179]


def test_hypotheses_and_answers_per_run(expectations):
    forms = {spec["id"]: [m["form"] for m in spec["milestones"] if m["kind"] == "hypothesis"]
             for spec in expectations["runs"]}
    assert forms["run1_3737"] == ["(Cycle 3 2 (3 7))"]
    assert forms["run2_373373"] == ["(Cycle 3 2 (3 7))", "(Cycle 3 3 (3 7 3))"]
    assert forms["run3_121231234"] == ["(Countup 1)", "(S-group 1 2)", "(S-group 1 (Countup 2))"]
    answers = {spec["id"]: [m["answer"] for m in spec["milestones"] if m["kind"] == "guess"]
               for spec in expectations["runs"]}
    assert answers == {"run1_3737": ["ok"], "run2_373373": ["ok"], "run3_121231234": ["nope", "ok"]}
    run3 = expectations["runs"][2]
    thresholds = [m["threshold"] for m in run3["milestones"] if m["kind"] == "bond-assessor"]
    assert thresholds == sum(run3["bond_assessor_thresholds_per_reformulation"], [])
    assert [m["ordinal"] for spec in expectations["runs"] for m in spec["milestones"]
            if m["kind"] == "hypothesis"] == [1, 1, 2, 1, 2, 3]


# --- the Blackburn dozen and the results -----------------------------------------------------

def test_blackburn_dozen(expectations):
    dozen = expectations["blackburn_dozen"]
    assert [b["number"] for b in dozen] == list(range(1, 13))
    for b in dozen:
        unparsed = [int(t) for t in re.findall(r"\d+", b["parsed"])]
        assert len(unparsed) >= 6 and unparsed == b["sequence"][:len(unparsed)], b["number"]
        assert b["result"] in ("solved", "unsolved", "not attempted")
        assert all(145 <= p <= 147 for p in b["pages"])
        for rel in b.get("related_solved", []):
            assert rel != b["sequence"]
    summary = expectations["blackburn_summary"]
    assert [b["number"] for b in dozen if b["result"] == "solved"] == summary["solved"] == [2, 8, 12]
    others = (summary["with_slight_extension"] + summary["needs_less_clinging"]
              + summary["needs_interference_handling"] + summary["needs_group_lengths_as_manifestations"])
    assert sorted(summary["solved"] + others) == list(range(1, 13)), "p. 147 accounts for all twelve"
    groups = expectations["human_difficulty_p148"]
    flat = sum(groups["easiest_to_hardest_groups"], []) + [groups["hardest"], groups["also_time_consuming"]]
    assert sorted(flat) == list(range(1, 13))


def test_solved_sequences_include_the_three_runs(expectations):
    solved = expectations["solved_sequences_p88"]
    assert len(solved) == 9
    by_run = {s["run"]: s["sequence"] for s in solved if "run" in s}
    for spec in expectations["runs"]:
        assert spec["id"] in by_run
        assert spec["sequence"] == by_run[spec["id"]][:len(spec["sequence"])]
    sequences = [s["sequence"] for s in solved]
    assert [1, 2, 3, 4] in sequences, "Blackburn 2 is on the p. 88 list"
    for s in solved:
        assert [int(t) for t in re.findall(r"\d+", s["as_printed"])] == s["sequence"]
    for u in expectations["unconquered_p150"]:
        assert [int(t) for t in re.findall(r"\d+", u["as_printed"])] == u["sequence"]
        assert u["sequence"] not in sequences
    assert len(expectations["stated_problems_pp148_153"]) == 10


def test_related_solved_sequences_are_not_blackburn_sequences(expectations):
    """The sequences the thesis says the program *can* solve next to a failed Blackburn problem
    (`2 2 3 3 3 4 4 4 4`, `1 1 2 2 2 3 3 3 3`, `1 8 5 8 1 1 8 5 8 1`, `1 2 1 2 3 1 2 3 4`) are
    distinct from the dozen [M86 pp. 145-146]."""
    dozen = [b["sequence"] for b in expectations["blackburn_dozen"]]
    related = [r for b in expectations["blackburn_dozen"] for r in b.get("related_solved", [])]
    assert len(related) == 4
    for r in related:
        assert all(d[:len(r)] != r for d in dozen)
