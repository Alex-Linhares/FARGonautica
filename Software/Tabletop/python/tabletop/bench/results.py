"""Build the tables of RESULTS.md from the benches' JSON records (D-075).

    python3 -m tabletop.bench.results strip bench-results results   # the records, minus every run
    python3 -m tabletop.bench.results tables results                # print every generated section
    python3 -m tabletop.bench.results update results RESULTS.md     # refresh the marked sections

The benches (`python3 -m tabletop.bench NAME --json FILE`, D-072, D-073) write one JSON record
per comparison with every run's (seed, answer, structure, codelets, capped). `strip` copies
those records into `results/NAME.json` without the per-run lists (which would be megabytes),
adding each survey's median and quartiles of codelets in their place, keeps of each record's
`engine` (the `accel.info()` of the process that ran it, D-107) the engine's name alone, and
merges a bench run in parts (`lesions-PROBLEM.json`) into one
file. RESULTS.md's tables are generated from the
stripped records only, between `<!-- begin NAME -->` and `<!-- end NAME -->` markers, so that
a test can regenerate every block and compare (`tests/test_results.py`); the prose around the
blocks is written by hand.

The measures M3-M7 and M9 (the validation protocol, RESULTS.md §1) are computed here from the
same records: M3 the directions within a family, M4 the main answer's frequency within 15
points, M5 the better-structured answer and whether it takes longer, M6 the run length, M7
the lesions against the bench's expectations [FCCA p. 365], M9 the Potelbat's failures
[FCCA pp. 355-356]. French's numbers come from `tabletop.reported`.

`results/v1/` holds the same records as they stood before roles were built into the engine
(D-078): `Results` reads it as its `v1` column source (`res.v1`, `v1_row`), so that a later
table can set French | v1 | roles side by side. The blocks of RESULTS.md §1-§10 read
`results/NAME.json` alone and render as before; the `variants` block (D-102) is the first
three-column one, v1 beside the roles.

`results/v2/` holds the records of the roles engine on its a-priori defaults, as they
stood before v3's optimized defaults (D-114, 2026-10-16), byte for byte; `Results` reads it
as `v2`, and the `v3` block alone sets French | v1 | v2 | v3 side by side, with the
optimization's objective J = (2F + H) / 3 worked out from the families records of each
(`objective`).

`results/v3/` holds the records of v3's defaults as they stood before v4's (D-115,
2026-10-17), byte for byte; `Results` reads it as `v3`. Since then the `v3` block reads its v3
column from there (so it reads as it did), and the `v4` block sets French | v1 | v2 | v3 | v4
side by side. `results/v4/` holds v4's records as they stood before the structural Blockage
fix (v5, D-116 to D-120, 2026-10-17); `Results` reads it as `v4`, the `v4` block takes its v4
column from there, and the `v5` block sets French | v1 | v2 | v3 | v4 | v5 side by side.
Every other block reads `results/NAME.json`, v5's since that day.
"""

import json
import math
import sys
from pathlib import Path

from tabletop import objects as OB
from tabletop import reported as RP
from tabletop.bench import churn as CH
from tabletop.bench import lesions as LS
from tabletop.bench import probes as PB
from tabletop.bench import stats as ST
from tabletop.problems import catalogue, load_problem

BENCHES = ("families", "families-french-faults", "fcca", "fcca-french-faults", "humans", "ch2",
           "ch4", "focus", "rho", "lesions", "faults", "potelbat", "stated",
           "families-human", "families-buridan", "calibrate-human", "calibrate-buridan",
           "temperature", "robustness", "variants", "churn")
# Benches first recorded on v1 (`results/v1/NAME.json`, from the baseline commit through
# `tools/run_on_baseline.py`) whose roles run is not recorded yet: `results/NAME.json` is
# absent until the roles are measured on them (D-102). Empty since the roles loop's item 12
# recorded every bench at full size.
V1_FIRST = ()
# Benches that read the role pool and so could not run on v1, which had none: no
# `results/v1/NAME.json` exists for them (D-106).
ROLES_ONLY = ("churn",)
# Blocks that set the v1 column beside the roles' and so render differently with and
# without the v1 source (D-078, D-102): French | v1 | roles, the roles loop's item 12.
V1_BLOCKS = ("families-summary", "families-tables", "fcca", "humans", "ch2", "ch4", "focus",
             "stated", "rho", "lesions", "faults", "temperature", "potelbat", "robustness",
             "variants", "roles-families", "v3", "v4", "v5")
CALIBRATED = (("human", "type_weight"), ("buridan", "incoherence_penalty"))   # D-076
FRENCH_RUNS = ST.FRENCH_RUNS
WITHIN_POINTS = 15      # M4: the main answer's frequency within 15 points of French's
SIGMAS = 2.0            # M3: a difference beyond 2 standard errors
REACH_WIDTHS = 2.5      # D-022: beyond 2.5 glass widths two objects are never linked
FAMILY_TITLES = (("surround", "Surround"), ("blockage", "Blockage"), ("buridan", "Buridan"),
                 ("figure-ground", "Figure/Ground"))
FAULTS = {1: "D-026", 2: "D-045", 3: "D-046", 4: "D-021", 5: "D-070", 6: "D-069", 7: "D-028"}


# ---- the records -----------------------------------------------------------------------

def read_records(path: Path) -> list:
    """The JSON-lines records of one file (D-075)."""
    with open(path, encoding="utf-8") as handle:
        return [json.loads(line) for line in handle if line.strip()]


def write_records(records: list, path: Path) -> None:
    """One compact record per line (D-075)."""
    with open(path, "w", encoding="utf-8") as handle:
        for r in records:
            handle.write(json.dumps(r, separators=(",", ":")) + "\n")


def quartiles(values: list) -> tuple:
    """The lower quartile, median and upper quartile (nearest rank) of a list (D-075)."""
    xs = sorted(values)
    n = len(xs)
    if n == 0:
        return (None, None, None)

    def rank(q: float):
        return xs[min(n - 1, max(0, math.ceil(q * n) - 1))]
    return (rank(0.25), rank(0.5), rank(0.75))


def strip_record(value):
    """A record without its per-run lists: every `results` list of runs becomes the count of
    runs, the median and quartiles of their codelets and the number of capped runs; a `paths`
    list (the ch4 bench) becomes its length (D-075)."""
    if isinstance(value, dict):
        out = {}
        for k, v in value.items():
            if k == "results" and isinstance(v, list) and all(isinstance(r, list) for r in v):
                codelets = [r[3] for r in v]
                low, median, high = quartiles(codelets)
                out["results_count"] = len(v)
                out["median_codelets"] = median
                out["codelets_quartiles"] = [low, high]
            elif k == "paths" and isinstance(v, list):
                out["paths_count"] = len(v)
            elif k == "engine" and isinstance(v, dict) and "engine" in v:
                # the engine's name alone: the whole `accel.info()` (the build stamp, the
                # compiler, the flags) is in the bench's own JSON and in RESULTS.md's "How
                # these numbers were made"; repeated on the variants bench's 477 records it
                # would put `results/variants.json` over the folder's 300 KB file limit (D-107)
                out[k] = {"engine": v["engine"]}
            else:
                out[k] = strip_record(v)
        return out
    if isinstance(value, list):
        return [strip_record(v) for v in value]
    return value


def strip(src: Path, dst: Path) -> list:
    """Copy every bench's records from `src` to `dst` without the runs; `NAME-*.json` parts
    are merged into `NAME.json` in the parts' sorted order (a part never belongs to another
    bench's name, so `families-human-X.json` is a part of `families-human`, not of
    `families`). Returns the files written (D-075)."""
    dst.mkdir(parents=True, exist_ok=True)
    written = []
    for name in BENCHES:
        whole = src / f"{name}.json"
        parts = sorted(p for p in src.glob(f"{name}-*.json")
                       if not any(p.stem == other or p.stem.startswith(other + "-")
                                  for other in BENCHES if other != name))
        if whole.exists():
            records = read_records(whole)
        elif parts:
            records = [r for p in parts for r in read_records(p)]
        else:
            continue
        write_records([strip_record(r) for r in records], dst / f"{name}.json")
        written.append(dst / f"{name}.json")
    return written


V1_DIR = "v1"       # the records before roles, under results/ (D-078)
V2_DIR = "v2"       # the records of the roles engine on its a-priori defaults (D-114)
V3_DIR = "v3"       # the records of v3's optimized defaults, before v4's (D-115)
V4_DIR = "v4"       # the records of v4's defaults, before the structural Blockage fix (v5, D-116 to D-120)


class Results:
    """The stripped records of every bench found in a directory (D-075), and, as `v1`,
    those of its `v1/` subfolder when there is one: the reconstruction's numbers before
    roles, the historical baseline of RESULTS.md's three-column tables (D-078); and, as
    `v2`, those of its `v2/` subfolder, the roles engine on its a-priori defaults before
    v3's optimized ones (D-114); as `v3`, those of its `v3/` subfolder, v3's defaults
    before v4's (D-115); and, as `v4`, those of its `v4/` subfolder, v4's defaults before
    v5's structural Blockage fix (D-116 to D-120). Each is None when its subfolder is absent
    or when `with_v1` is False (a subfolder's own `Results` never looks further down)."""

    def __init__(self, directory: Path, with_v1: bool = True):
        self.directory = Path(directory)
        self.records = {}
        for name in BENCHES:
            path = self.directory / f"{name}.json"
            if path.exists():
                self.records[name] = read_records(path)
        v1 = self.directory / V1_DIR
        self.v1 = Results(v1, with_v1=False) if with_v1 and v1.is_dir() else None
        # The roles engine on its a-priori defaults, before v3's (D-114), read by the v3
        # block alone; None where the subfolder is absent or `with_v1` is False.
        v2 = self.directory / V2_DIR
        self.v2 = Results(v2, with_v1=False) if with_v1 and v2.is_dir() else None
        # v3's optimized defaults, before v4's (D-115), read by the v3 and v4 blocks alone.
        v3 = self.directory / V3_DIR
        self.v3 = Results(v3, with_v1=False) if with_v1 and v3.is_dir() else None
        # v4's defaults, before the structural Blockage fix of v5 (D-116 to D-120), read by the
        # v4 and v5 blocks alone.
        v4 = self.directory / V4_DIR
        self.v4 = Results(v4, with_v1=False) if with_v1 and v4.is_dir() else None

    def __getitem__(self, name: str) -> list:
        return self.records.get(name, [])

    def comparison(self, bench: str, problem: str, program: str = None) -> dict | None:
        """The families or fcca record of a problem (and program, when given) (D-075)."""
        for r in self[bench]:
            if r["problem"] == problem and (program is None or r["program"] == program):
                return r
        return None


def v1_row(res: Results, bench: str, problem: str, label: str,
           program: str = None) -> dict | None:
    """The v1 row of a result set (the `label` row of the `v1/` record of `problem` in
    `bench`), or None when there is no v1 source or no such row: the "v1" column of a
    French | v1 | roles table (D-078). The row has the same shape as the current records'
    rows (`french`, `ours`, `z`), so a cell is read as `v1_row(...)["ours"]["frequency"]`."""
    if res.v1 is None:
        return None
    record = res.v1.comparison(bench, problem, program)
    return row_named(record, label) if record else None


# ---- helpers ---------------------------------------------------------------------------

def num(value, digits: int = 0) -> str:
    """A number for a cell; a dash where there is none [F95 p. 119]."""
    if value is None:
        return "-"
    return f"{value:.{digits}f}"


def pct(value) -> str:
    """A percentage with one decimal, or a dash (D-075)."""
    return "-" if value is None else f"{value:.1f}"


def main_row(record: dict) -> dict:
    """French's most frequent answer of a comparison record (the first among ties) (D-075)."""
    rows = [r for r in record["rows"] if r["french"]["frequency"] is not None]
    return max(rows, key=lambda r: r["french"]["frequency"])


def row_named(record: dict, label: str) -> dict | None:
    """The row of a comparison record with French's label (D-075)."""
    for r in record["rows"]:
        if r["label"] == label:
            return r
    return None


def family_of(problem: str) -> str:
    """The family of a problem name, its stem before the number (D-075)."""
    return problem.rsplit("-", 1)[0]


def set_key(record: dict) -> str:
    """A result set's name: the problem, with the program when it is not the original (D-075)."""
    if record["program"] != "original":
        return f"{record['problem']} ({record['program']})"
    return record["problem"]


def standard_error(p1: float, n1: float, p2: float, n2: float) -> float:
    """The standard error of the difference of two percentages (D-075)."""
    q1, q2 = p1 / 100, p2 / 100
    return 100 * math.sqrt(q1 * (1 - q1) / n1 + q2 * (1 - q2) / n2)


def z_against_v1(roles: float | None, n_roles: int, v1: float | None, n_v1: int) -> float | None:
    """The two-sample z of the roles' frequency against v1's on the same problem (D-078):
    None where either is missing, 0 where both are 0 or both 100 (no error to divide by)."""
    if roles is None or v1 is None:
        return None
    se = standard_error(roles, n_roles, v1, n_v1)
    return (roles - v1) / se if se > 0 else 0.0


def v1_record(res: Results, bench: str, **keys) -> dict | None:
    """The first `v1/` record of a bench whose fields equal `keys` (a nested field as
    `survey__problem`), or None without a v1 source (D-078)."""
    if res.v1 is None:
        return None

    def field(r: dict, path: str):
        for part in path.split("__"):
            r = r.get(part) if isinstance(r, dict) else None
        return r
    return next((r for r in res.v1[bench] if all(field(r, k) == v for k, v in keys.items())),
                None)


def v1_top(res: Results, bench: str, **keys) -> str:
    """A v1 survey record's main answer and share as two cells, `- | -` without one (D-078)."""
    r = v1_record(res, bench, **keys)
    if r is None:
        return "- | -"
    top = max(r["survey"]["tallies"], key=lambda t: t["count"])
    who = f"{top['answer']} ({top['label']})" if top["label"] else top["answer"]
    return f"{who} | {top['frequency']:.1f}"


def v1_pct(row: dict | None) -> str:
    """A v1 row's frequency cell (D-078)."""
    return pct(row["ours"]["frequency"]) if row else "-"


# ---- the fault marks (SPEC.md §6b: the result sets each fix may move) -----------------

def components_by_type(problem) -> dict:
    """Connected runs of objects of one type, two objects joined when their edge gap is
    within the proximity reach (D-022)."""
    out = {}
    for t in sorted({o.type for o in problem.objects}):
        members = [o for o in problem.objects if o.type == t]
        comps = []
        for o in members:
            joined = [c for c in comps if any(
                OB.gap_in_glass_widths(OB.edge_gap(o, m)) <= REACH_WIDTHS for m in c)]
            merged = [o] + [m for c in joined for m in c]
            comps = [c for c in comps if c not in joined] + [merged]
        out[t] = comps
    return out


def is_isolated(o, problem) -> bool:
    """Whether no other object lies within the proximity reach of this one (D-022, D-075)."""
    return all(OB.gap_in_glass_widths(OB.edge_gap(o, m)) > REACH_WIDTHS
               for m in problem.objects if m is not o)


def fault_marks(problem_name: str) -> list:
    """Which of the seven fixes may move a result set, after SPEC.md §6b: fault 1 where
    three or more identical objects are adjacent [F95 p. 140]; fault 2 for Blockage 3 and 4
    and every problem with an isolated object across from a group [F95 p. 125]; faults 3,
    4 and 5 for the Surround family, whose structure columns count correspondences inside
    mapped groups and whose rows enter subworlds [F95 pp. 89, 109]; fault 6 for Figure/Ground
    1 [F95 pp. 141-142]; fault 7 none [F95 p. 63]."""
    pb = load_problem(problem_name)
    marks = []
    if any(len(c) >= 3 for comps in components_by_type(pb).values() for c in comps):
        marks.append(1)
    touched = pb.touched_object()
    other_half = [o for o in pb.objects if (o.y > 50) != (touched.y > 50)]
    isolated = [o for o in other_half if is_isolated(o, pb)]
    grouped = [o for o in pb.objects if not is_isolated(o, pb)]
    if problem_name in ("blockage-03", "blockage-04") or (isolated and grouped):
        marks.append(2)
    if family_of(problem_name) == "surround":
        marks += [3, 4, 5]
    if problem_name == "figure-ground-01":
        marks.append(6)
    return marks


# ---- the 35 result sets (families) -----------------------------------------------------

def families_summary(res: Results) -> list:
    """One line per result set: French's main answer, his and our frequency, the z, the
    chi-square p, the same frequency under `french-faults`, and the fixes that may move
    the set (D-075)."""
    if not res["families"]:
        return ["(no families records)"]
    lines = ["| Result set | Page | French's main answer | French % | v1 % | `standard` % | z | "
             "z against v1 | chi-square p | `french-faults` % | fixes that may move it |",
             "|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---|"]
    for r in res["families"]:
        main = main_row(r)
        ff = res.comparison("families-french-faults", r["problem"], r["program"])
        ff_row = row_named(ff, main["label"]) if ff else None
        v1 = v1_row(res, "families", r["problem"], main["label"], r["program"])
        zv = z_against_v1(main["ours"]["frequency"], r["runs"],
                          v1["ours"]["frequency"] if v1 else None, v1_runs(res, "families", r))
        marks = fault_marks(r["problem"])
        lines.append(
            f"| {r['title']} "
            f"| {r['page']} | {main['label']} | {num(main['french']['frequency'])} "
            f"| {v1_pct(v1)} | {pct(main['ours']['frequency'])} | {main['z']:+.2f} "
            f"| {signed(zv)} | {r['p']:.3f} "
            f"| {pct(ff_row['ours']['frequency']) if ff_row else '-'} "
            f"| {', '.join(str(m) for m in marks) or 'none'} |")
    return lines


def v1_runs(res: Results, bench: str, record: dict) -> int:
    """The runs of the v1 record of a families or fcca record, or 0 (D-078)."""
    v1 = res.v1.comparison(bench, record["problem"], record["program"]) if res.v1 else None
    return v1["runs"] if v1 else 0


def signed(value: float | None, digits: int = 2) -> str:
    """A signed number, or a dash (D-078)."""
    return "-" if value is None else f"{value:+.{digits}f}"


def set_table(r: dict, ff: dict | None, v1r: dict | None = None) -> list:
    """One result set in French's format [F95 p. 115]: his three headings with the v1 and
    our column beside each (D-078), the z of every frequency against his and against v1,
    the `french-faults` frequency, and a line with the runs, the seeds, the capped runs, the
    median codelets and the chi-square."""
    marks = fault_marks(r["problem"])
    moved = (f" Fixes that may move this set: {', '.join(str(m) + ' (' + FAULTS[m] + ')' for m in marks)}."
             if marks else " No fix is expected to move this set.")
    lines = [f"#### {r['title']} [{r['source']} p. {r['page']}]", "",
             "| Object Eliza touches | Frequency, French | v1 | ours | z | z against v1 | "
             "`french-faults` | Average structure, French | ours | No. of codelets run, "
             "French | ours |",
             "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for row in r["rows"]:
        who = row["label"]
        if who == "other":
            who = "other (" + " ".join(f"{a} {n}" for a, n in row["ours"]["answers"]) + ")"
        ff_row = row_named(ff, row["label"]) if ff else None
        v1 = row_named(v1r, row["label"]) if v1r else None
        zv = z_against_v1(row["ours"]["frequency"], r["runs"],
                          v1["ours"]["frequency"] if v1 else None, v1r["runs"] if v1r else 0)
        lines.append(
            f"| {who} | {num(row['french']['frequency'])} | {v1_pct(v1)} "
            f"| {row['ours']['frequency']:.1f} | {row['z']:+.2f} | {signed(zv)} "
            f"| {pct(ff_row['ours']['frequency']) if ff_row else '-'} "
            f"| {num(row['french']['structure'])} | {num(row['ours']['structure'], 1)} "
            f"| {num(row['french']['codelets'])} | {num(row['ours']['codelets'], 1)} |")
    lines += ["", f"{r['runs']} runs under `{r['profile']}` (bench seed {r['seed']}, batch seed "
                  f"{r['batch_seed']}), {r['capped']} capped, mean codelets "
                  f"{r['mean_codelets']:.1f}, median {num(r['median_codelets'])}; chi-square "
                  f"{r['chi_square']:.2f} on {r['df']} df, p = {r['p']:.3f}; French's n = "
                  f"{FRENCH_RUNS}.{moved}", ""]
    return lines


def families_tables(res: Results) -> list:
    """Every result set, family by family (D-075)."""
    if not res["families"]:
        return ["(no families records)"]
    lines = []
    for family, title in FAMILY_TITLES:
        records = [r for r in res["families"] if family_of(r["problem"]) == family]
        if not records:
            continue
        lines += [f"### {title}", ""]
        for r in records:
            ff = res.comparison("families-french-faults", r["problem"], r["program"])
            v1r = res.v1.comparison("families", r["problem"], r["program"]) if res.v1 else None
            lines += set_table(r, ff, v1r)
    return lines


# ---- the measures ----------------------------------------------------------------------

KINDS = (("touched cup itself", "the touched cup"), ("small glass", "a small glass"),
         ("glasses", "a small glass"), ("glass", "a small glass"), ("cup", "a cup"),
         ("spoon", "a spoon"), ("fork", "a fork"), ("saucer", "a saucer"), ("plate", "a plate"),
         ("salt shaker", "a salt shaker"), ("knife", "a knife"))


def answer_kind(names: str) -> str:
    """The kind of object an answer's description names, the first kind word in it
    [F95 pp. 115-139], so that rows of different sets can be matched: French's labels
    change meaning across a family (E1 is a glass in Blockage 1-2 and E1+E2 the glasses
    after; Surround 7-9 replace the glass) and his descriptions differ in detail (D-075)."""
    found = [(names.find(word), kind) for word, kind in KINDS if word in names]
    return min(found)[1] if found else names


def direction_pairs(res: Results) -> list:
    """M3: within each family, every ordered pair of result sets and kind of answer
    (`answer_kind`) where French's two frequencies differ by more than two standard errors
    (n = 50 each);
    for each, whether ours go the same way, and whether ours differ significantly the
    other way (n = 500 each). Buridan uses the modified-program sets (Buridan 1 modified
    and 2-13), the program French ran the family with [F95 p. 130]."""
    out = []
    for family, _ in FAMILY_TITLES:
        records = [r for r in res["families"] if family_of(r["problem"]) == family]
        if family == "buridan":
            records = [r for r in records if r["program"] == "modified"]
        for i, a in enumerate(records):
            for b in records[i + 1:]:
                for ra in a["rows"]:
                    if ra["label"] == "other":
                        continue
                    rb = next((x for x in b["rows"] if x["label"] != "other"
                               and answer_kind(x["names"]) == answer_kind(ra["names"])
                               and x["french"]["frequency"] is not None), None)
                    if rb is None or ra["french"]["frequency"] is None:
                        continue
                    fa, fb = ra["french"]["frequency"], rb["french"]["frequency"]
                    se = standard_error(fa, FRENCH_RUNS, fb, FRENCH_RUNS)
                    if se == 0 or abs(fa - fb) <= SIGMAS * se:
                        continue
                    oa, ob = ra["ours"]["frequency"], rb["ours"]["frequency"]
                    ose = standard_error(oa, a["runs"], ob, b["runs"])
                    same = (oa - ob) * (fa - fb) > 0
                    reversed_sig = (not same) and ose > 0 and abs(oa - ob) > SIGMAS * ose
                    out.append({"family": family, "a": set_key(a), "b": set_key(b),
                                "names": answer_kind(ra["names"]), "french": (fa, fb),
                                "ours": (oa, ob),
                                "same": same, "reversed": reversed_sig})
    return out


def m3_lines(res: Results) -> list:
    """M3, the directions within the families, as Markdown (D-075)."""
    pairs = direction_pairs(res)
    right = sum(p["same"] for p in pairs)
    reversed_ = sum(p["reversed"] for p in pairs)
    lines = [f"**M3 Directions.** {len(pairs)} ordered pairs within the families where "
             f"French's frequencies differ by more than {SIGMAS:.0f} standard errors (n = "
             f"{FRENCH_RUNS} each); ours go the same way in {right} "
             f"({100 * right / len(pairs):.0f} percent) and are significantly reversed in "
             f"{reversed_}. The target is 85 percent right and none reversed.", "",
             "| Family | Pairs | Same direction | Significantly reversed |",
             "|---|---:|---:|---:|"]
    for family, title in FAMILY_TITLES:
        ps = [p for p in pairs if p["family"] == family]
        if ps:
            lines.append(f"| {title} | {len(ps)} | {sum(p['same'] for p in ps)} "
                         f"| {sum(p['reversed'] for p in ps)} |")
    wrong = [p for p in pairs if not p["same"]]
    if wrong:
        lines += ["", "The pairs that go the other way (French's two frequencies, then ours):",
                  ""]
        for p in wrong:
            lines.append(f"- {p['a']} against {p['b']}, {p['names']}: French "
                         f"{p['french'][0]:.0f} against {p['french'][1]:.0f}, ours "
                         f"{p['ours'][0]:.1f} against {p['ours'][1]:.1f}"
                         f"{' (significantly reversed)' if p['reversed'] else ''}")
    return lines


def m4_lines(res: Results) -> list:
    """M4, the main answer's frequency within 15 points of French's (D-075)."""
    records = res["families"]
    hits = []
    for r in records:
        main = main_row(r)
        gap = abs(main["ours"]["frequency"] - main["french"]["frequency"])
        hits.append((set_key(r), gap))
    within = [k for k, g in hits if g <= WITHIN_POINTS]
    lines = [f"**M4 Magnitudes.** French's main answer is within {WITHIN_POINTS} points of his "
             f"frequency on {len(within)} of the {len(records)} result sets "
             f"({100 * len(within) / len(records):.0f} percent; the target is 60 percent): "
             + (", ".join(within) if within else "none") + ".", ""]
    far = sorted(((g, k) for k, g in hits if g > WITHIN_POINTS), reverse=True)
    lines.append("The largest gaps: " + ", ".join(f"{k} ({g:.0f} points)" for g, k in far[:8])
                 + ".")
    return lines


def m5_lines(res: Results) -> list:
    """M5: on French's two-answer sets, whether our better-structured answer is his, and
    whether the better-structured answer takes longer as it does in his (D-075)."""
    agree_structure, agree_longer, judged, rows = 0, 0, 0, []
    longer_sets = {(rs.problem, rs.program) for rs in RP.better_structure_takes_longer()}
    for rs in RP.two_answer_sets():
        r = res.comparison("families", rs.problem, rs.program)
        if r is None:
            continue
        ours = {row["label"]: row["ours"] for row in r["rows"]}
        best = RP.best_structured(rs)
        other = [a for a in rs.answers if a is not best][0]
        ob, oo = ours.get(best.label), ours.get(other.label)
        if not ob or not oo or ob["structure"] is None or oo["structure"] is None:
            rows.append((set_key(r), best.label, "-", "-"))
            continue
        judged += 1
        same_best = ob["structure"] > oo["structure"]
        his_longer = (rs.problem, rs.program) in longer_sets
        our_longer = ob["codelets"] > oo["codelets"]
        agree_structure += same_best
        agree_longer += (his_longer == our_longer)
        rows.append((set_key(r), best.label, "yes" if same_best else "no",
                     "yes" if his_longer == our_longer else "no"))
    lines = [f"**M5 Structure and time.** Of French's {len(RP.two_answer_sets())} two-answer "
             f"sets, {judged} have both answers in our runs. Our better-structured answer is "
             f"his on {agree_structure} of {judged} ({100 * agree_structure / max(1, judged):.0f} "
             f"percent; the target is 80); whether the better-structured answer takes longer "
             f"agrees with his on {agree_longer} of {judged} "
             f"({100 * agree_longer / max(1, judged):.0f} percent; the target is 70; his own "
             f"rate is {len(RP.better_structure_takes_longer())} of {len(RP.two_answer_sets())}).",
             "", "| Result set | French's better-structured answer | ours the same | "
             "takes longer agrees |", "|---|---|---|---|"]
    lines += [f"| {k} | {b} | {s} | {l} |" for k, b, s, l in rows]
    return lines


def m6_lines(res: Results) -> list:
    """M6, the run length against French's 50-200 codelets [F95 pp. 115-139] (D-075)."""
    lines = ["**M6 Run length.** French's runs took 50-200 codelets; the target is a median "
             "within a factor of 2.5 of that range on every family, and Surround 10's mean "
             "run length at most 1.3 times Surround 5's.", "",
             "| Family | Median of the sets' median codelets | Range of the medians | "
             "French's means, range | Within a factor of 2.5 of 50-200 |", "|---|---:|---|---|---|"]
    for family, title in FAMILY_TITLES:
        records = [r for r in res["families"] if family_of(r["problem"]) == family]
        if not records:
            continue
        medians = [r["median_codelets"] for r in records]
        his = [a.codelets for r in records for a in RP.result_sets(r["problem"])[0].answers
               if a.codelets is not None]
        _, med, _ = quartiles(medians)
        ok = 50 / 2.5 <= med <= 200 * 2.5
        lines.append(f"| {title} | {med:.0f} | {min(medians):.0f}-{max(medians):.0f} "
                     f"| {min(his):.0f}-{max(his):.0f} | {'yes' if ok else 'no'} |")
    s5 = res.comparison("families", "surround-05")
    s10 = res.comparison("families", "surround-10")
    if s5 and s10:
        ratio = s10["mean_codelets"] / s5["mean_codelets"]
        lines += ["", f"Surround 10's mean run length is {ratio:.2f} times Surround 5's "
                      f"({s10['mean_codelets']:.1f} against {s5['mean_codelets']:.1f} codelets; "
                      f"French: {RP.result_sets('surround-10')[0].answers[0].codelets} and "
                      f"{RP.result_sets('surround-05')[0].answers[0].codelets} for E1), "
                      f"{'within' if ratio <= 1.3 else 'above'} the 1.3 of the target."]
    return lines


def lesion_quantities(record: dict, problem) -> dict:
    """The four quantities M7 reads off a lesions record (D-073): the literal share, the
    share of French's main answer, the mean codelets and the capped runs."""
    sv = record["survey"]
    french = PB.french_main(problem.name)
    labels = french.label.split("+") if french is not None else []
    return {"literal": sum(t["frequency"] for t in sv["tallies"] if t["answer"] == problem.touched),
            "french_main": sum(t["frequency"] for t in sv["tallies"] if t["label"] in labels),
            "codelets": sv["mean_codelets"], "capped": sv["capped"]}


def m7_lines(res: Results) -> list:
    """M7, the lesions [FCCA p. 365]: each lesion's verdict on each problem by the bench's
    reading of its own expectation (`lesions.READINGS`, `lesions.verdict`), and the counts
    (D-073, D-075)."""
    records = res["lesions"]
    lines = ["**M7 Lesions.** Each lesion should degrade the behaviour in the direction its "
             "expectation names (the `lesions` bench's own expectations, FCCA's criterion "
             "[FCCA p. 365]); the verdict reads the literal share, French's main answer's "
             "share, the mean codelets and the capped runs against the intact profile "
             "(beyond two standard errors for a share, a tenth for the run length, five "
             "percent of the runs for the cap).", ""]
    if not records:
        return lines + ["(no lesions records)"]
    problems = []
    for r in records:
        if r["survey"]["problem"] not in problems:
            problems.append(r["survey"]["problem"])
    lines += ["| Lesion | Expected | " + " | ".join(problems) + " |",
              "|---|---|" + "---|" * len(problems)]
    counts = {"as predicted": 0, "no change": 0, "against": 0, "moved": 0}
    for name, _, _, expect in LS.LESIONS:
        cells = []
        for pname in problems:
            pb = load_problem(pname)
            intact = next(r for r in records if r["variant"] == "intact"
                          and r["survey"]["problem"] == pname)
            lesioned = next((r for r in records if r["variant"] == name
                             and r["survey"]["problem"] == pname), None)
            if lesioned is None:
                cells.append("-")
                continue
            runs = lesioned["survey"]["runs"]
            v = LS.verdict(name, lesion_quantities(intact, pb), lesion_quantities(lesioned, pb),
                           runs)
            counts["moved" if v.startswith("moved") else v] += 1
            cells.append(v)
        lines.append(f"| {name} | {expect} | " + " | ".join(cells) + " |")
    total = sum(counts.values())
    lines += ["", f"Of {total} lesion-problem cells, {counts['as predicted']} as predicted, "
                  f"{counts['no change']} no change, {counts['against']} against"
                  + (f", {counts['moved']} moved where no change was expected" if counts["moved"]
                     else "") + "."]
    return lines


def m9_lines(res: Results) -> list:
    """M9: the Potelbat fails where FCCA says a formula must, Blockage 2 and 6 and Surround
    4 and 5 [FCCA pp. 355-356], while Tabletop does not."""
    must_fail = ("blockage-02", "blockage-06", "surround-04", "surround-05")
    lines = ["**M9 The Potelbat.** FCCA argues that a formula over the objects must fail on "
             "Blockage 2 and 6 and Surround 4 and 5 [FCCA pp. 355-356]. The baseline's answer "
             "against French's and against our most frequent answer:", "",
             "| Problem | French's answer | Potelbat's | Potelbat agrees | Ours, most frequent | "
             "ours agrees |", "|---|---|---|---|---|---|"]
    for r in res["potelbat"]:
        if r["problem"] not in must_fail:
            continue
        top = max(r["survey"]["tallies"], key=lambda t: t["count"])
        lines.append(f"| {r['problem']} | {r['french']} ({r['french_label']}) "
                     f"| {r['potelbat']['answer']} | "
                     f"{'yes' if r['potelbat']['answer'] == r['french'] else 'no'} "
                     f"| {top['answer']} ({top['frequency']:.0f} percent) "
                     f"| {'yes' if top['answer'] == r['french'] else 'no'} |")
    agree_p = sum(r["potelbat"]["answer"] == r["french"] for r in res["potelbat"] if r["french"])
    agree_t = sum(max(r["survey"]["tallies"], key=lambda t: t["count"])["answer"] == r["french"]
                  for r in res["potelbat"] if r["french"])
    n = sum(1 for r in res["potelbat"] if r["french"])
    lines += ["", f"Over the {n} problems with an answer of French's (the most frequent label "
                  f"of chapter 5, or the chapter 2 figure's named answer), the Potelbat agrees "
                  f"on {agree_p} and our most frequent answer on {agree_t}."]
    return lines


def m8_lines(res: Results) -> list:
    """M8, the human data under the `human` profile beside `standard` (D-076): the rank
    correlation with the subjects' percentages and the share within ten points, against
    French's "over half" [F95 p. 144]."""
    records = res["humans"]
    if not records:
        return ["**M8 Humans.** (no humans records)"]
    parts = []
    for profile in ("standard", "human"):
        if profile not in records[0]["ours"]:
            continue
        h = human_half_summary(records, profile)
        parts.append(f"`{profile}`: {h['within_ten']} of {h['count']} within ten points, "
                     f"Spearman {corr(h['with_subjects'])} with the subjects and "
                     f"{corr(h['with_tabletop'])} with Tabletop's second runs")
    french = sum(r["french_fit"] <= 10 for r in records)
    return [f"**M8 Humans.** {'; '.join(parts)}; French's own second runs are within ten "
            f"points on {french} of {len(records)}, 'over half' [F95 p. 144]. The `human` "
            f"profile's fit is §9."]


def measures(res: Results) -> list:
    """The measures M3-M9, one after another (D-075, D-076)."""
    if not res["families"] or not res["potelbat"]:
        return ["(no families or potelbat records)"]
    out = []
    for f in (m3_lines, m4_lines, m5_lines, m6_lines, m7_lines, m8_lines, m9_lines):
        out += f(res) + [""]
    return out[:-1]


# ---- FCCA, humans, the probe benches ---------------------------------------------------

def fcca_table(res: Results) -> list:
    """FCCA's 18 reprinted sets [FCCA pp. 394-399]: its number, the book's, ours, the z
    and the `french-faults` share of the main answer (D-075)."""
    if not res["fcca"]:
        return ["(no fcca records)"]
    lines = ["| Figure | Problem | Page | French's main answer | FCCA % | book % | v1 % | "
             "`standard` % | z (FCCA) | z against v1 | chi-square p | `french-faults` % |",
             "|---|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for r in res["fcca"]:
        if r["source"] != "FCCA":
            continue
        main = main_row(r)
        book = RP.result_sets(r["problem"])[0]
        book_f = next((a.frequency for a in book.answers if a.label == main["label"]), None)
        ff = res.comparison("fcca-french-faults", r["problem"], r["program"])
        ff_row = row_named(ff, main["label"]) if ff and ff["source"] == "FCCA" else None
        v1 = v1_row(res, "fcca", r["problem"], main["label"], r["program"])
        zv = z_against_v1(main["ours"]["frequency"], r["runs"],
                          v1["ours"]["frequency"] if v1 else None, v1_runs(res, "fcca", r))
        lines.append(f"| {r['figure']} | {r['problem']} | {r['page']} | {main['label']} "
                     f"| {num(main['french']['frequency'])} | {num(book_f)} | {v1_pct(v1)} "
                     f"| {pct(main['ours']['frequency'])} | {main['z']:+.2f} | {signed(zv)} "
                     f"| {r['p']:.3f} "
                     f"| {pct(ff_row['ours']['frequency']) if ff_row else '-'} |")
    return lines


def humans_tables(res: Results) -> list:
    """Tables 5.1-5.3 [F95 pp. 143-144] with our two columns beside the subjects' and
    Tabletop's, Figure 5.4's fits, the within-ten counts and the rank correlations."""
    records = res["humans"]
    if not records:
        return ["(no humans records)"]
    profiles = list(records[0]["ours"].keys())
    lines = []
    for table in ("5.1", "5.2", "5.3"):
        rows = [r for r in records if r["table"] == table]
        if not rows:
            continue
        labels = rows[0]["labels"]
        head = (f"| Problem | Subjects {labels[0]} | {labels[1]} | Tabletop {labels[0]} | "
                f"{labels[1]} | " + " | ".join(f"v1 `{p}` {labels[0]} | {labels[1]}"
                                              for p in profiles)
                + " | " + " | ".join(f"`{p}` {labels[0]} | {labels[1]}" for p in profiles)
                + " | Fit, French | " + " | ".join(f"fit, v1 `{p}`" for p in profiles)
                + " | " + " | ".join(f"fit, `{p}`" for p in profiles) + " |")
        lines += [f"**Table {table}** [F95 p. {rows[0]['page']}]", "", head,
                  "|---|" + "---:|" * (4 + 4 * len(profiles) + 1 + 2 * len(profiles))]
        for r in rows:
            v1 = v1_record(res, "humans", problem=r["problem"], table=table)
            ours = " | ".join(f"{r['ours'][p][0]:.0f} | {r['ours'][p][1]:.0f}" for p in profiles)
            fits = " | ".join(f"{r['fits'][p]:.0f}" for p in profiles)
            v1_ours = " | ".join(f"{v1['ours'][p][0]:.0f} | {v1['ours'][p][1]:.0f}" if v1
                                 else "- | -" for p in profiles)
            v1_fits = " | ".join(f"{v1['fits'][p]:.0f}" if v1 else "-" for p in profiles)
            lines.append(f"| {r['problem']} | {r['subjects'][0]} | {r['subjects'][1]} "
                         f"| {r['tabletop'][0]} | {r['tabletop'][1]} | {v1_ours} | {ours} "
                         f"| {r['french_fit']} | {v1_fits} | {fits} |")
        lines.append("")
    his_within = sum(r["french_fit"] <= 10 for r in records)
    v1_records = res.v1["humans"] if res.v1 else []
    v1_within = ("; " + "; ".join(f"v1 `{p}` on {sum(r['fits'][p] <= 10 for r in v1_records)}"
                                  for p in profiles)) if v1_records else ""
    lines.append(f"Within ten points of the subjects [F95 p. 144]: French's Tabletop on "
                 f"{his_within} of {len(records)}{v1_within}; " + "; ".join(
                     f"`{p}` on {sum(r['fits'][p] <= 10 for r in records)}" for p in profiles)
                 + ".")
    lines.append("")
    lines += ["| Profile | Spearman with the subjects | with Tabletop's second runs | "
              "our fit with French's fit |", "|---|---:|---:|---:|"]
    for engine, recs in (("v1 ", v1_records), ("", records)):
        for p in profiles:
            if recs:
                lines.append(spearman_row(engine, p, recs))
    return lines


def spearman_row(engine: str, profile: str, records: list) -> str:
    """One row of the rank correlations: with the subjects, with Tabletop's second runs,
    and of our fit with French's (D-075, D-078)."""
    subjects, tabletop, ours, his_fit, our_fit = [], [], [], [], []
    for r in records:
        i = 0 if r["subjects"][0] >= r["subjects"][1] else 1
        subjects.append(r["subjects"][i])
        tabletop.append(r["tabletop"][i])
        ours.append(r["ours"][profile][i])
        his_fit.append(r["french_fit"])
        our_fit.append(r["fits"][profile])
    return (f"| {engine}`{profile}` | {corr(PB.spearman(subjects, ours))} "
            f"| {corr(PB.spearman(tabletop, ours))} | {corr(PB.spearman(his_fit, our_fit))} |")


def corr(value) -> str:
    """A correlation coefficient with its sign, or a dash (D-075)."""
    return "-" if value is None else f"{value:+.2f}"


def survey_summary(sv: dict, touched: str | None = None) -> tuple:
    """A survey record's main answer (answer, label, percent), literal share, mean
    structure, mean and median codelets, capped runs (D-075)."""
    tallies = sv["tallies"]
    top = max(tallies, key=lambda t: t["count"])
    literal = sum(t["frequency"] for t in tallies if touched and t["answer"] == touched)
    n = sum(t["count"] for t in tallies)
    structure = sum(t["structure"] * t["count"] for t in tallies) / n if n else 0.0
    return (top, literal, structure, sv["mean_codelets"], sv.get("median_codelets"),
            sv["capped"])


def variant_tables(res: Results, bench: str) -> list:
    """The probe benches (rho, lesions, faults): a table per problem, a row per variant (D-075)."""
    records = res[bench]
    if not records:
        return [f"(no {bench} records)"]
    variants, problems = [], []
    for r in records:
        if r["variant"] not in variants:
            variants.append(r["variant"])
        if r["survey"]["problem"] not in problems:
            problems.append(r["survey"]["problem"])
    lines = ["| Variant | Changes |", "|---|---|"]
    for v in variants:
        r = next(x for x in records if x["variant"] == v)
        changes = ", ".join(f"{k} = {val}" for k, val in r["changes"]) or "none"
        lines.append(f"| {v} | {changes} |")
    lines.append("")
    for name in problems:
        pb = load_problem(name)
        french = PB.french_main(name)
        heading = f"**{name}**"
        if french is not None:
            heading += (f" (French's main answer {french.label}, {french.frequency} percent "
                        f"[F95 p. {RP.result_sets(name)[0].page}])")
        lines += [heading, "", "| Variant | Main answer | % | v1 main answer | % | "
                  "French's main, % | v1 | Literal, % | v1 | Mean codelets | Median | "
                  "Mean structure | Capped |",
                  "|---|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"]
        for v in variants:
            r = next(x for x in records if x["variant"] == v and x["survey"]["problem"] == name)
            sv = r["survey"]
            top, literal, structure, mean_c, median_c, capped = survey_summary(sv, pb.touched)
            v1 = v1_record(res, bench, variant=v, survey__problem=name)
            v1_sv = v1["survey"] if v1 else None
            v1_literal = survey_summary(v1_sv, pb.touched)[1] if v1_sv else None
            his = v1_his = None
            if french is not None:
                labels = french.label.split("+")
                his = sum(t["frequency"] for t in sv["tallies"] if t["label"] in labels)
                if v1_sv:
                    v1_his = sum(t["frequency"] for t in v1_sv["tallies"] if t["label"] in labels)
            who = f"{top['answer']} ({top['label']})" if top["label"] else top["answer"]
            lines.append(f"| {v} | {who} | {top['frequency']:.1f} "
                         f"| {v1_top(res, bench, variant=v, survey__problem=name)} "
                         f"| {pct(his)} | {pct(v1_his)} | {literal:.1f} | {pct(v1_literal)} "
                         f"| {mean_c:.1f} | {num(median_c)} | {structure:.1f} | {capped} |")
        lines.append("")
    return lines[:-1]


def ch2_table(res: Results) -> list:
    """(D-075)."""
    records = res["ch2"]
    if not records:
        return ["(no ch2 records)"]
    yes = sum(c["verdict"] is True for r in records for c in r["checks"])
    total = sum(1 for r in records for c in r["checks"])
    v1_recs = res.v1["ch2"] if res.v1 else []
    v1_yes = sum(c["verdict"] is True for r in v1_recs for c in r["checks"])
    v1_note = f" (v1: {v1_yes} of {total})" if v1_recs else ""
    lines = [f"{yes} of {total} checks agree with French's words{v1_note}.", "",
             "| Figure | Page | Most frequent answer | % | v1 most frequent | % | Checks | "
             "v1 | Verdicts |",
             "|---|---:|---|---:|---|---:|---:|---:|---|"]
    for r in records:
        top = max(r["survey"]["tallies"], key=lambda t: t["count"])
        v1 = v1_record(res, "ch2", problem=r["problem"])
        v1_checks = (f"{sum(c['verdict'] is True for c in v1['checks'])} of {len(v1['checks'])}"
                     if v1 else "-")
        verdicts = "; ".join(f"{verdict_word(c['verdict'])}: {c['text']} ({c['numbers']})"
                             for c in r["checks"])
        lines.append(f"| {r['problem']} | {r['page']} | {top['answer']} | {top['frequency']:.0f} "
                     f"| {v1_top(res, 'ch2', problem=r['problem'])} "
                     f"| {sum(c['verdict'] is True for c in r['checks'])} of {len(r['checks'])} "
                     f"| {v1_checks} | {verdicts} |")
    return lines


def verdict_word(verdict) -> str:
    """A check's verdict as a word: yes, no, or nothing to read (D-074, D-075)."""
    if verdict is True:
        return "yes"
    if verdict is False:
        return "no"
    return "nothing to read"


def bars(shares: dict) -> str:
    """A rack's bars as the screen prints them, label and whole percent (D-075)."""
    return " ".join(f"{k} {v:.0f}" for k, v in shares.items())


def ch4_tables(res: Results) -> list:
    """(D-075)."""
    records = res["ch4"]
    if not records:
        return ["(no ch4 records)"]
    r = records[0]
    v1 = res.v1["ch4"][0] if res.v1 and res.v1["ch4"] else None
    v1_dumps = {d["figure"]: d for d in v1["dumps"]} if v1 else {}
    v1_miles = {m["name"]: m for m in v1["milestones"]} if v1 else {}
    v1_tallies = {t["answer"]: t for t in v1["survey"]["tallies"]} if v1 else {}
    lines = ["| Dump | Page | Codelets | T, French | T, v1 (mean) | T, ours (mean) | "
             "Runs still going | Bars, French | Same labels, ours (mean) | Our other bars >= 5 |",
             "|---|---:|---:|---:|---:|---:|---:|---|---|---|"]
    for d in r["dumps"]:
        v1d = v1_dumps.get(d["figure"])
        lines.append(f"| {d['figure']} | {d['page']} | {d['codelets']} | {num(d['french_t'], 1)} "
                     f"| {num(v1d['ours_t'], 1) if v1d else '-'} "
                     f"| {num(d['ours_t'], 1)} | {d['alive']} | {bars(d['french_bars'])} "
                     f"| {bars(d['ours_bars'])} | {bars(d['others'])} |")
    lines += ["", "| Milestone | French's run: first dump showing it (codelets) | "
              "v1 runs reaching it, % | Our runs reaching it, % | Mean codelet, ours |",
              "|---|---:|---:|---:|---:|"]
    for m in r["milestones"]:
        v1m = v1_miles.get(m["name"])
        lines.append(f"| {m['name']} | {m['french']} | {pct(v1m['share']) if v1m else '-'} "
                     f"| {m['share']:.1f} | {num(m['mean'], 1)} |")
    lines += ["", "| Answer | Label | Runs | % | v1 % | Mean structure | Mean codelets |",
              "|---|---|---:|---:|---:|---:|---:|"]
    for t in r["survey"]["tallies"]:
        v1t = v1_tallies.get(t["answer"])
        lines.append(f"| {t['answer']} | {t['label'] or '-'} | {t['count']} | {t['frequency']:.1f} "
                     f"| {pct(v1t['frequency']) if v1t else ('0.0' if v1 else '-')} "
                     f"| {t['structure']:.1f} | {t['codelets']:.1f} |")
    return lines


def focus_tables(res: Results) -> list:
    """(D-075)."""
    records = res["focus"]
    if not records:
        return ["(no focus records)"]
    lines = ["| Problem | Junk objects | Main answer | % | v1 main answer | % | Literal, % | "
             "Mean codelets | v1 | Median | Mean structure | Capped |",
             "|---|---:|---|---:|---|---:|---:|---:|---:|---:|---:|---:|"]
    for r in records:
        sv = r["survey"]
        name = r["problem"]
        pb = load_problem(name) if name in catalogue() else None
        touched = pb.touched if pb else None
        top, literal, structure, mean_c, median_c, capped = survey_summary(sv, touched)
        who = f"{top['answer']} ({top['label']})" if top["label"] else top["answer"]
        junk = r.get("junk")
        label = f"{name}" if junk is None else f"{name} + junk"
        v1 = v1_record(res, "focus", problem=name, junk=junk)
        lines.append(f"| {label} | {num(junk) if junk is not None else '-'} | {who} "
                     f"| {top['frequency']:.1f} | {v1_top(res, 'focus', problem=name, junk=junk)} "
                     f"| {literal:.1f} | {mean_c:.1f} "
                     f"| {num(v1['survey']['mean_codelets'], 1) if v1 else '-'} | {num(median_c)} "
                     f"| {structure:.1f} | {capped} |")
    return lines


def temperature_curve_block(rows: list, french: dict | None) -> list:
    """One curve of the `temperature` bench (D-073): T's mean, quartiles and the runs still
    going at each codelet count, with French's thermometer above it at the chapter 4 dumps."""
    clocks = [int(r[0]) for r in rows]
    out = ["| Codelets run | " + " | ".join(str(c) for c in clocks) + " |",
           "|---|" + "---:|" * len(clocks)]
    if french is not None:
        out.append("| T, French's run [F95 pp. 96-107 (img)] | "
                   + " | ".join(num(french.get(c), 1) for c in clocks) + " |")
    for label, i in (("T, mean", 1), ("lower quartile", 2), ("median", 3),
                     ("upper quartile", 4), ("runs still going, %", 5)):
        out.append(f"| {label} | " + " | ".join(num(r[i], 1) for r in rows) + " |")
    return out


def temperature_variant_row(res: Results, r: dict, touched: str) -> str:
    """One variant's row of the `temperature` summary (D-075): the end and lowest T, the
    first change, the updates by cause, the mean terms and the main answer, with v1's end T,
    first change and no-change share beside (D-078)."""
    s, sv = r["summary"], r["survey"]
    v1 = v1_record(res, "temperature", variant=r["variant"], survey__problem=sv["problem"])
    v1s = v1["summary"] if v1 else {}
    top, literal, _, mean_c, _, _ = survey_summary(sv, touched)
    who = f"{top['answer']} ({top['label']})" if top["label"] else top["answer"]
    u = s["updates_by"]
    return (f"| {r['variant']} | {num(s['end_mean'], 1)} | {num(v1s.get('end_mean'), 1)} "
            f"| {num(s['end_median'], 1)} "
            f"| {num(s['lowest_mean'], 1)} | {num(s['first_change_median'])} "
            f"| {num(v1s.get('first_change_median'))} "
            f"| {num(s['no_change_share'], 1)} | {num(v1s.get('no_change_share'), 1)} "
            f"| {num(s['after_first_mean'], 1)} "
            f"| {num(u.get('schedule'), 1)} / {num(u.get('competition'), 1)} / "
            f"{num(u.get('change'), 1)} | " + ", ".join(num(t, 1) for t in s["terms_mean"])
            + f" | {who} | {top['frequency']:.1f} | {literal:.1f} | {mean_c:.1f} |")


def temperature_tables(res: Results) -> list:
    """The `temperature` bench (D-073): per problem the intact curve, Surround 5's beside
    French's thermometer, and the summary per variant (D-075)."""
    records = res["temperature"]
    if not records:
        return ["(no temperature records)"]
    variants, problems = [], []
    for r in records:
        if r["variant"] not in variants:
            variants.append(r["variant"])
        if r["survey"]["problem"] not in problems:
            problems.append(r["survey"]["problem"])
    lines = ["| Variant | Changes |", "|---|---|"]
    for v in variants:
        r = next(x for x in records if x["variant"] == v)
        lines.append(f"| {v} | {', '.join(f'{k} = {val}' for k, val in r['changes']) or 'none'} |")
    lines.append("")
    for pname in problems:
        pb = load_problem(pname)
        intact = next(r for r in records if r["variant"] == variants[0]
                      and r["survey"]["problem"] == pname)
        lines += [f"**{pname}**, intact: T against the codelets run (the runs still going at "
                  f"each count)", ""] + temperature_curve_block(intact["curve"], None) + [""]
        if intact.get("dumps"):
            french = {d["codelets"]: d["french_t"] for d in intact["dumps"]}
            lines += ["At the chapter 4 dumps' codelet counts, beside French's thermometer:", ""]
            lines += temperature_curve_block([d["ours"] for d in intact["dumps"]], french) + [""]
        lines += ["| Variant | T at the end, mean | v1 | median | Lowest T, mean | First change at "
                  "codelet (median) | v1 | Runs with no change, % | v1 | T after the first "
                  "change, mean | Updates per run: schedule / competition / change | Mean "
                  "terms: alpha dQ/Q, beta dWV, gamma Tcp | Main answer | % | Literal, % | "
                  "Mean codelets |",
                  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---|---|---:|---:|---:|"]
        for v in variants:
            r = next(x for x in records if x["variant"] == v and x["survey"]["problem"] == pname)
            lines.append(temperature_variant_row(res, r, pb.touched))
        lines.append("")
    return lines[:-1]


def stated_table(res: Results) -> list:
    """(D-075)."""
    records = [r for r in res["stated"] if "number" in r]
    if not records:
        return ["(no stated records)"]
    yes = sum(r["verdict"] is True for r in records)
    no = sum(r["verdict"] is False for r in records)
    v1_recs = [r for r in (res.v1["stated"] if res.v1 else []) if "number" in r]
    v1_by = {r["number"]: r for r in v1_recs}
    v1_note = (f" (v1: {sum(r['verdict'] is True for r in v1_recs)} agree, "
               f"{sum(r['verdict'] is False for r in v1_recs)} disagree)" if v1_recs else "")
    lines = [f"{yes} statements agree, {no} disagree, {len(records) - yes - no} have nothing "
             f"to read{v1_note}.", "",
             "| No. | Statement | Source | Problem | Profile | v1 numbers | v1 | Our numbers | "
             "Verdict |",
             "|---:|---|---|---|---|---|---|---|---|"]
    for r in records:
        v1 = v1_by.get(r["number"])
        lines.append(f"| {r['number']} | {r['statement']} | {r['source']} | {r['problem']} "
                     f"| {r['profile']} | {v1['numbers'] if v1 else '-'} "
                     f"| {verdict_word(v1['verdict']) if v1 else '-'} "
                     f"| {r['numbers']} | {verdict_word(r['verdict'])} |")
    return lines


def potelbat_table(res: Results) -> list:
    """(D-075)."""
    records = res["potelbat"]
    if not records:
        return ["(no potelbat records)"]
    lines = ["| Problem | French's answer | Potelbat's answer | margin | v1, most frequent | % "
             "| Ours, most frequent | % |", "|---|---|---|---:|---|---:|---|---:|"]
    for r in records:
        top = max(r["survey"]["tallies"], key=lambda t: t["count"])
        french = f"{r['french']} ({r['french_label']})" if r["french"] else "-"
        lines.append(f"| {r['problem']} | {french} | {r['potelbat']['answer']} "
                     f"| {r['potelbat']['margin']:.1f} "
                     f"| {v1_top(res, 'potelbat', problem=r['problem'])} "
                     f"| {top['answer']} | {top['frequency']:.0f} |")
    return lines


# ---- robustness (D-068, D-077) ------------------------------------------------------------

def survey_records(res: Results) -> list:
    """Every stripped survey in every bench's records, as (bench, label, survey) triples:
    a survey is a dictionary with `results_count` and `capped` (D-075)."""
    found = []

    def walk(bench, label, value):
        if isinstance(value, dict):
            if "results_count" in value and "capped" in value:
                found.append((bench, label, value))
                return
            for k, v in value.items():
                walk(bench, value.get("variant", value.get("problem", label)) if k in
                     ("survey", "surveys", "scaling") else label, v)
        elif isinstance(value, list):
            for v in value:
                walk(bench, label, v)

    for name in BENCHES:
        for r in res[name]:
            walk(name, r.get("variant", r.get("problem", "")), r)
    return found


def capped_table(res: Results) -> list:
    """The share of capped runs (D-061) over every survey of every bench, bench by bench,
    with the variants that cap runs by design named (D-077)."""
    surveys = survey_records(res)
    if not surveys:
        return ["(no records)"]
    lines = ["| Bench | Surveys | Runs | Capped | Share |", "|---|---:|---:|---:|---:|"]
    total_runs = total_capped = 0
    by_variant: dict = {}
    for name in BENCHES:
        rows = [(label, sv) for bench, label, sv in surveys if bench == name]
        if not rows:
            continue
        runs = sum(sv["results_count"] for _, sv in rows)
        capped = sum(sv["capped"] for _, sv in rows)
        total_runs += runs
        total_capped += capped
        lines.append(f"| {name} | {len(rows)} | {runs} | {capped} | {100.0 * capped / runs:.2f}% |")
        for label, sv in rows:
            if sv["capped"]:
                key = (name, str(label))
                by_variant[key] = (by_variant.get(key, (0, 0))[0] + sv["capped"],
                                   by_variant.get(key, (0, 0))[1] + sv["results_count"])
    lines.append(f"| all | {len(surveys)} | {total_runs} | {total_capped} | "
                 f"{100.0 * total_capped / total_runs:.2f}% |")
    if by_variant:
        lines += ["", "Where the capped runs are (bench, variant or problem: capped of runs):", ""]
        lines += [f"- {b}, {label}: {c} of {n}" for (b, label), (c, n) in
                  sorted(by_variant.items(), key=lambda kv: -kv[1][0])]
    return lines


def churn_tables(res: Results) -> list:
    """Roles over time from the `churn` records (D-106), RESULTS.md §11.7."""
    return CH.tables(res["churn"])


def robustness_roles_tables(res: Results) -> list:
    """The role statistics on the random tables from the `robustness` records (D-068,
    D-106), RESULTS.md §11.6."""
    from tabletop.bench import robustness as RB    # imports `quartiles` from here
    tables = [r for r in res["robustness"] if r.get("kind") == "tables" and "roles" in r]
    if not tables:
        return ["(no robustness records with role statistics)"]
    lines = []
    for s in tables:
        lines += [f"**{s['tables']} random tables under `{s['profile']}`** (bench seed "
                  f"{s['seed']}):", ""] + RB.roles_lines(s)
    return lines


def robustness_readings(s: dict) -> list:
    """The readings of one `tables` record as (label, cell) pairs (D-068, D-077, D-078)."""
    n = s["tables"]
    a = s["answers"]
    return [("Runs capped (D-061)", f"{s['capped']} of {n}"),
            ("Invariant violations (D-077)", f"{s['faults']} in {s['tables_with_faults']} tables"),
            ("Literal answers (D-063)", f"{a['literal']} ({100.0 * a['literal'] / n:.1f}%)"),
            ("Answers on the other side of the table",
             f"{a['other_side']} ({100.0 * a['other_side'] / n:.1f}%)"),
            ("Answers of the touched object's type",
             f"{a['same_type']} ({100.0 * a['same_type'] / n:.1f}%)"),
            ("Answers by route", ", ".join(f"{k} {v}" for k, v in sorted(a["through"].items()))),
            ("Codelets per run", f"min {s['codelets']['min']}, quartiles "
                                 f"{s['codelets']['quartiles']}, mean {s['codelets']['mean']:.1f}, "
                                 f"max {s['codelets']['max']}"),
            ("Groups, correspondences, Worldview size at the end (means)",
             f"{s['groups_mean']:.1f}, {s['correspondences_mean']:.1f}, {s['worldview_mean']:.2f}"),
            ("Touched types", ", ".join(f"{k} {v}" for k, v in
                                        sorted(s["touched"].items(), key=lambda kv: -kv[1])))]


def robustness_tables(res: Results) -> list:
    """The random tables' runs and the speed profile from the `robustness` records
    (D-068, D-077)."""
    records = res["robustness"]
    tables = [r for r in records if r.get("kind") == "tables"]
    speed = [r for r in records if r.get("kind") == "speed"]
    if not tables:
        return ["(no robustness records)"]
    lines = []
    for s in tables:
        n = s["tables"]
        v1 = v1_record(res, "robustness", kind="tables", profile=s["profile"], seed=s["seed"])
        lines += [f"**{n} random tables under `{s['profile']}`** (bench seed {s['seed']}): "
                  f"{s['objects']['min']} to {s['objects']['max']} objects, "
                  f"{s['objects']['mean']:.1f} on average"
                  + (" (v1 on the same tables beside)." if v1 else "."), "",
                  "| Reading | v1 | Value |", "|---|---|---|"]
        for label, cell in robustness_readings(s):
            lines.append(f"| {label} | {dict(robustness_readings(v1)).get(label, '-') if v1 else '-'} "
                         f"| {cell} |")
        lines.append("")
        if s["fault_kinds"]:
            lines += ["Violations found:", ""]
            lines += [f"- {k}: {v}" for k, v in sorted(s["fault_kinds"].items(),
                                                       key=lambda kv: -kv[1])]
            lines.append("")
    for sp in speed:
        lines += [f"**Speed on one core**, {sp['runs']} runs per problem, Python "
                  f"{sp['python']} (machine-dependent):", "",
                  "| Problem | Plain runs/s | Plain ms/codelet | Checked runs/s | "
                  "Checked ms/codelet | Overhead |", "|---|---:|---:|---:|---:|---:|"]
        for row in sp["rows"]:
            p, c = row["plain"], row["checked"]
            lines.append(f"| {row['problem']} | {p['runs_per_second']:.1f} | "
                         f"{p['ms_per_codelet']:.2f} | {c['runs_per_second']:.1f} | "
                         f"{c['ms_per_codelet']:.2f} | x{row['overhead']:.2f} |")
        lines.append("")
    lines += ["**Capped runs over every bench** (D-061):", ""]
    lines += capped_table(res)
    return lines


# ---- the calibrated profiles (D-076; ANSWERS.md 7) ----------------------------------------

def human_half_summary(records: list, profile: str) -> dict:
    """The `human` fit's readings on a set of humans records under one profile (D-076): the
    sum of squares against the subjects' two columns, the mean Figure 5.4 gap, the rows
    within ten points, and the rank correlations of our percentage for the subjects' most
    frequent answer with the subjects' and with Tabletop's second runs'."""
    sse = sum((r["ours"][profile][i] - r["subjects"][i]) ** 2 for r in records for i in (0, 1))
    subjects, tabletop, ours = [], [], []
    for r in records:
        i = 0 if r["subjects"][0] >= r["subjects"][1] else 1
        subjects.append(r["subjects"][i])
        tabletop.append(r["tabletop"][i])
        ours.append(r["ours"][profile][i])
    return {"count": len(records), "sum_of_squares": sse,
            "mean_gap": sum(r["fits"][profile] for r in records) / len(records) if records else 0.0,
            "within_ten": sum(r["fits"][profile] <= 10 for r in records),
            "with_subjects": PB.spearman(subjects, ours) if len(records) > 1 else None,
            "with_tabletop": PB.spearman(tabletop, ours) if len(records) > 1 else None}


def calibration_human_tables(res: Results) -> list:
    """The `human` calibration (D-076): the split, the scan over the grid on the fit half
    with the value chosen, then the evaluation under `standard` and `human` on the fit
    half, the held-out half and all 23 problems, from the `humans` bench's records."""
    scans = res["calibrate-human"]
    humans = res["humans"]
    if not scans or not humans:
        return ["(no calibrate-human or humans records)"]
    scan = scans[0]
    fit, held = scan["fit_problems"], scan["held_out_problems"]
    lines = [f"**The fit.** `{scan['field']}` scanned over {', '.join(str(v) for v in scan['grid'])} "
             f"on the {len(fit)} problems of the fit half (split seed {scan['split_seed']}, bench "
             f"seed {scan['seed']}, {scan['runs']} runs per survey); the objective is the sum of "
             f"squares against the subjects' two columns [F95 pp. 143-144]. Fit half: "
             + ", ".join(fit) + ". Held out: " + ", ".join(held) + ".", "",
             "| `type_weight` | Sum of squares, fit half | Mean gap | Within ten | Chosen |",
             "|---:|---:|---:|---:|---|"]
    for s in scan["scan"]:
        lines.append(f"| {s['value']} | {s['sum_of_squares']:.0f} | {s['mean_gap']:.1f} "
                     f"| {s['within_ten']} of {len(s['rows'])} "
                     f"| {'yes' if s['value'] == scan['chosen'] else ''} |")
    lines += ["", f"Chosen: `{scan['field']} = {scan['chosen']}` (before: {scan['before']}).", "",
              "**The evaluation** (the `humans` bench at 500 runs, both profiles):", "",
              "| Problems | Profile | Sum of squares | Mean gap | Within ten | Spearman with the "
              "subjects | with Tabletop's second runs |", "|---|---|---:|---:|---:|---:|---:|"]
    halves = (("fit half", [r for r in humans if r["problem"] in fit]),
              ("held-out half", [r for r in humans if r["problem"] in held]),
              ("all 23", humans))
    for title, records in halves:
        for profile in ("standard", "human"):
            if not records or profile not in records[0]["ours"]:
                continue
            h = human_half_summary(records, profile)
            lines.append(f"| {title} ({h['count']}) | `{profile}` | {h['sum_of_squares']:.0f} "
                         f"| {h['mean_gap']:.1f} | {h['within_ten']} | {corr(h['with_subjects'])} "
                         f"| {corr(h['with_tabletop'])} |")
    french = sum(r["french_fit"] <= 10 for r in humans)
    lines += ["", f"French's own second runs are within ten points of the subjects on {french} of "
                  f"{len(humans)} [F95 p. 144]."]
    return lines


def calibration_buridan_tables(res: Results) -> list:
    """The `buridan` calibration (D-076): the scan over the penalty on Buridan 1 under the
    modified program with the value chosen, then Buridan 1-13 under `standard` and
    `buridan` beside French's modified-program numbers, from the `families` records."""
    scans = res["calibrate-buridan"]
    if not scans:
        return ["(no calibrate-buridan records)"]
    scan = scans[0]
    fr = scan["french"]
    lines = [f"**The fit.** `{scan['field']}` scanned over {', '.join(str(v) for v in scan['grid'])} "
             f"on {scan['problem']} under the modified program [F95 p. {scan['page']}] (bench seed "
             f"{scan['seed']}, {scan['runs']} runs per survey); the objective is the imbalance "
             f"between E1 and E2, French's 'precisely poised'. French: E1 {fr['E1']}, E2 {fr['E2']}.",
             "", "| `incoherence_penalty` | E1 % | E2 % | other % | abs(E1 - E2) | Distance from "
             "50-50 | Mean codelets | Chosen |", "|---:|---:|---:|---:|---:|---:|---:|---|"]
    for s in scan["scan"]:
        lines.append(f"| {s['value']} | {s['E1']:.1f} | {s['E2']:.1f} | {s['other']:.1f} "
                     f"| {s['imbalance']:.1f} | {s['distance_from_50_50']:.1f} "
                     f"| {s['mean_codelets']:.0f} | {'yes' if s['value'] == scan['chosen'] else ''} |")
    lines += ["", f"Chosen: `{scan['field']} = {scan['chosen']}` (before: {scan['before']}).", "",
              "**The evaluation** (the `families` bench at 500 runs under both profiles; French's "
              "numbers are the modified program's for Buridan 1-13 [F95 pp. 130-139]):", "",
              "| Result set | French E1 | E2 | `standard` E1 | E2 | `buridan` E1 | E2 | z, "
              "`buridan` E1 |", "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for r in res["families"]:
        if family_of(r["problem"]) != "buridan" or r["program"] != "modified":
            continue
        b = res.comparison("families-buridan", r["problem"], r["program"])
        cells = [set_key(r)]
        for rec in (None, r, b):
            for label in ("E1", "E2"):
                row = row_named(rec or r, label)
                if rec is None:
                    cells.append(num(row["french"]["frequency"]))
                else:
                    cells.append(pct(row["ours"]["frequency"]) if row else "-")
        brow = row_named(b, "E1") if b else None
        cells.append(f"{brow['z']:+.2f}" if brow else "-")
        lines.append("| " + " | ".join(cells) + " |")
    return lines


def calibration_families_tables(res: Results) -> list:
    """Before and after on all families (D-076; TASK.md, Validation 2): French's main
    answer's frequency under `standard`, `human` and `buridan` for every result set, and
    M4's count and the mean absolute gap under each."""
    if not res["families"]:
        return ["(no families records)"]
    profiles = [("standard", "families"), ("human", "families-human"), ("buridan", "families-buridan")]
    lines = ["| Result set | French's main answer | French % | " + " | ".join(
        f"`{p}` %" for p, _ in profiles) + " |", "|---|---|---:|" + "---:|" * len(profiles)]
    gaps = {p: [] for p, _ in profiles}
    for r in res["families"]:
        main = main_row(r)
        cells = [set_key(r), main["label"], num(main["french"]["frequency"])]
        for p, bench in profiles:
            rec = res.comparison(bench, r["problem"], r["program"])
            row = row_named(rec, main["label"]) if rec else None
            if row:
                gaps[p].append(abs(row["ours"]["frequency"] - main["french"]["frequency"]))
            cells.append(pct(row["ours"]["frequency"]) if row else "-")
        lines.append("| " + " | ".join(cells) + " |")
    lines += ["", "| Profile | Within 15 points (M4) | Mean absolute gap |", "|---|---:|---:|"]
    for p, _ in profiles:
        if gaps[p]:
            lines.append(f"| `{p}` | {sum(g <= WITHIN_POINTS for g in gaps[p])} of {len(gaps[p])} "
                         f"| {sum(gaps[p]) / len(gaps[p]):.1f} |")
    return lines


# ---- the variants bench, v1 beside the roles (D-102) ---------------------------------------

VARIANT_ORDER = ("mirrored", "side-swapped", "rotated", "translated", "type-substituted",
                 "junk-added", "decoupled", "on-saucer")
EXACT_VARIANTS = ("mirrored", "side-swapped", "rotated")
EXCEPTION_P = 0.01


def variant_rows(records: list, variant: str, family: str | None = None) -> list:
    """The applicable variant records of one variant (and family, when given) (D-102)."""
    return [r for r in records if r.get("kind") == "variant" and r.get("applies")
            and r["variant"] == variant and (family is None or family_of(r["problem"]) == family)]


def variant_summary(rows: list) -> dict | None:
    """The readings of a set of variant records (D-102): how many, the mean and largest
    total-variation distance (with its problem), how many at chi-square p below 0.01, the
    mean share of paired runs that kept their role; None for no rows."""
    if not rows:
        return None
    worst = max(rows, key=lambda r: r["tv"])
    return {"n": len(rows), "tv_mean": sum(r["tv"] for r in rows) / len(rows),
            "tv_max": worst["tv"], "tv_max_problem": worst["problem"],
            "p01": sum(r["p"] < EXCEPTION_P for r in rows),
            "kept_mean": sum(r["role_kept"] for r in rows) / len(rows)}


def _pair(a: dict | None, b: dict | None, key: str, digits: int) -> str:
    def cell(s):
        return "-" if s is None else (f"{s[key]:.{digits}f}" if digits else str(s[key]))
    return f"{cell(a)} | {cell(b)}"


def variant_columns(res: Results) -> list:
    """The two engines' records: ("v1", the v1 source's) and ("roles", the current
    folder's), each possibly empty (D-102)."""
    v1 = res.v1["variants"] if res.v1 is not None else []
    return [("v1", v1), ("roles", res["variants"])]


def variants_summary_lines(columns: list) -> list:
    """One row per variant over every problem it applies to, v1 beside roles (D-102)."""
    lines = ["| Variant | Problems, v1 | roles | Mean TV, v1 | roles | Largest TV, v1 | roles "
             "| p < 0.01, v1 | roles | Role kept %, v1 | roles |",
             "|---|---:|---:|---:|---:|---|---|---:|---:|---:|---:|"]
    for v in VARIANT_ORDER:
        a, b = (variant_summary(variant_rows(recs, v)) for _, recs in columns)
        largest = " | ".join("-" if s is None else f"{s['tv_max']:.3f} ({s['tv_max_problem']})"
                             for s in (a, b))
        lines.append(f"| {v} | {_pair(a, b, 'n', 0)} | {_pair(a, b, 'tv_mean', 3)} | {largest} "
                     f"| {_pair(a, b, 'p01', 0)} | {_pair(a, b, 'kept_mean', 1)} |")
    return lines


def variants_family_lines(columns: list) -> list:
    """One row per variant and problem family (D-102)."""
    families = [f for f, _ in FAMILY_TITLES] + ["ch2"]
    lines = ["| Variant | Family | Problems, v1 | roles | Mean TV, v1 | roles | p < 0.01, v1 | roles "
             "| Role kept %, v1 | roles |", "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for v in VARIANT_ORDER:
        for f in families:
            a, b = (variant_summary(variant_rows(recs, v, f)) for _, recs in columns)
            if a is None and b is None:
                continue
            lines.append(f"| {v} | {f} | {_pair(a, b, 'n', 0)} | {_pair(a, b, 'tv_mean', 3)} | "
                         f"{_pair(a, b, 'p01', 0)} | {_pair(a, b, 'kept_mean', 1)} |")
    return lines


def _main_answer(counts: dict, answers: dict) -> str:
    answer = max(counts, key=counts.get)
    label = next((lb for lb, id_ in answers.items() if id_ == answer), "")
    share = 100.0 * counts[answer] / sum(counts.values())
    return (f"{answer} ({label})" if label else answer) + f" {share:.0f}%"


def variants_exception_lines(columns: list) -> list:
    """The exact variants whose answer distribution differs from the original's at
    chi-square p below 0.01, per engine: the findings of exact invariance (D-102)."""
    lines = ["| Engine | Problem | Variant | TV | 95% interval | p | Role kept % | "
             "Main answer, original | Main answer, variant |", "|---|---|---|---:|---|---:|---:|---|---|"]
    found = 0
    for engine, recs in columns:
        originals = {r["problem"]: r for r in recs if r.get("kind") == "original"}
        for r in recs:
            if (r.get("kind") == "variant" and r.get("applies") and r["variant"] in EXACT_VARIANTS
                    and r["p"] < EXCEPTION_P):
                found += 1
                answers = load_problem(r["problem"]).answers
                lo, hi = r["tv_interval"]
                lines.append(f"| {engine} | {r['problem']} | {r['variant']} | {r['tv']:.3f} | "
                             f"[{lo:.3f}, {hi:.3f}] | {r['p']:.4f} | {r['role_kept']:.1f} | "
                             f"{_main_answer(originals[r['problem']]['answers'], answers)} | "
                             f"{_main_answer(r['answers'], answers)} |")
    return lines if found else ["(none: no exact variant differs at p below 0.01)"]


def variants_mechanism_lines(columns: list) -> list:
    """The mechanism measures over the originals, per engine (D-102)."""
    lines = ["| Engine | Problems | Runs each | Worldview 2+ % | Coherent pairs | Incoherent pairs "
             "| Role tags per run | Mean codelets |", "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for engine, recs in columns:
        originals = [r for r in recs if r.get("kind") == "original"]
        if not originals:
            lines.append(f"| {engine} | 0 | - | - | - | - | - | - |")
            continue
        n = len(originals)
        mean = lambda key: sum(r["mechanism"][key] for r in originals) / n
        lines.append(f"| {engine} | {n} | {originals[0]['runs']} | {mean('wv2'):.1f} | "
                     f"{mean('coherent'):.3f} | {mean('incoherent'):.3f} | "
                     f"{mean('tags'):.3f} | "
                     f"{sum(r['mean_codelets'] for r in originals) / n:.1f} |")
    return lines


def variants_tables(res: Results) -> list:
    """The variants bench, v1 beside the roles (D-102): per variant, per family, the
    exceptions to exact invariance, and the mechanism on the originals."""
    columns = variant_columns(res)
    if not any(recs for _, recs in columns):
        return ["(no variants records)"]
    lines = []
    for engine, recs in columns:
        if not recs:
            lines.append(f"(no {engine} variants records yet)")
    if lines:
        lines.append("")
    lines += ["**Per variant**, over every problem it applies to. TV is the total-variation "
              "distance between the original's and the variant's answer distributions after "
              "relabelling; p the chi-square of homogeneity between the two; \"role kept\" the "
              "share of paired runs (the same run seed) whose answer carries the same label, or "
              "is the same object, as the original run's.", ""]
    lines += variants_summary_lines(columns) + ["", "**Per family.**", ""]
    lines += variants_family_lines(columns) + ["", "**Exact invariance, the exceptions** "
              "(mirrored, side-swapped, rotated at p below 0.01).", ""]
    lines += variants_exception_lines(columns) + ["", "**The mechanism on the originals.**", ""]
    lines += variants_mechanism_lines(columns)
    return lines


def roles_families_tables(res: Results) -> list:
    """RESULTS.md §11.1 (D-078): every result set of chapter 5 with French | v1 | roles for
    his main answer, the z of roles against v1, and the chi-square p of each engine against
    his counts; then every answer row that moved beyond z = 2 between v1 and roles, read as
    gained (nearer French's frequency) or lost (further from it), counted apart, never net."""
    records = res["families"]
    if not records or res.v1 is None or not res.v1["families"]:
        return ["(no families records with a v1 source)"]
    lines = ["| Result set | French's main answer | French % | v1 % | roles % | z, roles "
             "against v1 | chi-square p, v1 | p, roles |",
             "|---|---|---:|---:|---:|---:|---:|---:|"]
    moved = []
    for r in records:
        v1r = res.v1.comparison("families", r["problem"], r["program"])
        main = main_row(r)
        v1_main = row_named(v1r, main["label"]) if v1r else None
        zv = z_against_v1(main["ours"]["frequency"], r["runs"],
                          v1_main["ours"]["frequency"] if v1_main else None,
                          v1r["runs"] if v1r else 0)
        v1_p = f"{v1r['p']:.3f}" if v1r else "-"
        lines.append(f"| {r['title']} | {main['label']} | {num(main['french']['frequency'])} "
                     f"| {v1_pct(v1_main)} | {pct(main['ours']['frequency'])} | {signed(zv)} "
                     f"| {v1_p} | {r['p']:.3f} |")
        for row in r["rows"]:
            v1row = row_named(v1r, row["label"]) if v1r else None
            if v1row is None:
                continue
            z = z_against_v1(row["ours"]["frequency"], r["runs"], v1row["ours"]["frequency"],
                             v1r["runs"])
            if z is None or abs(z) <= SIGMAS:
                continue
            his = row["french"]["frequency"]
            if his is None:
                reading = "moved (French prints no bar)"
            else:
                nearer = abs(row["ours"]["frequency"] - his) < abs(v1row["ours"]["frequency"] - his)
                reading = "gained" if nearer else "lost"
            moved.append((reading, f"{r['title']}, {row['label']}: French {num(his)}, v1 "
                                   f"{pct(v1row['ours']['frequency'])}, roles "
                                   f"{pct(row['ours']['frequency'])} (z {z:+.1f})"))
    gained = [m for k, m in moved if k == "gained"]
    lost = [m for k, m in moved if k == "lost"]
    other = [m for k, m in moved if k not in ("gained", "lost")]
    lines += ["", f"**Answers moved beyond z = {SIGMAS:.0f} between v1 and roles** (n = 500 "
                  f"each): gained {len(gained)}, lost {len(lost)}"
                  + (f", moved where French prints no bar {len(other)}" if other else "") + ".", ""]
    for title, items in (("Gained", gained), ("Lost", lost), ("Moved", other)):
        if items:
            lines += [f"{title}:", ""] + [f"- {m}" for m in items] + [""]
    return lines[:-1] if lines[-1] == "" else lines


# ---- v3: the optimized defaults against French, v1 and v2 (D-114) ------------------------

class _Run:
    """A run rebuilt from a record's answer counts: what the distances read (D-114)."""

    def __init__(self, answer: str):
        self.seed, self.answer, self.structure_value, self.codelets, self.capped = (
            0, answer, 0.0, 0, False)


def _survey_from_counts(record: dict):
    """A families record's survey, its runs rebuilt from the answer counts of its rows
    (every answer is in one row's `answers`), enough for the distances below (D-114)."""
    from tabletop.bench import harness as H
    pb = load_problem(record["problem"])
    runs = [_Run(a) for row in record["rows"] for a, n in row["ours"]["answers"]
            for _ in range(n)]
    return H.Survey(pb.name, record["profile"], record["seed"], record["batch_seed"],
                    len(runs), H.tallies_of(runs, pb), 0, 0.0, ())


def _set_distance(sv, rs) -> float:
    """The total variation between a survey and a French result set, over his printed
    labels and `other` (his `other` is 100 less the printed sum), on [0, 1] (D-114)."""
    from tabletop.bench import harness as H
    c = H.compare(sv, rs)
    printed = sum(r.french_frequency for r in c.rows if r.french_frequency is not None)
    ours = [r.frequency for r in c.rows]
    his = [r.french_frequency if r.french_frequency is not None else max(0.0, 100.0 - printed)
           for r in c.rows]
    if not any(r.label == H.OTHER for r in c.rows) and printed < 100:
        ours.append(0.0)
        his.append(100.0 - printed)
    return 0.5 * sum(abs(x - y) for x, y in zip(ours, his)) / 100.0


def _tv(a: list, b: list) -> float:
    sa, sb = sum(a), sum(b)
    return 0.5 * sum(abs(x / sa - y / sb) for x, y in zip(a, b))


def objective(res: Results) -> dict | None:
    """J = (2F + H) / 3 (D-114), the Bayesian optimization's objective, from a results
    directory's families records: F the mean total variation over French's 37 targets (the
    35 chapter 5 sets and FCCA's two sets whose numbers differ from the book's), H over the
    28 human targets (the 23 rows of Tables 5.1-5.3, the two columns and the rest, the
    subjects clipped at 0 and renormalised; and the five other human data, |ours - theirs| /
    100). None without families records."""
    from tabletop.bench import fcca as FC
    from tabletop.bench import humans as HM
    records = {r["problem"]: r for r in res["families"]}
    if not records:
        return None
    surveys = {name: _survey_from_counts(r) for name, r in records.items()}
    french = [_set_distance(surveys[rs.problem], rs) for rs in RP.CHAPTER5]
    french += [_set_distance(surveys[rs.problem], rs) for rs in FC.divergent()]
    human = []
    for row in RP.SUBJECT_ROWS:
        sv, pb = surveys[row.problem], load_problem(row.problem)
        ids = HM.answer_ids(row, pb)
        o = [PB.frequency(sv, ids[0]), PB.frequency(sv, ids[1])]
        o.append(max(0.0, 100.0 - sum(o)))
        t = [float(row.subjects[0]), float(row.subjects[1])]
        t.append(max(0.0, 100.0 - sum(t)))
        human.append(_tv(o, t))
    for d in RP.HUMAN_DATA:
        if d.percent is None or d.problem not in surveys:
            continue
        obj = HM.datum_object(d, load_problem(d.problem))
        human.append(abs(PB.frequency(surveys[d.problem], [obj]) - d.percent) / 100.0)
    f, h = sum(french) / len(french), sum(human) / len(human)
    codelets = sum(r["mean_codelets"] for r in records.values()) / len(records)
    capped = sum(r["capped"] for r in records.values()) / sum(r["runs"] for r in records.values())
    return {"J": (2 * f + h) / 3, "F": f, "H": h, "french": len(french), "human": len(human),
            "codelets": codelets, "capped": 100.0 * capped}


def v3_tables(res: Results) -> list:
    """RESULTS.md's v3 section (D-114): J, F and H for v1, v2 (the roles engine on its
    a-priori defaults, `results/v2/`) and v3 (the optimized defaults, `results/`), and every
    result set's main answer French | v1 | v2 | v3 with the total variation from French of
    v2 and of v3. The cross-validated estimate and the mechanism measures are the study's,
    in the prose around the block. Since v4 (D-115) the v3 column reads `results/v3/` when
    it is there, so that the block stands as v3 left it."""
    if res.v1 is None or res.v2 is None or not res["families"]:
        return ["(no families records with v1 and v2 sources)"]
    cols = (("v1", res.v1), ("v2", res.v2), ("v3", res.v3 if res.v3 is not None else res))
    scores = {name: objective(r) for name, r in cols}
    lines = ["| Engine | J | F (French, mean TV) | H (human, mean TV) | French targets | "
             "human targets | mean codelets | capped % |", "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for name, _ in cols:
        s = scores[name]
        lines.append(f"| {name} | {s['J']:.4f} | {s['F']:.4f} | {s['H']:.4f} | {s['french']} | "
                     f"{s['human']} | {s['codelets']:.1f} | {s['capped']:.1f} |")
    lines += ["", "| Result set | French's main answer | French % | v1 % | v2 % | v3 % | "
                  "TV from French, v2 | TV, v3 |", "|---|---|---:|---:|---:|---:|---:|---:|"]
    for rs in RP.CHAPTER5:
        rec = {n: r.comparison("families", rs.problem, rs.program) for n, r in cols}
        main = main_row(rec["v3"])
        cells = [pct(row_named(rec[n], main["label"])["ours"]["frequency"]) for n, _ in cols]
        tvs = [_set_distance(_survey_from_counts(rec[n]), rs) for n in ("v2", "v3")]
        lines.append(f"| {rec['v3']['title']} | {main['label']} | "
                     f"{num(main['french']['frequency'])} | " + " | ".join(cells)
                     + f" | {tvs[0]:.3f} | {tvs[1]:.3f} |")
    return lines


def v4_tables(res: Results) -> list:
    """RESULTS.md's v4 section (D-115): J, F and H for v1, v2, v3 (`results/v3/`) and v4 (the
    second optimization's defaults, `results/`), and every result set's main answer
    French | v1 | v2 | v3 | v4 with the total variation from French of v3 and of v4. The
    cross-validated estimate and the mechanism measures are the second study's, in the prose
    around the block. Since v5 (D-116 to D-120) the v4 column reads `results/v4/` when it is
    there, so that the block stands as v4 left it."""
    if res.v1 is None or res.v2 is None or res.v3 is None or not res["families"]:
        return ["(no families records with v1, v2 and v3 sources)"]
    cols = (("v1", res.v1), ("v2", res.v2), ("v3", res.v3), ("v4", res.v4 if res.v4 is not None else res))
    scores = {name: objective(r) for name, r in cols}
    lines = ["| Engine | J | F (French, mean TV) | H (human, mean TV) | French targets | "
             "human targets | mean codelets | capped % |", "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for name, _ in cols:
        s = scores[name]
        lines.append(f"| {name} | {s['J']:.4f} | {s['F']:.4f} | {s['H']:.4f} | {s['french']} | "
                     f"{s['human']} | {s['codelets']:.1f} | {s['capped']:.1f} |")
    lines += ["", "| Result set | French's main answer | French % | v1 % | v2 % | v3 % | v4 % | "
                  "TV from French, v3 | TV, v4 |", "|---|---|---:|---:|---:|---:|---:|---:|---:|"]
    for rs in RP.CHAPTER5:
        rec = {n: r.comparison("families", rs.problem, rs.program) for n, r in cols}
        main = main_row(rec["v4"])
        cells = [pct(row_named(rec[n], main["label"])["ours"]["frequency"]) for n, _ in cols]
        tvs = [_set_distance(_survey_from_counts(rec[n]), rs) for n in ("v3", "v4")]
        lines.append(f"| {rec['v4']['title']} | {main['label']} | "
                     f"{num(main['french']['frequency'])} | " + " | ".join(cells)
                     + f" | {tvs[0]:.3f} | {tvs[1]:.3f} |")
    return lines


def v5_tables(res: Results) -> list:
    """RESULTS.md's v5 section (D-116 to D-120, the structural Blockage fix): J, F and H for
    v1, v2, v3, v4 (`results/v4/`) and v5 (the fixed engine, `results/`), and every result
    set's main answer French | v1 | v2 | v3 | v4 | v5 with the total variation from French
    of v4 and of v5; the mechanism table, the Blockage table and the scored predictions are
    in the prose around the block."""
    if res.v1 is None or res.v2 is None or res.v3 is None or res.v4 is None or not res["families"]:
        return ["(no families records with v1, v2, v3 and v4 sources)"]
    cols = (("v1", res.v1), ("v2", res.v2), ("v3", res.v3), ("v4", res.v4), ("v5", res))
    scores = {name: objective(r) for name, r in cols}
    lines = ["| Engine | J | F (French, mean TV) | H (human, mean TV) | French targets | "
             "human targets | mean codelets | capped % |", "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for name, _ in cols:
        s = scores[name]
        lines.append(f"| {name} | {s['J']:.4f} | {s['F']:.4f} | {s['H']:.4f} | {s['french']} | "
                     f"{s['human']} | {s['codelets']:.1f} | {s['capped']:.1f} |")
    lines += ["", "| Result set | French's main answer | French % | v1 % | v2 % | v3 % | v4 % | v5 % | "
                  "TV from French, v4 | TV, v5 |", "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for rs in RP.CHAPTER5:
        rec = {n: r.comparison("families", rs.problem, rs.program) for n, r in cols}
        main = main_row(rec["v5"])
        cells = [pct(row_named(rec[n], main["label"])["ours"]["frequency"]) for n, _ in cols]
        tvs = [_set_distance(_survey_from_counts(rec[n]), rs) for n in ("v4", "v5")]
        lines.append(f"| {rec['v5']['title']} | {main['label']} | "
                     f"{num(main['french']['frequency'])} | " + " | ".join(cells)
                     + f" | {tvs[0]:.3f} | {tvs[1]:.3f} |")
    return lines


BLOCKS = {
    "measures": measures,
    "roles-families": roles_families_tables,
    "families-summary": families_summary,
    "families-tables": families_tables,
    "fcca": fcca_table,
    "humans": humans_tables,
    "ch2": ch2_table,
    "ch4": ch4_tables,
    "focus": focus_tables,
    "stated": stated_table,
    "rho": lambda res: variant_tables(res, "rho"),
    "lesions": lambda res: variant_tables(res, "lesions"),
    "faults": lambda res: variant_tables(res, "faults"),
    "temperature": temperature_tables,
    "potelbat": potelbat_table,
    "calibration-human": calibration_human_tables,
    "calibration-buridan": calibration_buridan_tables,
    "calibration-families": calibration_families_tables,
    "robustness": robustness_tables,
    "robustness-roles": robustness_roles_tables,
    "variants": variants_tables,
    "churn": churn_tables,
    "v3": v3_tables,
    "v4": v4_tables,
    "v5": v5_tables,
}


# The benches each block reads (D-075); a block absent here reads every bench.
BLOCK_SOURCES = {
    "roles-families": ("families",),
    "families-summary": ("families", "families-french-faults"),
    "families-tables": ("families", "families-french-faults"),
    "fcca": ("fcca", "fcca-french-faults"),
    "humans": ("humans",),
    "ch2": ("ch2",), "ch4": ("ch4",), "focus": ("focus",), "stated": ("stated",),
    "rho": ("rho",), "lesions": ("lesions",), "faults": ("faults",),
    "temperature": ("temperature",), "potelbat": ("potelbat",),
    "calibration-human": ("calibrate-human", "humans"),
    "calibration-buridan": ("calibrate-buridan",),
    "calibration-families": ("families", "families-human", "families-buridan"),
    "robustness": ("robustness",), "robustness-roles": ("robustness",),
    "variants": ("variants",), "churn": ("churn",), "v3": ("families",), "v4": ("families",),
    "v5": ("families",),
}


def engines_of(records: list) -> list:
    """The names of the engines that made a list of records (D-107): each record carries
    `engine`, the `accel.info()` of the process that ran it, from the fast-engine loop on;
    a record without one predates that loop (the pure engine, `python`)."""
    out = []
    for r in records:
        info = r.get("engine")
        name = info.get("engine", "?") if isinstance(info, dict) else "python"
        if name not in out:
            out.append(name)
    return out


def engine_line(res: Results, name: str) -> str:
    """The first line of a block: the engine whose runs its numbers are (D-107), and, for a
    block with a v1 column, the engine of `results/v1/` (the pure engine before roles,
    D-078). Empty when the block has no records."""
    benches = BLOCK_SOURCES.get(name, BENCHES)
    engines = engines_of([r for b in benches for r in res[b]])
    if not engines:
        return ""
    line = f"*Engine: {', '.join(engines)}"
    if res.v1 is not None and name in V1_BLOCKS:
        v1 = engines_of([r for b in benches for r in res.v1[b]]) or ["python"]
        line += f"; the v1 columns: {', '.join(v1)}, before roles"
    return line + ".*"


def render(res: Results, name: str) -> str:
    """One block's text, the lines joined, without the markers (D-075): the engine line
    first (D-107), then the block's tables."""
    lines = BLOCKS[name](res)
    engine = engine_line(res, name)
    return "\n".join(([engine, ""] if engine else []) + list(lines)).strip("\n")


def begin_marker(name: str) -> str:
    """(D-075)."""
    return f"<!-- begin {name} -->"


def end_marker(name: str) -> str:
    """(D-075)."""
    return f"<!-- end {name} -->"


def blocks_in(text: str) -> dict:
    """The marked blocks of a Markdown text: name to the text between its markers (D-075)."""
    out = {}
    for name in BLOCKS:
        b, e = begin_marker(name), end_marker(name)
        if b in text and e in text:
            start = text.index(b) + len(b)
            out[name] = text[start:text.index(e, start)].strip("\n")
    return out


def update(res: Results, path: Path) -> list:
    """Replace the content of every marked block in the file; returns the names updated (D-075)."""
    text = path.read_text(encoding="utf-8")
    done = []
    for name in BLOCKS:
        b, e = begin_marker(name), end_marker(name)
        if b in text and e in text:
            start = text.index(b) + len(b)
            end = text.index(e, start)
            text = text[:start] + "\n" + render(res, name) + "\n" + text[end:]
            done.append(name)
    path.write_text(text, encoding="utf-8")
    return done


def main(argv: list) -> int:
    """(D-075)."""
    if len(argv) >= 3 and argv[0] == "strip":
        for p in strip(Path(argv[1]), Path(argv[2])):
            print(p)
        return 0
    if len(argv) >= 2 and argv[0] == "tables":
        res = Results(Path(argv[1]))
        for name in BLOCKS:
            print(begin_marker(name))
            print(render(res, name))
            print(end_marker(name))
            print()
        return 0
    if len(argv) >= 3 and argv[0] == "update":
        done = update(Results(Path(argv[1])), Path(argv[2]))
        print("updated: " + ", ".join(done))
        return 0
    print(__doc__.split("\n\n")[1], file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
