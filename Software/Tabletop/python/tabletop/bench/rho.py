"""The `rho` bench: the urgency-reduction parameter rho from 0.5 to 6 (D-073).

French's adjusted urgency divides a codelet's raw urgency by rho to the power of its
generation [F95 p. 81 (img)], with rho = 3 (D-052). He describes the two extremes: "If p
is 1, we have the original situation, in which all codelets, when put on the Coderack, are
given their raw urgency." [F95 p. 82]; a large value runs the program "in a completely
hierarchical way: parent codelets will invariably run before their children do" [F95
p. 82], breadth first, and a value below 1 depth first [F95 p. 82]. He reports no numbers
for any value but his own. The bench surveys a few problems at each value and prints the
answers, the run lengths and the structure values side by side.
"""

from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.config import Config

RHOS = (0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0)   # the values French discusses [F95 p. 82], and 3 (D-052)
SETS = P.DEFAULT_PROBLEMS
problems_of = P.problems_of


def variants(config: Config) -> list:
    """One variant per value of rho, the profile's own value marked (D-073)."""
    out = []
    for r in RHOS:
        note = ("the profile's own value [F95 p. 81]" if r == config.rho else
                "the raw urgencies themselves [F95 p. 82]" if r == 1.0 else
                "depth first: the newest codelets gain [F95 p. 82]" if r < 1.0 else
                "towards breadth first: the elders run first [F95 p. 82]")
        out.append(P.variant(config, f"rho {r:g}", note, rho=r))
    return out


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    return len(RHOS) * len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> dict:
    """Survey the problems at every value of rho (D-073). Returns the surveys keyed by
    (variant name, problem name), with the variants under `"variants"`."""
    vs = variants(config)
    surveys = P.survey_variants(vs, problems_of(sets, only), seed, runs, progress)
    return {"variants": vs, "problems": problems_of(sets, only), "surveys": surveys}


def report(results: dict) -> list:
    """The bench's text: the variants, then one table per problem (D-073)."""
    lines = ["## The urgency parameter rho, 0.5 to 6 [F95 pp. 81-82]", ""]
    lines += P.variants_table(results["variants"])
    for name in results["problems"]:
        lines += P.variant_table(name, results["variants"], results["surveys"])
    return lines


def records(results: dict) -> list:
    """The JSON records (D-073)."""
    return P.variant_records("rho", results["variants"], results["problems"],
                             results["surveys"])
