"""The `lesions` bench: FCCA's criterion 11, lesioning the architecture (D-073).

FCCA lists "just a few of dozens of interesting ways of 'lesioning' Copycat or Tabletop"
[FCCA p. 365]: making all concepts the same conceptual depth (2), clamping the
temperature "at a very high value or a very low value" (3), and preventing "link-lengths
in the Slipnet from changing at all during a run" (4) [FCCA p. 365]; the first, telescoping
the scout and effector codelets into one, has no knob here (D-073). Mitchell's lesions of
Copycat showed that "each lesion caused the expected type of serious degradation that it
should have" [FCCA p. 365]. To those three the bench adds rho = 1, the raw urgencies [F95
p. 82]; losers destroyed at once instead of fading [F95 p. 73] (D-041); and the context
mechanism switched off (D-021). Each lesion is one variant of the profile, surveyed on a
few problems beside the intact profile; the expectations in `LESIONS` are the bench's own
(D-073), not French's, who reports no lesion of Tabletop.
"""

from tabletop.bench import probes as P
from tabletop.config import Config

# (name, changes, what it is with its page, what we expect to see)
LESIONS = (
    ("temperature-high", {"temperature_fixed": 100.0},
     "lesion (3): the temperature clamped at a very high value [FCCA p. 365] (D-047)",
     "every choice nearly uniform: more unlikely answers, runs that end only by the cap"),
    ("temperature-low", {"temperature_fixed": 0.0},
     "lesion (3): the temperature clamped at a very low value [FCCA p. 365] (D-047)",
     "deterministic choices and find-answers from the start: shorter, more literal runs"),
    ("rho-1", {"rho": 1.0},
     "the raw urgencies, no reduction by generation [F95 p. 82] (D-052)",
     "the urgency explosion French describes: the children swamp their parents"),
    ("losers-destroyed", {"losers_destroyed": True},
     "a losing correspondence destroyed at once instead of fading [F95 p. 73] (D-041)",
     "no return of a loser: fewer changes of Worldview after the first fight"),
    ("flat-depths", {"depth_by_level": (50, 50, 50)},
     "lesion (2): every concept at the same conceptual depth [FCCA p. 365] (D-008)",
     "abstract concepts decay like concrete ones: less slippage, more literal answers"),
    ("fixed-lengths", {"label_shrink": 0.0},
     "lesion (4): link lengths that never change during a run [FCCA p. 365] (D-011)",
     "no label-driven slippage: fewer opposite-side correspondences"),
    ("context-off", {"context_mechanism": False},
     "no push or pop on entering a group correspondence [F95 pp. 87-89] (D-021)",
     "the same Slipnet inside and outside a subworld: little change, since pushes are rare"),
)
SETS = P.DEFAULT_PROBLEMS
problems_of = P.problems_of

# M7's reading of each expectation (D-073): per lesion, the quantities of a probe table that
# the expectation says should move, with the direction; an empty tuple expects no change.
# The quantities: `literal` (the literal answer's share), `french_main` (the share of
# French's main answer), `codelets` (the mean run length), `capped` (the capped runs).
READINGS = {
    "temperature-high": (("capped", "up"), ("codelets", "up")),
    "temperature-low": (("literal", "up"), ("codelets", "down")),
    "rho-1": (("literal", "down"),),
    "losers-destroyed": (("literal", "down"), ("codelets", "up")),
    "flat-depths": (("literal", "up"),),
    "fixed-lengths": (("french_main", "down"),),
    "context-off": (),
}
NOISE_SIGMAS = 2.0      # a percentage has moved beyond noise at two standard errors
CODELETS_MOVED = 0.10   # a mean run length has moved at a tenth of the intact mean
CAPPED_MOVED = 0.05     # the capped share has moved at five percent of the runs


def _moved(quantity: str, intact: dict, lesioned: dict, runs: int) -> int:
    """+1, 0 or -1: whether a quantity rose, stayed or fell beyond noise (D-073)."""
    a, b = intact[quantity], lesioned[quantity]
    if quantity == "codelets":
        limit = CODELETS_MOVED * a
    elif quantity == "capped":
        limit = CAPPED_MOVED * runs
    else:
        p = (a + b) / 200.0
        limit = NOISE_SIGMAS * 100.0 * (max(p * (1 - p), 1e-9) * 2.0 / runs) ** 0.5
    return 0 if abs(b - a) <= limit else (1 if b > a else -1)


def verdict(name: str, intact: dict, lesioned: dict, runs: int) -> str:
    """M7's verdict for one lesion on one problem (D-073): `as predicted` when every
    quantity `READINGS` names moved the predicted way (or, for a lesion expected to change
    nothing, when none of the four moved), `against` when one moved the other way, and
    `no change` otherwise. `intact` and `lesioned` are {quantity: value} with the four
    quantities of `READINGS`."""
    expected = READINGS[name]
    if not expected:
        moved = [q for q in ("literal", "french_main", "codelets", "capped")
                 if _moved(q, intact, lesioned, runs) != 0]
        return "as predicted" if not moved else "moved: " + ", ".join(moved)
    signs = [(_moved(q, intact, lesioned, runs), 1 if d == "up" else -1) for q, d in expected]
    if any(s == -want for s, want in signs if s != 0):
        return "against"
    if all(s == want for s, want in signs):
        return "as predicted"
    return "no change"


def variants(config: Config) -> list:
    """The intact profile, then one variant per lesion (D-073)."""
    out = [P.intact(config)]
    for name, changes, what, expect in LESIONS:
        out.append(P.variant(config, name, f"{what}; expected: {expect}", **changes))
    return out


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    return (1 + len(LESIONS)) * len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> dict:
    """Survey the problems intact and under every lesion [FCCA p. 365] (D-073)."""
    vs = variants(config)
    surveys = P.survey_variants(vs, problems_of(sets, only), seed, runs, progress)
    return {"variants": vs, "problems": problems_of(sets, only), "surveys": surveys}


def report(results: dict) -> list:
    """The bench's text: the lesions, then one table per problem (D-073)."""
    lines = ["## Lesions, FCCA's criterion 11 [FCCA p. 365]", ""]
    lines += P.variants_table(results["variants"])
    for name in results["problems"]:
        lines += P.variant_table(name, results["variants"], results["surveys"])
    return lines


def records(results: dict) -> list:
    """The JSON records (D-073)."""
    return P.variant_records("lesions", results["variants"], results["problems"],
                             results["surveys"])
