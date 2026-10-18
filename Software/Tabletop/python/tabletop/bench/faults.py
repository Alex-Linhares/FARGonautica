"""The `faults` bench: each of French's seven faults restored, one at a time and all
together, against the fixed profile (ANSWERS.md 2; D-073).

The `standard` profile fixes every fault French names in his own program (SPEC.md §6b);
`french-faults` restores all seven (D-066). This bench surveys, beside the fixed profile,
one variant per fault with that fault alone restored (the knob value `FAULT_KNOBS` gives,
D-021, D-026, D-028, D-045, D-046, D-069, D-070) and then all seven together, on the
problems SPEC.md §6b says each fix may move: the chapter 4 configuration [F95 p. 96] for
the group-level bias and the context faults, Blockage 3 and 4 [F95 p. 125] for the
group-to-object bias, a Buridan table with four cups in a row [F95 p. 140] for the kept
subgroups, and Figure/Ground 1 [F95 pp. 141-142] for the touched object's pull. RESULTS.md
reads the tables as the size of each fix.
"""

from tabletop.bench import probes as P
from tabletop.config import FAULT_KNOBS, PROFILES, Config

# The decision and page of each fault knob, in FAULT_KNOBS' order (SPEC.md §6b).
FAULT_NOTES = {
    "keep_all_subgroups": "fault 1: every subgroup kept [F95 p. 140] (D-026)",
    "group_object_penalty": "fault 2: the bias against group-to-object mappings [F95 p. 125] "
                            "(D-045)",
    "group_salience_points": "fault 3: the group-level bias [F95 p. 109] (D-046)",
    "pop_restores_spread": "fault 4: the lossy reconstruction after a pop [F95 p. 89] (D-021)",
    "push_reset_set": "fault 5: the push resets the two symmetry nodes only [F95 p. 88] "
                      "(D-070)",
    "member_salience_in_group_strength": "fault 6: the touched object's salience in a "
                                         "group's strength [F95 pp. 141-142] (D-069)",
    "ends_from_chain": "fault 7: one end object for an amorphous group [F95 p. 63] (D-028)",
}
SETS = ("surround-05", "blockage-03", "blockage-04", "buridan-12", "figure-ground-01")
problems_of = P.problems_of


def variants(config: Config) -> list:
    """The fixed profile, one variant per fault restored alone, then all seven (D-073)."""
    out = [P.variant(config, "fixed", "every fault fixed (ANSWERS.md 2)")]
    for i, (field, value) in enumerate(FAULT_KNOBS, 1):
        out.append(P.variant(config, f"fault-{i}", FAULT_NOTES[field], **{field: value}))
    out.append(P.variant(config, "all-faults", "all seven restored, the `french-faults` "
                         "profile (D-066)", **dict(FAULT_KNOBS)))
    return out


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    return (2 + len(FAULT_KNOBS)) * len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> dict:
    """Survey the problems under the fixed profile, each fault alone and all together
    (D-073). A configuration that is `french-faults` itself is measured as given, with
    the faults restored on top of it (no change)."""
    vs = variants(config)
    surveys = P.survey_variants(vs, problems_of(sets, only), seed, runs, progress)
    return {"variants": vs, "problems": problems_of(sets, only), "surveys": surveys}


def report(results: dict) -> list:
    """The bench's text: the variants, then one table per problem (D-073)."""
    lines = ["## French's faults restored, one at a time and together (ANSWERS.md 2)", ""]
    lines += P.variants_table(results["variants"])
    for name in results["problems"]:
        lines += P.variant_table(name, results["variants"], results["surveys"])
    return lines


def records(results: dict) -> list:
    """The JSON records (D-073)."""
    return P.variant_records("faults", results["variants"], results["problems"],
                             results["surveys"])


def all_faults_is_french_faults(config: Config = PROFILES["standard"]) -> bool:
    """Whether the last variant of `standard` equals the `french-faults` profile but for
    its name (D-066, D-073); a check the tests make."""
    last = variants(config)[-1].config
    return last.differences(PROFILES["french-faults"]) in ({}, {"profile": last.profile})
