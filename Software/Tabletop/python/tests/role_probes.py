"""The test-only probes of the roles (D-104; the roles loop's rule of honesty without an off
switch): a lesion is a patch of one module-level function for the duration of a run, never
an option of the engine, and the oracle probe replaces the perception entry point with roles
hand-read from the layouts. A helper, not a test file: `tests/test_role_consumers.py`,
`tests/test_role_lesions.py` and `tools/role_probes.py` import it. Nothing under `tabletop/`
imports this module or `unittest`.
"""

import contextlib
import json
from pathlib import Path
from unittest import mock

from tabletop import roles as R
from tabletop import temperature as TP
from tabletop.codelets.common import FOUND, Outcome, fizzle

ORACLE = Path(__file__).resolve().parent / "data" / "oracle_roles.json"

# One module-level function per consumer, and what the lesion replaces it with (D-104). A
# consumer that cannot be isolated by patching one function has no row here, and RESULTS.md
# §11.4 says what attribution is lost. Rows are added as the items build the consumers.
LESIONS = {
    "C1": ("tabletop.codelets.scouts.propose_role_cp",
           lambda world, codelet, a: fizzle("lesioned: C1 proposes nothing")),
    "C6": ("tabletop.correspondences.placement_slippages",
           lambda world, cp: []),
    # C5's lesion patches the slippages, not `role_points` as docs/predictions.md §5 wrote:
    # with the slippages gone the points are 0 and the role parallelism goes with them,
    # which is the degradation that row registers; item 13 scores it so.
    "C5": ("tabletop.correspondences.role_slippages",
           lambda world, cp: []),
    # C3: the member with the same roles is never found, so the group-frame words and then
    # the literal object answer (D-058, D-097), and examine-cp offers no anchor pair first.
    "C3": ("tabletop.answer.role_member",
           lambda world, cp, id_: None),
    # C2's lesion leaves pick-salient-obj to salience alone (D-096); K1's empties the
    # re-cast (D-093); L2's reads no column (D-085).
    "C2": ("tabletop.codelets.scouts.recast_weight",
           lambda world, item: 0.0),
    "K1": ("tabletop.roles.recast",
           lambda world: None),
    "L2": ("tabletop.describe.column_role",
           lambda world, item: []),
    # A2 reads no sidekick (D-090; docs/predictions.md §5's row).
    "A2": ("tabletop.describe.sidekicks_of",
           lambda world, item: []),
    # C7's lesion reads no role into salience (D-100): the role term 0 and the stray scale
    # 1; S2, L3, L4 and L5 read no gap, corner, loner or stray (D-086, D-085).
    "C7": ("tabletop.salience.role_reading",
           lambda world, item: (0.0, 1.0)),
    "S2": ("tabletop.describe.gaps_of",
           lambda world, item: []),
    "L3": ("tabletop.describe.corner_role",
           lambda world, item: []),
    "L4": ("tabletop.describe.loner_of",
           lambda world, item: []),
    "L5": ("tabletop.describe.stray_role",
           lambda world, item: []),
    # A4, A5, A6 and A7 read no figure, ground, echo or twin (D-091, D-092); K2 reads no
    # key off the Worldview (D-094).
    "A4": ("tabletop.describe.figure_role",
           lambda world, item: []),
    "A5": ("tabletop.describe.ground_role",
           lambda world, item: []),
    "A6": ("tabletop.correspondences.echoes_of",
           lambda world, item: []),
    "A7": ("tabletop.correspondences.twin_of",
           lambda world, item: []),
    "K2": ("tabletop.worldview.type_keys",
           lambda world, item: []),
    # S5 reads no barrier and no claimed object (D-089: the two share one detector, so
    # one patch lesions both); C4 posts no pick-strong-cp for a barrier's correspondence
    # (D-101; the `blocked` record is a record, read off the CLAIMED tag, so it goes with S5).
    "S5": ("tabletop.worldview.barrier_and_claimed",
           lambda world, item: []),
    "C4": ("tabletop.codelets.judges.barrier_fights",
           lambda world, codelet: []),
    # D1 (the author's answer that roles change at run time; D-079, D-106): the role
    # checker samples and flips its coin exactly as built but never breaks the tag, so the
    # draws match the plain run until the first break would have happened and the runs
    # that then differ are the ones a broken role changed.
    "D1": ("tabletop.codelets.judges.check_roles",
           lambda world, codelet, a: _check_roles_never_breaking(world, codelet, a)),
}


def _check_roles_never_breaking(world, codelet, a):
    """`judges.check_roles` with the discard left out (D-081, D-079): the same rating,
    the same sample, the same coin; the tag stands whatever the coin says."""
    tags = list(world.workspace.roles)
    if not tags:
        return fizzle("no role tag to check")
    for tag in tags:
        R.rate(world, tag)
    weights = TP.softened_weights([101.0 - t.strength for t in tags], a)
    tag = tags[world.rng.weighted_index(weights)]
    TP.passes(R.break_chance(world, tag), a, world.rng)
    return Outcome(FOUND, f"{tag.label()} stands at strength {tag.strength:.0f} (never broken)")


@contextlib.contextmanager
def lesioned(consumer: str):
    """Run with one consumer lesioned (D-104): its module-level function is replaced for
    the duration by a stand-in that does nothing, through `unittest.mock.patch`, so that a
    bench inside the block measures the engine without that consumer and the engine itself
    has no switch. `consumer` is `C1` to `C8`; KeyError for one with no lesion."""
    target, stand_in = LESIONS[consumer]
    with mock.patch(target, stand_in):
        yield


def oracle_tags(problem: str) -> dict:
    """The hand-read roles of a carrier, item to its (role, args, weight, evidence) tuples
    (`tests/data/oracle_roles.json`; RESULTS.md §11.3)."""
    data = json.loads(ORACLE.read_text())
    out: dict = {}
    for row in data[problem]:
        out.setdefault(row["item"], []).append((row["role"], tuple(row["args"]), row["weight"],
                                               row["evidence"]))
    return out


@contextlib.contextmanager
def oracle_roles(problem: str):
    """Run with perfect perception of the roles (D-104; the oracle probe of RESULTS.md
    §11.3): `roles.detect`, the one entry point the engine reads roles through, is replaced
    by the hand-read roles of the carrier (the objects' roles and the table's midline), so
    that every object gets its roles at first sight and the consumers read them as they
    would read perceived ones; the Worldview item (the symmetry axis, D-083) is handed
    back to the engine's own detectors, since it is read off the correspondences, not the
    layout. The gap between this and the engine's own perception is the cost of
    perception."""
    tags = oracle_tags(problem)
    engine_detect = R.detect

    def hand_read(world, item):
        if world.problem.name != problem:
            return []
        if item == R.WORLDVIEW_ITEM:        # the Worldview roles stay the engine's reading
            return engine_detect(world, item)
        return list(tags.get(item, []))

    with mock.patch("tabletop.roles.detect", hand_read):
        yield


def oracle_problems() -> list:
    """The carriers the oracle file reads, in its order."""
    return [k for k in json.loads(ORACLE.read_text()) if not k.startswith("_")]


__all__ = ["LESIONS", "lesioned", "oracle_roles", "oracle_tags", "oracle_problems", "R"]
