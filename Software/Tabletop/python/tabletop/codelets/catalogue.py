"""The catalogue of the codelet types (D-051, D-081): the 23 of the legend printed beside
the first screen dump [F95 p. 96 (img)], in the order of the Coderack strip [F95 p. 107
(img)], each with its job and where the job comes from, and after them the role codelets
of the re-interpretation (D-081), marked as such.

The text names twelve codelets and describes what they do [F95 p. 63; F95 pp. 76-77;
F95 p. 85; F95 p. 94]; the legend gives the full name of every label, and for seven types
the name is all there is: their jobs are inferred from it, and the entry says so
(`job_source` is `inferred`); the role codelets' jobs are the re-interpretation's
(`job_source` is `re-interpretation`, D-081). The bands are D-052's urgency bands: scouts,
searches, builders, the Worldview's codelets, and find-answer, whose urgency the schedule
sets (D-056); "band" and not "role", so that "role" means one thing in the package (D-079).
This module is data; the functions that run the codelets are in the modules beside it.
"""

from dataclasses import dataclass

from tabletop.config import CODELET_TYPES

TEXT = "text"            # the job is described in the text, at the pages cited
INFERRED = "inferred"    # the job is read off the legend's name (D-051)
REINTERPRETATION = "re-interpretation"   # a role codelet, beyond French's program (D-081)

# The bands, in the order of D-052's urgency bands.
SCOUT, SEARCH, BUILDER, WORLDVIEW, ANSWER = "scout", "search", "builder", "worldview", "answer"
BANDS = (SCOUT, SEARCH, BUILDER, WORLDVIEW, ANSWER)


@dataclass(frozen=True)
class CodeletType:
    """One row of the catalogue (D-051, D-081): the type's `name` (the legend's, as
    `Config` and the trace use it), its strip `label` [F95 p. 107 (img)], its `band`
    (D-052's urgency band), its `job` in a sentence, `job_source` (`text`, `inferred` or
    `re-interpretation`) and the `pages` the job or the name comes from."""

    name: str
    label: str
    band: str
    job: str
    job_source: str
    pages: str


def _row(name, band, job, source, pages):
    return CodeletType(name, dict(CODELET_TYPES)[name], band, job, source, pages)


CATALOGUE = (
    _row("find-group", SCOUT,
         "Scan a region for a bunch of objects close together, record it as a gestalt group, "
         "jolt 'group' and post the follow-ups of p. 76.",
         TEXT, "[F95 p. 63; F95 p. 76; F95 p. 97]"),
    _row("simple-sameness-detect", SCOUT,
         "Look for an identical copy of a given object or group elsewhere on the table; "
         "jolt 'sameness' on a find.",
         TEXT, "[F95 p. 63; F95 p. 76]"),
    _row("find-end-objs", SCOUT,
         "Find the end objects of a group just found; jolt 'end' and the end object's type.",
         TEXT, "[F95 p. 63; F95 p. 76]"),
    _row("find-middle-objs", SCOUT,
         "Find the objects in the middle of a group just found; jolt 'middle'.",
         TEXT, "[F95 p. 76; F95 p. 79]"),
    _row("find-sim-nhbr", SCOUT,
         "Look for a neighbour of the touched object (or of its argument) that is "
         "conceptually close to it.",
         INFERRED, "[F95 p. 96 (img)]"),
    _row("find-neighbor", SCOUT,
         "Determine the immediate neighbours of its argument inside its group; jolt "
         "'neighbor'; post a find-neighbor on each neighbour found.",
         TEXT, "[F95 p. 63; F95 p. 76; F95 p. 80]"),
    _row("build-obj-0", BUILDER,
         "The first micro-test of building a group out of two objects (or a group and one "
         "more): the physical distance.",
         TEXT, "[F95 p. 77; FCCA p. 384]"),
    _row("build-obj-1", BUILDER,
         "The second micro-test of building a group: the conceptual distance.",
         TEXT, "[F95 p. 77; FCCA p. 384]"),
    _row("build-obj-2", BUILDER,
         "The last micro-test of building a group: compatibility with the groups already "
         "built; the group is built.",
         TEXT, "[F95 p. 77; FCCA p. 384]"),
    _row("build-obj-desc", BUILDER,
         "Write a built group's descriptors: its kind, its count, its orientation.",
         INFERRED, "[F95 p. 96 (img)]"),
    _row("search-for-sam-gp", SEARCH,
         "Search for other occurrences of a group of the same composition elsewhere on the "
         "table; post create-cp on a find.",
         TEXT, "[F95 p. 101; FCCA p. 386]"),
    _row("search-for-sim-gp", SEARCH,
         "Look for a group of the same kind and size elsewhere on the table; post create-cp "
         "on a find.",
         INFERRED, "[F95 p. 96 (img); FCCA p. 386]"),
    _row("pick-salient-obj", SCOUT,
         "Pick an object (or a group) by salience for further processing, with a bias "
         "toward high salience; post create-cp with it.",
         TEXT, "[F95 p. 65; F95 p. 76]"),
    _row("pick-salient-subgp", SCOUT,
         "Pick a subgroup by salience and post a search for its twin.",
         INFERRED, "[F95 p. 96 (img)]"),
    _row("pick-2nd-salient-obj", SCOUT,
         "Pick an object by salience among those not yet described.",
         INFERRED, "[F95 p. 96 (img); F95 p. 98 (img)]"),
    _row("create-cp", BUILDER,
         "Create a correspondence between two objects or two groups, with its slippages.",
         TEXT, "[F95 p. 71]"),
    _row("pick-strong-cp", WORLDVIEW,
         "Pick a correspondence by its rating for further processing (the text's "
         "pick-salient-correspondence); post cp-compete for a tentative one.",
         TEXT, "[F95 p. 65]"),
    _row("examine-cp", WORLDVIEW,
         "Take a group-level correspondence and look for correspondences between the objects "
         "inside its two groups.",
         TEXT, "[F95 p. 85]"),
    _row("build-cp-descrip", BUILDER,
         "Write a built correspondence's descriptors: its level and its slippages.",
         INFERRED, "[F95 p. 96 (img)]"),
    _row("cp-compete", WORLDVIEW,
         "Hold the fight between a tentative correspondence's Rival Worldview and the "
         "Worldview as it stands.",
         TEXT, "[F95 p. 72; F95 p. 105]"),
    _row("check-worldview", WORLDVIEW,
         "Re-read the Worldview's ratings and post a cp-compete for any tentative "
         "correspondence that now outrates its rival.",
         INFERRED, "[F95 p. 96 (img)]"),
    _row("clean-up-corresp", WORLDVIEW,
         "Remove, probabilistically, a correspondence whose value has dropped below the "
         "threshold.",
         TEXT, "[F95 p. 73]"),
    _row("find-answer", ANSWER,
         "Announce the object corresponding to the touched object and stop the program.",
         TEXT, "[F95 p. 94; F95 p. 105]"),
    # The role codelets (D-081), after the legend's 23.
    _row("propose-role-cp", SCOUT,
         "Consumer C1: choose, by role weight under a(T), one pair that a role relation names "
         "across the midline, and post a create-cp for it into the existing chain.",
         REINTERPRETATION, "[F95 p. 36; F95 p. 131]"),
    _row("check-roles", SCOUT,
         "Re-examine one role tag: re-rate it from its evidence, its node and the Worldview's "
         "support, and break it with a chance that falls with its strength.",
         REINTERPRETATION, "[F95 p. 73; F95 p. 131]"),
    _row("find-roles", SCOUT,
         "Read the single-table roles of the table (the midline) and of one item in view; "
         "post a compare-roles for the item and a propose-role-cp when a tag stands.",
         REINTERPRETATION, "[F95 p. 39; F95 p. 118]"),
    _row("compare-roles", SCOUT,
         "Read the comparison roles of one item against the other row: its mirror and point "
         "images across a perceived midline; post a propose-role-cp when a tag stands.",
         REINTERPRETATION, "[F95 p. 28; F95 p. 66]"),
    _row("read-worldview-roles", WORLDVIEW,
         "Read the roles the Worldview supports: the symmetry axis its correspondences share, "
         "the touched object's own left out; jolt the axis's node.",
         REINTERPRETATION, "[F95 p. 86]"),
    _row("recast-role", SEARCH,
         "Carry the touched object's role signature to the other row in tiers (the image "
         "under the axis, the same column, the same type, the rest); tag the counterparts and "
         "post create-cp for the first tier.",
         REINTERPRETATION, "[F95 p. 6; F95 p. 12]"),
)


def names() -> list:
    """The type names in the strip's order (D-051), the role codelets last (D-081)."""
    return [row.name for row in CATALOGUE]


def entry(name: str) -> CodeletType:
    """The catalogue row of a type name (D-051); KeyError for a name not in the legend."""
    for row in CATALOGUE:
        if row.name == name:
            return row
    raise KeyError(name)


def of_band(band: str) -> list:
    """The types of one band, in the strip's order (D-052's bands)."""
    return [row.name for row in CATALOGUE if row.band == band]


def role_codelets() -> list:
    """The role codelets' names (D-081), in the strip's order."""
    return [row.name for row in CATALOGUE if row.job_source == REINTERPRETATION]
