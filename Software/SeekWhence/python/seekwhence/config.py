"""The knobs of the reconstruction, in one frozen dataclass.

Every field is commented with the source tag that gives its value or the decision number
(DECISIONS.md) that chose it; a test checks this. Holding every knob in one unchangeable
object, made once per run, makes a run reproducible from its seed and its configuration
alone. The tables (urgencies, s-links, bond values) are tuples of
tuples so that the dataclass stays frozen and hashable; `seekwhence.platoplasm` and
`seekwhence.taskrack` (later items) read them into dictionaries once per run.

The eight ideal types and the task names are spelled as the thesis prints them
([M86 p. 49], [M86 pp. 164-179]).
"""

from dataclasses import dataclass

# The eight non-atomic ideal types, in the thesis's order [M86 pp. 50-53].
IDEAL_TYPES = ("Constant", "Countup", "C-group", "S-group", "P-group", "Y-group", "Cycle", "Tuple")

# Every task type the thesis names or the transcripts print (SPEC.md section 2.4).
TASK_TYPES = (
    "Call-term", "Sparkler", "Sparkler-plus", "Tester", "Bonder", "Glom-scout", "Glomtester",
    "Glommer", "Glom-inspector", "Plato-scout", "Plato-evaluator", "Burster", "Dissolver",
    "Template-scout", "Template-applier", "Template-resolver", "Template-evaluator",
    "Template-reviewer", "Gnoth-maker", "Hypothesizer", "Hfilter", "Gnoth-setter",
    "Reformulator", "Gnoth-operator", "Capper", "Bond-assessor", "Gnoth-caster", "Slip-scout",
    "empty-task",
)


@dataclass(frozen=True)
class Config:
    """The knobs of one run (DECISIONS.md, from D-007 on). Each field is commented with its tag
    `[M86 p. N]` or its decision `D-NNN`; the value in the comment is the source's where there
    is one, otherwise the decision's default."""

    # ---- the taskrack and the run ------------------------------------------------------
    # DECISION D-007: one draw, probability proportional to urgency ** urgency_power.
    urgency_power: int = 1
    # DECISION D-008: the urgency of every task type but Sparkler-plus (D-015) and the
    # Glomtester (D-020); the Glomtester's entry here is its base before weighting.
    urgencies: tuple = (
        ("Call-term", 4), ("Sparkler", 5), ("Tester", 10), ("Bonder", 10), ("Glom-scout", 6),
        ("Glomtester", 1), ("Glommer", 8), ("Glom-inspector", 8), ("Plato-scout", 8),
        ("Plato-evaluator", 6), ("Burster", 3), ("Dissolver", 1), ("Template-scout", 6),
        ("Template-applier", 8), ("Template-resolver", 3), ("Template-evaluator", 8),
        ("Template-reviewer", 8), ("Gnoth-maker", 8), ("Hypothesizer", 8), ("Hfilter", 10),
        ("Gnoth-setter", 10), ("Reformulator", 5), ("Gnoth-operator", 5), ("Capper", 2),
        ("Bond-assessor", 5), ("Gnoth-caster", 5), ("Slip-scout", 5),
    )
    # DECISION D-010: the task types removed when a new term is entered (those created before
    # the previous term), when a Gnoth-setter runs, and when a hypothesis is accepted.
    cleaned_on_term: tuple = ("Sparkler", "Sparkler-plus", "Tester", "Dissolver")
    # DECISION D-010.
    cleaned_on_reform: tuple = ("Reformulator", "Gnoth-operator", "Bond-assessor",
                                "Gnoth-caster", "Slip-scout", "Capper")
    # DECISION D-010.
    cleaned_on_hypothesis: tuple = ("Template-scout", "Template-applier", "Template-evaluator",
                                    "Template-reviewer")
    # DECISION D-011: tasks run between two prompts before the program asks for the next term.
    max_tasks_per_term: int = 2000
    # DECISION D-011: tasks run in the whole run before the outcome is "capped".
    max_tasks: int = 200_000
    # DECISION D-055: print the `doing task` lines (runs 1 and 3) or not (run 2's terse mode).
    verbose: bool = True

    # ---- the cytoplasm ------------------------------------------------------------------
    # DECISION D-015: the urgency printed by a Sparkler-plus: (distinct real elements, a
    # self-pair or a pseudo-glom); the transcripts print 10 and 2 [M86 pp. 164, 170, 172, 176].
    sparkler_plus_urgencies: tuple = (10, 2)
    # DECISION D-016: Testers posted per spark (a "horde" [M86 p. 64]; the runs show one each).
    testers_per_spark: int = 1
    # DECISION D-016: Sparklers released by a bond [M86 p. 65].
    sparklers_per_bond: int = 2
    # DECISION D-016: Glom-scouts released by a bond [M86 p. 65].
    glom_scouts_per_bond: int = 2
    # DECISION D-016: plain Sparklers posted when a term is entered.
    sparklers_per_term: int = 3
    # DECISION D-017: plain Sparklers set out when a term refutes the hypothesis ("releases
    # sparks to encourage low-level activity" [M86 p. 98]).
    sparklers_on_refutation: int = 3
    # DECISION D-019: a bond's own strength is type * modifier * field, rounded down.
    bond_type_weights: tuple = (("Same", 10), ("Succ", 8), ("Pred", 8))
    # DECISION D-019: adjacent above remote [M86 pp. 64-65].
    bond_modifier_weights: tuple = (("adjacent", 1.0), ("remote", 0.5))
    # DECISION D-019: print-value is the only glom-field [M86 p. 65]; span bonds are weak.
    bond_field_weights: tuple = (("print-value", 1.0), ("span", 0.25))
    # DECISION D-019: the least bond strength a Glom-scout or Glomtester follows; span bonds
    # (strength 2) never cause glomming [M86 p. 62].
    glom_bond_minimum: int = 3
    # DECISION D-020: Glomtester urgency = test weight * family weight; "groupable" is
    # strongest and sameness is stronger than successorship [M86 p. 66]; a cover
    # (palindrome) is "somewhat subtler" [FCCA p. 59], so it is the weakest. The Glommer a
    # Glomtester sets out carries the same urgency (DECISION D-021).
    glomtester_test_weights: tuple = (("group", 3), ("fence", 2), ("cover", 1))
    # DECISION D-020.
    glomtester_family_weights: tuple = (("Same", 3), ("pred-succ", 2))

    # ---- templates ----------------------------------------------------------------------
    # DECISION D-061: plain Sparklers set out when a template is accepted ("increased
    # activity in the system" [M86 p. 75]; runs 1 and 2 print two sparks there).
    sparklers_on_acceptance: int = 2

    # ---- the hypothesis and the guess ------------------------------------------------------
    # DECISION D-032: confirming terms before the guess; every transcript guesses after one
    # [M86 pp. 166, 169, 175, 179].
    guess_validity: int = 1
    # DECISION D-033: the guess fires the box from the first term for at least this many hits
    # and until every entered term is covered.
    guess_min_hits: int = 3
    # DECISION D-035: a candidate repeating a freeze-dried form that explained at least as many
    # terms is rejected [M86 p. 91].
    reject_freeze_dried_repeats: bool = True

    # ---- reformulation ------------------------------------------------------------------
    # DECISION D-038: directed s-links with their slipperiness, 0 (non-slippery) to 1
    # (perfectly slippery) [M86 p. 90]; the thesis gives S-group to Countup 0.1 [M86 p. 90],
    # to Tuple 0.4 [M86 p. 91], to Y-group 0.4 [M86 p. 104]; Cycle's neighbours score 0 on a
    # remote sameness bond (`best: nil` [M86 p. 168]); the rest are engineering choices.
    s_links: tuple = (
        ("S-group", "Countup", 0.1), ("S-group", "C-group", 0.3), ("S-group", "Y-group", 0.4),
        ("S-group", "Cycle", 0.3), ("S-group", "Tuple", 0.4),
        ("Countup", "S-group", 0.5), ("Countup", "C-group", 0.3), ("Countup", "Tuple", 0.2),
        ("Constant", "C-group", 0.5), ("Constant", "Countup", 0.3), ("Constant", "Tuple", 0.2),
        ("C-group", "Constant", 0.1), ("C-group", "S-group", 0.3), ("C-group", "Y-group", 0.3),
        ("C-group", "Tuple", 0.4),
        ("P-group", "S-group", 0.3), ("P-group", "Y-group", 0.4), ("P-group", "Tuple", 0.4),
        ("Y-group", "S-group", 0.3), ("Y-group", "C-group", 0.3), ("Y-group", "Tuple", 0.4),
        ("Cycle", "Tuple", 0.4), ("Cycle", "C-group", 0.2),
        ("Tuple", "Cycle", 0.4), ("Tuple", "S-group", 0.2), ("Tuple", "C-group", 0.2),
        ("Tuple", "Y-group", 0.2),
    )
    # DECISION D-039: (class, bond type, modifier, value): a positive value is a pull, a
    # negative one a push, magnitudes 1 to 10 [M86 p. 91]; unlisted combinations are 0. The
    # thesis gives S-group +10 adjacent Succ and -5 remote Same [M86 p. 101], 0 for adjacent
    # Pred [M86 p. 175], and Y-group +10 remote Same [M86 p. 104]; Cycle and Tuple list no
    # bonds (`stayval: 0.0` with a remote sameness bond between two gnoths under Cycle
    # [M86 p. 168]); the rest are engineering choices.
    # A Same bond on the field span is keyed "Same-span" (DECISION D-039).
    bond_values: tuple = (
        ("S-group", "Succ", "adjacent", 10), ("S-group", "Same", "remote", -5),
        ("S-group", "Same", "adjacent", -5),
        ("P-group", "Pred", "adjacent", 10), ("P-group", "Same", "remote", -5),
        ("P-group", "Same", "adjacent", -5),
        ("C-group", "Same", "adjacent", 10), ("C-group", "Same-span", "adjacent", 3),
        ("C-group", "Succ", "adjacent", -5), ("C-group", "Pred", "adjacent", -5),
        ("Y-group", "Same", "remote", 10),
        ("Constant", "Same", "adjacent", 10),
        ("Countup", "Succ", "adjacent", 10),
    )
    # DECISION D-040: a strict reigning class slips at once; "such as Constant" [M86 p. 99],
    # and the transcript slips from Countup [M86 p. 174].
    strict_classes: tuple = ("Constant", "Countup")
    # DECISION D-041: the staying pressure is multiplied by this once per recent try of the
    # reigning class, not counting the hypothesis just refuted [M86 p. 99].
    stay_factor_per_try: float = 0.5
    # [M86 p. 105]: the first threshold is 80% of the strongest pull-bond strength of the
    # reigning type; cleared as DECISION D-046 says.
    threshold_fraction: float = 0.8
    # DECISION D-049: a Slip-scout repeats the slip check with the staying pressure multiplied
    # by this [M86 p. 126].
    slip_scout_factor: float = 0.5
    # DECISION D-050: Gnoth-caster failures in one episode before the blast [M86 p. 92].
    max_cast_failures: int = 3
