"""Every knob of the reconstruction in one frozen dataclass, and the named profiles
(DECISIONS.md; the profiles are D-066).

A run takes its configuration as data, never from module state: `run_problem(problem, config,
seed)` builds a fresh world from a `Config` and a seed, so a run is reproducible from those
two alone. Each field is commented with the source tag (`[F95 p. N]`, `[FCCA p. N]`,
`[FH91 p. N]`) that gives its value or the decision (`D-NNN`) that chose it;
`tests/test_conventions.py` and the loop's gate check this. The tables (urgencies, the
node-to-codelet table, the weights) are tuples of pairs so that the dataclass stays frozen and
hashable; the modules that use them read them into dictionaries once per run
(`as_dict`).

From D-079 on the fields are the roles' (always on, D-078): each is validated above zero by
`__post_init__`, since a value that turned a role off would be the switch the author
forbids. `PROFILES` maps a profile name to its `Config`: `standard` (the defaults), `buridan` and
`human` (French's two calibrations, each one knob fitted after the freeze by
`tabletop/bench/calibrate.py`, D-076), and `french-faults` (`standard` with the seven faults
French names in his own program restored, for diagnosis only; ANSWERS.md 2); the three keep
their overrides on top of whatever `standard` is. `config_defaults.lock` (item 25) freezes
all four. Through v2 every default was set a priori, from the text, the screen dumps or the
decision's stated reason, and never tuned. Since v3 (D-114, 2026-10-16) 67 defaults are the
configuration chosen by a cross-validated Bayesian optimization against French's result sets
and the human data (fold 1 trial 244): each changed field says so, with its v2 value, and
D-114 gives the table, the study and the trade-offs. Since v4 (D-115, 2026-10-17) 65 defaults
are the configuration chosen by a second optimization (fold 1 trial 800), chosen by the
author although its cross-validated estimate does not beat v3's: each changed field says so,
with its v3 value (`V3_DEFAULTS`), and D-115 gives the table and the trade-offs. Since v5
(D-116 to D-120, 2026-10-17, the structural Blockage fix) `proximity_zero_gap` is bounded by
French's Blockage 10 (`PROXIMITY_ZERO_GAP_MAX`, refused above it, so v3's and v4's values can
no longer be built as a `Config`), its default the bound, and `answer_patience` is the time
pressure's knob; the other four changes of v5 are mechanisms with no knob of their own.
"""

import dataclasses
from dataclasses import dataclass
from functools import cached_property

# The 23 codelet types, under the names of the legend printed beside the first screen dump
# [F95 p. 96 (img)], in the order of the Coderack strip (D-051), then the role codelets
# appended after find-answer (D-081: propose-role-cp, consumer C1, and check-roles, the
# role checker of the author's answer D1, from item 03; find-roles, compare-roles and
# read-worldview-roles from item 04; recast-role from item 07, D-093). The
# second column is the strip's label, which the trace prints [F95 p. 107 (img)].
CODELET_TYPES = (
    ("find-group", "FND-GP"),
    ("simple-sameness-detect", "SAM"),
    ("find-end-objs", "FD-END"),
    ("find-middle-objs", "FD-MID"),
    ("find-sim-nhbr", "FD-SIMN"),
    ("find-neighbor", "FD-NNBR"),
    ("build-obj-0", "BD-OB0"),
    ("build-obj-1", "BD-OB1"),
    ("build-obj-2", "BD-OB2"),
    ("build-obj-desc", "BD-OB-DSC"),
    ("search-for-sam-gp", "SC-SMGP"),
    ("search-for-sim-gp", "SC-SIMGP"),
    ("pick-salient-obj", "PK-SALO"),
    ("pick-salient-subgp", "PK-SALS"),
    ("pick-2nd-salient-obj", "PK2SALO"),
    ("create-cp", "CT-CP"),
    ("pick-strong-cp", "PK-SALC"),
    ("examine-cp", "EXAM-CP"),
    ("build-cp-descrip", "BLD-CP"),
    ("cp-compete", "CP-COMP"),
    ("check-worldview", "CHK-WV"),
    ("clean-up-corresp", "CP-CLN"),
    ("find-answer", "FND-ANS"),
    ("propose-role-cp", "PR-RCP"),
    ("check-roles", "CHK-RL"),
    ("find-roles", "FND-RL"),
    ("compare-roles", "CMP-RL"),
    ("read-worldview-roles", "RD-WVR"),
    ("recast-role", "RC-RL"),
)
# The role codelets (D-081), the types after find-answer.
ROLE_CODELET_TYPES = tuple(name for name, _ in CODELET_TYPES[23:])
# The two readings of `group_position_match` (D-097; D-058 revised): by the members' roles
# first, or by the group-frame words alone; `answer.py` reads them.
GROUP_POSITION_MATCHES = ("roles", "words")

# The seven knobs that restore French's faults, with their faulty values (ANSWERS.md 2;
# SPEC.md 6b; the FIXED entries of DECISIONS.md). `french-faults` is `standard` with these.
FAULT_KNOBS = (
    ("keep_all_subgroups", True),                 # D-026, fault 1 [F95 p. 140]
    ("group_object_penalty", 30),                 # D-045, fault 2 [F95 p. 125]
    ("group_salience_points", 15),                # D-046, fault 3 [F95 p. 109]
    ("pop_restores_spread", False),               # D-021, fault 4 [F95 p. 89]
    ("push_reset_set", "symmetries"),             # D-070, fault 5 [F95 p. 88]
    ("member_salience_in_group_strength", 0.2),   # D-069, fault 6 [F95 pp. 141-142]
    ("ends_from_chain", False),                   # D-028, fault 7 [F95 p. 63]
)

# The one field each calibrated profile may change (ANSWERS.md 7; D-066).
CALIBRATION_KNOBS = (
    ("buridan", "incoherence_penalty"),           # D-042 [F95 p. 130]
    ("human", "type_weight"),                     # D-036 [F95 pp. 142-143]
)

# DECISION D-118: the bound on `proximity_zero_gap`, in small-glass widths: Blockage 10's
# glasses, 2.64 widths apart on the digitized figure (D-003), are the pair French says "the
# program no longer perceives" as a group [F95 p. 129], so no value may link them;
# `__post_init__` refuses a larger value, as it refuses a role knob at zero (D-078).
PROXIMITY_ZERO_GAP_MAX = 2.6

# DECISION D-114: the 67 defaults v3 changed, with their v2 values (the a-priori defaults of
# D-004 to D-100), and the one more field v4 changed (D-115). The unit tests of those
# mechanisms check them at these values, through
# `dataclasses.replace(Config(), **dict(V2_DEFAULTS))`; the engine never reads this table.
V2_DEFAULTS = (
    ('region_blur_width', 4.0),
    ('region_blur_max', 0.5),
    ('depth_by_level', (20, 50, 80)),
    ('label_shrink', 0.6),
    ('max_path_links', 2),
    ('spread_constant', 0.5),
    ('spread_radius', 100.0),
    ('decay_rate', 0.1),
    ('jolt_structure', 40),
    ('jolt_description', 20),
    ('jolt_label', 20),
    ('initial_opposite', 139),
    ('initial_touched_type', 51),
    ('focus_jolt', 20),
    ('label_trickle', 0.05),
    ('label_trickle_threshold', 50),
    ('top_down_rise', 40.0),
    ('top_down_threshold', 100),
    ('pop_fixed_amount', 30),
    ('push_reset_set', 'slippage-concepts'),
    ('proximity_full_gap', 0.5),
    ('proximity_zero_gap', 2.5),
    ('group_stage_pass', (('sameness', 0.95), ('shared-isa', 0.6), ('mixed', 0.1))),
    ('shared_isa_max_length', 30),
    ('group_strength_weights', (('compactness', 20), ('size', 10), ('uniformity', 40),
                                ('gap', 20), ('similar_group', 10))),
    ('member_salience_in_group_strength', 0.0),
    ('salience_weights', (('type_activation', 0.3), ('proximity', 30), ('end', 10),
                          ('middle', 10), ('region', 40), ('in_worldview', 15), ('win', 5),
                          ('loss', 5), ('sameness_group', 10), ('shared_isa_group', 5),
                          ('size', 2), ('role', 5))),
    ('middle_over_end_factor', 2.0),
    ('group_salience_points', 10),
    ('region_preference', (('diagonal', 1.0), ('opposite', 0.7), ('near', 0.4), ('other', 0.15))),
    ('proximity_reach', 40.0),
    ('cp_static_weights', (('salience', 0.2), ('kind', 15), ('kind_active', 5),
                           ('end_middle', 10), ('position', 10), ('number', 10), ('isa', 10),
                           ('same_members', 10), ('orientation', 5), ('orientation_flip', 2),
                           ('role', 10))),
    ('type_weight', 30),
    ('group_object_penalty', 15),
    ('cp_dynamic_points', (('hobj', 15), ('hobj_neighbor', 10), ('worldview_neighbor', 5))),
    ('win_bonus', 5),
    ('loss_penalty', 8),
    ('removal_threshold', 20),
    ('incoherence_penalty', 40),
    ('coherence_reward', 10),
    ('parallel_points', 5),
    ('temperature_alpha', 600.0),
    ('temperature_beta', 4.0),
    ('temperature_gamma', 0.5),
    ('dq_window', 6),
    ('wv_window', 10),
    ('initial_temperature', 80.0),
    ('q_floor', 0.05),
    ('urgencies', (('find-group', 20), ('simple-sameness-detect', 20), ('find-end-objs', 20),
                   ('find-middle-objs', 20), ('find-sim-nhbr', 20), ('find-neighbor', 20),
                   ('build-obj-0', 40), ('build-obj-1', 40), ('build-obj-2', 40),
                   ('build-obj-desc', 40), ('search-for-sam-gp', 30), ('search-for-sim-gp', 30),
                   ('pick-salient-obj', 20), ('pick-salient-subgp', 20),
                   ('pick-2nd-salient-obj', 20), ('create-cp', 100), ('pick-strong-cp', 50),
                   ('examine-cp', 50), ('build-cp-descrip', 100), ('cp-compete', 100),
                   ('check-worldview', 50), ('clean-up-corresp', 50), ('find-answer', 0),
                   ('propose-role-cp', 60), ('check-roles', 20), ('find-roles', 30),
                   ('compare-roles', 30), ('read-worldview-roles', 50), ('recast-role', 40))),
    ('urgency_floor', 0.01),
    ('rho', 3.0),
    ('exponent_at_zero', 6.0),
    ('exponent_at_hundred', 0.1),
    ('max_codelet_age', 60),
    ('coderack_capacity', 150),
    ('initial_codelets', (('find-group', 20), ('pick-2nd-salient-obj', 2),
                          ('pick-salient-obj', 1), ('pick-strong-cp', 1), ('find-roles', 1))),
    ('answer_post_rate', 10.0),
    ('answer_post_power', 3.0),
    ('answer_urgency_max', 100.0),
    ('answer_urgency_power', 3.0),
    ('role_weight_points', 10.0),
    ('role_node_points', 30.0),
    ('role_story_points', 20.0),
    ('role_break_scale', 100.0),
    ('role_pair_floor', 1.0),
    ('role_recast_tiers', (8.0, 4.0, 2.0, 1.0, 0.5)),
    ('stray_salience_scale', 0.5),
    # DECISION D-115: v4 changed one field v3 had left at its v2 value (D-050); its v2 value
    # is here so that the table still gives v2 exactly.
    ('temperature_update_every', 5),
)

# DECISION D-115: the 65 defaults v4 changed, with their v3 values (fold 1 trial 244 of the
# first study, D-114). The tests that read a run's facts at the v3 defaults, and the bench
# records of v3 (`results/v3/`), use `dataclasses.replace(Config(), **dict(V3_DEFAULTS))`;
# the engine never reads this table.
V3_DEFAULTS = (
    ('region_blur_width', 2.2008565169053895),
    ('region_blur_max', 0.05778812116655216),
    ('depth_by_level', (45, 75, 45)),
    ('label_shrink', 0.19119336863601216),
    ('spread_constant', 0.19763673318675257),
    ('spread_radius', 89.08621192822841),
    ('decay_rate', 0.10537367710707346),
    ('jolt_structure', 11),
    ('jolt_description', 14),
    ('jolt_label', 9),
    ('initial_opposite', 68),
    ('initial_touched_type', 147),
    ('focus_jolt', 9),
    ('label_trickle', 0.024959712778966317),
    ('label_trickle_threshold', 32),
    ('top_down_rise', 10.568496687557131),
    ('top_down_threshold', 95),
    ('pop_fixed_amount', 56),
    ('proximity_full_gap', 0.1357240183058595),
    ('proximity_zero_gap', 4.935543682926514),
    ('group_stage_pass', (('sameness', 0.8106116908601358), ('shared-isa', 0.15809167139364852),
                          ('mixed', 0.018026748992329592))),
    ('shared_isa_max_length', 41),
    ('group_strength_weights', (('compactness', 26), ('size', 4), ('uniformity', 34), ('gap', 7),
                                ('similar_group', 3))),
    ('member_salience_in_group_strength', 0.20608913143316826),
    ('salience_weights', (('type_activation', 0.21750280081954354), ('proximity', 10), ('end', 2),
                          ('middle', 22), ('region', 14), ('in_worldview', 54), ('win', 6),
                          ('loss', 5), ('sameness_group', 9), ('shared_isa_group', 1), ('size', 4),
                          ('role', 1))),
    ('middle_over_end_factor', 4.054227501839888),
    ('group_salience_points', 29),
    ('region_preference', (('diagonal', 0.6894932752778693), ('opposite', 0.5034417243551861),
                           ('near', 0.10762862678377479), ('other', 0.41962064233625845))),
    ('proximity_reach', 15.68728895366461),
    ('cp_static_weights', (('salience', 0.06958464716207678), ('kind', 24), ('kind_active', 1),
                           ('end_middle', 3), ('position', 5), ('number', 3), ('isa', 2),
                           ('same_members', 24), ('orientation', 13), ('orientation_flip', 5),
                           ('role', 19))),
    ('type_weight', 26),
    ('group_object_penalty', 45),
    ('cp_dynamic_points', (('hobj', 11), ('hobj_neighbor', 6), ('worldview_neighbor', 8))),
    ('loss_penalty', 4),
    ('removal_threshold', 75),
    ('incoherence_penalty', 27),
    ('coherence_reward', 5),
    ('parallel_points', 16),
    ('temperature_alpha', 304.28518892626295),
    ('temperature_beta', 15.522158275420585),
    ('temperature_gamma', 0.8149287013356975),
    ('dq_window', 14),
    ('wv_window', 18),
    ('initial_temperature', 70.6525869148504),
    ('q_floor', 0.0383729024917592),
    ('temperature_update_every', 5),
    ('urgencies', (('find-group', 21), ('simple-sameness-detect', 12), ('find-end-objs', 5),
                   ('find-middle-objs', 22), ('find-sim-nhbr', 17), ('find-neighbor', 19),
                   ('build-obj-0', 70), ('build-obj-1', 13), ('build-obj-2', 25),
                   ('build-obj-desc', 64), ('search-for-sam-gp', 50), ('search-for-sim-gp', 19),
                   ('pick-salient-obj', 12), ('pick-salient-subgp', 76),
                   ('pick-2nd-salient-obj', 7), ('create-cp', 58), ('pick-strong-cp', 14),
                   ('examine-cp', 68), ('build-cp-descrip', 95), ('cp-compete', 99),
                   ('check-worldview', 34), ('clean-up-corresp', 20), ('find-answer', 0),
                   ('propose-role-cp', 89), ('check-roles', 16), ('find-roles', 40),
                   ('compare-roles', 14), ('read-worldview-roles', 15), ('recast-role', 35))),
    ('urgency_floor', 0.03709378761755724),
    ('rho', 1.5756786574636277),
    ('exponent_at_zero', 6.699341892946163),
    ('exponent_at_hundred', 0.21261646315192564),
    ('max_codelet_age', 216),
    ('coderack_capacity', 179),
    ('initial_codelets', (('find-group', 48), ('pick-2nd-salient-obj', 3), ('pick-salient-obj', 3),
                          ('pick-strong-cp', 3), ('find-roles', 3))),
    ('answer_post_rate', 1.0933454350080711),
    ('answer_post_power', 7.795829030201836),
    ('answer_urgency_max', 27.26120591316952),
    ('answer_urgency_power', 1.6674835999126352),
    ('role_weight_points', 6.3640660568260445),
    ('role_node_points', 14.092581291430136),
    ('role_story_points', 68.31619729057013),
    ('role_break_scale', 261.0167640762452),
    ('role_pair_floor', 2.392482515436875),
    ('role_recast_tiers', (11.85241303266348, 1.90191195551275, 5.824314877208685,
                           0.3522396443338284, 1.8269391338241927)),
    ('stray_salience_scale', 1.1999777021291365),
)


@dataclass(frozen=True)
class Config:
    """The knobs of one run (DECISIONS.md D-004 to D-070), frozen so that a run cannot
    change them. Each field is commented with its tag or its decision; the value in the
    comment is the source's where there is one, otherwise the decision's default."""

    # ---- the profile (D-066) --------------------------------------------------------------
    # DECISION D-066: the profile's name, recorded by every run and trace.
    profile: str = "standard"

    # ---- the table and its regions (D-004 to D-006) ---------------------------------------
    # DECISION D-004: the distance (units) within which a region boundary blurs; the
    # probability of being put on the far side is region_blur_max * exp(-d / width).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 4.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 2.2008565169053895.
    region_blur_width: float = 1.7288679467971633
    # DECISION D-004: the probability of crossing a boundary when standing on it.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.5.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.05778812116655216.
    region_blur_max: float = 0.07180613455879903

    # ---- the Slipnet (D-008 to D-021, D-070) ----------------------------------------------
    # DECISION D-008: the conceptual depth of levels 1, 2 and 3 [F95 pp. 61-62].
    # DECISION D-114 (v3, BO fold 1 trial 244): was (20, 50, 80).
    # DECISION D-115 (v4, second BO fold 1 trial 800): was (45, 75, 45).
    depth_by_level: tuple = (45, 77, 79)
    # DECISION D-011: a labeled link's length is scaled by (1 - label_shrink * a_label / 150).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.6.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.19119336863601216.
    label_shrink: float = 0.1863732972649799
    # DECISION D-012: paths of at most this many links enter the parallel-path distance.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 2.
    max_path_links: int = 3
    # DECISION D-013: node A sends spread_constant * (a_A - a_B) / d(A, B) to each cooler
    # B within spread_radius, the total capped at a_A - 1 [F95 pp. 59-61].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.5.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.19763673318675257.
    spread_constant: float = 0.2681677074771584
    # DECISION D-013: nodes farther apart than this receive no spread.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 100.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 89.08621192822841.
    spread_radius: float = 139.3105665668641
    # DECISION D-014: a <- a - (a - 1) * (100 - depth) / 100 * decay_rate per update.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.1.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.10537367710707346.
    decay_rate: float = 0.12024525799397957
    # DECISION D-016: the jolt when a structure is found or built [F95 p. 97].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 40.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 11.
    jolt_structure: int = 14
    # DECISION D-016: the jolt when a descriptor is noticed.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 20.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 14.
    jolt_description: int = 19
    # DECISION D-016: the jolt a slippage sends to its label node [F95 p. 62].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 20.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 9.
    jolt_label: int = 6
    # DECISION D-017: the activation of "opposite" at the start [F95 p. 96 (img)].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 139.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 68.
    initial_opposite: int = 76
    # DECISION D-017: the activation of the touched object's type at the start [F95 p. 96 (img)].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 51.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 147.
    initial_touched_type: int = 117
    # The dumps hold the touched object's type and "opposite" near the ceiling for the whole
    # run (cup 127-130, opposite 137-150 through 55 codelets [F95 pp. 98-107 (img)]):
    # DECISION D-017: the jolt each receives before every Slipnet update (0 restores item
    # 12's reading, in which they decayed; item 24).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 20.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 9.
    focus_jolt: int = 15
    # DECISION D-018: the share of the mean activation of a label's two concepts that the
    # label receives per update when both are above the threshold [F95 p. 62].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.05.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.024959712778966317.
    label_trickle: float = 0.01985159244084763
    # DECISION D-018: "active", for the trickle.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 50.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 32.
    label_trickle_threshold: int = 30
    # DECISION D-019: codelets between Slipnet updates (a "cycle" [F95 p. 62]).
    slipnet_update_every: int = 1
    # DECISION D-020: which codelet type an active node posts [F95 pp. 63-64; F95 p. 97].
    top_down_table: tuple = (
        ("group", "find-group"),
        ("neighbor", "find-neighbor"),
        ("end", "find-end-objs"),
        ("middle", "find-middle-objs"),
        ("sameness", "simple-sameness-detect"),
        ("cup", "pick-salient-obj"), ("saucer", "pick-salient-obj"),
        ("plate", "pick-salient-obj"), ("knife", "pick-salient-obj"),
        ("fork", "pick-salient-obj"), ("spoon", "pick-salient-obj"),
        ("saltshaker", "pick-salient-obj"), ("pepper shaker", "pick-salient-obj"),
        ("small glass", "pick-salient-obj"), ("big glass", "pick-salient-obj"),
        ("soup bowl", "pick-salient-obj"),
        ("mirror symmetry", "pick-strong-cp"),
        ("diagonal symmetry", "pick-strong-cp"),
        ("opposite", "pick-strong-cp"),
        ("isolated", "pick-2nd-salient-obj"),
        # DECISION D-081: the role nodes post consumer C1 at generation 0 (D-054), and
        # `role` posts the checker; the rows for find-roles, read-worldview-roles and
        # recast-role came with their items.
        ("sidekick", "propose-role-cp"), ("anchor", "propose-role-cp"),
        ("connector", "propose-role-cp"), ("container", "propose-role-cp"),
        ("echo", "propose-role-cp"), ("figure", "propose-role-cp"),
        ("ground", "propose-role-cp"), ("corner", "propose-role-cp"),
        ("key", "propose-role-cp"),
        ("role", "check-roles"),
        # DECISION D-081 (item 04): `separator` posts find-roles, `symmetry` posts
        # read-worldview-roles (D-083).
        ("separator", "find-roles"), ("symmetry", "read-worldview-roles"),
        # DECISION D-081 (item 07): `counterpart`, `row` and `column` post recast-role
        # (D-093), at generation 0.
        ("counterpart", "recast-role"), ("row", "recast-role"), ("column", "recast-role"),
    )
    # DECISION D-020: a node posts rise / top_down_rise codelets per update, where rise is
    # how much its activation grew since the last posting (the integer part, the rest with
    # one coin), "as the rate of group-finding falls (i.e., the rate of change of
    # activation of 'group' decreases) ... fewer codelets will be sent out" [F95 p. 64]; 40
    # is one structure jolt (D-016), so one find posts one more looker.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 40.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 10.568496687557131.
    top_down_rise: float = 14.793932955471826
    # DECISION D-036: "active", for the kind factor of a correspondence [F95 p. 68]: the
    # level a node must reach (two thirds of the ceiling [F95 p. 95]).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 100.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 95.
    top_down_threshold: int = 105
    # DECISION D-021: the amount a pop puts into each concept of a slippage it finds
    # [F95 pp. 88-89].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 30.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 56.
    pop_fixed_amount: int = 98
    # DECISION D-021, FIXED (ANSWERS 2): the pop also restores one spreading step from the
    # neighbours [F95 p. 89]; False is French's lossy pop (`french-faults`).
    pop_restores_spread: bool = True
    # DECISION D-021: the context push and pop can be switched off (a lesion [FCCA p. 365]).
    context_mechanism: bool = True
    # DECISION D-070, FIXED (ANSWERS 2): "slippage-concepts" resets every node named by the
    # correspondence's supporting slippages [F95 p. 88]; "symmetries" only the two symmetry
    # nodes (`french-faults`).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 'slippage-concepts'.
    push_reset_set: str = 'symmetries'

    # ---- perception, groups and salience (D-022 to D-034, D-046, D-069) -------------------
    # DECISION D-022: the gap (in small-glass widths) up to which two objects always link.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.5.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.1357240183058595.
    proximity_full_gap: float = 0.1075377865053709
    # DECISION D-022: the gap at which the link probability reaches 0 [F95 pp. 128-129].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 2.5.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 4.935543682926514.
    # DECISION D-118 (v5, the structural Blockage fix): was 5.478639330528657, which linked
    # Blockage 10's glasses half the time; now the bound `PROXIMITY_ZERO_GAP_MAX`, the largest
    # value allowed, pinned by French's words on Blockage 10 [F95 p. 129].
    proximity_zero_gap: float = PROXIMITY_ZERO_GAP_MAX
    # DECISION D-024: the chance of the conceptual micro-test by the group's kind
    # [F95 p. 65; FCCA p. 384].
    # DECISION D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons
    # the comments here give) are in D-114's table.
    # DECISION D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are
    # `V3_DEFAULTS`' and D-115's table.
    group_stage_pass: tuple = (("sameness", 0.7619120836473591),
                               ("shared-isa", 0.19797909282992776), ("mixed", 0.01552255806249547))
    # DECISION D-024: an ISA link of intrinsic length at most this joins a type to a category
    # that can make a shared-ISA group [F95 p. 65].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 30.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 41.
    shared_isa_max_length: int = 22
    # DECISION D-025: the five factors of a group's strength [FCCA pp. 382-383].
    # DECISION D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons
    # the comments here give) are in D-114's table.
    # DECISION D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are
    # `V3_DEFAULTS`' and D-115's table.
    group_strength_weights: tuple = (("compactness", 57), ("size", 3), ("uniformity", 36),
                                     ("gap", 6), ("similar_group", 4))
    # DECISION D-069, FIXED (ANSWERS 2): the share of the members' mean salience added to a
    # group's strength and stage chance; 0.2 is French's fault [F95 pp. 141-142].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.20608913143316826.
    member_salience_in_group_strength: float = 0.22218177813790996
    # DECISION D-026, FIXED (ANSWERS 2): True keeps every intermediate subgroup [F95 p. 140].
    keep_all_subgroups: bool = False
    # DECISION D-028, FIXED (ANSWERS 2): ends from the chain of neighbours; False takes the
    # extremes along the axis, one end per arm of an L [F95 p. 63].
    ends_from_chain: bool = True
    # DECISION D-031: the salience factors [F95 pp. 66-67] (the group points are D-046).
    # DECISION D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons
    # the comments here give) are in D-114's table.
    # DECISION D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are
    # `V3_DEFAULTS`' and D-115's table.
    salience_weights: tuple = (
        ("type_activation", 0.4381217734219883), ("proximity", 14), ("end", 4), ("middle", 41),
        ("region", 15), ("in_worldview", 24), ("win", 5), ("loss", 2),
        ("sameness_group", 10), ("shared_isa_group", 1), ("size", 2),
        # DECISION D-100: the ninth term, consumer C7 of the roles: the points per unit of
        # weight of an ANCHOR, CONNECTOR or FIGURE tag on the item; refused at zero.
        ("role", 2),
    )
    # DECISION D-031: "middle" must be at least this many times as active as "end" before
    # the middles of a group take the position points instead of its ends, "unless, of
    # course, the concept 'middle' is much more active" [F95 p. 66].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 2.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 4.054227501839888.
    middle_over_end_factor: float = 2.7850438681534455
    # DECISION D-046, FIXED (ANSWERS 2): the salience points for being a group [F95 p. 66];
    # 15 is the bias French calls "a bit too strong" [F95 p. 109].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 10.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 29.
    group_salience_points: int = 28
    # DECISION D-032: the preference of each region relative to the touched object's
    # [F95 p. 66; F95 p. 93].
    # DECISION D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons
    # the comments here give) are in D-114's table.
    # DECISION D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are
    # `V3_DEFAULTS`' and D-115's table.
    region_preference: tuple = (("diagonal", 0.6996372593354234), ("opposite", 0.788168830602624),
                                ("near", 0.09341379798490185), ("other", 0.5772725626840423))
    # DECISION D-033: the distance (units) beyond which the proximity term is 0 [F95 p. 66].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 40.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 15.68728895366461.
    proximity_reach: float = 11.115035683405221

    # ---- correspondences and the Worldview (D-036 to D-046) -------------------------------
    # DECISION D-036: the static factors [F95 pp. 68-69], but the type weight below.
    # DECISION D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons
    # the comments here give) are in D-114's table.
    # DECISION D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are
    # `V3_DEFAULTS`' and D-115's table.
    cp_static_weights: tuple = (
        ("salience", 0.06996111716741335), ("kind", 15), ("kind_active", 1), ("end_middle", 4),
        ("position", 9), ("number", 3), ("isa", 3), ("same_members", 21),
        ("orientation", 14), ("orientation_flip", 8),
        # DECISION D-098: the tenth factor, consumer C5 of the roles: the points per role
        # the two ends share, times the role node's activation over 150; refused at zero.
        ("role", 21),
    )
    # DECISION D-036: the points for conceptually close object types, the weight French
    # raised for his second runs [F95 pp. 142-143]; the `human` profile's knob (ANSWERS.md 7).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 30.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 26.
    type_weight: int = 34
    # DECISION D-045, FIXED (ANSWERS 2): the static points off a group-to-object
    # correspondence; 30 is the bias French says "should probably be relaxed" [F95 p. 125].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 15.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 45.
    group_object_penalty: int = 38
    # DECISION D-037: the dynamic factors [F95 p. 69].
    # DECISION D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons
    # the comments here give) are in D-114's table.
    # DECISION D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are
    # `V3_DEFAULTS`' and D-115's table.
    cp_dynamic_points: tuple = (("hobj", 10), ("hobj_neighbor", 6), ("worldview_neighbor", 7))
    # DECISION D-038: the bonus to every correspondence of a winning Worldview [F95 p. 72].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 5.
    win_bonus: int = 3
    # DECISION D-038: the penalty to the losing side [F95 p. 72].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 8.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 4.
    loss_penalty: int = 6
    # DECISION D-041: a non-Worldview correspondence below this rating may be removed
    # [F95 p. 73].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 20.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 75.
    removal_threshold: int = 126
    # DECISION D-041: True removes a loser at once, Copycat style (a lesion [FCCA p. 365]).
    losers_destroyed: bool = False
    # DECISION D-042: the penalty per incoherent pair in the Worldview rating [F95 p. 74;
    # FCCA p. 387]; the `buridan` profile's knob [F95 p. 130] (ANSWERS.md 7).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 40.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 27.
    incoherence_penalty: int = 24
    # DECISION D-042: the reward per coherent pair [FCCA p. 387].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 10.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 5.
    coherence_reward: int = 6
    # DECISION D-043: the points per pair of parallel correspondences [F95 p. 74].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 5.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 16.
    parallel_points: int = 6

    # ---- temperature (D-047 to D-050) -----------------------------------------------------
    # DECISION D-047: alpha in T = alpha * dQ/Q + beta * dWV - gamma * Tcp [F95 p. 91 (img)];
    # 100 per unit of the running average's weight 1 / dq_window, so that one change of the
    # whole of Q alone fills the thermometer.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 600.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 304.28518892626295.
    temperature_alpha: float = 634.263334931482
    # DECISION D-047: beta.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 4.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 15.522158275420585.
    temperature_beta: float = 8.825853589620962
    # DECISION D-047: gamma.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.5.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.8149287013356975.
    temperature_gamma: float = 0.9063537188894105
    # DECISION D-047: a value >= 0 clamps T to it for the whole run (a lesion [FCCA p. 365]).
    temperature_fixed: float = -1.0
    # DECISION D-048: signed differences of Q (False: absolute) [F95 p. 90].
    dq_signed: bool = False
    # DECISION D-048: each update's rate is "averaged in with" the past rates [F95 p. 90]:
    # dQ <- dQ + (rate - dQ) / dq_window, a running average with the weight of six.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 6.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 14.
    dq_window: int = 18
    # DECISION D-048: changes of the Worldview in the last ten competitions [F95 p. 91].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 10.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 18.
    wv_window: int = 11
    # DECISION D-049: T before the first Worldview change [F95 p. 98 (img)].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 80.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 70.6525869148504.
    initial_temperature: float = 81.86379423180182
    # DECISION D-049: the floor of Q, the Worldview's mean quality as a fraction of 1.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.05.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.0383729024917592.
    q_floor: float = 0.04082205415935979
    # DECISION D-050: codelets between temperature updates when nothing changed.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 5.
    temperature_update_every: int = 3

    # ---- the Coderack (D-052 to D-056) ----------------------------------------------------
    # The correspondence chain (create-cp, build-cp-descrip, cp-compete) is at the top of
    # the range, so that after the generation discount a found pair is pursued ahead of the
    # scouts, as the dumps show (no CT-CP or BLD-CP bar in nine dumps while correspondences
    # are built and enter within ten codelets [F95 pp. 98-107 (img)]; item 24).
    # DECISION D-052: the raw urgency of each codelet type, in 1-100 [F95 p. 81];
    # find-answer's is set at posting (D-056).
    # DECISION D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons
    # the comments here give) are in D-114's table.
    # DECISION D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are
    # `V3_DEFAULTS`' and D-115's table.
    urgencies: tuple = (
        ("find-group", 21), ("simple-sameness-detect", 13), ("find-end-objs", 2),
        ("find-middle-objs", 31), ("find-sim-nhbr", 12), ("find-neighbor", 33),
        ("build-obj-0", 68), ("build-obj-1", 8), ("build-obj-2", 19), ("build-obj-desc", 69),
        ("search-for-sam-gp", 61), ("search-for-sim-gp", 66), ("pick-salient-obj", 18),
        ("pick-salient-subgp", 90), ("pick-2nd-salient-obj", 3), ("create-cp", 57),
        ("pick-strong-cp", 17), ("examine-cp", 67), ("build-cp-descrip", 92),
        ("cp-compete", 99), ("check-worldview", 38), ("clean-up-corresp", 14),
        ("find-answer", 0),
        # DECISION D-081: propose-role-cp at 60, between the scouts (20-30) and the
        # correspondence chain (100), so that a role's pair is pursued ahead of the scouts;
        # check-roles at the scouts' 20, "at low urgency" (the author's answer D1).
        ("propose-role-cp", 97), ("check-roles", 19),
        # DECISION D-081 (item 04): the two role scouts at 30, with the searches, and
        # read-worldview-roles at 50 with the Worldview's codelets.
        ("find-roles", 43), ("compare-roles", 34), ("read-worldview-roles", 31),
        # DECISION D-081 (item 07): recast-role at the searches' 40 (D-093).
        ("recast-role", 40),
    )
    # DECISION D-052: the floor of an adjusted urgency raw / rho ** generation.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.01.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.03709378761755724.
    urgency_floor: float = 0.040642816704546704
    # The urgency-reduction coefficient rho [F95 p. 81].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 3.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 1.5756786574636277.
    rho: float = 1.5329834865509773
    # The temperature exponent a at T = 0 [F95 p. 92] (D-060: a(T) is linear between them).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 6.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 6.699341892946163.
    exponent_at_zero: float = 7.434572541903903
    # The temperature exponent a at T = 100 [F95 p. 92].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.1.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 0.21261646315192564.
    exponent_at_hundred: float = 0.13592262854844722
    # DECISION D-053: a codelet older than this (in codelets run) is removed [F95 p. 76].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 60.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 216.
    max_codelet_age: int = 196
    # The Coderack's capacity; the oldest are pushed off [F95 p. 82].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 150.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 179.
    coderack_capacity: int = 260
    # DECISION D-055: the starting codelets [F95 p. 65; F95 p. 98 (img)]; one find-roles
    # since item 04 of the roles loop (D-081, D-084: the midline is read early).
    # DECISION D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons
    # the comments here give) are in D-114's table.
    # DECISION D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are
    # `V3_DEFAULTS`' and D-115's table.
    initial_codelets: tuple = (("find-group", 56), ("pick-2nd-salient-obj", 3),
                               ("pick-salient-obj", 2), ("pick-strong-cp", 3), ("find-roles", 4))
    # DECISION D-039: a correspondence that fails its initial test is removed from the
    # Workspace, to be proposed again by a later pick (True), or stays tentative for a
    # pick-strong-cp to test again (False, item 17's reading) (item 24).
    failed_test_removes: bool = True
    # DECISION D-056: answer_post_rate * ((100 - T) / 100) ** answer_post_power find-answer
    # codelets are posted after each codelet (the integer part, the rest with one coin)
    # [F95 p. 94]: none at a high temperature, a flood at a low one [F95 pp. 98-107 (img)].
    # DECISION D-114 (v3, BO fold 1 trial 244): was 10.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 1.0933454350080711.
    answer_post_rate: float = 1.163287752380892
    # DECISION D-056: the power of the posting law.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 3.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 7.795829030201836.
    answer_post_power: float = 6.6307295348989665
    # DECISION D-056: its raw urgency is answer_urgency_max * ((100 - T) / 100) ** power,
    # at least 1 [F95 p. 105]: negligible at high temperature, the top of French's range
    # [F95 p. 81] at T = 0.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 100.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 27.26120591316952.
    answer_urgency_max: float = 21.381195132739556
    # DECISION D-056: the power of the urgency law.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 3.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 1.6674835999126352.
    answer_urgency_power: float = 2.860258435717891
    # DECISION D-120 (v5): the time pressure's patience, in codelets: while the Worldview
    # has changed within the last answer_patience codelets (or, before any change, the run
    # is younger than that) the two laws above read T as it is; past it they read T x (1 -
    # pressure), the pressure rising from 0 to 1 over the next answer_patience codelets, so
    # that a table where no structure can form still ends with the default answer [F95
    # p. 105] well before the cap (D-061), under "a competing pressure - time pressure"
    # [FCCA p. 387]. Above every idle stretch measured on the 35 result sets (RESULTS.md,
    # v5): runs where structure forms are unchanged. Refused at zero or below.
    answer_patience: int = 1000

    # ---- the answer and the run (D-058, D-061) --------------------------------------------
    # DECISION D-097 (D-058 revised): the answer when the touched object is mapped only
    # through its group: "roles" (consumer C3: the member with the same roles, then the same
    # group-frame words, then the literal object) or "words" (the words, then the literal).
    group_position_match: str = "roles"
    # DECISION D-061: the run cap, in codelets; a capped run answers as it stands.
    max_codelets: int = 3000

    # ---- the roles (D-079 on); every value below zero or at zero is refused -------------
    # DECISION D-079: a tag's strength takes these points per unit of its weight (the
    # objects in the item carrying the role).
    # DECISION D-114 (v3, BO fold 1 trial 244): was 10.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 6.3640660568260445.
    role_weight_points: float = 4.995662058779633
    # DECISION D-079: the points a tag takes from its node's activation over 150.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 30.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 14.092581291430136.
    role_node_points: float = 11.875393095617262
    # DECISION D-079: the points per Worldview correspondence with the tag's item, or an
    # item it names, at an end: the story's support.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 20.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 68.31619729057013.
    role_story_points: float = 74.22679372210203
    # DECISION D-079: a re-examined tag breaks with chance 1 - strength / this, under the
    # temperature law; a tag at or above it cannot be broken.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 100.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 261.0167640762452.
    role_break_scale: float = 154.5748536747861
    # DECISION D-095: the floor of a pair's weight in consumer C1's choice, so that no
    # pair a role names is impossible to propose.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 1.0.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 2.392482515436875.
    role_pair_floor: float = 4.356269749299028
    # DECISION D-093: the re-cast's tier weights, first to fifth tier (the image under
    # the axis with the signature, the signature in the same column, the same type, the
    # rest of the row, and last the candidates CLAIMED by a standing correspondence,
    # D-101), a priori and halving; consumer C2 multiplies a candidate's salience by one
    # plus its tier's weight (D-096). Every value refused at zero or below.
    # DECISION D-114 (v3, BO fold 1 trial 244): was (8.0, 4.0, 2.0, 1.0, 0.5).
    # DECISION D-115 (v4, second BO fold 1 trial 800): was
    # (11.85241303266348, 1.90191195551275, 5.824314877208685, 0.3522396443338284, 1.8269391338241927).
    role_recast_tiers: tuple = (8.611247787299543, 3.751299142362098, 6.3046551355946585,
                                0.34952049253875445, 2.052319176232804)
    # DECISION D-100: consumer C7 multiplies a stray's region term (D-031) by this, a
    # priori a half, French's implicit pruning of the junk; refused at zero or below.
    # DECISION D-114 (v3, BO fold 1 trial 244): was 0.5.
    # DECISION D-115 (v4, second BO fold 1 trial 800): was 1.1999777021291365.
    stray_salience_scale: float = 0.8655580465974726

    def __post_init__(self):
        """Refuse a role knob at zero or below (D-078, the author's rule that a knob which
        turned roles off by value would be a switch): every field with `role` in its name,
        numbers inside tuples included, the `role` row of every table field (the role
        factor of `cp_static_weights`, D-098; the role term of `salience_weights`, D-100),
        `stray_salience_scale` (D-100) and the role codelets' raw urgencies (D-081); and
        refuse a `group_position_match` outside its two readings (D-097). Refuse too a
        `proximity_zero_gap` above `PROXIMITY_ZERO_GAP_MAX`, the bound French's Blockage 10
        sets on the proximity law [F95 p. 129] (D-118), and an `answer_patience` at zero or
        below, which would put the answer under time pressure from the start (D-120)."""
        if self.proximity_zero_gap > PROXIMITY_ZERO_GAP_MAX:
            raise ValueError(f"Config.proximity_zero_gap must be at most {PROXIMITY_ZERO_GAP_MAX} "
                             f"glass widths, the bound of D-118: {self.proximity_zero_gap!r}")
        if not _all_positive(self.answer_patience):
            raise ValueError(f"Config.answer_patience must be above zero: {self.answer_patience!r}")
        for f in dataclasses.fields(self):
            value = getattr(self, f.name)
            if "role" in f.name and not _all_positive(value):
                raise ValueError(f"Config.{f.name} must be above zero: {value!r}")
            if isinstance(value, tuple) and not _all_positive(_role_rows(value)):
                raise ValueError(f"Config.{f.name}'s role row must be above zero: {value!r}")
        if not _all_positive(self.stray_salience_scale):
            raise ValueError(f"Config.stray_salience_scale must be above zero: "
                             f"{self.stray_salience_scale!r}")
        table = dict(self.urgencies)
        for name in ROLE_CODELET_TYPES:
            if table.get(name, 0) <= 0:
                raise ValueError(f"the raw urgency of {name} must be above zero")
        if self.group_position_match not in GROUP_POSITION_MATCHES:
            raise ValueError(f"Config.group_position_match must be one of "
                             f"{GROUP_POSITION_MATCHES}: {self.group_position_match!r}")

    @cached_property
    def salience_weights_by_name(self) -> dict:
        """`salience_weights` as a dict of floats by factor name, built once per
        configuration (D-031, D-112): the engine reads it on every salience term."""
        return {name: float(value) for name, value in self.salience_weights}

    def as_dict(self, field_name: str) -> dict:
        """A table field (a tuple of pairs) as a dictionary, for the module that reads it once
        per run (DECISIONS.md, the table knobs of D-020, D-024, D-025, D-031, D-032, D-036,
        D-037, D-052, D-055)."""
        return dict(getattr(self, field_name))

    def differences(self, other: "Config") -> dict:
        """The fields on which this configuration differs from `other`, name to this one's
        value (D-066: a run records every field that differs from `standard`)."""
        return {f.name: getattr(self, f.name) for f in dataclasses.fields(self)
                if getattr(self, f.name) != getattr(other, f.name)}


def _role_rows(table) -> tuple:
    """The values of the rows named `role` in a table field (D-078, D-098, D-100)."""
    return tuple(v for row in table if isinstance(row, tuple) and len(row) == 2 and row[0] == "role"
                 for v in (row[1],))


def _all_positive(value) -> bool:
    """Whether every number in a value (a number, or a tuple of pairs) is above zero
    (D-078); a string or a boolean is not a number here."""
    if isinstance(value, bool):
        return True
    if isinstance(value, (int, float)):
        return value > 0
    if isinstance(value, tuple):
        return all(_all_positive(v) for v in value)
    return True


def _profile(name: str, **changes) -> Config:
    return dataclasses.replace(Config(), profile=name, **changes)


# DECISION D-066: the four profiles. `buridan` and `human` differ from `standard` in one
# knob each, fitted after the freeze by `bench/calibrate.py` (D-076; ANSWERS.md 7):
# `buridan`'s penalty stayed at the v2 default, since the scan on Buridan 1 was flat (the
# Worldview never holds two correspondences, RESULTS.md §9); `human`'s type weight is the
# least-squares minimum on the fit half of Tables 5.1-5.3 under the recorded seeds, a
# shallow minimum (RESULTS.md §9). `french-faults` restores the seven faults (ANSWERS.md 2).
# Since v3 (D-114) the three keep these overrides on top of the new defaults, not re-fitted:
# `buridan`'s 40 now differs from the default (27 in v3, 24 in v4, D-115), and `french-faults`
# changes six knobs, since the default of `push_reset_set` is already French's "symmetries".
PROFILES = {
    "standard": Config(),
    "buridan": _profile("buridan", incoherence_penalty=40),   # calibrated (ANSWERS 7), D-076
    "human": _profile("human", type_weight=50),               # calibrated (ANSWERS 7), D-076
    "french-faults": _profile("french-faults", **dict(FAULT_KNOBS)),
}


def codelet_label(codelet_type: str) -> str:
    """The Coderack strip's label for a codelet type [F95 p. 107 (img)] (D-051)."""
    return dict(CODELET_TYPES)[codelet_type]
