"""The configuration and the profiles (DECISIONS.md D-066; ANSWERS.md 2 and 7).

Every knob is a field of one frozen `Config` with a cited comment (test_conventions.py checks
the comments); the profiles are `standard`, `buridan`, `human` and `french-faults`; each
profile differs from `standard` only in the fields its decisions name: the seven fault knobs
for `french-faults` (SPEC.md 6b), and, from item 26, one knob each for `buridan` (the
incoherence penalty [F95 p. 130]) and `human` (the object-type weight [F95 pp. 142-143]).
The tables name real codelet types; the decision numbers in the comments are headings of
DECISIONS.md.
"""

import dataclasses
import re
from pathlib import Path

import pytest

from tabletop import config
from tabletop.config import (CALIBRATION_KNOBS, CODELET_TYPES, FAULT_KNOBS, PROFILES, V2_DEFAULTS,
                             Config,
                             codelet_label)

PYTHON_DIR = Path(config.__file__).resolve().parent.parent
DECISIONS = (PYTHON_DIR / "DECISIONS.md").read_text()
FIELD_NAMES = [f.name for f in dataclasses.fields(Config)]
TYPE_NAMES = [name for name, _ in CODELET_TYPES]


def test_the_four_profiles_exist_and_carry_their_names():
    assert list(PROFILES) == ["standard", "buridan", "human", "french-faults"]
    for name, cfg in PROFILES.items():
        assert isinstance(cfg, Config)
        assert cfg.profile == name


def test_standard_is_the_plain_config():
    assert PROFILES["standard"] == Config()
    assert Config().profile == "standard"


def test_config_is_frozen_and_hashable():
    cfg = Config()
    with pytest.raises(dataclasses.FrozenInstanceError):
        cfg.rho = 2.0
    hash(cfg)


def test_french_faults_differs_from_standard_in_exactly_the_seven_fault_knobs():
    diff = PROFILES["french-faults"].differences(PROFILES["standard"])
    assert diff.pop("profile") == "french-faults"
    # v3's default of `push_reset_set` is French's "symmetries" already (D-114), so the
    # profile changes the other six; it still sets all seven on top of `standard`.
    assert diff == {k: v for k, v in FAULT_KNOBS if k != "push_reset_set"}
    assert Config().push_reset_set == dict(FAULT_KNOBS)["push_reset_set"]
    assert all(getattr(PROFILES["french-faults"], k) == v for k, v in FAULT_KNOBS)
    assert len(FAULT_KNOBS) == 7
    # Every fault knob is a field, and its faulty value differs from the fixed v2 default.
    v2 = dict(V2_DEFAULTS)
    for name, faulty in FAULT_KNOBS:
        assert name in FIELD_NAMES, name
        assert v2.get(name, getattr(Config(), name)) != faulty, name


def test_the_calibrated_profiles_differ_only_in_their_one_knob_or_not_at_all():
    # Until item 26 fits them, they equal `standard` (but for the name); afterwards each may
    # differ in its one knob only (ANSWERS.md 7).
    allowed = dict(CALIBRATION_KNOBS)
    for name in ("buridan", "human"):
        diff = PROFILES[name].differences(PROFILES["standard"])
        assert diff.pop("profile") == name
        assert set(diff) <= {allowed[name]}, (name, diff)


def test_the_faulty_values_are_the_ones_decisions_md_names():
    # The paragraph on the seven fixes lists the restorations with their values; it moved
    # from DECISIONS.md's head to DECISIONS-INDEX.md with v3 (D-114).
    index = (PYTHON_DIR / "DECISIONS-INDEX.md").read_text()
    for name, faulty in FAULT_KNOBS:
        shown = repr(faulty) if not isinstance(faulty, str) else f'"{faulty}"'
        assert f"`{name} = {shown}`" in index, (name, shown)


def test_every_field_names_a_decision_that_is_a_heading_or_a_source_page():
    source = (PYTHON_DIR / "tabletop" / "config.py").read_text()
    headings = set(re.findall(r"^###\s+(D-\d{3})\b", DECISIONS, flags=re.M))
    for number in set(re.findall(r"\bD-\d{3}\b", source)):
        assert number in headings, number


def test_every_knob_named_in_decisions_md_is_a_field():
    knobs = set(re.findall(r"`Config\.(\w+)`", DECISIONS))
    missing = sorted(knobs - set(FIELD_NAMES))
    assert not missing, missing


def test_the_tables_name_real_codelet_types():
    assert len(TYPE_NAMES) == 29 and len(set(TYPE_NAMES)) == 29   # the legend's 23 + 6 role codelets (D-081)
    cfg = Config()
    assert [name for name, _ in cfg.urgencies] == TYPE_NAMES
    for _, codelet in cfg.top_down_table:
        assert codelet in TYPE_NAMES, codelet
    for codelet, count in cfg.initial_codelets:
        assert codelet in TYPE_NAMES and count > 0
    assert sum(n for _, n in cfg.initial_codelets) == 68          # v4 (D-115; v3's 60); v2's 25:
    assert sum(n for _, n in dict(V2_DEFAULTS)["initial_codelets"]) == 25
    for name, urgency in cfg.urgencies:
        assert 0 <= urgency <= 100, (name, urgency)   # raw urgencies in 1-100 [F95 p. 81];
    assert cfg.as_dict("urgencies")["find-answer"] == 0   # find-answer's is set at posting


def test_the_strip_labels_are_the_screens():
    assert codelet_label("find-group") == "FND-GP"
    assert codelet_label("find-answer") == "FND-ANS"
    assert codelet_label("pick-strong-cp") == "PK-SALC"
    labels = [label for _, label in CODELET_TYPES]
    assert len(set(labels)) == 29


def test_frenchs_own_constants_are_the_books():
    """French's own constants were the defaults through v2; v3 departs from them (D-114)."""
    cfg = dataclasses.replace(Config(), **dict(V2_DEFAULTS))
    assert cfg.differences(Config()).keys() == dict(V2_DEFAULTS).keys()
    assert cfg.rho == 3.0                           # [F95 p. 81]
    assert cfg.coderack_capacity == 150             # [F95 p. 82]
    assert (cfg.exponent_at_zero, cfg.exponent_at_hundred) == (6.0, 0.1)   # [F95 p. 92]
    assert (cfg.dq_window, cfg.wv_window) == (6, 10)                       # [F95 pp. 90-91]
    assert cfg.initial_temperature == 80.0          # [F95 p. 98 (img)]
    assert (cfg.initial_opposite, cfg.initial_touched_type) == (139, 51)   # [F95 p. 96 (img)]


def test_the_proximity_law_is_bounded_by_blockage_10_and_the_patience_is_positive():
    """D-118 (v5): `proximity_zero_gap` above `PROXIMITY_ZERO_GAP_MAX` (2.6 glass widths,
    under Blockage 10's 2.64 [F95 p. 129]) is refused, as a role knob at zero is (D-078);
    the default is the bound itself, and v3's and v4's fitted values can no longer be
    built. D-120: `answer_patience` at zero or below is refused."""
    assert config.PROXIMITY_ZERO_GAP_MAX == 2.6
    assert Config().proximity_zero_gap == config.PROXIMITY_ZERO_GAP_MAX
    assert all(cfg.proximity_zero_gap <= 2.6 for cfg in PROFILES.values())
    Config(proximity_zero_gap=2.6)
    Config(proximity_zero_gap=1.0)
    with pytest.raises(ValueError):
        Config(proximity_zero_gap=2.7)
    with pytest.raises(ValueError):
        dataclasses.replace(Config(), **dict(config.V3_DEFAULTS))     # 4.94, refused
    with pytest.raises(ValueError):
        Config(proximity_zero_gap=5.478639330528657)                  # v4's value, refused
    for bad in (0, -1):
        with pytest.raises(ValueError):
            Config(answer_patience=bad)
    Config(answer_patience=1)


def test_as_dict_and_differences():
    cfg = dataclasses.replace(Config(), **dict(V2_DEFAULTS))
    assert cfg.as_dict("group_stage_pass") == {"sameness": 0.95, "shared-isa": 0.6, "mixed": 0.1}
    other = dataclasses.replace(cfg, rho=2.0, profile="x")
    assert other.differences(cfg) == {"profile": "x", "rho": 2.0}
    assert cfg.differences(cfg) == {}


def test_the_law_of_temperature_is_deterministic_at_zero_and_uniform_at_one_hundred():
    # D-060's two-outcome law, stated in DECISIONS-INDEX.md: q^a / (q^a + (1 - q)^a).
    def passes(q, a):
        return q ** a / (q ** a + (1 - q) ** a)
    cfg = Config()
    assert passes(0.6, cfg.exponent_at_zero) > 0.9
    assert passes(0.1, cfg.exponent_at_zero) < 1e-5
    assert abs(passes(0.6, cfg.exponent_at_hundred) - 0.5) < 0.03
    assert abs(passes(0.1, 1.0) - 0.1) < 1e-12
