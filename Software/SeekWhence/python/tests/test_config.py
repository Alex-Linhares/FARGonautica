"""The knobs of `Config` (item 05) agree with DECISIONS.md and with the sources' values.

- every `Config.<name>` named as a knob in DECISIONS.md is a field, and every field is named
  as a knob by some decision or carries a source tag;
- the decision numbers are consecutive from D-001;
- the tables are well formed: every task type has one urgency, every class in the s-link and
  bond tables is one of the eight, slipperiness is between 0 and 1 [M86 p. 90], bond values
  are between -10 and 10 [M86 p. 91];
- the values the thesis gives are in the tables: S-group to Countup 0.1 [M86 p. 90], to Tuple
  0.4 [M86 p. 91], to Y-group 0.4 [M86 p. 104]; S-group +10 adjacent Succ, -5 remote Same
  [M86 p. 101], 0 adjacent Pred [M86 p. 175]; Y-group +10 remote Same [M86 p. 104]; the
  first threshold at 80% [M86 p. 105].
"""

import re
from dataclasses import fields
from pathlib import Path

from seekwhence.config import IDEAL_TYPES, TASK_TYPES, Config

ROOT = Path(__file__).resolve().parents[1]
DECISIONS = (ROOT / "DECISIONS.md").read_text()
CONFIG_SOURCE = (ROOT / "seekwhence" / "config.py").read_text()


def knobs_named_in_decisions() -> set[str]:
    return set(re.findall(r"`Config\.(\w+)`", DECISIONS))


def test_every_knob_in_decisions_is_a_field_and_every_field_is_a_knob_or_cited():
    names = {f.name for f in fields(Config)}
    named = knobs_named_in_decisions()
    assert named <= names, f"knobs in DECISIONS.md with no Config field: {sorted(named - names)}"
    # A field not named by a decision must carry a source tag in config.py (threshold_fraction).
    for name in names - named:
        line = next(l for l in CONFIG_SOURCE.splitlines() if l.strip().startswith(f"{name}:"))
        above = CONFIG_SOURCE[:CONFIG_SOURCE.index(line)].splitlines()[-3:]
        assert re.search(r"\[M86 p", " ".join(above)), f"{name} is neither a knob nor cited"


def test_decision_numbers_are_consecutive_from_one():
    numbers = [int(n) for n in re.findall(r"^### D-(\d{3}):", DECISIONS, flags=re.M)]
    assert numbers == list(range(1, len(numbers) + 1))
    assert "### D-007: Taskrack sampling rule" in DECISIONS


def test_every_decision_has_the_five_parts():
    entries = re.split(r"^### D-\d{3}:", DECISIONS, flags=re.M)[1:]
    for entry in entries:
        for part in ("**Sources:**", "**Choice:**", "**Reason:**", "**Knob:**", "**Alternatives:**"):
            assert part in entry, f"an entry lacks {part}: {entry[:60]!r}"


def test_urgencies_cover_every_task_type_once():
    c = Config()
    names = [name for name, _ in c.urgencies]
    assert len(names) == len(set(names))
    expected = set(TASK_TYPES) - {"Sparkler-plus", "empty-task"}   # D-015, D-013
    assert set(names) == expected
    assert all(isinstance(u, int) and u >= 1 for _, u in c.urgencies)
    assert all(isinstance(u, int) and u >= 1 for u in c.sparkler_plus_urgencies)
    for group in (c.cleaned_on_term, c.cleaned_on_reform, c.cleaned_on_hypothesis):
        assert set(group) <= set(TASK_TYPES)


def test_s_links_are_well_formed_and_hold_the_thesis_values():
    c = Config()
    links = {(a, b): s for a, b, s in c.s_links}
    assert len(links) == len(c.s_links)
    assert all(a in IDEAL_TYPES and b in IDEAL_TYPES and a != b for a, b in links)
    assert all(0 <= s <= 1 for s in links.values())
    assert links["S-group", "Countup"] == 0.1                       # p. 90
    assert links["S-group", "Tuple"] == 0.4                         # p. 91
    assert links["S-group", "Y-group"] == 0.4                       # p. 104
    assert {b for a, b in links if a == "S-group"} == {"Countup", "C-group", "Y-group",
                                                        "Cycle", "Tuple"}   # p. 90
    assert ("Countup", "S-group") in links                          # the slip of p. 174
    assert all(any(a == cls for a, _ in links) for cls in IDEAL_TYPES)


def test_bond_values_are_well_formed_and_hold_the_thesis_values():
    c = Config()
    values = {(cls, kind, mod): v for cls, kind, mod, v in c.bond_values}
    assert len(values) == len(c.bond_values)
    assert all(cls in IDEAL_TYPES for cls, _, _ in values)
    assert all(mod in ("adjacent", "remote") for _, _, mod in values)
    assert all(-10 <= v <= 10 and v != 0 for v in values.values())
    assert values["S-group", "Succ", "adjacent"] == 10              # p. 101
    assert values["S-group", "Same", "remote"] == -5                # p. 101
    assert ("S-group", "Pred", "adjacent") not in values            # 0 on p. 175
    assert values["Y-group", "Same", "remote"] == 10                # p. 104
    assert values["C-group", "Same", "adjacent"] > 0                # p. 91
    assert values["C-group", "Succ", "adjacent"] < 0                # p. 100
    assert c.threshold_fraction == 0.8                              # p. 105
    assert c.strict_classes == ("Constant", "Countup")


def test_the_scalar_knobs_have_their_decided_defaults():
    c = Config()
    assert (c.guess_validity, c.guess_min_hits) == (1, 3)           # D-032, D-033
    assert (c.testers_per_spark, c.sparklers_per_bond, c.glom_scouts_per_bond,
            c.sparklers_per_term) == (1, 2, 2, 3)                   # D-016
    assert c.max_cast_failures == 3 and c.slip_scout_factor == 0.5  # D-050, D-049
    assert c.stay_factor_per_try == 0.5                             # D-041
    assert c.max_tasks_per_term < c.max_tasks                       # D-011
    assert hash(c) == hash(Config())                                # frozen and hashable
