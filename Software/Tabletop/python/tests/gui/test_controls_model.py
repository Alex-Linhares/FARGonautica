"""Loop0006 item 12: the controls' model (`tabletop/gui/models/controls.py`; G-044),
Qt-free, after the Seek-Whence GUI's `test_controls_model.py`.

- The catalogue holds every problem of the library exactly once, the chapter 4
  configuration first [F95 p. 118] and then each family in `problems.FAMILIES`' order,
  with the library's titles and pages; the labels and tooltips.
- The parsers accept what the CLI accepts and refuse the rest with a message.
- `run_inputs` and `values_of` are inverses on valid inputs; the settings round trip
  replaces a bad value by its default.
"""

import dataclasses

import pytest

from tabletop.config import PROFILES
from tabletop.gui.models import controls as cm
from tabletop.problems import FAMILIES, by_family, catalogue as library, load_problem


def test_the_catalogue_is_the_library_by_family_with_the_chapter_4_configuration_first():
    entries = cm.catalogue()
    names = [e.name for e in entries]
    assert names[0] == "surround-05" == cm.DEFAULT_PROBLEM
    assert sorted(names) == sorted(library()) and len(set(names)) == len(names) == 53
    expected = ["surround-05"]
    for family in FAMILIES:
        expected += [n for n in by_family(family) if n != "surround-05"]
    assert names == expected
    for entry in entries:
        problem = load_problem(entry.name)
        assert entry.title == problem.title and entry.family == problem.family
        assert entry.page == problem.source.get("page")
    first = entries[0]
    assert first.label == "Surround 5 (surround-05, p. 118)"
    assert first.citation == '[F95 p. 118]'
    assert "chapter 4" in first.tooltip and "page 118" in first.tooltip
    assert cm.entry_of("buridan-01").name == "buridan-01"
    with pytest.raises(KeyError):
        cm.entry_of("no-such-problem")
    assert set(cm.FAMILY_TITLES) == set(FAMILIES)


def test_an_entry_without_a_page_has_no_citation():
    entry = cm.ProblemEntry("x-01", "X 1", "x")
    assert entry.label == "X 1 (x-01)" and entry.citation == "" and "page" not in entry.tooltip


def test_the_seed_parser_accepts_the_clis_integers():
    assert cm.parse_seed("7") == 7 and cm.parse_seed(" 42 ") == 42 and cm.parse_seed(3) == 3
    assert cm.parse_seed("-1") == -1
    for bad in ("many", "1.5", "", "0x10"):
        with pytest.raises(cm.InputError) as error:
            cm.parse_seed(bad)
        assert "seed" in str(error.value)


def test_run_inputs_checks_every_field():
    assert cm.run_inputs(cm.ControlValues()) == ("surround-05", "standard", 1)
    assert cm.run_inputs(cm.ControlValues("buridan-02", "human", " 9")) == ("buridan-02", "human", 9)
    with pytest.raises(cm.InputError) as error:
        cm.run_inputs(cm.ControlValues(problem="eleventy"))
    assert "eleventy" in str(error.value)
    with pytest.raises(cm.InputError) as error:
        cm.run_inputs(cm.ControlValues(profile="nope"))
    assert "nope" in str(error.value)
    with pytest.raises(cm.InputError):
        cm.run_inputs(cm.ControlValues(seed="many"))


def test_values_of_shows_a_run_started_any_other_way():
    previous = cm.ControlValues(speed=3)
    values = cm.values_of("blockage-06", "french-faults", 2, previous)
    assert values == cm.ControlValues("blockage-06", "french-faults", "2", 3)
    assert cm.run_inputs(values) == ("blockage-06", "french-faults", 2)
    # an edited table (no name in the library) and a custom configuration leave the
    # previous choice standing; the stats pane says what the run really was (G-044)
    assert cm.values_of("table-edited", "custom", 5, previous) == cm.ControlValues(
        "surround-05", "standard", "5", 3)
    assert cm.values_of("buridan-01", "buridan", 1).speed == cm.DELAYS.index(cm.DEFAULT_DELAY)


def test_the_settings_round_trip_and_bad_values():
    values = cm.ControlValues("buridan-02", "buridan", "7", 10)
    settings = values.to_settings()
    assert settings == {"controls/problem": "buridan-02", "controls/profile": "buridan",
                        "controls/seed": 7, "controls/speed": 10}
    assert cm.ControlValues.from_settings(settings) == values
    assert cm.ControlValues.from_settings({}) == cm.ControlValues()
    bad = {"controls/problem": "eleventy", "controls/profile": "nope", "controls/seed": "many",
           "controls/speed": 99}
    assert cm.ControlValues.from_settings(bad) == cm.ControlValues()
    # the INI backend gives strings; booleans are not integers
    assert cm.ControlValues.from_settings({"controls/seed": "12", "controls/speed": "4"}) == \
        cm.ControlValues(seed="12", speed=4)
    assert cm.ControlValues.from_settings({"controls/seed": True, "controls/speed": True}) == \
        cm.ControlValues()
    assert cm.ControlValues(seed="many").to_settings()["controls/seed"] == "many"
    assert set(cm.SETTINGS_KEYS) == {k.split("/")[1] for k in settings}


def test_the_speeds_are_numbos_and_their_texts():
    assert cm.DELAYS == (1.0, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0)
    assert cm.DEFAULT_DELAY in cm.DELAYS
    assert [cm.delay_text(d) for d in (1.0, 0.5, 0.01, 0.0)] == ["1 s", "500 ms", "10 ms", "max"]


def test_the_cap_line_names_max_codelets_of_the_configuration():
    assert cm.cap_text(PROFILES["standard"]).startswith("3000 codelets (max_codelets")
    edited = dataclasses.replace(PROFILES["standard"], max_codelets=400)
    assert cm.cap_text(edited).startswith("400 codelets") and "Configuration" in cm.cap_text(edited)
