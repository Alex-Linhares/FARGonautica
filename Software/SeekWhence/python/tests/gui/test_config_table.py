"""Loop0004 item 15: the configuration pane's model
(`seekwhence/gui/models/config_table.py`), Qt-free (PANES.md P13; ANSWERS.md 5; G-050,
G-051).

- Every field of `dataclasses.fields(Config)` has exactly one row, and every row has at
  least one source tag or `D-NNN`; the decisions' titles come from DECISIONS.md.
- With no edits the next run's configuration equals `Config()`; an edit to one field of
  each kind (int, float, bool, tuple, a nested table) gives
  `dataclasses.replace(Config(), field=value)` and nothing else changes; invalid values
  are rejected with a message and leave the configuration unchanged; the controls' three
  fields are not edited here.
- A run started with the edited configuration records it in its `start` event, and the
  engine is the same engine (a plain `Session` with the same `Config` gives the same
  events).
- Reset per field and Reset all return to `Config()`; the settings round trip; bad
  settings are dropped; the filter and the tooltip.
"""

import dataclasses
import io
import json

import pytest

from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.gui.models import config_table as ct
from seekwhence.session import Session
from seekwhence.trace import event_record

from golden import appendix_inputs

FIELDS = [f.name for f in dataclasses.fields(Config)]


@pytest.fixture(scope="module")
def notes():
    return ct.read_fields()


@pytest.fixture
def table(notes):
    return ct.ConfigTable(notes, ct.decision_titles())


# -- the rows (PANES.md P13) ----------------------------------------------------------------------

def test_every_config_field_has_exactly_one_row_with_a_tag_or_decision(notes, table):
    assert [n.name for n in notes] == FIELDS
    rows = table.rows()
    assert [r.name for r in rows] == FIELDS
    for row in rows:
        assert row.note.tags or row.note.decisions, row.name
        assert row.source_text
        assert row.note.comment and row.note.section
        assert row.value == row.note.default and not row.edited
        assert row.controlled == (row.name in ct.CONTROLLED)
    assert {r.note.kind for r in rows} == {"int", "float", "bool", "tuple"}
    by_name = {n.name: n for n in notes}
    assert by_name["urgency_power"].decisions == ("D-007",)
    assert by_name["threshold_fraction"].tags == ("[M86 p. 105]",)
    assert by_name["threshold_fraction"].decisions == ("D-046",)
    assert by_name["s_links"].tags[0] == "[M86 p. 90]" and by_name["s_links"].decisions == ("D-038",)
    assert by_name["bond_values"].comment.endswith('keyed "Same-span" (DECISION D-039).')
    assert by_name["sparkler_plus_urgencies"].section == "the cytoplasm"
    assert by_name["max_cast_failures"].section == "reformulation"
    assert by_name["verbose"].kind == "bool" and by_name["urgency_power"].kind == "int"


def test_the_decision_titles_come_from_decisions_md(table, notes):
    assert table.title_of("D-007") == "D-007: Taskrack sampling rule"
    used = {d for n in notes for d in n.decisions}
    assert used and all(d in table.titles for d in used), used - set(table.titles)
    assert table.title_of("D-999") == "D-999"


def test_kind_of():
    assert ct.kind_of(True) == "bool" and ct.kind_of(3) == "int"
    assert ct.kind_of(0.5) == "float" and ct.kind_of(()) == "tuple"
    with pytest.raises(ct.ConfigError):
        ct.kind_of("x")


# -- the edits (ANSWERS.md 5) ------------------------------------------------------------------------

EDITS = (("guess_min_hits", "4", 4),
         ("stay_factor_per_try", "0.25", 0.25),
         ("reject_freeze_dried_repeats", "false", False),
         ("strict_classes", '("Constant",)', ("Constant",)),
         ("s_links", '(("S-group", "Countup", 1), ("Cycle", "Tuple", 0.5))',
          (("S-group", "Countup", 1.0), ("Cycle", "Tuple", 0.5))),
         ("bond_type_weights", '[["Same", 12], ["Succ", 8], ["Pred", 8]]',
          (("Same", 12), ("Succ", 8), ("Pred", 8))))


def test_no_edits_give_the_defaults(table):
    assert table.config() == Config()
    assert table.config(Config(verbose=False, max_tasks=5)) == Config(verbose=False, max_tasks=5)
    assert table.edits == {} and table.edited_names == ()


@pytest.mark.parametrize("name, text, value", EDITS)
def test_an_edit_to_one_field_of_each_kind_changes_that_field_only(table, name, text, value):
    assert table.set_text(name, text)
    assert table.config() == dataclasses.replace(Config(), **{name: value})
    assert table.edits == {name: value}
    assert type(getattr(table.config(), name)) is type(getattr(Config(), name))
    row = table.rows()[FIELDS.index(name)]
    assert row.edited and row.value == value
    assert [r.name for r in table.rows() if r.edited] == [name]
    # the text shown reads back as the same value
    assert ct.parse_text(ct.value_text(value), row.note.default) == value


BAD = (("guess_min_hits", "1.5"), ("guess_min_hits", "four"), ("guess_min_hits", ""),
       ("stay_factor_per_try", "x"), ("reject_freeze_dried_repeats", "maybe"),
       ("strict_classes", "nonsense"), ("strict_classes", "3"), ("strict_classes", "(1, 2)"),
       ("s_links", '(("S-group", "Countup"),)'), ("s_links", '(("S-group", 3, 0.1),)'),
       ("bond_type_weights", '(("Same", 1.5),)'), ("bond_type_weights", '(("Same", True),)'),
       ("urgencies", "(1, 2)"), ("s_links", "__import__('os')"))


@pytest.mark.parametrize("name, text", BAD)
def test_invalid_values_are_rejected_and_leave_the_configuration_unchanged(table, name, text):
    table.set_text("guess_validity", "2")
    before = table.config()
    with pytest.raises(ct.ConfigError) as info:
        table.set_text(name, text)
    assert str(info.value)
    assert table.config() == before and table.edits == {"guess_validity": 2}


def test_the_controlled_fields_and_unknown_fields_are_refused(table):
    for name in ct.CONTROLLED:
        with pytest.raises(ct.ConfigError, match="Controls pane"):
            table.set_text(name, "1")
    with pytest.raises(ct.ConfigError, match="no field"):
        table.set_text("no_such_field", "1")
    assert table.config() == Config()


def test_an_edit_equal_to_the_default_is_no_edit(table):
    assert not table.set_text("guess_min_hits", "3")
    assert table.edits == {}
    assert table.set_text("guess_min_hits", "4") and table.set_text("guess_min_hits", "3")
    assert table.edits == {}
    assert not table.set_value("s_links", list(list(e) for e in Config().s_links))


def test_the_controls_fields_are_kept_from_the_base(table):
    table.set_text("guess_min_hits", "4")
    base = Config(verbose=False, max_tasks_per_term=7, max_tasks=99, guess_min_hits=9,
                  urgency_power=2)
    config = table.config(base)
    assert config == Config(verbose=False, max_tasks_per_term=7, max_tasks=99, guess_min_hits=4)
    assert config.urgency_power == 1     # a reset field is the default, not the base's


def test_reset_and_reset_all_return_to_the_defaults(table):
    calls = []
    table.listeners.append(lambda: calls.append(dict(table.edits)))
    table.set_text("guess_min_hits", "4")
    table.set_text("slip_scout_factor", "0.75")
    assert table.edited_names == ("guess_min_hits", "slip_scout_factor")
    assert table.reset("guess_min_hits") and table.edits == {"slip_scout_factor": 0.75}
    assert not table.reset("guess_min_hits")
    assert table.reset_all() and table.config() == Config() and table.edits == {}
    assert not table.reset_all()
    assert calls == [{"guess_min_hits": 4}, {"guess_min_hits": 4, "slip_scout_factor": 0.75},
                     {"slip_scout_factor": 0.75}, {}]


def test_set_config_takes_the_edits_of_a_configuration(table):
    config = Config(verbose=False, max_tasks=5, guess_min_hits=4, testers_per_spark=2)
    assert table.set_config(config)
    assert table.edits == {"guess_min_hits": 4, "testers_per_spark": 2}
    assert table.config(config) == config
    assert not table.set_config(config)
    assert table.set_config(Config()) and table.edits == {}


def test_rows_of_a_shown_configuration(table):
    shown = Config(verbose=False, guess_min_hits=4)
    rows = {r.name: r for r in table.rows(shown)}
    assert rows["verbose"].edited and rows["verbose"].value is False and rows["verbose"].controlled
    assert rows["guess_min_hits"].edited and rows["guess_min_hits"].value == 4
    assert not rows["urgency_power"].edited


# -- values and texts --------------------------------------------------------------------------------

def test_value_text_and_parse_text_round_trip_every_default(notes):
    for note in notes:
        text = ct.value_text(note.default)
        assert ct.parse_text(text, note.default) == note.default, note.name
        if note.kind == "tuple" and note.default and isinstance(note.default[0], tuple):
            assert text.count("\n") == len(note.default) - 1     # one entry per line
    assert ct.value_text(True) == "true" and ct.value_text(0.5) == "0.5"
    assert ct.parse_text("yes", True) is True and ct.parse_text("0", True) is False
    assert ct.parse_text(" 2 ", 1) == 2 and ct.parse_text("1", 0.5) == 1.0


def test_conform_takes_lists_and_integers_for_floats_and_refuses_the_rest():
    assert ct.conform([["Same", 10]], Config().bond_type_weights) == (("Same", 10),)
    assert ct.conform((("S-group", "Countup", 1),), Config().s_links) == (("S-group", "Countup", 1.0),)
    assert ct.conform([], Config().strict_classes) == ()
    assert ct.conform(2, 0.5) == 2.0 and type(ct.conform(2, 0.5)) is float
    for value, default in ((True, 1), (1.5, 1), ("1", 1), (1, True), ("x", 0.5), (1, ()),
                           ((1,), ("a",)), ((("a",),), (("a", 1),))):
        with pytest.raises(ct.ConfigError):
            ct.conform(value, default)


# -- a run with the edited configuration (ANSWERS.md 5) ---------------------------------------------------

def test_a_run_started_with_the_edits_records_them_and_is_the_engines_run(table):
    table.set_text("guess_min_hits", "4")
    config, lines, answers = appendix_inputs("run1_3737")
    edited = table.config(config)
    assert edited == Config(guess_min_hits=4) and edited != config

    def run(cfg):
        rec, out = Recorder(), io.StringIO()
        s = Session(cfg, 54, out=out, lines=list(lines), answers=list(answers), observers=[rec])
        s.run()
        return [event_record(e) for e in rec.events], s.world.rng.draws, s.outcome
    records, draws, outcome = run(edited)
    assert dict(records[0]["config"])["guess_min_hits"] == 4
    assert (records, draws, outcome) == run(Config(guess_min_hits=4))
    assert records != run(config)[0]        # the edit changed the run: the guess fires later


# -- QSettings (Numbo GUI) ---------------------------------------------------------------------------

def test_the_settings_round_trip(table, notes):
    for name, text, _ in EDITS:
        table.set_text(name, text)
    settings = table.to_settings()
    assert set(settings) == {ct.SETTINGS_KEY}
    pairs = json.loads(settings[ct.SETTINGS_KEY])
    assert [p[0] for p in pairs] == list(table.edited_names)
    other = ct.ConfigTable(notes, {})
    assert other.from_settings(settings)
    assert other.edits == table.edits and other.config() == table.config()
    assert not ct.ConfigTable(notes, {}).from_settings({}) and ct.ConfigTable(notes, {}).edits == {}


@pytest.mark.parametrize("text", ["not json", "{}", "[1]", '[["no_such", 1]]', '[["verbose", false]]',
                                  '[["guess_min_hits", "4"]]', '[["guess_min_hits", 3]]',
                                  '[["s_links", [["a"]]]]', "42", ""])
def test_bad_settings_are_dropped(notes, text):
    table = ct.ConfigTable(notes, {})
    table.set_text("guess_min_hits", "4")
    restored = table.from_settings({ct.SETTINGS_KEY: text})
    assert not restored
    # a text that is not JSON or not a list leaves the edits; a list replaces them
    assert table.edits in ({}, {"guess_min_hits": 4})
    assert table.config() in (Config(), Config(guess_min_hits=4))


def test_a_good_pair_survives_a_bad_one(notes):
    table = ct.ConfigTable(notes, {})
    assert table.from_settings({ct.SETTINGS_KEY: '[["guess_min_hits", 4], ["x", 1]]'})
    assert table.edits == {"guess_min_hits": 4}


# -- the view's helpers (G-051) ----------------------------------------------------------------------

def test_filter_rows_and_the_tooltip(table):
    rows = table.rows(Config(guess_min_hits=4))
    assert ct.filter_rows(rows, "") == rows
    assert [r.name for r in ct.filter_rows(rows, "GUESS_")] == ["guess_validity", "guess_min_hits"]
    assert [r.name for r in ct.filter_rows(rows, "the guess")] == [
        "guess_validity", "guess_min_hits", "reject_freeze_dried_repeats"]   # the section
    assert [r.name for r in ct.filter_rows(rows, "D-011")] == ["max_tasks_per_term", "max_tasks"]
    assert all(r.note.section == "reformulation" for r in ct.filter_rows(rows, "reformulation"))
    assert [r.name for r in ct.filter_rows(rows, '[M86 p. 105]')] == ["threshold_fraction"]
    tip = ct.row_tooltip(rows[FIELDS.index("guess_min_hits")], table.titles)
    assert tip.startswith("guess_min_hits (int)") and "section: the hypothesis and the guess" in tip
    assert "D-033: " in tip and tip.endswith("default: 3")
    tip = ct.row_tooltip(rows[FIELDS.index("verbose")], table.titles)
    assert "Controls pane" in tip and "D-055: " in tip and "default" not in tip.split("\n")[-1]
