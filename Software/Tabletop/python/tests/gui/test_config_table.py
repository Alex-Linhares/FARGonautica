"""Loop0006 item 13: the configuration pane's model (`tabletop/gui/models/config_table.py`),
Qt-free (PANES.md P13; ANSWERS.md 3; G-046).

- One row per `dataclasses.fields(Config)`, in field order, each with a source tag or a
  `D-NNN`; the sections, names and comments equal `tools/knobs_table.py`'s own reading of
  `config.py` (loaded by path); the decisions' titles come from DECISIONS.md.
- No edits on profile p gives `PROFILES[p]`; an edit of one field of each kind (int,
  float, bool, str, a table of pairs, a flat tuple) gives
  `dataclasses.replace(PROFILES[p], field=value)` and nothing else; invalid values are
  rejected with a message and leave the configuration unchanged; the `profile` field is
  refused; an edit equal to the profile's value is no edit; the edits survive a profile
  change and are marked only where they differ from the profile.
- The rows' marks: French's seven faults with their faulty values, the two calibrated
  knobs, the fields where a profile differs from `standard`.
- A run under an edited configuration equals a plain `run_problem` under the same
  `Config` and its `RunStarted.changed` shows the edit; `config_defaults.lock` is
  untouched and `PROFILES` unchanged.
- Reset per field and Reset all; `set_config`; the settings round trip; bad settings
  dropped; the filter; the tooltip; `edited_fields` and `config_note_of`.
"""

import dataclasses
import hashlib
import importlib.util
import json
from pathlib import Path

import pytest

from tabletop.config import CALIBRATION_KNOBS, FAULT_KNOBS, PROFILES, Config
from tabletop.events import Collector, RunStarted, record_of
from tabletop.gui.models import config_table as ct
from tabletop.problems import load_problem
from tabletop.run import run_problem

PYTHON_DIR = Path(__file__).resolve().parents[2]
KNOBS_TABLE = PYTHON_DIR / "tools" / "knobs_table.py"
CONFIG_PY = PYTHON_DIR / "tabletop" / "config.py"
LOCK = PYTHON_DIR / "config_defaults.lock"
FIELDS = [f.name for f in dataclasses.fields(Config)]


@pytest.fixture(scope="module")
def notes():
    return ct.read_fields()


@pytest.fixture(scope="module")
def titles():
    return ct.decision_titles()


@pytest.fixture
def table(notes, titles):
    return ct.ConfigTable(notes, titles)


def knobs_table_rows():
    """`tools/knobs_table.py`'s `fields_of` on `config.py`, the tool loaded by path."""
    spec = importlib.util.spec_from_file_location("knobs_table", KNOBS_TABLE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.fields_of(CONFIG_PY.read_text(encoding="utf-8"))


# -- the rows (PANES.md P13) ----------------------------------------------------------------------

def test_every_config_field_has_exactly_one_row_with_a_tag_or_decision(notes, table):
    assert [n.name for n in notes] == FIELDS
    rows = table.rows()
    assert [r.name for r in rows] == FIELDS
    for row in rows:
        assert row.note.tags or row.note.decisions, row.name
        assert row.source_text and row.note.comment and row.note.section
        assert row.value == row.note.default == row.profile_value
        assert not row.edited and not row.differs
        assert row.fixed == (row.name == "profile")
    assert {r.note.kind for r in rows} == {"int", "float", "bool", "str", "tuple"}
    by_name = {n.name: n for n in notes}
    assert by_name["max_codelets"].decisions == ("D-061",) and by_name["max_codelets"].tags == ()
    assert by_name["region_preference"].tags == ('[F95 p. 66; F95 p. 93]',)
    assert by_name["region_preference"].decisions == ("D-032", "D-114", "D-115")
    assert by_name["initial_opposite"].tags == ('[F95 p. 96 (img)]',)
    assert by_name["max_codelets"].section.startswith("the answer and the run")
    assert by_name["rho"].kind == "float" and by_name["push_reset_set"].kind == "str"
    assert by_name["keep_all_subgroups"].kind == "bool" and by_name["urgencies"].kind == "tuple"


def test_the_comments_are_read_as_the_knobs_table_tool_reads_them(notes):
    """ANSWERS.md 3: the comments, as `tools/knobs_table.py` reads them for the README."""
    rows = knobs_table_rows()
    assert len(rows) == len(notes) == len(FIELDS)
    for (section, name, _default, comment), note in zip(rows, notes):
        assert (section, name, comment) == (note.section, note.name, note.comment)
    # a comment of several lines is whole, with the DECISION word dropped
    focus = next(n for n in notes if n.name == "focus_jolt")
    assert focus.comment.startswith("The dumps hold the touched object's type")
    assert "D-017: the jolt each receives" in focus.comment and "DECISION" not in focus.comment


def test_the_decision_titles_come_from_decisions_md(table, notes, titles):
    assert table.title_of("D-061") == "D-061: The run cap"
    assert titles["D-066"] == "D-066: Profiles"
    used = {d for n in notes for d in n.decisions}
    assert used and all(d in titles for d in used), used - set(titles)
    assert table.title_of("D-99") == "D-99"          # no such heading: the number alone


def test_kind_of():
    assert ct.kind_of(True) == "bool" and ct.kind_of(3) == "int"
    assert ct.kind_of(0.5) == "float" and ct.kind_of(()) == "tuple" and ct.kind_of("x") == "str"
    with pytest.raises(ct.ConfigError):
        ct.kind_of(None)


# -- the edits (ANSWERS.md 3) ---------------------------------------------------------------------

EDITS = (("max_codelets", "400", 400),
         ("rho", "2.5", 2.5),
         ("keep_all_subgroups", "true", True),
         ("push_reset_set", "slippage-concepts", "slippage-concepts"),   # v3's is "symmetries"
         ("depth_by_level", "(10, 40, 90)", (10, 40, 90)),
         ("region_preference", '(("diagonal", 1), ("opposite", 0.5))',
          (("diagonal", 1), ("opposite", 0.5))),
         ("initial_codelets", '[["find-group", 5], ["pick-strong-cp", 2]]',
          (("find-group", 5), ("pick-strong-cp", 2))))


@pytest.mark.parametrize("profile", list(PROFILES))
def test_no_edits_on_a_profile_give_that_profile(table, profile):
    table.set_profile(profile)
    assert table.config() == PROFILES[profile] and table.config() is not None
    assert table.edits == {} and table.edited_names == () and table.edited_text() == ""
    assert table.config().profile == profile


@pytest.mark.parametrize("name, text, value", EDITS)
@pytest.mark.parametrize("profile", ["standard", "french-faults"])
def test_an_edit_to_one_field_of_each_kind_changes_that_field_only(table, profile, name, text,
                                                                   value):
    table.set_profile(profile)
    base = PROFILES[profile]
    if getattr(base, name) == value:          # two faults are already on under french-faults
        text, value = {"keep_all_subgroups": ("false", False),
                       "push_reset_set": ("slippage-concepts", "slippage-concepts")}[name]
    assert table.set_text(name, text)
    assert table.config() == dataclasses.replace(base, **{name: value})
    assert table.edits == {name: value} and table.edited_names == (name,)
    assert table.edited_text() == f"edited: {name}"
    assert type(getattr(table.config(), name)) is type(getattr(base, name))
    row = table.rows()[FIELDS.index(name)]
    assert row.edited and row.value == value and row.profile_value == getattr(base, name)
    assert [r.name for r in table.rows() if r.edited] == [name]
    assert ct.parse_text(ct.value_text(value), row.note.default) == value


BAD = (("max_codelets", "1.5"), ("max_codelets", "four"), ("max_codelets", ""),
       ("rho", "x"), ("keep_all_subgroups", "maybe"), ("depth_by_level", "nonsense"),
       ("depth_by_level", "3"), ("depth_by_level", "(1.5, 2, 3)"),
       ("region_preference", '(("diagonal",),)'), ("region_preference", '((1, 1.0),)'),
       ("initial_codelets", '(("find-group", 1.5),)'), ("initial_codelets", '(("find-group", True),)'),
       ("urgencies", "(1, 2)"), ("urgencies", "__import__('os')"), ("profile", "human"),
       ("no_such_field", "1"))


@pytest.mark.parametrize("name, text", BAD)
def test_invalid_values_are_rejected_and_leave_the_configuration_unchanged(table, name, text):
    table.set_text("win_bonus", "7")
    before = table.config()
    with pytest.raises(ct.ConfigError) as info:
        table.set_text(name, text)
    assert str(info.value)
    assert table.config() == before and table.edits == {"win_bonus": 7}


def test_an_edit_equal_to_the_profiles_value_is_no_edit(table):
    assert not table.set_text("max_codelets", "3000")
    assert table.edits == {}
    assert table.set_text("max_codelets", "400") and table.set_text("max_codelets", "3000")
    assert table.edits == {}
    assert not table.set_value("region_preference", [list(e) for e in Config().region_preference])
    assert not table.set_value("cp_static_weights", Config().cp_static_weights)
    table.set_profile("human")
    assert not table.set_text("type_weight", "50") and table.edits == {}


def test_the_edits_survive_a_profile_change_and_are_marked_where_they_differ(table):
    """G-046: an edit is kept across a profile change; `edited_names` are the edits that
    differ from the profile's value."""
    table.set_text("type_weight", "50")
    table.set_text("max_codelets", "400")
    assert table.edited_names == ("type_weight", "max_codelets")
    assert table.set_profile("human")
    assert table.edits == {"type_weight": 50, "max_codelets": 400}
    assert table.edited_names == ("max_codelets",)       # 50 is human's own value
    assert table.config() == dataclasses.replace(PROFILES["human"], max_codelets=400)
    rows = {r.name: r for r in table.rows()}
    assert not rows["type_weight"].edited and rows["type_weight"].differs
    assert rows["max_codelets"].edited and not rows["max_codelets"].differs
    assert table.set_profile("standard")
    assert table.edited_names == ("type_weight", "max_codelets")
    assert not table.set_profile("standard")
    with pytest.raises(ct.ConfigError):
        table.set_profile("nope")


def test_reset_and_reset_all_return_to_the_profile(table):
    calls = []
    table.listeners.append(lambda: calls.append((table.profile, dict(table.edits))))
    table.set_profile("buridan")
    table.set_text("max_codelets", "400")
    table.set_text("rho", "2.5")
    assert table.edited_names == ("rho", "max_codelets")
    assert table.reset("max_codelets") and table.edits == {"rho": 2.5}
    assert not table.reset("max_codelets")
    assert table.reset_all() and table.config() == PROFILES["buridan"] and table.edits == {}
    assert not table.reset_all()
    assert calls == [("buridan", {}), ("buridan", {"max_codelets": 400}),
                     ("buridan", {"max_codelets": 400, "rho": 2.5}), ("buridan", {"rho": 2.5}),
                     ("buridan", {})]


def test_set_config_takes_the_profile_and_the_edits_of_a_configuration(table):
    config = dataclasses.replace(PROFILES["human"], max_codelets=400, win_bonus=7)
    assert table.set_config(config)
    assert table.profile == "human" and table.edits == {"max_codelets": 400, "win_bonus": 7}
    assert table.config() == config
    assert not table.set_config(config)
    assert table.set_config(PROFILES["standard"]) and table.edits == {} and table.profile == "standard"
    # a configuration whose profile is no profile keeps the chosen one
    odd = dataclasses.replace(Config(), profile="custom", rho=2.0)
    assert table.set_config(odd) and table.profile == "standard" and table.edits == {"rho": 2.0}


# -- the marks (ANSWERS.md 3) --------------------------------------------------------------------

def test_the_rows_mark_the_faults_the_calibrated_knobs_and_the_differences(table):
    rows = {r.name: r for r in table.rows()}
    for n, (name, value) in enumerate(FAULT_KNOBS, 1):
        assert rows[name].fault == n and rows[name].fault_value == value
        assert f"French's fault {n} (french-faults: {ct.value_text(value)})" in rows[name].marks_text
    assert {r.name for r in rows.values() if r.fault is not None} == {n for n, _ in FAULT_KNOBS}
    for profile, name in CALIBRATION_KNOBS:
        assert rows[name].calibrated == profile and f"calibrated for {profile}" in rows[name].marks_text
    assert all(not r.differs for r in rows.values())
    assert "set by the profile selector" in rows["profile"].marks_text
    table.set_profile("french-faults")
    rows = {r.name: r for r in table.rows()}
    differing = {r.name for r in rows.values() if r.differs}
    assert differing == set(PROFILES["french-faults"].differences(PROFILES["standard"]))
    # v3's `push_reset_set` is French's already (D-114): the other six differ
    assert differing == {n for n, _ in FAULT_KNOBS if n != "push_reset_set"} | {"profile"}
    assert rows["group_salience_points"].profile_value == 15 and rows["group_salience_points"].value == 15
    assert "differs from standard" in rows["group_salience_points"].marks_text
    table.set_profile("human")
    rows = {r.name: r for r in table.rows()}
    assert {r.name for r in rows.values() if r.differs} == {"type_weight", "profile"}


# -- values and texts ----------------------------------------------------------------------------

def test_value_text_and_parse_text_round_trip_every_default(notes):
    for note in notes:
        text = ct.value_text(note.default)
        assert ct.parse_text(text, note.default) == note.default, note.name
        if note.kind == "tuple" and note.default and isinstance(note.default[0], tuple):
            assert text.count("\n") == len(note.default) - 1     # one entry per line
    assert ct.value_text(True) == "true" and ct.value_text(0.5) == "0.5"
    assert ct.value_text("symmetries") == "symmetries" and ct.value_text((20, 50, 80)) == "(20, 50, 80)"
    assert ct.parse_text("yes", True) is True and ct.parse_text("off", True) is False
    assert ct.parse_text(" 2 ", 1) == 2 and ct.parse_text("1", 0.5) == 1.0
    assert ct.parse_text("  symmetries ", "x") == "symmetries"


def test_conform_keeps_the_engines_types_and_refuses_the_rest():
    assert ct.conform([["diagonal", 1]], Config().region_preference) == (("diagonal", 1),)
    assert type(ct.conform([["diagonal", 1]], Config().region_preference)[0][1]) is int
    assert ct.conform(Config().cp_static_weights, Config().cp_static_weights) == Config().cp_static_weights
    assert ct.conform([], Config().depth_by_level) == ()
    assert ct.conform(2, 0.5) == 2.0 and type(ct.conform(2, 0.5)) is float
    assert ct.conform("a", "b") == "a"
    for value, default in ((True, 1), (1.5, 1), ("1", 1), (1, True), ("x", 0.5), (1, ()),
                           ((1,), ("a",)), ((("a",),), (("a", 1),)), (1, "x"), ((1.5,), (1,))):
        with pytest.raises(ct.ConfigError):
            ct.conform(value, default)


# -- a run with the edited configuration (ANSWERS.md 3) --------------------------------------------

def run_with(config, problem="surround-05", seed=3):
    log = Collector()
    result = run_problem(load_problem(problem), config, seed, [log])
    return [record_of(e) for e in log.events], result.draws, result


def test_a_run_under_the_edits_is_the_engines_run_and_records_them(table):
    """ANSWERS.md 3: the engine is not changed, `RunStarted.changed` shows the edit, and
    `config_defaults.lock` and `PROFILES` are untouched."""
    lock_before = hashlib.sha256(LOCK.read_bytes()).hexdigest()
    profiles_before = {name: dataclasses.replace(c) for name, c in PROFILES.items()}
    table.set_profile("human")
    table.set_text("max_codelets", "20")
    edited = table.config()
    assert edited == dataclasses.replace(PROFILES["human"], max_codelets=20)
    records, draws, result = run_with(edited)
    assert (records, draws, result) == run_with(dataclasses.replace(PROFILES["human"], max_codelets=20))
    start = records[0]
    assert start["event"] == "RunStarted" and start["profile"] == "human"
    assert start["changed"] == (("max_codelets", "20"), ("profile", "'human'"), ("type_weight", "50"))
    assert result.capped and result.codelets == 20
    assert records != run_with(PROFILES["human"])[0]        # the edit changed the run
    assert ct.edited_fields(edited) == ("max_codelets",)
    assert ct.config_note_of(edited) == "human: max_codelets=20, type_weight=50"
    assert hashlib.sha256(LOCK.read_bytes()).hexdigest() == lock_before
    assert PROFILES == profiles_before and PROFILES["standard"] == Config()
    assert json.loads(LOCK.read_text())["standard"]["max_codelets"] == "3000"


def test_edited_fields_and_the_configuration_note():
    assert ct.edited_fields(Config()) == () and ct.edited_fields(PROFILES["human"]) == ()
    assert ct.edited_fields(dataclasses.replace(PROFILES["human"], type_weight=30)) == ("type_weight",)
    assert ct.edited_fields(dataclasses.replace(Config(), rho=2.0, max_codelets=5)) == ("rho", "max_codelets")
    assert ct.edited_fields(dataclasses.replace(Config(), profile="custom", rho=2.0)) == ()
    assert ct.config_note_of(Config()) == "standard (the defaults)"
    assert ct.config_note_of(PROFILES["buridan"]) == "buridan: incoherence_penalty=40"   # v3
    assert ct.config_note_of(PROFILES["human"]) == "human: type_weight=50"
    assert ct.config_note_of(dataclasses.replace(Config(), temperature_alpha=100.0)) == \
        "standard: temperature_alpha=100.0"
    start = RunStarted(clock=0, draws=0, problem="surround-05", seed=3, profile="human",
                       engine="compiled", changed=(("max_codelets", "20"), ("profile", "'human'"), ("type_weight", "50")),
                       touched="cup1", objects=("cup1",))
    assert ct.config_note(start.profile, start.changed) == ct.config_note_of(
        dataclasses.replace(PROFILES["human"], max_codelets=20))


# -- QSettings (Numbo GUI) -------------------------------------------------------------------------

def test_the_settings_round_trip(table, notes, titles):
    for name, text, _ in EDITS:
        table.set_text(name, text)
    settings = table.to_settings()
    assert set(settings) == {ct.SETTINGS_KEY}
    pairs = json.loads(settings[ct.SETTINGS_KEY])
    assert [p[0] for p in pairs] == [n for n, _, _ in EDITS]
    other = ct.ConfigTable(notes, titles)
    assert other.from_settings(settings)
    assert other.edits == table.edits and other.config() == table.config()
    assert not ct.ConfigTable(notes, titles).from_settings({})
    assert ct.ConfigTable(notes, titles).edits == {}


@pytest.mark.parametrize("text", ["not json", "{}", "[1]", '[["no_such", 1]]', '[["profile", "human"]]',
                                  '[["max_codelets", "4"]]', '[["depth_by_level", [["a"]]]]',
                                  "42", ""])
def test_bad_settings_are_dropped(notes, titles, text):
    table = ct.ConfigTable(notes, titles)
    table.set_text("max_codelets", "400")
    assert not table.from_settings({ct.SETTINGS_KEY: text})
    # a text that is not a JSON list leaves the edits; a list replaces them
    assert table.edits in ({}, {"max_codelets": 400})
    assert table.config() in (Config(), Config(max_codelets=400))
    assert table.config().profile == "standard"


def test_a_good_pair_survives_a_bad_one(notes, titles):
    table = ct.ConfigTable(notes, titles)
    assert table.from_settings({ct.SETTINGS_KEY: '[["max_codelets", 400], ["x", 1], ["rho", "no"]]'})
    assert table.edits == {"max_codelets": 400}


# -- the view's helpers (G-047) --------------------------------------------------------------------

def test_filter_rows_and_the_tooltip(table, titles):
    table.set_text("max_codelets", "400")
    rows = table.rows()
    assert ct.filter_rows(rows, "") == rows
    assert [r.name for r in ct.filter_rows(rows, "ANSWER_POST")] == ["answer_post_rate", "answer_post_power"]
    assert [r.name for r in ct.filter_rows(rows, "the answer and the run")] == \
        ["group_position_match", "max_codelets"]                 # the section
    assert [r.name for r in ct.filter_rows(rows, "D-061")] == \
        ["answer_patience", "group_position_match", "max_codelets"]   # the section names D-061 too; answer_patience's comment cites it (the cap, D-120, v5)
    assert [r.name for r in ct.filter_rows(rows, "D-064")] == []
    assert {r.name for r in ct.filter_rows(rows, "French's fault")} == {n for n, _ in FAULT_KNOBS}
    assert [r.name for r in ct.filter_rows(rows, "calibrated")] == ["type_weight", "incoherence_penalty"]
    assert [r.name for r in ct.filter_rows(rows, '[F95 p. 130]')] == ["incoherence_penalty"]
    tip = ct.row_tooltip(rows[FIELDS.index("max_codelets")], titles)
    assert tip.startswith("max_codelets (int)") and "section: the answer and the run" in tip
    assert "D-061: The run cap" in tip and tip.endswith("profile's value: 3000")
    tip = ct.row_tooltip(rows[FIELDS.index("group_salience_points")], titles)
    assert "French's fault 3 (french-faults: 15)" in tip and "D-046: " in tip
    assert "profile's value" not in tip
    tip = ct.row_tooltip(rows[FIELDS.index("profile")], titles)
    assert "set by the profile selector" in tip
