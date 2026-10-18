"""Loop0004 item 15: the configuration pane (`seekwhence/gui/config_view.py`) in the
window, offscreen, pytest-qt (PANES.md P13; ANSWERS.md 5; G-050, G-051).

- The dock is hidden by default and Edit > Configuration shows it; one row per `Config`
  field with a widget suited to its type, the controls' three fields shown and not
  editable, the note that edits apply to the next run.
- Editing a field in its widget, or in the editor under the table, changes the next run's
  configuration and nothing else; the row is tinted, the title and the status say which
  fields are edited; an invalid text shows an error and changes nothing; Reset per field
  and Reset all return to `Config()`; the filter hides rows.
- An edit made during a run does not change that run (its events equal the plain run's);
  the next run uses it, its `start` event's config shows it and the stats pane says
  `edited: ...`, and it equals a plain `Session` with the same configuration.
- A session saved with an edited configuration replays event for event, the pane showing
  the file's configuration read-only, and the edits come back after Close replay.
- The edits survive a restart through a temporary INI; bad settings give the defaults.
- The pane on screen is painted once per event (at most a few extra), and renders.
"""

import dataclasses
import io
import json

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings                                  # noqa: E402
from PySide6.QtWidgets import QCheckBox, QDoubleSpinBox, QLineEdit, QSpinBox   # noqa: E402

from seekwhence.config import Config                                  # noqa: E402
from seekwhence.events import Recorder                                # noqa: E402
from seekwhence.gui.config_view import COLUMNS                        # noqa: E402
from seekwhence.gui.main_window import MainWindow, PANES, title_for   # noqa: E402
from seekwhence.gui.models import config_table as ct                  # noqa: E402
from seekwhence.gui.models import presets                             # noqa: E402
from seekwhence.session import Session                                # noqa: E402
from seekwhence.trace import event_record                             # noqa: E402

from golden import APPENDIX                                           # noqa: E402

TIMEOUT = 60000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
FIELDS = [f.name for f in dataclasses.fields(Config)]


def records_of(window):
    return [event_record(e) for e in window.history.events]


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def run_plain(inputs):
    rec, out = Recorder(), io.StringIO()
    s = Session(inputs.config, inputs.seed, out=out, lines=list(inputs.lines),
                answers=list(inputs.answers), observers=[rec])
    s.run()
    return [event_record(e) for e in rec.events], s.world.rng.draws, s.outcome


def make_window(qtbot, settings=None):
    w = MainWindow(settings=settings, layout_path=None)
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


def spin(view, name, value):
    """Type a value into a spin box as the user does: set it, then leave the field."""
    w = view.widget_of(name)
    w.setValue(value)
    w.editingFinished.emit()


# -- the dock and the rows (PANES.md P13) --------------------------------------------------------------

def test_the_dock_is_hidden_until_edit_configuration(window):
    view = window.config_view
    assert window.config_dock.isHidden() and window.config_dock.widget() is view
    assert view.pane_id == "P13" and view.title == "Configuration"
    assert [attr for attr, *_ in PANES if attr == "config"] == ["config"]
    window.configuration_action.trigger()
    assert window.config_dock.isVisible()
    assert view.table.rowCount() == len(FIELDS)
    assert [view.table.horizontalHeaderItem(c).text() for c in range(len(COLUMNS))] == list(COLUMNS)
    assert [view.table.item(r, 0).text() for r in range(len(FIELDS))] == FIELDS
    assert "next run" in view.note_label.text() and "seed" in view.note_label.text()
    assert view.status_label.text() == "next run: default"
    kinds = {name: type(view.widget_of(name)) for name in FIELDS}
    assert kinds["urgency_power"] is QSpinBox and kinds["stay_factor_per_try"] is QDoubleSpinBox
    assert kinds["verbose"] is QCheckBox and kinds["s_links"] is QLineEdit
    for name in ct.CONTROLLED:
        assert not view.widget_of(name).isEnabled()
        assert "Controls pane" in view.widget_of(name).toolTip()
    assert view.widget_of("guess_min_hits").isEnabled()
    assert view.widget_of("guess_min_hits").value() == 3
    assert view.table.item(FIELDS.index("urgency_power"), 3).text() == "D-007"
    assert "D-007: Taskrack sampling rule" in view.table.item(FIELDS.index("urgency_power"), 3).toolTip()
    assert "[M86 p. 105]" in view.table.item(FIELDS.index("threshold_fraction"), 3).text()
    assert view.table.item(FIELDS.index("s_links"), 2).text().startswith("(('S-group', 'Countup', 0.1)")
    for name in ("urgency_power", "guess_min_hits", "s_links"):
        assert not view._resets[name].isEnabled()
    assert not view.reset_all_button.isEnabled()


# -- editing (ANSWERS.md 5) ---------------------------------------------------------------------------

def test_editing_a_field_changes_the_next_run_only_that_field(window):
    view = window.config_view
    spin(view, "guess_min_hits", 4)
    assert window.inputs.config == Config(guess_min_hits=4)
    assert window.controls.base_config == Config(guess_min_hits=4)
    assert window.windowTitle().endswith("[edited: guess_min_hits]")
    assert view.status_label.text() == "next run: edited: guess_min_hits"
    row = view.row_of("guess_min_hits")
    assert row.edited and view.table.item(FIELDS.index("guess_min_hits"), 0).font().bold()
    assert view._resets["guess_min_hits"].isEnabled() and view.reset_all_button.isEnabled()
    assert "default: 3" in view.widget_of("guess_min_hits").toolTip()
    # a check box, a decimal spin box and a table's line
    view.widget_of("reject_freeze_dried_repeats").setChecked(False)
    spin(view, "slip_scout_factor", 0.75)
    line = view.widget_of("strict_classes")
    line.setText('("Constant",)')
    line.editingFinished.emit()
    assert window.inputs.config == Config(guess_min_hits=4, reject_freeze_dried_repeats=False,
                                          slip_scout_factor=0.75, strict_classes=("Constant",))
    assert window.windowTitle().endswith(
        "[edited: guess_min_hits, reject_freeze_dried_repeats, strict_classes, slip_scout_factor]")
    # the controls' caps and trace stay the controls'
    c = window.controls
    c.cap_term_edit.setText("500")
    c.cap_term_edit.editingFinished.emit()
    assert window.inputs.config.max_tasks_per_term == 500 and window.inputs.config.guess_min_hits == 4
    assert view.row_of("max_tasks_per_term").value == 500 and view.row_of("max_tasks_per_term").edited
    # a preset chosen keeps the edits, with the preset's trace
    window.choose_preset(RUN2)
    assert window.inputs.config.guess_min_hits == 4 and not window.inputs.config.verbose
    assert window.inputs.config.max_tasks_per_term == 500
    window.choose_interactive()
    assert window.inputs.config.guess_min_hits == 4 and window.inputs.interactive


def test_the_editor_under_the_table_applies_and_refuses(window):
    view = window.config_view
    view.select("s_links")
    assert view.editor_label.text().startswith("s_links:")
    text = view.editor.toPlainText()
    assert text.count("\n") == len(Config().s_links) - 1 and text.startswith("(('S-group', 'Countup', 0.1)")
    view.editor.setPlainText('(("S-group", "Countup", 1.0),\n ("Cycle", "Tuple", 0.5))')
    assert view.apply_button.isEnabled()
    view.apply_button.click()
    assert window.inputs.config.s_links == (("S-group", "Countup", 1.0), ("Cycle", "Tuple", 0.5))
    assert view.error_label.isHidden()
    # an invalid text: an error, nothing changed
    before = window.inputs.config
    view.editor.setPlainText('(("S-group", "Countup"),)')
    view.apply_button.click()
    assert not view.error_label.isHidden() and view.error_label.text().startswith("s_links: expected")
    assert window.inputs.config == before
    line = view.widget_of("guess_validity")
    assert not view.edit("guess_validity", "x")
    assert view.error_label.text().startswith("guess_validity: expected an integer")
    assert window.inputs.config == before
    assert not view.edit("verbose", "false") and "Controls pane" in view.error_label.text()
    # the editor's Reset
    view.editor_reset_button.click()
    assert window.inputs.config == Config() and not view.error_label.isVisible()
    assert window.windowTitle().endswith('[M86 pp. 164-166]')
    # nothing selected: Apply says so
    view.table.clearSelection()
    assert not view.apply_editor() and "select" in view.error_label.text()


def test_reset_per_field_reset_all_and_the_filter(window):
    view = window.config_view
    spin(view, "guess_min_hits", 4)
    spin(view, "testers_per_spark", 2)
    assert window.inputs.config == Config(guess_min_hits=4, testers_per_spark=2)
    view._resets["testers_per_spark"].click()
    assert window.inputs.config == Config(guess_min_hits=4)
    assert not view.row_of("testers_per_spark").edited
    view.reset_all_button.click()
    assert window.inputs.config == Config() and window.config_table.edits == {}
    assert view.status_label.text() == "next run: default"
    assert not window.windowTitle().endswith("]") or window.windowTitle().endswith('[M86 pp. 164-166]')
    assert title_for(window.inputs) == window.windowTitle()
    view.filter_edit.setText("guess_")
    assert view.visible_names() == ("guess_validity", "guess_min_hits")
    view.filter_edit.setText("D-019")
    assert view.visible_names() == ("bond_type_weights", "bond_modifier_weights",
                                    "bond_field_weights", "glom_bond_minimum")
    view.filter_edit.setText("")
    assert len(view.visible_names()) == len(FIELDS)


# -- the run in progress is never changed (ANSWERS.md 5) ------------------------------------------------

def test_an_edit_during_a_run_changes_the_next_run_not_this_one(window, qtbot, appendix_runs):
    view = window.config_view
    inputs = presets.appendix_inputs(RUN3)
    window.set_inputs(inputs)
    window.start_session(inputs, paused=True)
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    for _ in range(5):
        window.controller.step()
        qtbot.waitUntil(lambda: window.position >= 0, timeout=TIMEOUT)
    assert window.history.events[0].kind == "start"
    spin(view, "guess_min_hits", 4)
    assert window.inputs.config == Config(guess_min_hits=4)
    assert view.status_label.text().endswith("run in progress: default")
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.controller.run_to_end()
    reference = appendix_runs[RUN3]
    assert records_of(window) == reference.records
    assert window.outcome.result["draws"] == reference.draws
    assert window.stats_view.text("config") == "default"
    # the next run uses the edit
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.run_to_end()
    records = records_of(window)
    assert dict(records[0]["config"])["guess_min_hits"] == 4
    assert window.stats_view.text("config") == "edited: guess_min_hits"
    assert view.status_label.text() == ("next run: edited: guess_min_hits\n"
                                        "run in progress: edited: guess_min_hits")
    assert window._started.config == Config(guess_min_hits=4)
    plain = run_plain(window._started)
    assert (records, window.outcome.result["draws"], window.outcome.result["outcome"]) == plain
    assert records != reference.records


# -- the session file (ANSWERS.md 5; G-046, G-047) ------------------------------------------------------

def test_a_session_with_an_edited_configuration_replays_with_it(window, qtbot, tmp_path):
    view = window.config_view
    spin(view, "guess_min_hits", 4)
    spin(view, "sparklers_per_term", 2)
    inputs = window.inputs
    assert inputs.config == Config(guess_min_hits=4, sparklers_per_term=2)
    run_to_end(qtbot, window, inputs)
    records = records_of(window)
    path = tmp_path / "edited.jsonl"
    assert window.save_session(path)
    run_line = json.loads(path.read_text().splitlines()[1])["run"]
    assert dict(run_line["config"])["guess_min_hits"] == 4
    # the edits are changed before the replay: the file's configuration must win
    view.reset_all_button.click()
    spin(view, "guess_validity", 2)
    assert window.inputs.config == Config(guess_validity=2)
    assert window.open_session(str(path))
    qtbot.waitUntil(lambda: window.state == "paused" and window.position == 0, timeout=TIMEOUT)
    assert view.replay == Config(guess_min_hits=4, sparklers_per_term=2)
    assert view.status_label.text().startswith(
        "replay: the file's configuration, edited: sparklers_per_term, guess_min_hits (read-only)")
    assert not view.widget_of("guess_min_hits").isEnabled() and not view.reset_all_button.isEnabled()
    assert view.row_of("guess_min_hits").value == 4 and view.row_of("guess_validity").value == 1
    assert not view.edit("guess_min_hits", "5") and "replaying" in view.error_label.text()
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.run_to_end()
    assert records_of(window) == records
    assert window._replay.checker.finish(stopped=False) is None
    assert "verified" in window.statusBar().currentMessage()
    assert dict(records_of(window)[0]["config"])["guess_min_hits"] == 4
    window.close_replay()
    assert view.replay is None and window.inputs.config == Config(guess_validity=2)
    assert view.widget_of("guess_min_hits").isEnabled() and view.row_of("guess_validity").edited
    assert window.windowTitle().endswith("[edited: guess_validity]")


# -- QSettings (ANSWERS.md 5; Numbo GUI) ------------------------------------------------------------------

def test_the_edits_survive_a_restart_through_a_temporary_ini(qtbot, ini, thread_errors):
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    spin(w.config_view, "guess_min_hits", 4)
    w.config_view.widget_of("reject_freeze_dried_repeats").setChecked(False)
    w.config_view.edit("s_links", '(("S-group", "Countup", 1),)')
    expected = Config(guess_min_hits=4, reject_freeze_dried_repeats=False,
                      s_links=(("S-group", "Countup", 1.0),))
    assert w.inputs.config == expected
    w.close()
    s = QSettings(ini, QSettings.Format.IniFormat)
    pairs = json.loads(s.value(ct.SETTINGS_KEY))
    assert [p[0] for p in pairs] == ["guess_min_hits", "reject_freeze_dried_repeats", "s_links"]
    w2 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w2.config_table.edits == w.config_table.edits
        assert w2.inputs.config == expected
        assert w2.controls.base_config == expected
        assert w2.config_view.row_of("guess_min_hits").value == 4
        assert w2.windowTitle().endswith(
            "[edited: guess_min_hits, reject_freeze_dried_repeats, s_links]")
        w2.config_view.reset_all_button.click()
        assert w2.inputs.config == Config()
    finally:
        w2.close()
        assert not w2.controller.is_alive()
    s = QSettings(ini, QSettings.Format.IniFormat)
    assert json.loads(s.value(ct.SETTINGS_KEY)) == []


def test_bad_config_settings_give_the_defaults(qtbot, ini, thread_errors, capfd):
    s = QSettings(ini, QSettings.Format.IniFormat)
    s.setValue(ct.SETTINGS_KEY, '[["guess_min_hits", "four"], ["verbose", false], ["x", 1], 7]')
    s.sync()
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w.config_table.edits == {} and w.inputs.config == Config()
        assert w.windowTitle().endswith('[M86 pp. 164-166]')
    finally:
        w.close()
    s = QSettings(ini, QSettings.Format.IniFormat)
    s.setValue(ct.SETTINGS_KEY, "not json")
    s.sync()
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w.config_table.edits == {} and w.inputs.config == Config()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


# -- painting (G-011) -----------------------------------------------------------------------------------

def test_the_pane_on_screen_is_painted_once_per_event_and_renders(window, qtbot, tmp_path):
    view = window.config_view
    window.show_configuration()
    qtbot.waitUntil(lambda: not view.visibleRegion().isEmpty(), timeout=TIMEOUT)
    before = {}
    anomalies = []

    class Probe:
        def on_event(self, event):
            before["paints"] = view.paints
    window.add_view(Probe())
    window.event_drawn.connect(
        lambda index: anomalies.append(index) if view.paints - before["paints"] < 1 else None)
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert anomalies == []
    assert view.events == len(window.history.events) == 174
    assert view.status_label.text().endswith("run in progress: default")
    path = str(tmp_path / "config.png")
    view.render_png(path)
    assert (tmp_path / "config.png").stat().st_size > 1000
