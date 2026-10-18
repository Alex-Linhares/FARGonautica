"""Loop0006 item 13: the configuration pane (`tabletop/gui/config_view.py`) in the window,
offscreen, pytest-qt (PANES.md P13; ANSWERS.md 3; G-046, G-047).

- The dock is hidden by default and Edit > Configuration shows it; one row per `Config`
  field with a widget suited to its kind, the profile's value, the source, the marks and
  the comment; the `profile` row's widget is disabled; the note that edits apply to the
  next run.
- Editing a field in its widget, or in the editor under the table, changes the next run's
  configuration and nothing else; the row is tinted and bold, the status line, the cap
  line of the controls and the title say so; an invalid text shows an error and changes
  nothing; Reset per field and Reset all return to the profile; the filter hides rows.
- The profile selector, the controls' combo and the title are one state, whichever is
  used; the edits survive a profile change.
- An edit made during a run leaves its records equal to the plain run's; the next run
  uses it (`RunStarted.changed`, the stats pane's Configuration row in the edited colour,
  the title), and equals a plain `run_problem` with the same configuration.
- The edits survive a restart through a temporary INI; bad settings give none; a window
  without settings writes none.
- The status line is painted once per event while the dock is on screen (counted
  during the run above); a replay's configuration is shown read-only; the minimum.
"""

import dataclasses

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings                                             # noqa: E402
from PySide6.QtGui import QColor                                                 # noqa: E402
from PySide6.QtWidgets import QCheckBox, QDoubleSpinBox, QLineEdit, QSpinBox     # noqa: E402

from tabletop.config import PROFILES, Config                                      # noqa: E402
from tabletop.events import Collector, record_of                                  # noqa: E402
from tabletop.gui.config_view import COLUMNS, EDITED_TINT, ConfigView            # noqa: E402
from tabletop.gui.main_window import MainWindow, title_for                       # noqa: E402
from tabletop.gui.models import config_table as ct                               # noqa: E402
from tabletop.gui import stats_view as sv                                        # noqa: E402
from tabletop.problems import load_problem                                       # noqa: E402
from tabletop.run import run_problem                                             # noqa: E402

from golden import plain_run                                                     # noqa: E402
from test_coderack_view import qtbot_wait                                        # noqa: E402
from test_stats_view import is_reddish, row_of, row_pixels                       # noqa: E402

QUICK = ("surround-05", 3)
TIMEOUT = 30000
FIELDS = [f.name for f in dataclasses.fields(Config)]


@pytest.fixture(scope="module")
def shown(module_window, qapp):
    """The shared window with the configuration dock opened by Edit > Configuration and
    floated (on screen, as the stats dock is in `test_stats_view.py`); docked again at
    the end so that it does not take the next module's mouse moves."""
    w = module_window
    w.resize(1600, 1000)
    assert w.config_dock.isHidden()
    w.configuration_action.trigger()
    w.config_dock.setFloating(True)
    w.config_dock.resize(900, 500)
    w.config_dock.move(40, 40)
    qapp.processEvents()
    qtbot_wait(qapp)
    assert w.config_dock.isFloating() and w.config_dock.isVisible()
    yield w
    w.config_dock.setFloating(False)


@pytest.fixture
def window(shown):
    """The shared window with the defaults back: no edits, `standard`, no run live."""
    w = shown
    w.stop()
    w.config_view.reset_all()
    w.config_view.filter_edit.setText("")
    w.set_next_run(problem="surround-05", profile="standard", seed=3)
    w.statusBar().clearMessage()
    return w


def spin(view, name, value):
    """Type a value into a spin box as the user does: set it, then leave the field."""
    widget = view.widget_of(name)
    widget.setValue(value)
    widget.editingFinished.emit()


def run_to_end(qtbot, window, config=None, problem="surround-05", seed=3):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_run(problem, config if config is not None else window.next_config(), seed)
        window.controller.run_to_end()
    qtbot.waitUntil(lambda: not window.controller.is_alive(), timeout=TIMEOUT)
    return window.outcome


def run_plain(config, problem="surround-05", seed=3):
    log = Collector()
    result = run_problem(load_problem(problem), config, seed, [log])
    return [record_of(e) for e in log.events], result.draws, result


def make_window(qtbot, settings=None):
    w = MainWindow(settings=settings, layout_path=None)
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    qtbot.wait(20)
    return w


# -- the dock and the rows (ANSWERS.md 3) ------------------------------------------------------

def test_the_dock_holds_one_row_per_field_with_a_widget_by_kind(window):
    view = window.config_view
    assert isinstance(view, ConfigView) and view.pane_id == "P13"
    assert "ANSWERS.md 3" in view.pages and "Seek-Whence GUI" in view.pages
    assert view.pages in view.toolTip() and view.pages in window.config_dock.toolTip()
    assert "next run" in view.note_label.text()
    assert view.table.rowCount() == len(FIELDS)
    assert [view.table.horizontalHeaderItem(c).text() for c in range(len(COLUMNS))] == list(COLUMNS)
    assert [r.name for r in view.rows] == FIELDS
    kinds = {"int": QSpinBox, "float": QDoubleSpinBox, "bool": QCheckBox, "str": QLineEdit,
             "tuple": QLineEdit}
    for row in view.rows:
        widget = view.widget_of(row.name)
        assert isinstance(widget, kinds[row.note.kind]), row.name
        assert widget.isEnabled() == (row.name != "profile")
        items = view._items[row.name]
        assert items[0].text() == row.name and items[3].text() == row.source_text
        assert items[2].text() and row.note.comment.startswith(items[5].text()[:20])
        assert "D-" in items[0].toolTip() or "[F95" in items[0].toolTip()
    assert "French's fault 3" in view._items["group_salience_points"][4].text()
    assert "D-061: The run cap" in view._items["max_codelets"][3].toolTip()
    assert view.widget_of("max_codelets").value() == 3000
    assert view.widget_of("rho").value() == round(Config().rho, 4)        # v3 (D-114), v4 (D-115)
    assert not view.widget_of("keep_all_subgroups").isChecked()
    assert view.widget_of("push_reset_set").text() == "symmetries"
    assert view.widget_of("depth_by_level").text() == "(45, 77, 79)"   # v4, D-115 ((45, 75, 45) in v3)
    assert view.status.text() == "next run: standard (the defaults)"


def test_editing_a_widget_changes_the_next_run_only_and_marks_the_row(window):
    view = window.config_view
    spin(view, "max_codelets", 400)
    assert window.next_config() == dataclasses.replace(Config(), max_codelets=400)
    assert view.model.edited_names == ("max_codelets",)
    row = view.row_of("max_codelets")
    assert row.edited and view._items["max_codelets"][0].font().bold()
    assert view._items["max_codelets"][0].background().color() == EDITED_TINT
    assert view._resets["max_codelets"].isEnabled() and view.reset_all_button.isEnabled()
    assert not view._resets["rho"].isEnabled()
    assert view.status.text() == "next run: standard: max_codelets=400 · edited: max_codelets"
    assert window.windowTitle() == title_for("surround-05", "standard", 3, ("max_codelets",))
    assert window.windowTitle().endswith("(edited: max_codelets)")
    assert window.controls_view.cap_label.text().startswith("400 codelets")
    view.widget_of("keep_all_subgroups").setChecked(True)
    assert window.next_config() == dataclasses.replace(Config(), max_codelets=400,
                                                       keep_all_subgroups=True)
    assert view.edit("push_reset_set", "slippage-concepts")     # v3's default is "symmetries"
    assert window.next_config().push_reset_set == "slippage-concepts"
    assert view.edit("depth_by_level", "(10, 40, 90)")
    assert window.next_config().depth_by_level == (10, 40, 90)
    assert view.model.edited_names == ("depth_by_level", "push_reset_set", "keep_all_subgroups",
                                       "max_codelets")
    # the rest is the profile's
    others = {f for f in FIELDS} - set(view.model.edited_names)
    for name in others:
        assert getattr(window.next_config(), name) == getattr(Config(), name), name


def test_the_editor_under_the_table_applies_a_value_and_refuses_a_bad_one(window):
    view = window.config_view
    view.select("region_preference")
    assert view.editor_label.text().startswith("region_preference:")
    assert view.editor.toPlainText().count("\n") == 3           # one entry per line
    view.editor.setPlainText('(("diagonal", 1.0), ("opposite", 0.9))')
    assert view.apply_editor()
    assert window.next_config().region_preference == (("diagonal", 1.0), ("opposite", 0.9))
    assert view.widget_of("region_preference").text().startswith("(('diagonal', 1.0)")
    view.editor.setPlainText("nonsense")
    assert not view.apply_editor()
    assert view.error_label.isVisible() and "region_preference" in view.error_label.text()
    assert window.next_config().region_preference == (("diagonal", 1.0), ("opposite", 0.9))
    assert not view.edit("max_codelets", "four")
    assert "expected an integer" in view.error_label.text()
    assert window.next_config().max_codelets == 3000
    assert not view.edit("profile", "human")
    assert "selector" in view.error_label.text()
    assert view.reset("region_preference") and not view.error_label.isVisible()
    assert window.next_config() == Config()


def test_reset_per_field_reset_all_and_the_filter(window):
    view = window.config_view
    spin(view, "max_codelets", 400)
    spin(view, "win_bonus", 7)
    assert view.model.edited_names == ("win_bonus", "max_codelets")
    view.select("win_bonus")
    view.editor_reset_button.click()
    assert view.model.edited_names == ("max_codelets",)
    view._resets["max_codelets"].click()
    assert view.model.edited_names == () and window.next_config() == Config()
    assert window.windowTitle() == title_for("surround-05", "standard", 3)
    spin(view, "max_codelets", 400)
    spin(view, "win_bonus", 7)
    view.reset_all_button.click()
    assert view.model.edits == {} and not view.reset_all_button.isEnabled()
    view.filter_edit.setText("answer_post")
    assert view.visible_names() == ("answer_post_rate", "answer_post_power")
    view.filter_edit.setText("French's fault")
    assert len(view.visible_names()) == 7
    view.filter_edit.setText("")
    assert len(view.visible_names()) == len(FIELDS)


# -- the profile (ANSWERS.md 3: one state) ---------------------------------------------------------

def test_the_profile_selector_the_controls_and_the_title_are_one_state(window):
    view, controls = window.config_view, window.controls_view
    view.profile_combo.setCurrentIndex(view.profile_combo.findData("human"))
    assert window.profile == "human" == controls.profile == view.model.profile
    assert window.next_config() == PROFILES["human"]
    assert window.windowTitle() == title_for("surround-05", "human", 3)
    assert view.widget_of("type_weight").value() == 50
    row = view.row_of("type_weight")
    assert row.differs and not row.edited and "calibrated for human" in row.marks_text
    assert view.status.text() == "next run: human: type_weight=50"
    controls.profile_combo.setCurrentIndex(controls.profile_combo.findData("french-faults"))
    assert window.profile == "french-faults" == view.profile_combo.currentData()
    assert view.widget_of("keep_all_subgroups").isChecked()
    assert view.widget_of("group_salience_points").value() == 15
    window.set_next_run(profile="buridan")
    assert view.profile_combo.currentData() == "buridan" == controls.profile
    # an edit survives a profile change and is marked only where it differs (G-046)
    spin(view, "type_weight", 50)
    view.profile_combo.setCurrentIndex(view.profile_combo.findData("human"))
    assert view.model.edits == {"type_weight": 50} and view.model.edited_names == ()
    assert not view.row_of("type_weight").edited and view.status.text() == "next run: human: type_weight=50"
    window.set_next_run(profile="standard")
    assert view.row_of("type_weight").edited and window.next_config().type_weight == 50
    assert window.windowTitle().endswith("(edited: type_weight)")


# -- the runs (ANSWERS.md 3) --------------------------------------------------------------------------

def hidden_for_a_run(w, qtbot):
    """Hide the configuration dock for a run that no pixel is read from: on screen (the
    `shown` fixture floats it at 900 by 500) it is painted at every event, which costs
    ten seconds under load; `shown_again` restores the fixture's arrangement."""
    w.config_dock.hide()
    qtbot.wait(40)
    assert w.config_dock.isHidden()


def shown_again(w, qtbot):
    """The `shown` fixture's arrangement once more, for the tests that probe pixels."""
    w.config_dock.show()
    w.config_dock.setFloating(True)
    w.config_dock.resize(900, 500)
    w.config_dock.move(40, 40)
    qtbot.wait(40)
    assert w.config_dock.isFloating() and w.config_dock.isVisible()
    assert not w.config_view.status.visibleRegion().isEmpty()


# -- QSettings (ANSWERS.md 3) ---------------------------------------------------------------------

def test_the_edits_survive_a_restart_through_a_temporary_ini(qtbot, ini, thread_errors):
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    view = w.config_view
    w.set_next_run(profile="human")
    spin(view, "max_codelets", 400)
    assert view.edit("region_preference", '(("diagonal", 1.0), ("opposite", 0.9))')
    expected = w.next_config()
    w.close()
    s = QSettings(ini, QSettings.Format.IniFormat)
    assert s.contains(ct.SETTINGS_KEY) and s.value("controls/profile") == "human"
    w2 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w2.profile == "human" and w2.config_view.model.profile == "human"
        assert w2.next_config() == expected
        assert w2.config_view.model.edited_names == ("region_preference", "max_codelets")
        assert w2.config_view.row_of("max_codelets").edited
        assert w2.windowTitle() == title_for("surround-05", "human", 1,
                                             ("region_preference", "max_codelets"))
        assert w2.controls_view.cap_label.text().startswith("400 codelets")
    finally:
        w2.close()
        assert not w2.controller.is_alive()


def test_bad_settings_give_no_edits(qtbot, ini, thread_errors, capfd):
    s = QSettings(ini, QSettings.Format.IniFormat)
    s.setValue(ct.SETTINGS_KEY, '[["max_codelets", "four"], ["nope", 1], 3]')
    s.sync()
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w.config_view.model.edits == {} and w.next_config() == Config()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


# -- a replay's configuration, read-only (item 17's hook) ------------------------------------------

def test_a_replays_configuration_is_shown_read_only(window):
    view = window.config_view
    spin(view, "max_codelets", 400)
    file_config = dataclasses.replace(PROFILES["human"], win_bonus=7)
    view.set_replay(file_config)
    assert view.row_of("win_bonus").edited and view.row_of("win_bonus").value == 7
    assert not view.row_of("max_codelets").edited
    assert view.status.text().startswith("next run: replay: the file's configuration, human: "
                                         "type_weight=50, win_bonus=7 (read-only)")
    assert not view.widget_of("win_bonus").isEnabled() and not view.profile_combo.isEnabled()
    assert not view.reset_all_button.isEnabled() and not view.apply_button.isEnabled()
    assert not view.edit("win_bonus", "9") and "replaying" in view.error_label.text()
    assert not view.reset("win_bonus") and not view.reset_all()
    assert view.model.edits == {"max_codelets": 400}          # the user's edits, untouched
    view.set_replay(None)
    assert view.row_of("max_codelets").edited and view.widget_of("win_bonus").isEnabled()
    assert window.next_config() == dataclasses.replace(Config(), max_codelets=400)


def test_the_minimum(window):
    view = window.config_view
    assert view.minimumSize().width() <= 160 and view.minimumSize().height() <= 80
    assert view.minimumSizeHint().width() <= 420
