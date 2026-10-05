"""The final audit: the Qt GUI against the tkinter GUI's inventory, entry by
entry (loop0003 item 11).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

python/tests/data/tk-gui-inventory.json (item 00) lists every element of the
tkinter GUI.  The menus, the control panel's five states, the Help, Clear
Memory and theme-edit dialogs, Save commentary and the bindings are compared
with the Qt GUI by test_qt_menus.py and test_qt_clicks.py.  This file covers
the rest, through drive_qt_audit.py (a fresh offscreen process):

- every graphics window is a pane with the inventory's panel, scrolling,
  aspect, resize method, visibility, press handlers and background;
- the Logo is the window icon;
- the speed slider gives the inventory's settings at each recorded value;
- the two input dialogs open at their places and read Enter as gui.ss does;
- the buttons run gui.py's actions, and closing the window quits.

Then a map from each inventory entry to the tests that check it, so that a new
inventory entry without a Qt test fails here; and, at run time, that the engine
imports neither PySide6 nor tkinter.
"""
from __future__ import annotations

import json
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

pytest.importorskip("PySide6.QtWidgets")

HERE = Path(__file__).resolve().parent
PY = HERE.parent
OUT = HERE / "screenshots-qt" / "audit"
SCRIPT = HERE / "drive_qt_audit.py"
INVENTORY = json.loads((HERE / "data" / "tk-gui-inventory.json").read_text())
SCENARIOS = ["windows", "speed-slider", "input-dialogs", "buttons"]

# the panes' keys that must equal the inventory's.  The sizes are not compared:
# a pane gives its panel the pane's size through the original's resize protocol
# (docs/divergences.md, "Python Qt GUI: windows as panes"), nor are the Tk
# toplevel's geometry, wm resizable, wm minsize and canvas bindings, which a
# pane doesn't have (its presses are test_qt_clicks.py's)
SAME = ["title", "global", "panel_class", "draws_in", "scrolling", "scrollbars", "resize_method",
        "visible_at_start", "left_press", "right_press", "shift_left_press", "background"]


@pytest.fixture(scope="module")
def audit():
    OUT.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ)
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run([sys.executable, str(SCRIPT), str(OUT)], capture_output=True,
                          text=True, timeout=600, env=env, cwd=str(PY))
    oks = [line.split()[1] for line in proc.stdout.splitlines() if line.startswith("ok ")]
    rep = json.loads((OUT / "audit.json").read_text())
    rep["_oks"] = oks
    rep["_proc"] = (proc.returncode, proc.stdout[-4000:], proc.stderr[-4000:])
    return rep


def test_every_audit_step_ran(audit):
    code, out, err = audit["_proc"]
    assert code == 0 and audit["_oks"] == SCENARIOS, out + err


# --- the windows -------------------------------------------------------------------

def tk_windows():
    return [w for w in INVENTORY["windows"] if w["name"] != "Logo"]


def test_every_window_is_a_pane(audit):
    assert [w["name"] for w in audit["windows"]] == [w["name"] for w in tk_windows()]
    for w in audit["windows"]:
        assert w["pane"] and w["in_splitter"] and w["view_item"], w


@pytest.mark.parametrize("name", [w["name"] for w in tk_windows()])
def test_each_pane_has_the_inventorys_panel(audit, name):
    (tk,) = [w for w in tk_windows() if w["name"] == name]
    (qt,) = [w for w in audit["windows"] if w["name"] == name]
    assert {k: qt[k] for k in SAME} == {k: tk[k] for k in SAME}


def test_unscrollable_panes_keep_tks_aspect_ratio(audit):
    for tk, qt in zip(tk_windows(), audit["windows"]):
        assert qt["aspect"] == (tk["aspect"][:2] if tk["aspect"] else None), tk["name"]


def test_the_logo_is_the_window_icon(audit):
    (logo,) = [w for w in INVENTORY["windows"] if w["name"] == "Logo"]
    assert logo["visible_at_start"] is False      # a hidden window in the tkinter GUI
    assert audit["icon"]["window"] and 256 in audit["icon"]["sizes"]


# --- the control strip -------------------------------------------------------------

def test_the_speed_slider(audit):
    tk, qt = INVENTORY["speed_slider"], audit["speed_slider"]
    assert (qt["from"], qt["to"], qt["value"]) == (tk["from"], tk["to"], tk["initial"])
    assert qt["labels"] == tk["labels"]
    assert qt["start"] == tk["sets"][str(tk["initial"])]
    assert qt["sets"] == tk["sets"]


@pytest.mark.parametrize("opened_by", ["Options > Set breakpoint",
                                       "Options > Step mode interval"])
def test_the_input_dialogs(audit, opened_by):
    (tk,) = [d for d in INVENTORY["dialogs"] if d["opened_by"] == opened_by]
    qt = audit["dialogs"][opened_by]
    assert tk["title"] == "Input"                    # the driver looked it up by this title
    assert qt["offset"] == tk["geometry_offset_from_control_panel"]
    assert qt["dialogs_after_second_trigger"] == 1
    # "not a number >= 1: 'Invalid input!' in red for 700 ms"
    for bad in qt["bad"]:
        assert bad["message"] == ["Invalid input!"] and bad["open"] == 1, bad
        assert "#ff0000" in bad["colour"]
    assert qt["message_after_700ms"] == [qt["message"][0]] != ["Invalid input!"]
    # "empty: close"; "otherwise set the value and close"
    assert qt["empty"] == {"open": 0, "value_unchanged": True}
    assert qt["number"] == {"open": 0, "value": 123}
    if opened_by == "Options > Set breakpoint":
        assert qt["breakpoint_label"] == "Breakpoint set for time step 123"


def test_the_buttons_run_gui_pys_actions(audit):
    tk = INVENTORY["buttons"]
    assert {k: v["action"] for k, v in audit["buttons"].items()} == \
        {k: tk[k] for k in ("Step", "Go", "Stop", "Reset")}
    # "Enter: same as Go in input mode"
    assert audit["command_line_action"] == tk["Go"]


def test_closing_the_window_quits(audit):
    # the control panel's close was gui._exit; the main window is the last window
    assert INVENTORY["control_panel"]["close"].startswith("gui._exit")
    assert audit["quit_on_last_window_closed"] is True


# --- every inventory entry has its Qt test -----------------------------------------

COVERAGE = {
    "windows": ["test_qt_audit.py::test_each_pane_has_the_inventorys_panel",
                "test_qt_audit.py::test_the_logo_is_the_window_icon",
                "test_qt_panes.py::test_run7_in_the_panes_gives_its_golden",
                "test_qt_layout.py::test_the_window_icon_is_the_logo"],
    "bindings": ["test_qt_clicks.py::test_the_inventory_has_exactly_these_bindings",
                 "test_qt_clicks.py::test_the_clicks_in_the_qt_gui_do_what_they_do_in_the_tkinter_gui"],
    "buttons": ["test_qt_audit.py::test_the_buttons_run_gui_pys_actions",
                "test_qt_engine.py::test_every_gui_scenario_in_the_qt_gui_gives_its_golden"],
    "control_panel": ["test_qt_menus.py::test_the_control_panel_matches_the_tkinter_gui",
                      "test_qt_audit.py::test_closing_the_window_quits"],
    "dialogs": ["test_qt_audit.py::test_the_input_dialogs",
                "test_qt_menus.py::test_the_help_window",
                "test_qt_menus.py::test_the_clear_memory_dialog",
                "test_qt_menus.py::test_the_theme_edit_dialog",
                "test_qt_menus.py::test_save_commentary_uses_the_originals_title",
                "test_qt_engine.py::test_invalid_input_shows_for_700_ms"],
    "menus": ["test_qt_menus.py::test_the_menu_bar_has_every_item_of_the_inventory",
              "test_qt_menus.py::test_every_demo_inits_its_problem"],
    "speed_slider": ["test_qt_audit.py::test_the_speed_slider",
                     "test_qt_engine.py::test_the_speed_slider_sets_the_pauses_as_gui_py_does"],
    "state_messages": ["test_qt_engine.py::test_the_modes_enable_the_widgets_as_gui_py_does",
                       "test_qt_menus.py::test_menu_states_match_the_tkinter_gui"],
    "states": ["test_qt_menus.py::test_menu_states_match_the_tkinter_gui",
               "test_qt_menus.py::test_the_control_panel_matches_the_tkinter_gui",
               "test_qt_menus.py::test_self_watching_off_hides_the_themes_panes"],
    # facts about the reference itself, not GUI elements
    "generated_by": [], "platform": [],
    "screen": ["test_qt_menus.py::test_the_menu_bar_font"],
}

# each dialog of the inventory, by what opens it
DIALOG_TESTS = {
    "Help": "test_qt_menus.py::test_the_help_window",
    "Options > Set breakpoint": "test_qt_audit.py::test_the_input_dialogs",
    "Options > Step mode interval": "test_qt_audit.py::test_the_input_dialogs",
    "Clear Memory": "test_qt_menus.py::test_the_clear_memory_dialog",
    "Options > Clamp theme pattern": "test_qt_menus.py::test_the_theme_edit_dialog",
    "Options > Save commentary to file":
        "test_qt_menus.py::test_save_commentary_uses_the_originals_title",
    "control panel display-error": "test_qt_engine.py::test_invalid_input_shows_for_700_ms",
}


def test_every_inventory_entry_has_a_qt_test():
    assert set(COVERAGE) == set(INVENTORY)
    assert set(DIALOG_TESTS) == {d["opened_by"] for d in INVENTORY["dialogs"]}
    for ref in [r for refs in COVERAGE.values() for r in refs] + list(DIALOG_TESTS.values()):
        path, name = ref.split("::")
        assert re.search(r"^def %s\(" % name, (HERE / path).read_text(), re.M), ref


# --- the engine never imports a GUI toolkit ----------------------------------------

def test_a_run_imports_neither_pyside6_nor_tkinter():
    """every engine module imported and a short run made: no PySide6, tkinter
    or GUI package in sys.modules (the static checks are test_qt_skeleton.py's
    and test_gui_windows.py's)"""
    code = (
        "import sys, pathlib, importlib\n"
        "names = sorted(p.stem for p in pathlib.Path('metacat').glob('*.py')\n"
        "               if p.stem not in ('__init__', '__main__'))\n"
        "for n in names: importlib.import_module('metacat.' + n)\n"
        "import contextlib, io\n"
        "from metacat.__main__ import main\n"
        "with contextlib.redirect_stdout(io.StringIO()):\n"
        "    main(['abc', 'abd', 'xyz', '--seed', '7', '--max-codelets', '200'])\n"
        "bad = sorted(m for m in sys.modules if m.split('.')[0] in\n"
        "             ('PySide6', 'shiboken6', 'tkinter', '_tkinter')\n"
        "             or m.startswith(('metacat.gui', 'metacat.qt')))\n"
        "print(len(names), bad)\n")
    proc = subprocess.run([sys.executable, "-c", code], cwd=str(PY), capture_output=True,
                          text=True, timeout=300)
    assert proc.returncode == 0, proc.stderr[-3000:]
    count, bad = proc.stdout.strip().splitlines()[-1].split(" ", 1)
    assert int(count) > 40 and bad.strip() == "[]", proc.stdout
