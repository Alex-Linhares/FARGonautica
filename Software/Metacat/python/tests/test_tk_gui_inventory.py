# Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
"""The inventory of the tkinter GUI (loop0003 item 00): tests/data/tk-gui-inventory.json,
made by tests/tk_gui_inventory.py under Xvfb, lists every window and every menu item
that gui.py and the panels define.  The expected labels are read from gui.py's
source (its calls of menu_item, check_menu_item, demo_menu_item, ...), not from the
inventory, so that this checks the inventory itself."""
from __future__ import annotations

import ast
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

HERE = Path(__file__).resolve().parent
PY = HERE.parent
INVENTORY = HERE / "data" / "tk-gui-inventory.json"
GUI = PY / "metacat" / "gui" / "gui.py"
CONSTANTS = PY / "metacat" / "gui" / "constants.py"


def load():
    return json.loads(INVENTORY.read_text())


def labels(items, prefix=""):
    out = []
    for e in items:
        if e["kind"] == "separator":
            continue
        out.append(prefix + e["label"])
        if e["kind"] == "cascade":
            out += labels(e["items"], prefix + e["label"] + " > ")
    return out


def leaf_labels(items):
    return {lab.rsplit(" > ", 1)[-1] for lab in labels(items)}


def figure(m, n, *opt):
    # gui.py's figure on linux
    return "Figure %s.%s%s" % (m, n, " (%s)" % opt[0] if opt else "")


def source_labels():
    """the labels of gui.py's menu items, read from its source"""
    tree = ast.parse(GUI.read_text())
    found = set()
    makers = {"menu_item", "check_menu_item", "demo_menu_item", "create_submenu",
              "submenu_anchor", "clear_memory_menu_item", "clamp_codelets_menu_item"}
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call) or not isinstance(node.func, ast.Name):
            continue
        name = node.func.id
        if name in makers and node.args:
            a = node.args[0]
            if isinstance(a, ast.Constant) and isinstance(a.value, str):
                found.add(a.value)
            elif (isinstance(a, ast.Call) and isinstance(a.func, ast.Name)
                  and a.func.id == "figure"):
                found.add(figure(*[ast.literal_eval(x) for x in a.args]))
        elif name == "comment_font_menu_item":
            found.add(ast.literal_eval(node.args[1]))
        elif name == "window_controller":
            found.add("Hide " + ast.literal_eval(node.args[0]) if
                      ast.literal_eval(node.args[2]) else "Show " + ast.literal_eval(node.args[0]))
    # DEMO_ITEMS: the eight runs
    for node in ast.walk(tree):
        if (isinstance(node, ast.Assign) and isinstance(node.targets[0], ast.Name)
                and node.targets[0].id == "DEMO_ITEMS"):
            found |= {t for t, _ in ast.literal_eval(node.value)}
    return found


def source_windows():
    """the window controllers' names in gui.py and the window titles of constants.py"""
    tree = ast.parse(GUI.read_text())
    names = {ast.literal_eval(n.args[0]) for n in ast.walk(tree)
             if isinstance(n, ast.Call) and isinstance(n.func, ast.Name)
             and n.func.id == "window_controller"}
    titles = set()
    for n in ast.walk(ast.parse(CONSTANTS.read_text())):
        if (isinstance(n, ast.Assign) and isinstance(n.targets[0], ast.Name)
                and n.targets[0].id.endswith("_window_title")):
            titles.add(ast.literal_eval(n.value.args[0]))
    return names, titles


def test_the_source_has_what_we_expect():
    # a guard on the parsing above: the inventory test is only as good as this list
    found = source_labels()
    for label in ("Help", "Clear Memory", "Set breakpoint", "Self-watching mode",
                  "Run 7:  abc -> abd; xyz -> ?", "Figure 5.4 (top)", "huge",
                  "sans-serif bold italic", "Rule codelet pattern", "Show all windows",
                  "Hide Workspace", "Show EEG", "Save commentary to file"):
        assert label in found, label
    assert len(found) > 80


def test_the_inventory_lists_every_menu_item():
    inv = load()
    have = leaf_labels(inv["menus"])
    missing = sorted(source_labels() - have)
    assert not missing, missing


def test_the_inventory_lists_every_window():
    inv = load()
    names, titles = source_windows()
    by_name = {w["name"]: w for w in inv["windows"]}
    assert set(by_name) == names
    assert {w["title"] for w in inv["windows"]} >= titles - {"Logo"} | set()
    for w in inv["windows"]:
        assert w["title"], w["name"]
        assert "geometry" in w and "resizable" in w


def test_the_windows_have_their_mouse_bindings():
    by_name = {w["name"]: w for w in load()["windows"]}
    assert by_name["Workspace"]["left_press"].endswith("workspace_window_press_handler")
    assert by_name["Temporal Trace"]["left_press"].endswith("trace_window_press_handler")
    assert by_name["Episodic Memory"]["left_press"].endswith("memory_window_press_handler")
    for t in ("Top", "Bottom", "Vertical"):
        w = by_name[t + " Themes"]
        assert "theme_window_left_press_handler" in w["left_press"]
        assert "theme_window_right_press_handler" in w["right_press"]
        assert w["shift_left_press"] == w["right_press"]
    for name in ("Workspace", "Slipnet", "Coderack", "Temperature", "Temporal Trace",
                 "Commentary", "Episodic Memory", "Top Themes", "EEG"):
        b = by_name[name]["canvas_bindings"]
        assert {"<Button-1>", "<Shift-Button-1>", "<Button-3>"} <= set(b), (name, b)


def test_the_states_and_dialogs_are_there():
    inv = load()
    st = inv["states"]
    assert set(st) >= {"initial", "input", "run", "disabled", "self-watching-off"}
    assert st["run"]["stop-button"] == "normal" and st["run"]["go-button"] == "disabled"
    assert st["input"]["stop-button"] == "disabled" and st["input"]["go-button"] == "normal"
    assert st["input"]["enter-action"].endswith("go_button_action")
    assert st["run"]["enter-action"].endswith("nop_event_handler")
    assert {d["opened_by"] for d in inv["dialogs"]} >= {
        "Help", "Options > Set breakpoint", "Options > Step mode interval", "Clear Memory",
        "Options > Clamp theme pattern", "Options > Save commentary to file"}
    seqs = {(b["toplevel"], b["sequence"]) for b in inv["bindings"]}
    assert ("Metacat Control Panel", "<Key-Return>") in seqs


def strip(x):
    """the inventory without what depends on the machine's fonts"""
    if isinstance(x, dict):
        return {k: strip(v) for k, v in x.items() if "font" not in k}
    if isinstance(x, list):
        return [strip(v) for v in x]
    return x


@pytest.mark.slow
@pytest.mark.skipif(shutil.which("xvfb-run") is None, reason="needs xvfb-run")
def test_the_inventory_script_runs_and_matches(tmp_path):
    """tk_gui_inventory.py under xvfb-run gives the committed inventory (fonts aside).
    Never on the owner's screen (WAYLAND_DISPLAY unset)."""
    out = tmp_path / "inv.json"
    proc = subprocess.run(
        ["xvfb-run", "-a", "-s", "-screen 0 1920x1200x24", sys.executable,
         str(HERE / "tk_gui_inventory.py"), str(out)],
        capture_output=True, text=True, timeout=300,
        env={k: v for k, v in os.environ.items() if k != "WAYLAND_DISPLAY"})
    assert proc.returncode == 0, proc.stdout[-3000:] + proc.stderr[-3000:]
    assert strip(json.loads(out.read_text())) == strip(load())
