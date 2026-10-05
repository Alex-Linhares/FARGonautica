"""The Qt GUI's control strip, menus and dialogs against the tkinter GUI's
inventory (loop0003 item 06).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

drive_qt_menus.py (a fresh process, offscreen, on a screen of the inventory's
size) records the Qt menu bar and the control panel in the five states of
python/tests/data/tk-gui-inventory.json, and drives every menu item and
dialog.  These tests compare its report with the inventory:

- every menu item of the tkinter GUI is in the Qt menu bar, with its label,
  kind, font, check state and demo problem, and its state (enabled or
  disabled) in each of the five states.  The Qt menu bar differs only where
  docs/divergences.md says so (Windows is View, with check marks for "Hide
  X"/"Show X" and no Logo; Help and Clear Memory are menus of one item; no
  blank spacer; View > Reset layout);
- the control panel's widgets have the inventory's states, texts, colours,
  fonts and Enter action in each state;
- each scenario of the driver (demos, View, Options, Help, Clear Memory, the
  theme-edit dialog, Save commentary, a real run) did what gui.py does.
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

pytest.importorskip("PySide6.QtWidgets")

HERE = Path(__file__).resolve().parent
OUT = HERE / "screenshots-qt" / "menus"
SCRIPT = HERE / "drive_qt_menus.py"
INVENTORY = json.loads((HERE / "data" / "tk-gui-inventory.json").read_text())

SCENARIOS = ["inventory", "demos", "view", "options", "help", "clear-memory", "theme-edit",
             "save-commentary", "run-mode"]

# the pane names of the Windows menu's items (the Logo is not a pane)
PANES = ["Workspace", "Slipnet", "Coderack", "Temperature", "Temporal Trace", "Commentary",
         "Episodic Memory", "Top Themes", "Bottom Themes", "Vertical Themes", "EEG"]
QT_ONLY = {"Help > Metacat help", "Memory > Clear Memory", "View > Reset layout"}


@pytest.fixture(scope="module")
def report():
    OUT.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ)
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run([sys.executable, str(SCRIPT), str(OUT)], capture_output=True,
                          text=True, timeout=900, env=env, cwd=str(HERE.parent))
    oks = [line.split()[1] for line in proc.stdout.splitlines() if line.startswith("ok ")]
    rep = json.loads((OUT / "report.json").read_text())
    rep["_oks"] = oks
    rep["_proc"] = (proc.returncode, proc.stdout[-4000:], proc.stderr[-4000:])
    return rep


def test_every_scenario_passed(report):
    code, out, err = report["_proc"]
    assert code == 0 and report["_oks"] == SCENARIOS, out + err


# --- the inventory's menus, mapped onto the Qt menu bar ------------------------------

def qt_path(path):
    """an inventory menu path as its Qt path (None: not in the Qt GUI)"""
    parts = path.split(" > ")
    if parts[0] == "  ":
        return None                         # the blank spacer
    if parts[0] == "Clear Memory":
        return "Memory"
    if parts[0] == "Help":
        return "Help"
    if parts[0] == "Windows":
        if len(parts) == 1:
            return "View"
        label = parts[1]
        if label in ("Show all windows", "Hide all windows"):
            return "View > " + label.replace("windows", "panes")
        name = label[5:]                    # "Hide X" / "Show X"
        return None if name == "Logo" else "View > " + name
    return path


def expected_tree(items):
    """the inventory's menu tree as the Qt one should read"""
    out = []
    for e in items:
        label = e.get("label")
        if label == "  ":
            continue
        if label == "Help":
            out.append({"kind": "cascade", "label": "Help", "state": "normal",
                        "font": e["font"],
                        "items": [{"kind": "command", "label": "Metacat help",
                                   "state": "normal", "font": None}]})
            continue
        if label == "Clear Memory":
            out.append({"kind": "cascade", "label": "Memory", "state": "normal",
                        "font": e["font"],
                        "items": [{"kind": "command", "label": "Clear Memory",
                                   "state": "normal", "font": None}]})
            continue
        if label == "Windows":
            items = []
            for w in e["items"]:
                if w["kind"] == "separator":
                    items.append({"kind": "separator"})
                elif w["label"] in ("Show all windows", "Hide all windows"):
                    items.append({"kind": "command",
                                  "label": w["label"].replace("windows", "panes"),
                                  "state": "normal", "font": w["font"]})
                elif w["label"][5:] != "Logo":
                    items.append({"kind": "pane", "label": w["label"][5:], "state": "normal",
                                  "font": w["font"], "highlighted": w["label"][:4] == "Hide"})
            items += [{"kind": "separator"},
                      {"kind": "command", "label": "Reset layout", "state": "normal",
                       "font": items[-1]["font"]}]
            out.append({"kind": "cascade", "label": "View", "state": "normal",
                        "font": e["font"], "items": items})
            continue
        x = {"kind": e["kind"]}
        if e["kind"] != "separator":
            x.update(label=label, state=e["state"], font=e["font"])
        if e["kind"] == "check":
            x["selected"] = e["selected"]
        if e["kind"] == "cascade":
            x["items"] = expected_tree(e["items"])
        if "problem" in e:
            x["problem"] = e["problem"]
            x["highlighted"] = False
        if e.get("action", "").endswith("face_action") or e.get("action", "").endswith(
                "size_action"):
            x["highlighted"] = e["background"] == "#ffffff"
        out.append(x)
    return out


# the faces fonts.ss picked under Tk (the inventory's), by role
TK_FACES = {"serif": "times", "sans-serif": "helvetica", "fancy": "palatino"}


def with_faces(tree, faces):
    """the tree with the Tk faces replaced by the Qt GUI's (docs/divergences.md,
    Python Qt GUI: the faces)"""
    swap = {TK_FACES[role]: face for role, face in faces.items()}
    for e in tree:
        if e.get("font"):
            e["font"] = [swap.get(e["font"][0], e["font"][0])] + e["font"][1:]
        with_faces(e.get("items", []), faces)
    return tree


def test_the_menu_bar_has_every_item_of_the_inventory(report):
    want = with_faces(expected_tree(INVENTORY["menus"]), report["faces"])
    got = report["menus"]

    def strip_fonts(tree):
        for e in tree:
            if e.get("font") is None:
                e.pop("font", None)
            strip_fonts(e.get("items", []))
        return tree
    # Help's and Memory's single items have no font of their own in the inventory
    assert [e["label"] for e in got] == [e["label"] for e in want]
    for g, w in zip(got, want):
        if w["label"] in ("Help", "Memory"):
            g["items"][0]["font"] = None
    assert strip_fonts(got) == strip_fonts(want)


def test_the_menu_bar_font(report):
    font = INVENTORY["menus"][0]["font"]
    assert report["menubar-font"] == font


@pytest.mark.parametrize("state", ["initial", "input", "disabled", "run", "self-watching-off"])
def test_menu_states_match_the_tkinter_gui(report, state):
    tk = INVENTORY["states"][state]["menus"]
    qt = report["states"][state]["menus"]
    want = {}
    for path, s in tk.items():
        p = qt_path(path)
        if p is not None:
            want[p] = s
    extra = {p: s for p, s in qt.items() if p in QT_ONLY}
    assert {p: s for p, s in qt.items() if p not in QT_ONLY} == want
    # Help's item answers always; Memory's and View's follow their menu
    assert extra["Help > Metacat help"] == "normal"
    assert extra["View > Reset layout"] == "normal"


CONTROL_KEYS = ["command-line", "step-button", "go-button", "stop-button", "reset-button",
                "speed-slider", "command-line-text", "command-line-justify",
                "command-line-foreground", "command-line-background", "command-line-font",
                "info-label", "breakpoint-label", "self-watching-warning-visible"]


@pytest.mark.parametrize("state", ["initial", "input", "disabled", "run", "self-watching-off"])
def test_the_control_panel_matches_the_tkinter_gui(report, state):
    tk = INVENTORY["states"][state]
    qt = report["states"][state]
    assert {k: qt[k] for k in CONTROL_KEYS} == {k: tk[k] for k in CONTROL_KEYS}
    # Enter: gui.py's go-button-action, or nothing, as the tkinter panel's
    assert qt["enter-action"].rsplit(".", 1)[-1] == tk["enter-action"].rsplit(".", 1)[-1]


def test_self_watching_off_hides_the_themes_panes(report):
    names = {"top-themes": "Top Themes", "bottom-themes": "Bottom Themes",
             "vertical-themes": "Vertical Themes"}
    hidden = [names[n] for n in report["states"]["self-watching-off"]["hidden_windows"]]
    want = [n for n in INVENTORY["states"]["self-watching-off"]["hidden_windows"]
            if n not in ("EEG", "Logo")]           # hidden from the start
    assert sorted(hidden) == sorted(want)
    assert len(report["visible-after-self-watching-on"]) == 10


def test_every_demo_inits_its_problem(report):
    def walk(prefix, items, out):
        for e in items:
            if e["kind"] == "cascade":
                walk(prefix + e["label"] + " > ", e["items"], out)
            elif "problem" in e:
                out[prefix + e["label"]] = e["problem"]
        return out
    (demos,) = [e for e in INVENTORY["menus"] if e.get("label") == "Demos"]
    want = walk("Demos > ", demos["items"], {})
    assert len(want) == 35
    got = report["demos"]

    def problem(tokens):
        """update-current-problem's problem: a, b, c, d or #f, seed"""
        return tokens if len(tokens) == 5 else tokens[:3] + [False] + tokens[3:]
    assert {p: r["problem"] for p, r in got.items()} == {p: problem(t) for p, t in want.items()}
    for path, r in got.items():
        a, b, c, d, seed = r["problem"]
        assert r["info"] == " %s -> %s; %s -> %s       seed:  %s " % (a, b, c, d or "?", seed)


def test_the_help_window(report):
    (tk,) = [d for d in INVENTORY["dialogs"] if d["opened_by"] == "Help"]
    qt = report["help"]
    assert qt["first-line"] == tk["text_first_line"]
    assert qt["lines"] == tk["text_lines"]
    text = tk["widgets"]["children"][0]["children"][0]
    assert " ".join(str(x) for x in qt["font"]) == text["font"]
    assert qt["wrap"] is True


def test_the_clear_memory_dialog(report):
    (tk,) = [d for d in INVENTORY["dialogs"] if d["opened_by"] == "Clear Memory"]
    message = tk["widgets"]["children"][0]["text"]
    labels = report["clear-memory"]["labels"]
    assert message in labels
    st = report["clear-memory"]["disabled-state"]
    want = INVENTORY["states"]["disabled"]
    for k in ("command-line", "step-button", "go-button", "stop-button", "reset-button"):
        assert st[k] == want[k], k


def test_the_theme_edit_dialog(report):
    (tk,) = [d for d in INVENTORY["dialogs"] if d["opened_by"] == "Options > Clamp theme pattern"]
    texts = []

    def walk(w):
        if w.get("class") == "Label":
            texts.append(w["text"])
        for c in w.get("children", []):
            walk(c)
    walk(tk["widgets"])
    assert any(t in report["theme-edit"]["labels"] for t in texts)
    assert report["theme-edit"]["background"] == "#ffff00"


def test_save_commentary_uses_the_originals_title(report):
    assert report["save-title"] == "Save Commentary to File"


def test_a_real_run_enables_only_stop(report):
    run = report["running"]
    want = INVENTORY["states"]["run"]
    for k in ("command-line", "step-button", "go-button", "stop-button", "reset-button",
              "speed-slider", "command-line-text"):
        assert run[k] == want[k], k
    stopped = report["stopped"]
    assert stopped["go-button"] == "normal" and stopped["stop-button"] == "disabled"


def test_the_strip_fits_a_1080p_window_with_every_message(report):
    """the self-watching warning shown, the window keeps its default width"""
    from metacat.qt.mainwindow import DEFAULT_SIZE
    assert report["states"]["self-watching-off"]["window-width"] == DEFAULT_SIZE[0]
