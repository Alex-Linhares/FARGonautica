"""The Tk canvas commands the panels send (loop0003 item 01).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The Qt canvas (python/metacat/qt/canvas.py, item 02) must execute every Tk canvas
command the panels send, with every option.  tests/data/tk-canvas-commands.json is
that list, made by tk_canvas_inventory.py from three sources:

- **code**: every call in python/metacat/ that sends a canvas command
  (sgl.py's tcl_eval, swl.swl_tcl_eval, a canvas's .tcl), read from the source
  with ast.  A call whose command is not a literal must be one of the known
  forwarders, so nothing escapes the scan;
- **fixtures**: every stream in python/fixtures/sgl-tcl/ (what the original's
  SGL interpreter sent Tk, captured from Chez);
- **runs** (slow): what reaches a recording canvas in real runs with every view
  attached: run7 (abc abd xyz 3852097033) and a justify run.  The recording
  canvas has only the canvas interface, so a panel using anything else fails.

The tests rebuild each part and compare it with the committed list: a command,
item kind, option or option value missing from the list fails, and so does an
entry nothing sends any more.
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

import tk_canvas_inventory as inv

HERE = Path(__file__).resolve().parent
COMMITTED = HERE / "data" / "tk-canvas-commands.json"


@pytest.fixture(scope="module")
def committed():
    return json.loads(COMMITTED.read_text())


def test_code_scan_finds_the_sgl_interpreter_commands():
    found = inv.scan_code()
    assert not found.problems, found.problems
    # sgl.py's draw-* methods and fonts.ss's measurement, at least
    for key in ("create rectangle", "create line", "create oval", "create arc",
                "create polygon", "create text", "move", "raise", "itemconfigure",
                "scale", "delete", "bbox", "canvasx", "canvasy"):
        assert key in found.commands, key
    assert "-state" in found.commands["create rectangle"]
    assert {"-state", "-tags"} <= found.commands["itemconfigure"]


def test_code_part_is_in_the_list(committed):
    found = inv.scan_code()
    assert not found.problems, found.problems
    assert inv.missing(found, committed) == [], "commands sent but not in the list"
    assert inv.part(committed, "code") == found.as_json()


def test_fixtures_part_is_in_the_list(committed):
    found = inv.scan_fixtures()
    assert found.lines > 600
    assert inv.missing(found, committed) == []
    assert inv.part(committed, "fixtures") == found.as_json()


def test_the_list_is_the_union_of_its_parts(committed):
    assert set(committed["parts"]) == {"code", "fixtures", "runs"}
    assert inv.build_from_parts(committed["parts"]) == committed


def test_every_listed_entry_has_a_source(committed):
    for key, entry in committed["commands"].items():
        assert entry["sources"], key
        assert set(entry["sources"]) <= {"code", "fixtures", "runs"}, key


def test_a_new_command_in_a_panel_is_caught(tmp_path, committed):
    """the scan sees a command no list entry has, in a module of its own"""
    panel = tmp_path / "new_panel.py"
    panel.write_text(
        "from metacat.gui.sgl import tcl_eval\n"
        "def draw(self):\n"
        "    tcl_eval(self, 'create', 'window', 0, 0, '-window', 'w', '-tags', 'x')\n"
        "    tcl_eval(self, 'itemconfigure', 'x', '-fill', 'red')\n"
        "    self.canvas.tcl('lower', 'x')\n")
    found = inv.scan_code([panel])
    assert sorted(inv.missing(found, committed)) == [
        "create window", "create window -tags", "create window -window",
        "itemconfigure -fill", "lower"]


def test_an_unknown_forwarder_is_a_problem(tmp_path):
    """a call whose command is computed must be a known forwarder"""
    panel = tmp_path / "sneaky.py"
    panel.write_text("def draw(win, cmd):\n    return win.tcl(cmd, 'all')\n")
    found = inv.scan_code([panel])
    assert len(found.problems) == 1 and "sneaky.py:2" in found.problems[0]


def test_option_values_are_listed(committed):
    values = committed["values"]
    assert set(values["-anchor"]) >= {"s", "nw"}
    assert set(values["-style"]) >= {"arc", "pieslice"}
    assert set(values["-state"]) >= {"hidden", "normal"}


@pytest.mark.slow
def test_runs_part_is_in_the_list(committed):
    found = inv.scan_runs()
    assert found.lines > 10000
    assert inv.missing(found, committed) == [], "commands sent in runs but not in the list"
    assert inv.part(committed, "runs") == found.as_json()


@pytest.mark.slow
def test_the_list_is_exactly_the_union(committed):
    built = inv.build([inv.scan_code(), inv.scan_fixtures(), inv.scan_runs()])
    assert built == committed
