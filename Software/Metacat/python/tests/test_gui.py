"""The control panel and windows (loop0002 item 15): gui.ss, demos.ss and setup.ss's
setup on tkinter.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
The counterpart of racket/gui-tests/control-panel-test.rkt.

- Against Chez (python/oracle/batteries/gui-battery.scm, captured into
  python/fixtures/gui/): the command-line parser, the Step/Go/Reset decision on
  each input, char-noise?, the speed slider's settings for every value, the
  figure titles, the demo problems and the clamp-codelets menu's patterns.
- Structure: every define of gui.ss and demos.ss has its Python name, docstrings
  name their origin, demos.py imports no GUI, gui.py imports tkinter only inside
  functions (so the engine can import it headless), and no engine module imports
  metacat.gui.
- Slow tier: tests/drive_gui.py under xvfb-run drives the GUI through its own
  widgets (a full run, step mode, stop and restart, a demo from the Demos menu,
  a breakpoint, a click on the Workspace, Reset, the dialogs and menus, a
  resize) and checks that each GUI run's trace equals its golden, byte for
  byte; it grabs the whole screen into a PNG.
"""
from __future__ import annotations

import ast
import importlib
import inspect
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

from chez_fixtures import chez as fixture, manifest
from scheme_canon import canon
from metacat.names import scheme_to_python

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
ORIGINAL = ROOT / "chez_scheme" / "original"


def M(name):
    return importlib.import_module("metacat." + name)


def battery_inputs():
    """gui-battery.scm's x:inputs, read from the battery (Scheme string syntax,
    with \\xHH; escapes)."""
    text = (ROOT / "python" / "oracle" / "batteries" / "gui-battery.scm").read_text()
    block = text[text.index("(define x:inputs"):text.index("(test tokenize-string")]
    out = []
    for m in re.finditer(r'"((?:[^"\\]|\\.)*)"', block):
        s = re.sub(r"\\x([0-9a-fA-F]+);", lambda g: chr(int(g.group(1), 16)), m.group(1))
        s = s.replace("\\t", "\t").replace("\\n", "\n")
        out.append(s)
    return out


def test_every_battery_test_is_translated():
    assert sorted(CASES) == sorted(manifest("gui"))


def case_tokenize():
    from metacat.chez import String
    gui = M("gui.gui")
    return [[String(s), gui.tokenize_string(String(s))] for s in battery_inputs()]


def case_decisions():
    gui = M("gui.gui")
    from metacat import sugar
    out = []
    for s in battery_inputs():
        if len(s) == 0:
            out.append("resume")
        else:
            tokens = gui.tokenize_string(s)
            out.append(["new", tokens] if sugar.valid_token_list_p(tokens) else "invalid")
    return out


def case_char_noise():
    from metacat.chez import Char
    gui = M("gui.gui")
    return [[Char(c), gui.char_noise_p(c)]
            for c in "aZ09 -;>?,.\tλé١½_"]


def case_speed_slider():
    gui = M("gui.gui")
    vg = M("view_globals")
    out = []
    for v in range(101):
        gui.speed_slider_action(False, v)
        out.append([v, vg.p_num_of_flashes, vg.p_flash_pause, vg.p_snag_pause,
                    vg.p_text_scroll_pause])
    return out


def case_speed_constants():
    gui = M("gui.gui")
    vg = M("view_globals")
    return [gui.p_max_num_of_flashes, gui.p_max_flash_pause, gui.p_max_snag_pause,
            gui.p_text_scroll_pause, gui.p_codelet_highlight_pause, gui.p_initial_speed,
            gui.p_gui_slider_length, gui.p_gui_slider_thickness]


def case_figures():
    from metacat.chez import String
    gui = M("gui.gui")
    return [String(t) for t in (gui.figure(5, 4, "top"), gui.figure(5, 4, "bottom"),
                                gui.figure(5, 7), gui.figure(5, 10))]


DEMO_NAMES = ["run1", "run2", "run3", "run4", "run5", "run6", "run7", "run8",
              "abc-xyd", "abc-wyz", "abc-dyz", "rst-xyu", "rst-wyz", "rst-uyz",
              "abc-mrrkkk", "abc-mrrjjjj", "xqc-mrrkkk", "xqc-mrrjjjj",
              "eqe-baaab", "eqe-aaabaaa", "eqe-qeeeq", "eqe-aaabccc",
              "fig5.4-top", "fig5.4-bottom", "fig5.5-top", "fig5.5-bottom",
              "fig5.7", "fig5.8", "fig5.10", "fig5.11",
              "misc1", "misc2", "misc3", "misc4", "misc5"]


def case_demos():
    demos = M("demos")
    return [getattr(demos, scheme_to_python(n)) for n in DEMO_NAMES]


def case_clamp_patterns():
    from metacat import engine
    from metacat.objects import tell
    engine.load()
    gui = M("gui.gui")

    def named(pattern):
        return [pattern[0]] + [[tell(e[0], "get-codelet-type-name"), *e[1:]]
                               for e in pattern[1:]]
    return [named(gui.clamp_codelets_pattern(t))
            for t in ("top-down", "bottom-up", "group", "bridge", "rule")]


CASES = {
    "tokenize-string": case_tokenize,
    "command-line-decisions": case_decisions,
    "char-noise": case_char_noise,
    "speed-slider": case_speed_slider,
    "speed-constants": case_speed_constants,
    "figure-titles": case_figures,
    "demos": case_demos,
    "clamp-codelet-patterns": case_clamp_patterns,
}


@pytest.mark.parametrize("name", list(CASES))
def test_gui_battery(name):
    vg = M("view_globals")
    saved = (vg.p_num_of_flashes, vg.p_flash_pause, vg.p_snag_pause, vg.p_text_scroll_pause)
    try:
        got = canon(CASES[name]())
    finally:
        vg.p_num_of_flashes, vg.p_flash_pause, vg.p_snag_pause, vg.p_text_scroll_pause = saved
    want = fixture("gui", name)
    if got != want:
        i = next((k for k, (a, b) in enumerate(zip(got, want)) if a != b), min(len(got), len(want)))
        pytest.fail("%s differs from Chez at %d: got %r, Chez %r"
                    % (name, i, got[max(0, i - 60):i + 60], want[max(0, i - 60):i + 60]))


# structure ----------------------------------------------------------------------------

def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


@pytest.mark.parametrize("stem,module,count", [("gui", "gui.gui", 61), ("demos", "demos", 36)])
def test_every_definition_has_its_python_name(stem, module, count):
    names = defines(ORIGINAL / (stem + ".ss"))
    assert len(names) == count
    mod = M(module)
    missing = [n for n in names if not hasattr(mod, scheme_to_python(n))]
    assert missing == []


def test_setup_and_enable_resizing_are_translated():
    app = M("gui.app")
    assert callable(app.setup) and callable(app.enable_resizing)


@pytest.mark.parametrize("module,origins", [
    ("gui.gui", ("gui.ss", "port")), ("demos", ("demos.ss",)),
    ("gui.app", ("setup.ss", "fonts.ss", "port"))])
def test_docstrings_name_their_origin(module, origins):
    mod = M(module)
    for fname, fn in vars(mod).items():
        if (inspect.isfunction(fn) or inspect.isclass(fn)) and fn.__module__ == mod.__name__ \
                and not fname.startswith("_"):
            assert fn.__doc__ and fn.__doc__.split(":")[0] in origins, fname


def imports(module):
    tree = ast.parse(inspect.getsource(module))
    top = {a.name for n in tree.body if isinstance(n, (ast.Import, ast.ImportFrom))
           for a in n.names} | {n.module for n in tree.body if isinstance(n, ast.ImportFrom)}
    every = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree)
                                  if isinstance(n, ast.ImportFrom)}
    return top, every


def test_demos_imports_no_gui():
    top, every = imports(M("demos"))
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in every)


@pytest.mark.parametrize("module", ["gui.gui", "gui.app"])
def test_gui_modules_import_tkinter_lazily(module):
    top, every = imports(M(module))
    assert not any(str(n).startswith("tkinter") for n in top)


def test_no_engine_module_imports_the_gui():
    from metacat import engine
    for mod in engine.translated_modules():
        top, every = imports(mod)
        assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in every), mod


def test_a_headless_run_loads_no_gui_module():
    """In a fresh process, importing every engine module, engine.load() and a headless
    run leave tkinter and every metacat.gui module (the package included) unloaded."""
    code = (
        "import sys, io, contextlib, pkgutil, importlib, metacat\n"
        "for m in pkgutil.iter_modules(metacat.__path__, 'metacat.'):\n"
        "    if m.name not in ('metacat.gui', 'metacat.__main__'):\n"
        "        importlib.import_module(m.name)\n"
        "from metacat import engine, headless\n"
        "engine.load()\n"
        "with contextlib.redirect_stdout(io.StringIO()):\n"
        "    headless.run_problem(['abc', 'abd', 'xyz'], seed=7, max_codelets=200)\n"
        "print(sorted(k for k in sys.modules\n"
        "             if k.split('.')[0] in ('tkinter', '_tkinter') or k.startswith('metacat.gui')))\n")
    proc = subprocess.run([sys.executable, "-c", code], capture_output=True, text=True,
                          cwd=HERE.parent, timeout=120)
    assert proc.returncode == 0, proc.stderr[-3000:]
    assert proc.stdout.strip() == "[]"


# the GUI, driven under Xvfb ---------------------------------------------------------------

@pytest.mark.slow
def test_drive_the_gui(tmp_path):
    """drive_gui.py under xvfb-run: every scenario passes and every GUI run's trace
    equals its golden.  Never on the owner's screen (WAYLAND_DISPLAY unset)."""
    proc = subprocess.run(
        ["xvfb-run", "-a", "-s", "-screen 0 2560x1600x24", sys.executable,
         str(HERE / "drive_gui.py"), str(tmp_path)],
        capture_output=True, text=True, timeout=900,
        env={k: v for k, v in os.environ.items() if k != "WAYLAND_DISPLAY"})
    assert proc.returncode == 0, proc.stdout[-4000:] + proc.stderr[-4000:]
    ok = [line for line in proc.stdout.splitlines() if line.startswith("ok ")]
    for scenario in ("windows", "invalid-input", "full-run", "step-mode", "demo-stop-go",
                     "breakpoint-click", "reset", "menus", "save-commentary", "resize",
                     "responsive", "screenshot"):
        assert any(line.split()[1] == scenario for line in ok), (scenario, proc.stdout[-3000:])
    assert (tmp_path / "screen.png").stat().st_size > 10000
