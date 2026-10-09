"""GUI test infrastructure (loop0002 item 000): draw ops (seqsee/gui/draw/ops.py), the colour
table (seqsee/gui/draw/colors.py) and the comparison helper (tests/gui_compare.py), checked
against Perl/Tk canvas dumps made by python/oracle/CanvasDump.pm (golden: gui_smoke).

Mirrors no SGUI module; it mirrors the Perl/Tk canvas item model that lib/SGUI/*.pm and
lib/Themes/Std2.pm draw with (createLine/Oval/Rectangle/Polygon/Arc/Text and their options).
"""

import pathlib
import subprocess
import sys

import pytest

import golden
from gui_compare import assert_ops_match, compare_ops, item_diff, normalize
from seqsee.gui.draw import colors, ops

PY = pathlib.Path(__file__).resolve().parents[1]
SRC = PY / "src" / "seqsee"
CASES = {c["name"]: c for c in golden.load("gui_smoke")}

FONT20 = "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4"
FONT10 = "-adobe-helvetica-bold-r-normal--10-140-100-100-p-105-iso8859-4"


# The same named recipes as oracle/gui_smoke.pl.
def recipe_defaults():
    return [
        ops.line(10, 10, 60, 40),
        ops.oval(70, 10, 110, 40),
        ops.rectangle(120, 10, 160, 40),
        ops.polygon(170, 40, 190, 10, 210, 40),
        ops.arc(220, 10, 280, 60),
        ops.text(150, 100, text="defaults"),
    ]


def recipe_styled():
    return [
        ops.rectangle(5, 5, 395, 255, fill="#EEEEEE", outline="", tags=["bg"]),
        ops.line(20, 200, 80, 120, 140, 200, fill="red", width=3, smooth=1,
                 arrow="last", arrowshape=[8, 12, 10], tags=["reln", "r1"]),
        ops.line(160, 30, 380, 30, fill="#0000FF", width=2, dash="---"),
        ops.line(160, 50, 380, 50, dash=[6, 4], arrow="both", capstyle="round"),
        ops.oval(160, 70, 220, 130, fill="#CCFFDD", outline="navy blue", width=0),
        ops.rectangle(240, 70, 300, 130, fill="gray75", outline="#000000", width=4,
                      stipple="gray75"),
        ops.polygon(320, 130, 350, 70, 380, 130, fill="", outline="DarkGreen", smooth=1, width=2),
        ops.arc(160, 150, 260, 250, start=30, extent=120, style="arc", outline="#FF0000", width=2),
        ops.arc(270, 150, 370, 250, start=200, extent=90, style="chord", fill="yellow"),
        ops.text(20, 20, text="nw anchor", anchor="nw", font=FONT20, fill="#FF0000",
                 tags=["label"]),
        ops.text(80, 240, text="two\nlines", justify="center", font=FONT10),
    ]


# --- goldens ---------------------------------------------------------------------------

def test_empty_canvas():
    assert CASES["empty"]["items"] == []
    assert_ops_match([], CASES["empty"]["items"])


def test_op_defaults_are_tk_defaults():
    expected = CASES["defaults"]["items"]
    # Raw to_dict, not just the tolerant comparison: no option differs from Tk's default.
    assert [op.to_dict() for op in recipe_defaults()] == expected
    assert_ops_match(recipe_defaults(), expected)


def test_styled_items_match_perl():
    assert_ops_match(recipe_styled(), CASES["styled"]["items"])


def test_styled_items_raw_dump_shape():
    """Ops already in normalised form serialise exactly like the dump."""
    exp = CASES["styled"]["items"][2]
    assert ops.line(160, 30, 380, 30, fill="#0000FF", width=2, dash="---").to_dict() == exp


def test_colour_table_matches_tk():
    rgb = CASES["colours"]["rgb"]
    bad = {n: (colors.to_hex(n), h) for n, h in rgb.items() if colors.to_hex(n) != h}
    assert rgb and not bad, bad
    assert set(colors.x11_names()) <= set(rgb)


def test_colour_unknown_and_unset():
    assert colors.to_hex(None) is None and colors.to_hex("") is None
    with pytest.raises(KeyError):
        colors.to_hex("no-such-colour")
    with pytest.raises(KeyError):
        colors.to_hex("#12345")


# --- ops -------------------------------------------------------------------------------

def test_create_accepts_perl_style_arguments():
    style = {"-fill": "#FF0000", "-smooth": "true", "-tags": "one"}
    op = ops.create("line", [1, 2], (3, 4), 5, 6, **style)
    assert isinstance(op, ops.Line)
    assert op.coords == (1, 2, 3, 4, 5, 6)
    assert op.fill == "#FF0000" and op.smooth == 1 and op.tags == ("one",)
    assert ops.create("oval", 0, 0, 1, 1, fill="").fill is None
    with pytest.raises(TypeError):
        ops.create("text", 0, 0, arrow="last")


def test_ops_are_immutable_and_hashable():
    op = ops.text(1, 2, text="x")
    with pytest.raises(Exception):
        op.text = "y"
    assert len({op, ops.text(1, 2, text="x")}) == 1


# --- comparison helper -----------------------------------------------------------------

def test_coord_tolerance():
    exp = {"type": "line", "coords": [0, 0, 10, 10], "opts": {}, "tags": []}
    assert item_diff(ops.line(0.4, 0, 10, 9.6), exp) is None
    assert "coords" in item_diff(ops.line(0.6, 0, 10, 10), exp)
    assert "coords" in item_diff(ops.line(0, 0, 10, 10, 20, 20), exp)


def test_colours_and_defaults_are_normalised():
    exp = {"type": "rectangle", "coords": [0, 0, 1, 1], "opts": {"fill": "#BFBFBF"}, "tags": []}
    assert item_diff(ops.rectangle(0, 0, 1, 1, fill="gray75", outline="black"), exp) is None
    assert item_diff(ops.rectangle(0, 0, 1, 1, fill="Gray75", outline="#000"), exp) is None
    assert "opts" in item_diff(ops.rectangle(0, 0, 1, 1, fill="gray50"), exp)
    assert normalize(ops.line(0, 0, 1, 1, width=1.0))["opts"] == {}


def test_type_option_and_tag_mismatches():
    exp = {"type": "text", "coords": [5, 5], "opts": {"text": "a"}, "tags": ["t"]}
    assert item_diff(ops.text(5, 5, text="a", tags=["t"]), exp) is None
    assert "type" in item_diff(ops.oval(5, 5, 6, 6), exp)
    assert "opts" in item_diff(ops.text(5, 5, text="b", tags=["t"]), exp)
    assert "opts" in item_diff(ops.text(5, 5, text="a", anchor="w", tags=["t"]), exp)
    assert "tags" in item_diff(ops.text(5, 5, text="a"), exp)


def test_ordered_and_multiset_comparison():
    a, b = ops.line(0, 0, 1, 1), ops.oval(0, 0, 1, 1)
    exp = [x.to_dict() for x in (a, b)]
    assert compare_ops([a, b], exp) == []
    assert len(compare_ops([b, a], exp)) == 2
    assert compare_ops([b, a], exp, ordered=False) == []
    assert compare_ops([a], exp) == ["missing item 1: oval [0.0, 0.0, 1.0, 1.0] {} tags=[]"]
    assert compare_ops([a, b, a], exp, ordered=False)[0].startswith("extra actual item 2")
    with pytest.raises(AssertionError, match="differ from the Perl canvas"):
        assert_ops_match([a], exp)


# --- architecture ----------------------------------------------------------------------

def test_draw_package_has_no_qt_imports():
    offenders = [
        str(p.relative_to(SRC))
        for p in (SRC / "gui" / "draw").rglob("*.py")
        if any(w in p.read_text() for w in ("PySide6", "PyQt", "import Qt", "from Qt"))
    ]
    assert not offenders, offenders


def _gui_imports(path):
    """(function name or None, how) of every GUI import in ``path``: import statements of
    seqsee.gui (absolute or relative) and string literals naming it (import_module)."""
    import ast
    tree = ast.parse(path.read_text())
    found = []

    def visit(node, func):
        for child in ast.iter_child_nodes(node):
            f = child.name if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef)) else func
            if isinstance(child, ast.Import):
                found.extend((f, a.name) for a in child.names
                             if a.name.startswith("seqsee.gui"))
            elif isinstance(child, ast.ImportFrom):
                mod = child.module or ""
                names = [a.name for a in child.names]
                if mod.startswith("seqsee.gui") or (child.level and (
                        mod.split(".")[0] == "gui" or (not mod and "gui" in names))) or (
                        mod == "seqsee" and "gui" in names):
                    found.append((f, f"from {'.' * child.level}{mod}"))
            elif isinstance(child, ast.Call) and child.args \
                    and isinstance(child.args[0], ast.Constant) \
                    and str(child.args[0].value).startswith("seqsee.gui"):
                found.append((f, f"call {child.args[0].value}"))
            visit(child, f)

    visit(tree, None)
    return found


def test_core_never_imports_gui():
    """The core never imports seqsee.gui, except the one lazy import the ``--gui`` switch
    needs: ``importlib.import_module("seqsee.gui.app")`` inside cli.main (item 021)."""
    allowed = {("cli.py", "main", "call seqsee.gui.app")}
    found = {
        (str(p.relative_to(SRC)), func, how)
        for p in SRC.rglob("*.py")
        if "gui" not in p.relative_to(SRC).parts
        for func, how in _gui_imports(p)
    }
    assert found <= allowed, found - allowed
    assert _gui_imports(SRC / "gui" / "app.py")       # the checker does see imports


def test_core_imports_and_runs_without_pyside6():
    code = (
        "import sys; sys.modules['PySide6'] = None\n"
        "from seqsee import s, cli, seqsee_main\n"
        "s.reset_all()\n"
        "import seqsee.gui.draw.ops, seqsee.gui.draw.colors\n"
        "assert not any(m.startswith('PySide6.') for m in sys.modules), 'Qt was imported'\n"
        "print('ok')\n"
    )
    out = subprocess.run([sys.executable, "-c", code], capture_output=True, text=True,
                         cwd=PY, env={"PYTHONPATH": str(PY / "src"), "PATH": ""})
    assert out.returncode == 0 and out.stdout.strip() == "ok", out.stderr


# --- Qt plumbing -----------------------------------------------------------------------

@pytest.mark.gui
def test_qt_runs_offscreen(qtbot):
    import os

    from PySide6.QtGui import QGuiApplication
    from PySide6.QtWidgets import QGraphicsScene, QGraphicsView

    assert os.environ["QT_QPA_PLATFORM"] == "offscreen"
    assert QGuiApplication.platformName() == "offscreen"
    scene = QGraphicsScene(0, 0, 100, 50)
    scene.addLine(0, 0, 100, 50)
    view = QGraphicsView(scene)
    qtbot.addWidget(view)
    assert len(scene.items()) == 1
