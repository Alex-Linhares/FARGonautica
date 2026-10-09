"""Final GUI audit (loop0002 item 024). Mirrors no single Perl file: it checks that every file of
the Perl/Tk GUI (lib/SGUI.pm, lib/SGUI/*.pm, lib/SGUI/List/*.pm, lib/Tk/*, lib/Themes/Std2.pm,
lib/SColor.pm, lib/UI/Graphical.pm) has a PORTING_MAP.md row marked done or skipped (with a
reason), that ``seqsee/gui`` has no ``NotImplementedError("TODO`` stubs, and that README.md
documents the GUI: how to launch it, its controls and its screenshots."""

import pathlib
import pytest
import re

from seqsee.gui.draw import views

PY = pathlib.Path(__file__).resolve().parents[1]
LIB = PY.parent / "lib"
MAP = PY / "PORTING_MAP.md"
README = PY / "README.md"
GUI_SRC = PY / "src" / "seqsee" / "gui"

GUI_FILE = re.compile(r"^(SGUI\.pm|SGUI/|Tk/|Themes/|SColor\.pm|UI/Graphical\.pm)")


def _rows():
    rows = []
    for line in MAP.read_text().splitlines():
        if line.startswith("|") and not line.startswith("|---"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if not cells[0].startswith("Perl module"):
                rows.append(cells)
    return rows


def _gui_files():
    return sorted(str(p.relative_to(LIB)) for p in LIB.rglob("*")
                  if p.is_file() and GUI_FILE.match(str(p.relative_to(LIB))))


def _rows_naming(path):
    pat = re.compile(r"(?<![\w/])" + re.escape(path) + r"(?![\w/])")
    return [c for c in _rows() if pat.search(c[0])]


@pytest.mark.perl_source
def test_every_perl_gui_file_has_a_finished_row():
    files = _gui_files()
    assert "SGUI.pm" in files and "Tk/Seqsee.pm" in files and "SColor.pm" in files
    missing = [f for f in files if not _rows_naming(f)]
    assert not missing, missing
    unfinished = [(f, c[3]) for f in files for c in _rows_naming(f)
                  if c[3] not in ("done", "skipped")]
    assert not unfinished, unfinished


def test_skipped_gui_rows_give_a_reason():
    for f in _gui_files():
        for c in _rows_naming(f):
            if c[3] == "skipped":
                assert len(c) > 4 and len(c[4]) > 10, (f, c)


def test_every_gui_row_is_done_or_skipped():
    """Rows about the GUI without a Perl module (renderer, runner, shell, …) are finished too."""
    bad = [(c[0][:60], c[3]) for c in _rows()
           if (any(GUI_FILE.match(m) for m in re.findall(r"[\w/]+\.(?:pm|xbm)\b", c[0]))
               or "GUI" in c[0] or "Qt" in c[0])
           and c[3] not in ("done", "skipped")]
    assert not bad, bad


def test_no_todo_stubs_in_gui():
    hits = [f"{p.relative_to(PY)}:{i}"
            for p in GUI_SRC.rglob("*.py")
            for i, line in enumerate(p.read_text().splitlines(), 1)
            if re.search(r"NotImplementedError\(\s*[\"']TODO", line)]
    assert not hits, hits


def _gui_section():
    text = README.read_text()
    m = re.search(r"^## GUI\b.*?(?=^## |\Z)", text, re.M | re.S)
    assert m, "README.md has no '## GUI' section"
    return m.group(0)


def test_readme_no_longer_says_the_gui_is_not_ported():
    assert "is not ported" not in README.read_text()


def test_readme_gui_section_explains_launching():
    sec = _gui_section()
    for needle in ("python3 -m seqsee --gui", "python3 -m seqsee.gui", "PySide6", "--seq",
                   "--seed", "--view", "QT_QPA_PLATFORM=offscreen"):
        assert needle in sec, needle


def test_readme_gui_section_lists_the_controls():
    from seqsee.gui.qt import controls
    sec = _gui_section()
    for b in controls.BINDINGS:
        assert re.search(rf"`{b.key}`", sec), b.key
    for menu in ("View", "Panes", "Run", "Save", "Help"):
        assert f"**{menu}**" in sec, menu
    for v in views.VIEW_OPTIONS:
        assert v.title in sec, v.title
    for needle in ("Sleep", "Restart", "PNG", "SVG", "Commentary"):
        assert needle in sec, needle


def test_readme_gui_section_shows_screenshots_that_exist():
    sec = _gui_section()
    images = re.findall(r"!\[[^\]]*\]\(([^)]+)\)", sec)
    assert any(i.startswith("docs/gui/screens/") for i in images), images
    assert any(i.startswith("docs/gui/perl/") for i in images), images
    missing = [i for i in images if not (PY / i).is_file()]
    assert not missing, missing


def test_pyproject_declares_the_gui_extra():
    text = (PY / "pyproject.toml").read_text()
    assert re.search(r'^gui\s*=\s*\[\s*"PySide6', text, re.M)
    assert "pytest-qt" in text
