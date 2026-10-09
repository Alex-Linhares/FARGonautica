"""Final audit (item 051): every Perl module under ``lib/`` has a PORTING_MAP.md row
marked done/skipped, and no unported ``NotImplementedError("TODO`` stubs remain in
``src/seqsee/``. Mirrors no single Perl file; it checks coverage of all of ``lib/``."""

import fnmatch
import pathlib
import pytest
import re

PY = pathlib.Path(__file__).resolve().parents[1]
REPO = PY.parent
LIB = REPO / "lib"
MAP = PY / "PORTING_MAP.md"


def _map_rows():
    rows = []
    for line in MAP.read_text().splitlines():
        if not line.startswith("|") or line.startswith("|---"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if cells[0].startswith("Perl module"):
            continue
        rows.append(cells)
    return rows


def _status_by_module():
    """Module path or glob (e.g. ``SGUI*``, ``Tk/*``) → statuses of its rows."""
    status = {}
    for cells in _map_rows():
        for mod in re.findall(r"[\w/]+\.p[ml]\b|[\w/]+\*", cells[0]):
            status.setdefault(mod, set()).add(cells[3])
    return status


def _covered(module, status):
    return module in status or any(
        "*" in pat and fnmatch.fnmatchcase(module, pat if pat.endswith(".pm") else pat + ".pm")
        for pat in status
    )


@pytest.mark.perl_source
def test_every_lib_module_has_a_row():
    status = _status_by_module()
    perl = sorted(str(p.relative_to(LIB)) for p in LIB.rglob("*.pm"))
    missing = [m for m in perl if not _covered(m, status)]
    assert perl and not missing, missing


GUI_MODULE = re.compile(r"^(SGUI\.pm|SGUI/|Tk/|Themes/|SColor\.pm|UI/Graphical\.pm)")


def _is_gui_row(cells):
    mods = re.findall(r"[\w/]+\.p[ml]\b", cells[0])
    return bool(mods) and all(GUI_MODULE.match(m) for m in mods)


def test_every_row_is_done_or_skipped():
    """Every row, the Perl GUI rows included (loop0002's final audit, item 024), is done/skipped."""
    bad = [(c[0][:60], c[3]) for c in _map_rows() if c[3] not in ("done", "skipped")]
    assert not bad, bad


@pytest.mark.perl_source
def test_gui_modules_have_one_row_each():
    """Item 000 split the single GUI row into one row per Perl GUI module."""
    perl = sorted(str(p.relative_to(LIB)) for p in LIB.rglob("*.pm"))
    gui = [m for m in perl if GUI_MODULE.match(m)]
    rows = {}
    for cells in _map_rows():
        if _is_gui_row(cells):
            for mod in re.findall(r"[\w/]+\.p[ml]\b", cells[0]):
                rows[mod] = rows.get(mod, 0) + 1
    assert gui and {m: rows.get(m, 0) for m in gui} == {m: 1 for m in gui}


def test_no_todo_stubs_left():
    hits = [
        f"{p.relative_to(PY)}:{i}"
        for p in (PY / "src").rglob("*.py")
        for i, line in enumerate(p.read_text().splitlines(), 1)
        if 'NotImplementedError("TODO' in line or "NotImplementedError('TODO" in line
    ]
    assert not hits, hits


def test_readme_documents_usage():
    readme = PY / "README.md"
    assert readme.exists()
    text = readme.read_text()
    for needle in ("python3 -m seqsee", "python3 -m pytest", "PORTING_MAP.md"):
        assert needle in text, needle
