"""The slow tests of `tests/gui/test_gui_readme.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_gui_readme import (
    DOCS,
    LIMIT,
    PICTURES,
    PYTHON_DIR,
    fresh_env,
    pytest,
    re,
    subprocess,
    sys,
)

pytestmark = pytest.mark.slow


@pytest.mark.slow
def test_the_screenshot_script_renders_the_five_pictures_offscreen(tmp_path):
    """`python3 -m scripts.docs_screenshots --out DIR` writes the five PNGs, each under the
    limit, the window 1000 px wide, and prints a line per picture; the committed pictures
    are not touched (G-056). Marked slow: a window, two runs and five renders in a
    subprocess."""
    pytest.importorskip("PySide6")
    before = {name: (DOCS / name).read_bytes() for name in PICTURES}
    out = tmp_path / "shots"
    proc = subprocess.run([sys.executable, "-m", "scripts.docs_screenshots", "--out", str(out)],
                          cwd=PYTHON_DIR, env=fresh_env(), capture_output=True, text=True,
                          timeout=600)
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert proc.stderr == "", proc.stderr
    assert sorted(p.name for p in out.iterdir()) == sorted(PICTURES)
    for name in PICTURES:
        data = (out / name).read_bytes()
        assert data.startswith(b"\x89PNG\r\n\x1a\n"), name
        assert 1000 < len(data) < LIMIT, (name, len(data))
        assert f"{name}: " in proc.stdout, name
    width = int(re.search(r"gui-window\.png: (\d+) x", proc.stdout).group(1))
    assert width == 1000
    assert {name: (DOCS / name).read_bytes() for name in PICTURES} == before
