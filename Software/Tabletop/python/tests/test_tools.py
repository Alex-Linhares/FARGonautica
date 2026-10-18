"""The digitization tools (D-003): `tools/measure_figure.py` reads and writes PNGs with the
standard library, finds a drawn frame, clusters the ink inside it and merges an icon's
fragments; `tools/render_problem.py` draws a problem file. Tested on synthetic images under a
temporary directory, so that no source image is needed.
"""

import importlib.util
import json
import sys
from pathlib import Path

import pytest

TOOLS = Path(__file__).resolve().parents[1] / "tools"
PROBLEMS = Path(__file__).resolve().parents[1] / "problems"


def load(name: str):
    spec = importlib.util.spec_from_file_location(name, TOOLS / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module  # render_problem imports measure_figure by name
    spec.loader.exec_module(module)
    return module


mf = load("measure_figure")
rp = load("render_problem")


def synthetic(width=480, height=200, frame=(10, 8, 200, 190), boxes=()):
    """White rows with a one-pixel grey frame at the left (where the figures draw the table,
    the bar graph taking the right part) and black filled boxes."""
    rows = [[250] * width for _ in range(height)]
    left, top, right, bottom = frame
    for x in range(left, right + 1):
        rows[top][x] = rows[bottom][x] = 150
    for y in range(top, bottom + 1):
        rows[y][left] = rows[y][right] = 150
    for x0, y0, x1, y1 in boxes:
        for y in range(y0, y1 + 1):
            for x in range(x0, x1 + 1):
                rows[y][x] = 20
    return rows


def test_png_round_trip(tmp_path):
    rows = synthetic()
    rgb = [[(v, v, v) for v in row] for row in rows]
    path = tmp_path / "a.png"
    mf.write_png(path, rgb)
    width, height, back = mf.read_png(path)
    assert (width, height) == (480, 200) and back == rows


def test_find_frame_and_components(tmp_path):
    boxes = [(20, 20, 25, 36), (40, 30, 53, 36), (60, 60, 61, 61)]
    rows = synthetic(boxes=boxes)
    frame = mf.find_frame(rows)
    assert frame == (10, 8, 200, 190)
    found = mf.components(rows, frame)
    assert [c["box"] for c in found] == boxes
    assert found[0]["pixels"] == 6 * 17
    fx, fy = found[0]["fx"], found[0]["fy"]
    assert abs(fx - (22.5 - 10) / 190) < 1e-9 and abs(fy - (28 - 8) / 182) < 1e-9


def test_merge_close_rebuilds_an_icon_from_fragments():
    # A spoon's bowl and its handle, one blank row apart; a second icon 14 columns away.
    rows = synthetic(boxes=[(20, 20, 25, 23), (22, 25, 23, 36), (40, 20, 45, 36)])
    frame = mf.find_frame(rows)
    parts = mf.components(rows, frame)
    assert len(parts) == 3
    merged = mf.merge_close(parts, frame, gap=2)
    assert [c["box"] for c in merged] == [(20, 20, 25, 36), (40, 20, 45, 36)]
    assert merged[0]["pixels"] == 24 + 24
    assert merged[0]["cx"] == 22.5 and merged[0]["cy"] == 28
    assert len(mf.merge_close(parts, frame, gap=1)) == 3, "one blank row needs a gap of 2"


def test_no_frame_raises():
    rows = [[250] * 480 for _ in range(200)]
    with pytest.raises(ValueError):
        mf.find_frame(rows)


def test_cli_prints_the_frame_and_components(tmp_path, capsys):
    rows = synthetic(boxes=[(20, 20, 25, 36)])
    path = tmp_path / "fig.png"
    mf.write_png(path, [[(v, v, v) for v in row] for row in rows])
    out = tmp_path / "big.png"
    assert mf.main([str(path), "--scale", "2", "--out", str(out)]) == 0
    text = capsys.readouterr().out
    assert "frame left=10 top=8 right=200 bottom=190" in text
    assert "   20    20    25    36   6  17  102" in text
    assert out.is_file()


def test_cli_takes_the_frame_by_hand(tmp_path, capsys):
    """`--frame L,T,R,B` replaces the profile search, for figures whose lines are too faint
    (Blockage 9, Buridan 12; D-003, docs/problems.md 7); a non-box is refused."""
    path = tmp_path / "f.png"
    mf.write_png(path, [[(v, v, v) for v in row] for row in synthetic()])
    assert mf.main([str(path), "--frame", "10,10,60,50"]) == 0
    out = capsys.readouterr().out
    assert "frame left=10 top=10 right=60 bottom=50 (inner 49 x 39 px)" in out
    assert mf.parse_frame("5,14,200,190") == (5, 14, 200, 190)
    with pytest.raises(ValueError, match="L < R"):
        mf.parse_frame("60,10,10,50")
    with pytest.raises(ValueError):
        mf.parse_frame("1,2,3")


def test_render_problem_draws_a_file_with_and_without_its_figure(tmp_path):
    problem = PROBLEMS / "surround-01.json"
    data = json.loads(problem.read_text())
    left, top, right, bottom = data["table"]["frame_px"]
    rows = synthetic(width=right + 20, height=bottom + 20, frame=(left, top, right, bottom),
                     boxes=[tuple(o["box_px"]) for o in data["objects"]])
    figure = tmp_path / "figure.png"
    mf.write_png(figure, [[(v, v, v) for v in row] for row in rows])
    out = tmp_path / "review.png"
    assert rp.main([f"{problem}:{figure}", str(problem), "--out", str(out), "--scale", "2"]) == 0
    width, height, back = mf.read_png(out)
    assert width > 2 * 2 * (right - left) and height > 2 * 2 * (bottom - top)
    assert any(v < 100 for row in back for v in row), "something was drawn"
