"""Layout polish: the main window on 1080p, 1440p and 4K screens (loop0003 item 08).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Each scenario of drive_qt_layout.py opens the window as `python3 -m metacat.qt`
does, in a fresh offscreen process on a screen of a given size:

- the default window fills the screen (maximised) when the screen is larger
  than the 1080p default, and the default sizes of docs/qt-gui-plan.md 2.2
  follow it;
- the splitter sizes, the hidden panes and the window geometry are saved in a
  QSettings file, 500 ms after a handle is dragged and at close, and restored
  at the next start; an old layout version is ignored;
- View > Reset layout restores the default panes and sizes, removes the saved
  layout, and the sizes follow the window again;
- the window has a minimum size that keeps the control strip and every pane
  at least at its minimum;
- high DPI: on a screen at device pixel ratio 2 the window is laid out in
  logical pixels and grabbed at twice the resolution;
- the window's icon is the original's logo (fonts.ss's create-mcat-logo);
- after a run, every pane is visible and has items at 1920x1080 and 2560x1440,
  and the run ends at its golden's codelet count and random state.
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")

HERE = Path(__file__).resolve().parent
SCRIPT = HERE / "drive_qt_layout.py"
OUT = HERE / "screenshots-qt" / "layout"

PANES = ["workspace", "slipnet", "coderack", "temperature", "trace", "commentary", "memory",
         "top-themes", "bottom-themes", "vertical-themes", "EEG"]
SPLITTERS = ["rows", "top", "middle", "themes", "bottom"]
HANDLE = 4


def drive(scenario, screen="1920x1080", settings=None, outdir=OUT):
    env = dict(os.environ)
    env.pop("WAYLAND_DISPLAY", None)
    args = [sys.executable, str(SCRIPT), str(outdir), scenario, "--screen", screen]
    if settings is not None:
        args += ["--settings", str(settings)]
    proc = subprocess.run(args, capture_output=True, text=True, timeout=400, env=env,
                          cwd=str(HERE.parent))
    assert proc.returncode == 0, "%s failed:\n%s\n%s" % (scenario, proc.stdout[-2000:],
                                                         proc.stderr[-4000:])
    return json.loads(proc.stdout.strip().splitlines()[-1])


def assert_default(sizes, default, hidden=("EEG",)):
    """the splitters' sizes are the default ones (a hidden pane has size 0;
    the Trace then fills the bottom row)"""
    for name in ("rows", "top", "middle", "themes"):
        assert sizes[name] == default[name], (name, sizes[name], default[name])
    assert sizes["bottom"][0] == default["bottom"][0]
    if "EEG" in hidden:
        assert sizes["bottom"][1] == 0


# --- the default layout with the EEG -----------------------------------------------

def test_the_eeg_doubles_the_bottom_row():
    from metacat.qt.mainwindow import default_sizes
    plain = default_sizes(1920, 990)
    s = default_sizes(1920, 990, eeg=True)
    assert sum(s["rows"]) == sum(plain["rows"]) == 990 - 2 * HANDLE
    assert abs(s["rows"][2] - 2 * plain["rows"][2]) <= 2
    assert sum(s["bottom"]) == s["rows"][2] - HANDLE
    assert abs(s["bottom"][0] - s["bottom"][1]) <= 1 and s["bottom"][0] >= 50
    assert abs(s["rows"][0] / s["rows"][1] - 60 / 31) < 0.02


# --- saving and restoring ------------------------------------------------------

@pytest.fixture(scope="module")
def saved(tmp_path_factory):
    ini = tmp_path_factory.mktemp("settings") / "metacat-qt.ini"
    first = drive("save", settings=ini)
    second = drive("restore", settings=ini)
    return first, second, ini


def test_a_new_window_has_the_default_layout(saved):
    first, _, _ = saved
    assert_default(first["initial"], first["initial_default"])


def test_dragging_a_handle_resizes_the_panes(saved):
    first, _, _ = saved
    before, after = first["initial"], first["dragged"]
    assert after["top"][1] == before["top"][1] - 120          # the Workspace
    assert after["top"][2] == before["top"][2] + 120          # the Coderack
    assert after["rows"][0] == before["rows"][0] + 40
    assert after["rows"][1] == before["rows"][1] - 40
    assert sum(after["top"]) == sum(before["top"])


def test_the_layout_is_saved_after_a_drag(saved):
    first, _, _ = saved
    assert not first["saved_at_once"]       # 500 ms after the last drag
    assert first["saved_after_delay"]


def test_hiding_a_pane_closes_the_gap(saved):
    first, _, _ = saved
    assert first["hidden"] == ["slipnet"]
    middle = first["at_close"]["middle"]
    # the Slipnet's handle goes with it
    assert middle[0] == 0 and sum(middle) == sum(first["dragged"]["middle"]) + HANDLE
    bottom = first["at_close"]["bottom"]
    assert bottom[1] > 0                    # the EEG shown


def test_the_settings_file_holds_the_layout(saved):
    first, _, _ = saved
    ini = set(first["ini"])
    assert {"layout/version", "window/geometry", "view/hidden", "layout/custom"} <= ini
    assert {"layout/" + s for s in SPLITTERS} <= ini


def test_save_and_restore_round_trips(saved):
    first, second, _ = saved
    assert second["geometry"] == first["geometry"]
    assert second["hidden"] == first["hidden"]
    assert second["view_checked"] == {n: n != "slipnet" for n in PANES}
    assert second["sizes"] == first["at_close"]


def test_reset_layout_restores_the_default(saved):
    first, second, _ = saved
    assert second["reset_hidden"] == ["EEG"]
    assert second["reset_view_checked"] == {n: n != "EEG" for n in PANES}
    assert_default(second["reset_sizes"], second["reset_default"])
    assert not [k for k in second["ini_after_reset"]
                if (k.startswith("layout/") and k != "layout/version") or k == "view/hidden"]


def test_after_a_reset_the_sizes_follow_the_window(saved):
    _, second, _ = saved
    assert second["grown_sizes"] != second["reset_sizes"]
    assert_default(second["grown_sizes"], second["grown_default"])


def test_a_default_layout_is_saved_as_default(saved):
    """after the reset, the close saves no splitter states: the next start
    lays the panes out for its own window size"""
    _, second, _ = saved
    ini = set(second["ini_after_close"])
    assert "layout/version" in ini and "view/hidden" in ini
    assert not {"layout/" + s for s in SPLITTERS} & ini


def test_an_old_layout_version_is_ignored(tmp_path):
    from PySide6.QtCore import QSettings
    from metacat.qt.mainwindow import SETTINGS_VERSION
    ini = tmp_path / "old.ini"
    s = QSettings(str(ini), QSettings.IniFormat)
    s.setValue("layout/version", SETTINGS_VERSION - 1)
    s.setValue("view/hidden", "workspace coderack")
    s.setValue("layout/custom", True)
    s.sync()
    r = drive("restore", settings=ini, outdir=tmp_path)
    assert r["hidden"] == ["EEG"]
    assert_default(r["sizes"], r["default"])


# --- the window on each screen ----------------------------------------------------

def test_the_window_fills_a_1080p_screen():
    r = drive("start", "1920x1080")
    assert r["window"] == [1920, 1080] and r["maximized"]
    assert r["image"] == [1920, 1080]


def test_the_window_has_a_minimum_size():
    r = drive("minimum", "1920x1080")
    assert r["minimum"][0] >= 1600 and r["minimum"][1] >= 800
    assert r["shrunk"] == r["minimum"]
    strip_width, hint, minimum = r["strip"]
    assert strip_width >= minimum
    for name, p in r["panes"].items():
        if p["visible"]:
            assert p["size"][0] >= p["minimum"][0] and p["size"][1] >= p["minimum"][1], (name, p)
    assert r["panes"]["commentary"]["minimum"][0] >= 120


def test_high_dpi_lays_out_in_logical_pixels():
    """a 4K screen at 200 %: a 1920x1080 logical window, grabbed at 3840x2160"""
    r = drive("start", "1920x1080@2")
    assert r["dpr"] == 2
    assert r["window"] == [1920, 1080]
    assert r["image"] == [3840, 2160]
    shown = {n: p for n, p in r["panes"].items() if p["visible"]}
    for name, p in shown.items():
        assert p["image"] == [2 * p["size"][0], 2 * p["size"][1]], (name, p)


def test_the_window_icon_is_the_logo(qapp):
    from PySide6.QtGui import QColor
    from metacat.qt import icon
    image = icon.logo_image(1)
    assert (image.width(), image.height()) == icon.LOGO_SIZE
    assert QColor(image.pixel(2, 2)).getRgb()[:3] == (135, 206, 250)     # light sky blue
    dark = sum(1 for x in range(image.width()) for y in range(image.height())
               if QColor(image.pixel(x, y)).lightness() < 80)
    assert dark > 100                                                     # "Metacat"
    sizes = {(s.width(), s.height()) for s in icon.app_icon().availableSizes()}
    assert {(16, 16), (32, 32), (64, 64), (256, 256)} <= sizes
    r = drive("start", "1920x1080")
    assert [256, 256] in r["icon_sizes"] and r["app_icon"]


# --- every pane after a run, at 1080p and 1440p -------------------------------------

@pytest.mark.slow
@pytest.mark.parametrize("screen", ["1920x1080", "2560x1440"])
def test_every_pane_is_visible_and_drawn_after_a_run(screen):
    r = drive("run", screen)
    w, h = (int(v) for v in screen.split("x"))
    assert r["window"] == [w, h]
    assert r["end"] == r["golden_end"]
    for name, p in r["panes"].items():
        assert p["visible"], name
        assert p["items"] > 0 and p["scene_items"] == p["items"], (name, p)
        assert p["colours"] >= 2, (name, p)
        assert p["size"][0] >= p["minimum"][0] and p["size"][1] >= p["minimum"][1], (name, p)
    # the EEG shown in the default layout: the bottom row doubles
    assert r["panes"]["trace"]["size"][1] >= 80 and r["panes"]["EEG"]["size"][1] >= 80
    if w == 2560:
        # the larger screen gives every pane more room than 1080p's layout
        from metacat.qt.mainwindow import default_sizes
        small = default_sizes(1920, 1000)
        assert r["panes"]["workspace"]["size"][0] > small["top"][1]
        assert r["panes"]["commentary"]["size"][0] > small["top"][4]
