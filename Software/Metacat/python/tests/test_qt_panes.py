"""Panels in panes: the Qt hosts and the main window's splitter tree (loop0003 item 04).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

- the default layout of docs/qt-gui-plan.md 2.2: rows of 60/31/9 %, the
  fixed-aspect panes as wide as their aspect ratio makes them at the row's
  height, the Commentary and the Memory taking the rest;
- the main window's splitter tree holds the eleven panes where 2.2 puts them,
  the EEG hidden, and the panes are never collapsible;
- a Qt host (metacat/qt/hosts.py) is a pane: a QGraphicsView over its Qt
  canvas's scene.  Unscrollable windows are letterboxed at Tk's aspect ratio
  ((w+2):(h+2), as make-resizable sets it), the Temperature at the top; the
  scrolling ones fill the pane beside an always-shown scrollbar.  A pane's size
  goes to the viewport as Tk's <Configure> did (configure(w+2, h+2)), and the
  configures of several panes are fed one at a time, so that the single resize
  queue of general-graphics.ss drops none (anomalies: "One resize queue for
  every window");
- scroll regions and scrolling asked from any thread wait for the GUI thread's
  sync;
- golden runs driven directly with every window on Qt hosts in one window
  (render_qt_panes.py, a fresh process each) give their golden traces, and
  every pane has items; the run7 pictures at codelets 300 and 800 and at the
  answer keep the main colours of the tkinter snapshots in snapshots/views/.
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
import threading
from pathlib import Path

import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")

import golden_harness as g  # noqa: E402

HERE = Path(__file__).resolve().parent
SNAPSHOTS = HERE / "snapshots" / "views"
OUT = HERE / "screenshots-qt" / "panes"
SCRIPT = HERE / "render_qt_panes.py"

PANES = ["workspace", "slipnet", "coderack", "temperature", "trace", "commentary", "memory",
         "top-themes", "bottom-themes", "vertical-themes", "EEG"]


# --- the default layout -------------------------------------------------------

def test_default_sizes_at_1080p():
    from metacat.qt.mainwindow import default_sizes
    s = default_sizes(1920, 960, handle=4)
    rows = s["rows"]
    assert sum(rows) == 960 - 2 * 4
    assert abs(rows[0] - 0.60 * 952) <= 1 and abs(rows[1] - 0.31 * 952) <= 1
    top_h, mid_h = rows[0], rows[1]
    temperature, workspace, coderack, vthemes, commentary = s["top"]
    assert sum(s["top"]) == 1920 - 4 * 4
    assert temperature == max(60, round(0.06 * 1920))
    assert workspace == round(top_h * 802 / 602)
    assert coderack == round(top_h * 232 / 600)
    assert vthemes == round(top_h * 162 / 592)
    assert commentary >= 200
    slipnet, themes, memory = s["middle"]
    assert sum(s["middle"]) == 1920 - 2 * 4
    assert slipnet == round(mid_h * 652 / 311)
    assert themes == round((mid_h - 4) / 2 * 602 / 142)
    assert memory >= 200
    assert sum(s["themes"]) == mid_h - 4 and abs(s["themes"][0] - s["themes"][1]) <= 1


def test_default_sizes_shrink_the_fixed_panes_when_the_rest_is_too_small():
    from metacat.qt.mainwindow import default_sizes
    s = default_sizes(1100, 1000, handle=4)
    assert s["top"][-1] == 200 and s["middle"][-1] >= 200
    assert sum(s["top"]) == 1100 - 16 and sum(s["middle"]) == 1100 - 8
    assert s["rows"][2] >= 50


def test_the_trace_row_is_at_least_50_pixels():
    from metacat.qt.mainwindow import default_sizes
    assert default_sizes(1920, 400, handle=4)["rows"][2] == 50


# --- hosts --------------------------------------------------------------------

class FakeViewport:
    def __init__(self):
        self.configures = []

    def configure(self, w, h):
        self.configures.append((w, h))


def make_host(scrolling, w, h, bg=None):
    from metacat.gui.colors import c_white
    from metacat.qt.hosts import QtHost
    host = QtHost(scrolling, lambda top: None)
    host.make_canvas(w, h, bg or c_white)
    vp = FakeViewport()
    host.show_viewport(vp)
    return host, vp


def show(host, w, h):
    """the pane at w x h, shown (offscreen) so that it gets its resize events"""
    host.pane.resize(w, h)
    host.pane.show()


def settle(qapp):
    from metacat.qt import hosts
    assert hosts.settle_resizes(qapp.processEvents, timeout=10)


def test_install_sets_the_fonts_and_qt_scrollbar_sizes(qapp):
    from PySide6.QtWidgets import QStyle
    from metacat.gui import fonts as gfonts
    from metacat.gui import hosts as ghosts
    from metacat.qt import hosts
    from metacat.qt.canvas import HiddenCanvas
    hosts.install()
    try:
        extent = qapp.style().pixelMetric(QStyle.PM_ScrollBarExtent)
        assert gfonts.g_scrollbar_width == gfonts.g_scrollbar_height == extent
        assert isinstance(gfonts.g_hidden_canvas, HiddenCanvas)
        host = ghosts.make_window_host("none", lambda top: None)
        assert isinstance(host, hosts.QtHost)
    finally:
        ghosts.set_window_host_maker(ghosts.OffscreenHost)


def test_a_host_is_a_view_of_its_canvas_scene(qapp):
    from PySide6.QtWidgets import QGraphicsView
    host, _ = make_host("none", 230, 598)
    assert isinstance(host.pane.view, QGraphicsView)
    assert host.pane.view.scene() is host.canvas.scene
    assert host.pane.parent() is None and not host.pane.isVisible()   # no top-level window
    host.show_window()
    assert not host.pane.isVisible()   # still not a window of its own
    assert (host.get_width(), host.get_height()) == (232, 600)


def test_an_unscrollable_pane_is_letterboxed_at_tks_aspect_ratio(qapp):
    host, vp = make_host("none", 800, 600)
    host.set_resizable_bang(True, True)
    show(host, 1000, 600)
    settle(qapp)
    v = host.pane.view.geometry()
    w = round(600 * 802 / 602)
    assert (v.width(), v.height()) == (w, 600)
    assert v.x() == (1000 - w) // 2 and v.y() == 0
    assert vp.configures[-1] == (w + 2, 602)
    show(host, 400, 900)
    settle(qapp)
    v = host.pane.view.geometry()
    assert v.width() == 400 and v.height() == round(400 * 602 / 802)
    assert v.y() == (900 - v.height()) // 2
    assert vp.configures[-1] == (402, v.height() + 2)


def test_the_temperature_sits_at_the_top_of_its_pane(qapp):
    host, vp = make_host("none", 70, 175)
    host.set_resizable_bang(True, True)
    host.pane.v_align = "top"
    show(host, 300, 600)
    settle(qapp)
    v = host.pane.view.geometry()
    assert v.y() == 0 and v.height() == 600 and v.width() == round(600 * 72 / 177)


def test_no_configure_before_the_window_is_resizable(qapp):
    host, vp = make_host("none", 800, 600)
    show(host, 500, 500)
    settle(qapp)
    assert vp.configures == []
    host.set_resizable_bang(True, True)
    settle(qapp)
    assert vp.configures


def test_scrolling_panes_fill_the_pane_beside_their_scrollbar(qapp):
    from metacat.gui import fonts as gfonts
    from metacat.qt import hosts
    hosts.install_scrollbar_sizes()
    sb = gfonts.g_scrollbar_width
    host, vp = make_host("horizontal", 1000, 69)
    host.set_resizable_bang(True, True)
    assert host.get_scrollbar("horizontal") and not host.get_scrollbar("vertical")
    show(host, 1500, 90)
    settle(qapp)
    assert vp.configures[-1] == (1502, 90 - sb + 2)
    host, vp = make_host("vertical", 300, 600)
    host.set_resizable_bang(True, True)
    assert host.get_scrollbar("vertical") and not host.get_scrollbar("horizontal")
    show(host, 500, 700)
    settle(qapp)
    assert vp.configures[-1] == (500 - sb + 2, 702)


def test_a_pane_that_comes_back_to_its_size_still_gets_a_configure(qapp):
    """Tk would have sent a configure for each size; between two of the feeder's
    turns the view may have clamped its scroll position, so the panel must
    still redraw and reposition (found in the run7 resize picture)"""
    host, vp = make_host("vertical", 300, 600)
    host.set_resizable_bang(True, True)
    show(host, 400, 600)
    settle(qapp)
    n = len(vp.configures)
    host.pane.resize(500, 900)     # both before the feeder's next turn
    host.pane.resize(400, 600)
    settle(qapp)
    assert len(vp.configures) == n + 1 and vp.configures[-1] == vp.configures[n - 1]
    host.pane.resize(400, 600)     # no change at all: nothing to tell
    settle(qapp)
    assert len(vp.configures) == n + 1


def test_tiny_panes_send_no_configure(qapp):
    host, vp = make_host("none", 800, 600)
    host.set_resizable_bang(True, True)
    show(host, 2, 300)
    settle(qapp)
    assert vp.configures == []


def test_every_window_resized_at_once_redraws(qapp, capsys):
    """general-graphics.ss keeps one waiting resize for all windows; the hosts
    feed the configures one at a time, so each window's resize method runs."""
    from metacat.gui import general_graphics as gg
    from metacat.gui import hosts as ghosts
    from metacat.objects import tell
    from metacat.qt import hosts
    hosts.install()
    old_pause = gg.p_resize_listener_pause
    gg.p_resize_listener_pause = 20
    try:
        hosts.ensure_resize_listener()
        windows = [gg.make_unscrollable_graphics_window(100 + i, 50) for i in range(5)]
        for w in windows:
            tell(w, "make-resizable")
        capsys.readouterr()
        for i, w in enumerate(windows):
            show(tell(w, "get-toplevel"), 300 + i, 200)
        settle(qapp)
        assert capsys.readouterr().out.count("warning: no resize method defined") == 5
        for i, w in enumerate(windows):
            info = tell(w, "get-info")
            assert info[0][1] < 300 + i + 1 and info[0][3] <= 200
    finally:
        gg.p_resize_listener_pause = old_pause
        ghosts.set_window_host_maker(ghosts.OffscreenHost)


def test_scrolling_from_another_thread_waits_for_the_sync(qapp):
    host, vp = make_host("vertical", 300, 200)
    show(host, 300, 200)
    qapp.processEvents()

    def worker():
        host.set_scroll_region_bang(0, 0, 300, 1000)
        host.set_vertical_view(0.5)
    t = threading.Thread(target=worker)
    t.start()
    t.join()
    bar = host.pane.view.verticalScrollBar()
    assert bar.maximum() == 0          # nothing happened outside the GUI thread
    host.sync()
    qapp.processEvents()
    assert host.pane.view.sceneRect().height() == 1000
    assert bar.value() == 500


def test_the_scroll_offset_reaches_canvasx(qapp):
    host, vp = make_host("horizontal", 300, 100)
    show(host, 300, 100)
    host.set_scroll_region_bang(0, 0, 3000, 100)
    host.sync()
    qapp.processEvents()
    host.pane.view.horizontalScrollBar().setValue(700)
    qapp.processEvents()
    assert float(str(host.canvas.tcl("canvasx", 10))) == 710


def test_the_letterbox_margins_have_the_panels_background(qapp, grab):
    from PySide6.QtGui import QImage
    from metacat.gui import colors
    host, vp = make_host("none", 100, 100, colors.c_black)
    show(host, 300, 100)
    host.sync()
    image = QImage(str(grab(host.pane, "letterbox")))
    assert image.pixelColor(5, 50).name() == "#000000"
    assert image.pixelColor(295, 50).name() == "#000000"


# --- the main window ----------------------------------------------------------

def fake_hosts():
    sizes = {"workspace": ("none", 800, 600), "slipnet": ("none", 650, 309),
             "coderack": ("none", 230, 598), "temperature": ("none", 70, 175),
             "trace": ("horizontal", 1000, 69), "commentary": ("vertical", 300, 600),
             "memory": ("vertical", 260, 400), "top-themes": ("none", 600, 140),
             "bottom-themes": ("none", 600, 140), "vertical-themes": ("none", 160, 590),
             "EEG": ("horizontal", 900, 120)}
    return {name: make_host(*args)[0] for name, args in sizes.items()}


def test_the_splitter_tree(qapp, grab):
    from PySide6.QtCore import Qt
    from metacat.qt.mainwindow import MainWindow
    window = MainWindow()
    hosts = fake_hosts()
    window.place_hosts(hosts)
    window.resize(1920, 1010)
    window.show()
    qapp.processEvents()
    s = window.splitters
    assert s["rows"].orientation() == Qt.Vertical
    assert [s["rows"].widget(i) for i in range(3)] == [s["top"], s["middle"], s["bottom"]]
    assert [s["top"].widget(i) for i in range(5)] == [
        hosts[n].pane for n in ("temperature", "workspace", "coderack", "vertical-themes",
                                "commentary")]
    assert [s["middle"].widget(i) for i in range(3)] == [
        hosts["slipnet"].pane, s["themes"], hosts["memory"].pane]
    assert s["themes"].orientation() == Qt.Vertical
    assert [s["themes"].widget(i) for i in range(2)] == [hosts["top-themes"].pane,
                                                         hosts["bottom-themes"].pane]
    assert [s["bottom"].widget(i) for i in range(2)] == [hosts["trace"].pane,
                                                         hosts["EEG"].pane]
    for sp in s.values():
        assert not sp.childrenCollapsible()
    for name in PANES:
        assert window.panes[name] is hosts[name].pane
        assert hosts[name].pane.isVisible() == (name != "EEG"), name
    sizes = window.splitter_sizes()
    assert sizes["top"][1] > sizes["top"][2] > sizes["top"][3]
    assert sizes["rows"][0] > sizes["rows"][1] > sizes["rows"][2]
    grab(window, "empty-panes")
    window.close()


def test_the_main_window_syncs_every_pane(qapp):
    from metacat.qt.mainwindow import MainWindow
    window = MainWindow()
    hosts = fake_hosts()
    window.place_hosts(hosts)
    hosts["slipnet"].canvas.tcl("create", "rectangle", 1, 1, 5, 5)
    window.sync()
    assert len(hosts["slipnet"].canvas.scene.items()) == 1
    assert window.sync_timer.interval() == 50 and window.sync_timer.isActive()
    window.close()


def test_the_default_window_is_for_a_1080p_screen(qapp):
    from metacat.qt.mainwindow import DEFAULT_SIZE, MainWindow
    assert DEFAULT_SIZE == (1920, 1010)
    window = MainWindow()
    assert (window.width(), window.height()) == DEFAULT_SIZE
    window.close()


# --- golden runs in the panes --------------------------------------------------

def run_scenes(names, size=None, outdir=OUT, extra=()):
    """render_qt_panes.py for each scene, in parallel fresh processes"""
    env = dict(os.environ, QT_QPA_PLATFORM="offscreen")
    env.pop("WAYLAND_DISPLAY", None)
    procs = {n: subprocess.Popen([sys.executable, str(SCRIPT), str(outdir), n]
                                 + ([size] if size else []) + list(extra),
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
                                 env=env)
             for n in names}
    results = {}
    for n, p in procs.items():
        out, err = p.communicate(timeout=600)
        assert p.returncode == 0, "%s failed:\n%s" % (n, err[-3000:])
        results[n] = json.loads(out.strip().splitlines()[-1])
    return results


def assert_trace(scene, golden, prefix=False, outdir=OUT):
    got = (outdir / ("%s.jsonl" % scene)).read_text()
    expected = g.golden_text(golden)
    if prefix:
        # a capped run: the same events as the golden up to the cap
        got_lines, exp_lines = got.splitlines(), expected.splitlines()
        body = got_lines[1:-1]
        assert body == exp_lines[1:1 + len(body)], scene
        assert len(body) > 100
        return
    if got != expected:
        line, e, a = g.first_difference(expected, got)
        pytest.fail("%s: first difference at line %s\n  golden: %s\n  qt:     %s"
                    % (scene, line, e, a), pytrace=False)


def test_a_short_golden_in_the_panes():
    result = run_scenes(["abz-1000"])["abz-1000"]
    assert result["settled"]
    assert_trace("abz-1000", "a-b-z_1.jsonl")
    panes = result["panes"]
    assert sorted(panes) == sorted(PANES)
    for name, p in panes.items():
        assert p["visible"] == (name != "EEG"), name
        if name not in ("bottom-themes", "memory", "trace", "vertical-themes"):
            assert p["items"] > 0, (name, p)
            assert p["scene_items"] == p["items"], (name, p)
    for name in ("workspace", "slipnet", "coderack", "commentary"):
        assert (OUT / ("%s-abz-1000.png" % name)).exists()


def top_colours(image, n):
    from collections import Counter
    counts = Counter()
    step = max(1, (image.width() * image.height()) // 40000)
    k = 0
    for y in range(image.height()):
        for x in range(image.width()):
            if k % step == 0:
                counts[image.pixel(x, y) & 0xFFFFFF] += 1
            k += 1
    total = sum(counts.values())
    return {c: v / total for c, v in counts.most_common(n)}, counts, total


@pytest.fixture(scope="module")
def run7_results():
    return run_scenes(["run7-300", "run7-800", "run7-answer"])


@pytest.mark.slow
def test_run7_in_the_panes_gives_its_golden(run7_results):
    assert run7_results["run7-answer"]["answers"] == ["wyz"]
    assert_trace("run7-answer", "abc-abd-xyz_3852097033.jsonl")
    assert_trace("run7-300", "abc-abd-xyz_3852097033.jsonl", prefix=True)
    assert_trace("run7-800", "abc-abd-xyz_3852097033.jsonl", prefix=True)
    assert run7_results["run7-300"]["codelets"] == 300
    assert run7_results["run7-800"]["codelets"] == 800


@pytest.mark.slow
@pytest.mark.parametrize("scene", ["run7-300", "run7-800", "run7-answer"])
def test_every_pane_has_items(run7_results, scene):
    result = run7_results[scene]
    assert result["settled"]
    for name, p in result["panes"].items():
        if name == "bottom-themes" or (name == "memory" and scene != "run7-answer"):
            continue
        assert p["items"] > 0, (scene, name, p)
        assert p["scene_items"] == p["items"], (scene, name, p)


@pytest.mark.slow
@pytest.mark.parametrize("scene", ["run7-300", "run7-800", "run7-answer"])
def test_the_panes_keep_the_tk_snapshots_colours(run7_results, scene):
    """each pane, at its own size, shows the main colours of the tkinter
    snapshot of the same window at the same point (backgrounds and fills are
    drawn without antialiasing, so they are exact).  Black is the text's
    colour: Tk's core fonts are 1-bit, Qt's text is antialiased
    (docs/divergences.md), so pure black is much rarer in Qt and is not
    compared."""
    from PySide6.QtGui import QImage
    checked = 0
    for snap in sorted(SNAPSHOTS.glob("*-%s.png" % scene)):
        name = snap.name[:-len("-%s.png" % scene)]
        if name == "EEG":
            continue    # hidden by default in the Qt window
        tk = QImage(str(snap))
        qt = QImage(str(OUT / snap.name))
        assert not qt.isNull(), snap.name
        tk_top, _, _ = top_colours(tk, 4)
        _, qt_counts, qt_total = top_colours(qt, 0)
        for colour, share in tk_top.items():
            if share < 0.01 or colour == 0x000000:
                continue
            assert qt_counts[colour] / qt_total >= share / 4, (
                snap.name, "#%06x" % colour, share, qt_counts[colour] / qt_total)
        checked += 1
    assert checked >= 8


@pytest.mark.slow
def test_resizing_the_window_during_run7_changes_nothing():
    """the window resized 14 times during the run (every 150 codelets); the
    panels redraw on the resize listener while the run goes on; the trace is
    the golden, and at the end every pane is redrawn at its size (the
    Commentary scrolled to its last line, as reposition-vertical-scrollbar
    leaves it)"""
    outdir = OUT / "resize"
    result = run_scenes(["run7-answer"], outdir=outdir, extra=["--resize"])["run7-answer"]
    assert result["resizes"] >= 10 and result["settled_after"]
    assert_trace("run7-answer", "abc-abd-xyz_3852097033.jsonl", outdir=outdir)
    commentary = result["panes"]["commentary"]
    assert commentary["vscroll"][0] == commentary["vscroll"][1] > 0
    sb = result["scrollbar"]
    for name, p in result["panes"].items():
        if p["visible"]:
            assert p["canvas"] == ([p["view"][2] - sb, p["view"][3]]
                                   if name in ("commentary", "memory") else
                                   [p["view"][2], p["view"][3] - sb] if name == "trace"
                                   else p["view"][2:]), (name, p)


def test_the_program_opens_every_pane(tmp_path):
    """python3 -m metacat.qt: the windows made on Qt hosts and placed in the
    main window, resizable, before any run; --screenshot grabs the window
    just before it quits"""
    from PySide6.QtGui import QImage
    shot = tmp_path / "start.png"
    env = dict(os.environ, QT_QPA_PLATFORM="offscreen")
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run([sys.executable, "-m", "metacat.qt", "--quit-after", "1500",
                           "--screenshot", str(shot)],
                          capture_output=True, text=True, timeout=120, env=env,
                          cwd=str(HERE.parent))
    assert proc.returncode == 0, proc.stderr[-2000:]
    assert "Metacat" in proc.stdout
    shown = [line for line in proc.stdout.splitlines() if line.startswith("Panes: ")]
    assert shown == ["Panes: " + " ".join(n for n in PANES if n != "EEG")]
    image = QImage(str(shot))
    assert (image.width(), image.height()) == (1920, 1010)


def test_fonts_measure_from_several_threads_at_once(qapp):
    """fonts.ss's get-pixel-size creates a text on the one hidden canvas, asks
    its bbox and deletes all: the engine and the resize listener measuring at
    once deleted each other's items (a run7 resize run crashed: IndexError in
    get_pixel_size).  The Qt hidden canvas keeps one display list per thread."""
    from metacat.gui import fonts as gfonts
    from metacat.qt import hosts
    hosts.install()
    font = gfonts.make_mfont(gfonts.sans_serif, -14, ["normal"])
    expected = tell_size(font, "Bond builders")
    errors, results = [], []

    def worker():
        try:
            for _ in range(300):
                results.append(tell_size(font, "Bond builders"))
        except Exception as e:   # noqa: BLE001 - reported below
            errors.append(e)
    threads = [threading.Thread(target=worker) for _ in range(4)]
    interval = sys.getswitchinterval()
    sys.setswitchinterval(1e-6)        # switch threads as often as possible
    try:
        for t in threads:
            t.start()
        for t in threads:
            t.join()
    finally:
        sys.setswitchinterval(interval)
    assert errors == [] and set(map(tuple, results)) == {tuple(expected)}


def test_the_garbage_collector_runs_on_the_gui_thread_only(qapp):
    """The test above hung after the other Qt tests: the automatic garbage
    collector ran in a worker measuring text, holding PAINT_GATE, and freed an
    earlier test's host and pane (they refer to each other); ~QWidget waited
    there for the GUI thread, which was joining the worker.  Now a worker
    making cycles enough to start the automatic collector many times starts
    none: the GUI thread's timer collects instead."""
    import gc
    import time
    from metacat.qt import hosts
    hosts.install()
    collections = []

    def note(phase, _info):
        if phase == "start":
            collections.append(threading.current_thread().name)

    def make_cycles():
        for _ in range(100000):
            cycle = []
            cycle.append(cycle)
    gc.callbacks.append(note)
    try:
        worker = threading.Thread(target=make_cycles, name="cycle-maker")
        worker.start()
        worker.join(30)
        assert not worker.is_alive() and collections == []
        deadline = time.monotonic() + 10
        while not collections and time.monotonic() < deadline:
            qapp.processEvents()
            time.sleep(0.01)
    finally:
        gc.callbacks.remove(note)
    assert collections and set(collections) == {threading.main_thread().name}


def tell_size(font, text):
    from metacat.objects import tell
    return tell(font, "get-pixel-size", text)
