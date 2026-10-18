"""Loop0006 item 16: the results pane (`tabletop/gui/results_view.py`; G-052) in the
window, offscreen, pytest-qt, one shared window with the results dock floated, after the
Numbo GUI's `test_gui_shell.py` and the Seek-Whence GUI's view tests.

- A batch of 5 through the pane equals `run_batch` run for run, with the progress bar, the
  table's cells, the bars' pixels (French's black, ours grey) and the footer.
- A cancelled batch leaves no thread and says so; a batch is refused while a run is live,
  and a live run while a batch runs; Run > Batch… raises the dock and starts one.
- The hover text of a bar; the batch line painted once per event during a capped run (the
  whole quick start is `tests_slow/gui/test_results_view_slow.py`'s); a bare pane and the
  minimum.
"""

import dataclasses
import time

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QEvent, QPointF, Qt                             # noqa: E402
from PySide6.QtGui import QColor, QMouseEvent                              # noqa: E402

from tabletop.config import PROFILES                                       # noqa: E402
from tabletop.gui import results_view as rv                                # noqa: E402
from tabletop.gui.models import results as rm                              # noqa: E402
from tabletop.problems import load_problem                                 # noqa: E402
from tabletop.run import run_batch                                         # noqa: E402
from test_coderack_view import qtbot_wait                                  # noqa: E402
from test_table_view import run_to_end                                     # noqa: E402

TIMEOUT = 60.0


@pytest.fixture(scope="module")
def shown(module_window, qapp):
    """The shared window with the results dock floated and large enough for the graph's
    pixels; docked again at teardown (a floating dock left on the offscreen screen takes
    the next module's mouse moves)."""
    w = module_window
    w.resize(1600, 1000)
    w.results_dock.setFloating(True)
    w.results_dock.resize(760, 560)
    w.results_dock.move(40, 40)
    w.results_dock.show()
    w.results_dock.raise_()
    qapp.processEvents()
    qtbot_wait(qapp)
    assert w.results_dock.isFloating()
    assert not w.results_view.graph.visibleRegion().isEmpty()
    yield w
    w.results_view.cancel_batch()
    w.results_view.worker.wait(TIMEOUT)
    w.results_dock.setFloating(False)


def wait_until(qapp, condition, timeout=TIMEOUT):
    deadline = time.monotonic() + timeout
    while not condition() and time.monotonic() < deadline:
        qapp.processEvents()
        time.sleep(0.002)
    assert condition()


def wait_for_batch(qapp, view):
    wait_until(qapp, lambda: view.model.state != "running")
    assert view.worker.wait(TIMEOUT) and not view.busy


def test_a_batch_of_5_through_the_pane_equals_run_batch(shown, qapp):
    w = shown
    view = w.results_view
    w.set_next_run("surround-05", "standard", 1)
    changes = []
    view.batch_changed.connect(lambda: changes.append(view.model.done))
    assert w.run_batch(5)
    assert view.busy and not view.run_button.isEnabled() and view.cancel_button.isEnabled()
    assert not view.count_box.isEnabled()
    wait_for_batch(qapp, view)
    view.batch_changed.disconnect()
    model = view.model
    plain = run_batch(load_problem("surround-05"), PROFILES["standard"], 1, 5)
    assert model.results == plain and model.state == "finished"
    assert changes == [1, 2, 3, 4, 5, 5]           # five runs, then the end
    assert view.progress.value() == 5 and view.progress.maximum() == 5
    assert view.count_box.value() == 5 and view.run_button.isEnabled()
    assert not view.cancel_button.isEnabled()
    assert view.cell_texts() == model.cells() and view.table.rowCount() == len(model.rows())
    assert [view.table.horizontalHeaderItem(j).text() for j in range(view.table.columnCount())] \
        == list(rm.HEADINGS)
    assert view.footer_label.text() == model.footer() and "chi-square" in model.footer()
    assert view.status_label.text() == model.status()
    assert view.next_label.text() == "next batch: surround-05 under standard, batch seed 1"
    assert w.statusBar().currentMessage() == "batch of 5 runs: surround-05 under standard, batch seed 1"
    assert view.graph.title == "Surround 5 [F95 p. 118 (img)]"
    assert view.set_combo.count() == 2 and view.set_combo.isVisible()   # F95 and FCCA


def pixel(widget, x, y):
    return QColor(widget.grab().toImage().pixel(int(x), int(y)))


def test_the_bars_are_frenchs_black_and_ours_grey(shown, qapp):
    view = shown.results_view
    graph = view.graph
    qapp.processEvents()
    layout = graph.layout_now()
    rows = view.model.rows()
    assert layout.bars and len(layout.bars) == 2 * len(rows)
    french = layout.bars[0]                         # E1: French 86 % [F95 p. 118 (img)]
    assert french.who == "French" and french.value == 86 and french.height > 20
    colour = pixel(graph, french.x + french.width / 2, french.y + french.height / 2)
    assert colour.lightness() < 40, colour.name()
    ours = next(b for b in layout.bars if b.who == "ours" and b.height > 4)
    colour = pixel(graph, ours.x + ours.width / 2, ours.y + ours.height / 2)
    assert 120 < colour.lightness() < 190 and colour.saturation() < 20, colour.name()
    # Above the plot the background shows.
    bx, by, bw, bh = layout.box
    assert pixel(graph, bx + bw - 2, by + 2).lightness() > 220
    assert graph.bar_at(french.x + 1, french.y + 1) == french
    assert graph.bar_at(bx + bw - 1, by + 1) is None


def test_the_hover_text_of_a_bar(shown, qapp):
    graph = shown.results_view.graph
    layout = graph.layout_now()
    french = layout.bars[0]
    point = QPointF(french.x + french.width / 2, french.y + french.height / 2)
    event = QMouseEvent(QEvent.Type.MouseMove, point, point, Qt.MouseButton.NoButton,
                        Qt.MouseButton.NoButton, Qt.KeyboardModifier.NoModifier)
    graph.mouseMoveEvent(event)
    assert graph.toolTip().startswith("E1: ") and "frequency: French 86 %" in graph.toolTip()
    away = QPointF(layout.box[0] + layout.box[2] - 1, layout.box[1] + 1)
    graph.mouseMoveEvent(QMouseEvent(QEvent.Type.MouseMove, away, away, Qt.MouseButton.NoButton,
                                     Qt.MouseButton.NoButton, Qt.KeyboardModifier.NoModifier))
    assert graph.toolTip().startswith(rv.NOTE)
    assert "[F95 p. 118 (img)]" in graph.toolTip()


def test_a_cancelled_batch_leaves_no_thread_and_says_so(shown, qapp):
    w = shown
    view = w.results_view
    assert w.run_batch(400)
    wait_until(qapp, lambda: view.model.done >= 1)
    view.cancel_button.click()
    wait_for_batch(qapp, view)
    model = view.model
    assert model.state == "cancelled" and 1 <= model.done < 400
    assert view.status_label.text().startswith(f"cancelled after {model.done} of 400 runs")
    assert view.progress.value() == model.done and view.progress.maximum() == 400
    assert not view.worker.is_alive() and view.run_button.isEnabled()
    assert model.results == run_batch(load_problem("surround-05"), PROFILES["standard"], 1,
                                      model.done)


def test_a_batch_is_refused_while_a_run_is_live_and_the_reverse(shown, qapp):
    w = shown
    view = w.results_view
    w.set_next_run("surround-05", "standard", 3)
    w.step()                                     # a run paused at its first event
    wait_until(qapp, lambda: w.state == "paused")
    assert not w.run_batch(2) and not view.busy
    assert w.statusBar().currentMessage() == "a batch is refused while a run is live: stop it first"
    assert not view.run_button.isEnabled()
    assert not w.batch_from_menu()
    w.stop()
    wait_until(qapp, lambda: w.state == "stopped")
    assert not w.controller.is_alive() and view.run_button.isEnabled()
    # The reverse: a live run cannot start during a batch (the engine lock, G-052).
    assert w.run_batch(300)
    w.play()
    assert not w.controller.is_alive()
    assert "busy" in w.statusBar().currentMessage()
    view.cancel_batch()
    wait_for_batch(qapp, view)
    assert view.model.state == "cancelled"
    w.set_next_run("surround-05", "standard", 1)


def test_batch_from_the_run_menu_raises_the_dock_and_starts_one(shown, qapp):
    w = shown
    view = w.results_view
    w.results_dock.hide()
    assert not w.results_dock.isVisible()
    view.count_box.setValue(2)
    assert w.batch_action.isEnabled()
    w.batch_action.trigger()
    assert w.results_dock.isVisible() and view.busy
    wait_for_batch(qapp, view)
    assert view.model.state == "finished" and view.model.done == 2 and view.model.count == 2
    assert view.model.results == run_batch(load_problem("surround-05"), PROFILES["standard"], 1, 2)


def test_the_batch_takes_the_next_runs_configuration_and_seed(shown, qapp):
    w = shown
    view = w.results_view
    w.set_next_run("blockage-06", "human", 7)
    w.config_view.edit("max_codelets", "20")
    assert view.next_label.text() == "next batch: blockage-06 under human, batch seed 7"
    assert w.run_batch(3)
    wait_for_batch(qapp, view)
    config = w.next_config()
    assert config.profile == "human" and config.max_codelets == 20
    assert view.model.results == run_batch(load_problem("blockage-06"), config, 7, 3)
    assert all(r.capped for r in view.model.results)
    assert view.model.title() == "blockage-06 under human, batch seed 7"
    assert view.graph.title == "Blockage 6 [F95 p. 126 (img)]"
    w.config_view.reset_all()
    w.set_next_run("surround-05", "standard", 1)


class LineProbe:
    """At each `event_drawn`: the pane's line was painted exactly once since the event."""

    def __init__(self, window):
        self.view = window.results_view
        self.before = 0
        self.anomalies = []
        self.checked = 0
        window.add_view(self)
        window.event_drawn.connect(self.drawn)

    def on_event(self, event):
        self.before = self.view.paints

    def drawn(self, index):
        self.checked += 1
        if self.view.paints - self.before != 1:
            self.anomalies.append((index, self.view.paints - self.before))


def test_a_bare_pane_and_the_minimum(qtbot):
    view = rv.ResultsView()
    qtbot.addWidget(view)
    assert view.pane_id == "P8" and view.title == "Results"
    assert "[F95 pp. 115-139]" in view.pages and view.pages in view.toolTip()
    assert view.minimumSize().width() == 200 and view.minimumSize().height() == 70
    assert view.count_box.value() == 50 == rm.DEFAULT_COUNT
    assert view.cell_texts() == () and view.footer_label.text() == ""
    assert view.status_label.text().startswith("no batch yet")
    assert view.line.text() == "no batch yet: choose a count and press Run batch · no run yet"
    assert not view.busy and not view.cancel_button.isEnabled() and view.run_button.isEnabled()
    assert not view.set_combo.isVisible()
    view.show()
    assert view.graph.layout_now().bars == ()
    assert view.paints >= 0
    view.set_state("running")
    assert not view.run_button.isEnabled()
    view.set_state("finished")
    assert view.run_button.isEnabled()
    view.shutdown()


def test_the_line_is_painted_once_per_event_and_says_the_run(shown, qapp):
    """A run capped at three codelets through the window, with the probe (the whole quick
    start, 720 events since v5's fix (D-116 to D-120; 507 before), is the slow suite's
    `test_the_line_is_painted_once_per_event_over_the_quick_start`);
    the line's batch part still says the batch of 3 of the test before."""
    w = shown
    probe = LineProbe(w)
    done = []
    w.run_finished.connect(done.append)
    try:
        w.start_run("surround-05", dataclasses.replace(PROFILES["standard"], max_codelets=3), 3)
        w.controller.run_to_end()
        wait_until(qapp, lambda: bool(done))
    finally:
        w.run_finished.disconnect(done.append)
        w.event_drawn.disconnect(probe.drawn)
    assert w.controller.wait(TIMEOUT) and not w.controller.is_alive()
    outcome = done[0]
    n = len(w.history)
    assert outcome.result is not None and outcome.result.capped and probe.checked == n > 10
    assert probe.anomalies == []
    assert w.results_view.events == n and w.results_view.line.state == "finished"
    assert w.results_view.line.text().endswith(f" · finished: {n} events")
    assert w.results_view.line.text().startswith("3 runs")
