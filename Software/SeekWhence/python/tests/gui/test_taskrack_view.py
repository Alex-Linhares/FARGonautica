"""Loop0004 item 07: the taskrack pane (`seekwhence/gui/taskrack_view.py`) in the window,
offscreen, pytest-qt (G-027, G-028, G-029; Numbo GUI, `test_pnet_coderack_views.py`).

- The dock holds the view, labelled "GUI design, not Meredith's" (ANSWERS.md 4).
- After run 1 the rack list's lines equal the pure `rack_lines` of a model built from the
  same events and the history has the run's 52 tasks [M86 p. 166]; seeking shows the
  first tasks [M86 p. 164], a stale one grey, and the end again.
- A click on a row lists its tasks; "every task"; the arena filter (DECISION D-057).
- A click on a task selects the structures its arguments name in the shared selection
  (G-025), the cytoplasm shows it, a second click clears; a selection made elsewhere
  outlines the tasks naming it.
- A new run clears the history; a 26,000-task history stays responsive: feeding and
  redrawing every event, then a paint, in bounded time.
- One offscreen render.
"""

import io
import time

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, Qt                             # noqa: E402

from seekwhence import events                                     # noqa: E402
from seekwhence.gui.models import presets                         # noqa: E402
from seekwhence.gui.models import taskrack as tr                  # noqa: E402
from seekwhence.gui.models.event_log import EventLog              # noqa: E402
from seekwhence.gui.models.tap import Tap                         # noqa: E402
from seekwhence.gui.taskrack_view import TaskrackView             # noqa: E402
from seekwhence.session import Session                            # noqa: E402

from golden import APPENDIX, appendix_inputs                      # noqa: E402

TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def logged(run_id):
    config, lines, answers = appendix_inputs(run_id)
    log = EventLog()
    tap = Tap(log)
    session = Session(config, SEEDS[run_id], out=io.StringIO(), lines=list(lines),
                      answers=list(answers), observers=[tap])
    tap.attach_session(session)
    session.run()
    return log


def model_at(log, index, selection=None):
    model = tr.TaskrackModel(selection)
    for i in range(index + 1):
        model.on_event(log.events[i])
        model.update(log.state_at(i))
    return model


def indexes(log, kind):
    return [i for i, e in enumerate(log.events) if e.kind == kind]


def click_row(qtbot, lst, row):
    """A left click on the row of a `LineList`."""
    lst.verticalScrollBar().setValue(max(0, row - lst.rows_in_view() + 1))
    y = (row - lst.first_row()) * lst.row_height + lst.row_height // 2
    qtbot.mouseClick(lst.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(20, y))


def test_the_dock_holds_the_view_labelled_gui_design(window):
    view = window.taskrack_view
    assert window.taskrack_dock.widget() is view and isinstance(view, TaskrackView)
    assert view.pane_id == "P6" and "GUI design, not Meredith's" in view.pages
    assert view.label.text() == "GUI design, not Meredith's"
    assert "[M86 pp. 41-42]" in view.label.toolTip() and "Numbo" in view.label.toolTip()
    assert "GUI design, not Meredith's" in window.taskrack_dock.toolTip()
    assert view.lines()[0].text == "nothing chosen yet" and view.history_rows() == []
    assert view.tally_label.text() == "since the start: nothing chosen"
    assert view.model.selection is window.selection


def test_run_1_through_the_window_matches_the_model(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view = window.taskrack_view
    log = window.history
    assert len(log) == 174 and view.events == 174
    model = model_at(log, 173)
    assert view.lines() == tr.rack_lines(model)
    assert view.model.corrections == 0
    assert [c.clock for c in view.history_rows()] == list(range(1, 53))       # [M86 p. 166]
    assert view.count_label.text() == "52 chosen"
    assert view.tally_label.text() == tr.tally_text(model)
    assert view.list.row_count() == 52 and view.rack.row_count() == len(view.lines())
    assert view.paints > 0 and view.list.paints > 0
    # The last history row is in view (Follow).
    assert view.list.first_row() + view.list.rows_in_view() >= 52
    assert view.list.line(51).text == tr.history_text(model.history[-1])


def test_seeking_shows_the_rack_of_the_past(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view = window.taskrack_view
    log = window.history
    first, second = indexes(log, "chosen")[:2]
    assert window.seek(first)
    assert view.lines()[0].text == "doing task Dissolver" and view.lines()[0].tint
    assert "chance 33.3% (weight 1 of 3 before the draw)" in view.lines()[1].text
    assert len(view.history_rows()) == 1 and view.list.line(0).text == "1  doing task Dissolver"
    assert window.seek(second)
    assert view.lines()[0].text == "doing task Sparkler-plus on (glint1 glint1 2)"  # [M86 p. 164]
    assert len(view.history_rows()) == 2
    stale = indexes(log, "stale")[0]
    assert window.seek(stale)
    assert view.lines()[0].grey and view.lines()[0].text.endswith("  stale")
    assert view.list.line(view.list.row_count() - 1).grey
    assert window.seek(173)
    assert view.lines() == tr.rack_lines(model_at(log, 173))
    assert len(view.history_rows()) == 52


def test_rows_expand_and_the_filter_applies(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    view = window.taskrack_view
    log = window.history
    # Seek to the rack at its fullest among the task boundaries.
    fullest = max((i for i in indexes(log, "chosen")),
                  key=lambda i: len(log.state_at(i).rack.tasks))
    assert window.seek(fullest)
    rows = [l for l in view.lines() if l.kind == "row"]
    assert len(rows) >= 3 and view.expanded == frozenset()
    assert not any(l.kind == "task" for l in view.lines())
    first = rows[0]
    assert view.toggle_row(first.type) and view.expanded == {first.type}
    tasks = [l for l in view.lines() if l.kind == "task"]
    assert len(tasks) == int(first.count)
    assert view.lines() == tr.rack_lines(view.model, {first.type})
    # A click on the row folds it again.
    row = next(i for i, l in enumerate(view.lines()) if l.kind == "row")
    click_row(qtbot, view.rack, row)
    assert view.expanded == frozenset()
    # "every task" lists them all.
    view.set_expand_all(True)
    assert view.expand_box.isChecked()
    assert len([l for l in view.lines() if l.kind == "task"]) == len(view.model.waiting)
    view.set_expand_all(False)
    assert not any(l.kind == "task" for l in view.lines())
    # The arena filter.
    arenas = {l.arena for l in rows}
    view.set_arena("cytoplasm")
    assert view.arena_box.currentData() == "cytoplasm"
    assert {l.arena for l in view.lines() if l.kind == "row"} <= {"cytoplasm"}
    assert all(c.arena == "cytoplasm" for c in view.history_rows())
    assert view.history_rows() == [c for c in view.model.history if c.arena == "cytoplasm"]
    view.arena_box.setCurrentIndex(view.arena_box.findData("socratoplasm"))
    assert view.arena == "socratoplasm"
    assert {l.arena for l in view.lines() if l.kind == "row"} <= {"socratoplasm"}
    view.set_arena(None)
    assert [l.arena for l in view.lines() if l.kind == "row"] == [l.arena for l in rows]
    assert len(view.history_rows()) == len(view.model.history) and arenas


def test_a_click_on_a_task_selects_what_it_names(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view = window.taskrack_view
    log = window.history
    second = indexes(log, "chosen")[1]            # the Sparkler-plus on (glint1 glint1 2)
    assert window.seek(second)
    assert window.selection.empty
    with qtbot.waitSignal(view.selected, timeout=TIMEOUT):
        click_row(qtbot, view.rack, 0)
    assert window.selection.kind == "task" and window.selection.includes("glint1")
    assert view.lines()[0].selected
    assert window.cytoplasm_view.view.model.is_selected("glint1")
    # A second click clears.
    click_row(qtbot, view.rack, 0)
    assert window.selection.empty and not view.lines()[0].selected
    # The history list selects too.
    assert window.seek(173)
    rows = view.history_rows()
    row = max(i for i, c in enumerate(rows) if tr.names_in(c.args))
    history_task = rows[row]
    names = tr.names_in(history_task.args)
    with qtbot.waitSignal(view.selected, timeout=TIMEOUT):
        click_row(qtbot, view.list, row)
    assert window.selection.name == f"task{history_task.number}"
    assert all(window.selection.includes(n) for n in names)
    assert view.list.line(row).selected
    # A selection made elsewhere outlines the tasks naming it.
    window.selection.select("glint", "glint1")
    assert [view.list.line(i).selected for i in range(view.list.row_count())] == \
        [tr.names_in(c.args).__contains__("glint1") for c in view.history_rows()]
    window.selection.clear()
    assert not any(view.list.line(i).selected for i in range(view.list.row_count()))


def test_a_new_run_clears_the_history(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert len(window.taskrack_view.history_rows()) == 52
    run_to_end(qtbot, window, presets.appendix_inputs(RUN2))
    view = window.taskrack_view
    assert len(view.history_rows()) == 90 and view.count_label.text() == "90 chosen"
    assert view.lines() == tr.rack_lines(model_at(window.history, len(window.history) - 1))


def test_a_26000_task_history_stays_responsive(qtbot):
    """A standalone view fed 26,000 posted-and-chosen pairs with a redraw after every
    event, as the window does, then painted and scrolled, in bounded time (PANES.md
    P6: about 26,000 tasks on blackburn-11)."""
    view = TaskrackView(EventLog())
    qtbot.addWidget(view)
    view.resize(500, 400)
    view.show()
    qtbot.waitExposed(view)
    view.set_position(0)
    view.on_event(events.RunStarted(1, (("urgency_power", 1),)))
    view.redraw()
    start = time.perf_counter()
    index = 1
    for n in range(1, 26001):
        for event in (events.TaskPosted(n, "Tester", (f"spark{n}",), 10, "cytoplasm", n - 1),
                      events.TaskChosen(n, n, "Tester", (f"spark{n}",), 10, "cytoplasm")):
            view.set_position(index)
            view.on_event(event)
            view.redraw()
            index += 1
    fed = time.perf_counter() - start
    assert len(view.model.history) == 26000 and view.list.row_count() == 26000
    start = time.perf_counter()
    view.rack.viewport().repaint()
    view.list.viewport().repaint()
    painted = time.perf_counter() - start
    view.list.verticalScrollBar().setValue(0)
    view.list.viewport().repaint()
    view.list.verticalScrollBar().setValue(13000)
    view.list.viewport().repaint()
    assert view.list.line(12999).text.startswith("13000  doing task Tester on (spark13000)")
    assert fed < 20.0, fed
    assert painted < 0.5, painted
    assert view.count_label.text() == "26000 chosen"


def test_render_png(window, qtbot, tmp_path):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    view = window.taskrack_view
    view.set_expand_all(True)
    path = str(tmp_path / "taskrack.png")
    image = view.render_png(path)
    assert image.width() > 100 and image.height() > 100
    assert (tmp_path / "taskrack.png").stat().st_size > 1000
