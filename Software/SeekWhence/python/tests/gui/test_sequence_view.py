"""Loop0004 item 03: the sequence pane (`seekwhence/gui/sequence_view.py`) in the
window, offscreen, pytest-qt (G-014, G-015, G-017).

- Typing `3 7 3 7` in the box on seed 54 and clicking OK equals the scripted run: the
  same events, transcript and draws [M86 pp. 164-166].
- The box is enabled only while an interactive run waits at a prompt; several terms
  queue; terms with no interactive run live are refused; End input ends with `eof`.
- The strip shows the cells, the ghost block and the badges of run 3, with tooltips and
  cell clicks [M86 pp. 175, 179]; seeking shows the past.
- Stop and close with the worker waiting for a term leave no live thread.
"""

import io

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import Qt                                # noqa: E402

from seekwhence.events import Recorder                        # noqa: E402
from seekwhence.gui.models import presets                     # noqa: E402
from seekwhence.session import Session                        # noqa: E402
from seekwhence.trace import event_record                     # noqa: E402

from golden import APPENDIX                                   # noqa: E402

TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def plain(config, seed, lines, answers):
    rec, out = Recorder(), io.StringIO()
    session = Session(config, seed, out=out, lines=list(lines), answers=list(answers),
                      observers=[rec])
    session.run()
    return session, [event_record(e) for e in rec.events], out.getvalue()


def records_of(window):
    return [event_record(e) for e in window.history.events]


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def type_terms(qtbot, window, text):
    edit = window.sequence_view.input_edit
    assert edit.isEnabled()
    qtbot.keyClicks(edit, text)
    qtbot.keyClick(edit, Qt.Key.Key_Return)


def click(qtbot, button):
    assert button.isEnabled()
    qtbot.mouseClick(button, Qt.MouseButton.LeftButton)


# -- typing the terms (G-015) -----------------------------------------------------------------------

def test_typing_3_7_3_7_and_answering_ok_equals_the_scripted_run(window, qtbot):
    inputs = presets.interactive_inputs(54)
    view = window.sequence_view
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
        qtbot.waitUntil(lambda: view.input_enabled, timeout=TIMEOUT)
        assert window.history.events[window.position].kind == "start"
        assert window.feed is not None
        type_terms(qtbot, window, "3 7 3 7")
        assert view.input_edit.text() == ""
        qtbot.waitUntil(lambda: window.guess_panel.waiting, timeout=TIMEOUT)
        assert window.guess_panel.pending.block == "3 7 3 7"              # [M86 p. 166]
        assert view.model.ghost_text() == "3 7 3 7" and view.queued == ()
        assert not view.input_enabled
        click(qtbot, window.guess_panel.ok_button)
    session, records, text = plain(inputs.config, 54, [3, 7, 3, 7], ["ok"])
    assert records_of(window) == records and len(records) == 174
    assert window.outcome.output == text
    assert window.outcome.result["draws"] == session.world.rng.draws
    assert window.outcome.result["outcome"] == "ok"
    assert window.feed.read == ["3", "7", "3", "7"]
    assert [(b.block, b.answer) for b in view.model.badges] == [("3 7 3 7", "ok")]
    assert [c.fate for c in view.model.terms] == ["unfiltered"] * 3 + ["predicted"]
    assert not view.input_enabled and not window.controller.is_alive()


def test_the_box_is_enabled_only_at_a_prompt_of_an_interactive_run(window, qtbot):
    view = window.sequence_view
    assert not view.input_enabled
    # A scripted run never enables it.
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert not view.input_enabled
    # An interactive run: enabled at the start, off once a term is read, on at the prompt.
    window.start_session(presets.interactive_inputs(54))
    window.controller.run_to_end()
    qtbot.waitUntil(lambda: view.input_enabled, timeout=TIMEOUT)
    type_terms(qtbot, window, "3 7")
    # Both terms read and the display at the prompt after them (the queue label is
    # refreshed at each draw, so it is empty only once that prompt is drawn).
    qtbot.waitUntil(lambda: window.feed.read == ["3", "7"] and view.input_enabled
                    and view.queued == (), timeout=TIMEOUT)
    assert window.history.events[window.position].kind == "prompt"
    assert view.model.waiting and len(view.model.terms) == 2
    window.stop()
    assert not window.controller.is_alive() and not view.input_enabled


def test_terms_with_no_interactive_run_live_are_refused(window, qtbot):
    assert window.enter_terms("3 7") == 0
    assert "no interactive run" in window.statusBar().currentMessage()
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert window.enter_terms("3") == 0


def test_end_input_ends_the_session_with_eof(window, qtbot):
    view = window.sequence_view
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(presets.interactive_inputs(54))
        window.controller.run_to_end()
        qtbot.waitUntil(lambda: view.input_enabled, timeout=TIMEOUT)
        assert window.enter_terms("3 7") == 2
        qtbot.waitUntil(lambda: window.feed.read == ["3", "7"] and view.input_enabled,
                        timeout=TIMEOUT)
        window.end_input()
    assert window.outcome.result["outcome"] == "eof"
    assert not window.controller.is_alive()


def test_stop_with_the_worker_waiting_for_a_term_leaves_no_live_thread(window, qtbot):
    window.start_session(presets.interactive_inputs(54))
    window.controller.run_to_end()
    qtbot.waitUntil(lambda: window.feed is not None and window.feed.waiting, timeout=TIMEOUT)
    window.stop()
    assert not window.controller.is_alive() and window.feed.cancelled
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.outcome.stopped
    # The next run is unaffected.
    inputs = presets.appendix_inputs(RUN1)
    run_to_end(qtbot, window, inputs)
    assert records_of(window) == plain(inputs.config, inputs.seed, inputs.lines,
                                       inputs.answers)[1]


def test_closing_with_the_worker_waiting_for_a_term_joins_the_worker(qtbot, thread_errors):
    from seekwhence.gui.main_window import MainWindow
    w = MainWindow(settings=None, layout_path=None)
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    w.start_session(presets.interactive_inputs(54))
    w.controller.run_to_end()
    qtbot.waitUntil(lambda: w.feed is not None and w.feed.waiting, timeout=TIMEOUT)
    assert w.shutdown()
    assert not w.controller.is_alive()
    w.close()


def test_the_interactive_preset_sets_the_inputs(window):
    window.choose_interactive()
    assert window.inputs.preset == presets.INTERACTIVE and window.inputs.lines is None
    assert window.inputs.interactive and window.inputs.seed == 54
    assert "interactive" in window.windowTitle()


# -- the strip (G-014) -----------------------------------------------------------------------------

def test_the_strip_shows_run_3s_cells_ghosts_and_badges(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    view = window.sequence_view
    strip = view.strip
    assert strip.paints > 0 and view.paints == strip.paints
    cells = strip.term_rects()
    assert [c.value for c, _ in cells] == [1, 2, 1, 2, 3, 1]
    assert [c.fate for c, _ in cells] == ["unfiltered"] * 5 + ["predicted"]
    ghosts = strip.ghost_rects()
    assert [g.value for g, _ in ghosts] == [1, 2, 1, 2, 3, 1, 2, 3, 4]
    assert [g.beyond for g, _ in ghosts] == [False] * 6 + [True] * 3
    # Ghosts sit under their positions.
    for (ghost, rect), (_cell, cell_rect) in zip(ghosts, cells):
        assert rect.x() == cell_rect.x() and rect.y() > cell_rect.bottom()
    assert [(b.block, b.verdict) for b in view.model.badges] == [
        ("(1 2)(1 2)(1 2)", "no"), ("(1 2)(1 2 3)(1 2 3 4)", "ok")]    # [M86 pp. 175, 179]
    assert strip.tooltip_at(cells[5][1].center()).startswith("term 6: 1, predicted")
    assert "not entered yet" in strip.tooltip_at(ghosts[8][1].center())
    assert strip.tooltip_at(strip.rect().bottomRight()) == ""
    clicked = []
    view.cell_clicked.connect(clicked.append)
    qtbot.mouseClick(strip, Qt.MouseButton.LeftButton, pos=cells[2][1].center())
    assert clicked == [3]


def test_seeking_shows_the_past(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view = window.sequence_view
    events = window.history.events
    at_hypothesis = next(i for i, e in enumerate(events) if e.kind == "hypothesis")
    assert window.seek(at_hypothesis)
    assert [c.value for c in view.model.terms] == [3, 7, 3]
    assert view.model.ghost_text() == "3 7 3" and view.model.badges == []
    assert view.events == at_hypothesis + 1
    assert window.seek(len(events) - 1)
    assert view.model.ghost_text() == "3 7 3 7" and len(view.model.badges) == 1


def test_an_empty_strip_says_so(window):
    view = window.sequence_view
    assert view.model.terms == [] and view.strip.term_rects() == []
    assert view.strip.cell_size() >= 16
