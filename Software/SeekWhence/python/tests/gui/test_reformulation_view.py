"""Loop0004 item 11: the reformulation pane (`seekwhence/gui/reformulation_view.py`) in the
window, offscreen, pytest-qt (G-039, G-040, G-041; Numbo GUI, `test_pnet_coderack_views.py`).

- The dock holds the view, tabbed under the hypothesis dock and not on screen at first.
- Run 3 raises the pane at its first refutation and leaves it raised: at the end it shows
  two episodes, the freeze-dried list `(S-group 1 2) (4 terms)` over `(Countup 1) (2
  terms)` [M86 p. 179], the threshold plot and the storyboard's lines; the pane and the
  hypothesis pane are each painted exactly once per event while on screen.
- Seeking to the first episode's candidate shows one episode [M86 p. 174]; a click on a
  card seeks to its event; a click on an episode's title folds and unfolds it; "In series"
  adds p. 120's table [M86 p. 120].
- Hovering a card selects the gnoths and gloms it names in the shared selection and the
  socratoplasm shows them; leaving restores the selection.
- A new run clears; one offscreen render.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, Qt                                 # noqa: E402

from seekwhence.gui.models import presets                             # noqa: E402

from golden import APPENDIX                                           # noqa: E402

TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def on_screen(widget):
    return not widget.visibleRegion().isEmpty()


def row_of(view, predicate):
    return next(i for i, line in enumerate(view.lines()) if predicate(line))


def click_row(qtbot, view, row):
    lst = view.list
    lst.verticalScrollBar().setValue(max(0, row - 2))
    y = (row - lst.first_row()) * lst.row_height + lst.row_height // 2
    qtbot.mouseClick(lst.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(20, y))


def test_the_dock_holds_the_view_tabbed_under_the_hypothesis(window):
    view = window.reformulation_view
    assert window.reformulation_dock.widget() is view
    assert view.pane_id == "P7" and "[M86 pp. 87-126" in view.pages
    assert not on_screen(view) and on_screen(window.hypothesis_view)
    assert view.head_label.text() == "no refutation yet"
    assert [l.text for l in view.lines()] == ["no refutation yet"]
    assert view.freeze_dried_text() == "freeze-dried: none"
    assert view.plot.points == () and not view.series_button.isChecked()


class PaintProbe:
    """At each `event_drawn`: the reformulation and hypothesis panes were painted exactly
    once since the probe's `on_event` when on screen, and not at all when hidden."""

    def __init__(self, window):
        self.views = (window.reformulation_view, window.hypothesis_view)
        self.before = {}
        self.anomalies = []
        self.raised_at = None
        window.add_view(self)
        window.event_drawn.connect(self.drawn)

    def on_event(self, event):
        self.before = {v: v.paints for v in self.views}

    def drawn(self, index):
        for view in self.views:
            paints = view.paints - self.before[view]
            expected = 1 if on_screen(view) else 0
            if paints != expected:
                self.anomalies.append((index, view.pane_id, paints, expected))
        if self.raised_at is None and on_screen(self.views[0]):
            self.raised_at = index


def test_run_3_raises_the_pane_at_the_first_refutation_and_paints_once_per_event(window, qtbot):
    view = window.reformulation_view
    probe = PaintProbe(window)
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    assert probe.anomalies == []
    assert probe.raised_at == view.model.first_refutation == 118
    assert on_screen(view) and not on_screen(window.hypothesis_view)
    assert view.events == len(window.history) == 602
    assert view.head_label.text() == "2 episodes"
    assert view.state_label.text() == "reigning S-group  threshold 10.0"
    assert view.freeze_dried_text() == \
        "freeze-dried:\n(S-group 1 2) (4 terms)\n(Countup 1) (2 terms)"          # [M86 p. 179]
    assert [v for _, v in view.plot.points] == [8.0, 9.0, 8.0, 9.0, 10.0]
    assert view.plot.end is not None and view.plot.paints >= 1
    lines = view.lines()
    assert lines[0].kind == "episode" and lines[0].collapsed and lines[0].episode == 1
    assert lines[0].text.startswith("episode 1: (Countup 1) refuted at term 3")
    assert lines[1].kind == "episode" and not lines[1].collapsed
    assert lines[1].text.endswith("candidate (S-group 1 (Countup 2))")
    assert [l.text for l in lines if l.kind == "head"][:4] == [
        "refutation", "freeze-dried", "gnoths set", "slip-check"]
    assert any(l.text == "set-out -- validity: 0" and l.kind == "bad" for l in lines)
    assert any(l.text == "new hypoth candidate (S-group 1 (Countup 2))" for l in lines)


def test_seeking_clicking_folding_and_the_series(window, qtbot):
    view = window.reformulation_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    assert window.seek(279)                                            # the first candidate
    assert view.head_label.text() == "1 episode"
    lines = view.lines()
    assert lines[0].kind == "episode" and not lines[0].collapsed
    assert [l.text for l in lines if l.kind == "head"][-2:] == ["casts", "candidate"]
    assert "casts: ((S-group 1 2) (S-group 1 1))" in [l.text for l in lines]  # [M86 p. 174]
    assert any(l.text.startswith("gnoth operation  (SHIFT-RIGHT") for l in lines)
    before = [l.text for l in lines if l.kind == "pic"]
    assert "  gnoth2: [ 1      2 ]" in before and "  gnoth1: [] (disappears)" in before
    # a click on the casts card seeks to its event
    row = row_of(view, lambda l: l.kind == "head" and l.text == "casts")
    assert view.lines()[row].index == 278
    with qtbot.waitSignal(view.card_clicked, timeout=TIMEOUT):
        click_row(qtbot, view, row)
    assert window.position == 278
    assert [l.text for l in view.lines() if l.kind == "head"][-1] == "casts"
    # fold and unfold the episode by clicking its title
    click_row(qtbot, view, 0)
    assert view.lines()[0].collapsed and len(view.lines()) == 1
    assert not view.model.is_open(1)
    view.toggle_episode(1)
    assert not view.lines()[0].collapsed and len(view.lines()) > 10
    # the operations in series
    view.series_button.setChecked(True)
    assert view.model.show_series
    series = [l.text for l in view.lines() if l.kind == "series"]
    assert series[0].split() == ["gnoth1", "gnoth2", "gnoth3"]                 # [M86 p. 120]
    assert series[1].strip() == "initial state"
    assert "[] (disappears)" in series[4]
    view.set_show_series(False)
    assert not view.series_button.isChecked()
    assert all(l.kind != "series" for l in view.lines())
    assert window.seek(len(window.history) - 1) and view.head_label.text() == "2 episodes"


def test_hovering_a_card_selects_what_it_names(window, qtbot):
    view = window.reformulation_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    selection = window.selection
    window.socratoplasm_view.select_gnoth("gnoth3")
    assert selection.name == "gnoth3"
    row = row_of(view, lambda l: l.kind == "head" and l.text == "gnoths set")
    names = view.lines()[row].names
    assert "gnoth5" in names and "glom10" in names
    lst = view.list
    lst.verticalScrollBar().setValue(max(0, row - 2))
    y = (row - lst.first_row()) * lst.row_height + lst.row_height // 2
    with qtbot.waitSignal(view.names_hovered, timeout=TIMEOUT):
        qtbot.mouseMove(lst.viewport(), pos=QPoint(30, y))
    assert selection.kind == "operation" and selection.includes("gnoth5")
    assert selection.includes("glom10")
    assert window.socratoplasm_view.model.is_selected("gnoth5")
    with qtbot.waitSignal(view.names_hovered, timeout=TIMEOUT):
        lst.leaveEvent(None)
    assert selection.name == "gnoth3" and not selection.includes("gnoth5")
    window.socratoplasm_view.select_gnoth("gnoth3")
    assert selection.empty


def test_a_new_run_clears_and_a_render(window, qtbot, tmp_path):
    view = window.reformulation_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    assert view.head_label.text() == "2 episodes"
    image = view.render_png(str(tmp_path / "p7.png"))
    assert image.width() > 0 and (tmp_path / "p7.png").stat().st_size > 0
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert view.head_label.text() == "no refutation yet"
    assert view.freeze_dried_text() == "freeze-dried: none" and view.plot.points == ()
    assert view.model.first_refutation is None and view.episodes() == []
    assert on_screen(view)                       # stays raised: nothing lowered it
