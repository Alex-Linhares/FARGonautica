"""Loop0004 item 05: the cytoplasm pane (`seekwhence/gui/cytoplasm_view.py`) in the
window, offscreen, pytest-qt (G-021, G-022, G-023; Numbo GUI, `test_tree_view.py`).

- The central splitter holds the canvas under the sequence pane; the legend names the
  three kinds of glue [FCCA pp. 58-60].
- Run 1 draws four glints and `bond1`, with `show`'s line as its tooltip [M86 p. 167];
  seeking shows the past (two glints before term 3, three after).
- Run 3 labels the bonds `+10`, `-5` and `0` under reformulation [M86 p. 102].
- A click on a term cell highlights its glint; a new run clears the scene.
- Zoom, fit and 1:1 (Numbo GUI, `tree_view.py`); one offscreen render of each run's
  end (`render.py`).
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtGui import QImage                                # noqa: E402

from seekwhence.gui.models import cytoplasm as cy               # noqa: E402
from seekwhence.gui.models import presets                       # noqa: E402

from golden import APPENDIX                                     # noqa: E402

TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def indexes(window, kind, name=None):
    return [i for i, e in enumerate(window.history.events)
            if e.kind == kind and (name is None or e.name == name)]


def test_the_central_splitter_holds_the_canvas(window):
    canvas = window.cytoplasm_view
    assert window.splitter.widget(1) is canvas and canvas.pane_id == "P2"
    assert canvas.view.history is window.history
    assert "[M86 pp. 44, 61-70, 73, 77, 92, 102, 106]" in canvas.pages and "[FCCA pp. 58-60]" in canvas.pages
    for word in ("Same", "Succ", "Pred", "spark"):
        assert word in canvas.legend.text()
    assert window._scenes == (canvas.scene(),)
    assert canvas.view.glint_items == {} and canvas.view.arc_items == {}
    assert canvas.view.auto_fit


def test_run_1_draws_four_glints_and_bond1(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view = window.cytoplasm_view.view
    assert sorted(view.glint_items) == ["glint1", "glint2", "glint3", "glint4"]
    assert [view.glint_items[f"glint{n}"].box.value for n in range(1, 5)] == [3, 7, 3, 7]
    assert "bond1" in view.arc_items
    bond = view.arc_items["bond1"]
    assert bond.toolTip() == "bond1 Same print-value (remote) -- (glint1 glint3)"   # [M86 p. 167]
    assert bond.style.color == cy.BOND_COLORS["Same"] and not bond.style.dashed
    assert (bond.shape_.x1, bond.shape_.x2) == (cy.SPACING, 3 * cy.SPACING)
    glint = view.glint_items["glint3"]
    assert glint.toolTip().startswith("name: glint3\nprint-value: 3\nposition: 3")   # [M86 p. 61]
    assert window.cytoplasm_view.events == 174 and view.paints >= 174
    assert view.sceneRect().contains(view.scene().itemsBoundingRect())


def test_seeking_shows_the_past(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view = window.cytoplasm_view.view
    prompts = indexes(window, "prompt")
    assert window.seek(prompts[1])
    assert sorted(view.glint_items) == ["glint1", "glint2"] and "bond1" not in view.arc_items
    assert window.seek(prompts[2])
    assert sorted(view.glint_items) == ["glint1", "glint2", "glint3"]
    assert "bond1" in view.arc_items                                   # [M86 p. 164]
    at = indexes(window, "bond", "bond1")[0]
    assert window.seek(at) and view.arc_items["bond1"].style.flash
    assert window.seek(len(window.history) - 1) and len(view.glint_items) == 4


def test_run_3_labels_the_bonds_with_their_values(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    view = window.cytoplasm_view.view
    log = window.history
    index = next(i for i in sorted(log.snapshots)
                 if any(g.external_bonds for g in log.snapshots[i].gnoths))
    assert window.seek(index)
    labels = {name: item.shape_.label for name, item in view.arc_items.items()
              if name.startswith("bond")}
    assert labels == {"bond1": "+10", "bond2": "-5", "bond3": "0"}      # [M86 p. 102]
    assert view.arc_items["bond1"].toolTip().endswith("value in S-group: +10")
    assert view.arc_items["bond1"].style.color == cy.BOND_COLORS["Succ"]
    assert view.arc_items["bond2"].style.color == cy.BOND_COLORS["Same"]
    assert view.arc_items["bond3"].style.color == cy.BOND_COLORS["Pred"]


def test_a_cell_click_highlights_the_glint(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    canvas = window.cytoplasm_view
    view = canvas.view
    assert canvas.highlight_term(3) == "glint3"
    assert view.glint_items["glint3"].style.border == cy.HIGHLIGHT_BORDER
    assert view.glint_items["glint1"].style.border == cy.GLINT_BORDER
    window.sequence_view.strip.cell_clicked.emit(1)
    assert canvas.model.highlighted == "glint1"
    assert view.glint_items["glint1"].style.border == cy.HIGHLIGHT_BORDER
    assert view.glint_items["glint3"].style.border == cy.GLINT_BORDER
    assert canvas.highlight_term(9) is None and canvas.model.highlighted is None


def test_a_new_run_clears_the_scene(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view = window.cytoplasm_view.view
    assert view.glint_items
    window.start_session(presets.appendix_inputs(RUN2), paused=True)
    window.step()
    qtbot.waitUntil(lambda: window.position == 0 and window.state == "paused", timeout=TIMEOUT)
    assert view.glint_items == {} and view.arc_items == {} and view.events == 1
    window.stop()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)


def test_zoom_fit_and_one_to_one(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    canvas = window.cytoplasm_view
    view = canvas.view
    assert view.auto_fit
    fitted = view.scale_factor()
    canvas.zoom_in_button.click()
    assert not view.auto_fit and view.scale_factor() == pytest.approx(fitted * 1.25)
    canvas.zoom_out_button.click()
    assert view.scale_factor() == pytest.approx(fitted)
    canvas.reset_zoom_button.click()
    assert view.scale_factor() == pytest.approx(1.0) and not view.auto_fit
    canvas.fit_button.click()
    assert view.auto_fit and view.scale_factor() == pytest.approx(fitted)


@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_a_render_of_each_runs_end(window, qtbot, tmp_path, run_id):
    run_to_end(qtbot, window, presets.appendix_inputs(run_id))
    view = window.cytoplasm_view.view
    path = tmp_path / f"{run_id}.png"
    window.cytoplasm_view.render_png(str(path))
    image = QImage(str(path))
    assert not image.isNull()
    rect = view.sceneRect()
    assert image.width() >= rect.width() and image.height() >= rect.height()
    assert len(view.glint_items) == len(window.history.state_at(len(window.history) - 1).terms)
