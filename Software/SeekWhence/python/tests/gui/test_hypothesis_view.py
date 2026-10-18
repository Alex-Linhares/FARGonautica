"""Loop0004 item 09: the hypothesis pane (`seekwhence/gui/hypothesis_view.py`) in the
window, offscreen, pytest-qt (G-033, G-034, G-035; Numbo GUI, `test_pnet_coderack_views.py`).

- The dock holds the view; the box tree is hidden by default.
- At the end of run 1 the form is `(Cycle 3 2 (3 7))` from the template, validity 1, the
  predictions `3 7 3 7` over the terms, the oval tree's nodes, and the box tree's
  records with the run's own names once the button is on [M86 pp. 83, 166].
- Seeking to run 3's second refutation shows validity 0 and the refuting term with the
  term predicted [M86 p. 176]; seeking back shows the candidate [M86 p. 174].
- A click on an oval highlights the positions it gives; a second click clears.
- The private Hit button hits a box of the form in force, then of a form typed, and
  Reset starts over; the run's recorded box is unchanged [M86 p. 161].
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


def indexes(window, kind):
    return [i for i, e in enumerate(window.history.events) if e.kind == kind]


def click_node(qtbot, tree, node_id):
    node = next(n for n in tree.nodes if n.id == node_id)
    qtbot.mouseClick(tree, Qt.MouseButton.LeftButton, pos=tree.rect_of(node).center().toPoint())


def test_the_dock_holds_the_view(window):
    view = window.hypothesis_view
    assert window.hypothesis_dock.widget() is view
    assert view.pane_id == "P4" and "[M86" in view.pages
    assert view.form() == "nil" and view.nodes() == () and view.cells() == ()
    assert not view.box_button.isChecked() and not view.model.show_box
    assert view.lines() == ()


def test_run_1_ends_with_the_form_the_predictions_and_the_box_tree(window, qtbot):
    view = window.hypothesis_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert view.form() == "(Cycle 3 2 (3 7))"                       # [M86 p. 166]
    assert view.source_label.text() == "from the template [M86 p. 79]"
    assert view.validity_label.text() == "validity 1  (guess at 1)"
    assert view.validity_bar.validity == 1 and view.validity_bar.threshold == 1
    assert [c.predicted for c in view.cells()] == [3, 7, 3, 7, 3, 7, 3]
    assert [c.state for c in view.cells()] == ["agrees"] * 4 + ["beyond"] * 3
    assert [n.label for n in view.nodes()] == ["Cycle", "3", "7"]
    assert view.nodes()[0].hits == "3 7 3"
    assert view.lines()[0].text == "guess: 3 7 3 7  (answered ok: accepted) [M86 p. 166]"
    assert all(line.kind != "box" for line in view.lines())
    view.box_button.setChecked(True)
    assert view.model.show_box
    kinds = [line.kind for line in view.lines()]
    assert kinds.count("box") == 3 and kinds.count("pstruc") == 3
    box = next(line for line in view.lines() if line.kind == "box")
    snapshot = window.history.last_snapshot.hypothesis
    assert box.name == snapshot.boxes[0].name and box.name != "box1" and not box.grey
    assert view.model.structure_text() == snapshot.structure_text
    assert box.tooltip.split("\n")[0] == box.name
    view.set_show_box(False)
    assert not view.box_button.isChecked() and all(l.kind != "box" for l in view.lines())
    assert view.events == len(window.history)


def test_seeking_shows_the_refutation_and_the_candidate(window, qtbot):
    view = window.hypothesis_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    refuted = indexes(window, "refuted")[1]
    assert window.seek(refuted)
    assert view.form() == "(S-group 1 2)"
    assert view.validity_label.text() == "validity 0  (guess at 1)  refuted"   # [M86 p. 176]
    cell = view.cells()[4]
    assert (cell.position, cell.predicted, cell.term, cell.state) == (5, 1, 3, "refuting")
    candidate = indexes(window, "candidate")[0]
    assert window.seek(candidate)
    assert view.form() == "(S-group 1 2)"                            # [M86 p. 174]
    assert view.source_label.text() == "a candidate cast under S-group [M86 p. 125]"
    assert view.lines()[-1].text.endswith("(S-group 1 2) postdicts 1 2 1: the new hypothesis")
    assert window.seek(0)
    assert view.form() == "nil" and view.cells() == ()
    assert window.seek(len(window.history) - 1)
    assert view.form() == "(S-group 1 (Countup 2))"
    assert view.validity_label.text() == "validity 1  (guess at 1)"


def test_clicking_an_oval_highlights_the_terms_it_gives(window, qtbot):
    view = window.hypothesis_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    with qtbot.waitSignal(view.node_clicked, timeout=TIMEOUT) as blocker:
        click_node(qtbot, view.tree, (1,))                   # the element 7
    assert blocker.args == [(1,)]
    assert view.model.selected == (1,) and view.tree.selected == (1,)
    assert view.strip.highlighted == (2, 4, 6, 8, 10, 12)
    click_node(qtbot, view.tree, (1,))                       # a second click clears
    assert view.model.selected is None and view.strip.highlighted == ()
    click_node(qtbot, view.tree, ())                         # the top oval: every term
    assert view.strip.highlighted[:4] == (1, 2, 3, 4)
    qtbot.mouseClick(view.tree, Qt.MouseButton.LeftButton, pos=QPoint(2, 2))
    assert view.model.selected is None


def test_the_private_hit_button_never_touches_the_run(window, qtbot):
    view = window.hypothesis_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    snapshot = window.history.last_snapshot.hypothesis
    before = (snapshot.covered, snapshot.boxes, snapshot.pstrucs)
    view.hit_button.click()
    view.hit_button.click()
    private = [line.text for line in view.lines() if line.kind == "private"]
    assert private == ["show-box 1: (1 2)", "show-box 2: (1 2 3)"]         # [M86 p. 179]
    assert view.model.private.text == "(S-group 1 (Countup 2))"
    view.reset_button.click()
    assert [line.text for line in view.lines() if line.kind == "private"] == []
    view.private_edit.setText("(C-group (Countup 1) shared)")                # [M86 p. 161]
    assert [view.private_hit() for _ in range(4)] == ["(1)", "(2 2)", "(3 3 3)", "(4 4 4 4)"]
    view.private_edit.setText("(nonsense")
    assert view.private_hit() == "" and "not a form" in view.private_edit.toolTip()
    after = window.history.last_snapshot.hypothesis
    assert (after.covered, after.boxes, after.pstrucs) == before
    assert view.form() == "(S-group 1 (Countup 2))"


def test_a_new_run_clears_and_the_pane_renders(window, qtbot, tmp_path):
    view = window.hypothesis_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    view.hit_button.click()
    assert view.nodes()
    path = str(tmp_path / "hypothesis.png")
    image = view.render_png(path)
    assert image.width() > 0 and (tmp_path / "hypothesis.png").exists()
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert view.form() == "(Cycle 3 2 (3 7))" and view.model.private is None
    assert [line.kind for line in view.lines()] == ["note"]
    assert view.events == len(window.history)
