"""Loop0004 item 10 (finished in iteration 15, ANSWERS.md 9): the platoplasm pane
(`seekwhence/gui/platoplasm_view.py`) in the window, offscreen, pytest-qt (G-036, G-037,
G-038; Numbo GUI, `test_pnet_coderack_views.py`).

- The dock holds the view, labelled "GUI design, not Meredith's" in the pane, its tooltip
  and the dock's tooltip (ANSWERS.md 4); the eight nodes and the s-links are drawn from
  the first event.
- Run 3 at the end: the reigning class S-group with the threshold, the crown over its
  box, the atoms strip, the list's lines [M86 pp. 176, 179]; seeking to the slip-check
  shows `best: (Y-group 4.0)` with the link lit [M86 p. 176]; seeking to the
  `class-changed` event puts the crown over Countup and the next events move it
  [M86 p. 174].
- A click on a type's box selects its actualizations in the shared selection, and the
  socratoplasm shows them; a second click clears; the "Links" button draws the tags.
- The pane is painted exactly once per event; a new run clears; a render.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, Qt                                 # noqa: E402

from seekwhence.gui.models import platoplasm as pm                    # noqa: E402
from seekwhence.gui.models import presets                             # noqa: E402
from seekwhence.gui.platoplasm_view import NOT_MEREDITHS              # noqa: E402

from golden import APPENDIX                                           # noqa: E402

TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def index_of(window, kind, nth=0):
    return [i for i, e in enumerate(window.history.events) if e.kind == kind][nth]


def test_the_dock_holds_the_view_with_the_label(window):
    view = window.platoplasm_view
    assert window.platoplasm_dock.widget() is view
    assert view.pane_id == "P5" and "GUI design, not Meredith's" in view.pages
    assert view.label.text() == NOT_MEREDITHS == "GUI design, not Meredith's"
    assert NOT_MEREDITHS in view.label.toolTip() and "ANSWERS.md 4" in view.label.toolTip()
    assert NOT_MEREDITHS in window.platoplasm_dock.toolTip()
    assert [n.name for n in view.nodes()] == list(pm.PlatoplasmModel().types)
    assert view.links() == () and view.crown() is None
    assert view.reigning_label.text() == "no reigning class"


def test_run_3_at_the_end_and_seeking(window, qtbot):
    view = window.platoplasm_view
    paints = []
    window.event_drawn.connect(lambda _i: paints.append(view.paints))
    assert run_to_end(qtbot, window, presets.appendix_inputs(RUN3)).result["outcome"] == "ok"
    # painted at least once per event (Numbo GUI's bound), with at most a few extra
    assert len(paints) == 602
    assert all(b > a for a, b in zip(paints, paints[1:]))
    assert paints[-1] - paints[0] <= 601 + 5
    assert len(view.links()) == len(view.model.link_triples()) > 0
    assert view.reigning_label.text() == "reigning: S-group   threshold 10.0"
    crown = view.crown()
    assert crown.to == "S-group" and crown.frm is None
    assert [a.value for a in view.atoms()] == [1, 2, 3]
    texts = [line.text for line in view.lines()]
    assert texts[0] == "reigning class: S-group   threshold 10.0"
    assert "slip-check: stayval: 15.0" in texts
    # seek to the slip-check [M86 p. 176]
    i = index_of(window, "slip-check")
    assert window.seek(i)
    assert "        best: (Y-group 4.0)" in [line.text for line in view.lines()]
    lit = {l.to: l for l in view.links() if l.lit}
    assert lit["Y-group"].best and lit["Y-group"].pressure == 4.0
    assert next(n for n in view.nodes() if n.name == "Y-group").best
    # seek to the class change [M86 p. 174]: the crown starts over Countup and travels
    i = index_of(window, "class-changed")
    assert window.seek(i)
    crown = view.crown()
    assert (crown.frm, crown.to, crown.progress) == ("Countup", "S-group", 0.0)
    countup = view.node_rect("Countup")
    assert countup.left() <= view.network.point(crown.x, crown.y).x() <= countup.right()
    assert window.seek(i + pm.CROWN_MOVE)
    crown = view.crown()
    assert crown.frm is None and crown.progress == 1.0
    s_group = view.node_rect("S-group")
    assert s_group.left() <= view.network.point(crown.x, crown.y).x() <= s_group.right()
    assert "Countup --> S-group" in [line.text for line in view.lines()][1]


def test_a_click_selects_a_type_and_the_links_button(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    view = window.platoplasm_view
    rect = view.node_rect("S-group")
    qtbot.mouseClick(view.network, Qt.MouseButton.LeftButton, pos=rect.center().toPoint())
    assert view.model.selected == "S-group"
    assert window.selection.includes("gnoth2") and window.selection.includes("gnoth3")
    assert next(n for n in view.nodes() if n.name == "S-group").selected
    assert window.socratoplasm_view.model.is_selected("gnoth2")
    qtbot.mouseClick(view.network, Qt.MouseButton.LeftButton, pos=rect.center().toPoint())
    assert view.model.selected is None and not window.selection.includes("gnoth2")
    assert view.network.lay.tags == ()
    view.links_button.click()
    assert view.model.show_links and view.network.lay.tags != ()
    assert {t.name for t in view.network.lay.tags} == {"gnoth2", "gnoth3"}
    view.links_button.click()
    assert view.network.lay.tags == ()


def test_a_new_run_clears_and_a_render(window, qtbot, tmp_path):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    view = window.platoplasm_view
    assert view.crown() is not None
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        window.start_session(presets.appendix_inputs(RUN1), paused=True)
        window.controller.step()
    assert view.crown() is None and view.atoms() == ()
    assert view.reigning_label.text() == "no reigning class"
    window.stop()
    assert not window.controller.is_alive()
    path = tmp_path / "platoplasm.png"
    view.render_png(str(path))
    assert path.stat().st_size > 1000
