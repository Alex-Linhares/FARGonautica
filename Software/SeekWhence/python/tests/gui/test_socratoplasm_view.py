"""Loop0004 item 08: the socratoplasm pane (`seekwhence/gui/socratoplasm_view.py`) in the
window, offscreen, pytest-qt (G-030, G-031, G-032; Numbo GUI, `test_pnet_coderack_views.py`).

- The dock holds the view; at the end of run 3 the parse is `((1 2) (1 2 3))`
  [M86 p. 178], the cards' field lines equal `Session.show_lines()`'s blocks and the
  strip has a box per gnoth and the catchall.
- Seeking to the prompt of the first `show-parse` shows `((1 2) (1))` [M86 p. 174]; the
  cards then equal the `show` of the snapshot.
- A click on a gnoth's box selects it with its glom and members in the shared selection,
  the cytoplasm shows them, a second click clears, empty space clears; a click on a card
  selects too; a selection made in the cytoplasm outlines the card.
- Hovering a bond line highlights the arc in the cytoplasm; leaving clears it.
- "Copy as `show`" puts the `gnoths:` block on the clipboard.
- A new run clears; one offscreen render.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, QPointF, Qt                        # noqa: E402
from PySide6.QtGui import QMouseEvent                                 # noqa: E402
from PySide6.QtWidgets import QApplication                            # noqa: E402

from seekwhence.gui.models import commands, presets                   # noqa: E402
from seekwhence.gui.models import socratoplasm as so                  # noqa: E402

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


def click_box(qtbot, strip, name):
    box = next(b for b in strip.boxes if b.name == name)
    rect = strip.rect_of(box)
    qtbot.mouseClick(strip, Qt.MouseButton.LeftButton, pos=rect.center().toPoint())


def click_row(qtbot, lst, row):
    lst.verticalScrollBar().setValue(max(0, row - lst.rows_in_view() + 1))
    y = (row - lst.first_row()) * lst.row_height + lst.row_height // 2
    qtbot.mouseClick(lst.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(20, y))


def hover_row(lst, row):
    lst.verticalScrollBar().setValue(max(0, row - lst.rows_in_view() + 1))
    y = (row - lst.first_row()) * lst.row_height + lst.row_height // 2
    pos = QPointF(20, y)
    event = QMouseEvent(QMouseEvent.Type.MouseMove, pos, pos, Qt.MouseButton.NoButton,
                        Qt.MouseButton.NoButton, Qt.KeyboardModifier.NoModifier)
    QApplication.sendEvent(lst.viewport(), event)


def test_the_dock_holds_the_view(window):
    view = window.socratoplasm_view
    assert window.socratoplasm_dock.widget() is view
    assert view.pane_id == "P3" and "[M86" in view.pages
    assert view.parse() == "()" and view.boxes() == ()
    assert view.lines()[1].text == "no template"


def test_run_3_ends_with_the_parse_the_cards_and_the_boxes(window, qtbot, appendix_runs):
    view = window.socratoplasm_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    assert view.parse() == "((1 2) (1 2 3))"                      # [M86 p. 178]
    assert view.count_label.text() == "2 gnoths + catchall"
    lines = appendix_runs[RUN3].show
    block = lines[lines.index("gnoths:") + 2:]
    blocks = "\n".join(block).split("\n\n")
    assert [view.card_lines(n) for n in ("gnoth2", "gnoth3")] == [b.split("\n") for b in blocks]
    assert [b.name for b in view.boxes()] == ["gnoth2", "gnoth3", "gnoth6"]
    assert view.boxes()[2].kind == "catchall"
    assert view.card_lines("gnoth6")[1] == "name: gnoth6"
    assert view.lines()[1].text.startswith("template: (form (S-group 1 2) state accepted")
    assert view.events == len(window.history)


def test_seeking_shows_the_parse_and_the_cards_of_the_past(window, qtbot):
    view = window.socratoplasm_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    prompts = indexes(window, "prompt")
    first = next(i for i in prompts if view.model.parse != "((1 2) (1))"
                 and commands.show_parse(window.history.state_at(i)) == "((1 2) (1))")
    assert window.seek(first)
    assert view.parse() == "((1 2) (1))"                            # [M86 p. 174]
    shown = commands.show_lines(window.history.state_at(first))
    assert "\n".join(view.model.show_block()) == "\n".join(shown[shown.index("gnoths:"):])
    assert [b.name for b in view.boxes()] == ["gnoth2", "gnoth3"]
    assert window.seek(0)
    assert view.parse() == "()" and view.boxes() == ()
    assert window.seek(len(window.history) - 1)
    assert view.parse() == "((1 2) (1 2 3))"


def test_clicking_a_gnoth_selects_its_glom_and_members(window, qtbot):
    view = window.socratoplasm_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    canvas = window.cytoplasm_view
    canvas.view.layer_actions["top-down"].setChecked(True)
    with qtbot.waitSignal(view.selected, timeout=TIMEOUT) as blocker:
        click_box(qtbot, view.strip, "gnoth1")
    assert blocker.args == ["gnoth1"]
    selection = window.selection
    assert selection.kind == "gnoth" and selection.name == "gnoth1"
    assert all(selection.includes(n) for n in ("glom3", "glom1", "glint1", "glint2"))
    assert canvas.view.model.is_selected("glom3") and canvas.view.model.is_selected("glint1")
    assert next(b for b in view.boxes() if b.name == "gnoth1").selected
    head = next(line for line in view.lines() if line.kind == "head" and line.name == "gnoth1")
    assert head.selected
    click_box(qtbot, view.strip, "gnoth1")                   # a second click clears
    assert selection.empty
    assert not next(b for b in view.boxes() if b.name == "gnoth1").selected
    # a click on a card selects; a click on empty space clears
    row = next(i for i, line in enumerate(view.lines()) if line.name == "gnoth2")
    click_row(qtbot, view.list, row)
    assert selection.name == "gnoth2" and selection.includes("glint3")
    qtbot.mouseClick(view.strip, Qt.MouseButton.LeftButton, pos=QPoint(2, 2))
    assert selection.empty
    # a selection made in the cytoplasm outlines the gnoth whose glom it is
    canvas.view.model.select("glom3")
    assert next(b for b in view.boxes() if b.name == "gnoth1").selected


def test_hovering_a_bond_line_highlights_the_arc(window, qtbot):
    view = window.socratoplasm_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    model = window.cytoplasm_view.view.model
    row = next(i for i, line in enumerate(view.lines()) if line.bond == "bond8")
    with qtbot.waitSignal(view.bond_hovered, timeout=TIMEOUT) as blocker:
        hover_row(view.list, row)
    assert blocker.args == ["bond8"] and model.highlighted == "bond8"
    assert view.list.hovered == "bond8"
    other = next(i for i, line in enumerate(view.lines()) if line.bond == "bond5")
    hover_row(view.list, other)
    assert model.highlighted == "bond5"
    head = next(i for i, line in enumerate(view.lines()) if line.kind == "head")
    hover_row(view.list, head)
    assert model.highlighted is None and view.list.hovered is None


def test_copy_as_show_puts_the_gnoths_block_on_the_clipboard(window, qtbot, appendix_runs):
    view = window.socratoplasm_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    text = view.copy_show()
    lines = appendix_runs[RUN1].show
    assert text == "\n".join(lines[lines.index("gnoths:"):])
    assert QApplication.clipboard().text() == text
    view.copy_button.click()
    assert QApplication.clipboard().text() == text


def test_a_new_run_clears_and_the_pane_renders(window, qtbot, tmp_path):
    view = window.socratoplasm_view
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    assert view.boxes()
    path = str(tmp_path / "socratoplasm.png")
    image = view.render_png(path)
    assert image.width() > 0 and (tmp_path / "socratoplasm.png").exists()
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert view.parse() == "(((3 7)))" and [b.name for b in view.boxes()] == ["gnoth1", "gnoth2"]
    assert view.events == len(window.history)
