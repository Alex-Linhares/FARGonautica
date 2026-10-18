"""The Coderack bars and the rack list panes (item 09; `coderack_view.py`, `rack_view.py`;
G-036 to G-038), offscreen, one shared window with the two docks split so that both are on
screen, and one run of `surround-05` seed 3: the run through the window equals the plain
run and both panes' backing stores equal fresh renders at sampled events; the drawn strip
is the model's, with the bars' and the chosen outline's pixels, and the counter over the
strip's right end; a fresh strip fed the history renders byte-identically at sampled
indexes; the list's rows equal the snapshot's codelets once re-based, and a fresh list fed
the history draws the window's picture; seeking shows the chosen band tinted at the choice
only and a bare list greys a removed codelet for one event; the legend is one action on
the bar and in the View menu; the hover texts; the option is remembered in the settings;
the panes keep their small minimums."""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QEvent, QPoint, QPointF, QSettings, Qt  # noqa: E402
from PySide6.QtGui import QColor, QMouseEvent  # noqa: E402
from PySide6.QtWidgets import QApplication  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop import events as ev  # noqa: E402
from tabletop.gui import coderack_view as cv  # noqa: E402
from tabletop.gui import rack_view as rv  # noqa: E402
from tabletop.gui.main_window import MainWindow  # noqa: E402
from tabletop.gui.models import coderack as cr  # noqa: E402
from test_table_view import EVERY, fresh, run_to_end, same_picture, shown  # noqa: E402

QUICK = ("surround-05", 3)       # 59 codelets since v5's fix, D-116 to D-120 (44 since item 08, 77 until then; the facts below are re-pinned)
PROBES = {}


class Probe:
    """At every `EVERY`th `event_drawn`: what the window shows of a widget equals a fresh
    render of it (Numbo GUI, `test_polish.py`)."""

    def __init__(self, window, widget, top=None):
        self.window = window
        self.widget = widget
        self.top = window if top is None else top     # the widget's own window
        self.mismatches = []
        self.checked = []
        window.event_drawn.connect(self.drawn)

    def drawn(self, index):
        if index % EVERY == 0:
            self.checked.append(index)
            if not same_picture(shown(self.top, self.widget), fresh(self.widget)):
                self.mismatches.append(index)


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """The quick run through the shared window with both panes on screen (the rack
    dock split under the bars, not tabbed) and probed."""
    w = module_window
    w.resize(1600, 1000)
    # The rack dock floats (its own window), so that both panes are on screen at once:
    # un-tabbing it inside the window leaves a stale tab bar painted over it (G-012).
    w.rack_dock.setFloating(True)
    w.rack_dock.resize(900, 260)
    w.rack_dock.move(50, 50)
    qapp.processEvents()
    qtbot_wait(qapp)
    assert w.rack_dock.isFloating() and w.rack_view.list.viewport().window() is w.rack_dock
    assert not w.rack_view.list.viewport().visibleRegion().isEmpty()
    assert not w.coderack_view.strip.visibleRegion().isEmpty()
    PROBES["strip"] = Probe(w, w.coderack_view.strip)
    PROBES["list"] = Probe(w, w.rack_view.list.viewport(), top=w.rack_dock)
    outcome = run_to_end(w, qapp, *QUICK)
    for probe in PROBES.values():
        w.event_drawn.disconnect(probe.drawn)
    return outcome


def qtbot_wait(qapp, ms=30):
    """Let Qt lay the floating dock out (the docks lay out on the next pass; G-012)."""
    import time
    end = time.monotonic() + ms / 1000
    while time.monotonic() < end:
        qapp.processEvents()


def pixel(widget, x, y):
    return QColor(widget.grab().toImage().pixel(int(x), int(y)))


def left_edge_colours(widget, x, y):
    """The colours from (x - 3, y) to (x + 3, y): a 2 px outline at a float coordinate
    lands somewhere in them."""
    image = widget.grab().toImage()
    return {QColor(image.pixel(int(x) + dx, int(y))).name() for dx in range(-3, 4)}


def darkest(widget, x, y, width, height):
    """The darkest pixel's grey level in a rectangle of the widget: the text's colour
    where the text is drawn with greyscale antialiasing (G-038)."""
    image = widget.grab().toImage()
    return min(QColor(image.pixel(int(x) + dx, int(y) + dy)).lightness()
               for dx in range(int(width)) for dy in range(int(height)))


def test_the_run_through_the_window_equals_the_plain_run_and_the_pixels_are_the_events(module_window, ran):
    ref = plain_run(*QUICK, V2)
    assert list(module_window.history.records) == list(ref.records)
    assert ran.result.draws == ref.draws
    for name, probe in PROBES.items():
        assert probe.checked == list(range(0, len(module_window.history), EVERY)), name
        assert probe.mismatches == [], name
    assert module_window.coderack_view.events == module_window.rack_view.events == len(ref.events)


# -- P4 the strip (G-037) -----------------------------------------------------------------------

def test_the_drawn_strip_is_the_models_and_the_counter_is_over_its_right_end(module_window, ran):
    w = module_window
    pane = w.coderack_view
    assert pane.pane_id == "P4" and '[F95 p. 97]' in pane.pages and pane.pages in pane.toolTip()
    assert cr.NOT_FRENCHS not in pane.pages and "(D-051)" in pane.pages
    pane.redraw(now=True)
    lay = pane.strip.drawn
    assert lay == pane.layout_now and len(lay.bars) == 29
    assert (lay.width, lay.height) == (pane.strip.width(), pane.strip.height())
    assert pane.counter.text() == lay.counter == "Number of codelets run: 59"
    assert pane.counter.alignment() & Qt.AlignmentFlag.AlignRight
    assert pane.counter.geometry().right() > pane.strip.geometry().right() - 20
    assert pane.counter.geometry().bottom() <= pane.strip.geometry().top()
    # At the end of the run FND-ANS, the codelet just chosen (codelet 59 since v5's fix,
    # D-116 to D-120), has the most waiting (33 of 118) and FND-GP the most of the others
    # (16; since item 08, codelet 44, 24 of 75 and PK-SALO with 15; until item 08, 24 of 62
    # and CHK-RL with 5; before item 07, 13 of 89 and PK-SALO with 11): the tallest bar not
    # chosen is painted without an outline, the chosen one is outlined.
    tallest = max((b for b in lay.bars if not b.chosen), key=lambda b: b.height)
    assert tallest.type == "find-group" and tallest.count == 16
    assert 10 < tallest.height < 11               # 13.6% of the box's inner height, 81 px here
    assert pixel(pane.strip, tallest.x + tallest.width / 2, tallest.y + tallest.height / 2) == cv.BAR
    assert cv.CHOSEN.name() not in left_edge_colours(pane.strip, tallest.x - 2,
                                                     tallest.y + tallest.height / 2)
    chosen = lay.bar("find-answer")
    assert chosen.chosen and chosen.count == 33 and chosen.height > 0
    assert pixel(pane.strip, chosen.x + chosen.width / 2, chosen.y + chosen.height / 2) == cv.BAR
    assert cv.CHOSEN.name() in left_edge_colours(pane.strip, chosen.x - 2,
                                                 chosen.y + chosen.height / 2)
    empty = next(b for b in lay.bars if b.count == 0)
    assert pixel(pane.strip, empty.slot_x + empty.slot_width / 2, lay.box[1] + lay.box[3] / 2) \
        == cv.BACKGROUND
    assert cv.CHOSEN.name() not in left_edge_colours(pane.strip, empty.x - 2,
                                                     lay.box[1] + lay.box[3] - 3)
    assert '[F95 p. 97]' in pane.label.text()


def test_a_fresh_strip_fed_the_history_draws_the_windows_picture(module_window, ran, tmp_path):
    w = module_window
    live = w.coderack_view
    events = w.history.events
    chosen = [i for i, e in enumerate(events) if isinstance(e, ev.CodeletChosen)]
    for index in (0, chosen[5], chosen[20] + 3, len(events) - 1):
        assert w.seek(index)
        other = cv.CoderackView(pages=live.pages)
        other.resize(live.size())
        other.strip.resize(live.strip.size())
        for event in events[:index + 1]:
            other.on_event(event)
        other.redraw()
        assert other.model.shares() == live.model.shares()
        assert other.model.chosen_type == live.model.chosen_type
        assert other.layout_now == live.layout_now
        assert other.counter.text() == live.counter.text()
        a = live.render_png(tmp_path / f"live-{index}.png")
        b = other.render_png(tmp_path / f"fresh-{index}.png")
        assert a == b, index
    assert w.seek(len(events) - 1)


def test_the_legend_is_one_action_on_the_bar_and_in_the_menu(module_window, ran, qapp):
    w = module_window
    pane = w.coderack_view
    assert [a.text() for a in w.coderack_menu.actions()] == ["Legend"]
    assert list(w.coderack_menu.actions()) == [pane.legend_action]
    assert pane.buttons["legend"].defaultAction() is pane.legend_action
    assert pane.buttons["legend"].minimumWidth() >= 24
    assert "(D-051)" in pane.legend_action.toolTip()
    assert pane.coderack_state() == {"legend": False} and not pane.legend.isVisible()
    pane.buttons["legend"].click()
    qapp.processEvents()
    assert pane.coderack_state() == {"legend": True} and pane.legend.isVisible()
    assert pane.legend.toPlainText() == cr.legend_text()
    assert pane.legend.toPlainText().count("\n") == 29 and pane.legend.isReadOnly()
    w.coderack_menu.actions()[0].trigger()
    qapp.processEvents()
    assert not pane.legend.isVisible() and pane.coderack_state() == {"legend": False}


def test_the_hover_text_of_a_bar_is_the_types(module_window, ran, qtbot, qapp):
    w = module_window
    pane = w.coderack_view
    pane.redraw(now=True)
    lay = pane.strip.drawn
    bar = lay.bar("find-group")
    qtbot.mouseMove(pane.strip, QPoint(int(bar.slot_x + bar.slot_width / 2), int(lay.box[1] + 4)))
    qapp.processEvents()
    text = pane.strip.toolTip()
    assert text == pane.model.tooltip("find-group") and text.startswith("FND-GP: find-group (scout)")
    assert "at codelet 59" in text               # since v5's fix, D-116 to D-120 (44 since item 08)
    qtbot.mouseMove(pane.strip, QPoint(1, 1))
    qapp.processEvents()
    assert pane.strip.toolTip() == "hover a bar for its codelet type"


# -- P4b the list (G-038) -----------------------------------------------------------------------

def test_the_list_equals_the_snapshots_codelets_once_re_based(module_window, ran):
    w = module_window
    pane = w.rack_view
    assert pane.pane_id == "P4b" and cr.NOT_FRENCHS in pane.pages and pane.pages in pane.toolTip()
    assert pane.history is w.history
    for index in list(w.history.snapshots.indexes)[::7]:
        assert w.seek(index)
        snap = w.history.state_at(index)
        assert pane.model.rebased_at == index
        assert pane.model.snapshot_codelets() == snap.coderack["codelets"]
        rows = pane.list.rows
        assert rows == pane.model.rows() and len(rows) == len(snap.coderack["codelets"])
        assert [r.waiting.type for r in rows] == [d["type"] for d in snap.coderack["codelets"]]
    last = len(w.history.events) - 1
    assert w.seek(last)
    assert pane.list.summary().startswith(f"{len(pane.model.rack)} waiting (at most ")
    assert pane.list.band == cr.band_lines(pane.model)
    assert pane.list.band[0].startswith("codelet 59: FND-ANS()  chance ")    # since v5's fix, D-116 to D-120 (44 since item 08)
    assert pane.list.verticalScrollBar().maximum() >= 0


def test_a_fresh_list_fed_the_history_draws_the_windows_picture(module_window, ran, qapp, tmp_path):
    w = module_window
    live = w.rack_view
    events = w.history.events
    chosen = [i for i, e in enumerate(events) if isinstance(e, ev.CodeletChosen)]
    for index in (chosen[3], chosen[30] + 2, len(events) - 1):
        assert w.seek(index)
        other = rv.RackView(pages=live.pages)
        other.attach(w.history, None)
        other.resize(live.size())
        other.list.resize(live.list.size())
        for i, event in enumerate(events[:index + 1]):
            other.set_position(i)
            other.on_event(event)
        other.redraw()
        assert other.model.snapshot_codelets() == live.model.snapshot_codelets()
        assert other.list.rows == live.list.rows and other.list.band == live.list.band
        # Shown (offscreen) so that its scroll bar is laid out as the window's is: the
        # rack has more rows than the list's height holds at these indexes.
        other.show()
        qapp.processEvents()
        assert other.list.viewport().size() == live.list.viewport().size(), index
        a = live.render_png(tmp_path / f"live-{index}.png")
        b = other.render_png(tmp_path / f"fresh-{index}.png")
        other.close()
        assert a == b, index
    assert w.seek(len(events) - 1)


def test_seeking_shows_the_chosen_band_tinted_at_the_choice_only(module_window, ran):
    w = module_window
    pane = w.rack_view
    events = w.history.events
    chosen = [i for i, e in enumerate(events) if isinstance(e, ev.CodeletChosen)]
    assert w.seek(chosen[2])
    assert pane.model.chosen_fresh and pane.list.band[0].startswith("codelet 3: ")
    assert pixel(pane.list.viewport(), 3, rv.PAD + 2) == rv.CHOSEN_TINT
    assert w.seek(chosen[2] + 1)
    assert not pane.model.chosen_fresh and pane.list.band[0].startswith("codelet 3: ")
    assert pixel(pane.list.viewport(), 3, rv.PAD + 2) == rv.BACKGROUND
    assert w.seek(0)
    assert pane.list.band[0] == "no codelet chosen yet" and pane.list.rows == ()
    assert w.seek(len(events) - 1)


def test_a_bare_list_greys_a_removed_codelet_for_one_event(golden_runs, qtbot):
    # surround-05 seed 2 is the golden run whose first removal is followed at once by
    # another (50 since item 09, for age, the first eleven at consecutive events, from
    # codelet 61; 53 and eleven at item 08, 16 and seven until item 08, 58 and eleven
    # before item 07) and whose removed row is the top of the list; blockage-06 seed 2 has
    # 23, surround-05 seed 1 none and blockage-06 seed 1 five since v5's fix, D-116 to
    # D-120 (21, seven and five since item 09; 45, 37 and five at item 08; 33, none and
    # none until then).
    events = plain_run("surround-05", 2, V2).events      # the v2 defaults (D-114)
    first = next(i for i, e in enumerate(events) if isinstance(e, ev.CodeletRemoved))
    pane = rv.RackView(pages="x")
    qtbot.addWidget(pane)
    pane.resize(600, 300)
    pane.show()
    for event in events[:first + 1]:
        pane.on_event(event)
    pane.redraw(now=True)
    grey = [r for r in pane.list.rows if r.removed]
    assert len(grey) == 1 and grey[0].reason == "age"
    row = grey[0]
    assert row.position < 6, "the removed row is on screen without scrolling"
    columns = pane.list.columns()
    x, width = columns[1][0], columns[1][1]

    def cell(position):
        y = rv.PAD + pane.list.line * (rv.HEADING_ROWS + position)
        return darkest(pane.list.viewport(), x, y, width, pane.list.line)

    grey_level = rv.REMOVED.lightness()
    assert abs(cell(row.position) - grey_level) <= 12
    other = next(r for r in pane.list.rows if not r.removed and r.position < 6)
    assert cell(other.position) <= rv.TEXT.lightness() + 12 < grey_level - 40
    while isinstance(events[first + 1], ev.CodeletRemoved):
        first += 1
        pane.on_event(events[first])
    pane.on_event(events[first + 1])
    pane.redraw(now=True)
    assert not any(r.removed for r in pane.list.rows)
    assert all(abs(cell(k) - grey_level) > 40 for k in range(min(6, len(pane.list.rows))))


def hover(widget, x, y):
    """A mouse move over the widget, sent straight to it (a floating window's viewport
    gets no cursor moves from the offscreen platform)."""
    local = QPointF(x, y)
    QApplication.sendEvent(widget, QMouseEvent(QEvent.Type.MouseMove, local, local,
                                               QPointF(widget.mapToGlobal(local.toPoint())),
                                               Qt.MouseButton.NoButton, Qt.MouseButton.NoButton,
                                               Qt.KeyboardModifier.NoModifier))


def test_the_hover_text_of_a_row_is_the_codelets(module_window, ran, qapp):
    w = module_window
    pane = w.rack_view
    assert w.seek(len(w.history.events) - 1)
    pane.list.verticalScrollBar().setValue(0)
    y = rv.PAD + pane.list.line * (rv.HEADING_ROWS + 0.5)
    hover(pane.list.viewport(), 20, y)
    qapp.processEvents()
    row = pane.list.rows[0]
    text = pane.list.viewport().toolTip()
    assert text == cr.row_tooltip(row) == pane.list.toolTip()
    assert text.startswith(cr.codelet_text(row.waiting.type, row.waiting.args))
    assert pane.list.row_at(y) is row and pane.list.row_at(2) is None
    hover(pane.list.viewport(), 20, 2)
    qapp.processEvents()
    assert pane.list.toolTip() == "hover a row for its codelet"


# -- the settings (G-037) and the minimums (G-014) --------------------------------------------------

def test_the_option_is_remembered_in_the_settings(qtbot, ini, thread_errors):
    def settings():
        return QSettings(ini, QSettings.Format.IniFormat)

    w = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w)
    w.coderack_view.set_coderack_state({"legend": True})
    w.close()
    assert not w.controller.is_alive()
    assert settings().value("controls/coderack-legend") in (True, "true")
    w2 = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w2)
    assert w2.coderack_view.coderack_state() == {"legend": True}
    w2.close()
    assert not w2.controller.is_alive()
    s = settings()
    s.setValue("controls/coderack-legend", "maybe")
    s.sync()
    w3 = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w3)
    assert w3.coderack_view.coderack_state() == {"legend": False}
    w3.close()
    assert not w3.controller.is_alive()


def test_the_panes_are_small_enough(module_window):
    bars, rack = module_window.coderack_view, module_window.rack_view
    assert (bars.minimumWidth(), bars.minimumHeight()) == cv.MIN_SIZE == (200, 70)
    assert (rack.minimumWidth(), rack.minimumHeight()) == rv.MIN_SIZE == (200, 70)
    assert bars.label.minimumWidth() <= 1 and rack.label.minimumWidth() <= 1
    assert bars.legend.minimumWidth() <= 40 and bars.counter.minimumWidth() <= 1
