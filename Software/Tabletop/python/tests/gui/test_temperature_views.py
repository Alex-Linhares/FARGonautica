"""The thermometer and the temperature plot (item 06; `thermometer_view.py`,
`temperature_view.py`; G-027 to G-029), offscreen, one shared window and one run of
`surround-05` seed 3: the run through the window equals the plain run and the
thermometer's backing store equals a fresh render at sampled events; the thermometer
draws the last reading (black after the jump, grey while held, empty before the first
reading) and a fresh view fed the history draws the window's picture; the plot's
options are the pane's own actions on its bar and in the View menu, the French option
is offered for `surround-05` only, the drawn layout is the model's, the hover text
names the reading under the mouse; the options are remembered in the settings; both
panes fit the smallest window."""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, QSettings  # noqa: E402
from PySide6.QtGui import QColor  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop.gui import temperature_view as pv  # noqa: E402
from tabletop.gui import thermometer_view as thv  # noqa: E402
from tabletop.gui.main_window import MainWindow  # noqa: E402
from tabletop.gui.models import temperature as tm  # noqa: E402
from test_table_view import EVERY, fresh, run_to_end, same_picture, shown  # noqa: E402

QUICK = ("surround-05", 3)
HELD, JUMP, BEFORE = 270, 289, 10      # a held index (after the scheduled reading at codelet 15), the first unheld update (the change at codelet 18), before any; since item 08's runs and still since v5's fix, D-116 to D-120 (500, 523 at item 07)
PROBES = {}


class ThermometerProbe:
    """At every `EVERY`th `event_drawn`: what the window shows of the thermometer equals
    a fresh render of it (Numbo GUI, `test_polish.py`)."""

    def __init__(self, window):
        self.window = window
        self.widget = window.thermometer_view
        self.mismatches = []
        self.checked = []
        window.event_drawn.connect(self.drawn)

    def drawn(self, index):
        if index % EVERY == 0:
            self.checked.append(index)
            if not same_picture(shown(self.window, self.widget), fresh(self.widget)):
                self.mismatches.append(index)


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """The quick run through the shared window with the thermometer probe attached."""
    w = module_window
    PROBES["thermo"] = ThermometerProbe(w)
    outcome = run_to_end(w, qapp, *QUICK)
    w.event_drawn.disconnect(PROBES["thermo"].drawn)
    return outcome


def pixel(view, x, y):
    return QColor(view.grab().toImage().pixel(x, y))


def column_point(view, share):
    """A point inside the column at a share of its height from the bottom."""
    column = thv.column_rect(view.width(), view.height())
    return (int(column.center().x()), int(column.bottom() - column.height() * share))


# -- the thermometer (G-027) ---------------------------------------------------------------------

def test_the_run_through_the_window_equals_the_plain_run_and_the_pixels_are_the_events(module_window, ran):
    ref = plain_run(*QUICK, V2)
    assert list(module_window.history.records) == list(ref.records)
    assert ran.result.draws == ref.draws
    probe = PROBES["thermo"]
    assert probe.checked == list(range(0, len(module_window.history), EVERY))
    assert probe.mismatches == []


def test_the_thermometer_draws_the_last_reading(module_window, ran):
    view = module_window.thermometer_view
    assert view.pane_id == "P2" and view.events == len(module_window.history)
    form = view.form
    # The last reading is the scheduled one at codelet 55, four codelets before the answer
    # at 59 (since v5's fix, D-116 to D-120; 41.0 at codelet 40 before the answer at 44 from
    # item 08's runs to v2; 34.6 at codelet 75 at item 07).
    assert (form.text, form.held, form.by, form.clock, form.age) == ("43.6", False, "schedule", 55, 4)
    assert form.value == ran.result.temperature
    # Black inside the fill, the background above it (the fill reaches 43.6 % of the
    # column, so the probe is midway between its top and the column's), the magenta tick
    # at the top.
    assert pixel(view, *column_point(view, 0.2)) == thv.FILL
    assert pixel(view, *column_point(view, 0.3)) == thv.FILL
    assert pixel(view, *column_point(view, (form.fill + 1) / 2)) == QColor("#fbfbf8")
    column = thv.column_rect(view.width(), view.height())
    top = int(column.bottom() - column.height() * form.fill)
    assert pixel(view, int(column.right()) + 4, top) == thv.TICK
    assert view.pages in view.toolTip() and "43.6 by schedule at codelet 55" in view.toolTip()
    assert "no update follows the answer" in view.toolTip()
    assert "the GUI's additions (D-049, D-050)" in view.toolTip()


def test_seeking_shows_the_held_band_and_the_empty_column(module_window, ran):
    w = module_window
    view = w.thermometer_view
    assert w.seek(HELD)
    assert view.form.held and view.form.text == "80.0" and view.form.by == "schedule"
    assert pixel(view, *column_point(view, 0.5)) == thv.HELD_FILL
    assert pixel(view, *column_point(view, 0.9)) == QColor("#fbfbf8")
    assert w.seek(JUMP)
    assert not view.form.held and view.form.text == "97.9" and view.form.by == "change"    # since v5's fix (96.8 from item 08 to v2)
    assert pixel(view, *column_point(view, 0.85)) == thv.FILL     # above the held 80, under the 97.9
    assert w.seek(BEFORE)
    assert view.form.value is None and view.form.note == "no reading yet"
    assert pixel(view, *column_point(view, 0.05)) == QColor("#fbfbf8")
    assert w.seek(len(w.history) - 1)
    assert view.form.text == "43.6" and view.form.clock == 55


def test_a_fresh_thermometer_fed_the_history_draws_the_windows_picture(module_window, ran, tmp_path):
    w = module_window
    live = w.thermometer_view
    events = w.history.events
    for index in (BEFORE, HELD, JUMP, len(events) - 1):
        assert w.seek(index)
        other = thv.ThermometerView(pages=live.pages)
        other.resize(live.size())
        for event in events[:index + 1]:
            other.on_event(event)
        assert other.form == live.form
        a = live.render_png(tmp_path / f"live-{index}.png")
        b = other.render_png(tmp_path / f"fresh-{index}.png")
        assert a == b, index


def test_the_thermometer_is_narrow_and_small_enough(module_window):
    view = module_window.thermometer_view
    assert (view.minimumWidth(), view.minimumHeight()) == thv.MIN_SIZE == (56, 60)
    assert view.sizeHint().width() <= 90
    assert '[F95 pp. 95-96]' in view.pages and '[F95 pp. 98-107 (img)]' in view.pages
    assert "the GUI's" in view.pages


# -- the temperature plot (G-028, G-029) -----------------------------------------------------------

def test_the_plots_options_are_one_state_on_the_bar_and_in_the_menu(module_window, ran, qapp):
    w = module_window
    pane = w.temperature_view
    assert pane.pane_id == "P2b" and pane.events == len(w.history)
    assert tm.NOT_FRENCHS in pane.pages and tm.NOT_FRENCHS in pane.label.text()
    assert '[F95 p. 91 (img)]' in pane.pages and pane.pages in pane.toolTip()
    assert [a.text() for a in w.plot_menu.actions()] == ["Terms", "French's ch. 4"]
    assert list(w.plot_menu.actions()) == [pane.terms_action, pane.french_action]
    for name, button in pane.buttons.items():
        assert button.defaultAction() is getattr(pane, f"{name}_action")
        assert tm.NOT_FRENCHS in button.toolTip() and button.minimumWidth() >= 24
    assert pane.plot_state() == {"terms": False, "french": False}
    assert pane.french_action.isEnabled()          # the run is surround-05 (G-029)
    w.temperature_dock.raise_()
    qapp.processEvents()
    before = pane.paints
    pane.buttons["terms"].click()
    assert pane.plot.terms and pane.plot_state()["terms"] and pane.paints == before + 1
    w.plot_menu.actions()[1].trigger()
    assert pane.plot.french and pane.plot_state() == {"terms": True, "french": True}
    lay = pane.plot.drawn
    assert len(lay.areas) == 3 and len(lay.french) == 7 and lay.x_max == 59     # the run's 59, past Fig. 4.9's 55 (since v5's fix, D-116 to D-120; his 55 past the run's 44 from item 08's runs to v2; the run's 77 at item 07)
    assert lay == tm.plot_layout(pane.model, pane.plot.width(), pane.plot.height(), True, True)
    pane.set_plot_state({"terms": False, "french": False})
    assert pane.plot.drawn.areas == () and pane.plot.drawn.french == () and pane.plot.drawn.x_max == 59
    w.worldview_dock.raise_()


def test_the_drawn_plot_is_the_models_layout_and_the_dots_are_dark(module_window, ran, qapp, tmp_path):
    w = module_window
    pane = w.temperature_view
    w.temperature_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    lay = pane.plot.drawn
    assert not lay.empty and lay.width == pane.plot.width() and lay.height == pane.plot.height()
    line = next(l for l in lay.band("temperature").lines if l.name == "temperature")
    assert len(line.points) == 13 and len(lay.marks) == 4       # since v5's fix (D-116 to D-120): 13 readings, the entry at 18 and the fight at 40 (10 and 2 from item 08's runs to v2; 16 readings at item 07)
    image = pane.plot.grab().toImage()
    x, y = line.points[-1]
    assert QColor(image.pixel(int(x), int(y))).lightness() < 100
    x, y = line.points[0]
    assert QColor(image.pixel(int(x), int(y))).lightness() < 100
    assert pane.render_png(tmp_path / "plot.png")[:8] == b"\x89PNG\r\n\x1a\n"
    w.worldview_dock.raise_()


def test_the_hover_text_names_the_reading_under_the_mouse(module_window, ran, qapp, qtbot):
    w = module_window
    pane = w.temperature_view
    w.temperature_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    lay = pane.plot.drawn
    qtbot.mouseMove(pane.plot, QPoint(int(lay.x_of(40)), int(lay.height / 2)))
    qapp.processEvents()
    assert pane.plot.toolTip().startswith("codelet 40: T 70.2 by change")     # since v5's fix (D-116 to D-120): the change at 40 (cp2 beats cp1); "T 41.0 by schedule" from item 08's runs to v2
    qtbot.mouseMove(pane.plot, QPoint(int(lay.x_of(0)), int(lay.height / 2)))
    qapp.processEvents()
    assert pane.plot.toolTip().startswith("codelet 0: T 80.0 by start (held, D-049)")
    w.worldview_dock.raise_()


def test_frenchs_option_is_offered_for_surround_05_only(golden_runs, qtbot):
    pane = pv.TemperatureView(pages="x")
    qtbot.addWidget(pane)
    pane.set_plot_state({"french": True})
    assert not pane.french_action.isEnabled() and pane.french_action.isChecked()
    for event in golden_runs[("blockage-06", 2)].events:
        pane.on_event(event)
    assert not pane.french_action.isEnabled() and pane.model.problem == "blockage-06"
    pane.resize(400, 300)
    lay = pane.layout_now
    assert lay.french == () and "apply to surround-05 only" in lay.note
    for event in golden_runs[("surround-05", 1)].events:
        pane.on_event(event)
    assert pane.french_action.isEnabled() and len(pane.layout_now.french) == 7
    assert pane.events == len(golden_runs[("surround-05", 1)].events) == 2690     # since v5's fix, D-116 to D-120 (3174 in v4, D-115; 1355 in v3, D-114; 901 since item 09, 1204 at item 08, 647 at item 07)


def test_the_plots_options_are_remembered_in_the_settings(qtbot, ini, thread_errors):
    def settings():
        return QSettings(ini, QSettings.Format.IniFormat)

    w = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w)
    w.temperature_view.set_plot_state({"terms": True, "french": True})
    w.close()
    assert not w.controller.is_alive()
    assert settings().value("controls/plot-terms") in (True, "true")
    w2 = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w2)
    assert w2.temperature_view.plot_state() == {"terms": True, "french": True}
    assert not w2.temperature_view.french_action.isEnabled()
    w2.close()
    assert not w2.controller.is_alive()
    s = settings()
    s.setValue("controls/plot-terms", "maybe")
    s.setValue("controls/plot-french", False)
    s.sync()
    w3 = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w3)
    assert w3.temperature_view.plot_state() == {"terms": False, "french": False}
    w3.close()
    assert not w3.controller.is_alive()


def test_the_plot_pane_is_small_enough(module_window):
    pane = module_window.temperature_view
    assert (pane.minimumWidth(), pane.minimumHeight()) == pv.MIN_SIZE == (120, 60)
    assert pane.label.minimumWidth() <= 1
