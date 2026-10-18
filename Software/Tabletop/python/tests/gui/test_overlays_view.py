"""The overlays on the table view (item 05; PANES.md "P1 overlays"; G-025, G-026),
offscreen: with every layer and Eliza's view on before the run, `surround-05` seed 3
through the window equals the plain run, the pixel probe finds no mismatch, and a fresh
view in the same state draws the window's picture at sampled events; the scene shows the
cells and the heat; the bar's toggles and the View menu's actions are one state and the
legend says "GUI design, not French's" only when something is on; the hover text lists
the nine terms (French's eight plus "role", since item 08; `tabletop.salience.FACTORS`);
the overlays are remembered in the settings; the canvas still fits the smallest window.

The run facts are v5's (the structural Blockage fix, D-116 to D-120): `surround-05` seed 3
has 59 codelets, 720 events, the answer spoon3 (none: an object French prints no bar for),
T 43.6, re-derived from the engine. Item 08's (roles loop0007) were 44 codelets, 507
events, cup2 (E2), T 41.0 (item 07's: 77 codelets, small-glass1 (E1)); item 09's runs left
seed 3 unchanged (checked against the engine: the same events and salience)."""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop.gui import table_view as tv  # noqa: E402
from tabletop.gui.main_window import MainWindow  # noqa: E402
from tabletop.gui.models import overlays as ov  # noqa: E402
from tabletop.problems import load_problem  # noqa: E402
from tabletop.salience import FACTORS  # noqa: E402
from test_table_view import EVERY, PixelProbe, run_to_end  # noqa: E402

QUICK = ("surround-05", 3)
ALL_ON = {"regions": True, "salience": True, "ranks": True, "roles": True, "eliza": True}
ALL_OFF = {name: False for name in ALL_ON}
PROBES = {}


def the_view(window):
    return window.table_view.view


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """Every layer and Eliza's view on through the View menu, then the quick run through
    the shared window with the pixel probe attached (ANSWERS.md 10)."""
    w = module_window
    for action in w.overlays_menu.actions():
        action.setChecked(True)
    w.eliza_view_action.setChecked(True)
    assert the_view(w).overlay_state() == ALL_ON
    PROBES["quick"] = PixelProbe(w)
    outcome = run_to_end(w, qapp, *QUICK)
    w.event_drawn.disconnect(PROBES["quick"].drawn)
    return outcome


def test_a_run_with_every_overlay_on_equals_the_plain_run(module_window, ran):
    ref = plain_run(*QUICK, V2)
    assert list(module_window.history.records) == list(ref.records)
    assert ran.result.draws == ref.draws
    assert module_window.statusBar().currentMessage() == ref.report == \
        "Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6."   # v5's run facts (item 08's: cup2 (E2), 75, 44, T 41.0)
    assert len(module_window.history) == len(ref.events) == 720          # 507 at item 08
    probe = PROBES["quick"]
    assert probe.checked == list(range(0, len(module_window.history), EVERY))
    assert probe.mismatches == []
    assert the_view(module_window).overlay_state() == ALL_ON


def test_the_scene_shows_the_cells_the_heat_and_elizas_view(module_window, ran):
    view = the_view(module_window)
    lay = view.layout
    assert lay.eliza and lay.overlays.names_shown and lay.overlays.ranks_shown
    assert view.regions_item.isVisible() and view.regions_item.form == lay.overlays
    assert view.salience_item.isVisible() and view.salience_item.form == lay.overlays
    assert view.roles_item.isVisible() and view.roles_item.form == lay.overlays
    assert lay.overlays.roles_shown and len(lay.overlays.badges) >= 1   # the first-sight tags (item 08)
    assert len(lay.overlays.regions) == 9 and len(lay.overlays.heat) > 5
    assert len(lay.overlays.heat) == 12          # since item 08's runs, and still on v5
    assert sorted(c.rank for c in lay.overlays.regions) == list(range(1, 10))
    assert lay.overlays.as_of == 59              # the run's last codelet (44 at item 08, 77 at item 07)
    # Eliza's name is written above the table, Henry's below (G-026).
    assert view.name_items["Eliza"].pos().y() < 0 < view.name_items["Henry"].pos().y()
    cup2 = lay.object("cup2")
    item = view.object_items["cup2"]
    assert (item.pos().x(), item.pos().y()) == (cup2.x * tv.SCALE, cup2.y * tv.SCALE)
    assert cup2.x == pytest.approx(110 - 87.6) and cup2.y == pytest.approx(100 - 77.3)
    assert view.regions_item.zValue() < view.salience_item.zValue() < view.group_items["g1"].zValue()


def test_a_fresh_view_in_the_same_state_draws_the_same_picture(module_window, ran, tmp_path):
    w = module_window
    live = the_view(w)
    events = w.history.events
    for index in (0, 200, len(events) - 1):
        assert w.seek(index)
        other = tv.TableView(w.history)
        other.set_problem(load_problem(QUICK[0]))
        other.set_overlay_state(ALL_ON)
        for i, event in enumerate(events[:index + 1]):
            other.set_position(i)
            other.on_event(event)
        other.redraw(now=False)
        assert other.layout == live.layout and other.layout.eliza
        a = live.render_png(tmp_path / f"live-{index}.png")
        b = other.render_png(tmp_path / f"fresh-{index}.png")
        assert a == b, index
    assert w.seek(len(events) - 1)


def test_the_toggles_on_the_bar_and_the_menu_are_one_state(module_window, ran):
    w = module_window
    view, canvas = the_view(w), w.table_view
    assert [a.text() for a in w.overlays_menu.actions()] == ["Regions", "Salience", "Region ranks",
                                                              "Roles"]   # Roles since item 14 of the roles loop
    assert list(w.overlays_menu.actions()) == list(view.layer_actions.values())
    assert w.eliza_view_action is view.eliza_action and w.overlays_menu.isEnabled()
    for name, button in canvas.overlay_buttons.items():
        action = view.eliza_action if name == "eliza" else view.layer_actions[name]
        assert button.defaultAction() is action and button.isChecked()
        assert ov.NOT_FRENCHS in action.toolTip() and ov.NOT_FRENCHS in button.toolTip()
        assert 24 <= button.minimumWidth() and button.maximumWidth() <= 80
    assert ov.NOT_FRENCHS in canvas.legend.text() and "as of codelet 59" in canvas.legend.text()   # 44 at item 08
    assert "Eliza's view" in canvas.legend.text() and "regions (blur 4)" in canvas.legend.text()
    assert "[F95 pp. 65-67]" in canvas.toolTip() and "[F95 pp. 65-67]" in canvas.legend.toolTip()
    # A button click flips the model's layer, the action and the picture.
    canvas.overlay_buttons["salience"].click()
    assert not view.model.overlays.layers["salience"] and not view.layer_actions["salience"].isChecked()
    assert not view.salience_item.isVisible() and view.layout.overlays.heat == ()
    assert "salience" not in canvas.legend.text()
    canvas.overlay_buttons["eliza"].click()
    assert not view.model.eliza and not view.layout.eliza
    assert view.name_items["Henry"].pos().y() < 0
    assert "Eliza's view" not in canvas.legend.text()
    # Everything off: the legend is item 04's and nothing is drawn.
    view.set_overlay_state(ALL_OFF)
    assert view.overlay_state() == ALL_OFF
    assert canvas.legend.text() == tv.legend_text()
    assert not view.regions_item.isVisible() and view.layout.overlays.empty
    assert all(not b.isChecked() for b in canvas.overlay_buttons.values())
    # And back on through the menu.
    w.overlays_menu.actions()[0].trigger()
    assert view.model.overlays.layers["regions"] and canvas.overlay_buttons["regions"].isChecked()
    assert view.regions_item.isVisible() and len(view.layout.overlays.regions) == 9
    assert view.layout.overlays.ranks_shown is False
    view.set_overlay_state(ALL_ON)


def test_the_hover_text_lists_the_nine_terms(module_window, ran):
    """French's eight terms plus "role", the ninth, since item 08 (consumer C7); the
    salience line is the answer's, spoon3's since v5's fix (D-116 to D-120), as of the
    run's last codelet (at item 08 it was cup2's, "salience 96.4 as of codelet 44"; at
    item 07 small-glass1's, "salience 65.8 as of codelet 77", listing eight terms)."""
    assert len(FACTORS) == 9 and FACTORS[-1] == "role"
    view = the_view(module_window)
    view.set_overlay_state(ALL_ON)
    text = view.object_items["spoon3"].toolTip()
    assert "salience 57.7 as of codelet 59" in text   # spoon3, the answer, on v5
    assert "salience 76.4 as of codelet 59" in view.object_items["cup2"].toolTip()   # cup2, item 08's answer
    terms = next(line for line in text.splitlines() if " · " in line)   # the roles' lines follow (G-061)
    assert [word for word in terms.split() if word in FACTORS] == list(FACTORS)
    assert terms.split("·")[-1].strip().startswith("role ")
    assert text.splitlines()[-1].startswith("  ") and "roles: " in text
    g1 = view.group_items["g1"].toolTip().splitlines()
    assert any(line.startswith("salience ") for line in g1)
    view.set_layer("salience", False)
    assert "salience " not in view.object_items["cup2"].toolTip()
    view.set_overlay_state(ALL_ON)


def test_the_overlays_are_remembered_in_the_settings(qtbot, ini, thread_errors):
    def settings():
        return QSettings(ini, QSettings.Format.IniFormat)

    w = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w)
    assert the_view(w).overlay_state() == ALL_OFF
    the_view(w).set_overlay_state({"regions": True, "eliza": True})
    w.close()
    assert not w.controller.is_alive()
    assert settings().value("controls/overlay-regions") in (True, "true")
    w2 = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w2)
    try:
        assert the_view(w2).overlay_state() == {"regions": True, "salience": False,
                                                "ranks": False, "roles": False, "eliza": True}
        assert w2.table_view.overlay_buttons["regions"].isChecked()
        assert the_view(w2).layout.eliza and the_view(w2).regions_item.isVisible()
    finally:
        w2.close()
    s = settings()
    s.setValue("controls/overlay-regions", "sometimes")
    s.setValue("controls/overlay-eliza", 0)
    s.sync()
    w3 = MainWindow(settings=settings(), layout_path=None)
    qtbot.addWidget(w3)
    try:
        assert the_view(w3).overlay_state() == ALL_OFF
    finally:
        w3.close()


def test_the_canvas_still_fits_the_smallest_window(module_window):
    canvas = module_window.table_view
    hint = canvas.minimumSizeHint()
    assert hint.width() <= 320 and hint.height() <= 200, (hint.width(), hint.height())
    assert len(canvas.overlay_buttons) == 5      # the roles layer since item 14 of the roles loop
