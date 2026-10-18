"""The Slipnet network pane (item 08; `slipnet_network_view.py`; G-033 to G-035),
offscreen, one shared window and one run of `surround-05` seed 3: the run through the
window equals the plain run and the network's backing store equals a fresh render at
sampled events, with the pixmap rendered fewer times than painted; the drawn layout is
the model's and the banner says "no context push in this run"; a fresh view fed the
history draws the window's picture; the grid's selection reaches the network and a click
here selects; the hover texts; on `ch2-fig11` seed 3 the push and the pop ring the
nodes their events name; the pane keeps its small minimum."""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, Qt  # noqa: E402
from PySide6.QtGui import QColor  # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop import events as ev  # noqa: E402
from tabletop.gui import slipnet_network_view as nv  # noqa: E402
from tabletop.gui.models import slipnet_network as sn  # noqa: E402
from test_table_view import EVERY, fresh, run_to_end, same_picture, shown  # noqa: E402
from golden import V2, V2_PROFILES  # noqa: E402

QUICK = ("surround-05", 3)
PUSH_RUN = ("ch2-fig11", 3)          # the first by seed of the 82 runs with a push in a bounded search
                                     # of 15,900 runs: every problem, seeds 1-300 (G-035; since item 09's
                                     # runs, with the FIGURE, GROUND, ECHO, TWIN and TYPE_KEY detectors;
                                     # `ch2-fig08` seed 2 pushes no more). Since v5's fix (D-116 to D-120): one
                                     # push and one pop of cp4 (four of each, all of cp4, from item 09 to v4).
PROBES = {}


class NetworkProbe:
    """At every `EVERY`th `event_drawn`: what the window shows of the network equals a
    fresh render of it (Numbo GUI, `test_polish.py`)."""

    def __init__(self, window):
        self.window = window
        self.widget = window.slipnet_network_view.network
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
    """The quick run through the shared window with the network dock raised."""
    w = module_window
    w.resize(1600, 1000)
    w.slipnet_network_dock.raise_()
    qapp.processEvents()
    PROBES["network"] = NetworkProbe(w)
    outcome = run_to_end(w, qapp, *QUICK)
    w.event_drawn.disconnect(PROBES["network"].drawn)
    return outcome


def near(colour, target, tolerance=70):
    """Whether a colour is within `tolerance` of the target on every channel (the rings
    are antialiased, so a pure pixel is not promised)."""
    return all(abs(a - b) <= tolerance for a, b in
               zip((colour.red(), colour.green(), colour.blue()),
                   (target.red(), target.green(), target.blue())))


def ring_colours(widget, dot, offset):
    """The colours along a short horizontal scan to the right of a dot, where a ring at
    `offset` beyond the radius lands."""
    image = widget.grab().toImage()
    y = int(round(dot.y))
    xs = range(int(dot.x + dot.radius + offset - 2), int(dot.x + dot.radius + offset + 3))
    return [QColor(image.pixel(x, y)) for x in xs]


def test_the_run_through_the_window_equals_the_plain_run_and_the_pixels_are_the_events(module_window, ran):
    w = module_window
    ref = plain_run(*QUICK, V2)
    assert list(w.history.records) == list(ref.records)
    assert ran.result.draws == ref.draws
    probe = PROBES["network"]
    assert probe.checked == list(range(0, len(w.history), EVERY))
    assert probe.mismatches == []
    widget = w.slipnet_network_view.network
    assert widget.paints >= len(w.history) and 0 < widget.renders < widget.paints


def test_the_drawn_layout_is_the_models_and_the_banner_says_no_push(module_window, ran, qapp):
    w = module_window
    pane = w.slipnet_network_view
    assert pane.pane_id == "P3b" and pane.events == len(w.history)
    assert pane.history is w.history and pane.network.selection is w.selection
    w.slipnet_network_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    lay = pane.network.drawn
    assert lay == pane.layout_now and len(lay.dots) == 63 and len(lay.lines) == 95
    assert (lay.width, lay.height) == (pane.network.width(), pane.network.height())
    assert lay.banner == sn.NO_PUSH and pane.model.pushes == []
    assert lay.dot("opposite").shortening and lay.line("left-right").shortened
    assert sn.NOT_FRENCHS in pane.pages and '[F95 p. 61 (img)]' in pane.pages
    assert pane.pages in pane.toolTip() and sn.NOT_FRENCHS in pane.label.text()
    assert 'D-011' in pane.label.text() and 'D-021' in pane.label.text()
    image = pane.network.grab().toImage()
    opposite = lay.dot("opposite")
    assert QColor(image.pixel(int(opposite.x), int(opposite.y))) == nv.DISC
    assert QColor(image.pixel(1, int(lay.height) - 1)) == nv.BACKGROUND


def test_a_fresh_view_fed_the_history_draws_the_windows_picture(module_window, ran, tmp_path):
    w = module_window
    live = w.slipnet_network_view
    events = w.history.events
    for index in (0, 211, len(events) - 1):
        assert w.seek(index)
        other = nv.SlipnetNetworkView(pages=live.pages)
        other.resize(live.size())
        other.network.resize(live.network.size())
        for event in events[:index + 1]:
            other.on_event(event)
        assert other.model.effective == live.model.effective
        assert other.layout_now == live.layout_now
        a = live.render_png(tmp_path / f"live-{index}.png")
        b = other.render_png(tmp_path / f"fresh-{index}.png")
        assert a == b, index


def test_the_selection_is_shared_with_the_grid(module_window, ran, qtbot, qapp):
    w = module_window
    pane = w.slipnet_network_view
    w.slipnet_network_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    before = pane.paints
    w.selection.select("node", "cup")                  # as a click on the grid does (G-030)
    assert pane.paints == before + 1
    lay = pane.network.drawn
    cup = lay.dot("cup")
    assert cup.selected and lay.line("cup-saucer").selected
    assert any(near(c, nv.SELECTED) for c in ring_colours(pane.network, cup, 5))
    qtbot.mouseClick(pane.network, Qt.MouseButton.LeftButton,
                     pos=QPoint(int(cup.x), int(cup.y)))
    assert w.selection.empty and not pane.network.drawn.dot("cup").selected
    assert not any(near(c, nv.SELECTED) for c in ring_colours(pane.network, cup, 5))
    saucer = lay.dot("saucer")
    qtbot.mouseClick(pane.network, Qt.MouseButton.LeftButton,
                     pos=QPoint(int(saucer.x), int(saucer.y)))
    assert (w.selection.kind, w.selection.name) == ("node", "saucer")
    assert w.statusBar().currentMessage().startswith("selected saucer: saucer: activation ")
    assert w.slipnet_grid_view.grid.current_layout().box("saucer").selected
    qtbot.mouseClick(pane.network, Qt.MouseButton.LeftButton, pos=QPoint(2, int(lay.height) - 2))
    assert w.selection.empty


def test_the_hover_texts_are_the_nodes_and_the_links(module_window, ran, qtbot, qapp):
    w = module_window
    pane = w.slipnet_network_view
    w.slipnet_network_dock.raise_()
    qapp.processEvents()
    pane.redraw(now=True)
    lay = pane.network.drawn
    cup = lay.dot("cup")
    qtbot.mouseMove(pane.network, QPoint(int(cup.x), int(cup.y)))
    qapp.processEvents()
    assert pane.network.toolTip() == pane.model.tooltip("cup")
    line = lay.line("knife-fork")
    mx, my = line.midpoint
    hit = sn.dot_at(lay, mx, my)
    if hit is None:                                     # the midpoint is clear of every dot
        qtbot.mouseMove(pane.network, QPoint(int(mx), int(my)))
        qapp.processEvents()
        assert pane.network.toolTip() == pane.model.link_tooltip(sn.line_at(lay, mx, my).name)
    qtbot.mouseMove(pane.network, QPoint(2, int(lay.height) - 2))
    qapp.processEvents()
    assert pane.network.toolTip() == f"{sn.NO_PUSH}\n{sn.CAPTION}"


def test_the_push_and_the_pop_ring_the_nodes_their_events_name(qtbot, qapp):
    ref = plain_run(*PUSH_RUN, V2)
    events = ref.events
    push = next(i for i, e in enumerate(events) if isinstance(e, ev.ContextPushed))
    pop = next(i for i, e in enumerate(events) if isinstance(e, ev.ContextPopped))
    assert (push, pop) == (849, 857)           # the one pair since v5's fix (D-116 to D-120); 851/859
                                               # from item 09's runs to v4, then three more pairs at
                                               # 994/1001, 1047/1054 and 1228/1235; 828 and 836 on
                                               # `ch2-fig08` seed 2 at item 08
    pane = nv.SlipnetNetworkView(pages="x")
    qtbot.addWidget(pane)
    pane.resize(700, 420)
    pane.show()
    qapp.processEvents()
    for event in events[:push + 1]:
        pane.on_event(event)
    pane.redraw(now=True)
    lay = pane.network.drawn
    assert lay.banner.startswith("inside cp4 since codelet 73: 6 nodes reset to 1")    # since item 09's runs (cp1 at codelet 75 at item 08)
    reset = [d for d in lay.dots if d.reset]
    assert {d.name for d in reset} == {n for n, _ in events[push].reset}
    sameness = lay.dot("sameness")
    assert sameness.reset and sameness.fill == 0.0
    assert any(near(c, nv.PUSH) for c in ring_colours(pane.network, sameness, 3))
    for event in events[push + 1:pop + 1]:
        pane.on_event(event)
    pane.redraw(now=True)
    lay = pane.network.drawn
    assert lay.banner.startswith("popped cp4 at codelet 74: 6 reinitialized")
    assert not any(d.reset for d in lay.dots)
    assert {d.name for d in lay.dots if d.restored} == {n for n, _ in events[pop].restored}
    many = lay.dot("many")           # restored to 61.0 by the fixed term alone (see the model's test)
    assert many.restored and many.activation == 61.0 and many.fill > 0
    assert any(near(c, nv.POP) for c in ring_colours(pane.network, many, 3))
    assert not any(near(c, nv.PUSH) for c in ring_colours(pane.network, lay.dot("sameness"), 3))
    for event in events[pop + 1:]:
        pane.on_event(event)
    pane.redraw(now=True)
    lay = pane.network.drawn
    assert lay.banner.startswith("popped cp4 at codelet 74") and lay.banner.endswith("pushes 1, pops 1")   # since v5's fix (D-116 to D-120): the one pair ("popped cp4 at codelet 110", "pushes 4, pops 4" from item 09 to v4)
    assert not any(d.restored for d in lay.dots) and not any(d.reset for d in lay.dots)
    assert pane.events == len(events) == 1229 and pane.model.ended      # since v5's fix (D-116 to D-120); 1467 from item 09's runs to v4 (1307 at item 08)


def test_the_pane_is_small_enough(module_window):
    pane = module_window.slipnet_network_view
    assert (pane.minimumWidth(), pane.minimumHeight()) == nv.MIN_SIZE == (150, 150)
    assert pane.label.minimumWidth() <= 1
