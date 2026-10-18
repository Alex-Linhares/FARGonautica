"""The Slipnet grid's model (item 07; `models/slipnet_grid.py`; G-030 to G-032),
Qt-free: the rows' labels equal `slipnet_def.SCREEN_GRID`, the seventh row holds
the seven nodes not on the screen and the three rows after it the 15 role nodes; after the first `SlipnetUpdated` of `surround-05`
seed 1 every box carries the event's value; at every snapshot index of two golden runs
the activations built from the events equal the snapshot's to a tenth; a jolt outlines
its box for one event; the square's side is a pure function of the activation; French's
nine grids equal `tests/data/ch4.json`'s and the nearest is offered for `surround-05`
only; the layout's geometry; the hover text; what a click selects."""

import json
from pathlib import Path

import pytest

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop import slipnet_def as sd
from tabletop.config import PROFILES
from tabletop.gui.models import slipnet_grid as sg
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.tap import Tap
from tabletop.problems import load_problem
from tabletop.slipnet import ACTIVATION_MAX, ACTIVATION_MIN
from tabletop.world import World

QUICK = ("surround-05", 3)       # Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6. (since v5's fix, D-116 to D-120; cup2 (E2), 44 codelets, T 41.0 from item 08's runs to v2)
CH4 = Path(__file__).resolve().parents[1] / "data" / "ch4.json"
TENTH = 0.051                    # the events round to a tenth (D-019)


def fed(events):
    """A model fed every event."""
    model = sg.SlipnetGridModel()
    for event in events:
        model.on_event(event)
    return model


def tapped(problem, seed):
    """The run with the tap as the world's observer, snapshots after every step."""
    log = EventLog()
    tap = Tap(log)
    world = World(load_problem(problem), V2_PROFILES["standard"], seed, observers=[tap])
    tap.attach(world)
    while not world.finished:
        world.step()
        tap.after_step(world)
    return log


@pytest.fixture(scope="module")
def quick():
    return plain_run(*QUICK, V2)


# -- the rows (G-030) ------------------------------------------------------------------------------

def test_the_rows_are_frenchs_grid_and_the_seventh_holds_the_seven_others():
    assert sg.SCREEN_ROWS == sd.SCREEN_GRID
    # French's six rows, the seventh of the seven nodes his screen lacks, then the 15 role
    # nodes (D-080) in rows of seven: 7, 7, 1.
    assert [len(row) for row in sg.ROWS] == [7, 7, 7, 7, 7, 6, 7, 7, 7, 1]
    assert sg.ROWS == sg.SCREEN_ROWS + (sg.EXTRA_ROW,) + sg.ROLE_ROWS
    assert sg.EXTRA_ROW == ("saucer", "plate", "saltshaker", "pepper shkr", "big glass",
                            "soup bowl", "isolated")
    assert set(sg.EXTRA_ROW) == {n.label for n in sd.NODES
                                 if not n.on_screen and n.name not in sd.ROLE_NODES}
    assert [label for row in sg.ROLE_ROWS for label in row] == \
        [sd.node_named(name).label for name in sd.ROLE_NODES]
    assert len(sd.ROLE_NODES) == 15 and all(not sd.node_named(n).on_screen for n in sd.ROLE_NODES)
    labels = [label for row in sg.ROWS for label in row]
    assert len(labels) == 63 == len(set(labels)) == len(sd.NODES)
    assert {sg.LABEL_TO_NAME[label] for label in labels} == set(sd.NODE_NAMES)
    assert sg.EXTRA_CAPTION.startswith(sg.NOT_FRENCHS)


# -- the activations from the events (G-030) ---------------------------------------------------------

def test_after_the_first_update_the_boxes_carry_its_values(golden_runs):
    events = plain_run("surround-05", 1, V2).events          # the v2 defaults (D-114)
    first = next(i for i, e in enumerate(events) if isinstance(e, ev.SlipnetUpdated))
    assert isinstance(events[0], ev.RunStarted) and events[first].clock == 0
    model = fed(events[:first + 1])
    changed = dict(events[first].changed)
    assert changed["opposite"] == 136.2 and changed["cup"] == 47.0
    assert model.activation["opposite"] == 136.2 and model.activation["cup"] == 47.0
    for name, value in changed.items():
        assert model.activation[name] == value
    for name in sd.NODE_NAMES:
        if name not in changed:
            assert model.activation[name] == ACTIVATION_MIN
    lay = model.layout(280, 300)
    assert lay.box("opposite").number == "136" and lay.box("cup").number == "47"
    assert lay.box("isolated").number == "1" and lay.box("isolated").square == 0.0
    assert model.events == first + 1 and model.index == first and model.problem == "surround-05"


def test_the_activations_equal_every_snapshots_to_a_tenth():
    for problem, seed in (QUICK, ("blockage-06", 2)):
        log = tapped(problem, seed)
        model = sg.SlipnetGridModel()
        checked = 0
        for index, event in enumerate(log.events):
            model.on_event(event)
            if index not in log.snapshots.indexes:
                continue
            state = log.state_at(index)
            for node in state.slipnet:
                assert abs(node["activation"] - model.activation[node["name"]]) <= TENTH, \
                    (problem, index, node["name"])
                assert model.depth(node["name"]) == node["depth"]
            checked += 1
        assert checked >= 40 and model.ended
        assert model.pushes == model.pops == 0        # ANSWERS.md 11: pushes are rare


def test_a_jolt_outlines_its_box_for_one_event_and_the_hover_names_it(quick):
    events = quick.events
    jolts = [i for i, e in enumerate(events) if isinstance(e, ev.NodeJolted)]
    assert len(jolts) > 50
    model = sg.SlipnetGridModel()
    for index, event in enumerate(events):
        model.on_event(event)
        outlined = {name for name in sd.NODE_NAMES if model.jolted(name)}
        if isinstance(event, ev.NodeJolted):
            assert outlined == {event.node}
            assert model.activation[event.node] == event.activation
            text = model.tooltip(event.node)
            assert f"last jolt +{event.amount:.0f} by {event.by} at this event" in text
            assert model.layout(280, 300).box(event.node).jolted
        else:
            assert outlined == set()
    jolt = model.jolts["opposite"]
    assert jolt.by == "focus" and jolt.amount > 0 and jolt.index in jolts
    assert f"by focus at codelet {jolt.clock}" in model.tooltip("opposite")


def test_a_run_started_forgets_the_run(quick, golden_runs):
    model = fed(quick.events)
    assert model.activation["opposite"] > 100 and model.jolts
    model.on_event(golden_runs[("blockage-06", 2)].events[0])
    assert all(a == ACTIVATION_MIN for a in model.activation.values())
    assert model.jolts == {} and model.events == 1 and model.problem == "blockage-06"
    assert not model.french_offered and model.french_grid() is None


# -- the square and the number (G-031) -------------------------------------------------------------

def test_the_square_is_a_pure_function_of_the_activation():
    inner = 30.0
    assert sg.square_side(ACTIVATION_MIN, inner) == 0.0
    assert sg.square_side(ACTIVATION_MAX, inner) == inner
    assert sg.square_side(0.0, inner) == 0.0 and sg.square_side(500.0, inner) == inner
    assert sg.square_side(75.5, inner) == pytest.approx(inner / 2)
    sides = [sg.square_side(a, inner) for a in range(1, 151)]
    assert sides == sorted(sides) and len(set(sides)) == 150
    assert sg.square_side(44.0, inner) / sg.square_side(139.0, inner) == pytest.approx(43 / 138)
    assert sg.square_side(44.0, inner) == sg.square_side(44.0, inner)
    assert sg.square_side(44.0, 60.0) == 2 * sg.square_side(44.0, inner)
    assert [sg.number_text(a) for a in (1.0, 1.4, 136.2, 47.0, 149.6, 150.0)] == \
        ["1", "1", "136", "47", "150", "150"]


# -- French's grids (G-032) ------------------------------------------------------------------------

def test_frenchs_grids_equal_ch4_json():
    data = json.loads(CH4.read_text())
    assert [list(row) for row in sg.SCREEN_ROWS] == data["slipnet_grid"]
    assert len(sg.FRENCH_CH4_GRIDS) == len(data["dumps"]) == 9
    for (figure, page, codelets, values), dump in zip(sg.FRENCH_CH4_GRIDS, data["dumps"]):
        assert (figure, page, codelets) == (dump["figure"], dump["page"], dump["codelets_run"])
        assert sg.french_grid_of((figure, page, codelets, values)) == dump["slipnet"]
        assert len(values) == 41


def test_the_nearest_dump_by_codelets_ties_to_the_earlier_figure():
    assert [sg.nearest_dump(c)[0] for c in (0, 1, 2, 7, 8, 17, 18, 33, 34, 35, 47, 51, 52, 99)] == \
        ["4.1", "4.1", "4.2", "4.2", "4.3", "4.3", "4.4", "4.5", "4.5", "4.6", "4.7", "4.7",
         "4.9", "4.9"]


def test_frenchs_grid_is_offered_for_surround_05_only(quick, golden_runs):
    model = fed(quick.events)
    assert model.french_offered and model.clock == 59        # since v5's fix (D-116 to D-120): 59 (44 from item 08's runs to v2, 77 before)
    grid = model.french_grid()         # the nearest dump to 59 codelets is Fig. 4.9 at 55 (Fig. 4.7 at 47 for the 44 of item 08 to v2; Fig. 4.9 for the 77 of item 07)
    assert (grid["figure"], grid["codelets"], grid["page"]) == ("4.9", 55, 107)
    assert grid["values"]["opposite"] == 150 and grid["values"]["cup"] == 130
    lay = model.layout(280, 300, french=True)
    assert lay.box("opposite").french == 150 and lay.box("isolated").french is None
    assert lay.note == "red: French's Fig. 4.9 at 55 codelets, beside the run's at codelet 59"
    assert sum(1 for b in lay.boxes if b.french is not None) == 41
    other = fed(golden_runs[("blockage-06", 2)].events)
    assert not other.french_offered
    lay = other.layout(280, 300, french=True)
    assert all(b.french is None for b in lay.boxes)
    assert lay.note == "French's chapter 4 grids apply to surround-05 only"
    assert other.layout(280, 300).note == ""


# -- the layout (G-031) ----------------------------------------------------------------------------

def test_the_layout_is_ten_rows_of_square_boxes(quick):
    model = fed(quick.events)
    lay = model.layout(280, 300, selected="cup")
    assert len(lay.boxes) == 63 and (lay.width, lay.height) == (280.0, 300.0)
    # French's six rows, the seventh of the nodes his screen lacks, then the 15 role nodes
    # (D-080) as the roles' band, in rows of seven under its own caption (G-062).
    assert [len([b for b in lay.boxes if b.row == r]) for r in range(10)] == [7, 7, 7, 7, 7, 6, 7, 7, 7, 1]
    assert [b.role for b in lay.boxes] == [b.row >= sg.FIRST_ROLE_ROW for b in lay.boxes]
    side = lay.side
    assert 0 < side <= 280 / 7
    # At 280 x 300 the ten rows, not the seven columns, set the side; two caption lines.
    assert side == pytest.approx((300 - 2 * sg.CAPTION_HEIGHT) / (len(sg.ROWS) + 2 * sg.GAP + 2 * sg.MARGIN))
    assert lay.left == pytest.approx((280 - 7 * side) / 2) and lay.left >= sg.MARGIN * side
    for box in lay.boxes:
        assert box.side == side and box.name == sg.LABEL_TO_NAME[box.label]
        assert box.x == pytest.approx(lay.left + box.col * side)
        gap = sg.GAP * side if box.row >= 6 else 0.0      # one gap before the seventh row
        if box.row >= sg.FIRST_ROLE_ROW:                  # its caption and a gap before the roles' band
            gap += sg.GAP * side + sg.CAPTION_HEIGHT
        assert box.y == pytest.approx(sg.MARGIN * side + box.row * side + gap)
        assert box.on_screen == (box.row < 6)
        assert box.square == sg.square_side(box.activation, sg.INNER * side)
        assert box.number == sg.number_text(box.activation)
        assert box.selected == (box.name == "cup")
        assert 0 <= box.x and box.x + side <= 280
        # The last row's bottom is the roles' caption's top, to a rounding (the same sum in another order).
        assert box.y + side <= lay.role_caption_y or box.y + side == pytest.approx(lay.role_caption_y)
    assert lay.caption == sg.EXTRA_CAPTION and lay.role_caption == sg.ROLE_CAPTION
    assert lay.role_caption_y + sg.CAPTION_HEIGHT <= 300
    # The seventh row's caption sits right under it, the roles' under the band (G-062).
    assert lay.caption_y == pytest.approx(sg.MARGIN * side + (7 + sg.GAP) * side)
    assert lay.role_caption_y == pytest.approx(sg.MARGIN * side + (len(sg.ROWS) + 2 * sg.GAP) * side
                                               + sg.CAPTION_HEIGHT)
    assert [b.label for b in lay.boxes[:7]] == list(sd.SCREEN_GRID[0])
    assert [b.label for b in lay.boxes[-1:]] == list(sg.ROLE_ROWS[-1])
    cup = lay.box("cup")
    assert sg.box_at(lay, cup.x + 1, cup.y + 1) is cup
    assert sg.box_at(lay, cup.x + side / 2, cup.y + side / 2) is cup
    assert sg.box_at(lay, 279, 299) is None and lay.box("no such node") is None
    tall = model.layout(100, 600)          # here the seven columns set the side
    assert tall.side == pytest.approx(100 / (7 + 2 * sg.MARGIN))
    assert tall.left == pytest.approx(sg.MARGIN * tall.side)
    assert sg.grid_layout(model, 280, 300, False, "cup") == lay
    # French's note takes a second caption line, so the boxes shrink to make room.
    noted = model.layout(280, 300, french=True)
    assert noted.note != "" and noted.side < side
    assert noted.caption_y + 2 * sg.CAPTION_HEIGHT <= 300


# -- the hover text and the selection (G-030, G-032) -----------------------------------------------

def test_the_hover_text_names_the_level_depth_place_and_links(quick):
    log = tapped(*QUICK)
    model = fed(log.events)
    model.update(log.state_at(len(log.events) - 1))
    text = model.tooltip("cup")
    lines = text.splitlines()
    assert lines[0] == f"cup: activation {model.activation['cup']:.1f} at codelet 59"     # since v5's fix (D-116 to D-120): 59 (44 from item 08's runs to v2)
    assert lines[1] == "level 1, depth 20" and lines[2] == "box 'cup', row 5, column 1 of French's grid"
    assert any(line.startswith("last jolt +") for line in lines)
    links = [line for line in lines if line.startswith("  ")]
    assert len(links) == len(sd.links_of("cup"))
    assert "  cup ISA liquid holder, length 15" in links
    assert f"(effective lengths as of codelet {model.dated})" in text and model.dated == 59
    assert any(", effective " in line for line in links)
    assert "level 3, depth 80" in model.tooltip("opposite")
    isolated = model.tooltip("isolated")
    assert "seventh row: not on French's screen" in isolated and sg.NOT_FRENCHS in isolated
    # isolated's one link is the role link to figure (ROLE_LINKS, D-080); the relation nodes
    # without links, horizontal among them, read "links: none".
    assert "links (1)" in isolated and "  isolated - figure [role], length 35" in isolated
    assert sd.links_of("horizontal") == [] and "links: none" in model.tooltip("horizontal")
    assert model.selection_of("cup") == ("node", "cup", ())
    # An older snapshot never rolls the dating back; None changes nothing.
    model.update(log.state_at(10))
    model.update(None)
    assert model.dated == 59


def test_a_fresh_model_before_any_run():
    model = sg.SlipnetGridModel()
    assert model.index == -1 and model.clock == 0 and not model.french_offered
    assert model.depth("cup") == PROFILES["standard"].depth_by_level[0]
    lay = model.layout(200, 200, french=True)
    assert all(b.number == "1" and b.square == 0.0 and not b.jolted for b in lay.boxes)
    assert "cup: activation 1.0 at codelet 0" in model.tooltip("cup")
    assert "as of codelet" not in model.tooltip("cup")
