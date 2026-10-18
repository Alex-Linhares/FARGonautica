"""The temperature panes' model (item 06; `models/temperature.py`; G-027 to G-029),
Qt-free: at every event of a run the thermometer shows the last `TemperatureUpdated`
and nothing newer; the held readings are exactly those before the first Worldview
change and the first unheld one is the jump; the last update is what the answer shows
(D-050); the five series and the marks equal the events; the three terms sum to T
under every profile and under an edited coefficient; French's chapter 4 points equal
`tests/data/ch4.json`'s and are offered for `surround-05` only; the plot's geometry
is the events' at pixel scale; the hover text and the bands' rows."""

import dataclasses
import json
from pathlib import Path

import pytest

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop.config import PROFILES
from tabletop.events import Collector
from tabletop.gui.models import temperature as tm
from tabletop.problems import load_problem
from tabletop.run import run_problem
from tabletop.temperature import clamp

QUICK = ("surround-05", 3)       # Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6. (since v5's fix, D-116 to D-120; cup2 (E2), 44 codelets, T 41.0 from item 08's runs to v2)
PUSH = ("ch2-fig11", 3)          # the push run of G-035 (since item 09's runs; one push since v5's fix,
                                 # D-116 to D-120), a run with a fight the rival won and two the old
                                 # Worldview held: `ch2-fig08` seed 2 pushes no more
CH4 = Path(__file__).resolve().parents[1] / "data" / "ch4.json"


def fed(events):
    """A model fed every event."""
    model = tm.TemperatureModel()
    for event in events:
        model.on_event(event)
    return model


@pytest.fixture(scope="module")
def quick():
    return plain_run(*QUICK, V2)


def last_update_before(events, index):
    """The last `TemperatureUpdated` at or before `index`, with its index."""
    found = None
    for i, event in enumerate(events[:index + 1]):
        if isinstance(event, ev.TemperatureUpdated):
            found = (i, event)
    return found


def test_the_thermometer_shows_the_last_update_at_every_event(quick, golden_runs):
    for run in (quick, golden_runs[("blockage-06", 2)]):
        model = tm.TemperatureModel()
        for index, event in enumerate(run.events):
            model.on_event(event)
            form = model.thermometer()
            found = last_update_before(run.events, index)
            assert model.events == index + 1 and model.clock == event.clock
            if found is None:
                assert form.value is None and form.text == "" and form.fill == 0.0
                assert form.note == "no reading yet"
                continue
            i, update = found
            assert model.last.index == i and model.last.clock == update.clock
            assert form.value == update.temperature and form.held == update.held
            assert form.by == update.by and form.clock == update.clock
            assert form.text == f"{update.temperature:.1f}"
            assert form.fill == pytest.approx(clamp(update.temperature) / 100)
            assert form.age == event.clock - update.clock


def test_the_held_readings_end_at_the_first_change_and_the_jump_is_the_events(quick):
    events = quick.events
    model = fed(events)
    first_change = next(i for i, e in enumerate(events) if isinstance(e, ev.WorldviewChanged))
    assert [r.held for r in model.readings] == [r.index < first_change for r in model.readings]
    held = [r for r in model.readings if r.held]
    # The start's reading and the three scheduled ones at codelets 5, 10 and 15, all held at
    # 80 until the first change at codelet 18 (since item 08's runs and still since v5's fix,
    # D-116 to D-120: nine held to the change at codelet 45 before item 08).
    assert len(held) == 4 and all(r.temperature == 80.0 for r in held)
    assert [r.by for r in held] == ["start"] + ["schedule"] * 3
    assert [r.clock for r in held] == [0, 5, 10, 15]
    jump = model.readings[len(held)]
    assert jump.by == "change" and jump.clock == 18 and jump.index == 289
    assert jump.temperature == pytest.approx(97.937, abs=0.001)      # since v5's fix (96.823 from item 08 to v2)
    # Eight readings follow the jump, none held: the scheduled ones at codelets 20 to 55 and
    # the change at 40 (cp2 beats cp1); the last, at 55, is four codelets before the answer
    # at 59 (since v5's fix, D-116 to D-120; five to codelet 40 and no fight from item 08 to v2).
    assert len(model.readings) == 13 and not any(r.held for r in model.readings[len(held):])
    assert (model.readings[-1].clock, model.readings[-1].by) == (55, "schedule")
    # The thermometer is grey at a held index and black after the jump (G-027).
    assert fed(events[:270]).thermometer().held and fed(events[:270]).thermometer().value == 80.0
    assert "held at the initial 80" in fed(events[:270]).thermometer().note
    after = fed(events[:290]).thermometer()
    assert not after.held and after.text == "97.9" and after.by == "change"


def test_the_answer_shows_the_last_update_not_a_new_one(quick):
    events = quick.events
    model = fed(events)
    answer = next(i for i, e in enumerate(events) if isinstance(e, ev.AnswerGiven))
    assert model.last.index < answer
    assert not any(isinstance(e, ev.TemperatureUpdated) for e in events[answer:])
    form = model.thermometer()
    assert form.value == events[-1].temperature == quick.result.temperature
    assert form.text == "43.6" and form.by == "schedule" and form.clock == 55    # since v5's fix (D-116 to D-120); 41.0 at codelet 40 from item 08's runs to v2
    assert form.age == 4 and "4 codelets old" in form.note
    assert "no update follows the answer" in form.note
    assert model.ended == form.value


def test_the_series_and_the_marks_equal_the_events(quick, golden_runs):
    events = quick.events
    model = fed(events)
    updates = [e for e in events if isinstance(e, ev.TemperatureUpdated)]
    series = tm.series_of(model)
    assert set(series) == {name for name, _l, _b in tm.SERIES}
    for name in series:
        assert series[name] == tuple((e.clock, getattr(e, name)) for e in updates)
    assert len(updates) == 13                      # since v5's fix (D-116 to D-120); 10 from item 08's runs to v2, 16 at item 07
    marks = [(e.clock, "competition" if isinstance(e, ev.CompetitionHeld) else "change")
             for e in events if isinstance(e, (ev.CompetitionHeld, ev.WorldviewChanged))]
    # The entry without a rival at 18 (cp1), then the fight at 40 that cp2 wins (chance 0.32)
    # and its change (since v5's fix, D-116 to D-120; the entry alone and no fight from item
    # 08's runs to v2). An entry logs its change first, a fight its competition first (D-040).
    assert [(m.clock, m.kind) for m in model.marks] == marks == \
        [(18, "change"), (18, "competition"), (40, "competition"), (40, "change")]
    assert model.marks[0].detail.startswith("no rival: +cp1")
    assert model.marks[1].detail == "cp1"
    assert model.marks[2].detail == "cp2" and model.marks[3].detail == "won: +cp2 -cp1"
    assert [r.a for r in model.readings] == [e.a for e in updates]
    # A fought competition's details on runs that fight: the golden `surround-05` seed 2 shows
    # a fight the old Worldview held, cp13 against cp5 at 90, with no change after it (since
    # item 09's runs and still since v5's fix), and the push run of G-035, `ch2-fig11` seed 3,
    # shows the newcomer at the fight and the win at the change (cp1 beats cp4 at 81, after
    # cp4's entry at 69), then two fights the old Worldview held, cp4 at 94 and cp3 at 103
    # (since v5's fix, D-116 to D-120; cp3 beat cp1 at 84 from item 09 to v4).
    held = fed(plain_run("surround-05", 2, V2).events)     # the v2 defaults (D-114)
    assert [(m.clock, m.kind, m.detail) for m in held.marks][6:] == [(90, "competition", "cp13")]
    fought = fed(plain_run(*PUSH, V2).events)
    assert [(m.clock, m.kind, m.detail) for m in fought.marks] == [
        (69, "change", "no rival: +cp4 -"), (69, "competition", "cp4"),
        (81, "competition", "cp1"), (81, "change", "won: +cp1 -cp4"),
        (94, "competition", "cp4"), (103, "competition", "cp3")]


@pytest.mark.parametrize("profile", sorted(PROFILES))
def test_the_terms_sum_to_t_under_every_profile(profile):
    run = plain_run(*QUICK, V2_PROFILES[profile])
    model = fed(run.events)
    assert model.config == V2_PROFILES[profile]
    assert tm.config_of_start(run.events[0]) == V2_PROFILES[profile]
    applied = 0
    for reading in model.readings:
        terms = model.terms(reading)
        assert terms.applied == (not reading.held)
        assert terms.raw == pytest.approx(terms.dq_over_q + terms.changes - terms.competitions)
        if terms.applied:
            applied += 1
            assert clamp(terms.raw) == pytest.approx(reading.temperature)
        else:
            assert terms.raw == 0.0
    assert applied > 0


def test_an_edited_coefficient_comes_back_from_the_start_event():
    config = dataclasses.replace(V2_PROFILES["standard"], temperature_alpha=100.0, dq_signed=True)
    log = Collector()
    run_problem(load_problem(QUICK[0]), config, QUICK[1], [log])
    model = fed(log.events)
    assert model.config == config and model.config.temperature_alpha == 100.0
    for reading in model.readings:
        if not reading.held:
            assert clamp(model.terms(reading).raw) == pytest.approx(reading.temperature)
    assert dict(log.events[0].changed)["temperature_alpha"] == "100.0"


def test_frenchs_points_equal_ch4_json_and_are_offered_for_surround_05_only(quick, golden_runs):
    data = json.loads(CH4.read_text())
    dumps = tuple((d["figure"], d["codelets_run"], d["temperature"]) for d in data["dumps"]
                  if d["temperature"] is not None)
    assert dumps == tm.FRENCH_CH4
    assert [t for _f, _c, t in tm.FRENCH_CH4] == [80.0, 59.0, 43.2, 31.3, 35.8, 13.6, 2.0]
    assert [c for _f, c, _t in tm.FRENCH_CH4] == [2, 22, 33, 36, 47, 47, 55]
    assert data["problem"]["same_as"].startswith("Surround 5")
    model = fed(quick.events)
    assert model.french_offered and model.french_points() == tm.FRENCH_CH4
    other = fed(golden_runs[("blockage-06", 2)].events)
    assert not other.french_offered and other.french_points() == ()
    assert tm.plot_layout(other, 400, 300, french=True).french == ()
    assert "apply to surround-05 only" in tm.plot_layout(other, 400, 300, french=True).note


def test_the_plot_layout_is_the_events_at_pixel_scale(quick):
    model = fed(quick.events)
    lay = tm.plot_layout(model, 400, 300, terms=True, french=True)
    assert (lay.width, lay.height, lay.left, lay.right) == (400, 300, tm.LEFT, 400 - tm.RIGHT)
    # Since v5's fix (D-116 to D-120) the run is 59 codelets, past French's last point, Fig.
    # 4.9 at 55: the axis reaches the run's 59 with his points on or off (from item 08's runs
    # to v2 the run was 44 codelets and his 55 set the axis with his points on).
    assert lay.x_max == 59 and not lay.empty
    assert tm.plot_layout(model, 400, 300).x_max == 59 == model.clock
    assert [kind for kind, _s in tm.BAND_SHARES] == [b.kind for b in lay.bands]
    t_band = lay.band("temperature")
    assert (t_band.low, t_band.high) == (0.0, 100.0) and t_band.label == "T"
    line = next(l for l in t_band.lines if l.name == "temperature")
    assert len(line.points) == len(model.readings) == 13          # since v5's fix (10 from item 08 to v2)
    for (x, y), reading in zip(line.points, model.readings):
        assert x == pytest.approx(lay.x_of(reading.clock))
        assert y == pytest.approx(t_band.bottom - (t_band.bottom - t_band.top) * reading.temperature / 100)
    assert [x for x, _y in line.points] == sorted(x for x, _y in line.points)
    assert lay.x_of(0) == lay.left and lay.x_of(lay.x_max) == lay.right
    # The count band's high is its floor of 2 (`max([2] + counts)`): Δ_WV and T_cp reach 2
    # in this run (an entry and a fight, since v5's fix; 1 from item 08 to v2).
    assert lay.band("fraction").high == 1.0 and lay.band("count").high == 2.0
    assert {l.name for l in lay.band("fraction").lines} == {"q", "dq"}
    assert {l.name for l in lay.band("count").lines} == {"dwv", "tcp"}
    # The marks stand at the competitions' and the changes' clocks: the entry at 18 and the
    # fight cp2 wins at 40 (since v5's fix, D-116 to D-120; the entry alone from item 08 to v2).
    assert [(m.kind, m.x) for m in lay.marks] == [("change", lay.x_of(18)), ("competition", lay.x_of(18)),
                                                  ("competition", lay.x_of(40)), ("change", lay.x_of(40))]
    # The stacked terms: the net of the three areas is the T line where the formula applied.
    assert [a.name for a in lay.areas] == ["dq_over_q", "changes", "competitions"]
    assert lay.areas[2].negative and not lay.areas[0].negative
    for i, reading in enumerate(model.readings):
        if not reading.held:
            assert lay.areas[2].lower[i][1] == pytest.approx(line.points[i][1])
            assert lay.areas[0].lower[i][1] == pytest.approx(t_band.bottom)
    assert lay.areas[1].lower == lay.areas[0].upper and lay.areas[2].upper == lay.areas[1].upper
    # French's points, labelled, at his codelets.
    assert [(p.clock, p.label) for p in lay.french][:2] == [(2, "Fig. 4.2: 80"), (22, "Fig. 4.4: 59")]
    assert lay.french[0].y == pytest.approx(line.points[0][1])      # both 80.0
    assert lay.french[-1].x == pytest.approx(lay.x_of(55)) and lay.french[-1].x < lay.right    # the run's 59 sets the axis, past his last point (since v5's fix; his 55 set it from item 08 to v2)
    assert lay.x_of(59) == lay.right and line.points[-1][0] == pytest.approx(lay.x_of(55))
    assert [label for _x, label in lay.ticks] == ["0", "10", "20", "30", "40", "50"]    # by tens: 59 / 10 <= 8
    assert lay.note.startswith(tm.NOT_FRENCHS) and "13 readings to codelet 55" in lay.note
    assert "terms stacked" in lay.note and "French's ch. 4 thermometer" in lay.note


def test_an_empty_model_and_a_started_run_lay_out(quick):
    empty = tm.TemperatureModel()
    lay = tm.plot_layout(empty, 200, 100)
    assert lay.empty and lay.x_max == 10 and lay.note == f"{tm.NOT_FRENCHS} · no run"
    assert all(line.points == () for b in lay.bands for line in b.lines)
    assert lay.areas == () and lay.french == () and lay.marks == ()
    assert empty.thermometer().note == "no run" and tm.reading_at(empty, lay, 50) is None
    started = fed(quick.events[:1])
    assert started.problem == "surround-05" and started.readings == []
    assert tm.plot_layout(started, 200, 100).note.endswith("no reading yet")
    assert tm.plot_layout(started, 200, 100, terms=True).areas == ()
    # A second RunStarted forgets the first run.
    again = fed(quick.events + quick.events[:1])
    assert again.readings == [] and again.events == 1


def test_the_hover_text_names_the_reading_nearest_the_mouse(quick):
    model = fed(quick.events)
    lay = tm.plot_layout(model, 400, 300)
    assert tm.reading_at(model, lay, lay.x_of(38)) is model.readings[9]       # nearer 40 than 35 (the change at 40; the last reading from item 08 to v2)
    assert tm.reading_at(model, lay, lay.x_of(57)) is model.readings[-1]      # nearer 55 than 50, the last reading (since v5's fix)
    assert tm.reading_at(model, lay, lay.x_of(22)) is model.readings[5]       # nearer 20 than 25
    assert tm.reading_at(model, lay, lay.x_of(0) - 100) is model.readings[0]
    text = tm.reading_text(model.last, model.terms())
    assert text.splitlines()[0] == "codelet 55: T 43.6 by schedule"       # since v5's fix (D-116 to D-120); codelet 40, T 41.0 from item 08's runs to v2
    assert "Q 0.760  ΔQ 0.0464  Δ_WV 2  T_cp 2  a 3.43" in text
    assert text.splitlines()[2].endswith("= 43.6")
    held = tm.reading_text(model.readings[0], model.terms(model.readings[0]))
    assert held.startswith("codelet 0: T 80.0 by start (held, D-049)") and held.endswith("(not applied)")


@pytest.mark.parametrize("height", (30, 100, 300))
def test_the_bands_fit_the_height_in_order(quick, height):
    lay = tm.plot_layout(fed(quick.events), 300, height)
    previous = tm.TOP
    for band in lay.bands:
        assert band.top >= previous and band.bottom > band.top
        previous = band.bottom
    if height >= 100:
        assert lay.bands[-1].bottom <= height - tm.BOTTOM
    for band in lay.bands:
        assert band.ticks[0][0] == band.bottom and band.ticks[1][0] == band.top
