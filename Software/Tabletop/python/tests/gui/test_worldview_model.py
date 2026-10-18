"""The Worldview and the competitions models (item 10; `models/worldview.py`,
`models/competitions.py`; G-039 to G-041), Qt-free:

- at every snapshot index of `surround-05` seed 3 and `surround-04` seed 187 (two fights,
  one won by the newcomer, then one held by the Old Worldview; item 05 of the roles loop
  left `blockage-06` seed 2 one fight, item 07 re-pinned every run fact, item 08 left
  `surround-04` seed 69 one competition without a fight, item 09 left seed 22 one and v5's
  fix left seed 187 two fights of its three) the
  Worldview's entries and the shadow cabinet's equal the
  snapshot's correspondences, their points, wins and losses the snapshot's, the rating
  and its pairs the snapshot's and `worldview.current`'s on a plain world at the same
  step, and the rivals `worldview.rivals_of`'s, read without a draw;
- the hand-built chapter 4 state of `tests/test_worldview.py` (`chapter_4_table`,
  `fig_4_7`), fed as a snapshot alone, shows the two Worldview correspondences with their
  incoherent and parallel pair and the cup-glass newcomer as cp1's rival;
- the competitions: one entry per `CompetitionHeld`, equal to `Snapshot.competitions` and
  `Snapshot.changes` at every snapshot, the winner as `rival_wins`, the Old and Rival
  Worldviews, the entry without a fight, the fresh mark at the event itself;
- both empty banners, and the plain words for one correspondence (ANSWERS.md 11);
- the proposals with their stages (D-024) against the snapshot's, on `surround-05` seed 67,
  which proposes three groups (seed 68, the run until v4, proposes two since v5's fix;
  seed 27, the run until item 09, proposes none since; seed 8, the run until item 08, none
  since; seed 2, nine before item 07, proposed three with none past stage 0 until item 08
  and none since; seed 6 proposes none since item 05);
- the lines' roles and the hover texts.
"""

import importlib.util
import sys
from pathlib import Path

import pytest

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop import worldview as WV
from tabletop.config import PROFILES
from tabletop.gui.models import competitions as cm
from tabletop.gui.models import worldview as wm
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.tap import Tap, worldview_readings
from tabletop.problems import load_all, load_problem
from tabletop.snapshot import snapshot as take_snapshot
from tabletop.world import World

QUICK = ("surround-05", 3)       # one correspondence in the Worldview at the end (cp2, which beat cp1); one proposal
FIGHTS = ("surround-04", 187)    # three competitions: cp3 enters alone, cp2 beats it, then cp3 comes back and is held off
HELD = ("ch2-fig13", 4)          # fights the Old Worldview holds (codelets 62 and 67), then one it loses (68)
PROPOSALS = ("surround-05", 67)  # three bottom-up group proposals, one (p1) past stage 0, none built
NO_PROPOSALS = ("blockage-06", 3)    # no bottom-up group proposed at all
# Item 05 (the roles loop) moved every phenomenon but the quick run's: blockage-06 seed 2 holds
# one fight now, surround-05 seed 10's held-off newcomer never wins, surround-05 seed 6 proposes
# no group and blockage-06 seed 12 proposes one. Item 07 (ROW and COLUMN roles, the re-cast)
# changed every run: FIGHTS and HELD still showed their phenomena with other correspondences
# at other codelets; surround-05 seed 2 proposed three groups, none past stage 0, so PROPOSALS
# moved to seed 8; blockage-06 seed 11 proposes one, so NO_PROPOSALS moved to seed 3 (18
# codelets, no correspondence at all). Item 08 (GAP, LONER, STRAY and CORNER roles; the
# midline on tables of loners) changed every run again: surround-04 seed 69 holds one
# competition and no fight, so FIGHTS moved to seed 22; ch2-fig13 seed 28's newcomer wins at
# once, never held off, so HELD moved to seed 27; surround-05 seed 8 proposes no group, so
# PROPOSALS moved to seed 27; and the quick run (44 codelets) proposes none (re-pinned below).
# Item 09 (FIGURE, GROUND, ECHO, TWIN and TYPE_KEY roles) changed every run but the quick
# one and NO_PROPOSALS: surround-04 seed 22 holds one competition and no fight, so FIGHTS
# moved to seed 187 (cp3 enters alone at codelet 59, holds cp7 off at 69 and cp2 at 70, and
# cp2 beats it at 80); ch2-fig13 seed 27's two held-off newcomers never win, so HELD moved to
# seed 293; and surround-05 seed 27 proposes no group, so PROPOSALS moved to seed 68. Since
# v5's fix (D-116 to D-120) every run changed again: surround-04 seed 187 keeps a won fight,
# a held one and its proposal (cp3 enters alone at codelet 59, cp2 beats it at 71, and cp3
# comes back at 76 and is held off), so FIGHTS stays; ch2-fig13 seed 293's one fight is won
# at once, never held, so HELD moved to seed 4 (cp4 enters alone at 40, holds cp6 off at 62
# and 67, and cp6 beats it at 68); surround-05 seed 68 proposes two groups, so PROPOSALS
# moved to seed 67, the first seed from 1 that proposes three with p1 alone past stage 0;
# the quick run's cp2 beats cp1 at 40 and the run proposes p1 (re-pinned below); and
# NO_PROPOSALS is unchanged.
STANDARD = V2_PROFILES["standard"]
ENGINE_TESTS = Path(__file__).resolve().parents[1]


def tapped(problem, seed):
    """The run with the tap as the world's observer, snapshots after every step."""
    log = EventLog()
    tap = Tap(log)
    world = World(load_problem(problem), STANDARD, seed, observers=[tap])
    tap.attach(world)
    while not world.finished:
        world.step()
        tap.after_step(world)
    return log


@pytest.fixture(scope="module")
def logs():
    return {case: tapped(*case) for case in (QUICK, FIGHTS, PROPOSALS)}


def fed(events, upto=None, log=None):
    """A Worldview model given the events up to `upto` (inclusive), with the snapshot in
    force, its terms and its readings when a log is given."""
    model = wm.WorldviewModel()
    last = len(events) - 1 if upto is None else upto
    for event in events[:last + 1]:
        model.on_event(event)
    if log is not None:
        model.update(log.state_at(last), log.terms_at(last), log.worldview_at(last))
    return model


def fights_fed(events, upto=None):
    model = cm.CompetitionsModel()
    last = len(events) - 1 if upto is None else upto
    for event in events[:last + 1]:
        model.on_event(event)
    return model


def engine_worldview_tests():
    """The engine's `tests/test_worldview.py`, loaded by path, for its hand-built chapter 4
    state (ANSWERS.md 11)."""
    spec = importlib.util.spec_from_file_location("engine_test_worldview",
                                                  ENGINE_TESTS / "test_worldview.py")
    module = importlib.util.module_from_spec(spec)
    if "v2_defaults" not in sys.modules:       # it imports `v2_defaults` from tests/, which pytest
        v2_spec = importlib.util.spec_from_file_location("v2_defaults",   # puts on the path only
                                                         ENGINE_TESTS / "v2_defaults.py")
        v2_module = importlib.util.module_from_spec(v2_spec)    # when the whole suite is collected,
        v2_spec.loader.exec_module(v2_module)                   # so it is loaded by its path here,
        sys.modules["v2_defaults"] = v2_module                  # as `golden.py` loads it (no sys.path)
    spec.loader.exec_module(module)
    return module


# -- the entries and the rating equal the snapshot's and worldview.current's (G-039) -----------

@pytest.mark.parametrize("case", [QUICK, FIGHTS])
def test_the_entries_and_the_rating_equal_the_snapshots_and_the_plain_worlds(logs, case):
    log = logs[case]
    events = log.events
    plain = World(load_problem(case[0]), STANDARD, case[1])
    checked = 0
    for index in log.snapshots.indexes:
        snap = log.state_at(index)
        while plain.clock < snap.clock and not plain.finished:
            plain.step()
        model = fed(events, index, log)
        worldview, shadow = model.entries()
        assert [e.name for e in worldview] == list(snap.worldview["names"])
        by_name = {c["name"]: c for c in snap.correspondences}
        assert [e.name for e in shadow] == [n for n in by_name if n not in snap.worldview["names"]]
        for e in worldview + shadow:
            c = by_name[e.name]
            assert (e.static, e.dynamic, e.bonus) == (c["static"], c["dynamic"], c["bonus"])
            assert e.rating == pytest.approx(c["rating"]) and e.dated
            assert (e.wins, e.losses, e.in_worldview) == (c["wins"], c["losses"], c["in_worldview"])
            assert e.slippages == tuple(tuple(s) for s in c["slippages"])
        r = model.rating()
        assert (r.names, r.total, r.coherent) == (snap.worldview["names"], snap.worldview["total"],
                                                  snap.worldview["coherent"])
        assert (r.strength, r.coherence, r.parallelism) == (
            snap.worldview["strength"], snap.worldview["coherence"], snap.worldview["parallelism"])
        assert (r.coherent_pairs, r.incoherent_pairs, r.parallel_pairs) == (
            snap.worldview["coherent_pairs"], snap.worldview["incoherent_pairs"],
            snap.worldview["parallel_pairs"])
        assert r.as_of == snap.clock and r.latest is None
        current = WV.current(plain)
        assert r.total == pytest.approx(current.total) and r.names == tuple(current.names)
        checked += 1
    assert checked == len(log.snapshots.indexes) >= 40


def test_the_rivals_are_the_engines_and_are_read_without_a_draw(logs):
    log = logs[FIGHTS]
    plain = World(load_problem(FIGHTS[0]), STANDARD, FIGHTS[1])
    rivals_seen = 0
    for index in log.snapshots.indexes:
        snap = log.state_at(index)
        while plain.clock < snap.clock and not plain.finished:
            plain.step()
        draws = plain.rng.draws
        expected = worldview_readings(plain)["rivals"]
        assert plain.rng.draws == draws
        assert expected == {cp.name: tuple(r.name for r in WV.rivals_of(plain, cp))
                            for cp in plain.workspace.correspondences}
        assert log.worldview_at(index)["rivals"] == expected
        model = fed(log.events, index, log)
        for e in sum(model.entries(), []):
            assert e.rivals == expected[e.name]
            rivals_seen += len(e.rivals)
    assert rivals_seen > 0
    # The readings sit beside every snapshot (G-039), and are empty before the first.
    assert sorted(log.snapshots.worldview_by_index) == log.snapshots.indexes
    assert log.worldview_at(0) == {"rivals": {}}


def test_the_entries_follow_the_events_between_snapshots(logs):
    """At the second competition's event (index 813 of the fights run, codelet 71) cp2 has
    won against cp3 but the change comes five events later: the model shows the win and
    the old membership, as the events say, and the rating's `latest` once the change has
    come. (Since v5's fix (D-116 to D-120); at item 09: the fourth competition, index 897,
    codelet 80, cp2 against cp3, the change five events later; at item 08: the second
    competition of seed 22, index 1048, codelet 91, cp2 against cp1, the change five events
    later; before item 08: the fourth competition, index 934, codelet 90, cp6 against cp5,
    the change five events later; before item 07: the third competition, index 553, codelet
    49, cp2 against cp1, the change seven events later.)"""
    events = logs[FIGHTS].events
    held = next(i for i, e in enumerate(events)
                if isinstance(e, ev.CompetitionHeld) and e.newcomer == "cp2" and e.rival_wins)
    changed = next(i for i, e in enumerate(events)
                   if isinstance(e, ev.WorldviewChanged) and e.added == ("cp2",))
    assert (held, changed) == (813, 818)
    model = fed(events, held, logs[FIGHTS])
    worldview, shadow = model.entries()
    assert [e.name for e in worldview] == ["cp3"] and worldview[0].losses == 1
    assert worldview[0].wins == 0          # entered without a fight at codelet 59, which awards
    # nothing; its first fight is this loss
    cp2 = next(e for e in shadow if e.name == "cp2")
    assert cp2.wins == 1 and cp2.losses == 0 and cp2.status.startswith("competing with cp3")
    assert model.rating().latest is None
    model = fed(events, changed, logs[FIGHTS])
    assert [e.name for e in model.entries()[0]] == ["cp2"]
    clock, total, coherent, reason = model.rating().latest
    assert (clock, reason, coherent) == (events[changed].clock, "won", True)
    assert total == pytest.approx(events[changed].rating)
    assert any("changed at codelet" in line.text for line in model.lines())


# -- the chapter 4 state (ANSWERS.md 11; G-039) ------------------------------------------------

def test_the_chapter_4_state_shows_two_correspondences_and_their_pair():
    engine = engine_worldview_tests()
    problems = {p.name: p for p in load_all()}
    world, cup_cup, spoons, cup_glass = engine.fig_4_7(problems)
    model = wm.WorldviewModel()
    model.update(take_snapshot(world), None, worldview_readings(world))
    worldview, shadow = model.entries()
    assert [e.name for e in worldview] == [cup_cup.name, spoons.name] == ["cp1", "cp2"]
    assert [e.name for e in shadow] == [cup_glass.name] == ["cp3"]
    assert worldview[0].incoherent == ("cp2",) and worldview[1].incoherent == ("cp1",)
    assert worldview[0].parallel == ("cp2",) and worldview[0].coherent == ()
    assert shadow[0].rivals == ("cp1",) and shadow[0].status == "tentative, not tested yet"
    assert (worldview[0].henry, worldview[0].eliza) == ("cup1", "cup2")
    assert model.is_group(spoons.henry) and model.is_group(spoons.eliza)
    r = model.rating()
    current = WV.current(world)
    assert r.incoherent_pairs == (("cp1", "cp2"),) and r.parallel_pairs == (("cp1", "cp2"),)
    assert not r.coherent and r.total == pytest.approx(current.total)
    assert r.coherence == pytest.approx(-STANDARD.incoherence_penalty)
    assert r.parallelism == pytest.approx(STANDARD.parallel_points)
    lines = model.lines()
    assert lines[0].text == "2 correspondences; 2 pairs rated"
    roles = {line.name: line.role for line in lines if line.name}
    assert roles["cp1"] == wm.WARNING and roles["cp3"] == wm.DETAIL
    assert [line.role for line in lines if line.text.startswith("cp")] == [
        wm.WORLDVIEW, wm.WORLDVIEW, wm.SHADOW]
    assert "(groups)" in next(line.text for line in lines if line.text.startswith("cp2"))
    warning = next(line for line in lines if line.text.startswith("incoherent pairs"))
    assert warning.role == wm.WARNING and warning.text == "incoherent pairs: cp1 - cp2"
    assert "rival of cp1" in model.tooltip("cp3") and "rival" not in model.tooltip("cp1")
    assert model.selection_of("cp3") == ("correspondence", "cp3", ("cup1", "small-glass1"))
    assert model.proposals() == [] and model.banner() == "2 correspondences; 2 pairs rated"


# -- the competitions (G-041) ------------------------------------------------------------------

@pytest.mark.parametrize("case", [QUICK, FIGHTS])
def test_one_entry_per_competition_held_equal_to_the_snapshots_logs(logs, case):
    log = logs[case]
    events = log.events
    for index in log.snapshots.indexes:
        snap = log.state_at(index)
        model = fights_fed(events, index)
        assert model.snapshot_competitions() == list(snap.competitions)
        assert model.changes == list(snap.changes)
        assert list(model.worldview) == list(snap.worldview["names"])
    end = fights_fed(events)
    held = [e for e in events if isinstance(e, ev.CompetitionHeld)]
    assert len(end.fights) == len(held) >= 1
    for fight, e in zip(end.fights, held):
        assert (fight.newcomer, fight.rivals, fight.rival_wins) == (e.newcomer, e.rivals, e.rival_wins)
        assert fight.winner == ("entered without a fight" if not e.rivals
                                else "the Rival Worldview won" if e.rival_wins
                                else "the Old Worldview held")


def test_the_fights_run_old_and_rival_worldviews_and_the_fresh_mark(logs):
    events = logs[FIGHTS].events
    end = fights_fed(events)
    first, second, third = end.fights
    assert not first.fought and first.old == () and first.rival == ("cp3",)
    assert first.chance == 1.0 and first.change is None
    # The second goes to the newcomer cp2 (99.6 against cp3's 76.8, chance 0.659), and its
    # change comes five events after the fight; the Old Worldview (cp2 now) holds the third
    # (105.4 against cp3's 69.1, chance 0.243). (Since v5's fix (D-116 to D-120); at item
    # 09 the Old Worldview (cp3) held the second, 61.1 against cp7's 85.7 at chance 0.653,
    # and the third, 66.5 against cp2's 83.9 at 0.634, and the fourth went to cp2, 75.4
    # against cp3's 71.3 at 0.545; at item 08 seed 22's second went to the newcomer and the
    # Old Worldview held the third and the fourth; before item 08 the Old Worldview held
    # the second and the third and the fourth went to the newcomer; before item 07 it held
    # the second and the fourth.)
    assert second.old == ("cp3",) and second.rival == ("cp2",) and second.rival_wins
    assert second.fought and second.chance == pytest.approx(0.659, abs=0.0005)
    assert isinstance(events[818], ev.WorldviewChanged) and events[818].added == ("cp2",)
    assert second.change == pytest.approx(events[818].rating)
    assert third.old == ("cp2",) and third.rival == ("cp3",) and not third.rival_wins
    assert third.fought and third.chance == pytest.approx(0.243, abs=0.0005)
    assert third.change is None
    assert end.banner() == "3 competitions: 2 fights, 1 entered without a fight"
    at_fight = fights_fed(events, second.index)
    assert at_fight.fresh is at_fight.fights[-1]
    assert all(line.fresh for line in at_fight.lines() if line.name == f"competition {second.index}")
    assert not any(line.fresh for line in fights_fed(events, second.index + 1).lines())
    assert not any(line.fresh for line in end.lines())
    cells = [line.cells for line in end.lines() if line.cells]
    assert (("Old Worldview", wm.HEADING), ("Rival Worldview", wm.HEADING)) in cells
    assert cells.count((("Old Worldview", wm.HEADING), ("Rival Worldview", wm.HEADING))) == 2
    assert (("cp3: cup1 → spoon4", wm.WORLDVIEW),
            ("cp2: cup2 → cup1", wm.SHADOW)) in cells                  # the second
    assert (("cp2: cup2 → cup1", wm.WORLDVIEW),
            ("cp3: cup1 → spoon4", wm.SHADOW)) in cells                # the third
    assert cells.count((("cp3: cup1 → spoon4", wm.WORLDVIEW), ("cp2: cup2 → cup1", wm.SHADOW))) == 1
    text = end.tooltip(f"competition {second.index}")
    assert text.startswith("competition at codelet 71: cp2 against cp3")
    assert "chance 0.659 at a 2.52: the Rival Worldview won" in text
    assert end.selection_of(f"competition {second.index}") == (
        "correspondence", "cp2", ("cup2", "cup1"))
    held_text = end.tooltip(f"competition {third.index}")
    assert held_text.startswith("competition at codelet 76: cp3 against cp2")
    assert "chance 0.243 at a 2.70: the Old Worldview held" in held_text
    assert end.tooltip("nothing") == "" and end.selection_of("nothing") is None


def test_a_fight_the_old_worldview_holds_has_no_change_and_the_newcomer_loses():
    # HELD (`ch2-fig13` seed 4, chosen because its loser wins later, on a problem other
    # than FIGHTS'): the Old Worldview (cp4, rated 76.7) holds cp6 (rated 72.0) off at
    # codelet 62 (chance 0.453) and again at 67 (81.7 against 63.3, chance 0.284), with no
    # change, and the same cp6 wins at codelet 68 (82.1 against 62.7, chance 0.254) for
    # good. Since v5's fix (D-116 to D-120): from item 09 to v4 the run was `ch2-fig13`
    # seed 293, whose Old Worldview (cp1, rated 65.4) held cp2 (rated 82.4) off at codelet
    # 40 (chance 0.571) and whose cp2 won at 55 (chance 0.541); since v5 its one fight is
    # won at once, never held (FIGHTS' cp2 was held off at 70 and won at 80 too then;
    # `surround-05` seed 10, the HELD run before item 05, has no such sequel since). From
    # item 08 to item 09 the run was `ch2-fig13` seed 27: cp1 (62.3) held cp2 (74.6) off at 57 (chance
    # 0.516) and cp2 won at 79 (0.483); since item 09's runs seed 27's two held-off
    # newcomers (cp4 at 68, cp5 at 69) never win. Until item 08 the run was `ch2-fig13` seed
    # 28: cp1 (83.2) held cp3 (83.9) off at 65 (chance 0.506), cp3 won at 66 (0.374), cp1
    # won back at 69 and cp3 at 73 for good; since item 08 seed 28's newcomer wins at once,
    # never held off. Before item 07 seed 28 held cp2 off at codelets 35 (chance 0.336) and
    # 42 and cp2 won at 47 (0.049).
    events = plain_run(*HELD, V2).events
    end = fights_fed(events)
    assert [(f.clock, f.newcomer, f.rivals, f.rival_wins) for f in end.fights] == [
        (40, "cp4", (), True), (62, "cp6", ("cp4",), False), (67, "cp6", ("cp4",), False),
        (68, "cp6", ("cp4",), True)]
    held, again, won = end.fights[1], end.fights[2], end.fights[3]
    assert held.change is None and held.old == ("cp4",) and held.rival == ("cp6",)
    assert held.chance == pytest.approx(0.453, abs=0.0005)
    assert held.old_rating > held.rival_rating
    assert again.change is None and again.old == ("cp4",) and again.rival == ("cp6",)
    assert again.chance == pytest.approx(0.284, abs=0.0005)
    assert won.change == pytest.approx(events[745].rating)
    assert isinstance(events[745], ev.WorldviewChanged) and events[745].added == ("cp6",)
    assert won.chance == pytest.approx(0.254, abs=0.0005)
    assert won.old_rating > won.rival_rating          # won though rated lower: a coin toss
    assert won.old == ("cp4",) and won.rival == ("cp6",) and won.rival_wins
    assert end.banner() == "4 competitions: 3 fights, 1 entered without a fight"
    text = end.tooltip(f"competition {held.index}")
    assert text.startswith("competition at codelet 62: cp6 against cp4")
    assert "the Old Worldview held" in text
    after = fed(events, 700)                  # between the hold (index 662) and the win (742)
    assert [e.name for e in after.entries()[0]] == ["cp4"]
    assert events[700].clock == 65 and "cp6" in [e.name for e in after.entries()[1]]


# -- the banners and the plain words (ANSWERS.md 11; G-040, G-041) -------------------------------

def test_both_empty_banners_and_the_plain_words_for_one_correspondence(logs):
    events = logs[QUICK].events
    assert wm.WorldviewModel().banner() == "no run yet"
    assert cm.CompetitionsModel().banner() == "no run yet"
    assert wm.WorldviewModel().lines()[0].text == "no run yet"
    assert wm.WorldviewModel().proposal_lines()[0].text == "no run yet"
    first_held = next(i for i, e in enumerate(events) if isinstance(e, ev.CompetitionHeld))
    first_built = next(i for i, e in enumerate(events) if isinstance(e, ev.CorrespondenceBuilt))
    for index in (0, first_built - 1, first_held - 2):
        fights = fights_fed(events, index)
        assert fights.banner() == cm.NO_COMPETITION == "no competition in this run"
        assert fights.lines() == [wm.TextLine(cm.NO_COMPETITION, wm.MUTED)]
    before = fed(events, first_built - 1, logs[QUICK])
    assert before.banner() == "no correspondence in the Worldview yet"
    assert before.lines()[-1].text == "none built, or all removed"
    built = fed(events, first_built, logs[QUICK])
    assert built.banner() == "no correspondence in the Worldview yet; 1 outside it"
    assert not built.entries()[1][0].dated      # its points are the first evaluation's
    assert "(first evaluation)" in built.lines()[-2].text
    end = fed(events, None, logs[QUICK])
    assert end.banner() == "one correspondence; no pairs to rate"
    assert end.ended and [e.name for e in end.entries()[0]] == ["cp2"]   # which beat cp1 at
    # codelet 40 (since v5's fix (D-116 to D-120); cp1 alone, never fought, until v4)
    assert [e.name for e in end.entries()[1]] == ["cp1"]
    assert fights_fed(events).banner() == "2 competitions: 1 fight, 1 entered without a fight"
    assert fights_fed(events).lines()[1].text.startswith("1. codelet 18: cp1 entered without a fight")
    assert fights_fed(events).lines()[2].text.startswith("2. codelet 40: cp2 against cp1")


def test_a_new_run_forgets_and_an_older_snapshot_never_rolls_the_numbers_back(logs):
    log = logs[FIGHTS]
    model = fed(log.events, None, log)
    older = log.snapshots.indexes[5]
    model.update(log.state_at(older), log.terms_at(older), log.worldview_at(older))
    assert model.snapshot_clock == log.state_at(len(log.events) - 1).clock
    model.on_event(logs[QUICK].events[0])
    assert model.entries() == ([], []) and model.changes == [] and model.rating_now is None
    fights = fights_fed(log.events)
    fights.on_event(logs[QUICK].events[0])
    assert fights.fights == [] and fights.banner() == cm.NO_COMPETITION


# -- the proposals (D-024; G-039) -------------------------------------------------------------

def test_the_proposals_are_listed_with_their_stages(logs):
    log = logs[PROPOSALS]
    events = log.events
    for index in log.snapshots.indexes:
        snap = log.state_at(index)
        listed = fed(events, index).proposals()
        assert [(p.name, p.members, p.stage) for p in listed] == [
            (p["name"], p["members"], p["stage"]) for p in snap.proposals]
        for p, s in zip(listed, snap.proposals):
            assert p.built == (s["group"] is not None) and p.group == (s["group"] or "")
    end = fed(events).proposals()
    names = ["p1", "p2", "p3"]
    assert [p.name for p in end] == names
    assert all(p.stage < 3 and not p.built for p in end)
    assert [p.stage for p in end] == [1, 0, 0]       # p1 (knife1 cup1, mixed) passed stage 0 at codelet 38
    lines = fed(events).proposal_lines()
    assert lines[0].text.startswith("3 proposals (D-024")
    assert [line.name for line in lines[1:]] == names
    assert "stage" in lines[1].text and lines[1].text.startswith("p1: ")
    assert lines[1].text.endswith("stage 1 of 3 passed")
    assert lines[2].text.endswith("stage 0 of 3 passed")
    assert [line.name for line in fed(logs[FIGHTS].events).proposal_lines()[1:]] == ["p1"]
    # The quick run proposes p1 (fork1 fork2, codelet 21, never past stage 0) since v5's fix
    # (D-116 to D-120); it proposed no group from item 08 to v4 (p1 and p2 at codelets 12
    # and 47 from item 05 to item 07, none before item 05). NO_PROPOSALS proposes none.
    quick = fed(logs[QUICK].events).proposal_lines()
    assert quick[0].text.startswith("1 proposal (D-024") and [l.name for l in quick[1:]] == ["p1"]
    assert quick[1].text.endswith("stage 0 of 3 passed")
    none = [wm.TextLine("no bottom-up group proposed in this run", wm.HEADING)]
    assert fed(plain_run(*NO_PROPOSALS, V2).events).proposal_lines() == none
    assert fed(events).tooltip("p1").startswith("p1: proposed")
    assert fed(events).selection_of("p1")[:2] == ("proposal", "p1")


def test_the_text_helpers():
    assert wm.arrow_text("cup1", "cup2") == "cup1 → cup2"
    assert wm.pair_text(()) == "none" and wm.pair_text((("a", "b"), ("c", "d"))) == "a - b, c - d"
    assert wm.slippage_text((("cup", "cup", "sameness"), ("object", "object", ""))) == (
        "cup to cup (sameness), object to object")
    assert wm.slippage_text(()) == "none"
