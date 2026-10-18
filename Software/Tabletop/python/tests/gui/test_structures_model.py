"""The table's structures (item 04; `tabletop/gui/models/structures.py` through
`models/table.py`; G-020, G-021, G-023), Qt-free:

- at every snapshot index of two runs (`surround-05` seed 3, the quick start, and
  `surround-04` seed 187, which holds two fights and a Worldview change; item 05 of the
  roles loop left `blockage-06` seed 2 one fight, item 07 re-pinned every run fact, item
  08 left `surround-04` seed 69 one competition without a fight, item 09 left seed 22
  one and v5's fix left seed 187 two fights of its three), the groups, proposals and
  correspondences built from the events equal the snapshot's;
- the flash phase depends only on the event index;
- the pure style functions;
- a new correspondence's ends flash for two codelets, a removed one fades for three and
  then goes, the incumbent and the newcomer are competing at the fight's codelet, and the
  newcomer is the loser when the Old Worldview holds (`ch2-fig13` seed 4);
- after `RunEnded` the end state stays drawn: the Worldview line and the groups;
- the geometry: boxes around the members, the wavy outline within its amplitude;
- the hover texts and what a click selects.
"""

import pytest

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop.config import PROFILES
from tabletop.gui.models import structures as st
from tabletop.gui.models import table as tm
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.tap import Tap
from tabletop.problems import load_problem
from tabletop.world import World

QUICK = ("surround-05", 3)       # Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6.
FIGHTS = ("surround-04", 187)    # two fights: cp2 beats cp3 (71), then cp3 comes back and cp2 holds it off (76)
HELD = ("ch2-fig13", 4)          # fights the Old Worldview holds (codelets 62 and 67), then one it loses (68)
# Item 05 (the roles loop) moved both phenomena: blockage-06 seed 2, the FIGHTS run until
# then, now holds one fight, and surround-05 seed 10's held-off newcomer no longer wins later.
# Item 07 (ROW and COLUMN roles, the re-cast) changed every run: the same seeds still showed
# the phenomena, with other correspondences at other codelets. Item 08 (GAP, LONER, STRAY
# and CORNER roles; the midline on tables of loners) changed every run again: surround-04
# seed 69, the FIGHTS run until then, holds one competition and no fight, so FIGHTS moved
# to seed 22; ch2-fig13 seed 28's newcomer wins at once, never held off, so HELD moved to
# seed 27; and the quick run (44 codelets now) builds two groups, two correspondences and
# no proposal (re-pinned below). Item 09 (FIGURE, GROUND, ECHO, TWIN and TYPE_KEY roles)
# changed every run but the quick one: surround-04 seed 22 holds one competition and no
# fight, so FIGHTS moved to seed 187 (84 codelets: cp3 enters alone at 59, holds cp7 off at
# 69 and cp2 at 70, and cp2 beats it at 80), and ch2-fig13 seed 27's two held-off newcomers
# never win, so HELD moved to seed 293. Since v5's fix (D-116 to D-120) every run changed
# again: surround-04 seed 187 keeps a won fight, a held one and its proposal (85 codelets:
# cp3 enters alone at 59, cp2 beats it at 71, and cp3 comes back at 76 and is held off), so
# FIGHTS stays; ch2-fig13 seed 293's one fight is won at once, never held, so HELD moved to
# seed 4 (cp4 enters alone at 40, holds cp6 off at 62 and 67, and cp6 beats it at 68); and
# the quick run (59 codelets) answers spoon3 through cp2, which beats cp1 at 40, and
# proposes p1 (re-pinned below).
STANDARD = V2_PROFILES["standard"]


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
    return {case: tapped(*case) for case in (QUICK, FIGHTS)}


def fed(problem, events, upto=None, log=None):
    """A model given the problem and the events up to `upto` (inclusive), with the
    snapshot in force and its terms when a log is given."""
    model = tm.TableModel()
    model.set_problem(load_problem(problem))
    last = len(events) - 1 if upto is None else upto
    for i, event in enumerate(events[:last + 1]):
        model.on_event(event)
    if log is not None:
        model.update(log.state_at(last), log.terms_at(last))
    return model


def clock_of(events, index):
    return events[index].clock


def first_index_at_clock(events, clock):
    return next(i for i, e in enumerate(events) if e.clock >= clock)


# -- the structures equal the snapshot's at every snapshot index (G-020) ---------------------

@pytest.mark.parametrize("case", [QUICK, FIGHTS])
def test_the_structures_equal_the_snapshots(logs, case):
    log = logs[case]
    events = log.events
    checked = 0
    for index in log.snapshots.indexes:
        snap = log.state_at(index)
        model = fed(case[0], events, index).structures
        groups = {g.name: (set(g.members), g.kind, g.parent or "") for g in model.live_groups()}
        assert groups == {g["name"]: (set(g["members"]), g["kind"], g["parent"] or "")
                          for g in snap.groups}, index
        proposals = {p.name: (set(p.members), p.kind, p.stage,
                              p.failed) for p in model.proposals.values()}
        assert proposals == {p["name"]: (set(p["members"]), p["kind"], p["stage"],
                                         bool(p["results"]) and not p["results"][-1][2])
                             for p in snap.proposals}, index
        cps = {c.name: (c.henry, c.eliza, c.level, c.tested, c.passed, c.in_worldview,
                        c.losses > 0 and not c.in_worldview, c.wins, c.losses,
                        tuple(tuple(s) for s in c.slippages))
               for c in model.live_correspondences()}
        assert cps == {c["name"]: (c["henry"], c["eliza"], c["level"], c["tested"], c["passed"],
                                   c["in_worldview"], c["loser"], c["wins"], c["losses"],
                                   tuple(tuple(s) for s in c["slippages"]))
                       for c in snap.correspondences}, index
        # What is drawn, fading structures aside, is the snapshot's.
        lay = fed(case[0], events, index).layout()
        assert {g.name for g in lay.groups if not g.fading} == set(groups)
        assert {c.name for c in lay.correspondences if not c.fading} == set(cps)
        assert {p.name for p in lay.proposals if not p.fading} == {
            n for n, (_, _, stage, failed) in proposals.items() if stage < st.STAGES and not failed}
        checked += 1
    assert checked == len(log.snapshots) > 40


def test_the_fights_run_holds_competitions_and_a_change(logs):
    events = logs[FIGHTS].events
    held = [e for e in events if isinstance(e, ev.CompetitionHeld) and e.rivals]
    assert len(held) == 2
    changes = [e for e in events if isinstance(e, ev.WorldviewChanged)]
    assert [c.reason for c in changes] == ["no rival", "won"]
    end = fed(FIGHTS[0], events).structures
    assert [c.name for c in end.live_correspondences() if c.in_worldview] == ["cp2"]
    cp2 = end.correspondences["cp2"]        # beat cp3 at codelet 71, held it off at 76
    assert cp2.wins == 2 and cp2.losses == 0 and cp2.in_worldview and cp2.removed_at is None
    cp3 = end.correspondences["cp3"]        # entered without a fight (codelet 59), which awards
    # nothing; lost to cp2 at 71 and was held off by it at 76
    assert cp3.wins == 0 and cp3.losses == 2 and not cp3.in_worldview and cp3.removed_at is None
    cp7 = end.correspondences["cp7"]        # spoon4=spoon1 (codelet 68), tested, never competed
    assert cp7.wins == 0 and cp7.losses == 0 and not cp7.in_worldview and cp7.removed_at is None


# -- the flash phase and the styles (G-021) -------------------------------------------------------

def test_the_flash_phase_depends_only_on_the_event_index(logs):
    for index in range(0, 60):
        assert st.flash_phase(index) == index % 2
    quick, fights = logs[QUICK].events, logs[FIGHTS].events
    for index in (0, 7, 141, 142, 200, 301, 400):
        a = fed(QUICK[0], quick, index).layout()
        b = fed(FIGHTS[0], fights, index).layout()
        assert a.phase == b.phase == index % 2
        for line in a.correspondences + b.correspondences:
            assert line.phase == index % 2
    # Fed again from the start, the same index gives the same picture.
    assert fed(QUICK[0], quick, 300).layout() == fed(QUICK[0], quick, 300).layout()


def line(**kw):
    base = dict(name="cp1", henry="cup2", eliza="cup1", a=(0.0, 0.0), b=(1.0, 1.0), level="top",
                in_worldview=False, group_level=False)
    base.update(kw)
    return st.CorrespondenceLine(**base)


def test_the_line_styles():
    worldview = st.line_style(line(in_worldview=True, phase=1))
    assert not worldview.dashed and worldview.phase == 0 and worldview.alpha == 255
    tentative = st.line_style(line(tentative=True, phase=1))
    assert tentative.dashed and tentative.phase == 1 and tentative.alpha == 255
    assert st.line_style(line(phase=0)).phase == 0
    assert st.line_style(line(group_level=True)).width > st.line_style(line()).width
    loser = st.line_style(line(loser=True))
    fading = st.line_style(line(loser=True, fading=True))
    assert 255 > loser.alpha > fading.alpha > 0


def test_the_box_styles():
    box = (0.0, 0.0, 10.0, 5.0)
    gestalt = st.GroupBox("g1", "gestalt", "", ("a",), "", box, wavy=True)
    sub = st.GroupBox("g3", "sameness", "sameness", ("a",), "g1", box, wavy=False)
    gone = st.GroupBox("g3", "sameness", "sameness", ("a",), "g1", box, wavy=False, fading=True)
    assert st.box_style(gestalt).border == "wavy"
    assert st.box_style(sub).border == "dashed"
    assert st.box_style(gone).alpha < st.box_style(sub).alpha
    proposal = st.ProposalBox("p1", ("a", "b"), "mixed", "", box, 0)
    failed = st.ProposalBox("p1", ("a", "b"), "mixed", "", box, 1, failed=True, fading=True)
    assert st.box_style(proposal).border == "dotted"
    assert st.box_style(proposal).alpha < st.box_style(sub).alpha
    assert st.box_style(failed).alpha < st.box_style(proposal).alpha


# -- new ends, fading, competing (G-021) -----------------------------------------------------------

def test_a_new_correspondence_flashes_its_ends_for_two_codelets(logs):
    # cp1 (cup2=cup1), built at codelet 9, is the run's first correspondence, and it passes
    # its test and stays (since item 08; before it cp2, small-glass1=cup1 at codelet 28, was
    # the first to stay and cp1, cup2=cup1 at codelet 22, failed its test and went at once).
    events = logs[QUICK].events
    built = next(i for i, e in enumerate(events)
                 if isinstance(e, ev.CorrespondenceBuilt) and e.correspondence == "cp1")
    clock = clock_of(events, built)
    assert (events[built].henry, events[built].eliza, clock) == ("cup2", "cup1", 9)

    def cp1_at(index):
        return fed(QUICK[0], events, index).layout().structure("cp1")

    at_build = cp1_at(built)
    assert at_build.new and at_build.tentative and not at_build.in_worldview
    assert at_build.end_boxes[0] == fed(QUICK[0], events, built).layout().object("cup2").box
    assert cp1_at(first_index_at_clock(events, clock + 1)).new
    later = cp1_at(first_index_at_clock(events, clock + st.NEW_CODELETS))
    assert not later.new and not later.tentative
    assert cp1_at(built - 1) is None


def test_a_removed_correspondence_fades_and_then_goes(logs):
    events = logs[FIGHTS].events
    removed = next(i for i, e in enumerate(events) if isinstance(e, ev.CorrespondenceRemoved))
    name, clock = events[removed].correspondence, clock_of(events, removed)
    assert (name, clock) == ("cp1", 24)       # spoon4=cup1, failing its test (since item 09;
    # seed 22's cp3 g2=g1 at codelet 69 at item 08, cp1 spoon4=cup1 at codelet 23 before
    # item 08, cp3 spoon2=g1 at 50 before item 07)

    def cp_at(index):
        return fed(FIGHTS[0], events, index).layout().structure(name)

    assert cp_at(removed - 1) is not None and not cp_at(removed - 1).fading
    assert cp_at(removed).fading and st.line_style(cp_at(removed)).alpha == 60
    assert cp_at(first_index_at_clock(events, clock + st.FADE_CODELETS - 1)).fading
    assert cp_at(first_index_at_clock(events, clock + st.FADE_CODELETS)) is None


def test_the_incumbent_and_the_newcomer_compete_at_the_fight(logs):
    # The first fight the newcomer wins: codelet 71, cp2 against cp3 (the fight at 76, which
    # cp2 then holds, is the HELD case's branch; since v5's fix (D-116 to D-120): at item 09
    # cp2 beat cp3 at 80 after cp3 had held cp7 off at 69 and cp2 at 70, at item 08 seed
    # 22's cp2 beat cp1 at 91 and then held two fights, before item 08 cp6 beat cp5 at 90
    # after cp5 had held two fights, before item 07 cp2 beat cp1 at 49).
    events = logs[FIGHTS].events
    fight = next(i for i, e in enumerate(events)
                 if isinstance(e, ev.CompetitionHeld) and e.rivals and e.rival_wins)
    e = events[fight]
    assert (e.clock, e.newcomer, e.rivals) == (71, "cp2", ("cp3",))
    lay = fed(FIGHTS[0], events, fight).layout()
    assert lay.structure("cp2").competing and lay.structure("cp3").competing
    assert lay.structure("cp3").in_worldview and not lay.structure("cp2").in_worldview
    assert st.line_style(lay.structure("cp3")).dashed is False
    after = fed(FIGHTS[0], events, first_index_at_clock(events, e.clock + 1)).layout()
    assert not after.structure("cp2").competing and not after.structure("cp3").competing
    assert after.structure("cp3").loser and not after.structure("cp3").in_worldview
    assert after.structure("cp2").in_worldview and not after.structure("cp2").loser
    assert "competing with cp3 at codelet" in fed(FIGHTS[0], events, fight).tooltip("cp2")


def test_the_newcomer_is_the_loser_when_the_old_worldview_holds():
    # HELD (`ch2-fig13` seed 4, chosen because its loser wins later, on a problem other
    # than FIGHTS'): the Old Worldview holds cp6 off at codelet 62 (cp6 against cp4) and
    # again at 67, and the same cp6 wins at codelet 68 for good. Since v5's fix (D-116 to
    # D-120): from item 09 to v4 the run was `ch2-fig13` seed 293, which held cp2 off at 40
    # and let it win at 55 (FIGHTS' cp2 was held off at 70 and won at 80 too then; since
    # v5 seed 293's one fight is won at once, never held); since item 05 `surround-05`
    # seed 10, the HELD run until then, has no such sequel.
    # From item 08 to item 09 the run was `ch2-fig13` seed 27, which
    # held cp2 off at 57 and let it win at 79; since item 09's runs seed 27's two held-off
    # newcomers (cp4 at 68, cp5 at 69) never win. Until item 08 the run was `ch2-fig13`
    # seed 28, which held cp3 off at 65 and let it win at 66 (cp1 back at 69, cp3 for good
    # at 73); since item 08 seed 28's newcomer wins at once, never held off. Before item 07
    # seed 28 held cp2 off at 35 and 42 and cp2 won at 47.
    events = plain_run(*HELD, V2).events
    held = next(i for i, e in enumerate(events)
                if isinstance(e, ev.CompetitionHeld) and e.rivals and not e.rival_wins)
    e = events[held]
    assert (e.clock, e.newcomer, e.rivals) == (62, "cp6", ("cp4",))
    lay = fed(HELD[0], events, held).layout()
    assert lay.structure("cp6").competing and lay.structure("cp4").competing
    assert lay.structure("cp4").in_worldview and not lay.structure("cp6").in_worldview
    after = fed(HELD[0], events, first_index_at_clock(events, e.clock + 1)).layout()
    assert not after.structure("cp6").competing and not after.structure("cp4").competing
    assert after.structure("cp6").loser and not after.structure("cp6").in_worldview
    assert after.structure("cp4").in_worldview and not after.structure("cp4").loser
    end = fed(HELD[0], events).layout()
    assert end.structure("cp6").in_worldview and not end.structure("cp6").loser
    assert end.structure("cp4").loser and not end.structure("cp4").in_worldview


# -- the end state and the geometry (G-021) -----------------------------------------------------

def test_the_end_state_stays_drawn_after_run_ended(logs):
    events = logs[QUICK].events
    assert isinstance(events[-1], ev.RunEnded)
    lay = fed(QUICK[0], events, log=logs[QUICK]).layout()
    cp2 = lay.structure("cp2")              # spoon3=cup1, the Worldview's only entry (since
    # v5's fix (D-116 to D-120); cp1, cup2=cup1, held it until v4)
    assert cp2.in_worldview and not cp2.fading and not st.line_style(cp2).dashed
    assert {cp2.henry, cp2.eliza} == {"spoon3", "cup1"}
    assert cp2.a == st.centre_of(lay.object("spoon3").box)
    assert cp2.b == st.centre_of(lay.object("cup1").box)
    assert [g.name for g in lay.groups] == ["g1", "g2"] and all(g.wavy for g in lay.groups)
    # cp1 (cup2=cup1) lost to cp2 at codelet 40 and is still drawn, a loser; cp3 (g2=fork1),
    # the run's only other correspondence, failed its test at codelet 56 and has faded and
    # gone by the end (codelet 59, past FADE_CODELETS): cp1 and cp2 are the lines.
    assert {c.name for c in lay.correspondences if not c.fading} == {"cp1", "cp2"}
    assert lay.structure("cp1").loser and not lay.structure("cp1").in_worldview
    assert [c.name for c in lay.correspondences if c.fading] == []
    assert lay.answer is not None and lay.answer.object == "spoon3"
    # The quick run's p1 (codelet 21, fork1 fork2, never past stage 0) is drawn after its
    # RunEnded (since v5; the run proposed no group from item 08 to v4, and until item 08
    # its p1 and p2, proposed at codelets 12 and 47 and never past stage 0, were drawn
    # after its RunEnded); so is the fights run's p1 (codelet 36, spoon4 small-glass1,
    # past stage 0). Nothing fades at its end: its last removal, cp9 (spoon2=g1) at
    # codelet 81, has gone by codelet 85 (cp10 at 77, gone by 84, since item 09; seed 22's
    # p1 to p4 and its cp10, removed at codelet 102 and still fading at the end, codelet
    # 104, within FADE_CODELETS, were the case at item 08).
    assert [p.name for p in lay.proposals] == ["p1"]
    assert [p.stage for p in lay.proposals] == [0] and not lay.proposals[0].failed
    fights = logs[FIGHTS].events
    assert isinstance(fights[-1], ev.RunEnded)
    end = fed(FIGHTS[0], fights, log=logs[FIGHTS]).layout()
    assert [p.name for p in end.proposals] == ["p1"]
    assert [p.stage for p in end.proposals] == [1] and not any(p.failed for p in end.proposals)
    assert [g.name for g in end.groups] == ["g1", "g2"] and end.answer.object == "cup2"
    assert {c.name for c in end.correspondences if not c.fading} == {
        "cp2", "cp3", "cp4", "cp7", "cp8", "cp10"}
    assert [c.name for c in end.correspondences if c.fading] == []


def test_the_boxes_surround_their_members(logs):
    # The quick run's end has two groups and one proposal, the fights run's two and one.
    forms = 0
    for case in (QUICK, FIGHTS):
        lay = fed(case[0], logs[case].events).layout()
        for form, pad in [(g, st.GESTALT_PAD) for g in lay.groups] + \
                [(p, st.PROPOSAL_PAD) for p in lay.proposals]:
            boxes = [lay.object(m).box for m in form.members]
            assert form.box == st.union_box(boxes, pad)
            for left, top, right, bottom in boxes:
                assert form.box[0] < left and form.box[1] < top
                assert form.box[2] > right and form.box[3] > bottom
            forms += 1
    assert forms == 6
    assert st.union_box([(1.0, 2.0, 3.0, 4.0), (0.0, 5.0, 2.0, 6.0)], 0.5) == (-0.5, 1.5, 3.5, 6.5)


def test_the_wavy_outline_stays_within_its_amplitude():
    box = (10.0, 20.0, 40.0, 32.0)
    amplitude = 0.6
    points = st.wavy_outline(box, amplitude=amplitude)
    assert len(points) >= 4 * 8
    left, top, right, bottom = box
    for x, y in points:
        inside_outer = left - amplitude - 1e-9 <= x <= right + amplitude + 1e-9 and \
            top - amplitude - 1e-9 <= y <= bottom + amplitude + 1e-9
        on_edge = (min(abs(x - left), abs(x - right)) <= amplitude + 1e-9
                   or min(abs(y - top), abs(y - bottom)) <= amplitude + 1e-9)
        assert inside_outer and on_edge, (x, y)
    xs, ys = [p[0] for p in points], [p[1] for p in points]
    assert min(xs) < left + 1e-9 and max(xs) > right - 1e-9
    assert min(ys) < top + 1e-9 and max(ys) > bottom - 1e-9


# -- the hover texts and the selection (G-022, G-023) -----------------------------------------------

def test_the_hover_texts(logs):
    log = logs[QUICK]
    model = fed(QUICK[0], log.events, log=log)
    text = model.tooltip("cp1")
    assert text.startswith("cp1: cup2 (Henry's end) to cup1 (Eliza's end), level top\n")
    assert "\nlost a competition, outside the Worldview\n" in text
    assert "(as of codelet 59)" in text
    assert "static terms: salience " in text and text.count(", ") >= 11
    assert "dynamic terms: hobj " in text
    assert "slippages: object to object, cup to cup (sameness), " \
           "one to one (sameness)" in text
    assert text.endswith("wins 0, losses 1")        # entered without a fight at codelet 18,
    # lost to cp2 at 40 (since v5's fix (D-116 to D-120); in the Worldview, wins 0, losses
    # 0, until v4)
    static_line = next(l for l in text.splitlines() if l.startswith("static terms"))
    assert [t.split()[0] for t in static_line[len("static terms: "):].split(", ")] == [
        "salience", "kind", "end_middle", "position", "type", "number", "isa",
        "same_members", "orientation", "role"]         # French's nine, then item 05's role
    group = model.tooltip("g1")
    assert group.startswith("g1: gestalt group of 6: fork1, fork2, knife1, cup1, spoon1, spoon2")
    assert "built at codelet 6" in group
    # The quick run's cp3 (g2 to fork1, a group's end) failed its test at codelet 56 (cp2,
    # cup2 to g1, at codelet 36 until v4).
    assert model.tooltip("cp3").startswith("cp3: g2 (Henry's end) to fork1 (Eliza's end)")
    assert "removed at codelet 56" in model.tooltip("cp3")
    assert model.tooltip("nothing") == ""
    # The fights run: cp3 (cup1 to spoon4) entered without a fight, lost to cp2 at codelet
    # 71 and was held off by it at 76.
    fights = fed(FIGHTS[0], logs[FIGHTS].events, log=logs[FIGHTS])
    cp3 = fights.tooltip("cp3")
    assert cp3.startswith("cp3: cup1 (Henry's end) to spoon4 (Eliza's end), level top\n")
    assert "\nlost a competition, outside the Worldview\n" in cp3
    assert cp3.endswith("wins 0, losses 2")
    # cp7 (spoon4 to spoon1) passed its test at codelet 68 and never competed: tested,
    # outside (cp9, spoon2 to spoon4, was the case until v4; the quick run's cp4, cup2 to
    # cup1, until item 08).
    cp7 = fights.tooltip("cp7")
    assert cp7.startswith("cp7: spoon4 (Henry's end) to spoon1 (Eliza's end), level top\n")
    assert "\ntested, outside the Worldview\n" in cp7
    assert cp7.endswith("wins 0, losses 0")
    # The proposal's text is the fights run's p1 (spoon4 small-glass1, a mixed group,
    # codelet 36, past its first stage).
    proposal = fights.tooltip("p1")
    assert proposal.startswith("p1: proposed mixed group of spoon4, small-glass1\n")
    assert proposal.endswith("1 of 3 stages passed")
    # A group's end (the fights run's cp9, spoon2 to g1, which fails its test at codelet 81;
    # cp10 at 77 until v4).
    assert fights.tooltip("cp9").startswith("cp9: spoon2 (Henry's end) to g1 (Eliza's end)")
    assert "removed at codelet 81" in fights.tooltip("cp9")
    # Without a snapshot the numbers are the build event's, undated (the Hobj points count
    # in the rating since v5, D-116: 86.9 = static 71.9 + dynamic 15.0).
    plain = fed(QUICK[0], log.events)
    assert "as of codelet" not in plain.tooltip("cp1") and "rating 86.9 = " in plain.tooltip("cp1")


def test_what_a_click_selects(logs):
    model = fed(QUICK[0], logs[QUICK].events)
    assert model.selection_of("cup2") == ("object", "cup2", ())
    kind, name, members = model.selection_of("g1")
    assert (kind, name) == ("group", "g1") and len(members) == 6 and "spoon1" in members
    assert model.selection_of("cp1") == ("correspondence", "cp1", ("cup2", "cup1", "cup2", "cup1"))
    # cp3 (g2 to fork1, removed at codelet 56) selects the group's members too.
    assert model.selection_of("cp3")[:2] == ("correspondence", "cp3")
    assert model.selection_of("cp3")[2][:2] == ("g2", "fork1") and "spoon3" in model.selection_of("cp3")[2]
    assert model.selection_of("teapot") is None
    # A proposal selects its members (the fights run's p1).
    fights = fed(FIGHTS[0], logs[FIGHTS].events)
    assert fights.selection_of("p1") == ("proposal", "p1", ("spoon4", "small-glass1"))
    # A correspondence to a group selects the group's members too (the fights run's cp9).
    assert fights.selection_of("cp9")[2][:2] == ("spoon2", "g1")
    assert "spoon3" in fights.selection_of("cp9")[2]


def test_a_model_fed_the_events_alone_draws_the_same_structures(logs):
    """The snapshot only feeds the hover text: with or without it, the layout is the same."""
    log = logs[FIGHTS]
    with_snapshot = fed(FIGHTS[0], log.events, log=log).layout()
    without = fed(FIGHTS[0], log.events).layout()
    assert with_snapshot == without
    assert plain_run(*FIGHTS, V2).report == "Eliza touches cup2 (E2). Structure 105. " \
                                        "Codelets 85. T 34.8."
