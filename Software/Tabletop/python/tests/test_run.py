"""Temperature, find-answer, the run loop and the Result (D-047 to D-050, D-056 to D-063):
the four factors and the formula with its clamps [F95 pp. 90-92], a(T) [F95 p. 92], the
find-answer schedule [F95 p. 94], the answer and the default answer [F95 p. 105], the cap
(D-061), whole seeded runs on every problem, repeatability and the absence of leaks between
runs (D-064).

No result set's frequencies are studied here (the loop's TASK.md, Validation 1): the whole
runs check only that every run ends with an answer and that a run is reproducible.
"""

import pytest

from tabletop import answer as AN
from tabletop import coderack as CR
from tabletop import correspondences as C
from tabletop import problems as P
from tabletop import run as R
from tabletop import temperature as TP
from tabletop import worldview as WV
from tabletop.config import PROFILES, Config
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]


@pytest.fixture(scope="module")
def problems():
    return {p.name: p for p in P.load_all()}


# ---------------------------------------------------------------------------------------------
# The temperature: the four factors and the formula (D-047 to D-050).


def test_the_formula_is_alpha_dq_over_q_plus_beta_dwv_minus_gamma_tcp():
    """T = α ΔQ/Q + β Δ_WV − γ T_cp [F95 p. 91 (img)] (D-047), clamped to [0, 100]."""
    t = TP.Temperature(CFG)
    a, b, g = CFG.temperature_alpha, CFG.temperature_beta, CFG.temperature_gamma
    assert t.formula(q=0.5, dq=0.05, dwv=2, tcp=4) == pytest.approx(a * 0.05 / 0.5 + b * 2 - g * 4)
    assert t.formula(q=1.0, dq=1.0, dwv=0, tcp=0) == 100.0            # clamped above
    assert t.formula(q=1.0, dq=0.0, dwv=0, tcp=1000) == 0.0            # clamped below
    assert t.formula(q=0.0, dq=0.01, dwv=0, tcp=0) == t.formula(q=CFG.q_floor, dq=0.01, dwv=0, tcp=0)


def test_the_three_cases_french_gives():
    """"If ΔQ is small ... and Q is large ... the temperature is low"; a large ΔQ keeps it
    high "Since Q can be at most 1"; a small Q keeps it high [F95 pp. 91-92 (img)]."""
    t = TP.Temperature(CFG)
    assert t.formula(0.9, 0.01, 0, 0) < 10
    assert t.formula(1.0, 0.5, 0, 0) == 100.0
    assert t.formula(0.1, 0.05, 0, 0) > t.formula(0.9, 0.05, 0, 0)


def test_the_rate_is_averaged_in_with_the_past_rates_and_decays_when_nothing_changes():
    """"averaged in with a certain number of past rates of change (generally averaged over
    the last six)" [F95 p. 90]; "If no additional structure is found ... the value of ΔQ
    will decrease over time" [F95 p. 92 (img)] (D-048)."""
    t = TP.Temperature(CFG)
    t.update(0.65, [], [1], "change", 1)
    first = t.dq
    assert first == pytest.approx((0.65 - CFG.q_floor) / CFG.dq_window)
    for i in range(6):
        t.update(0.65, [], [1], "schedule", 5 * (i + 2))
        assert t.dq < first
        first = t.dq
    assert t.dq == pytest.approx((0.65 - CFG.q_floor) / 6 * (5 / 6) ** 6)


def test_absolute_by_default_and_signed_with_the_knob():
    """A loss of quality counts like a gain unless `dq_signed` (D-048)."""
    t = TP.Temperature(CFG)
    t.update(0.8, [], [1], "change", 1)
    t.update(0.4, [], [1, 2], "change", 2)
    assert t.rates[-1] == pytest.approx(0.4)
    s = TP.Temperature(v2(dq_signed=True))
    s.update(0.8, [], [1], "change", 1)
    s.update(0.4, [], [1, 2], "change", 2)
    assert s.rates[-1] == pytest.approx(-0.4)


def test_changes_in_the_last_ten_competitions():
    """Δ_WV "is the number of changes in the Worldview in the last ten competitons" [F95
    p. 91]; every change while fewer than ten have been held (D-048)."""
    t = TP.Temperature(CFG)
    competitions = list(range(1, 16))              # fifteen competitions at clocks 1..15
    changes = [1, 2, 7, 12, 15]
    assert t.changes_in_window(competitions[:5], changes[:2]) == 2
    assert t.changes_in_window(competitions, changes) == 3          # since competition 6
    assert t.changes_in_window([], []) == 0


def test_temperature_holds_at_80_until_the_first_change_and_is_clamped(problems):
    """80.0 is read off the thermometer at two codelets [F95 p. 98 (img)] (D-049); the
    formula applies from the first change on; T stays in [0, 100]."""
    t = TP.Temperature(CFG)
    assert t.value == 80.0
    t.update(0.05, [], [], "schedule", 5)
    t.update(0.05, [1], [], "competition", 6)
    assert t.value == 80.0 and t.last.held
    t.update(0.7, [1, 2], [7], "change", 7)
    assert not t.last.held and 0.0 <= t.value <= 100.0
    assert t.value == pytest.approx(t.formula(t.q, t.dq, 1, 2))


def test_a_fixed_temperature_is_a_lesion():
    """`temperature_fixed` clamps T for the whole run [FCCA p. 365] (D-047)."""
    t = TP.Temperature(v2(temperature_fixed=30.0))
    assert t.value == 30.0
    t.update(0.9, [1, 2, 3], [1, 2, 3], "change", 3)
    assert t.value == 30.0
    assert t.exponent == pytest.approx(TP.exponent_at(30.0, CFG))


def test_the_exponent_follows_the_value():
    """a(T) from 6 at T = 0 to 0.1 at T = 100 [F95 p. 92] (D-060)."""
    t = TP.Temperature(CFG)
    assert t.exponent == pytest.approx(TP.exponent_at(80.0, CFG))
    t.value = 0.0
    assert t.exponent == 6.0


def test_quality_is_the_mean_rating_over_100_floored(problems):
    """Q "is measured as a fraction of an optimally good correspondence" [F95 p. 90] (D-049)."""
    world = World(problems["surround-05"], CFG, 1)
    assert world.quality() == CFG.q_floor
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    cp = C.create(world, "cup1", "cup2")
    WV.admit(world, cp)
    assert world.quality() == pytest.approx(min(1.0, C.evaluate(world, cp) / 100.0))


# ---------------------------------------------------------------------------------------------
# The find-answer schedule (D-056).


def test_find_answers_are_posted_more_often_and_more_urgently_as_t_falls(problems):
    """"The lower the temperature, the more such codelets are hung on the Coderack." [F95
    p. 94]; an early answer is "unlikely, but theoretically possible" [F95 p. 105] (D-056):
    at T = 80 under a tenth of a codelet per cycle at the least urgency, at T = 2 nine or
    ten per cycle near the top of the range."""
    world = World(problems["surround-05"], CFG, 1)
    world.temperature.value = 80.0
    posted = sum(len(world.schedule_find_answer()) for _ in range(200))
    # 0.08 per cycle over 200 cycles: 16 expected, s.d. sqrt(200 x 0.08 x 0.92) = 3.8,
    # four s.d. 15.4; the bound is the four-s.d. band, seeded.
    assert 1 <= posted <= 31
    world.temperature.value = 2.0
    cold = world.schedule_find_answer()
    assert len(cold) in (9, 10) and all(c.raw == 94 for c in cold)
    assert all(c.type == "find-answer" and c.generation == 0 and c.by == CR.ANSWER_SCHEDULE
               for c in cold)
    world.temperature.value = 100.0
    before = world.rng.draws
    assert world.schedule_find_answer() == [] and world.rng.draws == before   # no coin at 0


def test_no_coin_is_spent_when_the_expected_number_is_whole(problems):
    world = World(problems["surround-05"], CFG, 1)
    world.temperature.value = 0.0
    before = world.rng.draws
    assert len(world.schedule_find_answer()) == int(CFG.answer_post_rate)
    assert world.rng.draws == before


# ---------------------------------------------------------------------------------------------
# The answer (D-057, D-058, D-063).


def notice_all(world):
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)


def test_the_default_answer_is_the_touched_object(problems):
    """"the default object corresponding to the touched object is the touched object itself"
    [F95 p. 105] (D-063)."""
    world = World(problems["surround-05"], CFG, 1)
    a = AN.answer_for(world)
    assert (a.object, a.literal, a.through, a.label) == ("cup1", True, AN.DEFAULT, "")


def test_the_answer_is_the_other_end_of_the_touched_objects_worldview_correspondence(problems):
    """"the current object corresponding to the touched object" [F95 p. 105] (D-057); a
    tentative correspondence outside the Worldview does not count."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    cp = C.create(world, "cup1", "small-glass1")
    assert AN.answer_for(world).literal
    WV.admit(world, cp)
    a = AN.answer_for(world)
    assert (a.object, a.label, a.literal, a.through, a.via) == ("small-glass1", "E1", False,
                                                             AN.CORRESPONDENCE, "cp1")
    assert AN.label_for(world.problem, "cup2") == "E2" and AN.label_for(world.problem, "fork1") == ""


def test_a_group_at_the_other_end_answers_with_its_most_salient_member(problems):
    """A group-object correspondence from the touched object (D-057)."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    from tabletop.groups import Group
    g = Group(members=["spoon3", "spoon4"])
    world.workspace.add_group(g)
    cp = C.create(world, "cup1", g.name)
    WV.admit(world, cp)
    a = AN.answer_for(world)
    assert a.object in ("spoon3", "spoon4") and a.through == AN.GROUP and a.via == "cp1"


def test_a_touched_object_mapped_only_through_its_group(problems):
    """Under either value of `group_position_match` the member in the same position
    answers, the first of the other row's middles for the touched cup, a middle of its
    row (D-058 as revised by D-097): with no role tag standing, the `roles` reading falls
    back on the words. (`tests/test_roles_container.py` covers the roles' reading.)"""
    from tabletop import describe as DS
    from tabletop.groups import Group
    for knob in ("words", "roles"):
        world = World(problems["surround-05"], v2(group_position_match=knob), 1)
        notice_all(world)
        rows = []
        for members in (["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"],
                        ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]):
            g = Group(members=members)
            world.workspace.add_group(g)
            frontier = [members[0]]
            while frontier:
                for n, _ in DS.find_neighbours(world, g, frontier.pop(0)):
                    frontier.append(n)
            DS.find_ends(world, g)
            DS.find_middles(world, g)
            rows.append(g)
        cp = C.create(world, rows[0].name, rows[1].name)
        WV.admit(world, cp)
        a = AN.answer_for(world)
        assert a.via == "cp1" and world.workspace.roles == []
        assert a.object == rows[1].middles[0] and "cup1" in rows[0].middles
        assert not a.literal and a.through == AN.GROUP and a.roles == ()
        assert "position" in a.note


# ---------------------------------------------------------------------------------------------
# Whole runs (D-061, D-064).


def runs_to_an_answer(problems: dict, names, capped_ok=()) -> None:
    """Seeded whole runs on the named configurations end with an answer that is an
    object of the problem, within the cap (D-061); a problem in `capped_ok` may reach the
    cap, and then answers as it stands."""
    for name in names:
        problem = problems[name]
        r = R.run_problem(problem, PROFILES["standard"], 1)      # the defaults themselves
        assert r.answer in problem.ids(), name
        assert r.codelets <= CFG.max_codelets and (name in capped_ok or not r.capped), name
        assert r.literal == (r.answer == problem.touched)
        assert 0.0 <= r.temperature <= 100.0 and r.draws > 0
        assert r.profile == "standard" and r.changed == ()


def test_every_problem_runs_to_an_answer_with_one_seed(problems):
    """Every seventh problem here; all 53 are `tests_slow/test_run_slow.py` (ANSWERS.md D2)."""
    runs_to_an_answer(problems, sorted(problems)[::7])


def test_a_run_is_reproducible_from_its_seed_and_differs_between_seeds(problems):
    """Same problem, profile and seed: the same Result and draw count (D-064)."""
    problem = problems["surround-05"]
    first = [R.run_problem(problem, CFG, s) for s in (1, 2, 3)]
    second = [R.run_problem(problem, CFG, s) for s in (1, 2, 3)]
    assert first == second
    assert len({r.draws for r in first}) > 1


def test_two_worlds_share_nothing(problems):
    """A fresh world per run: stepping one does not move the other (D-064)."""
    a = World(problems["surround-05"], CFG, 7)
    b = World(problems["surround-05"], CFG, 7)
    for _ in range(20):
        a.step()
    assert b.clock == 0 and b.rng.draws == 0 and len(b.coderack) == 25
    assert b.workspace.noticed == ["cup1"] and b.workspace.groups == []
    c = World(problems["surround-05"], CFG, 7)
    while not c.finished and c.clock < 20:
        c.step()
    assert c.rng.draws == a.rng.draws and c.clock == a.clock


def test_the_cap_ends_a_run_with_the_answer_as_it_stands(problems):
    """A run that reaches `max_codelets` answers as it stands and is marked capped (D-061)."""
    r = R.run_problem(problems["surround-05"], v2(max_codelets=10), 1)
    assert r.capped and r.codelets == 10 and r.answer in problems["surround-05"].ids()


def test_the_time_pressure_is_zero_within_the_patience_and_rises_to_one_after_it(problems):
    """D-120 (v5): the pressure reads the codelets since the Worldview last changed (the
    start, before any change); 0 up to `answer_patience`, 1 at twice it, linear between,
    and the schedule then reads T x (1 - pressure): with no pressure the laws of D-056 are
    untouched, so a run where structure forms is the same run."""
    cfg = PROFILES["standard"]
    world = World(problems["ch2-fig02"], cfg, 1)
    assert world.idle() == 0 and world.time_pressure() == 0.0
    world.clock = cfg.answer_patience
    assert world.time_pressure() == 0.0
    world.clock = cfg.answer_patience + cfg.answer_patience // 2
    assert world.time_pressure() == pytest.approx(0.5)
    world.clock = 3 * cfg.answer_patience
    assert world.time_pressure() == 1.0
    world.clock = 0
    # With the Worldview held at its start, T is the initial value and the schedule posts
    # as D-056 says (an expected count of about 1e-5 per codelet at T 82); under full
    # pressure it reads T = 0 and posts `answer_post_rate` per codelet at the top urgency.
    world.clock = 3 * cfg.answer_patience
    posted = world.schedule_find_answer()
    assert len(posted) == int(cfg.answer_post_rate) and all(c.raw == round(cfg.answer_urgency_max) for c in posted)
    assert cfg.answer_patience == 1000      # above every idle stretch of the result sets (251, RESULTS.md v5)


def test_a_one_object_table_answers_before_the_cap(problems):
    """D-120 (v5): Figure 2.2, the touched cup alone [F95 p. 26], where no correspondence
    can ever be built and T never falls, used to reach the cap in 484 of 500 runs under v4
    (RESULTS.md §13); the time pressure ends it with the default answer, the touched
    object [F95 p. 105], after the patience and well before the cap."""
    cfg = PROFILES["standard"]
    for seed in (1, 2, 3):
        r = R.run_problem(problems["ch2-fig02"], cfg, seed)
        assert not r.capped and r.literal and r.answer == "cup1"
        assert cfg.answer_patience < r.codelets < 2 * cfg.answer_patience < cfg.max_codelets


def test_step_returns_none_once_finished_and_the_world_records_the_answer(problems):
    world = World(problems["surround-05"], CFG, 2)
    steps = []
    while not world.finished:
        steps.append(world.step())
    assert world.step() is None
    assert steps[-1].finished and steps[-1].codelet.type == "find-answer"
    assert world.answer.object == steps[-1].outcome.answer.object
    assert all(s.clock == i + 1 for i, s in enumerate(steps))
    assert all(0.0 <= s.temperature <= 100.0 for s in steps)


def test_the_temperature_is_recomputed_on_schedule_and_at_every_change(problems):
    """Every five codelets, and inside the codelet that held a competition or changed the
    Worldview (D-050)."""
    world = World(problems["surround-05"], CFG, 4)
    while not world.finished:
        s = world.step()
        due = (s.clock % CFG.temperature_update_every == 0)
        if s.temperature_update is None:
            assert not due
        else:
            assert s.temperature_update.by in ("schedule", "competition", "change")
            if s.temperature_update.by == "schedule":
                assert due


def test_a_batch_takes_its_seeds_from_the_batch_seed(problems):
    """"Almost all of the configurations were run fifty times." [F95 p. 114]: the seeds of a
    batch come from one generator, and any run can be replayed alone (D-064)."""
    from tabletop.rng import run_seeds
    batch = R.run_batch(problems["surround-01"], CFG, batch_seed=11, count=3)
    assert [r.seed for r in batch] == run_seeds(11, 3)
    assert batch[1] == R.run_problem(problems["surround-01"], CFG, batch[1].seed)


def test_the_report_line_imitates_french(problems):
    """"At the end of each run, Tabletop indicates the object touched by Eliza" [F95
    p. 145] (`docs/trace-format.md`)."""
    r = R.run_problem(problems["surround-05"], CFG, 3)
    line = R.report_line(r)
    assert line.startswith(f"Eliza touches {r.answer} (") and line.endswith(f"T {r.temperature:.1f}.")
    assert f"Codelets {r.codelets}." in line and "capped" not in line
    capped = R.report_line(R.run_problem(problems["surround-05"], v2(max_codelets=5), 1))
    assert "Codelets 5 (capped)." in capped


def test_the_french_faults_profile_runs_too(problems):
    r = R.run_problem(problems["blockage-05"], PROFILES["french-faults"], 1)
    changed = dict(r.changed)
    assert r.profile == "french-faults" and changed.pop("profile") == "french-faults"
    # six, not seven, since v3's `push_reset_set` is already French's "symmetries" (D-114)
    assert len(changed) == 6 and r.answer in problems["blockage-05"].ids()
