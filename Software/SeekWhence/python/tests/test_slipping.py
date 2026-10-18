"""The Gnoth-setter, freeze-dried hypotheses and the slip-or-stay decision [M86 pp. 90-92,
98-105, 168, 173-176] (DECISIONS D-035, D-036, D-040, D-041, D-042, D-046, D-063)."""

import io
from dataclasses import replace

import pytest

from seekwhence import bonds, gnoths, slipping
from seekwhence import cytoplasm as cy
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.forms import parse
from seekwhence.gnoths import Gnoth
from seekwhence.hypothesis import FreezeDried
from seekwhence.run import NOT_BUILT, enter_term, new_world, run_until_prompt
from seekwhence.slipping import (Frame, assess_bonds, gnoth_stability, layout, recent_tries,
                                 slip_check, slip_or_stay, slipping_pressure,
                                 staying_pressure, terms_explained)
from seekwhence.trace import Trace

FENCE_SEED = 4     # a seed on which `3 7 3` gloms the fence `(3 7)`, as runs 1 and 2 did


# ---- helpers -------------------------------------------------------------------------------

def run_to_prompt(world) -> str:
    while True:
        result = run_until_prompt(world)
        if result != "term-cap":
            return result


def full_run(terms, seed, config=None, verbose=True, reformulate=True, cast=True):
    """A run from scratch, every task live, the terms entered as the user would; with
    `reformulate` False the Reformulator is a no-op, so that the Gnoth-setter's state can
    be looked at before the reformulation changes it; with `cast` False the Gnoth-caster
    is a no-op, so that the state after the slip-or-stay decision stays as it is."""
    recorder, trace = Recorder(), Trace(io.StringIO(), verbose)
    world = new_world(config, seed=seed, observers=[recorder, trace])
    if not reformulate:
        world.handlers["Reformulator"] = lambda w, *args: None
    if not cast:
        world.handlers["Gnoth-caster"] = lambda w, *args: None
    results = []
    for term in terms:
        enter_term(world, term)
        results.append(run_to_prompt(world))
        if results[-1] == "done":
            break
    return world, recorder, trace, results


def still_world(*terms, config=None):
    """A world with the terms entered and no task run: the tests put the gnoths and bonds
    in by hand, as the thesis's worked examples give them."""
    recorder, trace = Recorder(), Trace(io.StringIO())
    world = new_world(config, seed=0, observers=[recorder, trace])
    for term in terms:
        bonds.new_term(world, term)
    world.taskrack.clean(tuple(world.urgencies))
    return world, recorder, trace


def gnoth_over(world, kind, frame, *names) -> Gnoth:
    """A gnoth with a gnoth-glom over the named elements."""
    return gnoths.make_gnoth(world, kind, [world.find(n) for n in names], frame)


def bond(world, type, modifier, a, b):
    return bonds.make_bond(world, type, "print-value", modifier, world.find(a), world.find(b))


def figure_1(world):
    """The state of Figure 1 [M86 pp. 101-102]: `1 2 1 2 3` as three gnoths `(1 2)`,
    `(1 2)`, `(3)` under S-group, with the five bonds a-e."""
    g1 = gnoth_over(world, "S-group", 0, "glint1", "glint2")
    g2 = gnoth_over(world, "S-group", 1, "glint3", "glint4")
    g3 = gnoth_over(world, "S-group", 2, "glint5")
    a = bond(world, "Succ", "adjacent", "glint1", "glint2")
    b = bond(world, "Same", "remote", "glint1", "glint3")
    c = bond(world, "Same", "remote", "glint2", "glint4")
    d = bond(world, "Succ", "adjacent", "glint3", "glint4")
    e = bond(world, "Succ", "adjacent", "glint4", "glint5")
    world.reigning = "S-group"
    return (g1, g2, g3), (a, b, c, d, e)


def page_176(world):
    """The state of the second slip check of run 3 [M86 pp. 174-176]: gnoths `(1 2)`,
    `(1 2)`, `(3)` under S-group, the three bonds of p. 174's `show` (`bond1 Succ
    print-value (adjacent) -- (glint1 glint2)`, `bond2 Pred print-value (adjacent) --
    (glint2 glint3)`, `bond3 Same print-value (remote) -- (glint1 glint3)`), and two
    freeze-dried hypotheses, `(Countup 1)` and `(S-group 1 2)`."""
    g2 = gnoth_over(world, "S-group", 1, "glint1", "glint2")
    g3 = gnoth_over(world, "S-group", 2, "glint3", "glint4")
    g5 = gnoth_over(world, "S-group", 2, "glint5")
    b1 = bond(world, "Succ", "adjacent", "glint1", "glint2")
    b2 = bond(world, "Pred", "adjacent", "glint2", "glint3")
    b3 = bond(world, "Same", "remote", "glint1", "glint3")
    world.freeze_dried = [FreezeDried(parse("(Countup 1)"), 2),
                          FreezeDried(parse("(S-group 1 2)"), 4)]
    world.reigning = "S-group"
    return (g2, g3, g5), (b1, b2, b3)


def lines_after(trace, prefix):
    start = max(i for i, l in enumerate(trace.lines) if l.startswith(prefix))
    return [l for l in trace.lines[start:] if not l.startswith("doing task")]


# ---- the layout of the old hypothesis (DECISION D-036) ------------------------------------------

class TestLayout:

    def test_the_p98_example(self):
        """"if the old hypothesis were '(S-group 1 3)', and two gnoths, each holding '1 2 3',
        were already in existence, the catchall gnoth might be holding '1 2 3 4'"; the
        Gnoth-setter makes "one to hold the initial '1 2 3' from the catchall and the
        other to hold the trailing 4" [M86 p. 98]: three complete frames and a final
        gnoth."""
        frames = layout(parse("(S-group 1 3)"), [1, 2, 3, 1, 2, 3, 1, 2, 3, 4])
        assert [(f.index, f.start, f.end, f.complete) for f in frames] == [
            (0, 1, 3, True), (1, 4, 6, True), (2, 7, 9, True), (3, 10, 10, False)]
        assert [f.value for f in frames] == [(1, 2, 3), (1, 2, 3), (1, 2, 3), None]
        assert terms_explained(frames) == 9

    def test_the_running_example_countup(self):
        """"When the next term entered is '1', a Gnoth-setter puts out three gnoths, one
        for each term. The first two are in accord with the discredited hypothesis ...
        The last one simply holds the non-fitting term" [M86 pp. 98-99]."""
        frames = layout(parse("(Countup 1)"), [1, 2, 1])
        assert frames == [Frame(0, 1, 1, 1, True), Frame(1, 2, 2, 2, True),
                          Frame(2, 3, 3, None, False)]
        assert terms_explained(frames) == 2

    def test_run_2s_cut_short_turn(self):
        """`(Cycle 3 2 (3 7))` over `3 7 3 3`: one turn `(3 7)` (DECISION D-004), a turn cut
        short by the failing term, `(3)`, and the final gnoth `(3)`, the three gnoths run 2's
        Gnoth-setter lists [M86 p. 168] and its Gnoth-caster casts [M86 p. 168] (DECISION
        D-036); the hypothesis explained three terms [M86 p. 91]."""
        frames = layout(parse("(Cycle 3 2 (3 7))"), [3, 7, 3, 3])
        assert [(f.index, f.start, f.end, f.complete) for f in frames] == [
            (0, 1, 2, True), (1, 3, 3, False), (2, 4, 4, False)]
        assert frames[0].value == (3, 7) and frames[2].value is None
        assert terms_explained(frames) == 3

    def test_run_3s_second_refutation_gives_the_final_gnoth_frame_2(self):
        """`(S-group 1 2)` over `1 2 1 2 3` [M86 p. 101]: two complete frames and a final
        gnoth whose frame number is 2, as `gnoth5` prints `frame: 2` [M86 p. 179]."""
        frames = layout(parse("(S-group 1 2)"), [1, 2, 1, 2, 3])
        assert [(f.index, f.start, f.end, f.complete) for f in frames] == [
            (0, 1, 2, True), (1, 3, 4, True), (2, 5, 5, False)]
        assert terms_explained(frames) == 4

    def test_a_y_group_refuted_by_its_fourth_term(self):
        frames = layout(parse("(Y-group 3 3 (3 7 3))"), [3, 7, 3, 7])
        assert [(f.index, f.start, f.end) for f in frames] == [(0, 1, 3), (1, 4, 4)]

    def test_nothing_fits(self):
        """A hypothesis whose first term is already wrong leaves everything to the final
        gnoth, frame 0."""
        assert layout(parse("(Countup 5)"), [1, 2]) == [Frame(0, 1, 2, None, False)]
        assert terms_explained(layout(parse("(Countup 5)"), [1, 2])) == 0


# ---- Figure 1: bond assessment, stability, pressures [M86 pp. 101-104] -------------------------

class TestFigure1:

    def test_the_bonds_are_classified_as_the_thesis_says(self):
        """Bond a "functions as an 'internal pull' for gnoth1 since it has a positive value
        and both members, glint1 and glint2, are within that gnoth"; bond e "functions as
        an 'external pull' between gnoth2 and gnoth3"; bond c "functions as an 'external
        push', tending to keep the parent gnoths, gnoth1 and gnoth2, apart" [M86 p. 101]."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (a, b, c, d, e) = figure_1(world)
        assess_bonds(world, "S-group")
        # each list by the gnoth's elements from right to left, creation order within one
        # element, the order of p. 179's printed lists (item 21)
        assert g1.internal_bonds == [(a, 10)] and g1.external_bonds == [(c, -5), (b, -5)]
        assert g2.internal_bonds == [(d, 10)] and g2.external_bonds == [(c, -5), (e, 10), (b, -5)]
        assert g3.internal_bonds == [] and g3.external_bonds == [(e, 10)]

    def test_the_assessed_lists_are_in_the_printouts_order(self):
        """`external-bonds: ((bond2 0) (bond5 -5) (bond6 0) (bond3 -5))` for the gnoth over
        `(1 2)` (glint2's bonds 2, 5, 6 then glint1's bond3), `((bond6 0) (bond5 -5) (bond2 0)
        (bond3 -5))` and `internal-bonds: ((bond8 10) (bond7 10))` for the gnoth over
        `(1 2 3)` [M86 p. 179]: the bonds of p. 178's `show`, made in its order."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        g2 = gnoth_over(world, "S-group", 1, "glint1", "glint2")
        g5 = gnoth_over(world, "S-group", 2, "glint3", "glint4", "glint5")
        b1 = bond(world, "Succ", "adjacent", "glint1", "glint2")
        b2 = bond(world, "Pred", "adjacent", "glint2", "glint3")
        b3 = bond(world, "Same", "remote", "glint1", "glint3")
        b5 = bond(world, "Same", "remote", "glint2", "glint4")
        b6 = bond(world, "Succ", "remote", "glint2", "glint5")
        b7 = bond(world, "Succ", "adjacent", "glint3", "glint4")
        b8 = bond(world, "Succ", "adjacent", "glint4", "glint5")
        assess_bonds(world, "S-group")
        assert [b.name for b, _ in g2.external_bonds] == [b2.name, b5.name, b6.name, b3.name]
        assert [b.name for b, _ in g2.internal_bonds] == [b1.name]
        assert [b.name for b, _ in g5.external_bonds] == [b6.name, b5.name, b2.name, b3.name]
        assert [b.name for b, _ in g5.internal_bonds] == [b8.name, b7.name]
        assert [v for _, v in g2.external_bonds] == [0, -5, 0, -5]
        assert [v for _, v in g5.internal_bonds] == [10, 10]

    def test_the_stabilities_15_10_minus_5_and_the_total_20(self):
        """`(+10 +2.5 +2.5) -(0) = 15` for gnoth1, `(+10 + 2.5 + 2.5) - (5) = 10` for gnoth2,
        `(0) - (5) = -5` for gnoth3, "a total system stability, in this case, of 15 + 10 -
        5 = +20" [M86 pp. 102-103]; "the reigning S-group, whose staying pressure is 20"
        [M86 p. 104]."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), _ = figure_1(world)
        assert staying_pressure(world, "S-group") == 20
        assert [gnoth_stability(g) for g in (g1, g2, g3)] == [15, 10, -5]

    def test_y_groups_slipping_pressure_is_8(self):
        """"0.4 [the s-link slipperiness] * (10 + 10) = 8, where the 10's are the strengths,
        in a Y-group environment, of the 'remote same' bonds between glint1 and glint3 and
        between glint2 and glint4. Thus Y-group, with a slipping pressure of 8, cannot
        seize the throne from the reigning S-group, whose staying pressure is 20"
        [M86 p. 104]."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        figure_1(world)
        assert slipping_pressure(world, "S-group", "Y-group") == 8.0
        check = slip_check(world)
        assert (check.stayval, check.best, check.pressure, check.slipped) == (20, "Y-group", 8.0, False)
        assert dict(check.pressures)["Y-group"] == 8.0
        assert all(p <= 8.0 for _, p in check.pressures)
        assert [to for to, _ in check.pressures] == list(world.platoplasm.neighbours("S-group"))

    def test_the_slipping_pressure_ignores_the_gnoths(self):
        """The slipping pressure is "taken over all existing bonds" [M86 p. 104], whatever
        the gnoths: with every glint free it is still 8."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        bond(world, "Same", "remote", "glint1", "glint3")
        bond(world, "Same", "remote", "glint2", "glint4")
        bond(world, "Succ", "adjacent", "glint1", "glint2")
        assert slipping_pressure(world, "S-group", "Y-group") == 8.0
        assert slipping_pressure(world, "S-group", "Countup") == pytest.approx(0.1 * 10)

    def test_pushes_count_by_absolute_value(self):
        """"the sum of all pulling-bond strengths, and the absolute value of the sum of all
        pushing-bond strengths" [M86 p. 104]: under C-group the three adjacent successor
        bonds are pushes of -5 and count 15."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        figure_1(world)
        link = world.platoplasm.s_link("S-group", "C-group")
        assert slipping_pressure(world, "S-group", "C-group") == pytest.approx(15 * link)


# ---- the transcript's own numbers [M86 pp. 168, 174-176] -------------------------------------

class TestTranscriptNumbers:

    def test_stayval_15_0_best_y_group_4_0(self):
        """`slip-check: stayval: 15.0` / `best: (Y-group 4.0)` [M86 p. 176] from the three
        bonds of p. 174 and the gnoths `(1 2)`, `(1 2)`, `(3)`: gnoth2 holds 10 (bond1) +
        2.5 (half of bond3's push), gnoth3 holds 2.5, gnoth5 nothing; Y-group values the
        one remote sameness bond 10 and the link is 0.4 (SPEC 11.6; DECISIONS D-039,
        D-041, D-042)."""
        world, _, trace = still_world(1, 2, 1, 2, 3)
        (g2, g3, g5), (b1, b2, b3) = page_176(world)
        check = slip_or_stay(world)
        assert (check.stayval, check.best, check.pressure, check.slipped) == (15.0, "Y-group", 4.0, False)
        assert [gnoth_stability(g) for g in (g2, g3, g5)] == [12.5, 2.5, 0]
        assert trace.lines[-2:] == ["slip-check: stayval: 15.0", "        best: (Y-group 4.0)"]
        assert world.reigning == "S-group"

    def test_the_assessed_lists_as_show_prints_them(self):
        """`external-bonds: ((bond2 0) (bond3 -5))` and `internal-bonds: ((bond1 10))` for
        gnoth2, `external-bonds: ((bond2 0) (bond3 -5))` and `internal-bonds: nil` for
        gnoth3 [M86 p. 175]."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        (g2, g3, g5), _ = page_176(world)
        assess_bonds(world, "S-group")
        assert dict(g2.show_fields())["external-bonds"] == "((bond2 0) (bond3 -5))"
        assert dict(g2.show_fields())["internal-bonds"] == "((bond1 10))"
        assert dict(g3.show_fields())["external-bonds"] == "((bond2 0) (bond3 -5))"
        assert dict(g3.show_fields())["internal-bonds"] == "nil"
        assert g2.show_fields()[0][0] == "external-bonds"

    def test_the_first_try_counts_for_nothing(self):
        """The 15.0 is the raw sum: the hypothesis just refuted is S-group's first try
        (DECISION D-041)."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        page_176(world)
        assert recent_tries(world, "S-group") == 0
        world.freeze_dried.append(FreezeDried(parse("(S-group 1 3)"), 4))
        assert recent_tries(world, "S-group") == 1
        assert slip_check(world).stayval == 7.5

    def test_with_bond5_the_numbers_of_p104_return(self):
        """Once `bond5`, the remote sameness between glint2 and glint4, is made [M86 p. 176],
        the state is Figure 1's but for the adjacent successor bonds of gnoth2 and gnoth3,
        and Y-group's pressure is p. 104's 8."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        page_176(world)
        bond(world, "Same", "remote", "glint2", "glint4")
        check = slip_check(world)
        assert (check.stayval, check.best, check.pressure) == (20.0, "Y-group", 8.0)

    def test_stayval_0_0_best_nil_under_cycle(self):
        """`slip-check: stayval: 0.0` / `best: nil` [M86 p. 168] with run 2's one bond, the
        remote sameness `bond1` between glint1 and glint3 [M86 p. 167], lying between the
        gnoths `(3 7)` and `(3)`: Cycle lists no pull or push bonds and its neighbours
        score 0 on that bond (DECISIONS D-038, D-039, D-042)."""
        world, _, trace = still_world(3, 7, 3, 3)
        gnoth_over(world, "Cycle", 0, "glint1", "glint2")
        gnoth_over(world, "Cycle", 1, "glint3")
        gnoth_over(world, "Cycle", 2, "glint4")
        bond(world, "Same", "remote", "glint1", "glint3")
        world.reigning = "Cycle"
        check = slip_or_stay(world)
        assert (check.stayval, check.best, check.pressure, check.slipped) == (0.0, None, 0.0, False)
        assert trace.lines[-2:] == ["slip-check: stayval: 0.0", "        best: nil"]
        assert world.platoplasm.strongest_pull("Cycle") == 0


# ---- recent tries, strict classes, the decision (DECISIONS D-040, D-041, D-042) -------------

class TestDecision:

    def test_recent_tries_count_the_classs_run_at_the_end_of_the_list(self):
        """"how many 'tries' the class has had since it seized power" [M86 p. 99]: the
        freeze-dried hypotheses of the class at the end of the list, less the one just
        refuted (DECISION D-041)."""
        world, _, _ = still_world(1, 2)
        s, c = parse("(S-group 1 2)"), parse("(C-group 1 2)")
        for forms, tries in (([], 0), ([s], 0), ([s, s], 1), ([c, s, s, s], 2), ([s, c], 0),
                             ([s, s, c, s], 0)):
            world.freeze_dried = [FreezeDried(f, 2) for f in forms]
            assert recent_tries(world, "S-group") == tries, forms

    def test_each_try_halves_the_staying_pressure(self):
        """DECISION D-041: `Config.stay_factor_per_try` (0.5) once per recent try."""
        world, _, _ = still_world(1, 2, 1, 2, 3)
        figure_1(world)
        s = parse("(S-group 1 2)")
        world.freeze_dried = [FreezeDried(s, 4)]
        assert slip_check(world).stayval == 20
        world.freeze_dried = [FreezeDried(s, 4)] * 2
        assert slip_check(world).stayval == 10
        world.freeze_dried = [FreezeDried(s, 4)] * 3
        assert slip_check(world).stayval == 5
        world2, _, _ = still_world(1, 2, 1, 2, 3, config=Config(stay_factor_per_try=1.0))
        figure_1(world2)
        world2.freeze_dried = [FreezeDried(s, 4)] * 3
        assert slip_check(world2).stayval == 20

    def test_a_strict_class_slips_at_once(self):
        """"if a reigning class -- such as Constant -- is very strict ... slipping is chosen
        immediately" [M86 p. 99]: Constant to C-group, Countup to S-group (`Countup -->
        S-group` [M86 p. 174]), no `slip-check` line (DECISIONS D-040, D-042)."""
        for old, new in (("Constant", "C-group"), ("Countup", "S-group")):
            world, recorder, trace = still_world(1, 1)
            gnoth_over(world, old, 0, "glint1")
            gnoth_over(world, old, 1, "glint2")
            world.reigning = old
            assert slip_or_stay(world) is None
            assert world.reigning == new
            changed = recorder.of_kind("class-changed")[-1]
            assert (changed.old, changed.new, changed.reason) == (old, new, "strict")
            assert not recorder.of_kind("slip-check")
            assert trace.lines[-1] == f"Changing to a new reigning class. {old} --> {new}"
            assert [g.plato_class for g in gnoths.gnoths(world)] == [new, new]
            assert [a.name for a in world.platoplasm.type(new).actualizations] == ["gnoth1", "gnoth2"]
            assert world.platoplasm.type(old).actualizations == []

    def test_the_strict_classes_knob(self):
        """With Countup not strict it goes through the check (DECISION D-040)."""
        world, recorder, _ = still_world(1, 2, 1, config=Config(strict_classes=("Constant",)))
        gnoth_over(world, "Countup", 0, "glint1")
        gnoth_over(world, "Countup", 1, "glint2")
        gnoth_over(world, "Countup", 2, "glint3")
        bond(world, "Succ", "adjacent", "glint1", "glint2")
        world.reigning = "Countup"
        check = slip_or_stay(world)
        assert check is not None and recorder.of_kind("slip-check")
        assert check.stayval == -10.0     # one external pull of 10: 5 torn from each gnoth

    def test_a_neighbour_slips_only_when_strictly_greater(self):
        """"If the largest of these values is greater than the 'staying pressure' ... then
        a slip to the corresponding class will be made" [M86 p. 104]: with the two remote
        sameness bonds alone the staying pressure under S-group is 10 (four half-pushes);
        Y-group's pressure is 20 times the link: 0.5 ties and stays, 0.6 slips."""
        for link, slipped in ((0.5, False), (0.6, True)):
            links = tuple((a, b, link if (a, b) == ("S-group", "Y-group") else s)
                          for a, b, s in Config().s_links)
            world, recorder, trace = still_world(1, 2, 1, 2, 3, config=Config(s_links=links))
            gnoth_over(world, "S-group", 0, "glint1", "glint2")
            gnoth_over(world, "S-group", 1, "glint3", "glint4")
            gnoth_over(world, "S-group", 2, "glint5")
            bond(world, "Same", "remote", "glint1", "glint3")
            bond(world, "Same", "remote", "glint2", "glint4")
            world.reigning = "S-group"
            world.threshold = 8.0
            check = slip_or_stay(world)
            assert (check.stayval, check.best, check.pressure) == (10.0, "Y-group", pytest.approx(20 * link))
            assert check.slipped is slipped
            assert world.reigning == ("Y-group" if slipped else "S-group")
            if slipped:
                changed = recorder.of_kind("class-changed")[-1]
                assert (changed.old, changed.new, changed.reason) == ("S-group", "Y-group", "slip")
                assert trace.lines[-1] == "Changing to a new reigning class. S-group --> Y-group"
                assert world.threshold is None
                assert all(g.plato_class == "Y-group" for g in gnoths.gnoths(world))
                assert [v for _, v in gnoths.gnoths(world)[0].external_bonds] == [10, 10]
            else:
                assert world.threshold == 8.0

    def test_ties_between_neighbours_go_to_the_more_slippery_link(self):
        """DECISION D-042."""
        links = (("S-group", "Y-group", 0.2), ("S-group", "P-group", 0.3))
        values = (("Y-group", "Same", "remote", 10), ("P-group", "Same", "remote", 10))
        world, _, _ = still_world(1, 2, 1, config=Config(s_links=links, bond_values=values))
        gnoth_over(world, "S-group", 0, "glint1", "glint2", "glint3")
        bond(world, "Same", "remote", "glint1", "glint3")
        world.reigning = "S-group"
        check = slip_check(world)
        assert (check.best, check.pressure) == ("P-group", 3.0)

    def test_best_is_nil_when_no_neighbour_scores(self):
        """`best: nil` when the largest pressure is 0 (DECISION D-042), and also when the
        class has no s-links."""
        world, _, _ = still_world(1, 2)
        gnoth_over(world, "S-group", 0, "glint1", "glint2")
        world.reigning = "S-group"
        assert slip_check(world).best is None
        world2, _, _ = still_world(1, 2, config=Config(s_links=()))
        gnoth_over(world2, "S-group", 0, "glint1", "glint2")
        bond(world2, "Same", "remote", "glint1", "glint2")
        world2.reigning = "S-group"
        assert slip_check(world2).best is None and slip_check(world2).pressures == ()

    def test_a_bond_outside_every_gnoth_is_assessed_in_neither_list(self):
        world, _, _ = still_world(1, 2, 1)
        g = gnoth_over(world, "S-group", 0, "glint1", "glint2")
        bond(world, "Same", "remote", "glint1", "glint3")
        b = bond(world, "Succ", "adjacent", "glint1", "glint2")
        assess_bonds(world, "S-group")
        assert g.internal_bonds == [(b, 10)] and g.external_bonds == []
        assert slipping_pressure(world, "S-group", "Y-group") == 4.0    # counted all the same


# ---- the Gnoth-setter in runs [M86 pp. 168, 172-176] ------------------------------------------

class TestGnothSetterRuns:

    def test_run_2s_gnoth_setter_and_slip_check(self):
        """`3 7 3` then `3` on the fence seed: after the filtering line the
        Gnoth-setter prints its `top-down glom` lines, `gnoths: (gnoth1 gnoth2 gnoth3)`,
        and `slip-check: stayval: 0.0` / `best: nil` [M86 p. 168]; gnoth1 over the fence
        glom is recapped (its old cap went at the refutation, D-063, and p. 168's five
        top-down gloms for three gnoths say all three were filled), gnoth2 holds the
        cut-short turn `(3)` and gnoth3 the
        troublesome `(3)` (DECISION D-036); the hypothesis is freeze-dried with the three
        terms it explained and its box is gone [M86 pp. 91, 99]; Cycle reigns and the
        Reformulator is set out [M86 p. 99]. The Gnoth-caster is turned off so that the
        state after the check can be looked at (`test_casting.py` has it on)."""
        world, recorder, trace, results = full_run([3, 7, 3, 3], FENCE_SEED, cast=False)
        after = lines_after(trace, "new term being hypothesis-filtered")
        assert not any(l.startswith("set-out") for l in trace.lines)   # verbose: run 3 prints none
        tops = [l for l in after if l.startswith("top-down glom")]
        # gnoth1's cap glom3 went at the refutation (the previous frame's cap, D-063), so
        # the Gnoth-setter recaps all three gnoths
        assert tops == ["top-down glom glom4", "top-down glom glom5", "top-down glom glom6"]
        i = after.index("gnoths: (gnoth1 gnoth2 gnoth3)")
        assert after[i + 1:i + 3] == ["slip-check: stayval: 0.0", "        best: nil"]
        g1, g2, g3 = gnoths.gnoths(world)
        assert (g1.name, g1.frame, g1.range, g1.glom.name) == ("gnoth1", 0, (1, 2), "glom4")
        assert g1.collection[0].name == "glom1" and g1.form == [(parse("(Cycle 3 2 (3 7))"), "pure")]
        assert (g2.name, g2.frame, g2.range, g2.terms) == ("gnoth2", 1, (3, 3), (3,))
        assert (g3.name, g3.frame, g3.range, g3.terms) == ("gnoth3", 2, (4, 4), (3,))
        assert [g.plato_class for g in (g1, g2, g3)] == ["Cycle"] * 3
        assert [g.equivalence_type for g in (g1, g2, g3)] == ["term", None, None]
        assert world.hypothesis is None and world.reigning == "Cycle"
        assert world.threshold == 0.0   # the Reformulator's 80% of Cycle's strongest pull, 0
        assert [str(f) for f in world.freeze_dried] == ["(Cycle 3 2 (3 7)) (3 terms)"]
        dried = recorder.of_kind("freeze-dried")[0]
        assert (dried.form, dried.explained, dried.reigning) == ("(Cycle 3 2 (3 7))", 3, "Cycle")
        assert gnoths.catchall(world) is None
        assert gnoths.parse_text(world) == "(((3 7)) (3) (3))"
        posted = [e.type for e in recorder.of_kind("posted")]
        assert posted[posted.index("Gnoth-setter") + 1:].count("Reformulator") == 1
        assert [e.type for e in recorder.of_kind("chosen")].count("Gnoth-caster") == 1
        assert results[-1] == "empty"

    @pytest.mark.parametrize("seed", range(10))
    def test_run_3s_first_gnoth_setter_slips_from_countup(self, seed):
        """`1 2` then `1`: the Gnoth-setter "puts out three gnoths, one for each term"
        [M86 pp. 98-99], printed `gnoths: (gnoth1 gnoth2 gnoth3)` [M86 p. 173] (gnoth1,
        the Gnoth-maker's gnoth over `(1 2)`, is filled afresh to the first frame and the
        bottom-up `(1 2)` is burst: `show` then lists bonds between glints only
        [M86 p. 174]); the strict Countup slips to S-group with no `slip-check` printed,
        `Countup --> S-group` [M86 pp. 173-174] (DECISIONS D-036, D-040, D-042); the first
        two gnoths "are in fact parse-equivalent to it" [M86 p. 99]. The Reformulator is
        turned off so that the Gnoth-setter's gnoths are seen before the first move
        (item 17's tests cover the rest of the episode)."""
        world, recorder, trace, results = full_run([1, 2, 1], seed, reformulate=False)
        after = lines_after(trace, "new term being hypothesis-filtered")
        i = after.index("gnoths: (gnoth1 gnoth2 gnoth3)")
        assert after[i + 1] == "Changing to a new reigning class. Countup --> S-group"
        assert not any(l.startswith("slip-check") for l in after)
        assert sum(l.startswith("top-down glom") for l in after) == 3
        g1, g2, g3 = gnoths.gnoths(world)
        assert [(g.name, g.frame, g.range) for g in (g1, g2, g3)] == [
            ("gnoth1", 0, (1, 1)), ("gnoth2", 1, (2, 2)), ("gnoth3", 2, (3, 3))]
        assert [g.terms for g in (g1, g2, g3)] == [(1,), (2,), (1,)]
        assert [g.equivalence_type for g in (g1, g2, g3)] == ["parse", "parse", None]
        assert [g.plato_class for g in (g1, g2, g3)] == ["S-group"] * 3
        assert world.reigning == "S-group" and world.hypothesis is None
        assert [str(f) for f in world.freeze_dried] == ["(Countup 1) (2 terms)"]
        assert world.find("glom1") is None
        for b in world.cytoplasm:
            if isinstance(b, cy.Bond):
                assert all(m.is_glint for m in b.members)
        assert all(x.pseudo for x in cy.lineup(world))
        changed = recorder.of_kind("class-changed")[0]
        assert (changed.old, changed.new, changed.reason) == ("Countup", "S-group", "strict")
        assert g1.show_fields()[0][0] == "external-bonds"

    def test_the_catchall_is_numbered_after_the_gnoths_and_before_the_next(self):
        """The catchall is made at the first confirming term, so in run 3 it is `gnoth4`,
        between the first Gnoth-setter's `gnoth3` and the second's `gnoth5` [M86 pp. 173,
        176] (DECISIONS D-030, D-063): here, `3 7 3 7 3 7 5` with the guesses refused, the
        catchall is `gnoth2` and the Gnoth-setter's new gnoths `gnoth3` to `gnoth5`."""
        recorder, trace = Recorder(), Trace(io.StringIO())
        from seekwhence.filtering import ScriptedAnswerer
        world = new_world(seed=FENCE_SEED, observers=[recorder, trace],
                          answerer=ScriptedAnswerer(["no", "no", "no"]))
        for term in (3, 7, 3, 7, 3, 7, 5):
            enter_term(world, term)
            run_to_prompt(world)
        assert recorder.of_kind("catchall")[0].name == "gnoth2"
        assert "gnoths: (gnoth1 gnoth3 gnoth4 gnoth5)" in trace.lines
        assert [g.frame for g in gnoths.gnoths(world)] == [0, 1, 2, 3]
        assert [g.equivalence_type for g in gnoths.gnoths(world)] == ["term", "parse", "parse", None]
        assert gnoths.catchall(world) is None

    def test_a_cover_seed_refuted_by_3_7_3_7_goes_through_the_check(self):
        """On a seed where `3 7 3` became `(Y-group 3 3 (3 7 3))`, the fourth term `7`
        refutes it; Y-group is not strict, so the check is printed and the decision agrees
        with its numbers [M86 pp. 104-105]; the gnoths are `(3 7 3)` and `(7)`. The
        Gnoth-caster is turned off so that the state after the check stays."""
        for seed in range(30):
            world, recorder, trace, results = full_run([3, 7, 3, 7], seed, cast=False)
            if not recorder.of_kind("refuted"):
                continue
            assert recorder.of_kind("refuted")[0].form == "(Y-group 3 3 (3 7 3))"
            check = recorder.of_kind("slip-check")[0]
            assert check.reigning == "Y-group"
            assert check.slipped is (check.best is not None and check.pressure > check.stayval)
            assert world.reigning == (check.best if check.slipped else "Y-group")
            assert [g.terms for g in gnoths.gnoths(world)] == [(3, 7, 3), (7,)]
            assert all(g.plato_class == world.reigning for g in gnoths.gnoths(world))
            assert f"slip-check: stayval: {check.stayval}" in trace.lines
            break
        else:
            pytest.fail("no cover seed among 0-29")

    def test_the_gnoth_setter_cleans_the_reformulation_tasks(self):
        """A Gnoth-setter removes the waiting Reformulator, Gnoth-operator, Bond-assessor,
        Gnoth-caster, Slip-scout and Capper tasks (DECISION D-010)."""
        recorder = Recorder()
        world = new_world(seed=FENCE_SEED, observers=[recorder])

        def setter_with_leftovers(w, *args):
            w.post("Gnoth-caster")
            w.post("Bond-assessor", "S-group", 8.0)
            slipping.gnoth_setter(w, *args)

        world.handlers["Gnoth-setter"] = setter_with_leftovers
        for term in (3, 7, 3, 3):
            enter_term(world, term)
            run_to_prompt(world)
        cleaned = [e for e in recorder.of_kind("cleaned") if e.moment == "reform"]
        assert cleaned and dict(cleaned[0].removed) == {"Gnoth-caster": 1, "Bond-assessor": 1}
        assert world.freeze_dried

    def test_stale_cases(self):
        """No hypothesis, or one not refuted: nothing is done (DECISION D-009)."""
        recorder = Recorder()
        world = new_world(seed=FENCE_SEED, observers=[recorder])
        world.post("Gnoth-setter")
        run_to_prompt(world)
        assert recorder.of_kind("stale")[-1].reason == "no hypothesis"
        for term in (3, 7, 3):
            enter_term(world, term)
            run_to_prompt(world)
        world.post("Gnoth-setter")
        run_to_prompt(world)
        assert recorder.of_kind("stale")[-1].reason == "the hypothesis is not refuted"
        assert world.hypothesis is not None and not world.freeze_dried

    def test_same_seed_same_trace(self):
        for terms in ([3, 7, 3, 3], [1, 2, 1]):
            traces = [full_run(terms, 3)[2].lines for _ in range(2)]
            assert traces[0] == traces[1]

    def test_the_slipping_module_is_installed(self):
        assert "Gnoth-setter" not in NOT_BUILT and "Gnoth-caster" not in NOT_BUILT
