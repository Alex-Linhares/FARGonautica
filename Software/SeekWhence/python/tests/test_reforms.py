"""The Reformulator, the threshold and the Bond-assessor [M86 pp. 105-109, 124, 174,
177-178] (DECISIONS D-043 to D-047, D-068)."""

import io

import pytest

from seekwhence import bonds, gnoths, reforms, slipping
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.operations import New, operation_text
from seekwhence.reforms import (Move, ancestors, assess_pair, best_of, bond_assessor,
                                build_program, find_move, find_no_op, firsts, holding,
                                internal_push, lasts, pull, reformulator, selection_order,
                                set_threshold, strongest_bond, within)
from seekwhence.run import NOT_BUILT, TASKS, enter_term, new_world, run_until_prompt
from seekwhence.trace import Trace
from test_operations import figure_1, figure_2, glom_over, gnoth_over
from test_slipping import page_176, still_world


# ---- helpers -------------------------------------------------------------------------------

def bond(world, type, modifier, a, b):
    return bonds.make_bond(world, type, "print-value", modifier, world.find(a), world.find(b))


def figure_1_with_bonds(world):
    """Figure 1 as pp. 101-102 give it: the gnoths `((1 2)) ((1 2)) (3)` over real gloms
    and the five bonds a-e [M86 pp. 101-102, 122]."""
    (g1, g2, g3), (glom1, glom2) = figure_1(world)
    a = bond(world, "Succ", "adjacent", "glint1", "glint2")
    b = bond(world, "Same", "remote", "glint1", "glint3")
    c = bond(world, "Same", "remote", "glint2", "glint4")
    d = bond(world, "Succ", "adjacent", "glint3", "glint4")
    e = bond(world, "Succ", "adjacent", "glint4", "glint5")
    world.reigning = "S-group"
    slipping.assess_bonds(world, "S-group")
    return (g1, g2, g3), (glom1, glom2), (a, b, c, d, e)


def names(chain) -> list:
    return [x.name for x in chain]


def run_to_prompt(world) -> str:
    while True:
        result = run_until_prompt(world)
        if result != "term-cap":
            return result


def full_run(terms, seed, config=None, verbose=True):
    recorder, trace = Recorder(), Trace(io.StringIO(), verbose)
    world = new_world(config, seed=seed, observers=[recorder, trace])
    results = []
    for term in terms:
        enter_term(world, term)
        results.append(run_to_prompt(world))
        if results[-1] == "done":
            break
    return world, recorder, trace, results


def post_and_run(world, type, *args):
    task = world.post(type, *args)
    world.taskrack.clean((type,))
    world.run_task(task)


# ---- the threshold [M86 pp. 105, 124] ------------------------------------------------------

class TestThreshold:

    def test_first_threshold_is_80_percent_of_the_strongest_pull(self):
        """"80% of the strongest pull-bond strength for the reigning type" [M86 p. 105]: 8.0
        for S-group, whose adjacent successor pull is 10 [M86 p. 174]."""
        w, _, _ = still_world(1, 2, 1)
        w.reigning = "S-group"
        assert w.threshold is None
        assert set_threshold(w) == 8.0 and w.threshold == 8.0

    def test_then_one_more_per_reformulator(self):
        """"1 more than the existing threshold value" [M86 p. 105]: 8.0, 9.0, 10.0
        [M86 pp. 174, 177-178]."""
        w, _, _ = still_world(1, 2, 1)
        w.reigning = "S-group"
        assert [set_threshold(w) for _ in range(3)] == [8.0, 9.0, 10.0]

    def test_the_fraction_knob(self):
        """`Config.threshold_fraction` (DECISION D-046)."""
        w, _, _ = still_world(1, 2, 1, config=Config(threshold_fraction=0.5))
        w.reigning = "S-group"
        assert set_threshold(w) == 5.0

    def test_cycle_starts_at_zero(self):
        """Cycle lists no pull-bonds (DECISION D-039), so its threshold starts at 0.0 and
        the Bond-assessor prints `(Cycle 0.0)`."""
        w, _, _ = still_world(3, 7, 3, 3)
        w.reigning = "Cycle"
        assert set_threshold(w) == 0.0

    def test_cleared_by_a_slip_and_set_afresh(self):
        """A slip clears the threshold, so each reigning class starts at 80% of its own
        strongest pull (DECISION D-046; both episodes of run 3 begin at 8.0
        [M86 pp. 174, 177])."""
        w, _, _ = still_world(1, 2, 1)
        w.reigning = "Countup"
        w.threshold = 3.0
        slipping.change_class(w, "S-group", "strict")
        assert w.threshold is None
        assert set_threshold(w) == 8.0

    def test_heating_and_cooling(self):
        """"Reformulators, by adding 1 to the threshold, have the opposite effect, cooling
        things down" to a Gnoth-operator's -1 [M86 p. 124]: the transcript's 8.0, then 8.0
        again after an operation [M86 pp. 177-178]."""
        w, _, _ = still_world(1, 2, 1)
        w.reigning = "S-group"
        set_threshold(w)
        w.threshold -= 1          # what a Gnoth-operator that acts does
        assert set_threshold(w) == 8.0


# ---- lasts, firsts and the selection order [M86 pp. 106-107] -------------------------------

class TestChainsAndOrder:

    def test_figure_2s_lasts_and_firsts(self):
        """"our system will be interested in the 'lasts' of gnoth3: (glom15 glom10 glom7
        glint4), and the 'firsts' of gnoth4: (glom8 glom3 glom1 glint5)" [M86 p. 106]."""
        w, _, _ = still_world(*([1] * 12))
        (g3, g4), f = figure_2(w)
        assert names(lasts(g3)) == [f["glom15"].name, f["glom10"].name, f["glom7"].name, "glint6"]
        assert names(firsts(g4)) == [f["glom8"].name, f["glom3"].name, f["glom1"].name, "glint7"]
        assert names(firsts(g3)) == [f["glom14"].name, "glint1"]
        assert names(lasts(g4)) == [f["glom17"].name, "glint12"]

    def test_a_gnoth_over_bare_glints(self):
        w, _, _ = still_world(1, 2, 1)
        g = gnoth_over(w, "glint1", "glint2")
        assert names(lasts(g)) == ["glint2"] and names(firsts(g)) == ["glint1"]
        empty = gnoths.Gnoth("gnothx")
        assert lasts(empty) == [] and firsts(empty) == []

    def test_the_selection_order_of_p107(self):
        """The order of p. 107 on Figure 2, level by level: `glom15 <--> glom8`; `glom10
        <--> glom8`, `glom15 <--> glom3`, `glom10 <--> glom3`; `glom7 <--> (glom8 glom3)`,
        `(glom15 glom10) <--> glom1`, `glom7 <--> glom1`; `glint4 <--> (glom8 glom3
        glom1)`, `(glom15 glom10 glom7) <--> glint5`, `glint4 <--> glint5` [M86 p. 107]."""
        w, _, _ = still_world(*([1] * 12))
        (g3, g4), f = figure_2(w)
        n = {k: v.name for k, v in f.items()}
        order = [(level, names(l), names(r)) for level, l, r in
                 selection_order(lasts(g3), firsts(g4))]
        assert order == [
            (1, [n["glom15"]], [n["glom8"]]),
            (2, [n["glom10"]], [n["glom8"]]),
            (2, [n["glom15"]], [n["glom3"]]),
            (2, [n["glom10"]], [n["glom3"]]),
            (3, [n["glom7"]], [n["glom8"], n["glom3"]]),
            (3, [n["glom15"], n["glom10"]], [n["glom1"]]),
            (3, [n["glom7"]], [n["glom1"]]),
            (4, ["glint6"], [n["glom8"], n["glom3"], n["glom1"]]),
            (4, [n["glom15"], n["glom10"], n["glom7"]], ["glint7"]),
            (4, ["glint6"], ["glint7"]),
        ]

    def test_uneven_chains(self):
        """A chain the other does not reach: the new element on the longer side is still
        assessed against everything on the shorter side, and the pairs a side cannot
        supply are left out (DECISION D-068)."""
        order = selection_order(["a1", "a2", "a3"], ["b1"])
        assert order == [(1, ["a1"], ["b1"]), (2, ["a2"], ["b1"]), (3, ["a3"], ["b1"])]
        order = selection_order(["a1"], ["b1", "b2"])
        assert order == [(1, ["a1"], ["b1"]), (2, ["a1"], ["b2"])]
        assert selection_order([], ["b1"]) == []


# ---- pulls, holdings and the direction of a move [M86 pp. 108-109] -------------------------

class TestPullsAndHoldings:

    def test_the_pull_between_the_2_and_the_3_of_figure_1(self):
        """"the last term -- the '3' -- is pulled by its predecessor -- a '2' -- which is
        nested within a glom whose print-value is '(1 2)'" [M86 p. 108]: the adjacent
        successor bond, 10 under S-group [M86 p. 102]; nothing pulls the glom `(1 2)`
        itself toward the 3 (DECISION D-043)."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2), _ = figure_1_with_bonds(w)
        assert pull(w, "S-group", w.find("glint4"), w.find("glint5")) == 10
        assert pull(w, "S-group", w.find("glint5"), w.find("glint4")) == 10
        assert pull(w, "S-group", glom2, w.find("glint5")) == 0
        assert pull(w, "S-group", glom1, glom2) == 0

    def test_pushes_do_not_pull(self):
        """A push counts for nothing in a pull: the remote sameness bonds of Figure 1 are
        pushes under S-group and pulls of 10 under Y-group [M86 pp. 101-102, 104]."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        figure_1_with_bonds(w)
        assert pull(w, "S-group", w.find("glint1"), w.find("glint3")) == 0
        assert pull(w, "Y-group", w.find("glint1"), w.find("glint3")) == 10

    def test_holdings(self):
        """"Single gloms are the most likely to move" [M86 p. 108]: the 3 alone in its gnoth
        is held by nothing; the 2 of `(1 2)` is held by its successor bond to the 1, 10
        [M86 p. 102]; the glom `(1 2)` itself, whose bond is inside it, by nothing
        (DECISION D-043)."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2), _ = figure_1_with_bonds(w)
        assert holding(w, "S-group", w.find("glint5"), g3) == 0
        assert holding(w, "S-group", w.find("glint4"), g2) == 10
        assert holding(w, "S-group", glom2, g2) == 0

    def test_a_bond_to_a_sibling_outside_the_glom_holds_it(self):
        """A glom is held by the bonds from anything within it to the other elements of its
        gnoth (DECISION D-043)."""
        w, _, _ = still_world(1, 2, 3, 4)
        glom = glom_over(w, "glint1", "glint2")
        g = gnoth_over(w, glom.name, "glint3")
        bond(w, "Succ", "adjacent", "glint2", "glint3")
        w.reigning = "S-group"
        assert holding(w, "S-group", glom, g) == 10
        assert holding(w, "S-group", w.find("glint3"), g) == 10
        assert within(glom, w.find("glint2")) and not within(glom, w.find("glint3"))

    def test_the_real_move_of_p122_moves_the_3(self):
        """"the 2 <--> 3 pull is resolved via" a SHIFT-LEFT of the 3 and its capture by
        `(1 2)` [M86 p. 122]: the 3, held by nothing, moves; its strength is the pull minus
        its holding, 10 (DECISION D-043 as amended in item 16)."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2), _ = figure_1_with_bonds(w)
        move = assess_pair(w, "S-group", 2, w.find("glint4"), w.find("glint5"), g2, g3)
        assert (move.mover, move.pull, move.held, move.strength) == ("right", 10, 0, 10)
        assert move.moving.name == "glint5" and move.target.name == "glint4"

    def test_two_single_glints_the_left_one_moves_right(self):
        """The transcript's `SHIFT-RIGHT gnoth1 gnoth2 (glint1)` on the gnoths `(1) (2)
        (1)`: equal holdings and sizes, so the left one moves right [M86 p. 174]
        (DECISION D-043)."""
        w, _, _ = still_world(1, 2, 1)
        g1 = gnoth_over(w, "glint1")
        g2 = gnoth_over(w, "glint2", frame=1)
        bond(w, "Succ", "adjacent", "glint1", "glint2")
        w.reigning = "S-group"
        move = assess_pair(w, "S-group", 1, w.find("glint1"), w.find("glint2"), g1, g2)
        assert (move.mover, move.strength) == ("left", 10)

    def test_the_smaller_one_moves_on_a_tie_of_holdings(self):
        """A glom and a glint, both unheld: the glint moves (DECISION D-043)."""
        w, _, _ = still_world(1, 2, 3)
        glom = glom_over(w, "glint1", "glint2")
        g1 = gnoth_over(w, glom.name)
        g2 = gnoth_over(w, "glint3", frame=1)
        bond(w, "Succ", "adjacent", glom.name, "glint3")
        w.reigning = "S-group"
        move = assess_pair(w, "S-group", 1, glom, w.find("glint3"), g1, g2)
        assert move.mover == "right"

    def test_no_pull_no_move(self):
        w, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2), _ = figure_1_with_bonds(w)
        assert assess_pair(w, "S-group", 1, glom2, w.find("glint5"), g2, g3) is None

    def test_with_each_in_the_list_takes_the_strongest(self):
        """"assess pull with each in the list" [M86 p. 107]: the strongest move of the
        group, not the first listed (DECISION D-043). Under Y-group a remote sameness bond
        is a pull of 10 and an adjacent one nothing [M86 p. 104]."""
        w, _, _ = still_world(1, 1, 1)
        g1 = gnoth_over(w, "glint1", "glint2")
        g2 = gnoth_over(w, "glint3", frame=1)
        bond(w, "Same", "adjacent", "glint2", "glint3")      # 0 under Y-group
        bond(w, "Same", "remote", "glint1", "glint3")        # 10 under Y-group
        w.reigning = "Y-group"
        best = best_of(w, "Y-group", 2, [w.find("glint1"), w.find("glint2")], [w.find("glint3")],
                       g1, g2)
        assert best.left.name == "glint1" and best.pull == 10 and best.strength == 10


# ---- find_move: the first strong enough, highest level first ------------------------------

class TestFindMove:

    def test_figure_1_finds_the_real_move_at_level_2(self):
        """On Figure 1 nothing pulls the top-level gloms (level 1); at level 2 the 2 of
        `(1 2)` pulls the 3 with strength 10, above the threshold of 8 [M86 pp. 105, 108,
        122]."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2), _ = figure_1_with_bonds(w)
        move = find_move(w, "S-group", 8.0)
        assert (move.level, move.left.name, move.right.name) == (2, "glint4", "glint5")
        assert (move.left_gnoth, move.right_gnoth) == (g2, g3)
        assert find_move(w, "S-group", 10.0) is None

    def test_the_highest_level_takes_precedence(self):
        """"Should both glom and subglom feel a pull, the glom move should take precedence
        since it is structurally more important" [M86 p. 107]: a weaker pull between the
        top-level gloms wins over a stronger one between their subgloms (DECISION D-068)."""
        w, _, _ = still_world(1, 2, 3, 4)
        glom1 = glom_over(w, "glint1", "glint2")
        glom2 = glom_over(w, "glint3", "glint4")
        g1 = gnoth_over(w, glom1.name)
        g2 = gnoth_over(w, glom2.name, frame=1)
        bond(w, "Succ", "adjacent", glom1.name, glom2.name)   # the top-level pull
        bond(w, "Succ", "adjacent", "glint2", "glint3")       # the subgloms' pull
        w.reigning = "S-group"
        move = find_move(w, "S-group", 5.0)
        assert (move.level, move.left.name, move.right.name) == (1, glom1.name, glom2.name)

    def test_levels_are_taken_over_the_whole_parse(self):
        """Level 1 of every neighbouring pair before level 2 of any (DECISION D-068): the
        top-level pull between the second and third gnoths is found before the nested pull
        between the first and second."""
        w, _, _ = still_world(1, 2, 3, 4, 5)
        glom1 = glom_over(w, "glint1", "glint2")
        g1 = gnoth_over(w, glom1.name)
        g2 = gnoth_over(w, "glint3", "glint4", frame=1)
        g3 = gnoth_over(w, "glint5", frame=2)
        bond(w, "Succ", "adjacent", "glint2", "glint3")       # level 2 between g1, g2
        bond(w, "Succ", "adjacent", "glint4", "glint5")       # level 1 between g2, g3
        w.reigning = "S-group"
        move = find_move(w, "S-group", 8.0)
        assert (move.level, move.left_gnoth, move.right_gnoth) == (1, g2, g3)

    def test_left_to_right_within_a_level(self):
        w, _, _ = still_world(1, 2, 3, 4)
        g1 = gnoth_over(w, "glint1")
        g2 = gnoth_over(w, "glint2", frame=1)
        g3 = gnoth_over(w, "glint3", "glint4", frame=2)
        bond(w, "Succ", "adjacent", "glint1", "glint2")
        bond(w, "Succ", "adjacent", "glint2", "glint3")
        w.reigning = "S-group"
        move = find_move(w, "S-group", 8.0)
        assert (move.left_gnoth, move.right_gnoth) == (g1, g2)

    def test_the_catchall_is_no_neighbour(self):
        """The catchall is never operated on (DECISION D-030)."""
        w, _, _ = still_world(1, 2, 3)
        g1 = gnoth_over(w, "glint1", "glint2")
        gnoths.make_catchall(w, [w.find("glint3")])
        bond(w, "Succ", "adjacent", "glint2", "glint3")
        w.reigning = "S-group"
        assert find_move(w, "S-group", 8.0) is None

    def test_no_gnoths_no_move(self):
        w, _, _ = still_world(1, 2)
        w.reigning = "S-group"
        assert find_move(w, "S-group", 8.0) is None


# ---- the operation or PROGRAM for a move [M86 pp. 121-123] (DECISION D-044) ----------------

class TestBuildProgram:

    def test_a_top_level_pull_is_one_bare_move(self):
        """"this high-level attraction is translated into the move: (SHIFT-LEFT gnoth3
        gnoth4 (glom8))" [M86 p. 122]; the transcript's bare `((SHIFT-RIGHT gnoth3 gnoth5
        (glint3)))` [M86 p. 177]."""
        w, _, _ = still_world(1, 2, 1)
        g1 = gnoth_over(w, "glint1")
        g2 = gnoth_over(w, "glint2", frame=1)
        bond(w, "Succ", "adjacent", "glint1", "glint2")
        w.reigning = "S-group"
        move = assess_pair(w, "S-group", 1, w.find("glint1"), w.find("glint2"), g1, g2)
        program = build_program(move)
        assert program == ("SHIFT-RIGHT", g1.name, g2.name, ("glint1",))
        assert operation_text(program) == f"(SHIFT-RIGHT {g1.name} {g2.name} (glint1))"

    def test_the_high_level_move_of_p122(self):
        """`(SHIFT-LEFT gnoth3 gnoth4 (glom8))` when glom8 is pulled by glom15 [M86
        p. 122]: glom8 has three subgloms, glom15 two, so on equal holdings the smaller
        one moves (both cover six and five terms here; glom15 covers 5, glom8 6)."""
        w, _, _ = still_world(*([1] * 12))
        (g3, g4), f = figure_2(w)
        bond(w, "Same", "adjacent", f["glom15"].name, f["glom8"].name)
        w.reigning = "C-group"
        move = assess_pair(w, "C-group", 1, f["glom15"], f["glom8"], g3, g4)
        assert move.mover == "left"      # glom15 covers five terms, glom8 six
        assert build_program(move) == ("SHIFT-RIGHT", g3.name, g4.name, (f["glom15"].name,))

    def test_the_real_move_of_p122(self):
        """The 3 pulled by the nested 2: `(SHIFT-LEFT gnoth2 gnoth3 (glint5))` then
        `(CAPTURE-RIGHT gnoth2 glom2 glint5)` [M86 p. 122]; nothing was burst, so no
        ENCLOSE is needed (DECISION D-044)."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2), _ = figure_1_with_bonds(w)
        move = find_move(w, "S-group", 8.0)
        program = build_program(move)
        assert program == ("PROGRAM", (("SHIFT-LEFT", g2.name, g3.name, ("glint5",)),
                                       ("CAPTURE-RIGHT", g2.name, glom2.name, "glint5")))

    def test_the_deeper_move_of_p122(self):
        """glom1 pulled by glom7 in Figure 2: the shift, the capture by glom10 (glom7's
        parent), then the repairs level by level, "re-enclosing the remaining gloms at each
        intermediate level" [M86 pp. 122-123]: glom3 had nothing but glom1 (`nil`), glom8's
        other subgloms glom4 and glom2, and glom15's other subglom glom9 with the new glom
        the capture made (DECISION D-044)."""
        w, _, _ = still_world(*([1] * 12))
        (g3, g4), f = figure_2(w)
        bond(w, "Same", "remote", f["glom7"].name, f["glom1"].name)
        bond(w, "Same", "remote", f["glom5"].name, f["glom7"].name)   # holds glom7 in place
        w.reigning = "Y-group"
        move = find_move(w, "Y-group", 5.0)
        assert (move.level, move.left, move.right) == (3, f["glom7"], f["glom1"])
        assert move.mover == "right"     # glom1, held by nothing, "is to be pulled away"
        assert ancestors(f["glom1"], g4) == [f["glom3"], f["glom8"]]
        assert ancestors(f["glom7"], g3) == [f["glom10"], f["glom15"]]
        program = build_program(move)
        assert program == ("PROGRAM", (
            ("SHIFT-LEFT", g3.name, g4.name, (f["glom1"].name,)),
            ("CAPTURE-RIGHT", g3.name, f["glom10"].name, f["glom1"].name),
            ("ENCLOSE", g4.name, None),
            ("ENCLOSE", g4.name, (f["glom4"].name, f["glom2"].name)),
            ("ENCLOSE", g3.name, (f["glom9"].name, New(1))),
        ))

    def test_the_deeper_move_runs(self):
        """The PROGRAM built for the deeper move carries out on Figure 2: glom1 ends inside
        the glom that replaced glom10, glom15 is rebuilt around it, and gnoth4 keeps glom4
        and glom2 under a new glom [M86 p. 122]."""
        w, recorder, _ = still_world(*([1] * 12))
        (g3, g4), f = figure_2(w)
        bond(w, "Same", "remote", f["glom7"].name, f["glom1"].name)
        bond(w, "Same", "remote", f["glom5"].name, f["glom7"].name)   # holds glom7 in place
        w.reigning = "Y-group"
        w.threshold = 5.0
        program = build_program(find_move(w, "Y-group", 5.0))
        post_and_run(w, "Gnoth-operator", program)
        operated = recorder.of_kind("operated")[-1]
        assert operated.acted and operated.stopped is None and operated.steps == 5
        top3 = [x.print_value for x in g3.collection]
        assert len(top3) == 2 and f["glom1"].parent is not None
        assert g3.range == (1, 8) and g4.range == (9, 12)
        assert [x.name for x in g4.collection][-1] == f["glom17"].name

    def test_a_mover_nested_on_its_own_side(self):
        """A nested mover pulled by a top-level target: the shift and one ENCLOSE per
        burst glom on the mover's side, no capture (DECISION D-044)."""
        w, _, _ = still_world(1, 2, 3, 4)
        inner = glom_over(w, "glint1", "glint2")
        outer = glom_over(w, inner.name, "glint3")
        g1 = gnoth_over(w, outer.name)
        g2 = gnoth_over(w, "glint4", frame=1)
        bond(w, "Succ", "adjacent", "glint3", "glint4")
        w.reigning = "S-group"
        move = find_move(w, "S-group", 8.0)
        assert move.moving.name == "glint3"       # held by nothing, like the 4
        assert build_program(move) == ("PROGRAM", (
            ("SHIFT-RIGHT", g1.name, g2.name, ("glint3",)),
            ("ENCLOSE", g1.name, (inner.name,)),
        ))


# ---- NO-OP [M86 p. 113] (DECISION D-045) ----------------------------------------------------

class TestNoOp:

    def test_internal_pushes_above_the_threshold_propose_a_no_op(self):
        """A gnoth whose internal pushes exceed the threshold, when no neighbour pull does,
        gets `(NO-OP gnoth)`; the largest pushes first (DECISION D-045)."""
        w, _, _ = still_world(1, 1, 2, 2, 3)
        g1 = gnoth_over(w, "glint1", "glint2")
        g2 = gnoth_over(w, "glint3", "glint4", frame=1)
        bond(w, "Same", "adjacent", "glint1", "glint2")      # a push of 5 under S-group
        bond(w, "Same", "adjacent", "glint3", "glint4")
        bond(w, "Same", "remote", "glint3", "glint4")        # another push in gnoth2
        w.reigning = "S-group"
        slipping.assess_bonds(w, "S-group")
        assert internal_push(g1) == 5 and internal_push(g2) == 10
        assert find_no_op(w, 8.0) == ("NO-OP", g2.name)
        assert find_no_op(w, 4.0) == ("NO-OP", g2.name)
        assert find_no_op(w, 10.0) is None

    def test_the_no_op_goes_through_the_gnoth_operator_and_the_capper(self):
        """The Gnoth-operator uncaps the gnoth and sets out a Capper, which recaps it
        [M86 p. 113]."""
        w, recorder, _ = still_world(1, 1, 2, 2, 3)
        g1 = gnoth_over(w, "glint1", "glint2")
        g2 = gnoth_over(w, "glint3", "glint4", frame=1)
        bond(w, "Same", "adjacent", "glint3", "glint4")
        bond(w, "Same", "remote", "glint3", "glint4")
        w.reigning = "S-group"
        post_and_run(w, "Reformulator")
        assert recorder.of_kind("reformulated")[-1].program == f"(NO-OP {g2.name})"
        posted = [e.type for e in recorder.of_kind("posted")]
        assert posted[-2:] == ["Gnoth-operator", "Bond-assessor"]
        post_and_run(w, "Gnoth-operator", ("NO-OP", g2.name))
        capper = recorder.of_kind("posted")[-1]
        assert g2.glom is None and capper.type == "Capper" and capper.args == (g2.name, (3, 4))
        post_and_run(w, "Capper", *capper.args)
        assert g2.glom is not None and g2.range == (3, 4)


# ---- the Reformulator task [M86 pp. 105-109] ------------------------------------------------

class TestReformulator:

    def test_the_transcripts_first_move(self):
        """On the gnoths `(1) (2) (1)` of p. 173 with p. 174's bonds, the Reformulator sets
        the threshold to 8.0, finds the pull between the 1 and the 2 and sets out a
        Gnoth-operator with `(SHIFT-RIGHT gnoth1 gnoth2 (glint1))` and a Bond-assessor on
        `(S-group 8.0)` [M86 p. 174]."""
        w, recorder, trace = still_world(1, 2, 1)
        g1 = gnoth_over(w, "glint1")
        g2 = gnoth_over(w, "glint2", frame=1)
        g3 = gnoth_over(w, "glint3", frame=2)
        bond(w, "Succ", "adjacent", "glint1", "glint2")
        bond(w, "Pred", "adjacent", "glint2", "glint3")
        bond(w, "Same", "remote", "glint1", "glint3")
        w.reigning = "S-group"
        post_and_run(w, "Reformulator")
        assert w.threshold == 8.0
        found = recorder.of_kind("move-found")[-1]
        assert (found.level, found.left, found.right, found.mover, found.strength) == \
            (1, "glint1", "glint2", "left", 10)
        posted = [e for e in recorder.of_kind("posted")][-2:]
        assert posted[0].type == "Gnoth-operator"
        assert posted[0].args == (("SHIFT-RIGHT", g1.name, g2.name, ("glint1",)),)
        assert posted[1].type == "Bond-assessor" and posted[1].args == ("S-group", 8.0)
        # the gnoths' bonds were assessed afresh [M86 p. 105]
        assert g1.external_bonds and g2.internal_bonds == []
        post_and_run(w, "Gnoth-operator", *posted[0].args)
        assert trace.lines[-2] == f"doing task Gnoth-operator on ((SHIFT-RIGHT {g1.name} {g2.name} (glint1)))"
        assert gnoths.parse_text(w) == "((1 2) (1))" and w.threshold == 7.0
        post_and_run(w, "Bond-assessor", *posted[1].args)
        assert trace.lines[-1] == "doing task Bond-assessor on (S-group 8.0)"

    def test_no_move_only_the_bond_assessor(self):
        """"If the Reformulator finds no actions to be taken ... it hangs a Bond-assessor
        task on the taskrack" [M86 p. 105]: on p. 174's gnoths `(1 2) (1)` nothing pulls
        across the border (the Pred bond is 0, the remote sameness a push)."""
        w, recorder, _ = still_world(1, 2, 1)
        g2 = gnoth_over(w, "glint1", "glint2", frame=1)
        g3 = gnoth_over(w, "glint3", frame=2)
        bond(w, "Succ", "adjacent", "glint1", "glint2")
        bond(w, "Pred", "adjacent", "glint2", "glint3")
        bond(w, "Same", "remote", "glint1", "glint3")
        w.reigning = "S-group"
        w.threshold = 7.0
        post_and_run(w, "Reformulator")
        assert w.threshold == 8.0
        assert not recorder.of_kind("move-found")
        done = recorder.of_kind("reformulated")[-1]
        assert (done.kind_, done.threshold, done.program) == ("S-group", 8.0, None)
        assert [e.type for e in recorder.of_kind("posted")][-1] == "Bond-assessor"
        assert [e.type for e in recorder.of_kind("posted")].count("Gnoth-operator") == 0

    def test_the_second_episode_of_run_3(self):
        """On the state of p. 176 (gnoths `(1 2) (1 2) (3)`, S-group, the three bonds of
        p. 174) with p. 177's `bond7` (Succ adjacent 1-2 inside the second gnoth) and
        `bond6` (Succ remote 2-5): no pull crosses a border, so this Reformulator proposes
        nothing. The transcript's `(SHIFT-RIGHT gnoth3 gnoth5 (glint3))` [M86 p. 177]
        is not derivable from the printed bonds by the rule of p. 106 (RESULTS.md)."""
        w, recorder, _ = still_world(1, 2, 1, 2, 3)
        (g2, g3, g5), _ = page_176(w)
        bond(w, "Same", "remote", "glint2", "glint4")
        bond(w, "Succ", "remote", "glint2", "glint5")
        bond(w, "Succ", "adjacent", "glint3", "glint4")
        post_and_run(w, "Reformulator")
        assert w.threshold == 8.0 and not recorder.of_kind("move-found")
        bond(w, "Succ", "adjacent", "glint4", "glint5")        # p. 177's bond8
        post_and_run(w, "Reformulator")
        found = recorder.of_kind("move-found")[-1]
        assert (found.left, found.right, found.mover) == ("glint4", "glint5", "right")
        assert found.program == f"(SHIFT-LEFT {g3.name} {g5.name} (glint5))"

    def test_stale_cases(self):
        """No reigning class, or a hypothesis already made: nothing is done
        (DECISION D-009)."""
        w, recorder, _ = still_world(1, 2, 1)
        post_and_run(w, "Reformulator")
        assert recorder.of_kind("stale")[-1].reason == "no reigning class"
        post_and_run(w, "Bond-assessor", "S-group", 8.0)
        assert recorder.of_kind("stale")[-1].reason == "no reigning class"
        w.reigning = "S-group"
        from seekwhence.forms import parse
        from seekwhence.hypothesis import Hypothesis
        w.hypothesis = Hypothesis(parse("(Countup 1)"))
        post_and_run(w, "Reformulator")
        assert recorder.of_kind("stale")[-1].reason == "a hypothesis exists"
        post_and_run(w, "Bond-assessor", "S-group", 8.0)
        assert recorder.of_kind("stale")[-1].reason == "a hypothesis exists"
        assert w.threshold is None

    def test_the_tasks_are_installed(self):
        assert TASKS["Reformulator"] is reformulator and TASKS["Bond-assessor"] is bond_assessor
        assert "Reformulator" not in NOT_BUILT and "Bond-assessor" not in NOT_BUILT
        assert "Gnoth-caster" not in NOT_BUILT


# ---- the Bond-assessor [M86 p. 124] (DECISION D-047) ----------------------------------------

class TestBondAssessor:

    def test_a_bond_above_the_threshold_means_another_reformulator(self):
        """`(S-group 8.0)` and `(S-group 9.0)` with a bond of 10 present: a Reformulator;
        `(S-group 10.0)`: a Gnoth-caster [M86 p. 178] (DECISION D-047: strictly
        greater)."""
        w, recorder, _ = still_world(1, 2, 1)
        bond(w, "Succ", "adjacent", "glint1", "glint2")
        w.reigning = "S-group"
        assert strongest_bond(w, "S-group") == 10
        for threshold, expected in ((8.0, "Reformulator"), (9.0, "Reformulator"),
                                    (10.0, "Gnoth-caster")):
            post_and_run(w, "Bond-assessor", "S-group", threshold)
            assert [e.type for e in recorder.of_kind("posted")][-1] == expected
            assessed = recorder.of_kind("bonds-assessed")[-1]
            assert (assessed.threshold, assessed.strongest, assessed.more) == \
                (threshold, 10, expected == "Reformulator")

    def test_pushes_count_by_absolute_value(self):
        """A push of 5 under S-group keeps reformulation going at a threshold of 4
        (DECISION D-047)."""
        w, recorder, _ = still_world(1, 1)
        bond(w, "Same", "adjacent", "glint1", "glint2")
        w.reigning = "S-group"
        assert strongest_bond(w, "S-group") == 5
        post_and_run(w, "Bond-assessor", "S-group", 4.0)
        assert [e.type for e in recorder.of_kind("posted")][-1] == "Reformulator"
        post_and_run(w, "Bond-assessor", "S-group", 5.0)
        assert [e.type for e in recorder.of_kind("posted")][-1] == "Gnoth-caster"

    def test_cycle_goes_straight_to_the_gnoth_caster(self):
        """Under Cycle every bond is worth 0 (DECISION D-039), so `(Cycle 0.0)` sets out the
        Gnoth-caster at once, as run 2 shows no reformulation [M86 pp. 168-169]."""
        w, recorder, _ = still_world(3, 7, 3, 3)
        bond(w, "Same", "remote", "glint1", "glint3")
        w.reigning = "Cycle"
        post_and_run(w, "Bond-assessor", "Cycle", 0.0)
        assert [e.type for e in recorder.of_kind("posted")][-1] == "Gnoth-caster"

    def test_without_arguments_it_reads_the_world(self):
        w, recorder, _ = still_world(1, 2)
        bond(w, "Succ", "adjacent", "glint1", "glint2")
        w.reigning = "S-group"
        w.threshold = 9.0
        post_and_run(w, "Bond-assessor")
        assessed = recorder.of_kind("bonds-assessed")[-1]
        assert (assessed.kind_, assessed.threshold, assessed.more) == ("S-group", 9.0, True)
        w.threshold = None
        post_and_run(w, "Bond-assessor")
        assert recorder.of_kind("bonds-assessed")[-1].threshold == 0.0


# ---- whole runs: the thresholds of run 3 and the settling ---------------------------------

class TestRuns:

    def test_run_3s_first_reformulation(self):
        """`1 2` then `1` on ten seeds: after `Countup --> S-group` the Reformulators,
        Bond-assessors and Gnoth-operators run until a Gnoth-caster is set out; the
        Bond-assessor's numbers are 8.0 at first and never above 10.0, each is one more
        than the previous unless a Gnoth-operator acted in between, the last is 10.0, the
        one shift of the 1 into the 2's gnoth is carried out once (a second Gnoth-operator
        finding gnoth1 gone, "it will simply terminate" [M86 p. 124]), `show-parse`
        ends at `((1 2) (1))` and the Gnoth-caster makes the second hypothesis
        [M86 pp. 174, 178]."""
        for seed in range(10):
            w, recorder, trace, results = full_run([1, 2, 1], seed)
            lines = trace.lines
            start = lines.index("Changing to a new reigning class. Countup --> S-group")
            after = lines[start:]
            numbers = [float(l.split()[-1].rstrip(")")) for l in after
                       if l.startswith("doing task Bond-assessor on (S-group ")]
            assert numbers and numbers[0] == 8.0 and numbers[-1] == 10.0 and max(numbers) == 10.0
            assert all(b - a in (0.0, 1.0) for a, b in zip(numbers, numbers[1:]))
            assert after.count("doing task Gnoth-caster") == 1
            operated = recorder.of_kind("operated")
            assert sum(1 for e in operated if e.acted) == 1
            assert gnoths.parse_text(w) == "((1 2) (1))"
            assert [g.name for g in gnoths.gnoths(w)] == ["gnoth2", "gnoth3"]
            # 10.0, or 9.0 when the acting Gnoth-operator ran after the last Reformulator
            # had raised it ("lowers the threshold" [M86 p. 124], DECISION D-067).
            assert w.threshold in (9.0, 10.0)
            assert w.hypothesis is not None and w.hypothesis.show_form() == "(S-group 1 2)"

    def test_the_transcripts_sequence_on_one_seed(self):
        """A seed on which the Gnoth-operator runs before the second Reformulator gives the
        transcript's numbers 8.0, 8.0, 9.0, 10.0 exactly [M86 pp. 174, 178]; if none of the
        first forty seeds does, the other order (8.0, 9.0, 9.0, 10.0) must."""
        seen = set()
        for seed in range(40):
            w, recorder, trace, results = full_run([1, 2, 1], seed)
            numbers = tuple(float(l.split()[-1].rstrip(")")) for l in trace.lines
                            if l.startswith("doing task Bond-assessor on (S-group "))
            seen.add(numbers)
        assert (8.0, 8.0, 9.0, 10.0) in seen or (8.0, 9.0, 9.0, 10.0) in seen

    def test_the_system_settles(self):
        """"Eventually, the system will settle as the Reformulators find fewer and fewer
        relevant operations to suggest to Gnoth-operators" [M86 p. 124]: over a run the
        moves found per Reformulator do not increase as the threshold rises, the last
        Reformulators find none, and the episode ends in a Gnoth-caster."""
        for seed in range(5):
            w, recorder, trace, results = full_run([1, 2, 1], seed)
            found = []
            for e in recorder.events:
                if e.kind == "reformulated":
                    found.append((e.threshold, e.program is not None))
            thresholds = [t for t, _ in found]
            assert thresholds == sorted(thresholds)
            assert found[-1][1] is False and found[-1][0] == 10.0
            assert any(e.kind == "posted" and e.type == "Gnoth-caster" for e in recorder.events)

    def test_a_gnoth_operator_whose_gnoths_vanished_does_nothing(self):
        """Two Reformulators propose the same shift; the second Gnoth-operator finds gnoth1
        gone and terminates, leaving the threshold alone [M86 p. 124]."""
        w, recorder, trace = still_world(1, 2, 1)
        g1 = gnoth_over(w, "glint1")
        g2 = gnoth_over(w, "glint2", frame=1)
        bond(w, "Succ", "adjacent", "glint1", "glint2")
        w.reigning = "S-group"
        post_and_run(w, "Reformulator")
        post_and_run(w, "Reformulator")
        programs = [e.args[0] for e in recorder.of_kind("posted") if e.type == "Gnoth-operator"]
        assert len(programs) == 2 and programs[0] == programs[1]
        post_and_run(w, "Gnoth-operator", programs[0])
        assert w.threshold == 8.0 and g1.name not in w.socratoplasm
        post_and_run(w, "Gnoth-operator", programs[1])
        assert w.threshold == 8.0
        assert recorder.of_kind("stale")[-1].reason == f"{g1.name} is gone"

    def test_run_2_reformulates_nothing_under_cycle(self):
        """`3 7 3 3` on the fence seed: the Reformulator under Cycle finds no pull (every
        bond is 0, DECISION D-039), the Bond-assessor prints `(Cycle 0.0)` and sets out the
        Gnoth-caster at once, as run 2's terse transcript shows no reformulation
        [M86 pp. 168-169]."""
        from test_filtering import FENCE_SEED
        w, recorder, trace, results = full_run([3, 7, 3, 3], FENCE_SEED)
        assert "doing task Bond-assessor on (Cycle 0.0)" in trace.lines
        assert not recorder.of_kind("move-found")
        posted = [e.type for e in recorder.of_kind("posted")]
        assert posted.count("Reformulator") == 1 and posted.count("Gnoth-caster") == 1

    def test_same_seed_same_trace(self):
        traces = [full_run([1, 2, 1], 7)[2].lines for _ in range(2)]
        assert traces[0] == traces[1]
