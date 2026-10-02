"""Loop0003 item 4: the Pnet and coderack models (numbo/models/pnet_model.py,
numbo/models/coderack_model.py), and the events they need.

The Pnet's activations change in four ways: spread-activation-in-pnet (a
pnet event with all 88), initialize-pnet at the start of config, a pnode's
own message (:add-activation, set-activation, :update-instances, ...) sent
by a codelet or by reactivate-cyto, and repump's set-up-activations.  The
oracle trace has only the first; events.py publishes the other three as
pnet-initialized and pnodes-changed.  The coderack is made by
create-coderack (coderack-created), then changes by post, choose and
rack-emptied events; a choice's draws say which codelet left its bin.

These tests check:
  - the Pnet's static structure (88 nodes, kinds, links with their types)
    is the one the World builds;
  - its layout is deterministic, cached, and has no overlaps, with the sums
    above and the products below their result;
  - both models on hand-made event sequences;
  - both models equal the state derived from the World after every event
    of the 11 chapter puzzles x seed 1 (the coderack's contents against the
    World's coderack bins, the activations and instances against *pnet*'s);
  - the models change nothing in a run.
"""

import concurrent.futures
import io
import os
import traceback

import pytest

from full_runs import PUZZLES
from numbo import events, harness, observe
from numbo.models import coderack_model, pnet_model
from numbo.models.coderack_model import Codelet, CoderackModel, CoderackModelError
from numbo.models.pnet_model import PnetModel, pnet_layout, pnet_structure, structure_from_world


# ---------------------------------------------------------------------------
# The Pnet's structure

def test_the_structure_has_the_88_pnodes_in_pnet_order():
    s = pnet_structure()
    world = harness.load_world()
    names = tuple(p.name.name for p in world[events._PNET])
    assert len(s.nodes) == 88
    assert s.names == names
    assert len(set(s.names)) == 88
    assert s.node("ONE").value == 1 and s.node("ONE").short_name == "1"
    assert s.index("ONE") == 0


def test_node_kinds():
    s = pnet_structure()
    kinds = {}
    for node in s.nodes:
        kinds.setdefault(node.kind, []).append(node.name)
    assert len(kinds["number"]) == 25
    assert len(kinds["plus"]) == 27 and len(kinds["times"]) == 26
    assert sorted(kinds["operation"]) == ["ADD", "MULTIPLY", "SUBTRACTION"]
    assert kinds["concept"] == ["MULTIPLE-OF-TEN"]
    assert sorted(kinds["link-type"]) == sorted(
        ["OPERAND", "RESULT+", "RESULTX", "SIMILAR", "OPERATION", "INSTANCE"])
    assert set(kinds) == {"number", "plus", "times", "operation", "concept", "link-type"}


def test_links_carry_their_types():
    s = pnet_structure()
    by_type = {}
    for link in s.links:
        by_type[link.type] = by_type.get(link.type, 0) + 1
        assert link.source in s.names and link.target in s.names
    assert by_type == {"RESULT+": 152, "OPERAND": 92, "SIMILAR": 47, "RESULTX": 44,
                       "OPERATION": 6, "INSTANCE": 1}
    assert pnet_model.PnetLink("TIMES2-2", "FOUR", "RESULTX") in s.links
    assert s.neighbors("PLUS1-1") == (("ONE", "RESULT+"), ("TWO", "RESULT+"))
    # Undirected edges: each pair and type once.
    edges = s.edges()
    assert len(edges) == len(set(edges))
    assert ("ONE", "PLUS1-1", "RESULT+") in edges or ("PLUS1-1", "ONE", "RESULT+") in edges


def test_the_structure_is_the_worlds_before_and_after_init():
    world = harness.load_world()
    assert structure_from_world(world) == pnet_structure()
    # After initialize-pnet-2 replaced the neighbor symbols by pnodes.
    from numbo import init, pnet_functions
    world.out = harness.OutputStream(io.StringIO())
    init.init_chiffre(world)
    pnet_functions.initialize_pnet(world)
    assert structure_from_world(world) == pnet_structure()


def test_the_result_of_each_operation_node():
    s = pnet_structure()
    assert s.result("PLUS1-1") == "TWO"
    assert s.result("PLUS7-8") == "FIFTEEN"
    assert s.result("TIMES2-2") == "FOUR"
    assert s.result("TIMES7-7") == "FIFTY"        # a SIMILAR link
    assert s.result("TIMES10-15") == "ONE-HUNDRED-FIFTY"
    assert s.result("ONE") is None


# ---------------------------------------------------------------------------
# The Pnet's layout

def test_the_layout_places_every_node_without_overlaps():
    layout = pnet_layout()
    s = pnet_structure()
    assert set(layout.positions) == set(s.names)
    w, h = layout.node_size
    points = sorted(layout.positions.values())
    for i, (x, y) in enumerate(points):
        assert 0 <= x - w / 2 and x + w / 2 <= layout.width
        assert 0 <= y - h / 2 and y + h / 2 <= layout.height
        for x2, y2 in points[i + 1:]:
            assert abs(x - x2) >= w or abs(y - y2) >= h, ((x, y), (x2, y2))


def test_the_layout_is_deterministic_and_cached():
    assert pnet_layout() is pnet_layout()
    fresh = pnet_model.layout_pnet(pnet_structure())
    assert fresh == pnet_layout() and fresh is not pnet_layout()
    bigger = pnet_layout(cell=(100, 60))
    assert bigger.width > pnet_layout().width


def test_numbers_in_a_row_sums_above_products_below():
    layout = pnet_layout()
    s = pnet_structure()
    numbers = sorted((n for n in s.nodes if n.kind == "number"), key=lambda n: n.value)
    xs = [layout.positions[n.name][0] for n in numbers]
    ys = {layout.positions[n.name][1] for n in numbers}
    assert xs == sorted(xs) and len(set(xs)) == 25 and len(ys) == 1
    (row,) = ys
    for n in s.nodes:
        x, y = layout.positions[n.name]
        if n.kind in ("plus", "times"):
            assert x == layout.positions[s.result(n.name)][0], n.name
            assert (y < row) if n.kind == "plus" else (y > row), n.name
        elif n.kind in ("operation", "concept", "link-type"):
            # A band of their own, below the products.
            assert y > max(layout.positions[t.name][1] for t in s.nodes if t.kind == "times")


# ---------------------------------------------------------------------------
# The Pnet model on hand-made events

def start_event():
    return observe.RunStarted(problem=(114, 11, 20, 7, 1, 6), seed=1, max_iterations=None,
                              rng="splitmix64", pnet=pnet_structure().names)


def test_the_pnet_model_follows_its_events():
    m = PnetModel()
    names = pnet_structure().names
    m.on_event(start_event())
    assert all(m.activation(n) is None for n in names)
    m.on_event(observe.PnetInitialized(activations=(10,) * 88))
    assert m.activation("ONE") == 10 and m.changed == frozenset(names)
    m.on_event(observe.PnetActivations(activations=tuple(float(i) for i in range(88))))
    assert m.activation(names[5]) == 5.0
    assert m.changed == frozenset(names)
    m.on_event(observe.PnodesChanged(field="activation", values=(("TWO", 99.5),)))
    assert m.activation("TWO") == 99.5 and m.changed == {"TWO"}
    m.on_event(observe.PnodesChanged(field="instances",
                                     values=(("TWO", (("2b", "CYTO-BRICK1"),)),)))
    assert m.instances("TWO") == (("2b", "CYTO-BRICK1"),)
    assert m.with_instances() == ("TWO",)
    assert m.activation("TWO") == 99.5
    # Other events change nothing.
    m.on_event(observe.RackEmptied())
    assert m.changed == frozenset()
    # A new run resets the model.
    m.on_event(start_event())
    assert m.activation("TWO") is None and m.instances("TWO") is None


def test_the_pnet_model_rejects_a_foreign_pnet():
    m = PnetModel()
    bad = observe.RunStarted(problem=(1, 1, 1, 1, 1, 1), seed=1, max_iterations=None,
                             rng="splitmix64", pnet=("A", "B"))
    with pytest.raises(pnet_model.PnetModelError):
        m.on_event(bad)


def test_cytoplasm_instances_are_told_from_configs_pseudo_instances():
    # config gives the operations and the link types pseudo-instances of
    # types 5g/6g; a cyto-node's instance has the node's type.
    m = PnetModel()
    m.on_event(start_event())
    m.on_event(observe.PnodesChanged(field="instances", values=(
        ("ADD", (("6g", "node-add"),)),
        ("OPERAND", (("5g", "operand"),)),
        ("TWO", (("2b", "CYTO-BRICK1"), ("4bl", "CYTO-BLOCK2-V1"))),
        ("SIX", (("3dt", "CYTO-TARGET-6-V2"),)))))
    assert set(m.with_instances()) == {"TWO", "SIX", "ADD", "OPERAND"}
    assert m.with_cyto_instances() == tuple(n for n in pnet_structure().names
                                            if n in ("TWO", "SIX"))
    assert m.cyto_instances("TWO") == (("2b", "CYTO-BRICK1"), ("4bl", "CYTO-BLOCK2-V1"))
    assert m.cyto_instances("ADD") == () and m.cyto_instances("ONE") == ()
    assert pnet_model.is_pseudo_instance(("6g", "node-add"))
    assert not pnet_model.is_pseudo_instance(("1t", "CYTO-TARGET"))


def test_pnet_models_compare_by_state():
    a, b = PnetModel(), PnetModel()
    assert a == b
    a.on_event(observe.PnodesChanged(field="activation", values=(("TWO", 1),)))
    assert a != b
    b.on_event(observe.PnodesChanged(field="activation", values=(("TWO", 1),)))
    assert a == b


# ---------------------------------------------------------------------------
# The coderack model on hand-made events

LEVELS = (600, 300, 7, 4, 1, 0)


def post(codelet, *args, urgency):
    return observe.CodeletPosted(codelet=codelet, args=args, urgency=urgency)


def chosen(codelet, args, urgency, draws, n=0):
    return observe.CodeletChosen(n=n, codelet=codelet, args=args, urgency=urgency,
                                 draws=draws)


def made():
    m = CoderackModel()
    m.on_event(observe.CoderackCreated(name="MY-CODERACK", levels=LEVELS))
    return m


def test_posts_go_first_in_their_bin():
    m = made()
    assert m.name == "MY-CODERACK"
    assert m.counts() == tuple((u, 0) for u in LEVELS) and m.total == 0
    m.on_event(post("READ-TARGET", urgency=7))
    m.on_event(post("LINK-TO-PNET", "CYTO-TARGET", urgency=7))
    m.on_event(post("LOOK-FOR-NEW-BLOCK", urgency=4))
    assert m.bin(7) == (Codelet("LINK-TO-PNET", ("CYTO-TARGET",), 7),
                        Codelet("READ-TARGET", (), 7))
    assert m.counts() == ((600, 0), (300, 0), (7, 2), (4, 1), (1, 0), (0, 0))
    assert m.total == 3
    assert m.last_posted == Codelet("LOOK-FOR-NEW-BLOCK", (), 4)
    assert m.changed == (4,)


def test_codelet_text_reads_like_the_lisp_form():
    text = coderack_model.codelet_text
    assert text(Codelet("LOOK-FOR-NEW-BLOCK", (), 4)) == "look-for-new-block"
    obj = {"obj": "cyto-node", "name": "CYTO-BRICK4"}
    assert text(Codelet("CONST-BLX", (obj, {"obj": "cyto-node", "name": "CYTO-BRICK1"}, 84),
                        150)) == "const-blx CYTO-BRICK4 CYTO-BRICK1 84"
    assert text(Codelet("TEST-IF-POSSIBLE-AND-DESIRABLE", (obj, {"str": "const-blx"}), 7)) \
        == 'test-if-possible-and-desirable CYTO-BRICK4 "const-blx"'
    assert text(Codelet("ACTIVATE", (165.46666666666667, "NODE-150", True), 7)) \
        == "activate 165.5 NODE-150 t"
    assert text(Codelet("DECOMPI", (obj, ("QUOTE", (0, 0, 1))), 7)) \
        == "decompi CYTO-BRICK4 '(0 0 1)"
    assert text(Codelet("X", (None, ":KEY", 2.0), 7)) == "x nil :KEY 2.0"


def test_bin_summary_groups_codelets_by_name_newest_first():
    m = made()
    for name in ("READ-TARGET", "LOOK-FOR-BL+", "READ-TARGET", "KILL-NODE"):
        m.on_event(post(name, urgency=7))
    assert coderack_model.bin_summary(m.bin(7)) == (
        ("KILL-NODE", 1), ("READ-TARGET", 2), ("LOOK-FOR-BL+", 1))
    assert coderack_model.bin_summary(()) == ()


def test_a_choice_removes_the_codelet_its_draws_name():
    m = made()
    for i in range(3):
        m.on_event(post("C", i, urgency=7))
    # Bin 7 is (C 2) (C 1) (C 0); the last draw (3 codelets, index 1) is C 1.
    m.on_event(chosen("C", (1,), 7, ((21, 5), (3, 1)), n=4))
    assert [c.args for c in m.bin(7)] == [(2,), (0,)]
    assert m.chosen == coderack_model.Choice(n=4, codelet=Codelet("C", (1,), 7), setup=False)
    assert m.changed == (7,)


def test_an_all_zero_rack_chooses_with_one_draw():
    m = made()
    m.on_event(post("Z", urgency=0))
    m.on_event(post("Y", urgency=0))
    m.on_event(observe.SetupChoose(codelet="Z", args=(), urgency=0,
                                   rack=m.counts(), draws=((2, 1),)))
    assert m.bin(0) == (Codelet("Y", (), 0),)
    assert m.chosen.setup and m.chosen.codelet.codelet == "Z"


def test_an_empty_choice_changes_nothing():
    m = made()
    m.on_event(chosen(None, None, None, ()))
    assert m.total == 0 and m.chosen == coderack_model.Choice(n=0, codelet=None, setup=False)


def test_a_choice_that_does_not_match_is_an_error():
    m = made()
    m.on_event(post("C", 0, urgency=7))
    with pytest.raises(CoderackModelError):
        m.on_event(chosen("D", (), 7, ((1, 0),)))
    m = made()
    m.on_event(post("C", 0, urgency=7))
    with pytest.raises(CoderackModelError):
        m.on_event(chosen("C", (0,), 7, ((2, 0),)))     # the bin has 1 codelet


def test_the_rack_field_is_checked():
    m = made()
    m.on_event(post("C", urgency=7))
    m.on_event(observe.IterationBegan(n=0, x=12, temperature=50, rack=m.counts()))
    with pytest.raises(CoderackModelError):
        m.on_event(observe.IterationBegan(n=1, x=13, temperature=50,
                                          rack=tuple((u, 0) for u in LEVELS)))


def test_rack_emptied_and_reset():
    m = made()
    m.on_event(post("C", urgency=7))
    m.on_event(post("D", urgency=1))
    m.on_event(observe.RackEmptied())
    assert m.total == 0 and m.counts() == tuple((u, 0) for u in LEVELS)
    m.on_event(start_event())
    assert m == CoderackModel() and m.name is None and m.counts() == ()


def test_coderack_models_compare_by_contents():
    a, b = made(), made()
    a.on_event(post("C", urgency=7))
    assert a != b
    b.on_event(post("C", urgency=7))
    assert a == b
    # The last choice and post are not part of the compared state.
    a.on_event(chosen("C", (), 7, ((1, 0),)))
    b.on_event(post("D", urgency=4))
    b.on_event(chosen("D", (), 4, ((1, 0),)))
    b.on_event(chosen("C", (), 7, ((1, 0),)))
    assert a == b and a.last_posted != b.last_posted


# ---------------------------------------------------------------------------
# Every event of the 11 chapter puzzles x seed 1, against the World

class WorldComparer:
    """After every event, the models (subscribed before this) equal the
    state derived from the World."""

    def __init__(self, pnet, rack):
        self.pnet, self.rack = pnet, rack
        self.compared = 0
        self.kinds = {}

    def on_event(self, event):
        world = events.current.world
        assert self.pnet == PnetModel.from_world(world), event
        assert self.rack == CoderackModel.from_world(world), event
        if isinstance(event, (observe.CodeletChosen, observe.SetupChoose)) \
                and event.codelet is not None:
            assert self.rack.chosen.codelet == Codelet(event.codelet, event.args, event.urgency)
        self.compared += 1
        self.kinds[event.kind] = self.kinds.get(event.kind, 0) + 1


def checked_run(puzzle, seed, cap=20000):
    """A process-pool job: run with both models and the comparer; plain data
    (or the failure)."""
    try:
        pnet, rack = PnetModel(), CoderackModel()
        comparer = WorldComparer(pnet, rack)
        errors = []
        subject = observe.Subject(on_error=errors.append)
        for o in (pnet, rack, comparer):
            subject.subscribe(o)
        result = harness.run_config(list(PUZZLES[puzzle - 1]), seed=seed, max_iterations=cap,
                                    out=io.StringIO(), observers=[subject])
        if errors:
            return {"failure": "".join(traceback.format_exception(errors[0].exception))}
        return {"failure": None, "outcome": result["outcome"], "kinds": comparer.kinds,
                "compared": comparer.compared, "marked": len(pnet.with_instances()),
                "max_total": rack.max_total}
    except Exception:
        return {"failure": traceback.format_exc()}


RUNS = [(p, 1) for p in range(1, 12)]


@pytest.fixture(scope="module")
def summaries():
    with concurrent.futures.ProcessPoolExecutor(
            max_workers=min(len(RUNS), os.cpu_count() or 1)) as pool:
        jobs = {run: pool.submit(checked_run, *run) for run in RUNS}
        return {run: job.result() for run, job in jobs.items()}


@pytest.mark.parametrize("puzzle,seed", RUNS)
def test_the_models_equal_the_world_after_every_event(summaries, puzzle, seed):
    s = summaries[puzzle, seed]
    assert s["failure"] is None, s["failure"]
    kinds = s["kinds"]
    assert kinds["coderack-created"] == 1 and kinds["pnet-initialized"] == 1
    assert kinds["pnodes-changed"] > 0 and kinds["post"] > 0
    assert s["compared"] == sum(kinds.values())
    assert s["marked"] > 0


def test_the_runs_cover_every_outcome_and_the_rest(summaries):
    assert {s["outcome"] for s in summaries.values()} >= {"solved", "capped"}
    kinds = {}
    for s in summaries.values():
        for k, n in s["kinds"].items():
            kinds[k] = kinds.get(k, 0) + n
    assert kinds["rack-emptied"] > 0 and kinds["pnet"] > 0 and kinds["codelet-chosen"] > 1000


def test_the_models_change_nothing_in_a_run():
    def run(observers):
        out, trace = io.StringIO(), io.StringIO()
        result = harness.run_config(list(PUZZLES[0]), seed=1, max_iterations=20000, out=out,
                                    trace=trace, observers=observers)
        return result, out.getvalue(), trace.getvalue()

    assert run([]) == run([PnetModel(), CoderackModel()])
