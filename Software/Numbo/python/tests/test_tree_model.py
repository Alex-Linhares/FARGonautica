"""Loop0003 item 2: the tree (AST) model (numbo/models/tree_model.py).

The model is a shadow of the cytoplasm, by node name, built either from the
World (TreeModel.from_world) or incrementally from a run's events
(TreeModel as an observer).  Its forest and expressions are pure functions
of that state.  These tests check:
  - the forest and the expressions on small hand-made event sequences
    (blocks, difference blocks, a decomposition, replace-target, kills,
    "Obvious.");
  - the incremental model equals from_world after every event of the 11
    chapter puzzles x seeds 1-3;
  - a solved run's final expression is check_solution's, and the model's
    decomposition is the oracle's decompose output, step for step;
  - the kill-block-gap run (puzzle 3 seed 8) shows the orphaned operation
    node, with the killed block under it, as decompose prints it.
"""

import concurrent.futures
import io
import os
import traceback

import pytest

from conftest import early_results
from full_runs import PUZZLES
from numbo import events, harness, observe
from numbo.models.tree_model import TreeModel, TreeNode
from numbo.solution_checker import check_solution


# ---------------------------------------------------------------------------
# Hand-made event sequences

def created(name, type, value, level, status="free", activation=50, success=1):
    return observe.NodeCreated(name=name, type=type, value=value, status=status,
                               level=level, activation=activation, success=success)


def op_created(name, result, op1, op2, level):
    return observe.OpNodeCreated(name=name, op="".join(c for c in name.split("-")[0]
                                                       if c.isalpha()),
                                 result=result, operands=(op1, op2), level=level)


def changed(name, field, value):
    return observe.NodeChanged(name=name, field=field, value=value)


def link(model, op, result, op1, op2, level):
    """The events of create-op-node: the op node, then its three neighbors'
    update-neighbors (RESULT first), as the engine publishes them."""
    evs = [op_created(op, result, op1, op2, level)]
    for name, link_type in ((result, "RESULT"), (op1, "OPERAND"), (op2, "OPERAND")):
        old = model.node(name).neighbors if model.has(name) else None
        evs.append(changed(name, "neighbors", ((op, link_type),) + (old or ())))
    for e in evs:
        model.on_event(e)


def puzzle1_start():
    """Puzzle 1 (114 from 11 20 7 1 6): the target and the five bricks."""
    m = TreeModel()
    m.on_event(observe.RunStarted(problem=PUZZLES[0], seed=1, max_iterations=None,
                                  rng="splitmix64", pnet=()))
    m.on_event(created("CYTO-TARGET", "1t", 114, 99, activation=150, success=0))
    for i, v in enumerate(PUZZLES[0][1:], 1):
        m.on_event(created(f"CYTO-BRICK{i}", "2b", v, 1))
    return m


def test_an_empty_model_has_an_empty_forest():
    f = TreeModel().forest()
    assert f.targets == f.blocks == f.bricks == f.ghosts == f.loose == ()


def test_the_target_and_the_free_bricks():
    m = puzzle1_start()
    f = m.forest()
    assert [t.name for t in f.targets] == ["CYTO-TARGET"]
    assert f.targets[0].type == "1t" and f.targets[0].children == ()
    # Bricks in brick order, whatever the creation order.
    assert [b.name for b in f.bricks] == [f"CYTO-BRICK{i}" for i in range(1, 6)]
    assert [b.value for b in f.bricks] == [11, 20, 7, 1, 6]
    assert f.blocks == f.ghosts == f.loose == ()
    assert m.expression("CYTO-BRICK2") == "20"
    assert m.expression("CYTO-TARGET") == "[114]"   # not derived yet


def build_120_and_6(m):
    """20 x 6 = 120 (a product block) and 7 - 1 = 6 (a difference block:
    const-bl+ makes the larger brick the RESULT)."""
    m.on_event(changed("CYTO-BRICK2", "status", "linked"))
    m.on_event(changed("CYTO-BRICK5", "status", "linked"))
    m.on_event(created("CYTO-BLOCK120-V1", "4bl", 120, 3, activation=300))
    link(m, "TIMES20-6-V1", "CYTO-BLOCK120-V1", "CYTO-BRICK2", "CYTO-BRICK5", 2)
    m.on_event(changed("CYTO-BRICK3", "status", "linked"))
    m.on_event(changed("CYTO-BRICK4", "status", "linked"))
    m.on_event(created("CYTO-BLOCK6-V2", "4bl", 6, 3))
    link(m, "PLUS1-6-V2", "CYTO-BRICK3", "CYTO-BRICK4", "CYTO-BLOCK6-V2", 2)


def test_blocks_are_expression_trees_over_bricks():
    m = puzzle1_start()
    build_120_and_6(m)
    f = m.forest()
    assert [b.name for b in f.blocks] == ["CYTO-BLOCK120-V1", "CYTO-BLOCK6-V2"]
    assert [b.name for b in f.bricks] == ["CYTO-BRICK1"]
    times = f.blocks[0].children[0]
    assert (times.name, times.type, times.op, times.symbol) == ("TIMES20-6-V1", "5g", "TIMES", "x")
    # The operands as decompose prints them (sup reverses the op's neighbors).
    assert [c.name for c in times.children] == ["CYTO-BRICK5", "CYTO-BRICK2"]
    assert m.expression("CYTO-BLOCK120-V1") == "6 x 20"
    minus = f.blocks[1].children[0]
    assert minus.symbol == "-"
    assert [c.name for c in minus.children] == ["CYTO-BRICK3", "CYTO-BRICK4"]
    assert m.expression("CYTO-BLOCK6-V2") == "7 - 1"
    assert m.equation("CYTO-BLOCK6-V2") == "6 = 7 - 1"


def decompose_114(m):
    """decomp+ of the target 114 by the block 120: 114 = 120 - 6, the
    derived target 6 (the block is larger, so it is the RESULT)."""
    m.on_event(created("CYTO-TARGET-6-V3", "3dt", 6, 97, activation=200, success=0))
    m.on_event(changed("CYTO-TARGET", "status", "linked"))
    m.on_event(changed("CYTO-BLOCK120-V1", "status", "linked"))
    m.on_event(observe.CurrentTargetChanged(name="CYTO-TARGET-6-V3", interest=50))
    link(m, "PLUS114-6-V3", "CYTO-BLOCK120-V1", "CYTO-TARGET", "CYTO-TARGET-6-V3", 98)


def test_a_decomposition_hangs_under_the_target():
    m = puzzle1_start()
    build_120_and_6(m)
    decompose_114(m)
    f = m.forest()
    assert [t.name for t in f.targets] == ["CYTO-TARGET"]
    assert [b.name for b in f.blocks] == ["CYTO-BLOCK6-V2"]
    (op,) = f.targets[0].children
    assert op.name == "PLUS114-6-V3" and op.symbol == "-"
    assert [c.name for c in op.children] == ["CYTO-BLOCK120-V1", "CYTO-TARGET-6-V3"]
    assert op.children[0].children[0].name == "TIMES20-6-V1"
    assert m.current_target == "CYTO-TARGET-6-V3"
    assert m.expression("CYTO-TARGET") == "(6 x 20) - [6]"


def replace_6(m):
    """replace-target: the block 6 takes the derived target 6's place."""
    m.on_event(changed("CYTO-BLOCK6-V2", "status", "linked"))
    m.on_event(changed("CYTO-BLOCK6-V2", "neighbors",
                       (("PLUS114-6-V3", "OPERAND"), ("PLUS1-6-V2", "OPERAND"))))
    m.on_event(changed("PLUS114-6-V3", "neighbors",
                       (("CYTO-BLOCK6-V2", "OPERAND"), ("CYTO-TARGET", "OPERAND"),
                        ("CYTO-BLOCK120-V1", "RESULT"))))
    m.on_event(observe.CurrentTargetChanged(name="CYTO-TARGET", interest=100))
    m.on_event(observe.NodeKilled(name="CYTO-TARGET-6-V3", cytoplasm=(
        "CYTO-BRICK1", "CYTO-BRICK2", "CYTO-BRICK3", "CYTO-BRICK4", "CYTO-BRICK5",
        "CYTO-TARGET", "CYTO-BLOCK120-V1", "TIMES20-6-V1", "CYTO-BLOCK6-V2", "PLUS1-6-V2",
        "PLUS114-6-V3")))
    m.on_event(observe.TargetReplaced(target="CYTO-TARGET-6-V3", block="CYTO-BLOCK6-V2",
                                      rebound=False))


def test_replace_target_completes_the_target_tree():
    m = puzzle1_start()
    build_120_and_6(m)
    decompose_114(m)
    replace_6(m)
    f = m.forest()
    assert f.blocks == () and [b.name for b in f.bricks] == ["CYTO-BRICK1"]
    (op,) = f.targets[0].children
    # replace-neighbors reversed the op's neighbors; decompose's order follows.
    assert [c.name for c in op.children] == ["CYTO-BLOCK120-V1", "CYTO-BLOCK6-V2"]
    assert m.equation("CYTO-TARGET") == "114 = (6 x 20) - (7 - 1)"
    # The replaced derived target is a ghost now.
    assert [g.name for g in f.ghosts] == ["CYTO-TARGET-6-V3"]
    assert not f.ghosts[0].alive and f.ghosts[0].status == "free"
    assert m.decomposition() == (
        observe.DecompositionStep("PLUS114-6-V3", "CYTO-BLOCK120-V1", 120,
                                  "CYTO-BLOCK6-V2", 6, "CYTO-TARGET"),
        observe.DecompositionStep("TIMES20-6-V1", "CYTO-BRICK5", 6, "CYTO-BRICK2", 20,
                                  "CYTO-BLOCK120-V1"),
        observe.DecompositionStep("PLUS1-6-V2", "CYTO-BRICK4", 1, "CYTO-BRICK3", 7,
                                  "CYTO-BLOCK6-V2"))


def test_killed_nodes_become_ghosts_and_their_operands_are_freed():
    m = puzzle1_start()
    build_120_and_6(m)
    n = m.events_seen
    # kill-block of 6: disconnect(block 6, PLUS1-6-V2), in the engine's order.
    m.on_event(observe.Disconnect(node="CYTO-BLOCK6-V2", node_type="4bl", node_value=6,
                                  op="PLUS1-6-V2", op_type="5g", op_value=None))
    m.on_event(changed("CYTO-BRICK3", "status", "free"))
    m.on_event(changed("CYTO-BRICK4", "status", "free"))
    m.on_event(changed("CYTO-BRICK3", "neighbors", None))
    m.on_event(changed("CYTO-BRICK4", "neighbors", None))
    m.on_event(changed("CYTO-BLOCK6-V2", "neighbors", None))
    left = [x for x in m.cytoplasm if x != "PLUS1-6-V2"]
    m.on_event(observe.NodeKilled(name="PLUS1-6-V2", cytoplasm=tuple(reversed(left))))
    left = [x for x in m.cytoplasm if x != "CYTO-BLOCK6-V2"]
    m.on_event(observe.NodeKilled(name="CYTO-BLOCK6-V2", cytoplasm=tuple(reversed(left))))
    m.on_event(changed("CYTO-BLOCK6-V2", "status", "killed"))
    f = m.forest()
    assert [b.name for b in f.blocks] == ["CYTO-BLOCK120-V1"]
    assert [b.name for b in f.bricks] == ["CYTO-BRICK1", "CYTO-BRICK3", "CYTO-BRICK4"]
    assert {g.name: g.status for g in f.ghosts} == {"CYTO-BLOCK6-V2": "killed",
                                                    "PLUS1-6-V2": None}
    # Ghosts know when they left (the view fades them); from_world can't.
    assert m.ghost_since == {"PLUS1-6-V2": n + 7, "CYTO-BLOCK6-V2": n + 8}
    assert m.ghost_age("PLUS1-6-V2") == 2 and m.ghost_age("CYTO-BRICK1") is None


def test_obvious_rebinds_the_solution_root_to_the_block():
    m = TreeModel()
    m.on_event(created("CYTO-TARGET", "1t", 6, 99, success=0))
    for i, v in enumerate((3, 3, 17, 11, 22), 1):
        m.on_event(created(f"CYTO-BRICK{i}", "2b", v, 1))
    m.on_event(created("CYTO-BLOCK6-V1", "4bl", 6, 3))
    link(m, "PLUS3-3-V1", "CYTO-BLOCK6-V1", "CYTO-BRICK1", "CYTO-BRICK2", 2)
    assert m.solution_root == "CYTO-TARGET"
    m.on_event(observe.TargetReplaced(target="CYTO-TARGET", block="CYTO-BLOCK6-V1",
                                      rebound=True))
    assert m.solution_root == "CYTO-BLOCK6-V1"
    assert m.equation(m.solution_root) == "6 = 3 + 3"
    f = m.forest()
    assert [t.name for t in f.targets] == ["CYTO-TARGET"]
    assert [b.name for b in f.blocks] == ["CYTO-BLOCK6-V1"]
    assert f.blocks[0].solution and not f.targets[0].solution


def test_a_new_run_resets_the_model():
    m = puzzle1_start()
    m.on_event(observe.RunStarted(problem=PUZZLES[1], seed=1, max_iterations=None,
                                  rng="splitmix64", pnet=()))
    assert m == TreeModel() and m.forest().bricks == ()


def test_tree_nodes_are_frozen():
    node = puzzle1_start().forest().bricks[0]
    assert isinstance(node, TreeNode)
    with pytest.raises(AttributeError):
        node.value = 3


def test_changed_names_what_the_latest_event_changed():
    """For a view's highlight (item 8): `changed`, the names the latest event
    changed (empty when it changed nothing in the trees), and `last_change`
    / `last_change_at`, the latest event that did change something."""
    m = puzzle1_start()
    assert m.changed == frozenset({"CYTO-BRICK5"})
    assert m.last_change == frozenset({"CYTO-BRICK5"}) and m.last_change_at == m.events_seen
    at = m.events_seen
    m.on_event(observe.IterationBegan(n=0, x=1, temperature=50, rack=()))
    assert m.changed == frozenset()
    assert m.last_change == frozenset({"CYTO-BRICK5"}) and m.last_change_at == at
    m.on_event(changed("CYTO-BRICK2", "activation", 120))
    assert m.changed == m.last_change == frozenset({"CYTO-BRICK2"})
    m.on_event(changed("CYTO-BRICK2", "plinks", ()))       # not a field the trees keep
    assert m.changed == frozenset()
    m.on_event(op_created("TIMES20-6-V1", "CYTO-BLOCK120-V1", "CYTO-BRICK2", "CYTO-BRICK5", 2))
    assert m.changed == frozenset({"TIMES20-6-V1"})
    m.on_event(observe.NodeKilled(name="CYTO-BRICK1", cytoplasm=()))
    assert m.changed == frozenset({"CYTO-BRICK1"})
    m.on_event(observe.CurrentTargetChanged(name="CYTO-TARGET", interest=1))
    assert m.changed == frozenset({"CYTO-TARGET"})
    m.on_event(observe.Disconnect(node="CYTO-BRICK3", node_type="2b", node_value=7,
                                  op="TIMES20-6-V1", op_type="5g", op_value=None))
    assert m.changed == frozenset({"CYTO-BRICK3", "TIMES20-6-V1"})
    m.on_event(observe.TargetReplaced(target="CYTO-TARGET-6-V2", block="CYTO-BLOCK6-V3",
                                      rebound=False))
    assert m.changed == frozenset({"CYTO-TARGET-6-V2", "CYTO-BLOCK6-V3"})
    m.on_event(observe.RunStarted(problem=PUZZLES[1], seed=1, max_iterations=None,
                                  rng="splitmix64", pnet=()))
    assert m.changed == m.last_change == frozenset() and m.last_change_at is None


# ---------------------------------------------------------------------------
# The incremental model against from_world, on real runs

GHOST_WINDOW = 50   # events; the forest a view would draw
FULL_EVERY = 500    # events between comparisons of the full forests


def check_forest(model, forest):
    """Every live node is in exactly one tree, and no node is drawn twice;
    ghosts are gone from the cytoplasm."""
    names = [n.name for n in forest.walk()]
    assert len(names) == len(set(names)), "a node drawn twice"
    ghosts = {g.name for g in forest.ghosts}
    shown = {n.name for t in forest.trees() if t.name not in ghosts for n in t.walk()}
    assert set(model.cytoplasm) <= shown
    assert all(not g.alive for g in forest.ghosts)


class WorldComparer:
    """After every event: the incremental model (subscribed before this)
    equals TreeModel.from_world(the World).  A node-created event is
    published as the call begins, before the node is in the World, so then
    the model without that node must equal the World's, and the node itself
    is checked at the next event.  The forest is a function of that state;
    the view's (windowed) forest is built after every event, and the full
    forests are compared every FULL_EVERY events and at the end."""

    def __init__(self, model):
        self.model = model
        self.compared = 0
        self.forests = 0
        self.final = None

    def on_event(self, event):
        world = events.current.world
        got = TreeModel.from_world(world)
        creation = isinstance(event, (observe.NodeCreated, observe.OpNodeCreated))
        if creation:
            assert not got.has(event.name)
            assert self.model.without(event.name) == got, event
        else:
            assert self.model == got, event
        windowed = self.model.forest(ghost_window=GHOST_WINDOW)
        check_forest(self.model, windowed)
        assert all(self.model.ghost_age(g.name) <= GHOST_WINDOW for g in windowed.ghosts)
        self.compared += 1
        ended = isinstance(event, observe.RunEnded)
        if not creation and (ended or self.compared % FULL_EVERY == 0):
            full = self.model.forest()
            assert full == got.forest(), event
            check_forest(self.model, full)
            assert {g.name for g in windowed.ghosts} <= {g.name for g in full.ghosts}
            self.forests += 1
        if ended:
            self.final = got


def model_run(problem, seed, cap=20000):
    model, rec = TreeModel(), []
    comparer = WorldComparer(model)

    class Rec:
        def on_event(self, event):
            rec.append(event)

    errors = []
    subject = observe.Subject(on_error=errors.append)
    for o in (model, comparer, Rec()):
        subject.subscribe(o)
    out = io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap, out=out,
                                observers=[subject])
    assert not errors, "".join(traceback.format_exception(errors[0].exception))
    assert comparer.compared == len(rec) and comparer.final == model
    assert comparer.forests >= 1
    return result, out.getvalue(), model, rec


def checked_summary(puzzle, seed):
    """A process-pool job: model_run, as plain data (or the failure)."""
    problem = PUZZLES[puzzle - 1]
    try:
        result, out, model, evs = model_run(problem, seed)
    except Exception:
        return {"failure": traceback.format_exc()}
    summary = {"failure": None, "outcome": result["outcome"], "events": len(evs),
               "nodes": len(model.nodes),
               "roots": [t.name for t in model.forest().targets]}
    if result["outcome"] == "solved":
        summary["check"] = check_solution(out, list(problem))
        summary["equation"] = model.equation(model.solution_root)
        summary["decomposition"] = model.decomposition() == evs[-1].decomposition
    return summary


RUNS = [(p, s) for p in range(1, 12) for s in (1, 2, 3)]


# Started as soon as the tests are collected (conftest.py).
EARLY_JOBS = ("summaries", checked_summary, RUNS)


@pytest.fixture(scope="module")
def summaries():
    """The 33 runs, checked event by event in parallel (puzzle 3 seed 1, the
    longest, has 92k events and 888 nodes)."""
    early = early_results(__name__)
    if early is not None:
        return early
    with concurrent.futures.ProcessPoolExecutor(
            max_workers=min(len(RUNS), os.cpu_count() or 1)) as pool:
        jobs = {run: pool.submit(checked_summary, *run) for run in RUNS}
        return {run: job.result() for run, job in jobs.items()}


@pytest.mark.parametrize("puzzle,seed", RUNS)
def test_the_incremental_model_equals_from_world_after_every_event(summaries, puzzle, seed):
    summary = summaries[puzzle, seed]
    assert summary["failure"] is None, summary["failure"]
    assert summary["roots"][:1] == ["CYTO-TARGET"]
    if summary["outcome"] == "solved":
        # The final expression is the checker's, and the decomposition is
        # the one decompose printed.
        ok, reason, expression = summary["check"]
        assert ok, reason
        assert summary["equation"] == expression
        assert summary["decomposition"]


def test_the_runs_cover_long_runs_and_every_outcome(summaries):
    outcomes = {s["outcome"] for s in summaries.values()}
    assert {"solved", "gave-up", "capped"} <= outcomes
    assert max(s["events"] for s in summaries.values()) > 90000
    assert max(s["nodes"] for s in summaries.values()) > 800


def test_the_kill_block_gap_run_shows_the_orphaned_operation():
    """Puzzle 3 seed 8 reaches "Done :" through a block killed after a
    derived target was built on it (PORTING_NOTES.md item 10): its
    operation node stays in the cytoplasm, the killed block under it."""
    problem = PUZZLES[2]
    result, out, model, evs = model_run(problem, 8)
    assert result["outcome"] == "solved"
    ok, reason, _ = check_solution(out, list(problem))
    assert not ok and "is used but never derived" in reason
    assert model.decomposition() == evs[-1].decomposition
    # The killed block: out of the cytoplasm, status killed, yet an operand
    # of a live operation node in the target's tree.
    tree = model.forest().targets[0]
    orphans = [n for n in tree.walk() if n.type == "5g"
               and any(not c.alive for c in n.children)]
    assert len(orphans) == 1
    (dead,) = [c for c in orphans[0].children if not c.alive]
    assert dead.type == "4bl" and dead.status == "killed" and dead.children == ()
    assert dead.name in reason
    assert any(s.a == dead.name or s.b == dead.name for s in model.decomposition())
    # The expression shows it as an underived leaf, as the checker reads it.
    assert f"[{dead.value}]" in model.equation(model.solution_root)
    # It is in the tree, so not among the free-standing ghosts.
    assert dead.name not in [g.name for g in model.forest().ghosts]


# -- loop0003 item 11: the windowed forest is kept while nothing in it changes ------

@pytest.mark.parametrize("puzzle, seed", [(1, 1), (3, 8)])
def test_the_windowed_forest_is_kept_until_the_trees_or_its_ghosts_change(puzzle, seed):
    """At every event the windowed forest equals one built fresh; it is the
    same object as at the event before exactly when the event changed no
    tree and no ghost left the window (ghost ages are not in the forest)."""
    m = TreeModel()
    evs = []

    class Rec:
        def on_event(self, event):
            evs.append(event)

    harness.run_config(list(PUZZLES[puzzle - 1]), seed=seed, max_iterations=20000,
                       out=io.StringIO(), observers=[Rec()])
    prev, prev_ghosts, kept = None, None, 0
    for event in evs:
        m.on_event(event)
        f = m.forest(ghost_window=50)
        assert f == m._build_forest(ghost_window=50), event
        ghosts = tuple(m._recent_ghosts(50))
        if f is prev:
            kept += 1
            assert not m.changed or event.kind == "disconnect", event
            assert sorted(ghosts) == sorted(prev_ghosts), event
        elif prev is not None and event.kind not in ("start", "disconnect") and not m.changed:
            # A new forest with no change to the trees: a ghost left the window.
            assert sorted(ghosts) != sorted(prev_ghosts), event
        prev, prev_ghosts = f, ghosts
    assert kept > len(evs) // 2
