"""The World: arenas, names, existence checks, posting and cleaning, terms
[M86 pp. 42-47] (DECISIONS D-008, D-009, D-010, D-014, D-057)."""

import pytest

from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.world import NAMED_KINDS, Arena, World


class Thing:
    def __init__(self, name):
        self.name = name


def make_world(seed=1, **knobs):
    world = World(Config(**knobs), seed)
    recorder = world.events.subscribe(Recorder())
    return world, recorder


# ---- arenas and names -----------------------------------------------------------------------

def test_the_world_has_the_three_arenas():
    """"three levels" [M86 p. 43]: cytoplasm, socratoplasm, platoplasm."""
    world, _ = make_world()
    assert world.cytoplasm.level == "cytoplasm"
    assert world.socratoplasm.level == "socratoplasm"
    assert world.platoplasm.atom(5).value == 5


def test_names_count_from_one_per_kind_and_are_never_reused():
    """DECISION D-014: `glint1`, `glint2`, ...; a destroyed structure's number is not reused."""
    world, _ = make_world()
    assert world.name("glint") == "glint1"
    assert world.name("glint") == "glint2"
    assert world.name("glom") == "glom1"
    world.cytoplasm.add(Thing("glom1"))
    world.cytoplasm.remove("glom1")
    assert world.name("glom") == "glom2"
    assert set(world.counters) == set(NAMED_KINDS) | {"task"}
    with pytest.raises(ValueError):
        world.name("widget")


def test_the_box_counters_are_the_worlds():
    """`BoxTree` takes the world's counters, so boxes and printstructures are numbered per
    run (DECISIONS D-014, D-056)."""
    from seekwhence.forms import parse
    from seekwhence.hypothesis import Hypothesis
    world, _ = make_world()
    Hypothesis(parse("(Cycle 3 2 (3 7))"), world.counters)
    assert world.counters["box"] >= 1 and world.counters["pstruc"] >= 1
    assert world.name("box") == f"box{world.counters['box']}"


def test_arena_add_find_remove_and_order():
    arena = Arena("cytoplasm")
    a, b = arena.add(Thing("glint1")), arena.add(Thing("glint2"))
    assert arena.names == ("glint1", "glint2") and len(arena) == 2
    assert "glint1" in arena and arena.find("glint1") is a
    assert list(arena) == [a, b]
    assert arena.remove("glint1") is a
    assert arena.find("glint1") is None and "glint1" not in arena
    with pytest.raises(KeyError):
        arena.remove("glint1")
    with pytest.raises(ValueError):
        arena.add(Thing("glint2"))
    with pytest.raises(TypeError):
        arena.add(object())


def test_exists_looks_in_the_cytoplasm_and_the_socratoplasm():
    """DECISION D-009: the check a task makes first, "to see that both gloms still exist"
    [M86 p. 64]."""
    world, _ = make_world()
    world.cytoplasm.add(Thing("glom1"))
    world.socratoplasm.add(Thing("gnoth1"))
    assert world.exists("glom1") and world.exists("gnoth1") and world.exists("glom1", "gnoth1")
    assert not world.exists("glom2") and not world.exists("glom1", "glom2")
    assert world.find("gnoth1").name == "gnoth1" and world.find("glom9") is None
    world.cytoplasm.remove("glom1")
    assert not world.exists("glom1")


# ---- posting -------------------------------------------------------------------------------

def test_post_uses_the_config_urgency_and_stamps_the_task():
    """DECISION D-008: the type's urgency from the table; the task records the clock and the
    terms entered."""
    world, recorder = make_world()
    world.enter_term(3)
    task = world.post("Tester")
    assert task.urgency == dict(Config().urgencies)["Tester"]
    assert (task.number, task.created_at, task.at_term, task.arena) == (1, 0, 1, "cytoplasm")
    posted = recorder.of_kind("posted")[0]
    assert (posted.number, posted.type, posted.urgency, posted.arena) == (1, "Tester", 10, "cytoplasm")
    assert world.taskrack.tasks == (task,)


def test_post_with_an_explicit_urgency_and_arguments():
    """`doing task Sparkler-plus on (glint2 glint1 10)` [M86 p. 164] (DECISION D-015: the
    number is its urgency)."""
    world, recorder = make_world()
    task = world.post("Sparkler-plus", "glint2", "glint1", 10, urgency=10)
    assert task.args == ("glint2", "glint1", 10) and task.urgency == 10
    assert recorder.of_kind("posted")[0].args == ("glint2", "glint1", 10)


def test_task_numbers_count_every_task_posted():
    world, _ = make_world()
    for _ in range(3):
        world.post("Sparkler")
    assert [t.number for t in world.taskrack.tasks] == [1, 2, 3]
    assert world.counters["task"] == 3


def test_post_refuses_unknown_types_and_sparkler_plus_without_urgency():
    world, _ = make_world()
    with pytest.raises(ValueError):
        world.post("Widget")
    with pytest.raises(KeyError):
        world.post("Sparkler-plus", "glint1", "glint1")


# ---- cleaning ------------------------------------------------------------------------------

def test_entering_a_term_cleans_the_cyto_types_posted_before_the_previous_term():
    """DECISION D-010: Sparklers, Sparkler-pluses, Testers and Dissolvers "created before
    the previous term" are removed when a new term is entered."""
    world, recorder = make_world()
    world.enter_term(3)                         # term 1
    world.post("Sparkler")                      # at_term 1: old once term 3 arrives
    world.post("Glom-scout")                    # not a cleaned type
    world.enter_term(7)                         # term 2: nothing posted before term 1
    assert recorder.of_kind("term")[-1].cleaned == 0
    world.post("Tester")                        # at_term 2
    n = world.enter_term(3)                     # term 3: the Sparkler of term 1 goes
    assert n == 3
    assert [t.type for t in world.taskrack.tasks] == ["Glom-scout", "Tester"]
    event = recorder.of_kind("term")[-1]
    assert (event.n, event.term, event.cleaned) == (3, 3, 1)
    assert recorder.of_kind("cleaned")[-1].removed == (("Sparkler", 1),)
    assert world.terms == [3, 7, 3]


def test_the_reform_and_hypothesis_cleanings():
    """DECISION D-010: a Gnoth-setter removes the reformulation types; an accepted
    hypothesis removes the template types."""
    world, recorder = make_world()
    for t in ("Reformulator", "Gnoth-operator", "Template-scout", "Hfilter", "Capper"):
        world.post(t)
    assert world.clean("reform") == 3
    assert [t.type for t in world.taskrack.tasks] == ["Template-scout", "Hfilter"]
    assert world.clean("hypothesis") == 1
    assert [t.type for t in world.taskrack.tasks] == ["Hfilter"]
    assert world.clean("hypothesis") == 0
    moments = [e.moment for e in recorder.of_kind("cleaned")]
    assert moments == ["reform", "hypothesis"]
    with pytest.raises(ValueError):
        world.clean("lunch")


def test_cleaning_lists_are_knobs():
    world, _ = make_world(cleaned_on_term=("Bonder",))
    world.enter_term(1)
    world.post("Bonder")
    world.post("Sparkler")
    world.enter_term(2)
    world.enter_term(3)
    assert [t.type for t in world.taskrack.tasks] == ["Sparkler"]


# ---- terms, stale tasks, messages ---------------------------------------------------------

def test_enter_term_accepts_only_nonnegative_integers():
    """"sequences of nonnegative integers" [MHN p. 3]."""
    world, _ = make_world()
    assert world.enter_term(0) == 1
    for bad in (-1, 2.5, "3", True):
        with pytest.raises(ValueError):
            world.enter_term(bad)


def test_run_task_calls_the_handler_and_advances_the_clock():
    world, recorder = make_world()
    calls = []
    world.handlers["Bonder"] = lambda w, *args: calls.append(args)
    task = world.post("Bonder", "glint1", "glint3")
    world.run_task(task)
    assert calls == [("glint1", "glint3")] and world.clock == 1
    chosen = recorder.of_kind("chosen")[0]
    assert (chosen.clock, chosen.number, chosen.type, chosen.args) == (1, 1, "Bonder", ("glint1", "glint3"))


def test_run_task_without_a_handler_is_a_program_error():
    world, _ = make_world()
    with pytest.raises(LookupError):
        world.run_task(world.post("Bonder"))


def test_a_stale_task_is_a_no_op_that_reports_itself():
    """"Such a task will probably do nothing, because the structures on which it was
    designed to operate no longer exist" [M86 p. 42] (DECISION D-009)."""
    world, recorder = make_world()
    acted = []

    def dissolver(w, name):
        if not w.exists(name):
            w.stale("Dissolver", (name,), f"{name} is not in cytoplasm")
            return
        acted.append(name)
        w.cytoplasm.remove(name)

    world.handlers["Dissolver"] = dissolver
    world.cytoplasm.add(Thing("glom3"))
    first, second = world.post("Dissolver", "glom3"), world.post("Dissolver", "glom3")
    world.run_task(first)
    assert acted == ["glom3"] and not world.exists("glom3")
    before = (world.cytoplasm.names, world.socratoplasm.names, dict(world.counters), world.terms[:])
    world.run_task(second)
    assert acted == ["glom3"]
    assert (world.cytoplasm.names, world.socratoplasm.names, dict(world.counters), world.terms) == before
    stale = recorder.of_kind("stale")
    assert len(stale) == 1
    assert (stale[0].type, stale[0].args, stale[0].reason) == ("Dissolver", ("glom3",), "glom3 is not in cytoplasm")


def test_say_publishes_a_message():
    world, recorder = make_world()
    world.say("gnoths constructed")
    assert recorder.of_kind("message")[0].text == "gnoths constructed"


def test_config_pairs_lists_every_field():
    from dataclasses import fields
    world, _ = make_world(verbose=False)
    pairs = dict(world.config_pairs())
    assert set(pairs) == {f.name for f in fields(Config)}
    assert pairs["verbose"] is False


def test_a_fresh_world_has_no_shared_state_yet():
    world, _ = make_world()
    assert world.hypothesis is None and world.template is None and world.reigning is None
    assert world.threshold is None and world.freeze_dried == []
    assert world.clock == 0 and world.terms == [] and len(world.taskrack) == 0
    assert world.rng.seed == 1 and world.seed == 1
