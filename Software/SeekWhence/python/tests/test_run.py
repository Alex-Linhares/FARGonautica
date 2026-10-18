"""The run loop: run until Call-term, an empty rack or a cap [M86 pp. 41-42, 125]
(DECISIONS D-007, D-011, D-012, D-013)."""

import pytest

from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.run import NOT_BUILT, TASKS, new_world, run_until_prompt


def spawn(w, type, *args):
    """A test handler: post one task of `type` (a task "may set out other tasks"
    [M86 p. 41])."""
    w.post(type, *args)


def make(seed=1, **knobs):
    recorder = Recorder()
    world = new_world(Config(**knobs), seed, observers=[recorder])
    world.handlers["Tester"] = lambda w, *args: None
    world.handlers["Bonder"] = lambda w, *args: None
    world.handlers["Sparkler"] = lambda w, *args: spawn(w, "Tester")
    return world, recorder


def test_new_world_installs_call_term_and_publishes_the_start():
    """`-> (startup)` [M86 p. 164]; `Call-term` is the loop's own task (DECISION D-012)."""
    world, recorder = make(seed=42)
    assert "Call-term" in TASKS and world.handlers["Call-term"] is TASKS["Call-term"]
    start = recorder.of_kind("start")[0]
    assert start.seed == 42 and dict(start.config)["max_tasks"] == 200_000
    assert new_world().seed == 0


def test_the_loop_stops_when_a_call_term_runs():
    """"At this point, the system typically calls for the next term" [M86 p. 125]
    (DECISION D-012)."""
    world, recorder = make()
    world.post("Call-term")
    assert run_until_prompt(world) == "call-term"
    assert world.clock == 1
    prompt = recorder.of_kind("prompt")[0]
    assert (prompt.reason, prompt.tasks) == ("call-term", 1)
    chosen = recorder.of_kind("chosen")[0]
    assert (chosen.type, chosen.args) == ("Call-term", ())


def test_tasks_before_the_call_term_all_run_first_when_more_urgent():
    world, recorder = make()
    for _ in range(5):
        world.post("Tester", urgency=1000)
    world.post("Call-term", urgency=1)
    # With weights 1000 against 1 the Call-term is (almost surely) chosen last.
    assert run_until_prompt(world) == "call-term"
    types = [e.type for e in recorder.of_kind("chosen")]
    assert types[-1] == "Call-term" and len(types) == 6


def test_an_empty_rack_prints_empty_task_and_prompts():
    """`doing task empty-task` after the first term of run 3 [M86 p. 170] (DECISION D-013)."""
    world, recorder = make()
    assert run_until_prompt(world) == "empty"
    chosen = recorder.of_kind("chosen")
    assert len(chosen) == 1 and chosen[0].type == "empty-task" and chosen[0].number is None
    assert chosen[0].arena == "user"
    assert recorder.of_kind("prompt")[0].reason == "empty"
    assert world.clock == 1


def test_the_per_term_cap_stops_the_loop_and_prompts():
    """DECISION D-011: `max_tasks_per_term` tasks between two prompts."""
    world, recorder = make(max_tasks_per_term=10)
    for _ in range(50):                          # more Testers than the cap
        world.post("Tester")
    assert run_until_prompt(world) == "term-cap"
    assert world.clock == 10
    assert recorder.of_kind("prompt")[0].tasks == 10
    assert len(world.taskrack) >= 40


def test_the_whole_run_cap_ends_the_run_as_capped():
    """DECISION D-011: `max_tasks` for the whole run; the outcome is "capped"."""
    world, recorder = make(max_tasks=7, max_tasks_per_term=3)
    for _ in range(20):
        world.post("Tester")
    assert run_until_prompt(world) == "term-cap"
    assert run_until_prompt(world) == "term-cap"
    assert run_until_prompt(world) == "capped"
    assert world.clock == 7
    assert recorder.of_kind("capped")[0].clock == 7
    assert run_until_prompt(world) == "capped"   # and stays capped
    assert len(world.taskrack) == 13


def test_tasks_set_out_by_tasks_are_run():
    """"may set out other tasks" [M86 p. 41]: a Sparkler posts a Tester, which runs before
    the rack empties."""
    world, recorder = make()
    world.post("Sparkler")
    assert run_until_prompt(world) == "empty"
    assert [e.type for e in recorder.of_kind("chosen")] == ["Sparkler", "Tester", "empty-task"]


def test_same_seed_same_task_order_and_different_seeds_differ():
    """The same seed gives the same run."""
    def order(seed):
        world, recorder = make(seed)
        for n in range(30):
            world.post(("Tester", "Bonder", "Sparkler")[n % 3], urgency=(n % 7) + 1)
        world.post("Call-term", urgency=3)
        run_until_prompt(world)
        return [(e.type, e.number) for e in recorder.of_kind("chosen")]
    assert order(11) == order(11)
    assert order(11) != order(12)


def test_the_rng_draws_once_per_task_chosen():
    """DECISION D-007: one draw per choice and no other randomness in the loop."""
    world, _ = make()
    for _ in range(9):
        world.post("Tester")
    world.post("Call-term", urgency=1)
    run_until_prompt(world)
    assert world.rng.draws == world.clock


def test_a_missing_handler_surfaces_as_an_error():
    world, _ = make()
    del world.handlers["Glommer"]
    world.post("Glommer")
    with pytest.raises(LookupError):
        run_until_prompt(world)


def test_every_task_type_has_a_handler_and_the_placeholder_still_works():
    """Every task type of SPEC 2.4 but `empty-task` has a handler now (`NOT_BUILT` is
    empty); the placeholder mechanism, kept for extensions, runs as a no-op and publishes
    `TaskStale` with the reason "not built yet" (DECISION D-009)."""
    from seekwhence.config import TASK_TYPES
    from seekwhence.run import not_built
    world, recorder = make()
    assert NOT_BUILT == ()
    assert all(t in TASKS for t in TASK_TYPES if t != "empty-task")
    world.handlers["Slip-scout"] = not_built("Slip-scout")
    world.post("Slip-scout", "glint1")
    assert run_until_prompt(world) == "empty"
    stale = recorder.of_kind("stale")
    assert stale[-1].type == "Slip-scout" and stale[-1].args == ("glint1",)
    assert stale[-1].reason == "not built yet"
