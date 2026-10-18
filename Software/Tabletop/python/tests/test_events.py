"""The typed events, the observer rules and the read-only snapshot (D-065;
`docs/engine-api.md`)."""

import dataclasses
import re
from pathlib import Path

import pytest

from tabletop import events as EV
from tabletop import snapshot as SN
from tabletop.config import PROFILES, V2_DEFAULTS
from tabletop.events import Collector
from tabletop.problems import load_problem
from tabletop.run import fingerprint, run_problem
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

HERE = Path(__file__).resolve().parent
ENGINE_API = HERE.parent / "docs" / "engine-api.md"
PLAIN = (int, float, str, bool, type(None))


def plain(value) -> bool:
    """Plain data only: numbers, strings, booleans, None and tuples of these (D-065)."""
    if isinstance(value, PLAIN):
        return True
    if isinstance(value, tuple):
        return all(plain(v) for v in value)
    return False


def collect(problem="surround-05", seed=3, profile="standard"):
    collector = Collector()
    result = run_problem(load_problem(problem), V2_PROFILES[profile], seed, [collector])
    return collector.events, result


@pytest.fixture(scope="module")
def run():
    return collect()


# ---- the event classes -------------------------------------------------------------------------


def test_every_event_is_a_frozen_dataclass_with_clock_and_draws_first():
    for cls in EV.EVENT_TYPES:
        assert dataclasses.is_dataclass(cls) and cls.__dataclass_params__.frozen
        names = [f.name for f in dataclasses.fields(cls)]
        assert names[:2] == ["clock", "draws"], cls.__name__
        assert issubclass(cls, EV.Event)


def test_event_names_are_unique_and_the_tuple_is_complete():
    names = [cls.__name__ for cls in EV.EVENT_TYPES]
    assert len(names) == len(set(names))
    defined = {name for name, obj in vars(EV).items()
               if isinstance(obj, type) and issubclass(obj, EV.Event) and obj is not EV.Event}
    assert defined == set(names)


def test_engine_api_names_every_event_class_and_every_field():
    """`docs/engine-api.md` lists every event with its fields, and names no event that does
    not exist (item 21)."""
    text = ENGINE_API.read_text()
    table = text[text.index("## The events"):text.index("## The read-only state")]
    documented = re.findall(r"^\| `([A-Z][A-Za-z]+)` \| (.*?) \|", table, flags=re.M)
    names = {cls.__name__ for cls in EV.EVENT_TYPES}
    assert {d for d, _ in documented} == names
    for name, fields_text in documented:
        cls = getattr(EV, name)
        for f in dataclasses.fields(cls)[2:]:
            assert f"`{f.name}`" in fields_text, f"{name}.{f.name} is not in engine-api.md"


# ---- a run's events ----------------------------------------------------------------------------


def test_a_run_starts_with_run_started_and_ends_with_run_ended(run):
    events, result = run
    assert events[0].name == "RunStarted" and events[-1].name == "RunEnded"
    assert events[-2].name == "AnswerGiven"
    assert sum(e.name == "RunStarted" for e in events) == 1
    assert sum(e.name == "RunEnded" for e in events) == 1
    first, last = events[0], events[-1]
    assert first.problem == "surround-05" and first.seed == 3 and first.profile == "standard"
    assert first.touched == "cup1" and first.objects[0] == "fork1"
    # the run is on the v2 defaults (D-114), so it records every field v3 changed
    assert {name for name, _ in first.changed} == {name for name, _ in V2_DEFAULTS}
    assert first.clock == 0 and first.draws == 0
    assert last.answer == result.answer and last.codelets == result.codelets
    assert last.structure_value == result.structure_value and last.capped == result.capped
    assert last.worldview == result.worldview


def test_every_event_carries_plain_data_and_a_non_decreasing_clock_and_draw_count(run):
    events, _ = run
    for e in events:
        for f in dataclasses.fields(e):
            assert plain(getattr(e, f.name)), f"{e.name}.{f.name} = {getattr(e, f.name)!r}"
    clocks = [e.clock for e in events]
    draws = [e.draws for e in events]
    assert clocks == sorted(clocks) and draws == sorted(draws)
    assert clocks[-1] == events[-1].codelets


def test_one_chosen_and_one_ran_event_per_codelet(run):
    events, result = run
    chosen = [e for e in events if e.name == "CodeletChosen"]
    ran = [e for e in events if e.name == "CodeletRan"]
    assert len(chosen) == len(ran) == result.codelets
    assert [e.clock for e in chosen] == list(range(1, result.codelets + 1))
    for c, r in zip(chosen, ran):
        assert (c.codelet, c.args) == (r.codelet, r.args)
        assert 0.0 < c.chance <= 1.0 and c.rack_size >= 1
    fizzled = [e for e in events if e.name == "CodeletFizzled"]
    assert len(fizzled) == sum(r.action == "fizzled" for r in ran)
    assert ran[-1].action == "answer" and chosen[-1].codelet == "find-answer"


def test_postings_removals_and_the_rack_add_up(run):
    """Every codelet on the rack was posted; what was chosen or removed was posted first;
    `CodeletPosted` and the `by` vocabulary (D-052 to D-056)."""
    events, result = run
    posted = [e for e in events if e.name == "CodeletPosted"]
    removed = [e for e in events if e.name == "CodeletRemoved"]
    chosen = [e for e in events if e.name == "CodeletChosen"]
    assert posted[0].by == "start" and posted[0].clock == 0
    assert len(posted) >= len(chosen) + len(removed)
    assert {e.reason for e in removed} <= {"age", "capacity"}
    assert all(e.urgency <= e.raw for e in posted)
    answers = [e for e in events if e.name == "AnswerCodeletPosted"]
    assert len(answers) == sum(e.by == "answer-schedule" for e in posted)


def test_the_slipnet_is_updated_before_the_first_codelet_and_after_every_one(run):
    events, result = run
    updates = [e for e in events if e.name == "SlipnetUpdated"]
    assert updates[0].clock == 0 and len(updates) == result.codelets + 1
    first = dict(updates[0].changed)
    assert first["opposite"] > 130 and first["cup"] > 45 and "liquid holder" in first
    jolts = [e for e in events if e.name == "NodeJolted"]
    assert jolts and all(j.amount > 0 and 1 <= j.activation <= 150 for j in jolts)
    assert all(j.by != "start" for j in jolts)


def test_structures_are_announced_in_order(run):
    """Every group, proposal and correspondence is announced before anything refers to
    it, with the engine's names (D-023, D-024, D-035)."""
    events, _ = run
    known = set()
    for e in events:
        if e.name == "GroupBuilt":
            assert re.fullmatch(r"g\d+", e.group) and e.group not in known
            known.add(e.group)
        elif e.name == "GroupProposed":
            assert re.fullmatch(r"p\d+", e.group)
            known.add(e.group)
        elif e.name == "GroupStagePassed":
            assert e.group in known and e.stage in (0, 1, 2)
        elif e.name == "GroupRemoved":
            assert e.group in known and e.reason in ("absorbed", "lost")
        elif e.name == "CorrespondenceProposed":
            assert re.fullmatch(r"cp\d+", e.correspondence) and e.level == "top"
            known.add(e.correspondence)
        elif e.name in ("CorrespondenceBuilt", "CorrespondenceTested", "CorrespondenceRemoved"):
            assert e.correspondence in known
        elif e.name == "CompetitionHeld":
            assert e.newcomer in known and all(r in known for r in e.rivals)
            assert 0.0 < e.chance <= 1.0
        elif e.name == "WorldviewChanged":
            assert all(n in known for n in e.added + e.removed)
            assert e.reason in ("no rival", "won", "removed")
        elif e.name == "NeighborFound":
            assert e.group in known and e.side in ("left", "right", "above", "below")


def test_object_noticed_once_with_a_region_name(run):
    events, _ = run
    noticed = [e for e in events if e.name == "ObjectNoticed"]
    ids = [e.object for e in noticed]
    assert len(ids) == len(set(ids)) and noticed[0].object == "cup1" and noticed[0].by == "start"
    assert all(re.fullmatch(r"(upper|middle|lower)(-(left|middle|right))?", e.region)
               for e in noticed)
    described = [e for e in events if e.name == "DescriptionAdded"]
    assert ("cup1", "cup") in [(e.item, e.descriptor) for e in described]


def test_temperature_updates_carry_their_reason(run):
    events, _ = run
    updates = [e for e in events if e.name == "TemperatureUpdated"]
    assert updates[0].by == "start" and updates[0].temperature == 80.0 and updates[0].held
    assert {e.by for e in updates} <= {"start", "schedule", "competition", "change"}
    assert all(0.0 <= e.temperature <= 100.0 for e in updates)


def test_every_event_class_occurs_over_a_few_runs():
    """The common events all occur on the central configuration; the context events need a
    group correspondence entered, which these seeds do not give (item 20's finding)."""
    seen = set()
    for seed in (1, 2, 3, 4, 5, 6, 12, 22):  # seed 12: a bottom-up stage passed, seed 22:
        # a group removed (item 08's runs; item 07's seed 8 passes no stage any more)
        events, _ = collect(seed=seed)
        seen |= {e.name for e in events}
    rare = {"ContextPushed", "ContextPopped", "GroupRemoved", "CorrespondenceRemoved"}
    # The role events occur in ordinary runs since the detectors of items 04 to 07; the
    # re-cast (RoleRecast, D-093) needs the touched object's row and the other row read,
    # and a fight or a break (RoleDiscarded) a rival or a weak tag, so those two stay rare.
    rare |= {"RoleDiscarded", "RoleRecast"}
    assert {cls.__name__ for cls in EV.EVENT_TYPES} - rare <= seen


# ---- the observer rules ------------------------------------------------------------------------


class Raising:
    def __init__(self, on="CodeletChosen"):
        self.on = on
        self.calls = 0

    def on_event(self, event):
        self.calls += 1
        if event.name == self.on:
            raise RuntimeError("an observer's bug")


class ReadsEverything:
    """An observer that reads every read-only accessor on every event (D-065)."""

    def __init__(self):
        self.world = None
        self.snapshots = 0

    def on_event(self, event):
        if self.world is None:
            return
        SN.snapshot(self.world)
        SN.answer_now(self.world)
        self.world.coderack.chances(self.world.temperature.exponent)
        self.world.quality()
        self.snapshots += 1


def test_a_raising_observer_is_recorded_and_the_run_is_unchanged(capsys):
    problem, config = load_problem("surround-05"), PROFILES["standard"]
    plain_result = run_problem(problem, config, 3)
    raising = Raising()
    collector = Collector()
    result = run_problem(problem, config, 3, [raising, collector])
    assert raising.calls == len(collector.events)
    assert len(result.observer_errors) == plain_result.codelets
    assert result.observer_errors[0][:2] == ("Raising", "CodeletChosen")
    assert "RuntimeError" in result.observer_errors[0][2]
    assert "observer Raising raised" in capsys.readouterr().err
    assert dataclasses.replace(result, observer_errors=()) == plain_result
    assert fingerprint("surround-05", 3) == fingerprint("surround-05", 3)


def test_an_observer_that_reads_everything_leaves_the_fingerprint_unchanged():
    reader = ReadsEverything()
    hasher = __import__("tabletop.observe", fromlist=["Hasher"]).Hasher()
    world = World(load_problem("surround-05"), PROFILES["standard"], 3, [reader, hasher])
    reader.world = world
    while not world.finished:
        world.step()
    assert reader.snapshots > 100 and not world.events.errors
    assert (hasher.digest(), world.rng.draws) == fingerprint("surround-05", 3)


def test_observers_are_called_in_subscription_order_with_the_same_event():
    order = []

    class A:
        def on_event(self, e):
            order.append(("a", id(e)))

    class B:
        def on_event(self, e):
            order.append(("b", id(e)))

    run_problem(load_problem("blockage-01"), PROFILES["standard"], 1, [A(), B()])
    assert order[::2] == [("a", i) for _, i in order[::2]]
    assert [i for _, i in order[::2]] == [i for _, i in order[1::2]]


def test_no_observer_builds_no_event():
    world = World(load_problem("surround-05"), PROFILES["standard"], 3)
    assert world.events.record(EV.RunEnded) is None and world.events.count == 0


def test_a_workspace_or_slipnet_on_its_own_publishes_to_no_one():
    from tabletop.slipnet import Slipnet
    from tabletop.workspace import Workspace
    ws = Workspace(load_problem("surround-05"), PROFILES["standard"])
    assert ws.describe("cup1", "cup") and ws.events.count == 0
    net = Slipnet(PROFILES["standard"])
    net.jolt("cup", 10.0)
    assert net.events.count == 0


# ---- the snapshot ------------------------------------------------------------------------------


def test_snapshot_is_plain_data_that_draws_nothing_and_changes_nothing():
    world = World(load_problem("surround-05"), PROFILES["standard"], 3)
    steps = 0
    while steps < 30 and world.step() is not None:     # a run may end at about 45 codelets
        steps += 1
    draws = world.rng.draws
    s = SN.snapshot(world)
    assert world.rng.draws == draws and s.draws == draws and s.clock == steps
    assert s.problem == "surround-05" and s.touched == "cup1" and not s.finished
    assert len(s.table) == 13 and s.table[3]["id"] == "cup1" and s.table[3]["noticed"]
    assert len(s.slipnet) == 63 and dict((n["name"], n["activation"]) for n in s.slipnet)["cup"] > 1
    assert all(l["effective"] <= l["length"] for l in s.links)
    assert len(s.groups) == len(world.workspace.groups)
    assert len(s.correspondences) == len(world.workspace.correspondences)
    assert set(s.worldview) >= {"names", "strength", "coherence", "parallelism", "total", "coherent"}
    assert s.coderack["codelets"] and abs(sum(p for _, p in s.coderack["shares"]) - 100) < 1e-6
    assert s.temperature["value"] == world.temperature.value
    assert s.saliences and s.saliences[0][0] == "cup1"
    # Mutating the copy changes nothing in the run.
    s.descriptions["cup1"] = ()
    s.groups[0]["members"] if s.groups else None
    assert world.workspace.descriptors("cup1")[0] == "cup"
    before = fingerprint("surround-05", 3)
    while not world.finished:
        world.step()
    assert world.rng.draws == before[1]
    assert SN.snapshot(world).answer["object"] == world.answer.object
