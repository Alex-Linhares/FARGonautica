"""The taskrack: tasks, posting, the urgency-weighted draw and cleaning [M86 pp. 41-42]
(DECISIONS D-007, D-008, D-010, D-057)."""

import pytest

from seekwhence.config import TASK_TYPES, Config
from seekwhence.rng import Rng
from seekwhence.taskrack import ARENAS, Task, Taskrack


def task(number, type="Tester", urgency=5, args=(), at_term=0, created_at=0):
    return Task(number, type, tuple(args), urgency, ARENAS[type], created_at, at_term)


# ---- the task record ------------------------------------------------------------------------

def test_task_fields_are_the_transcripts_name_arguments_and_urgency():
    """`doing task Sparkler-plus on (glint2 glint1 10)` [M86 p. 164]: a type, arguments and
    an integer weight [M86 p. 42]."""
    t = Task(1, "Sparkler-plus", ("glint2", "glint1", 10), 10, "cytoplasm", 3, 2)
    assert (t.number, t.type, t.args, t.urgency) == (1, "Sparkler-plus", ("glint2", "glint1", 10), 10)
    assert (t.arena, t.created_at, t.at_term) == ("cytoplasm", 3, 2)


@pytest.mark.parametrize("bad", [0, -1, 2.5, True, "10"])
def test_an_urgency_is_a_positive_integer_weight(bad):
    """"'urgencies' (integer weights)" [M86 p. 42]."""
    with pytest.raises(ValueError):
        Task(1, "Tester", (), bad, "cytoplasm", 0, 0)


def test_unknown_task_types_and_non_tuple_args_are_refused():
    with pytest.raises(ValueError):
        Task(1, "Frobnicator", (), 1, "cytoplasm", 0, 0)
    with pytest.raises(TypeError):
        Task(1, "Tester", ["glint1"], 1, "cytoplasm", 0, 0)


def test_every_task_type_has_an_arena_from_the_three_or_user():
    """"Each task operates in one of three arenas" [MHN p. 3] (DECISION D-057)."""
    assert set(ARENAS) == set(TASK_TYPES)
    assert set(ARENAS.values()) == {"cytoplasm", "socratoplasm", "platoplasm", "user"}
    assert ARENAS["Sparkler"] == "cytoplasm"
    assert ARENAS["Gnoth-setter"] == "socratoplasm"
    assert ARENAS["Plato-scout"] == "platoplasm"
    assert ARENAS["Call-term"] == ARENAS["empty-task"] == "user"


def test_the_config_urgency_table_covers_every_type_but_empty_task():
    """DECISION D-008: one urgency per task type; `empty-task` is never posted (D-013)."""
    urgencies = dict(Config().urgencies)
    assert set(urgencies) == set(TASK_TYPES) - {"empty-task", "Sparkler-plus"}
    assert all(isinstance(u, int) and u >= 1 for u in urgencies.values())


# ---- posting and choosing -------------------------------------------------------------------

def test_post_keeps_posting_order_and_choose_removes_the_task():
    """"place them on the taskrack where they will stay until chosen and run" [M86 p. 41]."""
    rack = Taskrack()
    a, b, c = task(1), task(2), task(3)
    for t in (a, b, c):
        rack.post(t)
    assert rack.tasks == (a, b, c) and len(rack) == 3
    chosen = rack.choose(Rng(1))
    assert chosen in (a, b, c)
    assert len(rack) == 2 and chosen not in rack.tasks


def test_choose_from_an_empty_rack_is_an_error():
    with pytest.raises(LookupError):
        Taskrack().choose(Rng(1))


def test_choose_makes_exactly_one_draw():
    """DECISION D-007: one draw from the run's Rng per choice."""
    rack = Taskrack()
    for n in range(5):
        rack.post(task(n))
    rng = Rng(7)
    rack.choose(rng)
    assert rng.draws == 1


def test_selection_frequencies_follow_the_urgencies():
    """"more urgent tasks will have a greater chance of being chosen than less urgent ones
    do" [M86 p. 42]: with weights 1, 2, 3, 4 the frequencies are within 0.02 of 0.1, 0.2,
    0.3, 0.4 over 20000 draws (DECISION D-007)."""
    rng = Rng(2024)
    counts = {1: 0, 2: 0, 3: 0, 4: 0}
    for _ in range(20000):
        rack = Taskrack()
        for u in (1, 2, 3, 4):
            rack.post(task(u, urgency=u))
        counts[rack.choose(rng).urgency] += 1
    for u in counts:
        assert abs(counts[u] / 20000 - u / 10) < 0.02, counts


def test_urgency_power_sharpens_the_choice():
    """DECISION D-007: with `urgency_power` 2 the weights are 1, 4, 9, 16 (of 30)."""
    rng = Rng(99)
    counts = {1: 0, 2: 0, 3: 0, 4: 0}
    for _ in range(20000):
        rack = Taskrack(urgency_power=2)
        for u in (1, 2, 3, 4):
            rack.post(task(u, urgency=u))
        counts[rack.choose(rng).urgency] += 1
    for u in counts:
        assert abs(counts[u] / 20000 - u * u / 30) < 0.02, counts
    assert Taskrack(urgency_power=2).urgency_power == 2
    with pytest.raises(ValueError):
        Taskrack(urgency_power=-1)


def test_urgency_power_zero_makes_every_task_equally_likely():
    """DECISION D-007's alternative, all weights 1."""
    rng = Rng(5)
    counts = {1: 0, 10: 0}
    for _ in range(4000):
        rack = Taskrack(urgency_power=0)
        rack.post(task(1, urgency=1))
        rack.post(task(2, urgency=10))
        counts[rack.choose(rng).urgency] += 1
    assert abs(counts[1] / 4000 - 0.5) < 0.03


def test_same_seed_same_order_of_choices():
    """A run repeats from its seed."""
    def order(seed):
        rack = Taskrack()
        for n in range(1, 21):
            rack.post(task(n, urgency=(n % 5) + 1))
        rng = Rng(seed)
        return [rack.choose(rng).number for _ in range(20)]
    assert order(3) == order(3)
    assert order(3) != order(4)


# ---- cleaning -------------------------------------------------------------------------------

def test_clean_removes_the_given_types_and_returns_them():
    """"removing old tasks of certain types" [M86 p. 42] (DECISION D-010)."""
    rack = Taskrack()
    kept = task(1, "Glom-scout")
    gone1, gone2 = task(2, "Sparkler"), task(3, "Tester")
    for t in (gone1, kept, gone2):
        rack.post(t)
    removed = rack.clean(("Sparkler", "Tester", "Dissolver"))
    assert removed == [gone1, gone2]
    assert rack.tasks == (kept,)


def test_clean_before_a_term_keeps_the_tasks_posted_since():
    """DECISION D-010: on a new term only the tasks "created before the previous term" go."""
    rack = Taskrack()
    old = task(1, "Sparkler", at_term=1)
    recent = task(2, "Sparkler", at_term=2)
    newest = task(3, "Sparkler", at_term=3)
    other = task(4, "Bonder", at_term=0)
    for t in (old, recent, newest, other):
        rack.post(t)
    # Term 3 was just entered: tasks posted when fewer than 2 terms were known are old.
    removed = rack.clean(("Sparkler",), before_term=2)
    assert removed == [old]
    assert rack.tasks == (recent, newest, other)


def test_clean_with_no_match_removes_nothing():
    rack = Taskrack()
    rack.post(task(1, "Bonder"))
    assert rack.clean(("Sparkler",)) == []
    assert len(rack) == 1


def test_count_by_type_is_in_order_of_first_appearance():
    rack = Taskrack()
    for t in (task(1, "Tester"), task(2, "Sparkler"), task(3, "Tester")):
        rack.post(t)
    assert rack.count_by_type() == {"Tester": 2, "Sparkler": 1}
    assert list(rack.count_by_type()) == ["Tester", "Sparkler"]
