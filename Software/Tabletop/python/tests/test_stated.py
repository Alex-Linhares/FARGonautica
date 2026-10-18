"""The `stated` bench (D-074): French's statements of what the program does as yes-or-no
checks over the runs. The fast tests run a few runs and check the bench's shape; the slow
survey runs the statements over many runs and asserts the ones the mechanisms guarantee,
leaving the rest as findings the bench prints (ANSWERS.md 1: a miss is a finding).
"""

import json
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import events as EV
from tabletop.bench import ch4
from tabletop.bench import stated as ST
from tabletop.bench.__main__ import BENCHES
from tabletop.config import PROFILES
from tabletop.problems import load_problem
from tabletop.run import result_of
from tabletop.world import World

PYTHON_DIR = Path(__file__).resolve().parents[1]
STANDARD = PROFILES["standard"]


def test_the_bench_is_listed_with_the_protocol():
    assert BENCHES["stated"][0] is ST
    for name in ("SETS", "problems_of", "planned", "run", "report", "records"):
        assert hasattr(ST, name)
    names = ST.problems_of()
    assert names[0] == ST.CH4 and len(names) == len(set(names))
    assert all(n.startswith("ch2-") for n in names[-19:])
    assert ST.planned() == len(ST.SETS) + 19
    assert ST.planned(only=[ST.CH4]) == 1


def test_facts_of_a_run_read_its_events():
    """The per-run facts come from the events of a plain run (D-074)."""
    problem = load_problem("surround-05")
    collector = EV.Collector()
    world = World(problem, STANDARD, 1, [collector])
    while world.step() is not None:
        pass
    result = result_of(world)
    f = ST.facts_of(problem, collector, result)
    assert f.answer == result.answer and f.literal == result.literal
    assert f.codelets == result.codelets and f.capped == result.capped
    assert f.last_codelet == "find-answer"
    assert f.first_group_kind == "gestalt"
    assert all(len(pair) == 2 for pair in f.pairs.values())
    assert f.focus["cup"] >= 100 and f.focus["opposite"] >= 100
    assert f.milestones == ch4.milestones_of(collector, list(problem.ids()))
    assert ST.milestones_in_order(f) in (True, False)


def test_a_sample_is_the_surveys_runs():
    """A sample's survey equals the harness's for the same seed, run for run (D-072)."""
    from tabletop.bench import harness as H
    s = ST.sample("ch2-fig03", STANDARD, 1, 3)
    sv = H.survey(load_problem("ch2-fig03"), STANDARD, 1, 3)
    assert s.survey.results == sv.results and len(s.facts) == 3
    assert [f.answer for f in s.facts] == [r[1] for r in sv.results]


def test_the_readings_on_known_facts():
    problem = load_problem("figure-ground-01")
    f = ST.Facts("fork3", False, 10, False,
                 (("gestalt", ("fork3", "small-glass3"), 80.0), ("mixed", ("fork3", "small-glass3"), 70.0)),
                 (), "gestalt", {}, None, (), (), 0, (), None, {}, {})
    assert ST.fork_glass_group(problem, f)
    g = ST.Facts("fork3", False, 10, False, (("gestalt", ("fork3", "small-glass3"), 80.0),),
                 (), "gestalt", {}, None, (), (), 0, (), None, {}, {})
    assert not ST.fork_glass_group(problem, g)          # a gestalt bunch is not the fault
    assert ST.grouped_together(g, "fork3", "small-glass3")
    assert not ST.grouped_together(g, "fork1", "fork3")
    cups = load_problem("buridan-12")
    h = ST.Facts("spoon1", False, 10, False,
                 (("sameness", ("cup3", "cup4", "cup5", "cup6"), 50.0), ("sameness", ("cup3", "cup4"), 50.0)),
                 (("g1", ("cup3", "cup4", "cup5", "cup6")), ("g2", ("cup3", "cup4"))),
                 "sameness", {}, None, (), (), 0, (), None, {}, {})
    assert ST.nested_sameness_run(cups, h)
    assert ST.pair_kinds(cups, ("g1", "cup3")) == ("group", "object")
    assert ST.unordered(("a", "b")) == frozenset(("b", "a")) and ST.unordered(None) == frozenset()
    assert ST.share([f, g], lambda x: x.answer == "fork3") == 100.0 and ST.share([], bool) == 0.0
    assert ST.mean([]) is None and ST.mean([1, 3]) == 2.0


def test_only_keeps_to_the_named_problems():
    result = ST.run(STANDARD, 1, 2, only=["surround-05", "ch2-fig03"], progress=lambda sv: None)
    assert set(result.samples) == {("surround-05", "standard")}
    assert [c.problem for c in result.checks] == ["surround-05"] * 16
    assert [c.name for c in result.series] == ["ch2-fig03"]


def test_the_command_line_runs_the_bench(tmp_path):
    out = tmp_path / "stated.json"
    proc = subprocess.run([sys.executable, "-m", "tabletop.bench", "stated", "--runs", "2",
                           "--problems", "surround-05", "--quiet", "--json", str(out)],
                          cwd=PYTHON_DIR, capture_output=True, text=True, timeout=600)
    assert proc.returncode == 0, proc.stderr
    assert "## French's statements" in proc.stdout
    records = [json.loads(l) for l in out.read_text().splitlines()]
    assert sum(1 for r in records if "number" in r) == 16
