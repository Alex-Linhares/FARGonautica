"""Tests for seqsee/cli.py and seqsee/__main__.py: the headless port of Seqsee.pl.

Mirrors Seqsee.pl (INITIALIZE, Interaction_continue) driving lib/Seqsee.pm, with
UI/Graphical.pm's ask_user_extension and a stand-in for $SGUI::Commentary.

Golden: oracle/cli.pl runs headless Seqsee.pl, one Perl process per run, either answering
every question "yes" or answering from a known continuation. Python can't follow Perl's exact
trajectory (hash order, addresses), so whole runs are compared per sequence: success rate,
the extensions found and the questions asked.
"""
import io
import json
import os
import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

import pytest

import golden
from seqsee import cli, global_ as Global, s, scoderack, sworkspace, user_interaction, util
from seqsee.user_interaction import SolutionConfirmation

GOLDEN = golden.load("cli")
SRC = Path(__file__).resolve().parents[1] / "src"


def _groups():
    groups = defaultdict(list)
    for case in GOLDEN:
        groups[(case["seq"], case["continuation"], case["max_steps"])].append(case)
    return groups


GROUPS = _groups()


def _py_runs(seq, cont, max_steps, seeds):
    runs = []
    for seed in seeds:
        argv = ["--seq", seq, "--seed", str(seed), "--max-steps", str(max_steps)]
        if cont is not None:
            argv += ["--continuation", cont]
        runs.append(cli.run_headless(argv))
    return runs


def _first_asked_term(question):
    """The first term in "Is the next term 4?", "Are the next terms: 4 5?" or "Are the next
    2 terms 4 and 5?"."""
    return int(re.search(r"next (?:\d+ )?terms?:? (-?\d+)", question).group(1))


def _shape(question):
    """(prefix, phrasing with numbers masked) of a question."""
    m = re.match(r"(.*?)(Is the next term|Are the next (?:\d+ )?terms:?) ", question)
    return m.group(1), re.sub(r"\d+", "#", m.group(2))


def _rate(runs, status):
    return sum(r["status"] == status for r in runs) / len(runs)


# ---- golden: whole runs, compared per sequence ----------------------------------------------
@pytest.mark.slow
@pytest.mark.parametrize("key", list(GROUPS), ids=lambda k: f"{k[0]}|{k[1]}")
def test_golden_parity(key):
    seq, cont, max_steps = key
    perl = GROUPS[key]
    py = _py_runs(seq, cont, max_steps, [c["seed"] for c in perl])
    n_initial = len(seq.split())
    known = seq.split() + (cont.split() if cont else [])

    for r in py:
        assert r["status"] in ("accepted", "max_steps", "out_of_terms"), r["error"]
        assert r["elements"][:n_initial] == [int(x) for x in seq.split()]
        assert r["elements"][n_initial:] == r["extension"]
        assert r["steps"] <= max_steps
        if cont is not None:
            # Only terms the answerer confirmed are inserted.
            assert [str(x) for x in r["extension"]] == known[n_initial:n_initial + len(r["extension"])]
        if r["status"] == "accepted":
            assert r["responses"], "a solution is accepted only through the Yes/No question"

    # Success rate within a generous tolerance (small samples, different trajectories).
    for status in ("accepted",):
        assert abs(_rate(py, status) - _rate(perl, status)) <= 0.5, (
            status, [r["status"] for r in py], [c["status"] for c in perl])

    # Where Perl always or never extends, so does Python.
    perl_extends = [bool(c["extension"]) for c in perl]
    py_extends = [bool(r["extension"]) for r in py]
    if all(perl_extends):
        assert sum(py_extends) >= len(py) / 2
    if not any(perl_extends):
        assert sum(py_extends) <= len(py) / 2

    # Every first term Python proposes, Perl proposes too, in some run on the same sequence
    # (either answering mode).
    perl_first = {_first_asked_term(q) for c in GOLDEN if c["seq"] == seq for q in c["asked"]}
    py_first = {_first_asked_term(q) for r in py for q in r["asked"]}
    assert py_first <= perl_first, (py_first, perl_first)

    # Same question wording: each question Python asks has a prefix and an "Is the next
    # term"/"Are the next N terms" phrasing that Perl uses too, numbers aside.
    perl_shapes = [_shape(q) for c in GOLDEN for q in c["asked"]]
    for r in py:
        for q in r["asked"]:
            prefix, phrasing = _shape(q)
            assert prefix in {p for p, _ in perl_shapes}, q
            assert phrasing in {f for _, f in perl_shapes}, q


def test_golden_fast_subset():
    """A quick parity check on the easiest sequence: 1 2 3 4 5 with its continuation."""
    perl = GROUPS[("1 2 3 4 5", "6 7 8 9 10", 600)]
    py = _py_runs("1 2 3 4 5", "6 7 8 9 10", 600, [1, 2, 3])
    assert _rate(perl, "accepted") == 1
    assert sum(r["status"] == "accepted" for r in py) >= 2
    for r in py:
        if r["status"] == "accepted":
            assert r["extension"][0] == 6
            assert r["asked"][0].endswith("Is the next term 6??")
            assert r["responses"][-1] == "Does this generate the sequence you had in mind?"


def test_golden_shape_matches_oracle():
    """The Python result dict has the oracle's keys."""
    r = cli.run_headless(["--seq", "5", "--seed", "1", "-n", "30"])
    assert set(r) == set(GOLDEN[0])
    case = next(c for c in GOLDEN if c["seq"] == "5" and c["seed"] == 1)
    assert case["status"] == "max_steps" and case["extension"] == [] and case["asked"] == []


# ---- from the source -------------------------------------------------------------------------
def test_translate_argv_hyphenated_options():
    assert cli.translate_argv(["--max-steps", "7", "--update-interval=3", "--seed", "2"]) == [
        "--max_steps", "7", "--update_interval=3", "--seed", "2"]
    assert cli.translate_argv(["-max-steps=9"]) == ["-max_steps=9"]
    # Values and the -- terminator are left alone.
    assert cli.translate_argv(["--seq", "1 2", "--", "--max-steps"]) == [
        "--seq", "1 2", "--", "--max-steps"]


def test_split_cli_options():
    rest, opts = cli.split_cli_options(
        ["--seq", "1 2 3", "--continuation", "4 5", "--answer=no", "--json", "-n", "5"])
    assert rest == ["--seq", "1 2 3", "-n", "5"]
    assert opts == {"continuation": "4 5", "answer": "no", "json": True}
    with pytest.raises(cli.UsageError):
        cli.split_cli_options(["--answer", "maybe"])
    with pytest.raises(cli.UsageError):
        cli.split_cli_options(["--continuation"])


def test_options_reach_read_config():
    r = cli.run_headless(["--seq", "1,2,3", "--seed", "4", "--max-steps", "12"])
    assert r["max_steps"] == 12 and r["seed"] == 4 and r["steps"] == 12
    assert Global.Options_ref["seq"] == ["1", "2", "3"]
    assert Global.Options_ref["max_steps"] == 12
    assert r["status"] == "max_steps"


def test_seed_makes_runs_reproducible():
    """Seqsee.pl never calls srand with --seed (PERL-QUIRK); the CLI does."""
    a = cli.run_headless(["--seq", "1 2 3 4 5", "--seed", "3", "-n", "150"])
    b = cli.run_headless(["--seq", "1 2 3 4 5", "--seed", "3", "-n", "150"])
    assert a == b


def test_each_run_starts_fresh():
    """A run resets all global state first, as a new Seqsee.pl process would."""
    cli.run_headless(["--seq", "1 2 3", "--seed", "1", "-n", "20"])
    assert Global.Steps_Finished == 20
    cli.run_headless(["--seq", "4 5", "--seed", "1", "-n", "10", "--answer", "no"])
    assert Global.Steps_Finished == 10
    assert sworkspace.ElementCount == 2


def test_callbacks_restored_after_run():
    before = (user_interaction.boolean_response, user_interaction.response,
              user_interaction.ask_user_extension, user_interaction.ask,
              SolutionConfirmation.__dict__["set_accepted_solution"])
    cli.run_headless(["--seq", "1 2 3", "--seed", "1", "-n", "20"])
    after = (user_interaction.boolean_response, user_interaction.response,
             user_interaction.ask_user_extension, user_interaction.ask,
             SolutionConfirmation.__dict__["set_accepted_solution"])
    assert before == after


def test_empty_sequence_is_a_usage_error():
    """Seqsee.pl would open SGUI->ask_seq; headless there is nobody to ask."""
    out, err = io.StringIO(), io.StringIO()
    assert cli.main(["--seed", "1"], out=out, err=err) == 2
    assert "--seq" in err.getvalue()


def test_bad_sequence_reports_read_config_error():
    out, err = io.StringIO(), io.StringIO()
    assert cli.main(["--seq", "1 a 2"], out=out, err=err) == 2
    assert "space or comma separated list of integers" in err.getvalue()


def test_continuation_answerer():
    state = cli._RunState(known=["1", "2", "3", "4", "5"], answer="yes")
    sworkspace.init({"seq": ["1", "2", "3"]})
    state.pending = ["4"]
    assert state.boolean_response("q1") == 1
    state.pending = ["4", "6"]
    assert state.boolean_response("q2") == 0
    state.pending = ["4", "5", "6"]
    with pytest.raises(cli.OutOfTerms):
        state.boolean_response("q3")
    assert state.asked == ["q1", "q2", "q3"]


def test_answer_modes():
    yes = cli._RunState(known=None, answer="yes")
    no = cli._RunState(known=None, answer="no")
    assert yes.boolean_response("q") == 1 and yes.response(["Yes", "No"], "r") == "Yes"
    assert no.boolean_response("q") == 0 and no.response(["Yes", "No"], "r") == "No"
    ask = cli._RunState(known=None, answer="ask", stdin=io.StringIO("y\nn\nyes\n"),
                        prompt_out=io.StringIO())
    assert ask.boolean_response("Is the next term 4?") == 1
    assert ask.boolean_response("Is the next term 5?") == 0
    assert ask.response(["Yes", "No"], "Does this generate the sequence you had in mind?") == "Yes"
    assert ask.responses == ["Does this generate the sequence you had in mind?"]


def test_answer_no_never_extends():
    r = cli.run_headless(["--seq", "1 2 3 4 5", "--seed", "2", "-n", "400", "--answer", "no"])
    assert r["extension"] == []
    assert r["status"] == "max_steps"
    if r["asked"]:
        assert Global.ExtensionRejectedByUser


def test_out_of_terms_status():
    # Seed 1 of 1 2 3 4 5 with a one-term continuation: asking for 6 7 or more runs out.
    for seed in range(1, 8):
        r = cli.run_headless(["--seq", "1 2 3 4 5", "--seed", str(seed), "-n", "600",
                              "--continuation", "6"])
        assert r["status"] in ("accepted", "max_steps", "out_of_terms")
        assert r["extension"] in ([], [6])
        if r["status"] == "out_of_terms":
            return
    pytest.skip("no seed ran out of terms")


def test_accepted_solution_ends_run_after_that_step(monkeypatch):
    accepted_at = []
    orig = SolutionConfirmation.set_accepted_solution

    def record(cls, rule, position_structure):
        accepted_at.append(Global.Steps_Finished)
        return orig(rule, position_structure)

    monkeypatch.setattr(SolutionConfirmation, "set_accepted_solution", classmethod(record))
    r = cli.run_headless(["--seq", "1 2 3 4 5", "--seed", "4", "-n", "600",
                          "--continuation", "6 7 8 9 10"])
    assert r["status"] == "accepted"
    assert accepted_at == [r["steps"]]
    assert Global.Steps_Finished == r["steps"] < 600
    assert SolutionConfirmation.accepted_rule is not None


def test_report_text_and_json():
    out, err = io.StringIO(), io.StringIO()
    argv = ["--seq", "1 2 3 4 5", "--seed", "4", "-n", "600", "--continuation", "6 7 8 9 10"]
    assert cli.main(argv, out=out, err=err) == 0
    text = out.getvalue()
    assert "Sequence: 1 2 3 4 5" in text
    assert "Extension found: 6" in text
    assert "Status: accepted" in text
    out = io.StringIO()
    assert cli.main(argv + ["--json"], out=out, err=err) == 0
    data = json.loads(out.getvalue())
    assert data["status"] == "accepted" and data["extension"][0] == 6


def test_run_output_is_quiet():
    """Seqsee's own prints (View: 1!, Initializing Coderack...) don't reach the report."""
    out, err = io.StringIO(), io.StringIO()
    cli.main(["--seq", "1 2", "--seed", "1", "-n", "5"], out=out, err=err)
    assert "Initializing" not in out.getvalue() and "View:" not in out.getvalue()


def test_python_dash_m_seqsee():
    env = dict(os.environ, PYTHONPATH=str(SRC))
    proc = subprocess.run(
        [sys.executable, "-m", "seqsee", "--seq", "1 2 3 4 5", "--seed", "4", "--max-steps",
         "600", "--continuation", "6 7 8 9 10", "--json"],
        capture_output=True, text=True, env=env, timeout=300)
    assert proc.returncode == 0, proc.stderr
    data = json.loads(proc.stdout)
    assert data["extension"][:1] == [6]
