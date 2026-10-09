"""Tests for item 047 (the main loop and config): ``seqsee.seqsee_main``.

Mirrors lib/Seqsee.pm (``run``, ``do_background_activity``, ``Seqsee_Step``,
``Interaction_step_n``, ``_read_commandline``, ``_read_config``, ``%DEFAULTS``) and the config
files config/seqsee.conf and config/start_codelets.conf. Golden data: oracle/seqsee_main.pl.

As in the oracle, codelets run through probe families: ``Probe`` (args t, brk, die) and stubbed
``FocusOn``/``CheckProgress``, which only record that they ran. SLTM::DecayAll,
SWorkspace::__UpdateObjectStrengths, SanityCheck and main::update_display are counted.
"""
import pytest

import golden
from seqsee import global_ as Global
from seqsee import scoderack, seqsee_main, sltm, sworkspace, util
from seqsee.codelets import family
from seqsee.errors import Confess, Fatal
from seqsee.scodelet import SCodelet

CASES = {c["case"]: c for c in golden.load("seqsee_main")}


def _cases(prefix):
    return sorted(name for name in CASES if name.startswith(prefix))


class _Recorder:
    def __init__(self):
        self.ran = []
        self.updates = self.sanities = self.decays = self.strengths = 0


class _Probe(family.CodeletFamily):
    def __init__(self, name, rec):
        super().__init__(name, [], None)
        self.rec = rec

    def run(self, action_object, args):
        if self.name == "Probe":
            self.rec.ran.append("Probe:" + util.perl_str(args.get("t")))
            if util.perl_true(args.get("brk")):
                Global.Break_Loop = 1
            if util.perl_true(args.get("die")):
                raise Confess("probe died\n")
            return "ignored"
        self.rec.ran.append(self.name)


@pytest.fixture
def rec(monkeypatch):
    r = _Recorder()
    family.load_families()
    for name in ("Probe", "FocusOn", "CheckProgress"):
        monkeypatch.setitem(family.FAMILIES, name, _Probe(name, r))
    real_decay, real_strengths = sltm.decay_all, sworkspace.update_object_strengths
    real_sanity = seqsee_main._sanity_check

    def decay():
        r.decays += 1
        return real_decay()

    def strengths():
        r.strengths += 1
        return real_strengths()

    def sanity():
        r.sanities += 1
        return real_sanity()

    def update():
        r.updates += 1

    monkeypatch.setattr(sltm, "decay_all", decay)
    monkeypatch.setattr(sworkspace, "update_object_strengths", strengths)
    monkeypatch.setattr(seqsee_main, "_sanity_check", sanity)
    monkeypatch.setattr(seqsee_main, "_update_display", update)
    return r


def _reset_state(rec, capsys, steps=0, tons=0, checker=0, sanity=1, seq=(1, 2, 3), probes=(),
                 seed=1):
    Global.Steps_Finished = steps
    Global.TimeOfNewStructure = tons
    seqsee_main._TimeLastProgressCheckerLaunched = checker
    Global.AcceptableTrustLevel = 0.5
    Global.Break_Loop = None
    Global.Sanity = sanity
    Global.CurrentRunnableString = ""
    Global.Feature.clear()
    sltm.clear()
    sworkspace.init({"seq": list(seq)})
    scoderack.clear()
    scoderack.LastSelectedRunnable = None
    rec.ran.clear()
    rec.updates = rec.sanities = rec.decays = rec.strengths = 0
    for p in probes:
        t, u, *extra = p
        args = {"t": t}
        args.update(dict(zip(extra[::2], extra[1::2])))
        scoderack.add_codelet(SCodelet("Probe", u, args))
    util.srand(seed)
    capsys.readouterr()


def _coderack_view():
    return [[c[0], c[1], c[3].get("t")] for c in scoderack.CODELETS]


def _check_state(case, rec):
    assert rec.ran == case["ran"]
    assert Global.Steps_Finished == case["steps"]
    assert _coderack_view() == case["coderack"]
    assert seqsee_main._TimeLastProgressCheckerLaunched == case["checker"]
    assert Global.AcceptableTrustLevel == pytest.approx(case["trust"])
    assert rec.updates == case["updates"]
    assert rec.sanities == case["sanities"]
    assert rec.decays == case["decays"]
    assert rec.strengths == case["strengths"]
    assert Global.CurrentRunnableString == case["current"]
    assert Global.Break_Loop == case["break_loop"]
    assert util.rand() == pytest.approx(case["next_draw"], abs=1e-12)


def _err(e):
    return str(e)


# ---- _read_config -----------------------------------------------------------------------------
_CONFIG_OPTS = {
    "config_empty": {},
    "config_spaces": {"seq": "1 2 3"},
    "config_commas": {"seq": " 1, 2,,3 "},
    "config_leading_comma": {"seq": ",1,2"},
    "config_trailing_newline": {"seq": "1 2\n"},
    "config_zero": {"seq": "0"},
    "config_blank": {"seq": "   "},
    "config_letters": {"seq": "1 a"},
    "config_negative": {"seq": "-1 2"},
    "config_decimal": {"seq": "1.5"},
    "config_overrides": {"seq": "4", "seed": 7, "max_steps": 50, "update_interval": 3, "view": 2,
                         "gui_config": "X", "DecayRate": 0.5, "UseScheduledThoughtProb": 1,
                         "ScheduledThoughtVanishProb": 0},
    "config_undef_override": {"seq": "4", "max_steps": None},
    "config_extra_ignored": {"seq": "5", "extra": 5, "n": 3},
}


def test_config_cases_cover_golden():
    assert set(_CONFIG_OPTS) == set(_cases("config_"))


@pytest.mark.parametrize("name", sorted(_CONFIG_OPTS))
def test_read_config_golden(name, capsys):
    case = CASES[name]
    opts = _CONFIG_OPTS[name]
    if case["error"]:
        with pytest.raises(Confess) as exc:
            seqsee_main.read_config(**opts)
        assert _err(exc.value) == case["error"]
        assert capsys.readouterr().out == case["output"]
        return
    result = dict(seqsee_main.read_config(**opts))
    assert capsys.readouterr().out == case["output"]
    seed = result.pop("seed")
    assert result == case["result"]
    if case["seed_given"] is not None:
        assert seed == case["seed_given"]
    assert isinstance(seed, int) and 0 <= seed < 32000


# ---- _read_commandline ------------------------------------------------------------------------
_CMD_ARGV = {
    "cmd_none": [],
    "cmd_seed_seq": ["--seed", "5", "--seq", "1 2 3"],
    "cmd_single_dash_eq": ["-seed=5", "-n", "100"],
    "cmd_max_steps_and_n": ["--max_steps", "10", "--n", "20"],
    "cmd_n_only": ["--n", "20"],
    "cmd_gui": ["--gui", "X"],
    "cmd_gui_config_and_gui": ["--gui_config", "A", "--gui", "B"],
    "cmd_sanity": ["--sanity"],
    "cmd_nosanity": ["--nosanity"],
    "cmd_no_dash_sanity": ["--no-sanity"],
    "cmd_view_extra": ["--view", "3", "extra", "args"],
    "cmd_features": ["-f", "LTM", "-f", "debugMAX"],
    "cmd_feature_typo": ["-f", "Bogus", "--seed", "3"],
    "cmd_bad_int": ["--seed", "abc"],
    "cmd_unknown": ["--bogus", "--view", "1"],
    "cmd_ambiguous": ["--se", "5"],
    "cmd_abbrev": ["--max", "7", "--upd", "4"],
    "cmd_seq_eq": ["--seq=1,2"],
    "cmd_negative_int": ["--seed", "-3"],
    "cmd_double_dash": ["--view", "2", "--", "--seed", "5"],
    "cmd_missing_value": ["--seed"],
    "cmd_repeat": ["--seed", "1", "--seed", "2"],
    "cmd_plus_int": ["--seed", "+4"],
    "cmd_case": ["--SEED", "6"],
}


def test_cmd_cases_cover_golden():
    assert set(_CMD_ARGV) == set(_cases("cmd_"))


@pytest.mark.parametrize("name", sorted(set(_CMD_ARGV) - {"cmd_feature_typo"}))
def test_read_commandline_golden(name, capsys):
    case = CASES[name]
    argv = list(_CMD_ARGV[name])
    options = seqsee_main.read_commandline(argv)
    captured = capsys.readouterr()
    assert options == case["options"]
    assert argv == case["argv"]
    assert sorted(Global.Feature) == case["features"]
    assert Global.debugMAX == case["debugMAX"]
    assert captured.out == case["output"]
    assert captured.err == "".join(case["warnings"])


def test_read_commandline_feature_typo_exits(capsys):
    """Perl exits inside the -f callback. (The oracle turns exit into a die, which Getopt::Long
    catches and warns as "EXIT", so its golden also shows the rest of the line being parsed.)"""
    case = CASES["cmd_feature_typo"]
    assert case["warnings"] == ["EXIT\n"]
    with pytest.raises(SystemExit):
        seqsee_main.read_commandline(list(_CMD_ARGV["cmd_feature_typo"]))
    assert capsys.readouterr().out == case["output"]
    assert Global.Feature == {}


def test_read_commandline_defaults_to_sys_argv(monkeypatch):
    monkeypatch.setattr("sys.argv", ["Seqsee.pl", "--seed", "9", "rest"])
    assert seqsee_main.read_commandline() == {"seed": 9}
    import sys
    assert sys.argv == ["Seqsee.pl", "rest"]


# ---- do_background_activity -------------------------------------------------------------------
_BG = {
    "bg_early": dict(steps=5, tons=0, checker=0),
    "bg_checker_toss": dict(steps=30, tons=0, checker=0),
    "bg_checker_sure": dict(steps=200, tons=0, checker=0),
    "bg_checker_recent": dict(steps=41, tons=0, checker=30),
    "bg_checker_21": dict(steps=51, tons=0, checker=30),
    "bg_new_structure": dict(steps=61, tons=61, checker=0),
    "bg_decay": dict(steps=10, tons=0, checker=0),
    "bg_decay_zero": dict(steps=0, tons=0, checker=0),
}
_BG_CASES = [(name, seed) for name in sorted(_BG) for seed in range(1, 7)]


def test_bg_cases_cover_golden():
    names = {f"{n}_{s}" for n, s in _BG_CASES} | {"bg_codelet_tree"}
    assert names == set(_cases("bg_"))


@pytest.mark.parametrize("name,seed", _BG_CASES)
def test_do_background_activity_golden(name, seed, rec, capsys):
    case = CASES[f"{name}_{seed}"]
    _reset_state(rec, capsys, seed=seed, **_BG[name])
    seqsee_main.do_background_activity()
    assert case["error"] is None
    _check_state(case, rec)


def test_do_background_activity_codelet_tree(rec, capsys, tmp_path):
    case = CASES["bg_codelet_tree"]
    _reset_state(rec, capsys, steps=3, seed=2)
    Global.Feature["CodeletTree"] = 1
    log = tmp_path / "tree.log"
    with open(log, "w") as fh:
        Global.CodeletTreeLogHandle = fh
        seqsee_main.do_background_activity()
    Global.CodeletTreeLogHandle = None
    assert log.read_text() == case["log"]
    _check_state(case, rec)


# ---- Seqsee_Step / Interaction_step_n ---------------------------------------------------------
_P3 = (("a", 10), ("b", 20), ("c", 30))
_STEP = {
    **{f"step_basic_{s}": (dict(probes=_P3, seed=s),
                           [{"n": 5, "max_steps": 100, "update_after": 2}]) for s in range(1, 5)},
    "step_need_n": (dict(probes=_P3), [{"max_steps": 100}]),
    "step_n_zero": (dict(probes=_P3), [{"n": 0, "max_steps": 100}]),
    "step_max_limits": (dict(probes=_P3, seed=5), [{"n": 10, "max_steps": 4}] * 2),
    "step_no_max": (dict(probes=_P3), [{"n": 3}]),
    "step_update_default": (dict(probes=_P3, seed=6), [{"n": 3, "max_steps": 10}]),
    "step_update_every": (dict(probes=_P3, seed=6), [{"n": 3, "max_steps": 10, "update_after": 1}]),
    "step_update_3_of_4": (dict(probes=_P3, seed=6),
                           [{"n": 4, "max_steps": 10, "update_after": 3}]),
    "step_break": (dict(probes=(("x", 1000, "brk", 1), ("y", 1)), seed=7),
                   [{"n": 5, "max_steps": 100, "update_after": 2}]),
    "step_die": (dict(probes=(("z", 1000, "die", 1),), seed=8), [{"n": 5, "max_steps": 100}]),
    "step_trust_100": (dict(steps=99, probes=_P3, seed=9), [{"n": 2, "max_steps": 1000}]),
    "step_print_1000": (dict(steps=999, probes=_P3, seed=10), [{"n": 2, "max_steps": 2000}]),
    "step_empty_coderack": (dict(seed=11), [{"n": 12, "max_steps": 100}]),
    "step_no_sanity": (dict(probes=_P3, seed=12, sanity=0), [{"n": 3, "max_steps": 100}]),
    "step_checker": (dict(steps=25, probes=(("p", 5), ("q", 5)), seed=13),
                     [{"n": 6, "max_steps": 100}]),
    **{f"step_long_{s}": (dict(probes=tuple((f"p{i}", 3 * i) for i in range(1, 9)), seed=s),
                          [{"n": 40, "max_steps": 100, "update_after": 7}]) for s in range(14, 18)},
}


def test_step_cases_cover_golden():
    assert set(_STEP) == set(_cases("step_"))


@pytest.mark.parametrize("name", sorted(_STEP))
def test_interaction_step_n_golden(name, rec, capsys):
    case = CASES[name]
    state, calls = _STEP[name]
    assert calls == case["calls"]
    _reset_state(rec, capsys, **state)
    for call, expected in zip(calls, case["results"]):
        error = ret = None
        try:
            ret = seqsee_main.interaction_step_n(dict(call))
        except Confess as e:
            error = _err(e)
        assert capsys.readouterr().out == expected["output"]
        assert error == expected["error"]
        assert ret == expected["ret"]
        assert rec.ran == expected["ran"]
        assert Global.Steps_Finished == expected["steps"]
    _check_state(case, rec)


def test_seqsee_step_returns_undef(rec, capsys):
    case = CASES["seqsee_step_return"]
    _reset_state(rec, capsys, probes=_P3, seed=3)
    assert seqsee_main.seqsee_step() is None
    assert case["defined"] == 0
    _check_state(case, rec)


# ---- run ---------------------------------------------------------------------------------------
@pytest.mark.parametrize("name,args", [("run_list", (1, 2, 3)), ("run_empty", ())])
def test_run_dies_in_workspace_init(name, args, rec, capsys):
    case = CASES[name]
    _reset_state(rec, capsys)
    with pytest.raises(Confess) as exc:
        seqsee_main.run(*args)
    assert _err(exc.value) == case["error"]
    assert capsys.readouterr().out == case["output"]
    assert sworkspace.ElementCount == case["elements"]


def test_run_hashref_dies_at_missing_main_loop(rec, capsys):
    case = CASES["run_hashref"]
    _reset_state(rec, capsys)
    with pytest.raises(Confess) as exc:
        seqsee_main.run({"seq": [4, 5]})
    assert _err(exc.value) == case["error"]
    assert capsys.readouterr().out == case["output"]
    assert sworkspace.ElementCount == case["elements"]
    assert Global.RealSequence == case["real"]
    assert _coderack_view() == case["codelets"]


# ---- from reading the source ------------------------------------------------------------------
def test_defaults():
    assert set(seqsee_main.DEFAULTS) == {"seed", "update_interval"}
    assert seqsee_main.DEFAULTS["update_interval"] == 0
    assert 0 <= seqsee_main.DEFAULTS["seed"] < 32000


def test_defaults_do_not_draw_from_central_rng():
    """Perl draws the default seed at load time, before any srand; the port draws it from a
    private generator so importing never shifts the seeded stream."""
    import importlib
    util.srand(5)
    expected = util.rand()
    util.srand(5)
    importlib.reload(seqsee_main)
    assert util.rand() == expected


def test_read_config_missing_option(monkeypatch, tmp_path):
    conf = tmp_path / "seqsee.conf"
    conf.write_text("[seqsee]\nmax_steps = 5\n")
    monkeypatch.setattr(seqsee_main, "_SEQSEE_CONF", conf)
    with pytest.raises(Confess, match="Option 'UseScheduledThoughtProb' not set either on "
                                      "command line, conf file or defauls"):
        seqsee_main.read_config(seq="1")


def test_non_codelet_runnable_is_fatal(rec, capsys, monkeypatch):
    monkeypatch.setattr(scoderack, "get_next_runnable", lambda: "junk")
    with pytest.raises(Fatal, match="Runnable object is junk: expected a SCodelet"):
        seqsee_main.seqsee_step()


def test_codelet_tree_logs_chosen_codelet(rec, capsys, tmp_path):
    _reset_state(rec, capsys, probes=(("a", 10),), seed=4)
    Global.Feature["CodeletTree"] = 1
    log = tmp_path / "tree.log"
    with open(log, "w") as fh:
        Global.CodeletTreeLogHandle = fh
        seqsee_main.seqsee_step()
    Global.CodeletTreeLogHandle = None
    lines = log.read_text().splitlines()
    assert lines[0] == "Background"
    assert any(line.startswith("Chose SCodelet=HASH(0x") for line in lines)


def test_log_activations_every_ten_steps(rec, capsys, monkeypatch):
    calls = []
    monkeypatch.setattr(sltm, "log_activations", lambda: calls.append(Global.Steps_Finished))
    _reset_state(rec, capsys, steps=8, seed=4)
    Global.Feature["LogActivations"] = 1
    seqsee_main.interaction_step_n({"n": 4, "max_steps": 100})
    assert calls == [10]


def test_interstep_sleep(rec, capsys, monkeypatch):
    slept = []
    monkeypatch.setattr(seqsee_main, "_sleep", slept.append)
    _reset_state(rec, capsys, seed=4)
    Global.InterstepSleep = 30
    seqsee_main.seqsee_step()
    assert slept == [0.03]


def test_main_loop_does_not_clear_script_spec_cache(rec, capsys):
    from seqsee import scripts
    scripts._cached_spec = ("sentinel",)
    _reset_state(rec, capsys, probes=_P3, seed=4)
    seqsee_main.interaction_step_n({"n": 3, "max_steps": 10})
    assert scripts._cached_spec == ("sentinel",)


def test_start_codelets_conf_is_read_by_coderack_init(capsys):
    scoderack.clear()
    scoderack.init()
    assert [(c[0], c[1]) for c in scoderack.CODELETS] == [("FocusOn", "100"), ("FocusOn", "50")]
