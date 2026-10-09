"""The model runner (loop0002 item 003): seqsee/gui/runner.py.

Mirrors Seqsee.pl's Interaction_step / Interaction_step_n / Interaction_crawl /
Interaction_continue and init_display/INITIALIZE, lib/Seqsee.pm's Interaction_step_n
(update_after, Break_Loop), the Pause binding of config/GUI_sparse.conf
(``$Global::Break_Loop = 1``), lib/UI/Graphical.pm (update_display, ask_user_extension,
ask_for_more_terms) and SGUI::ask_for_more_terms (lib/SGUI.pm), with Tk's event loop
replaced by a worker thread that sends snapshots and questions to the GUI thread.
"""
import sys
import threading
import time

import pytest

from seqsee import global_ as Global
from seqsee import scoderack, seqsee_main, sworkspace, user_interaction, util
from seqsee.categories.base import SCategory
from seqsee.codelets import all_mx

pytestmark = pytest.mark.gui

pytest.importorskip("PySide6.QtCore")
from seqsee.gui.runner import Runner  # noqa: E402
from seqsee.gui.snapshot import Snapshot  # noqa: E402

SEQ = "1 1 2 1 2 3"
ALTERNATING = "1 1 1 2 2 3 3 3 4 4 5 5 5 6 6"


def mags(snap):
    return [int(util.perl_num(e.mag)) for e in snap.elements]


class Recorder:
    """Collects everything the runner sends to the GUI thread (and where it arrived)."""

    def __init__(self, runner, answer=None):
        self.snapshots, self.questions, self.errors, self.done, self.states = [], [], [], [], []
        self.threads = set()
        self.answer = answer
        runner.snapshot.connect(self._snap)
        runner.question.connect(self._question)
        runner.error.connect(lambda msg, tb: self.errors.append((msg, tb)))
        runner.command_done.connect(lambda name, result: self.done.append((name, result)))
        runner.state_changed.connect(self.states.append)
        self.runner = runner

    def _snap(self, snap):
        self.threads.add(threading.current_thread())
        self.snapshots.append(snap)

    def _question(self, q):
        self.threads.add(threading.current_thread())
        self.questions.append(q)
        if self.answer is not None:
            self.runner.answer(q, self.answer(q))


@pytest.fixture
def runner(qtbot):
    r = Runner(min_interval=1 / 30)
    r.start()
    yield r
    assert r.quit(timeout=10), "the worker did not stop"


def wait_done(qtbot, rec, name, timeout=20000):
    """Wait until a command called ``name`` has finished; return its result."""
    n = sum(1 for d in rec.done if d[0] == name)
    qtbot.waitUntil(lambda: sum(1 for d in rec.done if d[0] == name) > n, timeout=timeout)
    return [d for d in rec.done if d[0] == name][-1][1]


def new_seq(qtbot, runner, rec, seq=SEQ, seed=7, **kw):
    with qtbot.waitSignal(runner.command_done, timeout=20000,
                          check_params_cb=lambda name, result: name == "new_sequence"):
        runner.new_sequence(seq, seed=seed, **kw)
    assert not rec.errors, rec.errors


# ------------------------------------------------------------------ commands and snapshots
def test_new_sequence_and_step_send_snapshots(qtbot, runner):
    rec = Recorder(runner)
    new_seq(qtbot, runner, rec)
    assert isinstance(rec.snapshots[-1], Snapshot)
    assert rec.snapshots[-1].element_count == 6
    assert rec.snapshots[-1].steps == 0
    assert runner.options["seq"] == SEQ.split()

    runner.step()
    wait_done(qtbot, rec, "step")
    assert rec.snapshots[-1].steps == 1
    assert mags(rec.snapshots[-1]) == [1, 1, 2, 1, 2, 3]
    # signals are delivered on the GUI (main) thread
    assert rec.threads == {threading.main_thread()}
    assert not rec.errors


def test_step_n_sends_snapshots_along_the_way(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec, update_interval=1)
    runner.crawl(2)  # 2 ms per step: long enough for several 1/30 s frames
    qtbot.waitUntil(lambda: len(rec.snapshots) >= 4, timeout=20000)
    runner.pause()
    wait_done(qtbot, rec, "crawl")
    steps = [s.steps for s in rec.snapshots]
    assert steps == sorted(steps)
    assert len(set(steps)) >= 3
    assert rec.snapshots[-1].steps == Global.Steps_Finished


def test_step_n_takes_n_steps(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec)
    runner.step_n(25)
    wait_done(qtbot, rec, "step_n")
    assert rec.snapshots[-1].steps == Global.Steps_Finished
    # a run of 25 steps stops early only on Break_Loop (a yes, an accepted solution)
    assert 1 <= Global.Steps_Finished <= 25


def test_snapshots_are_throttled(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec, update_interval=1)
    n0 = len(rec.snapshots)
    t0 = time.monotonic()
    runner.step_n(300)
    wait_done(qtbot, rec, "step_n", timeout=60000)
    elapsed = time.monotonic() - t0
    sent = len(rec.snapshots) - n0
    # update_after = 1 asks for a display after each of up to 300 steps; at most ~30 per
    # second get through, plus the final one and one before each question
    assert sent <= elapsed * 30 + 2 + len(rec.questions)
    assert rec.snapshots[-1].steps == Global.Steps_Finished


def test_commands_need_a_sequence(qtbot, runner):
    rec = Recorder(runner)
    runner.step()
    wait_done(qtbot, rec, "step")
    assert rec.errors and "sequence" in rec.errors[-1][0].lower()


def test_continue_and_crawl_set_interstep_sleep(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec, max_steps=3)
    runner.crawl(1)
    wait_done(qtbot, rec, "crawl")
    assert Global.InterstepSleep == 1
    runner.continue_()
    wait_done(qtbot, rec, "continue")
    assert Global.InterstepSleep == 0
    assert Global.Steps_Finished <= 3


def test_max_steps_ends_continue(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec, max_steps=20)
    runner.continue_()
    wait_done(qtbot, rec, "continue")
    runner.continue_()
    result = wait_done(qtbot, rec, "continue")
    if Global.Steps_Finished == 20:
        assert result == 1  # Interaction_step_n: nothing left to do


def test_model_runs_off_the_gui_thread(qtbot, runner, monkeypatch):
    rec = Recorder(runner)
    new_seq(qtbot, runner, rec)
    seen = []
    orig = scoderack.get_next_runnable

    def spy():
        seen.append(threading.current_thread())
        return orig()

    monkeypatch.setattr(scoderack, "get_next_runnable", spy)
    runner.step_n(3)
    wait_done(qtbot, rec, "step_n")
    assert seen and threading.main_thread() not in seen


# ------------------------------------------------------------------ pause
def test_pause_stops_a_crawl(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec, update_interval=1)
    runner.crawl(20)
    qtbot.waitUntil(lambda: Global.Steps_Finished >= 2, timeout=20000)
    t0 = time.monotonic()
    runner.pause()
    wait_done(qtbot, rec, "crawl")
    assert time.monotonic() - t0 < 2
    steps = Global.Steps_Finished
    qtbot.wait(150)
    assert Global.Steps_Finished == steps
    assert steps < util.perl_num(runner.options["max_steps"])
    assert rec.snapshots[-1].steps == steps
    assert rec.states[-1] == "idle"


def test_pause_stops_continue(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec)
    runner.continue_()
    qtbot.waitUntil(lambda: Global.Steps_Finished >= 5, timeout=20000)
    runner.pause()
    wait_done(qtbot, rec, "continue")
    assert Global.Steps_Finished < util.perl_num(runner.options["max_steps"])


def test_pause_cancels_queued_runs(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec)
    runner.continue_()
    runner.continue_()
    runner.pause()
    qtbot.waitUntil(lambda: rec.states and rec.states[-1] == "idle", timeout=20000)
    qtbot.wait(100)
    assert Global.Steps_Finished < util.perl_num(runner.options["max_steps"])
    assert sum(1 for d in rec.done if d[0] == "continue") <= 1


def test_a_run_after_pause_goes_on(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec)
    runner.pause()
    runner.step_n(5)
    wait_done(qtbot, rec, "step_n")
    assert Global.Steps_Finished >= 1


# ------------------------------------------------------------------ questions
def test_boolean_question_round_trip(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: 1)
    new_seq(qtbot, runner, rec)
    runner.call(user_interaction.ask_user_extension, [7])
    result = wait_done(qtbot, rec, "call")
    (q,) = rec.questions
    assert q.kind == "boolean"
    assert q.text == "Is the next term 7?"
    assert result == 1
    assert Global.AtLeastOneUserVerification == 1
    assert rec.states.count("waiting") == 1


def test_question_blocks_the_worker_until_answered(qtbot, runner):
    rec = Recorder(runner)  # no automatic answer
    new_seq(qtbot, runner, rec)
    runner.call(user_interaction.boolean_response, "Well?", "", "suffix", ["debug"])
    qtbot.waitUntil(lambda: bool(rec.questions), timeout=10000)
    q = rec.questions[0]
    assert q.extra == ("", "suffix", ["debug"])
    assert runner.pending_question is q
    qtbot.wait(200)
    assert not any(d[0] == "call" for d in rec.done)  # still blocked
    runner.answer(q, 0)
    assert wait_done(qtbot, rec, "call") == 0
    assert runner.pending_question is None


def test_response_question_round_trip(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: q.choices[0])
    new_seq(qtbot, runner, rec)
    runner.call(user_interaction.response, ["Yes", "No"], "Does this generate it?")
    assert wait_done(qtbot, rec, "call") == "Yes"
    (q,) = rec.questions
    assert (q.kind, q.text, q.choices) == ("response", "Does this generate it?", ("Yes", "No"))


def test_more_terms_question_inserts_the_terms(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: "  4, 5 6 ")
    new_seq(qtbot, runner, rec)
    runner.call(all_mx._ask_for_more_terms)
    wait_done(qtbot, rec, "call")
    (q,) = rec.questions
    assert q.kind == "more_terms"
    assert [int(e.get_mag()) for e in sworkspace.get_elements()] == [1, 1, 2, 1, 2, 3, 4, 5, 6]
    assert rec.snapshots[-1].element_count == 9


def test_more_terms_cancelled_inserts_nothing(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec)
    runner.call(all_mx._ask_for_more_terms)
    wait_done(qtbot, rec, "call")
    assert sworkspace.ElementCount == 6


def test_snapshot_is_sent_before_a_question(qtbot, runner):
    rec = Recorder(runner)
    new_seq(qtbot, runner, rec)

    def ask_with_hilit():
        Global.hilit(1, sworkspace.get_elements()[2])
        return user_interaction.boolean_response("Hilit?")

    runner.call(ask_with_hilit)
    qtbot.waitUntil(lambda: bool(rec.questions), timeout=10000)
    assert [e.hilit for e in rec.snapshots[-1].elements][2]
    runner.answer(rec.questions[0], None)
    wait_done(qtbot, rec, "call")


def test_real_run_with_questions_does_not_deadlock(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: 1 if q.kind == "boolean" else "Yes")
    new_seq(qtbot, runner, rec, seed=3, max_steps=400, update_interval=10)
    runner.continue_()
    wait_done(qtbot, rec, "continue", timeout=120000)
    assert not rec.errors, rec.errors
    assert rec.questions  # the model asked (and got answers) on its own
    assert rec.states[-1] == "idle"


# ------------------------------------------------------------------ errors
def test_model_exception_is_signalled_and_the_runner_survives(qtbot, runner, monkeypatch):
    rec = Recorder(runner)
    new_seq(qtbot, runner, rec)

    def boom():
        raise RuntimeError("codelet exploded")

    monkeypatch.setattr(scoderack, "get_next_runnable", boom)
    runner.step()
    wait_done(qtbot, rec, "step")
    msg, tb = rec.errors[-1]
    assert "codelet exploded" in msg and "RuntimeError" in tb
    monkeypatch.undo()
    runner.step()
    wait_done(qtbot, rec, "step")
    assert len(rec.errors) == 1
    assert rec.snapshots[-1].steps == Global.Steps_Finished == 2


def test_call_errors_are_signalled(qtbot, runner):
    rec = Recorder(runner)
    runner.call(lambda: 1 / 0)
    wait_done(qtbot, rec, "call")
    assert "ZeroDivisionError" in rec.errors[-1][1]
    runner.call(lambda: 42)
    assert wait_done(qtbot, rec, "call") == 42


# ------------------------------------------------------------------ quit, new sequence, hooks
def test_quit_while_a_question_is_pending(qtbot):
    r = Runner()
    r.start()
    rec = Recorder(r)
    r.call(user_interaction.boolean_response, "Never answered")
    qtbot.waitUntil(lambda: bool(rec.questions), timeout=10000)
    t0 = time.monotonic()
    assert r.quit(timeout=10)
    assert time.monotonic() - t0 < 5
    assert not r.is_alive()


def test_quit_while_running(qtbot):
    r = Runner()
    r.start()
    rec = Recorder(r, answer=lambda q: None)
    new_seq(qtbot, r, rec)
    r.continue_()
    qtbot.waitUntil(lambda: Global.Steps_Finished >= 3, timeout=20000)
    assert r.quit(timeout=10)
    assert not r.is_alive()
    assert r.quit(timeout=1)  # idempotent


def test_hooks_are_installed_and_restored(qtbot):
    saved = (seqsee_main.seqsee_step, seqsee_main._update_display, sworkspace._update_display,
             all_mx._update_display, all_mx._ask_for_more_terms, user_interaction._update_display,
             user_interaction.boolean_response, user_interaction.response)
    r = Runner()
    r.start()
    assert seqsee_main.seqsee_step is not saved[0]
    assert user_interaction.boolean_response is not saved[6]
    assert r.quit(timeout=10)
    assert (seqsee_main.seqsee_step, seqsee_main._update_display, sworkspace._update_display,
            all_mx._update_display, all_mx._ask_for_more_terms, user_interaction._update_display,
            user_interaction.boolean_response, user_interaction.response) == saved


def test_new_sequence_while_running_replaces_the_run(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec)
    runner.continue_()
    qtbot.waitUntil(lambda: Global.Steps_Finished >= 3, timeout=20000)
    new_seq(qtbot, runner, rec, seq="2 4 6 8", seed=5)
    assert rec.snapshots[-1].steps == 0
    assert mags(rec.snapshots[-1]) == [2, 4, 6, 8]
    runner.step()
    wait_done(qtbot, rec, "step")
    assert Global.Steps_Finished == 1


def test_new_sequence_cancels_a_pending_question(qtbot, runner):
    rec = Recorder(runner)
    new_seq(qtbot, runner, rec)
    runner.call(user_interaction.boolean_response, "Pending")
    qtbot.waitUntil(lambda: bool(rec.questions), timeout=10000)
    new_seq(qtbot, runner, rec, seq="3 3 3")
    assert sworkspace.ElementCount == 3
    assert rec.questions[0].cancelled


def test_bad_sequence_is_an_error(qtbot, runner):
    rec = Recorder(runner)
    runner.new_sequence("1 2 x")
    wait_done(qtbot, rec, "new_sequence")
    assert rec.errors


def test_same_seed_same_run(qtbot, runner):
    rec = Recorder(runner, answer=lambda q: None)
    results = []
    for _ in range(2):
        new_seq(qtbot, runner, rec, seed=11)
        runner.step_n(40)
        wait_done(qtbot, rec, "step_n")
        snap = rec.snapshots[-1]
        results.append((snap.steps, snap.current_runnable, mags(snap),
                        [(g.left, g.right, g.strength) for g in snap.groups],
                        [r.strength for r in snap.relations]))
    assert results[0] == results[1]


# ------------------------------------------------------------------ deep recursion
def _recurse(n):
    return 0 if n == 0 else 1 + _recurse(n - 1)


def test_worker_calls_have_recursion_headroom(qtbot, runner):
    rec = Recorder(runner)
    runner.call(_recurse, 20000)
    assert wait_done(qtbot, rec, "call") == 20000
    assert not rec.errors


def _frame_depth():
    depth, f = 0, sys._getframe()
    while f is not None:
        depth, f = depth + 1, f.f_back
    return depth


def test_alternating_sequence_does_not_crash(qtbot, runner, monkeypatch):
    """FindMapping on sameness recurses deeply in this sequence (loop0001 item 050b); the
    runner's steps must have the deep-stack headroom of ``interaction_step_n``."""
    deepest = [0]
    orig = SCategory.find_mapping_for_cat

    def measured(self, *args):
        depth = _frame_depth()
        if depth > deepest[0]:
            deepest[0] = depth
        return orig(self, *args)

    monkeypatch.setattr(SCategory, "find_mapping_for_cat", measured)
    rec = Recorder(runner, answer=lambda q: None)
    new_seq(qtbot, runner, rec, seq=ALTERNATING, seed=1, update_interval=25)
    runner.step_n(450)
    wait_done(qtbot, rec, "step_n", timeout=240000)
    assert not rec.errors, rec.errors[-1][1] if rec.errors else ""
    assert Global.Steps_Finished == 450
    # deeper than Python's default recursion limit allows
    assert deepest[0] > 1000
