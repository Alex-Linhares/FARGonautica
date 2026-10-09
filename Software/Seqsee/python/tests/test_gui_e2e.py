"""End-to-end GUI run (loop0002 item 022): the main window (seqsee/gui/app.py's ``build``,
seqsee/gui/qt/mainwindow.py) driven through a whole run on ``1 1 2 1 2 3`` by an automatic
answerer (seqsee/gui/autoanswer.py, seqsee/gui/qt/autoanswer.py) that answers in the
Commentary as a user would.

Mirrors Seqsee.pl with Tk (Interaction_continue from GUI_sparse.conf's c binding; the
questions of lib/UI/Graphical.pm's ask_user_extension and lib/Seqsee.pm's ask, shown by
lib/Tk/SCommentary.pm; DescribeSolution's "Does this generate the sequence you had in
mind?") and lib/Tk/Seqsee.pm's 11 @ViewOptions, drawn at the end of the run.

Perl can't replay the Python run's trajectory, so the Perl screenshots of the same views come
from a deterministic state modelled on the run's end: the ``solution`` recipe
(tests/gui_recipes.py / oracle/GuiRecipes.pm), drawn by oracle/gui_views.pl in all 11 views
(golden cases in tests/golden/gui_views.json, compared in test_gui_views.py; PNGs
docs/gui/perl/views_<view>_solution.png). The run's screenshots are
docs/gui/screens/e2e_run_<view>.png (``pytest --write-screens``).
"""
from pathlib import Path

import pytest

import gui_recipes
from seqsee import cli
from seqsee import global_ as Global
from seqsee.gui import app, autoanswer, snapshot
from seqsee.gui.draw import views

PY = Path(__file__).resolve().parents[1]
SCREENS_DIR = PY / "docs" / "gui" / "screens"
SEQ = "1 1 2 1 2 3"
CONTINUATION = "1 2 3 4 1 2 3 4 5 1 2 3 4 5 6 1 2 3 4 5 6 7"
SEED = 7
ARGV = ["--seq", SEQ, "--seed", str(SEED), "--max-steps", "3000"]
SOLUTION = "[1, [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]"
RULES_VIEW = views.view_index("Workspace + Rules")

qt = pytest.mark.gui


# ---- the answerer's decisions (pure) -----------------------------------------------------------
@pytest.mark.parametrize("text, terms", [
    (". Is the next term 4??", [4]),
    (". Are the next 4 terms 1, 2, 3, and 4?", [1, 2, 3, 4]),
    (" Extending analogy: Are the next 4 terms 1, 2, 3, and 4?", [1, 2, 3, 4]),
    ("Is the next term 12?", [12]),
    ("Are the next terms: 7 8?", [7, 8]),       # UI/Graphical.pm's ask_user_extension
    ("Is the next term -3?", [-3]),
    ("Does this generate the sequence you had in mind?", None),
])
def test_asked_terms(text, terms):
    assert autoanswer.asked_terms(text) == terms


def test_decisions():
    a = autoanswer.Answerer(SEQ + " " + CONTINUATION)
    assert a.known[:8] == ["1", "1", "2", "1", "2", "3", "1", "2"]
    # Terms after the 6 elements on screen.
    assert a.decide("boolean", ". Is the next term 1??", (), 6) == "yes"
    assert a.decide("boolean", ". Is the next term 4??", (), 6) == "no"
    assert a.decide("boolean", ". Are the next 4 terms 1, 2, 3, and 4?", (), 6) == "yes"
    assert a.decide("boolean", ". Are the next 4 terms 1, 2, 3, and 4?", (), 7) == "no"
    # Beyond the known terms: no (and remembered).
    assert a.decide("boolean", "Is the next term 7?", (), 28) == "no"
    assert a.beyond_known == ["Is the next term 7?"]
    # A boolean question about something else: yes (cli's --answer yes).
    assert a.decide("boolean", "Shall I go on?", (), 6) == "yes"
    # DescribeSolution's confirmation; main::message's 'continue'.
    assert a.decide("response", "Does this generate the sequence you had in mind?",
                    ("Yes", "No"), 15) == "Yes"
    assert a.is_solution_question("Does this generate the sequence you had in mind?")
    assert a.decide("response", "About to run X", ("continue",), 6) == "continue"
    # More terms: the next known ones.
    assert a.decide("more_terms", "", (), 6) == "1 2 3 4"
    assert a.decide("more_terms", "", (), 10) == "1 2 3 4 5"


def test_decisions_without_a_continuation():
    a = autoanswer.Answerer(None)
    assert a.decide("boolean", "Is the next term 4?", (), 6) == "yes"
    assert a.decide("response", "Does this generate the sequence you had in mind?",
                    ("Yes", "No"), 6) == "Yes"
    assert a.decide("more_terms", "", (), 6) is None


# ---- the run ----------------------------------------------------------------------------------
@pytest.fixture
def run_window(qtbot):
    made = []

    def make():
        pytest.importorskip("PySide6.QtWidgets")
        from seqsee.gui.runner import Runner
        runner = Runner()
        done = []
        runner.command_done.connect(lambda n, r: done.append(n))
        window = app.build(app.parse_args(ARGV), runner=runner)
        qtbot.addWidget(window)
        made.append(window)
        qtbot.waitUntil(lambda: "new_sequence" in done, timeout=20000)
        return window, runner, done

    yield make
    for w in made:
        w.close()
        if w.runner is not None:
            w.runner.quit(timeout=5)


@qt
def test_full_run_reaches_the_solution(qtbot, run_window, tmp_path, request):
    """Start (c) on 1 1 2 1 2 3 with seed 7; every question is answered through the
    Commentary's buttons; the run finds the blocks 1 / 1 2 / 1 2 3 / 1 2 3 4 / 1 2 3 4 5,
    the user confirms the solution, and Pause stops the run there. The run is the headless
    run with the same seed and answers (``cli.run_headless --continuation``). Then every view
    is drawn and saved."""
    from seqsee.gui.qt.autoanswer import AutoAnswerer
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    out_dir.mkdir(parents=True, exist_ok=True)
    window, runner, done = run_window()
    window.show()
    answerer = AutoAnswerer(window, SEQ + " " + CONTINUATION)
    window.run_command("continue")
    qtbot.waitUntil(lambda: answerer.errors or window.run_state == "paused", timeout=60000)

    # The solution was found and confirmed.
    assert answerer.errors == []
    assert answerer.accepted
    # The model ends the run command after each verified extension (Break_Loop): the user
    # pressed Continue again each time.
    assert answerer.continues >= 1
    assert done.count("continue") == answerer.continues + 1
    assert answerer.answers[-1] == ("response", autoanswer.SOLUTION_QUESTION, "Yes")
    asked = [text for kind, text, _ in answerer.answers if kind == "boolean"]
    # "Is the next term 4?" — no (it is 1); then the blocks 1 2 3 4 and 1 2 3 4 5: yes.
    assert [v for k, _, v in answerer.answers if k == "boolean"] == ["no", "yes", "yes"]
    snap = window.snapshot
    mags = [e.mag for e in snap.elements]
    assert mags[:6] == [1, 1, 2, 1, 2, 3] and len(mags) > 6
    assert [str(m) for m in mags] == autoanswer.Answerer(SEQ + " " + CONTINUATION).known[
        :len(mags)]
    assert snap.largest_group.span == snap.element_count
    assert snap.largest_group.structure_string == SOLUTION
    # The window shows it: steps, the codelet count, the run state, the commentary.
    assert snap.steps == Global.Steps_Finished
    assert window.count_label.text() == str(snap.steps)
    assert window.status_labels["steps"].text() == f"Steps: {snap.steps}"
    assert window.status_labels["elements"].text() == f"Elements: {len(mags)}"
    assert window.status_labels["state"].text() == "Paused"
    log = window.commentary.text.toPlainText()
    assert autoanswer.SOLUTION_QUESTION in log
    assert all(q.strip() in log for q in asked)
    assert window.commentary.pending is None and not window.attention_needed

    # Every view at the end of the run (the run is paused: the snapshot no longer changes).
    w, h = window.canvas_size()
    for v in range(len(views.VIEW_OPTIONS)):
        window.set_view(v)
        comp = window.composition
        assert comp == views.compose(v, snap, w, h, measure=window.measure,
                                     known_families=window.known_families)
        assert (comp.died is not None) == (v == RULES_VIEW), (v, comp.died)
        path = out_dir / f"e2e_run_{v}.png"
        window.save_image(path)
        assert path.stat().st_size > 1000
    window.set_view(0)
    assert window.grab().save(str(out_dir / "e2e_run_window.png"), "PNG")

    # The same run without the GUI: same answers, same trajectory.
    gui = {"elements": mags, "steps": snap.steps, "asked": asked}
    runner.quit(timeout=5)
    headless = cli.run_headless(ARGV + ["--continuation", CONTINUATION])
    assert headless["status"] == "accepted"
    assert headless["elements"] == gui["elements"]
    assert headless["steps"] == gui["steps"]
    assert headless["asked"] == gui["asked"]

    # The Perl screenshots' state (the ``solution`` recipe) models this end state.
    gui_recipes.build("solution")
    recipe = snapshot.take()
    assert [e.mag for e in recipe.elements] == gui["elements"]
    assert recipe.steps == gui["steps"]
    assert {(g.bounds_string, g.structure_string) for g in recipe.groups} == {
        (g.bounds_string, g.structure_string) for g in snap.groups}
    assert recipe.relations == snap.relations == ()
    assert recipe.current_runnable == snap.current_runnable


@qt
def test_answerer_answers_through_the_commentary(qtbot):
    """The answerer presses the Commentary's buttons (a fake runner records the answers)
    and types more terms into SGUI::ask_for_more_terms' window."""
    pytest.importorskip("PySide6.QtWidgets")
    from PySide6.QtCore import QObject, Signal
    from seqsee.gui.qt.autoanswer import AutoAnswerer
    from seqsee.gui.qt.mainwindow import MainWindow
    from seqsee.gui.runner import Question

    class FakeRunner(QObject):
        snapshot = Signal(object)
        error = Signal(str, str)
        message = Signal(object)
        question = Signal(object)
        question_closed = Signal(object)
        state_changed = Signal(str)
        command_done = Signal(str, object)
        state = "idle"

        def __init__(self):
            super().__init__()
            self.answers = []
            self.paused = 0

        def answer(self, q, value):
            self.answers.append((q.text, value))

        def pause(self):
            self.paused += 1

        def quit(self, timeout=None):
            pass

    window = MainWindow()
    qtbot.addWidget(window)
    fake = FakeRunner()
    window.attach_runner(fake)
    gui_recipes.build("six_elements")
    window.show_snapshot(snapshot.take())
    answerer = AutoAnswerer(window, SEQ + " " + CONTINUATION)
    for q in (Question("boolean", ". Is the next term 1??", parts=(". Is the next term 1??",)),
              Question("boolean", ". Is the next term 4??", parts=(". Is the next term 4??",)),
              Question("more_terms", "more"),
              Question("response", autoanswer.SOLUTION_QUESTION, choices=("Yes", "No"),
                       parts=(autoanswer.SOLUTION_QUESTION,))):
        n = len(fake.answers)
        fake.question.emit(q)
        qtbot.waitUntil(lambda: len(fake.answers) == n + 1, timeout=3000)
    assert fake.answers == [(". Is the next term 1??", 1), (". Is the next term 4??", 0),
                            ("more", "1 2 3 4"), (autoanswer.SOLUTION_QUESTION, "Yes")]
    assert answerer.accepted and fake.paused == 1     # paused before confirming
    log = window.commentary.text.toPlainText()
    assert "yes" in log and "no" in log and "Yes" in log


def test_e2e_screens_are_committed():
    """The run's screenshots (one per view, and the window) exist."""
    names = [f"e2e_run_{v}" for v in range(len(views.VIEW_OPTIONS))] + ["e2e_run_window"]
    missing = [n for n in names if not (SCREENS_DIR / f"{n}.png").exists()]
    assert not missing, missing
