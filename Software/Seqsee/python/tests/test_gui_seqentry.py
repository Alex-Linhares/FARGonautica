"""Sequence entry (seqsee/gui/seqentry.py, seqsee/gui/qt/seqentry.py, the main window's x
binding and more-terms requests, and the runner's ``accept_sequence``).

Mirrors lib/SGUI.pm's ask_seq (the "Seqsee Sequence Entry" Toplevel: a Tk::ComboEntry filled
from config/sequence.list, the ``$check_and_accept_input_sequence`` closure behind its
-invoke, <Return> and Go, which writes "Illformed input: …" or clears the workspace, coderack
and stream and inserts the terms) and ask_for_more_terms (the "Request for more terms"
Toplevel; UI/Graphical.pm waits for it). The golden gui_seqentry.json (oracle/gui_seqentry.pl)
records the widgets and scripted sessions with every model call they make.
"""
from pathlib import Path

import pytest

import golden
from seqsee import global_ as Global
from seqsee import scoderack, sworkspace, util
from seqsee.codelets import all_mx
from seqsee.gui import seqentry

pytestmark = pytest.mark.gui

QtCore = pytest.importorskip("PySide6.QtCore")
QtWidgets = pytest.importorskip("PySide6.QtWidgets")
from PySide6.QtCore import Qt  # noqa: E402

from seqsee.gui.qt import mainwindow  # noqa: E402
from seqsee.gui.qt import seqentry as qseqentry  # noqa: E402
from seqsee.gui.runner import Question, Runner  # noqa: E402

PY = Path(__file__).resolve().parents[1]
SCREENS_DIR = PY / "docs" / "gui" / "screens"
PERL_DIR = PY / "docs" / "gui" / "perl"
CASES = golden.load("gui_seqentry")
WIDGET = next(c for c in CASES if c["name"] == "ask_seq_widget")
SESSIONS = [c for c in CASES if c["name"] == "ask_seq_session"]
MORE_WIDGET = next(c for c in CASES if c["name"] == "more_terms_widget")
MORE_SESSIONS = [c for c in CASES if c["name"] == "more_terms_session"]
ACCEPT_CALLS = ["SWorkspace::clear", "SCoderack::clear", "MainStream::clear",
                "SWorkspace::insert_elements", "SGUI::Update"]


def inserted(step):
    """The terms a step inserted (None if it made no calls)."""
    if not step["calls"]:
        return None
    assert [c[0] for c in step["calls"]] == ACCEPT_CALLS
    return step["calls"][3][1:]


def step_text(step):
    if step["how"] == "select":
        return WIDGET["combo"]["list"][step["text"]]
    return step["text"]


class FakeRunner(QtCore.QObject):
    snapshot = QtCore.Signal(object)
    question = QtCore.Signal(object)
    question_closed = QtCore.Signal(object)
    message = QtCore.Signal(object)
    error = QtCore.Signal(str, str)
    command_done = QtCore.Signal(str, object)
    state_changed = QtCore.Signal(str)

    def __init__(self):
        super().__init__()
        self.calls = []
        self.state = "idle"

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        return lambda *a, **k: self.calls.append((name,) + a)


@pytest.fixture
def win(qtbot):
    w = mainwindow.MainWindow()
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    yield w
    for d in (w.seq_dialog, w.more_terms_dialog):
        if d is not None:
            d.close()


@pytest.fixture
def fake(win):
    r = FakeRunner()
    win.attach_runner(r)
    return r


# ---- the pure part, against the Perl closure ---------------------------------------------
def test_sequence_list_is_read_like_the_begin_block():
    assert seqentry.read_sequence_list() == WIDGET["sequence_list"]


def test_combo_list_is_sorted_like_selection_list():
    assert seqentry.combo_list(seqentry.read_sequence_list()) == WIDGET["combo"]["list"]


def test_widget_texts_match():
    slaves = WIDGET["slaves"]
    assert WIDGET["title"] == seqentry.TITLE
    assert [s["class"] for s in slaves] == ["Tk::Label", "Tk::Label", "Tk::ComboEntry",
                                            "Tk::Button"]
    assert slaves[0]["text"] == seqentry.PROMPT
    assert slaves[3]["text"] == seqentry.GO
    assert WIDGET["combo"]["width"] == seqentry.COMBO_WIDTH
    assert WIDGET["combo"]["entry_text"] == "" and WIDGET["label_text"] == ""
    assert MORE_WIDGET["title"] == seqentry.MORE_TERMS_TITLE
    assert [s["class"] for s in MORE_WIDGET["slaves"]] == ["Tk::Label", "Tk::Entry"]
    assert MORE_WIDGET["slaves"][0]["text"] == seqentry.MORE_TERMS_PROMPT
    assert MORE_WIDGET["entry_width"] == seqentry.MORE_TERMS_WIDTH


def test_opening_logs_new_sequence_started():
    """PERL-QUIRK: ask_seq logs "$seq\\n" with $seq still undef."""
    ((name, *args),) = WIDGET["opened"]["calls"]
    assert name == "Commentary::MessageRequiringNoResponse"
    assert list(seqentry.NEW_SEQUENCE_MESSAGE) == args


@pytest.mark.parametrize("session", SESSIONS, ids=[s["session"] for s in SESSIONS])
def test_check_matches_the_perl_closure(session):
    label = ""
    for step in session["steps"]:
        check = seqentry.check_sequence(step_text(step))
        if check.message is not None:
            label = check.message
        terms = inserted(step)
        assert check.accepted == (terms is not None)
        assert check.accepted == (not step["exists"])
        if check.accepted:
            assert check.terms == terms
        else:
            assert step["label"] == label


@pytest.mark.parametrize("case", MORE_SESSIONS, ids=[repr(c["text"]) for c in MORE_SESSIONS])
def test_more_terms_split_matches_perl(case):
    assert [c[0] for c in case["calls"]] == ["SWorkspace::insert_elements", "SGUI::Update"]
    assert seqentry.split_terms(case["text"]) == case["calls"][0][1:]


# ---- the Qt dialog ---------------------------------------------------------------------
@pytest.fixture
def dialog(qtbot):
    d = qseqentry.SequenceDialog()
    qtbot.addWidget(d)
    got = []
    d.accepted_terms.connect(lambda text, terms: got.append((text, list(terms))))
    d.got = got
    d.show()
    qtbot.waitExposed(d)
    return d


def test_dialog_widgets(dialog):
    assert dialog.windowTitle() == seqentry.TITLE
    assert dialog.prompt_label.text() == seqentry.PROMPT
    assert dialog.go_button.text() == seqentry.GO
    assert dialog.combo.isEditable()
    items = [dialog.combo.itemText(i) for i in range(dialog.combo.count())]
    assert items == WIDGET["combo"]["list"]
    assert dialog.combo.currentText() == ""
    assert dialog.message_label.text() == ""
    assert not dialog.isModal()
    assert dialog.focusWidget() in (dialog.combo, dialog.combo.lineEdit())  # (its proxy)
    fm = dialog.combo.lineEdit().fontMetrics()
    assert dialog.combo.lineEdit().width() >= 30 * fm.averageCharWidth()


@pytest.mark.parametrize("session", SESSIONS, ids=[s["session"] for s in SESSIONS])
def test_dialog_replays_the_sessions(qtbot, dialog, session):
    edit = dialog.combo.lineEdit()
    for step in session["steps"]:
        if step["how"] == "select":
            dialog.combo.activated.emit(step["text"] % dialog.combo.count())
        else:
            edit.clear()
            if "\t" in step["text"]:        # (a typed Tab would move the focus; paste it)
                edit.insert(step["text"])
            else:
                qtbot.keyClicks(edit, step["text"])
            if step["how"] == "return":
                qtbot.keyClick(edit, Qt.Key_Return)
            else:
                qtbot.mouseClick(dialog.go_button, Qt.LeftButton)
        terms = inserted(step)
        if terms is None:
            assert dialog.isVisible()
            assert dialog.message_label.text() == step["label"]
            assert dialog.got == []
        else:
            assert not dialog.isVisible()
            assert dialog.got == [(step_text(step), terms)]


def test_return_on_a_listed_sequence_accepts_once(qtbot, dialog):
    """An editable QComboBox also emits activated on Return when the text is an item."""
    qtbot.keyClicks(dialog.combo.lineEdit(), WIDGET["combo"]["list"][1])
    qtbot.keyClick(dialog.combo.lineEdit(), Qt.Key_Return)
    assert dialog.got == [(WIDGET["combo"]["list"][1], WIDGET["combo"]["list"][1].split())]
    assert dialog.combo.count() == len(WIDGET["combo"]["list"])   # nothing inserted


def test_more_terms_dialog(qtbot):
    d = qseqentry.MoreTermsDialog()
    qtbot.addWidget(d)
    got = []
    d.entered.connect(got.append)
    d.show()
    qtbot.waitExposed(d)
    assert d.windowTitle() == seqentry.MORE_TERMS_TITLE
    assert d.prompt_label.text() == seqentry.MORE_TERMS_PROMPT
    assert d.focusWidget() is d.entry
    qtbot.keyClicks(d.entry, " 7 8 ")
    qtbot.keyClick(d.entry, Qt.Key_Return)
    assert got == [" 7 8 "] and not d.isVisible()


def test_more_terms_dialog_closed_without_answer(qtbot):
    d = qseqentry.MoreTermsDialog()
    qtbot.addWidget(d)
    got, closed = [], []
    d.entered.connect(got.append)
    d.dismissed.connect(lambda: closed.append(1))
    d.show()
    d.close()
    assert got == [] and closed == [1]
    d.close_quietly()               # already closed: nothing more
    assert closed == [1]


# ---- the main window -------------------------------------------------------------------
def _commentary_text(win):
    return "".join(r[0] for r in win.commentary.log.runs())


def test_x_opens_the_dialog_and_logs(qtbot, win, fake):
    win.run_actions["x"].trigger()
    d = win.seq_dialog
    assert isinstance(d, qseqentry.SequenceDialog) and d.isVisible()
    assert _commentary_text(win) == "New Sequence Started: \n"
    win.run_actions["x"].trigger()          # the open dialog is raised, not duplicated
    assert win.seq_dialog is d
    assert _commentary_text(win) == "New Sequence Started: \n"
    assert fake.calls == []


def test_dialog_accept_drives_the_runner(qtbot, win, fake):
    win.known_families = ("x",)
    win.run_actions["x"].trigger()
    edit = win.seq_dialog.combo.lineEdit()
    qtbot.keyClicks(edit, "1 x")
    qtbot.keyClick(edit, Qt.Key_Return)
    assert fake.calls == [] and win.seq_dialog.isVisible()
    assert win.seq_dialog.message_label.text() == "Illformed input: 1 x"
    edit.clear()
    qtbot.keyClicks(edit, "1, 2 3")
    qtbot.keyClick(edit, Qt.Key_Return)
    assert fake.calls == [("accept_sequence", ["1", "2", "3"])]
    assert win.known_families == ()
    assert win.last_sequence == "1, 2 3"
    assert not win.seq_dialog.isVisible()
    # the next x opens a fresh dialog
    win.run_actions["x"].trigger()
    assert win.seq_dialog.isVisible() and win.seq_dialog.message_label.text() == ""


def test_accept_without_a_runner_says_so(qtbot, win):
    win.accept_sequence("1 2", ["1", "2"])
    assert "No model" in win.statusBar().currentMessage()


def test_accepted_sequence_reapplies_sleep_and_debug_max(win, fake):
    win.interstep_sleep = 12
    win.debug_max = 1
    fake.command_done.emit("accept_sequence", ["1"])
    assert ("set_interstep_sleep", 12) in fake.calls
    assert ("set_debug_max", 1) in fake.calls


def test_more_terms_question_opens_the_dialog(qtbot, win, fake):
    q = Question("more_terms", seqentry.MORE_TERMS_PROMPT)
    win.show_question(q)
    d = win.more_terms_dialog
    assert d.isVisible() and d.windowTitle() == seqentry.MORE_TERMS_TITLE
    assert not win.attention_needed          # SGUI::ask_for_more_terms doesn't call it
    assert win.commentary.pending is None
    qtbot.keyClicks(d.entry, "7 8")
    qtbot.keyClick(d.entry, Qt.Key_Return)
    assert fake.calls == [("answer", q, "7 8")]
    assert not d.isVisible()


def test_more_terms_dialog_closed_answers_none(qtbot, win, fake):
    q = Question("more_terms", seqentry.MORE_TERMS_PROMPT)
    win.show_question(q)
    win.more_terms_dialog.close()
    assert fake.calls == [("answer", q, None)]


def test_cancelled_more_terms_question_closes_the_dialog(qtbot, win, fake):
    q = Question("more_terms", seqentry.MORE_TERMS_PROMPT)
    win.show_question(q)
    q.cancelled = True
    win.question_closed(q)
    assert not win.more_terms_dialog.isVisible()
    assert fake.calls == []                  # the runner already gave up on it


# ---- the runner ------------------------------------------------------------------------
@pytest.fixture
def runner(qtbot):
    r = Runner(min_interval=1 / 30)
    r.start()
    yield r
    assert r.quit(timeout=10), "the worker did not stop"


class Rec:
    def __init__(self, runner, answers=()):
        self.done, self.errors, self.questions, self.snapshots = [], [], [], []
        self.answers = list(answers)
        runner.command_done.connect(lambda n, r: self.done.append((n, r)))
        runner.error.connect(lambda m, tb: self.errors.append(m))
        runner.snapshot.connect(self.snapshots.append)
        runner.question.connect(self._q)
        self.runner = runner

    def _q(self, q):
        self.questions.append(q)
        if self.answers:
            self.runner.answer(q, self.answers.pop(0))

    def wait(self, qtbot, name, timeout=20000):
        n = sum(1 for d in self.done if d[0] == name)
        qtbot.waitUntil(lambda: sum(1 for d in self.done if d[0] == name) > n,
                        timeout=timeout)
        return [d for d in self.done if d[0] == name][-1][1]


def _mags():
    return [util.perl_str(e.get_mag()) for e in sworkspace.get_elements()]


def test_first_accept_initializes_then_clears_and_inserts(qtbot, runner):
    """Seqsee.pl's INITIALIZE without a sequence, then the closure: workspace, coderack and
    stream cleared (so the coderack starts empty, as in Perl), the terms inserted."""
    rec = Rec(runner)
    runner.accept_sequence(["1", "1", "2", "1", "2", "3"])
    rec.wait(qtbot, "accept_sequence")
    assert not rec.errors, rec.errors
    assert runner.options is not None and runner.options["seq"] == []
    assert _mags() == ["1", "1", "2", "1", "2", "3"]
    assert scoderack.CODELET_COUNT == 0
    assert Global.MainStream.older_thoughts == []
    assert rec.snapshots[-1].element_count == 6
    runner.step_n(20)
    rec.wait(qtbot, "step_n")
    assert not rec.errors, rec.errors
    assert Global.Steps_Finished == 20


def test_accept_during_a_run_keeps_the_step_count(qtbot, runner):
    """SGUI::ask_seq doesn't restart: Steps_Finished goes on, the seed isn't reset."""
    rec = Rec(runner, answers=[0] * 50)
    runner.new_sequence("1 1 2 1 2 3", seed=3)
    rec.wait(qtbot, "new_sequence")
    runner.crawl(5)
    qtbot.waitUntil(lambda: Global.Steps_Finished >= 3, timeout=20000)
    runner.accept_sequence(["7", "1", "7", "2"])
    rec.wait(qtbot, "accept_sequence")
    assert not rec.errors, rec.errors
    steps = Global.Steps_Finished
    assert steps >= 3
    assert _mags() == ["7", "1", "7", "2"]
    assert scoderack.CODELET_COUNT == 0
    assert Global.MainStream.older_thoughts == [] and Global.MainStream.current_thought == ""
    runner.step()
    rec.wait(qtbot, "step")
    assert Global.Steps_Finished == steps + 1


def test_accept_with_a_bad_term_is_a_model_error(qtbot, runner):
    """"1-2" passes the closure's check but SWorkspace's insert dies (Perl: "Huh?"), after
    the workspace was cleared."""
    rec = Rec(runner)
    runner.new_sequence("1 2 3", seed=1)
    rec.wait(qtbot, "new_sequence")
    runner.accept_sequence(seqentry.check_sequence("1-2 3").terms)
    rec.wait(qtbot, "accept_sequence")
    assert rec.errors and "Huh?" in rec.errors[-1]
    assert _mags() == []


def test_more_terms_empty_answer_still_inserts(qtbot, runner):
    """Perl calls insert_elements() even with no terms, which sets TimeOfLastNewElement."""
    rec = Rec(runner, answers=["   "])
    runner.new_sequence("1 2 3", seed=1)
    rec.wait(qtbot, "new_sequence")
    runner.step_n(4)
    rec.wait(qtbot, "step_n")
    Global.TimeOfLastNewElement = 0
    runner.call(all_mx._ask_for_more_terms)
    rec.wait(qtbot, "call")
    assert Global.TimeOfLastNewElement == Global.Steps_Finished == 4
    assert _mags() == ["1", "2", "3"]


def test_more_terms_bad_input_asks_again(qtbot, runner):
    """In Perl the insert dies inside the Entry's <Return> callback, before $top->destroy:
    the window stays open (terms before the bad one are in) and waitWindow goes on."""
    rec = Rec(runner, answers=["4 a", "5"])
    runner.new_sequence("1 2 3", seed=1)
    rec.wait(qtbot, "new_sequence")
    runner.call(all_mx._ask_for_more_terms)
    rec.wait(qtbot, "call")
    assert len(rec.questions) == 2
    assert rec.errors and "Huh?" in rec.errors[0]
    assert _mags() == ["1", "2", "3", "4", "5"]


# ---- screenshots -----------------------------------------------------------------------
def test_seqentry_screenshots(qtbot, dialog, tmp_path, request):
    """The dialog after an illformed input (Perl: docs/gui/perl/seqentry.png) and the
    more-terms window (more_terms.png). ``pytest --write-screens`` saves them under
    docs/gui/screens/."""
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    assert (PERL_DIR / "seqentry.png").exists() and (PERL_DIR / "more_terms.png").exists()
    edit = dialog.combo.lineEdit()
    qtbot.keyClicks(edit, "1 x 2")
    qtbot.keyClick(edit, Qt.Key_Return)
    qtbot.wait(10)
    assert dialog.grab().save(str(out_dir / "seqentry.png"), "PNG")
    m = qseqentry.MoreTermsDialog()
    qtbot.addWidget(m)
    m.show()
    qtbot.waitExposed(m)
    m.entry.setText("7 8")
    qtbot.wait(10)
    assert m.grab().save(str(out_dir / "more_terms.png"), "PNG")
    m.close_quietly()
    assert (SCREENS_DIR / "seqentry.png").exists()
    assert (SCREENS_DIR / "more_terms.png").exists()
