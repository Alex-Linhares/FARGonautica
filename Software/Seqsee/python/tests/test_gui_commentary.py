"""The commentary (seqsee/gui/commentary.py, seqsee/gui/qt/commentary.py, the main window's
Commentary dock and the runner's message hooks).

Mirrors lib/Tk/SCommentary.pm (Populate, MessageRequiringNoResponse, MessageRequiringAResponse,
MessageRequiringBooleanResponse, the 1-4 key bindings and "Start Debug"), as lib/SGUI.pm's
CreateWidgets builds it from config/GUI_sparse.conf ([SCommentary], [SCommentary_tags]), and
lib/UI/Graphical.pm's main::message and main::ask_user_extension. The golden
gui_commentary.json (oracle/gui_commentary.pl) records the widget's configuration and scripted
sessions: after each call, the return value, the buttons while the question waited, the
AttentionNeeded flag, the text as runs of [text, [tags]] and the globals.
"""
import dataclasses
from pathlib import Path

import pytest

import golden
import gui_recipes
from seqsee import global_ as Global
from seqsee import scodelet_base, user_interaction
from seqsee.codelets import all_mx
from seqsee.gui import commentary as cm
from seqsee.gui import snapshot

pytestmark = pytest.mark.gui

QtCore = pytest.importorskip("PySide6.QtCore")
QtGui = pytest.importorskip("PySide6.QtGui")
QtWidgets = pytest.importorskip("PySide6.QtWidgets")
Qt = QtCore.Qt

from seqsee.gui import runner as runner_mod  # noqa: E402
from seqsee.gui.qt import commentary as qcm  # noqa: E402
from seqsee.gui.qt import mainwindow, render  # noqa: E402
from seqsee.gui.runner import Question, Runner  # noqa: E402

PY = Path(__file__).resolve().parents[1]
SCREENS_DIR = PY / "docs" / "gui" / "screens"
PERL_DIR = PY / "docs" / "gui" / "perl"
CASES = golden.load("gui_commentary")
WIDGET = next(c for c in CASES if c["kind"] == "widget")
SESSIONS = {c["name"]: c["steps"] for c in CASES if c["kind"] == "session"}
SEQ = "1 1 2 1 2 3"
_KEYS = {1: Qt.Key_1, 2: Qt.Key_2, 3: Qt.Key_3, 4: Qt.Key_4}


# ------------------------------------------------------------------ configuration (pure)
def test_text_widget_config():
    text = WIDGET["text"]
    assert cm.TEXT_FONT == text["font"]["xlfd"]
    assert (cm.TEXT_HEIGHT, cm.TEXT_WIDTH, cm.TEXT_WRAP) == (
        text["height"], text["width"], text["wrap"])


def test_tags_config():
    assert cm.TAG_CONFIG == WIDGET["tags_config"]
    for name, perl in WIDGET["tags"].items():
        assert cm.TAGS[name].foreground == perl["foreground"]
        assert cm.TAGS[name].font == (perl["font"]["xlfd"] if "font" in perl else None)
    assert list(cm.TAG_PRIORITY[:len(WIDGET["tag_priority"])]) == WIDGET["tag_priority"]


def test_style_of_overlapping_tags():
    # 'green' is above 'debug' in Tk's priority: its foreground wins, debug's font stays.
    s = cm.style_of(("debug", "green"))
    assert s.foreground == "#00FF00" and s.font == cm.TAGS["debug"].font
    assert cm.style_of(()) == cm.TagStyle()
    assert cm.style_of(("unknown",)) == cm.TagStyle()


def test_buttons_config():
    *answer, debug = WIDGET["buttons"]
    assert len(answer) == cm.BUTTON_COUNT
    for b in answer:
        assert b == {"text": "", "width": str(cm.BUTTON_WIDTH), "state": "disabled",
                     "side": "top"}
    assert debug == {"text": cm.DEBUG_BUTTON_TEXT, "width": str(cm.BUTTON_WIDTH),
                     "state": "normal", "side": "bottom"}


def test_insert_runs_and_log():
    assert cm.insert_runs("a", "green", "b", [], "c", ["x", "y"], "d") == [
        ("a", ("green",)), ("b", ()), ("c", ("x", "y")), ("d", ())]
    assert cm.insert_runs("", ["green"], 3) == [("3", ())]
    log = cm.Log()
    log.insert("a", [], "b\n")
    log.insert("c", ["green", "debug"])
    assert log.runs() == [("ab\n", []), ("c", ["debug", "green"])]
    assert log.text() == "ab\nc"


def test_message_request():
    assert cm.message_request("hi", 1) == (("hi\n",), None)
    assert cm.message_request(["F", ["codelet_family"], " x"], 1) == (
        ("F", ["codelet_family"], " x"), None)
    assert cm.message_request("stop") == (("stop",), ("continue",))
    assert cm.message_request(["F", "green", "run"], 0) == (("F", "green", "run"), ("continue",))
    assert cm.debug_message(1) == ("debugMAX=1\n",)


def test_key_choice_and_boolean_value():
    assert [cm.key_choice(k, 2) for k in (1, 2, 3, 4, 5)] == [0, 1, None, None, None]
    assert cm.boolean_value("yes") == 1 and cm.boolean_value("no") == 0


# ------------------------------------------------------------------ the dock
@pytest.fixture
def win(qtbot):
    w = mainwindow.MainWindow()
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


def _button_states(win):
    c = win.commentary
    return [{"text": b.text(), "state": "normal" if b.isEnabled() else "disabled"}
            for b in [*c.buttons, c.debug_button]]


def _ask(qtbot, win, question, actions):
    """Show ``question`` as the runner would; record the waiting state; act; return the
    answer the commentary gave (or the sentinel if none)."""
    got = []

    def record(q, v):
        got.append((q, v))

    win.commentary.answered.connect(record)
    win.show_question(question)
    waiting = {"buttons": _button_states(win), "attention": int(win.attention_needed)}
    for how, n in actions:
        if how == "button":
            qtbot.mouseClick(win.commentary.buttons[n], Qt.LeftButton)
        else:
            qtbot.keyClick(win.commentary.text, _KEYS[n])
    win.commentary.answered.disconnect(record)
    assert len(got) == 1 and got[0][0] is question, got
    return got[0][1], waiting


def _replay_step(qtbot, win, step):
    call, args, actions = step["call"], step["args"], step["answer"]
    waiting = None
    ret = None
    if call == "no_response":
        win.commentary.insert(*args)
    elif call == "response":
        q = Question("response", cm.parts_text(args[1:]), choices=args[0], parts=args[1:])
        ret, waiting = _ask(qtbot, win, q, actions)
    elif call == "boolean":
        q = Question("boolean", args[0], extra=args[1:], parts=args)
        ret, waiting = _ask(qtbot, win, q, actions)
    elif call == "message":
        parts, choices = cm.message_request(*args)
        if choices is None:
            win.commentary.insert(*parts)
        else:
            q = Question("response", cm.parts_text(parts), choices=choices, parts=parts)
            ret, waiting = _ask(qtbot, win, q, actions)
    elif call == "ask_user_extension":
        items, suffix, setup = (list(args) + [None, None])[:3]
        setup = setup or {}
        Global.Feature = dict(setup.get("feature") or {})
        for key in setup.get("rejected") or ():
            Global.ExtensionRejectedByUser[key] = 1
        seen = []

        def boolean_response(question, *rest):
            q = Question("boolean", question, extra=rest, parts=(question, *rest))
            value, w = _ask(qtbot, win, q, actions)
            seen.append(w)
            return value

        user_interaction.install(boolean_response=boolean_response)
        ret = user_interaction.gui_ask_user_extension(items, suffix)
        waiting = seen[0] if seen else None
    elif call == "start_debug":
        qtbot.mouseClick(win.commentary.debug_button, Qt.LeftButton)
    else:
        raise AssertionError(call)
    return ret, waiting


@pytest.mark.parametrize("name", sorted(SESSIONS))
def test_sessions_match_perl(qtbot, win, name):
    win.commentary.clear()
    Global.AtLeastOneUserVerification = 0
    Global.ExtensionRejectedByUser = {}
    Global.Feature = {}
    win.debug_max = 0
    for step in SESSIONS[name]:
        ret, waiting = _replay_step(qtbot, win, step)
        where = (step["call"], step["args"])
        assert ret == step["returned"], where
        assert waiting == step["waiting"], where
        assert [list(r) for r in win.commentary.log.runs()] == step["runs"], where
        assert win.commentary.log.text() == step["text"], where
        assert win.commentary.text.toPlainText() == step["text"], where
        assert not win.attention_needed
        assert win.commentary.pending is None
        g = step["globals"]
        if step["call"] == "ask_user_extension":
            assert util_num(Global.AtLeastOneUserVerification) == g["AtLeastOneUserVerification"]
        if step["call"] == "start_debug":
            assert win.debug_max == g["debugMAX"]


def util_num(v):
    return int(v or 0)


def test_text_shows_the_tag_colours_and_fonts(win):
    c = win.commentary
    c.insert("plain ", [], "Fam", ["codelet_family"], " both", ["debug", "green"],
             " red", ["user_response"], "\nnext line", "green")
    pos = 0
    default = c.text.palette().color(QtGui.QPalette.Text).name().upper()
    for text, tags in c.log.runs():
        cur = QtGui.QTextCursor(c.text.document())
        cur.setPosition(pos + 1)
        fmt = cur.charFormat()
        style = cm.style_of(tags)
        colour = fmt.foreground().color().name().upper() if fmt.hasProperty(
            QtGui.QTextFormat.ForegroundBrush) else default
        assert colour == (style.foreground or default), (text, tags)
        want_font = render.qfont(style.font or cm.TEXT_FONT)
        assert fmt.font().pixelSize() == want_font.pixelSize(), (text, tags)
        assert fmt.font().bold() == want_font.bold()
        pos += len(text)


def test_text_widget_is_read_only_word_wrapped_and_sized(win):
    t = win.commentary.text
    assert t.isReadOnly()
    assert t.lineWrapMode() == QtWidgets.QTextEdit.WidgetWidth
    assert t.wordWrapMode() == QtGui.QTextOption.WrapAtWordBoundaryOrAnywhere
    assert t.font().pixelSize() == 17 and t.font().bold()
    fm = QtGui.QFontMetrics(t.font())
    hint = t.sizeHint()
    assert hint.height() >= cm.TEXT_HEIGHT * fm.lineSpacing()
    assert hint.width() >= cm.TEXT_WIDTH * fm.averageCharWidth()


def test_initial_buttons(win):
    assert _button_states(win) == [{"text": "", "state": "disabled"}] * 4 + [
        {"text": "Start Debug", "state": "normal"}]
    assert win.commentary.pending is None
    assert win.commentary_dock.widget() is win.commentary
    assert win.commentary_dock.objectName() == "commentary"
    assert win.dockWidgetArea(win.commentary_dock) == Qt.BottomDockWidgetArea


def test_text_follows_the_end(win, qtbot):
    for i in range(60):
        win.commentary.insert(f"line {i}\n")
    qtbot.wait(10)
    bar = win.commentary.text.verticalScrollBar()
    assert bar.maximum() > 0 and bar.value() == bar.maximum()


def test_keys_past_the_active_buttons_and_without_a_question_do_nothing(qtbot, win):
    got = []
    win.commentary.answered.connect(lambda q, v: got.append(v))
    qtbot.keyClick(win.commentary.text, Qt.Key_1)
    assert got == [] and win.commentary.log.text() == ""
    q = Question("response", "Pick", choices=("a", "b"), parts=("Pick",))
    win.show_question(q)
    qtbot.keyClick(win.commentary.text, Qt.Key_3)
    assert got == []
    qtbot.keyClick(win.commentary.text, Qt.Key_2)
    assert got == ["b"]
    qtbot.keyClick(win.commentary.text, Qt.Key_1)
    assert got == ["b"]


def test_a_cancelled_question_resets_the_buttons(win):
    q = Question("boolean", "Is the next term 3?", parts=("Is the next term 3?",))
    win.show_question(q)
    assert win.attention_needed and win.commentary.pending is q
    q.cancelled = True
    win.question_closed(q)
    assert win.commentary.pending is None and not win.attention_needed
    assert _button_states(win)[:4] == [{"text": "", "state": "disabled"}] * 4
    assert win.commentary.log.text() == "Is the next term 3?  (cancelled)\n"


def test_more_terms_questions_are_not_the_commentarys(win):
    q = Question("more_terms", runner_mod.MORE_TERMS_TEXT)
    win.show_question(q)
    assert win.commentary.pending is None
    assert _button_states(win)[0] == {"text": "", "state": "disabled"}


def test_start_debug_toggles_debug_max_and_says_so(qtbot, win):
    qtbot.mouseClick(win.commentary.debug_button, Qt.LeftButton)
    assert win.debug_max == 1 and win.run_actions["m"].isChecked()
    qtbot.mouseClick(win.commentary.debug_button, Qt.LeftButton)
    assert win.debug_max == 0
    assert win.commentary.log.text() == "debugMAX=1\ndebugMAX=0\n"


def test_the_m_binding_does_not_log(qtbot, win):
    win.run_actions["m"].trigger()
    assert win.debug_max == 1 and win.commentary.log.text() == ""


# ------------------------------------------------------------------ the runner's bridge
class Recorder:
    def __init__(self, runner):
        self.messages, self.questions, self.done = [], [], []
        runner.message.connect(self.messages.append)
        runner.question.connect(self.questions.append)
        runner.command_done.connect(lambda n, r: self.done.append((n, r)))


@pytest.fixture
def runner(qtbot):
    r = Runner()
    r.start()
    yield r
    assert r.quit(timeout=10)


def test_runner_hooks_main_message(qtbot, runner):
    rec = Recorder(runner)
    runner.call(all_mx._message, "I believe I got it", 1)
    qtbot.waitUntil(lambda: bool(rec.done), timeout=10000)
    assert rec.messages == [("I believe I got it\n",)] and not rec.questions
    runner.call(scodelet_base._message, ["Reader", "green", "About to run: x"])
    qtbot.waitUntil(lambda: bool(rec.questions), timeout=10000)
    (q,) = rec.questions
    assert (q.kind, q.choices, q.parts) == ("response", ("continue",),
                                           ("Reader", "green", "About to run: x"))
    assert q.text == "ReaderAbout to run: x"
    runner.answer(q, "continue")
    qtbot.waitUntil(lambda: len(rec.done) == 2, timeout=10000)
    assert rec.done[-1] == ("call", "continue")


def test_runner_question_parts(qtbot, runner):
    rec = Recorder(runner)
    runner.call(user_interaction.boolean_response, "Is the next term 7?", "", "why", ["debug"])
    qtbot.waitUntil(lambda: bool(rec.questions), timeout=10000)
    q = rec.questions[0]
    assert q.parts == ("Is the next term 7?", "", "why", ["debug"])
    runner.answer(q, 1)
    runner.call(user_interaction.response, ["Yes", "No"], "Right?")
    qtbot.waitUntil(lambda: len(rec.questions) == 2, timeout=10000)
    assert rec.questions[1].parts == ("Right?",)
    runner.answer(rec.questions[1], "Yes")
    qtbot.waitUntil(lambda: len(rec.done) == 2, timeout=10000)


def test_runner_restores_the_message_hooks(qtbot):
    saved = [(m, getattr(m, "_message")) for m in runner_mod.MESSAGE_HOOK_MODULES]
    assert len(saved) >= 8
    r = Runner()
    r.start()
    assert all(getattr(m, "_message") is not f for m, f in saved)
    assert r.quit(timeout=10)
    assert all(getattr(m, "_message") is f for m, f in saved)


def test_window_answers_a_real_runner(qtbot, win):
    r = Runner()
    win.attach_runner(r)
    r.start()
    try:
        done = []
        r.command_done.connect(lambda n, res: done.append((n, res)))
        Global.AtLeastOneUserVerification = 0
        r.call(user_interaction.ask_user_extension, [3])
        qtbot.waitUntil(lambda: win.commentary.pending is not None, timeout=10000)
        assert win.attention_needed and win.run_state == "waiting"
        assert _button_states(win)[:2] == [{"text": "yes", "state": "normal"},
                                           {"text": "no", "state": "normal"}]
        qtbot.mouseClick(win.commentary.buttons[0], Qt.LeftButton)
        qtbot.waitUntil(lambda: bool(done), timeout=10000)
        assert done == [("call", 1)]
        assert Global.AtLeastOneUserVerification == 1
        assert win.commentary.log.text() == "Is the next term 3?  yes\n"
        assert not win.attention_needed
        r.call(all_mx._message, "That finishes the description!", 1)
        qtbot.waitUntil(lambda: len(done) == 2, timeout=10000)
        assert win.commentary.log.text().endswith("That finishes the description!\n")
    finally:
        assert r.quit(timeout=10)


def test_model_errors_are_logged(qtbot, win):
    r = Runner()
    win.attach_runner(r)
    r.start()
    try:
        r.call(lambda: 1 / 0)
        qtbot.waitUntil(lambda: "ZeroDivisionError" in win.commentary.log.text()
                        or "division" in win.commentary.log.text(), timeout=10000)
        (run,) = [x for x in win.commentary.log.runs() if cm.ERROR_TAG in x[1]]
        assert run[0].startswith("Model error: ") and run[0].endswith("\n")
    finally:
        assert r.quit(timeout=10)


# ------------------------------------------------------------------ screenshot
def test_commentary_screenshot(qtbot, win, tmp_path, request):
    """The dock in the state of Perl's docs/gui/perl/commentary.png (a question waiting).
    ``pytest --write-screens`` saves it under docs/gui/screens/."""
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    assert (PERL_DIR / "commentary.png").exists()
    gui_recipes.build("six_elements")
    win.show_snapshot(dataclasses.replace(snapshot.take(), steps=0))
    c = win.commentary
    c.insert("New Sequence Started: ", [], "1 1 2 1 2 3\n")
    c.insert(*cm.message_request(["Reader", "green", " About to run: SCodelet"], 1)[0])
    c.insert(*cm.message_request("\n", 1)[0])
    c.insert(*cm.message_request("I will describe the solution now!", 1)[0])
    win.show_question(Question("boolean", "Is the next term 4?",
                               parts=("Is the next term 4?",)))
    qtbot.wait(10)
    path = out_dir / "commentary.png"
    assert c.grab().save(str(path), "PNG")
    assert (SCREENS_DIR / "commentary.png").exists()
