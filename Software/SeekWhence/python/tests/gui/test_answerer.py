"""Loop0004 item 02: the interactive answerer (`seekwhence/gui/models/answerer.py`),
Qt-free (G-008).

- `answer` blocks the engine's thread until `give`; the answer is written after the
  prompt as a scripted answer is [M86 p. 166], and kept in `given`.
- `cancel` wakes a waiting `answer` with `RunStopped`, and makes a later one raise too.
- `give` with no guess waiting is refused.
- A `Session` whose guesses are answered from another thread gives the same events, text,
  draws and outcome as the same session scripted (run 1 answered `ok`, run 3 answered
  `no` then `ok` [M86 pp. 166, 175-179]).
"""

import io
import threading
import time

import pytest

from seekwhence.events import Recorder
from seekwhence.gui.models.answerer import GuessPending, InteractiveAnswerer
from seekwhence.gui.models.run_controller import RunStopped
from seekwhence.session import ANSWER_PROMPT, Session
from seekwhence.trace import event_record

from golden import appendix_inputs

TIMEOUT = 10.0


class FakeConsole:
    def __init__(self):
        self.told = []

    def tell(self, prompt, text):
        self.told.append((prompt, text))


def wait_until(predicate, timeout=TIMEOUT):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return True
        time.sleep(0.002)
    return predicate()


def on_thread(function):
    """Run `function` on a thread; returns the thread and a box for its result or error."""
    box = {}

    def work():
        try:
            box["result"] = function()
        except BaseException as error:    # RunStopped is a BaseException
            box["error"] = error

    thread = threading.Thread(target=work, daemon=True)
    thread.start()
    return thread, box


def test_answer_blocks_until_given():
    console, guesses = FakeConsole(), []
    answerer = InteractiveAnswerer(console, guesses.append)
    thread, box = on_thread(lambda: answerer.answer(None, "(Cycle 3 2 (3 7))", "3 7 3 7",
                                                     (3, 7, 3, 7)))
    assert wait_until(lambda: answerer.pending is not None)
    assert guesses == [GuessPending("(Cycle 3 2 (3 7))", "3 7 3 7", (3, 7, 3, 7))]
    time.sleep(0.05)
    assert thread.is_alive() and box == {}
    assert answerer.give("ok")
    thread.join(TIMEOUT)
    assert box == {"result": "ok"}
    assert console.told == [("\n" + ANSWER_PROMPT, "ok")]
    assert answerer.given == ["ok"] and answerer.pending is None
    assert not answerer.give("ok")          # nothing waits now


def test_cancel_raises_run_stopped_on_the_engine_thread():
    answerer = InteractiveAnswerer(FakeConsole(), lambda pending: None)
    thread, box = on_thread(lambda: answerer.answer(None, "f", "b", (1,)))
    assert wait_until(lambda: answerer.pending is not None)
    answerer.cancel()
    thread.join(TIMEOUT)
    assert isinstance(box.get("error"), RunStopped)
    assert answerer.cancelled and answerer.given == []
    with pytest.raises(RunStopped):          # and every later call
        answerer.answer(None, "f", "b", (1,))
    assert not answerer.give("ok")


def test_give_without_a_guess_is_refused():
    answerer = InteractiveAnswerer(FakeConsole(), lambda pending: None)
    assert not answerer.give("ok")
    assert answerer.pending is None and answerer.given == []


def interactive_run(run_id, seed, clicks):
    """An appendix run with its guesses answered by `clicks`, one per guess, from the
    main thread; returns (records, text, draws, outcome)."""
    config, lines, _answers = appendix_inputs(run_id)
    holder, rec, out = {}, Recorder(), io.StringIO()

    def make(console):
        holder["answerer"] = InteractiveAnswerer(console, lambda pending: None)
        return holder["answerer"]

    session = Session(config, seed, out=out, lines=list(lines), observers=[rec],
                      answerer=make)
    thread, box = on_thread(session.run)
    for click in clicks:
        assert wait_until(lambda: holder["answerer"].pending is not None), click
        assert holder["answerer"].give(click)
    thread.join(TIMEOUT)
    assert "error" not in box, box
    return ([event_record(e) for e in rec.events], out.getvalue(), session.world.rng.draws,
            session.outcome)


def scripted_run(run_id, seed, answers):
    config, lines, _answers = appendix_inputs(run_id)
    rec, out = Recorder(), io.StringIO()
    session = Session(config, seed, out=out, lines=list(lines), answers=list(answers),
                      observers=[rec])
    session.run()
    return ([event_record(e) for e in rec.events], out.getvalue(), session.world.rng.draws,
            session.outcome)


@pytest.mark.parametrize("run_id, seed, answers", [
    ("run1_3737", 54, ["ok"]),
    ("run1_3737", 54, ["no"]),
    ("run3_121231234", 30, ["no", "ok"]),
])
def test_an_interactive_session_equals_the_scripted_one(run_id, seed, answers):
    assert interactive_run(run_id, seed, answers) == scripted_run(run_id, seed, answers)
