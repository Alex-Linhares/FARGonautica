"""Loop0004 item 03: the term feed (`seekwhence/gui/models/term_feed.py`), Qt-free
(G-015).

- `readline` blocks until a line is put, gives the lines in order with their newline,
  the end of the stream once closed, and `RunStopped` once cancelled.
- A `Session` reading its terms from a feed fed `3 7 3 7` is the scripted run of those
  terms: the same events, transcript and draws [M86 pp. 164-166].
"""

import io
import threading

import pytest

from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.gui.models.run_controller import RunStopped
from seekwhence.gui.models.term_feed import TermFeed, split_words
from seekwhence.session import Session
from seekwhence.trace import event_record

TIMEOUT = 10


def read_on_a_thread(feed, count=1):
    """Read `count` lines on another thread; returns (thread, results, errors)."""
    results, errors = [], []

    def work():
        try:
            for _ in range(count):
                results.append(feed.readline())
        except BaseException as error:      # RunStopped is a BaseException
            errors.append(error)
    thread = threading.Thread(target=work, daemon=True)
    thread.start()
    return thread, results, errors


def wait_until(condition):
    for _ in range(TIMEOUT * 100):
        if condition():
            return True
        threading.Event().wait(0.01)
    return False


def test_split_words():
    assert split_words(" 3 7,3  7 ") == ["3", "7", "3", "7"]
    assert split_words("show-hypothesis") == ["show-hypothesis"]
    assert split_words("") == []


def test_readline_blocks_until_a_line_is_put():
    feed = TermFeed()
    thread, results, errors = read_on_a_thread(feed)
    assert wait_until(lambda: feed.waiting)
    assert results == []
    feed.put("3")
    thread.join(TIMEOUT)
    assert not thread.is_alive() and results == ["3\n"] and errors == []
    assert feed.read == ["3"] and feed.queued == () and not feed.waiting


def test_lines_come_in_order_and_put_words_queues_several():
    feed = TermFeed()
    assert feed.put_words("3 7 3 7") == 4
    assert feed.queued == ("3", "7", "3", "7")
    assert [feed.readline() for _ in range(4)] == ["3\n", "7\n", "3\n", "7\n"]
    assert feed.queued == () and feed.read == ["3", "7", "3", "7"]


def test_close_gives_the_end_of_the_stream_after_the_queue():
    feed = TermFeed()
    feed.put("5")
    feed.close()
    assert feed.closed
    assert feed.readline() == "5\n"
    assert feed.readline() == "" and feed.readline() == ""


def test_cancel_wakes_a_waiting_readline_with_run_stopped():
    feed = TermFeed()
    thread, results, errors = read_on_a_thread(feed)
    assert wait_until(lambda: feed.waiting)
    feed.cancel()
    thread.join(TIMEOUT)
    assert not thread.is_alive() and results == []
    assert len(errors) == 1 and isinstance(errors[0], RunStopped)
    assert feed.cancelled
    with pytest.raises(RunStopped):
        feed.readline()


def run_through_a_feed(seed, words, answers, close=False):
    """A session reading its terms from a feed fed `words`, on a thread."""
    feed, rec, out = TermFeed(), Recorder(), io.StringIO()
    session = Session(Config(), seed, out=out, lines=None, answers=list(answers), inp=feed,
                      observers=[rec])
    thread = threading.Thread(target=session.run, daemon=True)
    thread.start()
    assert wait_until(lambda: feed.waiting)
    feed.put_words(words)
    if close:
        feed.close()
    thread.join(TIMEOUT)
    assert not thread.is_alive()
    return session, rec, out.getvalue()


def test_a_session_fed_3_7_3_7_is_the_scripted_run():
    session, rec, text = run_through_a_feed(54, "3 7 3 7", ["ok"])
    plain_rec, plain_out = Recorder(), io.StringIO()
    plain = Session(Config(), 54, out=plain_out, lines=[3, 7, 3, 7], answers=["ok"],
                    observers=[plain_rec])
    plain.run()
    assert [event_record(e) for e in rec.events] == [event_record(e) for e in plain_rec.events]
    assert text == plain_out.getvalue()
    assert "please enter a term: 3\n" in text                          # echoed [M86 p. 164]
    assert session.world.rng.draws == plain.world.rng.draws
    assert session.outcome == plain.outcome == "ok"
    assert session.show_hypothesis() == "(Cycle 3 2 (3 7))"            # [M86 p. 166]
    assert session.console.asked == ["3", "7", "3", "7"]


def test_a_closed_feed_ends_the_session_with_eof():
    session, rec, text = run_through_a_feed(54, "3 7", [], close=True)
    assert session.outcome == "eof"
    assert [e.kind for e in rec.events if e.kind == "term"] == ["term", "term"]
    assert text.endswith("please enter a term: \n")
