"""The typed events and the Subject that publishes them (DECISION D-055)."""

import dataclasses

import pytest

from seekwhence import events
from seekwhence.events import (EVENT_TYPES, Event, Message, Recorder, Subject, TaskChosen,
                               TermEntered)


def test_every_event_type_is_a_frozen_dataclass_with_a_kind():
    """DECISION D-055: events are frozen dataclasses of plain data, one `kind` each."""
    assert len(EVENT_TYPES) >= 9
    for kind, cls in EVENT_TYPES.items():
        assert issubclass(cls, Event)
        assert dataclasses.is_dataclass(cls) and cls.__dataclass_params__.frozen
        assert cls.kind == kind
    assert set(EVENT_TYPES) >= {"start", "term", "posted", "chosen", "stale", "cleaned",
                                "prompt", "capped", "message"}


def test_events_are_immutable():
    e = TermEntered(1, 3, 0)
    with pytest.raises(dataclasses.FrozenInstanceError):
        e.term = 4


def test_subject_publishes_in_subscription_order():
    subject = Subject()
    seen = []

    class A:
        def on_event(self, event):
            seen.append(("a", event.kind))

    class B:
        def on_event(self, event):
            seen.append(("b", event.kind))

    a, b = subject.subscribe(A()), subject.subscribe(B())
    assert subject.observers == (a, b)
    subject.publish(Message("hi"))
    assert seen == [("a", "message"), ("b", "message")]
    subject.unsubscribe(a)
    subject.publish(Message("again"))
    assert seen[-1] == ("b", "message")


def test_an_observer_that_raises_does_not_stop_the_others():
    """DECISION D-055: an observer never changes the run's course."""
    reported = []
    subject = Subject(on_error=reported.append)

    class Bad:
        def on_event(self, event):
            raise RuntimeError("boom")

    recorder = Recorder()
    subject.subscribe(Bad())
    subject.subscribe(recorder)
    subject.publish(Message("x"))
    assert [e.text for e in recorder.events] == ["x"]
    assert len(subject.errors) == 1 and len(reported) == 1
    assert isinstance(reported[0].exception, RuntimeError)
    assert reported[0].event.kind == "message"


def test_the_default_error_report_goes_to_stderr(capsys):
    subject = Subject()

    class Bad:
        def on_event(self, event):
            raise ValueError("nope")

    subject.subscribe(Bad())
    subject.publish(Message("x"))
    err = capsys.readouterr().err
    assert "seekwhence: observer" in err and "message event" in err and "nope" in err


def test_only_observers_and_events_are_accepted():
    subject = Subject()
    with pytest.raises(TypeError):
        subject.subscribe(object())
    with pytest.raises(TypeError):
        subject.publish("not an event")


def test_a_subject_is_itself_an_observer():
    outer, inner = Subject(), Subject()
    recorder = inner.subscribe(Recorder())
    outer.subscribe(inner)
    outer.publish(TaskChosen(1, 1, "Tester", (), 10, "cytoplasm"))
    assert recorder.of_kind("chosen")[0].type == "Tester"


def test_recorder_filters_by_kind():
    r = Recorder()
    r.on_event(Message("a"))
    r.on_event(TermEntered(1, 3, 0))
    r.on_event(Message("b"))
    assert [e.text for e in r.of_kind("message")] == ["a", "b"]
    assert r.of_kind("prompt") == []
    assert events.RunStarted(1, ()).kind == "start"
