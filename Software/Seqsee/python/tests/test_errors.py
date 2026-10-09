"""Tests for seqsee.errors.

Mirrors lib/SErr.pm, the SErr::ElementsBeyondKnownSought declaration and
ActualQuestion/WorthAsking in lib/UserInteraction.pm, SErr::ScriptReturn /
SErr::CallSubscript in lib/Seqsee/Scripts.pm, and the Exception::Class::Base
behaviour they inherit. Golden data: oracle/errors.pl -> golden/errors.json.
"""
import pytest

import golden
from seqsee import errors
from seqsee.errors import (
    SErr,
    AskUser,
    CallSubscript,
    Confess,
    ElementsBeyondKnownSought,
    ExceptionClassBase,
    FinishedTest,
    LTM_LoadFailure,
    caught,
)

CASES = golden.load("errors")


def by_op(op):
    return [c for c in CASES if c["op"] == op]


def cls_for(perl_name):
    return errors.BY_PERL_NAME[perl_name]


def test_every_perl_class_is_ported():
    assert sorted(errors.BY_PERL_NAME) == sorted(c["class"] for c in by_op("class"))


@pytest.mark.parametrize("case", by_op("class"), ids=lambda c: c["class"])
def test_class_golden(case):
    cls = cls_for(case["class"])
    assert cls.perl_name == case["class"]
    # Exception::Class does not infer a hierarchy from names: every SErr::X
    # inherits from Exception::Class::Base directly, not from SErr.
    assert cls.__bases__ == (ExceptionClassBase,)
    assert sorted(cls.fields) == case["fields"]
    e = cls()
    assert e.description == case["description"]
    assert e.message == case["message"]
    assert e.as_string() == case["as_string"]
    assert str(e) == case["stringified"]
    assert int(isinstance(e, SErr)) == case["isa_serr"]
    assert cls("hello").message == case["new_with_message"]
    assert cls(error="err").message == case["new_with_error"]
    assert cls(message="m").message == case["new_with_message_key"]

    with pytest.raises(cls):
        cls.throw("x")
    try:
        cls.throw("x")
    except ExceptionClassBase as err:
        assert int(caught(err, cls) is err) == case["caught_as_self"]
        assert int(caught(err, SErr) is not None) == case["caught_as_serr"]

    with pytest.raises(ExceptionClassBase) as info:
        cls(bogus=1)
    assert type(info.value) is ExceptionClassBase
    assert info.value.message == case["unknown_field_error"]


def test_fields_golden():
    cases = {c["class"]: c for c in by_op("fields")}
    c = cases["SErr::LTM_LoadFailure"]
    assert LTM_LoadFailure(what="bad file").what == c["what"]
    assert LTM_LoadFailure().what == c["unset"]

    c = cases["SErr::FinishedTest"]
    e = FinishedTest(got_it=1)
    assert e.got_it == c["got_it"]
    assert e.message == c["message"]

    c = cases["SErr::AskUser"]
    e = AskUser(already_matched=[1, 2], next_elements=[3], from_position=4, message="q")
    for k in ("already_matched", "next_elements", "from_position", "object",
              "direction", "message"):
        assert getattr(e, k) == c[k]

    c = cases["SErr::CallSubscript"]
    e = CallSubscript(name="foo", arguments={"a": 1})
    assert (e.name, e.arguments) == (c["name"], c["arguments"])


@pytest.mark.parametrize("case", by_op("throw"), ids=lambda c: repr(c["arg"]))
def test_throw_golden(case):
    with pytest.raises(SErr) as info:
        SErr.throw(case["arg"])
    e = info.value
    assert e.message == case["message"]
    assert e.error == case["error"]
    assert e.as_string() == case["as_string"]


def test_throw_with_fields():
    with pytest.raises(FinishedTest) as info:
        FinishedTest.throw(got_it=1)
    assert info.value.got_it == 1


def test_rethrow_golden():
    (case,) = by_op("rethrow")
    with pytest.raises(FinishedTest) as info:
        try:
            FinishedTest.throw(got_it=0)
        except FinishedTest as e:
            e.rethrow()
    assert int(caught(info.value, FinishedTest) is not None) == case["caught"]
    assert info.value.got_it == case["got_it"]


def test_caught_plain_golden():
    (case,) = by_op("caught_plain")
    err = Confess("plain\n")
    assert int(caught(err, SErr) is not None) == case["caught_class"]
    assert caught(err) is err
    assert str(caught(err)) == case["caught_any"]
    assert caught(None) is None


def test_fatal_undeclared_quirk():
    """PERL-QUIRK: SErr::Fatal is thrown in Seqsee.pm but never declared."""
    (case,) = by_op("undeclared")
    assert case["dies"] == 1
    assert issubclass(errors.Fatal, ExceptionClassBase)
    with pytest.raises(errors.Fatal):
        errors.Fatal.throw("boom")


@pytest.mark.parametrize("case", by_op("actual_question"),
                         ids=lambda c: str(c["next_elements"]))
def test_actual_question_golden(case):
    e = ElementsBeyondKnownSought(next_elements=case["next_elements"])
    if case.get("dies"):
        with pytest.raises(Confess):
            e.actual_question()
    else:
        assert e.actual_question() == case["question"]


def test_worth_asking_golden():
    (case,) = by_op("worth_asking")
    assert case["dies"] == 1
    with pytest.raises(Confess, match="Should never be called"):
        ElementsBeyondKnownSought().worth_asking()


def test_exceptions_are_python_exceptions():
    assert issubclass(ExceptionClassBase, Exception)
    with pytest.raises(Exception):
        SErr.throw("x")
