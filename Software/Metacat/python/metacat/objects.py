"""Metacat's objects: record-case closures, tell and delegate (utilities.ss).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from utilities.ss's object procedures, in
the representation decided in loop0002 item 01 (candidate C3 of
docs/python-translation-plan.md, "Objects").

An original object is a closure

    (lambda msg (let ((self (1st msg))) (record-case (rest msg) clause ...
                                          (else (delegate msg parent ...)))))

Here it is an instance of a SchemeObject subclass.  The closure's variables are
the instance's attributes; each record-case clause is a method marked with
@message("scheme-name", ...), called as method(this, self, *formals), where
`this` is the object whose closure it is and `self` the receiver named in the
message (a delegating child, or a forwarder).  The else clause is `otherwise`.
Messages keep their Scheme names as strings: tell(bond, "get-string").

Calling an object, obj(self, msg, *args), is the original's (apply obj msg) and
keeps the protocol for plain procedures; tell and delegate look the method up
directly, which is what makes C3 fast.  The engine never imports tkinter.
"""
from __future__ import annotations

import sys

from metacat import chez

# The symbol 'invalid-message-indicator.  Every producer uses this constant,
# and tell and delegate compare with `is` (a str spelled out elsewhere would
# be equal but not necessarily identical).
INVALID = sys.intern("invalid-message-indicator")


class Reset(Exception):
    """Chez's (reset): abandons the computation back to the REPL; under
    `scheme --script` the process exits (anomalies_and_quirks.md,
    "report-error-and-halt in answer-justifier")."""


def message(*names):
    """Marks a method as the record-case clause for these message names; a
    clause with several keys, ((alias1 alias2) () ...), lists them all."""
    def mark(fn):
        fn.scheme_messages = names
        return fn
    return mark


class SchemeObject:
    """A record-case closure as a class (see the module docstring)."""
    __slots__ = ()
    MESSAGES: dict = {}

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        table = {}
        for klass in reversed(cls.__mro__):
            for value in vars(klass).values():
                for name in getattr(value, "scheme_messages", ()):
                    table[name] = value
        cls.MESSAGES = table

    def __call__(this, self, msg, *args):
        method = this.MESSAGES.get(msg)
        if method is not None:
            return method(this, self, *args)
        return this.otherwise(self, msg, args)

    def otherwise(this, self, msg, args):
        """record-case with no else clause: the value is unspecified (void),
        which tell does not treat as an error (fixture record-case-no-else)."""
        return None


class Lambda(SchemeObject):
    """A plain (lambda msg ...) object: fn(self, msg, *args) receives the whole
    message.  For stand-ins and small objects that dispatch by hand."""
    __slots__ = ("fn",)

    def __init__(this, fn):
        this.fn = fn

    def otherwise(this, self, msg, args):
        return this.fn(self, msg, *args)


class Forwarder(SchemeObject):
    """chez_scheme/oracle/trace.ss's (lambda msg (apply original original (cdr msg))):
    passes every message on with the original as self."""
    __slots__ = ("original",)

    def __init__(this, original):
        this.original = original

    def otherwise(this, self, msg, args):
        return this.original(this.original, msg, *args)


def procedure_p(x) -> bool:
    """Chez: procedure?  Objects are procedures in the original; print,
    say-object and slipnode? use this to tell them from other values."""
    return callable(x)


def tell(obj, msg, *args):
    """utilities.ss: tell"""
    method = obj.MESSAGES.get(msg)
    if method is not None:
        result = method(obj, obj, *args)
    else:
        result = obj.otherwise(obj, msg, args)
    if result is INVALID:
        # looked up at call time, so that a run's driver can replace it
        # (chez_scheme/oracle/run.ss does with set!)
        return report_error_and_halt((obj, msg) + args, obj)
    return result


def report_error_and_halt(message, obj):
    """utilities.ss: report-error-and-halt (message = (obj msg arg ...)).
    1.2: an object that does not understand object-type recurses forever here
    (porting-notes.md, item 03); Python raises RecursionError."""
    # 1.2: recurses forever for an object without object-type (porting-notes.md, item 03)
    chez.printf('Ooops: bad message "~a" sent to object of type ~a~%', message[1], tell(obj, "object-type"))
    raise Reset()


def tell_all(objects, msg, *args):
    """utilities.ss: tell-all.  chez: map's order of application (fixture
    tell-all-order)."""
    # chez: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
    def tell_one(obj):
        # tell, inlined (speed, item 12)
        method = obj.MESSAGES.get(msg)
        if method is not None:
            result = method(obj, obj, *args)
        else:
            result = obj.otherwise(obj, msg, args)
        if result is INVALID:
            return report_error_and_halt((obj, msg) + args, obj)
        return result
    return chez.map_(tell_one, objects)


def delegate(self, msg, args, *objects):
    """utilities.ss: delegate.  The original's (delegate msg o1 o2 ...), with
    msg = (self name . args) split into its parts: each object in turn gets the
    same message, self included, until one does not answer invalid."""
    for obj in objects:
        method = obj.MESSAGES.get(msg)
        if method is not None:
            result = method(obj, self, *args)
        else:
            result = obj.otherwise(self, msg, args)
        if result is not INVALID:
            return result
    return INVALID


class _Invalid(Exception):
    pass


def delegate_to_all(self, msg, args, *objects):
    """utilities.ss: delegate-to-all.  Unlike delegate, each object gets
    *itself* as self; the list of answers, or invalid as soon as one object
    answers invalid.  chez: map's order of application (fixture
    delegate-to-all-order)."""
    def each(obj):
        result = obj(obj, msg, *args)
        if result is INVALID:
            raise _Invalid()
        return result
    try:
        # chez: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
        return chez.map_(each, list(objects))
    except _Invalid:
        return INVALID


class BaseObject(SchemeObject):
    """utilities.ss: base-object"""
    __slots__ = ()

    @message("object-type")
    def object_type(this, self):
        return "base-object"

    def otherwise(this, self, msg, args):
        return INVALID


base_object = BaseObject()
BASE_OBJECT = base_object
