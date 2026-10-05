"""Prototypes of the object representation (item 01; see docs/python-translation-plan.md).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The original's objects (utilities.ss) are closures over their state that dispatch on a
message with record-case:

    (lambda msg                         ; msg = (self message-name arg ...)
      (let ((self (1st msg)))
        (record-case (rest msg)
          (get-string () string)
          ...
          (else (delegate msg workspace-structure)))))

    (tell obj 'msg a b)    = (obj obj 'msg a b), halting on 'invalid-message-indicator
    (delegate msg p1 p2)   = the first of (p1 . msg), (p2 . msg) that understands it;
                             p1 and p2 receive the *original* self

Four candidates for Python, all built from the same message tables so that the
micro-benchmark compares like with like:

  A  closure-if    one closure per object, an if/elif chain on the message (literal)
  B  closure-dict  one closure per object plus one inner closure per message, in a dict
  C  class-dict    one class per make-... procedure; instance attributes are the
                   closure's variables; a per-class dict maps message -> function
                   (this, self, *args); the instance is callable as (self, msg, *args)
  C2               C, with tell looking the message up itself (falling back to the
                   protocol for plain closures)
  C3               C, with every object a SchemeObject (base-object and forwarders
                   too), so that tell *and* delegate look messages up directly
  D  inheritance   Python classes with methods named after the messages, parents as base
                   classes, tell = getattr by a name-mangling cache

A, B and C keep the original's protocol exactly (an object is a callable taking self,
the message and its arguments), so delegation to a separate parent object, forwarders
(chez_scheme/oracle/trace.ss) and the battery's fakes all work unchanged. D merges a
parent's state into the child, which cannot express delegation to an existing object
(graphics windows, fonts, the battery's `delegate` test).

`python3 python/tests/object_prototypes.py` prints the benchmark as a Markdown table.
"""
from __future__ import annotations

import sys
import timeit

# The symbol 'invalid-message-indicator.  Symbols are Python str (plan, "Symbols and
# strings"); this one is compared with `is`, so every producer must use INVALID.
INVALID = sys.intern("invalid-message-indicator")


class Reset(Exception):
    """Chez's (reset): abandons the computation (compat.rkt's metacat-reset)."""


def report_error_and_halt(message, obj, out, tell_fn=None):
    """utilities.ss: report-error-and-halt (message = (obj msg arg ...))."""
    object_type = (tell_fn or tell)(obj, "object-type")
    out.append('Ooops: bad message "%s" sent to object of type %s\n' % (message[1], object_type))
    raise Reset()


# ---------------------------------------------------------------------------
# The shared protocol (A, B, C): an object is a callable obj(self, msg, *args).

def tell(obj, msg, *args, out=None):
    """utilities.ss: tell."""
    result = obj(obj, msg, *args)
    if result is INVALID:
        report_error_and_halt((obj, msg) + args, obj, out if out is not None else [])
    return result


def tell_c2(obj, msg, *args):
    """Candidate C2: tell with C's dispatch inlined (falls back to the protocol)."""
    try:
        method = obj.MESSAGES[msg]
    except (KeyError, AttributeError):
        result = obj(obj, msg, *args)
    else:
        result = method(obj, obj, *args)
    if result is INVALID:
        report_error_and_halt((obj, msg) + args, obj, [])
    return result


def delegate(self, msg, args, *objects):
    """utilities.ss: delegate.  (delegate msg o1 o2) with msg = (self name . args)."""
    for obj in objects:
        result = obj(self, msg, *args)
        if result is not INVALID:
            return result
    return INVALID


def chez_map1(f, ls):
    """Chez's one-list map: pairs from the end towards the front (compat.rkt)."""
    n = len(ls)
    out = [None] * n
    i = n - 2
    if n % 2 == 1:      # an odd last element is applied alone, before every pair
        out[n - 1] = f(ls[n - 1])
        i = n - 3
    while i >= 0:       # then the pairs, from the end towards the front
        out[i] = f(ls[i])
        out[i + 1] = f(ls[i + 1])
        i -= 2
    return out


def tell_all(objects, msg, *args):
    """utilities.ss: tell-all, through Chez's map order."""
    return chez_map1(lambda x: tell(x, msg, *args), objects)


def delegate_to_all(message, *objects):
    """utilities.ss: delegate-to-all.  Each object gets *itself* as self."""
    msg, args = message[1], message[2:]

    class Escape(Exception):
        pass

    def each(obj):
        result = obj(obj, msg, *args)
        if result is INVALID:
            raise Escape()
        return result
    try:
        return chez_map1(each, list(objects))
    except Escape:
        return INVALID


def base_object(self, msg, *args):
    """utilities.ss: base-object."""
    if msg == "object-type":
        return "base-object"
    return INVALID


# ---------------------------------------------------------------------------
# Candidate C: class-dict.

def message(*names):
    """Marks a method as the record-case clause for these message names."""
    def mark(fn):
        fn.scheme_messages = names
        return fn
    return mark


class SchemeObject:
    """A record-case closure as a class.  `this` holds the closure's variables,
    `self` is the receiver passed in the message (a delegating child, a forwarder)."""
    __slots__ = ()
    MESSAGES: dict = {}

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        table = {}
        for value in cls.__dict__.values():
            for name in getattr(value, "scheme_messages", ()):
                table[name] = value
        cls.MESSAGES = table

    def __call__(this, self, msg, *args):
        method = this.MESSAGES.get(msg)
        if method is not None:
            return method(this, self, *args)
        return this.otherwise(self, msg, args)

    def otherwise(this, self, msg, args):
        """record-case with no else clause: the value is unspecified (void)."""
        return None


# Candidate C3: C with every object a SchemeObject (base-object and forwarders too),
# so that tell and delegate can look the message up without going through __call__.

def tell_c3(obj, msg, *args, out=None):
    """utilities.ss: tell, for C3."""
    method = obj.MESSAGES.get(msg)
    if method is not None:
        result = method(obj, obj, *args)
    else:
        result = obj.otherwise(obj, msg, args)
    if result is INVALID:
        report_error_and_halt((obj, msg) + args, obj, out if out is not None else [], tell_c3)
    return result


def delegate_c3(self, msg, args, *objects):
    """utilities.ss: delegate, for C3."""
    for obj in objects:
        method = obj.MESSAGES.get(msg)
        if method is not None:
            result = method(obj, self, *args)
        else:
            result = obj.otherwise(self, msg, args)
        if result is not INVALID:
            return result
    return INVALID


class BaseObject(SchemeObject):
    """utilities.ss: base-object, as a C3 object."""
    __slots__ = ()

    @message("object-type")
    def object_type(this, self):
        return "base-object"

    def otherwise(this, self, msg, args):
        return INVALID


BASE_OBJECT = BaseObject()


class Forwarder(SchemeObject):
    """chez_scheme/oracle/trace.ss's (lambda msg (apply original original (cdr msg)))."""
    __slots__ = ("original",)

    def __init__(this, original):
        this.original = original

    def otherwise(this, self, msg, args):
        return this.original(this.original, msg, *args)


# ---------------------------------------------------------------------------
# The battery's fake object (tests/diff/utilities-battery.scm, make-fake), in each style.

LOG: list = []


def log(x):
    LOG.append(x)
    return x


def make_fake_a(type_, value):
    """Candidate A."""
    def obj(self, msg, *args):
        if msg == "object-type":
            return type_
        elif msg == "get-value":
            return value
        elif msg == "bump":
            n, = args
            log(["bump", value, n])
            return value + n
        elif msg == "both":
            a, *more = args
            return [a, more]
        elif msg == "alias1" or msg == "alias2":
            return "aliased"
        else:
            return INVALID
    return obj


def make_fake_b(type_, value):
    """Candidate B."""
    def object_type(self):
        return type_

    def get_value(self):
        return value

    def bump(self, n):
        log(["bump", value, n])
        return value + n

    def both(self, a, *more):
        return [a, list(more)]

    def aliased(self):
        return "aliased"
    methods = {"object-type": object_type, "get-value": get_value, "bump": bump,
               "both": both, "alias1": aliased, "alias2": aliased}

    def obj(self, msg, *args):
        method = methods.get(msg)
        if method is not None:
            return method(self, *args)
        return INVALID
    return obj


class Fake(SchemeObject):
    """Candidate C."""
    __slots__ = ("type_", "value")

    def __init__(this, type_, value):
        this.type_ = type_
        this.value = value

    @message("object-type")
    def object_type(this, self):
        return this.type_

    @message("get-value")
    def get_value(this, self):
        return this.value

    @message("bump")
    def bump(this, self, n):
        log(["bump", this.value, n])
        return this.value + n

    @message("both")
    def both(this, self, a, *more):
        return [a, list(more)]

    @message("alias1", "alias2")
    def aliased(this, self):
        return "aliased"

    def otherwise(this, self, msg, args):
        return INVALID


FAKE_MAKERS = {"A": make_fake_a, "B": make_fake_b, "C": Fake, "C3": Fake}


# ---------------------------------------------------------------------------
# Benchmark objects: a bond-like object of 40 messages delegating to a
# structure-like parent of 15 messages, which delegates to base-object (bonds.ss:
# make-bond -> workspace-structures.ss: make-workspace-structure).  Generated as
# source text so that every candidate gets the same tables.

CHILD = ["get-m%02d" % i for i in range(40)]
PARENT = ["get-s%02d" % i for i in range(15)]


def _a_source():
    lines = ["def make_parent_a():",
             "    state = 0",
             "    def obj(self, msg, *args):"]
    for i, m in enumerate(PARENT):
        lines.append("        %s msg == %r:" % ("if" if i == 0 else "elif", m))
        lines.append("            return state + %d" % i)
    lines += ["        else:",
              "            return delegate(self, msg, args, base_object)",
              "    return obj",
              "def make_child_a():",
              "    parent = make_parent_a()",
              "    state = 0",
              "    def obj(self, msg, *args):"]
    for i, m in enumerate(CHILD):
        lines.append("        %s msg == %r:" % ("if" if i == 0 else "elif", m))
        lines.append("            return state + %d" % i)
    lines += ["        else:",
              "            return delegate(self, msg, args, parent)",
              "    return obj"]
    return "\n".join(lines)


def _b_source():
    lines = ["def make_parent_b():", "    state = 0"]
    for i, m in enumerate(PARENT):
        lines.append("    def m%d(self): return state + %d" % (i, i))
    lines.append("    methods = {%s}" % ", ".join("%r: m%d" % (m, i) for i, m in enumerate(PARENT)))
    lines += ["    def obj(self, msg, *args):",
              "        method = methods.get(msg)",
              "        if method is not None:",
              "            return method(self, *args)",
              "        return delegate(self, msg, args, base_object)",
              "    return obj",
              "def make_child_b():",
              "    parent = make_parent_b()",
              "    state = 0"]
    for i, m in enumerate(CHILD):
        lines.append("    def m%d(self): return state + %d" % (i, i))
    lines.append("    methods = {%s}" % ", ".join("%r: m%d" % (m, i) for i, m in enumerate(CHILD)))
    lines += ["    def obj(self, msg, *args):",
              "        method = methods.get(msg)",
              "        if method is not None:",
              "            return method(self, *args)",
              "        return delegate(self, msg, args, parent)",
              "    return obj"]
    return "\n".join(lines)


def _c_source():
    lines = ["class ParentC(SchemeObject):",
             "    __slots__ = ('state',)",
             "    def __init__(this):",
             "        this.state = 0"]
    for i, m in enumerate(PARENT):
        lines += ["    @message(%r)" % m,
                  "    def m%d(this, self): return this.state + %d" % (i, i)]
    lines += ["    def otherwise(this, self, msg, args):",
              "        return delegate(self, msg, args, base_object)",
              "class ChildC(SchemeObject):",
              "    __slots__ = ('state', 'parent')",
              "    def __init__(this):",
              "        this.parent = ParentC()",
              "        this.state = 0"]
    for i, m in enumerate(CHILD):
        lines += ["    @message(%r)" % m,
                  "    def m%d(this, self): return this.state + %d" % (i, i)]
    lines += ["    def otherwise(this, self, msg, args):",
              "        return delegate(self, msg, args, this.parent)"]
    return "\n".join(lines)


def _c3_source():
    return (_c_source().replace("ParentC", "ParentC3").replace("ChildC", "ChildC3")
            .replace("delegate(", "delegate_c3(").replace("base_object", "BASE_OBJECT"))


def _pyname(msg):
    return msg.replace("-", "_")


def _d_source():
    lines = ["class BaseD:",
             "    __slots__ = ()",
             "    def object_type(self): return 'base-object'",
             "class ParentD(BaseD):",
             "    __slots__ = ('state',)",
             "    def __init__(self):",
             "        self.state = 0"]
    for i, m in enumerate(PARENT):
        lines.append("    def %s(self): return self.state + %d" % (_pyname(m), i))
    lines += ["class ChildD(ParentD):",
              "    __slots__ = ('cstate',)",
              "    def __init__(self):",
              "        ParentD.__init__(self)",
              "        self.cstate = 0"]
    for i, m in enumerate(CHILD):
        lines.append("    def %s(self): return self.cstate + %d" % (_pyname(m), i))
    return "\n".join(lines)


_PYNAMES: dict = {}


def tell_d(obj, msg, *args):
    """Candidate D's tell: message symbol -> Python method name, cached."""
    name = _PYNAMES.get(msg)
    if name is None:
        name = _PYNAMES[msg] = _pyname(msg)
    method = getattr(obj, name, None)
    if method is None:
        report_error_and_halt((obj, msg) + args, obj, [], tell_d)
    return method(*args)


_NS = dict(globals())
for _src in (_a_source(), _b_source(), _c_source(), _c3_source(), _d_source()):
    exec(_src, _NS)
make_child_a = _NS["make_child_a"]
make_child_b = _NS["make_child_b"]
ChildC = _NS["ChildC"]
ChildC3 = _NS["ChildC3"]
ChildD = _NS["ChildD"]

CANDIDATES = {
    # name: (maker, tell)
    "A closure-if": (make_child_a, tell),
    "B closure-dict": (make_child_b, tell),
    "C class-dict": (ChildC, tell),
    "C2 class-dict, inlined tell": (ChildC, tell_c2),
    "C3 class-dict, lookup inlined in tell and delegate": (ChildC3, tell_c3),
    "D inheritance": (ChildD, tell_d),
}

CASES = [
    # label, message
    ("first message", CHILD[0]),
    ("20th of 40 messages", CHILD[20]),
    ("40th of 40 messages", CHILD[39]),
    ("delegated, 8th of parent's 15", PARENT[7]),
    ("delegated twice (to base-object)", "object-type"),
]


def bench(number=20000, repeat=5):
    """ns per operation for every candidate and case, plus object creation."""
    results = {}
    for name, (maker, tell_fn) in CANDIDATES.items():
        obj = maker()
        row = {}
        for label, msg in CASES:
            t = min(timeit.repeat(lambda: tell_fn(obj, msg), number=number, repeat=repeat))
            row[label] = t / number * 1e9
        t = min(timeit.repeat(maker, number=number // 4, repeat=repeat))
        row["create (child + parent)"] = t / (number // 4) * 1e9
        results[name] = row
    # the floor: a plain Python method call
    obj = ChildD()
    t = min(timeit.repeat(obj.get_m20, number=number, repeat=repeat))
    results["(baseline: obj.get_m20())"] = {"20th of 40 messages": t / number * 1e9}
    return results


def markdown(results):
    columns = [label for label, _ in CASES] + ["create (child + parent)"]
    out = ["| candidate | " + " | ".join(columns) + " |",
           "|---" * (len(columns) + 1) + "|"]
    for name, row in results.items():
        out.append("| %s | " % name + " | ".join(
            ("%.0f" % row[c]) if c in row else "" for c in columns) + " |")
    return "\n".join(out)


if __name__ == "__main__":
    print("ns per operation (min of 5 repeats), Python %s" % sys.version.split()[0])
    print(markdown(bench()))
