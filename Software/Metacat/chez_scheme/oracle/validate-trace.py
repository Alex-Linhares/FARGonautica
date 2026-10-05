#!/usr/bin/env python3
"""Check that files are traces in the format of docs/trace-format.md.

    python3 chez_scheme/oracle/validate-trace.py FILE... [--require ev,ev,...]

Exits 1 and says why at the first problem. --require lists event types that
must occur in every file. Part of the Racket port of Metacat (GPL v2 or later).
"""
import json
import sys

NUM = (int, float, str)  # str: exact non-integer rationals ("7/3"), non-finite
NAME = (str, type(None))

# ev -> {field: allowed types}, in the order the fields are written
FIELDS = {
    "start": {"format": int, "problem": list, "seed": int, "max_codelets": (int, type(None)),
              "keep_going": bool, "slipnodes": list},
    "codelet": {"type": str, "urgency": NUM, "posted": int, "rng": int},
    "build": None,  # depends on kind
    "break": None,
    "temperature": {"value": NUM, "clamped": bool},
    "slipnet": {"activations": list, "rng": int},
    "themes": {"active": list, "themes": list},
    "event": {"type": str, "number": int, "name": str, "time": int, "temperature": NUM},
    "answer": {"answer": str, "quality": NUM, "temperature": NUM},
    "comment": {"text": str},
    "halt": {"message": str, "object": str},
    "end": {"reason": str, "temperature": NUM, "answers": list, "rng": int},
}

KINDS = {
    "bond": {"string": str, "from": NAME, "to": NAME, "category": NAME, "direction": NAME,
             "facet": NAME},
    "group": {"string": str, "name": NAME, "category": NAME, "direction": NAME, "facet": NAME,
              "objects": list},
    "bridge": {"type": str, "object1": NAME, "object2": NAME, "mappings": list},
    "description": {"string": str, "object": NAME, "type": NAME, "descriptor": NAME},
    "rule": {"type": str, "english": list},
}

EVENT_TYPES = {"answer", "snag", "clamp", "concept-activation", "concept-mapping", "rule", "group"}
REASONS = {"suspend", "cap", "halt"}


class Invalid(Exception):
    pass


def check_fields(obj, spec, where):
    keys = list(obj)[2:]
    if keys != list(spec):
        raise Invalid(f"{where}: fields {keys}, expected {list(spec)}")
    for k, types in spec.items():
        v = obj[k]
        if isinstance(v, bool) and types is not bool and not (isinstance(types, tuple) and bool in types):
            raise Invalid(f"{where}: {k} is a boolean")
        if not isinstance(v, types):
            raise Invalid(f"{where}: {k} = {v!r} has the wrong type")


def validate(path, required):
    with open(path, encoding="utf-8") as f:
        lines = f.read().split("\n")
    if lines[-1] != "":
        raise Invalid(f"{path}: does not end with a newline")
    lines = lines[:-1]
    if not lines:
        raise Invalid(f"{path}: empty")
    seen = set()
    last_t = 0
    codelets = 0
    start = None
    for n, line in enumerate(lines, 1):
        where = f"{path}:{n}"
        try:
            obj = json.loads(line)
        except json.JSONDecodeError as e:
            raise Invalid(f"{where}: not JSON: {e}")
        if not isinstance(obj, dict) or list(obj)[:2] != ["t", "ev"]:
            raise Invalid(f"{where}: must be an object starting with t, ev")
        t, ev = obj["t"], obj["ev"]
        if not isinstance(t, int) or isinstance(t, bool) or t < last_t:
            raise Invalid(f"{where}: t = {t!r} (previous {last_t})")
        last_t = t
        if ev not in FIELDS:
            raise Invalid(f"{where}: unknown event type {ev!r}")
        if (n == 1) != (ev == "start") or (n == len(lines)) != (ev == "end"):
            raise Invalid(f"{where}: start must be first and end last, once each")
        if ev in ("build", "break"):
            kind = obj.get("kind")
            if kind not in KINDS:
                raise Invalid(f"{where}: unknown structure kind {kind!r}")
            spec = {"kind": str, **KINDS[kind]}
            if ev == "build" and kind == "group":
                spec["flipped"] = bool
            check_fields(obj, spec, where)
        else:
            check_fields(obj, FIELDS[ev], where)
        if ev == "start":
            start = obj
            if obj["format"] != 1 or len(obj["problem"]) != 4:
                raise Invalid(f"{where}: bad format or problem")
        elif ev == "codelet":
            if t != codelets:
                raise Invalid(f"{where}: codelet event {codelets} has t = {t}")
            codelets += 1
        elif ev == "slipnet":
            if len(obj["activations"]) != len(start["slipnodes"]):
                raise Invalid(f"{where}: {len(obj['activations'])} activations")
        elif ev == "event":
            if obj["type"] not in EVENT_TYPES:
                raise Invalid(f"{where}: unknown trace event type {obj['type']!r}")
        elif ev == "end":
            if obj["reason"] not in REASONS:
                raise Invalid(f"{where}: unknown reason {obj['reason']!r}")
            # A run stopped by the cap stops between codelets; one that
            # suspends or halts stops inside codelet number t.
            expected = t if obj["reason"] == "cap" else t + 1
            if codelets != expected:
                raise Invalid(f"{where}: {codelets} codelet events, expected {expected}")
        seen.add(ev)
    missing = [ev for ev in required if ev not in seen]
    if missing:
        raise Invalid(f"{path}: no {', '.join(missing)} events")


def main(argv):
    required = []
    files = []
    args = iter(argv)
    for a in args:
        if a == "--require":
            required = [x for x in next(args, "").split(",") if x]
        else:
            files.append(a)
    if not files:
        print(__doc__, file=sys.stderr)
        return 2
    try:
        for path in files:
            validate(path, required)
    except (Invalid, OSError) as e:
        print(f"validate-trace: {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
