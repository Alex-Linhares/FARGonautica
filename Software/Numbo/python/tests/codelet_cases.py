"""Replaying the oracle's codelet cases (codelets_a.json, codelets_b.json).

A case (lisp/tests/oracle/codelets-a.lisp, codelets-b.lisp) is a call of one
codelet in a real oracle run, or made up on a real state: the World state
before it, its arguments, its result (or error), output and RNG draws, the
World state after it and the globals it changed.  `run_case` rebuilds the
state (world_state.py), calls the Python function on the same arguments, and
checks all of these, through json.dumps of the encoding, so 3 and 3.0,
"free" and FREE all differ.
"""

import io
import json

import pytest

from numbo.cyto_def import _Flavor
from numbo.franz import intern
from numbo.pnet_def import Pnode
from world_state import Encoder, load_world


def dumps(x):
    return json.dumps(x)


def case_ids(cases):
    """A readable, unique pytest id per case."""
    ids = []
    for case in cases:
        source = case["source"]
        if source[0] == ":RUN":
            where = f"{'-'.join(map(str, source[1]))}-s{source[2]}"
        else:
            where = f"made-up-{source[1]['str']}"
        base = f"{case['codelet'].lower()}-{where}"
        n = sum(1 for i in ids if i == base or i.startswith(base + "#"))
        ids.append(base if n == 0 else f"{base}#{n}")
    return ids


def same(x, y):
    """Lisp eq for objects, equality of the encoding for data: whether a
    global's value is unchanged (independently of the node numbering)."""
    if isinstance(x, (_Flavor, Pnode)) or isinstance(y, (_Flavor, Pnode)):
        return x is y
    if isinstance(x, list) and isinstance(y, list):
        return len(x) == len(y) and all(same(a, b) for a, b in zip(x, y))
    if isinstance(x, list) or isinstance(y, list):
        return False
    return type(x) is type(y) and x == y


def run_case(case, parameters, fn):
    """Replay CASE on the Python function FN and check it against the oracle."""
    world, decode = load_world(case["before"], parameters, case["arg-nodes"])
    world.out = io.StringIO()
    enc = Encoder(world)
    assert dumps(enc.state()) == dumps(case["before"]), "the state did not round-trip"
    count = len(enc.queue)
    args = decode(case["args"]) or []
    assert dumps(enc.data(args)) == dumps(case["args"])
    assert dumps(enc.nodes_from(count)) == dumps(case["arg-nodes"])
    before_values = dict(world.values)
    draws = world.rng.draws

    if case["error"]:
        with pytest.raises(Exception):
            fn(world, *args)
        result = None
    else:
        result = fn(world, *args)

    assert dumps(enc.data(result)) == dumps(case["result"]), "result"
    assert world.out.getvalue() == case["output"], "output"
    assert world.rng.draws - draws == case["draws"], "RNG draws"
    after = enc.state()
    for key in case["after"]:
        assert dumps(after[key]) == dumps(case["after"][key]), f"state after: {key}"
    # Every global the oracle changed has the oracle's value, and Python
    # changed no other.
    assert dumps({name: enc.value(intern(name)) for name in case["changed"]}) \
        == dumps(case["changed"])
    changed = {sym.name for sym, value in world.values.items()
               if sym not in before_values or not same(before_values[sym], value)}
    changed |= {sym.name for sym in before_values if sym not in world.values}
    assert changed <= set(case["changed"]), changed - set(case["changed"])
