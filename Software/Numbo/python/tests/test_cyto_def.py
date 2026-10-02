"""Tests for numbo.cyto_def, the port of lisp/src/cyto-def.lisp.

Every case comes from python/fixtures/cyto_def.json (lisp/tests/oracle/cyto-def.lisp):
  - init: config's (init-cytoplasm ...) in real runs of the 11 chapter
    puzzles, the state before and after;
  - snapshots: the states those runs (and a few mid-run ones, and a made-up
    cytoplasm) end in.  Each reader method on the cytoplasm and on every node,
    in sequence, with its result and the free globals TYPE, STATUS, LV after
    it (set to :before before each call); then a script of mutating calls, each with its result and the full
    state after it.
States, results and globals are compared through json.dumps of the
encoding (cyto_state.py), so 3 and 3.0, "free" and FREE all differ.
"""

import inspect
import json
import re
from pathlib import Path

import pytest

from conftest import load_fixture
from cyto_state import FREE_GLOBALS, Encoder, load_state
from numbo import cyto_def
from numbo.franz import intern

CD = load_fixture("cyto_def.json")
BEFORE = intern("BEFORE", "KEYWORD")  # what the oracle sets TYPE, STATUS, LV to before a reader
REPO = Path(__file__).resolve().parents[2]

FUNCTIONS = {
    intern("UPDATE-CONTEXT"): cyto_def.update_context,
    intern("UPDATE-CURRENT-TARGET"): cyto_def.update_current_target,
    intern("REPLACE-FUNCTION"): lambda world, *args: cyto_def.replace_function(*args),
    intern("INIT-CYTOPLASM"): cyto_def.init_cytoplasm,
}


def dumps(x):
    return json.dumps(x)


def send(world, recv, msg, *args):
    """send, for the cyto flavors' methods."""
    table = (cyto_def.CYTOPLASM_METHODS if isinstance(recv, cyto_def.Cytoplasm)
             else cyto_def.CYTO_NODE_METHODS)
    return table[msg](world, recv, *args)


def call(fn):
    """(result, error): fn's value, or an error (any Lisp error is one)."""
    try:
        return fn(), None
    except (TypeError, AttributeError, NameError) as e:
        return None, e


def snapshot_ids():
    return [f"{s['problem']}-s{s['seed']}-c{s['cap']}" if s["problem"] else "made-up"
            for s in CD["snapshots"]]


# --- the fixture -----------------------------------------------------------------

def test_fixture_shape():
    assert len(CD["init"]) == 11
    assert len(CD["snapshots"]) == 18
    assert len(CD["holders"]) == 91


@pytest.mark.parametrize("snap", CD["snapshots"], ids=snapshot_ids())
def test_state_round_trip(snap):
    """load_state and Encoder are inverse on every state: the harness itself."""
    world, _ = load_state(snap["state"])
    assert dumps(Encoder(world).state()) == dumps(snap["state"])
    for op in snap["ops"]:
        world, _ = load_state(op["state"])
        assert dumps(Encoder(world).state()) == dumps(op["state"])


# --- init-cytoplasm --------------------------------------------------------------

@pytest.mark.parametrize("case", CD["init"], ids=[str(c["problem"]) for c in CD["init"]])
def test_init_cytoplasm(case):
    world, decode = load_state(case["before"])
    result = cyto_def.init_cytoplasm(world, *decode(case["args"]))
    enc = Encoder(world)
    assert dumps(enc.state()) == dumps(case["after"])
    assert dumps(enc.data(result)) == dumps(case["result"])


def test_init_cytoplasm_makes_new_objects():
    world, decode = load_state(CD["init"][1]["before"])
    old = world[intern("*CYTOPLASM*")]
    cyto_def.init_cytoplasm(world, 87, 8, 3, 9, 10, 7)
    new = world[intern("*CYTOPLASM*")]
    assert new is not old
    assert new.current_target is world[intern("*CURRENT-TARGET*")]
    assert new.context is world[intern("*CONTEXT*")]
    assert new.nodes is None


# --- readers ---------------------------------------------------------------------

@pytest.mark.parametrize("snap", CD["snapshots"], ids=snapshot_ids())
def test_readers(snap):
    world, decode = load_state(snap["state"])
    enc = Encoder(world)
    enc.state()
    for i, rec in enumerate(snap["readers"]):
        recv = decode(rec["recv"])
        msg = decode(rec["msg"])
        for _, symbol in FREE_GLOBALS:
            world[symbol] = BEFORE
        result, error = call(lambda: send(world, recv, msg))
        where = f"reader {i}: {rec['msg']} on {rec['recv']}"
        assert bool(error) == bool(rec["error"]), f"{where}: {error!r}"
        assert dumps(enc.data(result)) == dumps(rec["result"]), where
        assert dumps(enc.free_globals()) == dumps(rec["globals"]), where


# --- ops -------------------------------------------------------------------------

@pytest.mark.parametrize("snap", [s for s in CD["snapshots"] if s["ops"]],
                         ids=[i for i, s in zip(snapshot_ids(), CD["snapshots"]) if s["ops"]])
def test_ops(snap):
    world, decode = load_state(snap["state"])
    for rec in snap["readers"]:  # the oracle ran them first (they set TYPE, ...)
        for _, symbol in FREE_GLOBALS:
            world[symbol] = BEFORE
        call(lambda: send(world, decode(rec["recv"]), decode(rec["msg"])))
    enc = Encoder(world)
    enc.state()
    for i, op in enumerate(snap["ops"]):
        recv = decode(op["recv"])
        msg = decode(op["msg"])
        args = decode(op["args"]) or []
        if recv is None:
            fn = FUNCTIONS[msg]
            result, error = call(lambda: fn(world, *args))
        else:
            result, error = call(lambda: send(world, recv, msg, *args))
        where = f"op {i}: {op['msg']} {op['args']}"
        assert bool(error) == bool(op["error"]), f"{where}: {error!r}"
        assert dumps(enc.data(result)) == dumps(op["result"]), where
        assert dumps(Encoder(world).state()) == dumps(op["state"]), where


# --- coverage --------------------------------------------------------------------

def lisp_definitions():
    """(methods, functions) defined in lisp/src/cyto-def.lisp, in source order."""
    text = (REPO / "lisp" / "src" / "cyto-def.lisp").read_text()
    methods = re.findall(r"^\(defmethod \((cytoplasm|cyto-node) (:[a-z-]+)\)", text, re.M)
    functions = re.findall(r"^\(defun ([a-z-]+)", text, re.M)
    return methods, functions


def test_census():
    methods, functions = lisp_definitions()
    assert len(methods) == 15
    assert functions == ["init-cytoplasm", "replace-function", "update-context",
                         "update-current-target"]
    for flavor, msg in methods:
        table = (cyto_def.CYTOPLASM_METHODS if flavor == "cytoplasm"
                 else cyto_def.CYTO_NODE_METHODS)
        fn = table[intern(msg[1:].upper(), "KEYWORD")]
        assert f"cyto-def.lisp: ({flavor} {msg})" in inspect.getdoc(fn)
    assert len(cyto_def.CYTOPLASM_METHODS) + len(cyto_def.CYTO_NODE_METHODS) == 15
    for name in functions:
        fn = getattr(cyto_def, name.replace("-", "_"))
        assert f"cyto-def.lisp: {name}" in inspect.getdoc(fn)


def test_every_method_and_function_has_oracle_cases():
    """Each of the 15 methods and 4 functions is called in the fixture, with a
    non-nil result at least once (and the readers with an error too where one
    can happen)."""
    methods, functions = lisp_definitions()
    seen, non_nil, errors = set(), set(), set()
    for snap in CD["snapshots"]:
        for rec in snap["readers"] + snap["ops"]:
            seen.add(rec["msg"])
            if rec["result"] is not None:
                non_nil.add(rec["msg"])
            if rec["error"]:
                errors.add(rec["msg"])
    wanted = {msg.upper() for _, msg in methods} | {f.upper() for f in functions}
    assert wanted <= seen
    assert wanted <= non_nil
    assert {":BLOCK-NEIGHBOR", ":UPPER-NEIGHBOR", ":LOWER-NEIGHBOR"} <= errors


def test_registry():
    """(set name node) / (eval name): the registry is the World's values."""
    world, _ = load_state(CD["snapshots"][0]["state"])
    node = cyto_def.CytoNode(name=intern("CYTO-TEST-NODE"), type="2b")
    cyto_def.register_node(world, node)
    assert cyto_def.node_named(world, intern("CYTO-TEST-NODE")) is node
    with pytest.raises(NameError):
        cyto_def.node_named(world, intern("CYTO-NO-SUCH-NODE"))
