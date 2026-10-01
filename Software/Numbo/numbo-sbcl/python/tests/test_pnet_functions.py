"""The pnode methods and Pnet functions of python/numbo/pnet_functions.py,
replayed on the oracle's snapshots in python/fixtures/pnet_functions.json
(tests/oracle/pnet-functions.lisp), loop0002 item 5.

Every state is compared through the trace's Lisp-data encoding with
json.dumps, so doubles must be equal (==, same repr) and 0 and 0.0 differ.
"""

import io
import json

import pytest

from conftest import load_fixture, lisp_data, lisp_data_decode

from numbo import pnet_def, pnet_functions
from numbo.franz import find_package, intern
from numbo.world import World, lisp_eval

FIX = load_fixture("pnet_functions.json")
PNET = load_fixture("pnet.json")
HOLDERS = [intern(e["holder"]) for e in PNET["holders"]]
KEYWORD = find_package("keyword")
PNODE_MARK = intern("PNODE", KEYWORD)
UNBOUND = intern("UNBOUND", KEYWORD)
NODE, RES, ITERATION = intern("NODE"), intern("RES"), intern("*ITERATION*")


class CytoStub:
    """Stands for a cyto-node in a pnode's instances (item 7 makes real
    ones): the Pnet only takes its identity."""

    def __init__(self, name):
        self.name = name


# --- the World ---------------------------------------------------------------

def new_world(init_chiffre=True):
    """A World as the oracle's is after loading (init-pnet, *iteration* 0)
    and, if INIT_CHIFFRE, after (init-chiffre)'s parameters and
    initialize-pnet-2.  The posts populate-coderack makes go to world.posts."""
    world = World()
    for name, value in PNET["parameters"]["init-chiffre" if init_chiffre else "defvar"].items():
        world[intern(name)] = value
    world[intern("%VERBOSE%")] = None
    world[intern("*CODERACK*")] = intern("CODERACK")
    world[ITERATION] = 0
    pnet_def.init_pnet(world)
    world[intern("*PNET*")] = pnet_def.pnet_list(world)
    world.posts = []
    world.cr_hang = lambda name, form, urgency: world.posts.append([form, urgency])
    if init_chiffre:
        pnet_functions.initialize_pnet_2(world)
    return world


# --- encoding and decoding states ----------------------------------------------

def encode(world, x):
    """X in the fixture's encoding: a pnode is (:pnode holder)."""
    if isinstance(x, pnet_def.Pnode):
        holder = next(h for h in HOLDERS if world[h] is x)
        return [":PNODE", holder.name]
    if isinstance(x, CytoStub):
        return {"obj": "cyto-node", "name": x.name}
    if isinstance(x, list):
        return [encode(world, v) for v in x] if x else None
    return lisp_data(x)


def decode(world, x, cytos):
    """The fixture's encoding -> Python data, pnodes and cyto-node stubs
    (one per name, in CYTOS) included."""
    if isinstance(x, dict) and "obj" in x:
        assert x["obj"] == "cyto-node", x
        return cytos.setdefault(x["name"], CytoStub(x["name"]))
    if isinstance(x, list) and len(x) == 2 and x[0] == ":PNODE":
        return world[intern(x[1])]
    if isinstance(x, list):
        return [decode(world, v, cytos) for v in x]
    return lisp_data_decode(x)


def global_state(world, sym):
    return encode(world, world[sym]) if sym in world else ":UNBOUND"


def pnode_state(world, holder, neighbors=False):
    p = world[holder]
    state = {"holder": holder.name,
             "activation": encode(world, p.activation),
             "spreadable-activation": encode(world, p.spreadable_activation),
             "temp-activation-holder": encode(world, p.temp_activation_holder),
             "instances": encode(world, p.instances),
             "codelets": encode(world, p.codelets)}
    if neighbors:
        state["neighbors"] = encode(world, p.neighbors)
    return state


def assert_state(world, expected, where):
    neighbors = "neighbors" in expected["pnodes"][0]
    for key, sym in (("iteration", ITERATION), ("node", NODE), ("res", RES)):
        assert json.dumps(global_state(world, sym)) == json.dumps(expected[key]), (where, key)
    assert [e["holder"] for e in expected["pnodes"]] == [h.name for h in HOLDERS]
    for holder, e in zip(HOLDERS, expected["pnodes"]):
        assert json.dumps(pnode_state(world, holder, neighbors)) == json.dumps(e), \
            (where, holder.name)


def load_state(world, state, cytos):
    """Set WORLD's dynamic Pnet state (and globals) to the fixture STATE."""
    for key, sym in (("iteration", ITERATION), ("node", NODE), ("res", RES)):
        if state[key] == ":UNBOUND":
            if sym in world:
                del world[sym]
        else:
            world[sym] = decode(world, state[key], cytos)
    for e in state["pnodes"]:
        p = world[intern(e["holder"])]
        p.activation = decode(world, e["activation"], cytos)
        p.spreadable_activation = decode(world, e["spreadable-activation"], cytos)
        p.temp_activation_holder = decode(world, e["temp-activation-holder"], cytos)
        p.instances = decode(world, e["instances"], cytos)
        p.codelets = decode(world, e["codelets"], cytos)
    assert_state(world, state, "load_state")


# --- the encoding's own checks -------------------------------------------------

def test_fixture_shape():
    assert [len(FIX[k]["pnodes"]) for k in ("loaded", "init-chiffre", "initialize-pnet")] \
        == [91, 91, 91]
    assert len(FIX["scenarios"]) == 6
    assert sum(len(p["posts"] or []) for s in FIX["scenarios"] for p in s["populate"]) > 10
    # the real starts carry cyto-node instances
    assert any("obj" in json.dumps(s["start"]) for s in FIX["scenarios"][3:])


def test_world_symbol_values():
    world = World()
    x = intern("%K%")
    assert x not in world
    with pytest.raises(NameError):
        world[x]
    world[x] = 0.001
    assert world[x] == 0.001 and x in world
    del world[x]
    assert x not in world


def test_lisp_eval_threshold_forms():
    world = World()
    world[intern("%FIRST-THRESHOLD%")] = 30
    world[ITERATION] = 20
    assert lisp_eval(world, intern("%FIRST-THRESHOLD%")) == 30
    assert lisp_eval(world, 7) == 7 and lisp_eval(world, 2.5) == 2.5
    assert lisp_eval(world, None) is None
    assert lisp_eval(world, intern("T")) is True
    assert lisp_eval(world, intern("NIL")) is None
    form = [intern("MAX"), 30, [intern("ADD"), 17, [intern("MINUS"), ITERATION], 60]]
    assert lisp_eval(world, form) == 57
    world[ITERATION] = 50
    v = lisp_eval(world, form)
    assert v == 30 and type(v) is int
    with pytest.raises(NameError):
        lisp_eval(world, intern("NO-SUCH-GLOBAL"))
    with pytest.raises(NotImplementedError):
        lisp_eval(world, [intern("NO-SUCH-FUNCTION"), 1])


# --- fresh pnodes, initialize-pnet-2, initialize-pnet ----------------------------

def test_loaded_state():
    world = new_world(init_chiffre=False)
    assert_state(world, FIX["loaded"], "loaded")


def test_initialize_pnet_2():
    world = new_world(init_chiffre=False)
    for name, value in PNET["parameters"]["init-chiffre"].items():
        world[intern(name)] = value
    pnet_functions.initialize_pnet_2(world)
    assert_state(world, FIX["init-chiffre"], "init-chiffre")
    # the link types are pnodes now
    assert world[intern("NODE-5")].neighbors[0][1] is world[intern("RESULT+")]


def test_initialize_pnet():
    world = new_world()
    pnet_functions.initialize_pnet(world)
    assert_state(world, FIX["initialize-pnet"], "initialize-pnet")


@pytest.mark.parametrize("case", FIX["print"], ids=lambda c: c["holder"])
def test_print(case):
    world = new_world(init_chiffre=False)
    world[intern("RESULT+")].instances = [["5g", "result+"]]
    world[intern("NODE-5")].instances = [["2b", intern("CYTO-BRICK2")],
                                         ["1t", intern("CYTO-TARGET")]]
    world[intern("NODE-5")].activation = 50.5
    blocks = [["4bl", intern(n)] for n in ("CYTO-BLOCK1-V1", "CYTO-BLOCK2-V1", "CYTO-BLOCK3-V1")]
    world[intern("NODE-7")].instances = [["4bl", intern("CYTO-BLOCK100-V1234")]] + blocks[:2]
    world[intern("NODE-8")].instances = [["4bl", intern("CYTO-BLOCK1000-V1234")]] + blocks[:2]
    world[intern("NODE-9")].instances = [["4bl", intern("CYTO-BLOCK1000-V1234")]] + blocks
    world.out = io.StringIO()
    pnet_functions.pnode_print(world, world[intern(case["holder"])])
    assert world.out.getvalue() == case["output"]


# --- scenarios: spreading, populate-coderack, initialize-pnet -------------------

@pytest.mark.parametrize("scenario", FIX["scenarios"], ids=lambda s: s["name"])
def test_scenario(scenario):
    world = new_world()
    cytos = {}
    load_state(world, scenario["start"], cytos)
    base = world[ITERATION]
    cycles = {c["cycles"]: c["state"] for c in scenario["cycles"]}
    for n in range(1, 11):
        pnet_functions.spread_activation_in_pnet(world)
        if n in cycles:
            assert_state(world, cycles[n], f"cycle {n}")
        if n == 1:
            for populate in scenario["populate"]:
                world[ITERATION] = populate["iteration"]
                world[intern("%VERBOSE%")] = True if populate["verbose"] else None
                world.posts = []
                world.out = io.StringIO()
                pnet_functions.populate_coderack(world)
                where = f"populate at {populate['iteration']}"
                assert json.dumps(encode(world, world.posts)) == json.dumps(populate["posts"]), where
                assert world.out.getvalue() == populate["output"], where
                assert_state(world, populate["state"], where)
    world[ITERATION] = base + 33
    assert scenario["initialize-pnet"]["iteration"] == base + 33
    pnet_functions.initialize_pnet(world)
    assert_state(world, scenario["initialize-pnet"]["state"], "initialize-pnet")


# --- method cases ----------------------------------------------------------------

@pytest.fixture
def methods_world():
    world = new_world()
    load_state(world, FIX["methods"]["start"], {})
    return world


def test_reader_methods(methods_world):
    world = methods_world
    pnet = world[intern("*PNET*")]
    assert len(FIX["methods"]["readers"]) == len(pnet) == 88
    for p, expected in zip(pnet, FIX["methods"]["readers"]):
        hot = pnet_functions.pnode_hotter_neighbor_activation(world, p)
        got = [p, pnet_functions.pnode_activation_decay(world, p),
               pnet_functions.pnode_activation_decay_factor(world, p),
               pnet_functions.pnode_link_length(world, p),
               pnet_functions.pnode_link_length(world, p, 5),
               hot, world[NODE],
               pnet_functions.pnode_codelet_urgency(world, p, 150)]
        got[0] = next(h for h in HOLDERS if world[h] is p)
        assert json.dumps(encode(world, got)) == json.dumps(expected), expected[0]


def test_mutating_methods_in_sequence():
    world = new_world()
    load_state(world, FIX["methods"]["ops-start"], {})
    for op in FIX["methods"]["ops"]:
        message, holder, *args = decode(world, op["send"], {})
        if message.name == "SET-UP-ACTIVATIONS":
            result = pnet_functions.set_up_activations(world, *args)
        elif message.name == "POPULATE-CODERACK":
            world.posts = []
            pnet_functions.populate_coderack(world)
            result = world.posts
        elif message.name.startswith("SET-"):
            # a settable instance variable's :set- message
            setattr(world[holder], message.name[4:].lower().replace("-", "_"), args[0])
            result = args[0]
        else:
            method = pnet_functions.PNODE_METHODS[message]
            result = method(world, world[holder], *args)
        assert json.dumps(encode(world, result)) == json.dumps(op["result"]), op["send"]
        assert_state(world, op["state"], op["send"])


def test_every_lisp_function_is_ported():
    """The 14 pnode methods and 6 functions of pnet-functions.lisp."""
    methods = ["activation-decay", "activation-decay-factor", "add-activation",
               "add-temp-activation-holder", "codelet-urgency",
               "hotter-neighbor-activation", "link-length", "modify-threshold",
               "print", "spread-activation", "subtract-activation",
               "suppress-instances", "update-activation", "update-instances"]
    functions = ["initialize-codelet", "initialize-pnet", "initialize-pnet-2",
                 "populate-coderack", "set-up-activations", "spread-activation-in-pnet"]
    assert sorted(m.name.lower() for m in pnet_functions.PNODE_METHODS) == methods
    for m in methods:
        fn = getattr(pnet_functions, "pnode_" + m.replace("-", "_"))
        assert fn is pnet_functions.PNODE_METHODS[intern(m.upper(), KEYWORD)]
        assert f"(pnode :{m})" in fn.__doc__
    for f in functions:
        fn = getattr(pnet_functions, f.replace("-", "_"))
        assert f"pnet-functions.lisp: {f}" in fn.__doc__
