"""The codelets, and the functions they use.

Port of src/codelets.lisp, in its order.  Each defun is a function of the
same name (round is round_, as it would shadow Python's), taking the World
first.  Loop0002 item 8 ports the codelets that create, link, activate and
kill nodes: activate, create-cyto-node, free-from-pnet, kill-node,
link-to-pnet, read-brick and read-target, with the helpers they call
(disconnect, is-linked-to-target, kill-block, kill-dtarget, ratio, repump,
round, sup).  Item 9 ports the block-search codelets: compare-b-to-t,
look-for-new-block, look-for-bl+, look-for-blx, look-for-approx-bl+,
look-for-approx-blx, look-for-diff and test-if-possible-and-desirable, with
their helpers (compare, digits-in-common, eliminate, find-activations,
find-in, find-interest-in-pnet, find-node, is-linked-to, multiple, randlist,
remove-dd, sim).  Item 10 ports the rest: decomp+, decompi, decompx,
const-bl+, const-blx, replace-target, propagate-success, create-op-node,
update-success, temperature, collect-misfortune, misfortune, mean,
check-temperature, diff300, decrease-interest, decompose and
create-coderack, so every codelets.lisp defun has its function here.
Names with + spell it _plus (look_for_bl_plus, decomp_plus).

The codelets keep their state in globals: the 1987 free SETQs (activation in
link-to-pnet and read-brick, div in round, ...) set World symbol values,
never Python locals (PORTING_NOTES.md, "Compile census").  A codelet run from
the coderack gets its form's arguments evaluated, so it receives cyto-nodes
and pnodes, not their names; (eval x) of an object is the object (lisp_eval).
Messages go through flavors.send where the receiver may be nil, which is an
error in Lisp as here.

python/tests/test_codelets_a.py, test_codelets_b.py and test_codelets_c.py replay the
oracle's cases, python/fixtures/codelets_a.json, codelets_b.json and
codelets_c.json.
"""

import functools
import math
import operator

from numbo import coderack, cyto_def, franz, pnet_functions
from numbo import world as world_module
from numbo.cyto_def import CytoNode, register_node, update_context, update_current_target
from numbo.flavors import send
from numbo.franz import intern as _S
from numbo.world import lisp_eval

_CYTOPLASM = _S("*CYTOPLASM*")
_CURRENT_TARGET = _S("*CURRENT-TARGET*")
_CODERACK = _S("*CODERACK*")
_ACTIVATION = _S("ACTIVATION")
_DIV = _S("DIV")
_A = _S("A")
_BRICK = _S("BRICK")
_BRICKI = _S("BRICKI")
_CYTO_BRICKI = _S("CYTO-BRICKI")
_TARGET = _S("TARGET")
_NODE_ADD = _S("NODE-ADD")
_NODE_SUBTRACT = _S("NODE-SUBTRACT")
_NODE_MULTIPLY = _S("NODE-MULTIPLY")
_NODE_1 = _S("NODE-1")
_NODE_150 = _S("NODE-150")
_NODE_PREFIX = _S("NODE-")
_BRICK_PREFIX = _S("BRICK")
_CYTO_BRICK_PREFIX = _S("CYTO-BRICK")
_CYTO_TARGET = _S("CYTO-TARGET")
_QUOTE = _S("QUOTE")
_ACTIVATE = _S("ACTIVATE")
_LINK_TO_PNET = _S("LINK-TO-PNET")
_COMPARE_B_TO_T = _S("COMPARE-B-TO-T")
_CREATE_CYTO_NODE = _S("CREATE-CYTO-NODE")
_FREE_FROM_PNET = _S("FREE-FROM-PNET")
_RESULT_PLUS = _S("RESULT+")
_RESULTX = _S("RESULTX")
_OPERAND = _S("OPERAND")
_SIMILAR = _S("SIMILAR")
_OPERATION = _S("OPERATION")
_INSTANCE = _S("INSTANCE")
_P_RESULTX = _S("%RESULTX%")
_P_RESULT_PLUS = _S("%RESULT+%")
_P_OPERAND = _S("%OPERAND%")
_P_SIMILAR = _S("%SIMILAR%")
_P_OPERATION = _S("%OPERATION%")
_P_INSTANCE = _S("%INSTANCE%")
_P_UPPER_URGENCY = _S("%UPPER-URGENCY%")
_P_FIRST_URGENCY = _S("%FIRST-URGENCY%")
_P_SECOND_URGENCY = _S("%SECOND-URGENCY%")
_P_TARGET_ACTIVATION = _S("%TARGET-ACTIVATION%")
_P_BRICK_ACTIVATION = _S("%BRICK-ACTIVATION%")
_P_DTARGET_ACTIVATION = _S("%DTARGET-ACTIVATION%")
_P_BLOCK_ACTIVATION = _S("%BLOCK-ACTIVATION%")
_P_DTARGET_PLUS = _S("%DTARGET-PLUS%")
_P_NODE_MINUS = _S("%NODE-MINUS%")
_P_THIRD_URGENCY = _S("%THIRD-URGENCY%")
_P_FOURTH_URGENCY = _S("%FOURTH-URGENCY%")
_P_FIFTH_URGENCY = _S("%FIFTH-URGENCY%")
_G_CURRENT_TARGET = _S("CURRENT-TARGET")
_VALUES_TO_FIND = _S("VALUES-TO-FIND")
_NODE1 = _S("NODE1")
_NODE2 = _S("NODE2")
_CYTO_BLOCK1 = _S("CYTO-BLOCK1")
_CYTO_BLOCK2 = _S("CYTO-BLOCK2")
_DIFF = _S("DIFF")
_DIFFREL = _S("DIFFREL")
_MIN = _S("MIN")
_LISTE = _S("LISTE")
_SIMILARITY = _S("SIMILARITY")
_ADDRESS = _S("ADDRESS")
_REPLACE_TARGET = _S("REPLACE-TARGET")
_DECOMP_PLUS = _S("DECOMP+")
_DECOMPI = _S("DECOMPI")
_LOOK_FOR_APPROX_BL_PLUS = _S("LOOK-FOR-APPROX-BL+")
_LOOK_FOR_APPROX_BLX = _S("LOOK-FOR-APPROX-BLX")
_LOOK_FOR_DIFF = _S("LOOK-FOR-DIFF")
_TEST_IF = _S("TEST-IF-POSSIBLE-AND-DESIRABLE")
_CONST_BL_PLUS = _S("CONST-BL+")
_CONST_BLX = _S("CONST-BLX")
_PROBLEM_SOLVED = _S("*PROBLEM-SOLVED*")
_NAME_COUNTER = _S("*NAME-COUNTER*")
_P_TEMPERATURE_THRESHOLD = _S("%TEMPERATURE-THRESHOLD%")
_RESERVE = _S("RESERVE")
_WEIGHTS = _S("WEIGHTS")
_KILL_NODE = _S("KILL-NODE")
_LOOK_FOR_BLX = _S("LOOK-FOR-BLX")
_N1 = _S("N1")
_N2 = _S("N2")
_N3 = _S("N3")
_OPER = _S("OPER")
_NN = _S("NN")
_PP = _S("PP")
_CONT = _S("CONT")
_N = _S("N")
_NEW = _S("NEW")
_RESULT = _S("RESULT")
_MY_CODERACK = _S("MY-CODERACK")
_CYTO_BLOCK_PREFIX = _S("CYTO-BLOCK")
_CYTO_TARGET_PREFIX = _S("CYTO-TARGET-")
_V_PREFIX = _S("-V")
_PLUS = _S("PLUS")
_TIMES = _S("TIMES")
_DASH = _S("-")
# digits-in-common's quoted constants '(0 0 0) and '(0 0 1): shared, as in
# Lisp (nothing changes them in place).
_DIGITS_000 = [0, 0, 0]
_DIGITS_001 = [0, 0, 1]
# read-target's ''cyto-target: one quoted constant, shared by every form it
# posts.
_QUOTED_CYTO_TARGET = [_QUOTE, _CYTO_TARGET]


def _car(x):
    """car: nil for nil."""
    return x[0] if x else None


def _cadr(x):
    """cadr: nil past the end."""
    return x[1] if x and len(x) > 1 else None


def _eq(x, y):
    if isinstance(x, str) and isinstance(y, str):
        raise TypeError("eq on two Lisp strings can't be modelled in Python")
    return x is y


def _caddr(x):
    """caddr: nil past the end."""
    return x[2] if x and len(x) > 2 else None


def _plus(*numbers):
    """plus: numbers only (nil is a Lisp error)."""
    return functools.reduce(operator.add, (cyto_def._number(x) for x in numbers))


def _cadar(x):
    """cadar: the second element of the first element."""
    return _cadr(_car(x))


def _nth(n, lst):
    """nth: N must be a non-negative integer (nil is a Lisp error); nil past
    the end."""
    if not franz._is_integer(n) or n < 0:
        raise TypeError(f"NTH: the value {n!r} is not of type UNSIGNED-BYTE")
    return lst[n] if lst and n < len(lst) else None


def _eqn(x, y):
    """=: numbers only (nil or a symbol is a Lisp error)."""
    return cyto_def._number(x) == cyto_def._number(y)


def _eq_dd(x, y):
    """remove-dd's eq.  Fixnums are eq when equal in SBCL.  A double is a
    boxed object, eq only to itself, as a Python float object is `is` only
    to itself: find-in removes (nth x res2) from its weights, and when two
    nodes have equal activations the one at x must go, not the first equal
    one, or the weights fall out of step with the nodes (loop0002 item 11:
    puzzle 6 seed 1, two blocks at 43.199999999999996)."""
    if isinstance(x, int) and not isinstance(x, bool) and \
            isinstance(y, int) and not isinstance(y, bool):
        return x == y
    return _eq(x, y)


def _add_all(numbers):
    """(apply 'add list): CL +, folded left."""
    return functools.reduce(operator.add, numbers)


def _current_target_value(world):
    """(send (eval (send *current-target* :name)) :value)"""
    return send(world, lisp_eval(world, send(world, world[_CURRENT_TARGET], "name")), "value")


def _expt(base, power):
    """(expt base power) for a double BASE and an integer POWER: SBCL computes
    it with pow(), not by repeated squaring, and so does Python's ** (checked
    on 0.9 ** 0 ... 400 against the oracle)."""
    return base ** power


def activate(world, activation, pnode, repump_yes_or_no):
    """codelets.lisp: activate.  Add ACTIVATION to PNODE; with
    REPUMP-YES-OR-NO (a target or derived target), set node-add,
    node-subtract and node-multiply from the pnode's value and repump."""
    send(world, lisp_eval(world, pnode), "add-activation", activation)
    if repump_yes_or_no:
        val = send(world, pnode, "value")
        val_minus = 5 + 50 * _expt(0.9, val)
        valx = 4 * math.sqrt(val)
        send(world, world[_NODE_ADD], "set-activation", 60 - val_minus - valx)
        send(world, world[_NODE_SUBTRACT], "set-activation", val_minus)
        send(world, world[_NODE_MULTIPLY], "set-activation", valx)
        return repump(world)
    return None


def check_temperature(world, *function):
    """codelets.lisp: check-temperature.  If the cytoplasm is hotter than
    %temperature-threshold%, draw a secondary node, weighted by 300 minus its
    activation, and post kill-node for it."""
    # 1987: (defun check-temperature function () ...) is a Franz lexpr: it
    # takes any number of arguments and binds FUNCTION to their count; () is
    # its first body form (PORTING_NOTES.md, census: lexpr).
    rvictim = 0
    victim = None
    if world[_P_TEMPERATURE_THRESHOLD] < temperature(world):
        # 1987: reserve and weights are free SETQs: the globals.
        world[_RESERVE] = send(world, world[_CYTOPLASM], "secondary-cyto-nodes")
        world[_WEIGHTS] = [diff300(a)
                           for a in find_activations(world, world[_RESERVE]) or ()] or None
        rvictim = randlist(world, world[_WEIGHTS])
        if rvictim is not None:
            victim = _nth(rvictim, world[_RESERVE])
            return world.cr_hang(world[_CODERACK], [_KILL_NODE, victim],
                                 world[_P_FIRST_URGENCY])
    return None


def collect_misfortune(world):
    """codelets.lisp: collect-misfortune.  The misfortunes of the secondary
    cyto-nodes, in their order."""
    res = []
    # 1987: current-target is a free SETQ: the global, which look-for-blx
    # reads (PORTING_NOTES.md, "Oracle hooks": temperature without side
    # effects).
    world[_G_CURRENT_TARGET] = send(world, world[_CURRENT_TARGET], "name")
    cyto_nodes = send(world, world[_CYTOPLASM], "secondary-cyto-nodes")
    for node in cyto_nodes or ():
        hap = misfortune(send(world, node, "activation"), send(world, node, "level"),
                         send(world, node, "status"))
        res.insert(0, hap)
    return res[::-1] or None


def compare(world, element, list):
    """codelets.lisp: compare.  t if a number of LIST is similar to ELEMENT
    (sim < 4)."""
    for x in list or ():
        if sim(world, element, x) < 4:
            return True
    return None


def compare_b_to_t(world, cyto_block):
    """codelets.lisp: compare-b-to-t.  Compare CYTO-BLOCK with the current
    target and the target: equal, post replace-target (freeing the block if
    it is linked), unless the block already leads to the target; else, if
    the block is free and similar, post decomp+ (urgency by similarity); and
    if they have digits in common, post decompi."""
    target = send(world, world[_CYTO_TARGET], "value")
    cyto_current_target = send(world, world[_CURRENT_TARGET], "name")
    current_target = send(world, lisp_eval(world, cyto_current_target), "value")
    block = send(world, cyto_block, "value")
    stat = send(world, cyto_block, "status")
    if _eqn(0, sim(world, block, current_target)) or _eqn(0, sim(world, block, target)):
        if is_linked_to_target(world, cyto_block):
            pass
        else:
            if franz.equal("linked", stat):
                kill_block(world, send(world, send(world, cyto_block, "upper-neighbor"),
                                       "upper-neighbor"))
            world.cr_hang(world[_CODERACK], [_REPLACE_TARGET, cyto_block, cyto_current_target],
                          world[_P_UPPER_URGENCY])
    elif franz.nequal("free", stat):
        pass
    else:
        if _eqn(1, sim(world, block, current_target)):
            world.cr_hang(world[_CODERACK], [_DECOMP_PLUS, cyto_block, cyto_current_target],
                          world[_P_FIRST_URGENCY])
        elif _eqn(2, sim(world, block, current_target)):
            world.cr_hang(world[_CODERACK], [_DECOMP_PLUS, cyto_block, cyto_current_target],
                          world[_P_SECOND_URGENCY])
        elif _eqn(3, sim(world, block, current_target)):
            # 1987: this clause reads Cyto-current-target.  Franz is case
            # sensitive, so that was an unbound variable; SBCL's reader
            # upcases it (PORTING_NOTES.md, "Observations (for item 9)").
            world.cr_hang(world[_CODERACK], [_DECOMP_PLUS, cyto_block, cyto_current_target],
                          world[_P_FOURTH_URGENCY])
    if not digits_in_common(block, current_target):
        return None
    return world.cr_hang(world[_CODERACK],
                         [_DECOMPI, cyto_block, cyto_current_target,
                          [_QUOTE, digits_in_common(block, current_target)]],
                         world[_P_SECOND_URGENCY])


def const_bl_plus(world, cyto_block1, cyto_block2, sum):
    """codelets.lisp: const-bl+.  If both blocks are still free, link them,
    create the block SUM above them (activation 300 for a multiple of 10,
    250 of 5, else 200) and a plus operation node, its result the new block
    (SUM = sum of the two) or the larger of the two (SUM = their
    difference)."""
    activation = 200
    cyto_res = cyto_op1 = cyto_op2 = None
    stblock1 = send(world, cyto_block1, "status")
    stblock2 = send(world, cyto_block2, "status")
    block1 = send(world, cyto_block1, "value")
    block2 = send(world, cyto_block2, "value")
    lev1 = send(world, cyto_block1, "level")
    lev2 = send(world, cyto_block2, "level")
    lev = franz.max_(lev1, lev2)
    if franz.nequal("free", stblock1) or franz.nequal("free", stblock2):
        return None
    send(world, cyto_block1, "set-status", "linked")
    send(world, cyto_block2, "set-status", "linked")
    cyto_block_sum = franz.concat(_CYTO_BLOCK_PREFIX, sum, _V_PREFIX, world[_NAME_COUNTER])
    # 1987: Franz mod is rem, so a negative SUM is a multiple of 5 only if
    # its remainder is 0 (PORTING_NOTES.md, census: `mod`).
    if _eqn(0, franz.mod(sum, 5)):
        activation = 250
    if _eqn(0, franz.mod(sum, 10)):
        activation = 300
    create_cyto_node(world, activation, cyto_block_sum, sum, "free", lev + 2, "4bl", 1)
    if franz.equal(sum, block1 + block2):
        cyto_res = lisp_eval(world, cyto_block_sum)
        cyto_op1 = cyto_block1
        cyto_op2 = cyto_block2
    elif franz.equal(sum, block1 - block2):
        cyto_res = cyto_block1
        cyto_op1 = cyto_block2
        cyto_op2 = lisp_eval(world, cyto_block_sum)
    elif franz.equal(sum, block2 - block1):
        cyto_res = cyto_block2
        cyto_op1 = cyto_block1
        cyto_op2 = lisp_eval(world, cyto_block_sum)
    # With no clause, cyto-op1 is nil and (send nil :value) is an error.
    cyto_op_node = franz.concat(
        _PLUS,
        franz.min_(send(world, cyto_op1, "value"), send(world, cyto_op2, "value")), _DASH,
        franz.max_(send(world, cyto_op1, "value"), send(world, cyto_op2, "value")),
        _V_PREFIX, world[_NAME_COUNTER])
    create_op_node(world, cyto_op_node, cyto_res, cyto_op1, cyto_op2, 50, lev + 1)
    world[_NAME_COUNTER] = 1 + world[_NAME_COUNTER]
    return world[_NAME_COUNTER]


def const_blx(world, cyto_block1, cyto_block2, prod):
    """codelets.lisp: const-blx.  If both blocks are still free, link them,
    create the block PROD above them (activation as in const-bl+) and a
    times operation node, its result the new block."""
    activation = 200
    stblock1 = send(world, cyto_block1, "status")
    stblock2 = send(world, cyto_block2, "status")
    block1 = send(world, cyto_block1, "value")
    block2 = send(world, cyto_block2, "value")
    lev1 = send(world, cyto_block1, "level")
    lev2 = send(world, cyto_block2, "level")
    lev = franz.max_(lev1, lev2)
    if franz.nequal("free", stblock1) or franz.nequal("free", stblock2):
        return None
    send(world, cyto_block1, "set-status", "linked")
    send(world, cyto_block2, "set-status", "linked")
    cyto_block_prod = franz.concat(_CYTO_BLOCK_PREFIX, prod, _V_PREFIX, world[_NAME_COUNTER])
    if _eqn(0, franz.mod(prod, 5)):
        activation = 250
    if _eqn(0, franz.mod(prod, 10)):
        activation = 300
    create_cyto_node(world, activation, cyto_block_prod, prod, "free", lev + 2, "4bl", 1)
    cyto_op_node = franz.concat(_TIMES, block1, _DASH, block2, _V_PREFIX, world[_NAME_COUNTER])
    create_op_node(world, cyto_op_node, lisp_eval(world, cyto_block_prod),
                   cyto_block1, cyto_block2, 50, lev + 1)
    world[_NAME_COUNTER] = world[_NAME_COUNTER] + 1
    return world[_NAME_COUNTER]


def create_coderack(world):
    """codelets.lisp: create-coderack.  Make the coderack my-coderack, with
    the six urgency levels and 0, and name it in *coderack*."""
    coderack.cr_make_coderack(world, _MY_CODERACK,
                              [world[_P_UPPER_URGENCY], world[_P_FIRST_URGENCY],
                               world[_P_SECOND_URGENCY], world[_P_THIRD_URGENCY],
                               world[_P_FOURTH_URGENCY], world[_P_FIFTH_URGENCY], 0])
    world[_CODERACK] = _MY_CODERACK
    return _MY_CODERACK


def create_cyto_node(world, activation, name, value, status, level, type, success):
    """codelets.lisp: create-cyto-node.  Make the cyto-node NAME (the global
    NAME holds it), add it to the cytoplasm, post link-to-pnet, and
    compare-b-to-t for a brick or block; a target or derived target becomes
    the current target."""
    register_node(world, CytoNode(activation=activation, name=name, value=value,
                                  status=status, level=level, type=type, success=success))
    world.out.write(f"Node {franz.princ_to_string(name)} created\n")
    cytoplasm = world[_CYTOPLASM]
    send(world, cytoplasm, "set-nodes",
         [lisp_eval(world, name)] + list(send(world, cytoplasm, "nodes") or ()))
    world.cr_hang(world[_CODERACK], [_LINK_TO_PNET, name, type, value],
                  world[_P_UPPER_URGENCY])
    if franz.equal(type, "2b") or franz.equal(type, "4bl"):
        world.cr_hang(world[_CODERACK], [_COMPARE_B_TO_T, name], world[_P_UPPER_URGENCY])
    if franz.equal(type, "1t") or franz.equal(type, "3dt"):
        return update_current_target(world, lisp_eval(world, name), 100)
    return None


def create_op_node(world, name, res, op1, op2, activation, level):
    """codelets.lisp: create-op-node.  Make the operation node NAME ("5g"),
    the neighbor of RES (result), OP1 and OP2 (operands), and add it to the
    cytoplasm.  ACTIVATION is not used."""
    # 1987: n1, n2 and n3 are free SETQs: the globals.  Their lists are the
    # node's neighbors (shared structure).
    world[_N1] = [res, _RESULT]
    world[_N2] = [op1, _OPERAND]
    world[_N3] = [op2, _OPERAND]
    register_node(world, CytoNode(name=name, type="5g", level=level,
                                  neighbors=[world[_N1], world[_N2], world[_N3]]))
    world.out.write(f"Node {franz.princ_to_string(name)} created\n")
    cytoplasm = world[_CYTOPLASM]
    send(world, cytoplasm, "set-nodes",
         [lisp_eval(world, name)] + list(send(world, cytoplasm, "nodes") or ()))
    send(world, res, "update-neighbors", [[lisp_eval(world, name), _RESULT]])
    send(world, op1, "update-neighbors", [[lisp_eval(world, name), _OPERAND]])
    return send(world, op2, "update-neighbors", [[lisp_eval(world, name), _OPERAND]])


def _post_compare_for_free_blocks(world):
    """decomp+ and decompx's last loop: compare-b-to-t for every brick and
    block not linked to the target."""
    for nn in send(world, world[_CYTOPLASM], "cyto-brick-block-nodes") or ():
        if is_linked_to_target(world, nn):
            pass
        else:
            world.cr_hang(world[_CODERACK], [_COMPARE_B_TO_T, nn], world[_P_FIRST_URGENCY])
    return None


def decomp_plus(world, cyto_block, cyto_current_target):
    """codelets.lisp: decomp+.  If the block and the current target are
    still free: equal, post replace-target; else create the derived target
    of their difference (the new current target), link both, and make the
    plus operation node; then post compare-b-to-t for the free bricks and
    blocks."""
    stblock = send(world, cyto_block, "status")
    sttarget = send(world, cyto_current_target, "status")
    lev = send(world, cyto_current_target, "level")
    block = send(world, cyto_block, "value")
    current_target = send(world, cyto_current_target, "value")
    if franz.nequal("free", stblock) or franz.nequal("free", sttarget):
        return None
    if _eqn(block, current_target):
        return world.cr_hang(world[_CODERACK],
                             [_REPLACE_TARGET, cyto_block, cyto_current_target],
                             world[_P_UPPER_URGENCY])
    # 1987: oper is not in the let: a free SETQ, the global.
    if block > current_target:
        res = block
        cyto_res = cyto_block
        world[_OPER] = current_target
        cyto_oper = cyto_current_target
    elif block <= current_target:
        world[_OPER] = block
        cyto_oper = cyto_block
        res = current_target
        cyto_res = cyto_current_target
    diff = res - world[_OPER]
    cyto_derived_target = franz.concat(_CYTO_TARGET_PREFIX, diff, _V_PREFIX,
                                       world[_NAME_COUNTER])
    create_cyto_node(world, 200, cyto_derived_target, diff, "free", lev - 2, "3dt", 0)
    send(world, cyto_current_target, "set-status", "linked")
    send(world, cyto_block, "set-status", "linked")
    update_current_target(world, lisp_eval(world, cyto_derived_target), 50)
    cyto_op_node = franz.concat(_PLUS, world[_OPER], _DASH, diff, _V_PREFIX,
                                world[_NAME_COUNTER])
    world[_NAME_COUNTER] = 1 + world[_NAME_COUNTER]
    create_op_node(world, cyto_op_node, cyto_res, cyto_oper,
                   lisp_eval(world, cyto_derived_target), 50, lev - 1)
    return _post_compare_for_free_blocks(world)


def decompx(world, cyto_block, cyto_current_target):
    """codelets.lisp: decompx.  As decomp+, for a quotient: the derived
    target CURRENT-TARGET / BLOCK and a times operation node.  Never posted
    (compare-b-to-t posts no decompx)."""
    # 1987: the let lists cyto-block, which shadows the argument with nil,
    # so the first send is (send nil :status), an error.  The rest is the
    # source, unreachable.
    cyto_block = None
    stblock = send(world, cyto_block, "status")
    sttarget = send(world, cyto_current_target, "status")
    lev = send(world, cyto_current_target, "level")
    if franz.nequal("free", stblock) or franz.nequal("free", sttarget):
        return None
    oper = send(world, cyto_block, "value")
    res = send(world, cyto_current_target, "value")
    cyto_oper = cyto_block
    cyto_res = cyto_current_target
    quot = franz.divide(res, oper)
    cyto_derived_target = franz.concat(_CYTO_TARGET_PREFIX, quot, _V_PREFIX,
                                       world[_NAME_COUNTER])
    create_cyto_node(world, 200, cyto_derived_target, quot, "free", lev - 2, "3dt", 0)
    send(world, cyto_current_target, "set-status", "linked")
    send(world, cyto_block, "set-status", "linked")
    update_current_target(world, lisp_eval(world, cyto_derived_target), 50)
    cyto_op_node = franz.concat(_TIMES, oper, _DASH, quot, _V_PREFIX, world[_NAME_COUNTER])
    world[_NAME_COUNTER] = world[_NAME_COUNTER] + 1
    create_op_node(world, cyto_op_node, cyto_res, cyto_oper,
                   lisp_eval(world, cyto_derived_target), 50, lev - 1)
    return _post_compare_for_free_blocks(world)


def decompi(world, cyto_block, cyto_current_target, rank_list):
    """codelets.lisp: decompi.  If the current target starts with the
    block's digits (RANK-LIST from digits-in-common: (_ 1 _) for 10 times,
    (1 _ _) for 100 times), raise the block's activation and post
    look-for-blx for block x 10 (or 100); nothing for a block of 1."""
    send(world, cyto_current_target, "value")  # target is read, and not used
    block = send(world, cyto_block, "value")
    op2 = None
    if _eqn(1, block):
        pass
    elif _eqn(1, _cadr(rank_list)):
        op2 = 10
        send(world, cyto_block, "set-activation", 200)
    elif _eqn(1, _car(rank_list)):
        op2 = 100
        send(world, cyto_block, "set-activation", 100)
    if op2:
        return world.cr_hang(world[_CODERACK], [_LOOK_FOR_BLX, block * op2, block, op2],
                             world[_P_SECOND_URGENCY])
    return None


def decompose(world, node):
    """codelets.lisp: decompose.  Print how the node named NODE was made:
    for each operation node next to it not yet listed, mark it listed,
    print its two other neighbors, and decompose those that are not
    bricks."""
    op = send(world, lisp_eval(world, node), "neighbors")
    for x in op or ():
        if send(world, x[0], "listed"):
            continue
        send(world, x[0], "set-listed", True)
        opn = send(world, x[0], "name")
        close = send(world, x[0], "neighbors")
        operands = sup(close, lisp_eval(world, node))
        op1v = send(world, _car(operands), "value")
        op2v = send(world, _cadr(operands), "value")
        op1n = send(world, _car(operands), "name")
        op2n = send(world, _cadr(operands), "name")
        p = franz.princ_to_string
        world.out.write(f"Operation {p(opn)} has been applied \n")
        world.out.write(f"to {p(op1n)} ( {p(op1v)}) and to {p(op2n)} ( {p(op2v)})\n")
        world.out.write(f"to get {p(send(world, lisp_eval(world, node), 'name'))}\n")
        if franz.nequal("2b", send(world, lisp_eval(world, op1n), "type")):
            decompose(world, op1n)
        if franz.nequal("2b", send(world, lisp_eval(world, op2n), "type")):
            decompose(world, op2n)
    return None


def decrease_interest(world):
    """codelets.lisp: decrease-interest.  Multiply the activation of the
    free secondary cyto-nodes by 0.6."""
    cyto_nodes = send(world, world[_CYTOPLASM], "free-secondary-cyto-nodes")
    for node in cyto_nodes or ():
        # 1987: new is a free SETQ: the global.
        world[_NEW] = send(world, node, "activation") * 0.6
        send(world, node, "set-activation", world[_NEW])
    return None


def diff300(val):
    """codelets.lisp: diff300.  300 - VAL (check-temperature's weights)."""
    return 300 - val


def digits_in_common(val1, val2):
    """codelets.lisp: digits-in-common.  Whether VAL1 (<= VAL2) ends VAL2
    ((0 0 1): same last digit), or starts it ((0 1 _): VAL2 / 10 = VAL1,
    (1 _ _): VAL2 / 100 = VAL1); nil if none."""
    res = _DIGITS_000
    if val1 > val2:
        res = None
    else:
        if franz.mod(val2 - val1, 10) == 0:
            res = _DIGITS_001
        if franz.equal(val1, franz.star_quo(val2, 10)):
            res = [0, 1, res[2]]
        if franz.equal(val1, franz.star_quo(val2, 100)):
            res = [1] + res[1:]
    if franz.equal(res, [0, 0, 0]):
        res = None
    return res


def disconnect(world, cyto_node, op):
    """codelets.lisp: disconnect.  Remove CYTO-NODE and its operation node
    OP: free OP's two other neighbors, unlink everything from OP, take both
    out of the cytoplasm, mark CYTO-NODE killed, and post free-from-pnet."""
    close = send(world, op, "neighbors")
    operands = sup(close, cyto_node)
    op1n = send(world, _car(operands), "name")
    op2n = send(world, _cadr(operands), "name")
    if franz.equal("linked", send(world, lisp_eval(world, op1n), "status")):
        send(world, lisp_eval(world, op1n), "set-status", "free")
    if franz.equal("linked", send(world, lisp_eval(world, op2n), "status")):
        send(world, lisp_eval(world, op2n), "set-status", "free")
    send(world, lisp_eval(world, op1n), "suppress-neighbors", op)
    send(world, lisp_eval(world, op2n), "suppress-neighbors", op)
    send(world, cyto_node, "suppress-neighbors", op)
    cytoplasm = world[_CYTOPLASM]
    send(world, cytoplasm, "suppress-node", op)
    opn = send(world, op, "name")
    world.out.write(f"Node {franz.princ_to_string(opn)} killed\n")
    send(world, world[_CYTOPLASM], "suppress-node", cyto_node)
    send(world, cyto_node, "set-status", "killed")
    cyton = send(world, cyto_node, "name")
    world.out.write(f"Node {franz.princ_to_string(cyton)} killed\n")
    return world.cr_hang(world[_CODERACK], [_FREE_FROM_PNET, cyto_node],
                         world[_P_FIRST_URGENCY])


def eliminate(world, element, list):
    """codelets.lisp: eliminate.  LIST without its member closest to ELEMENT
    (the first, on ties)."""
    diffmin = 999
    # 1987: diff and min are free SETQs, the globals.  With no difference
    # under 999 (or an empty LIST), min keeps its old value, which remove-dd
    # then removes, or misses (PORTING_NOTES.md, globals.lisp: `min`).
    for x in list or ():
        world[_DIFF] = abs(element - x)
        if world[_DIFF] < diffmin:
            diffmin = world[_DIFF]
            world[_MIN] = x
    return remove_dd(world[_MIN], list)


def find_activations(world, list):
    """codelets.lisp: find-activations.  The activations of the cyto-nodes
    of LIST."""
    res = [send(world, node, "activation") for node in list or ()]
    return res or None


def find_in(world, element, list, level=4):
    """codelets.lisp: find-in.  A node of LIST with a value similar to
    ELEMENT (sim below LEVEL): nodes are drawn by activation (randlist) and
    dropped until one is close enough; nil if none is."""
    res1 = list
    res2 = find_activations(world, list)
    rep = None
    x = randlist(world, res2)
    while not franz.equal(None, x):
        rep = _nth(x, res1)
        if level > sim(world, send(world, rep, "value"), element):
            res2 = None
        else:
            res1 = remove_dd(rep, res1)
            res2 = remove_dd(_nth(x, res2), res2)
            rep = None
        x = randlist(world, res2)
    return rep


def find_interest_in_pnet(world, val):
    """codelets.lisp: find-interest-in-pnet.  The urgency of building a block
    of value VAL: upper for the current target's value, first if its pnode's
    first instance is a target, third for a brick or block, else by the
    pnode's activation; fifth from 200, 0 from 500."""
    urgency = None
    if val < 200:
        node = find_node(world, val)
        priority = send(world, node, "activation")
        type = _car(_car(franz.sortcar(send(world, node, "instances"), None)))
        if _eqn(val, send(world, send(world, world[_CURRENT_TARGET], "name"), "value")):
            urgency = world[_P_UPPER_URGENCY]
        elif franz.equal(type, "1t") or franz.equal(type, "3dt"):
            urgency = world[_P_FIRST_URGENCY]
        elif franz.nequal(type, "2b") and franz.nequal(type, "4bl"):
            if priority < 10:
                urgency = world[_P_FIFTH_URGENCY]
            elif priority < 14:
                urgency = world[_P_FOURTH_URGENCY]
            elif priority < 50:
                urgency = world[_P_THIRD_URGENCY]
            else:
                urgency = world[_P_SECOND_URGENCY]
        else:
            urgency = world[_P_THIRD_URGENCY]
    elif val < 500:
        urgency = world[_P_FIFTH_URGENCY]
    else:
        urgency = 0
    return urgency


def find_node(world, val):
    """codelets.lisp: find-node.  The pnode for VAL, found as link-to-pnet
    finds it: node-VAL, else node-1 for 0, the rounded value's, or
    node-150."""
    # 1987: address is a free SETQ: the global ADDRESS.
    world[_ADDRESS] = franz.concat(_NODE_PREFIX, val)
    if world[_ADDRESS] in world:
        pass
    elif 0 == val:
        world[_ADDRESS] = _NODE_1
    elif round_(world, val) < 200:
        world[_ADDRESS] = franz.concat(_NODE_PREFIX, round_(world, val))
    else:
        world[_ADDRESS] = _NODE_150
    return lisp_eval(world, world[_ADDRESS])


def free_from_pnet(world, name):
    """codelets.lisp: free-from-pnet.  Remove the cyto-node NAME from the
    instances of its pnode, which loses %node-minus% activation."""
    pnode = _car(send(world, name, "plinks"))
    if pnode:
        send(world, pnode, "suppress-instances", name)
        return send(world, pnode, "add-activation", world[_P_NODE_MINUS])
    return None


def is_linked_to(world, cyto_block1, cyto_block2):
    """codelets.lisp: is-linked-to.  Whether the lower of two cyto-nodes (on
    different levels, not both free) leads up to the other, two levels
    (operation, node) at a time, before a free node or the target."""
    lev1 = send(world, cyto_block1, "level")
    lev2 = send(world, cyto_block2, "level")
    st1 = send(world, cyto_block1, "status")
    st2 = send(world, cyto_block2, "status")
    if _eqn(lev1, lev2):
        return None
    if franz.equal("free", st1) and franz.equal("free", st2):
        return None
    if lev1 > lev2:
        cyto_block1, cyto_block2 = cyto_block2, cyto_block1
    if franz.nequal("linked", send(world, cyto_block1, "status")):
        return None
    node = send(world, send(world, cyto_block1, "upper-neighbor"), "upper-neighbor")
    while node is not None:
        if _eq(node, cyto_block2):
            return True
        if franz.equal("free", send(world, node, "status")):
            return None
        if franz.equal("1t", send(world, node, "type")):
            return None
        node = send(world, send(world, node, "upper-neighbor"), "upper-neighbor")
    return None


def is_linked_to_target(world, cyto_block):
    """codelets.lisp: is-linked-to-target.  Whether the linked CYTO-BLOCK
    leads up to the target ("1t"), two levels (operation, node) at a time,
    without meeting a free node."""
    if franz.equal("linked", send(world, cyto_block, "status")):
        node = cyto_block
        while node is not None:
            if franz.equal("free", send(world, node, "status")):
                return None
            if franz.equal("1t", send(world, node, "type")):
                return True
            node = send(world, send(world, node, "upper-neighbor"), "upper-neighbor")
        return None
    return None


def kill_block(world, cyto_block):
    """codelets.lisp: kill-block.  Disconnect CYTO-BLOCK from the nodes under
    it; if it was linked, kill the block above it, or, if that is the target,
    the derived target next to it."""
    disconnect(world, cyto_block, send(world, cyto_block, "lower-neighbor"))
    if not send(world, cyto_block, "neighbors"):
        return None
    op = send(world, cyto_block, "neighbors")[0][0]
    cyto_node = send(world, op, "upper-neighbor")
    # 1987: no clause for a "3dt" parent, so a block used against a derived
    # target leaves its operation node behind (PORTING_NOTES.md, item 10,
    # "kill-block leaves dangling operation nodes").
    if franz.equal("4bl", send(world, cyto_node, "type")):
        return kill_block(world, cyto_node)
    if franz.equal("1t", send(world, cyto_node, "type")):
        cyto_node = send(world, op, "lower-dtarget-neighbor")
        return kill_dtarget(world, cyto_node)
    return None


def kill_dtarget(world, cyto_dtarget):
    """codelets.lisp: kill-dtarget.  Disconnect CYTO-DTARGET from the nodes
    above it; if it was linked, kill the derived target under it."""
    disconnect(world, cyto_dtarget, send(world, cyto_dtarget, "upper-neighbor"))
    if not send(world, cyto_dtarget, "neighbors"):
        return None
    op = send(world, cyto_dtarget, "neighbors")[0][0]
    cyto_node = send(world, op, "lower-dtarget-neighbor")
    return kill_dtarget(world, cyto_node)


def kill_node(world, cyto_node):
    """codelets.lisp: kill-node.  Kill the block or derived target CYTO-NODE
    (if still in the cytoplasm) and what depends on it, find a new current
    target and reactivate its pnode, and post compare-b-to-t for the bricks
    and blocks not linked to the target (except a killed derived target's
    old block)."""
    type = None
    pnode = None
    new_target = None
    old_block = None
    if franz.memq(cyto_node, send(world, world[_CYTOPLASM], "nodes")):
        type = send(world, cyto_node, "type")
        if franz.equal(type, "4bl"):
            kill_block(world, cyto_node)
        elif franz.equal(type, "3dt"):
            old_block = send(world, cyto_node, "block-neighbor")
            kill_dtarget(world, cyto_node)
        new_target = send(world, world[_CYTOPLASM], "find-new-target")
        update_current_target(world, new_target, 100)
        if new_target:
            pnode = _car(send(world, send(world, world[_CURRENT_TARGET], "name"), "plinks"))
        if pnode:
            world.cr_hang(world[_CODERACK], [_ACTIVATE, world[_P_DTARGET_PLUS], pnode, True],
                          world[_P_UPPER_URGENCY])
        for nn in send(world, world[_CYTOPLASM], "cyto-brick-block-nodes") or ():
            if is_linked_to_target(world, nn):
                pass
            elif _eq(old_block, nn):
                pass
            else:
                world.cr_hang(world[_CODERACK], [_COMPARE_B_TO_T, nn], world[_P_SECOND_URGENCY])
    return None


def link_to_pnet(world, name, type, value):
    """codelets.lisp: link-to-pnet.  Unless the cyto-node NAME was killed:
    record it as an instance of the pnode for VALUE (node-VALUE, else the
    rounded value's, node-1 for 0, node-150 past 200), link it to that
    pnode, and post activate with an activation that depends on its type
    (less for an approximate pnode)."""
    repump_yes_or_no = None
    address = None
    transact = None
    if franz.equal("killed", send(world, lisp_eval(world, name), "status")):
        return None
    # 1987: activation is not in the let, so these SETQs set the global
    # ACTIVATION, which keeps its last value for any other type
    # (PORTING_NOTES.md, "Compile census").
    if franz.equal(type, "1t"):
        world[_ACTIVATION] = world[_P_TARGET_ACTIVATION]
        repump_yes_or_no = True
    elif franz.equal(type, "2b"):
        world[_ACTIVATION] = world[_P_BRICK_ACTIVATION]
    elif franz.equal(type, "3dt"):
        world[_ACTIVATION] = world[_P_DTARGET_ACTIVATION]
        repump_yes_or_no = True
    elif franz.equal(type, "4bl"):
        world[_ACTIVATION] = world[_P_BLOCK_ACTIVATION]
    address = franz.concat(_NODE_PREFIX, value)
    if address in world:
        transact = world[_ACTIVATION]
    elif 0 == value:
        address = _NODE_1
        transact = 0
    elif round_(world, value) < 200:
        address = franz.concat(_NODE_PREFIX, round_(world, value))
        transact = world[_ACTIVATION] * ratio(world, value)
    else:
        address = _NODE_150
        transact = world[_ACTIVATION] * 0.50
    send(world, lisp_eval(world, address), "update-instances", [type, name])
    send(world, name, "update-plinks", lisp_eval(world, address))
    return world.cr_hang(world[_CODERACK], [_ACTIVATE, transact, address, repump_yes_or_no],
                         world[_P_UPPER_URGENCY])


def look_for_approx_bl_plus(world, res, op1, op2):
    """codelets.lisp: look-for-approx-bl+.  If one of RES = OP1 + OP2 is
    similar to the current target, find blocks similar to the two others
    and post test-if-possible-and-desirable for their difference (if RES is
    one of the two) or sum; with only one block, post decomp+ for it."""
    current_target = _current_target_value(world)
    if not compare(world, current_target, [res, op1, op2]):
        return None
    values_to_find = eliminate(world, current_target, [res, op1, op2])
    blocks = send(world, world[_CYTOPLASM], "cyto-brick-block-nodes")
    rn = world.rng.random(2)
    first_value = _nth(rn, values_to_find)
    cyto_block1 = find_in(world, first_value, blocks)
    # 1987: when find-in found nothing, (remove-dd nil blocks) misses and
    # returns the blocks reversed, so the second find-in draws from that
    # order (remove-dd's comment).
    n_blocks = remove_dd(cyto_block1, blocks)
    second_value = _nth(1 - rn, values_to_find)
    cyto_block2 = find_in(world, second_value, n_blocks)
    if cyto_block1 is None and cyto_block2 is None:
        return None
    if cyto_block1 is None:
        return world.cr_hang(world[_CODERACK],
                             [_DECOMP_PLUS, cyto_block2,
                              send(world, world[_CURRENT_TARGET], "name")],
                             world[_P_SECOND_URGENCY])
    if cyto_block2 is None:
        return world.cr_hang(world[_CODERACK],
                             [_DECOMP_PLUS, cyto_block1,
                              send(world, world[_CURRENT_TARGET], "name")],
                             world[_P_SECOND_URGENCY])
    v1 = send(world, cyto_block1, "value")
    v2 = send(world, cyto_block2, "value")
    if franz.member(res, values_to_find):
        sum = abs(v1 - v2)
    else:
        sum = v1 + v2
    return world.cr_hang(world[_CODERACK],
                         [_TEST_IF, cyto_block1, cyto_block2, "const-bl+", sum],
                         world[_P_SECOND_URGENCY])


def look_for_approx_blx(world, res, op1, op2):
    """codelets.lisp: look-for-approx-blx.  If one of RES = OP1 x OP2 is
    similar to the current target, find blocks similar to the two others
    and, if both are found, post test-if-possible-and-desirable for their
    product."""
    current_target = _current_target_value(world)
    if not compare(world, current_target, [res, op1, op2]):
        return None
    values_to_find = eliminate(world, current_target, [res, op1, op2])
    blocks = send(world, world[_CYTOPLASM], "cyto-brick-block-nodes")
    rn = world.rng.random(2)
    first_value = _nth(rn, values_to_find)
    cyto_block1 = find_in(world, first_value, blocks)
    # 1987: when find-in found nothing, (remove-dd nil blocks) misses and
    # returns the blocks reversed, so the second find-in draws from that
    # order (as in look-for-approx-bl+).
    n_blocks = remove_dd(cyto_block1, blocks)
    second_value = _nth(1 - rn, values_to_find)
    cyto_block2 = find_in(world, second_value, n_blocks)
    if cyto_block1 is None or cyto_block2 is None:
        return None
    v1 = send(world, cyto_block1, "value")
    v2 = send(world, cyto_block2, "value")
    prod = v1 * v2
    return world.cr_hang(world[_CODERACK],
                         [_TEST_IF, cyto_block1, cyto_block2, "const-blx", prod],
                         world[_P_SECOND_URGENCY])


def _is_target(world, cyto_node):
    """A target or derived target ("1t", "3dt")."""
    return (franz.equal("1t", send(world, cyto_node, "type"))
            or franz.equal("3dt", send(world, cyto_node, "type")))


def look_for_bl_plus(world, res, op1, op2):
    """codelets.lisp: look-for-bl+.  For RES = OP1 + OP2: drop the value
    closest to the current target, take a cyto-node of the pnode of each
    other one (the first instance, and the last, in case they are equal),
    and if they are two blocks or bricks giving the current target, post
    test-if-possible-and-desirable at upper urgency; else post
    look-for-approx-bl+."""
    current_target = _current_target_value(world)
    values_to_find = eliminate(world, current_target, [res, op1, op2])
    node1 = find_node(world, _car(values_to_find))
    node2 = find_node(world, _cadr(values_to_find))
    cyto_block1 = _cadar(send(world, lisp_eval(world, node1), "instances"))
    cyto_block2 = _cadar((send(world, lisp_eval(world, node2), "instances") or [])[::-1])
    approx = [_LOOK_FOR_APPROX_BL_PLUS, res, op1, op2]
    if cyto_block1 is None or cyto_block2 is None or _eq(cyto_block1, cyto_block2):
        return world.cr_hang(world[_CODERACK], approx, world[_P_FIRST_URGENCY])
    if _is_target(world, cyto_block1):
        return world.cr_hang(world[_CODERACK], approx, world[_P_FIRST_URGENCY])
    if _is_target(world, cyto_block2):
        return world.cr_hang(world[_CODERACK], approx, world[_P_FIRST_URGENCY])
    v1 = send(world, cyto_block1, "value")
    v2 = send(world, cyto_block2, "value")
    if _eqn(res, _car(values_to_find)):
        sum = v1 - v2
    elif _eqn(res, _cadr(values_to_find)):
        sum = v2 - v1
    else:
        sum = v1 + v2
    if franz.nequal(current_target, sum):
        return world.cr_hang(world[_CODERACK], approx, world[_P_FIRST_URGENCY])
    return world.cr_hang(world[_CODERACK],
                         [_TEST_IF, cyto_block1, cyto_block2, "const-bl+", sum],
                         world[_P_UPPER_URGENCY])


def look_for_blx(world, res, op1, op2):
    """codelets.lisp: look-for-blx.  For RES = OP1 x OP2, as look-for-bl+:
    post test-if-possible-and-desirable "const-blx" at upper urgency if the
    two cyto-nodes multiply to the current target, else
    look-for-approx-blx."""
    # 1987: unlike look-for-bl+, no let: current-target, values-to-find,
    # node1, node2, cyto-block1 and cyto-block2 are the globals
    # (PORTING_NOTES.md, "Warnings left in place").
    world[_G_CURRENT_TARGET] = _current_target_value(world)
    world[_VALUES_TO_FIND] = eliminate(world, world[_G_CURRENT_TARGET], [res, op1, op2])
    world[_NODE1] = find_node(world, _car(world[_VALUES_TO_FIND]))
    world[_NODE2] = find_node(world, _cadr(world[_VALUES_TO_FIND]))
    world[_CYTO_BLOCK1] = _cadar(send(world, lisp_eval(world, world[_NODE1]), "instances"))
    world[_CYTO_BLOCK2] = _cadar(
        (send(world, lisp_eval(world, world[_NODE2]), "instances") or [])[::-1])
    approx = [_LOOK_FOR_APPROX_BLX, res, op1, op2]
    if world[_CYTO_BLOCK1] is None or world[_CYTO_BLOCK2] is None \
            or _eq(world[_CYTO_BLOCK1], world[_CYTO_BLOCK2]):
        return world.cr_hang(world[_CODERACK], approx, world[_P_FIRST_URGENCY])
    if _is_target(world, world[_CYTO_BLOCK1]):
        return world.cr_hang(world[_CODERACK], approx, world[_P_FIRST_URGENCY])
    if _is_target(world, world[_CYTO_BLOCK2]):
        return world.cr_hang(world[_CODERACK], approx, world[_P_FIRST_URGENCY])
    if franz.nequal(world[_G_CURRENT_TARGET],
                    send(world, world[_CYTO_BLOCK1], "value")
                    * send(world, world[_CYTO_BLOCK2], "value")):
        return world.cr_hang(world[_CODERACK], approx, world[_P_FIRST_URGENCY])
    return world.cr_hang(world[_CODERACK],
                         [_TEST_IF, world[_CYTO_BLOCK1], world[_CYTO_BLOCK2], "const-blx",
                          world[_G_CURRENT_TARGET]],
                         world[_P_UPPER_URGENCY])


def look_for_diff(world, trial):
    """codelets.lisp: look-for-diff.  Draw a block (only free ones on TRIAL
    0), then draw the others until one is similar to it (sim <= 3), and post
    test-if-possible-and-desirable for their difference; with none, post
    look-for-diff for the next trial, up to trial 4."""
    cyto_block2 = None
    urgency = None
    _current_target_value(world)  # current-target is read, and not used
    if 0 == trial:
        blocks = send(world, world[_CYTOPLASM], "free-blocks")
    else:
        blocks = send(world, world[_CYTOPLASM], "cyto-brick-block-nodes")
    if not blocks:
        return None
    rn = world.rng.random(len(blocks))
    cyto_block1 = _nth(rn, blocks)
    value1 = send(world, cyto_block1, "value")
    n_blocks = remove_dd(cyto_block1, blocks)
    while n_blocks:
        node = _nth(world.rng.random(len(n_blocks)), n_blocks)
        # 1987: similarity is a free SETQ: the global.
        world[_SIMILARITY] = sim(world, value1, send(world, node, "value"))
        if 3 < world[_SIMILARITY]:
            n_blocks = remove_dd(node, n_blocks)
        else:
            n_blocks = None
            cyto_block2 = node
    if cyto_block2 is None and trial < 4:
        if 2 > trial:
            urgency = world[_P_FIRST_URGENCY]
        elif 5 > trial:
            urgency = world[_P_SECOND_URGENCY]
        trial = trial + 1
        return world.cr_hang(world[_CODERACK], [_LOOK_FOR_DIFF, trial], urgency)
    if cyto_block2 is not None:
        value2 = send(world, cyto_block2, "value")
        return world.cr_hang(world[_CODERACK],
                             [_TEST_IF, cyto_block1, cyto_block2, "const-bl+",
                              abs(value1 - value2)],
                             world[_P_FIRST_URGENCY])
    return None


def look_for_new_block(world):
    """codelets.lisp: look-for-new-block.  Draw two bricks or blocks by
    activation, and an operation by the activations of node-add,
    node-subtract and node-multiply, and post test-if-possible-and-desirable
    for the result (0 for a product with 1)."""
    blocks = send(world, world[_CYTOPLASM], "cyto-brick-block-nodes")
    # 1987: the let binds list, but the body uses liste, a free global
    # (PORTING_NOTES.md, globals.lisp: `liste`).
    world[_LISTE] = find_activations(world, blocks)
    rank = randlist(world, world[_LISTE])
    cyto_block1 = _nth(rank, blocks)
    blocks = remove_dd(cyto_block1, blocks)
    world[_LISTE] = remove_dd(_nth(rank, world[_LISTE]), world[_LISTE])
    rank = randlist(world, world[_LISTE])
    cyto_block2 = _nth(rank, blocks)
    w_plus = send(world, world[_NODE_ADD], "activation")
    w_minus = send(world, world[_NODE_SUBTRACT], "activation")
    wx = send(world, world[_NODE_MULTIPLY], "activation")
    rank = randlist(world, [w_plus, w_minus, wx])
    if _eqn(0, rank):
        res = send(world, cyto_block1, "value") + send(world, cyto_block2, "value")
        fun = "const-bl+"
    elif _eqn(1, rank):
        res = abs(send(world, cyto_block1, "value") - send(world, cyto_block2, "value"))
        fun = "const-bl+"
    else:
        res = send(world, cyto_block1, "value") * send(world, cyto_block2, "value")
        fun = "const-blx"
        if _eqn(1, send(world, cyto_block1, "value")) \
                or _eqn(1, send(world, cyto_block2, "value")):
            res = 0
    return world.cr_hang(world[_CODERACK], [_TEST_IF, cyto_block1, cyto_block2, fun, res],
                         world[_P_FIRST_URGENCY])


def mean(list):
    """codelets.lisp: mean.  The mean of LIST (0 if empty)."""
    if not list:
        return 0
    sum = _add_all(list)
    # 1987: quotient truncates when the sum is an integer (PORTING_NOTES.md,
    # census: `quotient`).
    return franz.quotient(sum, len(list))


def misfortune(interest, level, status):
    """codelets.lisp: misfortune.  20 / INTEREST, plus 10 if STATUS is
    "free".  LEVEL is not used."""
    st = 0
    if franz.equal("free", status):
        st = 10
    # 1987: quotient truncates for an integer interest: 20 / 300 is 0.
    return franz.quotient(20, interest) + st


def multiple(val1, val2):
    """codelets.lisp: multiple.  t if VAL2 is a multiple of VAL1, VAL1 not 1
    and smaller."""
    if val1 >= val2:
        return None
    if franz.equal(1, val1):
        return None
    if franz.mod(val2, val1) == 0:
        return True
    return None


def propagate_success(world):
    """codelets.lisp: propagate-success.  Until nothing changes: for each
    operation node with two neighbors of success 1, give the third success
    1; if the third is the target, the problem is solved."""
    # 1987: nn, cont and n are free SETQs: the globals.
    world[_NN] = send(world, world[_CYTOPLASM], "nodes")
    world[_CONT] = 1
    x = 1
    while not franz.equal(0, world[_CONT]):
        world[_CONT] = 0
        for node in world[_NN] or ():
            if franz.equal("5g", send(world, node, "type")):
                world[_N] = update_success(world, send(world, node, "neighbors"))
            else:
                world[_N] = None
            if world[_N] is None:
                pass
            elif franz.equal("1t", send(world, world[_N], "type")):
                world[_PROBLEM_SOLVED] = 1
                world[_CONT] = 0
            else:
                send(world, world[_N], "set-success", 1)
                world[_CONT] = 1
        x = x + 1
    return None


def randlist(world, list):
    """codelets.lisp: randlist.  A random index into LIST, with its elements
    as weights; nil if LIST is empty or its sum (fix) is not positive."""
    if not list:
        return None
    if 0 < franz.fix(_add_all(list)):
        r = world.rng.random(franz.fix(_add_all(list)))
        sum = 0
        i = -1
        for i, weight in enumerate(list):
            sum = sum + weight
            if r < sum:
                break
        return i
    return None


def ratio(world, num):
    """codelets.lisp: ratio.  How close NUM is to its round value:
    min/max of the two, as a float."""
    return franz.quotient(float(franz.min_(num, round_(world, num))),
                          float(franz.max_(num, round_(world, num))))


def read_brick(world, i):
    """codelets.lisp: read-brick.  Read brick I of the cytoplasm into the
    context, and post create-cyto-node for it (activation 300 for a multiple
    of 10, else 50)."""
    # 1987: bricki, a, brick, activation and cyto-bricki are free SETQs: the
    # globals (PORTING_NOTES.md, "Compile census").
    world[_BRICKI] = franz.concat(_BRICK_PREFIX, i)
    world[_A] = franz.intern(franz.uconcat("brick", i), franz.find_package("keyword"))
    world[_BRICK] = send(world, world[_CYTOPLASM], world[_A])
    world[_ACTIVATION] = 50
    if 0 == franz.mod(world[_BRICK], 10):
        world[_ACTIVATION] = 300
    update_context(world, "2b", franz.concat(_CYTO_BRICK_PREFIX, i), world[_BRICK], 50, "cyto")
    world[_CYTO_BRICKI] = franz.concat(_CYTO_BRICK_PREFIX, i)
    return world.cr_hang(world[_CODERACK],
                         [_CREATE_CYTO_NODE, world[_ACTIVATION], [_QUOTE, world[_CYTO_BRICKI]],
                          world[_BRICK], "free", 1, "2b", 1],
                         world[_P_FIRST_URGENCY])


def read_target(world):
    """codelets.lisp: read-target.  The current target becomes the symbol
    cyto-target; post create-cyto-node for the target."""
    # 1987: target is a free SETQ: the global.
    world[_TARGET] = send(world, world[_CYTOPLASM], "target")
    update_current_target(world, _CYTO_TARGET, 100)
    return world.cr_hang(world[_CODERACK],
                         [_CREATE_CYTO_NODE, 150, _QUOTED_CYTO_TARGET, world[_TARGET], "free",
                          99, "1t", 0],
                         world[_P_FIRST_URGENCY])


def remove_dd(l1, l):
    """codelets.lisp: remove-dd.  L without its first element eq to L1."""
    res = []
    for i, x in enumerate(l or ()):
        if _eq_dd(x, l1):
            return (res + l[i + 1:]) or None
        res.append(x)
    # 1987: with no such element, the list comes back reversed (as its
    # comment says).
    return res[::-1] or None


def replace_target(world, cyto_block, cyto_current_target):
    """codelets.lisp: replace-target.  If the block has the target's value,
    the problem is solved ("Obvious.").  Else, if CYTO-CURRENT-TARGET is
    still the current target, the block takes its place everywhere
    (neighbors, their neighbors, pnode instances, the cytoplasm), the
    current target becomes cyto-target, and success is propagated."""
    if franz.equal(send(world, world[_CYTO_TARGET], "value"), send(world, cyto_block, "value")):
        world[_PROBLEM_SOLVED] = 1
        world[_CYTO_TARGET] = cyto_block
        world.out.write("Obvious. ")
        return None
    if _eq(cyto_current_target, send(world, world[_CURRENT_TARGET], "name")):
        send(world, cyto_block, "set-status", "linked")
        # 1987: nn and pp are free SETQs: the globals.
        world[_NN] = send(world, cyto_current_target, "neighbors")
        send(world, cyto_block, "update-neighbors", world[_NN])
        for pair in world[_NN] or ():
            send(world, pair[0], "replace-neighbors", cyto_current_target, cyto_block)
        world[_PP] = send(world, cyto_current_target, "plinks")
        for pnode in world[_PP] or ():
            send(world, pnode, "suppress-instances", cyto_current_target)
        update_current_target(world, _CYTO_TARGET, 100)
        send(world, world[_CYTOPLASM], "suppress-node", cyto_current_target)
        return propagate_success(world)
    return None


def repump(world):
    """codelets.lisp: repump.  Set %resultx%, %result+% and %operand% from
    node-multiply's activation, and the activations of the result+, resultx,
    operand, similar, operation and instance pnodes."""
    resx = send(world, world[_NODE_MULTIPLY], "activation")
    res_plus = 60 - resx
    world[_P_RESULTX] = 300 * franz.quotient(resx, 60.0)
    world[_P_RESULT_PLUS] = 100 * franz.quotient(res_plus, 60.0)
    world[_P_OPERAND] = franz.quotient(world[_P_RESULTX], 1.5)
    return pnet_functions.set_up_activations(
        world, [_RESULT_PLUS, world[_P_RESULT_PLUS], _RESULTX, world[_P_RESULTX],
                _OPERAND, world[_P_OPERAND], _SIMILAR, world[_P_SIMILAR],
                _OPERATION, world[_P_OPERATION], _INSTANCE, world[_P_INSTANCE]])


def round_(world, num):
    """codelets.lisp: round.  The closest multiple of 5 (NUM < 20), 10
    (< 100) or 50."""
    # 1987: div is a free SETQ: the global DIV.
    if num < 20:
        world[_DIV] = 5
    elif num < 100:
        world[_DIV] = 10
    else:
        world[_DIV] = 50
    div = world[_DIV]
    # 1987: Franz / truncates; *mod is the balanced residue
    # (PORTING_NOTES.md, census: `/`, `*mod`).
    if franz.star_mod(num, div) < 0:
        return div * (1 + franz.divide(num, div))
    return div * franz.divide(num, div)


def sim(world, val1, val2):
    """codelets.lisp: sim.  The similarity of two values: 0 if equal, then
    1, 2, 3 for a difference relative to VAL2 up to 0.1, 0.2, 0.3, else 4."""
    # 1987: diff and diffrel are free SETQs: the globals.
    world[_DIFF] = abs(val1 - val2)
    world[_DIFFREL] = franz.quotient(float(world[_DIFF]), float(val2))
    if _eqn(0, world[_DIFF]):
        return 0
    if world[_DIFFREL] <= 0.1:
        return 1
    if world[_DIFFREL] <= 0.2:
        return 2
    if world[_DIFFREL] <= 0.3:
        return 3
    return 4


def sup(l, l1):
    """codelets.lisp: sup.  The cars of the pairs of L that don't start
    with L1, reversed."""
    res = []
    for x in l or ():
        if _eq(x[0], l1):
            pass
        else:
            res.insert(0, x[0])
    return res or None


def temperature(world):
    """codelets.lisp: temperature.  The temperature of the cytoplasm, from
    the misfortunes of the secondary nodes, the number of free nodes and the
    level of the current target: max + 2 x mean if there are enough free
    nodes and levels, else (6 - nfree - lev) x max + 2 x mean."""
    misfort = collect_misfortune(world)
    nfree = len(send(world, world[_CYTOPLASM], "free-cyto-nodes") or ())
    # 1987: Franz / truncates (PORTING_NOTES.md, census: `/`).
    lev = franz.divide(
        99 - send(world, send(world, world[_CURRENT_TARGET], "name"), "level"), 2)
    # 1987: with no secondary node, (apply 'max nil) is (max) = 0
    # (PORTING_NOTES.md, item 9).
    if nfree + lev > 4:
        return franz.max_(*(misfort or ())) + 2 * mean(misfort)
    return (6 - nfree - lev) * franz.max_(*(misfort or ())) + 2 * mean(misfort)


def test_if_possible_and_desirable(world, cyto_block1, cyto_block2, fun, res):
    """codelets.lisp: test-if-possible-and-desirable.  Unless the blocks are
    linked to each other or to the target, or RES is 0, post FUN (const-bl+
    or const-blx) for RES at the urgency of its interest
    (find-interest-in-pnet; upper for the target's value).  From the first
    urgency up, a linked block is freed first by killing the node above it,
    unless that node's value is RES."""
    urgency = None
    const = _CONST_BLX
    node = None
    if is_linked_to(world, cyto_block1, cyto_block2):
        urgency = None
    elif _eqn(res, send(world, world[_CYTO_TARGET], "value")):
        urgency = world[_P_UPPER_URGENCY]
    elif is_linked_to_target(world, cyto_block1):
        urgency = None
    elif is_linked_to_target(world, cyto_block2):
        urgency = None
    elif _eqn(0, res):
        urgency = None
    else:
        urgency = find_interest_in_pnet(world, res)
    if urgency is None:
        return None
    if urgency >= world[_P_FIRST_URGENCY]:
        if franz.equal("linked", send(world, cyto_block1, "status")):
            node = send(world, send(world, cyto_block1, "upper-neighbor"), "upper-neighbor")
            if franz.nequal(res, send(world, node, "value")):
                kill_node(world, node)
        if franz.equal("linked", send(world, cyto_block2, "status")):
            node = send(world, send(world, cyto_block2, "upper-neighbor"), "upper-neighbor")
            if franz.nequal(res, send(world, node, "value")):
                kill_node(world, node)
    if franz.equal(fun, "const-bl+"):
        const = _CONST_BL_PLUS
    return world.cr_hang(world[_CODERACK], [const, cyto_block1, cyto_block2, res], urgency)


def update_success(world, l):
    """codelets.lisp: update-success.  Of the three neighbors L of an
    operation node, the one with success 0 if the two others have success
    1, else nil."""
    n1 = _car(_car(l))
    n2 = _car(_cadr(l))
    n3 = _car(_caddr(l))
    s1 = send(world, n1, "success")
    s2 = send(world, n2, "success")
    s3 = send(world, n3, "success")
    if franz.nequal(2, _plus(s1, s2, s3)):
        return None
    if franz.equal(0, s1):
        return n1
    if franz.equal(0, s2):
        return n2
    if franz.equal(0, s3):
        return n3
    return None


# The Lisp name of every codelets.lisp defun -> its Python function's name.
LISP_FUNCTIONS = {_S(lisp.upper()): py for lisp, py in (
    ("activate", "activate"), ("check-temperature", "check_temperature"),
    ("collect-misfortune", "collect_misfortune"), ("compare", "compare"),
    ("compare-b-to-t", "compare_b_to_t"), ("const-bl+", "const_bl_plus"),
    ("const-blx", "const_blx"), ("create-coderack", "create_coderack"),
    ("create-cyto-node", "create_cyto_node"), ("create-op-node", "create_op_node"),
    ("decomp+", "decomp_plus"), ("decompi", "decompi"), ("decompose", "decompose"),
    ("decompx", "decompx"), ("decrease-interest", "decrease_interest"),
    ("diff300", "diff300"), ("digits-in-common", "digits_in_common"),
    ("disconnect", "disconnect"), ("eliminate", "eliminate"),
    ("find-activations", "find_activations"), ("find-in", "find_in"),
    ("find-interest-in-pnet", "find_interest_in_pnet"), ("find-node", "find_node"),
    ("free-from-pnet", "free_from_pnet"), ("is-linked-to", "is_linked_to"),
    ("is-linked-to-target", "is_linked_to_target"), ("kill-block", "kill_block"),
    ("kill-dtarget", "kill_dtarget"), ("kill-node", "kill_node"),
    ("link-to-pnet", "link_to_pnet"), ("look-for-approx-bl+", "look_for_approx_bl_plus"),
    ("look-for-approx-blx", "look_for_approx_blx"), ("look-for-bl+", "look_for_bl_plus"),
    ("look-for-blx", "look_for_blx"), ("look-for-diff", "look_for_diff"),
    ("look-for-new-block", "look_for_new_block"), ("mean", "mean"),
    ("misfortune", "misfortune"), ("multiple", "multiple"),
    ("propagate-success", "propagate_success"), ("randlist", "randlist"),
    ("ratio", "ratio"), ("read-brick", "read_brick"), ("read-target", "read_target"),
    ("remove-dd", "remove_dd"), ("replace-target", "replace_target"),
    ("repump", "repump"), ("round", "round_"), ("sim", "sim"), ("sup", "sup"),
    ("temperature", "temperature"),
    ("test-if-possible-and-desirable", "test_if_possible_and_desirable"),
    ("update-success", "update_success"))}

# The functions that take no World (the pure helpers) are called without it.
_PURE = {"diff300", "digits_in_common", "mean", "misfortune", "multiple", "remove_dd", "sup"}


def _evaluable(py_name):
    """The function lisp_eval calls for a form (lisp-name ...).  It looks the
    function up in this module when called, so that a hook installed on the
    module (trace.py and harness.py, as sb-int:encapsulate does in the
    oracle) is seen."""
    if py_name in _PURE:
        return lambda world, *args: globals()[py_name](*args)
    return lambda world, *args: globals()[py_name](world, *args)


world_module.EVAL_WORLD_FUNCTIONS.update(
    (symbol, _evaluable(py_name)) for symbol, py_name in LISP_FUNCTIONS.items())
