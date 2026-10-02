"""The top level: config, the set-up phase and the main loop.

Port of lisp/src/start.lisp: (defvar *iteration* 0) (load_start) and config.

config's free SETQs, x (the loop counter), retry and rescod, are World
globals, as in the Lisp (the trace reads x).  Its arguments t1, b1 ... b5
are locals.  (eval form) of a chosen codelet is eval_form: the codelet is
called on its evaluated arguments (world.lisp_eval).

Every function config calls is called through its module (franz.mod,
coderack.cr_choose, codelets.temperature, ...), so that the hooks of
trace.py and harness.py, which replace module functions as
sb-int:encapsulate replaces Lisp functions, see these calls: the oracle
starts an iteration at its first (mod x 40) or cr-empty-coderack call.
"""

from numbo import codelets, coderack, cyto_def, franz, init, pnet_functions
from numbo.flavors import send
from numbo.franz import intern as _S
from numbo.world import lisp_eval

_ITERATION = _S("*ITERATION*")
_CYTOPLASM = _S("*CYTOPLASM*")
_CODERACK = _S("*CODERACK*")
_PROBLEM_SOLVED = _S("*PROBLEM-SOLVED*")
_GRAPHICS = _S("%GRAPHICS%")
_FOURTH_URGENCY = _S("%FOURTH-URGENCY%")
_TEMPERATURE_THRESHOLD = _S("%TEMPERATURE-THRESHOLD%")
_RESULTX = _S("%RESULTX%")
_RESULT_PLUS = _S("%RESULT+%")
_OPERAND = _S("%OPERAND%")
_X = _S("X")
_RETRY = _S("RETRY")
_RESCOD = _S("RESCOD")
_CYTO_TARGET = _S("CYTO-TARGET")

# config's quoted constants: '(look-for-new-block), and the pseudo-instances
# of the link-type pnodes.
_LOOK_FOR_NEW_BLOCK = [_S("LOOK-FOR-NEW-BLOCK")]
_PSEUDO_INSTANCES = [
    (_S("RESULT+"), [["5g", "result+"]]),
    (_S("RESULTX"), [["5g", "resultx"]]),
    (_S("OPERAND"), [["5g", "operand"]]),
    (_S("SIMILAR"), [["5g", "similar"]]),
    (_S("OPERATION"), [["6g", "operation"]]),
    (_S("INSTANCE"), [["6g", "instance"]]),
    (_S("NODE-ADD"), [["6g", "node-add"]]),
    (_S("NODE-SUBTRACT"), [["6g", "node-subtract"]]),
    (_S("NODE-MULTIPLY"), [["6g", "node-multiply"]]),
]


def _eqn(x, y):
    """=: numbers only (anything else is a Lisp error)."""
    return cyto_def._number(x) == cyto_def._number(y)


def load_start(world):
    """start.lisp's (defvar *iteration* 0)."""
    if _ITERATION not in world:
        world[_ITERATION] = 0


def eval_form(world, form):
    """config's (eval form) of a codelet form from the coderack (nil is nil)."""
    return lisp_eval(world, form)


def _choose_and_eval(world):
    """(eval (cr-choose *coderack*))"""
    return eval_form(world, coderack.cr_choose(world, world[_CODERACK]))


def config(world, t1, b1, b2, b3, b4, b5):
    """start.lisp: config.  Set up the Pnet and the cytoplasm for target T1
    and bricks B1 ... B5, read the target and the bricks (13 codelets), then
    run the main loop until the problem is solved (print the decomposition)
    or the coderack is empty after the last retry."""
    out = world.out
    p = franz.princ_to_string
    pnet_functions.initialize_pnet(world)
    for holder, instances in _PSEUDO_INSTANCES:
        send(world, world[holder], "set-instances", instances)
    cyto_def.init_cytoplasm(world, t1, b1, b2, b3, b4, b5)
    out.write("Le jeu des chiffres")
    out.write("\n")
    t1 = send(world, world[_CYTOPLASM], "target")
    out.write("Initial configuration :")
    out.write("\n")
    out.write(f"   Target : {p(t1)}")
    out.write("\n")
    b1 = send(world, world[_CYTOPLASM], "brick1")
    b2 = send(world, world[_CYTOPLASM], "brick2")
    b3 = send(world, world[_CYTOPLASM], "brick3")
    b4 = send(world, world[_CYTOPLASM], "brick4")
    b5 = send(world, world[_CYTOPLASM], "brick5")
    out.write(f" Bricks : {p(b1)} {p(b2)} {p(b3)} {p(b4)} {p(b5)}")
    out.write("\n")
    world[_RETRY] = True
    if world[_GRAPHICS]:
        init.graphics_not_ported("config")
    # Initialization of the target and start of the outer loop.
    world[_PROBLEM_SOLVED] = 0
    coderack.cr_empty_coderack(world, world[_CODERACK])
    codelets.read_target(world)
    _choose_and_eval(world)
    _choose_and_eval(world)
    _choose_and_eval(world)
    world[_RESULTX] = 200
    world[_RESULT_PLUS] = 100
    world[_OPERAND] = 50
    codelets.repump(world)
    # Initialization of the bricks and first processing.
    for i in range(1, 6):
        codelets.read_brick(world, i)
        _choose_and_eval(world)
        _choose_and_eval(world)
    # "I start at x = 11 to have an early spreading of activation in the
    # pnet (after 9 iterations)."
    world[_X] = 11
    y = 0
    while True:
        if _eqn(1, world[_PROBLEM_SOLVED]):
            out.write("Done : ")
            return codelets.decompose(world, _CYTO_TARGET)
        world[_ITERATION] = y
        world[_X] = world[_X] + 1
        if world[_X] > 400 and codelets.temperature(world) > world[_TEMPERATURE_THRESHOLD]:
            coderack.cr_empty_coderack(world, world[_CODERACK])
            world[_X] = 39
        elif 0 == franz.mod(world[_X], 40):
            init.refresh_everything(world)
            init.reactivate_cyto(world)
        elif 0 == franz.mod(world[_X], 20):
            init.refresh_everything(world)
        elif 0 == franz.mod(world[_X], 5):
            world.cr_hang(world[_CODERACK], _LOOK_FOR_NEW_BLOCK, world[_FOURTH_URGENCY])
            codelets.decrease_interest(world)
            codelets.check_temperature(world)
        else:
            if coderack.cr_empty_p(world, world[_CODERACK]):
                world[_RESCOD] = None
            else:
                world[_RESCOD] = coderack.cr_choose(world, world[_CODERACK])
            # "If there is no more codelets, a last activation of the pnet
            # is tried."
            if world[_RESCOD] is None:
                if world[_RETRY]:
                    for _ in range(4):
                        world.cr_hang(world[_CODERACK], _LOOK_FOR_NEW_BLOCK,
                                      world[_FOURTH_URGENCY])
                    pnet_functions.spread_activation_in_pnet(world)
                    if world[_GRAPHICS]:
                        init.graphics_not_ported("config")
                    world[_RETRY] = None
                    pnet_functions.populate_coderack(world)
                    init.reactivate_cyto(world)
                else:
                    return None
            eval_form(world, world[_RESCOD])
        y = y + 1
