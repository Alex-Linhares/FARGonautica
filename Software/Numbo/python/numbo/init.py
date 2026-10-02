"""Initialization of the Pnet and parameters, and the periodic refresh.

Port of lisp/src/init.lisp: its DEFVARs (load_init, run when a World is loaded),
init-chiffre, quick, reactivate-cyto and refresh-everything.

The codelets and Pnet functions are called through their modules
(codelets.repump, pnet_functions.spread_activation_in_pnet, ...), never
imported by name, so that the trace and iteration-cap hooks (trace.py,
harness.py), which replace module functions as sb-int:encapsulate replaces
the Lisp functions, see these calls too.
"""

import os

from numbo import codelets, coderack, franz, pnet_functions
from numbo.flavors import send
from numbo.franz import intern as _S
from numbo.world import fresh_line, lisp_eval

_CODERACK = _S("*CODERACK*")
_CURRENT_TARGET = _S("*CURRENT-TARGET*")
_NAME_COUNTER = _S("*NAME-COUNTER*")
_GRAPHICS = _S("%GRAPHICS%")
_VERBOSE = _S("%VERBOSE%")
_TARGET_PLUS = _S("%TARGET-PLUS%")
_BRICK_PLUS = _S("%BRICK-PLUS%")
_DTARGET_PLUS = _S("%DTARGET-PLUS%")
_FOURTH_URGENCY = _S("%FOURTH-URGENCY%")
_CYTO_TARGET = _S("CYTO-TARGET")
_CYTO_BRICKS = [_S(f"CYTO-BRICK{i}") for i in range(1, 6)]

# refresh-everything's '(look-for-new-block): one quoted constant.
_LOOK_FOR_NEW_BLOCK = [_S("LOOK-FOR-NEW-BLOCK")]

# init.lisp's DEFVARs with a value, in source order.  (defvar *print-array*),
# (defvar *coderack*) and (defvar *current-target*) give no value.
DEFVARS = [
    ("%INITIAL-ACTIVATION%", 0.0), ("%MIN-ACTIVATION-TO-BE-ADDED%", 24.0),
    ("%MAX-ACTIVATION-TO-BE-TRANSMITTED%", 35.0), ("%K%", 0.001), ("%LENGTH%", 1000),
    ("%TARGET-ACTIVATION%", 180), ("%BRICK-ACTIVATION%", 60),
    ("%DTARGET-ACTIVATION%", 100), ("%BLOCK-ACTIVATION%", 30), ("%TARGET-PLUS%", 75),
    ("%BRICK-PLUS%", 25), ("%DTARGET-PLUS%", 50), ("%NODE-MINUS%", -25),
    ("%SIMILAR%", 50), ("%OPERATION%", 200), ("%INSTANCE%", 200), ("%VERBOSE%", None),
    ("%GRAPHICS%", None), ("%FIRST-DECAY-RATE%", 0.50), ("%SECOND-DECAY-RATE%", 0.50),
    ("%THIRD-DECAY-RATE%", 0.70), ("%FOURTH-DECAY-RATE%", 0.90),
    ("%FIFTH-DECAY-RATE%", 0.90), ("%SIXTH-DECAY-RATE%", 0.0), ("%UPPER-THRESHOLD%", 90),
    ("%FIRST-THRESHOLD%", 30), ("%UPPER-URGENCY%", 600), ("%FIRST-URGENCY%", 300),
    ("%SECOND-URGENCY%", 70), ("%THIRD-URGENCY%", 7), ("%FOURTH-URGENCY%", 4),
    ("%FIFTH-URGENCY%", 1), ("%TEMPERATURE-THRESHOLD%", 80), ("*NAME-COUNTER*", 1),
]

# The SETQs of init-chiffre, in order, up to (initialize-pnet-2), except
# %verbose% and %graphics%, which come in between.
_INIT_CHIFFRE_PARAMETERS = [
    ("%INITIAL-ACTIVATION%", 0.0), ("%MIN-ACTIVATION-TO-BE-ADDED%", 24.0),
    ("%MAX-ACTIVATION-TO-BE-TRANSMITTED%", 35.0), ("%K%", 0.001), ("%LENGTH%", 1000),
    ("%TARGET-ACTIVATION%", 170), ("%BRICK-ACTIVATION%", 60),
    ("%DTARGET-ACTIVATION%", 130), ("%BLOCK-ACTIVATION%", 30), ("%TARGET-PLUS%", 75),
    ("%BRICK-PLUS%", 40), ("%DTARGET-PLUS%", 30), ("%NODE-MINUS%", -25),
    ("%SIMILAR%", 50), ("%OPERATION%", 200), ("%INSTANCE%", 200),
]
_INIT_CHIFFRE_DECAY = [
    ("%FIRST-DECAY-RATE%", 0.50), ("%SECOND-DECAY-RATE%", 0.50),
    ("%THIRD-DECAY-RATE%", 0.70), ("%FOURTH-DECAY-RATE%", 0.90),
    ("%FIFTH-DECAY-RATE%", 0.90), ("%SIXTH-DECAY-RATE%", 0.0), ("%UPPER-THRESHOLD%", 60),
    ("%FIRST-THRESHOLD%", 30), ("%TEMPERATURE-THRESHOLD%", 200),
]
_INIT_CHIFFRE_URGENCIES = [
    ("%UPPER-URGENCY%", 600), ("%FIRST-URGENCY%", 300), ("%SECOND-URGENCY%", 150),
    ("%THIRD-URGENCY%", 7), ("%FOURTH-URGENCY%", 4), ("%FIFTH-URGENCY%", 1),
]


class GraphicsNotPorted(NotImplementedError):
    """%graphics% is on (WINDOW_GFX set): pnet-graphics.lisp is not ported."""


def graphics_not_ported(what):
    raise GraphicsNotPorted(f"{what}: the Pnet graphics (pnet-graphics.lisp) are not "
                            f"ported; unset WINDOW_GFX")


def load_init(world):
    """init.lisp's top-level DEFVARs: each gives its value to a variable that
    has none yet."""
    for name, value in DEFVARS:
        if _S(name) not in world:
            world[_S(name)] = value


def _setq_all(world, pairs):
    for name, value in pairs:
        world[_S(name)] = value


def init_chiffre(world):
    """init.lisp: init-chiffre.  Set the parameters, initialize the Pnet
    (initialize-pnet-2), make the coderack and reset the name counter."""
    # (setq *print-array* nil) sets a CL printer variable: nothing to port.
    _setq_all(world, _INIT_CHIFFRE_PARAMETERS)
    world[_VERBOSE] = None
    if franz.string_length(os.environ.get("WINDOW_GFX", "")) > 0:
        world[_GRAPHICS] = True
    else:
        world[_GRAPHICS] = None
    if world[_GRAPHICS]:
        fresh_line(world.out)
        world.out.write("Graphics is ON.\n")
    else:
        fresh_line(world.out)
        world.out.write("Graphics is OFF.\n")
    _setq_all(world, _INIT_CHIFFRE_DECAY)
    pnet_functions.initialize_pnet_2(world)
    # (compile-flavor-methods pnode) is a no-op (flavors-compat.lisp).
    _setq_all(world, _INIT_CHIFFRE_URGENCIES)
    codelets.create_coderack(world)
    world[_NAME_COUNTER] = 1
    return 1


def quick(world):
    """init.lisp: quick.  Choose a codelet, print it and evaluate it (a
    debugging aid; config does not call it)."""
    r = coderack.cr_choose(world, world[_CODERACK])
    # (print r): newline, r, space.  CL prints r with prin1 (strings in
    # quotes); princ_to_string is close enough for this debugging aid.
    world.out.write("\n" + franz.princ_to_string(r) + " ")
    world.out.write("\n")
    return lisp_eval(world, r)


def _plinks_car(world, cyto_node):
    plinks = send(world, cyto_node, "plinks")
    return plinks[0] if plinks else None


def reactivate_cyto(world):
    """init.lisp: reactivate-cyto.  Add %target-plus% to the target's pnode,
    set each brick's pnode to %brick-plus%, add %dtarget-plus% to the current
    target's pnode, and repump."""
    # 1987: a brick whose link-to-pnet codelet has not run yet has no plinks,
    # and (send (eval nil) ...) is an error: the reactivate-cyto race
    # (PORTING_NOTES.md, item 10).
    pnode = _plinks_car(world, world[_CYTO_TARGET])
    send(world, lisp_eval(world, pnode), "add-activation", world[_TARGET_PLUS])
    for brick in _CYTO_BRICKS:
        pnode = _plinks_car(world, world[brick])
        send(world, lisp_eval(world, pnode), "set-activation", world[_BRICK_PLUS])
    pnode = _plinks_car(world, lisp_eval(world, send(world, world[_CURRENT_TARGET], "name")))
    if pnode:
        send(world, lisp_eval(world, pnode), "add-activation", world[_DTARGET_PLUS])
    return codelets.repump(world)


def refresh_everything(world):
    """init.lisp: refresh-everything.  Post look-for-new-block, decrease the
    interest of the blocks, check the temperature, spread activation in the
    Pnet, and post the codelets of the active pnodes."""
    # Its LET variable nn only names the graphics' window dump.
    world.cr_hang(world[_CODERACK], _LOOK_FOR_NEW_BLOCK, world[_FOURTH_URGENCY])
    codelets.decrease_interest(world)
    codelets.check_temperature(world)
    pnet_functions.spread_activation_in_pnet(world)
    if world[_GRAPHICS]:
        graphics_not_ported("refresh-everything")
    return pnet_functions.populate_coderack(world)
