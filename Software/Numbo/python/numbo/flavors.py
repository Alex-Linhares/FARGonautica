"""send: a message to a flavor instance.

Port of `send` in lisp/src/flavors-compat.lisp.  The pnode methods are in
pnet_functions (PNODE_METHODS) and the cytoplasm and cyto-node methods in
cyto_def (CYTOPLASM_METHODS, CYTO_NODE_METHODS).  Every flavor of the source
has settable, inittable and gettable instance variables, so (send x :ivar)
reads the ivar and (send x :set-ivar v) sets it.  Anything else, and in
particular a message to nil, is an error, as in Lisp: the 1987 code relies on
it nowhere, but the errors it can hit (PORTING_NOTES.md, item 10:
reactivate-cyto) must stay errors.

Most of the Python calls a method directly; codelets.py sends where the
receiver may be nil, or where the message is computed (read-brick's
:brick1 ... :brick5).
"""

from numbo import cyto_def, pnet_functions
from numbo.cyto_def import Cytoplasm, CytoContext, CytoCurrentTarget, CytoNode
from numbo.franz import Symbol, intern
from numbo.pnet_def import SLOTS, Pnode


class SendError(Exception):
    """flavors-compat.lisp: the error send signals for an unhandled message."""


# flavor class -> (methods by message keyword, the ivars' Lisp names)
_FLAVORS = {
    Pnode: (pnet_functions.PNODE_METHODS, tuple(s.replace("_", "-") for s in SLOTS)),
    Cytoplasm: (cyto_def.CYTOPLASM_METHODS, Cytoplasm.IVARS),
    CytoNode: (cyto_def.CYTO_NODE_METHODS, CytoNode.IVARS),
    CytoCurrentTarget: ({}, CytoCurrentTarget.IVARS),
    CytoContext: ({}, CytoContext.IVARS),
}


def _message(message):
    """A message keyword: a keyword Symbol, or its name as a string
    ("set-status" or ":set-status")."""
    if isinstance(message, Symbol):
        return message
    return intern(message.lstrip(":").upper(), "KEYWORD")


def send(world, object, message, *args):
    """flavors-compat.lisp: send.  Call OBJECT's method for MESSAGE, or read
    (:ivar) or set (:set-ivar value) one of its instance variables."""
    key = _message(message)
    flavor = _FLAVORS.get(type(object))
    if flavor is not None:
        methods, ivars = flavor
        method = methods.get(key)
        if method is not None:
            return method(world, object, *args)
        name = key.name.lower()
        if name in ivars and not args:
            return getattr(object, name.replace("-", "_"))
        if name.startswith("set-") and name[4:] in ivars and len(args) == 1:
            setattr(object, name[4:].replace("-", "_"), args[0])
            return args[0]
    receiver = "NIL" if object is None else repr(object)
    raise SendError(f"SEND: {receiver} does not handle the message :{key.name}")
