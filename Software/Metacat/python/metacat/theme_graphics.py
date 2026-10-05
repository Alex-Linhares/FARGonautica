"""theme-graphics.ss: the Themespace panels (the engine's part).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from theme-graphics.ss, with
racket/engine/theme-graphics.rktl as a worked translation.

Only relation-name is translated here, verbatim, because its result reaches the
model's output: trace.ss's print-pattern prints theme patterns with it.  The rest
of theme-graphics.ss (the theme panels and windows, dimension-name,
abbreviated-dimension-name and the pexp builders) is translated in the panels
item, in this module, as group_graphics.py is for group-graphics.ss.
relation-name is pure: it reads the slipnet's relation nodes, draws no random
number and never imports tkinter.  Its strings are chez.Strings, as the other
modules' strings for printing are.
"""
from __future__ import annotations

from metacat import chez, slipnet

String = chez.String


def relation_name(relation):
    """theme-graphics.ss: relation-name"""
    if relation is False:
        return String("diff")
    if relation is slipnet.plato_identity:
        return String("iden")
    if relation is slipnet.plato_opposite:
        return String("opp")
    if relation is slipnet.plato_successor:
        return String("succ")
    if relation is slipnet.plato_predecessor:
        return String("pred")
    return False
