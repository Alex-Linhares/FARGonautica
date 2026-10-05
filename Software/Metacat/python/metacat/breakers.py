"""breakers.ss: the breaker codelet.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from breakers.ss, with
racket/engine/breakers.rktl as a worked translation.

The codelet procedure is given to its codelet type by load()
(define-codelet-procedure* needs the type, which coderack.load makes).
break-bond, break-group and break-bridge are read through their modules at call
time, so wrappers set on those modules reach the breaker.

Evaluation order (audited against Chez's): the draws are the three
stochastic-if* coins (each drawn before its probability, as
sugar.stochastic_if_star does) and random-pick, which sit in a body and a let*.
The two-binding let of p1 and p2 only reads (get-weakness and
temp-adjusted-probability), so Chez's order of evaluating it is not observable.
The engine never imports tkinter.
"""
from __future__ import annotations

from metacat import chez, sugar
from metacat import bonds, bridges, formulas, groups, setup, workspace
from metacat.objects import tell
from metacat.sugar import say
from metacat.utilities import (bond_p, exists_p, filter_out, hundred_minus, percent,
                               random_pick, rule_p)


def breaker():
    """breakers.ss: breaker (the codelet procedure)"""
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < percent(hundred_minus(setup.g_temperature)):
        say("Temperature is too low. Fizzling.")
        sugar.fizzle()
    breakable_structures = filter_out(rule_p, tell(workspace.g_workspace, "get-structures"))
    if len(breakable_structures) == 0:
        say("Couldn't choose structure. Fizzling.")
        sugar.fizzle()
    # the let*, in order: the structure (a draw), then its enclosing group
    structure = random_pick(breakable_structures)
    enclosing_group = tell(structure, "get-enclosing-group")
    if bond_p(structure) and exists_p(enclosing_group):
        # a two-binding let; both bindings only read
        p1 = formulas.temp_adjusted_probability(percent(tell(structure, "get-weakness")))
        p2 = formulas.temp_adjusted_probability(percent(tell(enclosing_group, "get-weakness")))
        # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
        coin_flip = chez.random(1.0)
        if coin_flip < chez.mul(p1, p2):
            groups.break_group(enclosing_group)
            bonds.break_bond(structure)
    else:
        # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
        coin_flip = chez.random(1.0)
        if coin_flip < formulas.temp_adjusted_probability(
                percent(tell(structure, "get-weakness"))):
            object_type = tell(structure, "object-type")
            if object_type == "bond":
                bonds.break_bond(structure)
            elif object_type == "group":
                groups.break_group(structure)
            elif object_type == "bridge":
                bridges.break_bridge(structure)
    return "done"


def load():
    """breakers.ss: the define-codelet-procedure* form."""
    sugar.define_codelet_procedure_star("breaker", breaker)
