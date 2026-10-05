"""workspace-structure-formulas.ss: probabilities and supports for groups and descriptions.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from
workspace-structure-formulas.ss, with racket/engine/workspace-structure-formulas.rktl
as a worked translation.

Plain procedures; arithmetic is exact unless a flonum enters (expt 0.5 ...).
plato-length is slipnet.plato_length (made by slipnet.load()) and
temp-adjusted-probability is formulas.temp_adjusted_probability, both read at
call time.  The engine never imports tkinter.
"""
from __future__ import annotations

from metacat import chez
from metacat import formulas, slipnet
from metacat.objects import tell
from metacat.utilities import average, count, cube, hundred_minus, percent, round_, times_100


def length_description_probability(group):
    """workspace-structure-formulas.ss: length-description-probability"""
    group_length = tell(group, "get-group-length")
    if group_length > 5:
        return 0
    if group_length == 1:
        return 1
    return formulas.temp_adjusted_probability(
        chez.expt(0.5, chez.mul(cube(group_length),
                                percent(hundred_minus(tell(slipnet.plato_length,
                                                           "get-activation"))))))


def single_letter_group_probability(group):
    """workspace-structure-formulas.ss: single-letter-group-probability"""
    n = tell(group, "get-num-of-local-supporting-groups")
    if n == 1:
        exponent = 4
    elif n == 2:
        exponent = 2
    else:
        exponent = 1
    return formulas.temp_adjusted_probability(
        chez.expt(chez.mul(percent(tell(group, "get-local-support")),
                           percent(tell(slipnet.plato_length, "get-activation"))),
                  exponent))


def descriptor_support(descriptor, string):
    """workspace-structure-formulas.ss: descriptor-support"""
    groups = tell(string, "get-groups")
    num_of_groups = len(groups)
    num_of_described_groups = count(
        lambda group: tell(group, "descriptor-present?", descriptor), groups)
    if num_of_groups == 0:
        return 0
    return times_100(chez.div(num_of_described_groups, num_of_groups))


def description_type_support(description_type, string):
    """workspace-structure-formulas.ss: description-type-support"""
    objects = tell(string, "get-objects")
    num_of_objects = len(objects)
    num_of_described_objects = count(
        lambda object_: tell(object_, "description-type-present?", description_type), objects)
    # 1.2: a string without objects divides by zero, as in the original
    local_support = times_100(chez.div(num_of_described_objects, num_of_objects))
    return round_(average(local_support, tell(description_type, "get-activation")))
