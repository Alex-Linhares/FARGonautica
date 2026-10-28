# Copyright (c) 2007-2017 Joseph Hager.
#
# Copycat is free software; you can redistribute it and/or modify
# it under the terms of version 2 of the GNU General Public License,
# as published by the Free Software Foundation.
#
# Copycat is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Copycat; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

"""Toolbox of utility functions."""

import math
import random

def weighted_average(weights, values):
    """Return the weighted arithmetic mean of arguments."""
    weight_sum = sum(weights)
    value_sum = sum(map(lambda a, b: a * b, weights, values))
    return int(value_sum / float(weight_sum))

def weighted_index(weights):
    """Probabilistically chooses one of the weights by value, returning its
    index."""
    total = sum(weights)
    if total <= 0:
        return random.randint(0, len(weights) - 1)
    value = random.randint(0, total - 1)
    new_total = 0
    index = 0
    for weight in weights:
        new_total += weight
        index += 1
        if new_total > value:
            return index - 1

def weighted_select(weights, items):
    """Return one of the items probabilistically by weight."""
    if items:
        return items[weighted_index(weights)]

def select_assoc(assoc_list):
    """Returns one of the items, chosen probabilistically.

    assoc_list is of the form: [(item, probability), (item, probability) ...]"""
    if assoc_list == []:
        return

    probability_sum = 0
    for item, probability in assoc_list:
        probability_sum += probability
    if probability_sum <= 0:
        return

    value = random.uniform(0, probability_sum)
    new_probability_sum = 0
    for item, probability in assoc_list:
        new_probability_sum += probability
        if new_probability_sum > value:
            return item

def flatten(sequence):
    """Flattens a sequence so that it has no nested structure."""
    # PY3-FIX: dict.values() is a view in Python 3, not a list; the workspace passes
    # such views here, and they used to pass through unflattened (see PORTING_NOTES.md).
    if isinstance(sequence, (list, tuple, type({}.values()))):
        return sum(map(flatten, sequence), [])
    else:
        return [sequence]

def unique(sequence):
    """The distinct items of sequence, first occurrence first.

    DETERMINISM-FIX: the engine used list(set(...)) to drop duplicates, and a set of
    workspace objects is ordered by their memory addresses, which differ from one process
    to the next; so the same seed gave different runs. This keeps the original order
    (see PORTING_NOTES.md)."""
    return list(dict.fromkeys(sequence))

def flip_coin(prob_of_true=.5):
    """Returns either True or False based on the probabity of true sent as an
    argument."""
    if prob_of_true >= 1:
        return True
    return select_assoc([[True, int(prob_of_true * 1000)],
                         [False, int((1 - prob_of_true) * 1000)]])

def average(*args):
    """Returns the arithmetic mean of its arguments."""
    if not args:
        # BUG-FIX: a correspondence proposed after flipping a string-spanning group can
        # have no distinguishing concept mappings; the urgency was then 0/0 and the run
        # died with ZeroDivisionError (see PORTING_NOTES.md).
        return 0
    return sum(args) / float(len(args))

def blur(number):
    """A normal distribution around number within square_root(number)."""
    return random.normalvariate(number, math.sqrt(number))
