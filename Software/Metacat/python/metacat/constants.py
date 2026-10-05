"""constants.ss: the model's constants.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from constants.ss, with
racket/engine/constants.rktl as a worked translation.

Only the model's part is here: the translation-temperature threshold
distributions.  The graphics part (window sizes, colours, fonts, window titles
and icons) belongs to the GUI items; the colours and fonts the model reads are
in view_globals.py, #f until the views are attached, as in the Racket port.
The engine never imports tkinter.
"""
from __future__ import annotations

from metacat.objects import SchemeObject, delegate, message
from metacat.utilities import base_object, stochastic_pick


# ----------------------------------------------------------------------
# Probability distributions

class ProbabilityDistribution(SchemeObject):
    """constants.ss: make-probability-distribution (the closure)"""
    __slots__ = ("values", "distribution_frequency_values")

    def __init__(this, values, distribution_frequency_values):
        this.values = values
        this.distribution_frequency_values = distribution_frequency_values

    @message("object-type")
    def object_type(this, self):
        return "probability-distribution"

    @message("choose-value")
    def choose_value(this, self):
        return stochastic_pick(this.values, this.distribution_frequency_values)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_probability_distribution(values, distribution_frequency_values):
    """constants.ss: make-probability-distribution"""
    return ProbabilityDistribution(values, distribution_frequency_values)


p_very_low_translation_temperature_threshold_distribution = make_probability_distribution(
    [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    [5, 150, 5, 2, 1, 1, 1, 1, 1, 1])


p_low_translation_temperature_threshold_distribution = make_probability_distribution(
    [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    [2, 5, 150, 5, 2, 1, 1, 1, 1, 1])


p_medium_translation_temperature_threshold_distribution = make_probability_distribution(
    [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    [1, 2, 5, 150, 5, 2, 1, 1, 1, 1])


p_high_translation_temperature_threshold_distribution = make_probability_distribution(
    [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    [1, 1, 2, 5, 150, 5, 2, 1, 1, 1])


p_very_high_translation_temperature_threshold_distribution = make_probability_distribution(
    [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    [1, 1, 1, 2, 5, 150, 5, 2, 1, 1])
