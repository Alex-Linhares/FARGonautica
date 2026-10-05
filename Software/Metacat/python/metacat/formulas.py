"""formulas.ss: temperature-adjusted probabilities and values, and the temperature.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from formulas.ss, with
racket/engine/formulas.rktl as a worked translation.

Plain procedures.  *temperature* is setup.g_temperature (read and assigned
qualified); the workspace strings and *workspace* are workspace.g_...; the
threshold distributions are constants.p_..._distribution.  *temperature-clamped?*
has no definition in the original (run.ss's init-mcat creates it with set!;
porting-notes.md, item 06), so it is read through the package as
_metacat.run.g_temperature_clamped_p.  The flonum literals (0.0, 0.5, 1.0) make
these formulas inexact exactly where the original's are.  The engine never
imports tkinter.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez
from metacat import constants, setup, workspace
from metacat.chez import add, div, mul, sub
from metacat.objects import tell
from metacat.utilities import (hundred_minus, log10, one_minus, percent, round_, ten_minus,
                               truncate, weighted_average)


def temp_adjusted_probability(prob):
    """formulas.ss: temp-adjusted-probability"""
    # (= prob 0.0) is numeric: exact 0 equals 0.0
    if prob == 0.0:
        return 0.0
    if prob <= 0.5:
        # truncate is utilities.ss's (exact); (max 1.0 ...) makes it a flonum
        low_prob_factor = chez.max_(1.0, truncate(chez.abs_(log10(prob))))
        return chez.min_(0.5, add(prob, mul(percent(ten_minus(chez.sqrt(hundred_minus(
                                                setup.g_temperature)))),
                                            sub(chez.expt(10, one_minus(low_prob_factor)),
                                                prob))))
    if prob > 0.5:
        return chez.max_(0.5, one_minus(add(one_minus(prob),
                                            mul(percent(ten_minus(chez.sqrt(hundred_minus(
                                                setup.g_temperature)))),
                                                prob))))
    return None  # cond without else (a NaN)


def temp_adjusted_values(value_list):
    """formulas.ss: temp-adjusted-values"""
    exponent = add(div(hundred_minus(setup.g_temperature), 30), 0.5)
    # chez: map's order of application (the procedure is pure)
    return chez.map_(lambda value: round_(chez.expt(value, exponent)), value_list)


def current_translation_temperature_threshold_distribution():
    """formulas.ss: current-translation-temperature-threshold-distribution"""
    i_length = tell(workspace.g_initial_string, "get-length")
    m_length = tell(workspace.g_modified_string, "get-length")
    t_length = tell(workspace.g_target_string, "get-length")
    if i_length == 1 and m_length == 1 and t_length == 1:
        bond_density = 1.0
    else:
        bond_density = div(add(len(tell(workspace.g_initial_string, "get-bonds")),
                               len(tell(workspace.g_modified_string, "get-bonds")),
                               len(tell(workspace.g_target_string, "get-bonds"))),
                           add(chez.sub1(i_length), chez.sub1(m_length), chez.sub1(t_length)))
    # 1.2: the exact density is compared with the flonums exactly, so 1/5, 2/5 and 4/5
    # fall into the hotter class (anomalies: "Exact bond densities meet flonum thresholds")
    if bond_density >= 0.8:
        return constants.p_very_low_translation_temperature_threshold_distribution
    if bond_density >= 0.6:
        return constants.p_low_translation_temperature_threshold_distribution
    if bond_density >= 0.4:
        return constants.p_medium_translation_temperature_threshold_distribution
    if bond_density >= 0.2:
        return constants.p_high_translation_temperature_threshold_distribution
    return constants.p_very_high_translation_temperature_threshold_distribution


def update_temperature():
    """formulas.ss: update-temperature"""
    if _metacat.run.g_temperature_clamped_p is False:
        ws = workspace.g_workspace
        if ((setup.p_justify_mode is not False
             and tell(ws, "rule-possible?", "top") is not False
             and tell(ws, "supported-rule-exists?", "top") is not False
             and tell(ws, "rule-possible?", "bottom") is not False
             and tell(ws, "supported-rule-exists?", "bottom") is not False)
                or (setup.p_justify_mode is False
                    and tell(ws, "rule-possible?", "top") is not False
                    and tell(ws, "supported-rule-exists?", "top") is not False)):
            rule_factor = 0
        else:
            rule_factor = 100
        setup.g_temperature = round_(weighted_average(
            [tell(ws, "get-average-unhappiness"), rule_factor],
            [70, 30]))
    return None
