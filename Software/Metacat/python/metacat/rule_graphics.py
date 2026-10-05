"""rule-graphics.ss: drawing rules in the Workspace window (the engine's part).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from rule-graphics.ss, with
racket/engine/rule-graphics.rktl as a worked translation.

The whole file: the model calls initialize-rule-graphics (rules.py, answers.py)
when %workspace-graphics% is on, and the Workspace window's update-rule-pexps
calls update-rule-pexps! when the window is resized.  They build SGL expressions
(general_graphics.py) from the sizes *workspace-window* (setup.g_workspace_window)
reports and send them to the rule; they draw nothing themselves, never draw a
random number and never import tkinter.  %rule-font% and =white= are read at
call time from view_globals.py.

Unlike the Racket port (whose pairs are immutable), update-rule-pexps! updates
the pexp in place as the original's set-car! does, so every holder of a shared
rule pexp sees the new one (anomalies_and_quirks.md, shared rule pexps).
"""
from __future__ import annotations

from fractions import Fraction as F

from metacat import chez, setup, view_globals
from metacat.chez import add, mul, sub
from metacat.general_graphics import dashed_box, outline_box
from metacat.objects import tell
from metacat.utilities import descending_index_list, first, maximum, second

import metacat as _metacat


def _rule_layout(rule_type, clauses):
    """The let* shared word for word by initialize-rule-graphics and
    make-new-rule-pexp, in its order (each step asks *workspace-window*)."""
    window = setup.g_workspace_window
    rule_font = view_globals.p_rule_font
    num_of_clauses = len(clauses)
    # chez: map's order of application (the window answers each width)
    clause_widths = chez.map_(
        lambda clause: tell(setup.g_workspace_window, "get-string-width", clause,
                            view_globals.p_rule_font),
        clauses)
    max_clause_width = maximum(clause_widths)
    text_height = tell(window, "get-character-height", chez.String(" "), rule_font)
    x_extra = tell(window, "get-string-width", chez.String("xx"), rule_font)
    y_extra = mul(F(3, 5), text_height)
    border_width = mul(F(3, 20), text_height)
    height_minus_border = add(mul(num_of_clauses, text_height), y_extra)
    total_height = add(height_minus_border, mul(2, border_width))
    center_coord = tell(window, "get-rule-coord", rule_type(), total_height)
    x1_border = sub(first(center_coord), mul(F(1, 2), max_clause_width), x_extra)
    x2_border = add(first(center_coord), mul(F(1, 2), max_clause_width), x_extra)
    y1_border = sub(second(center_coord), mul(F(1, 2), height_minus_border))
    y2_border = add(second(center_coord), mul(F(1, 2), height_minus_border))
    shading = tell(window, "get-width-per-pixel")
    x1 = sub(x1_border, border_width, shading)
    y1 = sub(y1_border, border_width)
    x2 = add(x2_border, border_width)
    y2 = add(y2_border, border_width, shading)
    text_x0 = sub(first(center_coord), mul(F(1, 2), max_clause_width))
    text_y0 = add(sub(second(center_coord), mul(F(1, 2), height_minus_border)), y_extra)
    clause_pexps = chez.map_(
        lambda i, clause: ["text", [text_x0, add(text_y0, mul(i, text_height))], clause],
        descending_index_list(num_of_clauses), clauses)
    rule_pexp = (["let-sgl", [["font", rule_font]],
                  ["let-sgl", [["foreground-color", view_globals.c_white]],
                   ["filled-rectangle", [x1, y1], [x2, y2]]]]
                 + clause_pexps
                 + [["let-sgl", [["line-width", 2]],
                     ["polyline", [x1, y1], [x1, y2], [x2, y2]]],
                    ["polyline", [x2, y2], [x2, y1], [x1, y1]],
                    outline_box(x1_border, y1_border, x2_border, y2_border)])
    return rule_font, center_coord, x1, y1, x2, y2, clause_pexps, rule_pexp


def initialize_rule_graphics(rule):
    """rule-graphics.ss: initialize-rule-graphics"""
    gg = _metacat.general_graphics
    clauses = tell(rule, "get-english-transcription")
    rule_font, center_coord, x1, y1, x2, y2, clause_pexps, rule_pexp = _rule_layout(
        lambda: tell(rule, "get-rule-type"), clauses)
    tagged_rule_pexp = ["rule", tell(rule, "get-rule-type"), clauses, rule_pexp]
    clamped_rule_pexp = (["let-sgl", [["font", rule_font]]]
                         + clause_pexps
                         + [dashed_box(x1, y1, x2, y2, gg.p_dash_density, gg.p_dash_length)])
    tell(rule, "set-graphics-pexp", tagged_rule_pexp)
    return tell(rule, "set-auxiliary-rule-graphics-info",
                clamped_rule_pexp, center_coord, sub(y2, y1))


def update_rule_pexps_bang(pexp):
    """rule-graphics.ss: update-rule-pexps!  In place, as the original's set-car!;
    port: returns pexp itself (the original's value was unspecified), so that a
    caller written after the Racket port, which stores the value, gets the same
    pexp."""
    # record-case binds the formals with car/cdr: extra elements are ignored
    tag = pexp[0]
    if tag == "rule":
        rule_type, clauses = pexp[1], pexp[2]
        pexp[3] = make_new_rule_pexp(rule_type, clauses)        # (set-car! (cdddr pexp) ...)
    elif tag == "let-sgl":
        for p in pexp[2:]:
            update_rule_pexps_bang(p)
    elif tag == "erase":
        update_rule_pexps_bang(pexp[2])
    return pexp


def make_new_rule_pexp(rule_type, clauses):
    """rule-graphics.ss: make-new-rule-pexp"""
    return _rule_layout(lambda: rule_type, clauses)[7]
