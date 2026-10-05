"""group-graphics.ss: drawing groups in the Workspace window (the engine's part).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from group-graphics.ss, with
racket/engine/group-graphics.rktl as a worked translation.

The model calls group-graphics (workspace.py, groups.py, when
%workspace-graphics% is on, and group-builder's consolidation of sameness groups
ungated), make-group-pexp (images.py, groups.py) and draw-group-grope (groups.py).
Everything here only builds SGL expressions (general_graphics.py) and sends
messages to *workspace-window* (setup.g_workspace_window) and the group; it
draws nothing itself, never draws a random number and never imports tkinter.
The arrow signs of the zigzag lines are chez.add and chez.sub (Scheme's + and -).
"""
from __future__ import annotations

from fractions import Fraction as F

import metacat as _metacat
from metacat import chez, setup, slipnet, workspace
from metacat.chez import add, div, make_rectangular, mul, sub
from metacat.general_graphics import (arrowhead, dashed_box, dotted_box, outline_box,
                                      zigzag_line_points)
from metacat.objects import tell
from metacat.utilities import exists_p, second, third

p_big_group_arrowhead_length = F(1, 100)
p_small_group_arrowhead_length = F(3, 500)
p_group_arrowhead_angle = 60
p_group_grope_zigzag_length = F(1, 50)
p_group_grope_num_of_flashes = 2


def group_dashed_line_density(letter_span):
    """group-graphics.ss: group-dashed-line-density"""
    return chez.max_(F(1, 4), sub(F(49, 100), mul(letter_span, F(2, 50))))


def group_graphics(op, group):
    """group-graphics.ss: group-graphics"""
    proposal_level = tell(group, "get-proposal-level")
    tell(setup.g_workspace_window, "caching-on")
    if op == "flash":
        if tell(group, "drawn?") is not False:
            tell(setup.g_workspace_window, "flash", tell(group, "get-graphics-pexp"))
        else:
            drawn_group = tell(group, "get-drawn-coincident-group")
            if (exists_p(drawn_group)
                    and proposal_level == tell(drawn_group, "get-proposal-level")):
                tell(setup.g_workspace_window, "flash", tell(drawn_group, "get-graphics-pexp"))
    elif op == "set-pexp-and-draw":
        tell(group, "set-graphics-pexp",
             _metacat.group_graphics.make_group_pexp(group, proposal_level))
        drawn_group = tell(group, "get-drawn-coincident-group")
        if not exists_p(drawn_group):
            tell(setup.g_workspace_window, "draw-group", group)
    elif op == "erase":
        if tell(group, "drawn?") is not False:
            tell(setup.g_workspace_window, "erase-group", group)
            # Repair damage to any overlapping groups:
            for g in tell(group, "get-drawn-overlapping-groups"):
                tell(setup.g_workspace_window, "draw-group", g)
            pending_group = tell(group, "get-highest-level-coincident-group")
            if exists_p(pending_group):
                tell(setup.g_workspace_window, "draw-group", pending_group)
    elif op == "update-level":
        new_pexp = _metacat.group_graphics.make_group_pexp(group, proposal_level)
        if tell(group, "drawn?") is not False:
            tell(setup.g_workspace_window, "erase", tell(group, "get-graphics-pexp"))
            tell(group, "set-graphics-pexp", new_pexp)
            tell(setup.g_workspace_window, "draw-group", group)
        else:
            tell(group, "set-graphics-pexp", new_pexp)
            drawn_group = tell(group, "get-drawn-coincident-group")
            if not exists_p(drawn_group):
                tell(setup.g_workspace_window, "draw-group", group)
            elif proposal_level > tell(drawn_group, "get-proposal-level"):
                tell(setup.g_workspace_window, "erase-group", drawn_group)
                tell(setup.g_workspace_window, "draw-group", group)
    tell(setup.g_workspace_window, "flush")
    return "done"


def make_group_pexp(group, proposal_level):
    """group-graphics.ss: make-group-pexp"""
    gg = _metacat.general_graphics
    x1 = tell(group, "get-graphics-x1")
    y1 = tell(group, "get-graphics-y1")
    x2 = tell(group, "get-graphics-x2")
    y2 = tell(group, "get-graphics-y2")
    x_mid = div(add(x1, x2), 2)
    letter_span = tell(group, "get-letter-span")
    if proposal_level == workspace.p_proposed:
        rectangle_pexp = dotted_box(x1, y1, x2, y2, gg.p_dot_interval)
    elif proposal_level == workspace.p_evaluated:
        rectangle_pexp = dashed_box(x1, y1, x2, y2, group_dashed_line_density(letter_span),
                                    gg.p_dash_length)
    elif proposal_level == workspace.p_built:
        rectangle_pexp = outline_box(x1, y1, x2, y2)
    else:
        rectangle_pexp = None       # 1.2: a cond without else
    direction = tell(group, "get-direction")
    if exists_p(direction):
        return ["let-sgl", [],
                rectangle_pexp,
                arrowhead(x_mid, y2, 0 if direction is slipnet.plato_right else 180,
                          (p_small_group_arrowhead_length
                           if proposal_level < workspace.p_built or letter_span == 1
                           else p_big_group_arrowhead_length),
                          p_group_arrowhead_angle)]
    letcat_pexp = tell(group, "get-letcat-graphics-pexp")
    if exists_p(letcat_pexp) and proposal_level == workspace.p_built:
        return ["let-sgl", second(letcat_pexp), rectangle_pexp, third(letcat_pexp)]
    return rectangle_pexp


def draw_group_grope(group):
    """group-graphics.ss: draw-group-grope"""
    x1 = tell(group, "get-graphics-x1")
    y1 = tell(group, "get-graphics-y1")
    x2 = tell(group, "get-graphics-x2")
    y2 = tell(group, "get-graphics-y2")
    grope_pexp = make_group_grope_pexp(x1, y1, x2, y2)
    return tell(setup.g_workspace_window, "flash", grope_pexp)


def make_group_grope_pexp(x1, y1, x2, y2):
    """group-graphics.ss: make-group-grope-pexp"""
    mid_left = add(x1, mul(F(1, 4), sub(x2, x1)))
    mid_right = sub(x2, mul(F(1, 4), sub(x2, x1)))
    length = p_group_grope_zigzag_length
    return ["let-sgl", [],
            ["polyline"]
            + zigzag_line_points(make_rectangular(mid_left, y1), make_rectangular(x1, y1), length, sub)
            + zigzag_line_points(make_rectangular(x1, y1), make_rectangular(x1, y2), length, add)
            + zigzag_line_points(make_rectangular(x1, y2), make_rectangular(mid_left, y2), length, sub),
            ["polyline"]
            + zigzag_line_points(make_rectangular(mid_right, y1), make_rectangular(x2, y1), length, add)
            + zigzag_line_points(make_rectangular(x2, y1), make_rectangular(x2, y2), length, sub)
            + zigzag_line_points(make_rectangular(x2, y2), make_rectangular(mid_right, y2), length, add)]
