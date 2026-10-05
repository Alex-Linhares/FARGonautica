"""bridge-graphics.ss: drawing bridges in the Workspace window (the engine's part).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from bridge-graphics.ss, with
racket/engine/bridge-graphics.rktl as a worked translation.

The whole file, verbatim: the model calls bridge-graphics, make-bridge-pexp,
draw-bridge-grope and new-bridge-label-number (bridges.py, answers.py) when
%workspace-graphics% is on.  Everything here only builds SGL expressions
(general_graphics.py) and sends messages to *workspace-window*
(setup.g_workspace_window), the bridge and its objects; it draws nothing itself,
never draws a random number and never imports tkinter.  %bridge-label-font% is
read at call time from view_globals.py.  The zigzag signs are chez.add and
chez.sub (Scheme's + and -).
"""
from __future__ import annotations

from fractions import Fraction as F

from metacat import chez, setup, view_globals, workspace
from metacat.chez import String, add, add1, div, magnitude, make_rectangular, mul, sub
from metacat.general_graphics import (circular_arc, dashed_circular_arc, dashed_elliptical_arc,
                                      dashed_line, dashed_line_points, dotted_circular_arc,
                                      dotted_elliptical_arc, dotted_line, dotted_line_points,
                                      elliptical_arc, centered_zigzag_line, zigzag_line_points)
from metacat.objects import tell, tell_all
from metacat.utilities import exists_p, letter_p, member_p, x_coord, y_coord
from metacat.workspace_objects import both_spanning_groups_p

import metacat as _metacat

p_arc_height_factor = F(15, 100)
p_bridge_dash_density = F(35, 100)
p_bridge_zigzag_length = F(1, 100)


def bridge_graphics(op, bridge):
    """bridge-graphics.ss: bridge-graphics"""
    proposal_level = tell(bridge, "get-proposal-level")
    tell(setup.g_workspace_window, "caching-on")
    if op == "flash":
        if tell(bridge, "drawn?") is not False:
            tell(setup.g_workspace_window, "flash", tell(bridge, "get-graphics-pexp"))
        else:
            drawn_bridge = tell(bridge, "get-drawn-coincident-bridge")
            if (exists_p(drawn_bridge)
                    and proposal_level == tell(drawn_bridge, "get-proposal-level")):
                tell(setup.g_workspace_window, "flash", tell(drawn_bridge, "get-graphics-pexp"))
    elif op == "set-pexp-and-draw":
        tell(bridge, "set-graphics-pexp", make_bridge_pexp(bridge, proposal_level))
        drawn_bridge = tell(bridge, "get-drawn-coincident-bridge")
        if not exists_p(drawn_bridge):
            tell(setup.g_workspace_window, "draw-bridge", bridge)
    elif op == "erase":
        if tell(bridge, "drawn?") is not False:
            tell(setup.g_workspace_window, "erase-bridge", bridge)
            # Repair damage to bridge's objects:
            # chez: a two-binding let whose bindings only ask the bridge
            if (proposal_level < workspace.p_built
                    and tell(bridge, "flipped-group1?") is not False):
                object1 = tell(bridge, "get-original-group1")
            else:
                object1 = tell(bridge, "get-object1")
            if (proposal_level < workspace.p_built
                    and tell(bridge, "flipped-group2?") is not False):
                object2 = tell(bridge, "get-original-group2")
            else:
                object2 = tell(bridge, "get-object2")
            tell(setup.g_workspace_window, "draw", tell(object1, "get-graphics-pexp"))
            if letter_p(object2):
                tell(setup.g_workspace_window, "draw", tell(object2, "get-graphics-pexp"))
            else:
                tell(setup.g_workspace_window, "draw-group", object2)
            pending_bridge = tell(bridge, "get-highest-level-coincident-bridge")
            if exists_p(pending_bridge):
                tell(setup.g_workspace_window, "draw-bridge", pending_bridge)
    elif op == "update-level":
        new_pexp = make_bridge_pexp(bridge, proposal_level)
        if tell(bridge, "drawn?") is not False:
            tell(setup.g_workspace_window, "erase", tell(bridge, "get-graphics-pexp"))
            tell(bridge, "set-graphics-pexp", new_pexp)
            tell(setup.g_workspace_window, "draw-bridge", bridge)
        else:
            tell(bridge, "set-graphics-pexp", new_pexp)
            drawn_bridge = tell(bridge, "get-drawn-coincident-bridge")
            if not exists_p(drawn_bridge):
                tell(setup.g_workspace_window, "draw-bridge", bridge)
            elif proposal_level > tell(drawn_bridge, "get-proposal-level"):
                tell(setup.g_workspace_window, "erase-bridge", drawn_bridge)
                tell(setup.g_workspace_window, "draw-bridge", bridge)
    tell(setup.g_workspace_window, "flush")
    return "done"


def make_bridge_pexp(bridge, proposal_level):
    """bridge-graphics.ss: make-bridge-pexp"""
    orientation = tell(bridge, "get-orientation")
    if orientation == "horizontal":
        return make_horizontal_bridge_pexp(bridge, proposal_level)
    if orientation == "vertical":
        return make_vertical_bridge_pexp(bridge, proposal_level)
    return None         # 1.2: a case without else


def make_horizontal_bridge_pexp(bridge, proposal_level):
    """bridge-graphics.ss: make-horizontal-bridge-pexp"""
    left = tell(bridge, "get-from-graphics-coord")
    right = tell(bridge, "get-to-graphics-coord")
    x_left = x_coord(left)
    y_left = y_coord(left)
    x_right = x_coord(right)
    y_right = y_coord(right)
    height = mul(p_arc_height_factor, sub(x_right, x_left))
    if tell(bridge, "group-spanning-bridge?") is not False:
        return make_spanning_horizontal_bridge_pexp(left, right, height, proposal_level)
    if letter_p(tell(bridge, "get-object1")) and letter_p(tell(bridge, "get-object2")):
        return elliptical_horizontal_bridge_arc(
            x_left, y_left, x_right, y_right, mul(F(2, 3), height), proposal_level)
    return elliptical_horizontal_bridge_arc(
        x_left, y_left, x_right, y_right, height, proposal_level)


def elliptical_horizontal_bridge_arc(x_left, y_left, x_right, y_right, arc_height,
                                     proposal_level):
    """bridge-graphics.ss: elliptical-horizontal-bridge-arc"""
    if proposal_level == workspace.p_proposed:
        return dotted_elliptical_arc(x_left, y_left, x_right, y_right, arc_height,
                                     _metacat.general_graphics.p_dot_interval)
    if proposal_level == workspace.p_evaluated:
        return dashed_elliptical_arc(x_left, y_left, x_right, y_right, arc_height)
    return elliptical_arc(x_left, y_left, x_right, y_right, arc_height)


def circular_horizontal_bridge_arc(x_left, y_left, x_right, y_right, arc_height,
                                   proposal_level):
    """bridge-graphics.ss: circular-horizontal-bridge-arc (not used)"""
    if proposal_level == workspace.p_proposed:
        return dotted_circular_arc(x_left, y_left, x_right, y_right, arc_height,
                                   _metacat.general_graphics.p_dot_interval)
    if proposal_level == workspace.p_evaluated:
        return dashed_circular_arc(x_left, y_left, x_right, y_right, arc_height)
    return circular_arc(x_left, y_left, x_right, y_right, arc_height)


def make_vertical_bridge_pexp(bridge, proposal_level):
    """bridge-graphics.ss: make-vertical-bridge-pexp"""
    gg = _metacat.general_graphics
    top = tell(bridge, "get-from-graphics-coord")
    bot = tell(bridge, "get-to-graphics-coord")
    x_top = x_coord(top)
    y_top = y_coord(top)
    x_bot = x_coord(bot)
    y_bot = y_coord(bot)
    if tell(bridge, "group-spanning-bridge?") is not False:
        return make_spanning_vertical_bridge_pexp(top, bot, proposal_level)
    if proposal_level == workspace.p_proposed:
        return dotted_line(x_top, y_top, x_bot, y_bot, gg.p_dot_interval)
    if proposal_level == workspace.p_evaluated:
        return dashed_line(x_top, y_top, x_bot, y_bot, p_bridge_dash_density, gg.p_dash_length)
    label_coord = sub(top, div(sub(top, bot), 3))
    x = x_coord(label_coord)
    y = y_coord(label_coord)
    label_num = tell(bridge, "get-bridge-label-number")
    return ["let-sgl", [],
            centered_zigzag_line(top, bot, p_bridge_zigzag_length, sub if x_top < x_bot else add),
            ["let-sgl", [["font", view_globals.p_bridge_label_font],
                         ["text-justification", "center"]],
             ["text", [x, y], String(chez.format_("~a", label_num))]]]


def make_spanning_horizontal_bridge_pexp(left, right, height, proposal_level):
    """bridge-graphics.ss: make-spanning-horizontal-bridge-pexp"""
    gg = _metacat.general_graphics
    y_left = y_coord(left)
    y_right = y_coord(right)
    delta = chez.abs_(sub(y_left, y_right))
    left_offset = make_rectangular(0, add(height, delta if y_left < y_right else 0))
    right_offset = make_rectangular(0, add(height, delta if y_right < y_left else 0))
    left_corner = add(left, left_offset)
    right_corner = add(right, right_offset)
    if proposal_level == workspace.p_proposed:
        return (["polypoints"]
                + dotted_line_points(x_coord(left), y_coord(left),
                                     x_coord(left_corner), y_coord(left_corner), gg.p_dot_interval)
                + dotted_line_points(x_coord(left_corner), y_coord(left_corner),
                                     x_coord(right_corner), y_coord(right_corner),
                                     gg.p_dot_interval)
                + dotted_line_points(x_coord(right_corner), y_coord(right_corner),
                                     x_coord(right), y_coord(right), gg.p_dot_interval))
    if proposal_level == workspace.p_evaluated:
        return (["line"]
                + dashed_line_points(x_coord(left), y_coord(left),
                                     x_coord(left_corner), y_coord(left_corner),
                                     p_bridge_dash_density, gg.p_dash_length)
                + dashed_line_points(x_coord(left_corner), y_coord(left_corner),
                                     x_coord(right_corner), y_coord(right_corner),
                                     p_bridge_dash_density, gg.p_dash_length)
                + dashed_line_points(x_coord(right_corner), y_coord(right_corner),
                                     x_coord(right), y_coord(right),
                                     p_bridge_dash_density, gg.p_dash_length))
    return (["polyline"]
            + zigzag_line_points(left, left_corner, p_bridge_zigzag_length, add)
            + zigzag_line_points(left_corner, right_corner, p_bridge_zigzag_length, add)
            + zigzag_line_points(right_corner, right, p_bridge_zigzag_length, add))


def make_spanning_vertical_bridge_pexp(top, bot, proposal_level):
    """bridge-graphics.ss: make-spanning-vertical-bridge-pexp"""
    gg = _metacat.general_graphics
    x_right = tell(setup.g_workspace_window, "get-spanning-vertical-bridge-right-x")
    top_right = make_rectangular(x_right, y_coord(top))
    bot_right = make_rectangular(x_right, y_coord(bot))
    x_top = x_coord(top)
    y_top = y_coord(top)
    x_bot = x_coord(bot)
    y_bot = y_coord(bot)
    if proposal_level == workspace.p_proposed:
        return (["polypoints"]
                + dotted_line_points(x_top, y_top, x_right, y_top, gg.p_dot_interval)
                + dotted_line_points(x_right, y_top, x_right, y_bot, gg.p_dot_interval)
                + dotted_line_points(x_right, y_bot, x_bot, y_bot, gg.p_dot_interval))
    if proposal_level == workspace.p_evaluated:
        return (["line"]
                + dashed_line_points(x_top, y_top, x_right, y_top,
                                     p_bridge_dash_density, gg.p_dash_length)
                + dashed_line_points(x_right, y_top, x_right, y_bot,
                                     p_bridge_dash_density, gg.p_dash_length)
                + dashed_line_points(x_right, y_bot, x_bot, y_bot,
                                     p_bridge_dash_density, gg.p_dash_length))
    return (["polyline"]
            + zigzag_line_points(top, top_right, p_bridge_zigzag_length, add)
            + zigzag_line_points(top_right, bot_right, p_bridge_zigzag_length, sub)
            + zigzag_line_points(bot_right, bot, p_bridge_zigzag_length, add))


def draw_bridge_grope(bridge_orientation, object1, object2):
    """bridge-graphics.ss: draw-bridge-grope"""
    if bridge_orientation == "horizontal":
        return draw_horizontal_bridge_grope(object1, object2)
    if bridge_orientation == "vertical":
        return draw_vertical_bridge_grope(object1, object2)
    return None         # 1.2: a case without else


def draw_horizontal_bridge_grope(object1, object2):
    """bridge-graphics.ss: draw-horizontal-bridge-grope"""
    if both_spanning_groups_p(object1, object2) is not False:
        return tell(setup.g_workspace_window, "flash",
                    make_spanning_horizontal_bridge_grope_pexp(
                        tell(object1, "get-group-spanning-bridge-graphics-coord", "horizontal"),
                        tell(object2, "get-group-spanning-bridge-graphics-coord", "horizontal")))
    return tell(setup.g_workspace_window, "flash",
                make_horizontal_bridge_grope_pexp(
                    tell(object1, "get-bridge-graphics-coord", "horizontal"),
                    tell(object2, "get-bridge-graphics-coord", "horizontal")))


def draw_vertical_bridge_grope(object1, object2):
    """bridge-graphics.ss: draw-vertical-bridge-grope"""
    if both_spanning_groups_p(object1, object2) is not False:
        return tell(setup.g_workspace_window, "flash",
                    make_spanning_vertical_bridge_grope_pexp(
                        tell(object1, "get-group-spanning-bridge-graphics-coord", "vertical"),
                        tell(object2, "get-group-spanning-bridge-graphics-coord", "vertical")))
    return tell(setup.g_workspace_window, "flash",
                make_vertical_bridge_grope_pexp(
                    tell(object1, "get-bridge-graphics-coord", "vertical"),
                    tell(object2, "get-bridge-graphics-coord", "vertical")))


def make_horizontal_bridge_grope_pexp(left, right):
    """bridge-graphics.ss: make-horizontal-bridge-grope-pexp"""
    offset = div(sub(right, left), 10)
    voffset = make_rectangular(0, magnitude(offset))
    return ["let-sgl", [],
            ["polyline"]
            + zigzag_line_points(left, add(left, voffset), p_bridge_zigzag_length, add)
            + zigzag_line_points(add(left, voffset), add(left, voffset, offset),
                                 p_bridge_zigzag_length, add),
            ["polyline"]
            + zigzag_line_points(right, add(right, voffset), p_bridge_zigzag_length, sub)
            + zigzag_line_points(add(right, voffset), add(right, voffset, sub(offset)),
                                 p_bridge_zigzag_length, sub)]


make_spanning_horizontal_bridge_grope_pexp = make_horizontal_bridge_grope_pexp


def make_vertical_bridge_grope_pexp(top, bot):
    """bridge-graphics.ss: make-vertical-bridge-grope-pexp"""
    offset = div(sub(top, bot), 4)
    top_prime = sub(top, offset)
    bot_prime = add(bot, offset)
    sign = add if x_coord(top) < x_coord(bot) else sub
    return ["let-sgl", [],
            centered_zigzag_line(top, top_prime, p_bridge_zigzag_length, sign),
            centered_zigzag_line(bot_prime, bot, p_bridge_zigzag_length, sign)]


_spanning_vertical_offset = make_rectangular(0, mul(F(1, 2), p_bridge_zigzag_length))


def make_spanning_vertical_bridge_grope_pexp(top, bot):
    """bridge-graphics.ss: make-spanning-vertical-bridge-grope-pexp (the offset
    is the closure's, computed once at load time)"""
    top = add(top, _spanning_vertical_offset)
    bot = sub(bot, _spanning_vertical_offset)
    right = tell(setup.g_workspace_window, "get-spanning-vertical-bridge-right-x")
    top_right = make_rectangular(right, y_coord(top))
    bot_right = make_rectangular(right, y_coord(bot))
    return (["polyline"]
            + zigzag_line_points(top, top_right, p_bridge_zigzag_length, sub)
            + zigzag_line_points(top_right, bot_right, p_bridge_zigzag_length, sub)
            + zigzag_line_points(bot_right, bot, p_bridge_zigzag_length, sub))


def new_bridge_label_number(bridge):
    """bridge-graphics.ss: new-bridge-label-number"""
    bridge_type = tell(bridge, "get-bridge-type")
    other_bridge_numbers = tell_all(
        chez.remq(bridge, tell(workspace.g_workspace, "get-bridges", bridge_type)),
        "get-bridge-label-number")
    n = 1
    while member_p(n, other_bridge_numbers):      # repeat* until (not (member? ...))
        n = add1(n)
    return n
