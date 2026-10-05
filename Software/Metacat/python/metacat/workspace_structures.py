"""workspace-structures.ss: the parent object of every workspace structure, and fights.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from workspace-structures.ss,
with racket/engine/workspace-structures.rktl as a worked translation.

make-workspace-structure's closure is the WorkspaceStructure class
(docs/python-translation-plan.md, "Objects"): descriptions, bonds, groups,
bridges and rules delegate to one.  Its time stamp is *codelet-count*
(setup.g_codelet_count) when it is made; get-age reads the count at call time.
%built% is workspace.p_built, temp-adjusted-values formulas.temp_adjusted_values
(both translated in the same item).  The engine never imports tkinter.
"""
from __future__ import annotations

from metacat import chez
from metacat import formulas, setup, workspace
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (base_object, hundred_minus, one_minus, round_, stochastic_pick,
                               weighted_average)


class WorkspaceStructure(SchemeObject):
    """workspace-structures.ss: make-workspace-structure (the closure)"""
    __slots__ = ("time_stamp", "enclosing_group", "strength", "proposal_level",
                 "graphics_pexp", "drawn_p")

    def __init__(this):
        this.time_stamp = setup.g_codelet_count
        this.enclosing_group = False
        this.strength = 0
        this.proposal_level = 0
        this.graphics_pexp = False
        this.drawn_p = False

    @message("object-type")
    def object_type(this, self):
        return "workspace-structure"

    @message("drawn?")
    def drawn_p_(this, self):
        return this.drawn_p

    @message("set-drawn?")
    def set_drawn_p(this, self, new_value):
        this.drawn_p = new_value
        return "done"

    @message("get-graphics-pexp")
    def get_graphics_pexp(this, self):
        return this.graphics_pexp

    @message("set-graphics-pexp")
    def set_graphics_pexp(this, self, pexp):
        this.graphics_pexp = pexp
        return "done"

    @message("get-time-stamp")
    def get_time_stamp(this, self):
        return this.time_stamp

    # This is the time since the structure was proposed (not built):
    @message("get-age")
    def get_age(this, self):
        return chez.sub(setup.g_codelet_count, this.time_stamp)

    @message("get-enclosing-group")
    def get_enclosing_group(this, self):
        return this.enclosing_group

    @message("get-strength")
    def get_strength(this, self):
        return this.strength

    @message("get-weakness")
    def get_weakness(this, self):
        return hundred_minus(chez.expt(this.strength, 0.95))

    @message("get-proposal-level")
    def get_proposal_level(this, self):
        return this.proposal_level

    @message("proposed?")
    def proposed_p(this, self):
        return this.proposal_level < workspace.p_built

    @message("update-proposal-level")
    def update_proposal_level(this, self, new_level):
        this.proposal_level = new_level
        return "done"

    @message("update-enclosing-group")
    def update_enclosing_group(this, self, new_group):
        this.enclosing_group = new_group
        return "done"

    @message("update-strength")
    def update_strength(this, self):
        # a let*: internal, then external strength, then thematic compatibility
        internal_strength = tell(self, "calculate-internal-strength")
        external_strength = tell(self, "calculate-external-strength")
        intrinsic_strength = weighted_average(
            [internal_strength, external_strength],
            [internal_strength, hundred_minus(internal_strength)])
        compatibility = tell(self, "get-thematic-compatibility")
        thematic_weight = chez.abs_(compatibility)
        this.strength = round_(weighted_average(
            [100 if compatibility > 0 else 0, intrinsic_strength],
            [thematic_weight, one_minus(thematic_weight)]))
        return "done"

    # For structures that lack their own local method:
    @message("get-thematic-compatibility")
    def get_thematic_compatibility(this, self):
        return 0

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_workspace_structure():
    """workspace-structures.ss: make-workspace-structure"""
    return WorkspaceStructure()


def wins_fight_p(challenger, challenger_weight, defender, defender_weight):
    """workspace-structures.ss: wins-fight?"""
    # chez: a body sequence: the challenger's strength is updated before the defender's
    tell(challenger, "update-strength")
    tell(defender, "update-strength")
    # the two get-strength tells in (list ...) are pure, so their order doesn't matter
    return stochastic_pick(
        [True, False],
        formulas.temp_adjusted_values(
            [chez.mul(challenger_weight, tell(challenger, "get-strength")),
             chez.mul(defender_weight, tell(defender, "get-strength"))]))


def wins_all_fights_p(challenger, challenger_weight, defending_structures, defender_weight_or_s):
    """workspace-structures.ss: wins-all-fights?"""
    # chez: andmap goes first to last and stops at the first loss, so the draws
    # stop there too
    if isinstance(defender_weight_or_s, list):
        return chez.andmap(
            lambda defender, defender_weight: wins_fight_p(challenger, challenger_weight,
                                                           defender, defender_weight),
            defending_structures,
            defender_weight_or_s)
    return chez.andmap(
        lambda defender: wins_fight_p(challenger, challenger_weight,
                                      defender, defender_weight_or_s),
        defending_structures)
