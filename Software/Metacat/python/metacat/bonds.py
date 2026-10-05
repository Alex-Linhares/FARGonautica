"""bonds.ss: bonds and the bond codelets.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from bonds.ss, with
racket/engine/bonds.rktl as a worked translation.

make-bond's closure is the Bond class (docs/python-translation-plan.md,
"Objects"); it delegates what it doesn't answer to a WorkspaceStructure.  The
five codelet procedures are given to their codelet types by load()
(define-codelet-procedure* needs the types, which coderack.load makes).  Names
from files translated later (groups.ss: get-common-groups, break-group,
directed-group?, same-group-category?, same-group-direction?; bridges.ss:
break-bridge, incompatible-horizontal-CMs?, incompatible-vertical-CMs?) are
read through the package at call time.  Arithmetic stays exact unless a flonum
enters (the 0.6, 1.0 and 0.7 literals, sqrt of a non-square); rounding is
utilities.round_.  The engine never imports tkinter.

Evaluation order (audited against Chez's): every draw (choose-object,
choose-neighbor, choose-left/right-neighbor, stochastic-pick, wins-all-fights?)
sits in a let*, a body or a cond, so Python's order is Chez's.  The only
multi-argument sites with calls (the (list ...) and (append ...) of
get-incompatible-bridges and bond-builder, the averages of the top-down scouts,
printf's arguments) have no effects that depend on order.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez, sugar
from metacat import (coderack, setup, slipnet, workspace, workspace_objects,
                     workspace_structure_formulas, workspace_structures, formulas)
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.sugar import say
from metacat.utilities import (average, compress, count, cube, exists_p, filter_map,
                               intersect, maximum, one_minus, percent, print_, remq_duplicates,
                               round_, select_meth, stochastic_pick, tell_all, workspace_p,
                               workspace_string_p)


class Bond(SchemeObject):
    """bonds.ss: make-bond (the closure)"""
    __slots__ = ("from_object", "to_object", "bond_category", "bond_facet",
                 "from_object_descriptor", "to_object_descriptor",
                 "workspace_structure", "left_object", "right_object", "string",
                 "direction", "left_string_pos", "right_string_pos", "bond_importance")

    def __init__(this, from_object, to_object, bond_category, bond_facet,
                 from_object_descriptor, to_object_descriptor):
        this.from_object = from_object
        this.to_object = to_object
        this.bond_category = bond_category
        this.bond_facet = bond_facet
        this.from_object_descriptor = from_object_descriptor
        this.to_object_descriptor = to_object_descriptor
        # the let*, in order
        this.workspace_structure = workspace_structures.make_workspace_structure()
        if tell(from_object, "get-left-string-pos") < tell(to_object, "get-left-string-pos"):
            this.left_object = from_object
        else:
            this.left_object = to_object
        this.right_object = to_object if this.left_object is from_object else from_object
        this.string = tell(this.left_object, "get-string")
        if bond_category is slipnet.plato_sameness:
            this.direction = False
        elif this.left_object is from_object:
            this.direction = slipnet.plato_right
        else:
            this.direction = slipnet.plato_left
        this.left_string_pos = tell(this.left_object, "get-left-string-pos")
        this.right_string_pos = tell(this.right_object, "get-right-string-pos")
        this.bond_importance = 50 if exists_p(this.direction) else 100

    @message("object-type")
    def object_type(this, self):
        return "bond"

    @message("print")
    def print_(this, self):
        bond_category = this.bond_category
        direction = this.direction
        if bond_category is slipnet.plato_sameness:
            control = "=~a="
        elif direction is slipnet.plato_left:
            control = "<-~a-"
        elif direction is slipnet.plato_right:
            control = "-~a->"
        else:
            control = None
        if bond_category is slipnet.plato_sameness:
            name = "same"
        elif bond_category is slipnet.plato_successor:
            name = "succ"
        elif bond_category is slipnet.plato_predecessor:
            name = "pred"
        else:
            name = None
        chez.printf("Bond ~a ~a ~a in ~a",
                    tell(this.left_object, "ascii-name"),
                    chez.format_(control, name),
                    tell(this.right_object, "ascii-name"),
                    tell(this.string, "generic-name"))
        if tell(self, "get-proposal-level") < workspace.p_built:
            chez.printf(" (~a)", _proposal_level_name(tell(self, "get-proposal-level")))
        return chez.newline()

    @message("get-string")
    def get_string(this, self):
        return this.string

    @message("get-from-object")
    def get_from_object(this, self):
        return this.from_object

    @message("get-to-object")
    def get_to_object(this, self):
        return this.to_object

    @message("get-left-object")
    def get_left_object(this, self):
        return this.left_object

    @message("get-right-object")
    def get_right_object(this, self):
        return this.right_object

    @message("get-bond-category")
    def get_bond_category(this, self):
        return this.bond_category

    @message("get-direction")
    def get_direction(this, self):
        return this.direction

    @message("get-bond-facet")
    def get_bond_facet(this, self):
        return this.bond_facet

    @message("get-from-object-descriptor")
    def get_from_object_descriptor(this, self):
        return this.from_object_descriptor

    @message("get-to-object-descriptor")
    def get_to_object_descriptor(this, self):
        return this.to_object_descriptor

    @message("leftmost-in-string?")
    def leftmost_in_string_p(this, self):
        return this.left_string_pos == 0

    @message("rightmost-in-string?")
    def rightmost_in_string_p(this, self):
        return this.right_string_pos == chez.sub1(tell(this.string, "get-length"))

    @message("get-incompatible-bonds")
    def get_incompatible_bonds(this, self):
        return remq_duplicates(
            compress([tell(this.left_object, "get-right-bond"),
                      tell(this.right_object, "get-left-bond")]))

    @message("get-incompatible-bridges")
    def get_incompatible_bridges(this, self, bridge_orientation):
        # (list ...) is left to right; neither element draws or changes state
        return compress(
            [tell(self, "get-incompatible-bridge", this.left_object, bridge_orientation),
             tell(self, "get-incompatible-bridge", this.right_object, bridge_orientation)])

    @message("get-incompatible-bridge")
    def get_incompatible_bridge(this, self, object_, bridge_orientation):
        # continuation-point* return: only escapes upwards, so (return x) is a Python return
        bridge = tell(object_, "get-bridge", bridge_orientation)
        if not exists_p(bridge):
            return False
        string_position_CM = select_meth(tell(bridge, "get-concept-mappings"),
                                         "CM-type?", slipnet.plato_string_position_category)
        if not exists_p(string_position_CM):
            return False
        other_object = tell(bridge, "get-other-object", object_)
        if not (tell(other_object, "leftmost-in-string?") is not False
                or tell(other_object, "rightmost-in-string?") is not False):
            return False
        if tell(other_object, "leftmost-in-string?") is not False:
            other_bond = tell(other_object, "get-right-bond")
        else:
            other_bond = tell(other_object, "get-left-bond")
        if not (exists_p(other_bond) and directed_p(other_bond)):
            return False
        # a 2-binding let; neither binding draws or changes state
        bond_direction_CM = _metacat.concept_mappings.make_concept_mapping(
            self,
            slipnet.plato_direction_category,
            this.direction,
            other_bond,
            slipnet.plato_direction_category,
            tell(other_bond, "get-direction"))
        if bridge_orientation == "horizontal":
            incompatible_p = _metacat.bridges.incompatible_horizontal_CMs_p
        elif bridge_orientation == "vertical":
            incompatible_p = _metacat.bridges.incompatible_vertical_CMs_p
        else:
            incompatible_p = None   # (case ...) without else: void (applying it fails, as in Chez)
        if incompatible_p(bond_direction_CM, string_position_CM) is not False:
            return bridge
        return False

    @message("make-flipped-version")
    def make_flipped_version(this, self):
        return make_bond(
            this.to_object, this.from_object,
            tell(this.bond_category, "get-related-node", slipnet.plato_opposite),
            this.bond_facet, this.to_object_descriptor, this.from_object_descriptor)

    @message("get-num-of-local-supporting-bonds")
    def get_num_of_local_supporting_bonds(this, self):
        def supporting_p(other_bond):
            return (workspace_objects.disjoint_objects_p(this.left_object,
                                                         tell(other_bond, "get-left-object"))
                    and workspace_objects.disjoint_objects_p(this.right_object,
                                                             tell(other_bond, "get-right-object"))
                    and tell(other_bond, "get-bond-category") is this.bond_category
                    and tell(other_bond, "get-direction") is this.direction)
        return count(supporting_p, chez.remq(self, tell(this.string, "get-bonds")))

    @message("get-local-density")
    def get_local_density(this, self):
        def neighbors(object_, choose_method):
            # (cons neighbor (neighbors neighbor ...)): each neighbor is chosen
            # (a draw) before the recursion goes on from it
            out = []
            while True:
                neighbor = tell(object_, choose_method)
                if not exists_p(neighbor):
                    return out
                out.append(neighbor)
                object_ = neighbor
        # chez: a let*: all the left neighbours (drawn) before the right ones
        left_neighbors = neighbors(this.left_object, "choose-left-neighbor")
        right_neighbors = neighbors(this.right_object, "choose-right-neighbor")
        num_of_bond_slots = len(left_neighbors) + len(right_neighbors)

        def bond_counter(get_bond_method):
            def counter(object_):
                bond = tell(object_, get_bond_method)
                return (exists_p(bond)
                        and tell(bond, "get-bond-category") is this.bond_category
                        and tell(bond, "get-direction") is this.direction)
            return counter
        num_of_similar_bonds = (count(bond_counter("get-right-bond"), left_neighbors)
                                + count(bond_counter("get-left-bond"), right_neighbors))
        if num_of_bond_slots == 0:
            return 100
        return round_(chez.mul(100, chez.div(num_of_similar_bonds, num_of_bond_slots)))

    @message("get-local-support")
    def get_local_support(this, self):
        number = tell(self, "get-num-of-local-supporting-bonds")
        if chez.zero_p(number):
            return 0
        density = tell(self, "get-local-density")
        adjusted_density = chez.mul(100, chez.sqrt(percent(density)))
        number_factor = chez.min_(1, chez.expt(0.6, chez.div(1, cube(number))))
        return round_(chez.mul(adjusted_density, number_factor))

    @message("calculate-internal-strength")
    def calculate_internal_strength(this, self):
        compatibility_factor = (1.0 if tell(this.from_object, "object-type")
                                == tell(this.to_object, "object-type") else 0.7)
        bond_facet_factor = 1.0 if this.bond_facet is slipnet.plato_letter_category else 0.7
        return round_(chez.mul(compatibility_factor,
                               bond_facet_factor,
                               bond_degree_of_assoc(this.bond_category)))

    @message("calculate-external-strength")
    def calculate_external_strength(this, self):
        return tell(self, "get-local-support")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.workspace_structure)


def _proposal_level_name(level):
    """(case level (0 "new") (1 "proposed") (2 "evaluated")), void otherwise."""
    if level == 0:
        return "new"
    if level == 1:
        return "proposed"
    if level == 2:
        return "evaluated"
    return None


def make_bond(from_object, to_object, bond_category, bond_facet,
              from_object_descriptor, to_object_descriptor):
    """bonds.ss: make-bond"""
    return Bond(from_object, to_object, bond_category, bond_facet,
                from_object_descriptor, to_object_descriptor)


def bottom_up_bond_scout():
    """bonds.ss: bottom-up-bond-scout (the codelet procedure)"""
    # a let*: the object is chosen (a draw) before its neighbour (a draw)
    from_object = tell(workspace.g_workspace, "choose-object", "get-intra-string-salience")
    to_object = tell(from_object, "choose-neighbor")
    say("Chose object ", tell(from_object, "ascii-name"))
    if not exists_p(to_object):
        say("Chosen object has no neighbor. Fizzling.")
        sugar.fizzle()
    say("Chose neighbor ", tell(to_object, "ascii-name"))
    bond_facet = choose_bond_facet(from_object, to_object)
    if not exists_p(bond_facet):
        say("No possible bond facet. Fizzling.")
        sugar.fizzle()
    from_object_descriptor = tell(from_object, "get-descriptor-for", bond_facet)
    to_object_descriptor = tell(to_object, "get-descriptor-for", bond_facet)
    bond_category = get_bond_category(from_object_descriptor, to_object_descriptor)
    say("Chose descriptors ", from_object_descriptor, " and ", to_object_descriptor)
    if not exists_p(bond_category):
        say("No bond possible between descriptors. Fizzling.")
        return sugar.fizzle()
    if incompatible_bond_candidates_p(from_object, to_object, bond_facet, bond_category):
        say("Incompatible bond-candidate objects. Fizzling.")
        return sugar.fizzle()
    return propose_bond(from_object, to_object, bond_category, bond_facet,
                        from_object_descriptor, to_object_descriptor)


def top_down_bond_scout__category(bond_category, scope):
    """bonds.ss: top-down-bond-scout:category (the codelet procedure)"""
    if workspace_p(scope):
        say("Scope is entire Workspace.")
    else:
        say("Focusing on ", tell(scope, "generic-name"), "...")
    # a let*: the string (a draw unless scope is a string), then the object
    # (a draw), then its neighbour (a draw)
    if workspace_string_p(scope):
        string = scope
    else:
        ws = workspace
        # (list ...) is left to right; the tells only read state
        string = stochastic_pick(
            ws.g_all_strings if setup.p_justify_mode is not False else ws.g_non_answer_strings,
            [average(tell(ws.g_initial_string, "get-bond-category-relevance", bond_category),
                     tell(ws.g_initial_string, "get-average-intra-string-unhappiness")),
             average(tell(ws.g_modified_string, "get-bond-category-relevance", bond_category),
                     tell(ws.g_modified_string, "get-average-intra-string-unhappiness")),
             average(tell(ws.g_target_string, "get-bond-category-relevance", bond_category),
                     tell(ws.g_target_string, "get-average-intra-string-unhappiness")),
             (average(tell(ws.g_answer_string, "get-bond-category-relevance", bond_category),
                      tell(ws.g_answer_string, "get-average-intra-string-unhappiness"))
              if setup.p_justify_mode is not False else 0)])
    object1 = tell(string, "choose-object", "get-intra-string-salience")
    object2 = tell(object1, "choose-neighbor")
    if not exists_p(object2):
        say("Chosen object has no neighbor. Fizzling.")
        sugar.fizzle()
    bond_facet = choose_bond_facet(object1, object2)
    if not exists_p(bond_facet):
        say("No possible bond-facet. Fizzling.")
        sugar.fizzle()
    # a 2-binding let; neither binding draws or changes state
    descriptor1 = tell(object1, "get-descriptor-for", bond_facet)
    descriptor2 = tell(object2, "get-descriptor-for", bond_facet)
    if incompatible_bond_candidates_p(object1, object2, bond_facet, bond_category):
        say("Incompatible bond-candidate objects. Fizzling.")
        return sugar.fizzle()
    if get_bond_category(descriptor1, descriptor2) is bond_category:
        return propose_bond(object1, object2, bond_category, bond_facet,
                            descriptor1, descriptor2)
    if get_bond_category(descriptor2, descriptor1) is bond_category:
        return propose_bond(object2, object1, bond_category, bond_facet,
                            descriptor2, descriptor1)
    say("No possible bond. Fizzling.")
    return sugar.fizzle()


def top_down_bond_scout__direction(direction, scope):
    """bonds.ss: top-down-bond-scout:direction (the codelet procedure)"""
    if workspace_p(scope):
        say("Scope is entire Workspace.")
    else:
        say("Focusing on ", tell(scope, "generic-name"), "...")
    # a let*: the string (a draw unless scope is a string), then the object
    # (a draw), then its neighbour (a draw)
    if workspace_string_p(scope):
        string = scope
    else:
        ws = workspace
        # (list ...) is left to right; the tells only read state
        string = stochastic_pick(
            ws.g_all_strings if setup.p_justify_mode is not False else ws.g_non_answer_strings,
            [average(tell(ws.g_initial_string, "get-direction-relevance", direction),
                     tell(ws.g_initial_string, "get-average-intra-string-unhappiness")),
             average(tell(ws.g_modified_string, "get-direction-relevance", direction),
                     tell(ws.g_modified_string, "get-average-intra-string-unhappiness")),
             average(tell(ws.g_target_string, "get-direction-relevance", direction),
                     tell(ws.g_target_string, "get-average-intra-string-unhappiness")),
             (average(tell(ws.g_answer_string, "get-direction-relevance", direction),
                      tell(ws.g_answer_string, "get-average-intra-string-unhappiness"))
              if setup.p_justify_mode is not False else 0)])
    from_object = tell(string, "choose-object", "get-intra-string-salience")
    to_object = tell(from_object, ("choose-left-neighbor" if direction is slipnet.plato_left
                                   else "choose-right-neighbor"))
    if not exists_p(to_object):
        say("Chosen object lacks appropriate left/right neighbor. Fizzling.")
        sugar.fizzle()
    bond_facet = choose_bond_facet(from_object, to_object)
    if not exists_p(bond_facet):
        say("No possible bond-facet. Fizzling.")
        sugar.fizzle()
    from_descriptor = tell(from_object, "get-descriptor-for", bond_facet)
    to_descriptor = tell(to_object, "get-descriptor-for", bond_facet)
    bond_category = get_bond_category(from_descriptor, to_descriptor)
    if not exists_p(bond_category) or bond_category is slipnet.plato_sameness:
        say("No possible bond in this direction. Fizzling.")
        return sugar.fizzle()
    if incompatible_bond_candidates_p(from_object, to_object, bond_facet, bond_category):
        say("Incompatible bond-candidate objects. Fizzling.")
        return sugar.fizzle()
    return propose_bond(from_object, to_object, bond_category, bond_facet,
                        from_descriptor, to_descriptor)


def propose_bond(from_object, to_object, bond_category, bond_facet,
                 from_object_descriptor, to_object_descriptor):
    """bonds.ss: propose-bond"""
    say("Proposing bond:")
    tell(from_object_descriptor, "activate-from-workspace")
    tell(to_object_descriptor, "activate-from-workspace")
    tell(bond_facet, "activate-from-workspace")
    # a 2-binding let; making the bond draws nothing, the string is only read
    proposed_bond = make_bond(from_object, to_object, bond_category, bond_facet,
                              from_object_descriptor, to_object_descriptor)
    string = tell(from_object, "get-string")
    if setup.p_verbose is not False:
        print_(proposed_bond)
    tell(string, "add-proposed-bond", proposed_bond)
    tell(proposed_bond, "update-proposal-level", workspace.p_proposed)
    return sugar.post_codelet_star(bond_degree_of_assoc(bond_category),
                                   coderack.bond_evaluator, proposed_bond)


def bond_evaluator(proposed_bond):
    """bonds.ss: bond-evaluator (the codelet procedure)"""
    tell(proposed_bond, "update-strength")
    strength = tell(proposed_bond, "get-strength")
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(formulas.temp_adjusted_probability(percent(strength))):
        say("Bond not strong enough. Fizzling.")
        tell(tell(proposed_bond, "get-string"), "delete-proposed-bond", proposed_bond)
        sugar.fizzle()
    tell(tell(proposed_bond, "get-from-object-descriptor"), "activate-from-workspace")
    tell(tell(proposed_bond, "get-to-object-descriptor"), "activate-from-workspace")
    tell(tell(proposed_bond, "get-bond-facet"), "activate-from-workspace")
    tell(proposed_bond, "update-proposal-level", workspace.p_evaluated)
    return sugar.post_codelet_star(strength, coderack.bond_builder, proposed_bond)


def bond_builder(proposed_bond):
    """bonds.ss: bond-builder (the codelet procedure)"""
    from_object = tell(proposed_bond, "get-from-object")
    to_object = tell(proposed_bond, "get-to-object")
    string = tell(from_object, "get-string")
    if (tell(workspace.g_workspace, "object-exists?", from_object) is False
            or tell(workspace.g_workspace, "object-exists?", to_object) is False):
        say("One or both of the objects no longer exist. Fizzling.")
        sugar.fizzle()
    tell(string, "delete-proposed-bond", proposed_bond)
    if tell(string, "bond-present?", proposed_bond) is not False:
        say("This bond already exists. Fizzling.")
        tell(tell(proposed_bond, "get-bond-category"), "activate-from-workspace")
        if directed_p(proposed_bond):
            tell(tell(proposed_bond, "get-direction"), "activate-from-workspace")
        sugar.fizzle()
    incompatible_bonds = tell(proposed_bond, "get-incompatible-bonds")
    if (len(incompatible_bonds) != 0
            and workspace_structures.wins_all_fights_p(proposed_bond, 1,
                                                       incompatible_bonds, 1) is False):
        say("Lost to incompatible bond. Fizzling.")
        sugar.fizzle()
    incompatible_groups = _metacat.groups.get_common_groups(from_object, to_object)
    if (len(incompatible_groups) != 0
            and workspace_structures.wins_all_fights_p(
                proposed_bond, 1,
                incompatible_groups, maximum(tell_all(incompatible_groups, "get-letter-span")))
            is False):
        say("Lost to incompatible group. Fizzling.")
        sugar.fizzle()
    if (directed_p(proposed_bond)
            and (tell(proposed_bond, "leftmost-in-string?") is not False
                 or tell(proposed_bond, "rightmost-in-string?") is not False)):
        # (append h v): Chez may evaluate v first, but neither call draws or
        # changes state, so left to right is equivalent (as in the Racket port)
        incompatible_bridges = (tell(proposed_bond, "get-incompatible-bridges", "horizontal")
                                + tell(proposed_bond, "get-incompatible-bridges", "vertical"))
    else:
        incompatible_bridges = []
    if (len(incompatible_bridges) != 0
            and workspace_structures.wins_all_fights_p(proposed_bond, 2,
                                                       incompatible_bridges, 3) is False):
        say("Lost to incompatible bridge. Fizzling.")
        sugar.fizzle()
    say("Won against all incompatible structures.")
    for group in incompatible_groups:
        if tell(workspace.g_workspace, "object-exists?", group) is not False:
            _metacat.groups.break_group(group)
    for bond in incompatible_bonds:
        break_bond(bond)
    for bridge in incompatible_bridges:
        _metacat.bridges.break_bridge(bridge)
    return build_bond(proposed_bond)


def build_bond(proposed_bond):
    """bonds.ss: build-bond"""
    say("Building bond:")
    if setup.p_verbose is not False:
        print_(proposed_bond)
    tell(tell(proposed_bond, "get-string"), "add-bond", proposed_bond)
    tell(tell(proposed_bond, "get-from-object"), "add-outgoing-bond", proposed_bond)
    tell(tell(proposed_bond, "get-to-object"), "add-incoming-bond", proposed_bond)
    tell(tell(proposed_bond, "get-left-object"), "update-right-bond", proposed_bond)
    tell(tell(proposed_bond, "get-right-object"), "update-left-bond", proposed_bond)
    tell(tell(proposed_bond, "get-bond-category"), "activate-from-workspace")
    if directed_p(proposed_bond):
        tell(tell(proposed_bond, "get-direction"), "activate-from-workspace")
    return tell(proposed_bond, "update-proposal-level", workspace.p_built)


def break_bond(bond):
    """bonds.ss: break-bond"""
    say("Breaking bond:")
    if setup.p_verbose is not False:
        print_(bond)
    tell(tell(bond, "get-string"), "delete-bond", bond)
    tell(tell(bond, "get-from-object"), "remove-outgoing-bond", bond)
    tell(tell(bond, "get-to-object"), "remove-incoming-bond", bond)
    tell(tell(bond, "get-left-object"), "update-right-bond", False)
    return tell(tell(bond, "get-right-object"), "update-left-bond", False)


def bonds_equal_p(bond1, bond2):
    """bonds.ss: bonds-equal?"""
    # 1.2: the last test calls same-direction?, which nothing defines (anomalies:
    # "bonds-equal? calls same-direction?, which nothing defines"); it raises
    # when reached, as in Chez
    return (tell(bond1, "get-from-object") is tell(bond2, "get-from-object")
            and tell(bond1, "get-to-object") is tell(bond2, "get-to-object")
            and same_bond_category_p(bond1, bond2)
            and same_direction_p(bond1, bond2))


def same_direction_p(*args):
    """bonds.ss: same-direction? (never defined by the original: racket/engine/pending.rktl).
    Raises Chez's "variable same-direction? is not bound"."""
    raise chez.UnboundVariable("same-direction?")


def directed_p(bond):
    """bonds.ss: directed?"""
    return exists_p(tell(bond, "get-direction"))


def same_bond_category_p(bond1, bond2):
    """bonds.ss: same-bond-category?"""
    return tell(bond1, "get-bond-category") is tell(bond2, "get-bond-category")


def same_bond_direction_p(bond1, bond2):
    """bonds.ss: same-bond-direction?"""
    return tell(bond1, "get-direction") is tell(bond2, "get-direction")


def opposite_bond_category_p(bond1, bond2):
    """bonds.ss: opposite-bond-category?"""
    return (directed_p(bond1)
            and directed_p(bond2)
            and tell(bond1, "get-bond-category")
            is tell(tell(bond2, "get-bond-category"), "get-related-node", slipnet.plato_opposite))


def opposite_bond_direction_p(bond1, bond2):
    """bonds.ss: opposite-bond-direction?"""
    return (directed_p(bond1)
            and directed_p(bond2)
            and tell(bond1, "get-direction")
            is tell(tell(bond2, "get-direction"), "get-related-node", slipnet.plato_opposite))


def choose_bond_facet(object1, object2):
    """bonds.ss: choose-bond-facet"""
    string = tell(object1, "get-string")
    object1_bond_facets = get_bond_facets(object1)
    object2_bond_facets = get_bond_facets(object2)
    bond_facets = intersect(object1_bond_facets, object2_bond_facets)
    # chez: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
    support_values = chez.map_(
        lambda description_type: workspace_structure_formulas.description_type_support(
            description_type, string),
        bond_facets)
    if len(bond_facets) == 0:
        return False
    return stochastic_pick(bond_facets, support_values)


def bond_degree_of_assoc(bond_category):
    """bonds.ss: bond-degree-of-assoc"""
    return chez.min_(100, round_(chez.mul(11, chez.sqrt(tell(bond_category,
                                                             "get-degree-of-assoc")))))


def instance_of_p(node, category):
    """bonds.ss: instance-of?"""
    super_category = tell(node, "get-category")
    return exists_p(super_category) and super_category is category


def get_bond_facets(object_):
    """bonds.ss: get-bond-facets"""
    return filter_map(
        lambda d: instance_of_p(tell(d, "get-description-type"), slipnet.plato_bond_facet),
        lambda d: tell(d, "get-description-type"),
        tell(object_, "get-descriptions"))


def get_bond_category(from_descriptor, to_descriptor):
    """bonds.ss: get-bond-category"""
    if from_descriptor is to_descriptor:
        return slipnet.plato_sameness
    return slipnet.get_label(from_descriptor, to_descriptor)


def bonded_p(object1, object2):
    """bonds.ss: bonded?"""
    return ((exists_p(tell(object1, "get-right-bond"))
             and tell(tell(object1, "get-right-bond"), "get-right-object") is object2)
            or (exists_p(tell(object1, "get-left-bond"))
                and tell(tell(object1, "get-left-bond"), "get-left-object") is object2))


# Since succ and pred groups now are allowed to have LettCtgy
# descriptions, sameness bonds between succ and pred groups based on
# letter-category are thus possible, whereas before such bonds could
# only be built between letters or sameness groups.  Also, a directed
# bond is now possible between a pred grp and a succ grp (or vice
# versa).  For example, now it is possible for the rightward SUCC
# group [ab> to get bonded to the leftward SUCC group <ba] to form
# the "sameness" group A:[[ab><ba]].  The following test disallows
# this, and similar cases.  Also, bonds based on length rather than
# letter-category are allowed as long as they're not between two
# directed groups going in opposite directions, such as <ab][d>.

def incompatible_bond_candidates_p(object1, object2, bond_facet, bond_category):
    """bonds.ss: incompatible-bond-candidates?"""
    groups = _metacat.groups
    if bond_facet is slipnet.plato_length:
        return (groups.directed_group_p(object1)
                and groups.directed_group_p(object2)
                and not groups.same_group_direction_p(object1, object2))
    if bond_category is slipnet.plato_sameness:
        return groups.directed_group_p(object1) or groups.directed_group_p(object2)
    if groups.directed_group_p(object1) and groups.directed_group_p(object2):
        return (not groups.same_group_category_p(object1, object2)
                or not groups.same_group_direction_p(object1, object2))
    return groups.directed_group_p(object1) or groups.directed_group_p(object2)


def load():
    """bonds.ss: the five define-codelet-procedure* forms, in the file's order."""
    sugar.define_codelet_procedure_star("bottom-up-bond-scout", bottom_up_bond_scout)
    sugar.define_codelet_procedure_star("top-down-bond-scout:category",
                                        top_down_bond_scout__category)
    sugar.define_codelet_procedure_star("top-down-bond-scout:direction",
                                        top_down_bond_scout__direction)
    sugar.define_codelet_procedure_star("bond-evaluator", bond_evaluator)
    sugar.define_codelet_procedure_star("bond-builder", bond_builder)
