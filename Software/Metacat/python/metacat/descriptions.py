"""descriptions.ss: descriptions and the description codelets.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from descriptions.ss, with
racket/engine/descriptions.rktl as a worked translation.

The four codelet procedures are given to their codelet types by load()
(define-codelet-procedure* needs the types, which coderack.load makes).  Names
from files translated later (*workspace*, %built% ... in workspace.ss,
make-workspace-structure, contains? in groups.ss, fully-active? and the plato-
nodes in slipnet.ss, temp-adjusted-probability in formulas.ss, *themespace* in
themes.ss) are read through the package at call time.  The coderack battery pins
descriptions-equal? and description-member?; the rest is pinned by the
batteries of the items that translate those files.  The engine never imports
tkinter.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez, sugar
from metacat import coderack, setup
from metacat.chez import String
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.sugar import say
from metacat.utilities import (average, count, exists_p, first, letter_p, maximum, member_p,
                               one_minus, percent, rest, stochastic_pick,
                               stochastic_pick_by_method, tell_all, workspace_p)


class Description(SchemeObject):
    """descriptions.ss: make-description (the closure)"""
    __slots__ = ("object", "description_type", "descriptor", "string", "workspace_structure")

    def __init__(this, object_, description_type, descriptor):
        this.object = object_
        this.description_type = description_type
        this.descriptor = descriptor
        # chez: a let's order is unspecified (porting-notes.md says right to left here); both
        # bindings are free of side effects (anomalies: "Chez doesn't evaluate arguments left to right")
        this.string = tell(object_, "get-string")
        this.workspace_structure = _metacat.workspace_structures.make_workspace_structure()

    @message("object-type")
    def object_type(this, self):
        return "description"

    # This is solely for the purposes of (say <description>):
    @message("print-name")
    def print_name(this, self):
        # a Scheme string (b:canon tells it from a symbol)
        return String(chez.format_("~a:~a",
                                   tell(this.description_type, "get-short-name"),
                                   tell(this.descriptor, "get-short-name")))

    @message("print")
    def print_(this, self):
        object_ = this.object
        if letter_p(object_) or tell(object_, "get-proposal-level") == _metacat.workspace.p_built:
            status = ""
        else:
            status = chez.format_(" (~a, not drawn)"
                                  if (setup.p_workspace_graphics is not False
                                      and not tell(object_, "drawn?"))
                                  else " (~a)",
                                  _proposal_level_name(tell(object_, "get-proposal-level")))
        chez.printf("Description of ~a~a in ~a: ~a:~a",
                    tell(object_, "ascii-name"),
                    status,
                    tell(this.string, "generic-name"),
                    tell(this.description_type, "get-short-name"),
                    tell(this.descriptor, "get-lowercase-name"))
        if tell(self, "get-proposal-level") < _metacat.workspace.p_built:
            chez.printf(" (~a)", _proposal_level_name(tell(self, "get-proposal-level")))
        return chez.newline()

    @message("get-theme-types")
    def get_theme_types(this, self):
        which = tell(this.object, "which-string")
        if which == "initial":
            return ["top-bridge", "vertical-bridge"]
        if which == "modified":
            return ["top-bridge"]
        if which == "target":
            if setup.p_justify_mode is not False:
                return ["vertical-bridge", "bottom-bridge"]
            return ["vertical-bridge"]
        if which == "answer":
            return ["bottom-bridge"]
        return None

    @message("get-object")
    def get_object(this, self):
        return this.object

    @message("get-description-type")
    def get_description_type(this, self):
        return this.description_type

    @message("get-descriptor")
    def get_descriptor(this, self):
        return this.descriptor

    @message("get-descriptor-activation")
    def get_descriptor_activation(this, self):
        return tell(this.descriptor, "get-activation")

    @message("description-type?")
    def description_type_p(this, self, type_):
        return type_ is this.description_type

    @message("relevant?")
    def relevant_p(this, self):
        return _metacat.slipnet.fully_active_p(this.description_type)

    @message("get-conceptual-depth")
    def get_conceptual_depth(this, self):
        return tell(this.descriptor, "get-conceptual-depth")

    @message("bond-description?")
    def bond_description_p(this, self):
        return (this.description_type is _metacat.slipnet.plato_bond_category
                or this.description_type is _metacat.slipnet.plato_bond_facet)

    # Themes:
    @message("get-thematic-compatibility")
    def get_thematic_compatibility(this, self):
        return tell(self, "get-max-theme-support")

    @message("get-max-theme-support")
    def get_max_theme_support(this, self):
        return maximum(tell(self, "get-theme-support-values"))

    @message("get-theme-support-values")
    def get_theme_support_values(this, self):
        def support(theme):
            if this.description_type is tell(theme, "get-dimension"):
                return percent(tell(theme, "get-absolute-activation"))
            return 0
        # chez: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
        return chez.map_(support, tell(_metacat.themes.g_themespace, "get-active-themes",
                                       tell(self, "get-theme-types")))

    @message("calculate-internal-strength")
    def calculate_internal_strength(this, self):
        return tell(this.descriptor, "get-conceptual-depth")

    @message("calculate-external-strength")
    def calculate_external_strength(this, self):
        return average(tell(self, "calculate-local-support"),
                       tell(this.description_type, "get-activation"))

    @message("calculate-local-support")
    def calculate_local_support(this, self):
        object_ = this.object
        groups = _metacat.groups

        def supporting_p(other_object):
            return (not groups.contains_p(object_, other_object)
                    and not groups.contains_p(other_object, object_)
                    and member_p(this.description_type,
                                 tell_all(tell(other_object, "get-descriptions"),
                                          "get-description-type")))
        number_of_supporting_objects = count(supporting_p,
                                             chez.remq(object_, tell(this.string, "get-objects")))
        return {0: 0, 1: 20, 2: 60, 3: 90}.get(number_of_supporting_objects, 100)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.workspace_structure)


def _proposal_level_name(level):
    """(case level (1 "proposed") (2 "evaluated")), void otherwise."""
    if level == 1:
        return "proposed"
    if level == 2:
        return "evaluated"
    return None


def make_description(object_, description_type, descriptor):
    """descriptions.ss: make-description"""
    return Description(object_, description_type, descriptor)


def bottom_up_description_scout():
    """descriptions.ss: bottom-up-description-scout (the codelet procedure)"""
    chosen_object = tell(_metacat.workspace.g_workspace, "choose-object", "get-average-salience")
    chosen_description = tell(chosen_object, "choose-relevant-description-by-activation")
    if not exists_p(chosen_description):
        say("Couldn't choose a description. Fizzling.")
        sugar.fizzle()
    chosen_descriptor = tell(chosen_description, "get-descriptor")
    property_links = tell(chosen_descriptor, "get-similar-property-links")
    if len(property_links) == 0:
        say("No short-enough property links. Fizzling.")
        sugar.fizzle()
    properties = tell_all(property_links, "get-to-node")
    property_activations = tell_all(properties, "get-activation")
    degrees_of_assoc = tell_all(property_links, "get-degree-of-assoc")
    chosen_property = stochastic_pick(properties,
                                      chez.map_(chez.mul, degrees_of_assoc, property_activations))
    return propose_description(chosen_object, tell(chosen_property, "get-category"), chosen_property)


def top_down_description_scout(description_type, scope):
    """descriptions.ss: top-down-description-scout (the codelet procedure)"""
    if workspace_p(scope):
        say("Scope is entire Workspace.")
    else:
        say("Focusing on ", tell(scope, "generic-name"), "...")
    chosen_object = tell(scope, "choose-object", "get-average-salience")
    possible_descriptors = tell(description_type, "get-possible-descriptors", chosen_object)
    if len(possible_descriptors) == 0:
        say("Couldn't make description. Fizzling.")
        sugar.fizzle()
    chosen_descriptor = stochastic_pick_by_method(possible_descriptors, "get-activation")
    return propose_description(chosen_object, description_type, chosen_descriptor)


def description_evaluator(proposed_description):
    """descriptions.ss: description-evaluator (the codelet procedure)"""
    descriptor = tell(proposed_description, "get-descriptor")
    tell(descriptor, "activate-from-workspace")
    tell(proposed_description, "update-strength")
    strength = tell(proposed_description, "get-strength")
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(_metacat.formulas.temp_adjusted_probability(percent(strength))):
        say("Description not strong enough. Fizzling.")
        sugar.fizzle()
    tell(proposed_description, "update-proposal-level", _metacat.workspace.p_evaluated)
    return sugar.post_codelet_star(strength, coderack.description_builder, proposed_description)


def description_builder(proposed_description):
    """descriptions.ss: description-builder (the codelet procedure)"""
    object_ = tell(proposed_description, "get-object")
    description_type = tell(proposed_description, "get-description-type")
    descriptor = tell(proposed_description, "get-descriptor")
    if tell(_metacat.workspace.g_workspace, "object-exists?", object_) is False:
        say("This object no longer exists. Fizzling.")
        sugar.fizzle()
    if tell(object_, "description-present?", proposed_description) is not False:
        say("This description already exists. Fizzling.")
        tell(description_type, "activate-from-workspace")
        tell(descriptor, "activate-from-workspace")
        sugar.fizzle()
    return build_description(proposed_description)


def propose_description(object_, description_type, descriptor):
    """descriptions.ss: propose-description"""
    proposed_description = make_description(object_, description_type, descriptor)
    tell(descriptor, "activate-from-workspace")
    tell(proposed_description, "update-proposal-level", _metacat.workspace.p_proposed)
    return sugar.post_codelet_star(tell(description_type, "get-activation"),
                                   coderack.description_evaluator, proposed_description)


def build_description(proposed_description):
    """descriptions.ss: build-description"""
    object_ = tell(proposed_description, "get-object")
    description_type = tell(proposed_description, "get-description-type")
    descriptor = tell(proposed_description, "get-descriptor")
    if tell(proposed_description, "bond-description?"):
        tell(object_, "add-bond-description", proposed_description)
    else:
        tell(object_, "add-description", proposed_description)
    tell(description_type, "activate-from-workspace")
    tell(descriptor, "activate-from-workspace")
    return tell(proposed_description, "update-proposal-level", _metacat.workspace.p_built)


def descriptions_equal_p(d1, d2):
    """descriptions.ss: descriptions-equal?"""
    return (chez.eq_p(tell(d1, "get-description-type"), tell(d2, "get-description-type"))
            and chez.eq_p(tell(d1, "get-descriptor"), tell(d2, "get-descriptor")))


def description_member_p(d, l):
    """descriptions.ss: description-member?"""
    for x in l:
        if descriptions_equal_p(d, x):
            return True
    return False


def load():
    """descriptions.ss: the four define-codelet-procedure* forms, in the file's order."""
    sugar.define_codelet_procedure_star("bottom-up-description-scout", bottom_up_description_scout)
    sugar.define_codelet_procedure_star("top-down-description-scout", top_down_description_scout)
    sugar.define_codelet_procedure_star("description-evaluator", description_evaluator)
    sugar.define_codelet_procedure_star("description-builder", description_builder)
