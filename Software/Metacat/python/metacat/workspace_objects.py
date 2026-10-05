"""workspace-objects.ss: letters and the workspace-object closure they share with groups.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from workspace-objects.ss, with
racket/engine/workspace-objects.rktl as a worked translation.

Closures are SchemeObject classes (docs/python-translation-plan.md, "Objects"):
new-letter's closure is Letter, make-workspace-object's is WorkspaceObject.  A
letter delegates what it doesn't answer to its WorkspaceObject, which delegates
to base-object; groups (groups.ss) delegate to a WorkspaceObject the same way.
The descriptions and bond lists are Python lists, rebuilt (never mutated) on
every cons and remq.  Values are exact unless a flonum enters (rounding is
utilities.round_).  Names of files translated later (bonds, groups, bridges,
rules, trace) are read through the package at call time; *workspace-window*,
%workspace-graphics% and %justify-mode% are setup's.  The engine never imports
tkinter.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez
from metacat import descriptions, formulas, images, setup, slipnet, workspace
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (average, base_object, compress, count, exists_p, filter_, first,
                               group_p, hundred_minus, letter_p, maximum, member_p, minimum,
                               ormap_meth, percent_20, percent_80, round_, second, select,
                               select_meth, stochastic_pick, stochastic_pick_by_method, sum_,
                               tell_all)


def make_letter(string, letter_category, string_pos):
    """workspace-objects.ss: make-letter"""
    letter = new_letter(string, letter_category, string_pos)
    tell(letter, "new-description", slipnet.plato_object_category, slipnet.plato_letter)
    tell(letter, "new-description", slipnet.plato_letter_category, letter_category)
    return letter


class Letter(SchemeObject):
    """workspace-objects.ss: new-letter (the closure)"""
    __slots__ = ("string", "letter_category", "string_pos", "workspace_object", "image",
                 "print_name", "ascii_name", "graphics_pexp", "graphics_text_coord")

    def __init__(this, string, letter_category, string_pos):
        this.string = string
        this.letter_category = letter_category
        this.string_pos = string_pos
        # chez: let* order (the workspace object, then the image, ...)
        this.workspace_object = make_workspace_object(string, string_pos, string_pos)
        this.image = images.make_letter_image(letter_category)
        this.print_name = tell(letter_category, "get-lowercase-name")
        this.ascii_name = chez.String(chez.format_("~a:~a", this.print_name, string_pos))
        # The graphics-text-coord of a letter is NOT the same as its
        # bottom-left-graphics-coord, the latter being the lower left corner of
        # the bounding-box.  Nor is it necessarily the lower left corner of the
        # letter's bit matrix.  The actual text origin relative to the bit matrix
        # depends on the particular font.  (Marshall's comment.)
        this.graphics_pexp = False
        this.graphics_text_coord = False

    @message("object-type")
    def object_type(this, self):
        return "letter"

    @message("print-name")
    def print_name_(this, self):
        return this.print_name

    @message("ascii-name")
    def ascii_name_(this, self):
        return this.ascii_name

    @message("get-string-pos")
    def get_string_pos(this, self):
        return this.string_pos

    @message("get-graphics-pexp")
    def get_graphics_pexp(this, self):
        return this.graphics_pexp

    @message("get-graphics-text-coord")
    def get_graphics_text_coord(this, self):
        return this.graphics_text_coord

    @message("set-graphics-pexp")
    def set_graphics_pexp(this, self, pexp):
        this.graphics_pexp = pexp
        return "done"

    @message("set-graphics-text-coord")
    def set_graphics_text_coord(this, self, coord):
        this.graphics_text_coord = coord
        return "done"

    @message("get-image")
    def get_image(this, self):
        return this.image

    # get-initial-letter-category is included to be consistent with groups,
    # and is used by images:
    @message("get-initial-letter-category")
    def get_initial_letter_category(this, self):
        return this.letter_category

    @message("get-platonic-length")
    def get_platonic_length(this, self):
        return slipnet.plato_one

    @message("get-letter-category")
    def get_letter_category(this, self):
        return this.letter_category

    @message("get-letters")
    def get_letters(this, self):
        return [self]

    @message("nested-member?")
    def nested_member_p(this, self, object_):
        return False

    @message("singleton-group?")
    def singleton_group_p(this, self):
        return False

    @message("make-flipped-version")
    def make_flipped_version(this, self):
        return self

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.workspace_object)


def new_letter(string, letter_category, string_pos):
    """workspace-objects.ss: new-letter"""
    return Letter(string, letter_category, string_pos)


def _proposal_level_name(level):
    """(case level (0 "new") (1 "proposed") (2 "evaluated")), void otherwise."""
    if level == 0:
        return "new"
    if level == 1:
        return "proposed"
    if level == 2:
        return "evaluated"
    return None


class WorkspaceObject(SchemeObject):
    """workspace-objects.ss: make-workspace-object (the closure)"""
    __slots__ = ("string", "left_string_pos", "right_string_pos",
                 "id_num", "raw_importance", "relative_importance",
                 "intra_string_unhappiness", "horizontal_inter_string_unhappiness",
                 "vertical_inter_string_unhappiness", "average_unhappiness",
                 "intra_string_salience", "horizontal_inter_string_salience",
                 "vertical_inter_string_salience", "average_salience",
                 "descriptions", "outgoing_bonds", "incoming_bonds", "left_bond", "right_bond",
                 "enclosing_group", "horizontal_bridge", "vertical_bridge",
                 "new_answer_letter_p", "salience_clamped_p",
                 "graphics_x1", "graphics_y1", "graphics_x2", "graphics_y2",
                 "horizontal_bridge_graphics_coord", "vertical_bridge_graphics_coord",
                 "horizontal_group_spanning_bridge_graphics_coord",
                 "vertical_group_spanning_bridge_graphics_coord")

    def __init__(this, string, left_string_pos, right_string_pos):
        this.string = string
        this.left_string_pos = left_string_pos
        this.right_string_pos = right_string_pos
        this.id_num = 0
        this.raw_importance = 0
        this.relative_importance = 0
        this.intra_string_unhappiness = 0
        this.horizontal_inter_string_unhappiness = 0
        this.vertical_inter_string_unhappiness = 0
        this.average_unhappiness = 0
        this.intra_string_salience = 0
        this.horizontal_inter_string_salience = 0
        this.vertical_inter_string_salience = 0
        this.average_salience = 0
        this.descriptions = []
        this.outgoing_bonds = []
        this.incoming_bonds = []
        this.left_bond = False
        this.right_bond = False
        this.enclosing_group = False
        this.horizontal_bridge = False
        this.vertical_bridge = False
        this.new_answer_letter_p = False
        this.salience_clamped_p = False
        this.graphics_x1 = False
        this.graphics_y1 = False
        this.graphics_x2 = False
        this.graphics_y2 = False
        this.horizontal_bridge_graphics_coord = False
        this.vertical_bridge_graphics_coord = False
        this.horizontal_group_spanning_bridge_graphics_coord = False
        this.vertical_group_spanning_bridge_graphics_coord = False

    @message("object-type")
    def object_type(this, self):
        return "workspace-object"

    @message("print")
    def print_(this, self):
        chez.printf("~a ~a in ~a",
                    _metacat.trace.full_workspace_object_name(self),
                    tell(self, "ascii-name"),
                    tell(this.string, "generic-name"))
        if group_p(self) and tell(self, "get-proposal-level") < workspace.p_built:
            chez.printf(" (~a, not drawn)"
                        if (setup.p_workspace_graphics is not False
                            and tell(self, "drawn?") is False)
                        else " (~a)",
                        _proposal_level_name(tell(self, "get-proposal-level")))
        return chez.newline()

    @message("get-id-num")
    def get_id_num(this, self):
        return this.id_num

    @message("set-id-num")
    def set_id_num(this, self, new_id):
        this.id_num = new_id
        return "done"

    @message("get-graphics-x1")
    def get_graphics_x1(this, self):
        return this.graphics_x1

    @message("get-graphics-y1")
    def get_graphics_y1(this, self):
        return this.graphics_y1

    @message("get-graphics-x2")
    def get_graphics_x2(this, self):
        return this.graphics_x2

    @message("get-graphics-y2")
    def get_graphics_y2(this, self):
        return this.graphics_y2

    @message("set-graphics-coords")
    def set_graphics_coords(this, self, left_bottom, right_top):
        this.graphics_x1 = first(left_bottom)
        this.graphics_y1 = second(left_bottom)
        this.graphics_x2 = first(right_top)
        this.graphics_y2 = second(right_top)
        return "done"

    @message("get-bridge-graphics-coord")
    def get_bridge_graphics_coord(this, self, bridge_orientation):
        if bridge_orientation == "horizontal":
            return this.horizontal_bridge_graphics_coord
        if bridge_orientation == "vertical":
            return this.vertical_bridge_graphics_coord
        return None

    @message("get-group-spanning-bridge-graphics-coord")
    def get_group_spanning_bridge_graphics_coord(this, self, bridge_orientation):
        if bridge_orientation == "horizontal":
            return this.horizontal_group_spanning_bridge_graphics_coord
        if bridge_orientation == "vertical":
            return this.vertical_group_spanning_bridge_graphics_coord
        return None

    @message("set-bridge-graphics-coords")
    def set_bridge_graphics_coords(this, self, v, h):
        this.vertical_bridge_graphics_coord = v
        this.horizontal_bridge_graphics_coord = h
        return "done"

    @message("set-group-spanning-bridge-graphics-coords")
    def set_group_spanning_bridge_graphics_coords(this, self, v, h):
        this.vertical_group_spanning_bridge_graphics_coord = v
        this.horizontal_group_spanning_bridge_graphics_coord = h
        return "done"

    @message("get-instantiated-image-object")
    def get_instantiated_image_object(this, self):
        image = tell(self, "get-image")
        swapped_image = tell(image, "get-swapped-image")
        if exists_p(swapped_image):
            return tell(swapped_image, "get-instantiated-object")
        return tell(image, "get-instantiated-object")

    @message("get-string")
    def get_string(this, self):
        return this.string

    @message("which-string")
    def which_string(this, self):
        return tell(this.string, "get-string-type")

    @message("in-string?")
    def in_string_p(this, self, s):
        return s is this.string

    @message("get-left-string-pos")
    def get_left_string_pos(this, self):
        return this.left_string_pos

    @message("get-right-string-pos")
    def get_right_string_pos(this, self):
        return this.right_string_pos

    @message("get-left-bond")
    def get_left_bond(this, self):
        return this.left_bond

    @message("get-right-bond")
    def get_right_bond(this, self):
        return this.right_bond

    @message("get-enclosing-group")
    def get_enclosing_group(this, self):
        return this.enclosing_group

    @message("get-bridge")
    def get_bridge(this, self, bridge_orientation):
        if bridge_orientation == "horizontal":
            return this.horizontal_bridge
        if bridge_orientation == "vertical":
            return this.vertical_bridge
        return None

    @message("get-descriptions")
    def get_descriptions(this, self):
        return this.descriptions

    @message("get-all-descriptions")
    def get_all_descriptions(this, self):
        if group_p(self):
            return this.descriptions + tell(self, "get-bond-descriptions")
        return this.descriptions

    @message("get-incident-bonds")
    def get_incident_bonds(this, self):
        return compress([this.left_bond, this.right_bond])

    @message("get-num-of-incident-bonds")
    def get_num_of_incident_bonds(this, self):
        return len(tell(self, "get-incident-bonds"))

    @message("get-concept-pattern")
    def get_concept_pattern(this, self):
        def entry(description):
            descriptor = tell(description, "get-descriptor")
            relevance = (slipnet.p_max_activation
                         if tell(description, "relevant?") is not False else 0)
            return [descriptor, relevance]
        # chez: map's order (the procedure is pure; kept for uniformity)
        return ["concepts"] + chez.map_(entry, tell(self, "get-all-descriptions"))

    @message("clamp-salience")
    def clamp_salience(this, self):
        this.salience_clamped_p = True
        return "done"

    @message("unclamp-salience")
    def unclamp_salience(this, self):
        this.salience_clamped_p = False
        return "done"

    @message("mapped?")
    def mapped_p(this, self, orientation):
        if orientation == "vertical":
            return exists_p(this.vertical_bridge)
        if orientation == "horizontal":
            return exists_p(this.horizontal_bridge)
        if orientation == "both":
            return exists_p(this.vertical_bridge) and exists_p(this.horizontal_bridge)
        return None

    @message("update-enclosing-group")
    def update_enclosing_group(this, self, new_group):
        this.enclosing_group = new_group
        return "done"

    @message("update-right-bond")
    def update_right_bond(this, self, new_bond):
        this.right_bond = new_bond
        return "done"

    @message("update-left-bond")
    def update_left_bond(this, self, new_bond):
        this.left_bond = new_bond
        return "done"

    @message("add-outgoing-bond")
    def add_outgoing_bond(this, self, bond):
        this.outgoing_bonds = [bond] + this.outgoing_bonds
        if tell(bond, "get-bond-category") is slipnet.plato_sameness:
            this.incoming_bonds = [bond] + this.incoming_bonds
        return "done"

    @message("add-incoming-bond")
    def add_incoming_bond(this, self, bond):
        this.incoming_bonds = [bond] + this.incoming_bonds
        if tell(bond, "get-bond-category") is slipnet.plato_sameness:
            this.outgoing_bonds = [bond] + this.outgoing_bonds
        return "done"

    @message("remove-outgoing-bond")
    def remove_outgoing_bond(this, self, bond):
        this.outgoing_bonds = chez.remq(bond, this.outgoing_bonds)
        if tell(bond, "get-bond-category") is slipnet.plato_sameness:
            this.incoming_bonds = chez.remq(bond, this.incoming_bonds)
        return "done"

    @message("remove-incoming-bond")
    def remove_incoming_bond(this, self, bond):
        this.incoming_bonds = chez.remq(bond, this.incoming_bonds)
        if tell(bond, "get-bond-category") is slipnet.plato_sameness:
            this.outgoing_bonds = chez.remq(bond, this.outgoing_bonds)
        return "done"

    @message("get-all-description-types")
    def get_all_description_types(this, self):
        return tell_all(this.descriptions, "get-description-type")

    @message("get-all-descriptors")
    def get_all_descriptors(this, self):
        return tell_all(this.descriptions, "get-descriptor")

    @message("get-descriptor-for")
    def get_descriptor_for(this, self, description_type):
        description = select_meth(this.descriptions, "description-type?", description_type)
        if exists_p(description):
            return tell(description, "get-descriptor")
        return False

    @message("distinguishing-descriptor?")
    def distinguishing_descriptor_p(this, self, descriptor):
        if (descriptor is slipnet.plato_letter
                or descriptor is slipnet.plato_group
                or member_p(descriptor, slipnet.g_slipnet_numbers)):
            return False
        if letter_p(self):
            other_objects = chez.remq(self, tell(this.string, "get-letters"))
        elif group_p(self):
            supergroup = tell(self, "get-enclosing-group")
            subgroups = filter_(group_p, tell(self, "get-constituent-objects"))
            other_objects = filter_(lambda group: not (group is self
                                                       or group is supergroup
                                                       or member_p(group, subgroups)),
                                    tell(this.string, "get-groups"))
        else:
            # 1.2: a cond without else; tell-all on void then fails, as in Chez
            other_objects = None
        other_descriptors = tell_all(
            [d for ds in tell_all(other_objects, "get-descriptions") for d in ds],
            "get-descriptor")
        return not member_p(descriptor, other_descriptors)

    @message("get-relevant-descriptions")
    def get_relevant_descriptions(this, self):
        return filter_(lambda d: tell(d, "relevant?"), this.descriptions)

    @message("get-distinguishing-descriptions")
    def get_distinguishing_descriptions(this, self):
        return filter_(lambda d: tell(self, "distinguishing-descriptor?",
                                      tell(d, "get-descriptor")),
                       this.descriptions)

    @message("get-relevant-distinguishing-descriptions")
    def get_relevant_distinguishing_descriptions(this, self):
        return filter_(lambda dd: tell(dd, "relevant?"),
                       tell(self, "get-distinguishing-descriptions"))

    @message("get-descriptions-for-rule")
    def get_descriptions_for_rule(this, self):
        def for_rule_p(rdd):
            return (tell(rdd, "description-type?", slipnet.plato_string_position_category)
                    or tell(rdd, "description-type?", slipnet.plato_alphabetic_position_category)
                    or (tell(rdd, "description-type?", slipnet.plato_letter_category)
                        and (letter_p(self)
                             or tell(self, "get-group-category") is slipnet.plato_samegrp)))
        return filter_(for_rule_p, tell(self, "get-relevant-distinguishing-descriptions"))

    @message("choose-description-for-rule")
    def choose_description_for_rule(this, self):
        possible_descriptions = tell(self, "get-descriptions-for-rule")
        return stochastic_pick(
            possible_descriptions,
            formulas.temp_adjusted_values(
                tell_all(possible_descriptions, "get-conceptual-depth")))

    @message("description-type-present?")
    def description_type_present_p(this, self, description_type):
        return ormap_meth(tell(self, "get-all-descriptions"), "description-type?", description_type)

    @message("description-present?")
    def description_present_p(this, self, description):
        return descriptions.description_member_p(description, tell(self, "get-all-descriptions"))

    @message("descriptor-present?")
    def descriptor_present_p(this, self, descriptor):
        return chez.ormap(lambda d: tell(d, "get-descriptor") is descriptor,
                          tell(self, "get-all-descriptions"))

    @message("all-description-types-present?")
    def all_description_types_present_p(this, self, description_types):
        return chez.andmap(lambda description_type:
                           tell(self, "description-type-present?", description_type),
                           description_types)

    @message("add-description")
    def add_description(this, self, new_description):
        this.descriptions = [new_description] + this.descriptions
        if (setup.p_workspace_graphics is not False
                and tell(new_description, "description-type?", slipnet.plato_length)
                and group_p(self)):
            tell(self, "enable-length-graphics")
            if tell(self, "drawn?") is not False:
                tell(setup.g_workspace_window, "draw-group-length", self)
        return "done"

    @message("new-description")
    def new_description(this, self, description_type, descriptor):
        description = descriptions.make_description(self, description_type, descriptor)
        tell(description, "update-proposal-level", workspace.p_built)
        return tell(self, "add-description", description)

    # Used only when creating horizontal bridges for translated rules:
    @message("attach-description")
    def attach_description(this, self, description_type, descriptor):
        description = descriptions.make_description(self, description_type, descriptor)
        tell(description, "update-proposal-level", workspace.p_built)
        this.descriptions = [description] + this.descriptions
        return "done"

    @message("delete-description-type")
    def delete_description_type(this, self, description_type):
        description = select_meth(this.descriptions, "description-type?", description_type)
        this.descriptions = chez.remq(description, this.descriptions)
        return "done"

    @message("update-bridge")
    def update_bridge(this, self, bridge_orientation, new_bridge):
        if bridge_orientation == "horizontal":
            this.horizontal_bridge = new_bridge
        elif bridge_orientation == "vertical":
            this.vertical_bridge = new_bridge
        return "done"

    @message("get-raw-importance")
    def get_raw_importance(this, self):
        return this.raw_importance

    @message("get-relative-importance")
    def get_relative_importance(this, self):
        return this.relative_importance

    @message("get-intra-string-unhappiness")
    def get_intra_string_unhappiness(this, self):
        return this.intra_string_unhappiness

    @message("get-inter-string-unhappiness")
    def get_inter_string_unhappiness(this, self, bridge_orientation):
        if bridge_orientation == "horizontal":
            return this.horizontal_inter_string_unhappiness
        if bridge_orientation == "vertical":
            return this.vertical_inter_string_unhappiness
        return None

    @message("get-average-unhappiness")
    def get_average_unhappiness(this, self):
        return this.average_unhappiness

    @message("get-intra-string-salience")
    def get_intra_string_salience(this, self):
        return this.intra_string_salience

    @message("get-inter-string-salience")
    def get_inter_string_salience(this, self, bridge_orientation):
        if bridge_orientation == "horizontal":
            return this.horizontal_inter_string_salience
        if bridge_orientation == "vertical":
            return this.vertical_inter_string_salience
        return None

    @message("get-average-salience")
    def get_average_salience(this, self):
        return this.average_salience

    @message("get-nesting-level")
    def get_nesting_level(this, self):
        if exists_p(this.enclosing_group):
            return chez.add1(tell(this.enclosing_group, "get-nesting-level"))
        return 0

    @message("get-letter-span")
    def get_letter_span(this, self):
        return chez.add1(chez.sub(this.right_string_pos, this.left_string_pos))

    @message("spans-whole-string?")
    def spans_whole_string_p(this, self):
        return tell(self, "get-letter-span") == tell(this.string, "get-length")

    @message("string-spanning-group?")
    def string_spanning_group_p(this, self):
        return group_p(self) and tell(self, "spans-whole-string?")

    @message("get-num-of-spanning-bridges")
    def get_num_of_spanning_bridges(this, self):
        if tell(self, "spans-whole-string?"):
            return count(exists_p, [this.horizontal_bridge, this.vertical_bridge])
        return 0

    @message("leftmost-in-string?")
    def leftmost_in_string_p(this, self):
        return this.left_string_pos == 0

    @message("middle-in-string?")
    def middle_in_string_p(this, self):
        left_neighbor = tell(self, "get-ungrouped-left-neighbor")
        right_neighbor = tell(self, "get-ungrouped-right-neighbor")
        return (exists_p(left_neighbor)
                and exists_p(right_neighbor)
                and tell(left_neighbor, "leftmost-in-string?")
                and tell(right_neighbor, "rightmost-in-string?"))

    @message("rightmost-in-string?")
    def rightmost_in_string_p(this, self):
        return this.right_string_pos == chez.sub1(tell(this.string, "get-length"))

    @message("get-all-left-neighbors")
    def get_all_left_neighbors(this, self):
        if tell(self, "leftmost-in-string?"):
            return []
        left_pos = chez.sub1(this.left_string_pos)
        return ([tell(this.string, "get-letter", left_pos)]
                + tell(this.string, "get-right-edged-groups", left_pos))

    @message("get-all-right-neighbors")
    def get_all_right_neighbors(this, self):
        if tell(self, "rightmost-in-string?"):
            return []
        right_pos = chez.add1(this.right_string_pos)
        return ([tell(this.string, "get-letter", right_pos)]
                + tell(this.string, "get-left-edged-groups", right_pos))

    @message("get-ungrouped-left-neighbor")
    def get_ungrouped_left_neighbor(this, self):
        def ungrouped_p(n):
            enclosing_group = tell(n, "get-enclosing-group")
            return (not exists_p(enclosing_group)
                    or tell(enclosing_group, "nested-member?", self))
        return select(ungrouped_p, tell(self, "get-all-left-neighbors"))

    @message("get-ungrouped-right-neighbor")
    def get_ungrouped_right_neighbor(this, self):
        def ungrouped_p(n):
            enclosing_group = tell(n, "get-enclosing-group")
            return (not exists_p(enclosing_group)
                    or tell(enclosing_group, "nested-member?", self))
        return select(ungrouped_p, tell(self, "get-all-right-neighbors"))

    @message("choose-left-neighbor")
    def choose_left_neighbor(this, self):
        left_neighbors = tell(self, "get-all-left-neighbors")
        if len(left_neighbors) == 0:
            return False
        return stochastic_pick_by_method(left_neighbors, "get-intra-string-salience")

    @message("choose-right-neighbor")
    def choose_right_neighbor(this, self):
        right_neighbors = tell(self, "get-all-right-neighbors")
        if len(right_neighbors) == 0:
            return False
        return stochastic_pick_by_method(right_neighbors, "get-intra-string-salience")

    @message("choose-neighbor")
    def choose_neighbor(this, self):
        # chez: append's arguments draw nothing, so their order doesn't matter
        neighbors = (tell(self, "get-all-left-neighbors")
                     + tell(self, "get-all-right-neighbors"))
        if len(neighbors) == 0:
            return False
        return stochastic_pick_by_method(neighbors, "get-intra-string-salience")

    @message("update-raw-importance")
    def update_raw_importance(this, self):
        relevant_descriptions = tell(self, "get-relevant-descriptions")
        result = chez.min_(300, sum_(tell_all(relevant_descriptions,
                                              "get-descriptor-activation")))
        if exists_p(this.enclosing_group):
            this.raw_importance = chez.mul(Fraction(2, 3), result)
        else:
            this.raw_importance = result
        return "done"

    @message("update-relative-importance")
    def update_relative_importance(this, self, new_value):
        this.relative_importance = new_value
        return "done"

    @message("update-intra-string-unhappiness")
    def update_intra_string_unhappiness(this, self):
        if tell(self, "spans-whole-string?"):
            value = 0
        elif exists_p(this.enclosing_group):
            value = hundred_minus(tell(this.enclosing_group, "get-strength"))
        else:
            bonds = tell(self, "get-incident-bonds")
            if len(bonds) == 0:
                value = 100
            elif tell(self, "leftmost-in-string?") or tell(self, "rightmost-in-string?"):
                value = hundred_minus(round_(chez.mul(Fraction(1, 3),
                                                      tell(first(bonds), "get-strength"))))
            else:
                value = hundred_minus(round_(chez.mul(Fraction(1, 6),
                                                      sum_(tell_all(bonds, "get-strength")))))
        this.intra_string_unhappiness = value
        return "done"

    @message("update-inter-string-unhappiness")
    def update_inter_string_unhappiness(this, self):
        if exists_p(this.horizontal_bridge):
            horizontal_weakness = hundred_minus(tell(this.horizontal_bridge, "get-strength"))
        elif exists_p(this.enclosing_group):
            h = tell(this.enclosing_group, "get-bridge", "horizontal")
            if exists_p(h):
                horizontal_weakness = hundred_minus(chez.mul(Fraction(1, 2),
                                                             tell(h, "get-strength")))
            else:
                horizontal_weakness = 100
        else:
            horizontal_weakness = 100
        if exists_p(this.vertical_bridge):
            vertical_weakness = hundred_minus(tell(this.vertical_bridge, "get-strength"))
        elif exists_p(this.enclosing_group):
            v = tell(this.enclosing_group, "get-bridge", "vertical")
            if exists_p(v):
                vertical_weakness = hundred_minus(chez.mul(Fraction(1, 2),
                                                           tell(v, "get-strength")))
            else:
                vertical_weakness = 100
        else:
            vertical_weakness = 100
        string_type = tell(this.string, "get-string-type")
        if string_type == "initial":
            this.horizontal_inter_string_unhappiness = horizontal_weakness
            this.vertical_inter_string_unhappiness = vertical_weakness
        elif string_type == "modified":
            this.horizontal_inter_string_unhappiness = horizontal_weakness
        elif string_type == "target":
            this.vertical_inter_string_unhappiness = vertical_weakness
            if setup.p_justify_mode is not False:
                this.horizontal_inter_string_unhappiness = horizontal_weakness
        elif string_type == "answer":
            this.horizontal_inter_string_unhappiness = horizontal_weakness
        return "done"

    @message("update-average-unhappiness")
    def update_average_unhappiness(this, self):
        string_type = tell(this.string, "get-string-type")
        if string_type == "initial":
            value = average(this.intra_string_unhappiness,
                            this.horizontal_inter_string_unhappiness,
                            this.vertical_inter_string_unhappiness)
        elif string_type == "modified":
            value = average(this.intra_string_unhappiness,
                            this.horizontal_inter_string_unhappiness)
        elif string_type == "target":
            if setup.p_justify_mode is not False:
                value = average(this.intra_string_unhappiness,
                                this.vertical_inter_string_unhappiness,
                                this.horizontal_inter_string_unhappiness)
            else:
                value = average(this.intra_string_unhappiness,
                                this.vertical_inter_string_unhappiness)
        elif string_type == "answer":
            value = average(this.intra_string_unhappiness,
                            this.horizontal_inter_string_unhappiness)
        else:
            value = None  # 1.2: case without else; round then fails on void
        this.average_unhappiness = round_(value)
        return "done"

    # intra/inter-string-salience =
    #    intra/inter-string-unhappiness weighted by relative-importance
    # Heavily weighted by importance for inter-string-salience,
    # lightly weighted by importance for intra-string-salience.

    @message("update-intra-string-salience")
    def update_intra_string_salience(this, self):
        if this.salience_clamped_p:
            this.intra_string_salience = 100
        else:
            this.intra_string_salience = round_(chez.add(percent_80(this.intra_string_unhappiness),
                                                         percent_20(this.relative_importance)))
        return "done"

    @message("update-inter-string-salience")
    def update_inter_string_salience(this, self):
        def inter(unhappiness):
            return round_(chez.add(percent_20(unhappiness), percent_80(this.relative_importance)))
        if this.salience_clamped_p:
            this.horizontal_inter_string_salience = 100
            this.vertical_inter_string_salience = 100
        elif tell(this.string, "string-type?", "initial"):
            this.horizontal_inter_string_salience = inter(this.horizontal_inter_string_unhappiness)
            this.vertical_inter_string_salience = inter(this.vertical_inter_string_unhappiness)
        elif tell(this.string, "string-type?", "modified"):
            this.horizontal_inter_string_salience = inter(this.horizontal_inter_string_unhappiness)
        elif tell(this.string, "string-type?", "target"):
            this.vertical_inter_string_salience = inter(this.vertical_inter_string_unhappiness)
            if setup.p_justify_mode is not False:
                this.horizontal_inter_string_salience = inter(
                    this.horizontal_inter_string_unhappiness)
        elif tell(this.string, "string-type?", "answer"):
            this.horizontal_inter_string_salience = inter(this.horizontal_inter_string_unhappiness)
        return "done"

    @message("update-average-salience")
    def update_average_salience(this, self):
        string_type = tell(this.string, "get-string-type")
        if string_type == "initial":
            value = average(this.intra_string_salience,
                            this.horizontal_inter_string_salience,
                            this.vertical_inter_string_salience)
        elif string_type == "modified":
            value = average(this.intra_string_salience,
                            this.horizontal_inter_string_salience)
        elif string_type == "target":
            if setup.p_justify_mode is not False:
                value = average(this.intra_string_salience,
                                this.vertical_inter_string_salience,
                                this.horizontal_inter_string_salience)
            else:
                value = average(this.intra_string_salience,
                                this.vertical_inter_string_salience)
        elif string_type == "answer":
            value = average(this.intra_string_salience,
                            this.horizontal_inter_string_salience)
        else:
            value = None  # 1.2: case without else; round then fails on void
        this.average_salience = round_(value)
        return "done"

    @message("update-description-strengths")
    def update_description_strengths(this, self):
        for description in this.descriptions:
            tell(description, "update-strength")
        return "done"

    @message("update-object-values")
    def update_object_values(this, self):
        tell(self, "update-intra-string-unhappiness")
        tell(self, "update-inter-string-unhappiness")
        tell(self, "update-average-unhappiness")
        tell(self, "update-intra-string-salience")
        tell(self, "update-inter-string-salience")
        tell(self, "update-average-salience")
        return tell(self, "update-description-strengths")

    @message("choose-relevant-description-by-activation")
    def choose_relevant_description_by_activation(this, self):
        relevant_descriptions = tell(self, "get-relevant-descriptions")
        if len(relevant_descriptions) == 0:
            return False
        return stochastic_pick_by_method(relevant_descriptions, "get-descriptor-activation")

    @message("choose-relevant-distinguishing-description-by-depth")
    def choose_relevant_distinguishing_description_by_depth(this, self):
        relevant_distinguishing_descriptions = tell(self, "get-relevant-distinguishing-descriptions")
        if len(relevant_distinguishing_descriptions) == 0:
            return False
        return stochastic_pick_by_method(relevant_distinguishing_descriptions,
                                         "get-conceptual-depth")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_workspace_object(string, left_string_pos, right_string_pos):
    """workspace-objects.ss: make-workspace-object"""
    return WorkspaceObject(string, left_string_pos, right_string_pos)


def lowest_level_object(objects):
    """workspace-objects.ss: lowest-level-object"""
    max_nesting_level = maximum(tell_all(objects, "get-nesting-level"))
    return select(lambda object_: tell(object_, "get-nesting-level") == max_nesting_level,
                  objects)


def highest_level_object(objects):
    """workspace-objects.ss: highest-level-object"""
    min_nesting_level = minimum(tell_all(objects, "get-nesting-level"))
    return select(lambda object_: tell(object_, "get-nesting-level") == min_nesting_level,
                  objects)


def disjoint_objects_p(object1, object2):
    """workspace-objects.ss: disjoint-objects?"""
    return (tell(object1, "get-right-string-pos") < tell(object2, "get-left-string-pos")
            or tell(object1, "get-left-string-pos") > tell(object2, "get-right-string-pos"))


def lone_spanning_object_p(object1, object2):
    """workspace-objects.ss: lone-spanning-object?"""
    return ((tell(object1, "spans-whole-string?")
             and not tell(object2, "spans-whole-string?"))
            or (tell(object2, "spans-whole-string?")
                and not tell(object1, "spans-whole-string?")))


def both_spanning_groups_p(object1, object2):
    """workspace-objects.ss: both-spanning-groups?"""
    return (tell(object1, "string-spanning-group?")
            and tell(object2, "string-spanning-group?"))


def both_spanning_objects_p(object1, object2):
    """workspace-objects.ss: both-spanning-objects?"""
    return (tell(object1, "spans-whole-string?")
            and tell(object2, "spans-whole-string?"))
