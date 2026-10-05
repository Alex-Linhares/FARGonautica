"""groups.ss: groups, the group codelets, and building and breaking groups.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from groups.ss, with
racket/engine/groups.rktl as a worked translation.

new-group's closure is the Group class (docs/python-translation-plan.md,
"Objects"): like a letter, a group delegates what it doesn't answer to its
WorkspaceObject (workspace_objects.make_workspace_object), then to its
WorkspaceStructure (workspace_structures.make_workspace_structure).  The five
codelet procedures are given to their codelet types by load().  Names of files
translated later (bonds, bridges, concept-mappings, trace, group-graphics,
general-graphics) are read through the package at call time; *workspace-window*,
%workspace-graphics%, %verbose% and %justify-mode% are setup's, the group fonts
view_globals'.  The one evaluation-order site of this file (get-local-density's
append, racket/engine/groups.rktl:371) is marked `# chez:`.  The engine never
imports tkinter.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez, sugar
from metacat import (coderack, descriptions, images, setup, slipnet, view_globals, workspace,
                     workspace_objects, workspace_structure_formulas, workspace_structures)
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.sugar import say, vprint, vprintf
from metacat.utilities import (average, compress, coord, count, cube, exists_p, filter_,
                               first, flatmap, group_p, hundred_minus, last, letter_p,
                               map_compress, member_p, one_minus, ormap_meth, percent, print_,
                               random_pick, remq_duplicates, round_, second, select,
                               select_meth, stochastic_pick, stochastic_pick_by_method,
                               tell_all, times_100, weighted_average, workspace_p,
                               workspace_string_p, adjacency_map, all_same_p)


def make_group(string, group_category, group_bond_facet, direction,
               left_object, right_object, objects, bonds):
    """groups.ss: make-group"""
    s = slipnet
    group = new_group(string, group_category, group_bond_facet, direction,
                      left_object, right_object, objects, bonds,
                      tell(left_object, "get-left-string-pos"),
                      tell(right_object, "get-right-string-pos"))
    tell(group, "new-description", s.plato_object_category, s.plato_group)
    tell(group, "new-description", s.plato_group_category, group_category)
    tell(group, "new-bond-description", s.plato_bond_category, tell(group, "get-bond-category"))
    if exists_p(direction):
        tell(group, "new-description", s.plato_direction_category, direction)
    if tell(group, "spans-whole-string?") is not False:
        tell(group, "new-description", s.plato_string_position_category, s.plato_whole)
    elif tell(group, "leftmost-in-string?") is not False:
        tell(group, "new-description", s.plato_string_position_category, s.plato_leftmost)
    elif tell(group, "middle-in-string?") is not False:
        tell(group, "new-description", s.plato_string_position_category, s.plato_middle)
    elif tell(group, "rightmost-in-string?") is not False:
        tell(group, "new-description", s.plato_string_position_category, s.plato_rightmost)
    tell(group, "new-bond-description", s.plato_bond_facet, group_bond_facet)
    # Attaching LettCtgy descriptions to groups even if they are successor or
    # predecessor groups allows horizontal bridges such as [abc] --> [bcd]:
    if group_bond_facet is s.plato_letter_category:
        initial_letter_category = tell(group, "get-initial-letter-category")
        tell(group, "new-description", s.plato_letter_category, initial_letter_category)
        if group_category is s.plato_samegrp:
            tell(group, "set-print-name", tell(initial_letter_category, "get-uppercase-name"))
    tell(group, "set-ascii-name")
    if setup.p_workspace_graphics is not False:
        tell(group, "set-graphics-parameters")
    return group


class Group(SchemeObject):
    """groups.ss: new-group (the closure)"""
    __slots__ = ("string", "group_category", "group_bond_facet", "direction",
                 "left_object", "right_object", "objects", "bonds",
                 "left_string_pos", "right_string_pos",
                 "workspace_object", "workspace_structure", "middle_object",
                 "ordered_objects", "initial_letter_category", "bond_descriptions",
                 "bond_category", "group_length", "platonic_length", "all_letter_group_p",
                 "letters", "image", "print_name", "ascii_name", "length_graphics_pexp",
                 "letcat_graphics_pexp", "length_graphics_enabled_p", "shrunk_singleton_p",
                 "shrunk_singleton_pexp")

    def __init__(this, string, group_category, group_bond_facet, direction, left_object,
                 right_object, objects, bonds, left_string_pos, right_string_pos):
        s = slipnet
        this.string = string
        this.group_category = group_category
        this.group_bond_facet = group_bond_facet
        this.direction = direction
        this.left_object = left_object
        this.right_object = right_object
        this.objects = objects
        this.bonds = bonds
        this.left_string_pos = left_string_pos
        this.right_string_pos = right_string_pos
        # chez: let* order, binding by binding
        this.workspace_object = workspace_objects.make_workspace_object(
            string, left_string_pos, right_string_pos)
        this.workspace_structure = workspace_structures.make_workspace_structure()
        this.middle_object = select(
            lambda object_: tell(object_, "get-descriptor-for",
                                 s.plato_string_position_category) is s.plato_middle,
            objects)
        this.ordered_objects = (list(reversed(objects)) if direction is s.plato_left
                                else objects)
        # This is mainly for efficiency, to avoid lots of searching
        # through descriptions every time a new group is created:
        this.initial_letter_category = tell(first(this.ordered_objects), "get-descriptor-for",
                                            s.plato_letter_category)
        this.bond_descriptions = []
        this.bond_category = tell(group_category, "get-related-node", s.plato_bond_category)
        this.group_length = len(objects)
        this.platonic_length = s.number_to_platonic_number(this.group_length)
        this.all_letter_group_p = chez.andmap(letter_p, objects)
        letters = []
        for part in tell_all(objects, "get-letters"):
            letters.extend(part)
        this.letters = letters
        # the arguments of make-image are free of side effects
        if this.group_length > 1:
            letter_relation = s.relationship_between(
                tell_all(this.ordered_objects, "get-initial-letter-category"))
        elif this.bond_category is s.plato_sameness:
            letter_relation = s.plato_identity
        else:
            letter_relation = this.bond_category
        if this.group_length > 1:
            length_relation = s.relationship_between(
                tell_all(this.ordered_objects, "get-platonic-length"))
        else:
            length_relation = s.plato_identity
        this.image = images.make_image(
            this.initial_letter_category,
            group_bond_facet,
            letter_relation,
            length_relation,
            direction if exists_p(direction) else s.plato_right,
            tell_all(this.ordered_objects, "get-image"))
        this.print_name = False
        this.ascii_name = False
        this.length_graphics_pexp = False
        this.letcat_graphics_pexp = False
        this.length_graphics_enabled_p = False
        this.shrunk_singleton_p = False
        this.shrunk_singleton_pexp = False

    @message("object-type")
    def object_type(this, self):
        return "group"

    @message("print-name")
    def print_name_(this, self):
        return this.print_name

    @message("ascii-name")
    def ascii_name_(this, self):
        return this.ascii_name

    @message("set-print-name")
    def set_print_name(this, self, pn):
        this.print_name = pn
        return "done"

    @message("set-ascii-name")
    def set_ascii_name(this, self):
        if exists_p(this.print_name):
            name = this.print_name
        elif this.direction is slipnet.plato_right:
            name = ">"
        elif this.direction is slipnet.plato_left:
            name = "<"
        else:
            name = this.group_length
        if tell(self, "spans-whole-string?") is not False:
            where = "*"
        else:
            where = chez.format_("~a,~a", this.left_string_pos, this.right_string_pos)
        this.ascii_name = chez.String(chez.format_("[~a]:~a", name, where))
        return "done"

    @message("get-graphics-pexp")
    def get_graphics_pexp(this, self):
        if this.shrunk_singleton_p is not False:
            return this.shrunk_singleton_pexp
        return tell(this.workspace_structure, "get-graphics-pexp")

    @message("set-shrunk-singleton?")
    def set_shrunk_singleton_p(this, self, new_value):
        this.shrunk_singleton_p = new_value
        return "done"

    @message("length-graphics-enabled?")
    def length_graphics_enabled_p_(this, self):
        return this.length_graphics_enabled_p

    @message("length-graphics-active?")
    def length_graphics_active_p(this, self):
        return (this.length_graphics_enabled_p is not False
                and tell(self, "get-proposal-level") == workspace.p_built)

    @message("enable-length-graphics")
    def enable_length_graphics(this, self):
        this.length_graphics_enabled_p = True
        return "done"

    @message("get-length-graphics-pexp")
    def get_length_graphics_pexp(this, self):
        return this.length_graphics_pexp

    @message("get-letcat-graphics-pexp")
    def get_letcat_graphics_pexp(this, self):
        return this.letcat_graphics_pexp

    @message("set-graphics-parameters")
    def set_graphics_parameters(this, self):
        s = slipnet
        add, sub, mul, div = chez.add, chez.sub, chez.mul, chez.div
        window = setup.g_workspace_window
        letcat_font = view_globals.p_group_letter_category_font
        length_font = view_globals.p_relevant_group_length_font
        print_name = this.print_name
        direction = this.direction
        # a let*: every binding reads, none draws
        left_letter = first(tell(this.left_object, "get-letters"))
        right_letter = last(tell(this.right_object, "get-letters"))
        x1 = tell(left_letter, "get-graphics-x1")
        y1 = tell(left_letter, "get-graphics-y1")
        x2 = tell(right_letter, "get-graphics-x2")
        y2 = tell(right_letter, "get-graphics-y2")
        x_delta = tell(this.string, "get-group-enclosure-x-delta")
        y_delta = tell(this.string, "get-group-enclosure-y-delta")
        sizing_factor = chez.max_(1, chez.sub1(tell(self, "get-letter-span")))
        # This special case is due to the fact that singleton groups have the
        # same sizing-factor as letter-span-2 groups, which causes graphics
        # problems for groups consisting of a singleton group and a letter (or
        # another singleton group):
        if (tell(self, "get-letter-span") == 2
                and (tell(this.left_object, "singleton-group?") is not False
                     or tell(this.right_object, "singleton-group?") is not False)):
            shrink_factor = Fraction(1, 4)
        elif direction is s.plato_right:
            shrink_factor = Fraction(-1, 2)
        else:
            shrink_factor = 0
        new_x1 = sub(x1, mul(sizing_factor, x_delta), mul(shrink_factor, x_delta))
        new_x2 = add(x2, mul(sizing_factor, x_delta), mul(shrink_factor, x_delta))
        new_y1 = sub(y1, mul(sizing_factor, y_delta), mul(shrink_factor, y_delta))
        new_y2 = add(y2, mul(sizing_factor, y_delta), mul(shrink_factor, y_delta))
        mid_x = div(add(new_x1, new_x2), 2)
        y2_lowered = sub(new_y2, mul(Fraction(3, 4), y_delta))
        length_string = chez.String(chez.format_("~a", len(this.objects)))
        if exists_p(print_name):
            length_x = add(mid_x,
                           mul(Fraction(1, 2), tell(window, "get-character-width",
                                                    print_name, letcat_font)),
                           mul(Fraction(1, 2), tell(window, "get-character-width",
                                                    chez.String(" "), letcat_font)),
                           mul(Fraction(1, 2), tell(window, "get-character-width",
                                                    length_string, length_font)))
        else:
            if direction is s.plato_right:
                offset_factor = Fraction(7, 8)
            elif direction is s.plato_left:
                offset_factor = Fraction(-7, 8)
            else:
                offset_factor = 0
            length_x = add(mid_x, mul(offset_factor,
                                      tell(window, "get-character-width",
                                           length_string, length_font)))
        length_coord = [length_x, y2_lowered]
        letcat_coord = [mid_x, y2_lowered]
        if not exists_p(print_name):
            top_y = new_y2
        else:
            bbox = tell(window, "get-character-bounding-box", print_name, letcat_font,
                        letcat_coord)
            letter_height = tell(window, "get-character-height", print_name, letcat_font)
            top_y = add(mul(Fraction(-1, 8), letter_height), second(second(bbox)))
        tell(self, "set-graphics-coords", [new_x1, new_y1], [new_x2, new_y2])
        tell(self, "set-bridge-graphics-coords",
             coord(mid_x, new_y1 if tell(this.string, "top-string?") is not False else top_y),
             coord(mid_x, top_y))
        if tell(self, "spans-whole-string?") is not False:
            tell(self, "set-group-spanning-bridge-graphics-coords",
                 coord(new_x2, div(add(new_y1, new_y2), 2)),
                 coord(mid_x, top_y))
        if exists_p(print_name):
            this.letcat_graphics_pexp = ["let-sgl", [["font", letcat_font],
                                                     ["text-justification", "center"],
                                                     ["text-mode", "image"]],
                                         ["text", letcat_coord, print_name]]
        else:
            this.letcat_graphics_pexp = False
        this.length_graphics_pexp = ["let-sgl", [["text-justification", "center"],
                                                 ["text-mode", "image"]],
                                     ["text", length_coord, length_string]]
        if tell(self, "singleton-group?") is not False:
            general_graphics = _metacat.general_graphics
            if exists_p(print_name):
                this.shrunk_singleton_pexp = [
                    "let-sgl", [],
                    this.letcat_graphics_pexp,
                    general_graphics.outline_box(x1, y1, x2, y2)]
            else:
                group_graphics = _metacat.group_graphics
                this.shrunk_singleton_pexp = [
                    "let-sgl", [],
                    general_graphics.arrowhead(
                        mid_x, y2, 0 if direction is s.plato_right else 180,
                        group_graphics.p_small_group_arrowhead_length,
                        group_graphics.p_group_arrowhead_angle),
                    general_graphics.outline_box(x1, y1, x2, y2)]
        return "done"

    @message("get-image")
    def get_image(this, self):
        return this.image

    @message("get-initial-letter-category")
    def get_initial_letter_category(this, self):
        return this.initial_letter_category

    @message("get-ending-letter-category")
    def get_ending_letter_category(this, self):
        return tell(last(this.ordered_objects), "get-descriptor-for",
                    slipnet.plato_letter_category)

    @message("get-platonic-length")
    def get_platonic_length(this, self):
        return this.platonic_length

    @message("get-group-category")
    def get_group_category(this, self):
        return this.group_category

    @message("get-direction")
    def get_direction(this, self):
        return this.direction

    @message("get-leftmost-object")
    def get_leftmost_object(this, self):
        return this.left_object

    @message("get-middle-object")
    def get_middle_object(this, self):
        return this.middle_object

    @message("get-rightmost-object")
    def get_rightmost_object(this, self):
        return this.right_object

    @message("get-constituent-objects")
    def get_constituent_objects(this, self):
        return this.objects

    @message("get-constituent-bonds")
    def get_constituent_bonds(this, self):
        return this.bonds

    @message("get-bond-facet")
    def get_bond_facet(this, self):
        return this.group_bond_facet

    @message("get-bond-descriptions")
    def get_bond_descriptions(this, self):
        return this.bond_descriptions

    @message("get-bond-category")
    def get_bond_category(this, self):
        return this.bond_category

    @message("get-letters")
    def get_letters(this, self):
        return this.letters

    @message("get-group-length")
    def get_group_length(this, self):
        return this.group_length

    @message("get-subobject-bridges")
    def get_subobject_bridges(this, self, bridge_orientation):
        return compress(tell_all(this.objects, "get-bridge", bridge_orientation))

    @message("get-highest-level-coincident-group")
    def get_highest_level_coincident_group(this, self):
        current_max = 0
        highest_level_group = False
        for g in tell(this.string, "get-all-other-coincident-groups",
                      self, this.left_string_pos, this.right_string_pos, this.direction):
            proposal_level = tell(g, "get-proposal-level")
            if proposal_level > current_max:
                current_max = proposal_level
                highest_level_group = g
        return highest_level_group

    @message("get-drawn-coincident-group")
    def get_drawn_coincident_group(this, self):
        return select(lambda g: tell(g, "drawn?"),
                      tell(this.string, "get-all-other-coincident-groups",
                           self, this.left_string_pos, this.right_string_pos, this.direction))

    @message("get-drawn-overlapping-groups")
    def get_drawn_overlapping_groups(this, self):
        return filter_(lambda g: (g is not self
                                  and tell(g, "drawn?") is not False
                                  and this.left_string_pos <= tell(g, "get-right-string-pos")
                                  and this.right_string_pos >= tell(g, "get-left-string-pos")),
                       tell(this.string, "get-all-groups"))

    @message("all-letter-group?")
    def all_letter_group_p_(this, self):
        return this.all_letter_group_p

    @message("singleton-group?")
    def singleton_group_p(this, self):
        return this.group_length == 1

    @message("top-level-member?")
    def top_level_member_p(this, self, object_):
        return member_p(object_, this.objects)

    @message("nested-member?")
    def nested_member_p(this, self, object_):
        # member? is #t or #f; the or's value is ormap-meth's otherwise
        if member_p(object_, this.objects):
            return True
        return ormap_meth(this.objects, "nested-member?", object_)

    @message("new-bond-description")
    def new_bond_description(this, self, description_type, descriptor):
        bond_description = descriptions.make_description(self, description_type, descriptor)
        tell(bond_description, "update-proposal-level", workspace.p_built)
        return tell(self, "add-bond-description", bond_description)

    @message("add-bond-description")
    def add_bond_description(this, self, description):
        this.bond_descriptions = [description] + this.bond_descriptions
        return "done"

    @message("set-new-constituent-bonds")
    def set_new_constituent_bonds(this, self, new_bonds):
        this.bonds = new_bonds
        return "done"

    @message("get-incompatible-groups")
    def get_incompatible_groups(this, self):
        return chez.remq(self, remq_duplicates(
            compress(tell_all(this.objects, "get-enclosing-group"))))

    @message("get-incompatible-bridges")
    def get_incompatible_bridges(this, self, bridge_orientation):
        if not exists_p(this.direction):
            return []
        # make-concept-mapping is free of side effects, so map-compress's order
        # (element by element) is all there is to keep
        return map_compress(
            lambda object_: tell(self, "get-incompatible-bridge", object_, bridge_orientation),
            this.objects)

    @message("get-incompatible-bridge")
    def get_incompatible_bridge(this, self, object_, bridge_orientation):
        # continuation-point* return: every (return #f) escapes from this method
        # only, so it is a plain Python return
        s = slipnet
        bridge = tell(object_, "get-bridge", bridge_orientation)
        if not exists_p(bridge):
            return False
        string_position_CM = select_meth(tell(bridge, "get-concept-mappings"),
                                         "CM-type?", s.plato_string_position_category)
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
        if not (exists_p(other_bond) and _metacat.bonds.directed_p(other_bond) is not False):
            return False
        # a two-binding let; make-concept-mapping has no side effects
        group_direction_CM = _metacat.concept_mappings.make_concept_mapping(
            self, s.plato_direction_category, this.direction,
            other_bond, s.plato_direction_category, tell(other_bond, "get-direction"))
        if bridge_orientation == "horizontal":
            incompatible_p = _metacat.bridges.incompatible_horizontal_CMs_p
        elif bridge_orientation == "vertical":
            incompatible_p = _metacat.bridges.incompatible_vertical_CMs_p
        else:
            incompatible_p = None  # case without else: void (never reached)
        if incompatible_p(group_direction_CM, string_position_CM) is not False:
            return bridge
        return False

    @message("make-flipped-version")
    def make_flipped_version(this, self):
        s = slipnet
        if this.group_category is s.plato_samegrp:
            return self
        flipped_bonds = tell_all(this.bonds, "make-flipped-version")
        flipped_group = make_group(
            this.string,
            tell(this.group_category, "get-related-node", s.plato_opposite),
            this.group_bond_facet,
            tell(this.direction, "get-related-node", s.plato_opposite),
            this.left_object, this.right_object, this.objects, flipped_bonds)
        # This is necessary to ensure that all bridges to this flipped
        # group will be stored in the same place in the workspace's
        # proposed-bridges-table as bridges to the unflipped version:
        tell(flipped_group, "set-id-num", tell(self, "get-id-num"))
        if tell(self, "description-type-present?", s.plato_length) is not False:
            attach_length_description(flipped_group)
        return flipped_group

    @message("get-num-of-local-supporting-groups")
    def get_num_of_local_supporting_groups(this, self):
        return count(
            lambda other_group: (
                workspace_objects.disjoint_objects_p(self, other_group) is not False
                and tell(other_group, "get-group-category") is this.group_category
                and tell(other_group, "get-direction") is this.direction),
            chez.remq(self, tell(this.string, "get-groups")))

    @message("get-local-density")
    def get_local_density(this, self):
        if tell(self, "spans-whole-string?") is not False:
            return 100

        def neighbors(object_, choose_method):
            neighbor = tell(object_, choose_method)
            if not exists_p(neighbor):
                return []
            group = tell(neighbor, "get-enclosing-group")
            if not (letter_p(neighbor) and exists_p(group)):
                return [neighbor] + neighbors(neighbor, choose_method)
            return [group] + neighbors(group, choose_method)

        # chez: evaluation order: both arguments of append draw
        # (choose-...-neighbor), and Chez evaluates append's second argument
        # first (racket/engine/groups.rktl:371; plan, "Evaluation order")
        right = neighbors(self, "choose-right-neighbor")
        left = neighbors(self, "choose-left-neighbor")
        other_objects = left + right
        num_of_objects = len(other_objects)
        num_of_similar_groups = count(
            lambda object_: (group_p(object_)
                             and workspace_objects.disjoint_objects_p(self, object_) is not False
                             and tell(object_, "get-group-category") is this.group_category
                             and tell(object_, "get-direction") is this.direction),
            other_objects)
        if num_of_objects == 0:
            return 100
        return round_(chez.mul(100, chez.div(num_of_similar_groups, num_of_objects)))

    @message("get-local-support")
    def get_local_support(this, self):
        num = tell(self, "get-num-of-local-supporting-groups")
        if num == 0:
            return 0
        density = tell(self, "get-local-density")
        adjusted_density = chez.mul(100, chez.sqrt(percent(density)))
        num_factor = chez.min_(1, chez.expt(0.6, chez.div(1, cube(num))))
        return round_(chez.mul(adjusted_density, num_factor))

    @message("calculate-internal-strength")
    def calculate_internal_strength(this, self):
        bond_factor = chez.mul(
            tell(this.bond_category, "get-degree-of-assoc"),
            1 if (exists_p(this.group_bond_facet)
                  and this.group_bond_facet is slipnet.plato_letter_category)
            else Fraction(1, 2))
        if this.group_length == 1:
            length_factor = 5
        # Original ccat value of group-length 2 case = 20
        elif this.group_length == 2:
            length_factor = 40
        elif this.group_length == 3:
            length_factor = 60
        else:
            length_factor = 90
        bond_factor_weight = chez.expt(bond_factor, 0.98)
        length_factor_weight = hundred_minus(bond_factor_weight)
        return round_(weighted_average([bond_factor, length_factor],
                                       [bond_factor_weight, length_factor_weight]))

    @message("calculate-external-strength")
    def calculate_external_strength(this, self):
        if tell(self, "spans-whole-string?") is not False:
            return 100
        return tell(self, "get-local-support")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.workspace_object, this.workspace_structure)


def new_group(string, group_category, group_bond_facet, direction, left_object,
              right_object, objects, bonds, left_string_pos, right_string_pos):
    """groups.ss: new-group"""
    return Group(string, group_category, group_bond_facet, direction, left_object,
                 right_object, objects, bonds, left_string_pos, right_string_pos)


def top_down_group_scout__category(group_category, scope):
    """groups.ss: top-down-group-scout:category (the codelet procedure)"""
    s = slipnet
    if workspace_p(scope):
        say("Scope is entire Workspace.")
    else:
        say("Focusing on ", tell(scope, "generic-name"), "...")
    # a let*: bond-category, string (draws), object (draws)
    bond_category = tell(group_category, "get-related-node", s.plato_bond_category)
    if workspace_string_p(scope):
        string = scope
    else:
        # the weights are built left to right; every element only reads
        ws = workspace
        justify_p = setup.p_justify_mode is not False
        string = stochastic_pick(
            ws.g_all_strings if justify_p else ws.g_non_answer_strings,
            [average(tell(ws.g_initial_string, "get-bond-category-relevance", bond_category),
                     tell(ws.g_initial_string, "get-average-intra-string-unhappiness")),
             average(tell(ws.g_modified_string, "get-bond-category-relevance", bond_category),
                     tell(ws.g_modified_string, "get-average-intra-string-unhappiness")),
             average(tell(ws.g_target_string, "get-bond-category-relevance", bond_category),
                     tell(ws.g_target_string, "get-average-intra-string-unhappiness")),
             (average(tell(ws.g_answer_string, "get-bond-category-relevance", bond_category),
                      tell(ws.g_answer_string, "get-average-intra-string-unhappiness"))
              if justify_p else 0)])
    object_ = tell(string, "choose-object", "get-intra-string-salience")
    if tell(object_, "spans-whole-string?") is not False:
        say("Chosen object spans whole string. Fizzling.")
        sugar.fizzle()
    if tell(object_, "leftmost-in-string?") is not False:
        direction_to_scan = s.plato_right
    elif tell(object_, "rightmost-in-string?") is not False:
        direction_to_scan = s.plato_left
    else:
        direction_to_scan = stochastic_pick_by_method([s.plato_right, s.plato_left],
                                                      "get-activation")
    number_to_scan = tell(string, "get-num-of-bonds-to-scan")
    if direction_to_scan is s.plato_left:
        initial_bond = tell(object_, "get-left-bond")
    else:
        initial_bond = tell(object_, "get-right-bond")
    if (exists_p(initial_bond)
            and tell(initial_bond, "get-bond-category") is bond_category):
        bonds = scan_bonds(number_to_scan, direction_to_scan, initial_bond)
        objects = [tell(first(bonds), "get-left-object")] + tell_all(bonds, "get-right-object")
        direction = tell(initial_bond, "get-direction")
        return propose_group(objects, bonds, group_category, direction)
    if group_p(object_):
        say("Can't make singleton group from a group. Fizzling.")
        sugar.fizzle()
    if group_category is s.plato_samegrp:
        singleton_direction = False
    else:
        # a two-binding let; descriptor-support only reads
        left_support = workspace_structure_formulas.descriptor_support(s.plato_left, string)
        right_support = workspace_structure_formulas.descriptor_support(s.plato_right, string)
        singleton_direction = stochastic_pick([s.plato_left, s.plato_right],
                                              [left_support, right_support])
    objects = [object_]
    bonds = []
    singleton_group = make_group(string, group_category, s.plato_letter_category,
                                 singleton_direction, object_, object_, objects, bonds)
    # chez: stochastic-if* draws its coin before the probability, which may draw
    # itself (get-local-support -> get-local-density) (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(
            workspace_structure_formulas.single_letter_group_probability(singleton_group)):
        say("Local support not strong enough. Fizzling.")
        sugar.fizzle()
    return propose_group(objects, bonds, group_category, singleton_direction)


def top_down_group_scout__direction(direction, scope):
    """groups.ss: top-down-group-scout:direction (the codelet procedure)"""
    s = slipnet
    if workspace_p(scope):
        say("Scope is entire Workspace.")
    else:
        say("Focusing on ", tell(scope, "generic-name"), "...")
    if workspace_string_p(scope):
        string = scope
    else:
        # the weights are built left to right; every element only reads
        ws = workspace
        justify_p = setup.p_justify_mode is not False
        string = stochastic_pick(
            ws.g_all_strings if justify_p else ws.g_non_answer_strings,
            [average(tell(ws.g_initial_string, "get-direction-relevance", direction),
                     tell(ws.g_initial_string, "get-average-intra-string-unhappiness")),
             average(tell(ws.g_modified_string, "get-direction-relevance", direction),
                     tell(ws.g_modified_string, "get-average-intra-string-unhappiness")),
             average(tell(ws.g_target_string, "get-direction-relevance", direction),
                     tell(ws.g_target_string, "get-average-intra-string-unhappiness")),
             (average(tell(ws.g_answer_string, "get-direction-relevance", direction),
                      tell(ws.g_answer_string, "get-average-intra-string-unhappiness"))
              if justify_p else 0)])
    object_ = tell(string, "choose-object", "get-intra-string-salience")
    if tell(object_, "spans-whole-string?") is not False:
        say("Chosen object spans whole string. Fizzling.")
        sugar.fizzle()
    if tell(object_, "leftmost-in-string?") is not False:
        direction_to_scan = s.plato_right
    elif tell(object_, "rightmost-in-string?") is not False:
        direction_to_scan = s.plato_left
    else:
        direction_to_scan = stochastic_pick_by_method([s.plato_right, s.plato_left],
                                                      "get-activation")
    number_to_scan = tell(string, "get-num-of-bonds-to-scan")
    if direction_to_scan is s.plato_left:
        initial_bond = tell(object_, "get-left-bond")
    else:
        initial_bond = tell(object_, "get-right-bond")
    if (not exists_p(initial_bond)
            or tell(initial_bond, "get-direction") is not direction):
        say("No appropriate bond in this direction. Fizzling.")
        sugar.fizzle()
    bond_facet = tell(initial_bond, "get-bond-facet")  # noqa: F841 (unused, as in 1.2)
    bond_category = tell(initial_bond, "get-bond-category")
    opposite_bond_category = tell(bond_category, "get-related-node", s.plato_opposite)  # noqa: F841
    opposite_direction = tell(direction, "get-related-node", s.plato_opposite)  # noqa: F841
    group_category = tell(bond_category, "get-related-node", s.plato_group_category)
    bonds = scan_bonds(number_to_scan, direction_to_scan, initial_bond)
    objects = [tell(first(bonds), "get-left-object")] + tell_all(bonds, "get-right-object")
    return propose_group(objects, bonds, group_category, direction)


def group_scout__whole_string():
    """groups.ss: group-scout:whole-string (the codelet procedure)"""
    s = slipnet
    # the weights are built left to right; every element only reads
    ws = workspace
    justify_p = setup.p_justify_mode is not False
    string = stochastic_pick(
        ws.g_all_strings if justify_p else ws.g_non_answer_strings,
        [tell(ws.g_initial_string, "get-average-intra-string-unhappiness"),
         tell(ws.g_modified_string, "get-average-intra-string-unhappiness"),
         tell(ws.g_target_string, "get-average-intra-string-unhappiness"),
         tell(ws.g_answer_string, "get-average-intra-string-unhappiness") if justify_p else 0])
    if len(tell(string, "get-bonds")) == 0:
        say("No bonds in chosen string. Fizzling.")
        sugar.fizzle()
    leftmost_object = tell(string, "choose-leftmost-object")
    right_bonds = right_adjacent_bonds(leftmost_object)
    bonded_objects = right_adjacent_objects(leftmost_object)
    if (len(right_bonds) == 0
            or tell(last(bonded_objects), "rightmost-in-string?") is False):
        say("Bonds do not span string. Fizzling.")
        sugar.fizzle()
    chosen_bond = random_pick(right_bonds)
    bond_facet = tell(chosen_bond, "get-bond-facet")
    bond_category = tell(chosen_bond, "get-bond-category")
    direction = tell(chosen_bond, "get-direction")
    polarized_bonds = polarize_bonds(right_bonds, bond_facet, bond_category, direction)
    if len(polarized_bonds) == 0:
        say("No possible group. Fizzling.")
        sugar.fizzle()
    group_category = tell(bond_category, "get-related-node", s.plato_group_category)
    return propose_group(bonded_objects, polarized_bonds, group_category, direction)


def group_evaluator(proposed_group):
    """groups.ss: group-evaluator (the codelet procedure)"""
    if setup.p_workspace_graphics is not False:
        _metacat.group_graphics.group_graphics("flash", proposed_group)
    tell(proposed_group, "update-strength")
    strength = tell(proposed_group, "get-strength")
    say("Evaluating group:")
    if setup.p_verbose is not False:
        print_(proposed_group)
    say("Strength of proposed group is ", strength)
    evaluation_prob = group_evaluation_probability(strength)
    say("Group evaluation probability of survival is ",
        chez.String(chez.format_("~a%", round_(times_100(evaluation_prob)))))
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(evaluation_prob):
        say("Group not strong enough. Fizzling.")
        tell(tell(proposed_group, "get-string"), "delete-proposed-group", proposed_group)
        if setup.p_workspace_graphics is not False:
            _metacat.group_graphics.group_graphics("erase", proposed_group)
        sugar.fizzle()
    tell(tell(proposed_group, "get-bond-category"), "activate-from-workspace")
    direction = tell(proposed_group, "get-direction")
    if exists_p(direction):
        tell(direction, "activate-from-workspace")
    tell(proposed_group, "update-proposal-level", workspace.p_evaluated)
    if setup.p_workspace_graphics is not False:
        _metacat.group_graphics.group_graphics("update-level", proposed_group)
    return sugar.post_codelet_star(strength, coderack.group_builder, proposed_group)


# Copycat has problems building weak groups in strings such as xwyxzy
# or abijxy, especially in the beginning of a run.  The following
# is a different evaluation test.  At higher temperatures, the
# probability is strongly boosted toward 1 for all but the lowest
# values of x.  At lower temperatures, the probability approaches
# a linear identity function.  Domain is 0..100, range is 0.0..1.0
# This function is equivalent to:
#   f(x) = T/100 * tanh(x/10) + (1 - T/100) * x/100

def group_evaluation_probability(x):
    """groups.ss: group-evaluation-probability"""
    temperature = setup.g_temperature
    return chez.add(
        chez.mul(percent(temperature),
                 chez.sub(chez.div(2, chez.add1(chez.exp(chez.div(chez.sub(x), 5)))), 1)),
        chez.mul(one_minus(percent(temperature)), percent(x)))


def group_builder(proposed_group):
    """groups.ss: group-builder (the codelet procedure)"""
    s = slipnet
    graphics_p = lambda: setup.p_workspace_graphics is not False  # noqa: E731
    group_graphics = lambda *args: _metacat.group_graphics.group_graphics(*args)  # noqa: E731
    # a let*: every binding reads
    string = tell(proposed_group, "get-string")
    group_category = tell(proposed_group, "get-group-category")
    direction = tell(proposed_group, "get-direction")
    equivalent_group = tell(string, "get-equivalent-group", proposed_group)
    constituent_bonds = tell(proposed_group, "get-constituent-bonds")
    constituent_objects = tell(proposed_group, "get-constituent-objects")
    tell(string, "delete-proposed-group", proposed_group)
    if exists_p(equivalent_group):
        say("This group already exists. Fizzling.")
        for description in tell(equivalent_group, "get-descriptions"):
            tell(tell(description, "get-descriptor"), "activate-from-workspace")
        for description in tell(proposed_group, "get-descriptions"):
            if tell(equivalent_group, "description-present?", description) is False:
                descriptions.build_description(
                    descriptions.make_description(
                        equivalent_group,
                        tell(description, "get-description-type"),
                        tell(description, "get-descriptor")))
        sugar.fizzle()
    if not chez.andmap(lambda bond: (tell(string, "bond-present?", bond) is not False
                                     or tell(string, "flipped-bond-present?", bond) is not False),
                       constituent_bonds):
        say("Not all the bonds in this group still exist. Fizzling.")
        if graphics_p():
            group_graphics("erase", proposed_group)
        sugar.fizzle()
    if graphics_p():
        group_graphics("flash", proposed_group)
    if group_category is s.plato_samegrp:
        bonds_to_be_flipped = []
    else:
        bonds_to_be_flipped = map_compress(
            lambda bond: tell(string, "get-equivalent-flipped-bond", bond),
            constituent_bonds)
    if (len(bonds_to_be_flipped) != 0
            and workspace_structures.wins_all_fights_p(
                proposed_group, tell(proposed_group, "get-letter-span"),
                bonds_to_be_flipped, 1) is False):
        say("Lost to existing bond. Fizzling.")
        if graphics_p():
            group_graphics("erase", proposed_group)
        sugar.fizzle()
    incompatible_groups = tell(proposed_group, "get-incompatible-groups")

    def fight(incompatible_group):
        if (tell(incompatible_group, "get-group-category") is group_category
                and tell(incompatible_group, "get-direction") is direction):
            return workspace_structures.wins_fight_p(
                proposed_group, tell(proposed_group, "get-group-length"),
                incompatible_group, tell(incompatible_group, "get-group-length"))
        return workspace_structures.wins_fight_p(proposed_group, 1, incompatible_group, 1)
    # chez: andmap goes first to last and stops at the first loss (and its draws)
    if len(incompatible_groups) != 0 and chez.andmap(fight, incompatible_groups) is False:
        say("Lost to incompatible group. Fizzling.")
        if graphics_p():
            group_graphics("erase", proposed_group)
        sugar.fizzle()
    # append's two arguments only read and make concept mappings (no side
    # effects), so the order of evaluation doesn't matter here
    incompatible_bridges = (tell(proposed_group, "get-incompatible-bridges", "vertical")
                            + tell(proposed_group, "get-incompatible-bridges", "horizontal"))
    if (len(incompatible_bridges) != 0
            and workspace_structures.wins_all_fights_p(
                proposed_group, 1, incompatible_bridges, 1) is False):
        say("Lost to incompatible bridge. Fizzling.")
        if graphics_p():
            group_graphics("erase", proposed_group)
        sugar.fizzle()
    say("Won against all incompatible structures.")
    for group in incompatible_groups:
        break_group(group)
    for bridge in incompatible_bridges:
        _metacat.bridges.break_bridge(bridge)

    # If a letter-sameness group is proposed that contains
    # other letter-sameness groups, then consolidate all
    # letters into one group.  A slightly better variation
    # would be to make this a probabilistic decision (not
    # sure how to bias it, though):
    if (group_category is s.plato_samegrp
            and tell(proposed_group, "get-bond-facet") is s.plato_letter_category
            and chez.ormap(group_p, constituent_objects) is not False):
        letters = tell(proposed_group, "get-letters")
        for group in filter_(group_p, constituent_objects):
            break_group(group)

        def letter_bond(l1, l2):
            bonds = _metacat.bonds
            if bonds.bonded_p(l1, l2) is not False:
                return tell(l1, "get-right-bond")
            new_bond = bonds.make_bond(l1, l2, s.plato_sameness, s.plato_letter_category,
                                       tell(l1, "get-letter-category"),
                                       tell(l2, "get-letter-category"))
            bonds.build_bond(new_bond)
            return new_bond
        # chez: adjacency-map is a two-list map, in Chez's order (build-bond has effects)
        letter_bonds = adjacency_map(letter_bond, letters)
        # 1.2: not gated by %workspace-graphics%
        group_graphics("erase", proposed_group)
        new_group_ = make_group(string, group_category, s.plato_letter_category, direction,
                                first(letters), last(letters), letters, letter_bonds)
        if tell(proposed_group, "description-type-present?", s.plato_length) is not False:
            attach_length_description(new_group_)
        proposed_group = new_group_

    elif (group_category is s.plato_samegrp
            and tell(proposed_group, "get-bond-facet") is s.plato_length
            and chez.ormap(length_group_p, constituent_objects) is not False):
        new_constituent_groups = flatmap(
            lambda group: (tell(group, "get-constituent-objects") if length_group_p(group)
                           else [group]),
            constituent_objects)
        for group in filter_(length_group_p, constituent_objects):
            break_group(group)
        if not all_same_p(tell_all(new_constituent_groups, "get-platonic-length")):
            say("New constituent groups have different lengths. Fizzling.")
            if graphics_p():
                group_graphics("erase", proposed_group)
            sugar.fizzle()

        def group_bond(g1, g2):
            bonds = _metacat.bonds
            if bonds.bonded_p(g1, g2) is not False:
                return tell(g1, "get-right-bond")
            new_bond = bonds.make_bond(g1, g2, s.plato_sameness, s.plato_length,
                                       tell(g1, "get-platonic-length"),
                                       tell(g2, "get-platonic-length"))
            bonds.build_bond(new_bond)
            return new_bond
        # chez: adjacency-map is a two-list map, in Chez's order (build-bond has effects)
        group_bonds = adjacency_map(group_bond, new_constituent_groups)
        # 1.2: not gated by %workspace-graphics%
        group_graphics("erase", proposed_group)
        new_group_ = make_group(string, group_category, s.plato_length, direction,
                                first(new_constituent_groups), last(new_constituent_groups),
                                new_constituent_groups, group_bonds)
        attach_length_description(new_group_)
        proposed_group = new_group_

    else:
        def new_bond(bond):
            if tell(string, "bond-present?", bond) is not False:
                return tell(string, "get-equivalent-bond", bond)
            flipped_bond = tell(string, "get-equivalent-flipped-bond", bond)
            _metacat.bonds.break_bond(flipped_bond)
            _metacat.bonds.build_bond(bond)
            return bond
        # chez: map's order of application (break-bond and build-bond have effects)
        new_bonds = chez.map_(new_bond, constituent_bonds)
        tell(proposed_group, "set-new-constituent-bonds", new_bonds)

    build_group(proposed_group, False)
    if graphics_p():
        return group_graphics("update-level", proposed_group)
    return None


def length_group_p(group):
    """groups.ss: length-group?"""
    return tell(group, "get-bond-facet") is slipnet.plato_length


# Propose-group assumes that objects are in string-position-order from left to right:

def propose_group(objects, bonds, group_category, direction):
    """groups.ss: propose-group"""
    s = slipnet
    say("Proposing group:")
    if setup.p_verbose is not False:
        print_(objects)
    left_object = first(objects)
    right_object = last(objects)
    string = tell(left_object, "get-string")
    bond_category = tell(group_category, "get-related-node", s.plato_bond_category)
    if len(bonds) == 0:
        group_bond_facet = s.plato_letter_category
    else:
        group_bond_facet = tell(first(bonds), "get-bond-facet")
    proposed_group = make_group(string, group_category, group_bond_facet, direction,
                                left_object, right_object, objects, bonds)
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < workspace_structure_formulas.length_description_probability(proposed_group):
        say("Attaching length ", len(objects), " description (if possible).")
        attach_length_description(proposed_group)
    tell(bond_category, "activate-from-workspace")
    if exists_p(direction):
        tell(direction, "activate-from-workspace")
    tell(string, "add-proposed-group", proposed_group)
    tell(proposed_group, "update-proposal-level", workspace.p_proposed)
    if setup.p_workspace_graphics is not False:
        _metacat.group_graphics.draw_group_grope(proposed_group)
        _metacat.group_graphics.group_graphics("set-pexp-and-draw", proposed_group)
    return sugar.post_codelet_star(_metacat.bonds.bond_degree_of_assoc(bond_category),
                                   coderack.group_evaluator, proposed_group)


def attach_length_description(group):
    """groups.ss: attach-length-description"""
    if not exists_p(tell(group, "get-descriptor-for", slipnet.plato_length)):
        platonic_length = tell(group, "get-platonic-length")
        # Check for groups longer than five objects:
        if exists_p(platonic_length):
            return tell(group, "new-description", slipnet.plato_length, platonic_length)
    return None


def right_adjacent_bonds(object_):
    """groups.ss: right-adjacent-bonds"""
    right_bond = tell(object_, "get-right-bond")
    if not exists_p(right_bond):
        return []
    return [right_bond] + right_adjacent_bonds(tell(right_bond, "get-right-object"))


def right_adjacent_objects(object_):
    """groups.ss: right-adjacent-objects"""
    right_bond = tell(object_, "get-right-bond")
    if not exists_p(right_bond):
        return [object_]
    return [object_] + right_adjacent_objects(tell(right_bond, "get-right-object"))


def polarize_bonds(bonds, bond_facet, bond_category, direction):
    """groups.ss: polarize-bonds"""
    s = slipnet

    def body(return_):
        def polarize(bond):
            # a three-binding let; the tells only read
            this_bond_facet = tell(bond, "get-bond-facet")
            this_bond_category = tell(bond, "get-bond-category")
            this_bond_direction = tell(bond, "get-direction")
            if this_bond_facet is not bond_facet:
                return return_([])
            if this_bond_category is bond_category and this_bond_direction is direction:
                return bond
            if (tell(this_bond_category, "get-related-node", s.plato_opposite) is bond_category
                    and tell(this_bond_direction, "get-related-node", s.plato_opposite)
                    is direction):
                return tell(bond, "make-flipped-version")
            return return_([])
        # chez: map's order of application (an escape can end it midway)
        return chez.map_(polarize, bonds)
    return sugar.continuation_point_star(body)


def scan_bonds(max_num_to_scan, direction_to_scan, initial_bond):
    """groups.ss: scan-bonds"""
    s = slipnet
    bond_facet = tell(initial_bond, "get-bond-facet")
    bond_category = tell(initial_bond, "get-bond-category")
    opposite_bond_category = tell(bond_category, "get-related-node", s.plato_opposite)
    direction = tell(initial_bond, "get-direction")
    opposite_direction = (tell(direction, "get-related-node", s.plato_opposite)
                          if exists_p(direction) else False)

    def scan(n, bond):
        # (cons x (scan ...)): make-flipped-version only allocates, so the
        # order of cons's arguments is not observable
        if n == 0 or not exists_p(bond):
            return []
        if (tell(bond, "get-bond-facet") is bond_facet
                and tell(bond, "get-bond-category") is bond_category
                and tell(bond, "get-direction") is direction):
            return [bond] + scan(chez.sub1(n), get_next_bond(bond, direction_to_scan))
        if (tell(bond, "get-bond-facet") is bond_facet
                and tell(bond, "get-bond-category") is opposite_bond_category
                and tell(bond, "get-direction") is opposite_direction):
            return ([tell(bond, "make-flipped-version")]
                    + scan(chez.sub1(n), get_next_bond(bond, direction_to_scan)))
        return []
    if direction_to_scan is s.plato_right:
        return scan(max_num_to_scan, initial_bond)
    return list(reversed(scan(max_num_to_scan, initial_bond)))


def get_next_bond(bond, direction_to_scan):
    """groups.ss: get-next-bond"""
    if direction_to_scan is slipnet.plato_left:
        return tell(tell(bond, "get-left-object"), "get-left-bond")
    if direction_to_scan is slipnet.plato_right:
        return tell(tell(bond, "get-right-object"), "get-right-bond")
    return None


def build_group(proposed_group, flipped_p):
    """groups.ss: build-group"""
    vprintf("Building ~agroup:~n", "flipped " if flipped_p is not False else "")
    vprint(proposed_group)
    string = tell(proposed_group, "get-string")
    tell(string, "add-group", proposed_group)
    for object_ in tell(proposed_group, "get-constituent-objects"):
        tell(object_, "update-enclosing-group", proposed_group)
    for bond in tell(proposed_group, "get-constituent-bonds"):
        tell(bond, "update-enclosing-group", proposed_group)
    for description in tell(proposed_group, "get-descriptions"):
        tell(tell(description, "get-descriptor"), "activate-from-workspace")
    tell(proposed_group, "update-proposal-level", workspace.p_built)
    # Building a non-spanning group may invalidate StrPosCtgy:middle descriptions
    # for some objects in the string.  Need to remove such descriptions from any
    # objects that are no longer describable as "middle":
    if tell(proposed_group, "spans-whole-string?") is False:
        tell(string, "delete-invalid-string-position-middle-descriptions")
    if (setup.p_workspace_graphics is not False
            and tell(proposed_group, "get-letter-span") == 2):
        window = setup.g_workspace_window
        left_object = tell(proposed_group, "get-leftmost-object")
        right_object = tell(proposed_group, "get-rightmost-object")
        tell(window, "caching-on")
        if tell(left_object, "singleton-group?") is not False:
            tell(window, "erase-group", left_object)
            tell(left_object, "set-shrunk-singleton?", True)
            tell(window, "draw-group", left_object)
        if tell(right_object, "singleton-group?") is not False:
            tell(window, "erase-group", right_object)
            tell(right_object, "set-shrunk-singleton?", True)
            tell(window, "draw-group", right_object)
        tell(window, "flush")
    return _metacat.trace.monitor_new_groups(proposed_group, flipped_p)


def break_group(group):
    """groups.ss: break-group"""
    say("Breaking group:")
    if setup.p_verbose is not False:
        print_(group)
    # a four-binding let; every binding only reads
    string = tell(group, "get-string")
    vertical_bridge = tell(group, "get-bridge", "vertical")
    horizontal_bridge = tell(group, "get-bridge", "horizontal")
    enclosing_group = tell(group, "get-enclosing-group")
    if exists_p(enclosing_group):
        break_group(enclosing_group)
    tell(string, "delete-group", group)
    tell(string, "delete-proposed-bonds", group)
    for bond in tell(group, "get-incident-bonds"):
        _metacat.bonds.break_bond(bond)
    if setup.p_workspace_graphics is not False:
        window = setup.g_workspace_window
        tell(window, "caching-on")
        for v in tell(workspace.g_workspace, "get-proposed-vertical-bridges", group):
            if tell(v, "drawn?") is not False:
                # Can't use (bridge-graphics 'erase ...) here because
                # we don't want any pending bridges to be re-drawn.
                # Also, probably should repair damage to bridge's other object.
                tell(window, "erase-bridge", v)
        for h in tell(workspace.g_workspace, "get-proposed-horizontal-bridges", group):
            if tell(h, "drawn?") is not False:
                tell(window, "erase-bridge", h)
        tell(window, "flush")
    tell(workspace.g_workspace, "delete-proposed-vertical-bridges", group)
    tell(workspace.g_workspace, "delete-proposed-horizontal-bridges", group)
    if exists_p(vertical_bridge):
        _metacat.bridges.break_bridge(vertical_bridge)
    if exists_p(horizontal_bridge):
        _metacat.bridges.break_bridge(horizontal_bridge)
    for object_ in tell(group, "get-constituent-objects"):
        tell(object_, "update-enclosing-group", False)
    for bond in tell(group, "get-constituent-bonds"):
        tell(bond, "update-enclosing-group", False)
    # Breaking a non-spanning group may invalidate StrPosCtgy:middle descriptions
    # for some objects in the string.  Need to remove such descriptions from any
    # objects that are no longer describable as "middle":
    if tell(group, "spans-whole-string?") is False:
        tell(string, "delete-invalid-string-position-middle-descriptions")
    if setup.p_workspace_graphics is not False:
        _metacat.group_graphics.group_graphics("erase", group)
        if tell(group, "get-letter-span") == 2:
            window = setup.g_workspace_window
            left_object = tell(group, "get-leftmost-object")
            right_object = tell(group, "get-rightmost-object")
            tell(window, "caching-on")
            if tell(left_object, "singleton-group?") is not False:
                tell(window, "erase-group", left_object)
                tell(left_object, "set-shrunk-singleton?", False)
                tell(window, "draw-group", left_object)
                # Repair any damage done to workspace letter:
                tell(window, "draw",
                     tell(tell(left_object, "get-leftmost-object"), "get-graphics-pexp"))
            if tell(right_object, "singleton-group?") is not False:
                tell(window, "erase-group", right_object)
                tell(right_object, "set-shrunk-singleton?", False)
                tell(window, "draw-group", right_object)
                # Repair any damage done to workspace letter:
                tell(window, "draw",
                     tell(tell(right_object, "get-leftmost-object"), "get-graphics-pexp"))
            tell(window, "flush")
    return "done"


def contains_p(object1, object2):
    """groups.ss: contains?"""
    return group_p(object1) and tell(object1, "nested-member?", object2)


def get_common_groups(object1, object2):
    """groups.ss: get-common-groups"""
    return filter_(lambda group: (tell(group, "nested-member?", object1) is not False
                                  and tell(group, "nested-member?", object2)),
                   tell(tell(object1, "get-string"), "get-groups"))


def same_group_category_p(group1, group2):
    """groups.ss: same-group-category?"""
    return tell(group1, "get-group-category") is tell(group2, "get-group-category")


def same_group_direction_p(group1, group2):
    """groups.ss: same-group-direction?"""
    return tell(group1, "get-direction") is tell(group2, "get-direction")


def same_letter_category_p(object1, object2):
    """groups.ss: same-letter-category?"""
    return tell(object1, "get-letter-category") is tell(object2, "get-letter-category")


def directed_group_p(object_):
    """groups.ss: directed-group?"""
    if not group_p(object_):
        return False
    group_category = tell(object_, "get-group-category")
    return (group_category is slipnet.plato_succgrp
            or group_category is slipnet.plato_predgrp)


def get_all_nested_groups(object_):
    """groups.ss: get-all-nested-groups"""
    if letter_p(object_):
        return []
    out = [object_]
    # chez: map's order of application (the procedure only reads)
    for part in chez.map_(get_all_nested_groups, tell(object_, "get-constituent-objects")):
        out.extend(part)
    return out


def load():
    """groups.ss: the five define-codelet-procedure* forms, in the file's order."""
    sugar.define_codelet_procedure_star("top-down-group-scout:category",
                                        top_down_group_scout__category)
    sugar.define_codelet_procedure_star("top-down-group-scout:direction",
                                        top_down_group_scout__direction)
    sugar.define_codelet_procedure_star("group-scout:whole-string", group_scout__whole_string)
    sugar.define_codelet_procedure_star("group-evaluator", group_evaluator)
    sugar.define_codelet_procedure_star("group-builder", group_builder)
