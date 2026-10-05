"""workspace-strings.ss: the workspace strings (initial, modified, target, answer).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from workspace-strings.ss, with
racket/engine/workspace-strings.rktl as a worked translation.

new-workspace-string's closure is WorkspaceString; it delegates to base-object.
Its vectors and tables are chez.Vectors (utilities.make_table), indexed by the
objects' id numbers and string positions and grown by expand-internal-storage;
the lists it hands out (letters, groups, bonds, proposed groups, the edge
groups and proposed-bond cells) are Python lists, rebuilt on every cons and
remq, never mutated.  print-name, ascii-name and generic-name are
chez.Strings; symbol-name and the string types are plain str (symbols).  The
bonds, groups, bridges and rules predicates are read through the package at
call time.  The engine never imports tkinter.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez
from metacat import constants, formulas, images, setup, slipnet, workspace, workspace_objects
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (ascending_index_list, base_object, compress, copy_table_contents_bang,
                               copy_vector_contents_bang, count, exists_p, filter_, filter_meth,
                               filter_out, first, flatten, initialize_column_bang,
                               initialize_row_bang, letter_p, make_table, member_p, ormap_meth,
                               random_pick, round_, average, second, select_meth, sort_by_method,
                               square, stochastic_pick, stochastic_pick_by_method, sum_,
                               symbol_to_letter_categories, table_ref, table_set_bang,
                               table_to_list, tell_all, third, times_100)


def make_workspace_string(string_type, symbol):
    """workspace-strings.ss: make-workspace-string"""
    letter_categories = symbol_to_letter_categories(symbol)
    string = new_workspace_string(string_type, letter_categories)
    # for* position from 0 to (sub1 n): inclusive bounds, first to last
    for position in range(0, len(letter_categories)):
        tell(string, "add-letter",
             workspace_objects.make_letter(string, letter_categories[position], position),
             position)
    tell(string, "set-letter-list")
    tell(string, "set-string-image", images.make_string_image(string, slipnet.plato_right))
    return string


def _lt(a, b):
    """Scheme's < as a two-argument procedure."""
    return a < b


class WorkspaceString(SchemeObject):
    """workspace-strings.ss: new-workspace-string (the closure)"""
    __slots__ = ("string_type", "letter_categories", "number_of_letters", "max_object_capacity",
                 "next_id_num", "letter_vector", "left_edge_group_vector",
                 "right_edge_group_vector", "group_vector", "proposed_group_table",
                 "from_to_bond_table", "left_right_bond_table", "proposed_bond_table",
                 "letter_list", "group_list", "proposed_group_list", "bond_list",
                 "average_intra_string_unhappiness", "bond_scan_distribution",
                 "translated_p", "string_image", "print_name", "group_enclosure_x_delta",
                 "group_enclosure_y_delta", "graphics_width", "spanning_group_x1",
                 "spanning_group_y1", "spanning_group_x2", "spanning_group_y2")

    def __init__(this, string_type, letter_categories):
        this.string_type = string_type
        this.letter_categories = letter_categories
        # chez: let* order
        this.number_of_letters = len(letter_categories)
        this.max_object_capacity = 2 * this.number_of_letters
        this.next_id_num = 0
        this.letter_vector = chez.make_vector(this.number_of_letters)
        this.left_edge_group_vector = chez.make_vector(this.number_of_letters, [])
        this.right_edge_group_vector = chez.make_vector(this.number_of_letters, [])
        this.group_vector = chez.make_vector(this.max_object_capacity, False)
        this.proposed_group_table = make_table(this.max_object_capacity,
                                               this.max_object_capacity, [])
        this.from_to_bond_table = make_table(this.max_object_capacity,
                                             this.max_object_capacity, False)
        this.left_right_bond_table = make_table(this.max_object_capacity,
                                                this.max_object_capacity, False)
        this.proposed_bond_table = make_table(this.max_object_capacity,
                                              this.max_object_capacity, [])
        this.letter_list = False
        this.group_list = []
        this.proposed_group_list = []
        this.bond_list = []
        this.average_intra_string_unhappiness = False
        # 1.2: (ascending-index-list 0) loops forever, so an empty string never gets here
        this.bond_scan_distribution = constants.make_probability_distribution(
            ascending_index_list(this.number_of_letters),
            [square(i) for i in ascending_index_list(this.number_of_letters)])
        this.translated_p = False
        this.string_image = False
        this.print_name = False
        this.group_enclosure_x_delta = False
        this.group_enclosure_y_delta = False
        this.graphics_width = False
        this.spanning_group_x1 = False
        this.spanning_group_y1 = False
        this.spanning_group_x2 = False
        this.spanning_group_y2 = False

    @message("object-type")
    def object_type(this, self):
        return "workspace-string"

    @message("print-name")
    def print_name_(this, self):
        return this.print_name

    @message("ascii-name")
    def ascii_name(this, self):
        return chez.String(chez.format_('"~a"', this.print_name))

    @message("generic-name")
    def generic_name(this, self):
        return chez.String(chez.format_("~a~a string",
                                        "translated " if this.translated_p else "",
                                        this.string_type))

    @message("symbol-name")
    def symbol_name(this, self):
        return str(this.print_name)

    @message("print")
    def print_(this, self):
        return chez.printf('~aWorkspace string "~a"~%',
                           "Translated " if this.translated_p else "",
                           this.print_name)

    @message("get-group-enclosure-x-delta")
    def get_group_enclosure_x_delta(this, self):
        return this.group_enclosure_x_delta

    @message("get-group-enclosure-y-delta")
    def get_group_enclosure_y_delta(this, self):
        return this.group_enclosure_y_delta

    @message("get-graphics-width")
    def get_graphics_width(this, self):
        return this.graphics_width

    @message("get-spanning-group-x1")
    def get_spanning_group_x1(this, self):
        return this.spanning_group_x1

    @message("get-spanning-group-y1")
    def get_spanning_group_y1(this, self):
        return this.spanning_group_y1

    @message("get-spanning-group-x2")
    def get_spanning_group_x2(this, self):
        return this.spanning_group_x2

    @message("get-spanning-group-y2")
    def get_spanning_group_y2(this, self):
        return this.spanning_group_y2

    @message("set-graphics-info")
    def set_graphics_info(this, self, x_delta, y_delta, width, span_x1, span_y1, span_x2, span_y2):
        this.group_enclosure_x_delta = x_delta
        this.group_enclosure_y_delta = y_delta
        this.graphics_width = width
        this.spanning_group_x1 = span_x1
        this.spanning_group_y1 = span_y1
        this.spanning_group_x2 = span_x2
        this.spanning_group_y2 = span_y2
        return "done"

    @message("get-max-object-capacity")
    def get_max_object_capacity(this, self):
        return this.max_object_capacity

    @message("get-random-letter")
    def get_random_letter(this, self):
        return random_pick(this.letter_list)

    @message("get-letter")
    def get_letter(this, self, position):
        return this.letter_vector[position]

    @message("get-letter-categories")
    def get_letter_categories(this, self):
        return this.letter_categories

    @message("get-left-edged-groups")
    def get_left_edged_groups(this, self, position):
        return this.left_edge_group_vector[position]

    @message("get-right-edged-groups")
    def get_right_edged_groups(this, self, position):
        return this.right_edge_group_vector[position]

    @message("get-length")
    def get_length(this, self):
        return this.number_of_letters

    @message("get-letters")
    def get_letters(this, self):
        return this.letter_list

    @message("get-groups")
    def get_groups(this, self):
        return this.group_list

    @message("get-all-groups")
    def get_all_groups(this, self):
        return this.group_list + this.proposed_group_list

    @message("get-bonds")
    def get_bonds(this, self):
        return this.bond_list

    @message("get-all-bonds")
    def get_all_bonds(this, self):
        return this.bond_list + [bond for cell in table_to_list(this.proposed_bond_table)
                                 for bond in cell]

    @message("get-objects")
    def get_objects(this, self):
        return this.letter_list + this.group_list

    @message("get-all-objects")
    def get_all_objects(this, self):
        return this.letter_list + this.group_list + this.proposed_group_list

    @message("get-average-intra-string-unhappiness")
    def get_average_intra_string_unhappiness(this, self):
        return this.average_intra_string_unhappiness

    @message("get-num-of-bonds-to-scan")
    def get_num_of_bonds_to_scan(this, self):
        return tell(this.bond_scan_distribution, "choose-value")

    @message("get-string-type")
    def get_string_type(this, self):
        return this.string_type

    @message("string-type?")
    def string_type_p(this, self, type_):
        return this.string_type == type_

    @message("top-string?")
    def top_string_p(this, self):
        return member_p(this.string_type, ["initial", "modified"])

    @message("vertical-string?")
    def vertical_string_p(this, self):
        return member_p(this.string_type, ["initial", "target"])

    @message("bottom-string?")
    def bottom_string_p(this, self):
        return member_p(this.string_type, ["target", "answer"])

    @message("translated?")
    def translated_p_(this, self):
        return this.translated_p

    @message("mark-as-translated")
    def mark_as_translated(this, self):
        this.translated_p = True
        return "done"

    @message("bond-present?")
    def bond_present_p(this, self, bond):
        return exists_p(tell(self, "get-equivalent-bond", bond))

    @message("get-equivalent-bond")
    def get_equivalent_bond(this, self, bond):
        i = tell(tell(bond, "get-from-object"), "get-id-num")
        j = tell(tell(bond, "get-to-object"), "get-id-num")
        equivalent_bond = table_ref(this.from_to_bond_table, i, j)
        if (exists_p(equivalent_bond)
                and _metacat.bonds.same_bond_category_p(bond, equivalent_bond) is not False
                and _metacat.bonds.same_bond_direction_p(bond, equivalent_bond) is not False):
            return equivalent_bond
        return False

    @message("flipped-bond-present?")
    def flipped_bond_present_p(this, self, bond):
        return exists_p(tell(self, "get-equivalent-flipped-bond", bond))

    @message("get-equivalent-flipped-bond")
    def get_equivalent_flipped_bond(this, self, bond):
        i = tell(tell(bond, "get-from-object"), "get-id-num")
        j = tell(tell(bond, "get-to-object"), "get-id-num")
        equivalent_flipped_bond = table_ref(this.from_to_bond_table, j, i)
        if (exists_p(equivalent_flipped_bond)
                and _metacat.bonds.opposite_bond_category_p(bond, equivalent_flipped_bond)
                is not False
                and _metacat.bonds.opposite_bond_direction_p(bond, equivalent_flipped_bond)
                is not False):
            return equivalent_flipped_bond
        return False

    @message("assign-id-num")
    def assign_id_num(this, self, object_):
        tell(object_, "set-id-num", this.next_id_num)
        this.next_id_num = this.next_id_num + 1
        if this.next_id_num == this.max_object_capacity:
            tell(self, "expand-internal-storage")
        return "done"

    @message("add-letter")
    def add_letter(this, self, letter, position):
        tell(self, "assign-id-num", letter)
        this.letter_vector[position] = letter
        return "done"

    @message("set-letter-list")
    def set_letter_list(this, self):
        this.letter_list = list(this.letter_vector)
        this.print_name = chez.String("".join(tell_all(this.letter_list, "print-name")))
        return "done"

    @message("add-bond")
    def add_bond(this, self, bond):
        i1 = tell(tell(bond, "get-from-object"), "get-id-num")
        j1 = tell(tell(bond, "get-to-object"), "get-id-num")
        i2 = tell(tell(bond, "get-left-object"), "get-id-num")
        j2 = tell(tell(bond, "get-right-object"), "get-id-num")
        table_set_bang(this.from_to_bond_table, i1, j1, bond)
        table_set_bang(this.left_right_bond_table, i2, j2, bond)
        if tell(bond, "get-bond-category") is slipnet.plato_sameness:
            table_set_bang(this.from_to_bond_table, j1, i1, bond)
            table_set_bang(this.left_right_bond_table, j2, i2, bond)
        this.bond_list = [bond] + this.bond_list
        return "done"

    @message("delete-bond")
    def delete_bond(this, self, bond):
        i1 = tell(tell(bond, "get-from-object"), "get-id-num")
        j1 = tell(tell(bond, "get-to-object"), "get-id-num")
        i2 = tell(tell(bond, "get-left-object"), "get-id-num")
        j2 = tell(tell(bond, "get-right-object"), "get-id-num")
        table_set_bang(this.from_to_bond_table, i1, j1, False)
        table_set_bang(this.left_right_bond_table, i2, j2, False)
        if tell(bond, "get-bond-category") is slipnet.plato_sameness:
            table_set_bang(this.from_to_bond_table, j1, i1, False)
            table_set_bang(this.left_right_bond_table, j2, i2, False)
        this.bond_list = chez.remq(bond, this.bond_list)
        return "done"

    @message("add-proposed-bond")
    def add_proposed_bond(this, self, bond):
        i = tell(tell(bond, "get-from-object"), "get-id-num")
        j = tell(tell(bond, "get-to-object"), "get-id-num")
        proposed_bonds = table_ref(this.proposed_bond_table, i, j)
        table_set_bang(this.proposed_bond_table, i, j, [bond] + proposed_bonds)
        return "done"

    @message("delete-proposed-bond")
    def delete_proposed_bond(this, self, bond):
        i = tell(tell(bond, "get-from-object"), "get-id-num")
        j = tell(tell(bond, "get-to-object"), "get-id-num")
        proposed_bonds = table_ref(this.proposed_bond_table, i, j)
        table_set_bang(this.proposed_bond_table, i, j, chez.remq(bond, proposed_bonds))
        return "done"

    @message("delete-proposed-bonds")
    def delete_proposed_bonds(this, self, object_):
        i = tell(object_, "get-id-num")
        initialize_row_bang(this.proposed_bond_table, i, [])
        initialize_column_bang(this.proposed_bond_table, i, [])
        return "done"

    @message("delete-all-proposed-bonds")
    def delete_all_proposed_bonds(this, self):
        # 1.2: for-each-vector-element* loops forever on an empty table (ascending-index-list 0)
        for i in ascending_index_list(len(this.proposed_bond_table)):
            initialize_row_bang(this.proposed_bond_table, i, [])
        return "done"

    @message("group-present?")
    def group_present_p(this, self, group):
        return exists_p(tell(self, "get-equivalent-group", group))

    @message("get-equivalent-object")
    def get_equivalent_object(this, self, object_):
        if letter_p(object_):
            return tell(self, "get-equivalent-letter", object_)
        return tell(self, "get-equivalent-group", object_)

    # get-equivalent-letter and get-equivalent-group assume that self and
    # (tell letter/group 'get-string) are strings consisting of exactly the same
    # letter-categories.  These strings may or may not be eq? (i.e., one could
    # be a translated-string).  (Marshall's comment.)
    @message("get-equivalent-letter")
    def get_equivalent_letter(this, self, letter):
        if member_p(letter, this.letter_list):
            return letter
        equivalent_letter = this.letter_vector[tell(letter, "get-string-pos")]
        if tell(letter, "get-letter-category") is tell(equivalent_letter, "get-letter-category"):
            return equivalent_letter
        return False

    @message("get-equivalent-group")
    def get_equivalent_group(this, self, group):
        if member_p(group, this.group_list):
            return group
        i = tell(tell(group, "get-leftmost-object"), "get-id-num")
        if i >= this.max_object_capacity:
            return False
        equivalent_group = this.group_vector[i]
        if (exists_p(equivalent_group)
                and _metacat.groups.same_group_category_p(group, equivalent_group) is not False
                and _metacat.groups.same_group_direction_p(group, equivalent_group) is not False
                and tell(group, "get-group-length") == tell(equivalent_group, "get-group-length")):
            return equivalent_group
        return False

    @message("get-all-other-coincident-groups")
    def get_all_other_coincident_groups(this, self, group, left_pos, right_pos, direction):
        def coincident_p(g):
            return (g is not group
                    and left_pos == tell(g, "get-left-string-pos")
                    and right_pos == tell(g, "get-right-string-pos")
                    and (direction is tell(g, "get-direction")
                         # This is for the extremely unlikely but theoretically
                         # possible case of a sameness group <=> left-directed
                         # group coincidence:
                         or (direction is not slipnet.plato_right
                             and tell(g, "get-direction") is not slipnet.plato_right)))
        return filter_(coincident_p, this.group_list + this.proposed_group_list)

    @message("add-group")
    def add_group(this, self, group):
        tell(self, "assign-id-num", group)
        left_pos = tell(group, "get-left-string-pos")
        right_pos = tell(group, "get-right-string-pos")
        left_edge_groups = this.left_edge_group_vector[left_pos]
        right_edge_groups = this.right_edge_group_vector[right_pos]
        i = tell(tell(group, "get-leftmost-object"), "get-id-num")
        this.group_vector[i] = group
        this.left_edge_group_vector[left_pos] = [group] + left_edge_groups
        this.right_edge_group_vector[right_pos] = [group] + right_edge_groups
        this.group_list = [group] + this.group_list
        return "done"

    @message("delete-group")
    def delete_group(this, self, group):
        left_pos = tell(group, "get-left-string-pos")
        right_pos = tell(group, "get-right-string-pos")
        left_edge_groups = this.left_edge_group_vector[left_pos]
        right_edge_groups = this.right_edge_group_vector[right_pos]
        i = tell(tell(group, "get-leftmost-object"), "get-id-num")
        this.group_vector[i] = False
        this.left_edge_group_vector[left_pos] = chez.remq(group, left_edge_groups)
        this.right_edge_group_vector[right_pos] = chez.remq(group, right_edge_groups)
        this.group_list = chez.remq(group, this.group_list)
        return "done"

    @message("add-proposed-group")
    def add_proposed_group(this, self, group):
        i = tell(tell(group, "get-leftmost-object"), "get-id-num")
        j = tell(tell(group, "get-rightmost-object"), "get-id-num")
        proposed_groups = table_ref(this.proposed_group_table, i, j)
        table_set_bang(this.proposed_group_table, i, j, [group] + proposed_groups)
        this.proposed_group_list = [group] + this.proposed_group_list
        return "done"

    @message("delete-proposed-group")
    def delete_proposed_group(this, self, group):
        i = tell(tell(group, "get-leftmost-object"), "get-id-num")
        j = tell(tell(group, "get-rightmost-object"), "get-id-num")
        proposed_groups = table_ref(this.proposed_group_table, i, j)
        table_set_bang(this.proposed_group_table, i, j, chez.remq(group, proposed_groups))
        this.proposed_group_list = chez.remq(group, this.proposed_group_list)
        return "done"

    @message("delete-all-proposed-groups")
    def delete_all_proposed_groups(this, self):
        # 1.2: for-each-vector-element* loops forever on an empty table (ascending-index-list 0)
        for i in ascending_index_list(len(this.proposed_group_table)):
            initialize_row_bang(this.proposed_group_table, i, [])
        this.proposed_group_list = []
        return "done"

    @message("delete-invalid-string-position-middle-descriptions")
    def delete_invalid_string_position_middle_descriptions(this, self):
        for object_ in tell(self, "get-all-objects"):
            if (tell(object_, "descriptor-present?", slipnet.plato_middle) is not False
                    and tell(object_, "middle-in-string?") is False):
                tell(object_, "delete-description-type", slipnet.plato_string_position_category)
                vertical_bridge = tell(object_, "get-bridge", "vertical")
                horizontal_bridge = tell(object_, "get-bridge", "horizontal")
                if (exists_p(vertical_bridge)
                        and tell(vertical_bridge, "CM-type-present?",
                                 slipnet.plato_string_position_category) is not False):
                    tell(vertical_bridge, "delete-concept-mapping-type",
                         slipnet.plato_string_position_category)
                    if len(tell(vertical_bridge, "get-all-concept-mappings")) == 0:
                        _metacat.bridges.break_bridge(vertical_bridge)
                if (exists_p(horizontal_bridge)
                        and tell(horizontal_bridge, "CM-type-present?",
                                 slipnet.plato_string_position_category) is not False):
                    tell(horizontal_bridge, "delete-concept-mapping-type",
                         slipnet.plato_string_position_category)
                    if len(tell(horizontal_bridge, "get-all-concept-mappings")) == 0:
                        _metacat.bridges.break_bridge(horizontal_bridge)
        return "done"

    @message("update-all-relative-importances")
    def update_all_relative_importances(this, self):
        objects = tell(self, "get-objects")
        total_raw_importance = sum_(tell_all(objects, "get-raw-importance"))
        if total_raw_importance == 0:
            importance = times_100(chez.div(1, len(objects)))
            for object_ in objects:
                tell(object_, "update-relative-importance", importance)
        else:
            for object_ in objects:
                raw_importance = tell(object_, "get-raw-importance")
                tell(object_, "update-relative-importance",
                     times_100(chez.div(raw_importance, total_raw_importance)))
        return "done"

    @message("update-average-intra-string-unhappiness")
    def update_average_intra_string_unhappiness(this, self):
        this.average_intra_string_unhappiness = round_(
            average(tell_all(tell(self, "get-objects"), "get-intra-string-unhappiness")))
        return "done"

    @message("choose-object")
    def choose_object(this, self, *message):
        objects = tell(self, "get-objects")
        weights = tell_all(objects, *message)
        return stochastic_pick(objects, formulas.temp_adjusted_values(weights))

    @message("choose-object-with-description-type")
    def choose_object_with_description_type(this, self, description_type, *message):
        objects = filter_meth(tell(self, "get-objects"), "description-type-present?",
                              description_type)
        # chez: the weights are computed (with tell-all's order) before the null? test
        weights = tell_all(objects, *message)
        if len(objects) == 0:
            return False
        return stochastic_pick(objects, formulas.temp_adjusted_values(weights))

    @message("choose-leftmost-object")
    def choose_leftmost_object(this, self):
        leftmost_objects = filter_(
            lambda object_: (tell(object_, "get-descriptor-for",
                                  slipnet.plato_string_position_category)
                             is slipnet.plato_leftmost),
            tell(self, "get-objects"))
        return stochastic_pick_by_method(leftmost_objects, "get-relative-importance")

    @message("get-relevance")
    def get_relevance(this, self, get_category_method_name, category):
        non_spanning_objects = filter_out(lambda object_: tell(object_, "spans-whole-string?"),
                                          tell(self, "get-objects"))
        if len(non_spanning_objects) == 0:
            return 0

        def bonded_p(object_):
            right_bond = tell(object_, "get-right-bond")
            return (exists_p(right_bond)
                    and tell(right_bond, get_category_method_name) is category)
        num_of_bonded_objects = count(bonded_p, non_spanning_objects)
        # 1.2: a single non-spanning object divides by zero, as in Chez
        return times_100(chez.div(num_of_bonded_objects,
                                  chez.sub1(len(non_spanning_objects))))

    @message("get-bond-category-relevance")
    def get_bond_category_relevance(this, self, bond_category):
        return tell(self, "get-relevance", "get-bond-category", bond_category)

    @message("get-direction-relevance")
    def get_direction_relevance(this, self, direction):
        return tell(self, "get-relevance", "get-direction", direction)

    @message("get-image")
    def get_image(this, self):
        return this.string_image

    @message("set-string-image")
    def set_string_image(this, self, image):
        this.string_image = image
        return "done"

    @message("generate-image-letters")
    def generate_image_letters(this, self):
        return flatten(tell(this.string_image, "generate"))

    @message("reset-string-image")
    def reset_string_image(this, self):
        return tell(this.string_image, "reset")

    # -----------------------------------------------------------------------
    # The following methods allow workspace-strings to behave as spanning
    # groups, so that a workspace-string can be used as the reference-object of
    # an intrinsic change-description or rule-clause-template.  (Marshall's comment.)
    @message("get-bridge")
    def get_bridge(this, self, bridge_type):
        return False

    @message("get-constituent-objects")
    def get_constituent_objects(this, self):
        # chez: Chez's sort (sort-by-method)
        return sort_by_method("get-left-string-pos", _lt, tell(self, "get-top-level-objects"))

    @message("get-top-level-objects")
    def get_top_level_objects(this, self):
        return filter_out(lambda obj: exists_p(tell(obj, "get-enclosing-group")),
                          this.letter_list + this.group_list)

    @message("nested-member?")
    def nested_member_p(this, self, object_):
        top_level_objects = tell(self, "get-top-level-objects")
        return (member_p(object_, top_level_objects)
                or ormap_meth(top_level_objects, "nested-member?", object_))

    @message("singleton-group?")
    def singleton_group_p(this, self):
        return len(tell(self, "get-top-level-objects")) == 1

    @message("get-subobject-bridges")
    def get_subobject_bridges(this, self, bridge_orientation):
        return compress(tell_all(tell(self, "get-top-level-objects"),
                                 "get-bridge", bridge_orientation))

    @message("get-left-string-pos")
    def get_left_string_pos(this, self):
        return 0

    @message("get-right-string-pos")
    def get_right_string_pos(this, self):
        return this.number_of_letters - 1

    @message("get-nesting-level")
    def get_nesting_level(this, self):
        return 0

    @message("get-bond-facet")
    def get_bond_facet(this, self):
        return slipnet.plato_letter_category

    # -----------------------------------------------------------------------
    @message("spanning-group-exists?")
    def spanning_group_exists_p(this, self):
        return ormap_meth(this.group_list, "spans-whole-string?")

    @message("get-spanning-group")
    def get_spanning_group(this, self):
        return select_meth(this.group_list, "spans-whole-string?")

    @message("get-all-reference-objects")
    def get_all_reference_objects(this, self, rule):
        # chez: map's order of application
        lists = chez.map_(lambda rc: tell(self, "get-reference-objects", rc),
                          tell(rule, "get-rule-clauses"))
        return [x for l in lists for x in l]

    @message("get-reference-objects")
    def get_reference_objects(this, self, rule_clause):
        if _metacat.rules.verbatim_clause_p(rule_clause) is not False:
            return []
        # chez: map's order of application
        lists = chez.map_(lambda od: tell(self, "get-object-description-ref-objects", od),
                          second(rule_clause))
        return [x for l in lists for x in l]

    @message("whole-group?")
    def whole_group_p(this, self):
        return exists_p(select_meth(this.group_list, "descriptor-present?", slipnet.plato_whole))

    @message("get-object-description-ref-objects")
    def get_object_description_ref_objects(this, self, object_description):
        object_type = first(object_description)
        description_type = second(object_description)
        descriptor = third(object_description)
        if chez.eq_p(object_type, "string"):
            # In abc->aaa, if group [abc] doesn't exist when a rule is created,
            # rule will be CHANGE (string <StrPos> <whole>) (subobjs <LetCat> <a>)
            # but then if [abc] is subsequently created, the rule will no longer
            # work, since string "abc" is retrieved, and subobjects are ([abc]).
            # Hence, for a "string" rule, need to retrieve the whole group if one
            # exists (else just retrieve the string).  (Marshall's comment.)
            whole_group = select_meth(this.group_list, "descriptor-present?", slipnet.plato_whole)
            if exists_p(whole_group):
                return [whole_group]
            return [self]
        all_candidates = filter_meth(
            this.letter_list if object_type is slipnet.plato_letter else this.group_list,
            "descriptor-present?", descriptor)
        if (description_type is slipnet.plato_string_position_category
                and len(all_candidates) > 1):
            objects_with_vertical_bridges = filter_(
                lambda obj: exists_p(tell(obj, "get-bridge", "vertical")), all_candidates)
            if len(objects_with_vertical_bridges) == 0:
                return [workspace_objects.lowest_level_object(all_candidates)]
            return [workspace_objects.lowest_level_object(objects_with_vertical_bridges)]
        return all_candidates

    @message("expand-internal-storage")
    def expand_internal_storage(this, self):
        this.max_object_capacity = 2 * this.max_object_capacity
        new_group_vector = chez.make_vector(this.max_object_capacity, False)
        new_proposed_group_table = make_table(this.max_object_capacity,
                                              this.max_object_capacity, [])
        new_from_to_bond_table = make_table(this.max_object_capacity,
                                            this.max_object_capacity, False)
        new_left_right_bond_table = make_table(this.max_object_capacity,
                                               this.max_object_capacity, False)
        new_proposed_bond_table = make_table(this.max_object_capacity,
                                             this.max_object_capacity, [])
        copy_vector_contents_bang(this.group_vector, new_group_vector)
        copy_table_contents_bang(this.proposed_group_table, new_proposed_group_table)
        copy_table_contents_bang(this.from_to_bond_table, new_from_to_bond_table)
        copy_table_contents_bang(this.left_right_bond_table, new_left_right_bond_table)
        copy_table_contents_bang(this.proposed_bond_table, new_proposed_bond_table)
        this.group_vector = new_group_vector
        this.proposed_group_table = new_proposed_group_table
        this.from_to_bond_table = new_from_to_bond_table
        this.left_right_bond_table = new_left_right_bond_table
        this.proposed_bond_table = new_proposed_bond_table
        if this.string_type == "initial":
            tell(workspace.g_workspace, "reallocate-top-bridges-storage")
            tell(workspace.g_workspace, "reallocate-vertical-bridges-storage")
        elif this.string_type == "modified":
            tell(workspace.g_workspace, "reallocate-top-bridges-storage")
        elif this.string_type == "target":
            tell(workspace.g_workspace, "reallocate-vertical-bridges-storage")
            if setup.p_justify_mode is not False:
                tell(workspace.g_workspace, "reallocate-bottom-bridges-storage")
        elif this.string_type == "answer":
            if not this.translated_p:
                tell(workspace.g_workspace, "reallocate-bottom-bridges-storage")
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def new_workspace_string(string_type, letter_categories):
    """workspace-strings.ss: new-workspace-string"""
    return WorkspaceString(string_type, letter_categories)
