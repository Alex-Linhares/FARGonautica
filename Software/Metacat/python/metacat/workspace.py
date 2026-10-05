"""workspace.ss: the Workspace, its strings, bridges and rules, and the mapping strengths.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from workspace.ss, with
racket/engine/workspace.rktl as a worked translation.

make-workspace's closure is the Workspace class (docs/python-translation-plan.md,
"Objects").  Its bridge and rule lists are Python lists, rebuilt (never mutated)
on every cons and remq; the built-bridge vectors and proposed-bridge tables are
chez.Vectors (tables are vectors of rows) whose cells hold such lists.
`(define *workspace* (make-workspace))` runs in load(); until then g_workspace
is #f.  The string globals (*initial-string* ... *all-strings*) are set by
run.ss's init-workspace.

Names of files translated later are read through the package at call time, and
only where the original evaluates them: *EEG* (eeg-graphics.ss), bridge-between?
(bridges.ss), equivalent-workspace-objects? (trace.ss), rule-describable-bridge?
(rules.ss), group-graphics and bridge-graphics.  The engine never imports
tkinter.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez
from metacat import formulas, setup, slipnet
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (adjacency_map, all_exist_p, all_same_p, ascending_index_list,
                               base_object, copy_table_contents_bang,
                               copy_vector_contents_bang, count, exists_p, filter_,
                               filter_meth, flatten, get_column, get_first, get_row,
                               hundred_minus, initialize_column_bang, initialize_row_bang,
                               make_table, member_p, ormap_meth, remq_duplicates, rough,
                               round_, select, select_meth, sets_equal_p, sort_by_method,
                               stochastic_pick, subset_p, sum_, table_ref, table_set_bang,
                               tell_all, times_100, weighted_average)

g_initial_string = False
g_modified_string = False
g_target_string = False
g_answer_string = False
g_top_strings = False
g_bottom_strings = False
g_vertical_strings = False
g_non_answer_strings = False
g_all_strings = False

p_expiration_period = 500
p_num_youngest_structures = 3

# Workspace structure proposal levels:
p_proposed = 1
p_evaluated = 2
p_built = 3


def _append_all(lists):
    """(apply append lists)"""
    result = []
    for l in lists:
        result = result + l
    return result


def _table_lists(table):
    """(map vector->list (vector->list table)): the rows as plain lists."""
    # chez: map's order is irrelevant here (vector->list is pure)
    return [list(row) for row in table]


def _justify():
    return setup.p_justify_mode is not False


class Workspace(SchemeObject):
    """workspace.ss: make-workspace (the closure)"""
    __slots__ = ("initial_string", "modified_string", "target_string", "answer_string",
                 "top_strings", "bottom_strings", "vertical_strings",
                 "top_bridges", "top_bridge_list", "proposed_top_bridge_table",
                 "bottom_bridges", "bottom_bridge_list", "proposed_bottom_bridge_table",
                 "vertical_bridges", "vertical_bridge_list", "proposed_vertical_bridge_table",
                 "average_intra_string_unhappiness", "average_top_inter_string_unhappiness",
                 "average_bottom_inter_string_unhappiness",
                 "average_vertical_inter_string_unhappiness", "average_unhappiness",
                 "top_mapping_strength", "bottom_mapping_strength",
                 "vertical_mapping_strength", "clamped_rule_list", "top_rule_list",
                 "bottom_rule_list", "top_rule_possible_p", "bottom_rule_possible_p")

    def __init__(this):
        this.initial_string = False
        this.modified_string = False
        this.target_string = False
        this.answer_string = False
        this.top_strings = False
        this.bottom_strings = False
        this.vertical_strings = False
        this.top_bridges = False
        this.top_bridge_list = []
        this.proposed_top_bridge_table = False
        this.bottom_bridges = False
        this.bottom_bridge_list = []
        this.proposed_bottom_bridge_table = False
        this.vertical_bridges = False
        this.vertical_bridge_list = []
        this.proposed_vertical_bridge_table = False
        this.average_intra_string_unhappiness = 0
        this.average_top_inter_string_unhappiness = 0
        this.average_bottom_inter_string_unhappiness = 0
        this.average_vertical_inter_string_unhappiness = 0
        this.average_unhappiness = 0
        this.top_mapping_strength = 0
        this.bottom_mapping_strength = 0
        this.vertical_mapping_strength = 0
        this.clamped_rule_list = []
        this.top_rule_list = []
        this.bottom_rule_list = []
        this.top_rule_possible_p = False
        this.bottom_rule_possible_p = False

    @message("object-type")
    def object_type(this, self):
        return "workspace"

    @message("initialize")
    def initialize(this, self, initial, modified, target, answer):
        this.initial_string = initial
        this.modified_string = modified
        this.target_string = target
        this.answer_string = answer
        this.top_strings = [initial, modified]
        this.bottom_strings = [target, answer]
        this.vertical_strings = [initial, target]
        this.top_bridges = chez.make_vector(tell(this.initial_string, "get-max-object-capacity"),
                                            False)
        this.top_bridge_list = []
        this.proposed_top_bridge_table = make_table(
            tell(this.initial_string, "get-max-object-capacity"),
            tell(this.modified_string, "get-max-object-capacity"),
            [])
        this.bottom_bridges = (chez.make_vector(tell(this.target_string, "get-max-object-capacity"),
                                                False)
                               if _justify() else False)
        this.bottom_bridge_list = []
        this.proposed_bottom_bridge_table = (make_table(
            tell(this.target_string, "get-max-object-capacity"),
            tell(this.answer_string, "get-max-object-capacity"),
            []) if _justify() else False)
        this.vertical_bridges = chez.make_vector(
            tell(this.initial_string, "get-max-object-capacity"), False)
        this.vertical_bridge_list = []
        this.proposed_vertical_bridge_table = make_table(
            tell(this.initial_string, "get-max-object-capacity"),
            tell(this.target_string, "get-max-object-capacity"),
            [])
        this.average_intra_string_unhappiness = 0
        this.average_top_inter_string_unhappiness = 0
        this.average_bottom_inter_string_unhappiness = 0
        this.average_vertical_inter_string_unhappiness = 0
        this.average_unhappiness = 0
        this.top_mapping_strength = 0
        this.bottom_mapping_strength = 0
        this.vertical_mapping_strength = 0
        this.top_rule_list = []
        this.bottom_rule_list = []
        this.clamped_rule_list = []
        this.top_rule_possible_p = False
        this.bottom_rule_possible_p = False
        tell(_metacat.eeg_graphics.g_EEG, "initialize")
        return "done"

    def _four(this, msg):
        # (append (tell initial msg) (tell modified msg) (tell target msg)
        #         (if %justify-mode% (tell answer msg) '()))
        # chez: append's argument order is unspecified; these tells are pure
        return (tell(this.initial_string, msg)
                + tell(this.modified_string, msg)
                + tell(this.target_string, msg)
                + (tell(this.answer_string, msg) if _justify() else []))

    @message("get-bonds")
    def get_bonds(this, self):
        return this._four("get-bonds")

    @message("get-groups")
    def get_groups(this, self):
        return this._four("get-groups")

    @message("get-all-letters")
    def get_all_letters(this, self):
        return this._four("get-letters")

    @message("get-all-groups")
    def get_all_groups(this, self):
        return this._four("get-all-groups")

    @message("get-bridges")
    def get_bridges(this, self, bridge_type):
        if bridge_type == "top":
            return this.top_bridge_list
        if bridge_type == "bottom":
            return this.bottom_bridge_list
        if bridge_type == "vertical":
            return this.vertical_bridge_list
        return None

    @message("get-all-bridges")
    def get_all_bridges(this, self):
        return this.top_bridge_list + this.bottom_bridge_list + this.vertical_bridge_list

    @message("get-objects")
    def get_objects(this, self):
        return this._four("get-objects")

    @message("get-other-string")
    def get_other_string(this, self, string, bridge_orientation):
        string_type = tell(string, "get-string-type")
        if string_type == "initial":
            if bridge_orientation == "horizontal":
                return this.modified_string
            if bridge_orientation == "vertical":
                return this.target_string
            return None
        if string_type == "modified":
            return this.initial_string
        if string_type == "target":
            if bridge_orientation == "horizontal":
                return this.answer_string
            if bridge_orientation == "vertical":
                return this.initial_string
            return None
        if string_type == "answer":
            return this.target_string
        return None

    @message("get-possible-bridge-objects")
    def get_possible_bridge_objects(this, self, bridge_type):
        if bridge_type == "top":
            lists = tell_all(this.top_strings, "get-objects")
        elif bridge_type == "bottom":
            lists = tell_all(this.bottom_strings, "get-objects")
        elif bridge_type == "vertical":
            lists = tell_all(this.vertical_strings, "get-objects")
        else:
            lists = None  # 1.2: (apply append <void>) is an error
        return _append_all(lists)

    @message("get-activity")
    def get_activity(this, self):
        average_age = tell(self, "get-youngest-structures-average-age")
        # 1.2: (min 1.0 ...) makes the ratio a flonum before 100* rounds it (anomalies:
        # "Exact bond densities meet flonum thresholds")
        return hundred_minus(times_100(chez.min_(1.0, chez.div(average_age, p_expiration_period))))

    @message("get-youngest-structures-average-age")
    def get_youngest_structures_average_age(this, self):
        structures = tell(self, "get-structures")
        # chez: Chez's sort and its predicate calls (ties on age)
        youngest_structures = get_first(chez.min_(len(structures), p_num_youngest_structures),
                                        sort_by_method("get-age", lambda a, b: a < b, structures))
        if len(youngest_structures) == 0:
            return 0
        return chez.div(sum_(tell_all(youngest_structures, "get-age")),
                        len(youngest_structures))

    @message("get-structures")
    def get_structures(this, self):
        return (tell(self, "get-bonds")
                + tell(self, "get-groups")
                + this.top_bridge_list
                + this.bottom_bridge_list
                + this.vertical_bridge_list
                + this.top_rule_list
                + this.bottom_rule_list)

    @message("get-proposed-bridges")
    def get_proposed_bridges(this, self, bridge_type):
        if bridge_type == "top":
            table = this.proposed_top_bridge_table
        elif bridge_type == "vertical":
            table = this.proposed_vertical_bridge_table
        elif bridge_type == "bottom":
            table = this.proposed_bottom_bridge_table if _justify() else chez.Vector([])
        else:
            table = None  # 1.2: (vector->list <void>) is an error
        return remq_duplicates(flatten(_table_lists(table)))

    @message("get-all-proposed-bridges")
    def get_all_proposed_bridges(this, self):
        return remq_duplicates(flatten(
            [_table_lists(this.proposed_vertical_bridge_table),
             _table_lists(this.proposed_top_bridge_table),
             _table_lists(this.proposed_bottom_bridge_table) if _justify() else []]))

    @message("get-proposed-vertical-bridges")
    def get_proposed_vertical_bridges(this, self, object_):
        id_ = tell(object_, "get-id-num")
        string = tell(object_, "get-string")
        if string is this.initial_string:
            return _append_all(get_row(this.proposed_vertical_bridge_table, id_))
        if string is this.target_string:
            return _append_all(get_column(this.proposed_vertical_bridge_table, id_))
        return []

    @message("get-proposed-horizontal-bridges")
    def get_proposed_horizontal_bridges(this, self, object_):
        id_ = tell(object_, "get-id-num")
        string = tell(object_, "get-string")
        if string is this.initial_string:
            return _append_all(get_row(this.proposed_top_bridge_table, id_))
        if string is this.modified_string:
            return _append_all(get_column(this.proposed_top_bridge_table, id_))
        if _justify() and string is this.target_string:
            return _append_all(get_row(this.proposed_bottom_bridge_table, id_))
        if _justify() and string is this.answer_string:
            return _append_all(get_column(this.proposed_bottom_bridge_table, id_))
        return []

    @message("get-all-other-coincident-bridges")
    def get_all_other_coincident_bridges(this, self, bridge, object1, object2):
        i = tell(object1, "get-id-num")
        j = tell(object2, "get-id-num")
        bridge_type = tell(bridge, "get-bridge-type")
        if bridge_type == "top":
            cell = table_ref(this.proposed_top_bridge_table, i, j)
        elif bridge_type == "bottom":
            cell = table_ref(this.proposed_bottom_bridge_table, i, j)
        elif bridge_type == "vertical":
            cell = table_ref(this.proposed_vertical_bridge_table, i, j)
        else:
            cell = None  # 1.2: (remq bridge <void>) is an error
        proposed_bridges = chez.remq(bridge, cell)
        bridge_type = tell(bridge, "get-bridge-type")
        if bridge_type == "top":
            built_bridge = this.top_bridges[i]
        elif bridge_type == "bottom":
            built_bridge = this.bottom_bridges[i]
        elif bridge_type == "vertical":
            built_bridge = this.vertical_bridges[i]
        else:
            built_bridge = None
        if (exists_p(built_bridge)
                and built_bridge is not bridge
                and tell(built_bridge, "get-object1") is tell(bridge, "get-original-object1")
                and tell(built_bridge, "get-object2") is tell(bridge, "get-original-object2")):
            return [built_bridge] + proposed_bridges
        return proposed_bridges

    @message("bridge-present?")
    def bridge_present_p(this, self, bridge):
        return exists_p(tell(self, "get-equivalent-bridge", bridge))

    @message("get-equivalent-bridge")
    def get_equivalent_bridge(this, self, bridge):
        bridge_type = tell(bridge, "get-bridge-type")
        bridge_orientation = tell(bridge, "get-orientation")
        if bridge_type == "top":
            bridge_list, string1, string2 = (this.top_bridge_list, this.initial_string,
                                             this.modified_string)
        elif bridge_type == "bottom":
            bridge_list, string1, string2 = (this.bottom_bridge_list, this.target_string,
                                             this.answer_string)
        elif bridge_type == "vertical":
            bridge_list, string1, string2 = (this.vertical_bridge_list, this.initial_string,
                                             this.target_string)
        else:
            bridge_list = string1 = string2 = None
        if member_p(bridge, bridge_list):
            return bridge
        # chez: a let's order is unspecified; both bindings are pure
        equivalent_object1 = tell(string1, "get-equivalent-object", tell(bridge, "get-object1"))
        equivalent_object2 = (tell(string2, "get-equivalent-object", tell(bridge, "get-object2"))
                              if exists_p(string2) else False)
        if (exists_p(equivalent_object1)
                and exists_p(equivalent_object2)
                and _metacat.bridges.bridge_between_p(bridge_orientation, equivalent_object1,
                                                      equivalent_object2) is not False):
            return tell(equivalent_object1, "get-bridge", bridge_orientation)
        return False

    @message("get-all-slippages")
    def get_all_slippages(this, self, bridge_type):
        return _append_all(tell_all(tell(self, "get-bridges", bridge_type), "get-slippages"))

    @message("get-all-non-symmetric-slippages")
    def get_all_non_symmetric_slippages(this, self, bridge_type):
        return _append_all(tell_all(tell(self, "get-bridges", bridge_type),
                                    "get-non-symmetric-slippages"))

    @message("get-all-vertical-CMs")
    def get_all_vertical_CMs(this, self):
        return _append_all(tell_all(this.vertical_bridge_list, "get-all-concept-mappings"))

    @message("object-exists?")
    def object_exists_p(this, self, object_):
        return member_p(object_, tell(self, "get-objects"))

    @message("spanning-bridge-exists?")
    def spanning_bridge_exists_p(this, self, bridge_type):
        return ormap_meth(tell(self, "get-bridges", bridge_type), "spanning-bridge?")

    @message("get-spanning-bridge")
    def get_spanning_bridge(this, self, bridge_type):
        return select_meth(tell(self, "get-bridges", bridge_type), "spanning-bridge?")

    @message("add-bridge")
    def add_bridge(this, self, bridge):
        i = tell(tell(bridge, "get-object1"), "get-id-num")
        bridge_type = tell(bridge, "get-bridge-type")
        if bridge_type == "top":
            this.top_bridges[i] = bridge
            this.top_bridge_list = [bridge] + this.top_bridge_list
        elif bridge_type == "bottom":
            this.bottom_bridges[i] = bridge
            this.bottom_bridge_list = [bridge] + this.bottom_bridge_list
        elif bridge_type == "vertical":
            this.vertical_bridges[i] = bridge
            this.vertical_bridge_list = [bridge] + this.vertical_bridge_list
        return "done"

    def _proposed_table(this, proposed_bridge):
        bridge_type = tell(proposed_bridge, "get-bridge-type")
        if bridge_type == "top":
            return this.proposed_top_bridge_table
        if bridge_type == "bottom":
            return this.proposed_bottom_bridge_table
        if bridge_type == "vertical":
            return this.proposed_vertical_bridge_table
        return None

    @message("add-proposed-bridge")
    def add_proposed_bridge(this, self, proposed_bridge):
        table = this._proposed_table(proposed_bridge)
        i = tell(tell(proposed_bridge, "get-object1"), "get-id-num")
        j = tell(tell(proposed_bridge, "get-object2"), "get-id-num")
        bridge_list = table_ref(table, i, j)
        table_set_bang(table, i, j, [proposed_bridge] + bridge_list)
        return "done"

    @message("delete-bridge")
    def delete_bridge(this, self, bridge):
        i = tell(tell(bridge, "get-object1"), "get-id-num")
        bridge_type = tell(bridge, "get-bridge-type")
        if bridge_type == "top":
            this.top_bridge_list = chez.remq(bridge, this.top_bridge_list)
            this.top_bridges[i] = False
        elif bridge_type == "bottom":
            this.bottom_bridge_list = chez.remq(bridge, this.bottom_bridge_list)
            this.bottom_bridges[i] = False
        elif bridge_type == "vertical":
            this.vertical_bridge_list = chez.remq(bridge, this.vertical_bridge_list)
            this.vertical_bridges[i] = False
        return "done"

    @message("delete-proposed-bridge")
    def delete_proposed_bridge(this, self, proposed_bridge):
        table = this._proposed_table(proposed_bridge)
        i = tell(tell(proposed_bridge, "get-object1"), "get-id-num")
        j = tell(tell(proposed_bridge, "get-object2"), "get-id-num")
        bridge_list = table_ref(table, i, j)
        table_set_bang(table, i, j, chez.remq(proposed_bridge, bridge_list))
        return "done"

    @message("delete-proposed-vertical-bridges")
    def delete_proposed_vertical_bridges(this, self, object_):
        id_ = tell(object_, "get-id-num")
        string = tell(object_, "get-string")
        if string is this.initial_string:
            initialize_row_bang(this.proposed_vertical_bridge_table, id_, [])
        elif string is this.target_string:
            initialize_column_bang(this.proposed_vertical_bridge_table, id_, [])
        return "done"

    @message("delete-proposed-horizontal-bridges")
    def delete_proposed_horizontal_bridges(this, self, object_):
        id_ = tell(object_, "get-id-num")
        string = tell(object_, "get-string")
        if string is this.initial_string:
            initialize_row_bang(this.proposed_top_bridge_table, id_, [])
        elif string is this.modified_string:
            initialize_column_bang(this.proposed_top_bridge_table, id_, [])
        elif _justify() and string is this.target_string:
            initialize_row_bang(this.proposed_bottom_bridge_table, id_, [])
        elif _justify() and string is this.answer_string:
            initialize_column_bang(this.proposed_bottom_bridge_table, id_, [])
        return "done"

    @message("delete-all-proposed-bridges")
    def delete_all_proposed_bridges(this, self):
        # 1.2: for-each-vector-element* loops forever on an empty vector
        # (ascending-index-list 0; porting-notes.md, item 03)
        for i in ascending_index_list(len(this.proposed_vertical_bridge_table)):
            initialize_row_bang(this.proposed_vertical_bridge_table, i, [])
        for i in ascending_index_list(len(this.proposed_top_bridge_table)):
            initialize_row_bang(this.proposed_top_bridge_table, i, [])
        if _justify():
            for i in ascending_index_list(len(this.proposed_bottom_bridge_table)):
                initialize_row_bang(this.proposed_bottom_bridge_table, i, [])
        return "done"

    @message("delete-proposed-structure")
    def delete_proposed_structure(this, self, struc):
        object_type = tell(struc, "object-type")
        if object_type == "bond":
            tell(tell(struc, "get-string"), "delete-proposed-bond", struc)
        elif object_type == "group":
            tell(tell(struc, "get-string"), "delete-proposed-group", struc)
            if setup.p_workspace_graphics is not False:
                _metacat.group_graphics.group_graphics("erase", struc)
        elif object_type == "bridge":
            tell(self, "delete-proposed-bridge", struc)
            if setup.p_workspace_graphics is not False:
                _metacat.bridge_graphics.bridge_graphics("erase", struc)
        return "done"

    @message("get-real-object")
    def get_real_object(this, self, fake_object):
        return select(lambda obj: _metacat.trace.equivalent_workspace_objects_p(obj, fake_object),
                      tell(self, "get-objects"))

    @message("get-clamped-rules")
    def get_clamped_rules(this, self):
        return this.clamped_rule_list

    @message("clamp-rule")
    def clamp_rule(this, self, rule):
        this.clamped_rule_list = [rule] + this.clamped_rule_list
        if setup.p_workspace_graphics is not False:
            tell(setup.g_workspace_window, "draw", tell(rule, "get-clamped-graphics-pexp"))
        return "done"

    @message("unclamp-rule")
    def unclamp_rule(this, self, rule):
        this.clamped_rule_list = chez.remq(rule, this.clamped_rule_list)
        if setup.p_workspace_graphics is not False:
            tell(setup.g_workspace_window, "erase", tell(rule, "get-clamped-graphics-pexp"))
        return "done"

    @message("unclamp-rules")
    def unclamp_rules(this, self):
        if len(this.clamped_rule_list) != 0:
            if setup.p_workspace_graphics is not False:
                for rule in this.clamped_rule_list:
                    tell(setup.g_workspace_window, "erase",
                         tell(rule, "get-clamped-graphics-pexp"))
            this.clamped_rule_list = []
        return "done"

    @message("get-all-rules")
    def get_all_rules(this, self):
        return this.top_rule_list + this.bottom_rule_list

    @message("get-rules")
    def get_rules(this, self, rule_type):
        if rule_type == "top":
            return this.top_rule_list
        if rule_type == "bottom":
            return this.bottom_rule_list
        return None

    @message("get-all-supported-rules")
    def get_all_supported_rules(this, self):
        return filter_meth(tell(self, "get-all-rules"), "supported?")

    @message("get-supported-rules")
    def get_supported_rules(this, self, rule_type):
        return filter_meth(tell(self, "get-rules", rule_type), "supported?")

    @message("rule-exists?")
    def rule_exists_p(this, self, rule_type):
        return len(tell(self, "get-rules", rule_type)) != 0

    @message("supported-rule-exists?")
    def supported_rule_exists_p(this, self, rule_type):
        return ormap_meth(tell(self, "get-rules", rule_type), "supported?")

    @message("get-possible-rule-types")
    def get_possible_rule_types(this, self):
        if this.top_rule_possible_p is not False and this.bottom_rule_possible_p is not False:
            return ["top", "bottom"]
        if this.top_rule_possible_p is not False:
            return ["top"]
        if this.bottom_rule_possible_p is not False:
            return ["bottom"]
        return []

    @message("rule-possible?")
    def rule_possible_p(this, self, rule_type):
        if rule_type == "top":
            return this.top_rule_possible_p
        if rule_type == "bottom":
            return this.bottom_rule_possible_p
        return None

    @message("check-if-rules-possible")
    def check_if_rules_possible(this, self):
        # chez: subset?'s two arguments are pure (rule-describable-bridge? only reads)
        this.top_rule_possible_p = subset_p(
            tell(this.initial_string, "get-letters") + tell(this.modified_string, "get-letters"),
            _append_all(tell_all(
                filter_(lambda b: _metacat.rules.rule_describable_bridge_p(b),
                        this.top_bridge_list),
                "get-covered-letters")))
        if _justify():
            this.bottom_rule_possible_p = subset_p(
                tell(this.target_string, "get-letters") + tell(this.answer_string, "get-letters"),
                _append_all(tell_all(
                    filter_(lambda b: _metacat.rules.rule_describable_bridge_p(b),
                            this.bottom_bridge_list),
                    "get-covered-letters")))
        return "done"

    @message("rule-present?")
    def rule_present_p(this, self, rule):
        return exists_p(tell(self, "get-equivalent-rule", rule))

    @message("get-equivalent-rule")
    def get_equivalent_rule(this, self, rule):
        return select_meth(tell(self, "get-rules", tell(rule, "get-rule-type")), "equal?", rule)

    @message("add-rule")
    def add_rule(this, self, rule):
        rule_type = tell(rule, "get-rule-type")
        if rule_type == "top":
            this.top_rule_list = [rule] + this.top_rule_list
        elif rule_type == "bottom":
            this.bottom_rule_list = [rule] + this.bottom_rule_list
        return "done"

    # unused:
    @message("delete-rule")
    def delete_rule(this, self, rule):
        rule_type = tell(rule, "get-rule-type")
        if rule_type == "top":
            this.top_rule_list = chez.remq(rule, this.top_rule_list)
        elif rule_type == "bottom":
            this.bottom_rule_list = chez.remq(rule, this.bottom_rule_list)
        return "done"

    # Themes:
    #
    # Don't need to update dominant themes or graphics here, since they get
    # updated automatically after activation spreads in the themespace, which
    # happens immediately after spread-activation-to-themespace (see run.ss):
    @message("spread-activation-to-themespace")
    def spread_activation_to_themespace(this, self):
        for bridge in tell(self, "get-all-bridges"):
            tell(bridge, "boost-themes")
        return "done"

    @message("choose-object")
    def choose_object(this, self, *message_):
        objects = tell(self, "get-objects")
        # chez: tell-all in map's order
        weights = tell_all(objects, *message_)
        return stochastic_pick(objects, formulas.temp_adjusted_values(weights))

    @message("get-average-intra-string-unhappiness")
    def get_average_intra_string_unhappiness(this, self):
        return this.average_intra_string_unhappiness

    @message("get-average-inter-string-unhappiness")
    def get_average_inter_string_unhappiness(this, self, bridge_type):
        if bridge_type == "top":
            return this.average_top_inter_string_unhappiness
        if bridge_type == "bottom":
            return this.average_bottom_inter_string_unhappiness
        if bridge_type == "vertical":
            return this.average_vertical_inter_string_unhappiness
        return None

    @message("get-max-inter-string-unhappiness")
    def get_max_inter_string_unhappiness(this, self):
        if _justify():
            return chez.max_(this.average_top_inter_string_unhappiness,
                             this.average_bottom_inter_string_unhappiness,
                             this.average_vertical_inter_string_unhappiness)
        return chez.max_(this.average_top_inter_string_unhappiness,
                         this.average_vertical_inter_string_unhappiness)

    @message("get-average-unhappiness")
    def get_average_unhappiness(this, self):
        return this.average_unhappiness

    @message("get-mapping-strength")
    def get_mapping_strength(this, self, bridge_type):
        if bridge_type == "top":
            return this.top_mapping_strength
        if bridge_type == "bottom":
            return this.bottom_mapping_strength
        if bridge_type == "vertical":
            return this.vertical_mapping_strength
        return None

    @message("get-min-mapping-strength")
    def get_min_mapping_strength(this, self):
        if _justify():
            return chez.min_(this.top_mapping_strength,
                             this.bottom_mapping_strength,
                             this.vertical_mapping_strength)
        return chez.min_(this.top_mapping_strength, this.vertical_mapping_strength)

    @message("maximal-mapping?")
    def maximal_mapping_p(this, self, bridge_type):
        if bridge_type == "top":
            strings = [this.initial_string, this.modified_string]
        elif bridge_type == "vertical":
            strings = [this.initial_string, this.target_string]
        elif bridge_type == "bottom":
            strings = [this.target_string, this.answer_string]
        else:
            strings = None  # 1.2: tell-all on <void> is an error
        return sets_equal_p(
            remq_duplicates(_append_all(tell_all(tell(self, "get-bridges", bridge_type),
                                                 "get-covered-letters"))),
            _append_all(tell_all(strings, "get-letters")))

    def _mapping_strength(this, self, bridge_type, raw_strength, string1, string2):
        # the cond that sets top-, bottom- and vertical-mapping-strength
        if tell(self, "spanning-bridge-exists?", bridge_type) is not False:
            return raw_strength
        if (spanning_group_possible_p(string1) is not False
                and spanning_group_possible_p(string2) is not False):
            return round_(chez.mul(Fraction(1, 2), raw_strength))
        if tell(self, "maximal-mapping?", bridge_type) is not False:
            return times_100(chez.tanh(chez.mul(Fraction(1, 40), raw_strength)))
        return raw_strength

    @message("update-average-unhappiness-values")
    def update_average_unhappiness_values(this, self):
        # chez: a let's order is unspecified; these bindings are pure
        all_objects = tell(self, "get-objects")
        top_objects = (tell(this.initial_string, "get-objects")
                       + tell(this.modified_string, "get-objects"))
        bottom_objects = ((tell(this.target_string, "get-objects")
                           + tell(this.answer_string, "get-objects"))
                          if _justify() else False)
        vertical_objects = (tell(this.initial_string, "get-objects")
                            + tell(this.target_string, "get-objects"))
        this.average_intra_string_unhappiness = round_(weighted_average(
            tell_all(all_objects, "get-intra-string-unhappiness"),
            tell_all(all_objects, "get-relative-importance")))
        this.average_top_inter_string_unhappiness = round_(weighted_average(
            tell_all(top_objects, "get-inter-string-unhappiness", "horizontal"),
            tell_all(top_objects, "get-relative-importance")))
        if _justify():
            this.average_bottom_inter_string_unhappiness = round_(weighted_average(
                tell_all(bottom_objects, "get-inter-string-unhappiness", "horizontal"),
                tell_all(bottom_objects, "get-relative-importance")))
        this.average_vertical_inter_string_unhappiness = round_(weighted_average(
            tell_all(vertical_objects, "get-inter-string-unhappiness", "vertical"),
            tell_all(vertical_objects, "get-relative-importance")))
        this.average_unhappiness = round_(weighted_average(
            tell_all(all_objects, "get-average-unhappiness"),
            tell_all(all_objects, "get-relative-importance")))
        raw_top_strength = hundred_minus(this.average_top_inter_string_unhappiness)
        raw_bottom_strength = (hundred_minus(this.average_bottom_inter_string_unhappiness)
                               if _justify() else False)
        raw_vertical_strength = hundred_minus(this.average_vertical_inter_string_unhappiness)
        this.top_mapping_strength = this._mapping_strength(
            self, "top", raw_top_strength, this.initial_string, this.modified_string)
        if _justify():
            this.bottom_mapping_strength = this._mapping_strength(
                self, "bottom", raw_bottom_strength, this.target_string, this.answer_string)
        this.vertical_mapping_strength = this._mapping_strength(
            self, "vertical", raw_vertical_strength, this.initial_string, this.target_string)
        return None  # the value of set!

    @message("get-rough-num-of-unrelated-objects")
    def get_rough_num_of_unrelated_objects(this, self):
        return rough_num_of_objects(count(unrelated_p, tell(self, "get-objects")))

    @message("get-rough-num-of-ungrouped-objects")
    def get_rough_num_of_ungrouped_objects(this, self):
        return rough_num_of_objects(count(ungrouped_p, tell(self, "get-objects")))

    @message("get-rough-num-of-unmapped-objects")
    def get_rough_num_of_unmapped_objects(this, self):
        return rough_num_of_objects(count(unmapped_p, tell(self, "get-objects")))

    @message("reallocate-top-bridges-storage")
    def reallocate_top_bridges_storage(this, self):
        max_i_capacity = tell(this.initial_string, "get-max-object-capacity")
        max_m_capacity = tell(this.modified_string, "get-max-object-capacity")
        new_vector = chez.make_vector(max_i_capacity, False)
        new_table = make_table(max_i_capacity, max_m_capacity, [])
        copy_vector_contents_bang(this.top_bridges, new_vector)
        copy_table_contents_bang(this.proposed_top_bridge_table, new_table)
        this.top_bridges = new_vector
        this.proposed_top_bridge_table = new_table
        return "done"

    @message("reallocate-bottom-bridges-storage")
    def reallocate_bottom_bridges_storage(this, self):
        max_t_capacity = tell(this.target_string, "get-max-object-capacity")
        max_a_capacity = tell(this.answer_string, "get-max-object-capacity")
        new_vector = chez.make_vector(max_t_capacity, False)
        new_table = make_table(max_t_capacity, max_a_capacity, [])
        copy_vector_contents_bang(this.bottom_bridges, new_vector)
        copy_table_contents_bang(this.proposed_bottom_bridge_table, new_table)
        this.bottom_bridges = new_vector
        this.proposed_bottom_bridge_table = new_table
        return "done"

    @message("reallocate-vertical-bridges-storage")
    def reallocate_vertical_bridges_storage(this, self):
        max_i_capacity = tell(this.initial_string, "get-max-object-capacity")
        max_t_capacity = tell(this.target_string, "get-max-object-capacity")
        new_vector = chez.make_vector(max_i_capacity, False)
        new_table = make_table(max_i_capacity, max_t_capacity, [])
        copy_vector_contents_bang(this.vertical_bridges, new_vector)
        copy_table_contents_bang(this.proposed_vertical_bridge_table, new_table)
        this.vertical_bridges = new_vector
        this.proposed_vertical_bridge_table = new_table
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_workspace():
    """workspace.ss: make-workspace"""
    return Workspace()


def spanning_group_possible_p(string):
    """workspace.ss: spanning-group-possible?"""
    r = tell(string, "spanning-group-exists?")
    if r is not False:
        return r
    objects = tell(string, "get-constituent-objects")

    def possible_for(bond_facet):
        def relation(obj1, obj2):
            # chez: a let's order is unspecified; both bindings are pure
            desc1 = tell(obj1, "get-descriptor-for", bond_facet)
            desc2 = tell(obj2, "get-descriptor-for", bond_facet)
            if exists_p(desc1) and exists_p(desc2):
                return slipnet.get_label(desc1, desc2)
            return False
        # chez: adjacency-map is a two-list map (chez.map_ order); relation is pure
        relations = adjacency_map(relation, objects)
        a = all_exist_p(relations)
        return a if a is False else all_same_p(relations)
    return chez.ormap(possible_for, tell(slipnet.plato_bond_facet, "get-instance-nodes"))


def rough_num_of_objects(num_of_objects):
    """workspace.ss: rough-num-of-objects"""
    # chez: (~ 4) is drawn only when the first test fails (cond)
    if num_of_objects < rough(2):
        return "few"
    if num_of_objects < rough(4):
        return "some"
    return "many"


def unrelated_p(object_):
    """workspace.ss: unrelated?"""
    if ungrouped_p(object_) is False:
        return False
    num_of_incident_bonds = tell(object_, "get-num-of-incident-bonds")
    if (tell(object_, "leftmost-in-string?") is not False
            or tell(object_, "rightmost-in-string?") is not False):
        return num_of_incident_bonds == 0
    return num_of_incident_bonds < 2


def ungrouped_p(object_):
    """workspace.ss: ungrouped?"""
    return (tell(object_, "spans-whole-string?") is False
            and not exists_p(tell(object_, "get-enclosing-group")))


def unmapped_p(object_):
    """workspace.ss: unmapped?"""
    which = tell(object_, "which-string")
    if which == "initial":
        return tell(object_, "mapped?", "both") is False
    if which == "modified":
        return tell(object_, "mapped?", "horizontal") is False
    if which == "target":
        if _justify():
            return tell(object_, "mapped?", "both") is False
        return tell(object_, "mapped?", "vertical") is False
    if which == "answer":
        return tell(object_, "mapped?", "horizontal") is False
    return None


g_workspace = False


def load():
    """workspace.ss: (define *workspace* (make-workspace))"""
    global g_workspace
    g_workspace = make_workspace()
