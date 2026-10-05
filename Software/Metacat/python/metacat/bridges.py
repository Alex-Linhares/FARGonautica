"""bridges.ss: horizontal and vertical bridges, the bridge codelets, and building
and breaking bridges.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from bridges.ss, with
racket/engine/bridges.rktl as a worked translation.

make-horizontal-bridge's and make-vertical-bridge's closures are the
HorizontalBridge and VerticalBridge classes (docs/python-translation-plan.md,
"Objects"); each delegates what it doesn't answer to a WorkspaceStructure.  The
record-case clauses the two closures share word for word (all but the
orientation-specific ones) are written once, in _BridgeClauses, which both
classes inherit (objects.py collects @message methods from base classes); the
clauses that differ (print, get-incompatible-bridges, get-incompatible-bond,
internally-coherent?, calculate-internal/external-strength, add-concept-mappings,
delete-concept-mapping-type, the graphics clauses and the horizontal-only ones)
are in each class.  The four codelet procedures are given to their codelet
types by load() (define-codelet-procedure* needs the types, which coderack.load
makes).

Names from files translated later are read through the package at call time:
themes.ss (*themespace*, bridge-type->theme-type, check-descriptions,
conflicts-with-theme?, supported-by-theme?, descriptions-affect-themespace?,
bridge-theme-compatibility-sigmoid), trace.ss (monitor-new-concept-mappings,
entries), justify.ss (remove-whole/single-concept-mappings) and
bridge-graphics.ss (bridge-graphics, draw-bridge-grope, new-bridge-label-number;
only with %workspace-graphics% on, as the original gates them).
*workspace-window*, *themespace-window*, %workspace-graphics%, %verbose%,
%justify-mode% and *temperature* are setup's.  Arithmetic stays exact unless a
flonum enters (the 0.8/1.2/1.6, 2.5/1.0 and 0.1 literals, temp-adjusted
probabilities); rounding is utilities.round_.  The engine never imports tkinter.

Evaluation order (audited against Chez's, as racket's porting-notes item 08
did): every draw (stochastic-pick of the bridge type, choose-object,
choose-relevant-distinguishing-description-by-depth, stochastic-pick-by-method,
stochastic-if*, wins-fight?/wins-all-fights?, and the bond and group building and
breaking of bridge-builder) sits in a let*, a body, an and/or or a cond, so
Python's order is Chez's.  Every stochastic-if* draws its coin first.  The
multi-argument calls and multi-binding lets (the appends of
get-incompatible-bridges and group-incompatible-bridges, the lets of
get-incompatible-bond, direction-incompatible-bridges, bridge-builder,
build-bridge and break-bridge, the arguments of all-possible-bridge-CMs,
make-concept-mapping and wins-all-fights?) only read state, so their order is
not observable.  maps go through chez.map_ (tell-all included);
cross-product-* keep utilities.ss's order.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez, sugar
from metacat import (bonds, coderack, concept_mappings, formulas, groups, setup, slipnet,
                     workspace, workspace_objects, workspace_structure_formulas,
                     workspace_structures)
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.sugar import say
from metacat.utilities import (andmap_meth, average, ceiling, cross_product_filter_map,
                               cross_product_for_each, cross_product_ormap, exists_p,
                               filter_, filter_meth, filter_out, filter_out_meth, first,
                               group_p, hundred_minus, intersect, letter_p, nth, one_minus,
                               ormap_meth, partition, percent, print_, product,
                               remq_duplicates, remq_elements, round_, round_to_100ths,
                               second, select, select_longest_list, select_meth,
                               stochastic_pick, stochastic_pick_by_method, sum_, tell_all,
                               weighted_average, y_coord)


# Horizontal bridges should have at least one _distinguishing_
# CM/slippage with a <relation>. This prevents stupid mappings such
# as b--d in abc->abcd, which is based on SLIPPAGE:StrPosCtgy:middle=>rmost
# and SLIPPAGE:LettCtgy:b=>d

# Symmetric concept-mappings are important.  They're used in
# important-object-bridge-scout codelets in the 'get-all-slippages
# call.  Example: abc ddbbaa cba with a horizontal bridge already
# made between a and aa (with cm StrPos:lmost=>rmost).  If
# description StrPos:rmost is chosen for c, applicable-slippage
# selected is the *symmetric* slippage StrPos:rmost=>lmost, which
# then yields lmost as object2-descriptor.  Then objects with
# description lmost in the modified string get focused on.
# Otherwise, only way to make this bridge is via
# bottom-up-bridge-scout codelets.


def _proposal_level_name(level):
    """(case level (0 "new") (1 "proposed") (2 "evaluated")), void otherwise."""
    if level == 0:
        return "new"
    if level == 1:
        return "proposed"
    if level == 2:
        return "evaluated"
    return None


def _graphics_coord(object_, group_spanning_bridge_p, orientation):
    """The from-/to-graphics-coord bindings of both makers (a cond)."""
    if setup.p_workspace_graphics is False:
        return False
    if group_spanning_bridge_p is not False:
        return tell(object_, "get-group-spanning-bridge-graphics-coord", orientation)
    return tell(object_, "get-bridge-graphics-coord", orientation)


class _BridgeClauses(SchemeObject):
    """bridges.ss: the record-case clauses that make-horizontal-bridge and
    make-vertical-bridge share word for word (and the closure variables they
    both bind)."""
    __slots__ = ("object1", "object2", "concept_mappings", "bridge_type", "theme_type",
                 "workspace_structure", "bond_concept_mappings", "all_concept_mappings",
                 "symmetric_slippages", "flipped_group1_p", "original_group1",
                 "flipped_group2_p", "original_group2", "spanning_bridge_p",
                 "group_spanning_bridge_p", "from_graphics_coord", "to_graphics_coord")

    @message("object-type")
    def object_type(this, self):
        return "bridge"

    @message("get-bridge-type")
    def get_bridge_type(this, self):
        return this.bridge_type

    @message("get-theme-type")
    def get_theme_type(this, self):
        return this.theme_type

    @message("bridge-type?")
    def bridge_type_p(this, self, type_):
        return type_ == this.bridge_type

    @message("spanning-bridge?")
    def spanning_bridge_p_(this, self):
        return this.spanning_bridge_p

    @message("group-spanning-bridge?")
    def group_spanning_bridge_p_(this, self):
        return this.group_spanning_bridge_p

    @message("get-from-graphics-coord")
    def get_from_graphics_coord(this, self):
        return this.from_graphics_coord

    @message("get-to-graphics-coord")
    def get_to_graphics_coord(this, self):
        return this.to_graphics_coord

    @message("get-object1")
    def get_object1(this, self):
        return this.object1

    @message("get-object2")
    def get_object2(this, self):
        return this.object2

    @message("get-enclosing-group1")
    def get_enclosing_group1(this, self):
        return tell(this.object1, "get-enclosing-group")

    @message("get-enclosing-group2")
    def get_enclosing_group2(this, self):
        return tell(this.object2, "get-enclosing-group")

    @message("get-original-object1")
    def get_original_object1(this, self):
        if this.flipped_group1_p is not False:
            return this.original_group1
        return this.object1

    @message("get-original-object2")
    def get_original_object2(this, self):
        if this.flipped_group2_p is not False:
            return this.original_group2
        return this.object2

    @message("get-concept-mapping-types")
    def get_concept_mapping_types(this, self):
        return tell_all(this.all_concept_mappings, "get-CM-type")

    @message("get-concept-mapping")
    def get_concept_mapping(this, self, description_type):
        return select_meth(this.all_concept_mappings, "CM-type?", description_type)

    @message("get-concept-mappings")
    def get_concept_mappings(this, self):
        return this.concept_mappings

    @message("get-bond-concept-mappings")
    def get_bond_concept_mappings(this, self):
        return this.bond_concept_mappings

    @message("get-all-concept-mappings")
    def get_all_concept_mappings(this, self):
        return this.all_concept_mappings

    @message("get-non-symmetric-slippages")
    def get_non_symmetric_slippages(this, self):
        return filter_meth(this.all_concept_mappings, "slippage?")

    @message("get-symmetric-slippages")
    def get_symmetric_slippages(this, self):
        return this.symmetric_slippages

    @message("get-slippages")
    def get_slippages(this, self):
        return tell(self, "get-non-symmetric-slippages") + this.symmetric_slippages

    @message("get-bond-slippages")
    def get_bond_slippages(this, self):
        return filter_meth(tell(self, "get-slippages"), "bond-concept-mapping?")

    @message("get-other-object")
    def get_other_object(this, self, object_):
        return this.object2 if object_ is this.object1 else this.object1

    @message("get-covered-letters")
    def get_covered_letters(this, self):
        # (append ...) of two reads
        return tell(this.object1, "get-letters") + tell(this.object2, "get-letters")

    @message("get-letter-span")
    def get_letter_span(this, self):
        return chez.add(tell(this.object1, "get-letter-span"),
                        tell(this.object2, "get-letter-span"))

    @message("flipped-group1?")
    def flipped_group1_p_(this, self):
        return this.flipped_group1_p

    @message("flipped-group2?")
    def flipped_group2_p_(this, self):
        return this.flipped_group2_p

    @message("get-original-group1")
    def get_original_group1(this, self):
        return this.original_group1

    @message("get-original-group2")
    def get_original_group2(this, self):
        return this.original_group2

    @message("mark-flipped-group1")
    def mark_flipped_group1(this, self, original_group):
        this.flipped_group1_p = True
        this.original_group1 = original_group
        return "done"

    @message("mark-flipped-group2")
    def mark_flipped_group2(this, self, original_group):
        this.flipped_group2_p = True
        this.original_group2 = original_group
        return "done"

    @message("get-drawn-coincident-bridge")
    def get_drawn_coincident_bridge(this, self):
        return select_meth(tell(workspace.g_workspace, "get-all-other-coincident-bridges",
                                self, this.object1, this.object2),
                           "drawn?")

    @message("get-highest-level-coincident-bridge")
    def get_highest_level_coincident_bridge(this, self):
        current_max = 0
        highest_level_bridge = False
        for b in tell(workspace.g_workspace, "get-all-other-coincident-bridges",
                      self, this.object1, this.object2):
            proposal_level = tell(b, "get-proposal-level")
            if proposal_level > current_max:
                current_max = proposal_level
                highest_level_bridge = b
        return highest_level_bridge

    @message("add-concept-mapping")
    def add_concept_mapping(this, self, cm):
        return tell(self, "add-concept-mappings", [cm])

    @message("add-bond-concept-mapping")
    def add_bond_concept_mapping(this, self, bond_cm):
        this.bond_concept_mappings = [bond_cm] + this.bond_concept_mappings
        this.all_concept_mappings = [bond_cm] + this.all_concept_mappings
        return "done"

    @message("add-symmetric-slippage")
    def add_symmetric_slippage(this, self, slippage):
        symmetric_slippage = tell(slippage, "symmetric-mapping")
        this.symmetric_slippages = [symmetric_slippage] + this.symmetric_slippages
        return "done"

    @message("concept-mapping-present?")
    def concept_mapping_present_p(this, self, concept_mapping):
        return chez.ormap(lambda cm: concept_mappings.CMs_equal_p(cm, concept_mapping),
                          this.all_concept_mappings)

    @message("CM-type-present?")
    def CM_type_present_p(this, self, type_):
        return ormap_meth(this.all_concept_mappings, "CM-type?", type_)

    @message("slippage-type-present?")
    def slippage_type_present_p(this, self, type_):
        cm = select_meth(this.all_concept_mappings, "CM-type?", type_)
        return exists_p(cm) and tell(cm, "slippage?")

    @message("supports-theme-pattern?")
    def supports_theme_pattern_p(this, self, pattern):
        # the two list arguments only read
        def matches(entry, cm):
            return (tell(cm, "get-CM-type") is first(entry)
                    and tell(cm, "get-label") is second(entry))
        return cross_product_ormap(
            matches,
            _metacat.trace.entries(pattern),
            _metacat.justify.remove_whole_or_single_concept_mappings(this.all_concept_mappings))

    @message("get-relevant-CMs")
    def get_relevant_CMs(this, self):
        return filter_meth(this.concept_mappings, "relevant?")

    @message("get-distinguishing-CMs")
    def get_distinguishing_CMs(this, self):
        return filter_meth(this.concept_mappings, "distinguishing?")

    @message("get-relevant-distinguishing-CMs")
    def get_relevant_distinguishing_CMs(this, self):
        return filter_meth(this.concept_mappings, "relevant-distinguishing?")

    # Themes:
    @message("incompatible-with-theme?")
    def incompatible_with_theme_p(this, self, theme):
        themes = _metacat.themes
        conflict = themes.check_descriptions(this.object1, this.object2,
                                             themes.conflicts_with_theme_p, theme)
        if conflict is not False:
            return conflict
        dimension = tell(theme, "get-dimension")
        object1_description_possible_p = tell(dimension, "description-possible?", this.object1)
        object2_description_possible_p = tell(dimension, "description-possible?", this.object2)
        return ((object1_description_possible_p is not False
                 and object2_description_possible_p is False)
                or (object1_description_possible_p is False
                    and object2_description_possible_p is not False)
                or (dimension is slipnet.plato_string_position_category
                    and object1_description_possible_p is False
                    and object2_description_possible_p is False))

    @message("supported-by-theme?")
    def supported_by_theme_p(this, self, theme):
        themes = _metacat.themes
        return themes.check_descriptions(this.object1, this.object2,
                                         themes.supported_by_theme_p, theme)

    @message("get-thematic-compatibility")
    def get_thematic_compatibility(this, self):
        return _metacat.themes.bridge_theme_compatibility_sigmoid(
            tell(self, "get-average-theme-support"))

    @message("get-average-theme-support")
    def get_average_theme_support(this, self):
        support_values = tell(self, "get-theme-support-values")
        neg_weight = chez.mul(2, len(support_values))
        # chez: map's order of application (the procedure only computes)
        support_weights = chez.map_(
            lambda n: chez.mul(neg_weight if n < 0 else 1, chez.abs_(n)),
            support_values)
        return weighted_average(support_values, support_weights)

    @message("get-theme-support-values")
    def get_theme_support_values(this, self):
        def support(theme):
            if tell(self, "incompatible-with-theme?", theme) is not False:
                return chez.sub(percent(tell(theme, "get-activation")))
            if tell(self, "supported-by-theme?", theme) is not False:
                return percent(tell(theme, "get-activation"))
            return 0
        # chez: map's order of application
        return chez.map_(support, tell(_metacat.themes.g_themespace, "get-active-themes",
                                       this.theme_type))

    @message("boost-themespace-activations")
    def boost_themespace_activations(this, self):
        tell(self, "boost-themes")
        tell(_metacat.themes.g_themespace, "update-dominant-themes", this.theme_type)
        # 1.2: not gated by %workspace-graphics% (anomalies: "Bridges call themes.ss on every bridge")
        tell(setup.g_themespace_window, "update-graphics", this.theme_type)
        return "done"

    # This method just boosts theme activations.
    # It does not update dominant themes or graphics:
    @message("boost-themes")
    def boost_themes(this, self):
        strength = tell(self, "get-strength")

        def boost(d1, d2):
            if _metacat.themes.descriptions_affect_themespace_p(d1, d2) is not False:
                # the arguments only read
                theme = tell(_metacat.themes.g_themespace, "add-theme-if-possible",
                             this.theme_type,
                             tell(d1, "get-description-type"),
                             slipnet.get_label(tell(d1, "get-descriptor"),
                                               tell(d2, "get-descriptor")))
                if exists_p(theme):
                    if this.spanning_bridge_p is not False:
                        tell(theme, "boost-activation", chez.mul(2, strength))
                    else:
                        tell(theme, "boost-activation", strength)
            return None
        # cross-product-for-each: utilities.ss's order (l1 first to last, then l2)
        cross_product_for_each(boost,
                               tell(this.object1, "get-descriptions"),
                               tell(this.object2, "get-descriptions"))
        return "done"

    @message("get-associated-thematic-relations")
    def get_associated_thematic_relations(this, self):
        return cross_product_filter_map(
            _metacat.themes.descriptions_affect_themespace_p,
            lambda d1, d2: [tell(d1, "get-description-type"),
                            slipnet.get_label(tell(d1, "get-descriptor"),
                                              tell(d2, "get-descriptor"))],
            tell(this.object1, "get-descriptions"),
            tell(this.object2, "get-descriptions"))

    def _get_incompatible_bridges(this, self, bridge_orientation, incompatible_bridges_p):
        # (append a b c): every part only reads, so Chez's order of evaluating
        # them (it may take the last first) is not observable
        part1 = filter_(lambda b: incompatible_bridges_p(b, self),
                        tell(workspace.g_workspace, "get-bridges", this.bridge_type))
        part2 = group_incompatible_bridges(bridge_orientation, this.object1, this.object2)
        if workspace_objects.both_spanning_groups_p(this.object1, this.object2) is not False:
            direction_CM = select_meth(this.concept_mappings,
                                       "CM-type?", slipnet.plato_direction_category)
            if exists_p(direction_CM):
                part3 = direction_incompatible_bridges(bridge_orientation,
                                                       this.object1, this.object2,
                                                       direction_CM)
            else:
                part3 = []
        else:
            part3 = []
        return remq_duplicates(part1 + part2 + part3)

    def _get_incompatible_bond(this, self, incompatible_with_any_CM_p):
        object1, object2 = this.object1, this.object2
        # a two-binding let; both bindings only read
        if tell(object1, "leftmost-in-string?") is not False:
            bond1 = tell(object1, "get-right-bond")
        else:
            bond1 = tell(object1, "get-left-bond")
        if tell(object2, "leftmost-in-string?") is not False:
            bond2 = tell(object2, "get-right-bond")
        else:
            bond2 = tell(object2, "get-left-bond")
        if (exists_p(bond1)
                and exists_p(bond2)
                and bonds.directed_p(bond1)
                and bonds.directed_p(bond2)):
            direction_category_CM = concept_mappings.make_concept_mapping(
                bond1,
                slipnet.plato_direction_category,
                tell(bond1, "get-direction"),
                bond2,
                slipnet.plato_direction_category,
                tell(bond2, "get-direction"))
            if incompatible_with_any_CM_p(direction_category_CM,
                                          this.concept_mappings) is not False:
                return bond2
        return False

    def _internally_coherent_p(this, self, supporting_CMs_p):
        relevant_distinguishing_CMs = tell(self, "get-relevant-distinguishing-CMs")
        return cross_product_ormap(
            lambda cm1, cm2: cm1 is not cm2 and supporting_CMs_p(cm1, cm2),
            relevant_distinguishing_CMs,
            relevant_distinguishing_CMs)

    def _calculate_external_strength(this, self, supporting_bridges_p):
        object1, object2 = this.object1, this.object2
        if ((letter_p(object1) and tell(object1, "spans-whole-string?") is not False)
                or (letter_p(object2) and tell(object2, "spans-whole-string?") is not False)):
            return 100
        supporting_bridges = filter_(
            lambda b: supporting_bridges_p(self, b),
            chez.remq(self, tell(workspace.g_workspace, "get-bridges", this.bridge_type)))
        total_support = sum_(tell_all(supporting_bridges, "get-strength"))
        # 1.2: a one-argument * (anomalies: "Dead code in bridges.ss")
        return round_(chez.mul(chez.min_(100, total_support)))

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.workspace_structure)


class HorizontalBridge(_BridgeClauses):
    """bridges.ss: make-horizontal-bridge (the closure)"""
    __slots__ = ("translated_rule_bridge_p",)

    def __init__(this, object1, object2, concept_mappings_):
        this.object1 = object1
        this.object2 = object2
        this.concept_mappings = concept_mappings_
        # the let*, in order
        which_string = tell(object1, "which-string")
        if which_string == "initial":
            this.bridge_type = "top"
        elif which_string == "target":
            this.bridge_type = "bottom"
        else:
            this.bridge_type = None     # (case ...) without else: void
        this.theme_type = _metacat.themes.bridge_type_to_theme_type(this.bridge_type)
        this.workspace_structure = workspace_structures.make_workspace_structure()
        # bond-CMs = BondCtgy and BondFacet concept-mappings only
        # all-CMs = CMs + bond-CMs
        # symmetric-slippages = symmetric versions of all-CMs _slippages_
        this.bond_concept_mappings = []
        this.all_concept_mappings = concept_mappings_
        this.symmetric_slippages = []
        this.flipped_group1_p = False
        this.original_group1 = False
        this.flipped_group2_p = False
        this.original_group2 = False
        this.spanning_bridge_p = workspace_objects.both_spanning_objects_p(object1, object2)
        # group-spanning-bridge? is only for graphics purposes:
        this.group_spanning_bridge_p = workspace_objects.both_spanning_groups_p(object1, object2)
        this.translated_rule_bridge_p = False
        this.from_graphics_coord = _graphics_coord(object1, this.group_spanning_bridge_p,
                                                   "horizontal")
        this.to_graphics_coord = _graphics_coord(object2, this.group_spanning_bridge_p,
                                                 "horizontal")

    @message("get-orientation")
    def get_orientation(this, self):
        return "horizontal"

    @message("print")
    def print_(this, self):
        chez.printf("~a from ~a to ~a",
                    ("Spanning horizontal bridge" if this.spanning_bridge_p is not False
                     else "Horizontal bridge"),
                    tell(this.object1, "ascii-name"),
                    tell(this.object2, "ascii-name"))
        if tell(self, "get-proposal-level") < workspace.p_built:
            chez.printf((" (~a, not drawn)"
                         if (setup.p_workspace_graphics is not False
                             and tell(self, "drawn?") is False)
                         else " (~a)"),
                        _proposal_level_name(tell(self, "get-proposal-level")))
        chez.newline()
        for cm in this.all_concept_mappings:
            chez.printf("     ")
            print_(cm)
        return None

    @message("short-print")
    def short_print(this, self):
        chez.printf("~a from ~a to ~a",
                    ("Spanning horizontal bridge" if this.spanning_bridge_p is not False
                     else "Horizontal bridge"),
                    tell(this.object1, "ascii-name"),
                    tell(this.object2, "ascii-name"))
        if tell(self, "get-proposal-level") < workspace.p_built:
            chez.printf((" (~a, not drawn)"
                         if (setup.p_workspace_graphics is not False
                             and tell(self, "drawn?") is False)
                         else " (~a)"),
                        _proposal_level_name(tell(self, "get-proposal-level")))
        return chez.newline()

    @message("translated-rule-bridge?")
    def translated_rule_bridge_p_(this, self):
        return this.translated_rule_bridge_p

    @message("mark-as-translated-rule-bridge")
    def mark_as_translated_rule_bridge(this, self):
        this.translated_rule_bridge_p = True
        return "done"

    @message("set-new-from-graphics-coord")
    def set_new_from_graphics_coord(this, self, from_group):
        this.from_graphics_coord = tell(from_group, "get-group-spanning-bridge-graphics-coord",
                                        "horizontal")
        return "done"

    @message("set-new-to-graphics-coord")
    def set_new_to_graphics_coord(this, self, to_group):
        this.to_graphics_coord = tell(to_group, "get-group-spanning-bridge-graphics-coord",
                                      "horizontal")
        return "done"

    # Currently no concept-mapping graphics for horizontal bridges:
    @message("activate-concept-mapping-graphics")
    def activate_concept_mapping_graphics(this, self):
        return "done"

    @message("concept-mapping-graphics-active?")
    def concept_mapping_graphics_active_p(this, self):
        return False

    # This is used to set concept-mappings for horizontal bridges created
    # by translating a rule:
    @message("set-concept-mappings")
    def set_concept_mappings(this, self, CM_list):
        this.bond_concept_mappings = filter_meth(CM_list, "bond-concept-mapping?")
        this.concept_mappings = remq_elements(this.bond_concept_mappings, CM_list)
        this.all_concept_mappings = this.concept_mappings + this.bond_concept_mappings
        this.symmetric_slippages = tell_all(filter_meth(this.all_concept_mappings, "slippage?"),
                                            "symmetric-mapping")
        return "done"

    @message("get-non-symmetric-non-bond-slippages")
    def get_non_symmetric_non_bond_slippages(this, self):
        return filter_meth(this.concept_mappings, "slippage?")

    @message("add-concept-mappings")
    def add_concept_mappings(this, self, cm_list):
        this.concept_mappings = cm_list + this.concept_mappings
        this.all_concept_mappings = cm_list + this.all_concept_mappings
        return "done"

    @message("delete-concept-mapping-type")
    def delete_concept_mapping_type(this, self, type_):
        # a two-binding let; both only read
        cm = select_meth(this.all_concept_mappings, "CM-type?", type_)
        ss = select_meth(this.symmetric_slippages, "CM-type?", type_)
        this.concept_mappings = chez.remq(cm, this.concept_mappings)
        this.all_concept_mappings = chez.remq(cm, this.all_concept_mappings)
        if exists_p(ss):
            this.symmetric_slippages = chez.remq(ss, this.symmetric_slippages)
        return "done"

    @message("get-incompatible-bridges")
    def get_incompatible_bridges(this, self):
        return this._get_incompatible_bridges(self, "horizontal",
                                              lambda b1, b2: incompatible_horizontal_bridges_p(b1, b2))

    @message("get-incompatible-bond")
    def get_incompatible_bond(this, self):
        return this._get_incompatible_bond(
            self, lambda cm, l: incompatible_with_any_horizontal_CM_p(cm, l))

    @message("internally-coherent?")
    def internally_coherent_p(this, self):
        return this._internally_coherent_p(
            self, lambda cm1, cm2: supporting_horizontal_CMs_p(cm1, cm2))

    @message("StrPosCtgy:Opposite-slippage?")
    def StrPosCtgy__Opposite_slippage_p(this, self):
        string_position_CM = select_meth(this.concept_mappings,
                                         "CM-type?", slipnet.plato_string_position_category)
        return (exists_p(string_position_CM)
                and tell(string_position_CM, "get-label") is slipnet.plato_opposite)

    @message("calculate-internal-strength")
    def calculate_internal_strength(this, self):
        relevant_distinguishing_CMs = tell(self, "get-relevant-distinguishing-CMs")
        if len(relevant_distinguishing_CMs) == 0:
            return 0
        average_strength = average(tell_all(relevant_distinguishing_CMs, "get-strength"))
        num_of_concept_mappings = len(relevant_distinguishing_CMs)
        if num_of_concept_mappings == 1:
            num_of_concept_mappings_factor = 0.8
        elif num_of_concept_mappings == 2:
            num_of_concept_mappings_factor = 1.2
        else:
            num_of_concept_mappings_factor = 1.6
        internal_coherence_factor = (2.5 if tell(self, "internally-coherent?") is not False
                                     else 1.0)
        singleton_factor = singleton_letter_factor(this.object1, this.object2)
        return chez.min_(100, round_(chez.mul(average_strength,
                                              num_of_concept_mappings_factor,
                                              internal_coherence_factor,
                                              # Only used for horizontal bridges:
                                              singleton_factor)))

    @message("calculate-external-strength")
    def calculate_external_strength(this, self):
        return this._calculate_external_strength(
            self, lambda b1, b2: supporting_horizontal_bridges_p(b1, b2))


def make_horizontal_bridge(object1, object2, concept_mappings_):
    """bridges.ss: make-horizontal-bridge"""
    return HorizontalBridge(object1, object2, concept_mappings_)


class VerticalBridge(_BridgeClauses):
    """bridges.ss: make-vertical-bridge (the closure)"""
    __slots__ = ("concept_mapping_graphics_active_p", "concept_mapping_list_coord",
                 "bridge_label_number")

    def __init__(this, object1, object2, concept_mappings_):
        this.object1 = object1
        this.object2 = object2
        this.concept_mappings = concept_mappings_
        # the let*, in order
        this.bridge_type = "vertical"
        this.theme_type = _metacat.themes.bridge_type_to_theme_type(this.bridge_type)
        this.workspace_structure = workspace_structures.make_workspace_structure()
        # bond-CMs = BondCtgy and BondFacet concept-mappings only
        # all-CMs = CMs + bond-CMs
        # symmetric-slippages = symmetric versions of all-CMs _slippages_
        this.bond_concept_mappings = []
        this.all_concept_mappings = concept_mappings_
        this.symmetric_slippages = []
        this.flipped_group1_p = False
        this.original_group1 = False
        this.flipped_group2_p = False
        this.original_group2 = False
        this.spanning_bridge_p = workspace_objects.both_spanning_objects_p(object1, object2)
        # group-spanning-bridge? is only for graphics purposes:
        this.group_spanning_bridge_p = workspace_objects.both_spanning_groups_p(object1, object2)
        this.from_graphics_coord = _graphics_coord(object1, this.group_spanning_bridge_p,
                                                   "vertical")
        this.to_graphics_coord = _graphics_coord(object2, this.group_spanning_bridge_p,
                                                 "vertical")
        this.concept_mapping_graphics_active_p = False
        this.concept_mapping_list_coord = False
        this.bridge_label_number = False

    @message("get-orientation")
    def get_orientation(this, self):
        return "vertical"

    @message("print")
    def print_(this, self):
        chez.printf("~a from ~a to ~a",
                    ("Spanning vertical bridge" if this.spanning_bridge_p is not False
                     else "Vertical bridge"),
                    tell(this.object1, "ascii-name"),
                    tell(this.object2, "ascii-name"))
        if tell(self, "get-proposal-level") < workspace.p_built:
            chez.printf((" (~a, not drawn)"
                         if (setup.p_workspace_graphics is not False
                             and tell(self, "drawn?") is False)
                         else " (~a)"),
                        _proposal_level_name(tell(self, "get-proposal-level")))
        elif (setup.p_workspace_graphics is not False
              and this.group_spanning_bridge_p is False):
            chez.printf(" (#~a)", this.bridge_label_number)
        chez.newline()
        for cm in this.all_concept_mappings:
            chez.printf("     ")
            print_(cm)
        return None

    @message("short-print")
    def short_print(this, self):
        chez.printf("~a from ~a to ~a",
                    ("Spanning vertical bridge" if this.spanning_bridge_p is not False
                     else "Vertical bridge"),
                    tell(this.object1, "ascii-name"),
                    tell(this.object2, "ascii-name"))
        if tell(self, "get-proposal-level") < workspace.p_built:
            chez.printf((" (~a, not drawn)"
                         if (setup.p_workspace_graphics is not False
                             and tell(self, "drawn?") is False)
                         else " (~a)"),
                        _proposal_level_name(tell(self, "get-proposal-level")))
        return chez.newline()

    @message("set-new-from-graphics-coord")
    def set_new_from_graphics_coord(this, self, from_group):
        this.from_graphics_coord = tell(from_group, "get-group-spanning-bridge-graphics-coord",
                                        "vertical")
        return "done"

    @message("set-new-to-graphics-coord")
    def set_new_to_graphics_coord(this, self, to_group):
        this.to_graphics_coord = tell(to_group, "get-group-spanning-bridge-graphics-coord",
                                      "vertical")
        return "done"

    @message("concept-mapping-graphics-active?")
    def concept_mapping_graphics_active_p_(this, self):
        return this.concept_mapping_graphics_active_p

    @message("get-cm-list-coord")
    def get_cm_list_coord(this, self):
        return this.concept_mapping_list_coord

    @message("get-bridge-label-number")
    def get_bridge_label_number(this, self):
        return this.bridge_label_number

    @message("activate-concept-mapping-graphics")
    def activate_concept_mapping_graphics(this, self):
        window = setup.g_workspace_window
        this.concept_mapping_graphics_active_p = True
        if this.group_spanning_bridge_p is not False:
            this.bridge_label_number = 0
        else:
            this.bridge_label_number = _metacat.bridge_graphics.new_bridge_label_number(self)
        if this.group_spanning_bridge_p is False:
            this.concept_mapping_list_coord = tell(window, "get-cm-list-coord",
                                                   this.bridge_label_number)
        else:
            # a two-binding let; both only read
            spanning_cm_list_x = chez.add(tell(window, "get-spanning-vertical-bridge-right-x"),
                                          tell(window, "get-spanning-cm-list-offset"))
            spanning_cm_list_y = chez.mul(Fraction(1, 2),
                                          chez.add(y_coord(this.from_graphics_coord),
                                                   y_coord(this.to_graphics_coord)))
            this.concept_mapping_list_coord = [spanning_cm_list_x, spanning_cm_list_y]
        tell(self, "set-concept-mapping-pexps", this.all_concept_mappings, 0)
        return "done"

    @message("set-concept-mapping-pexps")
    def set_concept_mapping_pexps(this, self, cm_list, starting_index_num):
        for i in range(0, chez.sub1(len(cm_list)) + 1):
            cm = nth(i, cm_list)
            n = chez.add(i, starting_index_num)
            if this.group_spanning_bridge_p is not False:
                line_offset = chez.mul(1, chez.sub1(chez.mul(chez.expt(-1, n),
                                                             ceiling(chez.div(n, 2)))))
            else:
                line_offset = chez.mul(1, chez.sub(n))
            tell(cm, "set-graphics-pexp",
                 ["let-sgl", [["origin", this.concept_mapping_list_coord]],
                  ["text", ["text-relative", [0, line_offset]], tell(cm, "print-name")]])
        return "done"

    @message("add-concept-mappings")
    def add_concept_mappings(this, self, cm_list):
        if setup.p_workspace_graphics is not False:
            tell(self, "set-concept-mapping-pexps", cm_list, len(this.all_concept_mappings))
        this.concept_mappings = cm_list + this.concept_mappings
        this.all_concept_mappings = cm_list + this.all_concept_mappings
        return "done"

    @message("delete-concept-mapping-type")
    def delete_concept_mapping_type(this, self, type_):
        # a two-binding let; both only read
        cm = select_meth(this.all_concept_mappings, "CM-type?", type_)
        ss = select_meth(this.symmetric_slippages, "CM-type?", type_)
        this.concept_mappings = chez.remq(cm, this.concept_mappings)
        this.all_concept_mappings = chez.remq(cm, this.all_concept_mappings)
        if exists_p(ss):
            this.symmetric_slippages = chez.remq(ss, this.symmetric_slippages)
        if setup.p_workspace_graphics is not False:
            window = setup.g_workspace_window
            tell(window, "caching-on")
            tell(window, "erase-concept-mapping", cm)
            for other_cm in this.all_concept_mappings:
                tell(window, "erase-concept-mapping", other_cm)
            tell(self, "set-concept-mapping-pexps", this.all_concept_mappings, 0)
            for other_cm in this.all_concept_mappings:
                tell(window, "draw-concept-mapping", other_cm)
            tell(window, "flush")
        return "done"

    @message("get-incompatible-bridges")
    def get_incompatible_bridges(this, self):
        return this._get_incompatible_bridges(self, "vertical",
                                              lambda b1, b2: incompatible_vertical_bridges_p(b1, b2))

    @message("get-incompatible-bond")
    def get_incompatible_bond(this, self):
        return this._get_incompatible_bond(
            self, lambda cm, l: incompatible_with_any_vertical_CM_p(cm, l))

    @message("internally-coherent?")
    def internally_coherent_p(this, self):
        return this._internally_coherent_p(
            self, lambda cm1, cm2: supporting_vertical_CMs_p(cm1, cm2))

    @message("calculate-internal-strength")
    def calculate_internal_strength(this, self):
        relevant_distinguishing_CMs = tell(self, "get-relevant-distinguishing-CMs")
        if len(relevant_distinguishing_CMs) == 0:
            return 0
        average_strength = average(tell_all(relevant_distinguishing_CMs, "get-strength"))
        num_of_concept_mappings = len(relevant_distinguishing_CMs)
        if num_of_concept_mappings == 1:
            num_of_concept_mappings_factor = 0.8
        elif num_of_concept_mappings == 2:
            num_of_concept_mappings_factor = 1.2
        else:
            num_of_concept_mappings_factor = 1.6
        internal_coherence_factor = (2.5 if tell(self, "internally-coherent?") is not False
                                     else 1.0)
        return chez.min_(100, round_(chez.mul(average_strength,
                                              num_of_concept_mappings_factor,
                                              internal_coherence_factor)))

    @message("calculate-external-strength")
    def calculate_external_strength(this, self):
        return this._calculate_external_strength(
            self, lambda b1, b2: supporting_vertical_bridges_p(b1, b2))


def make_vertical_bridge(object1, object2, concept_mappings_):
    """bridges.ss: make-vertical-bridge"""
    return VerticalBridge(object1, object2, concept_mappings_)


def singleton_letter_factor(object1, object2):
    """bridges.ss: singleton-letter-factor"""
    if singleton_letter_p(object1) is not False:
        return 1 if letter_p(object2) else 0.1
    if singleton_letter_p(object2) is not False:
        return 1 if letter_p(object1) else 0.1
    if tell(object1, "singleton-group?") is not False:
        return 1 if group_p(object2) else 0.1
    if tell(object2, "singleton-group?") is not False:
        return 1 if group_p(object1) else 0.1
    return 1


def singleton_letter_p(object_):
    """bridges.ss: singleton-letter?"""
    enclosing_group = tell(object_, "get-enclosing-group")
    if not (letter_p(object_) and exists_p(enclosing_group)):
        return False
    return tell(enclosing_group, "singleton-group?")


def group_incompatible_bridges(bridge_orientation, object1, object2):
    """bridges.ss: group-incompatible-bridges"""
    # a two-binding let and a four-part append; every part only reads
    enclosing_group1 = tell(object1, "get-enclosing-group")
    enclosing_group2 = tell(object2, "get-enclosing-group")
    if group_p(object1):
        subobject_bridges = tell(object1, "get-subobject-bridges", bridge_orientation)
        if letter_p(object2):
            part1 = subobject_bridges
        else:
            part1 = filter_out(
                lambda b: tell(object2, "top-level-member?", tell(b, "get-object2")),
                subobject_bridges)
    else:
        part1 = []
    if group_p(object2):
        subobject_bridges = tell(object2, "get-subobject-bridges", bridge_orientation)
        if letter_p(object1):
            part2 = subobject_bridges
        else:
            part2 = filter_out(
                lambda b: tell(object1, "top-level-member?", tell(b, "get-object1")),
                subobject_bridges)
    else:
        part2 = []
    if (exists_p(enclosing_group1)
            and exists_p(tell(enclosing_group1, "get-bridge", bridge_orientation))):
        group_bridge = tell(enclosing_group1, "get-bridge", bridge_orientation)
        other_object = tell(group_bridge, "get-object2")
        if not exists_p(enclosing_group2) or enclosing_group2 is not other_object:
            part3 = [group_bridge]
        else:
            part3 = []
    else:
        part3 = []
    if (exists_p(enclosing_group2)
            and exists_p(tell(enclosing_group2, "get-bridge", bridge_orientation))):
        group_bridge = tell(enclosing_group2, "get-bridge", bridge_orientation)
        other_object = tell(group_bridge, "get-object1")
        if not exists_p(enclosing_group1) or enclosing_group1 is not other_object:
            part4 = [group_bridge]
        else:
            part4 = []
    else:
        part4 = []
    return part1 + part2 + part3 + part4


def direction_incompatible_bridges(bridge_orientation, group1, group2, direction_CM):
    """bridges.ss: direction-incompatible-bridges"""
    direction_label = tell(direction_CM, "get-label")

    def partition_function(pred1_p, pred2_p):
        def same_order_p(b1, b2):
            # a four-binding let; every binding only reads
            b1_pos1 = tell(tell(b1, "get-object1"), "get-left-string-pos")
            b1_pos2 = tell(tell(b1, "get-object2"), "get-left-string-pos")
            b2_pos1 = tell(tell(b2, "get-object1"), "get-left-string-pos")
            b2_pos2 = tell(tell(b2, "get-object2"), "get-left-string-pos")
            return ((b1_pos1 < b2_pos1 and pred1_p(b1_pos2, b2_pos2))
                    or (b1_pos1 > b2_pos1 and pred2_p(b1_pos2, b2_pos2)))
        return same_order_p
    subobject_bridges = intersect(tell(group1, "get-subobject-bridges", bridge_orientation),
                                  tell(group2, "get-subobject-bridges", bridge_orientation))
    if direction_label is slipnet.plato_identity:
        pred = partition_function(lambda a, b: a < b, lambda a, b: a > b)
    elif direction_label is slipnet.plato_opposite:
        pred = partition_function(lambda a, b: a > b, lambda a, b: a < b)
    else:
        # 1.2: cond without else: void, which partition applies (an error) if it
        # ever compares two bridges
        pred = None
    mutually_compatible_bridges = select_longest_list(partition(pred, subobject_bridges))
    return remq_elements(mutually_compatible_bridges, subobject_bridges)


def bottom_up_bridge_scout():
    """bridges.ss: bottom-up-bridge-scout (the codelet procedure)"""
    # the let*, in order: the bridge type (a draw), then object1 (a draw),
    # then object2 (a draw)
    if setup.p_justify_mode is not False:
        bridge_types = ["top", "vertical", "bottom"]
    else:
        bridge_types = ["top", "vertical"]
    # chez: map's order of application (get-mapping-strength only reads)
    bridge_type_weights = chez.map_(
        lambda bridge_type: hundred_minus(tell(workspace.g_workspace, "get-mapping-strength",
                                               bridge_type)),
        bridge_types)
    bridge_type = stochastic_pick(bridge_types, bridge_type_weights)
    bridge_orientation = bridge_type_to_orientation(bridge_type)
    strings = _strings_of(bridge_type)
    object1 = tell(first(strings), "choose-object",
                   "get-inter-string-salience", bridge_orientation)
    object2 = tell(second(strings), "choose-object",
                   "get-inter-string-salience", bridge_orientation)
    say("Chose ", bridge_type, " objects ",
        tell(object1, "ascii-name"), " and ", tell(object2, "ascii-name"))
    if workspace_objects.lone_spanning_object_p(object1, object2) is not False:
        say("One object spans string, other doesn't. Fizzling.")
        sugar.fizzle()
    if setup.p_workspace_graphics is not False:
        _metacat.bridge_graphics.draw_bridge_grope(bridge_orientation, object1, object2)
    # the arguments only read; make-concept-mapping's activations are in
    # cross-product-filter-map's order
    possible_CMs = all_possible_bridge_CMs(
        bridge_orientation,
        object1, tell(object1, "get-relevant-descriptions"),
        object2, tell(object2, "get-relevant-descriptions"))
    # chez: map's order of application (the procedure only computes)
    slippabilities = chez.map_(
        lambda x: formulas.temp_adjusted_probability(percent(x)),
        tell_all(possible_CMs, "get-slippability"))
    say("Possible CMs:")
    if setup.p_verbose is not False:
        print_(possible_CMs)
    say("Slippabilities: ")
    say(chez.map_(round_to_100ths, slippabilities))
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < product(chez.map_(one_minus, slippabilities)):
        say("Couldn't make necessary slippages. Fizzling.")
        sugar.fizzle()
    # Bridges that don't have at least one relevant distinguishing CM with
    # an Identity/Opposite relation should not be proposed.  This prevents
    # making bridges between objects without _some_ a priori justification.
    # However, active horizontal-bridge themes can override this.
    # Example: in abc->abcd, b--d bridge CMs are ObjCtgy:letter=>letter,
    # StrPosCtgy:middle=>lmost, LettCtgy:b=>d.  The ObjCtgy CM is not
    # distinguishing.  Bridge should not be made based only on StrPosCtgy
    # and LettCtgy slippages (if an active horizontal-bridge theme such as
    # StrPosCtgy:different exists, such a bridge may be proposed by
    # thematic-bridge-scout codelets):
    distinguishing_CMs = filter_meth(possible_CMs, "distinguishing-identity/opposite?")
    if len(distinguishing_CMs) == 0:
        say("No distinguishing identity/opposite concept-mappings. Fizzling.")
        sugar.fizzle()
    if workspace_objects.both_spanning_groups_p(object1, object2) is not False:
        flip2_p = reverse_direction_orientation_p(possible_CMs)
    else:
        flip2_p = False
    proposed_bridge = propose_bridge(bridge_orientation, object1, False, object2, flip2_p)
    average_distinguishing_CM_strength = average(
        tell_all(tell(proposed_bridge, "get-distinguishing-CMs"), "get-strength"))
    return sugar.post_codelet_star(average_distinguishing_CM_strength,
                                   coderack.bridge_evaluator, proposed_bridge)


def _strings_of(bridge_type):
    """(case bridge-type (top *top-strings*) (bottom *bottom-strings*)
    (vertical *vertical-strings*)), void otherwise."""
    if bridge_type == "top":
        return workspace.g_top_strings
    if bridge_type == "bottom":
        return workspace.g_bottom_strings
    if bridge_type == "vertical":
        return workspace.g_vertical_strings
    return None


def important_object_bridge_scout():
    """bridges.ss: important-object-bridge-scout (the codelet procedure)"""
    # the let*, in order: the bridge type (a draw), object1 (a draw), then its
    # description (a draw)
    if setup.p_justify_mode is not False:
        bridge_types = ["top", "vertical", "bottom"]
    else:
        bridge_types = ["top", "vertical"]
    # chez: map's order of application (get-mapping-strength only reads)
    bridge_type_weights = chez.map_(
        lambda bridge_type: hundred_minus(tell(workspace.g_workspace, "get-mapping-strength",
                                               bridge_type)),
        bridge_types)
    bridge_type = stochastic_pick(bridge_types, bridge_type_weights)
    bridge_orientation = bridge_type_to_orientation(bridge_type)
    strings = _strings_of(bridge_type)
    object1 = tell(first(strings), "choose-object", "get-relative-importance")
    object1_description = tell(object1, "choose-relevant-distinguishing-description-by-depth")
    say("Chose ", tell(object1, "ascii-name"), " in ",
        tell(first(strings), "generic-name"), ".")
    if not exists_p(object1_description):
        say("No relevant distinguishing descriptions. Fizzling.")
        sugar.fizzle()
    object1_descriptor = tell(object1_description, "get-descriptor")
    applicable_slippage = select(
        lambda s: tell(s, "get-descriptor1") is object1_descriptor,
        tell(workspace.g_workspace, "get-all-slippages", bridge_type))
    if exists_p(applicable_slippage):
        object2_descriptor = tell(applicable_slippage, "get-descriptor2")
    else:
        object2_descriptor = object1_descriptor
    object2_candidates = filter_(
        lambda object_: chez.ormap(
            lambda d: tell(d, "get-descriptor") is object2_descriptor,
            tell(object_, "get-relevant-descriptions")),
        tell(second(strings), "get-objects"))
    say("Chose description ", object1_description)
    say("Looking for ", tell(second(strings), "generic-name"),
        " object with descriptor ", object2_descriptor)
    if len(object2_candidates) == 0:
        say("No object with proper descriptor. Fizzling.")
        sugar.fizzle()
    object2 = stochastic_pick_by_method(object2_candidates,
                                        "get-inter-string-salience", bridge_orientation)
    say("Chose object2 to be ", tell(object2, "ascii-name"))
    if workspace_objects.lone_spanning_object_p(object1, object2) is not False:
        say("One object spans string, other doesn't. Fizzling.")
        sugar.fizzle()
    if setup.p_workspace_graphics is not False:
        _metacat.bridge_graphics.draw_bridge_grope(bridge_orientation, object1, object2)
    possible_CMs = all_possible_bridge_CMs(
        bridge_orientation,
        object1, tell(object1, "get-relevant-descriptions"),
        object2, tell(object2, "get-relevant-descriptions"))
    # chez: map's order of application (the procedure only computes)
    slippabilities = chez.map_(
        lambda x: formulas.temp_adjusted_probability(percent(x)),
        tell_all(possible_CMs, "get-slippability"))
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < product(chez.map_(one_minus, slippabilities)):
        say("Couldn't make necessary slippages. Fizzling.")
        sugar.fizzle()
    # Bridges that don't have at least one relevant distinguishing CM with
    # an Identity/Opposite relation should not be proposed.  This prevents
    # making bridges between objects without _some_ a priori justification.
    # However, active bridge themes can override this.  Example:
    # In abc->abcd, b--d bridge CMs are ObjCtgy:letter=>letter,
    # StrPosCtgy:middle=>lmost, LettCtgy:b=>d.  The ObjCtgy CM is not
    # distinguishing.  Bridge should not be made based only on StrPosCtgy
    # and LettCtgy slippages (if an active horizontal-bridge theme such as
    # StrPosCtgy:different exists, such a bridge may be proposed by
    # thematic-bridge-scout codelets):
    distinguishing_CMs = filter_meth(possible_CMs, "distinguishing-identity/opposite?")
    if len(distinguishing_CMs) == 0:
        say("No distinguishing identity/opposite concept-mappings. Fizzling.")
        sugar.fizzle()
    if workspace_objects.both_spanning_groups_p(object1, object2) is not False:
        flip2_p = reverse_direction_orientation_p(possible_CMs)
    else:
        flip2_p = False
    proposed_bridge = propose_bridge(bridge_orientation, object1, False, object2, flip2_p)
    average_distinguishing_CM_strength = average(
        tell_all(tell(proposed_bridge, "get-distinguishing-CMs"), "get-strength"))
    return sugar.post_codelet_star(average_distinguishing_CM_strength,
                                   coderack.bridge_evaluator, proposed_bridge)


def reverse_direction_orientation_p(concept_mappings_):
    """bridges.ss: reverse-direction-orientation?"""
    if ormap_meth(concept_mappings_, "CM-type?", slipnet.plato_direction_category) is False:
        return False
    if andmap_meth(filter_meth(concept_mappings_, "reversible-CM-type?"),
                   "opposite-mapping?") is False:
        return False
    return not slipnet.fully_active_p(slipnet.plato_opposite)


# Good example of the contradictory assumptions that are deeply embedded within
# the fabric of Copycat:  When proposing a new bridge, only _relevant_ descriptions
# are used to create the new bridge's concept-mappings.  So if a particular concept
# such as Direction-Category happens to be momentarily inactive, then no
# Direction-Category concept-mapping will be included with the bridge.  Suppose that
# a spanning bridge >abc>---<cba< currently exists, along with a-a, b-b, and c-c.
# If the proposed bridge is between >abc> and a flipped version of <cba<, that is,
# >abc>--->cba>, it will not be judged incompatible with a-a, b-b, and c-c, due to
# the lack of a Direction:right=>right concept-mapping.  These latter three bridges
# therefore will not be broken.  The end result will be that the original >abc>--<cba<
# spanning bridge is replaced by >abc>--->cba>, but the original three a-a, b-b, and
# c-c bridges are still intact, resulting in a contradictory situation.  This problem
# will crop up rarely, but completely unpredictably (depending on the ever-changing
# activations of slipnodes).


def propose_bridge(bridge_orientation, object1, flip1_p, object2, flip2_p):
    """bridges.ss: propose-bridge"""
    # the let*, in order
    obj1 = tell(object1, "make-flipped-version") if flip1_p is not False else object1
    obj2 = tell(object2, "make-flipped-version") if flip2_p is not False else object2
    # This is a hack in order to get around the problem described above
    # (the arguments only read):
    if tell(obj1, "string-spanning-group?") is not False:
        obj1_descriptions = tell(obj1, "get-descriptions")
    else:
        obj1_descriptions = tell(obj1, "get-relevant-descriptions")
    if tell(obj2, "string-spanning-group?") is not False:
        obj2_descriptions = tell(obj2, "get-descriptions")
    else:
        obj2_descriptions = tell(obj2, "get-relevant-descriptions")
    concept_mappings_ = all_possible_bridge_CMs(bridge_orientation,
                                                obj1, obj1_descriptions,
                                                obj2, obj2_descriptions)
    if bridge_orientation == "horizontal":
        proposed_bridge = make_horizontal_bridge(obj1, obj2, concept_mappings_)
    elif bridge_orientation == "vertical":
        proposed_bridge = make_vertical_bridge(obj1, obj2, concept_mappings_)
    else:
        proposed_bridge = None      # (case ...) without else: void
    if flip1_p is not False:
        tell(proposed_bridge, "mark-flipped-group1", object1)
    if flip2_p is not False:
        tell(proposed_bridge, "mark-flipped-group2", object2)
    if setup.p_workspace_graphics is not False:
        if flip1_p is not False:
            tell(proposed_bridge, "set-new-from-graphics-coord", object1)
        if flip2_p is not False:
            tell(proposed_bridge, "set-new-to-graphics-coord", object2)
    tell(workspace.g_workspace, "add-proposed-bridge", proposed_bridge)
    tell(proposed_bridge, "update-proposal-level", workspace.p_proposed)
    for cm in concept_mappings_:
        tell(cm, "activate-descriptions")
    if setup.p_workspace_graphics is not False:
        _metacat.bridge_graphics.bridge_graphics("set-pexp-and-draw", proposed_bridge)
    if flip1_p is not False and flip2_p is not False:
        say("Proposing ", bridge_orientation, " bridge with both flipped groups:")
    elif flip1_p is not False:
        say("Proposing ", bridge_orientation, " bridge with flipped group1:")
    elif flip2_p is not False:
        say("Proposing ", bridge_orientation, " bridge with flipped group2:")
    else:
        say("Proposing ", bridge_orientation, " bridge:")
    if setup.p_verbose is not False:
        print_(proposed_bridge)
    if (bridge_orientation == "horizontal"
            and tell(object1, "get-platonic-length") is not tell(object2, "get-platonic-length")):
        tell(slipnet.plato_length, "activate-from-workspace")
        sugar.post_codelet_star(coderack.p_very_high_urgency,
                                coderack.top_down_description_scout,
                                slipnet.plato_length, tell(object1, "get-string"))
        sugar.post_codelet_star(coderack.p_very_high_urgency,
                                coderack.top_down_description_scout,
                                slipnet.plato_length, tell(object2, "get-string"))
        if letter_p(object1) and group_p(object2):
            tell(tell(object2, "get-group-category"), "activate-from-workspace")
            sugar.post_codelet_star(coderack.p_very_high_urgency,
                                    coderack.top_down_group_scout__category,
                                    tell(object2, "get-group-category"),
                                    tell(object1, "get-string"))
        if group_p(object1) and letter_p(object2):
            tell(tell(object1, "get-group-category"), "activate-from-workspace")
            sugar.post_codelet_star(coderack.p_very_high_urgency,
                                    coderack.top_down_group_scout__category,
                                    tell(object1, "get-group-category"),
                                    tell(object2, "get-string"))
    return proposed_bridge


def bridge_evaluator(proposed_bridge):
    """bridges.ss: bridge-evaluator (the codelet procedure)"""
    bridge_orientation = tell(proposed_bridge, "get-orientation")  # noqa: F841 (unused, as in 1.2)
    if setup.p_verbose is not False:
        print_(proposed_bridge)
    if not (tell(workspace.g_workspace, "object-exists?",
                 tell(proposed_bridge, "get-original-object1")) is not False
            and tell(workspace.g_workspace, "object-exists?",
                     tell(proposed_bridge, "get-original-object2")) is not False):
        say("One or both objects no longer exist. Fizzling.")
        # Erasing the bridge is unnecessary, since it was
        # erased at the time object1 or object2 got broken.
        sugar.fizzle()
    if setup.p_workspace_graphics is not False:
        _metacat.bridge_graphics.bridge_graphics("flash", proposed_bridge)
    tell(proposed_bridge, "update-strength")
    strength = tell(proposed_bridge, "get-strength")
    say("Strength is ", round_(strength))
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(formulas.temp_adjusted_probability(percent(strength))):
        say("Bridge not strong enough. Fizzling.")
        tell(workspace.g_workspace, "delete-proposed-bridge", proposed_bridge)
        if setup.p_workspace_graphics is not False:
            _metacat.bridge_graphics.bridge_graphics("erase", proposed_bridge)
        sugar.fizzle()
    concept_mappings_ = tell(proposed_bridge, "get-concept-mappings")
    for cm in concept_mappings_:
        tell(cm, "activate-descriptions")
    tell(proposed_bridge, "update-proposal-level", workspace.p_evaluated)
    if setup.p_workspace_graphics is not False:
        _metacat.bridge_graphics.bridge_graphics("update-level", proposed_bridge)
    return sugar.post_codelet_star(strength, coderack.bridge_builder, proposed_bridge)


def bridge_builder(proposed_bridge):
    """bridges.ss: bridge-builder (the codelet procedure)"""
    if setup.p_verbose is not False:
        print_(proposed_bridge)
    if not (tell(workspace.g_workspace, "object-exists?",
                 tell(proposed_bridge, "get-original-object1")) is not False
            and tell(workspace.g_workspace, "object-exists?",
                     tell(proposed_bridge, "get-original-object2")) is not False):
        say("One or both objects no longer exist. Fizzling.")
        # Erasing the bridge is unnecessary, since it was
        # erased at the time object1 or object2 got broken.
        sugar.fizzle()
    tell(workspace.g_workspace, "delete-proposed-bridge", proposed_bridge)
    # a four-binding let; every binding only reads
    bridge_orientation = tell(proposed_bridge, "get-orientation")
    object1 = tell(proposed_bridge, "get-object1")
    object2 = tell(proposed_bridge, "get-object2")
    concept_mappings_ = tell(proposed_bridge, "get-concept-mappings")
    # This is necessary because StrPosCtgy:middle descriptions can now be deleted:
    if chez.andmap(lambda type_: (tell(object1, "description-type-present?", type_) is not False
                                  and tell(object2, "description-type-present?", type_)
                                  is not False),
                   tell(proposed_bridge, "get-concept-mapping-types")) is False:
        say("Not all necessary descriptions still exist. Fizzling.")
        if setup.p_workspace_graphics is not False:
            _metacat.bridge_graphics.bridge_graphics("erase", proposed_bridge)
        sugar.fizzle()
    if bridge_between_p(bridge_orientation, object1, object2) is not False:
        say("This bridge already exists.")
        say("Adding any new concept-mappings and fizzling...")
        for cm in concept_mappings_:
            tell(cm, "activate-label")
        existing_bridge = tell(object1, "get-bridge", bridge_orientation)
        concept_mappings_to_be_added = filter_out(
            lambda cm: tell(existing_bridge, "concept-mapping-present?", cm),
            concept_mappings_)
        if len(concept_mappings_to_be_added) != 0:
            tell(existing_bridge, "add-concept-mappings", concept_mappings_to_be_added)
            _metacat.trace.monitor_new_concept_mappings(concept_mappings_to_be_added,
                                                        existing_bridge)
            if setup.p_verbose is not False:
                print_(concept_mappings_to_be_added)
            if setup.p_workspace_graphics is not False:
                _metacat.bridge_graphics.bridge_graphics("flash", existing_bridge)
                # Currently only vertical bridges show concept-mappings:
                if tell(proposed_bridge, "bridge-type?", "vertical") is not False:
                    window = setup.g_workspace_window
                    tell(window, "caching-on")
                    for cm in concept_mappings_to_be_added:
                        tell(window, "draw-concept-mapping", cm)
                    tell(window, "flush")
        sugar.fizzle()
    if andmap_meth(concept_mappings_, "relevant?") is False:
        say("Not all concept-mappings are still relevant. Fizzling.")
        if setup.p_workspace_graphics is not False:
            _metacat.bridge_graphics.bridge_graphics("erase", proposed_bridge)
        sugar.fizzle()
    if setup.p_workspace_graphics is not False:
        _metacat.bridge_graphics.bridge_graphics("flash", proposed_bridge)
    say("About to fight against incompatible structures...")
    incompatible_bridges = tell(proposed_bridge, "get-incompatible-bridges")
    if setup.p_verbose is not False and len(incompatible_bridges) != 0:
        say("Fighting against incompatible bridges:")
        print_(incompatible_bridges)
        say("Weight is ", tell(proposed_bridge, "get-letter-span"))
        say("Opposing weights are ", tell_all(incompatible_bridges, "get-letter-span"))
    # the weights are read before the fights, which draw
    if (len(incompatible_bridges) != 0
            and workspace_structures.wins_all_fights_p(
                proposed_bridge,
                tell(proposed_bridge, "get-letter-span"),
                incompatible_bridges,
                tell_all(incompatible_bridges, "get-letter-span")) is False):
        say("Lost to incompatible bridge. Fizzling.")
        if setup.p_workspace_graphics is not False:
            _metacat.bridge_graphics.bridge_graphics("erase", proposed_bridge)
        sugar.fizzle()
    if ((tell(object1, "leftmost-in-string?") is not False
         or tell(object1, "rightmost-in-string?") is not False)
            and (tell(object2, "leftmost-in-string?") is not False
                 or tell(object2, "rightmost-in-string?") is not False)):
        incompatible_bond = tell(proposed_bridge, "get-incompatible-bond")
    else:
        incompatible_bond = False
    if setup.p_verbose is not False and exists_p(incompatible_bond):
        say("Fighting against incompatible bond:")
        print_(incompatible_bond)
    if (exists_p(incompatible_bond)
            and workspace_structures.wins_fight_p(proposed_bridge, 3,
                                                  incompatible_bond, 2) is False):
        say("Lost to incompatible bond. Fizzling.")
        if setup.p_workspace_graphics is not False:
            _metacat.bridge_graphics.bridge_graphics("erase", proposed_bridge)
        sugar.fizzle()
    if exists_p(incompatible_bond):
        incompatible_group = tell(incompatible_bond, "get-enclosing-group")
    else:
        incompatible_group = False
    if setup.p_verbose is not False and exists_p(incompatible_group):
        say("Fighting against incompatible group:")
        print_(incompatible_group)
    if (exists_p(incompatible_group)
            and workspace_structures.wins_fight_p(proposed_bridge, 1,
                                                  incompatible_group, 1) is False):
        say("Lost to incompatible group. Fizzling.")
        if setup.p_workspace_graphics is not False:
            _metacat.bridge_graphics.bridge_graphics("erase", proposed_bridge)
        sugar.fizzle()
    if (setup.p_verbose is not False
            and tell(proposed_bridge, "flipped-group1?") is not False):
        say("Fighting against existing group1:")
        print_(tell(proposed_bridge, "get-original-group1"))
    if (tell(proposed_bridge, "flipped-group1?") is not False
            and workspace_structures.wins_fight_p(
                proposed_bridge, 1,
                tell(proposed_bridge, "get-original-group1"), 1) is False):
        say("Lost to existing group1. Fizzling.")
        if setup.p_workspace_graphics is not False:
            _metacat.bridge_graphics.bridge_graphics("erase", proposed_bridge)
        sugar.fizzle()
    if (setup.p_verbose is not False
            and tell(proposed_bridge, "flipped-group2?") is not False):
        say("Fighting against existing group2:")
        print_(tell(proposed_bridge, "get-original-group2"))
    if (tell(proposed_bridge, "flipped-group2?") is not False
            and workspace_structures.wins_fight_p(
                proposed_bridge, 1,
                tell(proposed_bridge, "get-original-group2"), 1) is False):
        say("Lost to existing group2. Fizzling.")
        if setup.p_workspace_graphics is not False:
            _metacat.bridge_graphics.bridge_graphics("erase", proposed_bridge)
        sugar.fizzle()
    say("Won against all incompatible structures.")
    if len(incompatible_bridges) != 0:
        for bridge in incompatible_bridges:
            break_bridge(bridge)
    if exists_p(incompatible_bond):
        bonds.break_bond(incompatible_bond)
    if exists_p(incompatible_group):
        groups.break_group(incompatible_group)
    if tell(proposed_bridge, "flipped-group1?") is not False:
        say("*** flipping group1 ***")
        original_group = tell(proposed_bridge, "get-original-group1")
        groups.break_group(original_group)
        for bond in tell(original_group, "get-constituent-bonds"):
            bonds.break_bond(bond)
        for bond in tell(object1, "get-constituent-bonds"):
            bonds.build_bond(bond)
        groups.build_group(object1, True)
        if setup.p_workspace_graphics is not False:
            _metacat.group_graphics.group_graphics("set-pexp-and-draw", object1)
            tell(proposed_bridge, "set-new-from-graphics-coord", object1)
    if tell(proposed_bridge, "flipped-group2?") is not False:
        say("*** flipping group2 ***")
        original_group = tell(proposed_bridge, "get-original-group2")
        groups.break_group(original_group)
        for bond in tell(original_group, "get-constituent-bonds"):
            bonds.break_bond(bond)
        for bond in tell(object2, "get-constituent-bonds"):
            bonds.build_bond(bond)
        groups.build_group(object2, True)
        if setup.p_workspace_graphics is not False:
            _metacat.group_graphics.group_graphics("set-pexp-and-draw", object2)
            tell(proposed_bridge, "set-new-to-graphics-coord", object2)
    build_bridge(bridge_orientation, proposed_bridge)
    # This must happen _after_ the bridge is built, since building the
    # bridge may cause bond-concept-mappings to be added to it (and
    # possibly an ObjCtgy concept-mapping, for horizontal bridges):
    return tell(proposed_bridge, "boost-themespace-activations")


def build_bridge(bridge_orientation, bridge):
    """bridges.ss: build-bridge"""
    say("Building bridge:")
    if setup.p_verbose is not False:
        print_(bridge)
    # a two-binding let; both only read
    object1 = tell(bridge, "get-object1")
    object2 = tell(bridge, "get-object2")
    tell(object1, "update-bridge", bridge_orientation, bridge)
    tell(object2, "update-bridge", bridge_orientation, bridge)
    tell(workspace.g_workspace, "add-bridge", bridge)

    if bridge_orientation == "horizontal":
        # Need to ensure that an ObjCtgy CM/slippage exists for every horizontal bridge,
        # whether or not it's relevant, so as to avoid problems with rule abstraction.
        # Example: ab --> aabb, a-->aa bridge has ObjCtgy:letter=>group slippage
        # but b-->bb bridge does not.  This leads to the following rule:
        #     Increase length of each object in string by one
        #     Change leftmost letter to a group
        if ormap_meth(tell(bridge, "get-concept-mappings"),
                      "CM-type?", slipnet.plato_object_category) is False:
            ObjCtgy_CM = concept_mappings.make_concept_mapping(
                object1, slipnet.plato_object_category,
                tell(object1, "get-descriptor-for", slipnet.plato_object_category),
                object2, slipnet.plato_object_category,
                tell(object2, "get-descriptor-for", slipnet.plato_object_category))
            tell(bridge, "add-concept-mapping", ObjCtgy_CM)
            if tell(ObjCtgy_CM, "slippage?") is not False:
                tell(bridge, "add-symmetric-slippage", ObjCtgy_CM)

    for slippage in filter_meth(tell(bridge, "get-concept-mappings"), "slippage?"):
        tell(bridge, "add-symmetric-slippage", slippage)
    if group_p(object1) and group_p(object2):
        for bond_cm in all_possible_bridge_CMs(
                bridge_orientation,
                object1, tell(object1, "get-bond-descriptions"),
                object2, tell(object2, "get-bond-descriptions")):
            tell(bridge, "add-bond-concept-mapping", bond_cm)
            if tell(bond_cm, "slippage?") is not False:
                tell(bridge, "add-symmetric-slippage", bond_cm)

    if bridge_orientation == "horizontal":
        # Attach length description(s) if lengths are different, and add a Length
        # slippage to bridge's concept-mappings, but don't active Length itself.
        # Special case: if bridge is from a letter to a singleton group, attach
        # a Length description (if one doesn't already exist) to the singleton group
        # and add a Length:one=>one concept-mapping.
        # Another special case:  if bridge is between a letter and a group,
        # try to propose a singleton group based on the letter, with the same
        # group category and direction as the corresponding group.
        # (a two-binding let; both only read)
        length1 = tell(object1, "get-platonic-length")
        length2 = tell(object2, "get-platonic-length")
        if (length1 is not length2
                and tell(bridge, "CM-type-present?", slipnet.plato_length) is False):
            tell(bridge, "add-concept-mapping",
                 concept_mappings.make_concept_mapping(
                     object1, slipnet.plato_length, length1,
                     object2, slipnet.plato_length, length2))

    for cm in tell(bridge, "get-concept-mappings"):
        tell(cm, "activate-label")
    tell(bridge, "update-proposal-level", workspace.p_built)
    _metacat.trace.monitor_new_concept_mappings(tell(bridge, "get-all-concept-mappings"), bridge)
    if setup.p_workspace_graphics is not False:
        tell(bridge, "activate-concept-mapping-graphics")
        return _metacat.bridge_graphics.bridge_graphics("update-level", bridge)
    return None


def propose_singleton_group(letter, group_category, direction):
    """bridges.ss: propose-singleton-group"""
    # 1.2: never called (anomalies: "Dead code in bridges.ss")
    return groups.propose_group([letter], [], group_category, direction)


def try_to_propose_singleton_group(letter, group_category, direction):
    """bridges.ss: try-to-propose-singleton-group"""
    # 1.2: never called (anomalies: "Dead code in bridges.ss")
    singleton_group = groups.make_group(tell(letter, "get-string"), group_category,
                                        slipnet.plato_letter_category, direction,
                                        letter, letter, [letter], [])
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < workspace_structure_formulas.single_letter_group_probability(singleton_group):
        say("Local support strong enough. Proposing single-letter group.")
        return groups.propose_group([letter], [], group_category, direction)
    return None


def break_bridge(bridge):
    """bridges.ss: break-bridge"""
    say("Breaking bridge:")
    if setup.p_verbose is not False:
        print_(bridge)
    # a three-binding let; every binding only reads
    bridge_orientation = tell(bridge, "get-orientation")
    object1 = tell(bridge, "get-object1")
    object2 = tell(bridge, "get-object2")
    tell(object1, "update-bridge", bridge_orientation, False)
    tell(object2, "update-bridge", bridge_orientation, False)
    tell(workspace.g_workspace, "delete-bridge", bridge)
    if setup.p_workspace_graphics is not False:
        _metacat.bridge_graphics.bridge_graphics("erase", bridge)
    return "done"


def bridge_type_to_orientation(bridge_type):
    """bridges.ss: bridge-type->orientation"""
    if bridge_type in ("top", "bottom"):
        return "horizontal"
    if bridge_type == "vertical":
        return "vertical"
    return None     # (case ...) without else: void


def enclosing_bridge_p(b1, b2):
    """bridges.ss: enclosing-bridge?"""
    if tell(tell(b1, "get-object1"), "nested-member?", tell(b2, "get-object1")) is False:
        return False
    return tell(tell(b1, "get-object2"), "nested-member?", tell(b2, "get-object2"))


def bridges_equal_p(bridge1, bridge2):
    """bridges.ss: bridges-equal?"""
    return (tell(bridge1, "get-object1") is tell(bridge2, "get-object1")
            and tell(bridge1, "get-object2") is tell(bridge2, "get-object2"))


def bridge_between_p(bridge_orientation, object1, object2):
    """bridges.ss: bridge-between?"""
    bridge = tell(object1, "get-bridge", bridge_orientation)
    return exists_p(bridge) and tell(bridge, "get-object2") is object2


def get_bridge_between(bridge_orientation, object1, object2):
    """bridges.ss: get-bridge-between"""
    bridge = tell(object1, "get-bridge", bridge_orientation)
    if exists_p(bridge) and tell(bridge, "get-object2") is object2:
        return bridge
    return False


def letter_category_or_length_slippage_p(cm):
    """bridges.ss: letter-category/length-slippage?"""
    if tell(cm, "CM-type?", slipnet.plato_letter_category) is not False:
        slippage_p = tell(cm, "slippage?")
        if slippage_p is not False:
            return slippage_p
    if tell(cm, "CM-type?", slipnet.plato_length) is not False:
        return tell(cm, "slippage?")
    return False


def all_possible_bridge_CMs(bridge_orientation, object1, object1_descriptions,
                            object2, object2_descriptions):
    """bridges.ss: all-possible-bridge-CMs"""
    if bridge_orientation == "horizontal":
        mappable_p = horizontal_mappable_descriptions_p(object1, object2)
    elif bridge_orientation == "vertical":
        mappable_p = vertical_mappable_descriptions_p(object1, object2)
    else:
        mappable_p = None   # (case ...) without else: void

    def make(description1, description2):
        # the arguments only read
        return concept_mappings.make_concept_mapping(
            object1,
            tell(description1, "get-description-type"),
            tell(description1, "get-descriptor"),
            object2,
            tell(description2, "get-description-type"),
            tell(description2, "get-descriptor"))
    # cross-product-filter-map: utilities.ss's order (make-concept-mapping activates)
    return cross_product_filter_map(mappable_p, make, object1_descriptions, object2_descriptions)


# object1 and object2 will never be Bfacet:length groups because such
# groups do not have LettCtgy descriptions:

def letter_category_mappable_objects_p(object1, object2):
    """bridges.ss: letter-category-mappable-objects?"""
    return ((letter_p(object1) and letter_p(object2))
            or (letter_p(object1) and group_p(object2)
                and tell(object2, "all-letter-group?") is not False)
            or (group_p(object1) and tell(object1, "all-letter-group?") is not False
                and letter_p(object2))
            or (group_p(object1) and group_p(object2)
                # 1.2: object1's group category twice (anomalies:
                # "letter-category-mappable-objects? compares a group with itself")
                and slipnet.related_p(tell(object1, "get-group-category"),
                                      tell(object1, "get-group-category"))))


# --------------------- Code specific to horizontal bridges ------------------------

def horizontal_mappable_descriptions_p(object1, object2):
    """bridges.ss: horizontal-mappable-descriptions?"""
    def mappable_p(description1, description2):
        """bridges.ss: horizontal-mappable-descriptions? (the predicate it makes)"""
        # a two-binding let; both only read
        description_type1 = tell(description1, "get-description-type")
        description_type2 = tell(description2, "get-description-type")
        if description_type1 is not description_type2:
            return False
        if description_type1 is slipnet.plato_letter_category:
            return letter_category_mappable_objects_p(object1, object2)
        descriptor1 = tell(description1, "get-descriptor")
        descriptor2 = tell(description2, "get-descriptor")
        return (description_type1 is slipnet.plato_string_position_category
                or description_type1 is slipnet.plato_length
                or descriptor1 is descriptor2
                or slipnet.slip_linked_p(descriptor1, descriptor2))
    return mappable_p


def supporting_horizontal_bridges_p(b1, b2):
    """bridges.ss: supporting-horizontal-bridges?"""
    if incompatible_horizontal_bridges_p(b1, b2) is not False:
        return False
    return cross_product_ormap(supporting_horizontal_CMs_p,
                               tell(b1, "get-distinguishing-CMs"),
                               tell(b2, "get-distinguishing-CMs"))


def incompatible_horizontal_bridges_p(b1, b2):
    """bridges.ss: incompatible-horizontal-bridges?"""
    if tell(b1, "get-object1") is tell(b2, "get-object1"):
        return True
    if tell(b1, "get-object2") is tell(b2, "get-object2"):
        return True
    # This is to avoid certain CM "incompatibilities" which are
    # not really incompatible. For example, in abc -> abcc we
    # don't want the spanning bridge to be incompatible with
    # c-->cc based on the Length CMs 3=>3 and 1=>2. In aabc ->
    # aabcc, we don't want aa-->aa (Length:2=>2) to be incompatible
    # with c-->cc (Length:1=>2).  Don't need to do this for vertical
    # bridges, since letter-category or length *slippages* cannot
    # underlie vertical bridges. Also, as for vertical bridges,
    # DirCtgy concept-mappings should only be considered if the
    # bridges are nested. Example: [[xy][fg]] --> [[fg][xy]]
    # (see incompatible-vertical-bridges? for more info):
    # (a two-binding let and a two-argument call; everything only reads)
    b1_concept_mappings = filter_out(letter_category_or_length_slippage_p,
                                     tell(b1, "get-concept-mappings"))
    b2_concept_mappings = filter_out(letter_category_or_length_slippage_p,
                                     tell(b2, "get-concept-mappings"))
    if enclosing_bridge_p(b1, b2) is not False:
        l1 = b1_concept_mappings
    else:
        l1 = filter_out_meth(b1_concept_mappings, "CM-type?", slipnet.plato_direction_category)
    if enclosing_bridge_p(b2, b1) is not False:
        l2 = b2_concept_mappings
    else:
        l2 = filter_out_meth(b2_concept_mappings, "CM-type?", slipnet.plato_direction_category)
    return incompatible_horizontal_CM_lists_p(l1, l2)


def incompatible_horizontal_CM_lists_p(l1, l2):
    """bridges.ss: incompatible-horizontal-CM-lists?"""
    return cross_product_ormap(incompatible_horizontal_CMs_p, l1, l2)


def incompatible_with_any_horizontal_CM_p(cm, l):
    """bridges.ss: incompatible-with-any-horizontal-CM?"""
    return cross_product_ormap(incompatible_horizontal_CMs_p, [cm], l)


def supporting_horizontal_CMs_p(cm1, cm2):
    """bridges.ss: supporting-horizontal-CMs?"""
    if concept_mappings.CMs_equal_p(cm1, cm2) is not False:
        return True
    return ((slipnet.related_p(tell(cm1, "get-descriptor1"), tell(cm2, "get-descriptor1"))
             or slipnet.related_p(tell(cm1, "get-descriptor2"), tell(cm2, "get-descriptor2")))
            and exists_p(tell(cm1, "get-label"))
            and exists_p(tell(cm2, "get-label"))
            and tell(cm1, "get-label") is tell(cm2, "get-label"))


def incompatible_horizontal_CMs_p(cm1, cm2):
    """bridges.ss: incompatible-horizontal-CMs?"""
    # a four-binding let; every binding only reads
    cm1_desc1 = tell(cm1, "get-descriptor1")
    cm1_desc2 = tell(cm1, "get-descriptor2")
    cm2_desc1 = tell(cm2, "get-descriptor1")
    cm2_desc2 = tell(cm2, "get-descriptor2")
    return ((slipnet.related_p(cm1_desc1, cm2_desc1)
             or slipnet.related_p(cm1_desc2, cm2_desc2))
            and exists_p(tell(cm1, "get-label"))
            and exists_p(tell(cm2, "get-label"))
            and tell(cm1, "get-label") is not tell(cm2, "get-label")
            and (slipnet.get_label(cm1_desc1, cm2_desc1)
                 is not slipnet.get_label(cm1_desc2, cm2_desc2)))


# -------------------- Code specific to vertical bridges ------------------------

def vertical_mappable_descriptions_p(object1, object2):
    """bridges.ss: vertical-mappable-descriptions?"""
    def mappable_p(description1, description2):
        """bridges.ss: vertical-mappable-descriptions? (the predicate it makes)"""
        # a two-binding let; both only read
        description_type1 = tell(description1, "get-description-type")
        description_type2 = tell(description2, "get-description-type")
        if description_type1 is not description_type2:
            return False
        descriptor1 = tell(description1, "get-descriptor")
        descriptor2 = tell(description2, "get-descriptor")
        return descriptor1 is descriptor2 or slipnet.slip_linked_p(descriptor1, descriptor2)
    return mappable_p


def supporting_vertical_bridges_p(b1, b2):
    """bridges.ss: supporting-vertical-bridges?"""
    if incompatible_vertical_bridges_p(b1, b2) is not False:
        return False
    return cross_product_ormap(supporting_vertical_CMs_p,
                               tell(b1, "get-distinguishing-CMs"),
                               tell(b2, "get-distinguishing-CMs"))


def incompatible_vertical_bridges_p(b1, b2):
    """bridges.ss: incompatible-vertical-bridges?"""
    if tell(b1, "get-object1") is tell(b2, "get-object1"):
        return True
    if tell(b1, "get-object2") is tell(b2, "get-object2"):
        return True
    # Direction-category-CM of a bridge should only be considered
    # if the bridge encloses the other bridge.  This prevents
    # unnecessary incompatibilities between directed-group
    # bridges.  Example:        [[xy][fg]] -> ...
    #                               \ /
    #                                X
    #                               / \
    #                           [[fg][xy]] -> ?
    # "DirCtgy:right=>right" CMs would be incompatible with the
    # "StrPosCtgy:lmost=(opp)=>rmost" and "StrPosCtgy:rmost=(opp)=>lmost"
    # CMs.  However, if the whole-string groups are directed the same way,
    # the spanning-bridge DirCtgy CM should be included to make
    # the StrPosCtgy mappings incompatible:
    # (a two-binding let and a two-argument call; everything only reads)
    b1_concept_mappings = tell(b1, "get-concept-mappings")
    b2_concept_mappings = tell(b2, "get-concept-mappings")
    if enclosing_bridge_p(b1, b2) is not False:
        l1 = b1_concept_mappings
    else:
        l1 = filter_out_meth(b1_concept_mappings, "CM-type?", slipnet.plato_direction_category)
    if enclosing_bridge_p(b2, b1) is not False:
        l2 = b2_concept_mappings
    else:
        l2 = filter_out_meth(b2_concept_mappings, "CM-type?", slipnet.plato_direction_category)
    return incompatible_vertical_CM_lists_p(l1, l2)


def incompatible_vertical_CM_lists_p(l1, l2):
    """bridges.ss: incompatible-vertical-CM-lists?"""
    return cross_product_ormap(incompatible_vertical_CMs_p, l1, l2)


def incompatible_with_any_vertical_CM_p(cm, l):
    """bridges.ss: incompatible-with-any-vertical-CM?"""
    return cross_product_ormap(incompatible_vertical_CMs_p, [cm], l)


def supporting_vertical_CMs_p(cm1, cm2):
    """bridges.ss: supporting-vertical-CMs?"""
    if concept_mappings.CMs_equal_p(cm1, cm2) is not False:
        return True
    return ((slipnet.related_p(tell(cm1, "get-descriptor1"), tell(cm2, "get-descriptor1"))
             or slipnet.related_p(tell(cm1, "get-descriptor2"), tell(cm2, "get-descriptor2")))
            and exists_p(tell(cm1, "get-label"))
            and exists_p(tell(cm2, "get-label"))
            and tell(cm1, "get-label") is tell(cm2, "get-label"))


def incompatible_vertical_CMs_p(cm1, cm2):
    """bridges.ss: incompatible-vertical-CMs?"""
    # a four-binding let; every binding only reads
    cm1_desc1 = tell(cm1, "get-descriptor1")
    cm1_desc2 = tell(cm1, "get-descriptor2")
    cm2_desc1 = tell(cm2, "get-descriptor1")
    cm2_desc2 = tell(cm2, "get-descriptor2")
    return ((slipnet.related_p(cm1_desc1, cm2_desc1)
             or slipnet.related_p(cm1_desc2, cm2_desc2))
            and exists_p(tell(cm1, "get-label"))
            and exists_p(tell(cm2, "get-label"))
            and tell(cm1, "get-label") is not tell(cm2, "get-label")
            and (slipnet.get_label(cm1_desc1, cm2_desc1)
                 is not slipnet.get_label(cm1_desc2, cm2_desc2)))


def load():
    """bridges.ss: the four define-codelet-procedure* forms, in the file's order."""
    sugar.define_codelet_procedure_star("bottom-up-bridge-scout", bottom_up_bridge_scout)
    sugar.define_codelet_procedure_star("important-object-bridge-scout",
                                        important_object_bridge_scout)
    sugar.define_codelet_procedure_star("bridge-evaluator", bridge_evaluator)
    sugar.define_codelet_procedure_star("bridge-builder", bridge_builder)
