"""themes.ss: the Themespace, theme clusters and bridge themes, the
thematic-bridge-scout codelet, and the theme tests bridges use.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from themes.ss, with
racket/engine/themes.rktl as a worked translation (verbatim there: it has no
port: changes).

The closures of make-themespace, make-theme-cluster, make-generic-theme and
make-bridge-theme are the Themespace, ThemeCluster, GenericTheme and
BridgeTheme classes (docs/python-translation-plan.md, "Objects").  The first
three delegate what they don't answer to base-object; a BridgeTheme delegates
to its own GenericTheme (so the generic theme's (tell self ...) and its
dominant? test see the bridge theme).  A closure variable whose name is also a
message name (frozen?) keeps the mapped name as an attribute, and the message's
method gets a trailing underscore (frozen_p_), as bridges.py does.  A theme
cluster's propagation, self-excitation, net-effect and activation functions
are Python closures made in its __init__, in the let*'s order; net-effect's
alpha is computed once there from the initial sensitivity 1.0, so set-sensitivity
changes the variable and not alpha, as in the original.  The
thematic-bridge-scout codelet procedure is given to its codelet type by load(),
which also makes *themespace* and the REPL abbreviations that name slipnodes
(lcat ... opp); top, bot, ver and diff (#f, the "different" relation) need no
other module and are plain module constants.

Modules loaded before this one are imported directly: slipnet (*slipnet-nodes*,
get-label, the plato nodes), workspace (*workspace*, %proposed%), workspace_objects
(lone-spanning-object?, both-spanning-groups?, both-spanning-objects?), bridges
(propose-bridge, bridge-type->orientation), descriptions (make-description,
build-description: called qualified, since a run's trace wraps build-description),
concept_mappings (make-concept-mapping), coderack (the bridge-evaluator and
description-evaluator codelet types, %very-high-urgency%,
%extremely-high-urgency%) and setup (*themespace-window*, %justify-mode%,
%self-watching-enabled%).  Names from files translated later are read through
the package at call time: run.ss's *display-mode?* (_metacat.run.g_display_mode_p,
only read when self-watching is disabled) and trace.ss's entries
(get-nonzero-theme-pattern).  The *themespace-window* messages are sent
ungated, exactly as the original sends them, so a headless null window receives
them.  The engine never imports tkinter.

Arithmetic: activations stay exact integers (clip of round); the propagation and
self-excitation values are exact rationals (weights are percentages), and the net
input buffer sums them exactly.  The one flonum is net-effect's alpha,
(* 1.0 1/50 (/ 1 num-relations)), so (* alpha net-input) is a flonum, except that
an exact 0 net input gives Chez's exact 0 (chez.mul), and tanh of exact 0 is exact
0 (chez.tanh); utilities.round_ makes the result exact again.  Every product,
quotient, max and min goes through chez's helpers.

Evaluation order (audited against Chez's, as racket's porting-notes item 10 did):
the draws are spread-activation-to-slipnet's two stochastic-if*s (each draws its
coin before evaluating its probability), and in thematic-bridge-scout: the
stochastic-pick of the theme type, the cluster filter's prob? (utilities.ss's
filter, first to last), (tell-all clusters 'pick-positive-theme) (one
stochastic-pick-by-method per cluster, in chez.map_'s order), the
stochastic-pick-by-method of the chosen object,
propose-description-based-on-theme's stochastic-pick-by-method (once per
non-applicable theme, in for*'s order), conditions-for-bridge's prob? (once per
candidate, inside map-compress, which runs first to last element by element),
the stochastic-select of the other object, and look-for-auxiliary-slippages'
prob? (one per possible auxiliary slippage).  Each sits in a let*, a body, a
utilities.ss filter, map-compress or a tell-all, so the Python sequence is
Chez's.  conditions-for-bridge's (themes-support? (flipped object1) (flipped
object2)) has two arguments that each make a flipped group; making one neither
draws nor changes shared state, so the order is not observable, and it is kept
left to right as the Racket port has it.  make-themespace's three maps of
make-theme-cluster make clusters only (no draw, no shared state); they use
chez.map_ anyway.  Every other multi-argument call and multi-binding let (the
theme-type weights' (* ... (100- ...)), get-complete-state's list,
get-partial-state's and look-for-auxiliary-slippages' lets, the post-codelet*
arguments, make-concept-mapping's and make-description's arguments) only reads.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez, sugar
from metacat import (bridges, coderack, concept_mappings, descriptions, setup, slipnet,
                     workspace, workspace_objects)
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.sugar import say, vprint, vprintf
from metacat.utilities import (andmap_meth, base_object, clip_function, count_meth, cube,
                               cross_product_map, cross_product_ormap, exists_p, filter_,
                               filter_meth, filter_out, filter_out_meth, first, flatmap,
                               fifth, fourth, hundred_minus, intersect, map_compress,
                               maximum, member_p, percent, print_, prob_p, remq_duplicates,
                               remq_elements, round_, second, select_meth, sgn,
                               sort_by_method, square, stochastic_pick,
                               stochastic_pick_by_method, stochastic_select, tell_all, third)

String = chez.String

p_max_theme_activation = 100
p_dominant_theme_margin = 90
p_theme_spread_amount = 20
p_theme_boost_amount = 7
p_theme_decay_amount = 25

# Intra-cluster theme weights (< 0 = inhibitory, > 0 = excitatory):
negative_to_negative_weight = 0
negative_to_positive_weight = +25
positive_to_negative_weight = -75
positive_to_positive_weight = -2
self_to_self_weight = +10


clip_positive = clip_function(0, p_max_theme_activation)

clip_negative = clip_function(chez.sub(p_max_theme_activation), 0)


def _null_p(l):
    return len(l) == 0


def _theme_types_literal():
    """'(top-bridge bottom-bridge vertical-bridge)"""
    return ["top-bridge", "bottom-bridge", "vertical-bridge"]


class Themespace(SchemeObject):
    """themes.ss: make-themespace (the closure)"""
    __slots__ = ("dimensions", "top_clusters", "bottom_clusters", "vertical_clusters",
                 "all_clusters", "all_themes", "active_theme_types", "stored_current_state")

    def __init__(this):
        # the let*, in order
        this.dimensions = filter_meth(slipnet.g_slipnet_nodes, "category?")
        # chez: map's order of application (making clusters draws nothing)
        this.top_clusters = chez.map_(make_theme_cluster("top-bridge"), this.dimensions)
        this.bottom_clusters = chez.map_(make_theme_cluster("bottom-bridge"), this.dimensions)
        this.vertical_clusters = chez.map_(make_theme_cluster("vertical-bridge"),
                                           this.dimensions)
        this.all_clusters = this.top_clusters + this.bottom_clusters + this.vertical_clusters
        this.all_themes = []
        # active-theme-types are those currently exerting thematic pressure:
        this.active_theme_types = []
        this.stored_current_state = False

    @message("object-type")
    def object_type(this, self):
        return "themespace"

    @message("print")
    def print_(this, self):
        sugar.printf("TOP BRIDGE THEMES:~%")
        print_(this.top_clusters)
        sugar.printf("BOTTOM BRIDGE THEMES:~%")
        print_(this.bottom_clusters)
        sugar.printf("VERTICAL BRIDGE THEMES:~%")
        return print_(this.vertical_clusters)

    @message("current-state-displayed?")
    def current_state_displayed_p(this, self):
        return not exists_p(this.stored_current_state)

    @message("save-current-state")
    def save_current_state(this, self):
        this.stored_current_state = tell(self, "get-complete-state")
        return "done"

    @message("restore-current-state")
    def restore_current_state(this, self):
        if exists_p(this.stored_current_state):
            tell(self, "restore-state", this.stored_current_state)
            this.stored_current_state = False
        return "done"

    @message("get-complete-state")
    def get_complete_state(this, self):
        # the list's elements only read
        return [
            _theme_types_literal(),
            this.active_theme_types,
            # chez: map's order of application (the procedure only reads)
            chez.map_(lambda theme: [tell(theme, "get-theme-type"),
                                     tell(theme, "get-dimension"),
                                     tell(theme, "get-relation"),
                                     tell(theme, "get-activation"),
                                     tell(theme, "individually-frozen?")],
                      this.all_themes),
            chez.map_(lambda cluster: [tell(cluster, "get-theme-type"),
                                       tell(cluster, "get-dimension"),
                                       tell(cluster, "frozen?")],
                      this.all_clusters)]

    @message("get-partial-state")
    def get_partial_state(this, self, theme_type):
        # a two-binding let; neither binding has effects
        complete_state = tell(self, "get-complete-state")

        def relevant_info_p(info):
            return first(info) == theme_type
        return [[theme_type],
                intersect([theme_type], second(complete_state)),
                filter_(relevant_info_p, third(complete_state)),
                filter_(relevant_info_p, fourth(complete_state))]

    @message("restore-state")
    def restore_state(this, self, state):
        all_theme_types = first(state)
        active_theme_types = second(state)
        theme_info = third(state)
        cluster_info = fourth(state)
        for theme_type in all_theme_types:
            tell(self, "delete-theme-type", theme_type)
            tell(self, "unfreeze-theme-type", theme_type)
            tell(self, "thematic-pressure-off", theme_type)
        for theme_type in active_theme_types:
            tell(self, "thematic-pressure-on", theme_type)
        for entry in theme_info:
            type_ = first(entry)
            dim = second(entry)
            rel = third(entry)
            act = fourth(entry)
            frozen_p = fifth(entry)
            tell(self, "set-theme-activation", type_, dim, rel, act)
            if frozen_p is not False:
                tell(self, "freeze-theme", type_, dim, rel)
        for entry in cluster_info:
            type_ = first(entry)
            dim = second(entry)
            frozen_p = third(entry)
            if frozen_p is not False:
                tell(self, "freeze-theme-cluster", type_, dim)
        return "done"

    @message("initialize")
    def initialize(this, self):
        this.stored_current_state = False
        tell(self, "delete-everything")
        tell(self, "unfreeze-everything")
        return tell(self, "thematic-pressure-off")

    @message("get-possible-theme-types")
    def get_possible_theme_types(this, self):
        if setup.p_justify_mode is not False:
            return ["top-bridge", "bottom-bridge", "vertical-bridge"]
        return ["top-bridge", "vertical-bridge"]

    @message("get-active-theme-types")
    def get_active_theme_types(this, self):
        return this.active_theme_types

    @message("get-active-bridge-theme-types")
    def get_active_bridge_theme_types(this, self):
        return intersect(_theme_types_literal(), tell(self, "get-active-theme-types"))

    @message("thematic-pressure?")
    def thematic_pressure_p(this, self, *types):
        if _null_p(types):
            return not _null_p(this.active_theme_types)
        return chez.andmap(lambda type_: member_p(type_, this.active_theme_types), list(types))

    @message("thematic-pressure-on")
    def thematic_pressure_on(this, self, *types):
        if _null_p(types):
            this.active_theme_types = tell(self, "get-possible-theme-types")
        else:
            for type_ in types:
                tell(self, "set-thematic-pressure", type_, True)
        # 1.2: not gated by a graphics switch; the headless null window absorbs it
        tell(setup.g_themespace_window, "update-thematic-pressure")
        return "done"

    @message("thematic-pressure-off")
    def thematic_pressure_off(this, self, *types):
        if _null_p(types):
            this.active_theme_types = []
        else:
            for type_ in types:
                tell(self, "set-thematic-pressure", type_, False)
        tell(setup.g_themespace_window, "update-thematic-pressure")
        return "done"

    @message("set-thematic-pressure")
    def set_thematic_pressure(this, self, type_, switch):
        if not (switch is tell(self, "thematic-pressure?", type_)):   # eq? on booleans
            if switch is not False:
                this.active_theme_types = [type_] + this.active_theme_types
            else:
                this.active_theme_types = chez.remq(type_, this.active_theme_types)
        return "done"

    @message("supported-by-active-theme?")
    def supported_by_active_theme_p(this, self, concept_mapping, bridge):
        theme_type = tell(bridge, "get-theme-type")
        theme = tell(self, "get-theme", theme_type,
                     tell(concept_mapping, "get-CM-type"),
                     tell(concept_mapping, "get-label"))
        if not exists_p(theme):
            return False
        if tell(self, "thematic-pressure?", theme_type) is False:
            return False
        return tell(theme, "dominant?")

    @message("get-all-themes")
    def get_all_themes(this, self):
        return this.all_themes

    # theme-type/s can be either a symbol or a list:
    @message("get-themes")
    def get_themes(this, self, theme_type_or_s):
        return filter_meth(this.all_themes, "theme-type?", theme_type_or_s)

    @message("get-all-active-themes")
    def get_all_active_themes(this, self):
        return tell(self, "get-themes", this.active_theme_types)

    @message("get-active-themes")
    def get_active_themes(this, self, theme_type_or_s):
        if isinstance(theme_type_or_s, list) and not _null_p(theme_type_or_s):   # pair?
            return tell(self, "get-themes", intersect(theme_type_or_s, this.active_theme_types))
        if member_p(theme_type_or_s, this.active_theme_types):
            return tell(self, "get-themes", theme_type_or_s)
        return []

    @message("get-theme")
    def get_theme(this, self, theme_type, dimension, relation):
        return tell(tell(self, "get-cluster", theme_type, dimension), "get-theme", relation)

    @message("get-clusters")
    def get_clusters(this, self, theme_type):
        if theme_type == "top-bridge":
            return this.top_clusters
        if theme_type == "bottom-bridge":
            return this.bottom_clusters
        if theme_type == "vertical-bridge":
            return this.vertical_clusters
        return None     # (case ...) without else: void

    @message("get-cluster")
    def get_cluster(this, self, theme_type, dimension):
        return select_meth(tell(self, "get-clusters", theme_type), "dimension?", dimension)

    @message("get-dimensions")
    def get_dimensions(this, self):
        return this.dimensions

    @message("get-relations")
    def get_relations(this, self, theme_type, dimension):
        return tell(tell(self, "get-cluster", theme_type, dimension), "get-relations")

    @message("get-complete-theme-pattern")
    def get_complete_theme_pattern(this, self, theme_type):
        # flatmap and map: chez.map_'s order (the procedures only read)
        return [theme_type] + flatmap(
            lambda cluster: chez.map_(lambda theme: [tell(theme, "get-dimension"),
                                                     tell(theme, "get-relation"),
                                                     tell(theme, "get-activation")],
                                      tell(cluster, "get-themes")),
            tell(self, "get-clusters", theme_type))

    @message("get-dominant-theme-pattern")
    def get_dominant_theme_pattern(this, self, theme_type):
        def dominant(cluster):
            dominant_theme = tell(cluster, "get-dominant-theme")
            if exists_p(dominant_theme):
                return [tell(dominant_theme, "get-dimension"),
                        tell(dominant_theme, "get-relation")]
            return False
        return [theme_type] + map_compress(dominant, tell(self, "get-clusters", theme_type))

    # ignores all themes with activation 0
    @message("get-nonzero-theme-pattern")
    def get_nonzero_theme_pattern(this, self, theme_type):
        return [theme_type] + filter_out(
            lambda entry: len(entry) == 3 and third(entry) == 0,
            _metacat.trace.entries(tell(self, "get-complete-theme-pattern", theme_type)))

    @message("get-all-complete-theme-patterns")
    def get_all_complete_theme_patterns(this, self):
        # chez: map's order of application (the procedure only reads)
        return chez.map_(lambda theme_type: tell(self, "get-complete-theme-pattern", theme_type),
                         tell(self, "get-possible-theme-types"))

    @message("get-all-dominant-theme-patterns")
    def get_all_dominant_theme_patterns(this, self):
        # chez: map's order of application (the procedure only reads)
        return chez.map_(lambda theme_type: tell(self, "get-dominant-theme-pattern", theme_type),
                         tell(self, "get-possible-theme-types"))

    @message("get-percentage-of-dominant-themes")
    def get_percentage_of_dominant_themes(this, self):
        if setup.p_justify_mode is not False:
            clusters = this.all_clusters
        else:
            clusters = this.top_clusters + this.vertical_clusters
        return chez.div(count_meth(clusters, "dominant-theme?"), len(clusters))

    @message("theme-present?")
    def theme_present_p(this, self, theme):
        return exists_p(tell(self, "get-equivalent-theme", theme))

    @message("get-equivalent-theme")
    def get_equivalent_theme(this, self, theme):
        return select_meth(this.all_themes, "equal?", theme)

    @message("get-max-positive-theme-activation")
    def get_max_positive_theme_activation(this, self, theme_type_or_s):
        return maximum(tell_all(tell(self, "get-themes", theme_type_or_s),
                                "get-positive-activation"))

    # --------------------------------------------------------------------
    # set-theme-activation adds a new theme if one doesn't exist or
    # changes the activation of an existing theme, without affecting the
    # frozen/unfrozen status of the theme or the theme's cluster.
    @message("set-theme-activation")
    def set_theme_activation(this, self, theme_type, dimension, relation, activation):
        theme = tell(self, "add-theme-unconditionally", theme_type, dimension, relation)
        if exists_p(theme):
            tell(theme, "set-activation", activation)
            tell(tell(theme, "get-cluster"), "update-dominant-theme")
            tell(setup.g_themespace_window, "update-graphics")
        return "done"

    @message("set-theme-cluster-activations")
    def set_theme_cluster_activations(this, self, theme_type, dimension, activation):
        cluster = tell(self, "get-cluster", theme_type, dimension)
        for relation in tell(cluster, "get-relations"):
            tell(self, "set-theme-activation", theme_type, dimension, relation, activation)
        return "done"

    @message("set-theme-type-activations")
    def set_theme_type_activations(this, self, theme_type, activation):
        for cluster in tell(self, "get-clusters", theme_type):
            tell(self, "set-theme-cluster-activations",
                 theme_type, tell(cluster, "get-dimension"), activation)
        return "done"

    @message("set-all-theme-activations")
    def set_all_theme_activations(this, self, activation):
        tell(self, "set-theme-type-activations", "top-bridge", activation)
        tell(self, "set-theme-type-activations", "bottom-bridge", activation)
        return tell(self, "set-theme-type-activations", "vertical-bridge", activation)

    # --------------------------------------------------------------------
    @message("theme-frozen?")
    def theme_frozen_p(this, self, theme_type, dimension, relation):
        theme = tell(self, "get-theme", theme_type, dimension, relation)
        if not exists_p(theme):
            return False
        return tell(theme, "frozen?")

    @message("cluster-frozen?")
    def cluster_frozen_p(this, self, theme_type, dimension):
        return tell(tell(self, "get-cluster", theme_type, dimension), "frozen?")

    @message("theme-type-frozen?")
    def theme_type_frozen_p(this, self, theme_type):
        return andmap_meth(tell(self, "get-clusters", theme_type), "frozen?")

    @message("everything-frozen?")
    def everything_frozen_p(this, self):
        return chez.andmap(lambda type_: tell(self, "theme-type-frozen?", type_),
                           tell(self, "get-possible-theme-types"))

    # --------------------------------------------------------------------
    @message("freeze-theme")
    def freeze_theme(this, self, theme_type, dimension, relation):
        theme = tell(self, "get-theme", theme_type, dimension, relation)
        if exists_p(theme):
            tell(theme, "freeze")
        return "done"

    # freeze-theme-cluster freezes an entire cluster.  As long as a
    # cluster remains frozen, no new themes can be added to it.
    @message("freeze-theme-cluster")
    def freeze_theme_cluster(this, self, theme_type, dimension):
        return tell(tell(self, "get-cluster", theme_type, dimension), "freeze")

    @message("freeze-theme-type")
    def freeze_theme_type(this, self, theme_type):
        for cluster in tell(self, "get-clusters", theme_type):
            tell(cluster, "freeze")
        return "done"

    @message("freeze-everything")
    def freeze_everything(this, self):
        for cluster in this.all_clusters:
            tell(cluster, "freeze")
        return "done"

    # --------------------------------------------------------------------
    # unfreeze-theme has no effect on a theme in a frozen cluster.
    @message("unfreeze-theme")
    def unfreeze_theme(this, self, theme_type, dimension, relation):
        theme = tell(self, "get-theme", theme_type, dimension, relation)
        if exists_p(theme):
            tell(theme, "unfreeze")
        return "done"

    # unfreeze-theme-cluster unfreezes a cluster and all of the
    # individual themes in the cluster.
    @message("unfreeze-theme-cluster")
    def unfreeze_theme_cluster(this, self, theme_type, dimension):
        return tell(tell(self, "get-cluster", theme_type, dimension), "unfreeze")

    @message("unfreeze-theme-type")
    def unfreeze_theme_type(this, self, theme_type):
        for cluster in tell(self, "get-clusters", theme_type):
            tell(cluster, "unfreeze")
        return "done"

    @message("unfreeze-everything")
    def unfreeze_everything(this, self):
        for cluster in this.all_clusters:
            tell(cluster, "unfreeze")
        return "done"

    # --------------------------------------------------------------------
    # These methods delete themes from a cluster without changing the
    # frozen/unfrozen status of the cluster.
    @message("delete-theme")
    def delete_theme(this, self, theme_type, dimension, relation):
        theme = tell(self, "get-theme", theme_type, dimension, relation)
        if exists_p(theme):
            tell(tell(theme, "get-cluster"), "delete-theme", theme)
            this.all_themes = chez.remq(theme, this.all_themes)
            tell(setup.g_themespace_window, "erase-theme", theme)
        return "done"

    @message("delete-theme-cluster")
    def delete_theme_cluster(this, self, theme_type, dimension):
        cluster = tell(self, "get-cluster", theme_type, dimension)
        for relation in tell(cluster, "get-relations"):
            tell(self, "delete-theme", theme_type, dimension, relation)
        return "done"

    @message("delete-theme-type")
    def delete_theme_type(this, self, theme_type):
        for cluster in tell(self, "get-clusters", theme_type):
            tell(cluster, "delete-themes")
        this.all_themes = filter_out_meth(this.all_themes, "theme-type?", theme_type)
        tell(setup.g_themespace_window, "erase-all-themes", theme_type)
        return "done"

    @message("delete-everything")
    def delete_everything(this, self):
        for cluster in this.all_clusters:
            tell(cluster, "delete-themes")
        this.all_themes = []
        tell(setup.g_themespace_window, "erase-all-themes")
        return "done"

    # --------------------------------------------------------------------
    @message("spread-activation")
    def spread_activation(this, self):
        for cluster in this.all_clusters:
            tell(cluster, "spread-activation")
            tell(cluster, "update-dominant-theme")
        tell(setup.g_themespace_window, "update-graphics")
        return "done"

    @message("update-dominant-themes")
    def update_dominant_themes(this, self, *theme_types):
        if _null_p(theme_types):
            for cluster in this.all_clusters:
                tell(cluster, "update-dominant-theme")
        else:
            for theme_type in theme_types:
                for cluster in tell(self, "get-clusters", theme_type):
                    tell(cluster, "update-dominant-theme")
        return "done"

    @message("add-theme-if-possible")
    def add_theme_if_possible(this, self, theme_type, dimension, relation):
        return tell(self, "add-theme", theme_type, dimension, relation, True)

    @message("add-theme-unconditionally")
    def add_theme_unconditionally(this, self, theme_type, dimension, relation):
        return tell(self, "add-theme", theme_type, dimension, relation, False)

    @message("add-theme")
    def add_theme(this, self, theme_type, dimension, relation, check_if_frozen_p):
        # *display-mode?* (run.ss) is read only when self-watching is disabled
        if (setup.p_self_watching_enabled is False
                and _metacat.run.g_display_mode_p is False):
            return False
        theme = tell(self, "get-theme", theme_type, dimension, relation)
        if exists_p(theme):
            return theme
        cluster = tell(self, "get-cluster", theme_type, dimension)
        if check_if_frozen_p is not False and tell(cluster, "frozen?") is not False:
            return False
        new_theme = tell(cluster, "add-theme", relation)
        if exists_p(new_theme):
            this.all_themes = [new_theme] + this.all_themes
            tell(setup.g_themespace_window, "set-theme-graphics-parameters-and-draw", new_theme)
        return new_theme

    @message("set-propagation-function")
    def set_propagation_function(this, self, func):
        for cluster in this.all_clusters:
            tell(cluster, "set-propagation-function", func)
        return "done"

    @message("set-activation-function")
    def set_activation_function(this, self, func):
        for cluster in this.all_clusters:
            tell(cluster, "set-activation-function", func)
        return "done"

    @message("set-sensitivity")
    def set_sensitivity(this, self, value):
        for cluster in this.all_clusters:
            tell(cluster, "set-sensitivity", value)
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_themespace():
    """themes.ss: make-themespace"""
    return Themespace()


def get_possible_relations(theme_type, dimension):
    """themes.ss: get-possible-relations"""
    instance_nodes = tell(dimension, "get-instance-nodes")
    return remq_duplicates(cross_product_map(slipnet.get_label, instance_nodes, instance_nodes))


class ThemeCluster(SchemeObject):
    """themes.ss: make-theme-cluster (the closure the procedure it returns makes)"""
    __slots__ = ("theme_type", "dimension", "relations", "num_relations",
                 "propagation_function", "sensitivity", "self_excitation_function",
                 "net_effect", "activation_function", "dominant_theme", "themes", "frozen_p")

    def __init__(this, theme_type, dimension):
        this.theme_type = theme_type
        this.dimension = dimension
        # the let*, in order
        this.relations = get_possible_relations(theme_type, dimension)
        this.num_relations = len(this.relations)

        # This function determines the amount of inhibitory (-) or excitatory (+)
        # activation that flows across a link from a theme with activation a1 to
        # a theme with activation a2.  A positive value represents an excitatory
        # effect, negative represents an inhibitory effect.
        def propagation_function(a1, a2):
            """themes.ss: make-theme-cluster (propagation-function)"""
            if a1 < 0 and a2 < 0:
                weight = negative_to_negative_weight
            elif a1 < 0:
                weight = negative_to_positive_weight
            elif a2 < 0:
                weight = positive_to_negative_weight
            else:
                weight = positive_to_positive_weight
            return chez.mul(chez.abs_(a1), percent(weight))
        this.propagation_function = propagation_function
        this.sensitivity = 1.0

        # This function determines the amount of self-excitation for a theme
        # with activation a:
        def self_excitation_function(a):
            """themes.ss: make-theme-cluster (self-excitation-function)"""
            if a > 0:
                return chez.mul(a, percent(self_to_self_weight))
            return 0
        this.self_excitation_function = self_excitation_function

        # This function takes the raw net inhibitory (-) or excitatory (+)
        # activation received by a theme from all themes in its cluster,
        # including itself, and scales it into the range (-N,+N), where N is the
        # maximum possible activation change allowed in one step.  The amount of
        # scaling depends on the number of possible themes in the cluster.  The
        # sensitivity parameter controls the slope of the sigmoid.  0.5 or less
        # makes themes relatively insensitive to impinging excitatory/inhibitory
        # activation, while above 2.0 or so makes them more sensitive:
        # 1.2: alpha is fixed here, so set-sensitivity has no effect on it
        alpha = chez.mul(this.sensitivity, Fraction(1, 50), chez.div(1, this.num_relations))

        def net_effect(net_input):
            """themes.ss: make-theme-cluster (net-effect)"""
            # chez: (* alpha 0) is exact 0 and (tanh 0) exact 0
            return round_(chez.mul(p_theme_spread_amount, chez.tanh(chez.mul(alpha, net_input))))
        this.net_effect = net_effect

        # This function determines the new activation of a theme based on the
        # net inhibitory or excitatory input it receives from other themes in
        # its cluster.  net-input < 0 causes an inhibitory effect, net-input > 0
        # causes an excitatory effect.  Themes themselves can be either positively
        # or negatively activated.  Inhibiting a negative theme pulls its
        # activation toward zero, while exciting it pushes its activation toward
        # -100.  Inhibiting a positive theme pulls its activation toward zero,
        # while exciting it pushes its activation toward +100:
        def activation_function(net_input, activation):
            """themes.ss: make-theme-cluster (activation-function)"""
            if activation < 0:
                return clip_negative(chez.sub(activation, net_effect(net_input)))
            return clip_positive(chez.add(activation, net_effect(net_input)))
        this.activation_function = activation_function
        this.dominant_theme = False
        this.themes = []
        this.frozen_p = False

    @message("object-type")
    def object_type(this, self):
        return "theme-cluster"

    @message("print")
    def print_(this, self):
        sugar.printf("~a ~a themes:",
                     this.theme_type,
                     tell(this.dimension, "get-short-name") if exists_p(this.dimension)
                     else String(""))
        if _null_p(this.themes):
            return sugar.printf(" None~%")
        for theme in this.themes:
            sugar.printf("~%   ~a (~a)", tell(theme, "ascii-name"), tell(theme, "get-activation"))
            if theme is this.dominant_theme:
                sugar.printf(" - DOMINANT")
        return sugar.printf("~%")

    @message("get-theme-type")
    def get_theme_type(this, self):
        return this.theme_type

    @message("get-theme")
    def get_theme(this, self, relation):
        return select_meth(this.themes, "relation?", relation)

    @message("get-themes")
    def get_themes(this, self):
        return this.themes

    @message("get-dominant-theme")
    def get_dominant_theme(this, self):
        return this.dominant_theme

    @message("dominant-theme?")
    def dominant_theme_p(this, self):
        return exists_p(this.dominant_theme)

    @message("get-dimension")
    def get_dimension(this, self):
        return this.dimension

    @message("get-relations")
    def get_relations(this, self):
        return this.relations

    @message("dimension?")
    def dimension_p(this, self, dim):
        return dim is this.dimension

    @message("get-max-positive-theme-activation")
    def get_max_positive_theme_activation(this, self):
        return maximum(tell_all(this.themes, "get-positive-activation"))

    @message("pick-positive-theme")
    def pick_positive_theme(this, self):
        return stochastic_pick_by_method(this.themes, "get-positive-activation")

    @message("frozen?")
    def frozen_p_(this, self):
        return this.frozen_p

    @message("freeze")
    def freeze(this, self):
        # This automatically freezes all themes in cluster:
        this.frozen_p = True
        return "done"

    @message("unfreeze")
    def unfreeze(this, self):
        this.frozen_p = False
        for theme in this.themes:
            tell(theme, "unfreeze")
        return "done"

    @message("update-dominant-theme")
    def update_dominant_theme(this, self):
        if _null_p(this.themes):
            this.dominant_theme = False
        else:
            # chez: Chez's sort and its predicate calls (utilities.sort_by_method)
            ranked_themes = sort_by_method("get-absolute-activation", lambda a, b: a > b,
                                           this.themes)
            if (chez.positive_p(tell(first(ranked_themes), "get-activation"))
                    and chez.sub(tell(first(ranked_themes), "get-absolute-activation"),
                                 (0 if _null_p(ranked_themes[1:])
                                  else tell(second(ranked_themes), "get-absolute-activation")))
                    > p_dominant_theme_margin):
                this.dominant_theme = first(ranked_themes)
            else:
                this.dominant_theme = False
        return "done"

    @message("spread-activation")
    def spread_activation(this, self):
        for theme in this.themes:
            tell(theme, "clear-net-input-buffer")
        for theme in this.themes:
            tell(theme, "spread-activation")
        for theme in this.themes:
            tell(theme, "update-activation")
        return "done"

    @message("add-theme")
    def add_theme(this, self, relation):
        if not member_p(relation, this.relations):
            return False
        theme = make_bridge_theme(this.theme_type, this.dimension, relation)
        for other_theme in this.themes:
            tell(other_theme, "add-outgoing-link", theme)
            tell(theme, "add-outgoing-link", other_theme)
        tell(theme, "set-cluster", self)
        tell(theme, "set-propagation-function", this.propagation_function)
        tell(theme, "set-self-excitation-function", this.self_excitation_function)
        tell(theme, "set-activation-function", this.activation_function)
        this.themes = [theme] + this.themes
        return theme

    @message("delete-theme")
    def delete_theme(this, self, theme):
        this.themes = chez.remq(theme, this.themes)
        for other_theme in this.themes:
            tell(other_theme, "delete-outgoing-link", theme)
        tell(self, "update-dominant-theme")
        return "done"

    @message("delete-themes")
    def delete_themes(this, self):
        this.themes = []
        this.dominant_theme = False
        return "done"

    @message("set-propagation-function")
    def set_propagation_function(this, self, func):
        for theme in this.themes:
            tell(theme, "set-propagation-function", func)
        this.propagation_function = func
        return "done"

    @message("set-self-excitation-function")
    def set_self_excitation_function(this, self, func):
        for theme in this.themes:
            tell(theme, "set-self-excitation-function", func)
        this.self_excitation_function = func
        return "done"

    @message("set-activation-function")
    def set_activation_function(this, self, func):
        for theme in this.themes:
            tell(theme, "set-activation-function", func)
        this.activation_function = func
        return "done"

    @message("set-sensitivity")
    def set_sensitivity(this, self, value):
        this.sensitivity = value
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_theme_cluster(theme_type):
    """themes.ss: make-theme-cluster"""
    def make(dimension):
        """themes.ss: make-theme-cluster (the procedure it returns)"""
        return ThemeCluster(theme_type, dimension)
    return make


class GenericTheme(SchemeObject):
    """themes.ss: make-generic-theme (the closure)"""
    __slots__ = ("theme_type", "activation", "net_input_buffer", "propagation_function",
                 "self_excitation_function", "activation_function", "theme_cluster",
                 "outgoing_theme_links", "graphics_window_panel", "graphics_coord",
                 "text_coord", "normal_graphics_pexp", "highlight_graphics_pexp",
                 "graphics_activation", "frozen_p")

    def __init__(this, theme_type):
        this.theme_type = theme_type
        this.activation = 0
        this.net_input_buffer = 0
        this.propagation_function = False
        this.self_excitation_function = False
        this.activation_function = False
        this.theme_cluster = False
        this.outgoing_theme_links = []
        this.graphics_window_panel = False
        this.graphics_coord = False
        this.text_coord = False
        this.normal_graphics_pexp = False
        this.highlight_graphics_pexp = False
        this.graphics_activation = 0
        this.frozen_p = False

    @message("object-type")
    def object_type(this, self):
        return "generic-theme"

    @message("get-theme-type")
    def get_theme_type(this, self):
        return this.theme_type

    # type/s may be either a symbol or a list:
    @message("theme-type?")
    def theme_type_p(this, self, type_or_s):
        if isinstance(type_or_s, str):      # symbol?
            return this.theme_type == type_or_s
        return member_p(this.theme_type, type_or_s)

    @message("set-graphics-parameters")
    def set_graphics_parameters(this, self, panel, center, text, norm_pexp, hl_pexp):
        this.graphics_window_panel = panel
        this.graphics_coord = center
        this.text_coord = text
        this.normal_graphics_pexp = norm_pexp
        this.highlight_graphics_pexp = hl_pexp
        this.graphics_activation = 0
        return "done"

    @message("get-normal-pexp")
    def get_normal_pexp(this, self):
        return this.normal_graphics_pexp

    @message("get-highlight-pexp")
    def get_highlight_pexp(this, self):
        return this.highlight_graphics_pexp

    @message("get-graphics-panel")
    def get_graphics_panel(this, self):
        return this.graphics_window_panel

    @message("clicked?")
    def clicked_p(this, self, x, y):
        if not exists_p(this.graphics_window_panel):
            return False
        delta = chez.mul(Fraction(1, 2),
                         tell(this.graphics_window_panel, "get-activation-diameter"))
        return ((chez.abs_(chez.sub(x, first(this.graphics_coord))) <= delta
                 and chez.abs_(chez.sub(y, second(this.graphics_coord))) <= delta)
                or (chez.abs_(chez.sub(x, first(this.text_coord))) <= delta
                    and chez.abs_(chez.sub(y, second(this.text_coord))) <= delta))

    @message("draw-activation-graphics")
    def draw_activation_graphics(this, self):
        tell(this.graphics_window_panel, "draw-absolute-activation",
             this.graphics_coord, this.activation)
        this.graphics_activation = this.activation
        return "done"

    @message("erase-activation-graphics")
    def erase_activation_graphics(this, self):
        tell(this.graphics_window_panel, "erase-activation", this.graphics_coord)
        this.graphics_activation = 0
        return "done"

    @message("update-activation-graphics")
    def update_activation_graphics(this, self):
        if not (this.activation == this.graphics_activation):
            if sgn(this.activation) == sgn(this.graphics_activation):
                if chez.abs_(this.activation) > chez.abs_(this.graphics_activation):
                    tell(this.graphics_window_panel, "draw-absolute-activation",
                         this.graphics_coord, this.activation)
                else:
                    tell(this.graphics_window_panel, "decrease-absolute-activation",
                         this.graphics_coord, this.activation)
            else:
                tell(this.graphics_window_panel, "erase-activation", this.graphics_coord)
                tell(this.graphics_window_panel, "draw-absolute-activation",
                     this.graphics_coord, this.activation)
            this.graphics_activation = this.activation
        return "done"

    @message("get-activation")
    def get_activation(this, self):
        return this.activation

    @message("get-absolute-activation")
    def get_absolute_activation(this, self):
        return chez.abs_(this.activation)

    @message("get-positive-activation")
    def get_positive_activation(this, self):
        return chez.max_(0, this.activation)

    @message("negative?")
    def negative_p(this, self):
        return this.activation < 0

    @message("get-cluster")
    def get_cluster(this, self):
        return this.theme_cluster

    @message("set-cluster")
    def set_cluster(this, self, c):
        this.theme_cluster = c
        return "done"

    @message("dominant?")
    def dominant_p(this, self):
        return self is tell(this.theme_cluster, "get-dominant-theme")

    @message("frozen?")
    def frozen_p_(this, self):
        if this.frozen_p is not False:
            return this.frozen_p
        return tell(this.theme_cluster, "frozen?")

    @message("individually-frozen?")
    def individually_frozen_p(this, self):
        return this.frozen_p

    @message("freeze")
    def freeze(this, self):
        this.frozen_p = True
        return "done"

    @message("unfreeze")
    def unfreeze(this, self):
        this.frozen_p = False
        return "done"

    @message("add-outgoing-link")
    def add_outgoing_link(this, self, theme):
        this.outgoing_theme_links = [theme] + this.outgoing_theme_links
        return "done"

    @message("delete-outgoing-link")
    def delete_outgoing_link(this, self, theme):
        this.outgoing_theme_links = chez.remq(theme, this.outgoing_theme_links)
        return "done"

    @message("clear-net-input-buffer")
    def clear_net_input_buffer(this, self):
        this.net_input_buffer = 0
        return "done"

    @message("increment-net-input-buffer")
    def increment_net_input_buffer(this, self, delta):
        this.net_input_buffer = chez.add(this.net_input_buffer, delta)
        return "done"

    @message("spread-activation")
    def spread_activation(this, self):
        for theme in this.outgoing_theme_links:
            tell(theme, "increment-net-input-buffer",
                 this.propagation_function(this.activation, tell(theme, "get-activation")))
        tell(self, "increment-net-input-buffer", this.self_excitation_function(this.activation))
        tell(self, "increment-net-input-buffer", chez.sub(p_theme_decay_amount))
        return "done"

    @message("update-activation")
    def update_activation(this, self):
        if tell(self, "frozen?") is False:
            this.activation = this.activation_function(this.net_input_buffer, this.activation)
        this.net_input_buffer = 0
        return "done"

    @message("boost-activation")
    def boost_activation(this, self, factor):
        if tell(self, "frozen?") is False:
            this.activation = clip_positive(
                round_(chez.add(this.activation,
                                chez.mul(percent(factor), p_theme_boost_amount))))
        return "done"

    @message("set-activation")
    def set_activation(this, self, n):
        this.activation = n
        return "done"

    @message("set-propagation-function")
    def set_propagation_function(this, self, func):
        this.propagation_function = func
        return "done"

    @message("set-self-excitation-function")
    def set_self_excitation_function(this, self, func):
        this.self_excitation_function = func
        return "done"

    @message("set-activation-function")
    def set_activation_function(this, self, func):
        this.activation_function = func
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_generic_theme(theme_type):
    """themes.ss: make-generic-theme"""
    return GenericTheme(theme_type)


class BridgeTheme(SchemeObject):
    """themes.ss: make-bridge-theme (the closure)"""
    __slots__ = ("theme_type", "dimension", "relation", "generic_theme")

    def __init__(this, theme_type, dimension, relation):
        this.theme_type = theme_type
        this.dimension = dimension
        this.relation = relation
        this.generic_theme = make_generic_theme(theme_type)

    @message("object-type")
    def object_type(this, self):
        return "bridge-theme"

    @message("ascii-name")
    def ascii_name(this, self):
        return String(chez.format_("~a:~a",
                                   tell(this.dimension, "get-short-name"),
                                   (tell(this.relation, "get-lowercase-name")
                                    if exists_p(this.relation) else String("different"))))

    @message("print")
    def print_(this, self):
        return sugar.printf("~a ~a theme  (~a)~%",
                            this.theme_type,
                            tell(self, "ascii-name"),
                            tell(self, "get-activation"))

    @message("get-dimension")
    def get_dimension(this, self):
        return this.dimension

    @message("get-relation")
    def get_relation(this, self):
        return this.relation

    @message("difference-theme?")
    def difference_theme_p(this, self):
        return not exists_p(this.relation)

    @message("identity-theme?")
    def identity_theme_p(this, self):
        return this.relation is slipnet.plato_identity

    @message("opposite-theme?")
    def opposite_theme_p(this, self):
        return this.relation is slipnet.plato_opposite

    @message("dimension?")
    def dimension_p(this, self, dim):
        return dim is this.dimension

    @message("relation?")
    def relation_p(this, self, rel):
        return rel is this.relation

    # ALL themes (regardless of type) should have an 'equal? method:
    @message("equal?")
    def equal_p(this, self, other_theme):
        return (tell(other_theme, "theme-type?", this.theme_type) is not False
                and tell(other_theme, "dimension?", this.dimension) is not False
                and tell(other_theme, "relation?", this.relation))

    @message("spread-activation-to-slipnet")
    def spread_activation_to_slipnet(this, self):
        # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
        coin_flip = chez.random(1.0)
        if coin_flip < cube(percent(tell(self, "get-absolute-activation"))):
            tell(this.dimension, "activate-from-workspace")
        if exists_p(this.relation):
            # chez: stochastic-if* draws its coin before the probability
            coin_flip = chez.random(1.0)
            if coin_flip < cube(percent(tell(self, "get-activation"))):
                tell(this.relation, "activate-from-workspace")
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.generic_theme)


def make_bridge_theme(theme_type, dimension, relation):
    """themes.ss: make-bridge-theme"""
    return BridgeTheme(theme_type, dimension, relation)


def theme_type_to_bridge_type(theme_type):
    """themes.ss: theme-type->bridge-type"""
    if theme_type == "top-bridge":
        return "top"
    if theme_type == "bottom-bridge":
        return "bottom"
    if theme_type == "vertical-bridge":
        return "vertical"
    return None     # (case ...) without else: void


def bridge_type_to_theme_type(theme_type):
    """themes.ss: bridge-type->theme-type"""
    if theme_type == "top":
        return "top-bridge"
    if theme_type == "bottom":
        return "bottom-bridge"
    if theme_type == "vertical":
        return "vertical-bridge"
    return None     # (case ...) without else: void


def thematic_bridge_scout():
    """themes.ss: thematic-bridge-scout (the codelet procedure)"""
    if setup.p_self_watching_enabled is False:
        say("Self-watching disabled. Fizzling.")
        sugar.fizzle()
    active_bridge_theme_types = tell(g_themespace, "get-active-bridge-theme-types")
    if _null_p(active_bridge_theme_types):
        say("No active bridge theme types. Fizzling.")
        sugar.fizzle()
    # the let*, binding by binding
    # chez: map's order of application (the procedure only reads; its two tells only read)
    theme_type_weights = chez.map_(
        lambda theme_type: chez.mul(
            tell(g_themespace, "get-max-positive-theme-activation", theme_type),
            hundred_minus(tell(workspace.g_workspace, "get-mapping-strength",
                               theme_type_to_bridge_type(theme_type)))),
        active_bridge_theme_types)
    theme_type = stochastic_pick(active_bridge_theme_types, theme_type_weights)
    bridge_type = theme_type_to_bridge_type(theme_type)
    bridge_orientation = bridges.bridge_type_to_orientation(bridge_type)
    # Only positive themes can exert active pressure in the form of
    # thematic codelets.  Negative themes can only influence
    # structure strengths.  Otherwise, too many spurious bridges get
    # created by thematic codelets looking for structures that are
    # "not LettCtgy:identity" or whatever:
    # utilities.ss's filter: one prob? per cluster, first to last
    clusters = filter_(
        lambda cluster: prob_p(square(percent(tell(cluster,
                                                   "get-max-positive-theme-activation")))),
        tell(g_themespace, "get-clusters", theme_type))
    # chez: tell-all's map order (one stochastic-pick-by-method per cluster)
    themes = tell_all(clusters, "pick-positive-theme")
    say("Currently active bridge-theme types, weights:")
    say(active_bridge_theme_types)
    say(theme_type_weights)
    say("Scouting for a ", bridge_type, " bridge...")
    say("Focusing on dimensions ",
        tell_all(tell_all(clusters, "get-dimension"), "get-short-name"))
    if _null_p(themes):
        say("Couldn't choose any ", theme_type, " themes. Fizzling.")
        sugar.fizzle()
    say("Looking for a way to build a ", bridge_type, " bridge that supports themes:")
    vprint(themes)
    objects = tell(workspace.g_workspace, "get-possible-bridge-objects", bridge_type)
    chosen_object = stochastic_pick_by_method(objects, "get-inter-string-salience",
                                              bridge_orientation)
    applicable_themes = filter_(
        lambda theme: tell(chosen_object, "description-type-present?",
                           tell(theme, "get-dimension")),
        themes)
    non_applicable_themes = remq_elements(applicable_themes, themes)
    chosen_string = tell(chosen_object, "get-string")
    say(bridge_type, " objects are:")
    say(tell_all(objects, "ascii-name"))
    say("Weights are:")
    say(tell_all(objects, "get-inter-string-salience", bridge_orientation))
    say("Chose ", tell(chosen_object, "ascii-name"), " in ", tell(chosen_string, "generic-name"))
    say("Applicable themes: ", tell_all(applicable_themes, "ascii-name"))
    say("Non-applicable themes: ", tell_all(non_applicable_themes, "ascii-name"))
    if not _null_p(non_applicable_themes):
        say("Object lacks the following description types:")
        for theme in non_applicable_themes:
            say(tell(theme, "get-dimension"))
        say("Proposing descriptions if possible...")
        for theme in non_applicable_themes:
            # Special case: if we're focusing on StringPos, but a StringPos
            # description is impossible for the chosen object, don't propose a
            # bridge with the object.  Example: iijjkk->aabbdd with StringPos:iden
            # and ObjCtgy:iden themes clamped.  Without this test, spurious i--b
            # bridges get proposed:
            if (tell(theme, "dimension?", slipnet.plato_string_position_category) is not False
                    and tell(slipnet.plato_string_position_category,
                             "description-possible?", chosen_object) is False):
                say("StringPos description impossible for chosen object. Fizzling.")
                sugar.fizzle()
            propose_description_based_on_theme(chosen_object, theme)
    other_string = tell(workspace.g_workspace, "get-other-string",
                        chosen_string, bridge_orientation)
    other_object_candidates = filter_meth(tell(other_string, "get-objects"),
                                          "all-description-types-present?",
                                          tell_all(applicable_themes, "get-dimension"))
    say("Looking for ", tell(other_string, "generic-name"),
        " object with ", tell_all(tell_all(applicable_themes, "get-dimension"),
                                  "get-short-name"), " descriptions...")
    if _null_p(other_object_candidates):
        say("Couldn't find ", tell(other_string, "generic-name"),
            " object with necessary descriptions. Fizzling.")
        sugar.fizzle()
    say("These ", tell(other_string, "generic-name"), " objects have the necessary descriptions:")
    say(tell_all(other_object_candidates, "ascii-name"))
    from_object_p = tell(chosen_string, "string-type?", "initial")
    if from_object_p is False:
        from_object_p = (tell(chosen_string, "string-type?", "target") is not False
                         and bridge_orientation == "horizontal")
    get_conditions = conditions_for_bridge(chosen_object, from_object_p, applicable_themes)

    def selection_entry(obj):
        conditions = get_conditions(obj)
        if exists_p(conditions):        # chez: #f only ('() is a condition list)
            selection_weight = tell(obj, "get-inter-string-salience", bridge_orientation)
            return [selection_weight, obj, conditions]
        return False
    # map-compress: first to last, element by element (get-conditions may draw)
    other_object_selection_list = map_compress(selection_entry, other_object_candidates)
    if _null_p(other_object_selection_list):
        say("No way to make a bridge that supports the themes. Fizzling.")
        sugar.fizzle()
    chosen_element = stochastic_select(other_object_selection_list)
    other_object = second(chosen_element)
    conditions = third(chosen_element)
    object1 = chosen_object if from_object_p is not False else other_object
    object2 = other_object if from_object_p is not False else chosen_object
    flip1_p = member_p(object1, conditions)
    flip2_p = member_p(object2, conditions)
    say("Chose ", tell(other_object, "ascii-name"), ".")
    proposed_bridge = bridges.propose_bridge(bridge_orientation, object1, flip1_p,
                                             object2, flip2_p)
    activations = tell_all(applicable_themes, "get-activation")
    if _null_p(activations):
        # (apply max '()): not reached (an empty theme list supports no bridge)
        raise chez.SchemeError("max", "incorrect number of arguments")
    max_theme_activation = chez.max_(*activations)
    urgency = round_(chez.mul(percent(max_theme_activation),
                              (coderack.p_extremely_high_urgency
                               if tell(chosen_object, "string-spanning-group?") is not False
                               else coderack.p_very_high_urgency)))
    look_for_auxiliary_slippages(proposed_bridge)
    return sugar.post_codelet_star(urgency, coderack.bridge_evaluator, proposed_bridge)


def look_for_auxiliary_slippages(proposed_bridge):
    """themes.ss: look-for-auxiliary-slippages"""
    vprintf("Looking for auxiliary slippages to make...~n")
    for slippage in tell(proposed_bridge, "get-slippages"):
        # the let*: only reads
        object1 = tell(slippage, "get-object1")
        object2 = tell(slippage, "get-object2")
        CM_type = tell(slippage, "get-CM-type")
        descriptor1 = tell(slippage, "get-descriptor1")
        descriptor2 = tell(slippage, "get-descriptor2")   # noqa: F841 (bound, never used)
        label = tell(slippage, "get-label")
        linked_instance_nodes = filter_(
            lambda node: (tell(node, "instance?") is not False
                          and not (tell(node, "get-category") is CM_type)),
            tell_all(tell(descriptor1, "get-outgoing-links"), "get-to-node"))
        for node in linked_instance_nodes:
            # a two-binding let; both only read
            new_CM_type = tell(node, "get-category")
            related_node = tell(node, "get-related-node", label)
            if (tell(proposed_bridge, "CM-type-present?", new_CM_type) is False
                    and exists_p(related_node)
                    and tell(node, "possible-descriptor?", object1) is not False
                    and tell(related_node, "possible-descriptor?", object2) is not False):
                vprintf("Auxiliary slippage ~a=>~a possible based on ~a...~n",
                        tell(node, "get-short-name"),
                        tell(related_node, "get-short-name"),
                        tell(slippage, "print-name"))
                vprintf("~a's degree-of-assoc is ~a~n",
                        tell(label, "get-lowercase-name"),
                        tell(label, "get-degree-of-assoc"))
                slippage_probability = percent(tell(label, "get-degree-of-assoc"))
                make_slippage_p = prob_p(slippage_probability)
                if make_slippage_p:
                    vprintf("Making slippage...~n")
                else:
                    vprintf("Failed. Not making slippage.~n")
                if make_slippage_p:
                    new_slippage = concept_mappings.make_concept_mapping(
                        object1, new_CM_type, node,
                        object2, new_CM_type, related_node)
                    if tell(object1, "description-type-present?", new_CM_type) is False:
                        new_description = descriptions.make_description(
                            object1, new_CM_type, node)
                        descriptions.build_description(new_description)
                        vprintf("Added description to ~a:~n", tell(object1, "ascii-name"))
                        vprint(new_description)
                        vprintf("Fizzling.~n")
                        sugar.fizzle()
                    if tell(object2, "description-type-present?", new_CM_type) is False:
                        new_description = descriptions.make_description(
                            object2, new_CM_type, related_node)
                        descriptions.build_description(new_description)
                        vprintf("Added description to ~a:~n", tell(object2, "ascii-name"))
                        vprint(new_description)
                        vprintf("Fizzling.~n")
                        sugar.fizzle()
                    tell(proposed_bridge, "add-concept-mapping", new_slippage)
                    vprintf("Added new ~a slippage to bridge:~n",
                            tell(new_slippage, "print-name"))
                    vprint(proposed_bridge)
    return None


def propose_description_based_on_theme(object_, theme):
    """themes.ss: propose-description-based-on-theme"""
    dimension = tell(theme, "get-dimension")
    possible_descriptors = tell(dimension, "get-possible-descriptors", object_)
    if _null_p(possible_descriptors):
        return say("   No ", dimension, " description possible.")
    chosen_descriptor = stochastic_pick_by_method(possible_descriptors, "get-activation")
    proposed_description = descriptions.make_description(object_, dimension, chosen_descriptor)
    tell(proposed_description, "update-proposal-level", workspace.p_proposed)
    tell(chosen_descriptor, "activate-from-workspace")
    say("   Proposing ", tell(proposed_description, "print-name"), " description.")
    # the post-codelet* arguments only read
    return sugar.post_codelet_star(tell(theme, "get-absolute-activation"),
                                   coderack.description_evaluator, proposed_description)


def conditions_for_bridge(chosen_object, from_object_p, themes):
    """themes.ss: conditions-for-bridge"""
    def conditions(other_object):
        """themes.ss: conditions-for-bridge (the procedure it returns)"""
        # a three-binding let; none has effects
        object1 = chosen_object if from_object_p is not False else other_object
        object2 = other_object if from_object_p is not False else chosen_object
        themes_support_p = theme_support_tester(themes)
        if workspace_objects.lone_spanning_object_p(object1, object2) is not False:
            return False
        if workspace_objects.both_spanning_groups_p(object1, object2) is False:
            return [] if themes_support_p(object1, object2) is not False else False
        if themes_support_p(object1, object2) is not False:
            return []
        # Don't always try to flip object1 first:
        num1 = tell(object1, "get-num-of-spanning-bridges")
        num2 = tell(object2, "get-num-of-spanning-bridges")
        if num1 > num2:
            object1_bias = percent(20)
        elif num1 < num2:
            object1_bias = percent(80)
        else:
            object1_bias = percent(50)
        # (themes-support? (flipped object1) (flipped object2)): making a flipped
        # version neither draws nor changes shared state, so left to right is safe
        if prob_p(object1_bias):
            # Try to flip object1 first
            if themes_support_p(flipped(object1), object2) is not False:
                return [object1]
            if themes_support_p(object1, flipped(object2)) is not False:
                return [object2]
            if themes_support_p(flipped(object1), flipped(object2)) is not False:
                return [object1, object2]
            return False
        # Try to flip object2 first
        if themes_support_p(object1, flipped(object2)) is not False:
            return [object2]
        if themes_support_p(flipped(object1), object2) is not False:
            return [object1]
        if themes_support_p(flipped(object1), flipped(object2)) is not False:
            return [object1, object2]
        return False
    return conditions


def flipped(object_):
    """themes.ss: flipped"""
    return tell(object_, "make-flipped-version")


# theme-support-tester assumes positively-activated themes:

def theme_support_tester(themes):
    """themes.ss: theme-support-tester"""
    def tester(object1, object2):
        """themes.ss: theme-support-tester (the procedure it returns)"""
        def conflicts_p(theme):
            return check_descriptions(object1, object2, conflicts_with_theme_p, theme)

        def supports_p(theme):
            return check_descriptions(object1, object2, supported_by_theme_p, theme)
        if chez.ormap(conflicts_p, themes) is not False:
            return False
        return chez.ormap(supports_p, themes)
    return tester


def check_descriptions(object1, object2, pred_p, theme):
    """themes.ss: check-descriptions"""
    def check(d1, d2):
        if tell(d1, "description-type?", tell(d2, "get-description-type")) is False:
            return False
        return pred_p(d1, d2, theme)
    return cross_product_ormap(check,
                               tell(object1, "get-descriptions"),
                               tell(object2, "get-descriptions"))


# conflicts-with-theme? and supported-by-theme? assume that descriptions
# d1 and d2 have the same description-type.  The three special cases are
# mutually exclusive.

def conflicts_with_theme_p(d1, d2, theme):
    """themes.ss: conflicts-with-theme?"""
    if (tell(d1, "description-type?", tell(theme, "get-dimension")) is False
            and special_direction_case_p(d1, d2, theme) is False):
        return False
    return (special_spanning_bridge_case_p(d1, d2, theme) is False
            and special_middle_middle_case_p(d1, d2, theme) is False
            and relation_consistent_with_theme_p(d1, d2, theme) is False)


def supported_by_theme_p(d1, d2, theme):
    """themes.ss: supported-by-theme?"""
    if (tell(d1, "description-type?", tell(theme, "get-dimension")) is False
            and special_direction_case_p(d1, d2, theme) is False):
        return False
    if special_spanning_bridge_case_p(d1, d2, theme) is not False:
        return False
    middle = special_middle_middle_case_p(d1, d2, theme)
    if middle is not False:
        return middle
    return relation_consistent_with_theme_p(d1, d2, theme)


def special_direction_case_p(d1, d2, theme):
    """themes.ss: special-direction-case?"""
    return (tell(d1, "description-type?", slipnet.plato_direction_category) is not False
            and workspace_objects.both_spanning_groups_p(tell(d1, "get-object"),
                                                         tell(d2, "get-object")) is not False
            and tell(theme, "dimension?", slipnet.plato_string_position_category))


def special_spanning_bridge_case_p(d1, d2, theme):
    """themes.ss: special-spanning-bridge-case?"""
    return ((tell(d1, "description-type?", slipnet.plato_object_category) is not False
             and workspace_objects.both_spanning_groups_p(tell(d1, "get-object"),
                                                          tell(d2, "get-object")) is not False
             and tell(theme, "dimension?", slipnet.plato_object_category) is not False)
            or (tell(d1, "description-type?", slipnet.plato_string_position_category)
                is not False
                and workspace_objects.both_spanning_objects_p(tell(d1, "get-object"),
                                                              tell(d2, "get-object"))
                is not False
                and tell(theme, "dimension?", slipnet.plato_string_position_category)))


def special_middle_middle_case_p(d1, d2, theme):
    """themes.ss: special-middle-middle-case?"""
    return (tell(d1, "get-descriptor") is slipnet.plato_middle
            and tell(d2, "get-descriptor") is slipnet.plato_middle
            and tell(theme, "dimension?", slipnet.plato_string_position_category) is not False
            and tell(theme, "relation?", slipnet.plato_opposite))


def relation_consistent_with_theme_p(d1, d2, theme):
    """themes.ss: relation-consistent-with-theme?"""
    label = slipnet.get_label(tell(d1, "get-descriptor"), tell(d2, "get-descriptor"))
    if tell(theme, "difference-theme?") is not False:
        return not (label is slipnet.plato_identity)
    return label is tell(theme, "get-relation")


def descriptions_affect_themespace_p(d1, d2):
    """themes.ss: descriptions-affect-themespace?"""
    return (tell(d1, "description-type?", tell(d2, "get-description-type")) is not False
            and ignore_descriptions_p(d1, d2) is False)


def ignore_descriptions_p(d1, d2):
    """themes.ss: ignore-descriptions?"""
    return (tell(d1, "relevant?") is False
            or tell(d2, "relevant?") is False
            or (tell(d1, "description-type?", slipnet.plato_object_category) is not False
                and workspace_objects.both_spanning_groups_p(tell(d1, "get-object"),
                                                             tell(d2, "get-object"))
                is not False)
            or (tell(d1, "description-type?", slipnet.plato_string_position_category)
                is not False
                and workspace_objects.both_spanning_objects_p(tell(d1, "get-object"),
                                                              tell(d2, "get-object"))
                is not False)
            or (tell(d1, "get-descriptor") is slipnet.plato_middle
                and tell(d2, "get-descriptor") is slipnet.plato_middle))


# This function "sharpens" a bridge's theme-compatibility rating. It is
# a squashing function from -1..+1 to -1..+1.

beta = 4


def bridge_theme_compatibility_sigmoid(x):
    """themes.ss: bridge-theme-compatibility-sigmoid"""
    return chez.sub1(chez.div(2, chez.add1(chez.exp(chez.mul(-2, beta, x)))))


# *slipnet-nodes* must already be defined: load() makes it.
g_themespace = False


# ---------------------------- Manual theme clamping ------------------------------

# abbreviations (the slipnode ones are set by load(), once the slipnet exists)
top = "top-bridge"
bot = "bottom-bridge"
ver = "vertical-bridge"
lcat = False
len_ = False
dir_ = False
spos = False
apos = False
otype = False
gtype = False
btype = False
facet = False
iden = False
succ = False
pred = False
opp = False
diff = False


def theme_help():
    """themes.ss: ?"""
    sugar.printf("Theme Types:  top bot ver~n")
    sugar.printf("Dimensions:   lcat len dir spos apos otype gtype btype facet~n")
    sugar.printf("Relations:    same succ pred opp diff~n")
    sugar.printf("---------------------------------------------------------------~n")
    sugar.printf("Set activation:         (set-themes [top] [lcat] [same] 100)~n")
    sugar.printf("Freeze:                 (freeze-themes [top] [lcat] [same])~n")
    sugar.printf("Unfreeze:               (unfreeze-themes [top] [lcat] [same])~n")
    sugar.printf("Unfreeze and clear:     (clear-themes [top] [lcat] [same])~n")
    sugar.printf("Ignore:                 (ignore-themes [top] [lcat] [same])~n")
    return sugar.printf("Delete from Themespace: (delete-themes [top] [lcat] [same])~n")


def set_themes(*args):
    """themes.ss: set-themes"""
    n = len(args)
    if n == 1:
        return tell(g_themespace, "set-all-theme-activations", args[0])
    if n == 2:
        return tell(g_themespace, "set-theme-type-activations", args[0], args[1])
    if n == 3:
        return tell(g_themespace, "set-theme-cluster-activations", args[0], args[1], args[2])
    if n == 4:
        return tell(g_themespace, "set-theme-activation", args[0], args[1], args[2], args[3])
    return None     # (case ...) without else: void


def freeze_themes(*args):
    """themes.ss: freeze-themes"""
    n = len(args)
    if n == 0:
        return tell(g_themespace, "freeze-everything")
    if n == 1:
        return tell(g_themespace, "freeze-theme-type", args[0])
    if n == 2:
        return tell(g_themespace, "freeze-theme-cluster", args[0], args[1])
    if n == 3:
        return tell(g_themespace, "freeze-theme", args[0], args[1], args[2])
    return None     # (case ...) without else: void


def unfreeze_themes(*args):
    """themes.ss: unfreeze-themes"""
    n = len(args)
    if n == 0:
        return tell(g_themespace, "unfreeze-everything")
    if n == 1:
        return tell(g_themespace, "unfreeze-theme-type", args[0])
    if n == 2:
        return tell(g_themespace, "unfreeze-theme-cluster", args[0], args[1])
    if n == 3:
        return tell(g_themespace, "unfreeze-theme", args[0], args[1], args[2])
    return None     # (case ...) without else: void


def clear_themes(*args):
    """themes.ss: clear-themes"""
    unfreeze_themes(*args)
    return delete_themes(*args)


def ignore_themes(*args):
    """themes.ss: ignore-themes"""
    clear_themes(*args)
    return freeze_themes(*args)


# delete-themes by itself doesn't change the frozen/unfrozen status of clusters:
def delete_themes(*args):
    """themes.ss: delete-themes"""
    n = len(args)
    if n == 0:
        return tell(g_themespace, "delete-everything")
    if n == 1:
        return tell(g_themespace, "delete-theme-type", args[0])
    if n == 2:
        return tell(g_themespace, "delete-theme-cluster", args[0], args[1])
    if n == 3:
        return tell(g_themespace, "delete-theme", args[0], args[1], args[2])
    return None     # (case ...) without else: void


def load():
    """themes.ss: the top-level forms that need other modules, in the file's order:
    the define-codelet-procedure* form (thematic-bridge-scout), *themespace*, and
    the REPL abbreviations that name slipnodes."""
    global g_themespace, lcat, len_, dir_, spos, apos, otype, gtype, btype, facet
    global iden, succ, pred, opp
    sugar.define_codelet_procedure_star("thematic-bridge-scout", thematic_bridge_scout)
    g_themespace = make_themespace()
    lcat = slipnet.plato_letter_category
    len_ = slipnet.plato_length
    dir_ = slipnet.plato_direction_category
    spos = slipnet.plato_string_position_category
    apos = slipnet.plato_alphabetic_position_category
    otype = slipnet.plato_object_category
    gtype = slipnet.plato_group_category
    btype = slipnet.plato_bond_category
    facet = slipnet.plato_bond_facet
    iden = slipnet.plato_identity
    succ = slipnet.plato_successor
    pred = slipnet.plato_predecessor
    opp = slipnet.plato_opposite
