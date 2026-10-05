"""slipnet.ss: slipnodes, slipnet links, activation, and the Slipnet itself.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from slipnet.ss, with
racket/engine/slipnet.rktl as a worked translation.

Closures are SchemeObject classes (docs/python-translation-plan.md, "Objects"):
make-slipnode's closure is Slipnode, make-slipnet-link's is SlipnetLink.  A
node's link lists are Python lists, rebuilt (never mutated) on every cons.

The file's top-level forms after the procedures (*slipnet-nodes* and the other
node lists, the top-down codelet types, intrinsic link lengths, descriptor
predicates and the 202 links) need coderack.py's codelet types and so run in
load(), in the file's order.  Each node is a module attribute (slipnet.plato_a)
and a top-level value; each link is only a top-level value (a-b-link), as in
the original.  Globals of files translated later (run.ss's
%update-cycle-length%, trace.ss's monitor-slipnode-activation-change,
formulas.ss's temp-adjusted-probability, *themespace*, *workspace*) are read
through the package at call time.  Activation arithmetic is exact (porting-notes.md,
item 05).  The engine never imports tkinter.
"""
from __future__ import annotations

import sys

import metacat as _metacat
from metacat import chez, sugar
from metacat import coderack
from metacat.chez import String
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (base_object, cd, compose, cube, exists_p, all_exist_p,
                               all_same_p, adjacency_map, filter_, filter_meth, first,
                               group_p, hundred_minus, letter_p, list_index, member_p, nth,
                               one_minus, percent, percent_40, prob_p, rest, round_, select,
                               select_meth, string_downcase, string_suffix, string_upcase,
                               tell_all)

p_max_activation = 100
p_workspace_activation = 100
p_full_activation_threshold = 50


class Slipnode(SchemeObject):
    """slipnet.ss: make-slipnode (the closure)"""
    __slots__ = ("name_symbol", "short_name", "conceptual_depth", "activation",
                 "activation_buffer", "frozen_p", "changed_frozen_p", "rate_of_decay",
                 "intrinsic_link_length", "shrunk_link_length", "top_down_codelet_types",
                 "incoming_links", "category_links", "instance_links", "property_links",
                 "lateral_links", "lateral_sliplinks", "links_labeled_by_node",
                 "descriptor_predicate_p", "full_lowercase_name", "full_uppercase_name",
                 "graphics_coord", "graphics_label_coord")

    def __init__(this, name_symbol, short_name, conceptual_depth):
        this.name_symbol = name_symbol
        this.short_name = short_name
        this.conceptual_depth = conceptual_depth
        this.activation = 0
        this.activation_buffer = 0
        this.frozen_p = False
        this.changed_frozen_p = False
        this.rate_of_decay = False
        this.intrinsic_link_length = 0
        this.shrunk_link_length = 0
        this.top_down_codelet_types = []
        this.incoming_links = []
        this.category_links = []
        this.instance_links = []
        this.property_links = []
        this.lateral_links = []
        this.lateral_sliplinks = []
        this.links_labeled_by_node = []
        this.descriptor_predicate_p = lambda object_: False
        this.full_lowercase_name = String(string_downcase(string_suffix(name_symbol, 6)))
        this.full_uppercase_name = String(string_upcase(this.full_lowercase_name))
        this.graphics_coord = False
        this.graphics_label_coord = False

    @message("object-type")
    def object_type(this, self):
        return "slipnode"

    @message("get-name-symbol")
    def get_name_symbol(this, self):
        return this.name_symbol

    @message("get-lowercase-name")
    def get_lowercase_name(this, self):
        return this.full_lowercase_name

    @message("get-uppercase-name")
    def get_uppercase_name(this, self):
        return this.full_uppercase_name

    @message("get-short-name")
    def get_short_name(this, self):
        return this.short_name

    @message("get-CM-short-name")
    def get_CM_short_name(this, self):
        if self is plato_letter_category:
            # This avoids occasional graphics problems with overlapping CMs:
            return String("LettCtgy")
        return tell(self, "get-short-name")

    # This is solely for the purposes of (say <slipnode>):
    @message("print-name")
    def print_name(this, self):
        return this.short_name

    @message("print")
    def print_(this, self):
        return chez.printf('Slipnode "~a"~%', this.short_name)

    @message("draw-activation-graphics")
    def draw_activation_graphics(this, self, slipnet_window):
        return tell(slipnet_window, "draw-activation",
                    this.graphics_coord, this.activation, this.frozen_p)

    @message("reset")
    def reset(this, self):
        this.activation = 0
        this.activation_buffer = 0
        this.frozen_p = False
        this.changed_frozen_p = False
        this.rate_of_decay = one_minus(chez.expt(percent(this.conceptual_depth),
                                                 chez.div(_metacat.run.p_update_cycle_length, 15)))
        return "done"

    @message("get-graphics-coord")
    def get_graphics_coord(this, self):
        return this.graphics_coord

    @message("get-graphics-label-coord")
    def get_graphics_label_coord(this, self):
        return this.graphics_label_coord

    @message("set-graphics-coord")
    def set_graphics_coord(this, self, coord):
        this.graphics_coord = coord
        return "done"

    @message("set-graphics-label-coord")
    def set_graphics_label_coord(this, self, coord):
        this.graphics_label_coord = coord
        return "done"

    @message("get-conceptual-depth")
    def get_conceptual_depth(this, self):
        return this.conceptual_depth

    @message("frozen?")
    def frozen_p_(this, self):
        return this.frozen_p

    @message("get-activation")
    def get_activation(this, self):
        return this.activation

    @message("get-intrinsic-link-length")
    def get_intrinsic_link_length(this, self):
        return this.intrinsic_link_length

    @message("get-shrunk-link-length")
    def get_shrunk_link_length(this, self):
        return this.shrunk_link_length

    @message("get-incoming-links")
    def get_incoming_links(this, self):
        return this.incoming_links

    @message("get-category-links")
    def get_category_links(this, self):
        return this.category_links

    @message("get-lateral-links")
    def get_lateral_links(this, self):
        return this.lateral_links

    @message("get-lateral-sliplinks")
    def get_lateral_sliplinks(this, self):
        return this.lateral_sliplinks

    @message("get-property-links")
    def get_property_links(this, self):
        return this.property_links

    @message("get-links-labeled-by-node")
    def get_links_labeled_by_node(this, self):
        return this.links_labeled_by_node

    @message("get-degree-of-assoc")
    def get_degree_of_assoc(this, self):
        return hundred_minus(this.shrunk_link_length if fully_active_p(self)
                             else this.intrinsic_link_length)

    @message("category?")
    def category_p(this, self):
        return len(this.instance_links) != 0

    @message("instance?")
    def instance_p(this, self):
        return len(this.category_links) != 0

    @message("get-category")
    def get_category(this, self):
        if len(this.category_links) == 0:
            return False
        return tell(first(this.category_links), "get-to-node")

    @message("get-outgoing-links")
    def get_outgoing_links(this, self):
        return (this.category_links + this.instance_links + this.property_links
                + this.lateral_links + this.lateral_sliplinks)

    @message("get-instance-nodes")
    def get_instance_nodes(this, self):
        return tell_all(this.instance_links, "get-to-node")

    @message("get-similar-property-links")
    def get_similar_property_links(this, self):
        return filter_(lambda link: prob_p(_metacat.formulas.temp_adjusted_probability(
                           percent(tell(link, "get-degree-of-assoc")))),
                       this.property_links)

    @message("get-related-node")
    def get_related_node(this, self, relation):
        if relation is plato_identity:
            return self
        related_nodes = tell_all(
            filter_(lambda link: tell(link, "get-label-node") is relation,
                    tell(self, "get-outgoing-links")),
            "get-to-node")
        if len(related_nodes) == 0:
            return False
        if len(related_nodes) == 1:
            return first(related_nodes)
        return select(lambda node: tell(node, "get-category") is tell(self, "get-category"),
                      related_nodes)

    @message("freeze")
    def freeze(this, self):
        this.frozen_p = True
        this.changed_frozen_p = True
        return "done"

    @message("unfreeze")
    def unfreeze(this, self):
        this.frozen_p = False
        this.changed_frozen_p = True
        return "done"

    @message("clamp")
    def clamp(this, self, new_value):
        _metacat.trace.monitor_slipnode_activation_change(self, this.activation, new_value)
        this.activation = new_value
        this.activation_buffer = 0
        this.frozen_p = True
        this.changed_frozen_p = True
        return "done"

    @message("set-activation")
    def set_activation(this, self, new_value):
        if this.frozen_p is False:
            this.activation = new_value
            this.activation_buffer = 0
        return "done"

    @message("update-activation")
    def update_activation(this, self, new_value):
        if this.frozen_p is False:
            _metacat.trace.monitor_slipnode_activation_change(self, this.activation, new_value)
            this.activation = new_value
            this.activation_buffer = 0
        return "done"

    @message("increment-activation-buffer")
    def increment_activation_buffer(this, self, delta):
        if this.frozen_p is False:
            this.activation_buffer = chez.add(this.activation_buffer, delta)
        return "done"

    @message("decrement-activation-buffer")
    def decrement_activation_buffer(this, self, delta):
        if this.frozen_p is False:
            this.activation_buffer = chez.sub(this.activation_buffer, delta)
        return "done"

    @message("flush-activation-buffer")
    def flush_activation_buffer(this, self):
        new_value = chez.min_(p_max_activation, chez.add(this.activation, this.activation_buffer))
        _metacat.trace.monitor_slipnode_activation_change(self, this.activation, new_value)
        this.activation = new_value
        this.activation_buffer = 0
        return "done"

    @message("activate-from-workspace")
    def activate_from_workspace(this, self):
        tell(self, "increment-activation-buffer", p_workspace_activation)
        return "done"

    @message("decay-activation")
    def decay_activation(this, self):
        decay_amount = round_(chez.mul(this.rate_of_decay, this.activation))
        tell(self, "decrement-activation-buffer", decay_amount)
        return "done"

    @message("spread-activation")
    def spread_activation(this, self):
        for link in tell(self, "get-outgoing-links"):
            to_node = tell(link, "get-to-node")
            association = tell(link, "get-intrinsic-degree-of-assoc")
            # chez: (* a b c) multiplies left to right
            spread_amount = round_(chez.mul(chez.mul(chez.div(_metacat.run.p_update_cycle_length, 15),
                                                     percent(association)),
                                            this.activation))
            tell(to_node, "increment-activation-buffer", spread_amount)
        return "done"

    @message("set-intrinsic-link-length")
    def set_intrinsic_link_length(this, self, new_value):
        this.intrinsic_link_length = new_value
        this.shrunk_link_length = round_(percent_40(new_value))
        return "done"

    @message("define-descriptor-predicate")
    def define_descriptor_predicate(this, self, new_procedure):
        this.descriptor_predicate_p = new_procedure
        return "done"

    @message("possible-descriptor?")
    def possible_descriptor_p(this, self, object_):
        return this.descriptor_predicate_p(object_)

    @message("description-possible?")
    def description_possible_p(this, self, object_):
        return len(tell(self, "get-possible-descriptors", object_)) != 0

    @message("get-possible-descriptors")
    def get_possible_descriptors(this, self, object_):
        return filter_meth(tell_all(this.instance_links, "get-to-node"),
                           "possible-descriptor?", object_)

    @message("set-top-down-codelet-types")
    def set_top_down_codelet_types(this, self, *codelet_types):
        this.top_down_codelet_types = list(codelet_types)
        return "done"

    @message("attempt-to-post-top-down-codelets")
    def attempt_to_post_top_down_codelets(this, self):
        if above_threshold_p(self):
            urgency = chez.mul(percent(this.conceptual_depth), this.activation)
            for codelet_type in this.top_down_codelet_types:
                # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
                coin_flip = chez.random(1.0)
                if coin_flip < coderack.post_codelet_probability(codelet_type):
                    for _ in range(coderack.num_of_codelets_to_post(codelet_type)):
                        # Scope for top-down codelets posted by
                        # active slipnodes is entire workspace:
                        tell(coderack.g_coderack, "add-deferred-codelet",
                             tell(codelet_type, "make-codelet",
                                  urgency, self, _metacat.workspace.g_workspace))
        return "done"

    @message("add-to-incoming-links")
    def add_to_incoming_links(this, self, new_link):
        this.incoming_links = [new_link] + this.incoming_links
        return "done"

    @message("add-to-outgoing-links")
    def add_to_outgoing_links(this, self, link_type, new_link):
        if link_type == "category":
            this.category_links = [new_link] + this.category_links
        elif link_type == "instance":
            this.instance_links = [new_link] + this.instance_links
        elif link_type == "property":
            this.property_links = [new_link] + this.property_links
        elif link_type == "lateral":
            this.lateral_links = [new_link] + this.lateral_links
        elif link_type == "lateral-sliplink":
            this.lateral_sliplinks = [new_link] + this.lateral_sliplinks
        return "done"

    @message("add-to-links-labeled-by-node")
    def add_to_links_labeled_by_node(this, self, link):
        this.links_labeled_by_node = [link] + this.links_labeled_by_node
        return "done"

    # slippage <desc1>==<label>==><desc2> is applicable to <node> iff:
    #
    # (1) <desc1> = <node>
    #   In this case the slipped node is <desc2>
    #
    # (2) <node> has a sliplink with label
    #   In this case the slipped node is the node related to <node> by <label>
    #   (probability is a function of sliplink's current degree of association)
    # Example:   a => b
    #            |
    #            z => ?
    # first=(opp)=>last slippage applied to <successor> node
    # should sometimes cause a slippage to <predecessor>.

    @message("apply-slippages")
    def apply_slippages(this, self, slippages, sliplog):
        if len(slippages) == 0:
            return self
        if tell(first(slippages), "get-descriptor1") is self:
            tell(sliplog, "applied", first(slippages))
            return tell(first(slippages), "get-descriptor2")
        # See if a coattail slippage can be made:
        label = tell(first(slippages), "get-label")
        if (not exists_p(label)
                or tell(first(slippages), "get-CM-type") is tell(self, "get-category")):
            return tell(self, "apply-slippages", rest(slippages), sliplog)
        sliplink = select_meth(this.lateral_sliplinks, "labeled?", label)
        if (exists_p(sliplink)
                and prob_p(coattail_slippage_probability(first(slippages), label, self, sliplink))):
            node2 = tell(self, "get-related-node", label)
            tell(sliplog, "coattail", self, label, node2, first(slippages))
            return node2
        return tell(self, "apply-slippages", rest(slippages), sliplog)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_slipnode(name_symbol, short_name, conceptual_depth):
    """slipnet.ss: make-slipnode"""
    return Slipnode(name_symbol, short_name, conceptual_depth)


def coattail_slippage_probability(inducing_slippage, inducing_slippage_label, node, sliplink):
    """slipnet.ss: coattail-slippage-probability"""
    return percent(tell(sliplink, "get-degree-of-assoc"))


def get_label(from_node, to_node):
    """slipnet.ss: get-label"""
    if from_node is to_node:
        return plato_identity
    link = select(lambda link: tell(link, "get-from-node") is from_node,
                  tell(to_node, "get-incoming-links"))
    if exists_p(link):
        return tell(link, "get-label-node")
    return False


def relationship_between(nodes):
    """slipnet.ss: relationship-between.  1.2: an error for fewer than two
    nodes (anomalies: "Latent errors: relationship-between of fewer than two
    nodes"); here first([]) raises IndexError."""
    if all_exist_p(nodes):
        relations = adjacency_map(get_label, nodes)
        if all_exist_p(relations) and all_same_p(relations):
            return first(relations)
        return False
    return False


class SlipnetLink(SchemeObject):
    """slipnet.ss: make-slipnet-link (the closure)"""
    __slots__ = ("from_node", "to_node", "link_type", "label_node", "fixed_length_p",
                 "link_length", "print_name_")

    def __init__(this, from_node, to_node, link_type):
        this.from_node = from_node
        this.to_node = to_node
        this.link_type = link_type
        this.label_node = False
        this.fixed_length_p = False
        this.link_length = 0
        this.print_name_ = String(chez.format_("~a-->~a",
                                               tell(from_node, "get-lowercase-name"),
                                               tell(to_node, "get-lowercase-name")))

    @message("object-type")
    def object_type(this, self):
        return "slipnet-link"

    @message("print-name")
    def print_name(this, self):
        return this.print_name_

    @message("print")
    def print_(this, self):
        return chez.printf("Sliplink ~a~n", this.print_name_)

    @message("get-link-type")
    def get_link_type(this, self):
        return this.link_type

    @message("get-from-node")
    def get_from_node(this, self):
        return this.from_node

    @message("get-to-node")
    def get_to_node(this, self):
        return this.to_node

    @message("get-label-node")
    def get_label_node(this, self):
        return this.label_node

    @message("get-link-length")
    def get_link_length(this, self):
        return this.link_length

    @message("get-intrinsic-degree-of-assoc")
    def get_intrinsic_degree_of_assoc(this, self):
        if this.fixed_length_p is not False:
            return hundred_minus(this.link_length)
        return hundred_minus(tell(this.label_node, "get-intrinsic-link-length"))

    @message("get-degree-of-assoc")
    def get_degree_of_assoc(this, self):
        if this.fixed_length_p is not False:
            return hundred_minus(this.link_length)
        if fully_active_p(this.label_node):
            return hundred_minus(tell(this.label_node, "get-shrunk-link-length"))
        return hundred_minus(tell(this.label_node, "get-intrinsic-link-length"))

    @message("labeled?")
    def labeled_p(this, self, node):
        return this.label_node is node

    @message("set-label-node")
    def set_label_node(this, self, node):
        this.label_node = node
        tell(this.label_node, "add-to-links-labeled-by-node", self)
        return "done"

    @message("set-link-length")
    def set_link_length(this, self, new_length):
        this.link_length = new_length
        this.fixed_length_p = True
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_slipnet_link(from_node, to_node, link_type):
    """slipnet.ss: make-slipnet-link"""
    return SlipnetLink(from_node, to_node, link_type)


def related_p(node1, node2):
    """slipnet.ss: related?"""
    return node1 is node2 or linked_p(node1, node2)


def linked_p(node1, node2):
    """slipnet.ss: linked?"""
    return member_p(node2, tell_all(tell(node1, "get-outgoing-links"), "get-to-node"))


def slip_linked_p(node1, node2):
    """slipnet.ss: slip-linked?"""
    return member_p(node2, tell_all(tell(node1, "get-lateral-sliplinks"), "get-to-node"))


def establish_link(top_level_name, from_node, to_node, link_type):
    """slipnet.ss: establish-link"""
    new_link = make_slipnet_link(from_node, to_node, link_type)
    chez.define_top_level_value(top_level_name, new_link)
    tell(from_node, "add-to-outgoing-links", link_type, new_link)
    tell(to_node, "add-to-incoming-links", new_link)
    return new_link


def update_slipnet_activations():
    """slipnet.ss: update-slipnet-activations"""
    for theme in tell(_metacat.themes.g_themespace, "get-all-active-themes"):
        tell(theme, "spread-activation-to-slipnet")
    for node in g_slipnet_nodes:
        tell(node, "decay-activation")
    for node in filter_(fully_active_p, g_slipnet_nodes):
        tell(node, "spread-activation")
    for node in g_slipnet_nodes:
        tell(node, "flush-activation-buffer")
    for node in filter_(partially_active_p, g_slipnet_nodes):
        # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
        coin_flip = chez.random(1.0)
        if coin_flip < cube(percent(tell(node, "get-activation"))):
            tell(node, "update-activation", p_max_activation)


def fully_active_p(node):
    """slipnet.ss: fully-active?"""
    return tell(node, "get-activation") == p_max_activation


def above_threshold_p(node):
    """slipnet.ss: above-threshold?"""
    return tell(node, "get-activation") >= p_full_activation_threshold


def partially_active_p(node):
    """slipnet.ss: partially-active?"""
    return above_threshold_p(node) and not fully_active_p(node)


# (plato-name short-name conceptual-depth), in slipnet.ss's order
SLIPNODE_SPECS = [(name, String(short), depth) for name, short, depth in [
    ("plato-a", "a", 10), ("plato-b", "b", 10), ("plato-c", "c", 10), ("plato-d", "d", 10),
    ("plato-e", "e", 10), ("plato-f", "f", 10), ("plato-g", "g", 10), ("plato-h", "h", 10),
    ("plato-i", "i", 10), ("plato-j", "j", 10), ("plato-k", "k", 10), ("plato-l", "l", 10),
    ("plato-m", "m", 10), ("plato-n", "n", 10), ("plato-o", "o", 10), ("plato-p", "p", 10),
    ("plato-q", "q", 10), ("plato-r", "r", 10), ("plato-s", "s", 10), ("plato-t", "t", 10),
    ("plato-u", "u", 10), ("plato-v", "v", 10), ("plato-w", "w", 10), ("plato-x", "x", 10),
    ("plato-y", "y", 10), ("plato-z", "z", 10),
    ("plato-one", "one", 30), ("plato-two", "two", 30), ("plato-three", "three", 30),
    ("plato-four", "four", 30), ("plato-five", "five", 30),
    ("plato-leftmost", "lmost", 40), ("plato-rightmost", "rmost", 40),
    ("plato-middle", "middle", 40), ("plato-single", "single", 40), ("plato-whole", "whole", 40),
    ("plato-alphabetic-first", "first", 60), ("plato-alphabetic-last", "last", 60),
    ("plato-left", "left", 40), ("plato-right", "right", 40),
    ("plato-predecessor", "pred", 50), ("plato-successor", "succ", 50),
    ("plato-sameness", "same", 80),
    ("plato-predgrp", "predgrp", 50), ("plato-succgrp", "succgrp", 50),
    ("plato-samegrp", "samegrp", 80),
    ("plato-identity", "Identity", 90), ("plato-opposite", "Opposite", 90),
    ("plato-letter", "letter", 20), ("plato-group", "group", 80),
    ("plato-letter-category", "LetterCtgy", 30),
    ("plato-string-position-category", "StringPos", 70),
    ("plato-alphabetic-position-category", "AlphaPos", 80),
    ("plato-direction-category", "Direction", 70),
    ("plato-bond-category", "BondCtgy", 80),
    ("plato-group-category", "GroupCtgy", 80),
    ("plato-length", "Length", 60),
    ("plato-object-category", "ObjectCtgy", 90),
    ("plato-bond-facet", "BondFacet", 90)]]


def platonic_letter_p(node):
    """slipnet.ss: platonic-letter?"""
    return member_p(node, g_slipnet_letters)


def platonic_number_p(node):
    """slipnet.ss: platonic-number?"""
    return member_p(node, g_slipnet_numbers)


def number_to_platonic_number(n):
    """slipnet.ss: number->platonic-number"""
    if n > len(g_slipnet_numbers):
        return False
    if n < 1:
        # chez: list-tail of a negative index is an error (Python's l[-1] is not)
        raise chez.SchemeError("list-tail", "index ~s is out of range", n - 1)
    return nth(n - 1, g_slipnet_numbers)


def platonic_number_to_number(node):
    """slipnet.ss: platonic-number->number"""
    return chez.add1(list_index(g_slipnet_numbers, node))


def platonic_relation_p(node):
    """slipnet.ss: platonic-relation?"""
    return (node is plato_identity
            or node is plato_opposite
            or node is plato_predecessor
            or node is plato_successor)


platonic_literal_p = compose(lambda x: x is False, platonic_relation_p)


def inverse(node):
    """slipnet.ss: inverse"""
    if node is plato_identity:
        return plato_identity
    if exists_p(node):
        return tell(node, "get-related-node", plato_opposite)
    return False


# Descriptor predicates (slipnet.ss's define-descriptor-predicate forms)

def _group_of_length(n):
    return lambda object_: group_p(object_) and tell(object_, "get-group-length") == n


def _leftmost_p(object_):
    if tell(object_, "string-spanning-group?") is not False:
        return False
    return tell(object_, "leftmost-in-string?")


def _rightmost_p(object_):
    if tell(object_, "string-spanning-group?") is not False:
        return False
    return tell(object_, "rightmost-in-string?")


def _middle_p(object_):
    return tell(object_, "middle-in-string?")


def _single_p(object_):
    return letter_p(object_) and tell(object_, "spans-whole-string?")


def _whole_p(object_):
    return tell(object_, "string-spanning-group?")


def _alphabetic_first_p(object_):
    return tell(object_, "get-descriptor-for", plato_letter_category) is plato_a


def _alphabetic_last_p(object_):
    return tell(object_, "get-descriptor-for", plato_letter_category) is plato_z


def _links():
    """slipnet.ss: the link definitions, in the file's order."""
    s = sugar
    letters = "a b c d e f g h i j k l m n o p q r s t u v w x y z".split()

    # SUCCESSOR and PREDECESSOR links
    for n1, n2 in zip(letters, letters[1:]):
        s.lateral_link_star(n1, n2, label="successor")
    for n1, n2 in zip(letters[::-1], letters[::-1][1:]):
        s.lateral_link_star(n1, n2, label="predecessor")
    numbers = ["one", "two", "three", "four", "five"]
    for n1, n2 in zip(numbers, numbers[1:]):
        s.lateral_link_star(n1, n2, label="successor")
    for n1, n2 in zip(numbers[::-1], numbers[::-1][1:]):
        s.lateral_link_star(n1, n2, label="predecessor")

    # LETTER-CATEGORY links
    s.instance_links_star("letter-category", letters, 97)
    s.category_links_star(letters, "letter-category", 0)
    letter_category_depth = cd(plato_letter_category)
    for letter in g_slipnet_letters:
        tell(first(tell(letter, "get-category-links")),
             "set-link-length", chez.sub(letter_category_depth, cd(letter)))
    s.lateral_link_star("samegrp", "letter-category", length=50)

    # LENGTH links
    s.instance_links_star("length", numbers, 100)
    s.category_links_star(numbers, "length", 0)
    length_depth = cd(plato_length)
    for number in g_slipnet_numbers:
        tell(first(tell(number, "get-category-links")),
             "set-link-length", chez.sub(length_depth, cd(number)))
    s.lateral_link_star("predgrp", "length", length=95)
    s.lateral_link_star("succgrp", "length", length=95)
    s.lateral_link_star("samegrp", "length", length=95)

    # OPPOSITE links
    s.lateral_sliplink_star("alphabetic-first", "alphabetic-last", label="opposite", two_way=True)
    s.lateral_sliplink_star("leftmost", "rightmost", label="opposite", two_way=True)
    s.lateral_sliplink_star("left", "right", label="opposite", two_way=True)
    s.lateral_sliplink_star("successor", "predecessor", label="opposite", two_way=True)
    s.lateral_sliplink_star("predgrp", "succgrp", label="opposite", two_way=True)

    # PROPERTY links
    s.property_link_star("a", "alphabetic-first", 75)
    s.property_link_star("z", "alphabetic-last", 75)

    def category_pair(category, instance):
        """(instance-link* category --> instance length: 100) and
        (category-link* instance --> category length: (- (cd category) (cd instance)))"""
        s.instance_link_star(category, instance, 100)
        s.category_link_star(instance, category,
                             chez.sub(cd(chez.top_level_value("plato-" + category)),
                                      cd(chez.top_level_value("plato-" + instance))))

    # OBJECT-CATEGORY links
    category_pair("object-category", "letter")
    category_pair("object-category", "group")

    # STRING-POSITION-CATEGORY links
    for instance in ["leftmost", "rightmost", "middle", "single", "whole"]:
        category_pair("string-position-category", instance)

    # ALPHABETIC-POSITION-CATEGORY links
    category_pair("alphabetic-position-category", "alphabetic-first")
    category_pair("alphabetic-position-category", "alphabetic-last")

    # DIRECTION-CATEGORY links
    category_pair("direction-category", "left")
    category_pair("direction-category", "right")

    # BOND-CATEGORY links
    for instance in ["predecessor", "successor", "sameness"]:
        category_pair("bond-category", instance)

    # GROUP-CATEGORY links
    for instance in ["predgrp", "succgrp", "samegrp"]:
        category_pair("group-category", instance)

    # ASSOCIATED GROUP links
    s.lateral_link_star("sameness", "samegrp", length=30, label="group-category")
    s.lateral_link_star("successor", "succgrp", length=60, label="group-category")
    s.lateral_link_star("predecessor", "predgrp", length=60, label="group-category")

    # ASSOCIATED BOND-CATEGORY links
    s.lateral_link_star("samegrp", "sameness", length=90, label="bond-category")
    s.lateral_link_star("succgrp", "successor", length=90, label="bond-category")
    s.lateral_link_star("predgrp", "predecessor", length=90, label="bond-category")

    # BOND-FACET links
    category_pair("bond-facet", "letter-category")
    category_pair("bond-facet", "length")

    # LETTER-CATEGORY-LENGTH links
    s.lateral_sliplink_star("letter-category", "length", length=95, two_way=True)

    # LETTER-GROUP links
    s.lateral_sliplink_star("letter", "group", length=90, two_way=True)

    # DIRECTION-POSITION, DIRECTION-NEIGHBOR, and POSITION-NEIGHBOR links
    # (see slipnet.ss for Marshall's note on first=>first and lmost=>rmost)
    s.lateral_link_star("leftmost", "left", length=90, label="identity", two_way=True)
    s.lateral_link_star("leftmost", "right", length=100, label="opposite", two_way=True)
    s.lateral_link_star("rightmost", "left", length=100, label="opposite", two_way=True)
    s.lateral_link_star("rightmost", "right", length=90, label="identity", two_way=True)
    s.lateral_link_star("alphabetic-first", "leftmost", length=100, two_way=True)
    s.lateral_link_star("alphabetic-first", "rightmost", length=100, two_way=True)
    s.lateral_link_star("alphabetic-last", "leftmost", length=100, two_way=True)
    s.lateral_link_star("alphabetic-last", "rightmost", length=100, two_way=True)

    # OTHER LINKS
    s.lateral_sliplink_star("single", "whole", length=90, two_way=True)


def load():
    """slipnet.ss: the top-level forms after the procedures, in the file's order:
    *slipnet-nodes* (each node also a module attribute and top-level value), the
    letter, number, top-down and initially clamped node lists, the top-down
    codelet types, intrinsic link lengths, descriptor predicates and links."""
    global g_slipnet_nodes, g_slipnet_letters, g_slipnet_numbers
    global g_top_down_slipnodes, g_initially_clamped_slipnodes
    this = sys.modules[__name__]
    g_slipnet_nodes = sugar.slipnet_node_list_star(SLIPNODE_SPECS, module=this)
    g_slipnet_letters = [plato_a, plato_b, plato_c, plato_d, plato_e, plato_f, plato_g,
                         plato_h, plato_i, plato_j, plato_k, plato_l, plato_m, plato_n,
                         plato_o, plato_p, plato_q, plato_r, plato_s, plato_t, plato_u,
                         plato_v, plato_w, plato_x, plato_y, plato_z]
    g_slipnet_numbers = [plato_one, plato_two, plato_three, plato_four, plato_five]
    g_top_down_slipnodes = [plato_left, plato_right, plato_predecessor, plato_successor,
                            plato_sameness, plato_predgrp, plato_succgrp, plato_samegrp,
                            plato_string_position_category,
                            plato_alphabetic_position_category, plato_length]
    g_initially_clamped_slipnodes = [plato_letter_category, plato_string_position_category]

    # Attach top-down codelet-types to top-down slipnodes.  This can only
    # be done after *codelet-types* has been defined.
    for node in [plato_left, plato_right]:
        tell(node, "set-top-down-codelet-types",
             coderack.top_down_bond_scout__direction, coderack.top_down_group_scout__direction)
    for node in [plato_predecessor, plato_successor, plato_sameness]:
        tell(node, "set-top-down-codelet-types", coderack.top_down_bond_scout__category)
    for node in [plato_predgrp, plato_succgrp, plato_samegrp]:
        tell(node, "set-top-down-codelet-types", coderack.top_down_group_scout__category)
    for node in [plato_string_position_category, plato_alphabetic_position_category,
                 plato_length]:
        tell(node, "set-top-down-codelet-types", coderack.top_down_description_scout)

    # Set intrinsic-link-length values for the slipnodes
    # that serve as label nodes for certain slipnet links.
    tell(plato_predecessor, "set-intrinsic-link-length", 60)
    tell(plato_successor, "set-intrinsic-link-length", 60)
    tell(plato_sameness, "set-intrinsic-link-length", 0)
    tell(plato_identity, "set-intrinsic-link-length", 0)
    tell(plato_opposite, "set-intrinsic-link-length", 80)

    # Define possible-descriptor? predicate for slipnodes
    # that can be used as descriptors for objects.
    tell(plato_one, "define-descriptor-predicate", _group_of_length(1))
    tell(plato_two, "define-descriptor-predicate", _group_of_length(2))
    tell(plato_three, "define-descriptor-predicate", _group_of_length(3))
    tell(plato_four, "define-descriptor-predicate", _group_of_length(4))
    tell(plato_five, "define-descriptor-predicate", _group_of_length(5))
    tell(plato_leftmost, "define-descriptor-predicate", _leftmost_p)
    tell(plato_rightmost, "define-descriptor-predicate", _rightmost_p)
    tell(plato_middle, "define-descriptor-predicate", _middle_p)
    tell(plato_single, "define-descriptor-predicate", _single_p)
    tell(plato_whole, "define-descriptor-predicate", _whole_p)
    tell(plato_alphabetic_first, "define-descriptor-predicate", _alphabetic_first_p)
    tell(plato_alphabetic_last, "define-descriptor-predicate", _alphabetic_last_p)
    tell(plato_letter, "define-descriptor-predicate", letter_p)
    tell(plato_group, "define-descriptor-predicate", group_p)

    _links()
