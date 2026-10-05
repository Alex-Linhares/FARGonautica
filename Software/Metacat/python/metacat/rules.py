"""rules.ss: rules, the rule codelets, rule abstraction, rule application, rule
quality and the English transcription of rules.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from rules.ss, with
racket/engine/rules.rktl as a worked translation.

make-rule's closure is the Rule class, which delegates what it doesn't answer to
a WorkspaceStructure; make-extrinsic-change-description's and
make-intrinsic-change-description's closures are ExtrinsicChangeDescription and
IntrinsicChangeDescription, which delegate to base-object
(docs/python-translation-plan.md, "Objects").  The three codelet procedures are
given to their codelet types by load() (define-codelet-procedure* needs the
types, which coderack.load makes).  load() also makes the top-level values that
need the slipnet's nodes (*rule-dimension-order* and new-object-phrase's list of
"an" letters), and registers format-slipnode as a Chez top-level value, which
utilities.reveal_obj reads (anomalies_and_quirks.md, "Verbose mode reached an
unregistered format-slipnode").

Names from files translated later are read through the package at call time:
themes.ss (*themespace*), trace.ss (monitor-new-rules), general-graphics.ss
(find-next-space-position, called by every make-rule through
transcribe-to-english: anomalies_and_quirks.md, "Rules and answers lean on later
files and on a REPL abbreviation") and rule-graphics.ss (initialize-rule-graphics;
only with %workspace-graphics% on, as the original gates it).  *workspace* and
the four strings are workspace's, %justify-mode% and %workspace-graphics% setup's.

Scheme strings: every English phrase, every format and string-append result and
every string literal that is returned as a value is a chez.String, so that
b:canon and get-concept-pattern's (filter-out symbol? ...) (rules.ss:269) tell
them from symbols (anomalies_and_quirks.md, "The graphics and rules.ss tell
strings from symbols").  Arithmetic stays exact unless a flonum enters (the
0.01 and 0.75 probabilities, exp of an inexact uniformity, temp-adjusted
values); rounding is utilities.round_ (through 100*).  The engine never imports
tkinter.

Evaluation order (audited against Chez's; rules.rktl has no port: changes): every
draw (rule-scout's stochastic-if* and random-picks, abstract-change-descriptions'
random partition size, bounded-random-partition, stochastic-if* coins and prob?
calls, choose-description-for-rule and the stochastic-pick of
instantiate-change-template) sits in a let*, a body, an and/or or a map, so
Python's order is Chez's.  Every stochastic-if* draws its coin first.  The
multi-argument calls and multi-binding lets (the lists built by
instantiate-rule-clause-template, the appends of remove-redundant-change-
descriptions and get-changes-implied-by-swap, the lets of
make-intrinsic-change-description, transform-image and get-dimension-transforms,
the arguments of all-possible-bridge-CMs and of the remaining-swaps
add-extrinsic-change-description, whose only draw is its last argument) have at
most one subexpression with effects, so their order is not observable.  No site
needed reordering.  maps with effects (draws, attach-length-description, escapes)
go through chez.map_ (tell-all included); sorts through chez.sort.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez, sugar
from metacat import (bridges, coderack, formulas, groups, images, setup, slipnet, workspace,
                     workspace_objects, workspace_structures)
from metacat.objects import SchemeObject, base_object, delegate, message, tell
from metacat.sugar import say
from metacat.utilities import (all_but_last, all_exist_p, all_same_p, average, bounded_random_partition,
                               ceiling, compose, compress, count, count_meth, cross_product, cube,
                               exists_p, filter_, filter_map, filter_meth, filter_out,
                               filter_out_meth, first, flatmap, flatten, fourth, group_p, last,
                               letter_p, list_index, map_compress, maximum, member_p, one_minus,
                               ormap_meth, pairwise_andmap, pairwise_map, partition, percent,
                               prob_p, product, random_pick, remove_duplicates, remq_duplicates,
                               remq_elements, rest, second, select, sets_equal_p, sigmoid,
                               sort_by_method, sort_wrt_order, square, stochastic_pick,
                               round_, string_suffix, sum_, tell_all, third, times_100,
                               weighted_average, workspace_string_p)


def _lt(a, b):
    return a < b


def _format(control, *args):
    """(format control arg ...) as a Scheme string."""
    return chez.String(chez.format_(control, *args))


def _symbol_p(x):
    """Chez: symbol?  (a str that is not a Scheme string or character)"""
    return isinstance(x, str) and not isinstance(x, (chez.String, chez.Char))


def _caddr(x):
    """Chez: caddr (utilities.ss's 3rd), which raises on #f and short lists."""
    if (type(x) is list or type(x) is tuple) and len(x) >= 3:
        return x[2]
    raise chez.SchemeError("caddr", "incorrect list structure ~s", x)


def _cadr(x):
    """Chez: cadr (utilities.ss's 2nd), which raises on #f and short lists."""
    if (type(x) is list or type(x) is tuple) and len(x) >= 2:
        return x[1]
    raise chez.SchemeError("cadr", "incorrect list structure ~s", x)


# NOTE: in the code here, I use the tag symbols 'self and 'subobjects instead
# of 'object and 'components as described in section 3.3.4 of my PhD thesis (for
# example, in Figures 3.2 through 3.4).  I originally developed the code using
# the self/subobjects notation, but never got around to changing the names to
# object/components, even though I think object/components is clearer.
#
# Likewise, there are several other notational differences between the grammar
# shown in Figure 3.2 and the code here (for example, a <change-description> in
# Figure 3.2 is called a <change> here).  However, all of these differences are
# purely syntactic.  The structure of rules is exactly the same.  One of these
# days I'll update the code to reflect the notation used in Chapter 3.

# ------------------------------------- Rules ----------------------------------------

# <intrinsic-rule-clause-template> ::=
#    (intrinsic <ref-object> (<change-template> ...))
# <extrinsic-rule-clause-template> ::=
#    (extrinsic (<ref-object> ...) (<dimension> ...))
#
# Normally, the denoted objects of an extrinsic rule-clause-template are
# just the reference objects themselves, but if the template has exactly one
# <ref-object>, then the denoted objects are the _subobjects_ of <ref-object>
#
# <intrinsic-rule-clause> ::=
#    (intrinsic (<object-description>) (<change> ...))
# <extrinsic-rule-clause> ::=
#    (extrinsic (<object-description> ...) (<dimension> ...))
# <verbatim-rule-clause> ::=
#    (verbatim (<letter-category> ...))
#
# <scope> ::= self | subobjects
# <change-template> ::= (<scope> <dimension> (<descriptor> ...))
# <change> ::= (<scope> <dimension> <descriptor>)
# <object-description> ::= (<object-type> <object-desc-type> <object-descriptor>)
#                        | (string <StrPosCtgy> <whole>)
#
# Example:
#   intrinsic-rule-clause-template:
#      (intrinsic [abc] ((self <DirCtgy> (<opp> <left>))
#                        (subobjects <Length> (<succ> <two>))
#                        (subobjects <ObjCtgy> (<group>))))
#   intrinsic-rule-clause:
#      (intrinsic (<group> <StrPosCtgy> <whole>) ((self <DirCtgy> <opp>)
#                                                 (subobjects <Length> <two>)
#                                                 (subobjects <ObjCtgy> <group>)))

def rule_characterization(rule):
    """rules.ss: rule-characterization"""
    if tell(rule, "verbatim?") is not False:
        characterization = "verbatim"
    elif tell(rule, "literal?") is not False:
        characterization = "literal"
    elif tell(rule, "abstract?") is not False:
        characterization = "abstract"
    else:
        characterization = None     # cond without else: void
    return [tell(rule, "get-rule-type"), characterization]


class Rule(SchemeObject):
    """rules.ss: make-rule (the closure)"""
    __slots__ = ("rule_type", "rule_clauses", "workspace_structure", "intrinsic_rule_clauses",
                 "extrinsic_rule_clauses", "bridge_theme_type", "rule_clause_templates",
                 "theme_pattern", "tagged_supporting_horizontal_bridges",
                 "supporting_horizontal_bridges", "translated_p", "original_rule",
                 "translation_direction", "uniformity", "abstractness", "succinctness",
                 "quality", "intrinsic_quality", "english_transcription",
                 "clamped_graphics_pexp", "rule_graphics_center_coord", "rule_graphics_height")

    def __init__(this, rule_type, rule_clauses):
        this.rule_type = rule_type
        this.rule_clauses = rule_clauses
        # the let*, in order
        this.workspace_structure = workspace_structures.make_workspace_structure()
        this.intrinsic_rule_clauses = filter_(intrinsic_clause_p, rule_clauses)
        this.extrinsic_rule_clauses = filter_(extrinsic_clause_p, rule_clauses)
        if rule_type == "top":
            this.bridge_theme_type = "top-bridge"
        elif rule_type == "bottom":
            this.bridge_theme_type = "bottom-bridge"
        else:
            this.bridge_theme_type = None   # (case ...) without else: void
        this.rule_clause_templates = False
        this.theme_pattern = False
        this.tagged_supporting_horizontal_bridges = False
        this.supporting_horizontal_bridges = False
        this.translated_p = False
        this.original_rule = False
        this.translation_direction = False
        this.uniformity = 0
        this.abstractness = 0
        this.succinctness = 0
        this.quality = 0
        # temporary. leave this for now:
        this.intrinsic_quality = 0
        this.english_transcription = transcribe_to_english(rule_type, rule_clauses)
        this.clamped_graphics_pexp = False
        this.rule_graphics_center_coord = False
        this.rule_graphics_height = False

    @message("object-type")
    def object_type(this, self):
        return "rule"

    @message("get-rule-type")
    def get_rule_type(this, self):
        return this.rule_type

    @message("type?")
    def type_p(this, self, type_):
        return chez.eq_p(type_, this.rule_type)

    @message("get-clamped-graphics-pexp")
    def get_clamped_graphics_pexp(this, self):
        return this.clamped_graphics_pexp

    @message("get-rule-graphics-center-coord")
    def get_rule_graphics_center_coord(this, self):
        return this.rule_graphics_center_coord

    @message("get-rule-graphics-height")
    def get_rule_graphics_height(this, self):
        return this.rule_graphics_height

    @message("set-auxiliary-rule-graphics-info")
    def set_auxiliary_rule_graphics_info(this, self, pexp, coord, height):
        this.clamped_graphics_pexp = pexp
        this.rule_graphics_center_coord = coord
        this.rule_graphics_height = height
        return "done"

    @message("print-english")
    def print_english(this, self):
        # for* each: the value is the last printf's (void)
        result = None
        for line in this.english_transcription:
            result = chez.printf("~a~n", line)
        return result

    @message("print")
    def print_(this, self):
        tell(self, "print-english")
        # the arguments only read
        chez.printf("(Type=~a, Q=~a, RQ=~a, Uniformity=~a, Abstractness=~a, ",
                    this.rule_type, this.quality, tell(self, "get-relative-quality"),
                    this.uniformity, this.abstractness)
        return chez.printf("Succinctness=~a)~n~n", this.succinctness)

    @message("show")
    def show(this, self):
        for rc in this.rule_clauses:
            printf_rule_clause(rc)
        return chez.newline()

    @message("show-all")
    def show_all(this, self):
        chez.newline()
        for template in this.rule_clause_templates:
            printf_rule_clause_template(template)
        chez.newline()
        tell(self, "show")
        return tell(self, "print")

    @message("mark-as-translated")
    def mark_as_translated(this, self, rule, direction):
        this.original_rule = rule
        this.translation_direction = direction
        this.translated_p = True
        return "done"

    @message("get-original-rule")
    def get_original_rule(this, self):
        return this.original_rule

    @message("get-translation-direction")
    def get_translation_direction(this, self):
        return this.translation_direction

    @message("set-abstracted-rule-information")
    def set_abstracted_rule_information(this, self, templates):
        this.rule_clause_templates = templates
        # chez: map's order of application (the procedure only reads)
        this.tagged_supporting_horizontal_bridges = chez.map_(
            get_tagged_supporting_horizontal_bridges, templates)
        this.supporting_horizontal_bridges = _append_all(
            chez.map_(second, this.tagged_supporting_horizontal_bridges))
        for bridge in this.supporting_horizontal_bridges:
            if tell(bridge, "slippage-type-present?", slipnet.plato_length) is not False:
                # a two-binding let; both only read
                object1 = tell(bridge, "get-object1")
                object2 = tell(bridge, "get-object2")
                if (group_p(object1)
                        and tell(object1, "description-type-present?",
                                 slipnet.plato_length) is False):
                    groups.attach_length_description(object1)
                if (group_p(object2)
                        and tell(object2, "description-type-present?",
                                 slipnet.plato_length) is False):
                    groups.attach_length_description(object2)
        this.theme_pattern = tell(_metacat.themes.g_themespace, "get-dominant-theme-pattern",
                                  this.bridge_theme_type)
        return "done"

    @message("set-translated-rule-information")
    def set_translated_rule_information(this, self, bridges_):
        for bridge in bridges_:
            # the let*, in order
            object1 = tell(bridge, "get-object1")
            fake_object2 = tell(bridge, "get-object2")
            real_object2 = tell(workspace.g_workspace, "get-real-object", fake_object2)
            if exists_p(real_object2):
                for d in tell(real_object2, "get-descriptions"):
                    # a two-binding let; both only read
                    dtype = tell(d, "get-description-type")
                    descriptor = tell(d, "get-descriptor")
                    if tell(fake_object2, "description-type-present?", dtype) is False:
                        tell(fake_object2, "attach-description", dtype, descriptor)
                # the arguments only read
                bridge_CMs = bridges.all_possible_bridge_CMs(
                    "horizontal",
                    object1, tell(object1, "get-descriptions"),
                    real_object2, tell(real_object2, "get-descriptions"))
                tell(bridge, "set-concept-mappings", bridge_CMs)
        this.supporting_horizontal_bridges = bridges_
        # chez: map's order of application (the procedure only builds lists)
        this.tagged_supporting_horizontal_bridges = chez.map_(
            lambda bridge: [False, [bridge]], bridges_)
        this.theme_pattern = [this.bridge_theme_type] + remove_duplicates(
            _append_all(tell_all(bridges_, "get-associated-thematic-relations")))
        return "done"

    @message("set-verbatim-rule-information")
    def set_verbatim_rule_information(this, self):
        this.rule_clause_templates = []
        this.tagged_supporting_horizontal_bridges = []
        this.supporting_horizontal_bridges = []
        this.theme_pattern = [this.bridge_theme_type]
        return "done"

    @message("set-quality-values")
    def set_quality_values(this, self):
        this.uniformity = compute_rule_uniformity(self)
        this.abstractness = compute_rule_abstractness(self)
        this.succinctness = compute_rule_succinctness(self)
        # temporary:
        this.intrinsic_quality = compute_rule_intrinsic_quality(self)
        this.quality = compute_rule_quality(self)
        return "done"

    @message("get-verbatim-letter-categories")
    def get_verbatim_letter_categories(this, self):
        return second(first(this.rule_clauses))

    @message("identity?")
    def identity_p(this, self):
        return len(this.rule_clauses) == 0

    @message("verbatim?")
    def verbatim_p(this, self):
        return len(this.rule_clauses) == 1 and verbatim_clause_p(first(this.rule_clauses))

    @message("literal?")
    def literal_p(this, self):
        if tell(self, "verbatim?") is not False:
            return False
        return chez.ormap(literal_clause_p, this.rule_clauses)

    @message("abstract?")
    def abstract_p(this, self):
        return (tell(self, "verbatim?") is False
                and tell(self, "literal?") is False)

    @message("get-characterization")
    def get_characterization(this, self):
        if tell(self, "verbatim?") is not False:
            return "verbatim"
        if tell(self, "literal?") is not False:
            return "literal"
        if tell(self, "abstract?") is not False:
            return "abstract"
        return None     # cond without else: void

    @message("translated?")
    def translated_p_(this, self):
        return this.translated_p

    @message("supported?")
    def supported_p(this, self):
        return chez.andmap(lambda b: tell(workspace.g_workspace, "bridge-present?", b),
                           this.supporting_horizontal_bridges)

    @message("currently-works?")
    def currently_works_p(this, self):
        # the let*, in order
        if this.rule_type == "top":
            string1 = workspace.g_initial_string
            string2 = workspace.g_modified_string
        elif this.rule_type == "bottom":
            string1 = workspace.g_target_string
            string2 = workspace.g_answer_string
        else:
            string1 = string2 = None    # (case ...) without else: void
        result = apply_rule(self, string1, ignore_snag)
        return (exists_p(result)
                and chez.equal_p(tell(string1, "generate-image-letters"),
                                 tell(string2, "get-letter-categories")))

    @message("equal?")
    def equal_p(this, self, rule):
        return rules_equal_p(self, rule)

    @message("get-degree-of-support")
    def get_degree_of_support(this, self):
        # chez: map's order of application (the procedure only reads)
        return times_100(product(chez.map_(
            lambda b: percent(tell(tell(workspace.g_workspace, "get-equivalent-bridge", b),
                                   "get-strength")),
            this.supporting_horizontal_bridges)))

    @message("get-quality")
    def get_quality(this, self):
        return this.quality

    @message("get-relative-quality")
    def get_relative_quality(this, self):
        ranked_rules = sort_by_method("get-quality", _lt,
                                      tell(workspace.g_workspace, "get-rules", this.rule_type))
        if member_p(self, ranked_rules):
            rank = chez.add(1, list_index(ranked_rules, self))
            return times_100(chez.div(rank, len(ranked_rules)))
        return this.quality

    @message("get-uniformity")
    def get_uniformity(this, self):
        return this.uniformity

    @message("get-abstractness")
    def get_abstractness(this, self):
        return this.abstractness

    @message("get-succinctness")
    def get_succinctness(this, self):
        return this.succinctness

    # temporary:
    @message("get-intrinsic-quality")
    def get_intrinsic_quality(this, self):
        return this.intrinsic_quality

    @message("get-rule-clauses")
    def get_rule_clauses(this, self):
        return this.rule_clauses

    @message("get-intrinsic-rule-clauses")
    def get_intrinsic_rule_clauses(this, self):
        return this.intrinsic_rule_clauses

    @message("get-extrinsic-rule-clauses")
    def get_extrinsic_rule_clauses(this, self):
        return this.extrinsic_rule_clauses

    @message("get-english-transcription")
    def get_english_transcription(this, self):
        return this.english_transcription

    @message("get-rule-clause-templates")
    def get_rule_clause_templates(this, self):
        return this.rule_clause_templates

    @message("get-tagged-supporting-horizontal-bridges")
    def get_tagged_supporting_horizontal_bridges_(this, self):
        return this.tagged_supporting_horizontal_bridges

    @message("get-supporting-horizontal-bridges")
    def get_supporting_horizontal_bridges_(this, self):
        return this.supporting_horizontal_bridges

    @message("get-theme-pattern")
    def get_theme_pattern(this, self):
        return this.theme_pattern

    @message("get-concept-pattern")
    def get_concept_pattern(this, self):
        # rules.ss:269: (filter-out symbol? ...) drops the tag symbols and keeps
        # the slipnodes (a Scheme string would survive too: chez.String)
        nodes = remq_duplicates(filter_out(_symbol_p, flatten(this.rule_clauses)))
        # chez: map's order of application (the procedure only builds lists)
        return ["concepts"] + chez.map_(lambda node: [node, slipnet.p_max_activation], nodes)

    @message("revise-abstracted-rule-information")
    def revise_abstracted_rule_information(this, self, proposed_rule):
        if chez.ormap(unsupported_self_change_p, this.tagged_supporting_horizontal_bridges) is not False:
            def choose(old_bridges, new_bridges):
                if (unsupported_self_change_p(old_bridges) is not False
                        and unsupported_self_change_p(new_bridges) is False):
                    return new_bridges
                return old_bridges
            # chez: map's order of application (the procedure only reads)
            this.tagged_supporting_horizontal_bridges = chez.map_(
                choose,
                this.tagged_supporting_horizontal_bridges,
                tell(proposed_rule, "get-tagged-supporting-horizontal-bridges"))
            this.supporting_horizontal_bridges = remq_duplicates(
                _append_all(chez.map_(second, this.tagged_supporting_horizontal_bridges)))
        this.theme_pattern = tell(proposed_rule, "get-theme-pattern")
        return "done"

    @message("calculate-internal-strength")
    def calculate_internal_strength(this, self):
        return tell(self, "get-relative-quality")

    @message("calculate-external-strength")
    def calculate_external_strength(this, self):
        return tell(self, "calculate-internal-strength")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.workspace_structure)


def make_rule(rule_type, rule_clauses):
    """rules.ss: make-rule"""
    return Rule(rule_type, rule_clauses)


def _append_all(lists):
    """(apply append lists)"""
    out = []
    for part in lists:
        out.extend(part)
    return out


def unsupported_self_change_p(tagged_bridges):
    """rules.ss: unsupported-self-change? (1st)"""
    return first(tagged_bridges)


# This returns a list of the form (<boolean> (<bridge> ...)), where
# (<bridge> ...) are the supporting bridges for the rule-clause-template.
# For example, for [abc] -> [cba], a rule-clause-template with a reference
# object of [abc] and a (self <Dir> ...) change-template would be supported
# by the [abc]--[cba] bridge.  <boolean> = #t iff the rule-clause-template
# specifies a self change but a bridge doesn't exist for the reference object.
# In this case, the reference object's subobject bridges are returned.  For
# example, if no bridge exists from [abc] yet, the subobject bridges a-a, b-b,
# c-d are returned, with <boolean> =  #t.  If the [abc]--[cba] bridge is later
# created, the supporting bridges can be revised from a-a, b-b, c-d to just
# [abc]--[cba].

def get_tagged_supporting_horizontal_bridges(rule_clause_template):
    """rules.ss: get-tagged-supporting-horizontal-bridges"""
    tag = rule_clause_template[0]
    if tag == "extrinsic":
        ref_objects = rule_clause_template[1]
        if len(ref_objects) == 1:
            return [False, tell(first(ref_objects), "get-subobject-bridges", "horizontal")]
        return [False, tell_all(ref_objects, "get-bridge", "horizontal")]
    if tag == "intrinsic":
        ref_object = rule_clause_template[1]
        change_templates = rule_clause_template[2]
        # the let*, in order
        if workspace_string_p(ref_object):
            self_bridge = False
        else:
            self_bridge = tell(ref_object, "get-bridge", "horizontal")
        if chez.ormap(lambda ct: first(ct) == "subobjects", change_templates) is not False:
            subobject_bridges = tell(ref_object, "get-subobject-bridges", "horizontal")
        else:
            subobject_bridges = []
        change_self_p = chez.ormap(lambda ct: first(ct) == "self", change_templates)
        if change_self_p is not False:
            if exists_p(self_bridge):
                supporting_bridges = [self_bridge] + subobject_bridges
            else:
                supporting_bridges = tell(ref_object, "get-subobject-bridges", "horizontal")
        else:
            supporting_bridges = subobject_bridges
        unsupported_self_change = change_self_p is not False and not exists_p(self_bridge)
        return [unsupported_self_change, supporting_bridges]
    return None     # record-case without else: void


def get_supporting_horizontal_bridges(rule_clause_template):
    """rules.ss: get-supporting-horizontal-bridges"""
    tag = rule_clause_template[0]
    if tag == "extrinsic":
        ref_objects = rule_clause_template[1]
        if len(ref_objects) == 1:
            return tell(first(ref_objects), "get-subobject-bridges", "horizontal")
        return tell_all(ref_objects, "get-bridge", "horizontal")
    if tag == "intrinsic":
        ref_object = rule_clause_template[1]
        change_templates = rule_clause_template[2]
        # the let*, in order
        if workspace_string_p(ref_object):
            self_bridge = False
        else:
            self_bridge = tell(ref_object, "get-bridge", "horizontal")
        if chez.ormap(lambda ct: first(ct) == "subobjects", change_templates) is not False:
            subobject_bridges = tell(ref_object, "get-subobject-bridges", "horizontal")
        else:
            subobject_bridges = []
        change_self_p = chez.ormap(lambda ct: first(ct) == "self", change_templates)
        if change_self_p is not False:
            if exists_p(self_bridge):
                return [self_bridge] + subobject_bridges
            return tell(ref_object, "get-subobject-bridges", "horizontal")
        return subobject_bridges
    return None     # record-case without else: void


# This works because all the components of the rule-clause-templates,
# as well as the rule-clause-templates themselves, are fully sorted
# when they get created:

def rules_equal_p(rule1, rule2):
    """rules.ss: rules-equal?"""
    return rule_clause_lists_equal_p(tell(rule1, "get-rule-clauses"),
                                     tell(rule2, "get-rule-clauses"))


def rule_clause_lists_equal_p(rc_list1, rc_list2):
    """rules.ss: rule-clause-lists-equal?"""
    if len(rc_list1) != len(rc_list2):
        return False
    return chez.andmap(rule_clauses_equal_p, rc_list1, rc_list2)


def rule_clauses_equal_p(rc1, rc2):
    """rules.ss: rule-clauses-equal?"""
    if not chez.eq_p(first(rc1), first(rc2)):
        return False
    if len(second(rc1)) != len(second(rc2)):
        return False
    if verbatim_clause_p(rc1):
        return chez.equal_p(second(rc1), second(rc2))
    if chez.andmap(object_descriptions_equal_p, second(rc1), second(rc2)) is False:
        return False
    return chez.equal_p(third(rc1), third(rc2))


def object_descriptions_equal_p(od1, od2):
    """rules.ss: object-descriptions-equal?"""
    return ((chez.eq_p(first(od1), first(od2))
             or ((chez.eq_p(first(od1), slipnet.plato_group) or chez.eq_p(first(od1), "string"))
                 and (chez.eq_p(first(od2), slipnet.plato_group)
                      or chez.eq_p(first(od2), "string"))))
            and chez.eq_p(second(od1), second(od2))
            and chez.eq_p(third(od1), third(od2)))


# --------------------------------- Rule Codelets --------------------------------------

p_verbatim_rule_probability = 0.01


def rule_scout():
    """rules.ss: rule-scout (the codelet procedure)"""
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < p_verbatim_rule_probability:
        # the let*, in order
        if setup.p_justify_mode is not False:
            rule_type = random_pick(["top", "bottom"])
        else:
            rule_type = "top"
        if rule_type == "top":
            letter_categories = tell(workspace.g_modified_string, "get-letter-categories")
        elif rule_type == "bottom":
            letter_categories = tell(workspace.g_answer_string, "get-letter-categories")
        else:
            letter_categories = None    # (case ...) without else: void
        rule_clauses = [["verbatim", letter_categories]]
        proposed_rule = make_rule(rule_type, rule_clauses)
        say("Proposing a verbatim ", rule_type, " rule...")
        tell(proposed_rule, "set-quality-values")
        tell(proposed_rule, "set-verbatim-rule-information")
        tell(proposed_rule, "update-proposal-level", workspace.p_proposed)
        sugar.post_codelet_star(coderack.p_low_urgency, coderack.rule_evaluator, proposed_rule)
        sugar.fizzle()
    possible_rule_types = tell(workspace.g_workspace, "get-possible-rule-types")
    if len(possible_rule_types) == 0:
        say("No rule possible. Fizzling.")
        sugar.fizzle()
    # the let*, in order: the rule type (a draw), then the change-descriptions
    # (draws)
    rule_type = random_pick(possible_rule_types)
    describable_bridges = filter_(rule_describable_bridge_p,
                                  tell(workspace.g_workspace, "get-bridges", rule_type))
    all_change_descriptions = abstract_change_descriptions(describable_bridges)
    final_change_descriptions = remove_redundant_change_descriptions(all_change_descriptions)
    # chez: map's order of application (the procedure only reads)
    rule_clause_templates = sort_templates(chez.map_(
        change_descriptions_to_rule_clause_template,
        partition(lambda c1, c2: (tell(c1, "same-change-type?", c2) is not False
                                  and tell(c1, "same-reference-objects?", c2) is not False),
                  final_change_descriptions)))
    if possible_to_instantiate_p(rule_clause_templates) is False:
        say("Not all rule objects are describable. Fizzling.")
        sugar.fizzle()
    # chez: map's order of application (instantiating a template draws)
    rule_clauses = chez.map_(instantiate_rule_clause_template, rule_clause_templates)
    proposed_rule = make_rule(rule_type, rule_clauses)
    tell(proposed_rule, "set-quality-values")
    tell(proposed_rule, "set-abstracted-rule-information", rule_clause_templates)
    tell(proposed_rule, "update-proposal-level", workspace.p_proposed)
    return sugar.post_codelet_star(coderack.p_high_urgency, coderack.rule_evaluator,
                                   proposed_rule)


def possible_to_instantiate_p(rule_clause_templates):
    """rules.ss: possible-to-instantiate?"""
    def possible_p(template):
        if intrinsic_clause_p(template):
            return object_description_possible_p(second(template))
        return chez.andmap(object_description_possible_p, second(template))
    return chez.andmap(possible_p, rule_clause_templates)


def object_description_possible_p(object_):
    """rules.ss: object-description-possible?"""
    return (workspace_string_p(object_)
            or len(tell(object_, "get-descriptions-for-rule")) != 0)


def rule_evaluator(proposed_rule):
    """rules.ss: rule-evaluator (the codelet procedure)"""
    if tell(proposed_rule, "currently-works?") is False:
        say("Proposed rule doesn't work. Fizzling.")
        sugar.fizzle()
    tell(proposed_rule, "update-strength")
    strength = tell(proposed_rule, "get-strength")
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(percent(strength)):
        say("Proposed rule not strong enough. Fizzling.")
        sugar.fizzle()
    tell(proposed_rule, "update-proposal-level", workspace.p_evaluated)
    return sugar.post_codelet_star(coderack.p_high_urgency, coderack.rule_builder, proposed_rule)


def rule_builder(proposed_rule):
    """rules.ss: rule-builder (the codelet procedure)"""
    activate_rule_descriptors_from_workspace(proposed_rule)
    equivalent_rule = tell(workspace.g_workspace, "get-equivalent-rule", proposed_rule)
    if exists_p(equivalent_rule):
        say("This rule already exists. Fizzling.")
        tell(equivalent_rule, "revise-abstracted-rule-information", proposed_rule)
        sugar.fizzle()
    tell(proposed_rule, "update-proposal-level", workspace.p_built)
    tell(workspace.g_workspace, "add-rule", proposed_rule)
    _metacat.trace.monitor_new_rules(proposed_rule)
    sugar.vprint(proposed_rule)
    if setup.p_workspace_graphics is not False:
        _metacat.rule_graphics.initialize_rule_graphics(proposed_rule)
    if setup.p_justify_mode is not False:
        return sugar.post_codelet_star(coderack.p_extremely_high_urgency,
                                       coderack.answer_justifier)
    return sugar.post_codelet_star(coderack.p_extremely_high_urgency, coderack.answer_finder)


def activate_rule_descriptors_from_workspace(rule):
    """rules.ss: activate-rule-descriptors-from-workspace"""
    for clause in tell(rule, "get-rule-clauses"):
        if not verbatim_clause_p(clause):
            for object_description in second(clause):
                tell(third(object_description), "activate-from-workspace")
            if intrinsic_clause_p(clause):
                for change in third(clause):
                    tell(third(change), "activate-from-workspace")
    return None


# -------------------------------- Rule Abstraction -----------------------------------

# See section 3.3.6 of my PhD thesis for an explanation of rule abstraction.


# rule-describable-bridge? avoids cases where a horizontal bridge cannot be
# considered as describing a change from object1 to object2 because that change
# is too hard to express:
# (1) letter=>letter.  All letter->letter bridges are ok.
# (2) letter=>group.  Example: a --> [>]:((aa)(bb))   The only type of group a
#     letter can change into directly is a letter-category sameness group.
#     For any other letter=>group change, the letter first has to be perceived
#     as a group itself (maybe if images were given the group category
#     involved they could handle such changes directly...?  Otherwise, this
#     might be a good point to add top-down pressure to find single-letter
#     groups of the desired group category).
# (3) group=>letter.  To change a group into a letter, images don't need the
#     group category, so as long as the group is based on letter-category
#     rather than lengths, such a bridge should be ok.
# (4) group=>group.  For bridges between groups, both groups must have
#     the same bond-facet. Ex: [>]:mrrjjj --> [<]:mmmrrj should be ok,
#     but [>]:abc --> [>]:mrrjjj is too hard to describe.

def rule_describable_bridge_p(bridge):
    """rules.ss: rule-describable-bridge?"""
    # a two-binding let; both only read
    object1 = tell(bridge, "get-object1")
    object2 = tell(bridge, "get-object2")
    if letter_p(object1) and letter_p(object2):
        return True
    if (letter_p(object1) and group_p(object2)
            and tell(object2, "get-bond-facet") is slipnet.plato_letter_category
            and tell(object2, "get-group-category") is slipnet.plato_samegrp):
        return True
    if (group_p(object1) and letter_p(object2)
            and tell(object1, "get-bond-facet") is slipnet.plato_letter_category):
        return True
    if (group_p(object1) and group_p(object2)
            and tell(object1, "get-bond-facet") is tell(object2, "get-bond-facet")):
        return slipnet.related_p(tell(object1, "get-group-category"),
                                 tell(object2, "get-group-category"))
    return False


def abstract_change_descriptions(bridges_):
    """rules.ss: abstract-change-descriptions"""
    all_change_descriptions = []

    def add_extrinsic_change_description(objects, dim, descs, abstraction_possible_p):
        nonlocal all_change_descriptions
        change_description = make_extrinsic_change_description(objects, dim, descs)
        if abstraction_possible_p is not False:
            tell(change_description, "mark-as-subobjects-swap-if-possible")
        all_change_descriptions = [change_description] + all_change_descriptions
        return "done"

    def add_intrinsic_change_description(object_, scope, dim, desc1, relation, desc2):
        nonlocal all_change_descriptions
        # Disallow individual StrPosCtgy changes:
        if dim is not slipnet.plato_string_position_category:
            all_change_descriptions = (
                [make_intrinsic_change_description(object_, scope, dim, desc1, relation, desc2)]
                + all_change_descriptions)
        return "done"

    swap_abstraction_probability = 0.75
    subobjects_abstraction_probability = 0.75
    # Add individual (ie., intrinsic) change-descriptions:
    for b in bridges_:
        for slippage in tell(b, "get-non-symmetric-non-bond-slippages"):
            # the arguments only read
            add_intrinsic_change_description(tell(b, "get-object1"), "self",
                                             tell(slippage, "get-CM-type"),
                                             tell(slippage, "get-descriptor1"),
                                             tell(slippage, "get-label"),
                                             tell(slippage, "get-descriptor2"))
    # Add some swap (ie., extrinsic) change-descriptions:
    if len(bridges_) == 0:
        swaps_partition = []
    else:
        # the bound is drawn, then the partition draws
        swaps_partition = bounded_random_partition(
            disjoint_left_objects_p, bridges_, chez.add1(chez.random(len(bridges_))))
    for cluster in swaps_partition:
        # the let*, in order
        all_swaps = get_all_swaps(cluster)
        Length_swap = select_swap(slipnet.plato_length, all_swaps)
        ObjCtgy_swap = select_swap(slipnet.plato_object_category, all_swaps)
        remaining_swaps = chez.remq(Length_swap, chez.remq(ObjCtgy_swap, all_swaps))
        # Add swaps probabilistically.  However, if both Length and ObjCtgy swaps
        # exist, make sure that either they both get added, or neither gets added.
        # This ensures better rule-clause uniformity:
        # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
        coin_flip = chez.random(1.0)
        if coin_flip < swap_abstraction_probability:
            abstract_subobjects_p = prob_p(subobjects_abstraction_probability)
            if exists_p(Length_swap):
                add_extrinsic_change_description(
                    swap_objs(Length_swap), slipnet.plato_length, swap_descs(Length_swap),
                    abstract_subobjects_p)
            if exists_p(ObjCtgy_swap):
                add_extrinsic_change_description(
                    swap_objs(ObjCtgy_swap), slipnet.plato_object_category,
                    swap_descs(ObjCtgy_swap), abstract_subobjects_p)
        for swap in remaining_swaps:
            # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
            coin_flip = chez.random(1.0)
            if coin_flip < swap_abstraction_probability:
                # the other arguments only read; prob? is the one draw
                add_extrinsic_change_description(
                    swap_objs(swap), swap_dim(swap), swap_descs(swap),
                    prob_p(subobjects_abstraction_probability))
    # Try to abstract changes common to all bridges in each partition set:
    subobjects_partition = partition(same_left_enclosing_objects_p, bridges_)
    for cluster in subobjects_partition:
        left_enclosing_object = get_left_enclosing_object(cluster)
        if tell(left_enclosing_object, "singleton-group?") is False:
            # This allows direction reversal to be abstracted based on
            # StrPosCtgy:Opposite slippages:
            if (spans_left_side_p(cluster) is not False
                    and count_meth(cluster, "StrPosCtgy:Opposite-slippage?") == 2):
                # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
                coin_flip = chez.random(1.0)
                if coin_flip < subobjects_abstraction_probability:
                    add_intrinsic_change_description(left_enclosing_object, "self",
                                                     slipnet.plato_direction_category,
                                                     False, slipnet.plato_opposite, False)
            for schema in get_common_change_schemas(cluster):
                if ((spans_left_side_p(cluster) is not False
                     and common_right_enclosing_object_p(cluster) is False)
                        or (spans_left_side_p(cluster) is not False
                            and common_right_enclosing_object_p(cluster) is not False
                            and spans_right_side_p(cluster) is not False)
                        or (spans_left_side_p(cluster) is not False
                            and common_right_enclosing_object_p(cluster) is not False
                            and all_subobjects_describable_p(
                                get_right_enclosing_object(cluster),
                                schema_dim(schema),
                                schema_desc2(schema)) is not False)
                        or (common_right_enclosing_object_p(cluster) is not False
                            and spans_right_side_p(cluster) is not False
                            and all_subobjects_describable_p(
                                left_enclosing_object,
                                schema_dim(schema),
                                schema_desc1(schema)) is not False)):
                    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
                    coin_flip = chez.random(1.0)
                    if coin_flip < subobjects_abstraction_probability:
                        add_intrinsic_change_description(left_enclosing_object, "subobjects",
                                                         schema_dim(schema), schema_desc1(schema),
                                                         schema_relation(schema),
                                                         schema_desc2(schema))
    return all_change_descriptions


def all_subobjects_describable_p(object_, description_type, descriptor):
    """rules.ss: all-subobjects-describable?"""
    if not exists_p(descriptor):
        return False
    return chez.andmap(
        lambda obj: tell(obj, "get-descriptor-for", description_type) is descriptor,
        tell(object_, "get-constituent-objects"))


def same_left_enclosing_objects_p(b1, b2):
    """rules.ss: same-left-enclosing-objects?"""
    return tell(b1, "get-enclosing-group1") is tell(b2, "get-enclosing-group1")


def disjoint_left_objects_p(b1, b2):
    """rules.ss: disjoint-left-objects?"""
    return workspace_objects.disjoint_objects_p(tell(b1, "get-object1"), tell(b2, "get-object1"))


def common_right_enclosing_object_p(bridges_):
    """rules.ss: common-right-enclosing-object?"""
    return all_same_p(tell_all(bridges_, "get-enclosing-group2"))


def get_left_enclosing_object(bridges_):
    """rules.ss: get-left-enclosing-object"""
    return get_enclosing_object(tell(first(bridges_), "get-object1"))


def get_right_enclosing_object(bridges_):
    """rules.ss: get-right-enclosing-object"""
    return get_enclosing_object(tell(first(bridges_), "get-object2"))


def get_enclosing_object(object_):
    """rules.ss: get-enclosing-object"""
    enclosing_group = tell(object_, "get-enclosing-group")
    if exists_p(enclosing_group):
        return enclosing_group
    return tell(object_, "get-string")


def spans_left_side_p(bridges_):
    """rules.ss: spans-left-side?"""
    # the two arguments only read
    return sets_equal_p(tell_all(bridges_, "get-object1"),
                        tell(get_left_enclosing_object(bridges_), "get-constituent-objects"))


def spans_right_side_p(bridges_):
    """rules.ss: spans-right-side?"""
    # the two arguments only read
    return sets_equal_p(tell_all(bridges_, "get-object2"),
                        tell(get_right_enclosing_object(bridges_), "get-constituent-objects"))


# ----------------------------------- Schemas -----------------------------------------

# <schema> ::= (<schema-dim> <schema-desc1> <schema-relation> <schema-desc2>)
# Examples:  (Length two Identity two)  for bridge [aa]-->[bb]
#            (ObjCtgy letter #f group)           "       a-->[bb]
#            (LettCtgy a successor b)            "       a-->[bb]

def get_common_change_schemas(cluster):
    """rules.ss: get-common-change-schemas"""
    return filter_out(lambda s: schema_relation(s) is slipnet.plato_identity,
                      get_common_schemas(cluster))


def get_common_schemas(cluster):
    """rules.ss: get-common-schemas"""
    # the let*, in order
    num_bridges = len(cluster)
    concept_mapping_partition = partition(
        lambda cm1, cm2: tell(cm1, "get-CM-type") is tell(cm2, "get-CM-type"),
        _append_all(tell_all(cluster, "get-concept-mappings")))
    return map_compress(concept_mappings_to_schema,
                        filter_out(lambda p: len(p) < num_bridges, concept_mapping_partition))


def concept_mappings_to_schema(concept_mappings_):
    """rules.ss: concept-mappings->schema"""
    # the let*, in order (every binding only reads)
    cm_type = tell(first(concept_mappings_), "get-CM-type")
    labels = tell_all(concept_mappings_, "get-label")
    descriptor1s = tell_all(concept_mappings_, "get-descriptor1")
    descriptor2s = tell_all(concept_mappings_, "get-descriptor2")
    common_relation = first(labels) if all_same_p(labels) is not False else False
    common_descriptor1 = first(descriptor1s) if all_same_p(descriptor1s) is not False else False
    common_descriptor2 = first(descriptor2s) if all_same_p(descriptor2s) is not False else False
    if exists_p(common_relation) or exists_p(common_descriptor2):
        return [cm_type, common_descriptor1, common_relation, common_descriptor2]
    return False


def printf_schema(s):
    """rules.ss: printf-schema"""
    return chez.printf(
        "  ~a : ~a ~a ~a~n",
        tell(schema_dim(s), "get-short-name"),
        (tell(schema_desc1(s), "get-short-name") if exists_p(schema_desc1(s))
         else chez.String("*")),
        (tell(schema_relation(s), "get-short-name") if exists_p(schema_relation(s))
         else chez.String("*")),
        (tell(schema_desc2(s), "get-short-name") if exists_p(schema_desc2(s))
         else chez.String("*")))


def schema_dim(s):
    """rules.ss: schema-dim (1st)"""
    return first(s)


def schema_desc1(s):
    """rules.ss: schema-desc1 (2nd)"""
    return second(s)


def schema_relation(s):
    """rules.ss: schema-relation (3rd)"""
    return third(s)


def schema_desc2(s):
    """rules.ss: schema-desc2 (4th)"""
    return fourth(s)


# ------------------------------------- Swaps -----------------------------------------

# <swap> ::= ((<object> ...) <dimension> (<descriptor> <descriptor>))

def get_all_swaps(cluster):
    """rules.ss: get-all-swaps"""
    return map_compress(
        slippages_to_swap,
        partition(lambda s1, s2: tell(s1, "get-CM-type") is tell(s2, "get-CM-type"),
                  _append_all(tell_all(cluster, "get-non-symmetric-non-bond-slippages"))))


def slippages_to_swap(slippages):
    """rules.ss: slippages->swap"""
    # a two-binding let; both only read
    from_descriptors = remq_duplicates(tell_all(slippages, "get-descriptor1"))
    to_descriptors = remq_duplicates(tell_all(slippages, "get-descriptor2"))
    if sets_equal_p(from_descriptors, to_descriptors) is not False and len(from_descriptors) == 2:
        return [tell_all(slippages, "get-object1"),
                tell(first(slippages), "get-CM-type"),
                from_descriptors]
    return False


def select_swap(dimension, swaps):
    """rules.ss: select-swap"""
    return select(lambda s: second(s) is dimension, swaps)


def swap_objs(swap):
    """rules.ss: swap-objs (1st)"""
    return first(swap)


def swap_dim(swap):
    """rules.ss: swap-dim (2nd)"""
    return second(swap)


def swap_descs(swap):
    """rules.ss: swap-descs (3rd)"""
    return third(swap)


# -------------------------------------------------------------------------------------

def change_descriptions_to_rule_clause_template(change_descriptions):
    """rules.ss: change-descriptions->rule-clause-template"""
    object_type = tell(first(change_descriptions), "object-type")
    if object_type == "intrinsic-change-description":
        # a three-binding let; every binding only reads
        ref_object = tell(first(change_descriptions), "get-reference-object")
        BondFacet_change = tell(first(change_descriptions), "get-BondFacet-change")
        change_templates = tell_all(change_descriptions, "make-change-template")
        if exists_p(BondFacet_change):
            return ["intrinsic", ref_object, [BondFacet_change] + change_templates]
        return ["intrinsic", ref_object, change_templates]
    if object_type == "extrinsic-change-description":
        # the arguments only read
        if ormap_meth(change_descriptions, "subobjects-swap?") is not False:
            ref_objects = [tell(first(change_descriptions), "get-enclosing-object")]
        else:
            ref_objects = sort_by_method("get-left-string-pos", _lt,
                                         tell(first(change_descriptions),
                                              "get-reference-objects"))
        return ["extrinsic", ref_objects, tell_all(change_descriptions, "get-dimension")]
    return None     # (case ...) without else: void


# Sort intrinsic clauses by reference objects' <level of nesting>+<string position>

def sort_templates(rule_clause_templates):
    """rules.ss: sort-templates"""
    def before_p(t1, t2):
        return ((intrinsic_clause_p(t1) and extrinsic_clause_p(t2))
                or (extrinsic_clause_p(t1) and extrinsic_clause_p(t2)
                    and len(third(t1)) > len(third(t2)))
                or (intrinsic_clause_p(t1) and intrinsic_clause_p(t2)
                    and (tell(second(t2), "nested-member?", second(t1)) is not False
                         or (workspace_objects.disjoint_objects_p(second(t1), second(t2))
                             is not False
                             and (tell(second(t1), "get-left-string-pos")
                                  < tell(second(t2), "get-left-string-pos"))))))
    # chez: Chez's sort algorithm and predicate calls
    return chez.sort(before_p, rule_clause_templates)


def intrinsic_clause_p(rc):
    """rules.ss: intrinsic-clause?"""
    return first(rc) == "intrinsic"


def extrinsic_clause_p(rc):
    """rules.ss: extrinsic-clause?"""
    return first(rc) == "extrinsic"


def verbatim_clause_p(rc):
    """rules.ss: verbatim-clause?"""
    return first(rc) == "verbatim"


def literal_clause_p(rc):
    """rules.ss: literal-clause?"""
    tag = rc[0]
    if tag == "intrinsic":
        object_descriptions, changes = rc[1], rc[2]
        r = chez.ormap(literal_object_description_p, object_descriptions)
        if r is not False:
            return r
        return chez.ormap(literal_change_p, changes)
    if tag == "extrinsic":
        object_descriptions = rc[1]
        return chez.ormap(literal_object_description_p, object_descriptions)
    return None     # record-case without else: void


def literal_object_description_p(object_description):
    """rules.ss: literal-object-description?"""
    return member_p(second(object_description),
                    [slipnet.plato_alphabetic_position_category, slipnet.plato_letter_category,
                     slipnet.plato_length])


def literal_change_p(change):
    """rules.ss: literal-change?"""
    return slipnet.platonic_literal_p(third(change))


def instantiate_rule_clause_template(rule_clause_template):
    """rules.ss: instantiate-rule-clause-template"""
    tag = rule_clause_template[0]
    if tag == "intrinsic":
        ref_object, change_templates = rule_clause_template[1], rule_clause_template[2]
        object_description = reference_object_to_object_description(ref_object)
        # (list 'intrinsic (list od) (map ...)): only the map draws
        # chez: map's order of application (instantiate-change-template draws)
        return ["intrinsic",
                [object_description],
                chez.map_(instantiate_change_template(object_description),
                          sort_change_templates(change_templates))]
    if tag == "extrinsic":
        ref_objects, dimensions = rule_clause_template[1], rule_clause_template[2]
        # only the map draws; the dimensions' sort only reads
        # chez: map's order of application (choose-description-for-rule draws)
        object_descriptions = chez.map_(reference_object_to_object_description,
                                        sort_reference_objects(ref_objects))
        return ["extrinsic", object_descriptions, sort_rule_dimensions(dimensions)]
    return None     # record-case without else: void


def reference_object_to_object_description(object_):
    """rules.ss: reference-object->object-description"""
    if workspace_string_p(object_):
        return ["string", slipnet.plato_string_position_category, slipnet.plato_whole]
    chosen_description = tell(object_, "choose-description-for-rule")
    return [tell(object_, "get-descriptor-for", slipnet.plato_object_category),
            tell(chosen_description, "get-description-type"),
            tell(chosen_description, "get-descriptor")]


def instantiate_change_template(object_description):
    """rules.ss: instantiate-change-template"""
    def instantiate(change_template):
        """rules.ss: instantiate-change-template (the procedure it makes)"""
        # the let*, in order
        scope = first(change_template)
        dimension = second(change_template)
        possible_descriptors = third(change_template)
        chosen_descriptor = stochastic_pick(
            possible_descriptors,
            formulas.temp_adjusted_values(tell_all(possible_descriptors, "get-conceptual-depth")))
        # This avoids rules such as "Change LettCtgy of object with
        # LettCtgy `c' to successor" by substituting the literal
        # descriptor (i.e., `d') for the relation:
        if (dimension is second(object_description)
                and slipnet.platonic_relation_p(chosen_descriptor) is not False):
            descriptor = tell(third(object_description), "get-related-node", chosen_descriptor)
        else:
            descriptor = chosen_descriptor
        return [scope, dimension, descriptor]
    return instantiate


def sort_reference_objects(ref_objects):
    """rules.ss: sort-reference-objects"""
    return sort_by_method("get-left-string-pos", _lt, ref_objects)


def sort_rule_dimensions(dimensions):
    """rules.ss: sort-rule-dimensions"""
    return sort_wrt_order(dimensions, g_rule_dimension_order)


# Sort change-templates by <scope>+<dimension>

def sort_change_templates(change_templates):
    """rules.ss: sort-change-templates"""
    def before_p(ct1, ct2):
        return ((first(ct1) == "self" and first(ct2) == "subobjects")
                or (first(ct1) == first(ct2)
                    and (list_index(g_rule_dimension_order, second(ct1))
                         < list_index(g_rule_dimension_order, second(ct2)))))
    # chez: Chez's sort algorithm and predicate calls
    return chez.sort(before_p, change_templates)


# (define *rule-dimension-order* (list plato-direction-category ...)): made by load()
g_rule_dimension_order = None


# ------------------------------- Change-descriptions ----------------------------------

class ExtrinsicChangeDescription(SchemeObject):
    """rules.ss: make-extrinsic-change-description (the closure)"""
    __slots__ = ("reference_objects", "dimension", "descriptors",
                 "equivalent_intrinsic_changes", "subobjects_swap_p")

    def __init__(this, reference_objects, dimension, descriptors):
        this.reference_objects = reference_objects
        this.dimension = dimension
        this.descriptors = descriptors

        def equivalent_change(obj):
            # the arguments only read
            if tell(obj, "get-descriptor-for", dimension) is first(descriptors):
                descriptor2 = second(descriptors)
            else:
                descriptor2 = first(descriptors)
            return make_intrinsic_change_description(obj, "self", dimension, False, False,
                                                     descriptor2)
        # chez: map's order of application (the procedure only reads)
        this.equivalent_intrinsic_changes = chez.map_(equivalent_change, reference_objects)
        this.subobjects_swap_p = False

    @message("object-type")
    def object_type(this, self):
        return "extrinsic-change-description"

    @message("print")
    def print_(this, self):
        names = [tell(first(this.reference_objects), "ascii-name")] + chez.map_(
            lambda obj: chez.format_(", ~a", tell(obj, "ascii-name")),
            rest(this.reference_objects))
        return chez.printf("CD: Swap ~a of ~a~n",
                           tell(this.dimension, "get-short-name"),
                           chez.String("".join(names)))

    @message("intrinsic?")
    def intrinsic_p(this, self):
        return False

    @message("implies?")
    def implies_p(this, self, c):
        if tell(c, "intrinsic?") is not False:
            return extrinsic_implies_intrinsic_p(self, c)
        return extrinsic_implies_extrinsic_p(self, c)

    @message("get-reference-objects")
    def get_reference_objects(this, self):
        return this.reference_objects

    @message("get-dimension")
    def get_dimension(this, self):
        return this.dimension

    @message("get-equivalent-intrinsic-changes")
    def get_equivalent_intrinsic_changes(this, self):
        return this.equivalent_intrinsic_changes

    @message("same-change-type?")
    def same_change_type_p(this, self, c):
        return tell(c, "intrinsic?") is False

    @message("dimension?")
    def dimension_p(this, self, dim):
        return dim is this.dimension

    @message("same-reference-objects?")
    def same_reference_objects_p(this, self, ec):
        return sets_equal_p(this.reference_objects, tell(ec, "get-reference-objects"))

    @message("common-reference-object?")
    def common_reference_object_p(this, self, ic):
        return member_p(tell(ic, "get-reference-object"), this.reference_objects)

    @message("get-enclosing-object")
    def get_enclosing_object_(this, self):
        return get_enclosing_object(first(this.reference_objects))

    @message("subobjects-swap?")
    def subobjects_swap_p_(this, self):
        return this.subobjects_swap_p

    @message("mark-as-subobjects-swap-if-possible")
    def mark_as_subobjects_swap_if_possible(this, self):
        if sets_equal_p(tell(tell(self, "get-enclosing-object"), "get-constituent-objects"),
                        this.reference_objects) is not False:
            this.subobjects_swap_p = True
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_extrinsic_change_description(reference_objects, dimension, descriptors):
    """rules.ss: make-extrinsic-change-description"""
    return ExtrinsicChangeDescription(reference_objects, dimension, descriptors)


class IntrinsicChangeDescription(SchemeObject):
    """rules.ss: make-intrinsic-change-description (the closure)"""
    __slots__ = ("reference_object", "scope", "dimension", "descriptor1", "relation",
                 "descriptor2", "descriptors", "enclosing_object")

    def __init__(this, reference_object, scope, dimension, descriptor1, relation, descriptor2):
        this.reference_object = reference_object
        this.scope = scope
        this.dimension = dimension
        this.descriptor1 = descriptor1
        this.relation = relation
        this.descriptor2 = descriptor2
        # a two-binding let; both only read
        this.descriptors = compress([relation, descriptor2])
        if workspace_string_p(reference_object):
            this.enclosing_object = False
        else:
            this.enclosing_object = get_enclosing_object(reference_object)

    @message("object-type")
    def object_type(this, self):
        return "intrinsic-change-description"

    @message("print")
    def print_(this, self):
        return chez.printf("CD: Change ~a of ~a~a to ~a~n",
                           tell(this.dimension, "get-short-name"),
                           chez.String("all subobjects of " if this.scope == "subobjects" else ""),
                           tell(this.reference_object, "ascii-name"),
                           tell_all(this.descriptors, "get-short-name"))

    @message("intrinsic?")
    def intrinsic_p(this, self):
        return True

    @message("implies?")
    def implies_p(this, self, c):
        if tell(c, "intrinsic?") is not False:
            return intrinsic_implies_intrinsic_p(self, c)
        return False

    @message("conflicts?")
    def conflicts_p(this, self, ic):
        r = intrinsic_implies_intrinsic_p(self, ic)
        if r is not False:
            return r
        return intrinsic_implies_intrinsic_p(ic, self)

    @message("get-reference-object")
    def get_reference_object(this, self):
        return this.reference_object

    @message("get-scope")
    def get_scope(this, self):
        return this.scope

    @message("get-dimension")
    def get_dimension(this, self):
        return this.dimension

    @message("get-descriptor1")
    def get_descriptor1(this, self):
        return this.descriptor1

    @message("get-descriptor2")
    def get_descriptor2(this, self):
        return this.descriptor2

    @message("get-descriptors")
    def get_descriptors(this, self):
        return this.descriptors

    @message("get-enclosing-object")
    def get_enclosing_object_(this, self):
        return this.enclosing_object

    @message("same-change-type?")
    def same_change_type_p(this, self, c):
        return tell(c, "intrinsic?")

    @message("same-scope?")
    def same_scope_p(this, self, ic):
        return chez.eq_p(this.scope, tell(ic, "get-scope"))

    @message("change-self?")
    def change_self_p(this, self):
        return this.scope == "self"

    @message("change-subobjects?")
    def change_subobjects_p(this, self):
        return this.scope == "subobjects"

    @message("dimension?")
    def dimension_p(this, self, dim):
        return dim is this.dimension

    @message("same-dimension?")
    def same_dimension_p(this, self, ic):
        return tell(ic, "dimension?", this.dimension)

    @message("LettCtgy/AlphPosCtgy-dimension?")
    def LettCtgy_or_AlphPosCtgy_dimension_p(this, self):
        return (this.dimension is slipnet.plato_letter_category
                or this.dimension is slipnet.plato_alphabetic_position_category)

    @message("same-reference-objects?")
    def same_reference_objects_p(this, self, ic):
        return this.reference_object is tell(ic, "get-reference-object")

    @message("encloses?")
    def encloses_p(this, self, ic):
        return this.reference_object is tell(ic, "get-enclosing-object")

    @message("encloses-at-any-level?")
    def encloses_at_any_level_p(this, self, ic):
        return tell(this.reference_object, "nested-member?", tell(ic, "get-reference-object"))

    @message("change-to-letter?")
    def change_to_letter_p(this, self):
        return this.descriptor2 is slipnet.plato_letter

    @message("same-dimension-as-GroupCtgy-medium?")
    def same_dimension_as_GroupCtgy_medium_p(this, self, ic):
        return this.dimension is tell(tell(ic, "get-reference-object"), "get-bond-facet")

    @message("implied-by-opposite-GroupCtgy?")
    def implied_by_opposite_GroupCtgy_p(this, self):
        return (group_p(this.reference_object)
                and tell(this.reference_object, "get-ending-letter-category") is this.descriptor2)

    @message("get-BondFacet-change")
    def get_BondFacet_change(this, self):
        if (group_p(this.reference_object)
                and this.dimension is slipnet.plato_group_category
                and this.scope == "self"):
            return ["self", slipnet.plato_bond_facet,
                    [tell(this.reference_object, "get-bond-facet")]]
        return False

    @message("make-change-template")
    def make_change_template(this, self):
        # Disallow literal DirCtgy or GroupCtgy changes (ie, DirCtgy:left):
        if (this.dimension is slipnet.plato_direction_category
                or this.dimension is slipnet.plato_group_category):
            return [this.scope, this.dimension, chez.remq(this.descriptor2, this.descriptors)]
        return [this.scope, this.dimension, this.descriptors]

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_intrinsic_change_description(reference_object, scope, dimension, descriptor1,
                                      relation, descriptor2):
    """rules.ss: make-intrinsic-change-description"""
    return IntrinsicChangeDescription(reference_object, scope, dimension, descriptor1,
                                      relation, descriptor2)


# ------------------------- Change-description heuristics -----------------------------

def remove_redundant_change_descriptions(change_descriptions):
    """rules.ss: remove-redundant-change-descriptions"""
    # (append a b): both parts only read, so Chez's order of evaluating them
    # (the second first) is not observable
    return remq_elements(
        pairwise_map(get_redundant_change, change_descriptions)
        + changes_implied_by_string_position_swaps(change_descriptions),
        change_descriptions)


def changes_implied_by_string_position_swaps(change_descriptions):
    """rules.ss: changes-implied-by-string-position-swaps"""
    string_position_change_descriptions = filter_meth(
        change_descriptions, "dimension?", slipnet.plato_string_position_category)
    # the arguments only read
    # chez: map's order of application (the procedure only reads)
    return _append_all(chez.map_(
        get_changes_implied_by_swap(
            filter_meth(change_descriptions, "intrinsic?"),
            remq_elements(string_position_change_descriptions,
                          filter_out_meth(change_descriptions, "intrinsic?"))),
        string_position_change_descriptions))


def get_changes_implied_by_swap(intrinsic_change_descriptions,
                                other_extrinsic_change_descriptions):
    """rules.ss: get-changes-implied-by-swap"""
    def implied(string_position_ec):
        """rules.ss: get-changes-implied-by-swap (the procedure it makes)"""
        # (append a b): both parts only read
        return (filter_meth(other_extrinsic_change_descriptions,
                            "same-reference-objects?", string_position_ec)
                + _append_all(pairwise_map(get_symmetric_changes(string_position_ec),
                                           intrinsic_change_descriptions)))
    return implied


def get_symmetric_changes(string_position_ec):
    """rules.ss: get-symmetric-changes"""
    def symmetric(ic1, ic2):
        """rules.ss: get-symmetric-changes (the procedure it makes)"""
        if (tell(string_position_ec, "common-reference-object?", ic1) is not False
                and tell(string_position_ec, "common-reference-object?", ic2) is not False
                and tell(ic1, "same-dimension?", ic2) is not False
                and tell(ic1, "get-descriptor1") is tell(ic2, "get-descriptor2")
                and tell(ic1, "get-descriptor2") is tell(ic2, "get-descriptor1")):
            return [ic1, ic2]
        return []
    return symmetric


def get_redundant_change(c1, c2):
    """rules.ss: get-redundant-change"""
    if tell(c1, "implies?", c2) is not False:
        return c2
    if tell(c2, "implies?", c1) is not False:
        return c1
    return False


# Rule-abstraction Heuristics
# ---------------------------
# (see pages 110-113 of my PhD thesis for an explanation of all this)
#
# (intrinsic-implies-intrinsic? ic1 ic2) is true iff intrinsic change ic2 is already
# implicit in intrinsic change ic1. The possible cases are described below:
#
# (1) a change to an object should be ignored if the object has an immediate
#     enclosing group and there exists a change of the same type to the enclosing
#     group's subobjects:
#     [xyz] --> [[xx][yy][zz]]
#        "[xyz] subobjects Length:succ"
#        "x self Length:succ"  (IMPLICIT)
#     [xyz] --> [[xx][y][zz]]
#        "[xyz] subobjects ObjCtgy:group"
#        "x self ObjCtgy:group"  (IMPLICIT)
#
# (2.1) a LettCtgy change to an object should be ignored if there exists a LettCtgy
#       change to some other object that encloses the object at any level of nesting,
#       since the highest-level LettCtgy change will change the letter categories of
#       all nested objects automatically:
#       [[aa]b] --> [[bb]c]
#          "[[aa]b] {self || subobjects} LettCtgy:succ"
#          "[aa] {self || subobjects} LettCtgy:succ"  (IMPLICIT)
#          "a self LettCtgy:succ"  (IMPLICIT)
#       If an object has a LettCtgy change both to itself and to its subobjects,
#       only the change to the subobjects should be ignored:
#       [[aa]b] --> [[bb]c]
#          "[[aa]b] self LettCtgy:succ"
#          "[[aa]b] subobjects LettCtgy:succ"  (IMPLICIT)
#
# (2.2) a LettCtgy change to an object should be ignored if there exists an AlphPosCtgy
#       change to either the object itself, or to some other object that encloses it at
#       any level of nesting, since the AlphPosCtgy change will automatically change
#       the letter categories of all nested objects:
#       [aa] --> zz
#          "[aa] {self || subobjects} AlphPosCtgy:opp"
#          "[aa] {self || subobjects} LettCtgy:z"  (IMPLICIT)
#
# (2.3) a LettCtgy change to an object should be ignored if the object has an
#       enclosing group whose Length changes, since this might implicitly change
#       the letter-categories of all the group's subobjects in a way that could
#       be inconsistent with the object's own LettCtgy change:
#       [abc] --> [abcd]
#          "[abc] self Length:succ"
#          "c self LettCtgy:succ"  (IMPLICIT)
#
# (2.4) a LettCtgy change to an object should be ignored if the GroupCtgy of the
#       object's immediate enclosing group changes, or if the object itself is a
#       group whose GroupCtgy changes, and the LettCtgy change is to the group's
#       *ending* letter-category:
#       [abc]> --> [cba]>
#          "[abc] self GroupCtgy:opp"
#          "c self LettCtgy:a"  (IMPLICIT)
#       [abc]> --> [cba]>
#          "[abc] self GroupCtgy:opp"
#          "[abc] self LettCtgy:c"  (IMPLICIT)
#
# (3) a Length change to an object should be ignored if there exists an ObjCtgy:letter
#     change to the same object (or if both of these changes refer to the subobjects),
#     or if the Length change is to the object and the ObjCtgy:letter change is to its
#     immediate-enclosing-group's subobjects, since the ObjCtgy:letter change
#     automatically implies Length:
#     [[aa][b][cc]] --> [abc]
#        "[[aa][b][cc]] subobjects ObjCtgy:letter"
#        "[aa] self Length:pred"  (IMPLICIT)
#     [aa] --> a
#        "[aa] self ObjCtgy:letter"
#        "[aa] self Length:one"  (IMPLICIT)
#     [[aa][bb][cc]] --> [abc]
#        "[[aa][bb][cc]] subobjects ObjCtgy:letter"
#        "[[aa][bb][cc]] subobjects Length:one"  (IMPLICIT)

def intrinsic_implies_intrinsic_p(ic1, ic2):
    """rules.ss: intrinsic-implies-intrinsic?  (The value is only ever tested,
    so the or of ands is written with booleans; the tells are made in the
    original's order and short-circuited as it does.)"""
    def t(obj, msg, *args):
        return tell(obj, msg, *args) is not False

    return ((t(ic1, "same-dimension?", ic2)
             and ((t(ic1, "same-reference-objects?", ic2)
                   and t(ic1, "same-scope?", ic2))
                  or (t(ic1, "encloses?", ic2)
                      and t(ic1, "change-subobjects?")
                      and t(ic2, "change-self?"))))
            or (t(ic1, "LettCtgy/AlphPosCtgy-dimension?")
                and t(ic2, "LettCtgy/AlphPosCtgy-dimension?")
                and ((t(ic1, "same-reference-objects?", ic2)
                      and ((t(ic1, "dimension?", slipnet.plato_alphabetic_position_category)
                            and t(ic2, "dimension?", slipnet.plato_letter_category))
                           or (t(ic1, "same-dimension?", ic2)
                               and t(ic2, "change-subobjects?"))))
                     or t(ic1, "encloses-at-any-level?", ic2)))
            or (t(ic1, "dimension?", slipnet.plato_length)
                and t(ic2, "dimension?", slipnet.plato_letter_category)
                and t(ic1, "encloses?", ic2)
                and t(ic1, "change-self?"))
            or (t(ic1, "dimension?", slipnet.plato_group_category)
                and t(ic1, "change-self?")
                and t(ic2, "same-dimension-as-GroupCtgy-medium?", ic1)
                and (t(ic1, "encloses?", ic2)
                     or (t(ic1, "same-reference-objects?", ic2)
                         and t(ic2, "dimension?", slipnet.plato_letter_category)
                         and t(ic2, "implied-by-opposite-GroupCtgy?"))))
            or (t(ic1, "dimension?", slipnet.plato_string_position_category)
                and t(ic2, "dimension?", slipnet.plato_direction_category)
                and t(ic2, "encloses?", ic1)
                and t(ic2, "change-self?"))
            or (t(ic1, "change-to-letter?")
                and t(ic2, "dimension?", slipnet.plato_length)
                and ((t(ic1, "same-reference-objects?", ic2)
                      and t(ic1, "same-scope?", ic2))
                     or (t(ic1, "encloses?", ic2)
                         and t(ic1, "change-subobjects?")
                         and t(ic2, "change-self?")))))


def extrinsic_implies_intrinsic_p(ec, ic):
    """rules.ss: extrinsic-implies-intrinsic?"""
    return ormap_meth(tell(ec, "get-equivalent-intrinsic-changes"), "conflicts?", ic)


def extrinsic_implies_extrinsic_p(ec1, ec2):
    """rules.ss: extrinsic-implies-extrinsic?"""
    equivalent_ic2_changes = tell(ec2, "get-equivalent-intrinsic-changes")
    return chez.andmap(
        lambda ic1: chez.ormap(lambda ic2: tell(ic1, "implies?", ic2), equivalent_ic2_changes),
        tell(ec1, "get-equivalent-intrinsic-changes"))


# --------------------------------- Rule Application -----------------------------------

def ignore_snag(failure_result):
    """rules.ss: ignore-snag"""
    return "done"


# apply-rule returns a list of the form ((<object> (<transform> ...)) ...),
# where <object> is either a workspace-object or a workspace-string, or #f
# if the rule application fails:

def apply_rule(rule, string, failure_action):
    """rules.ss: apply-rule"""
    if tell(rule, "verbatim?") is not False:
        # the arguments only read
        tell(tell(string, "get-image"), "new-appearance",
             tell(rule, "get-verbatim-letter-categories"))
        return []

    def body(abort):
        tell(string, "reset-string-image")

        def swap_fail(objects, dimension):
            failure_action(["SWAP", objects, dimension])
            return abort(False)
        # the let*, in order
        extrinsic_transforms = get_extrinsic_transforms(rule, string, swap_fail)
        string_position_swaps = filter_(
            lambda x: first(x) is slipnet.plato_string_position_category, extrinsic_transforms)
        extrinsic_object_transform_pairs = remq_elements(string_position_swaps,
                                                         extrinsic_transforms)
        intrinsic_object_transform_pairs = get_intrinsic_transforms(rule, string)
        all_object_transform_pairs = (extrinsic_object_transform_pairs
                                      + intrinsic_object_transform_pairs)

        def conflict_fail(object1, dimension1, object2, dimension2):
            failure_action(["CONFLICT", object1, dimension1, object2, dimension2])
            return abort(False)
        check_for_conflicts(all_object_transform_pairs, conflict_fail)
        # a two-binding let; the first only reads, the second makes a procedure
        # chez: map's order of application (the procedure only builds lists)
        transforms_grouped_by_object = chez.sort(
            lambda ot1, ot2: (tell(first(ot1), "get-nesting-level")
                              > tell(first(ot2), "get-nesting-level")),
            chez.map_(lambda p: [first(first(p)), chez.map_(second, p)],
                      partition(lambda ot1, ot2: first(ot1) is first(ot2),
                                all_object_transform_pairs)))

        def fail(object_):
            def with_transform(transform):
                def thunk():
                    failure_action(["CHANGE", object_, transform])
                    return abort(False)
                return thunk
            return with_transform
        # transforms-grouped-by-object is a list of the form
        #    ((<object> (<transform> ...)) ...)
        # where each (<transform> ...) is a list of all the transforms that
        # apply to <object> in the string according to the changes specified
        # in the rule, including all StrPosCtgy changes.  Deepest-level objects
        # are transformed first ("inside-out" order).
        for l in transforms_grouped_by_object:
            object_ = first(l)
            transforms = second(l)
            apply_transforms(transforms, tell(object_, "get-image"), fail(object_))
        for swap in string_position_swaps:
            object1 = second(swap)
            object2 = third(swap)
            apply_string_position_swap(object1, object2)
        return transforms_grouped_by_object
    return sugar.continuation_point_star(body)


def check_for_conflicts(object_transform_pairs, fail):
    """rules.ss: check-for-conflicts"""
    # chez: map's order of application (the procedure only reads)
    equivalent_change_descriptions = chez.map_(
        lambda ot: make_intrinsic_change_description(first(ot), "self", first(second(ot)),
                                                     False, False, second(second(ot))),
        object_transform_pairs)

    def check(ic1, ic2):
        if tell(ic1, "conflicts?", ic2) is not False:
            # the arguments only read
            return fail(tell(ic1, "get-reference-object"),
                        tell(ic1, "get-dimension"),
                        tell(ic2, "get-reference-object"),
                        tell(ic2, "get-dimension"))
        return False
    # pairwise-map: utilities.ss's order (fail escapes at the first conflict)
    pairwise_map(check, equivalent_change_descriptions)
    return "done"


def apply_transforms(transforms, image, fail):
    """rules.ss: apply-transforms"""
    # chez: Chez's sort algorithm and predicate calls
    ordered_transforms = chez.sort(apply_before_p(tell(image, "get-length")), transforms)
    result = None
    for transform in ordered_transforms:
        if first(transform) is slipnet.plato_group_category:
            # 1.2: (2nd (assq plato-bond-facet transforms)) is cadr of #f, an
            # error, if no BondFacet transform comes with the GroupCtgy one
            medium = _cadr(chez.assq(slipnet.plato_bond_facet, transforms))
            GroupCtgy_transform = [slipnet.plato_group_category, slipnet.plato_opposite, medium]
            result = transform_image(image, GroupCtgy_transform, fail(GroupCtgy_transform))
        else:
            result = transform_image(image, transform, fail(transform))
    return result


# Special case:  BondFacet "transforms" are allowed in a rule, but have no effect,
# and are included in a rule solely in conjunction with (<GroupCtgy> <Opposite>)
# transforms to indicate which medium (i.e., letter-category or length) is being
# reversed in the group.  Without this extra "transform", the bond-facet information
# would be lost whenever a rule specifying a GroupCtgy reversal for a group is
# abstracted (i.e., predgrp=(Opp)=>succgrp or succgrp=(Opp)=>predgrp).  Whenever
# transform-image is invoked with a GroupCtgy transform, the transform is first
# augmented with the bond-facet information gotten from the BondFacet "transform".
# Thus, in transform-image,
#
# transform ::= (<dimension> <descriptor>) | (<GroupCtgy> <Opposite> <bond-facet>)

def transform_image(image, transform, fail):
    """rules.ss: transform-image"""
    # a two-binding let; both only read
    dimension = first(transform)
    descriptor = second(transform)
    name = tell(dimension, "get-name-symbol")
    if name == "plato-object-category":
        if descriptor is slipnet.plato_letter:
            return tell(image, "letter", fail)
        return tell(image, "group", fail)
    if name == "plato-letter-category":
        return tell(image, "new-start-letter", descriptor, fail)
    if name == "plato-length":
        return tell(image, "new-length", descriptor, fail)
    if name == "plato-direction-category":
        return tell(image, "reverse-direction", fail)
    if name == "plato-group-category":
        return tell(image, "reverse-medium", third(transform), fail)
    if name == "plato-alphabetic-position-category":
        return tell(image, "new-alpha-position-category", descriptor, fail)
    # BondFacet "transforms" have no effect:
    if name == "plato-bond-facet":
        return "done"
    # StrPosCtgy swaps are handled separately:
    if name == "plato-string-position-category":
        return "done"
    return None     # (case ...) without else: void


def apply_before_p(platonic_image_length):
    """rules.ss: apply-before?"""
    def before_p(t1, t2):
        """rules.ss: apply-before? (the predicate it makes)"""
        return (first(t1) is slipnet.plato_group_category
                or second(t1) is slipnet.plato_letter
                or (first(t1) is slipnet.plato_length
                    and images.change_length_first_p(second(t1), platonic_image_length)
                    is not False)
                or (first(t2) is slipnet.plato_length
                    and images.change_length_first_p(second(t2), platonic_image_length)
                    is False))
    return before_p


def apply_string_position_swap(object1, object2):
    """rules.ss: apply-string-position-swap"""
    # the let*, in order
    image1 = tell(object1, "get-image")
    image2 = tell(object2, "get-image")
    image_state1 = tell(image1, "get-state")
    image_state2 = tell(image2, "get-state")
    tell(image1, "new-state", image_state2)
    tell(image2, "new-state", image_state1)
    tell(image1, "update-swapped-image", image2)
    return tell(image2, "update-swapped-image", image1)


# get-extrinsic-transforms returns a list of the form (<element> ...)
#
# <element> ::= (<object> <transform>) | (<StrPosCtgy> <object> <object>)
# <transform> ::= (<dimension> <descriptor>)

def get_extrinsic_transforms(rule, string, fail):
    """rules.ss: get-extrinsic-transforms"""
    def clause_transforms(rule_clause):
        # the let*, in order
        reference_objects = tell(string, "get-reference-objects", rule_clause)
        if len(second(rule_clause)) == 1:
            denoted_objects = _append_all(tell_all(reference_objects, "get-constituent-objects"))
        else:
            denoted_objects = reference_objects
        dimensions = third(rule_clause)
        if len(denoted_objects) < 2:
            return []
        if pairwise_andmap(workspace_objects.disjoint_objects_p, denoted_objects) is False:
            return fail(denoted_objects, first(dimensions))
        # chez: map's order of application (attach-length-description, fail)
        return _append_all(chez.map_(get_dimension_transforms(denoted_objects, fail),
                                     dimensions))
    # chez: map's order of application (attach-length-description, fail)
    return _append_all(chez.map_(clause_transforms, tell(rule, "get-extrinsic-rule-clauses")))


def get_dimension_transforms(denoted_objects, fail):
    """rules.ss: get-dimension-transforms"""
    def dimension_transforms(dimension):
        """rules.ss: get-dimension-transforms (the procedure it makes)"""
        if dimension is slipnet.plato_string_position_category:
            if len(denoted_objects) > 2:
                return fail(denoted_objects, dimension)
            return StrPosCtgy_transforms(first(denoted_objects), second(denoted_objects))

        # If a Length swap is attempted for a group without a Length
        # description, then one is attached (if possible), since this
        # amounts to explicitly noticing the group's Length.  If the
        # object is a letter, a descriptor is returned (plato-one) for
        # the letter, but a Length description is not attached:
        def descriptor_of(object_):
            if dimension is not slipnet.plato_length:
                return tell(object_, "get-descriptor-for", dimension)
            if letter_p(object_):
                return slipnet.plato_one
            if tell(object_, "description-type-present?", slipnet.plato_length) is not False:
                return tell(object_, "get-descriptor-for", slipnet.plato_length)
            groups.attach_length_description(object_)
            return tell(object_, "get-descriptor-for", slipnet.plato_length)
        # chez: map's order of application (attach-length-description)
        denoted_object_descriptors = chez.map_(descriptor_of, denoted_objects)
        if all_exist_p(denoted_object_descriptors) is False:
            return fail(denoted_objects, dimension)
        swap_descriptors = remq_duplicates(denoted_object_descriptors)
        if len(swap_descriptors) > 2:
            return fail(denoted_objects, dimension)
        # a two-binding let; both only read
        swap_descriptor1 = first(swap_descriptors)
        if len(rest(swap_descriptors)) == 0:
            swap_descriptor2 = first(swap_descriptors)
        else:
            swap_descriptor2 = second(swap_descriptors)
        # chez: map's order of application (the procedure only builds lists)
        return chez.map_(
            lambda object_, descriptor: [object_,
                                         [dimension,
                                          (swap_descriptor2 if descriptor is swap_descriptor1
                                           else swap_descriptor1)]],
            denoted_objects,
            denoted_object_descriptors)
    return dimension_transforms


# StrPosCtgy-transforms returns both a "swap transform" of the
# form (<StrPosCtgy> <object1> <object2>), and two regular
# object-transform pairs -- one for each object -- of the form
# (<object> (<StrPosCtgy> <descriptor>))

def StrPosCtgy_transforms(object1, object2):
    """rules.ss: StrPosCtgy-transforms"""
    return [[slipnet.plato_string_position_category, object1, object2],
            [object1,
             [slipnet.plato_string_position_category,
              tell(object2, "get-descriptor-for", slipnet.plato_string_position_category)]],
            [object2,
             [slipnet.plato_string_position_category,
              tell(object1, "get-descriptor-for", slipnet.plato_string_position_category)]]]


# get-intrinsic-transforms pairs all string objects denoted by the rule's intrinsic
# clauses with all changes that apply to the objects.  It returns a list of the form
# ((<object> <transform>) ...)
#
# <transform> ::= (<dimension> <descriptor>)
#
# TERMINOLOGY: A "reference object" in a string is an object directly specified
# by the <obj-type> <obj-desc-type> <obj-desc> attributes of an object-description.
# A "denoted object" may either be a reference object (in the case of a 'self change),
# or one of the subobjects of a reference object (in the case of a 'subobjects change).
# Example: in the string [abc], the rule clause
#
# (intrinsic ((<group> <StrPosCtgy> <whole>))
#            ((self <LettCtgy> <succ>)
#             (subobjects <Length> <succ>)))
#
# names the group [abc] as the *reference* object, while the *denoted* objects
# are both the group [abc] (via self), and the letters a, b, c (via subobjects).

def get_intrinsic_transforms(rule, string):
    """rules.ss: get-intrinsic-transforms"""
    def clause_transforms(rule_clause):
        # the let*, in order
        ref_objects = tell(string, "get-reference-objects", rule_clause)
        changes = third(rule_clause)
        self_transforms = filter_map(lambda c: first(c) == "self", rest, changes)
        subobject_transforms = filter_map(lambda c: first(c) == "subobjects", rest, changes)
        if len(subobject_transforms) == 0:
            return cross_product(ref_objects, self_transforms)
        all_subobjects = _append_all(tell_all(ref_objects, "get-constituent-objects"))
        # (append a b): both parts only read
        return (cross_product(ref_objects, self_transforms)
                + cross_product(all_subobjects, subobject_transforms))
    # chez: map's order of application
    return _append_all(chez.map_(clause_transforms, tell(rule, "get-intrinsic-rule-clauses")))


# ----------------------------------- Rule Quality -------------------------------------

def compute_rule_quality(rule):
    """rules.ss: compute-rule-quality"""
    return round_(chez.mul(
        percent(tell(rule, "get-uniformity")),
        weighted_average([tell(rule, "get-abstractness"), tell(rule, "get-succinctness")],
                         [3, 2])))


def compute_rule_uniformity(rule):
    """rules.ss: compute-rule-uniformity"""
    if tell(rule, "identity?") is not False or tell(rule, "verbatim?") is not False:
        return 100
    # the let*, in order (every binding only reads)
    rule_clauses = tell(rule, "get-rule-clauses")
    intrinsic_clauses = tell(rule, "get-intrinsic-rule-clauses")
    extrinsic_clauses = tell(rule, "get-extrinsic-rule-clauses")
    changes = filter_out(
        lambda c: (second(c) is slipnet.plato_object_category
                   or second(c) is slipnet.plato_bond_facet),
        flatmap(third, intrinsic_clauses))
    changes_grouped_by_dimension = partition(lambda c1, c2: second(c1) is second(c2), changes)
    all_intrinsic_object_descriptions = flatmap(second, intrinsic_clauses)
    if len(intrinsic_clauses) == 0:
        intrinsic_clauses_uniformity = 1
    else:
        intrinsic_clauses_uniformity = average(
            product(chez.map_(change_abstractness_uniformity, changes_grouped_by_dimension)),
            object_description_uniformity(all_intrinsic_object_descriptions))
    if len(extrinsic_clauses) == 0:
        extrinsic_clauses_uniformity = 1
    else:
        extrinsic_clauses_uniformity = average(chez.map_(
            compose(cube, object_description_uniformity),
            chez.map_(second, extrinsic_clauses)))
    clause_type_uniformity = chez.div(chez.max_(count(intrinsic_clause_p, rule_clauses),
                                                count(extrinsic_clause_p, rule_clauses)),
                                      len(rule_clauses))
    raw_uniformity = weighted_average(
        [intrinsic_clauses_uniformity, extrinsic_clauses_uniformity, clause_type_uniformity],
        [5, 5, 1])
    adjusted_uniformity = chez.exp(chez.mul(4, chez.sub(raw_uniformity, 1)))
    return times_100(adjusted_uniformity)


# (let ((adjust-depth (sigmoid 3 40))) (lambda (rule) ...))
_adjust_depth = sigmoid(3, 40)


def compute_rule_abstractness(rule):
    """rules.ss: compute-rule-abstractness"""
    if tell(rule, "identity?") is not False:
        return 100
    if tell(rule, "verbatim?") is not False:
        return 0
    # the let*, in order (every binding only reads)
    object_descriptions = _append_all(chez.map_(second, tell(rule, "get-rule-clauses")))
    intrinsic_changes = _append_all(chez.map_(third, tell(rule, "get-intrinsic-rule-clauses")))
    swap_dimensions = _append_all(chez.map_(third, tell(rule, "get-extrinsic-rule-clauses")))
    average_object_description_type_depth = average(
        tell_all(chez.map_(second, object_descriptions), "get-conceptual-depth"))
    average_change_descriptor_depth = average(
        tell_all(chez.map_(third, intrinsic_changes), "get-conceptual-depth"))
    average_swap_dimension_depth = average(tell_all(swap_dimensions, "get-conceptual-depth"))
    return times_100(_adjust_depth(average(filter_out(
        chez.zero_p,
        [average_object_description_type_depth,
         average_change_descriptor_depth,
         average_swap_dimension_depth]))))


def compute_rule_succinctness(rule):
    """rules.ss: compute-rule-succinctness"""
    if tell(rule, "identity?") is not False or tell(rule, "verbatim?") is not False:
        return 100

    def weight(rule_clause):
        if intrinsic_clause_p(rule_clause):
            return 1
        if len(second(rule_clause)) > 1:
            return 2
        return 1
    return times_100(chez.div(4, chez.add(3, sum_(chez.map_(weight,
                                                            tell(rule, "get-rule-clauses"))))))


def object_description_uniformity(object_descriptions):
    """rules.ss: object-description-uniformity"""
    description_types = chez.map_(second, object_descriptions)
    return chez.div(maximum(chez.map_(len, partition(chez.eq_p, description_types))),
                    len(description_types))


# This returns a value in [0,1].  When changes have all relation descriptors or
# all literal descriptors, value = 1; when changes are evenly mixed, value = 0:

def change_abstractness_uniformity(changes):
    """rules.ss: change-abstractness-uniformity"""
    descriptors = chez.map_(third, changes)
    return chez.mul(2, chez.abs_(chez.sub(chez.div(count(slipnet.platonic_relation_p, descriptors),
                                                   len(descriptors)),
                                          Fraction(1, 2))))


# temporary. leave this for now:
def compute_rule_intrinsic_quality(rule):
    """rules.ss: compute-rule-intrinsic-quality"""
    if tell(rule, "identity?") is not False:
        return 100
    if tell(rule, "verbatim?") is not False:
        return 10
    # the let*, in order (every binding only reads)
    rule_clauses = tell(rule, "get-rule-clauses")
    intrinsic_clauses = tell(rule, "get-intrinsic-rule-clauses")
    extrinsic_clauses = tell(rule, "get-extrinsic-rule-clauses")
    object_description_types = chez.map_(second,
                                         _append_all(chez.map_(second, rule_clauses)))
    object_description_depth_factor = sigmoid(3, 30)(
        average(tell_all(object_description_types, "get-conceptual-depth")))
    changes = filter_out(lambda c: second(c) is slipnet.plato_object_category,
                         _append_all(chez.map_(third, intrinsic_clauses)))
    change_descriptors = chez.map_(third, changes)
    if len(change_descriptors) == 0:
        change_descriptor_abstractness = 1
    else:
        change_descriptor_abstractness = chez.div(
            count(slipnet.platonic_relation_p, change_descriptors), len(change_descriptors))
    changes_grouped_by_dimension = partition(lambda c1, c2: second(c1) is second(c2), changes)
    intrinsic_object_descriptions = _append_all(chez.map_(second, intrinsic_clauses))
    if len(intrinsic_clauses) == 0:
        intrinsic_clauses_uniformity = 1
    else:
        intrinsic_clauses_uniformity = chez.mul(
            product(chez.map_(change_abstractness_uniformity, changes_grouped_by_dimension)),
            square(object_description_uniformity(intrinsic_object_descriptions)))
    if len(extrinsic_clauses) == 0:
        extrinsic_clauses_uniformity = 1
    else:
        extrinsic_clauses_uniformity = average(chez.map_(
            compose(cube, object_description_uniformity),
            chez.map_(second, extrinsic_clauses)))
    raw_uniformity = average(intrinsic_clauses_uniformity, extrinsic_clauses_uniformity)
    cohesion_factor = chez.exp(chez.mul(5, chez.sub(raw_uniformity, 1)))
    return times_100(chez.mul(cohesion_factor,
                              average(object_description_depth_factor,
                                      change_descriptor_abstractness)))


# ------------------------------ English Transcription -------------------------------

p_maximum_rule_line_length = 60


def transcribe_to_english(rule_type, rule_clauses):
    """rules.ss: transcribe-to-english"""
    # the let*, in order
    if rule_type == "top":
        string = workspace.g_initial_string
    elif rule_type == "bottom":
        string = workspace.g_target_string
    else:
        string = None   # (case ...) without else: void
    if len(rule_clauses) == 0:
        clause_strings = [chez.String("Don't change anything")]
    else:
        # chez: map's order of application (the phrases only read; a crash in
        # one clause's phrases is the same whichever clause goes first)
        clause_strings = _append_all(chez.map_(get_rule_clause_phrases(string), rule_clauses))

    def adjusted_line_length(s):
        len_ = len(s)
        return ceiling(chez.div(len_, ceiling(chez.div(len_, p_maximum_rule_line_length))))
    adjusted_line_lengths = chez.map_(adjusted_line_length, clause_strings)
    max_line_length = maximum(adjusted_line_lengths)
    return _append_all(chez.map_(separate_into_multiple_lines(max_line_length, chez.String("  ")),
                                 clause_strings))


def separate_into_multiple_lines(max_length, indent):
    """rules.ss: separate-into-multiple-lines"""
    def separate(s):
        """rules.ss: separate-into-multiple-lines (the procedure it makes)"""
        next_pos = _metacat.general_graphics.find_next_space_position(s, max_length)
        if next_pos == len(s):
            return [s]
        return ([chez.String(s[0:next_pos])]
                + separate_into_multiple_lines(max_length, indent)(
                    chez.String(indent + string_suffix(s, next_pos))))
    return separate


def get_rule_clause_phrases(string):
    """rules.ss: get-rule-clause-phrases"""
    def phrases(rule_clause):
        """rules.ss: get-rule-clause-phrases (the procedure it makes)"""
        tag = rule_clause[0]
        if tag == "verbatim":
            letter_categories = rule_clause[1]
            return [get_verbatim_phrase(letter_categories)]
        if tag == "extrinsic":
            object_descriptions, dimensions = rule_clause[1], rule_clause[2]
            return [get_swap_phrase(object_descriptions, dimensions, string)]
        if tag == "intrinsic":
            object_descriptions, changes = rule_clause[1], rule_clause[2]
            # the let*, in order (every binding only reads)
            plural_p = plural_object_phrase_p(first(object_descriptions), string)
            object_phrase = get_object_phrase(first(object_descriptions), plural_p, string)
            ObjCtgy_or_Length_or_LettCtgy_phrases = get_ObjCtgy_or_Length_or_LettCtgy_change_phrases(
                object_phrase, plural_p, changes)
            BondFacet_change = select_change(slipnet.plato_bond_facet, "self", changes)
            other_changes = filter_out(
                ObjCtgy_or_Length_change_p,
                chez.remq(BondFacet_change, remove_LettCtgy_change_if_necessary(changes)))
            # chez: map's order of application (the phrases only read)
            other_change_phrases = chez.map_(
                get_change_phrase(object_phrase, plural_p, BondFacet_change), other_changes)
            return other_change_phrases + ObjCtgy_or_Length_or_LettCtgy_phrases
        return None     # record-case without else: void
    return phrases


def get_verbatim_phrase(letter_categories):
    """rules.ss: get-verbatim-phrase"""
    return _format("Change string to \"~a\"",
                   chez.String("".join(tell_all(letter_categories, "get-lowercase-name"))))


def get_swap_phrase(object_descriptions, dimensions, string):
    """rules.ss: get-swap-phrase"""
    # a two-binding let; both only read
    # chez: map's order of application (the phrases only read)
    objects_phrase = punctuate(chez.map_(
        lambda od: get_object_phrase(od, plural_object_phrase_p(od, string), string),
        object_descriptions))
    dimensions_phrase = punctuate(chez.map_(lambda dim: get_dimension_phrase(dim, True),
                                            dimensions))
    if len(object_descriptions) == 1:
        return _format("Swap ~a of all objects in ~a", dimensions_phrase, objects_phrase)
    return _format("Swap ~a of ~a", dimensions_phrase, objects_phrase)


def plural_object_phrase_p(object_description, string):
    """rules.ss: plural-object-phrase?"""
    return len(tell(string, "get-object-description-ref-objects", object_description)) > 1


def get_dimension_phrase(dimension, plural_p):
    """rules.ss: get-dimension-phrase"""
    name = tell(dimension, "get-name-symbol")
    plural = plural_p is not False
    if name == "plato-direction-category":
        phrase = "directions" if plural else "direction"
    elif name == "plato-group-category":
        phrase = "group-types" if plural else "group-type"
    elif name == "plato-alphabetic-position-category":
        phrase = "alphabetic-positions" if plural else "alphabetic-position"
    elif name == "plato-letter-category":
        phrase = "letter-categories" if plural else "letter-category"
    elif name == "plato-length":
        phrase = "lengths" if plural else "length"
    elif name == "plato-object-category":
        phrase = "object-types" if plural else "object-type"
    elif name == "plato-string-position-category":
        phrase = "positions" if plural else "position"
    else:
        return None     # (case ...) without else: void
    return chez.String(phrase)


def punctuate(l):
    """rules.ss: punctuate"""
    if len(l) == 0:
        return chez.String("")
    if len(l) == 1:
        return first(l)
    if len(l) == 2:
        return _format("~a and ~a", first(l), second(l))
    # chez: map's order of application (format only computes)
    parts = (chez.map_(lambda x: chez.format_("~a, ", x), all_but_last(1, l))
             + [chez.format_("and ~a", last(l))])
    return chez.String("".join(parts))


def get_object_phrase(object_description, plural_p, string):
    """rules.ss: get-object-phrase"""
    # a two-binding let; both only read
    object_type = first(object_description)
    object_descriptor = third(object_description)
    if chez.eq_p(object_type, "string") or object_descriptor is slipnet.plato_whole:
        if tell(string, "whole-group?") is not False:
            return chez.String("whole group")
        return chez.String("string")
    if slipnet.platonic_letter_p(object_descriptor) is not False:
        if plural_p is not False:
            return _format("all `~a' ~as",
                           tell(object_descriptor, "get-lowercase-name"),
                           tell(object_type, "get-lowercase-name"))
        if object_type is slipnet.plato_letter:
            return _format("letter `~a'", tell(object_descriptor, "get-lowercase-name"))
        return _format("`~a' group", tell(object_descriptor, "get-lowercase-name"))
    if plural_p is not False:
        return _format("all ~a ~as",
                       tell(object_descriptor, "get-lowercase-name"),
                       tell(object_type, "get-lowercase-name"))
    return _format("~a ~a",
                   tell(object_descriptor, "get-lowercase-name"),
                   tell(object_type, "get-lowercase-name"))


def get_change_phrase(object_phrase, plural_p, BondFacet_change):
    """rules.ss: get-change-phrase"""
    def phrase(change):
        """rules.ss: get-change-phrase (the procedure it makes)"""
        # a three-binding let; every binding only reads
        scope = first(change)
        dimension = second(change)
        descriptor = third(change)
        subobjects_p = scope == "subobjects"
        if dimension is slipnet.plato_direction_category:
            return _format("Reverse ~a of ~a~a",
                           get_dimension_phrase(dimension, plural_p is not False or subobjects_p),
                           chez.String("all objects in " if subobjects_p else ""),
                           object_phrase)
        if dimension is slipnet.plato_group_category:
            if scope == "self":
                # 1.2: (3rd BondFacet-change) is caddr of #f, a Chez error, when the
                # clause has no BondFacet change (anomalies: "`caddr` of `#f` in
                # `transcribe-to-english`")
                if _caddr(BondFacet_change) is slipnet.plato_letter_category:
                    medium_phrase = chez.String("letter-categories")
                elif _caddr(BondFacet_change) is slipnet.plato_length:
                    medium_phrase = chez.String("lengths")
                else:
                    medium_phrase = None    # cond without else: void
                return _format("Reverse starting and ending ~a of ~a",
                               medium_phrase, object_phrase)
            return _format("Reverse starting and ending points of all objects in ~a",
                           object_phrase)
        if slipnet.platonic_letter_p(descriptor) is not False:
            descriptor_phrase = _format("`~a'", tell(descriptor, "get-lowercase-name"))
        else:
            descriptor_phrase = tell(descriptor, "get-lowercase-name")
        return _format("Change ~a of ~a~a to ~a",
                       get_dimension_phrase(dimension, plural_p is not False or subobjects_p),
                       chez.String("all objects in " if subobjects_p else ""),
                       object_phrase,
                       descriptor_phrase)
    return phrase


def ObjCtgy_or_Length_change_p(c):
    """rules.ss: ObjCtgy/Length-change?"""
    return second(c) is slipnet.plato_object_category or second(c) is slipnet.plato_length


# Form of ObjCtgy/Length/LettCtgy-change-phrases for all possible combinations of
# ObjCtgy and Length changes:
#
# (*) denotes cases in which LettCtgy change may be incorporated into the phrase.
#
# <object-phrase> may be either singular or plural.
# (Example: "rightmost group" versus "all `c' groups")
#
# --------------------------------------------------------------------------------------
# <ObjCtgy:self>   <Length:self>
# if ObjCtgy:self = letter
# (*)"Change <object-phrase> to (a) letter(s) {<LettCtgy:self>}"
# if ObjCtgy:self = group & Length:self is relation
#    "Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
# if ObjCtgy:self = group & Length:self is literal
# (*)"Change <object-phrase> to (a) {<LettCtgy:self>} group(s) of length <Length:self>"
# --------------------------------------------------------------------------------------
# <ObjCtgy:self>
# (*)"Change <object-phrase> to (a) {<LettCtgy:self>} <ObjCtgy:self>"
# --------------------------------------------------------------------------------------
# <ObjCtgy:subs>   <Length:self>   <Length:subs>
# if ObjCtgy:subs = letter
# (*)"Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
#    "Change all objects in <object-phrase> to (a) letter(s) {<LettCtgy:subs>}"
# if ObjCtgy:subs = group & Length:subs is relation
#    "Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
#    "Change/Increase/Decrease lengths of all objects in <object-phrase>
#       to/by <Length:subs>/one"
# if ObjCtgy:subs = group & Length:subs is literal
# (*)"Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
#    "Change all objects in <object-phrase> to (a) {<LettCtgy:subs>} group(s)
#       of length <Length:subs>"
# --------------------------------------------------------------------------------------
# <ObjCtgy:subs>   <Length:self>
# (*)"Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
#    "Change all objects in <object-phrase> to (a) {<LettCtgy:subs>} <ObjCtgy:subs>"
# --------------------------------------------------------------------------------------
# <ObjCtgy:subs>   <Length:subs>
# if ObjCtgy:subs = letter
# (*)"Change all objects in <object-phrase> to (a) letter(s) {<LettCtgy:subs>}"
# if ObjCtgy:subs = group & Length:subs is relation
#    "Change/Increase/Decrease lengths of all objects in <object-phrase>
#       to/by <Length:subs>/one"
# if ObjCtgy:subs = group & Length:subs is literal
# (*)"Change all objects in <object-phrase> to (a) {<LettCtgy:subs>} group(s)
#       of length <Length:subs>"
# --------------------------------------------------------------------------------------
# <ObjCtgy:subs>
# (*)"Change all objects in <object-phrase> to (a) {<LettCtgy:subs>} <ObjCtgy:subs>"
# --------------------------------------------------------------------------------------
# <Length:self>   <Length:subs>
#    "Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
#    "Change/Increase/Decrease lengths of all objects in <object-phrase>
#       to/by <Length:subs>/one"
# --------------------------------------------------------------------------------------
# <Length:self>
#    "Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
# --------------------------------------------------------------------------------------
# <Length:subs>
#    "Change/Increase/Decrease lengths of all objects in <object-phrase>
#       to/by <Length:subs>/one"
# --------------------------------------------------------------------------------------

def get_ObjCtgy_or_Length_or_LettCtgy_change_phrases(object_phrase, plural_p, changes):
    """rules.ss: get-ObjCtgy/Length/LettCtgy-change-phrases"""
    def present_p(*l):
        return chez.andmap(exists_p, list(l))

    # the let*, in order (every binding only reads)
    ObjCtgy__self = select_descriptor(slipnet.plato_object_category, "self", changes)
    ObjCtgy__subs = select_descriptor(slipnet.plato_object_category, "subobjects", changes)
    Length__self = select_descriptor(slipnet.plato_length, "self", changes)
    Length__subs = select_descriptor(slipnet.plato_length, "subobjects", changes)
    LettCtgy__self = select_descriptor(slipnet.plato_letter_category, "self", changes)
    LettCtgy__subs = select_descriptor(slipnet.plato_letter_category, "subobjects", changes)
    if present_p(ObjCtgy__self, Length__self) is not False:
        if ObjCtgy__self is slipnet.plato_letter:
            return [change_to_object_type(object_phrase, plural_p, slipnet.plato_letter,
                                          LettCtgy__self, False)]
        if slipnet.platonic_relation_p(Length__self) is not False:
            return [change_length_of(object_phrase, plural_p, Length__self, False)]
        return [change_to_group_of_length(object_phrase, plural_p, Length__self,
                                          LettCtgy__self, False)]
    if present_p(ObjCtgy__self) is not False:
        return [change_to_object_type(object_phrase, plural_p, ObjCtgy__self,
                                      LettCtgy__self, False)]
    if present_p(ObjCtgy__subs, Length__self, Length__subs) is not False:
        # `(,a ,b): list's arguments; both only compute
        phrase1 = change_length_of(object_phrase, plural_p, Length__self, False)
        if ObjCtgy__subs is slipnet.plato_letter:
            phrase2 = change_to_object_type(object_phrase, plural_p, slipnet.plato_letter,
                                            LettCtgy__subs, True)
        elif slipnet.platonic_relation_p(Length__subs) is not False:
            phrase2 = change_length_of(object_phrase, plural_p, Length__subs, True)
        else:
            phrase2 = change_to_group_of_length(object_phrase, plural_p, Length__subs,
                                                LettCtgy__subs, True)
        return [phrase1, phrase2]
    if present_p(ObjCtgy__subs, Length__self) is not False:
        return [change_length_of(object_phrase, plural_p, Length__self, False),
                change_to_object_type(object_phrase, plural_p, ObjCtgy__subs,
                                      LettCtgy__subs, True)]
    if present_p(ObjCtgy__subs, Length__subs) is not False:
        if ObjCtgy__subs is slipnet.plato_letter:
            return [change_to_object_type(object_phrase, plural_p, slipnet.plato_letter,
                                          LettCtgy__subs, True)]
        if slipnet.platonic_relation_p(Length__subs) is not False:
            return [change_length_of(object_phrase, plural_p, Length__subs, True)]
        return [change_to_group_of_length(object_phrase, plural_p, Length__subs,
                                          LettCtgy__subs, True)]
    if present_p(ObjCtgy__subs) is not False:
        return [change_to_object_type(object_phrase, plural_p, ObjCtgy__subs,
                                      LettCtgy__subs, True)]
    if present_p(Length__self, Length__subs) is not False:
        return [change_length_of(object_phrase, plural_p, Length__self, False),
                change_length_of(object_phrase, plural_p, Length__subs, True)]
    if present_p(Length__self) is not False:
        return [change_length_of(object_phrase, plural_p, Length__self, False)]
    if present_p(Length__subs) is not False:
        return [change_length_of(object_phrase, plural_p, Length__subs, True)]
    return []


def remove_LettCtgy_change_if_necessary(changes):
    """rules.ss: remove-LettCtgy-change-if-necessary"""
    ObjCtgy_change = select_change(slipnet.plato_object_category, False, changes)
    if not exists_p(ObjCtgy_change):
        return changes
    # a two-binding let; both only read
    same_scope_LettCtgy_change = select_change(slipnet.plato_letter_category,
                                               first(ObjCtgy_change), changes)
    same_scope_Length_change = select_change(slipnet.plato_length,
                                             first(ObjCtgy_change), changes)
    if (exists_p(same_scope_LettCtgy_change)
            and slipnet.platonic_literal_p(third(same_scope_LettCtgy_change)) is not False
            and (not exists_p(same_scope_Length_change)
                 or slipnet.platonic_literal_p(third(same_scope_Length_change)) is not False
                 or third(ObjCtgy_change) is slipnet.plato_letter)):
        return chez.remq(same_scope_LettCtgy_change, changes)
    return changes


def select_descriptor(dimension, scope, changes):
    """rules.ss: select-descriptor"""
    change = select_change(dimension, scope, changes)
    if exists_p(change):
        return third(change)
    return False


def select_change(dimension, scope, changes):
    """rules.ss: select-change"""
    def matches_p(c):
        if exists_p(scope):
            return chez.eq_p(first(c), scope) and second(c) is dimension
        return second(c) is dimension
    return select(matches_p, changes)


def change_to_object_type(object_phrase, plural_p, object_type, LettCtgy_descriptor,
                          subobjects_p):
    """rules.ss: change-to-object-type"""
    return _format("Change ~a~a to ~a",
                   chez.String("all objects in " if subobjects_p is not False else ""),
                   object_phrase,
                   new_object_phrase(object_type, LettCtgy_descriptor,
                                     plural_p is not False or subobjects_p is not False))


def change_to_group_of_length(object_phrase, plural_p, length, LettCtgy_descriptor,
                              subobjects_p):
    """rules.ss: change-to-group-of-length"""
    return _format("Change ~a~a to ~a of length ~a",
                   chez.String("all objects in " if subobjects_p is not False else ""),
                   object_phrase,
                   new_object_phrase(slipnet.plato_group, LettCtgy_descriptor,
                                     plural_p is not False or subobjects_p is not False),
                   tell(length, "get-lowercase-name"))


# (define new-object-phrase (let ((an-letters (list plato-a ...))) (lambda ...))):
# the list is made by load(), once the slipnet's nodes exist
_an_letters = None


def new_object_phrase(new_object_type, LettCtgy_descriptor, plural_p):
    """rules.ss: new-object-phrase"""
    if new_object_type is slipnet.plato_letter:
        if (exists_p(LettCtgy_descriptor)
                and slipnet.platonic_literal_p(LettCtgy_descriptor) is not False):
            return _format("the letter `~a'", tell(LettCtgy_descriptor, "get-lowercase-name"))
        return chez.String("letters" if plural_p is not False else "a letter")
    if new_object_type is slipnet.plato_group:
        if (exists_p(LettCtgy_descriptor)
                and slipnet.platonic_literal_p(LettCtgy_descriptor) is not False):
            if plural_p is not False:
                return _format("`~a' groups", tell(LettCtgy_descriptor, "get-lowercase-name"))
            return _format("~a `~a' group",
                           chez.String("an" if member_p(LettCtgy_descriptor, _an_letters)
                                       else "a"),
                           tell(LettCtgy_descriptor, "get-lowercase-name"))
        return chez.String("groups" if plural_p is not False else "a group")
    return None     # cond without else: void


def change_length_of(object_phrase, plural_p, length, subobjects_p):
    """rules.ss: change-length-of"""
    plural = plural_p is not False or subobjects_p is not False
    in_phrase = chez.String("all objects in " if subobjects_p is not False else "")
    if length is slipnet.plato_successor:
        return _format("Increase ~a of ~a~a by one",
                       get_dimension_phrase(slipnet.plato_length, plural),
                       in_phrase, object_phrase)
    if length is slipnet.plato_predecessor:
        return _format("Decrease ~a of ~a~a by one",
                       get_dimension_phrase(slipnet.plato_length, plural),
                       in_phrase, object_phrase)
    return _format("Change ~a of ~a~a to ~a",
                   get_dimension_phrase(slipnet.plato_length, plural),
                   in_phrase, object_phrase,
                   tell(length, "get-lowercase-name"))


# ----------------------------------- Rule Printing -----------------------------------

def printf_object_description(object_description):
    """rules.ss: printf-object-description"""
    return chez.printf("~a~n", format_object_description(object_description))


def printf_rule_clause_template(rule_clause_template):
    """rules.ss: printf-rule-clause-template"""
    tag = rule_clause_template[0]
    if tag == "intrinsic":
        ref_obj, change_templates = rule_clause_template[1], rule_clause_template[2]
        chez.printf("intrinsic ~a~n", tell(ref_obj, "ascii-name"))
        result = None
        for ct in change_templates:
            result = chez.printf("   ~a~n", format_change_template(ct))
        return result
    if tag == "extrinsic":
        ref_objs, dimensions = rule_clause_template[1], rule_clause_template[2]
        return chez.printf("extrinsic ~a ~a~n",
                           tell_all(ref_objs, "ascii-name"),
                           chez.map_(format_slipnode, dimensions))
    return None     # record-case without else: void


def printf_rule_clause(rule_clause):
    """rules.ss: printf-rule-clause"""
    tag = rule_clause[0]
    if tag == "verbatim":
        letter_categories = rule_clause[1]
        return chez.printf("VERBATIM ~a~n", tell_all(letter_categories, "get-lowercase-name"))
    if tag == "intrinsic":
        object_descriptions, changes = rule_clause[1], rule_clause[2]
        chez.printf("CHANGE ~a~n", format_object_description(first(object_descriptions)))
        result = None
        for change in changes:
            result = chez.printf("   (~a ~a ~a)~n",
                                 first(change),
                                 format_slipnode(second(change)),
                                 format_slipnode(third(change)))
        return result
    if tag == "extrinsic":
        object_descriptions, dimensions = rule_clause[1], rule_clause[2]
        names = [format_slipnode(first(dimensions))] + chez.map_(
            lambda dim: chez.format_(", ~a", format_slipnode(dim)), rest(dimensions))
        chez.printf("SWAP ~a of~n", chez.String("".join(names)))
        if len(object_descriptions) == 1:
            return chez.printf("   subobjects ~a~n",
                               format_object_description(first(object_descriptions)))
        result = None
        for obj_desc in object_descriptions:
            result = chez.printf("   ~a~n", format_object_description(obj_desc))
        return result
    return None     # record-case without else: void


def format_change_template(ct):
    """rules.ss: format-change-template"""
    return _format("(~a ~a ~a)",
                   first(ct),
                   format_slipnode(second(ct)),
                   chez.map_(format_slipnode, third(ct)))


def format_object_description(od):
    """rules.ss: format-object-description"""
    return _format("(~a ~a ~a)",
                   "string" if chez.eq_p(first(od), "string") else format_slipnode(first(od)),
                   format_slipnode(second(od)),
                   format_slipnode(third(od)))


def format_slipnode(node):
    """rules.ss: format-slipnode"""
    return _format("<~a>", tell(node, "get-short-name"))


def load():
    """rules.ss: the top-level forms that need other modules, in the file's order:
    the three define-codelet-procedure* forms, *rule-dimension-order*,
    new-object-phrase's list of "an" letters, and format-slipnode as a top-level
    value (utilities.reveal_obj reads it)."""
    global g_rule_dimension_order, _an_letters
    sugar.define_codelet_procedure_star("rule-scout", rule_scout)
    sugar.define_codelet_procedure_star("rule-evaluator", rule_evaluator)
    sugar.define_codelet_procedure_star("rule-builder", rule_builder)
    g_rule_dimension_order = [
        slipnet.plato_direction_category,
        slipnet.plato_group_category,
        slipnet.plato_alphabetic_position_category,
        slipnet.plato_letter_category,
        slipnet.plato_length,
        slipnet.plato_object_category,
        slipnet.plato_string_position_category,
        slipnet.plato_bond_facet]
    _an_letters = [slipnet.plato_a, slipnet.plato_e, slipnet.plato_f, slipnet.plato_h,
                   slipnet.plato_i, slipnet.plato_l, slipnet.plato_m, slipnet.plato_n,
                   slipnet.plato_o, slipnet.plato_r, slipnet.plato_s, slipnet.plato_x]
    if "format-slipnode" not in chez.TOP_LEVEL:
        chez.define_top_level_value("format-slipnode", format_slipnode)
