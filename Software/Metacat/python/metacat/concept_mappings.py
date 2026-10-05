"""concept-mappings.ss: concept mappings (the identities and slippages of bridges).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from concept-mappings.ss, with
racket/engine/concept-mappings.rktl as a worked translation.

make-concept-mapping's closure is the ConceptMapping class
(docs/python-translation-plan.md, "Objects"); it delegates what it doesn't
answer to base-object.  print-name, english-name and long-name are Scheme
strings (chez.String).  object1 may be the symbol coattail (bridges.ss's
coattail slippages), so it is compared with chez.eq_p.  Strength and
slippability stay exact unless a flonum degree of association or depth enters;
rounding is utilities.round_.  Nothing here draws or depends on evaluation
order (the let* is followed in order anyway).  The engine never imports
tkinter.
"""
from __future__ import annotations

from metacat import chez
from metacat import slipnet
from metacat.chez import String
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (average, base_object, compress, exists_p, one_minus, percent,
                               remove_duplicates_pred, round_, select, square)


class ConceptMapping(SchemeObject):
    """concept-mappings.ss: make-concept-mapping (the closure)"""
    __slots__ = ("object1", "description_type1", "descriptor1",
                 "object2", "description_type2", "descriptor2",
                 "label", "identity", "slipnet_link", "print_name_", "english_name_",
                 "graphics_pexp", "previously_relevant")

    def __init__(this, object1, description_type1, descriptor1,
                 object2, description_type2, descriptor2):
        this.object1 = object1
        this.description_type1 = description_type1
        this.descriptor1 = descriptor1
        this.object2 = object2
        this.description_type2 = description_type2
        this.descriptor2 = descriptor2
        # the let*, in order
        this.label = slipnet.get_label(descriptor1, descriptor2)
        this.identity = descriptor1 is descriptor2
        # slipnet-link is usually a lateral-sliplink; however, it can be a
        # lateral-link in the case of LettCtgy/Length pred/succ "slippages"
        # such as LettCtgy:a=(succ)=>b or Length:two=(pred)=>one:
        if this.identity:
            this.slipnet_link = False
        else:
            this.slipnet_link = select(
                lambda link: tell(link, "get-to-node") is descriptor2,
                tell(descriptor1, "get-lateral-links")
                if (description_type1 is slipnet.plato_letter_category
                    or description_type1 is slipnet.plato_length)
                else tell(descriptor1, "get-lateral-sliplinks"))
        this.print_name_ = String(tell(descriptor1, "get-CM-short-name")
                                  + "=>" + tell(descriptor2, "get-CM-short-name"))
        this.english_name_ = String(tell(descriptor1, "get-lowercase-name") + " <=> "
                                    + tell(descriptor2, "get-lowercase-name"))
        this.graphics_pexp = False
        this.previously_relevant = False

    @message("object-type")
    def object_type(this, self):
        return "concept-mapping"

    @message("get-object1")
    def get_object1(this, self):
        return this.object1

    @message("get-object2")
    def get_object2(this, self):
        return this.object2

    @message("print-name")
    def print_name(this, self):
        return this.print_name_

    @message("english-name")
    def english_name(this, self):
        return this.english_name_

    @message("long-name")
    def long_name(this, self):
        if this.identity:
            kind = "CM"
        elif chez.eq_p(this.object1, "coattail"):
            kind = "COATTAIL SLIPPAGE"
        else:
            kind = "SLIPPAGE"
        label = this.label
        if not exists_p(label) or label is slipnet.plato_identity:
            arrow = "=>"
        else:
            arrow = chez.format_("=(~a)=>", tell(label, "get-short-name"))
        return String(chez.format_("~a ~a:~a~a~a",
                                   kind,
                                   tell(this.description_type1, "get-short-name"),
                                   tell(this.descriptor1, "get-short-name"),
                                   arrow,
                                   tell(this.descriptor2, "get-short-name")))

    @message("print")
    def print_(this, self):
        return chez.printf("~a~n", tell(self, "long-name"))

    @message("get-graphics-pexp")
    def get_graphics_pexp(this, self):
        return this.graphics_pexp

    @message("set-graphics-pexp")
    def set_graphics_pexp(this, self, pexp):
        this.graphics_pexp = pexp
        return "done"

    @message("previously-relevant?")
    def previously_relevant_p(this, self):
        return this.previously_relevant

    @message("update-previously-relevant?")
    def update_previously_relevant_p(this, self, new_value):
        this.previously_relevant = new_value
        return "done"

    # could use description-type2 here instead:
    @message("get-CM-type")
    def get_CM_type(this, self):
        return this.description_type1

    @message("get-slipnet-link")
    def get_slipnet_link(this, self):
        return this.slipnet_link

    @message("CM-type?")
    def CM_type_p(this, self, description_type):
        return this.description_type1 is description_type

    @message("bond-concept-mapping?")
    def bond_concept_mapping_p(this, self):
        return (this.description_type1 is slipnet.plato_bond_category
                or this.description_type1 is slipnet.plato_bond_facet)

    # Used in deciding whether to re-perceive spanning groups as flipped:
    @message("reversible-CM-type?")
    def reversible_CM_type_p(this, self):
        return (this.description_type1 is slipnet.plato_direction_category
                or this.description_type1 is slipnet.plato_bond_category
                or this.description_type1 is slipnet.plato_group_category)

    @message("get-descriptor1")
    def get_descriptor1(this, self):
        return this.descriptor1

    @message("get-descriptor2")
    def get_descriptor2(this, self):
        return this.descriptor2

    @message("get-label")
    def get_label(this, self):
        return this.label

    # Previously defined as (exists? sliplink).  However, this
    # fails to classify a=(succ)=>b type CMs as slippages.
    # Since CMs can now be of the form LettCtgy:a=(succ)=>b
    # (but only in a horizontal-bridge's concept-mappings list),
    # defining 'slippage? as (exists? sliplink) won't suffice.
    # (not identity?) will classify CMs as slippages whenever
    # desc1 != desc2.  This includes ObjCtgy:let=>grp slippages
    # too (i.e., non-labeled concept-mappings):
    @message("slippage?")
    def slippage_p(this, self):
        return not this.identity

    @message("identity?")
    def identity_p(this, self):
        return this.identity

    @message("opposite-mapping?")
    def opposite_mapping_p(this, self):
        return this.label is slipnet.plato_opposite

    @message("identity/opposite-mapping?")
    def identity_or_opposite_mapping_p(this, self):
        return (this.label is slipnet.plato_identity
                or this.label is slipnet.plato_opposite
                or (this.descriptor1 is slipnet.plato_whole
                    and this.descriptor2 is slipnet.plato_single)
                or (this.descriptor1 is slipnet.plato_single
                    and this.descriptor2 is slipnet.plato_whole))

    @message("relevant?")
    def relevant_p(this, self):
        return (slipnet.fully_active_p(this.description_type1)
                and slipnet.fully_active_p(this.description_type2))

    @message("distinguishing?")
    def distinguishing_p(this, self):
        # chez: (and ...) returns the last test's value, which need not be a boolean
        if this.identity and this.descriptor1 is slipnet.plato_whole:
            return False
        if tell(this.object1, "distinguishing-descriptor?", this.descriptor1) is False:
            return False
        return tell(this.object2, "distinguishing-descriptor?", this.descriptor2)

    @message("relevant-distinguishing?")
    def relevant_distinguishing_p(this, self):
        # chez: #f only
        if tell(self, "relevant?") is False:
            return False
        return tell(self, "distinguishing?")

    @message("distinguishing-identity/opposite?")
    def distinguishing_identity_or_opposite_p(this, self):
        # chez: #f only
        if tell(self, "distinguishing?") is False:
            return False
        return tell(self, "identity/opposite-mapping?")

    @message("get-degree-of-assoc")
    def get_degree_of_assoc(this, self):
        if this.identity:
            return 100
        if exists_p(this.slipnet_link):
            return tell(this.slipnet_link, "get-degree-of-assoc")
        # Else the CM is an unlabeled LettCtgy/Length "slippage"
        # such as LettCtgy:m=>j or Length:one=>three:
        return 5

    @message("get-conceptual-depth")
    def get_conceptual_depth(this, self):
        return average(tell(this.descriptor1, "get-conceptual-depth"),
                       tell(this.descriptor2, "get-conceptual-depth"))

    @message("get-strength")
    def get_strength(this, self):
        degree_of_assoc = tell(self, "get-degree-of-assoc")
        if degree_of_assoc == 100:
            return 100
        return round_(chez.mul(degree_of_assoc,
                               chez.add(1, square(percent(tell(self, "get-conceptual-depth"))))))

    @message("get-slippability")
    def get_slippability(this, self):
        degree_of_assoc = tell(self, "get-degree-of-assoc")
        if degree_of_assoc == 100:
            return 100
        return round_(chez.mul(degree_of_assoc,
                               one_minus(square(percent(tell(self, "get-conceptual-depth"))))))

    @message("get-concept-pattern")
    def get_concept_pattern(this, self):
        max_activation = slipnet.p_max_activation
        return compress(
            ["concepts",
             [this.description_type1, max_activation],
             [this.descriptor1, max_activation],
             [this.descriptor2, max_activation],
             [this.label, max_activation] if exists_p(this.label) else False])

    @message("symmetric?")
    def symmetric_p(this, self, cm):
        return (tell(cm, "get-descriptor1") is this.descriptor2
                and tell(cm, "get-descriptor2") is this.descriptor1)

    @message("symmetric-mapping")
    def symmetric_mapping(this, self):
        if this.identity:
            return self
        return make_concept_mapping(
            this.object1, this.description_type2, this.descriptor2,
            this.object2, this.description_type1, this.descriptor1)

    @message("activate-descriptions")
    def activate_descriptions(this, self):
        tell(this.description_type1, "activate-from-workspace")
        tell(this.descriptor1, "activate-from-workspace")
        tell(this.description_type2, "activate-from-workspace")
        return tell(this.descriptor2, "activate-from-workspace")

    @message("activate-label")
    def activate_label(this, self):
        if exists_p(this.label):
            tell(this.label, "activate-from-workspace")
            # Do this so that the activation of the label node will show up
            # in the trace before the concept-mapping that caused it:
            tell(this.label, "flush-activation-buffer")
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_concept_mapping(object1, description_type1, descriptor1,
                         object2, description_type2, descriptor2):
    """concept-mappings.ss: make-concept-mapping"""
    return ConceptMapping(object1, description_type1, descriptor1,
                          object2, description_type2, descriptor2)


def CMs_equal_p(cm1, cm2):
    """concept-mappings.ss: CMs-equal?"""
    return (tell(cm1, "get-descriptor1") is tell(cm2, "get-descriptor1")
            and tell(cm1, "get-descriptor2") is tell(cm2, "get-descriptor2"))


# concept-mappings.ss: remove-duplicate-CMs.  As in Chez, the value of
# CMs-equal? is taken when this is defined.
remove_duplicate_CMs = remove_duplicates_pred(CMs_equal_p)
