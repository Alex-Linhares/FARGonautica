"""Port of lib/SThought/SObject.pm: SThought::Seqsee::Anchored (focusing on a group) and
SThought::Seqsee::Element (focusing on a single element).

The fringe comes from the multimethod ``get_fringe_for``, dispatched on the core's
effective object for a group (``GetEffectiveObject``), so a group with an active metonym
gets the fringe of its starred element. Elements get no actions, but thinking about them
still spikes, spreads activation and draws (with the LTM feature).

The module functions ``strengthen_link``, ``extend_from_memory``,
``add_categories_from_memory`` and ``is_this_a_mountain_upslope`` are Perl's
``SThought::Seqsee::Anchored::StrengthenLink``, ``ExtendFromMemory``,
``AddCategoriesFromMemory`` and ``IsThisAMountainUpslope``.

PERL-QUIRKs (oracle-confirmed):
- ExtendFromMemory falls off its if/elsif when the LTM_expt feature is off, so it returns
  the feature's value (undef or 0): a group's get_actions then holds a non-codelet entry.
  The port returns ``[Global.Feature.get("LTM_expt")]`` there.
- SThought::Seqsee::Element's get_actions calls ExtendFromMemory and
  AddCategoriesFromMemory but throws their codelets away; it always returns nothing.
- get_actions computes a DoTheSameThing urgency (reading the transform's activation, which
  can add an LTM node) but launches the codelet with urgency 100.
- SRelation's SuggestCategoryForEnds is always empty, so the "is not anchored" check and
  its CheckIfInstance codelets never run.
"""
import sys

from seqsee import global_ as Global
from seqsee import s as S
from seqsee import slink_activation as sla
from seqsee import sltm, sworkspace, util
from seqsee.constants import DIR, METO_MODE
from seqsee.errors import Confess
from seqsee.multimethods import Multimethod, perl_isa
from seqsee.scodelet import SCodelet
from seqsee.sthought import SThought

get_fringe_for = Multimethod("get_fringe_for")


@get_fringe_for.variant("Seqsee::Anchored")
def _get_fringe_for_anchored(core):
    ret = []
    core.get_structure()
    ret.append([core.get_pure(), 100])

    rel = core.get_underlying_reln()
    if util.perl_true(rel):
        ret.append([rel.get_rule().get_transform(), 50])

    for category in list(core.get_categories()):
        ret.append([category, 100])
        sltm.spike_by(5, category)

        bindings = core.get_binding_for_category(category)
        meto_mode = bindings.get_metonymy_mode()
        if meto_mode is not METO_MODE.NONE:
            ret.append([bindings.get_position(), 100])
            ret.append([meto_mode, 100])
            ret.append([bindings.get_metonymy_type(), 100])
    return ret


@get_fringe_for.variant("Seqsee::Element")
def _get_fringe_for_element(core):
    from seqsee.categories import prime
    from seqsee.sltm_platonic import SLTMPlatonic
    mag = core.get_mag()
    ret = []

    for cat in list(core.get_categories()):
        if cat is S.NUMBER:
            continue
        sltm.spike_by(10, cat)
        ret.append([cat, 80])

        if cat is S.PRIME:
            nxt = prime.next_prime(mag)
            prev = prime.previous_prime(mag)
            if util.perl_true(nxt):
                ret.append([SLTMPlatonic.create(nxt), 100])
            if util.perl_true(prev):
                ret.append([SLTMPlatonic.create(prev), 100])

    m = util.perl_num(mag)
    literal_cats = [SLTMPlatonic.create(m + d) for d in (0, 1, -1)]
    ret.append([literal_cats[0], 100])
    ret.append([literal_cats[1], 30])
    ret.append([literal_cats[-1], 30])

    pos = util.perl_num(core.get_left_edge())
    ret.append([f"absolute_position_{util.perl_str(pos)}", 80])
    ret.append([f"absolute_position_{util.perl_str(pos - 1)}", 20])
    ret.append([f"absolute_position_{util.perl_str(pos + 1)}", 20])
    return ret


def strengthen_link(o1, o2):
    """Perl: StrengthenLink($o1, $o2): with a relation between them, spike ISA links from
    both to the relation's category and a FOLLOWS link from the category to the relation.
    Returns the last spike's value, or None without a relation."""
    relation = o1.get_relation(o2)
    if not util.perl_true(relation):
        return None
    category = relation.get_type().get_category()
    sla.spike(sltm.insert_isa_link(o1, category), 10)
    sla.spike(sltm.insert_isa_link(o2, category), 10)
    return sla.spike(sltm.insert_follows_link(category, relation), 15)


def extend_from_memory(core):
    """Perl: ExtendFromMemory($core): a list (see the module docstring for the odd
    fall-through value)."""
    from seqsee.errors import ElementsBeyondKnownSought
    actions = []
    flush_right = core.is_flush_right()
    flush_left = core.is_flush_left()

    weighted_set = sltm.find_active_followers(core)
    if not util.perl_true(weighted_set.is_not_empty()):
        return []
    chosen_follower = weighted_set.choose()

    if util.perl_true(flush_right) and (util.perl_true(flush_left)
                                        or util.perl_num(sworkspace.ElementCount) <= 3):
        exception = ElementsBeyondKnownSought(next_elements=chosen_follower.get_flattened())
        exception.ask()
        return []
    ltm_expt = Global.Feature.get("LTM_expt")
    if util.perl_true(ltm_expt):
        nxt = sworkspace.get_something_like({
            "object": chosen_follower,
            "start": util.perl_num(core.get_right_edge()) + 1,
            "direction": DIR.RIGHT,
            "trust_level": 50,
        })
        if not util.perl_true(nxt):
            return []
        actions.append(SCodelet("FindIfRelated", 1000, {"a": core, "b": nxt}))
        return actions
    # PERL-QUIRK: falls off the elsif, returning the condition's value.
    return [ltm_expt]


def add_categories_from_memory(core):
    """Perl: AddCategoriesFromMemory($core): maybe a CheckIfInstance codelet for an active
    remembered category (activation at least 0.3)."""
    actions = []
    weighted_set = sltm.find_active_categories(core)
    weighted_set.delete_below_threshold(0.3)
    if util.perl_true(weighted_set.is_not_empty()):
        category = weighted_set.choose()
        actions.append(SCodelet("CheckIfInstance", 100, {"obj": core, "cat": category}))
    return actions


def is_this_a_mountain_upslope(core):
    """Perl: IsThisAMountainUpslope($core): the descending group with as many parts that
    starts with core's last part, or 0."""
    if not util.perl_true(core.is_of_category_p(S.ASCENDING)):
        return 0
    parts = list(core)
    if len(parts) == 1:
        return 0
    last_part = parts[-1]
    left_end_of_last_part = last_part.get_left_edge()
    groups_starting_here = sworkspace.get_objects_with_ends_exactly(
        util.perl_num(left_end_of_last_part), None)
    possible_downslopes = [
        g for g in groups_starting_here
        if util.perl_true(g.is_of_category_p(S.DESCENDING))
        and len(g) == len(parts) and g[0] is last_part
    ]
    if not possible_downslopes:
        return 0
    return possible_downslopes[0]   # There can only be 1.


class SThoughtSeqseeAnchored(SThought):
    """Perl: package SThought::Seqsee::Anchored (extends SThought)."""

    perl_name = "SThought::Seqsee::Anchored"
    NAME = "Focusing on a Group"

    def get_fringe(self):
        return get_fringe_for.call(self.core().get_effective_object())

    def get_actions(self):
        core = self.core()
        feature = Global.Feature
        actions = []
        if util.perl_true(feature.get("LTM")):
            sltm.spike_by(10, core)

        core.get_metonym()
        core.get_metonym_activeness()
        core.get_strength()
        core.is_flush_right()
        flush_left = core.is_flush_left()
        element_count = util.perl_num(sworkspace.ElementCount)
        if element_count == 0:
            raise Confess("Illegal division by zero")
        span_fraction = util.perl_num(core.get_span()) / element_count
        underlying_reln = core.get_underlying_reln()
        parts_count = len(core)

        if util.perl_true(flush_left) or util.toss(0.8):
            actions.append(SCodelet("AttemptExtensionOfGroup", 80,
                                    {"object": core, "direction": DIR.RIGHT}))

        if not util.perl_true(flush_left):
            actions.append(SCodelet("AttemptExtensionOfGroup", 150,
                                    {"object": core, "direction": DIR.LEFT}))

        if len(core) > 1 and util.toss(0.8):
            squinting = util.perl_true(feature.get("AllowSquinting"))
            if util.toss(0.5):
                urgency = 200 if squinting and \
                    not util.perl_true(core[-1].get_metonym_activeness()) else 10
                actions.append(SCodelet("ConvulseEnd", urgency,
                                        {"object": core, "direction": DIR.RIGHT}))
            else:
                urgency = 200 if squinting and \
                    not util.perl_true(core[0].get_metonym_activeness()) else 10
                actions.append(SCodelet("ConvulseEnd", urgency,
                                        {"object": core, "direction": DIR.LEFT}))

        if util.perl_true(feature.get("LTM")):
            # Spread activation from corresponding node:
            sltm.spread_activation_from(sltm.get_memory_index(core))
            actions.extend(extend_from_memory(core))
            actions.extend(add_categories_from_memory(core))

        poss_cat = None
        first_reln = core[0].get_relation(core[1] if len(core) > 1 else None)
        if util.perl_true(first_reln):
            poss_cat = first_reln.suggest_category()
        if util.perl_true(poss_cat):
            is_inst = core.is_of_category_p(poss_cat)
            if not util.perl_true(is_inst):
                actions.append(SCodelet("CheckIfInstance", 500, {"obj": core, "cat": poss_cat}))

        possible_category_for_ends = None
        if util.perl_true(first_reln):
            possible_category_for_ends = first_reln.suggest_category_for_ends()
        if util.perl_true(possible_category_for_ends):
            for item in list(core.get_underlying_reln().get_items()):
                if not perl_isa(item, "Seqsee::Anchored"):
                    items_text = "; ".join(util.perl_str(x) for x in core)
                    ruleapp_text = "; ".join(
                        util.perl_str(x) for x in core.get_underlying_reln().get_items())
                    sys.stdout.write(
                        f"An item of an Seqsee::Anchored object({util.perl_str(core)}) "
                        "is not anchored!\n"
                        f"The anchored object is {core.get_structure_string()}\n"
                        f"Its items are: {items_text}"
                        f"Items of the underlying ruleapp are: {ruleapp_text}")
                    raise Confess(f"{util.perl_str(item)} is not anchored!")
                is_inst = item.is_of_category_p(possible_category_for_ends)
                if not util.perl_true(is_inst):
                    actions.append(SCodelet("CheckIfInstance", 100,
                                            {"obj": item, "cat": possible_category_for_ends}))

        if span_fraction > 0.5:
            actions.append(SCodelet("LargeGroup", 100, {"group": core}))

        if util.perl_true(feature.get("LTM")):
            if parts_count >= 3:
                for i in range(parts_count - 1):
                    strengthen_link(core[i], core[i + 1])

        categories = list(core.get_categories())
        category = sltm.spike_and_choose(0, *categories)
        if util.perl_true(category) and util.toss(0.5 * sltm.spike_by(5, category)):
            actions.append(SCodelet("FocusOn", 100, {"what": category}))

        actions.append(SCodelet("LookForSimilarGroups", 20, {"group": core}))
        actions.append(SCodelet("CleanUpGroup", 20, {"group": core}))

        if util.perl_true(underlying_reln):
            transform = underlying_reln.get_rule().get_transform()
            if perl_isa(transform, "Mapping::Structural") and \
                    not perl_isa(transform.get_category(), "SCategory::Interlaced"):
                sltm.get_real_activations_for_one_concept(transform)   # the unused urgency
                actions.append(SCodelet("DoTheSameThing", 100, {"transform": transform}))

        downslope = is_this_a_mountain_upslope(core)
        if util.perl_true(downslope):
            upslope = list(core)
            downslope_parts = list(downslope)[1:]
            mountain_activation = sltm.spike_by(20, S.MOUNTAIN)
            if util.significant(mountain_activation) and util.toss(mountain_activation):
                actions.append(SCodelet("CreateGroup", 100, {
                    "items": [*upslope, *downslope_parts],
                    "category": S.MOUNTAIN,
                }))

        if util.perl_true(feature.get("AllowSquinting")) and core.is_this_a_metonymed_object():
            pass

        if util.perl_true(feature.get("Alternating")):
            # Look for adjacent objects. If all 3 belong to the some common category, check
            # for alternatingness.
            left_neighbour = sworkspace.choose_by_strength(
                *sworkspace.get_objects_with_ends_exactly(
                    None, util.perl_num(core.get_left_edge()) - 1))
            right_neighbour = sworkspace.choose_by_strength(
                *sworkspace.get_objects_with_ends_exactly(
                    util.perl_num(core.get_right_edge()) + 1, None))
            if util.perl_true(left_neighbour) and util.perl_true(right_neighbour):
                if core.get_common_categories(left_neighbour, right_neighbour):
                    actions.append(SCodelet("CheckIfAlternating", 100, {
                        "first": left_neighbour,
                        "second": core,
                        "third": right_neighbour,
                    }))
        return actions

    def as_text(self):
        return "Group " + util.perl_str(self.core().as_text())


class SThoughtSeqseeElement(SThought):
    """Perl: package SThought::Seqsee::Element (extends SThought)."""

    perl_name = "SThought::Seqsee::Element"
    NAME = "Focusing on a Single Element"

    _UNBUILT = object()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        merged = {**args[0], **kwargs} if args else kwargs
        self._magnitude = merged.get("magnitude", self._UNBUILT)

    def magnitude(self, *value):
        """Perl: the lazy_build rw attribute ``magnitude`` (the core's mag)."""
        if value:
            self._magnitude = value[0]
        elif self._magnitude is self._UNBUILT:
            self._magnitude = self.core().get_mag()
        return self._magnitude

    def get_fringe(self):
        return get_fringe_for.call(self.core())

    def get_actions(self):
        core = self.core()
        self.magnitude()
        if util.perl_true(Global.Feature.get("LTM")):
            sltm.spike_by(10, core)

        if util.perl_true(Global.Feature.get("LTM")):
            # Spread activation from corresponding node:
            sltm.spread_activation_from(sltm.get_memory_index(core))
            extend_from_memory(core)
            add_categories_from_memory(core)
        return []

    def as_text(self):
        return "Element " + util.perl_str(self.core().as_text())
