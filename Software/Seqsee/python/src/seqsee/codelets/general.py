"""Port of lib/Seqsee/SCF_MX/General.pm: general codelet families.

Item 040: LookForSimilarGroups, MergeGroups, CleanUpGroup, DoTheSameThing and CreateGroup.
Item 041: FindIfRelatedRelations, CheckIfAlternating, FindIfRelated (+ ShouldIContinue) and
AttemptExtensionOfRelation (+ EstimateAskability).

Each family is registered with ``@codelet_family`` under its Perl name (package
``Seqsee::SCF::<Name>``); the body gets the validated arguments positionally.
"""
from seqsee import schoose, sworkspace, util
from seqsee.codelets.family import codelet_family
from seqsee.errors import Confess, ElementsBeyondKnownSought
from seqsee.multimethods import perl_isa


def _perl_eval(fn, *args):
    """Perl ``eval { ... }``: ``(True, value)``, or ``(False, error)`` if it died.

    ``NotImplementedError`` (an unported stub) is not a Perl death and propagates."""
    try:
        return True, fn(*args)
    except NotImplementedError:
        raise
    except Exception as e:  # noqa: BLE001 (Perl: eval catches every die)
        return False, e


def _schedule(family, urgency, args):
    """Perl: ``SCodelet->new($family, $urgency, {...})->schedule()``."""
    sworkspace._scodelet_new(family, urgency, args).schedule()


def _perl_eq(a, b):
    """Perl ``eq``: scalars by string, references by identity."""
    if util._is_scalar(a) and util._is_scalar(b):
        return util.perl_str(a) == util.perl_str(b)
    return a is b


# ---- LookForSimilarGroups ---------------------------------------------------------------
@codelet_family("LookForSimilarGroups", attributes=[("group", {"required": 1})])
def look_for_similar_groups(group):
    """Perl: Seqsee::SCF::LookForSimilarGroups. Up to 3 FocusOn/50 codelets for objects
    sharing a category with ``group`` (weighted by the category's activation).

    PERL-QUIRK: a group without categories gets undef from
    __GetObjectsBelongingToSimilarCategories, and ``->is_empty`` on it dies."""
    wset = sworkspace.get_objects_belonging_to_similar_categories(group)
    if wset is None:
        raise Confess('Can\'t call method "is_empty" on an undefined value')
    if util.perl_true(wset.is_empty()):
        return None
    for what in wset.choose_a_few_nonzero(3):
        _schedule("FocusOn", 50, {"what": what})
    return None


# ---- MergeGroups ------------------------------------------------------------------------
@codelet_family("MergeGroups", attributes=[("a", {"required": 1}), ("b", {"required": 1})])
def merge_groups(a, b):
    """Perl: Seqsee::SCF::MergeGroups. Builds a group of the (unique) items of ``a`` and
    ``b`` if they abut exactly. The group is added only if ``a`` has an underlying relation
    (whose rule it gets, with ``a``'s categories).

    The part after the holes check runs inside Perl's ``eval``, so its errors (and a
    refused add_group) are swallowed."""
    if _perl_eq(a, b):
        return None
    if not sworkspace.check_liveness(a, b):
        return None
    items = util.uniq(*a, *b)
    items = sworkspace.sort_l_to_r_by_left_edge(*items)
    if util.perl_true(sworkspace.are_there_holes_or_overlap(*items)):
        return None

    def body():
        unstarred_items = [x.get_concrete_object() for x in items]
        if not sworkspace.check_liveness(*unstarred_items):  # dead objects.
            return None
        from seqsee.objects.anchored import Anchored
        new_group = Anchored.create(*unstarred_items)
        if util.perl_true(new_group) and util.perl_true(a.get_underlying_reln()):
            new_group.set_underlying_ruleapp(a.get_underlying_reln().get_rule())
            a.copy_categories_to(new_group)
            sworkspace.add_group(new_group)
        return None

    _perl_eval(body)
    return None


# ---- CleanUpGroup -----------------------------------------------------------------------
@codelet_family("CleanUpGroup", attributes=[("group", {"required": 1})])
def clean_up_group(group):
    """Perl: Seqsee::SCF::CleanUpGroup. Deletes the live groups within ``group``'s edges
    that are not its (deep) subgroups."""
    if not sworkspace.check_liveness(group):
        return None
    edges = group.get_edges()
    potential_cruft = sworkspace.get_objects_with_ends_not_beyond(*edges)
    sworkspace.delete_non_subgroups_of_from({"of": [group], "from": potential_cruft})
    return None


# ---- DoTheSameThing ---------------------------------------------------------------------
def _apply_mapping(transform, obj):
    """Perl: the bare ``ApplyMapping(...)`` call in package Seqsee::SCF::DoTheSameThing.

    PERL-QUIRK: that package never imports the multimethod (``multimethod 'ApplyMapping'``
    appears only in later packages of General.pm), so the call always dies
    (oracle-confirmed). The die happens inside ``eval { } or return``, so DoTheSameThing
    always returns there."""
    raise Confess("Undefined subroutine &Seqsee::SCF::DoTheSameThing::ApplyMapping called")


def _plonk_into_place(start, direction, obj):
    """Perl: the bare ``__PlonkIntoPlace(...)`` call in DoTheSameThing, also never imported
    there (unreachable, since ApplyMapping dies first)."""
    raise Confess("Undefined subroutine &Seqsee::SCF::DoTheSameThing::__PlonkIntoPlace called")


@codelet_family("DoTheSameThing", attributes=[
    ("group", {"default": 0}),
    ("category", {"default": 0}),
    ("direction", {"default": 0}),
    ("transform", {"required": 1}),
])
def do_the_same_thing(group, category, direction, transform):
    """Perl: Seqsee::SCF::DoTheSameThing. Applies ``transform`` to ``group`` (or to a
    group of ``category``, chosen by strength) and, if the result is present next to it,
    plonks it and relates the two.

    The direction (if not given) is drawn first, then the group (if not given). Because
    of the ApplyMapping quirk (see ``_apply_mapping``) nothing past CheckSanity has any
    effect."""
    from seqsee.constants import DIR, DISTANCE
    if not (util.perl_true(group) or util.perl_true(category)):
        category = transform.get_category()
    if util.perl_true(group) and util.perl_true(category):
        raise Confess("Need exactly one of group and category: got both.")
    if not util.perl_true(direction):
        direction = schoose.choose([1, 1], [DIR.LEFT, DIR.RIGHT])
    if not util.perl_true(group):
        groups_of_cat = sworkspace.get_objects_belonging_to_category(category)
        if not groups_of_cat:
            return None
        group = sworkspace.choose_by_strength(*groups_of_cat)

    effective_transform = transform if direction is DIR.RIGHT else transform.flipped_version()
    if not util.perl_true(effective_transform):
        return None
    if not util.perl_true(effective_transform.check_sanity()):
        raise Confess("Mapping insane!")

    # BandAid: The following occasionally crashes.
    ok, expected_next_object = _perl_eval(_apply_mapping, effective_transform, group)
    if not ok or not util.perl_true(expected_next_object):
        return None
    if not len(expected_next_object):
        return None

    next_pos = sworkspace.get_position_in_direction_at_distance({
        "from_object": group, "direction": direction, "distance": DISTANCE.zero()})
    if next_pos is None or next_pos > sworkspace.ElementCount:
        return None

    ok, is_this_what_is_present = _perl_eval(sworkspace.check_at_location, {
        "start": next_pos, "direction": direction, "what": expected_next_object})
    if not ok:
        if not isinstance(is_this_what_is_present, ElementsBeyondKnownSought):
            raise is_this_what_is_present
        is_this_what_is_present = None  # Ignore.

    if util.perl_true(is_this_what_is_present):
        plonk_result = _plonk_into_place(next_pos, direction, expected_next_object)
        if not util.perl_true(plonk_result.plonk_was_successful()):
            return None
        wso = plonk_result.resultant_object()
        if not util.perl_true(wso):
            return None
        wso.describe_as(effective_transform.get_category())
        ends = (group, wso) if direction is DIR.RIGHT else (wso, group)
        from seqsee.srelation import SRelation
        SRelation({"first": ends[0], "second": ends[1], "type": transform}).insert()
    return None


# ---- CreateGroup ------------------------------------------------------------------------
@codelet_family("CreateGroup", attributes=[
    ("items", {"required": 1}),
    ("category", {"default": 0}),
    ("transform", {"default": 0}),
])
def create_group(items, category, transform):
    """Perl: Seqsee::SCF::CreateGroup. Unless an object already covers the items' span,
    makes a group of them described as ``category`` (or the category a ``transform``
    implies, which also becomes the underlying rule) and adds it."""
    if items is None:
        raise Confess("Can't use an undefined value as an ARRAY reference")
    left_edges = [x.get_left_edge() for x in items]
    right_edges = [x.get_right_edge() for x in items]
    left_edge = min((util.perl_num(x) for x in left_edges), default=None)
    right_edge = max((util.perl_num(x) for x in right_edges), default=None)
    is_covering = len(sworkspace.get_objects_with_ends_beyond(left_edge, right_edge))
    if is_covering:
        return None

    if not (util.perl_true(category) or util.perl_true(transform)):
        raise Confess("At least one of category or transform needed. Got neither.")
    if util.perl_true(category) and util.perl_true(transform):
        raise Confess("Exactly one of  category or transform needed. Got both.")

    if not util.perl_true(category):
        # Generate from transform.
        if not perl_isa(transform, "Mapping"):
            raise Confess("transform should be a Mapping!")
        if perl_isa(transform, "Mapping::Numeric"):
            category = transform.get_relation_based_category()
        else:
            from seqsee.categories.mapping_based import MappingBased
            category = MappingBased.create(transform)

    unstarred_items = [x.get_concrete_object() for x in items]
    if not sworkspace.check_liveness(*unstarred_items):  # dead objects.
        return None
    from seqsee.objects.anchored import Anchored
    new_group = Anchored.create(*unstarred_items)
    if not util.perl_true(new_group):
        return None
    if not util.perl_true(new_group.describe_as(category)):
        return None
    if util.perl_true(transform):
        new_group.set_underlying_ruleapp(transform)
    sworkspace.add_group(new_group)
    return None


# ---- FindIfRelatedRelations -------------------------------------------------------------
@codelet_family("FindIfRelatedRelations",
                attributes=[("a", {"required": 1}), ("b", {"required": 1})])
def find_if_related_relations(a, b):
    """Perl: Seqsee::SCF::FindIfRelatedRelations. If relations ``a`` and ``b`` chain
    (a's second end is b's first, after swapping them if needed), schedules CreateGroup/100
    for the three objects: with the shared type if both have the same one, else (with the
    Alternating feature and a common category) with CheckForAlternation's mapping.

    Types are compared by identity (Perl ``eq``). Note Mapping::Numeric's memo key: a
    mapping created before its category is in the LTM differs from later ones of the same
    name (oracle-confirmed, scenario fir_relations)."""
    from seqsee import global_
    af, as_, bf, bs = (*a.get_ends(), *b.get_ends())
    if _perl_eq(bs, af):
        # Switch the two...
        af, as_, a, bf, bs, b = bf, bs, b, af, as_, a
    if not _perl_eq(as_, bf):
        return None

    a_transform, b_transform = a.get_type(), b.get_type()
    if _perl_eq(a_transform, b_transform):
        _schedule("CreateGroup", 100, {"items": [af, as_, bs], "transform": a_transform})
    elif (util.perl_true(global_.Feature.get("Alternating"))
          and _perl_eq(a_transform.get_category(), b_transform.get_category())):
        # There is a chance that these are somehow alternating...
        from seqsee.categories.alternating import Alternating
        new_transform = Alternating.check_for_alternation(af, as_, bs)
        if util.perl_true(new_transform):
            _schedule("CreateGroup", 100, {"items": [af, as_, bs], "transform": new_transform})
    return None


# ---- CheckIfAlternating -----------------------------------------------------------------
@codelet_family("CheckIfAlternating", attributes=[
    ("first", {"required": 1}),
    ("second", {"required": 1}),
    ("third", {"required": 1}),
])
def check_if_alternating(first, second, third):
    """Perl: Seqsee::SCF::CheckIfAlternating. CreateGroup/100 for the three objects, with
    their common mapping if first→second and second→third map the same way, else with
    CheckForAlternation's mapping (if any)."""
    from seqsee.mapping import find_mapping
    t1 = find_mapping(first, second)
    t2 = find_mapping(second, third)
    if util.perl_true(t1) and _perl_eq(t1, t2):
        transform_to_consider = t1
    else:
        from seqsee.categories.alternating import Alternating
        transform_to_consider = Alternating.check_for_alternation(first, second, third)
        if not util.perl_true(transform_to_consider):
            return None
    _schedule("CreateGroup", 100, {"items": [first, second, third],
                                   "transform": transform_to_consider})
    return None


# ---- FindIfRelated ----------------------------------------------------------------------
@codelet_family("FindIfRelated", attributes=[("a", {"required": 1}), ("b", {"required": 1})])
def find_if_related(a, b):
    """Perl: Seqsee::SCF::FindIfRelated.

    - Overlapping objects with the same underlying rule, whose subgroups overlap: schedules
      MergeGroups/200.
    - Already related: spikes the relation's type and schedules FocusOn/100 on it.
    - Otherwise, if FindMapping finds a type: with probability ShouldIContinue (one toss)
      inserts the relation and schedules FocusOn/200 on it."""
    from seqsee import global_, sltm
    from seqsee.constants import DISTANCE_MODE
    from seqsee.mapping import find_mapping
    if not sworkspace.check_liveness(a, b):
        return None
    a, b = sworkspace.sort_l_to_r_by_left_edge(a, b)
    if util.perl_true(a.overlaps(b)):
        ul_a, ul_b = a.get_underlying_reln(), b.get_underlying_reln()
        if not (util.perl_true(ul_a) and util.perl_true(ul_b)):
            return None
        if not _perl_eq(ul_a.get_rule(), ul_b.get_rule()):
            return None
        # Perl: `$a->[-1] ~~ @$b`, i.e., actual subgroups overlap. Seqsee::Object
        # overloads ~~ as `eq` (no "" overload), so this is membership by identity.
        last = a[-1]
        if not any(x is last for x in b):
            return None
        _schedule("MergeGroups", 200, {"a": a, "b": b})
        return None

    relation = a.get_relation(b)
    if util.perl_true(relation):
        sltm.spike_by(10, relation.get_type())
        _schedule("FocusOn", 100, {"what": relation})
        return None

    reln_type = find_mapping(a, b)
    if not util.perl_true(reln_type):
        return None
    sltm.spike_by(10, reln_type)

    # insert relation with certain probability:
    transform_complexity = reln_type.get_complexity()
    transform_activation = sltm.get_real_activations_for_one_concept(reln_type)
    distance = sworkspace.find_distance(a, b, DISTANCE_MODE.ELEMENT).get_magnitude()
    sense_in_continuing = should_i_continue(transform_complexity, transform_activation,
                                            distance)
    if util.perl_true(global_.debugMAX):
        from seqsee import scodelet_base
        scodelet_base._message(f"Sense in continuing={util.perl_str(sense_in_continuing)}")
    if not util.toss(sense_in_continuing):
        return None

    sltm.spike_by(10, reln_type)
    from seqsee.srelation import SRelation
    relation = SRelation({"first": a, "second": b, "type": reln_type})
    relation.insert()
    _schedule("FocusOn", 200, {"what": relation})
    return None


def should_i_continue(transform_complexity, transform_activation, distance):
    """Perl: Seqsee::SCF::FindIfRelated::ShouldIContinue:
    ``1 - complexity * (1 - activation) * sqrt(distance)``."""
    import math
    distance = util.perl_num(distance)
    if distance < 0:
        raise ValueError(f"Can't take sqrt of {util.perl_str(distance)}")
    not_continue = (util.perl_num(transform_complexity)
                    * (1 - util.perl_num(transform_activation)) * math.sqrt(distance))
    return 1 - not_continue


# ---- AttemptExtensionOfRelation ---------------------------------------------------------
@codelet_family("AttemptExtensionOfRelation",
                attributes=[("core", {"required": 1}), ("direction", {"required": 1})])
def attempt_extension_of_relation(core, direction):
    """Perl: Seqsee::SCF::AttemptExtensionOfRelation. Applies the relation's type (flipped
    for a leftward extension) to the end object; if the result is present at the same
    distance beyond it, plonks it, describes it as the type's category and inserts the
    relation to it. Running past the known elements schedules AskIfThisIsTheContinuation/100
    if EstimateAskability says so.

    Unlike DoTheSameThing, this package does import ApplyMapping and __PlonkIntoPlace.
    The distance comes from __FindDistance without a mode (one draw). A non-zero distance is
    in elements, so __GetPositionInDirectionAtDistance's quirk (see sworkspace) makes such
    relations unextendable (oracle-confirmed)."""
    from seqsee import sltm
    from seqsee.constants import DIR
    from seqsee.mapping import apply_mapping
    transform = core.get_type()
    end1, end2 = core.get_ends()
    if _perl_eq(direction, DIR.RIGHT):
        effective_transform, object_at_end = transform, end2
    else:
        effective_transform = transform.flipped_version()
        if not util.perl_true(effective_transform):
            return None
        object_at_end = end1

    distance = sworkspace.find_distance(end1, end2)
    next_pos = sworkspace.get_position_in_direction_at_distance({
        "from_object": object_at_end, "direction": direction, "distance": distance})
    if next_pos is None or next_pos > sworkspace.ElementCount:
        return None

    what_next = apply_mapping(effective_transform, object_at_end.get_effective_object())
    if not util.perl_true(what_next):
        return None
    if not len(what_next):  # 0 elts also not okay
        return None

    ok, is_this_what_is_present = _perl_eval(sworkspace.check_at_location, {
        "start": next_pos, "direction": direction, "what": what_next})
    if not ok:
        err = is_this_what_is_present
        if not isinstance(err, ElementsBeyondKnownSought):
            raise err
        if not util.perl_true(estimate_askability(core, transform, end1, end2)):
            return None
        _schedule("AskIfThisIsTheContinuation", 100, {
            "relation": core,
            "exception": err,
            "expected_object": what_next,
            "start_position": next_pos,
            "known_term_count": sworkspace.ElementCount,
        })
        return None

    if util.perl_true(is_this_what_is_present):
        sltm.spike_by(10, transform)
        plonk_result = sworkspace.plonk_into_place.call(next_pos, direction, what_next)
        if not util.perl_true(plonk_result.plonk_was_successful()):
            return None
        wso = plonk_result.resultant_object()

        cat = transform.get_category()
        sltm.spike_by(10, cat)
        if not util.perl_true(wso.describe_as(cat)):
            return None

        from seqsee.srelation import SRelation
        if _perl_eq(direction, DIR.RIGHT):
            reln_to_add = SRelation({"first": end2, "second": wso, "type": transform})
        else:
            reln_to_add = SRelation({"first": wso, "second": end1, "type": transform})
        if util.perl_true(reln_to_add):
            reln_to_add.insert()
        # SanityCheck($reln_to_add) is commented out in the Perl.
    return None


def estimate_askability(relation, transform, end1, end2):
    """Perl: Seqsee::SCF::AttemptExtensionOfRelation::EstimateAskability. 0 if either end
    has a super-supergroup; else one toss with the transform's activation, times 0.4 if
    either end has a supergroup."""
    from seqsee import sltm
    if (sworkspace.are_there_any_super_super_groups(end1)
            or sworkspace.are_there_any_super_super_groups(end2)):
        return 0
    supergroup_penalty = 0
    if sworkspace.get_super_groups(end1) or sworkspace.get_super_groups(end2):
        supergroup_penalty = 0.6
    transform_activation = sltm.get_real_activations_for_one_concept(transform)
    return util.toss(transform_activation * (1 - supergroup_penalty))
