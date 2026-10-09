"""Port of lib/SWorkspace.pm (``SWorkspace``): the workspace, the elements of the sequence and
the groups and relations built on them. All of its state is module-level, as in Perl.

Item 031: the state, ``__Clear``/``clear``/``init``/``insert_elements``,
``_insert_element``/``__InsertElement``, ``GetElements``, ``GetSuperGroups``, the liveness
one-liners, the magnitude scanners and the bar lines. Item 032: groups (add_group,
remove_gp, __DeleteGroup, bookkeeping, supergroups), conflicts (including FightUntoDeath),
liveness diagnosis, finding objects by edges, the edge sorts, AddRelation/RemoveRelation.
Item 033: distances and positions, position structures, sameness groups,
check_at_location, rapid_create_gp, __CopyAttributes, __PlonkIntoPlace, GetSomethingLike
and LookForSomethingLike. Item 034: the choice distributions, __ReadObjectOrRelation,
_saccade, __UpdateObjectStrengths, the uniform choosers (read_relation,
_get_some_object_at, __ChooseByStrength), the category lookups, are_there_holes_here,
SErr::AskUser::WorthAsking/Ask (``ask_user_worth_asking``/``ask_user_ask``, called by
errors.AskUser), DeleteObjectsInconsistentWith and __DeleteNonSubgroupsOfFrom.

Name clashes: ``__FindGroupsConflictingWith`` → ``find_groups_conflicting_with`` and the
public ``FindGroupsConflictingWith`` → ``find_groups_conflicting_with_as_list``;
``__AddGroup`` → ``_add_group``.

Naming: Perl's ``__Foo`` subs are called from many other modules, so they become public
snake_case functions (``__ScanRightwardForElements`` → ``scan_rightward_for_elements``,
``__CheckLiveness`` → ``check_liveness``). ``__Clear`` → ``_clear`` (only ``clear`` calls
it). The package argument of class-method calls (``SWorkspace->init``) is dropped.

State: scalars keep their Perl names and are rebound (``ElementCount``, ``ReadHead``,
``BarLineCount``; read them as ``sworkspace.ElementCount``). Containers are mutated in
place: ``ELEMENTS``, ``ELEMENT_MAGNITUDES``, ``OBJECTS``, ``NON_ELT_OBJECTS``,
``LEFT_EDGE_OF``, ``RIGHT_EDGE_OF``, ``SUPER_GROUPS_OF``, ``SPAN_OF``,
``LIVE_AT_SOME_POINT``, ``BAR_LINES``, and the ``our`` variables ``relations``,
``relations_by_ends`` and ``elements``. Perl hashes keyed by stringified refs are dicts
keyed by the objects (identity hashing).

PERL-QUIRKs (oracle-confirmed):
- ``clear`` doesn't forget %LiveAtSomePoint, the bar lines or %relations_by_ends. ``reset``
  (a test hook) forgets everything.
- ``_insert_element`` dispatches on Class::Multimethods types. A number ("#") is used as
  the magnitude as is, so 3.7 dies in Moose's Int check. A string ("$", also undef) that
  looks like a number is truncated with ``int`` ("3.7" → 3, "1e2" → 100, " 3", "3 " and
  "0 but true" are fine, "Inf" stays Inf and fails the Int check). Anything else dies
  "Huh? Trying to insert '<what>' into the workspace" (undef shows as '').
- An inserted Element object keeps its identity; its edges are overwritten. Inserting the
  same object twice registers it twice in @Elements, with the second position's edges.
- ``init`` and ``insert_elements`` stop at the first bad value, leaving the earlier
  elements inserted and the globals (RealSequence/InitialTermCount, TimeOf*) untouched.
- The scanners index @ElementMagnitudes with Perl semantics: a negative position counts
  from the end, and a position outside the array reads undef, which ``!=``/``==`` treat as
  0. So a hunt for [0] matches past the end, and an empty hunt matches anywhere
  (rightward: any start up to ElementCount; leftward: start + 1 whenever that is >= 0).
- The closest-bar-line subs return undef (an empty list in list context) when there is
  none; the port returns None.
- ``__GroupAddSanityCheck`` calls ``__AreHolesPresent``, which doesn't exist, so it always
  dies (after the liveness check).
- ``__UpdateGroup`` vivifies the edge entries of dead parts (as undef).
- ``__FindGroupsConflictingWith``/``FindGroupsConflictingWith`` list a conflicting object
  with exactly the same edges twice.
- A failed Smart::Comments ``### require:`` prints the assertion and dies "\\n"; the port
  raises Confess("require: ...").
- The strength chooser ignores strengths (uniform choice); a GROUP distance counts
  elements; a non-GROUP distance on the right adds the DISTANCE ref's address;
  LookForSomethingLike confesses on start position 0. See the functions.
"""
import math
import re

from seqsee import global_ as Global
from seqsee import sltm, util
from seqsee.errors import Confess, ElementsBeyondKnownSought
from seqsee.multimethods import Multimethod

ElementCount = 0          # Perl: our $ElementCount
ELEMENTS = []             # Perl: my @Elements
ELEMENT_MAGNITUDES = []   # Perl: my @ElementMagnitudes
OBJECTS = {}              # All objects.
NON_ELT_OBJECTS = {}      # Only groups (of size 2 or more).
LEFT_EDGE_OF = {}         # Left edges of "registered" objects.
RIGHT_EDGE_OF = {}        # Likewise, right edges.
SUPER_GROUPS_OF = {}      # Groups whose direct element is this group: {object: {group: group}}.
SPAN_OF = {}              # Span.
LIVE_AT_SOME_POINT = {}   # All objects ever live.

BAR_LINES = []
BarLineCount = 0

relations = {}            # Perl: our %relations
relations_by_ends = {}    # Perl: our %relations_by_ends; keys "end1;end2", 1 if present.

elements = []             # Perl: our @elements (only ever emptied).
ReadHead = 0              # Points just beyond the last object read.


def get_elements():
    """Perl: GetElements: the elements, left to right (a new list)."""
    return list(ELEMENTS)


def get_groups():
    """Perl: GetGroups: the groups (not elements), longest span first."""
    return sorted(NON_ELT_OBJECTS.values(), key=lambda g: -util.perl_num(SPAN_OF.get(g)))


def get_super_groups(group):
    """Perl: GetSuperGroups($group): the groups having ``group`` as a direct part.
    Perl's ``%{$SuperGroups_of{$group}}`` autovivifies a missing entry; so does this."""
    return list(SUPER_GROUPS_OF.setdefault(group, {}).values())


def are_there_any_super_super_groups(group):
    """Perl: AreThereAnySuperSuperGroups($group): 1 if a supergroup of ``group`` has a
    supergroup itself, else 0."""
    for super_group in list(SUPER_GROUPS_OF.setdefault(group, {}).values()):
        if SUPER_GROUPS_OF.get(super_group):
            return 1
    return 0


def _clear():
    """Perl: __Clear."""
    global ElementCount
    ElementCount = 0
    ELEMENTS.clear()
    ELEMENT_MAGNITUDES.clear()
    OBJECTS.clear()
    NON_ELT_OBJECTS.clear()
    LEFT_EDGE_OF.clear()
    RIGHT_EDGE_OF.clear()
    SUPER_GROUPS_OF.clear()
    SPAN_OF.clear()


def check_liveness(*objects):
    """Perl: __CheckLiveness: true if every object is live (vacuously true for none)."""
    return all(o in OBJECTS for o in objects)


def check_liveness_at_some_point(*objects):
    """Perl: __CheckLivenessAtSomePoint: true if every object was ever live."""
    return all(o in LIVE_AT_SOME_POINT for o in objects)


def check_liveness_and_diagnose(*objects):
    """Perl: __CheckLivenessAndDiagnose: 1 if all objects are live, else confess with a
    description of each dead one. (An undef dies in ``as_text`` before its own check.)"""
    problems_so_far = 0
    msg = ""
    for o in objects:
        if o in OBJECTS:
            continue
        if o is None:
            raise Confess('Can\'t call method "as_text" on an undefined value')
        msg += "NON_LIVE OBJECT: >>" + util.perl_str(o.as_text()) + "<<\n"
        if o in LIVE_AT_SOME_POINT:
            msg += "But was live once!\n"
        unstarred = o.get_concrete_object()
        if unstarred is not o:
            msg += "A METONYM IS BEING CHECKED FOR LIVENESS!\n"
            if unstarred in OBJECTS:
                msg += "\tIts unstarred *is* live.\n"
            else:
                msg += "\tEven its unstarred is non-live.\n"
        problems_so_far += 1
    if problems_so_far:
        raise Confess("Dying because of liveness issues!\n" + msg)
    return 1


def grep_liveness(*objects):
    """Perl: __GrepLiveness: the live ones, in order (duplicates kept)."""
    return [o for o in objects if o in OBJECTS]


# ---- finding objects by their edges ---------------------------------------------------------
def _edge(edges, o):
    return util.perl_num(edges.get(o))


def get_objects_with_ends_exactly(left, right):
    """Perl: __GetObjectsWithEndsExactly($left, $right): live objects with that left edge
    and that right edge (an undef end matches anything)."""
    objects = list(OBJECTS.values())
    if left is not None:
        objects = [o for o in objects if _edge(LEFT_EDGE_OF, o) == left]
    if right is not None:
        objects = [o for o in objects if _edge(RIGHT_EDGE_OF, o) == right]
    return objects


def get_objects_with_ends_beyond(left, right):
    """Perl: __GetObjectsWithEndsBeyond: live objects with left edge <= left and right edge
    >= right (an undef end matches anything)."""
    objects = list(OBJECTS.values())
    if left is not None:
        objects = [o for o in objects if _edge(LEFT_EDGE_OF, o) <= left]
    if right is not None:
        objects = [o for o in objects if _edge(RIGHT_EDGE_OF, o) >= right]
    return objects


def get_objects_with_ends_not_beyond(left, right):
    """Perl: __GetObjectsWithEndsNotBeyond: live objects with left edge >= left and right
    edge <= right (an undef end matches anything)."""
    objects = list(OBJECTS.values())
    if left is not None:
        objects = [o for o in objects if _edge(LEFT_EDGE_OF, o) >= left]
    if right is not None:
        objects = [o for o in objects if _edge(RIGHT_EDGE_OF, o) <= right]
    return objects


def get_exact_object_if_present(obj):
    """Perl: __GetExactObjectIfPresent($object): the live object with the same edges and
    structure string, or None."""
    left, right = obj.get_edges()
    structure_string = obj.get_structure_string()
    matching = [o for o in get_objects_with_ends_exactly(left, right)
                if o.get_structure_string() == structure_string]
    return matching[0] if matching else None


def get_groups_that_partially_overlap(obj):
    """Perl: __GetGroupsThatPartiallyOverlap($object): live groups overlapping ``obj``
    without either containing the other."""
    left, right = (util.perl_num(x) for x in obj.get_edges())
    result = []
    for g in NON_ELT_OBJECTS.values():
        obj_left, obj_right = _edge(LEFT_EDGE_OF, g), _edge(RIGHT_EDGE_OF, g)
        if (obj_left < left <= obj_right < right) or (left < obj_left <= right < obj_right):
            result.append(g)
    return result


def sort_l_to_r_by_left_edge(*objects):
    """Perl: __SortLtoRByLeftEdge (stable; the objects must be live)."""
    check_liveness_and_diagnose(*objects)
    return sorted(objects, key=lambda o: _edge(LEFT_EDGE_OF, o))


def sort_r_to_l_by_left_edge(*objects):
    """Perl: __SortRtoLByLeftEdge (stable; the objects must be live)."""
    check_liveness_and_diagnose(*objects)
    return sorted(objects, key=lambda o: -_edge(LEFT_EDGE_OF, o))


def sort_l_to_r_by_right_edge(*objects):
    """Perl: __SortLtoRByRightEdge (stable; the objects must be live)."""
    check_liveness_and_diagnose(*objects)
    return sorted(objects, key=lambda o: _edge(RIGHT_EDGE_OF, o))


def sort_r_to_l_by_right_edge(*objects):
    """Perl: __SortRtoLByRightEdge (stable; the objects must be live)."""
    check_liveness_and_diagnose(*objects)
    return sorted(objects, key=lambda o: -_edge(RIGHT_EDGE_OF, o))


# ---- adding and deleting groups ---------------------------------------------------------------
def delete_group(group):
    """Perl: __DeleteGroup($group): delete its live supergroups (recursively), unregister it
    from its parts, uninsert its relations and forget it. Works on dead groups too (Perl
    vivifies ``$SuperGroups_of{$part}`` for parts that have no entry)."""
    for super_group in list(SUPER_GROUPS_OF.setdefault(group, {}).values()):
        if not check_liveness(super_group):
            continue
        delete_group(super_group)

    for part in group.get_items_array():
        SUPER_GROUPS_OF.setdefault(part, {}).pop(group, None)

    group.remove_all_relations()
    for h in (LEFT_EDGE_OF, RIGHT_EDGE_OF, SPAN_OF, SUPER_GROUPS_OF, OBJECTS, NON_ELT_OBJECTS):
        h.pop(group, None)


def check_two_groups_for_conflict(a, b):
    """Perl: __CheckTwoGroupsForConflict($A, $B): 1 if they are the same object, or if the
    smaller (by span; A on a tie) is a group whose items are a contiguous run of the bigger
    one's items. ``b`` must be live."""
    from seqsee.multimethods import perl_isa
    if a is b:
        return 1
    if not check_liveness(b):
        raise Confess("This method only works when the second object is live")

    smaller, bigger = sorted((a, b), key=lambda o: util.perl_num(o.get_span()))
    if perl_isa(smaller, "Seqsee::Element"):
        return 0
    if not util.perl_true(bigger.spans(smaller)):
        return 0

    smaller_left_edge = util.perl_num(smaller.get_left_edge())
    bigger_items = list(bigger)
    for current_index, piece_of_bigger in enumerate(bigger_items):
        if _edge(RIGHT_EDGE_OF, piece_of_bigger) < smaller_left_edge:
            continue
        # If @$smaller is a subset of @$bigger, it must start here: no "next" from here on.
        for piece_of_smaller in smaller:
            if current_index >= len(bigger_items) or \
                    piece_of_smaller is not bigger_items[current_index]:
                return 0
            current_index += 1
        # All smaller pieces match a bigger piece (maybe all of them; still a conflict).
        return 1
    raise Confess("Should never reach here.")


def group_add_sanity_check(*parts):
    """Perl: __GroupAddSanityCheck(@parts).

    PERL-QUIRK: it calls ``__AreHolesPresent``, which doesn't exist. Class::Std's AUTOLOAD
    turns that into a "Can't locate ... method" death, after the liveness check."""
    check_liveness_and_diagnose(*parts)
    kind = "object" if parts else "class"
    raise Confess(f'Can\'t locate {kind} method "__AreHolesPresent" via package "SWorkspace"')


def do_group_add_bookkeeping(group):
    """Perl: __DoGroupAddBookkeeping($group): register a group (sanity checks assumed)."""
    OBJECTS[group] = group
    NON_ELT_OBJECTS[group] = group
    SUPER_GROUPS_OF[group] = {}
    LIVE_AT_SOME_POINT[group] = 1
    sltm.insert_unless_present(group)
    update_group(group)


def _perl_min_max(values, want_max):
    """List::Util::min/max: compares numerically but returns the original value."""
    best = None
    for i, v in enumerate(values):
        if i == 0 or (util.perl_num(v) > util.perl_num(best) if want_max
                      else util.perl_num(v) < util.perl_num(best)):
            best = v
    return best


def update_group(group):
    """Perl: __UpdateGroup($group): register the group as a supergroup of its parts and
    recompute its edges and span from theirs.

    It requires (Smart::Comments) that the group has no supergroups; otherwise Perl prints
    the assertion and dies "\\n" (here: Confess("require: ...")).

    PERL-QUIRK: ``@LeftEdge_of{@parts}`` passed to List::Util::min vivifies the edge
    entries of dead parts as undef, and min/max treat undef as 0 (returning the undef)."""
    if SUPER_GROUPS_OF.setdefault(group, {}):
        raise Confess("require: not(%{$SuperGroups_of{$group}})")
    parts = group.get_items_array()
    for part in parts:
        SUPER_GROUPS_OF.setdefault(part, {})[group] = group

    left_edge = _perl_min_max([LEFT_EDGE_OF.setdefault(p, None) for p in parts], False)
    right_edge = _perl_min_max([RIGHT_EDGE_OF.setdefault(p, None) for p in parts], True)
    LEFT_EDGE_OF[group] = left_edge
    RIGHT_EDGE_OF[group] = right_edge
    SPAN_OF[group] = util.perl_num(right_edge) - util.perl_num(left_edge) + 1


def remove_from_supergroups_of(subgroup, supergroup):
    """Perl: __RemoveFromSupergroups_of($subgroup, $supergroup)."""
    SUPER_GROUPS_OF.setdefault(subgroup, {}).pop(supergroup, None)


def find_object_set_direction(*objects):
    """Perl: __FindObjectSetDirection(@objects): RIGHT/LEFT if the (workspace) left edges
    strictly increase/decrease, UNKNOWN as soon as two in a row are equal, NEITHER if
    mixed. The objects must be live, and there must be at least 2."""
    from seqsee.constants import DIR
    check_liveness_and_diagnose(*objects)
    left_edges = [_edge(LEFT_EDGE_OF, o) for o in objects]
    if len(objects) <= 1:
        raise Confess("Need at least 2")
    leftward = rightward = 0
    for i in range(len(objects) - 1):
        diff = left_edges[i + 1] - left_edges[i]
        if diff > 0:
            rightward += 1
        elif diff < 0:
            leftward += 1
        else:
            return DIR.UNKNOWN
    if leftward and rightward:
        return DIR.NEITHER
    if leftward:
        return DIR.LEFT
    if rightward:
        return DIR.RIGHT
    raise Confess("huh?")


def are_there_holes_or_overlap(*parts):
    """Perl: __AreThereHolesOrOverlap(@parts): 0 if the (live) parts abut exactly, in
    order, rightward or leftward; 1 otherwise (including an UNKNOWN/NEITHER direction)."""
    from seqsee.constants import DIR
    check_liveness_and_diagnose(*parts)
    direction = find_object_set_direction(*parts)
    if direction is DIR.RIGHT:
        for a, b in zip(parts, parts[1:]):
            if _edge(RIGHT_EDGE_OF, a) + 1 != _edge(LEFT_EDGE_OF, b):
                return 1  # Hole/overlap present
        return 0
    if direction is DIR.LEFT:
        for a, b in zip(parts, parts[1:]):
            if _edge(LEFT_EDGE_OF, a) - 1 != _edge(RIGHT_EDGE_OF, b):
                return 1
        return 0
    return 1  # funny direction, so the question of holes is moot


def find_groups_conflicting_with(obj):
    """Perl: __FindGroupsConflictingWith($object): a ResultOfGetConflicts whose exact
    conflict is the live object with the same edges and structure ('' if none) and whose
    overlapping conflicts are the other live objects in conflict with it. With the
    NoGpOverlap feature, partially overlapping groups are added.

    PERL-QUIRK: an object with exactly the same edges is found both "beyond" and "not
    beyond", so a same-span conflict is listed twice."""
    from seqsee.objects.result_of_get_conflicts import ResultOfGetConflicts
    left, right = obj.get_edges()
    exact_span = get_objects_with_ends_exactly(left, right)
    structure_string = obj.get_structure_string()
    exact = [o for o in exact_span if o.get_structure_string() == structure_string]
    exact_conflict = exact[0] if exact else ""

    conflicting = [o for o in (get_objects_with_ends_beyond(left, right)
                               + get_objects_with_ends_not_beyond(left, right))
                   if check_two_groups_for_conflict(obj, o) and o is not exact_conflict]
    if util.perl_true(Global.Feature.get("NoGpOverlap")):
        conflicting.extend(get_groups_that_partially_overlap(obj))
    return ResultOfGetConflicts(challenger=obj, exact_conflict=exact_conflict,
                                overlapping_conflicts=conflicting)


def find_groups_conflicting_with_as_list(obj):
    """Perl: FindGroupsConflictingWith($object) (the public one): a list of the exact
    conflict (None if none) followed by every conflicting live object, the exact conflict
    included (twice, as it is both "beyond" and "not beyond")."""
    left, right = obj.get_edges()
    structure_string = obj.get_structure_string()
    exact = [o for o in get_objects_with_ends_exactly(left, right)
             if o.get_structure_string() == structure_string]
    conflicting = [o for o in (get_objects_with_ends_beyond(left, right)
                               + get_objects_with_ends_not_beyond(left, right))
                   if check_two_groups_for_conflict(obj, o)]
    return [exact[0] if exact else None, *conflicting]


def are_groups_in_conflict(a, b):
    """Perl: AreGroupsInConflict($A, $B): the conflict check if ``b`` is live, else undef."""
    if check_liveness(b):
        return check_two_groups_for_conflict(a, b)
    return None


def add_group(gp):
    """Perl: add_group($gp): resolve conflicts (failing on an exact one); if that works,
    set $Global::TimeOfNewStructure, register the group and return 1. Else None."""
    conflicts = find_groups_conflicting_with(gp)
    if conflicts:
        if not util.perl_true(conflicts.resolve({"FailIfExact": 1})):
            return None
    Global.TimeOfNewStructure = Global.Steps_Finished
    do_group_add_bookkeeping(gp)
    return 1


def _add_group(gp):
    """Perl: __AddGroup($gp): the same as add_group."""
    return add_group(gp)


def remove_gp(gp):
    """Perl: remove_gp($gp)."""
    delete_group(gp)


def fight_unto_death(opts):
    """Perl: FightUntoDeath({challenger =>, incumbent =>}): 1 if the challenger wins (a dead
    incumbent loses without a fight; the loser of a fight is deleted), 0 if the incumbent
    is locked against deletion or wins the toss (challenger strength / (challenger + 1.5 ×
    incumbent))."""
    challenger, incumbent = opts.get("challenger"), opts.get("incumbent")
    if not check_liveness(incumbent):
        return 1
    if util.perl_true(incumbent.get_is_locked_against_deletion()):
        return 0
    s0, s1 = (util.perl_num(x.get_strength()) for x in (challenger, incumbent))
    if not s0 + s1:
        raise Confess("Both strengths 0")
    if util.toss(s0 / (s0 + 1.5 * s1)):
        delete_group(incumbent)
        return 1
    return 0


def find_sets_of_objects_with_overlapping_subgroups(*objects):
    """Perl: __FindSetsOfObjectsWithOverlappingSubgroups(@objects): for each item shared by
    two or more of the objects, the list of those objects (in the order given, once per
    occurrence)."""
    subgroup_to_objects = {}
    for o in objects:
        for sub in o:
            subgroup_to_objects.setdefault(sub, []).append(o)
    return [v for v in subgroup_to_objects.values() if len(v) >= 2]


def remove_groups_crossing_bar_lines():
    """Perl: __RemoveGroupsCrossingBarLines: delete each live group that crosses a bar line
    inappropriately."""
    for group in get_groups():
        if not check_liveness(group):
            continue
        if check_if_crosses_bar_lines_inappropriately(group):
            delete_group(group)


# ---- relations ------------------------------------------------------------------------------
def add_relation(reln):
    """Perl: AddRelation($reln): register a relation unless one between the same ends (in
    that order) is registered. Returns the relation, or None if refused. Metonymed ends
    confess."""
    f, s = reln.get_ends()
    for end in (f, s):
        if util.perl_true(end.is_this_a_metonymed_object()):
            raise Confess("Metonym'd end of relation")
    key = (f, s)
    if key in relations_by_ends:
        return None
    relations_by_ends[key] = 1
    relations[reln] = reln
    return reln


def remove_relation(reln):
    """Perl: RemoveRelation($reln)."""
    relations_by_ends.pop(tuple(reln.get_ends()), None)
    relations.pop(reln, None)


# ---- strengths, distributions and reading ----------------------------------------------------
def update_object_strengths():
    """Perl: __UpdateObjectStrengths: UpdateStrength on every relation, then every object."""
    for o in list(relations.values()) + list(OBJECTS.values()):
        o.update_strength()


def _has_super_groups(o):
    """Perl: ``%{ $SuperGroups_of{$o} }`` in boolean context (dies if there is no entry)."""
    if o not in SUPER_GROUPS_OF:
        raise Confess("Can't use an undefined value as a HASH reference")
    return bool(SUPER_GROUPS_OF[o])


def _normalised(values, objects):
    total = sum(values)
    if total:
        values = [v / total for v in values]
    return values, objects


def get_object_choice_probability_distribution():
    """Perl: __GetObjectChoiceProbabilityDistribution: (likelihoods, objects) for the
    objects starting at or after ReadHead. Strength ×2 for a span over 4, ×0.3 for an
    object with supergroups; zero strengths are left out. Likelihoods sum to 1 (or the
    lists are empty)."""
    values, objects = [], []
    for o in get_objects_with_ends_beyond(ReadHead, ReadHead):
        strength = util.perl_num(o.get_strength())
        if SPAN_OF[o] > 4:
            strength *= 2
        if _has_super_groups(o):
            strength *= 0.3
        if not strength:
            continue
        objects.append(o)
        values.append(strength)
    return _normalised(values, objects)


def get_relation_choice_probability_distribution():
    """Perl: __GetRelationChoiceProbabilityDistribution: (likelihoods, relations); strength
    ×0.8 when either end has supergroups; zero strengths are left out."""
    values, objects = [], []
    for reln in list(relations.values()):
        strength = util.perl_num(reln.get_strength())
        end1, end2 = reln.get_ends()
        if _has_super_groups(end1) or _has_super_groups(end2):
            strength *= 0.8
        if not strength:
            continue
        objects.append(reln)
        values.append(strength)
    return _normalised(values, objects)


def get_object_or_relation_choice_probability_distribution():
    """Perl: __GetObjectOrRelationChoiceProbabilityDistribution: both distributions, each
    halved, objects first."""
    obj_values, obj_list = get_object_choice_probability_distribution()
    rel_values, rel_list = get_relation_choice_probability_distribution()
    return [v * 0.5 for v in obj_values + rel_values], obj_list + rel_list


def read_object_or_relation():
    """Perl: __ReadObjectOrRelation: choose from the combined distribution. A chosen group
    moves ReadHead just past itself, or saccades if it ends at the last element. Elements
    and relations leave ReadHead alone."""
    global ReadHead
    from seqsee import schoose
    from seqsee.multimethods import perl_isa
    values, objects = get_object_or_relation_choice_probability_distribution()
    chosen = schoose.choose(values, objects)
    if perl_isa(chosen, "Seqsee::Anchored"):
        right_edge = RIGHT_EDGE_OF.get(chosen)
        if right_edge == ElementCount - 1:
            _saccade()
        else:
            ReadHead = right_edge + 1
    return chosen


def _saccade():
    """Perl: _saccade: toss(0.5) → ReadHead 0, else a random position. Returns ReadHead."""
    global ReadHead
    if util.toss(0.5):
        ReadHead = 0
        return ReadHead
    ReadHead = int(util.rand() * ElementCount)
    return ReadHead


def choose_by_strength(*objects):
    """Perl: __ChooseByStrength(@objects) (uniform; see _strength_chooser)."""
    return _strength_chooser(list(objects))


def read_relation():
    """Perl: read_relation: a (uniformly) chosen relation."""
    return _strength_chooser(list(relations.values()))


def _get_some_object_at(idx):
    """Perl: _get_some_object_at($idx): a (uniformly) chosen object spanning ``idx``."""
    matching = [o for o in OBJECTS.values()
                if util.perl_num(o.get_left_edge()) <= idx <= util.perl_num(o.get_right_edge())]
    return _strength_chooser(matching)


# ---- categories -------------------------------------------------------------------------------
def get_objects_belonging_to_category(cat):
    """Perl: __GetObjectsBelongingToCategory($cat)."""
    return [o for o in OBJECTS.values() if util.perl_true(o.is_of_category_p(cat))]


def get_objects_belonging_to_similar_categories(obj):
    """Perl: __GetObjectsBelongingToSimilarCategories($object): a Set::Weighted of
    [object, real activation of the category] for every object sharing one of ``obj``'s
    categories (once per category), without ``obj``. None if ``obj`` has no categories."""
    from seqsee.set.weighted import SetWeighted
    cats = list(obj.get_categories())
    if not cats:
        return None
    wset = SetWeighted()
    for c in cats:
        activation_level = sltm.get_real_activations_for_one_concept(c)
        wset.insert(*[[o, activation_level] for o in get_objects_belonging_to_category(c)])
    wset.delete_key(obj)
    return wset


def are_there_holes_here(*items):
    """Perl: are_there_holes_here(@items): 1 if the items' edges leave a gap, else 0 (also 0
    for no items). A non-anchored item throws SErr."""
    from seqsee.errors import SErr
    from seqsee.multimethods import perl_isa
    if not items:
        return 0
    slots_taken = set()
    for item in items:
        if not perl_isa(item, "Seqsee::Anchored"):
            from seqsee.objects.object import _perl_string
            SErr.throw("SWorkspace are_there_holes_here called with a non anchored object "
                       + _perl_string(item))
        left, right = item.get_edges()
        slots_taken.update(range(int(left), int(right) + 1))
    left, right = util.minmax(*slots_taken)
    return 0 if len(slots_taken) == right - left + 1 else 1


# ---- SErr::AskUser (defined in SWorkspace.pm) ---------------------------------------------------
def ask_user_worth_asking(err, trust_level):
    """Perl: SErr::AskUser::WorthAsking($trust_level): raise the trust by the fraction
    already matched; 0 below $Global::AcceptableTrustLevel, else toss(trust) ? trust : 0.
    Nothing matched or asked dies "Illegal division by zero"."""
    match_size, ask_size = len(err.already_matched), len(err.next_elements)
    if not match_size + ask_size:
        raise Confess("Illegal division by zero")
    trust_level += (1 - trust_level) * (match_size / (match_size + ask_size))
    if trust_level < Global.AcceptableTrustLevel:
        return 0
    return trust_level if util.toss(trust_level) else 0


def ask_user_ask(err, msg):
    """Perl: SErr::AskUser::Ask($msg): ask the user about next_elements. Yes: insert them,
    reset $Global::AcceptableTrustLevel to 0.5, update the display, set
    $Global::Break_Loop and plonk the object (if any) into place. No: remember the
    rejected extension. Returns the answer."""
    already_matched = err.already_matched
    next_elements = err.next_elements
    if already_matched:
        msg = util.perl_str(msg) + "I found the expected terms " + \
            ", ".join(util.perl_str(x) for x in already_matched)
    answer = _ask_user_extension(next_elements, msg)
    if util.perl_true(answer):
        insert_elements(*next_elements)
        Global.AcceptableTrustLevel = 0.5
        _update_display()
        Global.Break_Loop = 1
        if err.object is not None:
            plonk_into_place.call(err.from_position, err.direction, err.object)
    else:
        Global.ExtensionRejectedByUser[", ".join(util.perl_str(x) for x in next_elements)] = 1
    return answer


# ---- deletions ----------------------------------------------------------------------------------
def delete_objects_inconsistent_with(ruleapp):
    """Perl: DeleteObjectsInconsistentWith($ruleapp): uninsert every relation, then delete
    each live group the rule app finds inconsistent."""
    for reln in list(relations.values()):
        reln.uninsert()
    for gp in list(NON_ELT_OBJECTS.values()):
        if not check_liveness(gp):
            continue
        if not util.perl_true(ruleapp.check_consitency_of_group(gp)):
            delete_group(gp)


def delete_non_subgroups_of_from(opts):
    """Perl: __DeleteNonSubgroupsOfFrom({of => [...], from => [...]}): delete each live
    group in ``from`` that is not one of ``of`` or a (deep) subgroup of one."""
    of = opts.get("of")
    if of is None:
        raise Confess("need of")
    frm = opts.get("from")
    if frm is None:
        raise Confess("need from")
    from seqsee.multimethods import perl_isa
    groups_to_keep = set()
    queue = list(of)
    while queue:
        front = queue.pop(0)
        if perl_isa(front, "Seqsee::Element"):
            continue
        groups_to_keep.add(id(front))
        queue.extend(front)
    for potential_delete in frm:
        if id(potential_delete) in groups_to_keep:
            continue
        if perl_isa(potential_delete, "Seqsee::Element"):
            continue
        if not check_liveness(potential_delete):
            continue
        delete_group(potential_delete)


# ---- hooks to unported modules ----------------------------------------------------------------
def _scodelet_new(family, urgency, args):
    """Perl: SCodelet->new($family, $urgency, \\%args)."""
    from seqsee.scodelet import SCodelet
    return SCodelet(family, urgency, args)


def _coderack_add_codelet(codelet):
    """Perl: SCoderack->add_codelet($codelet)."""
    from seqsee import scoderack
    scoderack.add_codelet(codelet)


def _ask_user_extension(next_elements, msg):
    """Perl: main::ask_user_extension(\\@next_elements, $msg), installed by the UI: the
    pluggable user_interaction.ask_user_extension."""
    from seqsee import user_interaction
    return user_interaction.ask_user_extension(next_elements, msg)


def _update_display():
    """Perl: main::update_display (GUI). Headless: nothing."""
    return None


def _strength_chooser(objects):
    """Perl: ``my $strength_chooser = SChoose->create({map => \\&SFasc::get_strength})``.

    PERL-QUIRK (oracle-confirmed): SFasc::get_strength is a Class::Std accessor, and for a
    Seqsee object (not an SFasc) it reads an unset attribute, so every likelihood is undef
    and the choice is uniform (one draw)."""
    from seqsee import schoose
    return schoose.create(map=lambda o: None)(objects)


def _element_at(position):
    """Perl ``$Elements[$position]``: negative positions count from the end; outside the
    array, undef (None)."""
    n = len(ELEMENTS)
    return ELEMENTS[position] if -n <= position < n else None


# ---- distances and positions ------------------------------------------------------------------
def _has_non_adhoc_category(o):
    return util.perl_true(o.has_non_ad_hoc_category())


def get_longest_non_adhoc_with_ends_exactly(left, right):
    """Perl: __GetLongestNonAdHocWithEndsExactly($left, $right): given exactly one end, the
    longest live object with a non-ad-hoc category that has that end, else the element at
    that end (Perl indexing: negative counts from the end, beyond the end is undef)."""
    if left is not None and right is None:
        for gp in sort_r_to_l_by_right_edge(*get_objects_with_ends_exactly(left, None)):
            if _has_non_adhoc_category(gp):
                return gp
        return _element_at(left)
    if right is not None and left is None:
        for gp in sort_l_to_r_by_left_edge(*get_objects_with_ends_exactly(None, right)):
            if _has_non_adhoc_category(gp):
                return gp
        return _element_at(right)
    raise Confess("__GetLongestNonAdHocWithEndsExactly needs exactly one defined argument. "
                  f"Got '{util.perl_str(left)}' and '{util.perl_str(right)}'")


def get_longest_non_adhoc_with_left_exact_right_below(left, right):
    """Perl: __GetLongestNonAdHocWithLeftExactRightBelow($left, $right): the longest live
    non-ad-hoc object with left edge ``left`` and right edge <= ``right``, else the element
    at ``left``."""
    for gp in sort_r_to_l_by_right_edge(*get_objects_with_ends_exactly(left, None)):
        if _edge(RIGHT_EDGE_OF, gp) > right:
            continue
        if _has_non_adhoc_category(gp):
            return gp
    return _element_at(left)


def find_distance(object1, object2, requested_mode=None):
    """Perl: __FindDistance($object1, $object2, $mode): the gap between two live objects as
    a DISTANCE (Zero if adjacent or overlapping). Without a mode, DISTANCE_MODE::PickOne
    chooses (one draw, made before the adjacency check); an unrequested GROUP distance whose
    magnitude equals the gap in elements is relabelled ELEMENT.

    PERL-QUIRK: the helper always counts elements (see _find_distance_helper), so an
    unrequested GROUP distance is always relabelled."""
    from seqsee.constants import DISTANCE, DISTANCE_MODE
    check_liveness_and_diagnose(object1, object2)
    mode = requested_mode or DISTANCE_MODE.pick_one()

    min_right = min(_edge(RIGHT_EDGE_OF, object1), _edge(RIGHT_EDGE_OF, object2))
    max_left = max(_edge(LEFT_EDGE_OF, object1), _edge(LEFT_EDGE_OF, object2))
    if max_left <= min_right + 1:  # Adjacent or overlapping.
        return DISTANCE.zero()

    distance = _find_distance_helper(min_right + 1, max_left - 1, mode)
    if mode.is_unit_groups() and not requested_mode:
        magnitude = distance.get_magnitude()
        if max_left - min_right - 1 == magnitude:
            distance = DISTANCE.in_elements(magnitude)  # change units
    return distance


def _find_distance_helper(left_end_of_gap, right_end_of_gap, mode):
    """Perl: __FindDistanceHelper_($left, $right, $mode): the size of the gap, in elements,
    or (GROUP mode) greedily in non-ad-hoc groups.

    PERL-QUIRK (oracle-confirmed): the greedy loop moves to the group's *left* edge + 1, so
    each step covers one element and a GROUP distance is the gap in elements."""
    from seqsee.constants import DISTANCE
    if not mode.is_unit_groups():
        return DISTANCE.in_elements(1 + right_end_of_gap - left_end_of_gap)
    intermediate_groups_seen = 0
    while left_end_of_gap <= right_end_of_gap:
        if left_end_of_gap == right_end_of_gap:
            intermediate_groups_seen += 1
            break
        longest_first_group = get_longest_non_adhoc_with_left_exact_right_below(
            left_end_of_gap, right_end_of_gap)
        left_end_of_gap = _edge(LEFT_EDGE_OF, longest_first_group) + 1
        intermediate_groups_seen += 1
    return DISTANCE.in_groups(intermediate_groups_seen)


def get_position_in_direction_at_distance(opts):
    """Perl: __GetPositionInDirectionAtDistance({from_object, direction, distance}): the
    position ``distance`` away from the object, or None if that falls off the left end (or,
    in GROUP units, if an object is missing).

    PERL-QUIRK: with a non-GROUP distance, ``$end + $distance`` adds the DISTANCE ref's
    address (here ``id``) on the right, and ``$end >= $distance`` is always false on the
    left (None)."""
    from seqsee.constants import DIR, DISTANCE
    from_object = opts.get("from_object")
    if not util.perl_true(from_object):
        raise Confess("Need from_object")
    direction = opts.get("direction")
    if not util.perl_true(direction):
        raise Confess("Need direction")
    distance = opts.get("distance")
    if not util.perl_true(distance):
        raise Confess("Need distance")
    if not isinstance(distance, DISTANCE):
        raise Confess('require: $distance->isa("DISTANCE")')

    if direction is DIR.RIGHT:
        end = _edge(RIGHT_EDGE_OF, from_object) + 1
        if not distance.is_unit_groups():
            return end + id(distance)
        for _ in range(util.perl_num(distance.get_magnitude())):
            next_object = get_longest_non_adhoc_with_ends_exactly(end, None)
            if next_object is None:
                return None
            end = 1 + _edge(RIGHT_EDGE_OF, next_object)
        return end
    if direction is DIR.LEFT:
        end = _edge(LEFT_EDGE_OF, from_object) - 1
        if not distance.is_unit_groups():
            return None  # PERL-QUIRK: $end >= <address> is false
        for _ in range(util.perl_num(distance.get_magnitude())):
            if end < 0:
                return None
            next_object = get_longest_non_adhoc_with_ends_exactly(None, end)
            if next_object is None:
                return None
            end = _edge(LEFT_EDGE_OF, next_object) - 1
        if end < 0:
            return None
        return end
    raise Confess("HUH?")


def _has_category_named_unlike(gp, pattern):
    for c in gp.get_categories():
        if not re.search(pattern, util.perl_str(c.get_name())):
            return True
    return False


def get_longest_non_adhoc_object_starting_at(left):
    """Perl: get_longest_non_adhoc_object_starting_at($left): the longest live object
    starting at ``left`` with a category whose name lacks "Interlaced" (objects without
    categories are skipped), else the element there. Confesses "Why am I being asked
    this?" when ``left`` >= ElementCount and no object qualifies."""
    for gp in sort_r_to_l_by_right_edge(*get_objects_with_ends_exactly(left, None)):
        if _has_category_named_unlike(gp, "Interlaced"):
            return gp
    if left >= ElementCount:
        raise Confess("Why am I being asked this?")
    return _element_at(left)


def get_longest_non_adhoc_object_ending_at(right):
    """Perl: get_longest_non_adhoc_object_ending_at($right): the same, ending at ``right``
    (here the name test is /^Interlaced/), else the element there (no range check)."""
    for gp in sort_l_to_r_by_left_edge(*get_objects_with_ends_exactly(None, right)):
        if _has_category_named_unlike(gp, "^Interlaced"):
            return gp
    return _element_at(right)


def get_intervening_objects(l, r):
    """Perl: get_intervening_objects($l, $r): the longest non-ad-hoc objects tiling l..r
    from the left, or [] if the last one overshoots r."""
    if r >= ElementCount:
        raise Confess("get_intervening_objects called with right end of gap beyond known "
                      "elements")
    ret = []
    left = l
    while left <= r:
        o = get_longest_non_adhoc_object_starting_at(left)
        ret.append(o)
        left = util.perl_num(o.get_right_edge()) + 1
    if left == r + 1:  # Not overshot
        return ret
    return []


def get_position_structure(group):
    """Perl: __GetPositionStructure($group): an Element's left edge, an Anchored group's
    list of its items' structures, else None (exact classes, as ``ref`` is compared)."""
    ref = util.perl_ref(group)
    if ref == "Seqsee::Element":
        return LEFT_EDGE_OF.get(group)
    if ref == "Seqsee::Anchored":
        return [get_position_structure(x) for x in group]
    return None


def get_position_structure_as_string(group):
    """Perl: __GetPositionStructureAsString($group)."""
    return util.stringify_deep_array(get_position_structure(group))


# ---- sameness --------------------------------------------------------------------------------
def get_sameness_around(pos):
    """Perl: __GetSamenessAround($pos): the (left, right) margins of the run of equal
    magnitudes around ``pos`` (Perl indexing of @ElementMagnitudes)."""
    magnitude = _magnitude_at(pos)
    left_margin = right_margin = pos
    while left_margin > 0:
        if _magnitude_at(left_margin - 1) != magnitude:
            break
        left_margin -= 1
    while right_margin < ElementCount - 1:
        if _magnitude_at(right_margin + 1) != magnitude:
            break
        right_margin += 1
    return left_margin, right_margin


def create_sameness_group_around(pos):
    """Perl: __CreateSamenessGroupAround($pos): add a SAMENESS group over the run of equal
    magnitudes around ``pos``. Returns 1, or None if the run is a single element, if a live
    object covers the run and toss(0.5) says no, if an element has an active metonym, or if
    adding fails."""
    from seqsee import s as S
    from seqsee.objects.anchored import Anchored
    left_margin, right_margin = get_sameness_around(pos)
    span = right_margin - left_margin + 1
    if span < 2:
        return None
    covering = get_objects_with_ends_beyond(left_margin, right_margin)
    if covering and util.toss(0.5):
        return None
    items = [_element_at(i) for i in range(left_margin, right_margin + 1)]
    for item in items:
        if util.perl_true(item.get_metonym_activeness()):
            return None
    new_group = Anchored.create(*items)
    new_group.describe_as(S.SAMENESS)
    return _add_group(new_group)


# ---- checking for elements ------------------------------------------------------------------
def check_at_location(opts):
    """Perl: check_at_location({start, direction, what}): 1 if ``what``'s flattened
    magnitudes are present starting at ``start`` (rightward) or ending there (leftward),
    else None. Throws SErr::ElementsBeyondKnownSought when the check runs past the known
    elements."""
    from seqsee.constants import DIR
    direction = opts.get("direction")
    if not util.perl_true(direction):
        raise Confess("need direction")
    if opts.get("start") is None:
        raise Confess("Need start")
    start = opts["start"]
    what = opts.get("what")
    flattened = list(what.get_flattened())
    span = len(flattened)
    if direction is DIR.RIGHT:
        return check_elements_rightward_from_location(start, flattened, what, start, direction)
    if direction is DIR.LEFT:
        if span > start + 1:  # would extend beyond left edge
            return None
        left_end_of_potential_match = start - span + 1
        return check_elements_rightward_from_location(left_end_of_potential_match, flattened,
                                                      what, start, direction)
    raise Confess("Huh?")


def check_elements_rightward_from_location(start, elements_ref, object_being_looked_for=None,
                                           position_it_is_being_looked_from=None,
                                           direction_to_look_in=None):
    """Perl: CheckElementsRightwardFromLocation($start, \\@magnitudes, ...): 1 if the
    elements from ``start`` have these magnitudes, None at the first mismatch. Running past
    the known elements throws SErr::ElementsBeyondKnownSought with the magnitudes still
    unchecked (the missing one included). A negative ``start`` counts from the end."""
    flattened = list(elements_ref)
    current_pos = start - 1
    while flattened:
        current_pos += 1
        if current_pos >= ElementCount:
            raise ElementsBeyondKnownSought(next_elements=list(flattened))
        element = _element_at(current_pos)
        if element is None:
            raise Confess('Can\'t call method "get_mag" on an undefined value')
        if util.perl_num(element.get_mag()) == util.perl_num(flattened[0]):
            flattened.pop(0)
        else:
            return None
    return 1


def rapid_create_gp(cats, *items):
    """Perl: rapid_create_gp(\\@cats, @items): create a group (nested lists are
    ``[cats, items...]`` specs for subgroups), add it (the result is ignored) and describe
    it with each category; ``"metonym", cat, name`` also annotates and activates a metonym.
    Like Perl, it consumes the ``cats`` list."""
    from seqsee.objects.anchored import Anchored
    items = [rapid_create_gp(*x) if isinstance(x, list) else x for x in items]
    obj = Anchored.create(*items)
    add_group(obj)
    while cats:
        nxt = cats.pop(0)
        if isinstance(nxt, str) and nxt == "metonym":
            cat = cats.pop(0) if cats else None
            name = cats.pop(0) if cats else None
            obj.describe_as(cat)
            obj.annotate_with_metonym(cat, name)
            obj.SetMetonymActiveness(1)
        else:
            obj.describe_as(nxt)
    return obj


# ---- attribute copying and plonking ---------------------------------------------------------
def copy_attributes(opts):
    """Perl: __CopyAttributes({from, to}): copy the metonym (and its activeness), the
    relation scheme, groupness and categories (telling the directed story when the source
    has a position mode). Failed() if some category doesn't fit ``to``, else Success()."""
    from seqsee.objects.result_of_attribute_copy import ResultOfAttributeCopy
    frm, to = opts.get("from"), opts.get("to")
    if not util.perl_true(frm) or not util.perl_true(to):
        raise Confess("Missing from or to")
    any_failure_so_far = 0

    metonym = frm.get_metonym()
    if util.perl_true(metonym):
        to.annotate_with_metonym(metonym.get_category(), metonym.get_name())
        to.SetMetonymActiveness(frm.get_metonym_activeness())

    rel_scheme = frm.get_reln_scheme()
    if util.perl_true(rel_scheme):
        to.apply_reln_scheme(rel_scheme)

    if util.perl_true(frm.get_group_p()):
        to.set_group_p(1)

    for category in frm.get_categories():
        bindings = to.describe_as(category)
        if not util.perl_true(bindings):
            any_failure_so_far += 1
            continue
        position_mode_for_from = frm.describe_as(category).get_position_mode()
        if position_mode_for_from is not None:
            bindings.tell_directed_story(to, position_mode_for_from)
    if any_failure_so_far:
        return ResultOfAttributeCopy.Failed()
    return ResultOfAttributeCopy.Success()


plonk_into_place = Multimethod("__PlonkIntoPlace")


@plonk_into_place.variant("#", "DIR", "Seqsee::Element")
def _(start, direction, element):
    """Perl: __PlonkIntoPlace(#, DIR, Seqsee::Element): the workspace element at ``start``
    (Perl indexing) if it has the same magnitude, with ``element``'s attributes copied."""
    from seqsee.objects.result_of_plonk import ResultOfPlonk
    magnitude = element.get_mag()
    if util.perl_num(magnitude) != _magnitude_at(start):
        return ResultOfPlonk.Failed(element)
    attribute_copy_result = copy_attributes({"from": element, "to": _element_at(start)})
    return ResultOfPlonk(object_being_plonked=element, resultant_object=_element_at(start),
                         attribute_copy_result=attribute_copy_result)


@plonk_into_place.variant("#", "DIR", "Seqsee::Object")
def _(start, direction, obj):
    """Perl: __PlonkIntoPlace(#, DIR, Seqsee::Object): plonk the items one by one from
    ``start`` (leftward: ending at ``start``), then find or add the group made of the
    results and copy ``obj``'s attributes to it."""
    from seqsee.constants import DIR
    from seqsee.objects.anchored import Anchored
    from seqsee.objects.result_of_attribute_copy import ResultOfAttributeCopy
    from seqsee.objects.result_of_plonk import ResultOfPlonk
    span = obj.get_span()
    if not util.perl_true(span):
        return ResultOfPlonk.Failed(obj)
    span = util.perl_num(span)

    if direction is DIR.LEFT:
        if start < span - 1:
            return ResultOfPlonk.Failed(obj)
        return plonk_into_place.call(start - span + 1, DIR.RIGHT, obj)

    new_parts = []
    plonk_cursor = start
    attribute_copy_status_so_far = ResultOfAttributeCopy.Success()
    for subobject in obj.get_items_array():
        subobjectspan = util.perl_num(subobject.get_span())
        plonk_result = plonk_into_place.call(plonk_cursor, DIR.RIGHT, subobject)
        if plonk_result.plonk_was_successful():
            new_parts.append(plonk_result.resultant_object())
            plonk_cursor += subobjectspan
            attribute_copy_status_so_far.update_with(plonk_result.attribute_copy_result())
        else:
            return ResultOfPlonk.Failed(obj)

    new_obj = Anchored.create(*new_parts)
    existing_object = get_exact_object_if_present(new_obj)
    if existing_object is not None:
        new_obj = existing_object
    elif not add_group(new_obj):
        return ResultOfPlonk.Failed(obj)

    attribute_copy_result = copy_attributes({"from": obj, "to": new_obj})
    attribute_copy_status_so_far.update_with(attribute_copy_result)
    return ResultOfPlonk(object_being_plonked=obj, resultant_object=new_obj,
                         attribute_copy_result=attribute_copy_status_so_far)


# ---- looking for objects ---------------------------------------------------------------------
def _objects_at(start, direction):
    from seqsee.constants import DIR
    if direction is DIR.RIGHT:
        return get_objects_with_ends_exactly(start, None)
    if direction is DIR.LEFT:
        return get_objects_with_ends_exactly(None, start)
    return []


def _split_by_structure(objects, expected_structure_string):
    matching, potentially_matching = [], []
    for o in objects:
        if o.get_effective_object().get_structure_string() == expected_structure_string:
            matching.append(o)
        else:
            potentially_matching.append(o)
    return matching, potentially_matching


def get_something_like(opts):
    """Perl: GetSomethingLike({object, start, direction, trust_level, reason, hilit_set}):
    a live object at ``start`` (starting there rightward, ending there leftward) whose
    effective structure matches ``object``'s, chosen by the strength chooser (see
    _strength_chooser). If ``object`` is literally present it is plonked into place and
    returned outright on toss(0.5). Running past the known elements may ask the user
    (toss(trust_level * 0.02)); a refusal returns None. With the AllowSquinting feature,
    each non-matching object there gets a TryToSquint codelet."""
    obj = opts.get("object")
    start_pos = opts.get("start")
    direction = opts.get("direction")
    if obj is None or start_pos is None or direction is None:
        raise Confess("")
    reason = opts.get("reason") if util.perl_true(opts.get("reason")) else ""
    trust_level = opts.get("trust_level")
    if trust_level is None:
        raise Confess("")
    hilit_set = opts.get("hilit_set")

    matching_objects, potentially_matching_objects = _split_by_structure(
        _objects_at(start_pos, direction), obj.get_structure_string())

    is_object_literally_present = None
    check_opts = {"direction": direction, "start": start_pos, "what": obj}
    try:
        is_object_literally_present = check_at_location(check_opts)
    except ElementsBeyondKnownSought as e:
        trust_level = util.perl_num(trust_level) * 0.02  # had multiplied by 50 for toss...
        if util.toss(trust_level):  # Kludge.
            if hilit_set is None:
                raise Confess("Can't use an undefined value as an ARRAY reference")
            Global.hilit(1, *hilit_set)
            if not util.perl_true(e.ask(f"{util.perl_str(reason)}. ", "")):
                return None
            Global.clear_hilit()
            try:
                is_object_literally_present = check_at_location(check_opts)
            except Exception:  # noqa: BLE001 (Perl: a bare eval {})
                pass

    if util.perl_true(is_object_literally_present):
        plonk_result = plonk_into_place.call(start_pos, direction, obj)
        if not plonk_result:
            return None
        present_object = plonk_result.resultant_object()
        if util.toss(0.5):
            return present_object
        matching_objects.append(present_object)

    if util.perl_true(Global.Feature.get("AllowSquinting")):
        for o in potentially_matching_objects:
            _coderack_add_codelet(_scodelet_new("TryToSquint", 200,
                                                {"actual": o, "intended": obj}))

    return _strength_chooser(matching_objects)


def look_for_something_like(opts):
    """Perl: LookForSomethingLike({object, start_position, direction}): a
    ResultOfGetSomethingLike with the matching ("probable") and other ("potential")
    objects at the position, ``[start, direction, object]`` if ``object`` is literally
    present (else None), and, if the check ran past the known elements, a ``to_ask`` dict.

    PERL-QUIRK: ``start_position or confess``, so position 0 confesses."""
    from seqsee.objects.result_of_get_something_like import ResultOfGetSomethingLike
    obj = opts.get("object")
    if not util.perl_true(obj):
        raise Confess("need object")
    start_position = opts.get("start_position")
    if not util.perl_true(start_position):
        raise Confess("need start_position")
    direction = opts.get("direction")
    if not util.perl_true(direction):
        raise Confess("need direction")

    matching_objects, potentially_matching_objects = _split_by_structure(
        _objects_at(start_position, direction), obj.get_structure_string())

    is_object_literally_present = None
    to_ask = None
    try:
        is_object_literally_present = check_at_location(
            {"direction": direction, "start": start_position, "what": obj})
    except ElementsBeyondKnownSought as e:
        to_ask = {"expected_object": obj, "exception": e, "start_position": start_position}

    if util.perl_true(is_object_literally_present):
        is_object_literally_present = [start_position, direction, obj]

    return ResultOfGetSomethingLike({
        "to_ask": to_ask,
        "literally_present": is_object_literally_present,
        "probable_matches": matching_objects,
        "potential_matches": potentially_matching_objects,
    })

_insert_element_object = Multimethod("__InsertElement")


@_insert_element_object.variant("Seqsee::Element")
def _(element):
    """Perl: __InsertElement(Seqsee::Element)."""
    global ElementCount
    magnitude = element.get_mag()
    element.set_edges(ElementCount, ElementCount)
    ELEMENTS.append(element)
    ELEMENT_MAGNITUDES.append(magnitude)
    Global.update_extensions_rejected_by_user(magnitude)

    LEFT_EDGE_OF[element] = ElementCount
    RIGHT_EDGE_OF[element] = ElementCount
    SPAN_OF[element] = 1
    OBJECTS[element] = element
    LIVE_AT_SOME_POINT[element] = 1
    SUPER_GROUPS_OF[element] = {}

    sltm.insert_unless_present(element)
    ElementCount += 1


# ---- clear / init / insert ------------------------------------------------------------------
def clear():
    """Perl: clear: starts the workspace off as new."""
    global ElementCount, ReadHead
    ElementCount = 0
    elements.clear()
    relations.clear()
    ReadHead = 0
    _clear()


def reset():
    """Test hook: ``clear`` plus what Perl's clear keeps (%LiveAtSomePoint, the bar lines
    and %relations_by_ends)."""
    clear()
    LIVE_AT_SOME_POINT.clear()
    relations_by_ends.clear()
    clear_bar_lines()


def init(options):
    """Perl: init($OPTIONS_ref): clear, then insert ``options["seq"]``."""
    clear()
    seq = list(options["seq"])
    for x in seq:
        _insert_element.call(x)
    Global.RealSequence[:] = seq
    Global.InitialTermCount = len(seq)


def insert_elements(*items):
    """Perl: insert_elements(@items)."""
    for x in items:
        _insert_element.call(x)
    Global.TimeOfLastNewElement = Global.Steps_Finished
    Global.TimeOfNewStructure = Global.Steps_Finished


_insert_element = Multimethod("_insert_element")


def _element_create(mag):
    from seqsee.objects.element import Element
    return Element.create(mag, 0)  # bogus edges, fixed on insertion


@_insert_element.variant("#")
def _(mag):
    _insert_element.call(_element_create(mag))


_INF_NAN = re.compile(r"\s*([+-]?)(inf(?:inity)?|nan)\s*", re.IGNORECASE)


def _perl_int(string):
    """Perl ``int`` of a string that looks like a number."""
    m = _INF_NAN.fullmatch(string)
    if m:
        if m.group(2).lower() == "nan":
            return math.nan
        return -math.inf if m.group(1) == "-" else math.inf
    n = util.perl_num(string)
    if isinstance(n, float):
        if abs(n) >= 2.0 ** 63:
            return float(math.trunc(n))  # Perl keeps an NV beyond the IV range
        return int(n)
    return n


@_insert_element.variant("$")
def _(what):
    if util.looks_like_number(what):
        _insert_element.call(_element_create(_perl_int(what)))
    else:
        raise Confess(f"Huh? Trying to insert '{util.perl_str(what)}' into the workspace")


@_insert_element.variant("Seqsee::Element")
def _(elt):
    Global.ExtensionRejectedByUser.clear()
    _insert_element_object.call(elt)


# ---- scanning ------------------------------------------------------------------------------
def _magnitude_at(position):
    """Perl ``$ElementMagnitudes[$position]``, numified: negative positions count from the
    end, and positions outside the array read undef (0)."""
    n = len(ELEMENT_MAGNITUDES)
    if -n <= position < n:
        return util.perl_num(ELEMENT_MAGNITUDES[position])
    return 0


def scan_rightward_for_elements(start_position, magnitudes):
    """Perl: __ScanRightwardForElements: the leftmost position >= start_position where the
    magnitudes occur, or None."""
    hunt = [util.perl_num(m) for m in magnitudes]
    largest_possible_leftmost_position = ElementCount - len(hunt)
    for leftmost in range(start_position, largest_possible_leftmost_position + 1):
        if all(_magnitude_at(leftmost + i) == m for i, m in enumerate(hunt)):
            return leftmost
    return None


def scan_leftward_for_elements(start_position, magnitudes):
    """Perl: __ScanLeftwardForElements: the rightmost leftmost-position whose run ends at or
    before start_position, scanning leftward, or None."""
    hunt = [util.perl_num(m) for m in magnitudes]
    leftmost = start_position - len(hunt) + 1
    while leftmost >= 0:
        if all(_magnitude_at(leftmost + i) == m for i, m in enumerate(hunt)):
            return leftmost
        leftmost -= 1
    return None


def check_magnitudes_rightwards(start_position, expected_magnitudes):
    """Perl: __CheckMagnitudesRightwards: 1 if the elements from start_position have the
    expected magnitudes, 0 if one differs. Running past the known elements throws
    SErr::ElementsBeyondKnownSought with the magnitudes after the missing one."""
    position = start_position
    expected = list(expected_magnitudes)
    while expected:
        next_expected = expected.pop(0)
        if position >= ElementCount:
            raise ElementsBeyondKnownSought(next_elements=expected)
        if _magnitude_at(position) == util.perl_num(next_expected):
            position += 1
        else:
            return 0
    return 1


# ---- bar lines -----------------------------------------------------------------------------
def clear_bar_lines():
    """Perl: __ClearBarLines."""
    global BarLineCount
    BAR_LINES.clear()
    BarLineCount = 0


def add_bar_lines(*indices):
    """Perl: __AddBarLines(@indices): merged in and sorted numerically (duplicates kept)."""
    global BarLineCount
    BAR_LINES[:] = sorted(BAR_LINES + list(indices), key=util.perl_num)
    BarLineCount = len(BAR_LINES)


def get_bar_lines():
    """Perl: GetBarLines."""
    return list(BAR_LINES)


def closest_bar_line_to_left_given_index(index):
    """Perl: __ClosestBarLineToLeftGivenIndex: the last bar line <= index, or None."""
    if not BarLineCount:
        return None
    if util.perl_num(BAR_LINES[0]) > index:
        return None
    count = 1
    while count < BarLineCount and util.perl_num(BAR_LINES[count]) <= index:
        count += 1
    return BAR_LINES[count - 1]


def closest_bar_line_to_right_given_index(index):
    """Perl: __ClosestBarLineToRightGivenIndex: the first bar line > index, or None."""
    if not BarLineCount:
        return None
    if util.perl_num(BAR_LINES[-1]) <= index:
        return None
    count = BarLineCount - 2
    while count > -1 and util.perl_num(BAR_LINES[count]) > index:
        count -= 1
    return BAR_LINES[count + 1]


def check_if_crosses_bar_lines_inappropriately(group):
    """Perl: __CheckIfCrossesBarLinesInappropriately: 1 if a bar line falls inside the
    group without bar lines at both of its ends, else None."""
    l, r = LEFT_EDGE_OF.get(group), RIGHT_EDGE_OF.get(group)
    closest_bar_line_before_end = closest_bar_line_to_left_given_index(r)
    if closest_bar_line_before_end is None:
        return None
    if util.perl_num(closest_bar_line_before_end) <= l:
        return None  # No crossing!

    # Crossing exists. It is appropriate if there are bar lines at either end.
    if util.perl_num(closest_bar_line_to_left_given_index(l)) != l:
        return 1
    rightward_closest = closest_bar_line_to_right_given_index(r)
    if rightward_closest is None:
        return None
    if util.perl_num(rightward_closest) != r + 1:
        return 1
    return None
