"""Port of lib/Seqsee/SCF_MX/AllMX.pm: the families CheckIfInstance, FocusOn,
ActOnOverlappingThoughts (+ the ActionForThoughtTypes multimethod), AreTheseGroupable,
AreWeDone (+ BelieveDone), ConvulseEnd and CheckProgress (+ CalculateDesperation).

Each family is registered with ``@codelet_family`` under its Perl name (package
``Seqsee::SCF::<Name>``); the body gets the validated arguments positionally.

Module state: AreWeDone's file lexical ``$LastSolutionDescriptionTime`` →
``_last_solution_description_time``; CheckProgress's ``state $last_time_progresschecker_run``
→ ``_last_time_progresschecker_run``. ``reset()`` restores both (conftest).
UI hooks (headless): ``_message`` (main::message, logged), ``_update_display`` (no-op) and
``_ask_for_more_terms`` (main::ask_for_more_terms, logged).
"""
import logging
import sys

from seqsee import sworkspace, util
from seqsee.codelets.family import action, codelet_family
from seqsee.codelets.general import _perl_eq, _perl_eval
from seqsee.errors import Confess, CouldNotCreateExtendedGroup
from seqsee.multimethods import Multimethod, perl_isa

_log = logging.getLogger(__name__)

_last_solution_description_time = None   # Perl: my $LastSolutionDescriptionTime (AreWeDone)
_last_time_progresschecker_run = 0       # Perl: state $last_time_progresschecker_run = 0


def reset():
    """Forget the module state (for tests; Perl keeps it for the whole process)."""
    global _last_solution_description_time, _last_time_progresschecker_run
    _last_solution_description_time = None
    _last_time_progresschecker_run = 0


def _message(msg, *rest):
    """Perl: main::message (UI). Headless: logged."""
    _log.info("%s", msg)


def _update_display():
    """Perl: main::update_display (GUI). Headless: nothing."""


def _ask_for_more_terms():
    """Perl: main::ask_for_more_terms (UI). Headless: logged."""
    _log.info("ask_for_more_terms")


def _action(urgency, family, options):
    """Perl: ACTION($urgency, $family, $options)."""
    return action(urgency, family, options)


# ---- CheckIfInstance --------------------------------------------------------------------
@codelet_family("CheckIfInstance", attributes=[("obj", {}), ("cat", {})])
def check_if_instance(obj, cat):
    """Perl: Seqsee::SCF::CheckIfInstance. Describes ``obj`` as ``cat``; under the LTM
    feature, then spikes the category by 10 and the obj→cat ISA link by 5.

    PERL-QUIRK (oracle-confirmed): Seqsee objects have no InsertISALink method (only
    categories do, via LTMStorable), so with LTM on a successful describe spikes the category
    and then dies."""
    from seqsee import global_ as Global
    from seqsee import slink_activation as sla
    if util.perl_true(obj.describe_as(cat)) and util.perl_true(Global.Feature.get("LTM")):
        cat.spike_by(10)
        insert_isa_link = getattr(obj, "insert_isa_link", None)
        if insert_isa_link is None:
            raise Confess(f'Can\'t locate object method "InsertISALink" via package '
                          f'"{util.perl_ref(obj)}"')
        sla.spike(insert_isa_link(cat), 5)
    return None


# ---- FocusOn ----------------------------------------------------------------------------
@codelet_family("FocusOn", attributes=[("what", {"optional": 1})])
def focus_on(what):
    """Perl: Seqsee::SCF::FocusOn. Thinks about ``what`` (ContinueWith its thought). Without
    it, acts as the reader: with probability 0.3 makes a sameness group around ReadHead and
    stops; else reads an object or relation and thinks about it."""
    from seqsee import global_ as Global
    from seqsee.codelets.scf import continue_with
    from seqsee.sthought import SThought
    if util.perl_true(what):
        # ContinueWith(SThought->create($what)): create in list context.
        continue_with(SThought.create(what, list_context=True))
        return None
    # Equivalent to Reader
    if util.toss(0.3):
        sworkspace.create_sameness_group_around(sworkspace.ReadHead)
        return None
    obj = sworkspace.read_object_or_relation()
    if obj is None:
        return None
    if util.perl_true(Global.debugMAX):
        _message("Focusing on: " + obj.as_text())
    continue_with(SThought.create(obj, list_context=True))
    return None


# ---- ActOnOverlappingThoughts -----------------------------------------------------------
ACTION_FOR_THOUGHT_TYPES = Multimethod("ActionForThoughtTypes")


@ACTION_FOR_THOUGHT_TYPES.variant("*", "*")
def _action_default(a_core, b_core):
    """By default, do nothing."""
    return None


@ACTION_FOR_THOUGHT_TYPES.variant("SRelation", "SRelation")
def _action_relations(a_core, b_core):
    return _action(100, "FindIfRelatedRelations", {"a": a_core, "b": b_core})


@ACTION_FOR_THOUGHT_TYPES.variant("Seqsee::Object", "Seqsee::Object")
def _action_objects(a_core, b_core):
    return _action(100, "FindIfRelated", {"a": a_core, "b": b_core})


def action_for_thought_types(a_core, b_core):
    """Perl: Seqsee::SCF::ActOnOverlappingThoughts::ActionForThoughtTypes (multimethod):
    two relations → ACTION FindIfRelatedRelations/100, two objects → ACTION FindIfRelated/100,
    anything else (mixed, categories, undef) → nothing."""
    return ACTION_FOR_THOUGHT_TYPES.call(a_core, b_core)


def _core_if_can(x):
    """Perl: ``$x->can('core') ? $x->core() : undef``."""
    if x is None:
        raise Confess('Can\'t call method "can" on an undefined value')
    if util.perl_ref(x) == "":
        return None  # "foo"->can('core'): no such package
    core = getattr(x, "core", None)
    return core() if callable(core) else None


@codelet_family("ActOnOverlappingThoughts", attributes=[("a", {}), ("b", {})])
def act_on_overlapping_thoughts(a, b):
    """Perl: Seqsee::SCF::ActOnOverlappingThoughts. Acts on the two thoughts' cores (see
    ``action_for_thought_types``); a non-thought has no core (undef)."""
    a_core = _core_if_can(a)
    b_core = _core_if_can(b)
    action_for_thought_types(a_core, b_core)
    return None


# ---- AreTheseGroupable ------------------------------------------------------------------
@codelet_family("AreTheseGroupable", attributes=[("items", {}), ("reln", {})])
def are_these_groupable(items, reln):
    """Perl: Seqsee::SCF::AreTheseGroupable. Makes a group of the items (unless it exists,
    or overlaps groups and a 0.2 toss to fight loses), with ``reln`` as underlying rule, adds
    it and describes it by the relation type (MappingBased for structural or non-NUMBER
    types, else SAMENESS/ASCENDING/DESCENDING by name).

    PERL-QUIRK: the items are sorted (which checks liveness) but the group is built from the
    unsorted list, so out-of-order items make no group (oracle-confirmed). The add_group
    result is ignored: the group is described even if it was not added."""
    from seqsee import s as S
    from seqsee.objects.anchored import Anchored
    if items is None:
        raise Confess("Can't use an undefined value as an ARRAY reference")
    if not sworkspace.sort_l_to_r_by_left_edge(*items):
        return None

    # Create potential group first:
    concrete_items = [x.get_concrete_object() for x in items]
    if not sworkspace.check_liveness(*concrete_items):  # dead objects.
        return None
    new_group = Anchored.create(*concrete_items)
    if not util.perl_true(new_group):  # Failed _CheckValidity!
        return None

    conflicts = sworkspace.find_groups_conflicting_with(new_group)
    if util.perl_true(conflicts.exact_conflict()):
        return None  # Already exists!
    if util.perl_true(conflicts.has_overlapping_conflicts()):
        # Should I fight?
        if util.toss(0.2):
            if not util.perl_true(conflicts.resolve()):
                return None

    # If we get here, all conflicting incumbents are dead.
    new_group.set_underlying_ruleapp(reln)
    sworkspace.add_group(new_group)
    reln_type = reln.get_type()
    if perl_isa(reln_type, "Mapping::Structural") or reln_type.get_category() is not S.NUMBER:
        from seqsee.categories.mapping_based import MappingBased
        if not util.perl_true(new_group.describe_as(MappingBased.create(reln_type))):
            _message(f"Unable to describe {new_group.as_text()}  as based on "
                     f"{reln_type.as_text()}")
    else:
        cat = {"same": S.SAMENESS, "succ": S.ASCENDING,
               "pred": S.DESCENDING}.get(util.perl_str(reln_type.get_name()))
        if not util.perl_true(cat):
            raise Confess(f"Should not be here ({util.perl_ref_string(reln_type)})")
        new_group.describe_as(cat)
    return None


# ---- AreWeDone --------------------------------------------------------------------------
def believe_done(group):
    """Perl: Seqsee::SCF::AreWeDone::BelieveDone. In testing mode throws
    SErr::FinishedTest(got_it => 1). Otherwise, unless a solution was described after the
    last new element, ACTION DescribeSolution/100.

    PERL-QUIRK: a description time of 0 is false, so at step 0 it fires every time."""
    global _last_solution_description_time
    from seqsee import global_ as Global
    from seqsee.errors import FinishedTest
    if util.perl_true(Global.TestingMode):
        # Currently assume belief always right.
        raise FinishedTest(got_it=1)
    last = _last_solution_description_time
    if util.perl_true(last) and util.perl_num(last) > util.perl_num(Global.TimeOfLastNewElement):
        return None
    _last_solution_description_time = Global.Steps_Finished
    _message("I believe I got it", 1)
    return _action(100, "DescribeSolution", {"group": group})


def _ratio(span, total):
    total = util.perl_num(total)
    if total == 0:
        raise Confess("Illegal division by zero")
    return util.perl_num(span) / total


@codelet_family("AreWeDone", attributes=[("group", {})])
def are_we_done(group):
    """Perl: Seqsee::SCF::AreWeDone. A group over half the sequence makes its rule app the
    recent one. Once the user has verified a term, a group over 80% starting at the left
    edge is either the whole sequence (BelieveDone) or worth extending rightward
    (ACTION AttemptExtensionOfGroup/80)."""
    from seqsee import global_ as Global
    from seqsee.constants import DIR
    gp = group
    span = gp.get_span()
    total_count = sworkspace.ElementCount
    left_edge = gp.get_left_edge()
    underlying_rule_app = gp.get_underlying_reln()

    if _ratio(span, total_count) > 0.5:
        if util.perl_true(underlying_rule_app):
            Global.set_rule_app_as_recent(underlying_rule_app)

    if util.perl_true(Global.AtLeastOneUserVerification) and _ratio(span, total_count) > 0.8:
        if left_edge == 0:
            if span == total_count:
                # Bingo!
                Global.clear_hilit()
                Global.hilit(2, *gp)
                _update_display()
                believe_done(group)
            else:
                _action(80, "AttemptExtensionOfGroup", {"object": gp, "direction": DIR.RIGHT})
    return None


# ---- ConvulseEnd ------------------------------------------------------------------------
@codelet_family("ConvulseEnd", attributes=[("object", {}), ("direction", {})])
def convulse_end(object, direction):  # noqa: A002 (Perl's name)
    """Perl: Seqsee::SCF::ConvulseEnd. If the group's extension in ``direction`` that skips
    its end item (FindExtension($direction, 1)) is a different object, swaps the end item
    for it: eject the end, Extend with the new object, and put the end back if Extend says
    no. The rule app is sanity-checked before and after looking. A
    SErr::CouldNotCreateExtendedGroup confesses "Unable to extend group!"; other errors
    propagate."""
    from seqsee.constants import DIR
    from seqsee.sanity import sanity_check
    if not sworkspace.check_liveness(object):
        return None
    change_at_end_p = 1 if _perl_eq(direction, DIR.RIGHT) else 0
    object_parts = object.get_items_array()
    if object_parts:
        ejected_object = object_parts.pop() if change_at_end_p else object_parts.pop(0)
    else:
        ejected_object = None

    underlying_reln = object.get_underlying_reln()
    if util.perl_true(underlying_reln):
        sanity_check(object, underlying_reln, "Pre-extension")

    new_extension = object.find_extension(direction, 1)
    if not util.perl_true(new_extension):
        return None
    new_extension = new_extension.get_concrete_object()

    if new_extension is not ejected_object:
        if util.perl_true(underlying_reln):
            sanity_check(object, underlying_reln, "post-extension")

        structure_string_before_ejection = object.as_text()
        parts = object.get_parts_ref()
        ejected_object = parts.pop() if change_at_end_p else parts.pop(0)
        sworkspace.remove_from_supergroups_of(ejected_object, object)
        object.recalculate_edges()

        ok, extended = _perl_eval(object.extend, new_extension, change_at_end_p)
        if not ok:
            if isinstance(extended, CouldNotCreateExtendedGroup):
                err = sys.stderr
                err.write(f"(structure before ejection): {structure_string_before_ejection}\n")
                err.write(f"Extending group: {object.as_text()}\n")
                err.write(f"(But effectively): {object.get_effective_structure_string()}")
                err.write(f"Ejected object: {ejected_object.get_structure_string()}\n")
                err.write(f"(But effectively): {ejected_object.get_effective_structure_string()}")
                err.write(f"New object: {new_extension.get_structure_string()}\n")
                err.write(f"(But effectively): {new_extension.get_effective_structure_string()}")
                raise Confess("Unable to extend group!")
            raise extended

        if not util.perl_true(extended):
            # main::message("Failed to extend, and no deaths!");
            if change_at_end_p:
                parts.append(ejected_object)
            else:
                parts.insert(0, ejected_object)
            object.recalculate_edges()
    return None


# ---- CheckProgress ----------------------------------------------------------------------
@codelet_family("CheckProgress", attributes=[])
def check_progress():
    """Perl: Seqsee::SCF::CheckProgress. At most once per 100 steps: by the desperation
    (time since new structure / new elements) asks for more terms (>50), removes a group
    chosen by 100 - strength (>30), or uninserts old weak relations (>10; two tosses each).

    The relations are visited in Perl hash order there and insertion order here, so with
    several relations the draws can pair up differently."""
    global _last_time_progresschecker_run
    from seqsee import global_ as Global
    from seqsee import schoose
    steps = util.perl_num(Global.Steps_Finished)
    time_since_last_addn = steps - util.perl_num(Global.TimeOfNewStructure)
    time_since_new_elements = steps - util.perl_num(Global.TimeOfLastNewElement)
    time_since_codelet_run = steps - util.perl_num(_last_time_progresschecker_run)

    # Don't run too frequently
    if time_since_codelet_run < 100:
        return None
    _last_time_progresschecker_run = Global.Steps_Finished

    desperation = calculate_desperation(time_since_last_addn, time_since_new_elements)

    # Perl: SChoose->create({ map => q{100 - $_->get_strength()} })
    chooser_on_inv_strength = schoose.create(map=lambda x: 100 - util.perl_num(x.get_strength()))
    if desperation > 50:
        _ask_for_more_terms()
    elif desperation > 30:
        gp = chooser_on_inv_strength(list(sworkspace.get_groups()))
        if util.perl_true(gp):
            sworkspace.remove_gp(gp)
    elif desperation > 10:
        for reln in list(sworkspace.relations.values()):
            age = reln.get_age()
            if (util.toss((100 - util.perl_num(reln.get_strength())) / 200)
                    and util.toss(util.perl_num(age) / 400)):
                reln.uninsert()
    return None


_CUTOFFS = ((1500, 0, 80), (800, 2500, 80), (500, 0, 40), (200, 0, 20))


def calculate_desperation(time_since_last_addn, time_since_new_elements):
    """Perl: Seqsee::SCF::CheckProgress::CalculateDesperation: the first cutoff
    (addn >= a and new_elements >= b) gives c; else 0."""
    for a, b, c in _CUTOFFS:
        if (util.perl_num(time_since_last_addn) >= a
                and util.perl_num(time_since_new_elements) >= b):
            return c
    return 0
