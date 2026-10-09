"""Port of lib/Seqsee/Scripts/DescribeSolution.pm: the scripts that describe a solution
(DescribeSolution, DescribeInitialBlemish, DescribeBlocks, DescribeRule, DescribeMapping,
DescribeRelationSimple, DescribeRelationCompound, DescribeRelnCategory,
DescribeInterlacedCategory, Describe2InterlacedCategory,
DescribeMultipleInterlacedCategory, DescribeRelnMetoMode).
DescribeSolution2.pm and Scripts/Load.pm have no code of their own.

Each script is registered with ``define_script`` (seqsee/scripts/__init__.py) under its Perl
name. Steps get the validated arguments positionally. Because of the shared spec cache
(PERL-QUIRK, see seqsee.scripts), a step can get another script's arguments, so every
parameter defaults to None and extras are ignored, as with Perl's ``my (...) = @_``.

Quirks kept from the Perl:
- Scripts that end without RETURN (DescribeRelationSimple, DescribeRelnCategory,
  Describe2InterlacedCategory, ...) never resume their caller. DescribeRule's
  SetRuleAppAsBest/RETURN after its SCRIPT call are never reached.
- DescribeSolution step 3 no longer describes the rule ("RULE DESCRIPTION CURRENTLY
  BROKEN!!").

UI hooks (headless): ``_message`` (main::message) and ``_debug_message``
(main::debug_message) are logged. ``_sltm_dump`` is SLTM->Dump. The Yes/No question goes
through ``user_interaction.response``.
"""
import logging

from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sworkspace, user_interaction, util
from seqsee.categorizable import get_common_categories
from seqsee.errors import Confess
from seqsee.multimethods import perl_isa
from seqsee.position_structure import PositionStructure
from seqsee.scripts import RETURN, SCRIPT, define_script

_log = logging.getLogger(__name__)


def _message(msg, *rest):
    """Perl: main::message (UI). Headless: logged."""
    _log.info("%s", msg)


def _debug_message(msg, *rest):
    """Perl: main::debug_message (UI). Headless: logged."""
    _log.debug("%s", msg)


def _sltm_dump(filename):
    """Perl: SLTM->Dump($filename)."""
    from seqsee import sltm
    sltm.dump(filename)


def _str(x):
    """Perl string interpolation of a scalar or a ref."""
    if util._is_scalar(x) or x is None or isinstance(x, bool):
        return util.perl_str(x)
    return util.perl_ref_string(x)


def _ss(obj):
    """``$obj->get_structure_string()`` as a string (an element's is its number)."""
    return util.perl_str(obj.get_structure_string())


# ---- DescribeSolution -----------------------------------------------------------------
def _ds_check(group=None, *_):
    """Stop unless the group has a rule app whose solution was not rejected."""
    if group is None:
        raise Confess('Can\'t call method "get_underlying_reln" on an undefined value')
    ruleapp = group.get_underlying_reln()
    if not util.perl_true(ruleapp):
        RETURN()
    rule = ruleapp.get_rule()
    position_structure = PositionStructure.create(group)
    if util.perl_true(user_interaction.SolutionConfirmation.has_this_been_rejected(
            rule, position_structure)):
        RETURN()


def _ds_initial_blemish(group=None, *_):
    ruleapp = group.get_underlying_reln()
    if util.perl_true(ruleapp):
        sworkspace.delete_objects_inconsistent_with(ruleapp)
    _message("I will describe the solution now!", 1)
    SCRIPT("DescribeInitialBlemish", {"group": group})


def _ds_blocks(group=None, *_):
    SCRIPT("DescribeBlocks", {"group": group})


def _ds_rule(group=None, *_):
    ruleapp = group.get_underlying_reln()
    ruleapp.get_rule()
    _message("RULE DESCRIPTION CURRENTLY BROKEN!!", 1)


def _ds_dump(group=None, *_):
    if util.perl_true(Global.Feature.get("LTM")):
        _sltm_dump("memory_dump.dat")


def _ds_finished(group=None, *_):
    _message("That finishes the description!", 1)


def _ds_confirm(group=None, *_):
    response = user_interaction.response(["Yes", "No"],
                                         "Does this generate the sequence you had in mind?")
    rule = group.get_underlying_reln().get_rule()
    group_position = PositionStructure.create(group)
    if util.perl_str(response) == "Yes":
        user_interaction.SolutionConfirmation.set_accepted_solution(rule, group_position)
    else:
        user_interaction.SolutionConfirmation.add_rejected_solution(rule, group_position)


define_script("DescribeSolution", [("group", {})], [
    _ds_check, _ds_initial_blemish, _ds_blocks, _ds_rule, _ds_dump, _ds_finished, _ds_confirm,
])


# ---- DescribeInitialBlemish, DescribeBlocks -------------------------------------------
def describe_initial_blemish(group=None, *_):
    """Perl: Seqsee::SCF::DescribeInitialBlemish. Names the elements left of the group."""
    le = group.get_left_edge()
    if util.perl_true(le):
        initial_bl = [e.get_mag() for e in sworkspace.get_elements()[0:int(util.perl_num(le))]]
        _message("There is an initial blemish in the sequence: "
                 + ", ".join(util.perl_str(m) for m in initial_bl)
                 + (" don't fit" if len(initial_bl) > 1 else " doesn't fit"), 1)
    RETURN()


define_script("DescribeInitialBlemish", [("group", {"required": 1})], [describe_initial_blemish])


def describe_blocks(group=None, *_):
    """Perl: Seqsee::SCF::DescribeBlocks. Lists the group's parts."""
    msg = "; ".join(_ss(part) for part in group)
    _message(f"The sequence consists of the blocks {msg}", 1)
    RETURN()


define_script("DescribeBlocks", [("group", {"required": 1})], [describe_blocks])


# ---- DescribeRule, DescribeMapping, DescribeRelationSimple ------------------------------
def describe_rule(rule=None, ruleapp=None, *_):
    """Perl: Seqsee::SCF::DescribeRule. Describes the rule's transform (DescribeMapping).

    PERL-QUIRK: SCRIPT throws, so SetRuleAppAsBest and RETURN are never reached."""
    _debug_message(f"Rule is {_str(rule)}", 1)
    reln = rule.get_transform()
    SCRIPT("DescribeMapping", {"reln": reln, "ruleapp": ruleapp})
    Global.set_rule_app_as_best(ruleapp)
    RETURN()


define_script("DescribeRule", [("rule", {"required": 1}), ("ruleapp", {"required": 1})],
              [describe_rule])


def describe_mapping(reln=None, ruleapp=None, *_):
    """Perl: Seqsee::SCF::DescribeMapping. Structural → DescribeRelationCompound, numeric →
    DescribeRelationSimple."""
    if perl_isa(reln, "Mapping::Structural"):
        SCRIPT("DescribeRelationCompound", {"reln": reln, "ruleapp": ruleapp})
    elif perl_isa(reln, "Mapping::Numeric"):
        SCRIPT("DescribeRelationSimple", {"reln": reln})
    else:
        _message("Strange bond! Something wrong, let abhijit know", 1)


define_script("DescribeMapping", [("reln", {"required": 1}), ("ruleapp", {"default": 0})],
              [describe_mapping])


def describe_relation_simple(reln=None, *_):
    """Perl: Seqsee::SCF::DescribeRelationSimple (no RETURN)."""
    string = util.perl_str(reln.get_name())
    msg = "Each succesive term is the "
    if string == "succ":
        msg += "successor "
    elif string == "pred":
        msg += "predecessor "
    elif string == "same":
        msg += "same as "
    cat = reln.get_category()
    if cat is S.NUMBER or string == "same":
        msg += "the previous term"
    else:
        msg += "the previous term seen as a " + util.perl_str(cat.get_name())
    _message(msg, 1)


define_script("DescribeRelationSimple", [("reln", {"required": 1})], [describe_relation_simple])


# ---- DescribeRelationCompound and the category scripts -----------------------------------
def _drc_category(reln=None, ruleapp=None, *_):
    category = reln.get_category()
    SCRIPT("DescribeRelnCategory", {"cat": category, "ruleapp": ruleapp})


def _drc_meto_mode(reln=None, ruleapp=None, *_):
    meto_mode = reln.get_meto_mode()
    meto_reln = reln.get_metonymy_reln()
    SCRIPT("DescribeRelnMetoMode", {"meto_mode": meto_mode, "meto_reln": meto_reln,
                                    "ruleapp": ruleapp})


define_script("DescribeRelationCompound",
              [("reln", {"required": 1}), ("ruleapp", {"required": 1})],
              [_drc_category, _drc_meto_mode])


def describe_reln_category(cat=None, ruleapp=None, *_):
    """Perl: Seqsee::SCF::DescribeRelnCategory (no RETURN)."""
    if perl_isa(cat, "SCategory::Interlaced"):
        SCRIPT("DescribeInterlacedCategory", {"cat": cat, "ruleapp": ruleapp})
    else:
        name = util.perl_str(cat.get_name())
        _message(f"Each block is an instance of {name}. "
                 "(Better descriptions of categories will be implemented)", 1)


define_script("DescribeRelnCategory", [("cat", {"required": 1}), ("ruleapp", {"required": 1})],
              [describe_reln_category])


def describe_interlaced_category(cat=None, ruleapp=None, *_):
    """Perl: Seqsee::SCF::DescribeInterlacedCategory (no RETURN)."""
    parts = cat.get_parts_count()
    if util.perl_num(parts) == 2:
        SCRIPT("Describe2InterlacedCategory", {"cat": cat, "ruleapp": ruleapp})
    else:
        SCRIPT("DescribeMultipleInterlacedCategory", {"cat": cat, "ruleapp": ruleapp})


define_script("DescribeInterlacedCategory",
              [("cat", {"required": 1}), ("ruleapp", {"required": 1})],
              [describe_interlaced_category])


def _first_and_second_items(ruleapp):
    items = list(ruleapp.get_items())
    first_items = [item[0] for item in items]
    second_items = [item[1] for item in items]
    if len(first_items) > 3:
        first_items = first_items[0:3]
    if len(second_items) > 3:
        second_items = second_items[0:3]
    return first_items, second_items


def _also_categories(categories):
    """PERL-QUIRK: the slice ``@categories[2 .. $#categories]`` skips index 1, and the
    sentence starts after a ". " (two spaces)."""
    msg = " and also of the categories "
    msg += ", ".join("'" + c.as_text() + "'" for c in categories[2:])
    return msg + ". "


def describe_2_interlaced_category(cat=None, ruleapp=None, *_):
    """Perl: Seqsee::SCF::Describe2InterlacedCategory (no RETURN). Categories shared by the
    first (second) items come in Perl hash order there; here first-seen order."""
    first_items, second_items = _first_and_second_items(ruleapp)
    msg = "The group is thus made up of a size-2 template. "
    first_categories = get_common_categories(*first_items)
    second_categories = get_common_categories(*second_items)

    if first_categories:
        msg += ("An instance of the first item in the template is an instance of '"
                + first_categories[0].as_text() + "'. ")
        if len(first_categories) > 1:
            msg += _also_categories(first_categories)

    if second_categories:
        msg += ("The second item in the template is an instance of '"
                + second_categories[0].as_text() + "'. ")
        if len(second_categories) > 1:
            msg += _also_categories(second_categories)

    msg += ("The sequence can also be thought as consisting of two interlaced sequences. "
            "Seen this way, the first of these interlaced groups consists of "
            + ", ".join(_ss(x) for x in first_items)
            + " and so forth, whereas the second consists of "
            + ", ".join(_ss(x) for x in second_items)
            + " and so forth.")
    _message(msg, 1)


define_script("Describe2InterlacedCategory",
              [("cat", {"required": 1}), ("ruleapp", {"required": 1})],
              [describe_2_interlaced_category])


def describe_multiple_interlaced_category(cat=None, ruleapp=None, *_):
    """Perl: Seqsee::SCF::DescribeMultipleInterlacedCategory (no RETURN)."""
    first_items, second_items = _first_and_second_items(ruleapp)
    count = util.perl_str(cat.get_parts_count())
    msg = (f"The sequence consists of {count} interlaced sequences. "
           "The first of these consists of "
           + ", ".join(_ss(x) for x in first_items)
           + f" and so forth, and the second of these {count} sequences consists of "
           + ", ".join(_ss(x) for x in second_items)
           + " and so forth.")
    _message(msg, 1)


define_script("DescribeMultipleInterlacedCategory",
              [("cat", {"required": 1}), ("ruleapp", {"required": 1})],
              [describe_multiple_interlaced_category])


def describe_reln_meto_mode(meto_mode=None, meto_reln=None, ruleapp=None, *_):
    """Perl: Seqsee::SCF::DescribeRelnMetoMode. Without metonymy, RETURN; else describe how
    the first (up to) three items are squinted (no RETURN)."""
    if not util.perl_true(meto_mode.is_metonymy_present()):
        RETURN()
    _message("I am squinting in order to see the blocks as instances of that category", 1)
    items = list(ruleapp.get_items())
    to_describe = items[0:3] if len(items) > 3 else items
    _message("I am seeing: ", 1)
    for item in to_describe:
        _message("\t" + _ss(item) + " is being seen as "
                 + util.perl_str(item.get_effective_structure_string()), 1)
    _message("\t\t... and so forth", 1)


define_script("DescribeRelnMetoMode", [
    ("meto_mode", {"required": 1}),
    ("meto_reln", {"required": 1}),
    ("ruleapp", {"required": 1}),
], [describe_reln_meto_mode])
