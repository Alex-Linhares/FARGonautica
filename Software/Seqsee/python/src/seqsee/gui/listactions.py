"""The list popups' actions: the ``ActionButtons`` of lib/SGUI/List/Groups.pm and
lib/SGUI/List/Categories.pm (SGUI::List::Rules and SGUI::List::Stream have none, so their
popups are empty).

SGUI::List::CreatePopupWidget packs one button per ``each %{ $self->{ActionButtons} }``, so
the buttons come in Perl's hash order; ``ACTIONS`` keeps the order oracle/gui_list_interaction.pl
recorded (fixed hash seed). A button runs its action on the list's SelectedItem, then
Tk::Seqsee::Update and withdraws the popup.

The actions change the model: ``run`` must be called on the thread that owns it (the runner's
worker, ``Runner.list_action``), with the live object. ``message(msg, no_break=None)`` is
UI/Graphical.pm's main::message (without no_break the model waits for 'continue').
"""
from seqsee import sltm, sworkspace, util
from seqsee.multimethods import perl_isa

GROUPS = "SGUI::List::Groups"
CATEGORIES = "SGUI::List::Categories"
RULES = "SGUI::List::Rules"
STREAM = "SGUI::List::Stream"


def popup_title(part):
    """CreatePopupWidget's ``-title => 'Actions for ' . ref($self)``."""
    return "Actions for " + part


# ---- SGUI::List::Groups ----------------------------------------------------------------------
def _delete(group, message):
    sworkspace.delete_group(group)


def _lock(group, message):
    group.set_is_locked_against_deletion(1)


def _unlock(group, message):
    group.set_is_locked_against_deletion(0)


def _show_followers(group, message):
    weighted_set = sltm.find_active_followers(group)
    if not weighted_set.is_not_empty():
        return
    followers = weighted_set.get_elements()
    message("Followers of " + util.perl_str(group.as_text()) + ":"
            + " and ".join(util.perl_str(f.as_text()) for f in followers), 1)


def _history(group, message):
    message(group.history_as_text())


def fringe_string(group):
    """Fringe's ``"[$activation] $text; "`` per get_fringe_for entry (undef if none)."""
    from seqsee.gui.snapshot import perl_string
    from seqsee.sthought.sobject import get_fringe_for
    out = None
    for component, activation in get_fringe_for.call(group):
        as_text = getattr(component, "as_text", None)
        text = util.perl_str(as_text()) if callable(as_text) else perl_string(component)
        out = (out or "") + "[{}] {}; ".format(util.perl_str(activation), text)
    return out


def _fringe(group, message):
    message(fringe_string(group))


def _action_fringe(group, message):
    from seqsee.sthought import SThought
    actions = SThought.create(group).get_actions()
    message("\n".join(util.perl_str(a.as_text()) for a in actions))


# ---- SGUI::List::Categories ------------------------------------------------------------------
def mark_descendents_to_keep(keep, group):
    """MarkDescendentsToKeep(\\%Keep, $group), on live objects (by id)."""
    if perl_isa(group, "Seqsee::Element"):
        return
    keep[id(group)] = group
    for item in group:
        mark_descendents_to_keep(keep, item)


def _delete_all_other(category, message):
    keep = {}
    for gp in sworkspace.get_groups():
        if not util.perl_true(gp.is_of_category_p(category)):
            continue
        mark_descendents_to_keep(keep, gp)
    for gp in sworkspace.get_groups():
        if id(gp) not in keep:
            sworkspace.delete_group(gp)


def _add_barlines_before(category, message):
    sworkspace.clear_bar_lines()
    for gp in sworkspace.get_groups():
        if not util.perl_true(gp.is_of_category_p(category)):
            continue
        sworkspace.add_bar_lines(gp.get_left_edge())
    sworkspace.remove_groups_crossing_bar_lines()


ACTIONS = {
    GROUPS: {
        "ActionFringe": _action_fringe,
        "ShowFollowers": _show_followers,
        "Unlock": _unlock,
        "History": _history,
        "Fringe": _fringe,
        "Lock": _lock,
        "Delete": _delete,
    },
    CATEGORIES: {
        "AddBarlinesBefore": _add_barlines_before,
        "DeleteAllOther": _delete_all_other,
    },
    RULES: {},
    STREAM: {},
}


def run(part, action, item, message):
    """The button ``action`` of list ``part``'s popup, on the live ``item``."""
    return ACTIONS[part][action](item, message)
