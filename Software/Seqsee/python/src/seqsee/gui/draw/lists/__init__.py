"""The paged lists (lib/SGUI/List.pm): the base the Groups, Categories, Rules and Stream lists
share.

Setup's geometry: the rectangle shrunk by ``Margin`` on every side; ``EntriesPerPage =
int(EffectiveHeight / HeightPerRow)``. DrawIt takes the entries of the current page
(GetEntriesOnCurrentPage) and, for each, draws a bar across the effective width
(``#FFFFDD`` and ``#FFDDCC`` alternately, no outline), lowered below everything on the canvas,
then the subclass's DrawOneItem at the bar's top left; rows are HeightPerRow apart. Every
item of a row is tagged ``(name, entry, name-Clickable-Item)``. DrawBookKeeping then writes
"Page #… Entries …" at the bottom left and two red 10 px squares on the right edge: the
top one (tag ``name-pagedown``) goes back a page, the bottom one (``name-pageup``) forward.
Setup binds button 1 on those tags and on ``name-Clickable-Item`` (a row: ProcessClickOnItem
on the row's entry): ``click_of_tags``, ``page_after`` and ``entry_for`` (item 020; the popups
are gui/qt/listpopup.py).

The page number is the widget's state: ``draw_list`` takes it and returns, in a
``ListState``, the page Perl leaves in ``$self->{PageNumber}`` (GetEntriesOnCurrentPage
clamps a page past the last one to the last). Page up is ``page_number + 1`` and page down
``page_number - 1`` unless it is 0; both redraw.

``name`` stands for the stringified list object in the tags (Perl's ``"$self"``): the
oracle maps it to the class name, e.g. ``SGUI::List::Groups``.

A page redraw (ReDrawIt) is Clear, which deletes only the items tagged with the list, then
DrawIt; ``survivors`` gives what Clear leaves.

PERL-QUIRK: a rectangle less than 2 Margin + HeightPerRow high has no whole row:
EntriesPerPage 0 dies ("Illegal division by zero") before anything is drawn, and a negative
EntriesPerPage (under 2 Margin high) picks entries through Perl's int() and negative slice
bounds (see ``current_page``).

Item 013: the bars are lowered below every item of the whole canvas; ``draw_list`` returns
them apart (latest first, as Tk stacks them).
"""
import dataclasses

from seqsee.util import perl_num

from .. import ops

MARGIN = 20                                     # config/GUI_ws3.conf [Layout] Margin
BAR_COLOURS = ("#FFFFDD", "#FFDDCC")            # even, odd rows


@dataclasses.dataclass(frozen=True)
class ListState:
    """What GetEntriesOnCurrentPage leaves in the list object, and the page's entries."""
    page_number: int        # $self->{PageNumber}
    shown_from: object      # $self->{EntriesShownFrom}: an index, or '--'
    shown_to: object        # $self->{EntriesShownTo}
    entries_count: int      # $self->{EntriesCount}
    entries: tuple          # the entries drawn (None for an undef slot)


class Untagged(list):
    """What a DrawOneItem returns when it ``return``s before its list of item ids: the
    items it drew are on the canvas but get none of the row's tags (SGUI::List::Stream)."""


def survivors(previous, name):
    """Clear (``$Canvas->delete($self)``): the ops of an earlier drawing that are not tagged
    with the list stay on the canvas. A page redraw is ``lowered + survivors + others``."""
    return [o for o in previous if name not in o.tags]


def entries_per_page(height, height_per_row, margin=MARGIN):
    """Setup: ``int(($Height - 2 * $Margin) / $HeightPerRow)`` (int truncates towards 0)."""
    return int((height - 2 * margin) / height_per_row)


def perl_slice(lst, first, last):
    """Perl's ``@lst[$first .. $last]``: negative indices count from the end, and indices
    out of range give undef (None)."""
    n = len(lst)
    return [lst[i] if -n <= i < n else None for i in range(first, last + 1)]


def current_page(entries, per_page, page_number):
    """GetEntriesOnCurrentPage. Raises ``ops.DrawDied`` (nothing drawn) if ``per_page`` is 0."""
    entries = list(entries)
    count = len(entries)
    if per_page == 0:
        raise ops.DrawDied("Illegal division by zero", [])
    page_count = int((count + per_page - 1) / per_page)
    if page_number >= page_count:
        if page_count:
            page_number = page_count - 1
            first, last = per_page * (page_count - 1), count - 1
            return ListState(page_number, first, last, count,
                             tuple(perl_slice(entries, first, last)))
        return ListState(page_number, "--", "--", count, ())
    first = per_page * page_number
    last = min(per_page * (page_number + 1) - 1, count - 1)
    return ListState(page_number, first, last, count, tuple(perl_slice(entries, first, last)))


def bookkeeping_text(state):
    """DrawBookKeeping's text ('--' numifies to 0)."""
    if not state.entries_count:
        return "No entries to show."
    return "Page #{}. Entries {}-{} of {} Shown.".format(
        state.page_number + 1, int(perl_num(state.shown_from)) + 1,
        int(perl_num(state.shown_to)) + 1, state.entries_count)


def draw_bookkeeping(state, x, y, w, h, name, margin=MARGIN):
    """DrawBookKeeping: the page text and the page-down (top) and page-up (bottom) squares."""
    tx, ty_top, ty_bottom = x + w - margin, y + margin, y + h - margin
    return [
        ops.text(x + margin, y + h - margin, text=bookkeeping_text(state), anchor="nw",
                 tags=(name,)),
        ops.rectangle(tx, ty_top, tx + 10, ty_top + 10, tags=(name + "-pagedown", name),
                      fill="#FF0000"),
        ops.rectangle(tx, ty_bottom, tx + 10, ty_bottom + 10, tags=(name + "-pageup", name),
                      fill="#FF0000"),
    ]


def draw_list(entries, draw_one, x, y, w, h, name, height_per_row, page=0, tag_of=str,
              margin=MARGIN):
    """SGUI::List::Setup(canvas, x, y, w, h); PageNumber = page; DrawIt.

    ``entries`` is GetItemList's result; ``draw_one(left, top, entry)`` is the subclass's
    DrawOneItem (a list of ops without tags; an ``Untagged`` list keeps them without);
    ``tag_of(entry)`` is the entry's tag.
    Returns ``(lowered, others, state)``. Raises ``ops.DrawDied`` with the canvas's ops so
    far if Perl dies part way."""
    per_page = entries_per_page(h, height_per_row, margin)
    state = current_page(entries, per_page, page)
    left, top, width = x + margin, y + margin, w - 2 * margin
    lowered, out = [], []
    for counter, entry in enumerate(state.entries):
        tags = (name, tag_of(entry), name + "-Clickable-Item")
        lowered.insert(0, ops.rectangle(left, top, left + width, top + height_per_row,
                                        fill=BAR_COLOURS[counter % 2], outline="", tags=tags))
        try:
            items = draw_one(left, top, entry)
        except ops.DrawDied as e:
            raise ops.DrawDied(str(e), lowered + out + e.ops, lowered=len(lowered)) from None
        if isinstance(items, Untagged):
            out.extend(items)
        else:
            out.extend(dataclasses.replace(o, tags=tags) for o in items)
        top += height_per_row
    out.extend(draw_bookkeeping(state, x, y, w, h, name, margin))
    return lowered, out, state


# ---- interaction (item 020): Setup's '<1>' bindings -------------------------------------------
CLICKABLE = "-Clickable-Item"


@dataclasses.dataclass(frozen=True)
class Click:
    """What a click on a list item does: ``kind`` 'pageup', 'pagedown' or 'item'; ``part``
    the list's name; ``key`` the row's entry tag (items only)."""
    kind: str
    part: str
    key: object = None


def click_of_tags(tags):
    """The binding a click on an item with ``tags`` fires (Tk binds on the 'current' item's
    tags): ``name-pageup`` / ``name-pagedown``, or ``name-Clickable-Item``, whose callback
    takes the first tag that is neither 'current', the list nor its Clickable tag as the
    entry. None if no binding applies (the "Page #..." text, other parts' items)."""
    tags = tuple(tags)
    for t in tags:
        for kind in ("pageup", "pagedown"):
            if t.endswith("-" + kind):
                return Click(kind, t[:-len(kind) - 1])
    for t in tags:
        if t.endswith(CLICKABLE):
            name = t[:-len(CLICKABLE)]
            key = next((u for u in tags if u not in ("current", name, t)), None)
            return Click("item", name, key)
    return None


def page_after(kind, page):
    """The page bindings: ``PageNumber++`` / ``PageNumber-- if PageNumber``, then ReDrawIt
    (whose GetEntriesOnCurrentPage clamps a page past the last)."""
    if kind == "pageup":
        return page + 1
    if kind == "pagedown":
        return page - 1 if page else page
    raise ValueError(kind)


def entry_for(snap, part, key):
    """``$self->{ItemVivify}{$useful_tag}``: the entry of list ``part`` drawn with tag
    ``key`` (an ObjectSnap for the Groups list, a CategoryEntry for the Categories list, an
    ``(index, ThoughtSnap)`` for the Stream list), or None."""
    from . import categories, rules, stream
    from . import groups as groups_list
    if key is None:
        return None
    if part == groups_list.NAME:
        return next((g for g in snap.groups if "obj%d" % g.oid == key), None)
    if part == categories.NAME:
        return next((e for e in categories.item_list(snap) if "cat%d" % e.category.cid == key),
                    None)
    if part == stream.NAME:
        return next((e for e in stream.item_list(snap) if stream.tag_of(e) == key), None)
    if part == rules.NAME:
        return None
    raise KeyError(part)
