"""The composite views (lib/Tk/Seqsee.pm): several drawings on one canvas.

``VIEW_OPTIONS`` is Tk::Seqsee's ``@ViewOptions``: 11 views, each a title and its parts,
``(part, left, top, width, height)`` in percent of the canvas. SetupParts gives part ``p``
the rectangle ``(l * 0.01 * W, t * 0.01 * H, w * 0.01 * W, h * 0.01 * H)``. Parts are named
after their Perl modules (``SGUI::Workspace``, ``SGUI::List::Groups``, …).

``compose(view, snap, w, h)`` is Update: ``$Canvas->delete('all')``, each part's DrawIt in
order, then DrawAttentionDirectingArrows if AttentionNeeded was called (Tk::SCommentary does
it while a question waits for an answer). It returns a ``Composition``: the canvas's ops in
display-list order, the die message (if any) and the paging state of the lists drawn.

All the parts share one canvas, so the stacking operations act across parts:
- ``$Canvas->lower($id)`` (the Coderack's and Relations' stripes, the lists' bars) puts an
  item below everything on the canvas, including the parts drawn before.
- ``$Canvas->raise('hilit')`` (end of DrawGroups in SGUI::Workspace and
  SGUI::Workspace_Attention) lifts every 'hilit' item on the canvas: in 'Workspace +
  Attention', the Workspace's highlighted group borders end up above the attention part's
  black rectangle and groups.

PERL-QUIRK (confirmed by the oracle): Update doesn't catch a die in a part's DrawIt
(Workspace_Attention with any relation, the Rules list always, the Slipnet with an active
Mapping::Dir node). The die leaves Update: the items drawn so far stay, the later parts and
the attention arrows are not drawn. So 'Workspace + Rules' only ever shows the Workspace, and
in view 0 a dying Slipnet (drawn first) leaves the canvas almost empty.

The list viewers are module-level objects in Perl, shared by the views (the Groups list is in
views 0 and 8), but SGUI::List::Setup sets PageNumber back to 0, and SetupParts calls it on
every view change: a page lasts only while the view stays (confirmed by the oracle's
'before_setup' cases). ``pages`` maps a list's part name to its page in the current view;
the widget must reset it to {} when the view changes. ``Composition.lists`` gives the state
Perl leaves in each list whose DrawIt ran (``page_number`` is the next ``pages`` value; the
page squares add or subtract 1 and redraw: ``lists.page_after``).
"""
import dataclasses

from seqsee.util import perl_num

from . import coderack, ops, relations, slipnet, stream, workspace, workspace_attention
from .lists import ListState
from .lists import categories as list_categories
from .lists import groups as list_groups
from .lists import rules as list_rules
from .lists import stream as list_stream
from .theme import Style
from .workspace import approx_measure

WORKSPACE = "SGUI::Workspace"
ATTENTION = "SGUI::Workspace_Attention"
SLIPNET = "SGUI::Slipnet"
CODERACK = "SGUI::Coderack"
STREAM = "SGUI::Stream"
RELATIONS = "SGUI::Relations"
GROUPS_LIST = list_groups.NAME
CATEGORIES_LIST = list_categories.NAME
RULES_LIST = list_rules.NAME
STREAM_LIST = list_stream.NAME
LISTS = (GROUPS_LIST, CATEGORIES_LIST, RULES_LIST, STREAM_LIST)


@dataclasses.dataclass(frozen=True)
class View:
    title: str
    parts: tuple            # ((part, left %, top %, width %, height %), ...)


def _ws_and(part, title):
    return View(title, ((WORKSPACE, 0, 0, 100, 50), (part, 0, 50, 100, 50)))


VIEW_OPTIONS = (
    View("Workspace + Groups + Relations + Slipnet",
         ((SLIPNET, 65, 0, 35, 50), (WORKSPACE, 0, 0, 65, 50), (GROUPS_LIST, 0, 50, 35, 50),
          (RELATIONS, 35, 50, 65, 50))),
    View("Workspace", ((WORKSPACE, 0, 0, 100, 100),)),
    _ws_and(ATTENTION, "Workspace + Attention"),
    _ws_and(SLIPNET, "Workspace + Slipnet"),
    _ws_and(CATEGORIES_LIST, "Workspace + Categories"),
    _ws_and(CODERACK, "Workspace + Coderack"),
    _ws_and(RULES_LIST, "Workspace + Rules"),
    _ws_and(RELATIONS, "Workspace + Relations"),
    _ws_and(GROUPS_LIST, "Workspace + Groups"),
    _ws_and(STREAM, "Workspace + Stream"),
    _ws_and(STREAM_LIST, "Workspace + Stream2"),
)


def pane_view(part):
    """A pane (a dock in the Qt shell, not in Perl): ``part`` alone over the whole canvas,
    as SetupParts would place it in a view ``[[part, 0, 0, 100, 100]]``."""
    return View(part, ((part, 0, 0, 100, 100),))


def _view(view):
    """A ``View``, from itself, an index in ``VIEW_OPTIONS`` or a title."""
    return view if isinstance(view, View) else VIEW_OPTIONS[view_index(view)]


def view_index(view):
    """A view given by its index in ``VIEW_OPTIONS`` or by its title."""
    if isinstance(view, int):
        VIEW_OPTIONS[view]
        return view
    for i, v in enumerate(VIEW_OPTIONS):
        if v.title == view:
            return i
    raise KeyError(view)


def initial_view(options):
    """The view Populate sets up: ``$ViewOptions[ $Global::Options_ref->{view} || 0 ]``."""
    return int(perl_num(options.get("view") or 0))


def part_rects(view, w, h):
    """SetupParts: ``[(part, x, y, width, height), ...]`` for a ``w`` × ``h`` canvas."""
    return [(part, l * 0.01 * w, t * 0.01 * h, pw * 0.01 * w, ph * 0.01 * h)
            for part, l, t, pw, ph in _view(view).parts]


def attention_arrows(w, h):
    """DrawAttentionDirectingArrows: a thick red arrow down the middle of the bottom of the
    canvas and "PLEASE SEE BELOW" above it (Style::Element(0)'s font, then a red fill).

    PERL-QUIRK (confirmed by the oracle): the options are ``-anchor => 'n',
    Style::Element(0), -fill => ...``, and Style::Element has its own ``-anchor``
    ('center'), which comes later and wins: the text is centred on 0.88 H, not hung from it."""
    arrow_top, arrow_bottom = 0.92 * h, 0.99 * h
    x_top = x_bottom = w * 5 / 10
    text_opts = {"anchor": "n"}
    text_opts.update(Style.Element(0))
    text_opts["fill"] = "#FF0000"
    return [
        ops.line(x_top, arrow_top, x_bottom, arrow_bottom, arrow="last", width=15,
                 fill="#FF0000"),
        ops.text(w * 0.5, h * 0.88, text="PLEASE SEE BELOW", **text_opts),
    ]


@dataclasses.dataclass(frozen=True)
class Composition:
    """What Update leaves: the canvas's ``ops`` in display-list order, ``died`` (the uncaught
    die's message, or None) and the part it came from, and ``lists``: part name →
    ``ListState`` for each list whose DrawIt ran (a list that died in
    GetEntriesOnCurrentPage keeps its page and has no counts; one that died later, on a
    row, is given the same, although Perl had already clamped its page: no recipe does it)."""
    ops: tuple
    died: object = None
    died_part: object = None
    lists: dict = dataclasses.field(default_factory=dict)


class _Canvas:
    """The display list of the shared canvas: draw (append), lower, raise by tag."""

    def __init__(self):
        self.items = []

    def draw(self, items):
        self.items.extend(items)

    def lower(self, latest_first):
        """Items lowered one by one (``latest_first`` as the draw modules return them): the
        last one lowered ends at the very bottom."""
        self.items[:0] = latest_first

    def raise_tag(self, tag):
        self.items = ([o for o in self.items if tag not in o.tags]
                      + [o for o in self.items if tag in o.tags])

    def draw_raised(self, items, raised_at):
        """A workspace part: its ops, with DrawGroups' raise('hilit') applied to the whole
        canvas after the first ``raised_at`` of them."""
        if raised_at is None:
            self.draw(items)
            return
        self.draw(items[:raised_at])
        self.raise_tag("hilit")
        self.draw(items[raised_at:])


def _draw_part(canvas, part, snap, rect, page, measure, known_families, rules):
    """One part's DrawIt on the shared canvas. Returns the list's ListState (or None)."""
    if part in (WORKSPACE, ATTENTION):
        module = workspace if part == WORKSPACE else workspace_attention
        out, raised_at = module.draw_raised(snap, *rect, measure=measure)
        canvas.draw_raised(out, raised_at)
        return None
    if part == SLIPNET:
        canvas.draw(slipnet.draw(snap, *rect))
        return None
    if part == STREAM:
        canvas.draw(stream.draw(snap, *rect))
        return None
    if part == CODERACK:
        lowered, out = coderack.draw_layers(snap, *rect, known_families=known_families)
    elif part == RELATIONS:
        lowered, out = relations.draw_layers(snap, *rect)
    elif part == GROUPS_LIST:
        lowered, out, state = list_groups.draw_layers(snap, *rect, page=page)
    elif part == CATEGORIES_LIST:
        lowered, out, state = list_categories.draw_layers(snap, *rect, page=page)
    elif part == RULES_LIST:
        lowered, out, state = list_rules.draw_layers(snap, *rect, page=page, rules=rules)
    elif part == STREAM_LIST:
        lowered, out, state = list_stream.draw_layers(snap, *rect, page=page)
    else:
        raise KeyError(part)
    canvas.lower(lowered)
    canvas.draw(out)
    return state if part in LISTS else None


def compose(view, snap, w, h, attention_needed=False, pages=None, measure=approx_measure,
            known_families=(), rules=None):
    """Tk::Seqsee's Update for ``view`` (an index, a title or a ``View``) on a ``w`` × ``h`` canvas.

    ``pages``: list part name → page number (default 0). ``measure`` sizes the workspace's
    metonym texts (see ``workspace.draw``); ``known_families`` is the Coderack's (see
    ``coderack.draw``); ``rules`` the Rules list's (None: the real SRule, which dies)."""
    pages = pages or {}
    canvas = _Canvas()
    states = {}
    for part, *rect in part_rects(view, w, h):
        page = pages.get(part, 0)
        try:
            state = _draw_part(canvas, part, snap, rect, page, measure, known_families, rules)
        except ops.DrawDied as e:
            if part in (WORKSPACE, ATTENTION):
                canvas.draw_raised(e.ops, e.raised_at)
            else:
                canvas.lower(e.ops[:e.lowered])
                canvas.draw(e.ops[e.lowered:])
            if part in LISTS:
                states[part] = ListState(page, None, None, None, ())
            return Composition(tuple(canvas.items), str(e), part, states)
        if state is not None:
            states[part] = state
    if attention_needed:
        canvas.draw(attention_arrows(w, h))
    return Composition(tuple(canvas.items), None, None, states)
