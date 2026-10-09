"""The Workspace view (lib/SGUI/Workspace.pm): a snapshot drawn into a rectangle as draw ops.

``draw(snap, x, y, w, h)`` is ``SGUI::Workspace->Setup($canvas, x, y, w, h)`` followed by
``SGUI::Workspace->DrawIt()``, in the same order: groups, elements, relations, bar lines,
the last-runnable label. The layout constants are config/GUI_ws3.conf's ``[Layout]`` and
``[WorkspaceLayout]``.

Each element's text op is tagged ``("obj<oid>", "element", "<index>")``: Perl tags it with
the element object itself, 'element' and its index; the oid stands in for the object. The
border oval of a highlighted group is tagged ``"hilit"``, as in Perl.

DrawMetonym crosses out a metonym's text with two lines through the text's bbox
(``$Canvas->bbox``), so drawing needs text metrics: ``measure(text, font)`` returns
``(width, linespace)`` in pixels. The Qt view can pass one built on QFontMetrics; the
default, ``approx_measure``, estimates them from the font's pixel size; the tests use the
metrics the Perl oracle recorded on its X server. ``text_bbox`` turns them into Tk's bbox.
"""
import dataclasses
import math
import re

from seqsee.util import perl_num, perl_str, perl_true

from . import ops
from .family import FAMILY_NAMES, family_to_name  # noqa: F401  (re-exported)
from .theme import Style


@dataclasses.dataclass(frozen=True)
class Layout:
    """config/GUI_ws3.conf, read by SGUI::Workspace's BEGIN block."""
    margin: float = 20
    elements_y_fraction: float = 0.5
    min_gp_height_fraction: float = 0.02
    max_gp_height_fraction: float = 0.2
    meto_y_fraction: float = 0.75
    reln_zenith_fraction: float = 0.15
    barline_height_fraction: float = 0.3


LAYOUT = Layout()


@dataclasses.dataclass(frozen=True)
class Geometry:
    """The file lexicals that SGUI::Workspace::Setup sets."""
    x_offset: float
    y_offset: float
    width: float
    height: float
    margin: float
    effective_width: float
    effective_height: float
    elements_y: float
    min_gp_height: float
    max_gp_height: float
    meto_y: float
    barline_top: float
    barline_bottom: float
    reln_zenith_fraction: float


def setup(x, y, w, h, layout=LAYOUT):
    """SGUI::Workspace::Setup."""
    m = layout.margin
    eh = h - 2 * m
    ew = w - 2 * m
    elements_y = y + m + eh * layout.elements_y_fraction
    return Geometry(
        x_offset=x, y_offset=y, width=w, height=h, margin=m,
        effective_width=ew, effective_height=eh,
        elements_y=elements_y,
        min_gp_height=eh * layout.min_gp_height_fraction,
        max_gp_height=eh * layout.max_gp_height_fraction,
        meto_y=y + m + eh * layout.meto_y_fraction,
        barline_top=elements_y - eh * 0.5 * layout.barline_height_fraction,
        barline_bottom=elements_y + eh * 0.5 * layout.barline_height_fraction,
        reln_zenith_fraction=layout.reln_zenith_fraction,
    )


def _text(x, y, text, **opts):
    """``createText`` with ``-text => $text``; undef and '' leave Tk's default empty text."""
    if text is not None and perl_str(text) != "":
        opts["text"] = perl_str(text)
    return ops.text(x, y, **opts)


_PIXEL_SIZE = re.compile(r"^-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-(\d+)-")


_NARROW_GLYPHS = " ,.;:'!|ijl[]()"   # about 0.3 em in Helvetica; other glyphs about 0.56 em


def approx_measure(text, font):
    """A rough ``(width, linespace)`` of ``text`` in ``font`` without a font engine, from the
    pixel size of an X11 font name (or of a Tk "Family -px" / "Family pt" font) and
    Helvetica-like glyph widths; the linespace is 1.1 em."""
    m = _PIXEL_SIZE.match(font or "")
    if m:
        px = int(m.group(1))
    else:
        nums = re.findall(r"-?\d+", font or ops.DEFAULT_FONT)
        size = int(nums[0]) if nums else -12
        px = -size if size < 0 else round(size * 4 / 3)     # Tk: positive sizes are points
    ems = sum(0.3 if ch in _NARROW_GLYPHS else 0.56 for ch in text)
    return round(ems * px), round(1.1 * px)


def _tk_round(v):
    """Tk's ROUND, floor(v + 0.5). (On the oracle's X server Tk rounds a few exact .5
    positions down instead, e.g. 99.5 to 99; no golden case hits one.)"""
    return int(math.floor(v + 0.5))


def text_bbox(x, y, text, font, measure=approx_measure):
    """``$canvas->bbox`` of a centre-anchored single-line text item at (x, y): Tk's
    ComputeTextBbox (tkCanvText.c), with its 1 px insert-cursor fudge left and right."""
    width, height = measure(text, font)
    left = _tk_round(x) - width // 2
    top = _tk_round(y) - height // 2
    return (left - 1, top, left + width + 1, top + height)


class _Drawer:
    """One DrawIt call: the geometry plus DrawIt's per-call bookkeeping."""

    def __init__(self, snap, geometry, measure=approx_measure):
        self.snap = snap
        self.g = geometry
        self.measure = measure
        self.out = []
        count = snap.element_count
        self.group_ht_per_unit_span = (geometry.max_gp_height - geometry.min_gp_height) / (
            count or 1)
        self.space_per_element = geometry.effective_width / (count + 1)
        self.anchors_for_relations = {}   # oid -> (x, y)
        self.relations_to_hide = set()    # (oid, oid) pairs
        self.raised_at = None             # len(out) after DrawGroups' raise('hilit')

    def draw_it(self):
        self.draw_groups()
        self.draw_elements()
        self.draw_relations()
        self.draw_bar_lines()
        self.draw_last_runnable()
        return self.out

    def draw_groups(self):
        """DrawGroups: the largest group (the first of GetGroups) with $is_largest, the
        others, then each element that is squinted (group_p) or has an active metonym,
        drawn as a group. Then $Canvas->raise('hilit').

        PERL-QUIRK: with no groups, ``my @groups = ... or return`` returns first, so
        squinted elements and element metonyms are not drawn at all."""
        groups = self.snap.groups
        if not groups:
            return
        start = len(self.out)
        self.draw_anchored(groups[0], 1)
        for gp in groups[1:]:
            self.draw_anchored(gp)
        for elt in self.snap.elements:
            if elt.group_p or elt.metonym_active:
                self.draw_anchored(elt)
        # raise('hilit') moves the tagged items above everything drawn so far, keeping their
        # order. (On a shared canvas, Tk raises every 'hilit' item, not only this view's.)
        drawn = self.out[start:]
        self.out[start:] = ([o for o in drawn if "hilit" not in o.tags]
                            + [o for o in drawn if "hilit" in o.tags])
        self.raised_at = len(self.out)

    def draw_anchored(self, obj, is_largest=0):
        """Seqsee::Anchored::draw_ws3, with find_group_style and find_group_border_style.
        Neighbouring items of the group hide the relations between them.

        Groups do add $XOffset, unlike elements and bar lines (see draw_elements)."""
        g = self.g
        items = obj.items
        for a, b in zip(items, items[1:]):
            self.relations_to_hide.add((a, b))
            self.relations_to_hide.add((b, a))
        left_x = g.x_offset + g.margin + (perl_num(obj.left) + 0.1) * self.space_per_element
        right_x = g.x_offset + g.margin + (perl_num(obj.right) + 0.9) * self.space_per_element
        span = perl_num(obj.span)
        top = g.elements_y - g.min_gp_height - span * self.group_ht_per_unit_span
        bottom = g.elements_y + g.min_gp_height + span * self.group_ht_per_unit_span
        is_meto = 1 if obj.metonym_active else 0
        if is_meto:
            self.draw_metonym(obj.structure_string, obj.starred_structure_string,
                              left_x, right_x)
        self.anchors_for_relations[obj.oid] = ((left_x + right_x) / 2, top)
        self.out.append(ops.oval(left_x, top, right_x, bottom,
                                 **self.find_group_style(obj, is_meto, is_largest)))
        self.out.append(ops.oval(left_x, top, right_x, bottom,
                                 tags=("hilit",) if obj.hilit else (),
                                 **self.find_group_border_style(obj.hilit)))

    # The find_*_style methods are SGUI::Workspace's; SGUI::Workspace_Attention overrides them.
    def find_element_style(self, elt):
        return Style.Element(elt.hilit)

    def find_group_style(self, obj, is_meto, is_largest):
        return Style.Group2(is_meto, obj.category_kind, is_largest)

    def find_group_border_style(self, is_hilit):
        return Style.GroupBorder(is_hilit)

    def find_relation_style(self, rel, is_hilit):
        return dict(Style.Relation(rel.strength, is_hilit), arrowshape=(8, 12, 10))

    def draw_metonym(self, actual, starred, x1, x2):
        """DrawMetonym: the actual structure, crossed out through its bbox, below the
        starred one."""
        g = self.g
        x = (x1 + x2) / 2
        style = Style.Element(0)
        self.out.append(_text(x, g.meto_y + 20, actual, **style))
        bx1, by1, bx2, by2 = text_bbox(x, g.meto_y + 20, perl_str(actual or ""),
                                       style["font"], self.measure)
        self.out.append(ops.line(bx1, by1, bx2, by2))
        self.out.append(ops.line(bx2, by1, bx1, by2))
        self.out.append(_text(x, g.meto_y, starred, **Style.Starred()))

    def draw_relations(self):
        """DrawRelations: SRelation::draw_ws3 for each relation (in Perl, hash order)."""
        for rel in self.snap.relations:
            self.draw_relation(rel)

    def draw_relation(self, rel):
        """SRelation::draw_ws3, with find_relation_style. A relation between neighbouring
        items of a group is hidden unless highlighted. One whose end has no anchor (not
        drawn, or outside the workspace), or an anchor at x = 0, is skipped."""
        first, second = rel.ends
        if (first, second) in self.relations_to_hide and not perl_true(rel.hilit):
            return
        x1, y1 = self.anchors_for_relations.get(first, (None, None))
        x2, y2 = self.anchors_for_relations.get(second, (None, None))
        if not (perl_true(x1) and perl_true(x2)):
            return
        g = self.g
        zenith = g.y_offset + g.margin + g.reln_zenith_fraction * g.effective_height
        self.out.append(ops.line(x1, y1, (x1 + x2) / 2, zenith, x2, y2,
                                 **self.find_relation_style(rel, rel.hilit)))

    def draw_elements(self):
        """DrawElements. PERL-QUIRK: x is $Margin + (0.5 + i) * $SpacePerElement, without
        $XOffset (groups do add it)."""
        g = self.g
        for counter, elt in enumerate(self.snap.elements):
            self.draw_element(elt, counter,
                              g.margin + (0.5 + counter) * self.space_per_element, g.elements_y)

    def draw_element(self, elt, idx, x, y):
        """Seqsee::Element::draw_ws3 (with find_element_style)."""
        self.out.append(_text(x, y, elt.mag, tags=("obj%d" % elt.oid, "element", str(idx)),
                              **self.find_element_style(elt)))
        if perl_true(self.snap.debug):
            self.out.append(_text(x + 5, y + 10, idx))
        self.anchors_for_relations.setdefault(elt.oid, (x, y - 10))

    def draw_bar_lines(self):
        """DrawBarLines. PERL-QUIRK: x is $Margin + $index * $SpacePerElement, without
        $XOffset."""
        g = self.g
        for index in self.snap.bar_lines:
            xpos = g.margin + index * self.space_per_element
            self.out.append(ops.line(xpos, g.barline_top, xpos, g.barline_bottom,
                                     **self.BAR_LINE_OPTIONS))

    BAR_LINE_OPTIONS = {"width": 3}

    def draw_last_runnable(self):
        """DrawLastRunnable. PERL-QUIRK: placed at ($Margin, $Height - $Margin), without
        either offset. The text is family_to_name($Global::CurrentRunnableString), which is
        empty for every codelet family (none defines $NAME)."""
        g = self.g
        self.out.append(_text(g.margin, g.height - g.margin, self.last_runnable_text(),
                              anchor="sw"))

    def last_runnable_text(self):
        return family_to_name(self.snap.current_runnable)


def draw(snap, x, y, w, h, layout=LAYOUT, measure=approx_measure):
    """SGUI::Workspace->Setup(canvas, x, y, w, h); ->DrawIt(): the list of draw ops.
    ``measure(text, font) -> (width, linespace)`` sizes metonym texts (see the module doc)."""
    return _Drawer(snap, setup(x, y, w, h, layout), measure).draw_it()


def draw_raised(snap, x, y, w, h, layout=LAYOUT, measure=approx_measure, drawer=None):
    """``draw``, and the number of ops drawn when DrawGroups ran ``raise('hilit')`` (None if
    it returned before). On Tk::Seqsee's shared canvas that raise also lifts the 'hilit'
    items of the parts drawn before (item 013, ``views.compose``). A ``DrawDied`` carries
    the same number as ``raised_at``."""
    d = drawer or _Drawer(snap, setup(x, y, w, h, layout), measure)
    try:
        out = d.draw_it()
    except ops.DrawDied as e:
        e.raised_at = d.raised_at
        raise
    return out, d.raised_at
