"""The Workspace attention view (lib/SGUI/Workspace_Attention.pm): the workspace drawn on a
black rectangle, each element, group and relation coloured by its attention (the chance that
a codelet looks at it next, ``SCoderack->AttentionDistribution``, read from the snapshot's
``attention`` fields).

The Perl module is a copy of SGUI::Workspace ("Before I ever make a third similar class, I
must refactor."), so this module reuses ``workspace._Drawer`` and overrides what differs:
the layout (``[Workspace_AttentionLayout]``: shorter bar lines), the black rectangle drawn
first, the ``find_*_style`` methods (``Style::ElementAttention``, ``GroupAttention``,
``GroupBorderAttention``, ``RelationAttention``), thin bar lines, and the raw
``$Global::CurrentRunnableString`` as the label. DrawLegend is empty.

PERL-QUIRK: DrawRelations calls ``$rel->draw_attention``, but the method is defined in
package SReln, and relations are SRelation objects. So DrawIt dies ("Can't locate object
method ...") at the first relation, after the groups and elements are drawn and before the
bar lines and label. ``draw`` raises ``ops.DrawDied`` with the ops drawn so far, which is
what the Tk canvas shows. ``relations_die=False`` runs SReln::draw_attention's body instead.
"""
from . import ops, workspace
from .theme import Style
from .workspace import Layout, _Drawer, approx_measure, setup

LAYOUT = Layout(barline_height_fraction=0.2)

DIED = 'Can\'t locate object method "draw_attention" via package "SRelation"'


class _AttentionDrawer(_Drawer):
    BAR_LINE_OPTIONS = {}

    def __init__(self, snap, geometry, measure=approx_measure, relations_die=True):
        super().__init__(snap, geometry, measure)
        self.relations_die = relations_die

    def draw_it(self):
        self.draw_black_rectangle()      # PrepareForDrawing
        return super().draw_it()         # DrawLegend(10, 10) draws nothing

    def draw_black_rectangle(self):
        """DrawBlackRectangle: the effective area, with both offsets."""
        g = self.g
        self.out.append(ops.rectangle(
            g.x_offset + g.margin, g.y_offset + g.margin,
            g.x_offset + g.margin + g.effective_width,
            g.y_offset + g.margin + g.effective_height, fill="#000000"))

    def find_element_style(self, elt):
        return Style.ElementAttention(elt.attention)

    def find_group_style(self, obj, is_meto, is_largest):
        return Style.GroupAttention(obj.attention)

    def find_group_border_style(self, is_hilit):
        return Style.GroupBorderAttention()

    def find_relation_style(self, rel, is_hilit):
        return Style.RelationAttention(rel.attention)

    def draw_relation(self, rel):
        """SReln::draw_attention: never reached in Perl (see the module doc). Its body is
        SRelation::draw_ws3's, with Style::RelationAttention."""
        if self.relations_die:
            raise ops.DrawDied(DIED, self.out)
        super().draw_relation(rel)

    def last_runnable_text(self):
        """DrawLastRunnable shows $Global::CurrentRunnableString itself (no family_to_name)."""
        return self.snap.current_runnable


def draw(snap, x, y, w, h, layout=LAYOUT, measure=approx_measure, relations_die=True):
    """SGUI::Workspace_Attention->Setup(canvas, x, y, w, h); ->DrawIt(): the list of draw
    ops. Raises ``ops.DrawDied`` (with the ops drawn so far) if the snapshot has relations,
    unless ``relations_die`` is False. ``measure`` is as in ``workspace.draw``."""
    return _AttentionDrawer(snap, setup(x, y, w, h, layout), measure, relations_die).draw_it()


def draw_raised(snap, x, y, w, h, layout=LAYOUT, measure=approx_measure, relations_die=True):
    """``draw``, and where DrawGroups' ``raise('hilit')`` ran (see ``workspace.draw_raised``)."""
    return workspace.draw_raised(snap, x, y, w, h, drawer=_AttentionDrawer(
        snap, setup(x, y, w, h, layout), measure, relations_die))
