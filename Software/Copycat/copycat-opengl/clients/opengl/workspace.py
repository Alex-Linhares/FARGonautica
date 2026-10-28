# Copyright (c) 2007-2017 Joseph Hager.
#
# Copycat is free software; you can redistribute it and/or modify
# it under the terms of version 2 of the GNU General Public License,
# as published by the Free Software Foundation.
#
# Copycat is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Copycat; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

# IDEAS:
# Add drawing routines for workspace structures
#
# GUI-2026: the drawing routines are below (class Structures): bonds as arcs over the
# letters, groups as boxes, correspondences as curves between the strings, the rule and the
# translated rule as text, and the answer. Hager's Letter/String/Workspace classes are kept;
# the additions are marked GUI-2026.

import pyglet
from pyglet import shapes  # GUI-2026

# GUI-2026: chalk colours by structure kind (RGB); alpha says built or only proposed.
COLORS = {
    'successor': (170, 235, 170),
    'predecessor': (245, 195, 125),
    'sameness': (150, 205, 255),
    'succesor group': (170, 235, 170),      # the Slipnet's own spelling
    'predecessor group': (245, 195, 125),
    'sameness group': (150, 205, 255),
    'correspondence': (235, 205, 255),
    'other': (220, 220, 220),
}
ALPHA_BUILT = 210
ALPHA_PROPOSED = 75
LETTER_STEP = 40          # Hager's spacing between letters
LETTER_HALF = 13          # about half a 36-point EraserDust glyph
LETTER_TOP = 40           # the glyph's top, over the baseline
LETTER_BOTTOM = -10       # a little under the baseline

class Letter(object):
    def __init__(self, letter, x, y, batch):
        self.letter = letter
        self.label = pyglet.text.Label(letter.name, font_name="EraserDust", font_size=36, x=x, y=y,
                                       color=(255, 255, 255, 100), batch=batch)

    def update(self, _):
        if self.letter.is_changed:
            self.label.color = (255, 100, 100, 130)

class String(object):
    def __init__(self, string, x, y, batch):
        self.letters = []
        self.string = string              # GUI-2026
        self.y = y                        # GUI-2026: the baseline
        self.centers = {}                 # GUI-2026: letter position -> centre x
        t = len(string.get_letters()) * 19
        new_x = x - t
        for letter in string.get_letters():
            self.letters.append(Letter(letter, new_x, y, batch))
            self.centers[letter.left_string_position] = new_x + LETTER_HALF  # GUI-2026
            new_x += LETTER_STEP

    def update(self, dt):
        for letter in self.letters:
            letter.update(dt)

    # GUI-2026: geometry of an object (letter or group) of this string.
    def span(self, obj):
        """The x extent of an object: the centres of its leftmost and rightmost letters."""
        return (self.centers[obj.left_string_position],
                self.centers[obj.right_string_position])

    def center(self, obj):
        left, right = self.span(obj)
        return (left + right) / 2.0

class Structures(object):
    """GUI-2026: draws the workspace's bonds, groups, correspondences and rules.

    Every update it computes a cheap signature of the structures (which ones exist and
    whether they are built or only proposed); the shapes are rebuilt only when it changes,
    so the per-step cost is a few list comprehensions."""

    def __init__(self, workspace, initial, target, batch, rule_label, translated_label):
        self.workspace = workspace
        self.initial = initial
        self.target = target
        self.batch = batch
        self.rule_label = rule_label
        self.translated_label = translated_label
        self.group = pyglet.graphics.Group(order=-1)   # under the letters
        self.items = []
        self.signature = None
        self.corr_count = 0

    # -- helpers -----------------------------------------------------------------------

    def color(self, name, built):
        rgb = COLORS.get(name, COLORS['other'])
        return rgb + (ALPHA_BUILT if built else ALPHA_PROPOSED,)

    def depth(self, obj):
        """Nesting depth: a letter is 0, a group is 1 + its deepest member."""
        if obj.type_name != 'group':
            return 0
        return 1 + max([self.depth(o) for o in obj.objects] + [0])

    def curve(self, points, color, thickness):
        c = shapes.BezierCurve(*points, segments=24, thickness=thickness, color=color,
                               batch=self.batch, group=self.group)
        self.items.append(c)
        return c

    def line(self, x1, y1, x2, y2, color, thickness):
        l = shapes.Line(x1, y1, x2, y2, thickness=thickness, color=color,
                        batch=self.batch, group=self.group)
        self.items.append(l)
        return l

    def text(self, s, x, y, color, size=10, anchor_x='center'):
        multiline = "\n" in s
        label = pyglet.text.Label(s, font_name="EraserDust", font_size=size, x=x, y=y,
                                  anchor_x=anchor_x, anchor_y='center', color=color,
                                  multiline=multiline, width=120 if multiline else None,
                                  batch=self.batch)
        self.items.append(label)
        return label

    # -- the drawings ------------------------------------------------------------------

    def draw_bond(self, view, bond, built):
        """A bond is an arc over its two objects, labelled by category, with an arrowhead
        at the object it points to (none for sameness)."""
        name = bond.bond_category.name
        color = self.color(name, built)
        x1 = view.center(bond.left_object)
        x2 = view.center(bond.right_object)
        if x1 > x2:
            x1, x2 = x2, x1
        base = view.y + LETTER_TOP + 8 * max(self.depth(bond.left_object),
                                              self.depth(bond.right_object))
        rise = 14 + (x2 - x1) * 0.18
        thickness = 2.5 if built else 1.2
        self.curve([(x1, base), ((x1 + x2) / 2.0, base + 2 * rise), (x2, base)],
                   color, thickness)
        self.text(bond.bond_category.short_name, (x1 + x2) / 2.0, base + rise + 6,
                  color, size=9)
        direction = bond.direction_category
        if direction is not None:
            # the arc's end slope: at the ends a quadratic Bezier heads towards the apex
            if direction.name == 'right':
                tip, dx = x2, -1
            else:
                tip, dx = x1, 1
            self.line(tip, base, tip + 7 * dx, base + 8, color, thickness)
            self.line(tip, base, tip + 1 * dx, base + 10, color, thickness)

    def draw_group(self, view, group, built):
        """A group is a box around its letters, grown with its nesting depth, with the
        group category written at its top left."""
        name = group.group_category.name
        color = self.color(name, built)
        left, right = view.span(group)
        pad = 6 + 7 * (self.depth(group) - 1)
        x = left - LETTER_HALF - pad
        w = right - left + 2 * LETTER_HALF + 2 * pad
        y = view.y + LETTER_BOTTOM - pad
        h = LETTER_TOP - LETTER_BOTTOM + 2 * pad
        box = shapes.Box(x, y, w, h, thickness=2 if built else 1, color=color,
                         batch=self.batch, group=self.group)
        self.items.append(box)
        self.text(group.group_category.short_name, x + 2, y + h + 6, color, size=9,
                  anchor_x='left')
        if group.direction_category is not None:
            arrow = '>' if group.direction_category.name == 'right' else '<'
            self.text(arrow, x + w - 4, y + h + 6, color, size=11)

    def draw_correspondence(self, corr, built):
        """A correspondence is a curve from an object of the initial string (top) to one
        of the target string (bottom), with its distinguishing concept mappings beside
        it."""
        color = self.color('correspondence', built)
        x1 = self.initial.center(corr.object1)
        y1 = self.initial.y + LETTER_BOTTOM - 7 * self.depth(corr.object1) - 4
        x2 = self.target.center(corr.object2)
        y2 = self.target.y + LETTER_TOP + 7 * self.depth(corr.object2) + 4
        mid_y = (y1 + y2) / 2.0
        self.curve([(x1, y1), (x1, mid_y), (x2, mid_y), (x2, y2)], color,
                   2.5 if built else 1.2)
        mappings = [cm for cm in corr.concept_mappings if cm.is_distinguishing()]
        if not mappings:
            mappings = corr.concept_mappings
        words = ["%s->%s" % (cm.descriptor1.short_name, cm.descriptor2.short_name)
                 for cm in mappings[:2]]
        # the mappings sit by the curve's upper end, clear of the target string's bonds;
        # successive correspondences alternate two heights so the words don't overlap
        stagger = 12 * (self.corr_count % 2)
        self.corr_count += 1
        self.text("\n".join(words), x1 + 5, y1 - 10 - stagger, color, size=8,
                  anchor_x='left')

    # -- update ------------------------------------------------------------------------

    def current(self):
        """The structures to draw, as (kind, object, built) triples."""
        ws = self.workspace
        out = []
        for view in (self.initial, self.target):
            s = view.string
            out.extend([('bond', view, b, True) for b in s.get_bonds()])
            out.extend([('bond', view, b, False) for b in s.get_proposed_bonds()
                        if b.string == s])
            out.extend([('group', view, g, True) for g in s.get_groups()])
            out.extend([('group', view, g, False) for g in s.get_proposed_groups()])
        out.extend([('corr', None, c, True) for c in ws.correspondences()])
        out.extend([('corr', None, c, False) for c in ws.get_proposed_correspondences()])
        return out

    def update(self, dt):
        ws = self.workspace
        current = self.current()
        rule = ws.rule.to_string() if ws.rule else ""
        translated = ws.translated_rule.to_string() if ws.translated_rule else ""
        signature = (tuple((kind, id(obj), built) for kind, _, obj, built in current),
                     rule, translated)
        if signature == self.signature:
            return
        self.signature = signature

        for item in self.items:
            item.delete()
        self.items = []
        self.corr_count = 0
        # correspondences left to right, so their labels alternate heights spatially
        current.sort(key=lambda t: (t[0] != 'corr', t[2].object1.left_string_position)
                     if t[0] == 'corr' else (True, 0))
        for kind, view, obj, built in current:
            try:
                if kind == 'bond':
                    self.draw_bond(view, obj, built)
                elif kind == 'group':
                    self.draw_group(view, obj, built)
                else:
                    self.draw_correspondence(obj, built)
            except (KeyError, AttributeError):
                # an object of a string we don't draw (a flipped group, say): skip it
                continue

        if self.rule_label.text != rule:
            self.rule_label.text = rule
        if self.translated_label.text != translated:
            self.translated_label.text = translated

class Workspace(object):
    def __init__(self, workspace, x, y, w, h, batch, rule_label=None):
        self.workspace = workspace
        self.batch = batch

        arrow = pyglet.resource.image("arrow.png")
        arrow.anchor_x = arrow.width / 2.0
        arrow.anchor_y = arrow.height / 2.0
        self.arrow1 = pyglet.sprite.Sprite(arrow, 512, 505, batch=self.batch)
        self.arrow1.opacity = 100
        self.arrow2 = pyglet.sprite.Sprite(arrow, 512, 390, batch=self.batch)
        self.arrow2.opacity = 100

        left_x = w / 4.0
        right_x = left_x * 3.0
        bot_y = y + h / 4.0
        top_y = y + 3 * h / 4.0 - 35

        self.answer_x = right_x
        self.answer_y = bot_y

        self.strings = [String(workspace.initial_string, left_x, top_y, self.batch),
                        String(workspace.modified_string, right_x, top_y, self.batch),
                        String(workspace.target_string, left_x, bot_y, self.batch)]

        # GUI-2026: the structures layer and the translated rule (the rule itself is the
        # window's label, between the two rows; the translated rule goes under the bottom row).
        if rule_label is None:
            rule_label = pyglet.text.Label("", font_name="EraserDust", font_size=16, x=512, y=445,
                                           color=(255, 255, 255, 190), batch=self.batch,
                                           anchor_x="center")
        self.rule_label = rule_label
        self.translated_label = pyglet.text.Label("", font_name="EraserDust", font_size=14,
                                                  x=right_x, y=y + 16,
                                                  color=(255, 255, 255, 170), batch=self.batch,
                                                  anchor_x="center", anchor_y="center",
                                                  multiline=True, width=470, align="center")
        self.structures = Structures(workspace, self.strings[0], self.strings[2], self.batch,
                                     self.rule_label, self.translated_label)
        self.answer_drawn = False

    def update(self, dt):
        if self.workspace.answer_string and not self.answer_drawn:   # GUI-2026: once
            self.answer_drawn = True
            self.strings.append(String(self.workspace.answer_string,
                                       self.answer_x, self.answer_y, self.batch))

        for string in self.strings:
            string.update(dt)
        self.structures.update(dt)   # GUI-2026
