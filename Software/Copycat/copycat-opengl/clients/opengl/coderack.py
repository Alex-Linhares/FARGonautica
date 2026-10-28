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
# Intensity of the codelet count should be how probable that type is to be chosen

from collections import defaultdict as dd
import pyglet

class Coderack(object):
    def __init__(self, coderack, x, y, w, h, batch):
        self.coderack = coderack
        self.x = x
        self.y = y
        self.w = w
        self.h = h

        self.batch = batch

        names = ['BondBottomUpScout', 'BondBuilder', 'BondStrengthTester',
                 'BondTopDownCategoryScout', 'BondTopDownDirectionScout',
                 'Breaker', 'CorrespondenceBottomUpScout',
                 'CorrespondenceBuilder', 'CorrespondenceImportantObjectScout',
                 'CorrespondenceStrengthTester', 'DescriptionBottomUpScout',
                 'DescriptionBuilder', 'DescriptionStrengthTester',
                 'DescriptionTopDownScout', 'GroupBuilder',
                 'GroupStrengthTester', 'GroupTopDownCategoryScout',
                 'GroupTopDownDirectionScout', 'GroupWholeStringScout',
                 'ReplacementFinder', 'RuleBuilder', 'RuleScout',
                 'RuleStrengthTester', 'RuleTranslator']

        vnames = ['Bond Scout', 'Bond Builder', 'Bond Strength Tester',
                  'Bond Category Scout', 'Bond Direction Scout',
                  'Breaker', 'Correspondence Scout',
                  'Correspondence Builder', 'Correspondence Importance Scout',
                  'Correspondence Strength Tester', 'Description Scout',
                  'Description Builder', 'Description Strength Tester',
                  'Description Top Down Scout', 'Group Builder',
                  'Group Strength Tester', 'Group Category Scout',
                  'Group Direction Scout', 'Group Whole String Scout',
                  'Replacement Finder', 'Rule Builder', 'Rule Scout',
                  'Rule Strenth Tester', 'Rule Translator']

        self.codelets = []
        self.counts = []
        x = self.x + 43
        y = self.h - 10
        z = 0
        for name, vname in zip(names, vnames):
            if z == 12:
                x += 270
                y = self.h - 10
            label = pyglet.text.Label(vname, font_name="EraserDust", font_size=12, x=x, y=y,
                                      color=(200, 255, 180, 80), batch=self.batch)
            label2 = pyglet.text.Label("", font_name="EraserDust", font_size=12, x=x - 25, y=y,
                                       color=(255, 255, 255, 0), batch=self.batch)
            label.name = name
            self.codelets.append(label)
            self.counts.append(label2)
            y -= 25
            z += 1

    def update(self, dt):
        counts = dd(int)
        for codelet in self.coderack.codelets():
            counts[codelet.__class__.__name__] += 1

        for name, number in zip(self.codelets, self.counts):
            if self.coderack.last_chosen.__class__.__name__ == name.name:
                name.color = (200, 255, 180, 200)
            else:
                alpha = int(name.color[3] - (name.color[3] * dt))
                if alpha > 80:
                    name.color = (200, 255, 180, alpha)

            # Change this to urgency instead of number.
            num = counts[name.name]
            name = str(num)
            if number.text != name:
                number.text = name
                number.color = (255, 255, 255, min(200, num * 40))
