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

"""Run enscapulates all the moving parts for a single copycat run."""

import random

from copycat.coderack import Coderack
from copycat.slipnet import Slipnet
from copycat.workspace import Workspace
import copycat.coderack.codelets
from copycat.coderack.codelets import AnswerBuilder

class Run(object):
    """Run

    Attributes:
        coderack:
        slipnet:
        workspace:
        timestep: The number of codelets to run before an update."""

    def __init__(self, initial, modified, target, seed):
        """Initialize Run."""
        self.coderack = Coderack()
        self.slipnet = Slipnet()
        self.workspace = Workspace(initial, modified, target, self.slipnet)
        random.seed(seed)
        self.timestep = 15

    def step(self):
        """Make one step through a run."""
        if self.coderack.time % self.timestep == 0:
            self.update()

        if self.coderack.is_empty():
            self.slipnet.clamp_initial_nodes()
            codelets = self.workspace.initial_codelets()
            for codelet, urgency in codelets:
                deleted = self.coderack.post(codelet, urgency)
                if deleted:
                    for structure in deleted.arguments:
                        self.workspace.remove_proposed_structure(structure)

        self.run_codelet(self.coderack.choose())

        if self.workspace.translated_rule:
            self.run_codelet(AnswerBuilder())
            self.update()

    def run_codelet(self, codelet):
        """Run a single codelet, posting any new codelets they create."""
        codelets = codelet.run(self.coderack, self.slipnet, self.workspace)
        if not codelets:
            return
        for codelet, urgency in codelets:
            deleted = self.coderack.post(codelet, urgency)
            if deleted:
                for structure in deleted.arguments:  # BUG-FIX: was the typo 'agruments'
                    self.workspace.remove_proposed_structure(structure)

    def update(self):
        """Update everything."""
        self.workspace.update()

        if self.coderack.time == self.slipnet.clamp_time * self.timestep:
            self.slipnet.unclamp_initial_nodes()

        self.coderack.update(self.workspace.temperature)
        codelets = self.workspace.bottom_up_codelets()
        top_down_codelet_types = self.slipnet.top_down_codelets()
        for codelet_name, args, urgency in top_down_codelet_types:
            codelet = getattr(copycat.coderack.codelets, codelet_name)
            category = codelet.structure_category
            cs = self.workspace.get_codelets(category, codelet, urgency, args)
            codelets.extend(cs)
        for (codelet, urgency) in codelets:
            deleted = self.coderack.post(codelet, urgency)
            if deleted != None:
                for structure in deleted.arguments:
                    self.workspace.remove_proposed_structure(structure)

        self.slipnet.update()

    def deal_with_snag(self):
        """If there is a snag in building the answer, delete all proposed
        structures, empty the coderack, raise and clamp the temperature,
        and activate and clamp activation of all the descriptions of the
        object causing the snag."""
        self.workspace.snag_count += 1
        self.workspace.last_snag_time = self.coderack.time
        self.workspace.snag_structures = self.workspace.structures()
        for bond in self.workspace.get_proposed_bonds():
            bond.string.remove_proposed_bond(bond)
        for group in self.workspace.get_proposed_groups():
            group.string.remove_proposed_group(group)
        for correspondence in self.workspace.get_proposed_correspondences():
            self.workspace.remove_proposed_correspondence(correspondence)
        self.workspace.translated_rule = None
        self.workspace.answer_string = None
        self.workspace.snag_condition = True
        self.workspace.temperature = 100
        self.workspace.clamp_temperature = True
        for description in self.workspace.snag_object.descriptions:
            description.descriptor.clamped = True
        self.workspace.snag_object.clamp_salience = True
        self.coderack.clear()
        self.update()
