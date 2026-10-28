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

"""Break Codelets"""

import random
import copycat.toolbox as toolbox
from copycat.coderack import Codelet
from copycat.workspace import Bond, Group, Correspondence

class Breaker(Codelet):
    """Choose a structure at random and decide whether to break it as a
    function of its total weakness."""

    def run(self, coderack, slipnet, workspace):
        if toolbox.flip_coin((100.0 - workspace.temperature) / 100.0):
            return # Fizzle

        structures = workspace.structures()
        if not structures:
            return # Fizzle

        structure = random.choice(structures)
        if not structure:
            return # Fizzle

        if isinstance(structure, Bond) and structure.group:
            structures = [structure, structure.group]
        else:
            structures = [structure]

        for structure in structures:
            probability = structure.total_weakness() / 100.0
            probability = workspace.temperature_adjusted_probability(probability)
            if not toolbox.flip_coin(probability):
                return # Fizzle

        for structure in structures:
            if isinstance(structure, Bond):
                workspace.break_bond(structure)
            elif isinstance(structure, Group):
                workspace.break_group(structure)
            elif isinstance(structure, Correspondence):
                workspace.break_correspondence(structure)
