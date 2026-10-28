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

"""Structure"""

import copycat.toolbox as toolbox

class Structure(object):
    """Structure

    Attributes:
        string: The string the structure is in.
        structure_category: E.g., Bond or Group.
        group: True if the structure is inside a group.
        internal_strength:
        external_strength:
        total_strength:"""

    def __init__(self):
        """Initialize Structure."""
        self.string = None
        self.structure_category = None
        self.group = None
        self.internal_strength = 0
        self.external_strength = 0
        self.total_strength = 0
        self.proposal_level = None

    def update_strengths(self):
        """Update the values for the structure."""
        self.update_internal_strength()
        self.update_external_strength()
        self.update_total_strength()

    def calculate_internal_strength(self):
        """Will be overwritten by other structures."""
        return 0

    def calculate_external_strength(self):
        """Will be overwritten by other structures."""
        return 0

    def update_internal_strength(self):
        """Update the internal strength."""
        self.internal_strength = self.calculate_internal_strength()

    def update_external_strength(self):
        """Update the external strength."""
        self.external_strength = self.calculate_external_strength()

    def update_total_strength(self):
        """Update the total strength."""
        weights = [self.internal_strength, 100 - self.internal_strength]
        values = [self.internal_strength, self.external_strength]
        self.total_strength = toolbox.weighted_average(weights, values)

    def total_weakness(self):
        """Return the total weakness of the structure.

        Even structures with 100 strength have a chance of being broken."""
        return 100 - (self.total_strength ** .95)
