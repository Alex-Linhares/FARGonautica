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

"""Distribution"""

import copycat.toolbox as toolbox

class Distribution(object):
    """Distribution is used by rule translator codelets to decide whether to
    fizzle as a function of temperature.

    The distribution used is a function of how many bonds have been built.

    Attributes:
        name: The string name give to the distribution.
        probabilities: A map of temperature to probabilities."""

    def __init__(self, name):
        """Initialize Distribution."""
        self.name = name
        self.probabilities = {}

    def set(self, position, value):
        """Set a probability at a certain temperature."""
        self.probabilities[position] = value

    def get(self, position):
        """Return a probability at a certain temperature."""
        return self.probabilities.get(position, 0)

    def sum(self):
        """Return the sum of probabilities in the distribution."""
        return sum(self.probabilities.values())

    def choose(self):
        """Return a number 0-100 based on the probabilities."""
        values = [self.probabilities[i] for i in sorted(self.probabilities)]
        return toolbox.weighted_select(values, sorted(self.probabilities))
