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

"""Coderack"""

import random
import copycat.toolbox as toolbox
from copycat.coderack.codelet import Codelet

class Bin(object):
    """Bin is a bucket for holding codelets of a certain urgency.

    Bins are used, and only ever used, by Coderack for managing codelets in a
    certain urgency range.

    Attributes:
        urgency_code: An integer indicating the level of urgency of the bin.
        codelets: A list of the codelets in the bin."""

    def __init__(self, urgency_code):
        """Initialize Bin."""
        self.urgency_code = urgency_code
        self.codelets = []

    def add(self, codelet):
        """Add a codelet to the bin."""
        self.codelets.append(codelet)
        codelet.bin = self

    def choose(self):
        """Choose and remove a random codelet from the bin."""
        codelet = random.choice(self.codelets)
        self.remove(codelet)
        return codelet

    def clear(self):
        """Clear the bin of all its codelets."""
        self.codelets = []

    def remove(self, codelet):
        """Remove a codelet from the bin."""
        if codelet in self.codelets:
            self.codelets.remove(codelet)

    def urgency(self, temperature):
        """Return this bin's urgency.

        The urgency value is a function of the bin's urgency code and the current
        temperature.
        """
        return round((self.urgency_code + 1) ** ((110 - temperature) / 15))

    def urgency_sum(self, temperature):
        """Return the sum of urgencies in this bin."""
        return len(self.codelets) * self.urgency(temperature)


class Coderack(object):
    """Coderack holds codelets waiting to be run.

    The coderack is split up into seven bins based on urgency. Codelets can be
    added to and chosen from the coderack.

    Attributes:
        max_codelets: The maximum size of the coderack.
        temperature: The value indicating how random a codelet choice is.
        time: The number of codelets that have been chosen so far.
        bins: A list of urgency bins in the coderack."""

    def __init__(self):
        """Initialize Coderack."""
        self.max_codelets = 100
        self.temperature = 0
        self.time = 0

        self.extremely_low_bin = Bin(0)
        self.very_low_bin = Bin(1)
        self.low_bin = Bin(2)
        self.medium_bin = Bin(3)
        self.high_bin = Bin(4)
        self.very_high_bin = Bin(5)
        self.extremely_high_bin = Bin(6)
        self.bins = [self.extremely_low_bin, self.very_low_bin,
                     self.low_bin, self.medium_bin, self.high_bin,
                     self.very_high_bin, self.extremely_high_bin]

        self.last_chosen = None

    def choose(self):
        """Choose a codelet from the coderack."""
        if self.is_empty():
            return None
        urgencies = [bin.urgency_sum(self.temperature) for bin in self.bins]
        pbin = toolbox.weighted_select(urgencies, self.bins)
        self.time += 1
        self.last_chosen = pbin.choose()
        return self.last_chosen

    def clear(self):
        """Empty the coderack of all codelets."""
        for pbin in self.bins:
            pbin.clear()

    def codelets(self):
        """Return a list of codelets in the coderack."""
        codelets = []
        for pbin in self.bins:
            codelets.extend(pbin.codelets)
        return codelets

    def is_empty(self):
        """Return True if the coderack is empty."""
        return len(self.codelets()) == 0

    def post(self, codelet, urgency):
        """Post a codelet to the coderack.

        If the coderack is already at its maximum size, remove a codelet to make
        room for the new one. The bin to post to is a function of the numver of
        bins and the urgency of the codelet passed in."""
        removed_codelet = None
        codelets = self.codelets()
        if len(codelets) == self.max_codelets:
            probabilities = [self.remove_probability(c) for c in codelets]
            removed_codelet = toolbox.weighted_select(probabilities, codelets)
            removed_codelet.bin.remove(removed_codelet)

        if urgency >= 100:
            pbin = self.extremely_high_bin
        else:
            index = int((len(self.bins) * urgency) / 100.0)
            pbin = self.bins[index]
        pbin.add(codelet)
        codelet.timestamp = self.time

        return removed_codelet

    def remove_probability(self, codelet):
        """Return the probability of removing the given codelet.

        Removal is a function of the codelet's bin urgency, biased toward
        deleting low urgency, older codelets. The '1 +' allows for some
        possibility of the highest urgency being removed."""
        codelet_age = self.time - codelet.timestamp
        codelet_bin_urgency = codelet.bin.urgency(self.temperature)
        highest_bin_urgency = self.extremely_high_bin.urgency(self.temperature)
        return codelet_age * (1 + codelet_bin_urgency - highest_bin_urgency)

    def update(self, temperature):
        """Store the current temperature for urgency calculations."""
        self.temperature = temperature

    def urgency_sum(self):
        """Return the sum of urgency of all bins in the coderack."""
        return sum([bin.urgency_sum(self.temperature) for bin in self.bins])
