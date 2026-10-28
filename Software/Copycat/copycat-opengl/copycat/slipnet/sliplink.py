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

"""Sliplink"""

class Sliplink(object):
    """Sliplink is a link between two nodes in the slipnet.

    Attributes:
        from_node: The node this link starts at.
        to_node: The node this link ends at.
        label: The node that labels this link.
        fixed_length: A static length of the link has no label."""

    def __init__(self, from_node, to_node, label, fixed_length):
        """Initializes Sliplink."""
        self.from_node = from_node
        self.to_node = to_node
        self.label = label
        self.fixed_length = fixed_length

    def intrinsic_degree_of_association(self):
        """Return the intrinsic degree of association of the link."""
        if self.fixed_length != None:
            return 100 - self.fixed_length
        else:
            return self.label.intrinsic_degree_of_association()

    def degree_of_association(self):
        """Return the degree of association of the link."""
        if self.fixed_length != None:
            return 100 - self.fixed_length
        else:
            return self.label.degree_of_association()
