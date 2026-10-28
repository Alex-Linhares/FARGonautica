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

"""Description"""

from copycat.workspace import Structure
import copycat.toolbox as toolbox

class Description(Structure):
    """Description

    Attributes:
        object: The object the description is attached to.
        string: The string this description is in.
        description_type: The facet of the object this refers to.
        descriptor: the descriptor applying to the facet.
        description_number: A unique identifier within an object."""

    def __init__(self, workspace, obj, description_type, descriptor):
        """Initialize Description."""
        super(Description, self).__init__()
        self.workspace = workspace
        self.slipnet = self.workspace.slipnet
        self.object = obj
        self.string = obj.string
        self.description_type = description_type
        self.descriptor = descriptor
        self.description_number = None

    def __eq__(self, other):
        """Return True if the two descriptions are equal."""
        if other is None or not isinstance(other, Description):
            return False
        return self.description_type == other.description_type and \
                self.descriptor == other.descriptor

    def calculate_internal_strength(self):
        """Return the internal strength of the description."""
        return self.descriptor.conceptual_depth

    def calculate_external_strength(self):
        """Return the external strength of the description."""
        return toolbox.average(self.local_support(),
                               self.description_type.activation)

    def local_support(self):
        """Return the support for this description in its string."""
        total = 0
        objects = self.string.get_objects()
        if self.object in objects:
            objects.remove(self.object)

        for obj in objects:
            dtypes = [o.description_type for o in obj.descriptions]
            if not (obj.is_recursive_member(self.object) or \
                        obj.is_recursive_member(self.object)) and \
                    self.description_type in dtypes:
                total += 1

        values = {0: 0, 1: 20, 2: 60, 3: 90}
        if total in values:
            return values[total]
        else:
            return 100

    def is_relevant(self):
        """Return True if the description type being described is active."""
        return self.description_type.is_active()

    def conceptual_depth(self):
        """Return this description's conceptual depth."""
        return self.descriptor.conceptual_depth

    def is_bond_description(self):
        """Return True if the description refers to the bonds making up the
        group, either the bond category or the bond facet."""
        return self.description_type == self.slipnet.plato_bond_category or \
                self.description_type == self.slipnet.plato_bond_facet

    def apply_slippages(self, obj, slippages):
        """Return a new description with the slippages applied."""
        new_description_type = self.description_type
        new_descriptor = self.descriptor
        for slippage in slippages:
            if slippage.descriptor1 == self.description_type:
                new_description_type = slippage.descriptor2
            if slippage.descriptor1 == self.descriptor:
                new_descriptor = slippage.descriptor2

        return Description(self.workspace, obj, new_description_type, new_descriptor)

class ExtrinsicDescription(object):
    """ExtrinsicDescription is a description with respect to another object
    on the workspace.

    For example, 'successor' of 'letter-category' of other-obj.
    In 'abc -> abd', the 'd' might get the description "successor of the 'c'."""

    def __init__(self, relation, description_type_related, other_object):
        """Initialize ExtrinsicDescription."""
        self.relation = relation
        self.description_type_related = description_type_related
        self.other_object = other_object

    def conceptual_depth(self):
        """Return the conceptual depth of the extrinsic description."""
        return self.relation.conceptual_depth
