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

import math

import copycat.toolbox as toolbox

class Slipnode(object):
    """Slipnode

    Attributes:
        name: A unique string name.
        conceptual_depth: A built in, given value based on perceived depth.
        initially_clamped: A boolean whether to clamp at the start of a run.
        directed:
        codelets: A list of codelet types attached to this node.
        clamp: A boolean that when True, activation is clamped to 100.
        intrinsic_link_length: The intrinsic length of links labeled by the node.
        shrunk_link_length: .4 of the intrinsic link length.
        activation: An integer between 0-100.
        activation_buffer: A buffer for storing activation between updates.
        category_links:
        instance_links:
        has_property_links:
        lateral_slip_links:
        lateral_nonslip_links:
        incoming_links:
        description_tester: Function testing for descriptor possibility.
        iterate_group: Function used to iterate group nodes."""

    def __init__(self, name, depth, codelets=[], intrinsic_link_length=None,
                 initially_clamped=False, directed=False):
        """Initialize Slipnode."""
        if isinstance(name, tuple):
            self.name = name[0]
            self.short_name = name[1]
        else:
            self.name = name
            self.short_name = name
        self.conceptual_depth = depth
        self.initially_clamped = initially_clamped
        self.directed = directed
        self.codelets = codelets
        self.clamp = False
        self.intrinsic_link_length = intrinsic_link_length
        if intrinsic_link_length != None:
            self.shrunk_link_length = round(intrinsic_link_length * .4)
        else:
            self.shrunk_link_length = None

        self.activation = 0
        self.activation_buffer = 0

        self.category_links = []
        self.instance_links = []
        self.has_property_links = []
        self.lateral_slip_links = []
        self.lateral_nonslip_links = []
        self.incoming_links = []

        self.description_tester = None
        self.iterate_group = None

    def are_related(self, other_node):
        """Return True if the two nodes are equal or are linked."""
        return self == other_node or self.are_linked(other_node)

    def are_linked(self, other_node):
        """Return True if the two nodes are linked in the slipnet."""
        return other_node in [ol.to_node for ol in self.outgoing_links()]

    def are_slip_linked(self, other_node):
        """Return True if the two nodes are linked by a slip link."""
        return other_node in [lsl.to_node for lsl in self.lateral_slip_links]

    def local_descriptor_support(self, string, object_category):
        """Return the percentage of objects of the category in the string that
        have this descriptor."""
        if object_category.name == 'letter':
            objects = string.letters
        else:
            objects = string.get_groups()

        objects = list(filter(lambda x: x != None, objects))

        if not objects:
            return 0

        descriptor_count = 0
        for obj in objects:
            if obj.is_descriptor_present(self):
                descriptor_count += 1

        return round(100 * (descriptor_count / float(len(objects))))

    def local_description_type_support(self, string):
        """Return the percentge of objects in the string that have descriptions
        with this descriptor type."""
        objects = string.get_objects()
        description_type_count = 0
        for obj in objects:
            if obj is None:
                continue
            if obj.is_description_type_present(self):
                description_type_count += 1

        return round(100 * (description_type_count / float(len(objects))))

    def total_description_type_support(self, string):
        """Return the total description type support with string.

        A function of the local description type support and the node's
        activation."""
        support = self.local_description_type_support(string)
        return round((support + self.activation) / 2.0)

    def outgoing_links(self):
        """Return a list of the links emanating from this node."""
        return self.has_property_links + \
               self.lateral_slip_links + \
               self.lateral_nonslip_links + \
               self.category_links + \
               self.instance_links

    def category(self):
        """Returns the category this node belongs to.

        For example, 'leftmost' belongs to 'string_position_category'. This code
        assumes that each node belongs to at most one category."""
        if self.category_links:
            return self.category_links[0].to_node
        else:
            return None

    def intrinsic_degree_of_association(self):
        """Return the intrinsic degree of association.

        This value is 100 minus the build in link length."""
        return 100 - self.intrinsic_link_length

    def degree_of_association(self):
        """Return the degree of association encoded in the links labeled."""
        if self.is_active():
            return 100 - self.shrunk_link_length
        else:
            return 100 - self.intrinsic_link_length

    def bond_degree_of_association(self):
        """Return the degree of association bonds of the given cagtegory are
        considered to have."""
        return min(100, round(11 * math.sqrt(self.degree_of_association())))

    def is_active(self):
        """Return True if the nodes activation is the max value (100)."""
        return self.activation == 100

    def get_possible_descriptors(self, obj):
        """Return a list of instances of the node that could be used as
        descriptors for the given object."""
        descriptors = []
        for link in self.instance_links:
            instance = link.to_node
            if instance.description_tester(obj):
                descriptors.append(instance)
        return descriptors

    def apply_slippages(self, slippage_list):
        """Return the node that is the translation of the node according to the
        given slippages."""
        for slippage in slippage_list:
            if slippage.descriptor1 == self:
                return slippage.descriptor2
        return self

    def decay(self):
        """Remove 100 - conceptual_depth percent of the node's activation."""
        amount = round(((100 - self.conceptual_depth) / 100.0) * self.activation)
        self.activation_buffer = self.activation_buffer - amount

    def similar_has_property_links(self):
        """Return a list of has property links that are similar."""
        similar_links = []
        for link in self.has_property_links:
            prob = link.degree_of_association() / 100.0
            if toolbox.flip_coin(prob):
                similar_links.append(link)
        return similar_links
