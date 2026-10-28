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

"""Mapping"""

import copycat.toolbox as toolbox

class Mapping(object):
    """Mapping

    Attributes:
        description_type1:
        description_type2:
        descriptor1:
        descritpor2:
        object1:
        object2:
        label:"""

    def __init__(self, workspace, description_type1, description_type2,
                 descriptor1, descriptor2, object1, object2):
        """Initializes Mapping."""
        self.workspace = workspace
        self.slipnet = self.workspace.slipnet
        self.description_type1 = description_type1
        self.description_type2 = description_type2
        self.descriptor1 = descriptor1
        self.descriptor2 = descriptor2
        self.label = self.slipnet.get_label_node(descriptor1, descriptor2)
        self.object1 = object1
        self.object2 = object2

    def slippability(self):
        """Return a value representing the ease with which this slippage can be
        made. Conceptual depth gives a resistance to slippage."""
        degree_of_association = self.degree_of_association()
        if degree_of_association == 100:
            return 100
        return degree_of_association * (1 - (self.conceptual_depth() / 100.0) ** 2)

    def strength(self):
        """Return a value representing the strength of the conceptual mapping.
        The closer and more general the nodes, the stronger it will be."""
        degree_of_association = self.degree_of_association()
        if degree_of_association == 100:
            return 100
        return degree_of_association * (1 + ((self.conceptual_depth()) / 100.0) ** 2)

    def is_incompatible_concept_mapping(self, other):
        """Return True if the mappings are incompatible."""
        if not (self.descriptor1.are_related(other.descriptor1) or \
                self.descriptor2.are_related(other.descriptor2)):
            return False
        if self.label is None or other.label is None:
            return False
        if self.label != other.label:
            return True
        return False

    def is_supporting_concept_mapping(self, other):
        """Return True if the two mappings support each other."""
        if self.descriptor1 == other.descriptor1 and \
           self.descriptor2 == other.descriptor2:
            return True
        elif not (self.descriptor1.are_related(other.descriptor1) or \
                  self.descriptor2.are_related(other.descriptor2)):
            return False
        elif self.label is None or other.label is None:
            return False
        elif self.label == other.label:
            return True
        return False

    def is_slippage(self):
        """Return True if the concept mapping is not an identity."""
        return self.label != self.slipnet.plato_identity

    def degree_of_association(self):
        """This method assumes the two descriptors in the concept mapping are
        connected in the self.slipnet by at most one slip link. It should be
        generalized eventually."""
        if self.descriptor1 == self.descriptor2:
            return 100
        else:
            for link in self.descriptor1.lateral_slip_links:
                if link.to_node == self.descriptor2:
                    return link.degree_of_association()

    def conceptual_depth(self):
        """Return the conceptual depth of the mapping."""
        return toolbox.average(self.descriptor1.conceptual_depth,
                               self.descriptor2.conceptual_depth)

    def is_relevant(self):
        """Return True if the mapping's description types are relevant."""
        return self.description_type1.is_active() and \
                self.description_type2.is_active()

    def is_distinguishing(self):
        """For now, the concept mapping "whole -> whole" is not considered
        distinguishing.  That is, a correspondence cannot be build on it
        alone.  This should eventually be generalized or changed."""
        if self.descriptor1 == self.slipnet.plato_whole and \
           self.descriptor2 == self.slipnet.plato_whole:
            return None
        else:
            return self.object1.is_distinguishing_descriptor(self.descriptor1) and \
                    self.object2.is_distinguishing_descriptor(self.descriptor2)

    def label_relevance(self):
        """Return the relevance of the mapping's label."""
        if self.label is None:
            return 50
        elif self.label.is_active():
            return 100
        else:
            return 0

    def symmetric_version(self):
        """Return a symmetric version of this mapping.

        For example, if the concept mapping is 'rightmost -> leftmost', return
        'leftmost -> rightmost'."""
        if self.label == self.slipnet.plato_identity:
            return self
        elif self.slipnet.get_label_node(self.descriptor2, self.descriptor1) !=\
                self.label:
            return None
        else:
            return Mapping(self.workspace,
                           self.description_type2, self.description_type1,
                           self.descriptor2, self.descriptor1,
                           self.object1, self.object2)

    def are_contradictory_concept_mappings(self, other):
        """Return True if the two concept mappings contradict each other."""
        return (self.descriptor1 == other.descriptor1 and \
                self.descriptor2 != other.descriptor2) or \
               (self.descriptor2 == other.descriptor2 and \
                self.descriptor1 != other.descriptor1)
