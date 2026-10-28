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

"""Rule"""

from copycat.workspace import Structure
import copycat.toolbox as toolbox

class Rule(Structure):
    """Rule

    Attributes:
        object_category1:
        object_category2:
        descriptor1:
        descriptor2:
        descriptor1_facet:
        replace_description_type:
        relation:
        structure_category:"""

    def __init__(self, workspace, object_category1, descriptor1_facet,
                 descriptor1, object_category2, replaced_description_type,
                 descriptor2):
        """Initializes Rule."""
        super(Rule, self).__init__()
        self.workspace = workspace
        self.slipnet = self.workspace.slipnet
        self.object_category1 = object_category1
        self.descriptor1_facet = descriptor1_facet
        self.descriptor1 = descriptor1
        self.descriptor2 = descriptor2
        self.object_category2 = object_category2
        self.replaced_description_type = replaced_description_type
        self.relation = None
        self.structure_category = Rule

    def __eq__(self, other):
        """Return True if the two rules are equal."""
        return all([self.object_category1 == other.object_category1,
                    self.descriptor1_facet == other.descriptor1_facet,
                    self.descriptor1 == other.descriptor1,
                    self.object_category2 == other.object_category2,
                    self.descriptor2 == other.descriptor2,
                    self.relation == other.relation,
                    self.replaced_description_type == \
                        other.replaced_description_type])

    def to_string(self):
        """Convert the rule to a human readable sentence."""
        if self.has_no_change():
            return "Don't replace anything."

        if self.slipnet.is_adjective(self.descriptor1):
            part1 = "%s of %s %s" % (self.replaced_description_type.name,
                                     self.descriptor1.name,
                                     self.object_cateogry1.name)
        else:
            if not self.object_category1:
                part1 = self.descriptor1.name
            else:
                part1 = "%s of %s '%s'" % (self.replaced_description_type.name,
                                           self.object_category1.name,
                                           self.descriptor1.name)

        if self.expresses_relation():
            part2 = self.relation.name
        else:
            if self.slipnet.is_adjective(self.descriptor2):
                part2 = "%s of %s %s" % (self.replaced_description_type.name,
                                         self.descriptor2.name,
                                         self.object_category2.name)
            else:
                part2 = "%s" % self.descriptor2.name

        return "Replace %s by %s." % (part1, part2)

    def expresses_relation(self):
        """Return True if the rule expresses a relation between the modified
        string and the initial string."""
        return self.relation

    def has_no_change(self):
        """Return True if the rule specifies no changes are to be made."""
        return self.descriptor1 is None

    def calculate_internal_strength(self):
        """Return the rule's internal strength."""
        if self.has_no_change():
            return 100

        conceptual_depth1 = self.descriptor1.conceptual_depth
        if self.relation:
            conceptual_depth2 = self.relation.conceptual_depth
        else:
            conceptual_depth2 = self.descriptor2.conceptual_depth

        conceptual_difference = abs(conceptual_depth1 - conceptual_depth2)

        for obj in self.workspace.initial_string.get_objects():
            if obj.is_changed:
                i_object = obj
                break

        if i_object.correspondence:
            i_object_corresponding_object = i_object.correspondence.object2
        else:
            i_object_corresponding_object = None

        if not i_object_corresponding_object:
            shared_descriptor_term = 0
        else:
            slipped_descriptors = []
            for description in i_object_corresponding_object.relevant_descriptions():
                slips = description.apply_slippages(i_object_corresponding_object,
                                                    self.workspace.slippages())
                slipped_descriptors.append(slips.descriptor)
            if self.descriptor1 in slipped_descriptors:
                shared_descriptor_term = 100
            else:
                return 0

        depth = (100 - self.descriptor1.conceptual_depth) / 10.
        shared_descriptor_weight = round(depth ** 1.4)
        weights = [18, 12, shared_descriptor_weight]
        items = [toolbox.average(conceptual_depth1, conceptual_depth2),
                 100 - conceptual_difference,
                 shared_descriptor_term]
        rule_strength = round(toolbox.weighted_average(weights, items))
        return min(rule_strength, 100)

    def calculate_external_strength(self):
        """Return external strength."""
        return self.internal_strength
