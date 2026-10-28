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

"""Group"""

import math

import copycat.toolbox as toolbox
from copycat.workspace import Object, Structure, Description, Mapping

class Group(Object, Structure):
    """Group

    Attributes:
        string: The string the group is in.
        bond_facet: the description type upon which the group's bonds are based.
        bonds: A list of bonds in the group.
        objects: A list of objects in the group.
        bond_category: The category associated with the group category.
        bond_descriptions: Descriptions involving the bonds in the group."""

    def __init__(self, workspace, string, group_category, direction_category,
                 left_object, right_object, objects, bonds):
        """Initialize Group."""
        Object.__init__(self, workspace)
        Structure.__init__(self)
        self.type_name = 'group'
        self.workspace = workspace
        self.string = string
        self.structure_category = Group
        self.group_category = group_category
        self.direction_category = direction_category
        self.left_object = left_object
        self.right_object = right_object
        self.objects = objects
        self.bonds = bonds
        self.proposal_level = None

        self.middle_object = None
        for obj in objects:
            if obj.get_descriptor(self.slipnet.plato_string_position_category) == \
                    self.slipnet.plato_middle:
                self.middle_object = obj
                break

        self.left_object_position = left_object.left_string_position
        self.right_object_position = right_object.right_string_position
        self.left_string_position = self.left_object_position
        self.right_string_position = self.right_object_position

        self.bond_category = self.slipnet.get_related_node(group_category,
                                                           self.slipnet.plato_bond_category)
        self.bond_facet = None
        self.bond_descriptions = []

        if self.spans_whole_string():
            self.add_description(Description(self.workspace, self,
                                             self.slipnet.plato_object_category,
                                             self.slipnet.plato_whole))

        self.add_description(Description(self.workspace, self,
                                         self.slipnet.plato_object_category,
                                         self.slipnet.plato_group))

        category = self.slipnet.plato_string_position_category
        if self.is_leftmost_in_string() and not self.spans_whole_string():
            self.add_description(Description(self.workspace, self,
                                             self.slipnet.plato_string_position_category,
                                             self.slipnet.plato_leftmost))
        elif self.is_middle_in_string():
            self.add_description(Description(self.workspace, self,
                                             self.slipnet.plato_string_position_category,
                                             self.slipnet.plato_middle))
        elif self.is_rightmost_in_string() and not self.spans_whole_string():
            self.add_description(Description(self.workspace, self,
                                             self.slipnet.plato_string_position_category,
                                             self.slipnet.plato_rightmost))

        if group_category == self.slipnet.plato_sameness_group and \
                (bonds == [] or \
                    bonds[0].bond_facet == self.slipnet.plato_letter_category):
            category = self.slipnet.plato_letter_category
            new_group_letter_category = left_object.get_descriptor(category)
            self.add_description(Description(self.workspace, self,
                                             category,
                                             new_group_letter_category))

        category = self.slipnet.plato_group_category
        self.add_description(Description(self.workspace, self,
                                         self.slipnet.plato_group_category,
                                         group_category))

        if direction_category:
            self.add_description(Description(self.workspace, self,
                                             self.slipnet.plato_direction_category,
                                             direction_category))

        if bonds:
            new_bond_facet = bonds[0].bond_facet
            self.bond_facet = new_bond_facet
            category = self.slipnet.plato_bond_facet
            self.add_bond_description(Description(self.workspace, self,
                                                  self.slipnet.plato_bond_facet,
                                                  new_bond_facet))

        self.add_bond_description(Description(self.workspace, self,
                                              self.slipnet.plato_bond_category,
                                              self.bond_category))

        if toolbox.flip_coin(self.length_description_probability()):
            self.add_description(Description(self.workspace, self,
                                             self.slipnet.plato_length,
                                             self.slipnet.get_plato_number(self.length())))

    def __eq__(self, other):
        """Return True if the given object is equal to this group."""
        if other is None or not isinstance(other, Group):
            return False
        return all([self.left_object_position == other.left_object_position,
                    self.right_object_position == other.right_object_position,
                    self.direction_category == other.direction_category,
                    self.group_category == other.group_category])

    def __hash__(self):
        return hash((self.left_object_position, self.right_object_position,
                     self.direction_category, self.group_category))

    def calculate_internal_strength(self):
        """For now, groups based on letter category are stronger than groups
        based on other facets. This should be fixed; a more general mechanism is
        needed."""
        if self.bond_facet == self.slipnet.plato_letter_category:
            bond_facet_factor = 1.0
        else:
            bond_facet_factor = .5

        related = self.slipnet.get_related_node(self.group_category,
                                                self.slipnet.plato_bond_category)
        bond_component = related.degree_of_association() * bond_facet_factor
        length_component = {1:5, 2:20, 3:60}.get(self.length(), 90)

        bond_component_weight = bond_component ** .98
        length_component_weight = 100 - bond_component_weight
        return toolbox.weighted_average((bond_component_weight,
                                         length_component_weight),
                                        (bond_component,
                                         length_component))

    def calculate_external_strength(self):
        """Return the group's external strength."""
        if self.spans_whole_string():
            return 100
        return self.local_support()

    def number_of_local_supporting_groups(self):
        """Return the number of supporting groups in the given gruop's string.
        Looks at all the other groups in the string, counting groups of the
        same group category and direction category.  Does not take distance
        into acount; all qualifying groups in the string are counted the
        same."""
        number_of_supporting_groups = 0
        groups = self.string.get_groups()
        if self in groups:
            groups.remove(self)
        for other_group in groups:
            if (not (self.is_subgroup_of(other_group) or \
                         other_group.is_subgroup_of(self) or \
                         self.overlaps(other_group))) and \
                         other_group.group_category == self.group_category and \
                         other_group.direction_category == self.direction_category:
                number_of_supporting_groups += 1
        return number_of_supporting_groups

    def local_density(self):
        """Return the rough measure of the density in the string of groups of
        the same group category and directin category as the given group. This
        method is used in calculating the external strength of a group."""
        if self.is_string_spanning_group():
            return 100

        slot_sum = 0
        support_sum = 0

        next_object = self.left_object.choose_left_neighbor()
        if next_object and next_object.type_name == 'letter' and next_object.group:
            next_object = next_object.group
        while next_object != None:
            if next_object.type_name == 'letter':
                next_group = None
            else:
                next_group = next_object
            slot_sum += 1
            if next_group and not self.overlaps(next_group) and \
               next_group.group_category == self.group_category and \
               next_group.direction_category == self.direction_category:
                support_sum += 1
            next_object = next_object.choose_left_neighbor()

        next_object = self.right_object.choose_right_neighbor()
        if next_object and next_object.type_name == 'letter' and next_object.group:
            next_object = next_object.group
        while next_object != None:
            if next_object.type_name == 'letter':
                next_group = None
            else:
                next_group = next_object
            slot_sum += 1
            if next_group and not self.overlaps(next_group) and \
               next_group.group_category == self.group_category and \
               next_group.direction_category == self.direction_category:
                support_sum += 1
            next_object = next_object.choose_right_neighbor()

        if slot_sum == 0:
            return 100
        else:
            return round(100 * (support_sum / float(slot_sum)))

    def local_support(self):
        """Return the local support of the group in the string."""
        number = self.number_of_local_supporting_groups()
        if number == 0:
            return 0
        density = self.local_density()
        adjusted_density = 100 * (math.sqrt(density / 100.0))
        number_factor = min(1, .6 ** (1 / number ** 3))
        return round(adjusted_density * number_factor)

    def sharing_group(self, other):
        """Return True if this group is the same as the given group."""
        return self.group == other.group

    def is_subgroup_of(self, other):
        """Return True if this group is a subgroup of the given group."""
        return other.left_object_position <= self.left_object_position and \
                other.right_object_position >= self.right_object_position

    def overlaps(self, other):
        """Return True if the two groups overlap."""
        return set(self.objects).issubset(set(other.objects))

    def get_incompatible_groups(self):
        """Return a list of the groups that are incompatible with the group."""
        groups = toolbox.unique([obj.group for obj in self.objects])  # DETERMINISM-FIX
        return [g for g in groups if g is not self and g is not None]

    def get_incompatible_correspondences(self):
        """Return a list of the correspondences that are incompatible."""
        correspondences = []
        for obj in self.objects:
            if obj.correspondence and \
               self.is_incompatible_correspondence(obj.correspondence, obj):
                correspondences.append(obj.correspondence)
        return correspondences

    def is_incompatible_correspondence(self, correspondence, obj):
        """Return True if the given corresponence is incompatible with the
        group."""
        concept_mapping = None
        for mapping in correspondence.get_concept_mappings():
            if mapping.description_type1 == self.slipnet.plato_string_position_category:
                concept_mapping = mapping
                break
        if concept_mapping is None:
            return False

        other_object = correspondence.other_object(obj)
        other_bond = None
        if other_object.is_leftmost_in_string():
            other_bond = other_object.right_bond
        elif other_object.is_rightmost_in_string():
            other_bond = other_object.left_bond
        if other_bond != None:
            if other_bond.direction_category != None and \
                    self.direction_category != None:
                direction = self.slipnet.plato_direction_category
                group_mapping = Mapping(self.workspace, direction, direction,
                                        self.direction_category,
                                        other_bond.direction_category,
                                        None, None)
                if group_mapping.is_incompatible_concept_mapping(concept_mapping):
                    return True

    def leftmost_letter(self):
        """Return the leftmost letter in the group or in the leftmost subgroup
        of the group."""
        if self.left_object.type_name == 'letter':
            return self.left_object
        else:
            return self.left_object.leftmost_letter()

    def rightmost_letter(self):
        """Return the rightmost letter in the group or the rightmost subgroup
        of the group."""
        if self.right_object.type_name == 'letter':
            return self.right_object
        else:
            return self.right_object.rightmost_letter()

    def is_leftmost_in_string(self):
        """Return True if the group is leftmost in its string."""
        return self.left_object_position == 0

    def is_rightmost_in_string(self):
        """Return True if the group is righmost in its string."""
        return self.right_object_position == self.string.length - 1

    def get_left_neighbor(self):
        """Return the leftmost neighbor, if any."""
        if not self.leftmost_in_string():
            position = self.left_object_position - 1
            possible_left_neighbor = self.string.get_letter(position).group
            if possible_left_neighbor:
                if self not in possible_left_neighbor.objects and \
                   not self.is_subgroup(possible_left_neighbor):
                    return possible_left_neighbor

    def get_right_neighbor(self):
        """Return the rightmost neighbor, if any."""
        if not self.rightmost_in_string():
            position = self.right_object_position + 1
            possible_right_neighbor = self.string.get_letter(position).group
            if possible_right_neighbor:
                if self not in possible_right_neighbor.objects and \
                   not self.is_subgroup(possible_right_neighbor):
                    return possible_right_neighbor


    def add_bond_description(self, description):
        """Add a bond description to the group's list of bond descriptions."""
        self.bond_descriptions.append(description)

    def length(self):
        """Return the number of objects in the group."""
        return len(self.objects)

    def flipped_version(self):
        """Return the flipped version of this group."""
        if self.group_category == self.slipnet.plato_predecessor_group or \
                self.group_category == self.slipnet.plato_successor_group:
            new_bonds = [bond.flipped_version() for bond in self.bonds]
            opposite = self.slipnet.plato_opposite
            group_category = self.slipnet.get_related_node(self.group_category, opposite)
            direction_category = self.slipnet.get_related_node(self.direction_category,
                                                               opposite)
            flipped_group = Group(self.workspace, self.string, group_category,
                                  direction_category, self.left_object,
                                  self.right_object, self.objects, new_bonds)
            flipped_group.proposal_level = self.proposal_level
            return flipped_group
        else:
            return self

    def get_bonds_to_be_flipped(self):
        """Return a list of the bonds that need to be flipped in order for the
        group to be built."""
        bonds_to_be_flipped = []
        for bond in self.bonds:
            to_be_flipped = self.string.get_bond(bond.to_object, bond.from_object)
            if to_be_flipped and bond == to_be_flipped.flipped_version():
                bonds_to_be_flipped.append(to_be_flipped)
        return bonds_to_be_flipped

    def spans_whole_string(self):
        """Return True if the group spans the string."""
        return self.letter_span() == self.string.length

    def is_proposed(self):
        """Return True if the group's proposal level is less than built."""
        return self.proposal_level < self.workspace.built

    def single_letter_group_probability(self):
        """Return the probability to be used in deciding whether or not to
        propose the single letter group."""
        exp = {1:4, 2:2}.get(self.number_of_local_supporting_groups(), 1)
        support = self.local_support() / 100.
        percent = self.slipnet.plato_length.activation / 100.
        prob = (support * percent) ** exp
        return self.workspace.temperature_adjusted_probability(prob)

    def length_description_probability(self):
        """Return the probability to be used in deciding to add a length
        description."""
        if self.length() > 5:
            return 0
        value = self.length() ** 3
        percent = (100 - self.slipnet.plato_length.activation) / 100.
        prob = .5 ** (value * percent)
        return self.workspace.temperature_adjusted_probability(prob)
