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

"""Bond"""

import math

import copycat.toolbox as toolbox
from copycat.workspace import Structure, Mapping

class Bond(Structure):
    """Bond

    Attributes:
        bond_category:
        direction_category:
        from_object:
        to_object:
        bond_facet: Which facet is being related.
        from_object_descriptor:
        to_object_descriptor:"""

    def __init__(self, workspace, from_object, to_object, bond_category,
                 bond_facet, from_object_descriptor, to_object_descriptor):
        """Initialize Bond."""
        super(Bond, self).__init__()
        self.workspace = workspace
        self.slipnet = self.workspace.slipnet

        if from_object.left_string_position < to_object.left_string_position:
            self.direction_category = self.slipnet.plato_right
            self.left_object = from_object
            self.right_object = to_object
        else:
            self.direction_category = self.slipnet.plato_left
            self.left_object = to_object
            self.right_object = from_object
        if bond_category == self.slipnet.plato_sameness:
            self.direction_category = None

        self.left_string_position = min(from_object.left_string_position,
                                        to_object.left_string_position)
        self.right_string_position = max(from_object.right_string_position,
                                         to_object.right_string_position)

        self.proposal_level = None
        self.string = from_object.string
        self.structure_category = Bond
        self.bond_category = bond_category
        self.bond_facet = bond_facet

        self.from_object = from_object
        self.to_object = to_object

        self.from_object_descriptor = from_object_descriptor
        self.to_object_descriptor = to_object_descriptor

    def __eq__(self, other):
        """Return True if this and the given bond represent the same bond."""
        if other is None or not isinstance(other, Bond):
            return False
        return all([self.from_object == other.from_object,
                    self.to_object == other.to_object,
                    self.bond_category == other.bond_category,
                    self.direction_category == other.direction_category])

    def __hash__(self):
        return hash((self.from_object, self.to_object,
                     self.bond_category, self.direction_category))

    def calculate_external_strength(self):
        """Return the bond's external strength."""
        return self.local_support()

    def calculate_internal_strength(self):
        """Bonds between objects of the same type are stronger than bonds
        between different types. Letter category bonds are stronger than other
        types of bonds.  A more general mechanism is needed."""
        if type(self.from_object) is type(self.to_object):
            member_compatibility_factor = 1.0
        else:
            member_compatibility_factor = .7

        if self.bond_facet == self.slipnet.plato_letter_category:
            bond_facet_factor = 1.0
        else:
            bond_facet_factor = .7

        degree_of_association = self.bond_category.bond_degree_of_association()
        return min(100, round(member_compatibility_factor * \
                              bond_facet_factor * degree_of_association))

    def choose_left_neighbor(self):
        """Return one of the left neighbors of the bond chosen by salience."""
        if self.is_leftmost_in_string():
            return None
        left_neighbors = []
        for left_neighbor_object in self.left_object.all_left_neighbors():
            left1 = left_neighbor_object.string_number
            left2 = self.left_object.string_number
            possible_left_neighbor = self.string.left_right_bonds.get((left1, left2))
            if possible_left_neighbor != None:
                left_neighbors.append(possible_left_neighbor)
        saliences = [neighbor.salience() for neighbor in left_neighbors]
        return toolbox.weighted_select(saliences, left_neighbors)

    def choose_right_neighbor(self):
        """Return one of the right neighbors of the bond chosen by salience."""
        if self.is_rightmost_in_string():
            return None
        right_neighbors = []
        for right_neighbor_object in self.right_object.all_right_neighbors():
            right1 = self.right_object.string_number
            right2 = right_neighbor_object.string_number
            possible_right_neighbor = self.string.left_right_bonds.get((right1, right2))
            if possible_right_neighbor != None:
                right_neighbors.append(possible_right_neighbor)
        saliences = [neighbor.salience() for neighbor in right_neighbors]
        return toolbox.weighted_select(saliences, right_neighbors)

    def happiness(self):
        """Return the happiness of the bond."""
        if self.group != None:
            return self.group.total_strength
        return 0

    def unhappiness(self):
        """Return the unhappiness of the bond."""
        return 100 - self.happiness()

    def salience(self):
        """Return the salience of the bond."""
        return round(toolbox.average(self.importance(), self.unhappiness()))

    def has_members(self, object1, object2):
        '''
        Return True of the two objects are the objects in this bond.
        '''
        objects = [self.from_object, self.to_object]
        return object1 in objects and object2 in objects

    def importance(self):
        """Sameness bonds are more important than other bonds of other
        categories."""
        if self.bond_category == self.slipnet.plato_sameness:
            return 100
        return 50

    def incompatible_bonds(self):
        """Return the bonds that are incompatible with the bond."""
        return [b for b in toolbox.unique([self.left_object.right_bond,  # DETERMINISM-FIX
                                           self.right_object.left_bond]) if b is not None]

    def incompatible_correspondences(self):
        """Return the correspondences that are incompatible with this bond. This
        only applies to directed bonds and to correspondences between objects
        at the edges of strings. E.g., in "abc -> abd, pqrs -> ?, if there is
        a correspondence between the "a" and the "p" (with concept mapping
        "leftmost -> leftmost"), and a right going succesor bond from the "a"
        to the "b" in "abc", then the correspondence will be incompatible with
        a left going predecessor bond from the "q" to the "p" in "pqrs",
        because the correspondence would then imply both "leftmost -> leftmost"
        (the letters) and "right -> left (the bonds.)"""
        incompatible_correspondences = []

        if self.is_leftmost_in_string():
            correspondence = self.left_object.correspondence
            if not correspondence:
                return []
            other_object = correspondence.other_object(self.left_object)
        elif self.is_rightmost_in_string():
            correspondence = self.right_object.correspondence
            if not correspondence:
                return []
            other_object = correspondence.other_object(self.right_object)
        else:
            return []

        plato_string_position_category = self.slipnet.plato_string_position_category
        string_position_category_mapping = None
        for mapping in correspondence.get_concept_mappings():
            if mapping.description_type1 == plato_string_position_category:
                string_position_category_mapping = mapping
        if string_position_category_mapping is None:
            return []

        if other_object.is_leftmost_in_string():
            other_bond = other_object.right_bond
        elif other_object.is_rightmost_in_string():
            other_bond = other_object.left_bond
        else:
            return []

        if not other_bond:
            return []
        if other_bond.direction_category is None:
            return []

        mapping = Mapping(self.workspace,
                          self.slipnet.plato_direction_category,
                          self.slipnet.plato_direction_category,
                          self.direction_category,
                          other_bond.direction_category,
                          None, None)
        if mapping.is_incompatible_concept_mapping(string_position_category_mapping):
            incompatible_correspondences.append(correspondence)

        return incompatible_correspondences

    def is_in_group(self, group):
        """Return True if the bond is in the given group."""
        objects = group.objects
        return self.from_object in objects and self.to_object in objects

    def is_proposed(self):
        """Return True if proposal level is less than the level for  built
        structures."""
        return self.proposal_level < self.workspace.built

    def is_leftmost_in_string(self):
        """Return True if the bond is on the left edge of the string."""
        return self.left_string_position == 0

    def is_rightmost_in_string(self):
        """Return True if the bond is on the right edge of the string."""
        return self.right_string_position == self.string.length - 1

    def flipped_version(self):
        """Return the flipped version of this bond.

        For example, if the bond is a successor bond going to the right,
        returns a predecessor bond going to the left using the same two
        objects."""
        category = self.slipnet.get_related_node(self.bond_category,
                                                 self.slipnet.plato_opposite)
        flipped_bond = Bond(self.workspace, self.to_object, self.from_object,
                            category, self.bond_facet, self.to_object_descriptor,
                            self.from_object_descriptor)
        flipped_bond.proposal_level = self.proposal_level
        return flipped_bond

    def letter_span(self):
        """Return the number of letters spanned by the bond. This is 2 if the
        objects are not groups; otherwise it is the sum of the lengths of the
        groups."""
        return self.from_object.letter_span() + self.to_object.letter_span()

    def local_density(self):
        """Return a rough measure of the density in the string of bonds of the
        same bond category and direction category as the given bond. This method
        is used in calculating the external strength of a bond."""
        def calc(direction):
            """Inner calculation."""
            slot_sum = 0
            support_sum = 0
            method_name = 'choose_%s_neighbor' % direction
            last_object = {'left': self.left_object,
                           'right': self.right_object}[direction]
            next_object = getattr(last_object, method_name)()
            while next_object:
                slot_sum += 1
                first = next_object.string_number
                last = last_object.string_number
                bond = self.string.left_right_bonds.get((first, last))
                if bond:
                    if bond.bond_category == self.bond_category and \
                       bond.direction_category == self.direction_category:
                        support_sum += 1
                last_object = next_object
                next_object = getattr(next_object, method_name)()
            return slot_sum, support_sum

        slot_sum, support_sum = map(sum, zip(calc('left'), calc('right')))
        if slot_sum == 0:
            return 100
        return round(100 * (support_sum / float(slot_sum)))

    def local_support(self):
        """Return this bond's local support in the string."""
        number = self.number_of_local_supporting_bonds()
        if number == 0:
            return 0
        density = self.local_density()
        adjusted_density = 100 * (math.sqrt(density / 100.0))
        number_factor = min(1, .6 ** (1.0 / number ** 3))
        return round(adjusted_density * number_factor)

    def number_of_local_supporting_bonds(self):
        """Return the number of supporting bonds in the given bond's string.
        Looks at all the other bonds in the string, counting bonds of the same
        bond category and direction category.  Does not take distance into
        account; all qualifying bonds in the string are counted the same."""
        number_of_supporting_bonds = 0
        letter_distance = self.workspace.letter_distance
        bonds = self.string.get_bonds()
        if self in bonds:
            bonds.remove(self)
        for bond in self.string.get_bonds():
            if all([letter_distance(self.left_object, bond.left_object) != 0,
                    letter_distance(self.right_object, bond.right_object) != 0,
                    bond.bond_category == self.bond_category,
                    bond.direction_category == self.direction_category]):
                number_of_supporting_bonds += 1
        return number_of_supporting_bonds
