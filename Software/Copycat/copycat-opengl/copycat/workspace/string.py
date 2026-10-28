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

"""String"""

import random

import copycat.toolbox as toolbox

class String(object):
    """String is a letter string in the workspace.

    This could be the initial string, modified string or target string.
    Each object in a string has a unique string number that identifies
    it from other objects in the string."""

    def __init__(self, workspace, string):
        self.workspace = workspace
        self.slipnet = self.workspace.slipnet
        self.name = string
        self.highest_string_number = -1
        self.length = len(string)
        self.letters = {}
        self.groups = {}
        self.proposed_groups = {}
        self.object_positions = {}
        self.left_right_bonds = {}
        self.from_to_bonds = {}
        self.proposed_bonds = {}
        self.intra_string_unhappiness = 0
        self.bonds_to_scan_distribution = range(self.length)

    def add_to_object_positions(self, obj, position):
        """Add an object to the object positions."""
        if position in self.object_positions:
            self.object_positions[position].append(obj)
        else:
            self.object_positions[position] = [obj]

    def remove_from_object_positions(self, obj, position):
        """Remove an object from the object positions."""
        if obj in self.object_positions[position]:
            self.object_positions[position].remove(obj)

    def add_letter(self, letter):
        """Add a letter to the string."""
        self.highest_string_number += 1
        letter.string_number = self.highest_string_number
        position = letter.left_string_position
        self.letters[position] = letter
        self.add_to_object_positions(letter, position)

    def get_letters(self):
        """Return a list of letters in the string."""
        return [self.letters[index] for index in sorted(self.letters.keys())]

    def get_letter(self, position):
        """Return the letter at the given position in the string."""
        return self.letters.get(position)

    def get_random_letter(self):
        """Return a random letter from the string."""
        return random.choice(self.get_letters())

    def get_leftmost_letter(self):
        """Return the leftmost letter in the string."""
        return self.letters.get(0)

    def get_rightmost_letter(self):
        """Return the rightmost letter in the string."""
        return self.letters.get(len(self.letters) - 1)

    def add_group(self, group):
        """Add a group to the string."""
        self.highest_string_number += 1
        group.string_number = self.highest_string_number
        self.groups[group.left_object.string_number] = group
        self.add_to_object_positions(group, group.left_string_position)
        self.add_to_object_positions(group, group.right_string_position)

    def remove_group(self, group):
        """Remove a group from the string."""
        if group.left_object.string_number in self.groups:
            del self.groups[group.left_object.string_number]
        self.remove_from_object_positions(group, group.left_string_position)
        self.remove_from_object_positions(group, group.right_string_position)

    def get_groups(self):
        """Return a list of groups in the string."""
        return list(self.groups.values())

    def get_group(self, position):
        """Return the group at the given position in letters.

        Positions start at 0 and refer to the position of the leftmost object
        in the group."""
        return self.get_letter(position).group

    def get_existing_group(self, group):
        """Return the group in the string if it has the same properties as
        the given group."""
        existing_group = self.groups.get(group.left_object.string_number)
        if existing_group:
            if existing_group.length == group.length and \
                 existing_group.group_category == group.group_category and \
                 existing_group.direction_category == group.direction_category:
                return existing_group

    def add_proposed_group(self, group):
        """Add a proposed group to the string."""
        position = (group.left_object.string_number,
                    group.right_object.string_number)
        if position in self.proposed_groups:
            self.proposed_groups[position].append(group)
        else:
            self.proposed_groups[position] = [group]

    def remove_proposed_group(self, group):
        """Remove a proposed group from the string."""
        position = (group.left_object.string_number,
                    group.right_object.string_number)
        items = self.proposed_groups.get(position, [])
        if group in items:
            self.proposed_groups[position].remove(group)

    def get_proposed_groups(self):
        """Return a list of the proposed groups in the string."""
        return toolbox.unique(toolbox.flatten(self.proposed_groups.values()))  # DETERMINISM-FIX

    def get_proposed_group(self, first, second):
        """Return the proposed group at first, second position."""
        return self.proposed_groups.get((first, second))

    def add_bond(self, bond):
        """Add a bond to the string, sameness bonds in both directions."""
        left_number = bond.left_object.string_number
        right_number = bond.right_object.string_number
        self.left_right_bonds[(left_number, right_number)] = bond

        from_number = bond.from_object.string_number
        to_number = bond.to_object.string_number
        self.from_to_bonds[(from_number, to_number)] = bond

        if bond.bond_category == self.slipnet.plato_sameness:
            self.left_right_bonds[(right_number, left_number)] = bond
            self.from_to_bonds[(to_number, from_number)] = bond

    def remove_bond(self, bond):
        """Remove a built bond from the string."""
        left_number = bond.left_object.string_number
        right_number = bond.right_object.string_number
        if (left_number, right_number) in self.left_right_bonds:
            del self.left_right_bonds[(left_number, right_number)]

        from_number = bond.from_object.string_number
        to_number = bond.to_object.string_number
        if (from_number, to_number) in self.from_to_bonds:
            del self.from_to_bonds[(from_number, to_number)]

        if bond.bond_category == self.slipnet.plato_sameness:
            if (right_number, left_number) in self.left_right_bonds:
                del self.left_right_bonds[(right_number, left_number)]
            if (to_number, from_number) in self.from_to_bonds:
                del self.from_to_bonds[(to_number, from_number)]

    def get_bonds(self):
        """Return a list of the built bonds in the string."""
        return toolbox.unique(self.from_to_bonds.values())  # DETERMINISM-FIX

    def get_bond(self, from_object, to_object):
        """Return the bond between the two objects, if any."""
        return self.from_to_bonds.get((from_object.string_number,
                                       to_object.string_number))

    def get_existing_bond(self, bond):
        """Return the bond in the string if it has the same properties as
        the given bond."""
        existing_bond = self.get_bond(bond.from_object, bond.to_object)
        if existing_bond:
            if existing_bond.bond_category == bond.bond_category and \
                    existing_bond.direction_category == bond.direction_category:
                return existing_bond

    def add_proposed_bond(self, bond):
        """Add the proposed bond to the string."""
        position = (bond.from_object.string_number,
                    bond.to_object.string_number)
        if position in self.proposed_bonds:
            self.proposed_bonds[position].append(bond)
        else:
            self.proposed_bonds[position] = [bond]

    def remove_proposed_bond(self, bond):
        """Add the proposed bond to the string."""
        position = (bond.from_object.string_number,
                    bond.to_object.string_number)
        if position in self.proposed_bonds:
            items = self.proposed_bonds[position]
            if bond in items:
                self.proposed_bonds[position].remove(bond)

    def get_proposed_bonds(self):
        """Return a list of proposed bonds in the string."""
        return toolbox.unique(toolbox.flatten(self.proposed_bonds.values()))  # DETERMINISM-FIX

    def get_proposed_bond(self, first, second):
        """Return a proposed bonds at first, second in the string."""
        return self.proposed_bonds.get((first, second))

    def get_objects(self, category=None):
        """Return the list of objects of the given object category.

        If no category is given, return all objects."""
        if category == self.slipnet.plato_letter:
            return self.get_letters()
        elif category == self.slipnet.plato_group:
            return self.get_groups()
        return self.get_letters() + self.get_groups()

    def get_non_string_spanning_objects(self):
        """Return all objects that do not span the entire string."""
        return [o for o in self.get_objects() if not o.spans_whole_string()]

    def get_random_object(self, method=None):
        """Return a random object from the string."""
        if method:
            objects = self.get_objects()
            values = [getattr(obj, method) for obj in objects]
            values = self.workspace.temperature_adjusted_values(values)
            return objects[toolbox.weighted_index(values)]
        return random.choice(self.get_objects())

    def get_random_leftmost_object(self):
        """Return a random leftmost object from the string."""
        leftmost_objects = []
        category = self.slipnet.plato_string_position_category
        for obj in self.get_objects():
            if obj.get_descriptor(category) == self.slipnet.plato_leftmost:
                leftmost_objects.append(obj)
        if leftmost_objects:
            values = [obj.relative_importance for obj in leftmost_objects]
            return toolbox.weighted_select(values, leftmost_objects)

    def update_relative_importances(self):
        """Update the relative, normalized importances of all the objects in
        the string."""
        raw_importance = sum([o.raw_importance for o in self.get_objects()])
        for obj in self.get_objects():
            if raw_importance == 0:
                importance = 0
            else:
                quot = obj.raw_importance / float(raw_importance)
                importance = round(100 * quot)
            obj.relative_importance = importance

    def update_intra_string_unhappiness(self):
        """Calculate the average of the intra-string unhappiness of all the
        objects in the string."""
        unhappiness = [o.intra_string_unhappiness for o in self.get_objects()]
        self.intra_string_unhappiness = round(toolbox.average(*unhappiness))

    def local_bond_category_relevance(self, bond_category):
        """A function of how many bonds in the string have the given bond
        category. This function is not perfect; it gives just a rough
        estimate of the relevance of this bond category."""
        objects = self.get_non_string_spanning_objects()
        if len(objects) == 1:
            return 0
        bond_count = 0
        for obj in objects:
            if obj.right_bond:
                if obj.right_bond.bond_category == bond_category:
                    bond_count += 1
        return 100 * (float(bond_count) / (len(objects) - 1))

    def local_direction_category_relevance(self, direction_category):
        """A function of how many bonds in the string have the given direction
        category. This function is not perfect; it gives just a rough estimate
        of the relevance of this direction category."""
        objects = self.get_non_string_spanning_objects()
        if len(objects) == 1:
            return 0
        bond_count = 0
        for obj in objects:
            if obj.right_bond:
                if obj.right_bond.direction_category == direction_category:
                    bond_count += 1
        return 100 * (float(bond_count) / (len(objects) - 1))
