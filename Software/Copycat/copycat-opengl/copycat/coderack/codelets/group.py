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

"""Group Codelets"""

import random
import copycat.toolbox as toolbox
from copycat.coderack import Codelet
from copycat.workspace import Group, Description

class GroupBuilder(Codelet):
    """Attempt to build the proposed group, fighting with competitors."""

    structure_category = 'group'

    def run(self, coderack, slipnet, workspace):
        group = self.arguments[0]
        string = group.string

        existing_group = string.get_existing_group(group)
        if existing_group:
            for description in existing_group.descriptions:
                description.descriptor.activation_buffer += workspace.activation
            for description in group.descriptions:
                if not existing_group.is_description_present(description):
                    new_description = Description(workspace,
                                                  existing_group,
                                                  description.description_type,
                                                  description.descriptor)
                    workspace.build_description(new_description)
            string.remove_proposed_group(group)
            return # Fizzle

        all_bonds_exist = True
        for bond in group.bonds:
            flipped = bond.flipped_version()
            if not (string.get_existing_bond(bond) or \
                        string.get_existing_bond(flipped)):
                all_bonds_exist = False
                break
        if not all_bonds_exist:
            string.remove_proposed_group(group)
            return # Fizzle

        string.remove_proposed_group(group)

        bonds_to_flip = group.get_bonds_to_be_flipped()
        if bonds_to_flip:
            if not workspace.fight_it_out(group, group.letter_span(),
                                          bonds_to_flip, 1):
                return # Fizzle

        incompatible_groups = group.get_incompatible_groups()
        for incompatible_group in incompatible_groups:
            if (group.group_category == incompatible_group.group_category) and\
            (group.direction_category == incompatible_group.direction_category):
                group_weight = group.length()
                incompatible_weight = incompatible_group.length()
            else:
                group_weight = 1
                incompatible_weight = 1
            if not workspace.fight_it_out(group, group_weight,
                                          [incompatible_group],
                                          incompatible_weight):
                return # Fizzle

        incompatible_correspondences = group.get_incompatible_correspondences()
        if group.direction_category and incompatible_correspondences:
            if not workspace.fight_it_out(group, 1,
                                          incompatible_correspondences, 1):
                return # Fizzle

        for incompatible_group in incompatible_groups:
            workspace.break_group(incompatible_group)

        # Flip any bonds that need it and replace any bonds that were rebuilt.
        new_bonds = []
        if bonds_to_flip:
            for bond in group.bonds:
                flipped_bond = string.get_existing_bond(bond.flipped_version())
                if flipped_bond:
                    workspace.break_bond(flipped_bond)
                    workspace.build_bond(bond)
                    new_bonds.append(bond)
                else:
                    existing_bond = string.get_existing_bond(bond)
                    if existing_bond != bond:
                        new_bonds.append(existing_bond)
                    else:
                        new_bonds.append(bond)

        # Actually replacing the bonds is not in the copycat source.
        group.bonds = new_bonds

        for incompatible_correspondence in incompatible_correspondences:
            workspace.break_correspondence(incompatible_correspondence)

        workspace.build_group(group)

class GroupStrengthTester(Codelet):
    """Calculate the proposed group's strength and probabilistically decide
    whether to post a group builder codelet with urgency a function of the
    strength."""

    structure_category = 'group'

    def run(self, coderack, slipnet, workspace):
        group = self.arguments[0]

        group.update_strengths()
        strength = group.total_strength

        probability = strength / 100.0
        probability = workspace.temperature_adjusted_probability(probability)
        if not toolbox.flip_coin(probability):
            group.string.remove_proposed_group(group)
            return # Fizzle

        group.proposal_level = 2

        group.bond_category.activation_buffer += workspace.activation
        if group.direction_category:
            group.direction_category.activation_buffer += workspace.activation

        return [(GroupBuilder([group]), strength)]

class GroupTopDownCategoryScout(Codelet):
    """Choose an object, a direction to scan in, and a number of bonds to
    scan in that direction. The direction category of the group is the
    direction of the first bond scanned. If there are no bonds and the
    group category is not plato-same-group, the chooses a direction
    category probabilistically as a function of global and local relevance.
    Scans until no more bonds of the necessary type and direction are
    found. If possible, makes a proposed group of the given type of the
    objects scanned and posts a group length tester codelet with urgency
    a function of the degree of association of bonds of the given bond
    category."""

    structure_category = 'group'

    def run(self, coderack, slipnet, workspace):
        category = self.arguments[0]
        bond_category = slipnet.get_related_node(category,
                                                 slipnet.plato_bond_category)

        i_string = workspace.initial_string
        i_relevance = i_string.local_bond_category_relevance(bond_category)
        i_unhappiness = i_string.intra_string_unhappiness
        t_string = workspace.target_string
        t_relevance = t_string.local_bond_category_relevance(bond_category)
        t_unhappiness = t_string.intra_string_unhappiness
        weights = [round(toolbox.average(i_relevance, i_unhappiness)),
                   round(toolbox.average(t_relevance, t_unhappiness))]
        choices = [i_string, t_string]
        string = toolbox.weighted_select(weights, choices)

        obj = string.get_random_object('intra_string_salience')
        if obj.spans_whole_string():
            return # Fizzle

        if obj.is_leftmost_in_string():
            direction = slipnet.plato_right
        elif obj.is_rightmost_in_string():
            direction = slipnet.plato_left
        else:
            activations = [slipnet.plato_left.activation, slipnet.plato_right.activation]
            choices = [slipnet.plato_left, slipnet.plato_right]
            direction = toolbox.weighted_select(activations, choices)

        number = toolbox.weighted_index(string.bonds_to_scan_distribution)

        if direction == slipnet.plato_left:
            bond = obj.left_bond
        else:
            bond = obj.right_bond

        if bond is None or bond.bond_category != bond_category:
            if obj.type_name == 'group':
                return # Fizzle
            objects = [obj]
            bonds = []
            if category == slipnet.plato_sameness_group:
                single_letter_group_direction = None
            else:
                choices = [slipnet.plato_left, slipnet.plato_right]
                weights = [node.local_descriptor_support(string, slipnet.plato_group) \
                            for node in choices]
                index = toolbox.weighted_index(weights)
                single_letter_group_direction = choices[index]
                single_letter_group = Group(workspace, string, category,
                                            single_letter_group_direction,
                                            obj, obj, objects, bonds)

                probability = single_letter_group.single_letter_group_probability()
                if toolbox.flip_coin(probability):
                    return workspace.propose_group(objects, bonds, category,
                                                   single_letter_group_direction)
            return # Fizzle

        direction_category = bond.direction_category
        facet = bond.bond_facet
        opposite_bond_category = slipnet.get_related_node(bond_category,
                                                          slipnet.plato_opposite)
        if direction_category:
            opposite_direction_category = slipnet.get_related_node(direction_category,
                                                                   slipnet.plato_opposite)
        else:
            opposite_direction_category = None

        objects = [bond.left_object, bond.right_object]
        bonds = [bond]
        next_bond = bond
        for _ in range(2, number):
            bond_to_add = None
            if direction == slipnet.plato_left:
                next_bond = next_bond.choose_left_neighbor()
                if next_bond is None:
                    break
                else:
                    next_object = next_bond.left_object
            else:
                next_bond = next_bond.choose_right_neighbor()
                if next_bond is None:
                    break
                else:
                    next_object = next_bond.right_object

            if next_bond is None:
                bond_to_add = None
            elif all([next_bond.bond_category == bond_category,
                      next_bond.direction_category == direction_category,
                      next_bond.bond_facet == facet]):
                bond_to_add = next_bond
            elif all([next_bond.bond_category == opposite_bond_category,
                      next_bond.direction_category == opposite_direction_category,
                      next_bond.bond_facet == facet]):
                bond_to_add = next_bond.flipped_version()

            if bond_to_add:
                objects.append(next_object)
                bonds.append(bond_to_add)
            else:
                break

        return workspace.propose_group(objects, bonds, category, direction_category)

class GroupTopDownDirectionScout(Codelet):
    """Choose an object, a direction to scan in, and a number of bonds to
    scan in that direction. The category of the group is the associated
    group category of the first bond scanned. Scans until no more bonds of
    the necessary type and direction are found. If possible, makes a
    proposed group of the given direction out of the objects scanned and
    posts a group strength tester codelet with urgency a function of the
    degree of association of bonds of the given bond category."""

    structure_category = 'group'

    def run(self, coderack, slipnet, workspace):
        category = self.arguments[0]

        # Choose a string based on local direction category relevance.
        i_string = workspace.initial_string
        i_relevance = i_string.local_direction_category_relevance(category)
        i_unhappiness = i_string.intra_string_unhappiness
        t_string = workspace.target_string
        t_relevance = t_string.local_direction_category_relevance(category)
        t_unhappiness = t_string.intra_string_unhappiness
        choices = [i_string, t_string]
        weights = [round(toolbox.average(i_relevance, i_unhappiness)),
                   round(toolbox.average(t_relevance, t_unhappiness))]
        string = toolbox.weighted_select(weights, choices)

        obj = string.get_random_object('intra_string_salience')
        if obj.spans_whole_string():
            return # Fizzle

        if obj.is_leftmost_in_string():
            direction = slipnet.plato_right
        elif obj.is_rightmost_in_string():
            direction = slipnet.plato_left
        else:
            choices = [slipnet.plato_left, slipnet.plato_right]
            activations = [slipnet.plato_left.activation,
                           slipnet.plato_right.activation]
            direction = toolbox.weighted_select(activations, choices)

        number = toolbox.weighted_index(string.bonds_to_scan_distribution)

        if direction == slipnet.plato_left:
            bond = obj.left_bond
        else:
            bond = obj.right_bond

        if not bond or bond.direction_category != category:
            return # Fizzle

        bond_category = bond.bond_category
        facet = bond.bond_facet

        opposite_bond_category = slipnet.get_related_node(bond_category,
                                                          slipnet.plato_opposite)
        opposite_category = slipnet.get_related_node(category,
                                                     slipnet.plato_opposite)

        objects = [bond.left_object, bond.right_object]
        bonds = [bond]
        next_bond = bond
        for _ in range(2, number):
            bond_to_add = None
            if direction == slipnet.plato_left:
                next_bond = next_bond.choose_left_neighbor()
                if not next_bond:
                    break
                else:
                    next_object = next_bond.left_object
            else:
                next_bond = next_bond.choose_right_neighbor()
                if not next_bond:
                    break
                else:
                    next_object = next_bond.right_object

            if not next_bond:
                bond_to_add = None
            elif (next_bond.bond_category == bond_category) and \
                 (next_bond.direction_category == category) and \
                 (next_bond.bond_facet == facet):
                bond_to_add = next_bond
            elif (next_bond.bond_category == opposite_bond_category) and \
                 (next_bond.direction_category == opposite_category) and \
                 (next_bond.bond_facet == facet):
                bond_to_add = next_bond.flipped_version()

            if bond_to_add:
                objects.append(next_object)
                bonds.append(bond_to_add)
            else:
                break

        group_category = slipnet.get_related_node(bond_category,
                                                  slipnet.plato_group_category)

        return workspace.propose_group(objects, bonds, group_category, category)

class GroupWholeStringScout(Codelet):
    """Try to make a group out of the entire string. If possible, makes a
    proposed string spanning group and posts a group strength tester codelet
    with urgency a function of the degree of association of bonds of the given
    category."""

    structure_category = 'group'

    def run(self, coderack, slipnet, workspace):
        string = workspace.random_string()
        if not string.get_bonds():
            return # Fizzle

        left_object = string.get_random_leftmost_object()
        next_bond = left_object.right_bond
        objects = [left_object]
        bonds = []
        next_object = None
        while next_bond:
            bonds.append(next_bond)
            next_object = next_bond.right_object
            objects.append(next_object)
            next_bond = next_object.right_bond
        right_object = next_object
        if not bonds or not right_object.is_rightmost_in_string():
            return # Fizzle

        # Choose a random bond and try making a group based on it.
        bond = random.choice(bonds)
        bond_category = bond.bond_category
        direction_category = bond.direction_category
        bond_facet = bond.bond_facet
        bonds = workspace.possible_group_bonds(bond_category, direction_category,
                                               bond_facet, bonds)
        if not bonds:
            return # Fizzle

        group_category = slipnet.get_related_node(bond_category,
                                                  slipnet.plato_group_category)

        return workspace.propose_group(objects, bonds, group_category,
                                       direction_category)
