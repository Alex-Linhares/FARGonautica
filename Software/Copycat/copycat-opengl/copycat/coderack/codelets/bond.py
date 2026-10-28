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

"""Bond Codelets"""

import copycat.toolbox as toolbox
from copycat.coderack import Codelet

class BondBottomUpScout(Codelet):
    """Choose an object and a neighbor of that object probabilistically by
    intra string salience. Choose a bond facet probabilistically by
    relevance in the string. Check if there is a bond between the two
    descriptors of this facet. Post a bond strength tester codelet with
    urgency a function of the degree of association of bonds of the bond
    category."""

    structure_category = 'bond'

    def run(self, coderack, slipnet, workspace):
        from_object = workspace.choose_object('intra_string_salience')
        to_object = from_object.choose_neighbor()
        if not to_object:
            return # Fizzle

        bond_facet = workspace.choose_bond_facet(from_object, to_object)
        if not bond_facet:
            return # Fizzle

        from_descriptor = from_object.get_descriptor(bond_facet)
        to_descriptor = to_object.get_descriptor(bond_facet)
        if not from_descriptor or not to_descriptor:
            return # Fizzle

        bond_category = slipnet.get_bond_category(from_descriptor, to_descriptor)
        if not bond_category:
            return # Fizzle

        return workspace.propose_bond(from_object, to_object, bond_category,
                                      bond_facet, from_descriptor, to_descriptor)


class BondBuilder(Codelet):
    """Attempt to build the proposed bond, fighting with any competitiors."""

    structure_category = 'bond'

    def run(self, coderack, slipnet, workspace):
        bond = self.arguments[0]
        string = bond.string
        from_object = bond.from_object
        to_object = bond.to_object

        objects = workspace.objects()
        if (from_object not in objects) or (to_object not in objects):
            return # Fizzle

        existing_bond = string.get_existing_bond(bond)
        if existing_bond:
            existing_bond.bond_category.activation_buffer += workspace.activation
            direction_category = existing_bond.direction_category
            if direction_category:
                direction_category.activation_buffer += workspace.activation
            string.remove_proposed_bond(bond)
            return # Fizzle

        string.remove_proposed_bond(bond)

        incompatible_bonds = bond.incompatible_bonds()
        if not workspace.fight_it_out(bond, 1, incompatible_bonds, 1):
            return # Fizzle

        incompatible_groups = workspace.get_common_groups(from_object, to_object)
        spans = [group.letter_span() for group in incompatible_groups]
        strength = 0 if len(spans) == 0 else max(spans)
        if not workspace.fight_it_out(bond, 1, incompatible_groups, strength):
            return # Fizzle

        incompatible_corrs = []
        at_edge = bond.is_leftmost_in_string() or bond.is_rightmost_in_string()
        if bond.direction_category and at_edge:
            incompatible_corrs = bond.incompatible_correspondences()
            if not workspace.fight_it_out(bond, 2, incompatible_corrs, 3):
                return # Fizzle

        for ibond in incompatible_bonds:
            workspace.break_bond(ibond)

        for igroup in incompatible_groups:
            workspace.break_group(igroup)

        for icorrespondence in incompatible_corrs:
            workspace.break_correspondence(icorrespondence)

        return workspace.build_bond(bond)

class BondStrengthTester(Codelet):
    """Calculate the proposed bond's strength and decide probabilistically
    whether to post a bond builder codelet with urgency a function of the
    strength."""

    structure_category = 'bond'

    def run(self, coderack, slipnet, workspace):
        bond = self.arguments[0]

        bond.update_strengths()
        strength = bond.total_strength

        probability = strength / 100.0
        probability = workspace.temperature_adjusted_probability(probability)
        if not toolbox.flip_coin(probability):
            bond.string.remove_proposed_bond(bond)
            return # Fizzle

        bond.proposal_level = 2

        bond.from_object_descriptor.activation_buffer += workspace.activation
        bond.to_object_descriptor.activation_buffer += workspace.activation
        bond.bond_facet.activation_buffer += workspace.activation

        return [(BondBuilder([bond]), strength)]

class BondTopDownCategoryScout(Codelet):
    """Choose a string probabilistically by the relevance of the category in
    the string and the string's unhappiness. Chooses an object and a neighbor
    of the object in the string probabilistically by instra string salience.
    Choose a bond facet probabilistically by relevance in in the string.
    Checks if there is a bond of the category between the two descriptors of
    the facet, posting a bond strength tester codelet with urgency a function
    of the degree of association of bonds of the category."""

    structure_category = 'bond'

    def run(self, coderack, slipnet, workspace):
        category = self.arguments[0]

        initial_string = workspace.initial_string
        target_string = workspace.target_string
        i_relevance = initial_string.local_bond_category_relevance(category)
        t_relevance = target_string.local_bond_category_relevance(category)
        i_unhappiness = initial_string.intra_string_unhappiness
        t_unhappiness = target_string.intra_string_unhappiness
        values = [round(toolbox.average(i_relevance, i_unhappiness)),
                  round(toolbox.average(t_relevance, t_unhappiness))]
        string = toolbox.weighted_select(values, [initial_string, target_string])

        obj = string.get_random_object('intra_string_salience')
        neighbor = obj.choose_neighbor()
        if neighbor is None:
            return # Fizzle

        facet = workspace.choose_bond_facet(obj, neighbor)
        if facet is None:
            return # Fizzle

        object_descriptor = obj.get_descriptor(facet)
        neighbor_descriptor = neighbor.get_descriptor(facet)
        if object_descriptor is None or neighbor_descriptor is None:
            return # Fizzle

        if slipnet.get_bond_category(object_descriptor,
                                     neighbor_descriptor) == category:
            from_object = obj
            to_object = neighbor
            from_descriptor = object_descriptor
            to_descriptor = neighbor_descriptor
        elif slipnet.get_bond_category(neighbor_descriptor,
                                       object_descriptor) == category:
            from_object = neighbor
            to_object = obj
            from_descriptor = neighbor_descriptor
            to_descriptor = object_descriptor
        else:
            return # Fizzle

        return workspace.propose_bond(from_object, to_object, category, facet,
                                      from_descriptor, to_descriptor)

class BondTopDownDirectionScout(Codelet):
    """Choose a string probabilistically by the relevance of the direction
    category in the string and the string's unhappiness. Chooses an object
    in the string probabilisitically by intra string salience. Chooses a
    neighbor of the object in the given direction. Chooses a bond facet
    probabilistically by relevance in the string. Checks if there is a
    bond of the given direction between the two descriptors of the facet,
    posting a bond strength tester codelet with urgency a function of the
    degree of association of bonds of the bond category."""

    structure_category = 'bond'

    def run(self, coderack, slipnet, workspace):
        category = self.arguments[0]

        initial_string = workspace.initial_string
        target_string = workspace.target_string
        i_relevance = initial_string.local_direction_category_relevance(category)
        t_relevance = target_string.local_direction_category_relevance(category)
        i_unhappiness = initial_string.intra_string_unhappiness
        t_unhappiness = target_string.intra_string_unhappiness
        values = [round(toolbox.average(i_relevance, i_unhappiness)),
                  round(toolbox.average(t_relevance, t_unhappiness))]
        string = toolbox.weighted_select(values, [initial_string, target_string])

        obj = string.get_random_object('intra_string_salience')
        if category == slipnet.plato_left:
            neighbor = obj.choose_left_neighbor()
        elif category == slipnet.plato_right:
            neighbor = obj.choose_right_neighbor()
        if neighbor is None:
            return # Fizzle

        facet = workspace.choose_bond_facet(obj, neighbor)
        if facet is None:
            return # Fizzle

        object_descriptor = obj.get_descriptor(facet)
        neighbor_descriptor = neighbor.get_descriptor(facet)
        if object_descriptor is None or neighbor_descriptor is None:
            return # Fizzle

        bond_category = slipnet.get_bond_category(object_descriptor,
                                                  neighbor_descriptor)
        if bond_category is None or not bond_category.directed:
            return # Fizzle

        return workspace.propose_bond(obj, neighbor,
                                      bond_category, facet,
                                      object_descriptor, neighbor_descriptor)
