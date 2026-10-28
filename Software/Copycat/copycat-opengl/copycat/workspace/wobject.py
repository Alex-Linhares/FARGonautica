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

"""Workspace Object."""

import random
import copycat.toolbox as toolbox

class Object(object):
    """Object is either a letter or group in the workspace.

    Attributes:
        string: Workspace String the object is a part of.
        string_number: A unique identifying number in its string.
        left_string_position: Position of the leftmost letter in the object.
        right_string_position: Position of the rightmost letter in the object.
        raw_importance:
        relative_importance:
        intra_string_happiness:
        intra_string_unhappiness:
        inter_string_happiness:
        inter_string_unhappiness:
        total_happiness:
        total_unhappiness:
        intra_string_salience:
        inter_string_salience:
        total_salience:
        descriptions: A list of descriptions of the object.
        extrinsic_descriptions: Descriptions with respect to other objects.
        outgoing_bonds:
        incoming_bonds:
        left_bond:
        right_bond:
        group:
        replacement:
        correspondence:
        is_change: True if the letter is the initial_string letter that changed.
        is_new_answer_letter: True if this is the new letter for the answer.
        clamp_salience: True if the salience of the object is to be clamped."""

    def __init__(self, workspace):
        """Initializes Object."""
        self.workspace = workspace
        self.slipnet = self.workspace.slipnet
        self.type_name = ''
        self.string = None
        self.string_number = None
        self.left_string_position = None
        self.right_string_position = None
        self.raw_importance = 0
        self.relative_importance = 0
        self.intra_string_happiness = 0
        self.intra_string_unhappiness = 0
        self.inter_string_happiness = 0
        self.inter_string_unhappiness = 0
        self.total_happiness = 0
        self.total_unhappiness = 0
        self.intra_string_salience = 0
        self.inter_string_salience = 0
        self.total_salience = 0
        self.descriptions = []
        self.extrinsic_descriptions = []
        self.outgoing_bonds = []
        self.incoming_bonds = []
        self.left_bond = None
        self.right_bond = None
        self.group = None
        self.replacement = None
        self.correspondence = None
        self.is_changed = False
        self.is_new_answer_letter = False
        self.clamp_salience = False
        self.objects = []

    def flipped_version(self):
        """Return the default flipped version of an object, itself."""
        return self

    def calculate_raw_importance(self):
        """Return the raw importance of the object.

        This is a function of the number and activation of the object's relevant
        descriptions. In addition, the importance of the changed object is
        enhanced, and the importance of objects in groups is dimished. Sum the
        activation of the descriptors of relevant descriptions up to 300."""
        descriptions = self.relevant_descriptions()
        result = min(300, sum([d.descriptor.activation for d in descriptions]))
        if self.is_changed:
            result *= 2
        if self.group:
            result *= 2/3.0
        return result

    def calculate_intra_string_happiness(self):
        """Return the intra string happiness of the object.

        This value represents how well the object is fitting into a structuring
        of its string and is a function of the strength of bonds or a group
        involving the object."""
        if self.spans_whole_string():
            return 100
        if self.group:
            return self.group.total_strength
        bonds = self.all_bonds()
        if not bonds:
            return 0
        if self.is_leftmost_in_string() or self.is_rightmost_in_string():
            return round(bonds[0].total_strength / 3.0)
        return round(sum([b.total_strength for b in bonds]) / 6.0)

    def calculate_intra_string_unhappiness(self):
        """Return the intra string unhappiness."""
        return 100 - self.intra_string_happiness

    def calculate_inter_string_happiness(self):
        """Return the inter string happiness.

        This value represents how well the object is fitting into a mapping
        from the initial string to the target string."""
        if self.correspondence:
            return self.correspondence.total_strength
        else:
            return 0

    def calculate_inter_string_unhappiness(self):
        """Return the inter string unhappiness."""
        return 100 - self.inter_string_happiness

    def calculate_total_happiness(self):
        """Return the total string happiness."""
        return round(toolbox.average(self.intra_string_happiness,
                                     self.inter_string_happiness))

    def calculate_total_unhappiness(self):
        """Return the total string unhappiness."""
        return 100 - self.total_happiness

    def calculate_intra_string_salience(self):
        """Return the intra string salience.

        This value represents how much the object is crying out for attention
        from codelets that build structures inside a single string."""
        if self.clamp_salience:
            return 100
        else:
            weights = [2, 8]
            values = [self.relative_importance, self.intra_string_unhappiness]
            return round(toolbox.weighted_average(weights, values))

    def calculate_inter_string_salience(self):
        """Return the inter string salience.

        This value represents how much the object is crying out for attention
        from codelets that build structures between strings."""
        if self.clamp_salience:
            return 100
        else:
            weights = [8, 2]
            values = [self.relative_importance, self.inter_string_unhappiness]
            return round(toolbox.weighted_average(weights, values))

    def calculate_total_salience(self):
        """Return the total salience of the object."""
        return round(toolbox.average(self.intra_string_salience,
                                     self.inter_string_salience))

    def update_object_values(self):
        """Update all the values for the object."""
        self.raw_importance = self.calculate_raw_importance()
        self.intra_string_happiness = self.calculate_intra_string_happiness()
        self.intra_string_unhappiness = self.calculate_intra_string_unhappiness()
        self.inter_string_happiness = self.calculate_inter_string_happiness()
        self.inter_string_unhappiness = self.calculate_inter_string_unhappiness()
        self.total_happiness = self.calculate_total_happiness()
        self.total_unhappiness = self.calculate_total_unhappiness()
        self.intra_string_salience = self.calculate_intra_string_salience()
        self.inter_string_salience = self.calculate_inter_string_salience()
        self.total_salience = self.calculate_total_salience()

    def letter_span(self):
        """Return the number of letters spanned by the object."""
        if self.type_name == 'letter':
            return 1
        return sum([obj.letter_span() for obj in self.objects])

    def letters(self):
        """Return a list of the letters at the lowest level of the object."""
        if self.type_name == 'letter':
            return [self]
        letters = []
        for obj in self.objects:
            letters.extend(obj.letters())
        return letters

    def is_leftmost_in_string(self):
        """Return True if object is leftmost in its string."""
        return self.left_string_position == 0

    def is_rightmost_in_string(self):
        """Return True if object is rightmost in its string."""
        right_position = len(self.string.get_letters()) - 1
        return self.right_string_position == right_position

    def is_middle_in_string(self):
        """Return True if object is in the middle of its string."""
        left_neighbor = self.ungrouped_left_neighbor()
        right_neighbor = self.ungrouped_right_neighbor()
        return left_neighbor and right_neighbor and \
                left_neighbor.is_leftmost_in_string() and \
                right_neighbor.is_rightmost_in_string()

    def spans_whole_string(self):
        """Return True if the object is the single letter in its string or a
        group that spans the string."""
        return self.letter_span() == self.string.length

    def is_string_spanning_group(self):
        """Return True if the object is a group that spans the string."""
        return self.type_name == 'group' and self.spans_whole_string()

    def ungrouped_left_neighbor(self):
        """Return the left neighbor of the group that either isn't in a group
        or is in the same group as the object."""
        if not self.is_leftmost_in_string():
            for left_neighbor in self.all_left_neighbors():
                group = left_neighbor.group
                if not group or group.is_recursive_member(self):
                    return left_neighbor

    def ungrouped_right_neighbor(self):
        """Return the right neighbor of the group that either isn't in a group
        or is in the same group as the object."""
        if not self.is_rightmost_in_string():
            for right_neighbor in self.all_right_neighbors():
                group = right_neighbor.group
                if not group or group.is_recursive_member(self):
                    return right_neighbor

    def all_left_neighbors(self):
        """Return a list of all the object's immediate left neighbors, both
        letters and groups."""
        objects = []
        if not self.is_leftmost_in_string():
            position = self.left_string_position - 1
            for obj in self.string.object_positions[position]:
                if not obj:
                    continue
                if not (obj.type_name == 'group' and \
                        obj.is_recursive_member(self)):
                    objects.append(obj)
        return objects

    def all_right_neighbors(self):
        """Return a list of all the objects's right neighbors."""
        objects = []
        if not self.is_rightmost_in_string():
            position = self.right_string_position + 1
            for obj in self.string.object_positions[position]:
                if not obj:
                    continue
                if not (obj.type_name == 'group' and \
                        obj.is_recursive_member(self)):
                    objects.append(obj)
        return objects

    def all_neighbors(self):
        """Return a list of all the immediate neighbors of the objects."""
        return self.all_left_neighbors() + self.all_right_neighbors()

    def random_left_neighbor(self):
        """Return a randomly selected left neighbor."""
        return random.choice(self.all_left_neighbors())

    def random_right_neighbor(self):
        """Return a randomly selected right neighbor."""
        return random.choice(self.all_right_neighbors())

    def random_neighbor(self):
        """Return a randomly selected neighbor."""
        return random.choice(self.all_neighbors())

    def choose_left_neighbor(self):
        """Choose a left neighbor probabilistically based on intra string
        salience."""
        neighbors = self.all_left_neighbors()
        values = [obj.intra_string_salience for obj in neighbors]
        return toolbox.weighted_select(values, neighbors)

    def choose_right_neighbor(self):
        """Choose a right neighbor probabilistically based on intra string
        salience."""
        neighbors = self.all_right_neighbors()
        values = [obj.intra_string_salience for obj in neighbors]
        return toolbox.weighted_select(values, neighbors)

    def choose_neighbor(self):
        """Choose a neighbor probabilistically using intra string salience."""
        neighbors = self.all_neighbors()
        saliences = [obj.intra_string_salience for obj in neighbors]
        return toolbox.weighted_select(saliences, neighbors)

    def all_bonds(self):
        """Return all bonds connected to this object."""
        return self.incoming_bonds + self.outgoing_bonds

    def add_description(self, description):
        """Add the given description to the object's description list."""
        description.descriptor_number = len(self.descriptions)
        self.descriptions.append(description)

    def add_extrinsic_description(self, description):
        """Add the given extrinsic description to the object's extrinsic
        description list."""
        self.extrinsic_descriptions.append(description)

    def get_descriptor(self, description_type):
        """Return the descriptor of the object corresponding to the given
        desicription type."""
        for description in self.descriptions:
            if description.description_type == description_type:
                return description.descriptor

    def relevant_descriptions(self):
        """Return a list of the object's relevant desriptions; those whose
        description type is fully active."""
        return [d for d in self.descriptions if d.description_type.is_active()]

    def distinguishing_descriptions(self):
        """Return a list of the object's distinguishing descriptions; those
        which distinguish it in a string."""
        dist = self.is_distinguishing_descriptor
        return [d for d in self.descriptions if dist(d.descriptor)]

    def non_distinguishing_descriptions(self):
        """Return a list of the object's non distinguishing descriptions."""
        dist = self.is_distinguishing_descriptor
        return [d for d in self.descriptions if not dist(d.descriptor)]

    def is_distinguishing_descriptor(self, descriptor):
        """Return True if no other object of the same type has the same
        descriptor."""
        if descriptor == self.slipnet.plato_letter or \
           descriptor == self.slipnet.plato_group or \
           descriptor in self.slipnet.slipnet_numbers:
            return False
        if self.type_name == 'letter':
            other_objects = self.string.get_letters()
            if self in other_objects:
                other_objects.remove(self)
        else:
            other_objects = self.string.get_groups()
            if self in other_objects:
                other_objects.remove(self)
            for obj in self.objects:
                if obj.type_name == 'group':
                    if obj in other_objects:
                        other_objects.remove(obj)
        descriptions = []
        for obj in other_objects:
            descriptions.extend(obj.descriptions)
        other_descriptors = [d.descriptor for d in descriptions]
        return not descriptor in other_descriptors

    def is_recursive_member(self, obj):
        """Return True if the object is a member of the gruop or is a member
        of a member, etc."""
        if obj.type_name == 'letter':
            return False
        elif obj in self.objects:
            return True
        for obj in self.objects:
            if obj.is_recursive_member(obj):
                return True

    def relevant_distinguishing_descriptions(self):
        """Return a list of relevant distinguishing descriptions."""
        descriptions = self.distinguishing_descriptions()
        return [d for d in descriptions if d.description_type.is_active()]

    def relevant_non_distinguishing_descriptions(self):
        """Return a list of relevant non distinguishing descriptions."""
        descriptions = self.non_distinguishing_descriptions()
        return [d for d in descriptions if d.description_type.is_active()]

    def choose_relevant_description_by_activation(self):
        """Return a relevant description probabilistically based on the
        descriptor's activation."""
        relevant_descriptions = self.relevant_descriptions()
        if relevant_descriptions:
            descriptors = [d.descriptor for d in relevant_descriptions]
            activations = [d.activation for d in descriptors]
            return toolbox.weighted_select(activations, relevant_descriptions)

    def choose_relevant_distinguishing_description_by_conceptual_depth(self):
        """Return a relevant, distinguishing description probabilistically
        based on the descriptor's conceptual depth."""
        relevant_descriptions = self.relevant_distinguishing_descriptions()
        if relevant_descriptions:
            depths = [d.conceptual_depth() for d in relevant_descriptions]
            return toolbox.weighted_select(depths, relevant_descriptions)

    def is_description_present(self, description):
        """Return True if this object already has this description."""
        for description in self.descriptions:
            if description.description_type == description.description_type and \
               description.descriptor == description.descriptor:
                return True

    def rule_initial_string_descriptions(self):
        """Return all the descriptions that can be used in making the initial
        string part of the rule, with this object as the changed object."""
        descriptions = []
        for description in self.descriptions:
            if description.description_type.is_active() and \
               self.is_distinguishing_descriptor(description.descriptor) and \
               description.descriptor != self.slipnet.plato_object_category:
                descriptions.append(description)
        return descriptions

    def rule_modified_string_descriptions(self):
        """Return all the non extrinsic descriptions that can be used in make
        the modified string part of the rule with this object as the object in
        string corresponding to the initial string changed object."""
        descriptions = []
        categories = [self.slipnet.plato_string_position_category,
                      self.slipnet.plato_object_category]
        for description in self.descriptions:
            if description.description_type.is_active() and \
               self.is_distinguishing_descriptor(description.descriptor) and \
               description.description_type not in categories:
                descriptions.append(description)
        return descriptions

    def add_incoming_bond(self, bond):
        """Add a new incoming bond to the object."""
        self.incoming_bonds.append(bond)

    def add_outgoing_bond(self, bond):
        """Add a new outgoing bond to the object."""
        self.outgoing_bonds.append(bond)

    def remove_incoming_bond(self, bond):
        """Remove an incoming bond from the object."""
        if bond in self.incoming_bonds:
            self.incoming_bonds.remove(bond)

    def remove_outgoing_bond(self, bond):
        """Remove an outgoing bond from the object."""
        if bond in self.outgoing_bonds:
            self.outgoing_bonds.remove(bond)

    def structures(self):
        """Return a list of the structures attached to the object."""
        return self.descriptions + [self.left_bond, self.right_bond,
                                    self.group, self.correspondence]

    def is_descriptor_present(self, descriptor):
        """Return True if this object has a description with descriptor."""
        for description in self.descriptions:
            if description.descriptor == descriptor:
                return True

    def is_description_type_present(self, description_type):
        """Return True if this object has a description with the given type."""
        for description in self.descriptions:
            if description.description_type == description_type:
                return True
