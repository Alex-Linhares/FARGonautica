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

"""Workspace is the blackboard or workspace of copycat."""

import math
import random
import copycat.toolbox as toolbox
from copycat.workspace.structure import Structure
from copycat.workspace.wobject import Object
from copycat.workspace.description import Description
from copycat.workspace.description import ExtrinsicDescription
from copycat.workspace.mapping import Mapping
from copycat.workspace.group import Group
from copycat.workspace.letter import Letter
from copycat.workspace.correspondence import Correspondence
from copycat.workspace.bond import Bond
from copycat.workspace.replacement import Replacement
from copycat.workspace.rule import Rule
from copycat.workspace.string import String
from copycat.workspace.distribution import Distribution
from copycat.coderack.codelets import *

VERY_LOW_DISTRIBUTION = Distribution("very_low")
for i, v in {10:5, 20:150, 30:5, 40:2, 50:1,
             60:1, 70:1, 80:1, 90:1, 100:1}.items():
    VERY_LOW_DISTRIBUTION.set(i, v)

LOW_DISTRIBUTION = Distribution("low")
for i, v in {10:2, 20:5, 30:150, 40:5, 50:2,
             60:1, 70:1, 80:1, 90:1, 100:1}.items():
    LOW_DISTRIBUTION.set(i, v)

MEDIUM_DISTRIBUTION = Distribution("medium")
for i, v in {10:1, 20:2, 30:5, 40:150, 50:5,
             60:2, 70:1, 80:1, 90:1, 100:1}.items():
    MEDIUM_DISTRIBUTION.set(i, v)

HIGH_DISTRIBUTION = Distribution("high")
for i, v in {10:1, 20:1, 30:2, 40:5, 50:150,
             60:5, 70:2, 80:1, 90:1, 100:1}.items():
    HIGH_DISTRIBUTION.set(i, v)

VERY_HIGH_DISTRIBUTION = Distribution("very_high")
for i, v in {10:5, 20:150, 30:5, 40:2, 50:1,
             60:1, 70:1, 80:1, 90:1, 100:1}.items():
    VERY_HIGH_DISTRIBUTION.set(i, v)

class Workspace(object):
    """Workspace

    The workspace contains a list of replacements (mappings from the
    initial string to the modified string, e.g., from "abc" to "abd"),
    a vector of correspondences (mappings from the initial-string to the
    target string, e.g., from "abc" to "pqrs"), and an array of proposed
    correspondences.

    Attributes:
        initial_string:
        modified_string:
        target_string:
        answer_string:"""

    def __init__(self, initial, modified, target, slipnet):
        """Initializes Workspace."""
        self.slipnet = slipnet

        self.initial_string = String(self, initial)
        self.modified_string = String(self, modified)
        self.target_string = String(self, target)
        self.answer_string = None

        self.activation = 100
        self.temperature = 0
        self.clamp_temperature = False
        self.built = 3

        self.replacements = []
        self._correspondences = {}
        self.proposed_correspondences = {}

        self.rule = None
        self.translated_rule = None

        self.snag_object = None
        self.snag_condition = None
        self.snag_count = 0
        self.last_snag_time = 0
        self.snag_structures = []

        self.changed_length_group = None
        self.amount_length_changed = None
        self.modified_letters = None

        if self.initial_string.length == 1 or self.target_string.length == 1:
            self.slipnet.plato_object_category.activation_buffer += self.activation

        self.make_letters()
        self.add_descriptions()

    def make_letters(self):
        """Make letters for each string."""
        for string in [self.initial_string,
                       self.modified_string,
                       self.target_string]:
            count = 0
            for character in string.name:
                letter_category = self.slipnet.get_plato_letter(character)
                letter = Letter(self, character, string, letter_category, count)
                string.add_letter(letter)
                count += 1

    def add_descriptions(self):
        """Add initial descriptions to the letters in the strings."""
        for string in [self.initial_string,
                       self.modified_string,
                       self.target_string]:
            for letter in string.get_letters():
                description = Description(self, letter,
                                          self.slipnet.plato_object_category,
                                          self.slipnet.plato_letter)
                letter.add_description(description)
                description = Description(self, letter,
                                          self.slipnet.plato_letter_category,
                                          self.slipnet.get_plato_letter(letter.name))
                letter.add_description(description)

            leftmost_letter = string.get_leftmost_letter()
            if string.length > 1:
                rightmost_letter = string.get_rightmost_letter()
                description = Description(self, leftmost_letter,
                                          self.slipnet.plato_string_position_category,
                                          self.slipnet.plato_leftmost)
                leftmost_letter.add_description(description)

                description = Description(self, rightmost_letter,
                                          self.slipnet.plato_string_position_category,
                                          self.slipnet.plato_rightmost)
                rightmost_letter.add_description(description)
            else:
                description = Description(self, leftmost_letter,
                                          self.slipnet.plato_string_position_category,
                                          self.slipnet.plato_single)
                leftmost_letter.add_description(description)

            if string.length == 3:
                middle_letter = string.get_letter(1)
                description = Description(self, middle_letter,
                                          self.slipnet.plato_string_position_category,
                                          self.slipnet.plato_middle)
                middle_letter.add_description(description)

        for obj in self.objects():
            for description in obj.descriptions:
                description.descriptor.activation_buffer += self.activation

    def initial_codelets(self):
        """Return the codelets the program starts out with."""
        codelets = [(BondBottomUpScout(), 0),
                    (ReplacementFinder(), 0),
                    (CorrespondenceBottomUpScout(), 0)]
        number_needed = len(self.objects()) * 2
        return codelets * number_needed

    def update(self):
        """Update various values of the structures, objects, and strings in the
        workspace. Check to see if the snag conditions have been met so that
        everything can go back to normal.  Finally, update the temperature."""
        for structure in self.structures():
            structure.update_strengths()
        for obj in self.objects():
            obj.update_object_values()

        self.initial_string.update_relative_importances()
        self.target_string.update_relative_importances()
        self.initial_string.update_intra_string_unhappiness()
        self.target_string.update_intra_string_unhappiness()

        self.test_snag_condition()

        self.update_temperature()

    def test_snag_condition(self):
        """If the program is dealing with a snag, then see if any new
        structures have been made. If so, see if the snag condition should
        be ended. This will also need work as we learn more about handling
        snags."""
        if self.snag_object and self.snag_condition:
            new_structures = []
            for structure in self.structures():
                if not isinstance(structure, Bond) and \
                        not self.is_structure_in_snag_structures(structure):
                    new_structures.append(structure)

            unclamp_probability = 0
            if new_structures:
                strengths = [s.total_strength for s in new_structures]
                unclamp_probability = max(strengths) / 100.0

            if toolbox.flip_coin(unclamp_probability):
                self.snag_condition = None
                self.clamp_temperature = False
                for description in self.snag_object.descriptions:
                    description.descriptor.clamp = False
                self.snag_object.clamp_salience = False

    def update_temperature(self):
        """Update the temperature, which is a function of the average total
        unhappiness of objects in the workspace and weakness of the rule."""
        if not self.clamp_temperature:
            rule_weakness = 100
            if self.rule:
                rule_weakness -= self.rule.total_strength
            self.temperature = toolbox.weighted_average([8, 2],
                                                        [self.total_unhappiness(),
                                                         rule_weakness])

    def get_unmodified_letters_for_answer(self, objects_to_change):
        """Return the letters from the targe string that do not need to be
        changed for the answer."""
        letters = []
        category = self.slipnet.plato_letter_category
        for letter in self.target_string.get_letters():
            present = False
            for obj in objects_to_change:
                if letter in obj.letters():
                    present = True
            if not present:
                letters.append(Letter(self, letter.name, self.answer_string,
                                      letter.get_descriptor(category),
                                      letter.left_string_position))
        return letters

    def get_modified_letters_for_answer(self, obj, description_type):
        """Return a list of letters modified as directed by the translated
        rule."""
        modified_letters = []

        if isinstance(obj, Letter):
            new_descriptor = self.get_new_descriptor_for_answer(obj,
                                                                description_type)
            if not new_descriptor:
                self.snag_object = obj
                return [] # Snag

            if description_type == self.slipnet.plato_letter_category:
                new_letter = Letter(self,
                                    new_descriptor.name,
                                    self.answer_string,
                                    new_descriptor,
                                    obj.left_string_position)
                modified_letters.append(new_letter)
            else:
                self.snag_object = obj
        else:
            if description_type == self.slipnet.plato_letter_category:
                for letter in obj.letters():
                    new_descriptor = self.get_new_descriptor_for_answer(letter,
                                                                        description_type)
                    if not new_descriptor:
                        self.snag_object = letter
                        return [] # Snag

                    new_letter = Letter(self, new_descriptor.name,
                                        self.answer_string,
                                        new_descriptor,
                                        letter.left_string_position)
                    modified_letters.append(new_letter)
            else:
                self.changed_length_group = obj
                new_descriptor = self.get_new_descriptor_for_answer(obj,
                                                                    description_type)
                if new_descriptor not in self.slipnet.slipnet_numbers:
                    self.snag_object = obj
                    return [] # Snag

                for group_member in obj.objects:
                    if isinstance(group_member, Group):
                        self.snag_object = obj
                        return [] # Snag

                group_direction = obj.direction_category
                initial = int(new_descriptor.name)
                end = int(obj.get_descriptor(self.slipnet.plato_length).name)

                self.amount_length_changed = initial - end

                if not group_direction or group_direction == self.slipnet.plato_right:
                    first_letter = obj.string.get_letter(obj.left_object_position)
                    new_string_position = first_letter.left_string_position
                else:
                    first_letter = obj.string.get_letter(obj.right_object_position)
                    new_string_position = first_letter.left_string_position + \
                        self.amount_length_changed

                desc = first_letter.get_descriptor(self.slipnet.plato_letter_category)
                new_letter = Letter(self, desc.name, self.answer_string, desc,
                                    new_string_position)
                modified_letters.append(new_letter)
                new_string_position = new_letter.left_string_position

                for _ in range(1, int(new_descriptor.name) - 1):
                    if not new_letter:
                        break
                    if not group_direction or \
                       group_direction == self.slipnet.plato_right:
                        new_string_position = new_string_position + 1
                    else:
                        new_string_position = new_string_position - 1
                    gcat = obj.group_category
                    let = self.slipnet.get_plato_letter(new_letter.name)
                    new_letter_category = gcat.iterate_group(let)
                    if not new_letter_category:
                        self.snag_object = new_letter
                        return [] # Snag
                    new_letter = Letter(self, new_letter_category.name,
                                        self.answer_string,
                                        new_letter_category,
                                        new_string_position)
                    modified_letters.append(new_letter)
            self.modified_letters = modified_letters
        return modified_letters

    def get_objects_to_change_for_answer(self):
        """Return a list of objects in the target string that should be changed
        for the answer."""
        objects_to_change = []

        descriptor1 = self.translated_rule.descriptor1
        descriptor1_facet = self.translated_rule.descriptor1_facet
        for obj in self.target_string.get_objects():
            if obj.get_descriptor(self.slipnet.plato_object_category) == \
               self.translated_rule.object_category1:
                if obj.get_descriptor(descriptor1_facet) == descriptor1:
                    objects_to_change.append(obj)
            else:
                if self.translated_rule.descriptor1.description_tester and \
                   self.translated_rule.descriptor1.description_tester(obj):
                    desc = Description(self, obj, descriptor1_facet, descriptor1)
                    obj.add_description(desc)
                    objects_to_change.append(obj)

        if all([descriptor1_facet == self.slipnet.plato_string_position_category,
                len(objects_to_change) > 1]):
            for obj in self.initial_string.get_objects():
                if obj.is_changed:
                    changed_object_correspondence = obj.correspondence
                    break
            if changed_object_correspondence and \
                    changed_object_correspondence.object2 in objects_to_change:
                return [changed_object_correspondence.object2]
            else:
                for obj in objects_to_change:
                    group = obj.group
                    category = self.slipnet.plato_string_position_category
                    if not group or \
                            group.get_descriptor(category) != descriptor1:
                        return [obj]
        else:
            return objects_to_change

    def get_new_descriptor_for_answer(self, obj, description_type):
        """Return the new descriptor that should be applied to the given object
        for the answer."""
        old_descriptor = obj.get_descriptor(description_type)
        if not old_descriptor:
            return None
        if self.translated_rule.relation:
            return self.slipnet.get_related_node(old_descriptor,
                                                 self.translated_rule.relation)
        else:
            return self.translated_rule.descriptor2

    def remove_proposed_structure(self, structure):
        """Remove the given proposed structure from the workspace."""
        if isinstance(structure, Bond):
            structure.string.remove_proposed_bond(structure)
        elif isinstance(structure, Group):
            structure.string.remove_proposed_group(structure)
        elif isinstance(structure, Correspondence):
            self.remove_proposed_correspondence(structure)

    def slippages(self):
        """Return a list of slippages in all correspondences."""
        slippages = []
        for correspondence in self.correspondences():
            slippages.extend(correspondence.slippages())
        return slippages

    def intra_string_unhappiness(self):
        """Return the weighted average of the intra string unhappiness of
        objects on the workspace, weighted by each object's relative imoprtance
        in the string."""
        unhappiness = sum([obj.relative_importance * obj.intra_string_unhappiness
                           for obj in self.objects()])
        return min(100, unhappiness / 200.0)

    def inter_string_unhappiness(self):
        """Return a weighted average of the inter string unhappiness of ojbects
        on the workspace, weighted by each object's relative importnace in the
        string."""
        unhappiness = sum([obj.relative_importance * obj.inter_string_unhappiness
                           for obj in self.objects()])
        return min(100, unhappiness / 200.0)

    def total_unhappiness(self):
        """Return a weighted average of the total unhappiness of ojbects on
        the workspace, weighted by each object's relative importnace in the
        string."""
        unhappiness = sum([obj.relative_importance * obj.total_unhappiness
                           for obj in self.objects()])
        return min(100, unhappiness / 200.0)

    def is_structure_in_snag_structures(self, structure):
        """This method is used after a snag has been hit and the temperature has
        been clamped, to determine whether or not to release the temperature
        clamp.  Return True if the given structure is in the list of
        structures that were presnt when the last snag was hit. If the given
        structure was built since the snag was hit then there is some change
        that the temperature clamp with be released."""
        if isinstance(structure, Bond):
            for struct in self.snag_structures:
                if isinstance(struct, Bond):
                    if struct.from_object == structure.from_object and \
                       struct.to_object == structure.to_object and \
                       struct.bond_category == structure.bond_category and \
                       struct.direction_category == structure.direction_category:
                        return True
        elif isinstance(structure, Group):
            for struct in self.snag_structures:
                if isinstance(struct, Group):
                    if struct.left_object == structure.left_object and \
                       struct.right_object == structure.right_object and \
                       struct.group_category == structure.group_category and \
                       struct.direction_category == structure.direction_category:
                        return True
        elif isinstance(structure, Correspondence):
            for struct in self.snag_structures:
                if isinstance(struct, Correspondence):
                    if struct.object1 == structure.object1 and \
                       struct.object2 == structure.object2 and \
                       len(struct.relevant_distinguishing_concept_mappings()) >= \
                       len(structure.relevant_distinguishing_concept_mappings()):
                        return True
        elif isinstance(structure, Rule):
            for struct in self.snag_structures:
                if isinstance(struct, Rule):
                    return struct == structure
        return False

    def get_concept_mappings(self, object1, object2,
                             descriptions1, descriptions2):
        """Return the list of concept mappings between the given descriptions of
        these two objects."""
        concept_mappings = []
        for d1 in descriptions1:
            for d2 in descriptions2:
                if d1.description_type == d2.description_type and \
                   (d1.descriptor == d2.descriptor or \
                    d1.descriptor.are_slip_linked(d2.descriptor)):
                    cm = Mapping(self, d1.description_type, d2.description_type,
                                 d1.descriptor, d2.descriptor, object1,
                                 object2)
                    concept_mappings.append(cm)
        return concept_mappings

    def get_leftmost_and_rightmost_incompatible_correspondences(self, group1, group2,
                                                                direction_category_cm):
        """Return any correspondences between leftmost and rightmost objects in
        group1 and group2 that are incompatible with a correspondence between
        the groups."""
        incompatible_correspondences = []

        leftmost_object1 = group1.left_object
        rightmost_object1 = group1.right_object
        leftmost_object2 = group2.left_object
        rightmost_object2 = group2.right_object

        if direction_category_cm.label == self.slipnet.plato_identity:
            leftmost_correspondence = leftmost_object1.correspondence
            if leftmost_correspondence and \
                    leftmost_correspondence != leftmost_object2:
                incompatible_correspondences.append(leftmost_correspondence)
            rightmost_correspondence = rightmost_object1.correspondence
            if rightmost_correspondence and \
                    rightmost_correspondence != rightmost_object2:
                incompatible_correspondences.append(rightmost_correspondence)

        if direction_category_cm.label == self.slipnet.plato_opposite:
            leftmost_correspondence = leftmost_object1.correspondence
            if leftmost_correspondence and \
                    leftmost_correspondence.object2 != rightmost_object2:
                incompatible_correspondences.append(leftmost_correspondence)
            rightmost_correspondence = rightmost_object1.correspondence
            if rightmost_correspondence and \
                    rightmost_correspondence.object2 != leftmost_object2:
                incompatible_correspondences.append(rightmost_correspondence)

        return incompatible_correspondences

    def letter_distance(self, object1, object2):
        """Return the distance in letters between the two objects."""
        if object1.left_string_position < object2.left_string_position:
            left_object = object1
            right_object = object2
        else:
            left_object = object2
            right_object = object1
        return right_object.left_string_position - left_object.right_string_position

    def get_common_groups(self, object1, object2):
        """Return any groups that contain both objects at the same level."""
        common_groups = []
        for group in object1.string.get_groups():
            if group is None:
                continue
            if group.is_recursive_member(object1) and \
                    group.is_recursive_member(object2):
                common_groups.append(group)
        return common_groups

    def proposed_bonds(self):
        """Return a list of the proposed bonds on the workspace."""
        return self.initial_string.get_proposed_bonds() + \
                self.target_string.get_proposed_bonds()

    def proposed_groups(self):
        """Return a list of the proposed groups on the workspace."""
        return self.initial_string.get_proposed_groups() + \
                self.target_string.get_proposed_groups()

    def add_proposed_correspondence(self, correspondence):
        """Add a proposed correspondence to the workspace."""
        position = (correspondence.object1.string_number,
                    correspondence.object2.string_number)
        if position in self.proposed_correspondences:
            self.proposed_correspondences[position].append(correspondence)
        else:
            self.proposed_correspondences[position] = [correspondence]

    def remove_proposed_correspondence(self, correspondence):
        """Remove a proposed correspondence from the workspace."""
        position = (correspondence.object1.string_number,
                    correspondence.object2.string_number)
        if position in self.proposed_correspondences:
            items = self.proposed_correspondences[position]
            if correspondence in items:
                items.remove(correspondence)

    def get_proposed_correspondences(self):
        """Return a list of proposed correspondences in the workspace."""
        return toolbox.flatten(self.proposed_correspondences.values())

    def get_proposed_correspondence(self, first, second):
        """Return a proposed correspondence at first, second."""
        return self.proposed_correspondences.get((first, second))

    def correspondences(self):
        """Return a list of the built correspondences on the workspace."""
        return list(self._correspondences.values())

    def add_replacement(self, replacement):
        """Add a replacement to the workspace's list of replacements."""
        self.replacements.append(replacement)

    def add_correspondence(self, correspondence):
        """Add a correspondence to the workspace."""
        index = correspondence.object1.string_number
        self._correspondences[index] = correspondence

    def delete_correspondence(self, correspondence):
        """Delete a correspondence from the workpace."""
        index = correspondence.object1.string_number
        del(self._correspondences[index])

    def is_correspondence_present(self, correspondence):
        """Return True if the given correspondence exists on the workspace."""
        if correspondence.object1.correspondence:
            existing_correspondence = correspondence.object1.correspondence
            if existing_correspondence.object2 == correspondence.object2:
                return existing_correspondence

    def letters(self):
        """Return a list of all the letters on the workspace."""
        return self.initial_string.get_letters() + \
                self.target_string.get_letters()

    def random_string(self):
        """Return either the initial string or the target string chosen
        at random."""
        return random.choice([self.initial_string, self.target_string])

    def random_object(self):
        """Return a random object on the workspace."""
        return random.choice(self.objects())

    def random_group(self):
        """Return a random group on the workspace."""
        return random.choice(self.groups())

    def random_correspondence(self):
        """Return a random correspondence on the workspace."""
        return random.choice(self.correspondences())

    def choose_object(self, method):
        """Return an object on the workspace chosen probabilistically
        (adjusted for temperature) according to the given method."""
        values = [getattr(obj, method) for obj in self.objects()]
        adjusted_values = self.temperature_adjusted_values(values)
        return toolbox.weighted_select(adjusted_values, self.objects())

    def has_null_replacement(self):
        """Return True if there is at least one letter in the initial string
        that deson't yet have a replacement."""
        for letter in self.initial_string.get_letters():
            if not letter.replacement:
                return True
        return False

    def rough_importance_of_uncorresponding_objects(self):
        """Return 'low', 'medium' or 'high' indicating the rough importance of
        uncorresponding objects in the workspace."""
        uncorresponding_objects = self.uncorresponding_objects()
        if not uncorresponding_objects:
            importance = 0
        else:
            importance = max([o.relative_importance()
                              for o in uncorresponding_objects])
        return self.rough_indication(importance / 10)

    def propose_correspondence(self, object1, object2, mappings, flip_obj2):
        """Create a proposed correspondence and pots a correspondece strength
        tester codelet with urgency a function of the strength of the
        distinguishing concept mappings underlying the correspondence."""
        correspondence = Correspondence(self, object1, object2, mappings)
        correspondence.proposal_level = 1
        for maps in correspondence.get_concept_mappings():
            maps.description_type1.activation_buffer += self.activation
            maps.descriptor1.activation_buffer += self.activation
            maps.description_type2.activation_buffer += self.activation
            maps.descriptor2.activation_buffer += self.activation
        self.add_proposed_correspondence(correspondence)
        dist_mappings = correspondence.get_distinguishing_mappings()
        return [(CorrespondenceStrengthTester([correspondence, flip_obj2]),
                 toolbox.average(*[cm.strength() for cm in dist_mappings]))]

    def propose_group(self, objects, bonds, group_category, direction_category):
        """Create a proposed group, returning a group strength tester codelet
        with urgency a function fo the degree of association of the bonds
        of the bond category associated with this group."""
        string = objects[0].string

        positions = [obj.left_string_position for obj in objects]
        left_object = objects[positions.index(min(positions))]
        positions = [obj.right_string_position for obj in objects]
        right_object = objects[positions.index(max(positions))]

        bond_category = self.slipnet.get_related_node(group_category,
                                                      self.slipnet.plato_bond_category)

        group = Group(self, string, group_category, direction_category,
                      left_object, right_object, objects, bonds)
        group.proposal_level = 1

        group.bond_category.activation_buffer += self.activation
        if group.direction_category:
            group.direction_category.activation_buffer += self.activation

        string.add_proposed_group(group)
        urgency = bond_category.bond_degree_of_association()

        return [(GroupStrengthTester([group]), urgency)]

    def build_group(self, group):
        """Build the given group."""
        string = group.string
        group.proposal_level = self.built
        string.add_group(group)
        for obj in group.objects:
            obj.group = group
        for bond in group.bonds:
            bond.group = group
        for description in group.descriptions:
            description.descriptor.activation_buffer += self.activation

    def break_group(self, group):
        """Break the given group."""
        string = group.string
        if group.group:
            self.break_group(group.group)
        string.remove_group(group)

        proposed_bonds = []
        for i in range(string.highest_string_number):
            bonds = string.get_proposed_bond(group.string_number, i)
            if bonds:
                proposed_bonds.extend(bonds)
            bonds = string.get_proposed_bond(i, group.string_number)
            if bonds:
                proposed_bonds.extend(bonds)
        for bond in toolbox.unique(proposed_bonds):  # DETERMINISM-FIX
            string.remove_proposed_bond(bond)

        for bond in group.incoming_bonds + group.outgoing_bonds:
            self.break_bond(bond)

        proposed_correspondences = []
        if string == self.initial_string:
            for i in range(self.target_string.highest_string_number):
                proposed = self.get_proposed_correspondence(group.string_number, i)
                if proposed:
                    proposed_correspondences.extend(proposed)
        else:
            for i in range(self.initial_string.highest_string_number):
                proposed = self.get_proposed_correspondence(group.string_number, i)
                if proposed:
                    proposed_correspondences.extend(proposed)
        for proposed in proposed_correspondences:
            self.remove_proposed_correspondence(proposed)

        if group.correspondence:
            self.break_correspondence(group.correspondence)

        for obj in group.objects:
            obj.group = None

        for bond in group.bonds:
            bond.group = None

    def build_description(self, description):
        """Build the new description."""
        if description.is_bond_description():
            description.object.add_bond_description(description)
        else:
            description.object.add_description(description)
        description.description_type.activation_buffer += self.activation
        description.descriptor.activation_buffer += self.activation

    def propose_description(self, obj, description_type, descriptor):
        """Create a proposed description and post a description strength tester
        codelet with urgency a function of the activation of the description's
        descriptor."""
        description = Description(self, obj, description_type, descriptor)
        description.descriptor.activation_buffer += self.activation
        urgency = description_type.activation
        return [(DescriptionStrengthTester([description]), urgency)]

    def build_bond(self, bond):
        """Build a new bond."""
        bond.proposal_level = self.built
        bond.string.add_bond(bond)
        bond.from_object.add_outgoing_bond(bond)
        bond.to_object.add_incoming_bond(bond)

        if bond.bond_category == self.slipnet.plato_sameness:
            bond.to_object.add_outgoing_bond(bond)
            bond.from_object.add_incoming_bond(bond)

        bond.left_object.right_bond = bond
        bond.right_object.left_bond = bond

        bond.bond_category.activation_buffer += self.activation
        if bond.direction_category:
            bond.direction_category.activation_buffer += self.activation

    def break_bond(self, bond):
        """Break a currently built bond."""
        bond.string.remove_bond(bond)
        bond.from_object.remove_outgoing_bond(bond)
        bond.to_object.remove_incoming_bond(bond)

        if bond.bond_category == self.slipnet.plato_sameness:
            bond.to_object.remove_outgoing_bond(bond)
            bond.from_object.remove_incoming_bond(bond)

        bond.left_object.right_bond = None
        bond.right_object.left_bond = None

    def choose_bond_facet(self, obj1, obj2):
        """Return a bond facet that is shared by both objects, probabilistically
        by description type support of the facet in obj1's string."""
        obj1_bond_facets = []
        for description_type in [d.description_type for d in obj1.descriptions]:
            if description_type.category() == self.slipnet.plato_bond_facet:
                obj1_bond_facets.append(description_type)

        obj2_bond_facets = []
        for description_type in [d.description_type for d in obj2.descriptions]:
            if description_type.category() == self.slipnet.plato_bond_facet:
                obj2_bond_facets.append(description_type)

        items = [f for f in toolbox.unique(obj1_bond_facets) if f in obj2_bond_facets]  # DETERMINISM-FIX
        support = [f.total_description_type_support(obj1.string) for f in items]
        return toolbox.weighted_select(support, items)

    def propose_bond(self, from_object, to_object, bond_category,
                     bond_facet, from_descriptor, to_descriptor):
        """Create a proposed bond and posts a bond strength tester codelet
        with urgency a function of the degree of association of the bond."""
        from_descriptor.activation_buffer += self.activation
        to_descriptor.activation_buffer += self.activation
        bond_facet.activation_buffer += self.activation

        proposed_bond = Bond(self, from_object, to_object, bond_category,
                             bond_facet, from_descriptor, to_descriptor)
        proposed_bond.proposal_level = 1
        from_object.string.add_proposed_bond(proposed_bond)

        urgency = bond_category.bond_degree_of_association()
        return [(BondStrengthTester([proposed_bond]), urgency)]

    def build_correspondence(self, correspondence):
        """Build the new correspondence."""
        correspondence.proposal_level = self.built
        object1 = correspondence.object1
        object2 = correspondence.object2
        object1.correspondence = correspondence
        object2.correspondence = correspondence
        self.add_correspondence(correspondence)

        mappings = correspondence.relevant_distinguishing_concept_mappings() + \
                   correspondence.accessory_concept_mappings
        for mapping in mappings:
            if mapping.is_slippage():
                correspondence.add_accessory_concept_mapping(mapping.symmetric_version())

        if isinstance(object1, Group) and isinstance(object2, Group):
            for mapping in self.get_concept_mappings(object1, object2,
                                                     object1.bond_descriptions,
                                                     object2.bond_descriptions):
                correspondence.add_accessory_concept_mapping(mapping)
                if mapping.is_slippage():
                    mapping_sym = mapping.symmetric_version()
                    correspondence.add_accessory_concept_mapping(mapping_sym)

        for mapping in correspondence.concept_mappings:
            if mapping.label:
                mapping.label.activation_buffer += self.activation

    def break_correspondence(self, correspondence):
        """Break a correspondence in the workspace."""
        correspondence.object1.correspondence = None
        correspondence.object2.correspondence = None
        self.delete_correspondence(correspondence)

    def possible_group_bonds(self, bond_category, direction_category,
                             bond_facet, bonds):
        """Return a list of bonds that could be used in making a group of the
        entire string."""
        new_bonds = []
        opposite = self.slipnet.plato_opposite
        for bond in bonds:
            if not bond:
                new_bonds = []
                break
            elif bond.bond_facet != bond_facet:
                new_bonds = []
                break
            elif self.slipnet.get_related_node(bond.bond_category, opposite) == \
                    bond_category and \
                    self.slipnet.get_related_node(bond.direction_category, opposite) == \
                    direction_category:
                new_bonds.append(bond.flipped_version())
            elif bond.bond_category != bond_category or \
                    bond.direction_category != direction_category:
                new_bonds = []
                break
            else:
                new_bonds.append(bond)
        return new_bonds

    def answer_temperature_threshold_distribution(self):
        """Return the probability distribution from which to choose a
        temperature threshold, used by rule-translator codelets in deciding
        whether or not to try to translate the rule and create an answer."""
        if self.initial_string.length == 1 and \
           self.target_string.length == 1:
            bond_density = 1
        else:
            bonds = len(self.initial_string.get_bonds() + \
                    self.target_string.get_bonds())
            letters = (1 - self.initial_string.length) + \
                    (1 - self.target_string.length)
            bond_density = bonds / float(letters)

        if bond_density >= .8:
            return VERY_LOW_DISTRIBUTION
        elif bond_density >= .6:
            return LOW_DISTRIBUTION
        elif bond_density >= .4:
            return MEDIUM_DISTRIBUTION
        elif bond_density >= .2:
            return HIGH_DISTRIBUTION
        else:
            return VERY_HIGH_DISTRIBUTION

    def temperature_adjusted_probability(self, probability):
        """Takes a probability and returns a new probability from 0 to 1 based
        on that value and the temperature."""
        if probability == 0:
            return 0
        elif probability <= .5:
            prob_factor = max(1, int(abs(math.log(probability, 10))))
            value1 = (10 - math.sqrt(100 - self.temperature)) / 100.0
            value2 = 10 ** -(prob_factor - 1) - probability
            return min(.5, probability + value1 * value2)
        elif probability == .5:
            return .5
        elif probability > .5:
            value1 = 1 - probability
            value2 = (10 - math.sqrt(100 - self.temperature)) / 100.0
            value3 = 1 - (1 - probability)
            return max(.5, 1 - value1 + (value2 * value3))

    def temperature_adjusted_values(self, values):
        """Return a list with values that are exponential functins of the
        original values, with the exponent being a funtion of the temperature.
        The higher the temperature, the bigger the difference between unequal
        values."""
        exponent = ((100 - self.temperature) / 30.0) + .5
        return [round(val ** exponent) for val in values]

    def post_codelet_probability(self, category):
        """Return a probability to use when deciding to post codelets searching
        for this type of structure."""
        probability = 0
        if category == 'description':
            probability = math.sqrt(self.temperature)
        elif category in ['bond', 'group']:
            probability = self.intra_string_unhappiness()
        elif category == 'replacement' and self.unreplaced_objects():
            probability = 100
        elif category == 'correspondence':
            probability = self.inter_string_unhappiness()
        elif category == 'rule' and self.rule:
            probability = self.rule.total_weakness()
        elif category == 'rule':
            probability = 100
        elif category == 'translator_rule' and self.rule:
            probability = 100

        return probability / 100.

    def post_codelet_number(self, category):
        """Return the number of codelets looking for the structure given by
        category that should be posted."""
        number = 0
        case = {'few': 1, 'medium': 2, 'many': 3}
        if category == 'description':
            number = 1
        elif category == 'bond':
            number = case[self.rough_number_of_unrelated_objects()]
        elif category == 'group' and self.bonds:
            number = case[self.rough_number_of_ungrouped_objects()]
        elif category == 'replacement' and not self.rule:
            # BUG-FIX: was `and self.rule`, the inverse of Mitchell's formulas.l (0 once there
            # is a rule, else by the number of unreplaced objects): no replacement finder was
            # re-posted, a letter missed by the initial batch never got its replacement, and
            # the rule scout fizzled forever (see PORTING_NOTES.md).
            number = case[self.rough_number_of_unreplaced_objects()]
        elif category == 'correspondence':
            number = case[self.rough_number_of_uncorresponding_objects()]
        elif category == 'rule':
            number = 2
        elif category == 'translator_rule' and self.rule is not None:
            number = 1

        return number

    def get_codelets(self, category, codelet, urgency, args=[]):
        """Based on the category sent in, testfor the probability for the
        codelet related to that category to be posted.  If the test does
        succeed, the number of each codelet to return is determined based
        on category."""
        codelets = []
        probability = self.post_codelet_probability(category)
        number = self.post_codelet_number(category)
        if toolbox.flip_coin(probability):
            for _ in range(number):
                codelets.append((codelet(args), urgency))
        return codelets

    def bottom_up_codelets(self):
        """Returns various bottom up codelets, with urgency and number based on
        how many of each type of codelet is needed."""
        types = [('description', DescriptionBottomUpScout, 30),
                 ('bond', BondBottomUpScout, 30),
                 ('group', GroupWholeStringScout, 30),
                 ('replacement', ReplacementFinder, 30),
                 ('correspondence', CorrespondenceBottomUpScout, 30),
                 ('correspondence', CorrespondenceImportantObjectScout, 30),
                 ('rule', RuleScout, 30),
                 ('translator_rule', RuleTranslator,
                  30 if self.temperature > 25 else 60)]

        codelets = [(Breaker(), 0)]
        for category, codelet, urgency in types:
            codelets.extend(self.get_codelets(category, codelet, urgency))
        return codelets

    def objects(self):
        """Return a list of all objects in the workspace."""
        return self.initial_string.get_objects() + \
            self.target_string.get_objects()

    def structures(self):
        """Return a list of structures in the workspace."""
        structures = self.bonds() + self.groups() + self.correspondences()
        if self.rule:
            return structures + [self.rule]
        else:
            return structures

    def bonds(self):
        """Return all bonds in the workspace."""
        return self.initial_string.get_bonds() + self.target_string.get_bonds()

    def groups(self):
        """Return all groups in the workspace."""
        return self.initial_string.get_groups() + self.target_string.get_groups()

    def unrelated_objects(self):
        """Return a list of all objects in the workspace that have at least one
        bond slot open."""
        unrelated_objects = []
        for obj in self.objects():
            if not obj.spans_whole_string() and not obj.group:
                number_of_bonds = len(obj.incoming_bonds) + len(obj.outgoing_bonds)
                if obj.is_leftmost_in_string() or obj.is_rightmost_in_string():
                    if number_of_bonds == 0:
                        unrelated_objects.append(obj)
                else:
                    if number_of_bonds < 2:
                        unrelated_objects.append(obj)
        return unrelated_objects

    def ungrouped_objects(self):
        """Return a list of all objects in the workspace not in a group."""
        ungrouped_objects = []
        for obj in self.objects():
            if not obj.spans_whole_string() and not obj.group:
                ungrouped_objects.append(obj)
        return ungrouped_objects

    def ungrouped_bonds(self):
        """Return a list of all bonds in the workspace not in groups."""
        ungrouped_bonds = []
        for bond in self.bonds():
            if not bond.from_object.group or not bond.to_object.group:
                ungrouped_bonds.append(bond)
        return ungrouped_bonds

    def unreplaced_objects(self):
        """Return a list of all objects in the initial string that do not
        have a replacement."""
        letters = self.initial_string.get_letters()
        return [letter for letter in letters if not letter.replacement]

    def uncorresponding_objects(self):
        """Return a list of all objects in the workspace that don't have a
        correspondence."""
        return [obj for obj in self.objects() if not obj.correspondence]

    def rough_indication(self, number):
        """Return 'few', 'medium' or 'many' indicating a rought number based on
        the concrete number given."""
        if number < toolbox.blur(2):
            return 'few'
        elif number < toolbox.blur(4):
            return 'medium'
        else:
            return 'many'

    def rough_number_of_unrelated_objects(self):
        """Return 'few', 'medium' or 'many' indicating the rough number of
        unrelated objects in the workspace."""
        return self.rough_indication(len(self.unrelated_objects()))

    def rough_number_of_ungrouped_objects(self):
        """Return 'few', 'medium' or 'many' indicating the rough number of
        ungrouped objects in the workspace."""
        return self.rough_indication(len(self.ungrouped_objects()))

    def rough_number_of_unreplaced_objects(self):
        """Return 'few', 'medium' or 'many' indicating the rough number of
        unreplaced objects in the workspace."""
        return self.rough_indication(len(self.unreplaced_objects()))

    def rough_number_of_uncorresponding_objects(self):
        """Return 'few', 'medium' or 'many' indicating the rough number of
        uncorresponding objects in the workspace."""
        return self.rough_indication(len(self.uncorresponding_objects()))

    def structure_vs_structure(self, structure1, weight1, structure2, weight2):
        """Choose probabilistically between the two structures based on strength
        and the given weights. Return True if structure1 wins and False if
        structure2 wins."""
        structure1.update_strengths()
        structure2.update_strengths()
        strengths = [structure1.total_strength * weight1,
                     structure2.total_strength * weight2]
        adjusted_strengths = self.temperature_adjusted_values(strengths)
        return toolbox.weighted_select(adjusted_strengths, [True, False])

    def fight_it_out(self, structure, structure_weight, others, others_weight):
        """Choose probabilistically between the structure and the other
        structures using the method structure_vs_structure. Return True if the
        structure wins."""
        for competition in others:
            if not self.structure_vs_structure(structure, structure_weight,
                                               competition, others_weight):
                return False
        return True

    def build_rule(self, rule):
        """Build the new rule."""
        self.rule = rule
        self.activate_from_workspace_rule_descriptions(rule)

    def build_translated_rule(self, translated_rule):
        """Build the translated rule."""
        self.translated_rule = translated_rule

    def break_rule(self, _):
        """Break the rule. The only reason this function has an argument is so
        that it matchs the form of the other "break" functions and thus the
        break codelets that call it."""
        self.rule = None

    def propose_rule(self, i_object, i_description, m_object, m_description):
        """Create a proposed rule and post a rule strength tester codelet with
        urgency a function of the degree of conceptual depth of descriptions
        in the rule."""
        if not i_object:
            proposed_rule = Rule(self, None, None, None, None, None, None)
        else:
            obj_category = self.slipnet.plato_object_category
            if isinstance(m_description, ExtrinsicDescription):
                proposed_rule = Rule(self,
                                     i_object.get_descriptor(obj_category),
                                     i_description.description_type,
                                     i_description.descriptor,
                                     m_object.get_descriptor(obj_category),
                                     m_description.description_type_related,
                                     None)
                proposed_rule.relation = m_description.relation
            else:
                proposed_rule = Rule(self,
                                     i_object.get_descriptor(obj_category),
                                     i_description.description_type,
                                     i_description.descriptor,
                                     m_object.get_descriptor(obj_category),
                                     m_description.description_type,
                                     m_description.descriptor)

        if not i_description:
            urgency = 100
        else:
            value = toolbox.average(i_description.conceptual_depth(),
                                    m_description.conceptual_depth()) / 100.0
            urgency = math.sqrt(value) * 100

        return [(RuleStrengthTester([proposed_rule]), urgency)]

    def activate_from_workspace_rule_descriptions(self, rule):
        """Activate the nodes corresponding to the descriptions in the rule."""
        if rule.descriptor1:
            rule.descriptor1.activation_buffer += self.activation
        if rule.expresses_relation():
            rule.relation.activation_buffer += self.activation
        else:
            if rule.descriptor2:
                rule.descriptor2.activation_buffer += self.activation
