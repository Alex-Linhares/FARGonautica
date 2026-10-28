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

"""Slipnet"""

import string
import copycat.toolbox as toolbox
from copycat.slipnet.slipnode import Slipnode
from copycat.slipnet.sliplink import Sliplink

class Slipnet(object):
    """Slipnet contains nodes and the links between them.

    The Slipnet manages the activation and decay of nodes and the conceptual
    distance between them.

    Attributes:
        slipnodes: The nodes in the slipnet.
        sliplinks: The links between the nodes.
        clamp_time: The amount of steps to clamp activation in the slipnet."""

    def __init__(self):
        """Initializes Slipnet."""
        self.slipnodes = []
        self.sliplinks = []
        self.clamp_time = 50

        # Letter nodes
        self.slipnet_letters = []
        for letter in string.ascii_lowercase:
            self.slipnet_letters.append(self.add_node(str(letter), 10))

        # Number nodes
        self.slipnet_numbers = []

        node = self.add_node("1", 30)
        node.description_tester = (lambda obj:
            obj.type_name == 'group' and obj.length() == 1)
        self.slipnet_numbers.append(node)

        node = self.add_node("2", 30)
        node.description_tester = (lambda obj:
            obj.type_name == 'group' and obj.length() == 2)
        self.slipnet_numbers.append(node)

        node = self.add_node("3", 30)
        node.description_tester = (lambda obj:
            obj.type_name == 'group' and obj.length() == 3)
        self.slipnet_numbers.append(node)

        node = self.add_node("4", 30)
        node.description_tester = (lambda obj:
            obj.type_name == 'group' and obj.length() == 4)
        self.slipnet_numbers.append(node)

        node = self.add_node("5", 30)
        node.description_tester = (lambda obj:
            obj.type_name == 'group' and obj.length() == 5)
        self.slipnet_numbers.append(node)

        # String position nodes.
        self.plato_leftmost = self.add_node(('leftmost', 'lmost'), 40)
        self.plato_leftmost.description_tester = (lambda obj:
            not obj.spans_whole_string() and obj.is_leftmost_in_string())
        self.plato_rightmost = self.add_node(('rightmost', 'rmost'), 40)
        self.plato_rightmost.description_tester = (lambda obj:
            not obj.spans_whole_string() and obj.is_rightmost_in_string())
        self.plato_middle = self.add_node('middle', 40)
        self.plato_middle.description_tester = (lambda obj:
            obj.ungrouped_left_neighbor() and obj.ungrouped_right_neighbor() and
            obj.ungrouped_left_neighbor().is_leftmost_in_string() and
            obj.ungrouped_right_neighbor().is_rightmost_in_string())
        self.plato_single = self.add_node('single', 40)
        self.plato_single.description_tester = (lambda obj:
            obj.type_name == 'letter' and obj.spans_whole_string())
        self.plato_whole = self.add_node('whole', 40)
        self.plato_whole.description_tester = (lambda obj:
           obj.type_name == 'group' and obj.spans_whole_string())

        # Alphabetic position nodes
        self.plato_first = self.add_node('first', 60)
        self.plato_first.description_tester = (lambda obj:
            obj.get_descriptor(self.plato_letter_category) == self.slipnet_letters[0])
        self.plato_last = self.add_node('last', 60)
        self.plato_last.description_tester = (lambda obj:
            obj.get_descriptor(self.plato_letter_category) ==
            self.slipnet_letters[len(self.slipnet_letters) - 1])

        # Direction nodes
        self.plato_left = self.add_node('left', 40,
                                        ['BondTopDownDirectionScout',
                                         'GroupTopDownDirectionScout'])
        self.plato_right = self.add_node('right', 40,
                                         ['BondTopDownDirectionScout',
                                          'GroupTopDownDirectionScout'])

        # Bond nodes
        self.plato_predecessor = self.add_node(('predecessor', 'pred'), 50,
                                               ['BondTopDownCategoryScout'],
                                               60, directed=True)
        self.plato_successor = self.add_node(('successor', 'succ'), 50,
                                             ['BondTopDownCategoryScout'],
                                             60, directed=True)
        self.plato_sameness = self.add_node(('sameness', 'same'), 80,
                                            ['BondTopDownCategoryScout'], 0)

        # Group nodes
        self.plato_predecessor_group = self.add_node(('predecessor group',
                                                      'predgr'), 50,
                                                     ['GroupTopDownCategoryScout'],
                                                     directed=True)
        self.plato_predecessor_group.iterate_group = (lambda category:
            self.get_related_node(category, self.plato_predecessor))
        self.plato_successor_group = self.add_node(('succesor group',
                                                    'succgr'), 50,
                                                   ['GroupTopDownCategoryScout'],
                                                   directed=True)
        self.plato_successor_group.iterate_group = (lambda category:
            self.get_related_node(category, self.plato_successor))
        self.plato_sameness_group = self.add_node(('sameness group', 'samegr'),
                                                  80, ['GroupTopDownCategoryScout'])
        self.plato_sameness_group.iterate_group = lambda category: category

        # Other relation nodes
        self.plato_identity = self.add_node(('identity', 'ident'), 90, [], 0)
        self.plato_opposite = self.add_node(('opposite', 'oppos'), 90, [], 80)

        # Object nodes
        self.plato_letter = self.add_node('letter', 20)
        self.plato_letter.description_tester = (lambda obj:
            obj.type_name == 'letter')
        self.plato_group = self.add_node('group', 80)
        self.plato_group.description_tester = (lambda obj:
            obj.type_name == 'group')

        # Category nodes
        self.plato_letter_category = self.add_node(('letter category',
                                                    'ltr-c'), 30,
                                                   initially_clamped=True)
        self.plato_string_position_category = self.add_node(
            ('string category', 'str-c'), 70, ['DescriptionTopDownScout'],
            initially_clamped=True)
        self.plato_alphabetic_position_category = self.add_node(
            ('alphabetic position category',
             'alph-c'), 80, ['DescriptionTopDownScout'])
        self.plato_direction_category = self.add_node(('direction category',
                                                       'dir-c'), 70)
        self.plato_bond_category = self.add_node(('bond category', 'bnd-c'), 80)
        self.plato_group_category = self.add_node(('group cagegory',
                                                   'grp-c'), 80)
        self.plato_length = self.add_node('length', 60,
                                          ['DescriptionTopDownScout'])
        self.plato_object_category = self.add_node('obj-c', 90)
        self.plato_bond_facet = self.add_node(('bond facet', 'facet'), 90)

        # Letter links
        for i in range(25):
            before = self.slipnet_letters[i]
            after = self.slipnet_letters[i + 1]
            self.add_link('nonslip', before, after, self.plato_successor, None)
            self.add_link('nonslip', after, before, self.plato_predecessor, None)

        # Number links
        for i in range(4):
            before = self.slipnet_numbers[i]
            after = self.slipnet_numbers[i + 1]
            self.add_link('nonslip', before, after, self.plato_successor, None)
            self.add_link('nonslip', after, before, self.plato_predecessor, None)

        # Letter category links
        for i in range(26):
            letter = self.slipnet_letters[i]
            fixed_length = (self.plato_letter_category.conceptual_depth -
                            letter.conceptual_depth)
            self.add_link('category', letter, self.plato_letter_category,
                          None, fixed_length)
            self.add_link('instance', self.plato_letter_category, letter, None, 97)
        self.add_link('category', self.plato_sameness_group,
                      self.plato_letter_category, None, 50)

        # Length links
        for i in range(5):
            num = self.slipnet_numbers[i]
            fixed_length = self.plato_length.conceptual_depth - num.conceptual_depth
            self.add_link('category', num, self.plato_length, None, fixed_length)
            self.add_link('instance', self.plato_length, num, None, 100)
        self.add_link('nonslip', self.plato_predecessor_group,
                      self.plato_length, None, 95)
        self.add_link('nonslip', self.plato_successor_group,
                      self.plato_length, None, 95)
        self.add_link('nonslip', self.plato_sameness_group,
                      self.plato_length, None, 95)

        # Opposite links
        self.add_link('slip', self.plato_first, self.plato_last,
                      self.plato_opposite, None)
        self.add_link('slip', self.plato_last, self.plato_first,
                      self.plato_opposite, None)
        self.add_link('slip', self.plato_leftmost, self.plato_rightmost,
                      self.plato_opposite, None)
        self.add_link('slip', self.plato_rightmost, self.plato_leftmost,
                      self.plato_opposite, None)
        self.add_link('slip', self.plato_left, self.plato_right,
                      self.plato_opposite, None)
        self.add_link('slip', self.plato_right, self.plato_left,
                      self.plato_opposite, None)
        self.add_link('slip', self.plato_successor, self.plato_predecessor,
                      self.plato_opposite, None)
        self.add_link('slip', self.plato_predecessor, self.plato_successor,
                      self.plato_opposite, None)
        self.add_link('slip', self.plato_successor_group,
                      self.plato_predecessor_group, self.plato_opposite, None)
        self.add_link('slip', self.plato_predecessor_group,
                      self.plato_successor_group, self.plato_opposite, None)

        # Has property links
        self.add_link('property', self.slipnet_letters[0],
                      self.plato_first, None, 75)
        self.add_link('property', self.slipnet_letters[25],
                      self.plato_last, None, 75)

        # Object category links
        self.add_link('category', self.plato_letter,
                      self.plato_object_category, None,
                      (self.plato_object_category.conceptual_depth -
                       self.plato_letter.conceptual_depth))
        self.add_link('instance', self.plato_object_category,
                      self.plato_letter, None, 100)
        self.add_link('category', self.plato_group,
                      self.plato_object_category, None,
                      (self.plato_object_category.conceptual_depth -
                       self.plato_group.conceptual_depth))
        self.add_link('instance', self.plato_object_category,
                      self.plato_group, None, 100)

        # String position links
        self.add_link('category', self.plato_leftmost,
                      self.plato_string_position_category, None,
                      (self.plato_string_position_category.conceptual_depth -
                       self.plato_leftmost.conceptual_depth))
        self.add_link('instance', self.plato_string_position_category,
                      self.plato_leftmost, None, 100)
        self.add_link('category', self.plato_rightmost,
                      self.plato_string_position_category, None,
                      (self.plato_string_position_category.conceptual_depth -
                       self.plato_rightmost.conceptual_depth))
        self.add_link('instance', self.plato_string_position_category,
                      self.plato_rightmost, None, 100)
        self.add_link('category', self.plato_middle,
                      self.plato_string_position_category, None,
                      (self.plato_string_position_category.conceptual_depth -
                       self.plato_middle.conceptual_depth))
        self.add_link('instance', self.plato_string_position_category,
                      self.plato_middle, None, 100)
        self.add_link('category', self.plato_single,
                      self.plato_string_position_category, None,
                      (self.plato_string_position_category.conceptual_depth -
                       self.plato_single.conceptual_depth))
        self.add_link('instance', self.plato_string_position_category,
                      self.plato_single, None, 100)
        self.add_link('category', self.plato_whole,
                      self.plato_string_position_category, None,
                      (self.plato_string_position_category.conceptual_depth -
                       self.plato_whole.conceptual_depth))
        self.add_link('instance', self.plato_string_position_category,
                      self.plato_whole, None, 100)

        # Alphabetic position category links
        self.add_link('category', self.plato_first,
                      self.plato_alphabetic_position_category, None,
                      (self.plato_alphabetic_position_category.conceptual_depth -
                       self.plato_first.conceptual_depth))
        self.add_link('instance', self.plato_alphabetic_position_category,
                      self.plato_first, None, 100)
        self.add_link('category', self.plato_last,
                      self.plato_alphabetic_position_category, None,
                      (self.plato_alphabetic_position_category.conceptual_depth -
                       self.plato_last.conceptual_depth))
        self.add_link('instance', self.plato_alphabetic_position_category,
                      self.plato_last, None, 100)

        # Direction category links
        self.add_link('category', self.plato_left,
                      self.plato_direction_category, None,
                      (self.plato_direction_category.conceptual_depth -
                       self.plato_left.conceptual_depth))
        self.add_link('instance', self.plato_direction_category,
                      self.plato_left, None, 100)
        self.add_link('category', self.plato_right,
                      self.plato_direction_category, None,
                      (self.plato_direction_category.conceptual_depth -
                       self.plato_right.conceptual_depth))
        self.add_link('instance', self.plato_direction_category,
                      self.plato_right, None, 100)

        # Bond category links
        self.add_link('category', self.plato_predecessor,
                      self.plato_bond_category, None,
                      (self.plato_bond_category.conceptual_depth -
                       self.plato_predecessor.conceptual_depth))
        self.add_link('instance', self.plato_bond_category,
                      self.plato_predecessor, None, 100)
        self.add_link('category', self.plato_successor,
                      self.plato_bond_category, None,
                      (self.plato_bond_category.conceptual_depth -
                       self.plato_successor.conceptual_depth))
        self.add_link('instance', self.plato_bond_category,
                      self.plato_successor, None, 100)
        self.add_link('category', self.plato_sameness,
                      self.plato_bond_category, None,
                      (self.plato_bond_category.conceptual_depth -
                       self.plato_sameness.conceptual_depth))
        self.add_link('instance', self.plato_bond_category,
                      self.plato_sameness, None, 100)

        # Group category links
        self.add_link('category', self.plato_predecessor_group,
                      self.plato_group_category, None,
                      (self.plato_group_category.conceptual_depth -
                       self.plato_predecessor_group.conceptual_depth))
        self.add_link('instance', self.plato_group_category,
                      self.plato_predecessor_group, None, 100)
        self.add_link('category', self.plato_successor_group,
                      self.plato_group_category, None,
                      (self.plato_group_category.conceptual_depth -
                       self.plato_successor_group.conceptual_depth))
        self.add_link('instance', self.plato_group_category,
                      self.plato_successor_group, None, 100)
        self.add_link('category', self.plato_sameness_group,
                      self.plato_group_category, None,
                      (self.plato_group_category.conceptual_depth -
                       self.plato_sameness_group.conceptual_depth))
        self.add_link('instance', self.plato_group_category,
                      self.plato_sameness_group, None, 100)

        # Associated group links
        self.add_link('nonslip', self.plato_sameness, self.plato_sameness_group,
                      self.plato_group_category, 30)
        self.add_link('nonslip', self.plato_successor, self.plato_successor_group,
                      self.plato_group_category, 60)
        self.add_link('nonslip', self.plato_predecessor,
                      self.plato_predecessor_group, self.plato_group_category, 60)

        # Associated bond links
        self.add_link('nonslip', self.plato_sameness_group, self.plato_sameness,
                      self.plato_bond_category, 90)
        self.add_link('nonslip', self.plato_successor_group,
                      self.plato_successor, self.plato_bond_category, 90)
        self.add_link('nonslip', self.plato_predecessor_group,
                      self.plato_predecessor, self.plato_bond_category, 90)

        # Bond facet links
        self.add_link('category', self.plato_letter_category,
                      self.plato_bond_facet, None,
                      (self.plato_bond_facet.conceptual_depth -
                       self.plato_letter_category.conceptual_depth))
        self.add_link('instance', self.plato_bond_facet,
                      self.plato_letter_category, None, 100)
        self.add_link('category', self.plato_length,
                      self.plato_bond_facet, None,
                      (self.plato_bond_facet.conceptual_depth -
                       self.plato_length.conceptual_depth))
        self.add_link('instance', self.plato_bond_facet,
                      self.plato_length, None, 100)

        # Letter category links
        self.add_link('slip', self.plato_letter_category,
                      self.plato_length, None, 95)
        self.add_link('slip', self.plato_length,
                      self.plato_letter_category, None, 95)

        # Letter group links
        self.add_link('slip', self.plato_letter, self.plato_group, None, 90)
        self.add_link('slip', self.plato_group, self.plato_letter, None, 90)

        # Direction position, direction neighbor, position neighbor links
        self.add_link('nonslip', self.plato_left, self.plato_leftmost, None, 90)
        self.add_link('nonslip', self.plato_leftmost, self.plato_left, None, 90)
        self.add_link('nonslip', self.plato_right, self.plato_leftmost, None, 100)
        self.add_link('nonslip', self.plato_leftmost, self.plato_right, None, 100)
        self.add_link('nonslip', self.plato_right, self.plato_rightmost, None, 90)
        self.add_link('nonslip', self.plato_rightmost, self.plato_right, None, 90)
        self.add_link('nonslip', self.plato_left, self.plato_rightmost, None, 100)
        self.add_link('nonslip', self.plato_rightmost, self.plato_left, None, 100)
        self.add_link('nonslip', self.plato_leftmost, self.plato_first, None, 100)
        self.add_link('nonslip', self.plato_first, self.plato_leftmost, None, 100)
        self.add_link('nonslip', self.plato_rightmost, self.plato_first, None, 100)
        self.add_link('nonslip', self.plato_first, self.plato_rightmost, None, 100)
        self.add_link('nonslip', self.plato_leftmost, self.plato_last, None, 100)
        self.add_link('nonslip', self.plato_last, self.plato_leftmost, None, 100)
        self.add_link('nonslip', self.plato_rightmost, self.plato_last, None, 100)
        self.add_link('nonslip', self.plato_last, self.plato_rightmost, None, 100)

        # Other links
        self.add_link('slip', self.plato_single, self.plato_whole, None, 90)
        self.add_link('slip', self.plato_whole, self.plato_single, None, 90)

    def add_node(self, name, depth, codelets=[], intrinsic_link_length=None,
                 initially_clamped=False, directed=False):
        slipnode = Slipnode(name, depth, codelets, intrinsic_link_length,
                            initially_clamped, directed)
        self.slipnodes.append(slipnode)
        return slipnode

    def add_link(self, kind, from_node, to_node, label, fixed_length):
        sliplink = Sliplink(from_node, to_node, label, fixed_length)
        self.sliplinks.append(sliplink)
        to_node.incoming_links.append(sliplink)
        if kind == 'slip':
            from_node.lateral_slip_links.append(sliplink)
        elif kind == 'nonslip':
            from_node.lateral_nonslip_links.append(sliplink)
        elif kind == 'property':
            from_node.has_property_links.append(sliplink)
        elif kind == 'instance':
            from_node.instance_links.append(sliplink)
        elif kind == 'category':
            from_node.category_links.append(sliplink)


    def get_bond_category(self, from_node, to_node):
        """Return the node representing the label of the link between the nodes.

        There is either zero or one link between the two nodes."""
        if from_node == to_node:
            return self.plato_sameness
        else:
            for link in from_node.outgoing_links():
                if link.to_node == to_node:
                    return link.label

    def get_label_node(self, from_node, to_node):
        """Return the the label of the link between from_node and to_node."""
        if from_node == to_node:
            return self.plato_identity
        for link in from_node.outgoing_links():
            if link.to_node == to_node:
                return link.label

    def get_related_node(self, node, relation):
        """Return the node related to the given node by relation.

        For example, if given 'left' and 'opposite', returns 'right'."""
        if relation == self.plato_identity:
            return node
        for link in node.outgoing_links():
            if link.label == relation:
                return link.to_node

    def get_plato_letter(self, character):
        """Given a character, return the corresponding slipnet letter node."""
        for node in self.slipnet_letters:
            if node.name == str(character):
                return node

    def get_plato_number(self, number):
        """Given a numver, return the corresponding slipnet number node."""
        for node in self.slipnet_numbers:
            if node.name == str(number):
                return node

    def are_all_opposite_concept_mappings(self, concept_mappings):
        """Return True if all mappings in the list have the label 'opposite'."""
        for mapping in concept_mappings:
            if mapping.label != self.plato_opposite:
                return False
            return True

    def is_adjective(self, node):
        """Used for converting a rule to a sentence."""
        return node.category == self.plato_string_position_category or \
            node.category == self.plato_alphabetic_position_category

    def update(self):
        """Update activations and link lenths."""
        for node in self.slipnodes:
            node.decay()
            if node.is_active():
                for link in node.outgoing_links():
                    amount_to_spread = round(node.activation * \
                            (link.intrinsic_degree_of_association() / 100.0))
                    link.to_node.activation_buffer += amount_to_spread

        for node in self.slipnodes:
            node.activation = min(100, node.activation + node.activation_buffer)
            if node.clamp:
                node.activation = 100
            else:
                if node.activation >= 50:
                    full_activation_probability = (node.activation / 100.0) ** 3
                    if toolbox.flip_coin(full_activation_probability):
                        node.activation = 100
            node.activation_buffer = 0

    def clear(self):
        """Zero out the activations of all slipnodes."""
        for node in self.slipnodes:
            node.activation_buffer = 0
            node.activation = 0

    def clamp_initial_nodes(self):
        """Clamp those slipnodes that were marked to be initially clamped."""
        for node in self.slipnodes:
            if node.initially_clamped:
                node.clamp = True

    def unclamp_initial_nodes(self):
        """Unclamp those slipnodes that were marked to be initially clamped."""
        for node in self.slipnodes:
            if node.initially_clamped:
                node.clamp = False

    def top_down_codelets(self):
        """Return a list of codelets attached to active nodes."""
        codelets = []
        for node in self.slipnodes:
            if node.activation >= 50:
                for codelet in node.codelets:
                    codelets.append((codelet, [node], node.conceptual_depth / 100.))
        return codelets
