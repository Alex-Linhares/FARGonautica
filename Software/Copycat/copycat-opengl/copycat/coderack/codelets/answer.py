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

"""Answer Codelet"""

from copycat.coderack import Codelet
from copycat.workspace import String

class AnswerBuilder(Codelet):
    def run(self, coderack, slipnet, workspace):
        workspace.answer_string = String(workspace, "")

        workspace.changed_length_group = None
        workspace.snag_object = None

        if workspace.translated_rule.has_no_change():
            objects_to_change = []
        else:
            objects_to_change = workspace.get_objects_to_change_for_answer()

        desc_type = workspace.translated_rule.replaced_description_type

        answer_string_letters = []
        for obj in workspace.target_string.get_objects():
            if obj in objects_to_change:
                letters = workspace.get_modified_letters_for_answer(obj, desc_type)
                answer_string_letters.extend(letters)

        if workspace.snag_object:
            workspace.snag_count += 1
            workspace.last_snag_time = coderack.time
            workspace.snag_structures = workspace.structures()

            for bond in workspace.proposed_bonds():
                bond.string.remove_proposed_bond(bond)
            for group in workspace.proposed_groups():
                group.string.remove_proposed_group(group)
            for correspondence in workspace.get_proposed_correspondences():
                workspace.remove_proposed_correspondence(correspondence)

            workspace.translated_rule = None
            workspace.answer_string = None
            workspace.snag_condition = True
            workspace.temperature = 100
            workspace.clamp_temperature = True
            for description in workspace.snag_object.descriptions:
                description.descriptor.clamp = True
            workspace.snag_object.clamp_salience = True

            coderack.clear()

            return workspace.initial_codelets()

        letters = workspace.get_unmodified_letters_for_answer(objects_to_change)
        answer_string_letters.extend(letters)
        if workspace.changed_length_group:
            for letter in answer_string_letters:
                group_position = workspace.changed_length_group.right_string_position
                if all([letter not in workspace.modified_letters,
                        letter.left_string_position > group_position]):
                    letter.left_string_position += workspace.amount_length_changed
                    letter.right_string_position = letter.left_string_position + \
                            workspace.amount_length_changed

        workspace.answer_string.length = len(answer_string_letters)
        for letter in answer_string_letters:
            workspace.answer_string.add_letter(letter)
        for letter in workspace.answer_string.get_letters():
            workspace.answer_string.name += letter.name
