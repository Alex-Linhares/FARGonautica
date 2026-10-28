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

"""Rule Codelets"""

import sys
import copycat.toolbox as toolbox
from copycat.coderack import Codelet
from copycat.workspace import Rule, ExtrinsicDescription

class RuleBuilder(Codelet):
    """Try to build the proposed rule, fighting with competitors as needed."""

    def run(self, coderack, slipnet, workspace):
        rule = self.arguments[0]

        if workspace.rule:
            if workspace.rule == rule:
                workspace.activate_from_workspace_rule_descriptions(rule)
                return # Fizzle
            if not workspace.fight_it_out(rule, 1, [workspace.rule], 1):
                return # Fizzle
            workspace.break_rule(workspace.rule)
        return workspace.build_rule(rule)

class RuleScout(Codelet):
    """Fills in the rule template, 'Replace _____ by _____'.

    To do this, it chooses descriptions of the changed object in the initial
    string and the object in the modified string that replaces it. If a rule
    can be made, it is proposed, and a rule strength tester codelet is posted
    with urgency a function of the degree of conceptual depth of the chosen
    descriptions."""

    def run(self, coderack, slipnet, workspace):
        if workspace.has_null_replacement():
            return # Fizzle

        changed_objects = []
        for obj in workspace.initial_string.get_objects():
            if obj.is_changed:
                changed_objects.append(obj)

        if len(changed_objects) > 1:
            print("Can't solve problems with more than one letter changed.")
            # FIXME: Too harsh, especially for graphical clients.
            sys.exit()

        if not changed_objects:
            return workspace.propose_rule(None, None, None, None)

        i_object = changed_objects[0]
        m_object = i_object.replacement.object2

        if not i_object.correspondence:
            i_descriptions = i_object.rule_initial_string_descriptions()
        else:
            correspondence_slippages = i_object.correspondence.slippages()
            i_descriptions = []
            for description in i_object.rule_initial_string_descriptions():
                applied = description.apply_slippages(i_object,
                                                      correspondence_slippages)
                relevant = i_object.correspondence.object2.relevant_descriptions()
                if applied in relevant:
                    i_descriptions.append(description)
        if not i_descriptions:
            return # Fizzle

        depths = [d.conceptual_depth() for d in i_descriptions]
        i_probabilities = workspace.temperature_adjusted_values(depths)
        i_description = toolbox.weighted_select(i_probabilities, i_descriptions)

        m_descriptions = m_object.extrinsic_descriptions + \
                         m_object.rule_modified_string_descriptions()
        if not m_descriptions:
            return # Fizzle

        depths = [d.conceptual_depth() for d in m_descriptions]
        m_probabilities = workspace.temperature_adjusted_values(depths)
        m_description = toolbox.weighted_select(m_probabilities, m_descriptions)

        if isinstance(m_description, ExtrinsicDescription):
            related_descriptor = slipnet.get_related_node(i_description.descriptor,
                                                          m_description.relation)
            if related_descriptor:
                for description in m_object.descriptions:
                    if description.descriptor == related_descriptor:
                        m_description = description
                        break

        return workspace.propose_rule(i_object, i_description,
                                      m_object, m_description)

class RuleStrengthTester(Codelet):
    """Calculate the proposed rule's strength and probabilistically decides
    whether or not to post a rule builder codelet with urgency a fucntion
    for its strength."""

    def run(self, coderack, slipnet, workspace):
        rule = self.arguments[0]

        rule.update_strengths()
        strength = rule.total_strength

        probability = strength / 100.0
        probability = workspace.temperature_adjusted_probability(probability)
        if not toolbox.flip_coin(probability):
            return # fizzle
        return [(RuleBuilder([rule]), strength)]

class RuleTranslator(Codelet):
    """Translate the rule according to the translation rules given in the
    slippages on the workspace."""

    def run(self, coderack, slipnet, workspace):
        workspace_rule = workspace.rule
        if not workspace_rule:
            return # Fizzle
        if workspace_rule.has_no_change():
            workspace.translated_rule = Rule(workspace, None, None, None,
                                             None, None, None)
            return # Fizzle

        threshold = workspace.answer_temperature_threshold_distribution().choose()
        if workspace.temperature > threshold:
            return # Fizzle

        changed_object = None
        for obj in workspace.initial_string.get_objects():
            if obj.is_changed:
                changed_object = obj
                break
        if not changed_object:
            return # Fizzle

        changed_object_correspondence = changed_object.correspondence

        slippages = workspace.slippages()
        if changed_object_correspondence:
            for slippage in workspace.slippages():
                for mapping in changed_object_correspondence.get_concept_mappings():
                    if mapping.are_contradictory_concept_mappings(slippage):
                        slippages.remove(slippage)

        rule = workspace_rule
        if rule.expresses_relation():
            args = []
            for arg in [rule.object_category1, rule.descriptor1_facet,
                        rule.descriptor1, rule.object_category2,
                        rule.replaced_description_type]:
                args.append(arg.apply_slippages(slippages))
            args.append(None)
            translated_rule = Rule(workspace, *args)
            translated_rule.relation = rule.relation.apply_slippages(slippages)
        else:
            args = []
            for arg in [rule.object_category1, rule.descriptor1_facet,
                        rule.descriptor1, rule.object_category2,
                        rule.replaced_description_type, rule.descriptor2]:
                args.append(arg.apply_slippages(slippages))
            translated_rule = Rule(workspace, *args)

        return workspace.build_translated_rule(translated_rule)
