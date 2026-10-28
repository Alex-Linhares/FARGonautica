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

"""Description Codelets"""

from copycat.coderack import Codelet
import copycat.toolbox as toolbox

class DescriptionBottomUpScout(Codelet):
    """Choose an object probabilistically by total salience and chooses a
    relevant description of the object probabilistically by activation.
    If the description has any "has property" links that are short enough,
    chooses one of the properties probabilistically based on degree of
    association and activation. Then proposes a description based on the
    property and posts a description strength tester codelet with urgency
    a function of the activation of the property."""

    structure_category = 'description'

    def run(self, coderack, slipnet, workspace):
        obj = workspace.choose_object('total_salience')

        description = obj.choose_relevant_description_by_activation()
        if description is None:
            return # Fizzle
        descriptor = description.descriptor

        links = descriptor.similar_has_property_links()
        if links == []:
            return # Fizzle

        associations = [link.degree_of_association() for link in links]
        activations = [link.to_node.activation for link in links]
        # PY3-FIX: map() is lazy in Python 3; weighted_select sums the weights and then
        # iterates them again, so a map object made it return None (see PORTING_NOTES.md).
        choices = list(map(lambda a, b: a * b, associations, activations))
        prop = toolbox.weighted_select(choices, links).to_node

        return workspace.propose_description(obj, prop.category(), prop)

class DescriptionBuilder(Codelet):
    """Attempt to build the proposed description. If it already exists, its
    activations are boosted."""

    structure_category = 'description'

    def run(self, coderack, slipnet, workspace):
        description = self.arguments[0]

        if description.object not in workspace.objects():
            return # Fizzle

        if description in description.object.descriptions:
            description.description_type.activation_buffer += workspace.activation
            description.descriptor.activation_buffer += workspace.activation
            return # Fizzle

        workspace.build_description(description)

class DescriptionStrengthTester(Codelet):
    """Calculate the proposed descriptions's strength and probabilistically
    decides whether or not to post a description builder codelet with urgency
    as a function of the strength."""

    structure_category = 'description'

    def run(self, coderack, slipnet, workspace):
        description = self.arguments[0]

        description.descriptor.activation_buffer += workspace.activation

        description.update_strengths()
        strength = description.total_strength

        probability = strength / 100.0
        probability = workspace.temperature_adjusted_probability(probability)
        if not toolbox.flip_coin(probability):
            return # Fizzle

        return [(DescriptionBuilder([description]), strength)]

class DescriptionTopDownScout(Codelet):
    """Choose an object probabilistically by total salience, checking if it
    fits any of the descriptions in the description_type's "has instance"
    list. If so, proposes a description based on the property and posts a
    description strength tester codelet with urgency a funtion of the
    activation of the proposed descriptor."""

    structure_category = 'description'

    def run(self, coderack, slipnet, workspace):
        description_type = self.arguments[0]

        obj = workspace.choose_object('total_salience')

        descriptors = description_type.get_possible_descriptors(obj)
        if descriptors == []:
            return # Fizzle

        activations = [descriptor.activation for descriptor in descriptors]
        descriptor = toolbox.weighted_select(activations, descriptors)

        return workspace.propose_description(obj, description_type, descriptor)
