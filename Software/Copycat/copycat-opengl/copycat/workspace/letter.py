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

"""Letter"""

from copycat.workspace import Object

class Letter(Object):
    """Letter

    Attributes:
        type_name: Used as a hack so wobject knows which object it is.
        name: The character name of the letter.
        string: The string this letter is in.
        left_string_position: Position in the string.
        right_string_position: Position in the string."""

    def __init__(self, workspace, name, string, category, string_position):
        """Initialize Letter."""
        super(Letter, self).__init__(workspace)
        self.type_name = 'letter'
        self.name = name
        self.string = string
        self.category = category
        self.left_string_position = string_position
        self.right_string_position = string_position
        self.objects = []
