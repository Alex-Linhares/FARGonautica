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

"""Replacement"""

from copycat.workspace import Structure

class Replacement(Structure):
    """Replacement

    Attributes:
        object1: The object to replace.
        object2: The object replaced with."""

    def __init__(self, object1, object2):
        """Initialize Replacement."""
        super(Replacement, self).__init__()
        self.object1 = object1
        self.object2 = object2
