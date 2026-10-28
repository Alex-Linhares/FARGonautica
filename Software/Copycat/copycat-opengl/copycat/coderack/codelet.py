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

"""Codelet"""

class Codelet(object):
    """Codelet is a small amount of code that has a chance to be run.

    Attributes:
        arguments: A tuple of arguments that the codelet can affect.
        timestamp: The time when the codelet was created.
        bin: The coderack bin the codelet is stored in."""

    def __init__(self, arguments=()):
        """Initialize Codelet."""
        self.arguments = arguments
        self.timestamp = None
        self.bin = None

    def run(self, coderack, slipnet, workspace):
        """Run the codelet.

        Each specific codelet must implement this for itself."""
        pass
