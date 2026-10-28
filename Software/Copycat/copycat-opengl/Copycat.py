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

"""Copycat creates a run with a supplied problem and displays the result."""

import argparse
import sys
from copycat.run import Run
sys.path.insert(0, "lib")

def main():
    """Prompt for default strings to supply to the run."""
    parser = argparse.ArgumentParser()
    parser.add_argument("initial", metavar="INITIAL")
    parser.add_argument("modified", metavar="MODIFIED")
    parser.add_argument("target", metavar="TARGET")
    parser.add_argument("-s", "--seed", dest="seed", default=None, type=int)
    parser.add_argument("-q", "--quiet",
                        action="store_true", dest="quiet", default=False,
                        help="run in headless mode")
    args = parser.parse_args()

    if args.quiet:
        run = Run(args.initial, args.modified, args.target, args.seed)
        while not run.workspace.answer_string:
            run.step()
        print(run.workspace.rule.to_string())
        print("Answer: " + run.workspace.answer_string.name)
        print("Temperature: " + str(run.workspace.temperature))
        print("Steps: " + str(run.coderack.time))
    else:
        from clients import OpenglClient
        OpenglClient(args.initial, args.modified, args.target, args.seed)

if __name__ == "__main__":
    main()
