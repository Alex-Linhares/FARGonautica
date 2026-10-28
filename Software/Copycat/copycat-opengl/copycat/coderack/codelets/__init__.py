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

"""Codelets"""

from copycat.coderack.codelets.answer import AnswerBuilder
from copycat.coderack.codelets.bond import BondBottomUpScout
from copycat.coderack.codelets.bond import BondBuilder
from copycat.coderack.codelets.bond import BondStrengthTester
from copycat.coderack.codelets.bond import BondTopDownCategoryScout
from copycat.coderack.codelets.bond import BondTopDownDirectionScout
from copycat.coderack.codelets.breaker import Breaker
from copycat.coderack.codelets.correspondence import CorrespondenceBottomUpScout
from copycat.coderack.codelets.correspondence import CorrespondenceBuilder
from copycat.coderack.codelets.correspondence import CorrespondenceImportantObjectScout
from copycat.coderack.codelets.correspondence import CorrespondenceStrengthTester
from copycat.coderack.codelets.description import DescriptionBottomUpScout
from copycat.coderack.codelets.description import DescriptionBuilder
from copycat.coderack.codelets.description import DescriptionStrengthTester
from copycat.coderack.codelets.description import DescriptionTopDownScout
from copycat.coderack.codelets.group import GroupBuilder
from copycat.coderack.codelets.group import GroupStrengthTester
from copycat.coderack.codelets.group import GroupTopDownCategoryScout
from copycat.coderack.codelets.group import GroupTopDownDirectionScout
from copycat.coderack.codelets.group import GroupWholeStringScout
from copycat.coderack.codelets.replacement import ReplacementFinder
from copycat.coderack.codelets.rule import RuleBuilder
from copycat.coderack.codelets.rule import RuleScout
from copycat.coderack.codelets.rule import RuleStrengthTester
from copycat.coderack.codelets.rule import RuleTranslator
