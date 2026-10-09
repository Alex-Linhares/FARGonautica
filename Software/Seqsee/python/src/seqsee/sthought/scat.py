"""Port of lib/SThought/SCat.pm: SThought::SCat, focusing on a category.

Its actions suggest merging groups of the category that share a subgroup.
"""
from seqsee import sworkspace
from seqsee.categories.interlaced import Interlaced
from seqsee.scodelet import SCodelet
from seqsee.sthought import SThought


class SThoughtSCat(SThought):
    """Perl: package SThought::SCat (extends SThought)."""

    perl_name = "SThought::SCat"
    NAME = "Focusing on a Category"

    def get_fringe(self):
        return [[self.core(), 100]]

    def get_actions(self):
        """One MergeGroups codelet (urgency 100, a/b = the first two objects) per set of
        objects of the category that share a subgroup. Nothing for Interlaced categories.
        The sets and their order follow Perl hash order in Perl, insertion order here."""
        cat = self.core()
        if isinstance(cat, Interlaced):
            return []
        objects_of_cat = sworkspace.get_objects_belonging_to_category(cat)
        overlapping_sets = sworkspace.find_sets_of_objects_with_overlapping_subgroups(
            *objects_of_cat)
        return [SCodelet("MergeGroups", 100, {"a": s[0], "b": s[1]}) for s in overlapping_sets]

    def as_text(self):
        return "Category " + self.core().as_text()
