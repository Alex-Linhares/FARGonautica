"""Port of lib/SThought/Relations.pm: SThought::SRelation, focusing on an analogy (a relation).

Its actions: maybe check whether the two ends can be grouped, always try extending the
relation both ways, and, when objects lie between the ends, maybe build an ad hoc
Interlaced group (with random draws, which match Perl draw for draw).
"""
from seqsee import global_ as Global
from seqsee import sltm, sworkspace, util
from seqsee.categories.interlaced import Interlaced
from seqsee.constants import DIR
from seqsee.errors import Confess
from seqsee.objects.anchored import Anchored
from seqsee.scodelet import SCodelet
from seqsee.sthought import SThought


class SThoughtSRelation(SThought):
    """Perl: package SThought::SRelation (extends SThought)."""

    perl_name = "SThought::SRelation"
    NAME = "Focusing on an Analogy"

    def get_fringe(self):
        core = self.core()
        if not util.perl_true(core):
            raise Confess("Core is empty!")
        return [[core.get_type(), 100], [core.get_first(), 50], [core.get_second(), 50]]

    def get_actions(self):
        core = self.core()
        actions = []
        end1, end2 = core.get_ends()
        extent_left, extent_right = core.get_extent()
        relntype = core.get_type()
        sltm.spike_by(5, relntype)
        are_ends_contiguous = core.are_ends_contiguous()

        if are_ends_contiguous and relntype.is_effectively_a_sameness_relation():
            actions.append(SCodelet("AreTheseGroupable", 100,
                                    {"items": [end1, end2], "reln": core}))
        elif are_ends_contiguous and \
                not sworkspace.get_objects_with_ends_beyond(extent_left, extent_right):
            actions.append(SCodelet("AreTheseGroupable", 80,
                                    {"items": [end1, end2], "reln": core}))

        actions.append(SCodelet("AttemptExtensionOfRelation", 100,
                                {"core": core, "direction": DIR.RIGHT}))
        actions.append(SCodelet("AttemptExtensionOfRelation", 100,
                                {"core": core, "direction": DIR.LEFT}))

        ends = sworkspace.sort_l_to_r_by_left_edge(end1, end2)
        intervening_objects = sworkspace.get_intervening_objects(
            util.perl_num(ends[0].get_right_edge()) + 1,
            util.perl_num(ends[1].get_left_edge()) - 1)
        distance_magnitude = len(intervening_objects)
        if distance_magnitude:
            possible_ad_hoc_cat = Interlaced.create(distance_magnitude + 1)
            ad_hoc_activation = sltm.spike_by(20 / distance_magnitude, possible_ad_hoc_cat)
            if util.significant(ad_hoc_activation) and util.toss(ad_hoc_activation) \
                    and not util.perl_true(Global.Feature.get("NoInterlaced")):
                if util.toss(0.5):
                    new_object_parts = [ends[0], *intervening_objects]
                else:
                    new_object_parts = [*intervening_objects, ends[1]]
                if not sworkspace.get_objects_with_ends_exactly(
                        new_object_parts[0].get_left_edge(),
                        new_object_parts[-1].get_right_edge()):
                    new_obj = Anchored.create(*new_object_parts)
                    sworkspace.add_group(new_obj)
                    new_obj.describe_as(possible_ad_hoc_cat)
        return actions

    def as_text(self):
        return self.core().as_text()
