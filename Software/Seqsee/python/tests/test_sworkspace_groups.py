"""Tests for SWorkspace.pm, part II (item 032): add_group/remove_gp, conflicts, group
bookkeeping, supergroups and liveness.

Mirrors lib/SWorkspace.pm (GetGroups, AreThereAnySuperSuperGroups,
__CheckLivenessAndDiagnose, __GrepLiveness, __GetObjectsWithEndsExactly/Beyond/NotBeyond,
__GetExactObjectIfPresent, __GetGroupsThatPartiallyOverlap, the four __Sort* subs,
__DeleteGroup, __CheckTwoGroupsForConflict, __GroupAddSanityCheck,
__DoGroupAddBookkeeping, __UpdateGroup, __RemoveFromSupergroups_of,
__FindObjectSetDirection, __AreThereHolesOrOverlap, __FindGroupsConflictingWith,
__AddGroup, add_group, remove_gp, AreGroupsInConflict, FindGroupsConflictingWith,
__FindSetsOfObjectsWithOverlappingSubgroups, __RemoveGroupsCrossingBarLines,
FightUntoDeath, AddRelation, RemoveRelation). Golden data: oracle/sworkspace_groups.pl,
whose scenarios (lists of ops) are replayed here op for op.
"""
import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sltm, sworkspace, util
from seqsee.constants import DIR
from seqsee.errors import Confess, ExceptionClassBase
from seqsee.mapping.numeric import MappingNumeric
from seqsee.objects import anchored as anchored_mod
from seqsee.objects import result_of_get_conflicts as conflicts_mod
from seqsee.objects.anchored import Anchored
from seqsee.srelation import SRelation
from seqsee import srelation as srelation_mod

CASES = golden.load("sworkspace_groups")


def err(e):
    if isinstance(e, ExceptionClassBase):
        return {"class": e.perl_name, "message": e.message or ""}
    return str(e)


def same_error(got, want):
    """A failed Smart::Comments ``### require:`` dies "\\n" in Perl (after printing the
    assertion); the port raises Confess("require: …")."""
    if want == "\n":
        return isinstance(got, str) and got.startswith("require:")
    return got == want


def same(got, want):
    """Equality, with floats compared approximately (JSON::PP prints 15 digits)."""
    if isinstance(want, float) or (isinstance(got, float) and isinstance(want, int)):
        return got == pytest.approx(want)
    if isinstance(want, list):
        return isinstance(got, list) and len(got) == len(want) and \
            all(same(g, w) for g, w in zip(got, want))
    if isinstance(want, dict):
        return isinstance(got, dict) and got.keys() == want.keys() and \
            all(same(got[k], want[k]) for k in want)
    return got == want


class Replay:
    """The oracle's op interpreter."""

    def __init__(self):
        self.obj = {}
        self.name_of = {}

    def reg(self, name, o):
        self.obj[name] = o
        self.name_of[id(o)] = name

    def nm(self, o):
        if o is None:
            return None
        if isinstance(o, str) and o == "":
            return ""
        if id(o) in self.name_of:
            return self.name_of[id(o)]
        return "?" + o.as_text()

    def names(self, objs):
        return sorted(self.nm(o) for o in objs)

    def O(self, names):
        return [self.obj.get(n) for n in names]

    def state(self):
        ws = sworkspace
        live = sorted(ws.OBJECTS.values(), key=self.nm)
        return {
            "live": [
                [self.nm(o), ws.LEFT_EDGE_OF.get(o), ws.RIGHT_EDGE_OF.get(o), ws.SPAN_OF.get(o),
                 self.names(ws.SUPER_GROUPS_OF[o].values()) if o in ws.SUPER_GROUPS_OF else None,
                 1 if o in ws.NON_ELT_OBJECTS else 0]
                for o in live
            ],
            "counts": [len(h) for h in (ws.OBJECTS, ws.NON_ELT_OBJECTS, ws.LEFT_EDGE_OF,
                                        ws.RIGHT_EDGE_OF, ws.SPAN_OF, ws.SUPER_GROUPS_OF)],
            "live_once": self.names(o for o in self.obj.values() if o in ws.LIVE_AT_SOME_POINT),
            "relations": self.names(ws.relations.values()),
        }

    @staticmethod
    def _add_result(r):
        return [[] if r is None else [r], Global.TimeOfNewStructure]

    def run(self, kind, args):
        ws = sworkspace
        if kind == "init":
            sltm.clear()
            ws.init({"seq": args[0]})
            ws.clear_bar_lines()
            Global.Feature.clear()
            Global.Steps_Finished = 0
            self.obj.clear()
            self.name_of.clear()
            es = ws.get_elements()
            for i, e in enumerate(es):
                self.reg(f"e{i}", e)
            return len(es)
        if kind == "srand":
            util.srand(args[0])
            return None
        if kind == "feature":
            Global.Feature[args[0]] = args[1]
            return None
        if kind == "steps":
            Global.Steps_Finished = args[0]
            return None
        if kind == "gp":
            g = Anchored.create(*self.O(args[1:]))
            self.reg(args[0], g)
            return [g.as_text(), g.get_strength()]
        if kind == "reln":
            name, f, s, typ = args
            r = SRelation({"first": self.obj[f], "second": self.obj[s],
                           "type": MappingNumeric.create(typ, S.NUMBER)})
            self.reg(name, r)
            return None
        if kind == "insert_rel":
            return self.obj[args[0]].insert()
        if kind == "add_rel":
            r = ws.add_relation(self.obj[args[0]])
            return [] if r is None else [self.nm(r)]
        if kind == "remove_rel":
            ws.remove_relation(self.obj[args[0]])
            return None
        if kind == "has_rel":
            return 1 if util.perl_true(self.obj[args[0]].get_relation(self.obj[args[1]])) else 0
        if kind == "rel_ends":
            return len(ws.relations_by_ends)
        if kind in ("add", "add_internal"):
            Global.TimeOfNewStructure = -1
            f = ws.add_group if kind == "add" else ws._add_group
            return self._add_result(f(self.obj[args[0]]))
        if kind == "remove":
            ws.remove_gp(self.obj[args[0]])
            return None
        if kind == "delete":
            ws.delete_group(self.obj[args[0]])
            return None
        if kind == "live":
            return 1 if ws.check_liveness(*self.O(args)) else 0
        if kind == "live_once":
            return 1 if ws.check_liveness_at_some_point(*self.O(args)) else 0
        if kind == "grep_live":
            return [self.nm(o) for o in ws.grep_liveness(*self.O(args))]
        if kind == "diagnose":
            return ws.check_liveness_and_diagnose(*self.O(args))
        if kind == "exactly":
            return self.names(ws.get_objects_with_ends_exactly(*args))
        if kind == "beyond":
            return self.names(ws.get_objects_with_ends_beyond(*args))
        if kind == "notbeyond":
            return self.names(ws.get_objects_with_ends_not_beyond(*args))
        if kind == "exact_obj":
            return self.nm(ws.get_exact_object_if_present(self.obj[args[0]]))
        if kind == "partial":
            return self.names(ws.get_groups_that_partially_overlap(self.obj[args[0]]))
        sorts = {"sort_lr_left": ws.sort_l_to_r_by_left_edge,
                 "sort_rl_left": ws.sort_r_to_l_by_left_edge,
                 "sort_lr_right": ws.sort_l_to_r_by_right_edge,
                 "sort_rl_right": ws.sort_r_to_l_by_right_edge}
        if kind in sorts:
            return [self.nm(o) for o in sorts[kind](*self.O(args))]
        if kind == "conflict2":
            return ws.check_two_groups_for_conflict(*self.O(args))
        if kind == "in_conflict":
            return [ws.are_groups_in_conflict(*self.O(args))]
        if kind == "find_conflicts":
            c = ws.find_groups_conflicting_with(self.obj[args[0]])
            return {"challenger": self.nm(c.challenger()), "exact": self.nm(c.exact_conflict()),
                    "overlapping": self.names(c.overlapping_conflicts()),
                    "bool": 1 if c else 0}
        if kind == "find_conflicts_list":
            exact, *rest = ws.find_groups_conflicting_with_as_list(self.obj[args[0]])
            return [self.nm(exact), self.names(rest)]
        if kind == "direction":
            d = ws.find_object_set_direction(*self.O(args))
            return {DIR.LEFT: "LEFT", DIR.RIGHT: "RIGHT", DIR.UNKNOWN: "UNKNOWN",
                    DIR.NEITHER: "NEITHER"}[d]
        if kind == "holes":
            return ws.are_there_holes_or_overlap(*self.O(args))
        if kind == "sanity":
            return ws.group_add_sanity_check(*self.O(args))
        if kind == "bookkeep":
            ws.do_group_add_bookkeeping(self.obj[args[0]])
            return None
        if kind == "update":
            ws.update_group(self.obj[args[0]])
            return None
        if kind == "rm_super":
            ws.remove_from_supergroups_of(*self.O(args))
            return None
        if kind == "supergroups":
            return self.names(ws.get_super_groups(self.obj[args[0]]))
        if kind == "supersuper":
            return ws.are_there_any_super_super_groups(self.obj[args[0]])
        if kind == "groups":
            g = ws.get_groups()
            return [[ws.SPAN_OF[x] for x in g], self.names(g)]
        if kind == "overlapping_sets":
            sets = [self.names(s) for s in
                    ws.find_sets_of_objects_with_overlapping_subgroups(*self.O(args))]
            return sorted(sets, key=lambda s: ",".join(s))
        if kind == "barlines":
            ws.add_bar_lines(*args)
            return None
        if kind == "remove_crossing":
            ws.remove_groups_crossing_bar_lines()
            return None
        if kind == "fight":
            return ws.fight_unto_death({"challenger": self.obj[args[0]],
                                        "incumbent": self.obj[args[1]]})
        if kind == "lock":
            self.obj[args[0]].set_is_locked_against_deletion(1)
            return None
        if kind == "set_strength":
            self.obj[args[0]].set_strength(args[1])
            return None
        if kind == "state":
            return self.state()
        raise AssertionError(f"unknown op {kind}")


@pytest.mark.parametrize("case", CASES, ids=[c["scenario"] for c in CASES])
def test_golden_scenario(case):
    r = Replay()
    for i, (op, want) in enumerate(zip(case["ops"], case["results"])):
        kind, args = op[0], op[1:]
        try:
            got = {"value": r.run(kind, args)}
        except (Confess, ExceptionClassBase) as e:
            got = {"error": err(e)}
        where = f"op {i}: {op}"
        if "error" in want:
            assert "error" in got, f"{where}: expected error {want['error']!r}, got {got}"
            assert same_error(got["error"], want["error"]), where
        else:
            assert "value" in got, f"{where}: unexpected error {got.get('error')!r}"
            assert same(got["value"], want["value"]), \
                f"{where}: {got['value']!r} != {want['value']!r}"


# --- tests from reading the source --------------------------------------------------------


def _ws(seq):
    sworkspace.init({"seq": seq})
    return sworkspace.get_elements()


def test_add_group_sets_time_of_new_structure():
    e = _ws([1, 2, 3])
    Global.Steps_Finished = 42
    g = Anchored.create(e[0], e[1])
    assert sworkspace.add_group(g) == 1
    assert Global.TimeOfNewStructure == 42
    assert sworkspace.get_groups() == [g]


def test_update_group_vivifies_dead_parts():
    """PERL-QUIRK: ``@LeftEdge_of{@parts}`` passed to List::Util::min vivifies the dead
    parts' edge entries (as undef)."""
    e = _ws([1, 2, 3, 4])
    a = Anchored.create(e[0], e[1])
    b = Anchored.create(e[2], e[3])
    sworkspace.add_group(a)
    sworkspace.add_group(b)
    c = Anchored.create(a, b)
    sworkspace.remove_gp(a)
    sworkspace.update_group(c)
    assert a in sworkspace.LEFT_EDGE_OF and sworkspace.LEFT_EDGE_OF[a] is None
    assert a in sworkspace.RIGHT_EDGE_OF and a not in sworkspace.SPAN_OF
    assert sworkspace.LEFT_EDGE_OF[c] is None   # min(undef, 2): undef counts as 0
    assert sworkspace.RIGHT_EDGE_OF[c] == 3
    assert sworkspace.SPAN_OF[c] == 4


def test_check_liveness_and_diagnose_metonym_and_undef():
    e = _ws([1, 2])
    with pytest.raises(Confess, match='Can\'t call method "as_text" on an undefined value'):
        sworkspace.check_liveness_and_diagnose(None)

    class Starred:
        def as_text(self):
            return "STAR"

        def get_concrete_object(self):
            return e[0]

    with pytest.raises(Confess) as info:
        sworkspace.check_liveness_and_diagnose(Starred())
    assert str(info.value) == ("Dying because of liveness issues!\nNON_LIVE OBJECT: >>STAR<<\n"
                               "A METONYM IS BEING CHECKED FOR LIVENESS!\n"
                               "\tIts unstarred *is* live.\n")


def test_hooks_point_at_the_workspace():
    e = _ws([1, 2, 3])
    g = Anchored.create(e[0], e[1])
    assert anchored_mod._find_groups_conflicting_with(g).challenger() is g
    sworkspace.add_group(g)
    assert anchored_mod._get_super_groups(e[0]) == [g]
    assert conflicts_mod._check_liveness(g) is True
    anchored_mod._remove_gp(g)
    assert not sworkspace.check_liveness(g)
    sworkspace.add_group(g)
    anchored_mod._delete_group(g)
    assert not sworkspace.check_liveness(g)
    sworkspace.do_group_add_bookkeeping(g)
    assert conflicts_mod._fight_unto_death({"challenger": e[2], "incumbent": g}) in (0, 1)
    r = SRelation({"first": e[0], "second": e[1],
                   "type": MappingNumeric.create("succ", S.NUMBER)})
    assert srelation_mod._workspace_add_relation(r) is r
    srelation_mod._workspace_remove_relation(r)
    assert not sworkspace.relations


def test_add_relation_refuses_metonymed_ends():
    e = _ws([1, 2])
    r = SRelation({"first": e[0], "second": e[1],
                   "type": MappingNumeric.create("succ", S.NUMBER)})
    e[0].is_this_a_metonymed_object = lambda: 1
    with pytest.raises(Confess, match="Metonym'd end of relation"):
        sworkspace.add_relation(r)


def test_check_two_groups_should_never_reach_here():
    """If no piece of the bigger group reaches the smaller one's left edge, Perl confesses."""
    e = _ws([1, 2, 3, 4])
    big = Anchored.create(e[0], e[1], e[2])
    sworkspace.add_group(big)
    small = Anchored.create(e[1], e[2])
    sworkspace.RIGHT_EDGE_OF[e[1]] = sworkspace.RIGHT_EDGE_OF[e[2]] = -5
    sworkspace.RIGHT_EDGE_OF[e[0]] = -5
    with pytest.raises(Confess, match="Should never reach here."):
        sworkspace.check_two_groups_for_conflict(small, big)
