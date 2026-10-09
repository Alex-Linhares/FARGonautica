"""Tests for the first half of the general codelet families (item 040).

Mirrors lib/Seqsee/SCF_MX/General.pm: the families LookForSimilarGroups, MergeGroups,
CleanUpGroup, DoTheSameThing and CreateGroup (packages Seqsee::SCF::<Name>), run through
their installed ``run`` (MooseX/SCF.pm). Golden data: oracle/scf_general1.pl, whose
scenarios are replayed here op for op.
"""
import pytest

import golden
from test_sthought import err
from test_sthought_sobject import Replay as _BaseReplay
from test_sthought_sobject import _norm, cat, mapping
from test_sworkspace_groups import same
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scoderack, sltm, sworkspace, util
from seqsee.categories.mapping_based import MappingBased
from seqsee.codelets import general
from seqsee.codelets.family import FAMILIES, family_run
from seqsee.constants import DIR
from seqsee.errors import Confess, ExceptionClassBase
from seqsee.mapping import find_mapping
from seqsee.objects.anchored import Anchored
from seqsee.srelation import SRelation

CASES = golden.load("scf_general1")


class Replay(_BaseReplay):
    """The oracle's op interpreter."""

    def argval(self, spec):
        kind, v = spec[0], spec[1:]
        if kind == "obj":
            return self.obj.get(v[0])  # an unknown name is undef, as in the oracle
        if kind == "list":
            return self.O(v)
        if kind == "cat":
            return cat(v[0])
        if kind == "map":
            return mapping(*v)
        if kind == "type":
            return self.obj[v[0]].get_type()
        if kind == "dir":
            return DIR.LEFT if v[0] == "LEFT" else DIR.RIGHT
        if kind == "val":
            return v[0]
        if kind == "undef":
            return None
        if kind == "mbcat":
            return MappingBased.create(self.obj[v[0]].get_type())
        raise AssertionError(kind)

    def state(self):
        ws = sworkspace
        out = []
        for o in sorted(ws.OBJECTS.values(), key=self.nm):
            ul = o.get_underlying_reln()
            out.append([self.nm(o), ws.LEFT_EDGE_OF.get(o), ws.RIGHT_EDGE_OF.get(o),
                        sorted(c.as_text() for c in o.get_categories()),
                        ul.get_rule().get_transform().as_text() if util.perl_true(ul) else None])
        return out

    def coderack(self):
        out = []
        for cl in scoderack.CODELETS:
            c = self.codelet(cl)
            args = [f"{k}={'+'.join(v) if isinstance(v, list) else v}" for k, v in c[3]]
            out.append([c[1], c[2], *args])
        return sorted(out, key=lambda c: ",".join(util.perl_str(x) for x in c))

    def run(self, kind, args):
        ws = sworkspace
        if kind == "init":
            scoderack.clear()
            return super().run(kind, args)
        if kind == "relnf":
            name, a, b = args
            for c in list(self.obj[a].get_categories()):
                sltm.spike_by(100, c)
            util.srand(1)
            return super().run(kind, args)
        if kind == "remove":
            ws.remove_gp(self.obj[args[0]])
            return None
        if kind == "is_a_metonym":
            self.obj[args[0]].set_is_a_metonym(self.obj[args[1]])
            return None
        if kind == "relations":
            return sorted(" ".join([self.nm(r.get_first()), self.nm(r.get_second()),
                                    r.get_type().as_text()]) for r in ws.relations.values())
        if kind == "coderack":
            return self.coderack()
        if kind == "run":
            family, pairs = args[0], args[1:]
            family_run(family, None, {k: self.argval(spec) for k, spec in pairs})
            return None
        if kind == "name_new":
            new = sorted((o for o in ws.OBJECTS.values() if id(o) not in self.name_of),
                         key=lambda o: (ws.LEFT_EDGE_OF[o], ws.RIGHT_EDGE_OF[o]))
            for i, o in enumerate(new):
                self.reg(f"{args[0]}{i}", o)
            return [o.as_text() for o in new]
        return super().run(kind, args)


def _same_error(got, want):
    if isinstance(want, str):
        want = want.rstrip("\n")
    return got == want


@pytest.mark.parametrize("case", CASES, ids=[c["scenario"] for c in CASES])
def test_golden_scenario(case):
    replay = Replay()
    for op, want in zip(case["ops"], case["results"]):
        kind, args = op[0], op[1:]
        try:
            got = {"value": replay.run(kind, args)}
        except (Confess, ExceptionClassBase) as e:
            got = {"error": err(e)}
        if "error" in want:
            assert "error" in got, (op, got, want)
            assert _same_error(got["error"], want["error"]), (op, got, want)
        else:
            assert "value" in got, (op, got, want)
            # Mapping::Structural as_text lists its slippages in Perl hash order.
            assert same(_norm(got["value"]), _norm(want["value"])), (op, got, want)


# --- tests from reading the source ------------------------------------------------------

def _groups(seq, *spans):
    sworkspace.init({"seq": seq})
    e = sworkspace.get_elements()
    out = []
    for lo, hi in spans:
        g = Anchored.create(*e[lo:hi + 1])
        sworkspace.add_group(g)
        out.append(g)
    return e, out


def test_families_are_registered():
    for name in ("LookForSimilarGroups", "MergeGroups", "CleanUpGroup", "DoTheSameThing",
                 "CreateGroup"):
        assert FAMILIES[name].called == f"Seqsee::SCF::{name}::run"
    # General.pm's attribute specs: DoTheSameThing's group/category/direction default to 0.
    assert dict(FAMILIES["DoTheSameThing"].attributes)["direction"] == {"default": 0}


def test_family_run_loads_the_family_modules():
    # Perl's `use S` loads Seqsee/SCF_MX/*.pm; family_run imports them on a miss.
    import subprocess
    import sys
    code = ("from seqsee.codelets.family import FAMILIES, family_run\n"
            "assert 'CreateGroup' not in FAMILIES\n"
            "try:\n    family_run('CreateGroup', None, {})\n"
            "except Exception as e:\n    print(e)\n")
    out = subprocess.run([sys.executable, "-c", code], capture_output=True, text=True,
                         check=True, cwd=str(__import__("pathlib").Path(__file__).parent.parent / "src"))
    assert out.stdout.strip() == ("Mandatory parameter 'items' missing in call to "
                                  "Seqsee::SCF::CreateGroup::run")


def test_look_for_similar_groups_schedules_focus_on_all_when_few():
    e, (a, b, c) = _groups([1, 2, 3, 4, 5, 6, 7, 8], (0, 1), (3, 4), (6, 7))
    for g in (a, b, c):
        g.describe_as(S.ASCENDING)
    util.srand(5)
    family_run("LookForSimilarGroups", None, {"group": a})
    got = sorted(id(cl[3]["what"]) for cl in scoderack.CODELETS)
    assert got == sorted([id(b), id(c)])
    assert {(cl[0], cl[1]) for cl in scoderack.CODELETS} == {("FocusOn", 50)}


def test_do_the_same_thing_never_gets_past_apply_mapping(monkeypatch):
    # With the quirk lifted (ApplyMapping/__PlonkIntoPlace bound), the rest of the body runs:
    # the next group is plonked, described and related to the group.
    from seqsee.mapping import apply_mapping
    e, (a, b) = _groups([1, 2, 3, 2, 3, 4, 3, 4, 5, 9], (0, 2), (3, 5))
    for g in (a, b):
        g.describe_as(S.ASCENDING)
    sltm.spike_by(100, S.ASCENDING)
    util.srand(1)
    t = find_mapping(a, b)
    before = set(map(id, sworkspace.OBJECTS.values()))
    family_run("DoTheSameThing", None, {"group": b, "transform": t, "direction": DIR.RIGHT})
    assert set(map(id, sworkspace.OBJECTS.values())) == before
    monkeypatch.setattr(general, "_apply_mapping", apply_mapping)
    monkeypatch.setattr(general, "_plonk_into_place", sworkspace.plonk_into_place)
    family_run("DoTheSameThing", None, {"group": b, "transform": t, "direction": DIR.RIGHT})
    (new,) = [o for o in sworkspace.OBJECTS.values() if id(o) not in before]
    assert new.get_edges() == (6, 8)
    assert util.perl_true(new.is_of_category_p(S.ASCENDING))
    assert util.perl_true(b.get_relation(new))


def test_do_the_same_thing_ignores_elements_beyond_known(monkeypatch):
    from seqsee.mapping import apply_mapping
    e, (a, b) = _groups([1, 2, 3, 2, 3, 4, 3, 4], (0, 2), (3, 5))
    for g in (a, b):
        g.describe_as(S.ASCENDING)
    sltm.spike_by(100, S.ASCENDING)
    util.srand(1)
    t = find_mapping(a, b)
    monkeypatch.setattr(general, "_apply_mapping", apply_mapping)
    n = len(sworkspace.OBJECTS)
    family_run("DoTheSameThing", None, {"group": b, "transform": t, "direction": DIR.RIGHT})
    assert len(sworkspace.OBJECTS) == n


def test_merge_groups_swallows_errors_in_its_eval(monkeypatch):
    e, (a, b) = _groups([1, 2, 3, 4, 5, 6], (0, 2), (2, 4))
    r = SRelation({"first": e[0], "second": e[1], "type": mapping("succ")})
    r.insert()
    a.set_underlying_ruleapp(r)

    def boom(*_):
        raise Confess("boom")
    monkeypatch.setattr(sworkspace, "add_group", boom)
    family_run("MergeGroups", None, {"a": a, "b": b})  # no exception


def test_create_group_needs_a_mapping_transform():
    e, _ = _groups([1, 2, 3])
    with pytest.raises(Confess, match="transform should be a Mapping!"):
        family_run("CreateGroup", None, {"items": e, "transform": "succ"})
    with pytest.raises(Confess, match="Got neither"):
        family_run("CreateGroup", None, {"items": e, "transform": 0, "category": ""})


def test_perl_eval_lets_stubs_through():
    def stub():
        raise NotImplementedError("TODO: iteration 999")
    with pytest.raises(NotImplementedError):
        general._perl_eval(stub)
    assert general._perl_eval(lambda: 3) == (True, 3)
    ok, e = general._perl_eval(lambda: 1 / 0)
    assert not ok and isinstance(e, ZeroDivisionError)
