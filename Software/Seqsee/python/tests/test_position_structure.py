"""Tests for seqsee.position_structure, mirroring PositionStructure.pm
(PositionStructure->Create, IsASubsetOf) and the SWorkspace.pm helpers it calls
(__GetPositionStructure, through a hook until item 033).

Golden data: oracle/position_structure.pl → tests/golden/position_structure.json.
"""
import pytest

import golden
from seqsee import position_structure as ps_mod
from seqsee.position_structure import PositionStructure
from seqsee.util import perl_str

CASES = golden.load("position_structure")


def cases(kind):
    return [c for c in CASES if c["kind"] == kind]


class FakeElement:
    """Stand-in for Seqsee::Element: only its left edge matters here."""

    def __init__(self, left_edge):
        self.left_edge = left_edge


def build(shape):
    """Rebuild the oracle's group shape: {"E": edge} is an element, a list a group."""
    if isinstance(shape, dict):
        return FakeElement(shape["E"])
    return [build(x) for x in shape]


def fake_get_position_structure(obj):
    """SWorkspace::__GetPositionStructure over the fakes."""
    if isinstance(obj, FakeElement):
        return obj.left_edge
    return [fake_get_position_structure(x) for x in obj]


@pytest.fixture
def fake_workspace(monkeypatch):
    monkeypatch.setattr(ps_mod, "_get_position_structure", fake_get_position_structure)


def as_strings(ps):
    return [None if x is None else perl_str(x) for x in ps]


@pytest.mark.parametrize("case", cases("create"), ids=lambda c: c["name"])
def test_create_golden(case, fake_workspace):
    ps = PositionStructure.create(build(case["shape"]))
    assert isinstance(ps, PositionStructure)
    assert as_strings(ps) == case["result"]


def test_create_keeps_element_edges_unstringified(fake_workspace):
    # StringifyDeepArray returns a non-ref unchanged, so a bare element gives its edge.
    ps = PositionStructure.create([FakeElement(0), [FakeElement(1), FakeElement(2)]])
    assert list(ps) == [0, "[1, 2]"]


def test_create_nested_undef_edge_stringifies_empty(fake_workspace):
    ps = PositionStructure.create([[FakeElement(1), FakeElement(None)]])
    assert list(ps) == ["[1, ]"]


def test_as_string_golden(fake_workspace):
    (case,) = cases("as_string")
    big = build([[[{"E": 1}, {"E": 2}], [{"E": 3}, {"E": 4}, {"E": 5}]]])[0]
    assert ps_mod.get_position_structure_as_string(big) == case["result"]


def test_workspace_hook_is_wired():
    """The hook is SWorkspace::__GetPositionStructure (item 033): anything that is not an
    exact Seqsee::Element or Seqsee::Anchored gives undef."""
    assert ps_mod._get_position_structure(FakeElement(0)) is None


@pytest.mark.parametrize("case", cases("subset"),
                         ids=lambda c: f"{c['a']}-in-{c['b']}")
def test_is_a_subset_of_golden(case):
    a, b = PositionStructure(case["a"]), PositionStructure(case["b"])
    result = a.is_a_subset_of(b)
    assert result == case["scalar"]
    assert ([] if result is None else [result]) == case["list"]


def test_is_a_subset_of_real_golden(fake_workspace):
    by_name = {c["name"]: PositionStructure.create(build(c["shape"])) for c in cases("create")}
    for case in cases("subset_real"):
        assert by_name[case["a"]].is_a_subset_of(by_name[case["b"]]) == case["scalar"], case


def test_is_a_subset_of_compares_as_strings():
    assert PositionStructure([1, 2]).is_a_subset_of(PositionStructure(["0", "1", "2"])) == 1
    assert PositionStructure([1.0]).is_a_subset_of(PositionStructure(["1"])) == 1
    assert PositionStructure(["1.0"]).is_a_subset_of(PositionStructure(["1"])) is None
    assert PositionStructure([None]).is_a_subset_of(PositionStructure([""])) == 1
