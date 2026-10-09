"""Tests for item 050b: recursion headroom for the main loop.

Mirrors: lib/Seqsee.pm (``Interaction_step_n``, which now runs its steps through
``util.call_with_deep_stack``) and the Class::Multimethods dispatch (``Multimethod.call``)
behind ``FindMapping`` in lib/SCategory.pm / lib/Mapping.pm.

Perl has no recursion limit. In the Alternating sequence ``1 1 1 2 2 3 3 3 4 4 5 5 5 6 6``,
FindMapping(Element, Element) → sameness FindMappingForCat → CalculateBindingsChange →
FindMapping on ``each`` recurses hundreds of levels deep before ``number`` wins
SpikeAndChoose (PROGRESS.md, iteration 51). The port must survive that too:
- steps run in a thread with a large stack and a raised Python recursion limit;
- multimethod dispatch must not go through the ``__call__`` slot. CPython 3.12 counts such
  C-level calls against a fixed C recursion limit (an instance ``__call__`` stopped at
  depth 3331), while Python-to-Python calls only count against ``sys.getrecursionlimit()``.
"""
import sys
import threading

import pytest

from seqsee import mapping, seqsee_main, util
from seqsee.multimethods import Multimethod

DEPTH = 20000   # well past both the default Python limit and 3.12's C recursion limit


def _recurse(n):
    return 0 if n == 0 else 1 + _recurse(n - 1)


# ------------------------------------------------------------------ call_with_deep_stack
def test_deep_stack_returns_the_value():
    assert util.call_with_deep_stack(lambda a, b=0: a + b, 2, b=3) == 5


def test_deep_stack_allows_deep_python_recursion():
    with pytest.raises(RecursionError):
        _recurse(DEPTH)
    assert util.call_with_deep_stack(_recurse, DEPTH) == DEPTH


def test_deep_stack_reraises_the_same_exception():
    err = ValueError("boom")

    def die():
        raise err

    with pytest.raises(ValueError) as info:
        util.call_with_deep_stack(die)
    assert info.value is err


def test_deep_stack_reraises_base_exceptions():
    def leave():
        raise SystemExit(3)

    with pytest.raises(SystemExit) as info:
        util.call_with_deep_stack(leave)
    assert info.value.code == 3


def test_deep_stack_restores_the_recursion_limit_and_stack_size():
    limit, size = sys.getrecursionlimit(), threading.stack_size()
    seen = util.call_with_deep_stack(sys.getrecursionlimit)
    assert seen >= util.DEEP_RECURSION_LIMIT
    assert sys.getrecursionlimit() == limit
    assert threading.stack_size() == size


def test_deep_stack_nested_calls_run_inline():
    def outer():
        me = threading.current_thread()
        return util.call_with_deep_stack(lambda: threading.current_thread() is me)

    assert util.call_with_deep_stack(outer) is True


# ------------------------------------------------------------------ multimethods
class _Probe:
    perl_name = "DeepProbe"


def test_multimethod_call_has_no_c_level_frame():
    mm = Multimethod("Deep")

    @mm.variant("#")
    def _down(n):
        return 0 if n == 0 else 1 + mm.call(n - 1)

    assert mm(3) == 3                       # calling the instance still works
    assert util.call_with_deep_stack(mm.call, DEPTH) == DEPTH


def test_find_mapping_dispatch_has_no_c_level_frame(monkeypatch):
    def deep(a, n):
        return 0 if n == 0 else 1 + mapping.find_mapping(a, n - 1)

    monkeypatch.setitem(mapping.FIND_MAPPING.dispatch, ("DeepProbe", "#"), deep)
    assert util.call_with_deep_stack(mapping.find_mapping, _Probe(), DEPTH) == DEPTH


# ------------------------------------------------------------------ main loop
def test_interaction_step_n_has_recursion_headroom(monkeypatch):
    depths = []

    def deep_step():
        depths.append(_recurse(DEPTH))

    monkeypatch.setattr(seqsee_main, "seqsee_step", deep_step)
    seqsee_main.interaction_step_n({"n": 2, "max_steps": 10})
    assert depths == [DEPTH, DEPTH]


# The real Alternating runs are checked by test_e2e_parity.py's grid (no crash unless Perl
# crashes). Seed 1 there used to crash at step ~391 (default limit) and, with only a raised
# limit, at step 4005 in Multimethod.__call__ (3.12's C recursion limit); that step now
# recurses 3638 levels deep (1.6M FindMappingForCat calls, ~90 s) and the run ends
# NotEvenExtended.
