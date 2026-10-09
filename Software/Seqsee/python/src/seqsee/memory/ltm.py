"""Port of Memory/LTM.pm (package ``Memory::LTM``): maps stored cores to Memory::Node
objects.

Perl's class methods become module functions: ``InsertItem`` → ``insert_item(item)``,
``SpikeBy`` → ``spike_by(amount, *items)``, ``WeakenBy`` → ``weaken_by(amount, *items)``,
``_InsertMissingItem`` → ``_insert_missing_item(storable)``. Perl keys ``%CoreToNode``
and ``%_currently_installing`` by the stringified core: here scalars key by their
string and objects by identity (the dicts keep the objects alive, so ids stay unique).
``reset()`` empties both (Perl never does; conftest calls it).

Method calls on things that lack the method raise Perl's errors ("Can't locate object
method ...", "Can't call method ... on an undefined value").

PERL-QUIRKs (oracle-confirmed):
- The module calls ``confess`` without importing Carp, so in Perl it doesn't compile
  ("syntax error"). The port (and the oracle, which imports Carp into the package
  first) is of the module as it would then behave.
- A die while installing (e.g. a dependency that isn't an object, or a loop) leaves the
  item marked as currently installing, so inserting it again reports a loop. The loop
  message lists the marks' values, which are all 1: "... Currently installing: 1, 1".
- SpikeBy/WeakenBy insert the item, then die because Memory::Node has no
  SpikeBy/WeakenBy.
"""
from seqsee import util
from seqsee.errors import Confess
from seqsee.memory.node import Node

_CORE_TO_NODE = {}            # key -> Node
_CURRENTLY_INSTALLING = {}    # key -> storable


def reset():
    """Empty %CoreToNode and %_currently_installing (for test isolation)."""
    _CORE_TO_NODE.clear()
    _CURRENTLY_INSTALLING.clear()


def _key(x):
    if x is None or util._is_scalar(x) or isinstance(x, bool):
        return ("str", util.perl_str(x))
    return ("ref", id(x))


def _call(obj, perl_method, method, *args):
    """``$obj->perl_method(@args)``, with Perl's errors when obj can't do it."""
    if obj is None:
        raise Confess(f'Can\'t call method "{perl_method}" on an undefined value')
    if util._is_scalar(obj) or isinstance(obj, bool):
        pkg = util.perl_str(obj)
        raise Confess(f'Can\'t locate object method "{perl_method}" via package "{pkg}" '
                      f'(perhaps you forgot to load "{pkg}"?)')
    if not hasattr(obj, method):
        raise Confess(f'Can\'t locate object method "{perl_method}" via package '
                      f'"{util.perl_ref(obj)}"')
    return getattr(obj, method)(*args)


def _node_for(storable):
    """Perl: ``$CoreToNode{$storable} //= _InsertMissingItem($storable)``."""
    node = _CORE_TO_NODE.get(_key(storable))
    if node is None:
        node = _CORE_TO_NODE[_key(storable)] = _insert_missing_item(storable)
    return node


def insert_item(item):
    """Perl: Memory::LTM->InsertItem($item): the item's node, inserting its normalized
    form (and its dependencies) if needed."""
    if not _call(item, "does", "does", "Memory::Insertible"):
        raise Confess(f"Non-insertible object '{util.perl_ref_string(item)}'")
    storable = _call(item, "GetNormalizedForMemory", "get_normalized_for_memory")
    return _node_for(storable)


def spike_by(amount, *items):
    """Perl: Memory::LTM->SpikeBy($amount, @items)."""
    for item in items:
        normalized = _call(item, "GetNormalizedForMemory", "get_normalized_for_memory")
        _call(_node_for(normalized), "SpikeBy", "spike_by", amount)


def weaken_by(amount, *items):
    """Perl: Memory::LTM->WeakenBy($amount, @items)."""
    for item in items:
        normalized = _call(item, "GetNormalizedForMemory", "get_normalized_for_memory")
        _call(_node_for(normalized), "WeakenBy", "weaken_by", amount)


def _insert_missing_item(storable):
    """Perl: _InsertMissingItem($storable)."""
    key = _key(storable)
    if key in _CURRENTLY_INSTALLING:
        # There is a dependency loop!
        raise Confess("Loop in dependencies detected! Currently installing: "
                      + ", ".join("1" for _ in _CURRENTLY_INSTALLING))
    _CURRENTLY_INSTALLING[key] = storable
    for dep in _call(storable, "GetMemoryDependencies", "get_memory_dependencies"):
        insert_item(dep)
    node = _CORE_TO_NODE[key] = Node(core=storable)
    del _CURRENTLY_INSTALLING[key]
    return node
