"""Port of SLTM.pm: the long-term memory (nodes, links, activations).

Ported: ``%MEMORY``/``@MEMORY``/``$NodeCount``, ``Clear``, ``GetNodeCount``,
``GetMemoryIndex`` (alias ``InsertUnlessPresent``), ``InsertNode``, ``encode``/
``encode_hash``/``decode``/``decode_hash`` (item 017); and (item 029) ``@ACTIVATIONS``,
``@LINKS``, ``@OUT_LINKS``, the link functions, ``SpreadActivationFrom``, ``SpikeBy``,
``WeakenBy``, ``SpikeAndChoose``, ``DecayAll``, the ``Get*``/``Set*``/``Choose*`` helpers
and ``GetTopConcepts``; and (item 030) ``init``, ``Dump``/``Load``/``Load_Helper``,
``FindActiveFollowers``/``FindActiveCategories``, ``LogActivations``/``PrintNode`` and
``Print`` (``print_ltm``).

Perl's ``%MEMORY`` is keyed by the stringified object (its address); here ``_INDEX`` is
keyed by ``id(obj)``, and ``MEMORY`` keeps every node alive so the ids stay unique.
``memory_index(obj)`` is the plain ``$MEMORY{$obj}`` lookup (no insertion).

``ACTIVATIONS`` and ``OUT_LINKS`` are 1-based Perl arrays. ``OUT_LINKS[i]`` is a list
indexed by link type (holes are None); each entry is a dict ``to_index -> SLinkActivation``
(Perl: a hash, so iteration order differs; see ``_hash_key`` for the keys).

PERL-QUIRK: reading ``$ACTIVATIONS[$i]->[...]`` past the end autovivifies an unblessed
``[]`` at ``$i`` (the gap filled with undef); the getters and setters do the same here, and
``DecayAll`` then vivifies every hole (see ``decay_all``).
"""
import logging
import re

from seqsee import schoose, util
from seqsee import slink_activation as sla
from seqsee import snode_activation as sna
from seqsee.errors import Confess, LTM_LoadFailure

_log = logging.getLogger(__name__)

LTM_FOLLOWS = 1         # Link of type A often follows B in sequences
LTM_IS = 2              # A is an instance of B
LTM_CAN_BE_SEEN_AS = 3  # A has been squinted as B
LTM_TYPE_COUNT = 3

LinkType2Str = {1: "FOLLOWS", 2: "IS", 3: "CAN_BE_SEEN_AS"}

PRECALCULATED = sla.PRECALCULATED

SEP1, SEP2, CHAR1, CHAR2, CHAR3 = (chr(c) for c in range(129, 134))

# Perl %_PURE_CLASSES_: classes stored as themselves (others go through get_pure).
PURE_CLASSES = frozenset((
    "SCat::OfObj::Std", "SCat::OfObj::RelationTypeBased", "SCat::OfObj::Interlaced",
    "SCat::OfObj::Assuming", "SCat::OfObj::Alternating", "SLTM::Platonic",
    "METO_MODE", "POS_MODE",
    "Mapping::Numeric", "Mapping::Structural", "Mapping::Position", "Mapping::MetoType",
    "Mapping::Dir", "SMetonymType",
))

MEMORY = ["!!!"]       # 1-based
_INDEX = {}            # id(node) -> index into MEMORY
_CURRENTLY_INSTALLING = set()
NodeCount = 0
ACTIVATIONS = [sna.SNodeActivation()]   # 1-based; slot 0 is a dummy activation
LINKS = []                              # all links, for decay
OUT_LINKS = ["!!!"]                     # 1-based; outgoing links by type


def _debug_message(text, *args):
    """Perl: main::debug_message($text, 1, 1) (GUI commentary); logged here."""
    _log.debug("%s", text)


def clear():
    """Perl: Clear (also run at load time). The test fixture calls it too. The lists are
    cleared in place (SLinkActivation's AmountToSpread reads ``ACTIVATIONS``)."""
    global NodeCount
    MEMORY[:] = ["!!!"]
    _INDEX.clear()
    _CURRENTLY_INSTALLING.clear()
    NodeCount = 0
    ACTIVATIONS[:] = [sna.SNodeActivation()]
    LINKS.clear()
    OUT_LINKS[:] = ["!!!"]


def get_node_count():
    """Perl: GetNodeCount."""
    return NodeCount


def memory_index(obj):
    """Perl ``$MEMORY{$obj}``: the node's index, or None if it isn't in the LTM."""
    return _INDEX.get(id(obj))


def get_memory_index(x):
    """Perl: GetMemoryIndex($x), alias InsertUnlessPresent: the index of x's pure
    version, inserting it first if needed."""
    pure = x if util.perl_ref(x) in PURE_CLASSES else x.get_pure()
    if not util.perl_true(pure):
        raise Confess(f"No pure version of {util.stringify_for_carp(x)} available!")
    return memory_index(pure) or insert_node(pure)


insert_unless_present = get_memory_index


def insert_node(pure):
    """Perl: InsertNode($pure): insert the node's memory dependencies (unless present),
    then the node itself; returns its index."""
    global NodeCount
    if not util.perl_ref(pure):
        raise Confess(f"Attempt to insert bogus object: '{util.perl_str(pure)}'")
    if id(pure) in _CURRENTLY_INSTALLING:
        raise Confess("")
    _CURRENTLY_INSTALLING.add(id(pure))
    for dep in pure.get_memory_dependencies():
        if not memory_index(dep):
            insert_node(dep)
    NodeCount += 1
    MEMORY.append(pure)
    ACTIVATIONS.append(sna.SNodeActivation())
    OUT_LINKS.append([])
    _INDEX[id(pure)] = NodeCount
    _CURRENTLY_INSTALLING.discard(id(pure))
    return NodeCount


# ---- Perl array helpers ----

def _int_index(i):
    return int(util.perl_num(i))


def _hash_key(x):
    """A Perl hash key: keys are strings, so 3 and "3" are the same key. Integer-looking
    keys are kept as ints here (they index MEMORY/ACTIVATIONS)."""
    s = util.perl_str(x)
    return int(s) if re.fullmatch(r"-?[1-9]\d*|0", s) else s


def _vivify(array, i, empty=list):
    """Perl lvalue ``$array[$i]`` used as a reference: past the end, pad with undef; an
    undef slot becomes a new empty container. Returns the slot's value."""
    i = _int_index(i)
    if i >= len(array):
        array.extend([None] * (i + 1 - len(array)))
    if array[i] is None:
        array[i] = empty()
    return array[i]


def _store(array, i, value):
    """Perl ``$array[$i] = $value`` (pads with undef)."""
    if i >= len(array):
        array.extend([None] * (i + 1 - len(array)))
    array[i] = value
    return value


def _activation(i):
    """Perl ``$ACTIVATIONS[$i]`` dereferenced (autovivifies past the end)."""
    i = _int_index(i)
    if -len(ACTIVATIONS) <= i < 0:
        return ACTIVATIONS[i]
    return _vivify(ACTIVATIONS, i)


def _field(act, k):
    """Perl ``$act->[$k]``: undef past the end."""
    return act[k] if k < len(act) else None


def _out_links_of_type(index, type_):
    """Perl ``$OUT_LINKS[$index][$type] ||= {}``."""
    return _vivify(_vivify(OUT_LINKS, index), type_, dict)


# ---- links ----

def insert_link_unless_present(from_index, to_index, modifier_index, type_):
    """Perl: __InsertLinkUnlessPresent($from_index, $to_index, $modifier_index, $type).
    An existing link is returned as is (its modifier is not changed)."""
    outgoing = _out_links_of_type(from_index, type_)
    key = _hash_key(to_index)
    link = outgoing.get(key)
    if link:
        return link
    new_link = sla.SLinkActivation(modifier_index)
    outgoing[key] = new_link
    LINKS.append(new_link)
    return new_link


def insert_follows_link(category, relation):
    """Perl: InsertFollowsLink($category, $relation)."""
    return insert_link_unless_present(get_memory_index(category), get_memory_index(relation),
                                      0, LTM_FOLLOWS)


def insert_isa_link(frm, to):
    """Perl: InsertISALink($from, $to)."""
    return insert_link_unless_present(get_memory_index(frm), get_memory_index(to), 0, LTM_IS)


def strengthen_link_given_index(frm, to, type_, amount):
    """Perl: StrengthenLinkGivenIndex($from, $to, $type, $amount): spikes the link and
    returns its new real activation.

    A missing link fails the Smart::Comments ``### require: exists(...)``, which dies "\\n"
    in Perl (after printing the assertion); here it raises Confess("require: ..."). The
    ``||= {}`` has already vivified the empty hash."""
    outgoing = _out_links_of_type(frm, type_)
    key = _hash_key(to)
    if key not in outgoing:
        raise Confess("require: exists($outgoing_links_ref->{$to})")
    return sla.spike(outgoing[key], amount)


def strengthen_link_given_nodes(frm, to, type_, amount):
    """Perl: StrengthenLinkGivenNodes($from, $to, $type, $amount)."""
    return strengthen_link_given_index(get_memory_index(frm), get_memory_index(to), type_, amount)


def _memory_object(index):
    node = sla.perl_index(MEMORY, index)
    if node is None or isinstance(node, str):
        raise Confess('Can\'t call method "as_text" on an undefined value')
    return node


def _link_sets(index):
    """Perl ``@{ $OUT_LINKS[$index] }``: the per-type hashes (holes skipped)."""
    return [h for h in _vivify(OUT_LINKS, index) if h]


def spread_activation_from(root_index):
    """Perl: SpreadActivationFrom($root_index).

    Every node linked from the root gets ``int(AmountToSpread(root activation))``; nodes
    that got more than 5 in total pass on 0.3 × AmountToSpread (still of the root's
    activation) to their own targets, except nodes already reached at distance 1. Perl
    walks the hashes in hash order; this matters only when one target saturates twice."""
    root_name = _memory_object(root_index).as_text()
    root_key = _hash_key(root_index)
    nodes_at_distance_below_1 = {root_key: 0}
    activation = _activation(root_index)[sna.REAL_ACTIVATION]
    for link_set in _link_sets(root_index):
        for target_index, link in list(link_set.items()):
            amount_to_spread = link.amount_to_spread(activation)
            sna.spike_several(int(amount_to_spread), _activation(target_index))
            nodes_at_distance_below_1[target_index] = (
                nodes_at_distance_below_1.get(target_index, 0) + amount_to_spread)
            node_name = _memory_object(target_index).as_text()
            _debug_message(f"distance = 1 [{target_index}] >{node_name}< got an extra "
                           f"{util.perl_str(amount_to_spread)} from >{root_name}<", 1, 1)

    for node, amount_spiked_by in list(nodes_at_distance_below_1.items()):
        if not amount_spiked_by > 5:
            continue
        for link_set in _link_sets(node):
            for target_index, link in list(link_set.items()):
                if target_index in nodes_at_distance_below_1:
                    continue
                amount_to_spread = link.amount_to_spread(activation)
                amount_to_spread *= 0.3
                sna.spike_several(int(amount_to_spread), _activation(target_index))
                node_name = _memory_object(target_index).as_text()
                _debug_message(f"distance = 2 [{target_index}] >{node_name}< got an extra "
                               f"{util.perl_str(amount_to_spread)} from >{root_name}<", 1, 1)


# ---- setters ----

def set_significance_and_stability_for_index(index, significance, stability):
    """Perl: SetSignificanceAndStabilityForIndex($index, $significance, $stability).

    PERL-QUIRK: it uses SLinkActivation's indices on a node activation, so it overwrites
    the node's depth reciprocal (with ``int(significance / 5)``) and its real activation
    (with the stability). Returns the stability."""
    act = _activation(index)
    _store(act, sla.RAW_SIGNIFICANCE, int(util.perl_num(significance) / 5))
    return _store(act, sla.STABILITY_RECIPROCAL, stability)


def set_depth_reciprocal_for_index(index, depth_reciprocal):
    """Perl: SetDepthReciprocalForIndex($index, $depth_reciprocal). The value is stored
    as given (the real activation is not recomputed)."""
    return _store(_activation(index), sna.DEPTH_RECIPROCAL, depth_reciprocal)


def set_raw_activation_for_index(index, activation):
    """Perl: SetRawActivationForIndex($index, $activation). The real activation is not
    recomputed."""
    return _store(_activation(index), sla.RAW_ACTIVATION, activation)


# ---- spiking ----

def _indices_of_refs(concepts):
    return [get_memory_index(c) for c in concepts if util.perl_ref(c)]


def spike_by(amount, *concepts):
    """Perl: SpikeBy($amount, @concepts). Non-references are skipped. Returns the last
    concept's new real activation; with no concepts left it dies like SpikeSeveral."""
    return sna.spike_several(amount, *[ACTIVATIONS[i] for i in _indices_of_refs(concepts)])


def weaken_by(amount, *concepts):
    """Perl: WeakenBy($amount, @concepts)."""
    return sna.weaken_several(amount, *[ACTIVATIONS[i] for i in _indices_of_refs(concepts)])


def spike_and_choose(amount, *concepts):
    """Perl: SpikeAndChoose($amount, @concepts): spike the concepts, then choose one
    weighted by real activation; activations of 0.02 or less (or undef) count as 0, and if
    all do, nothing is chosen (None, no draw). No concepts → None (Perl: empty list)."""
    if not concepts:
        return None
    if any(c is None for c in concepts):
        raise Confess("undef was one argument to SpikeAndChoose")
    relevant_activations = [ACTIVATIONS[get_memory_index(c)] for c in concepts]
    sna.spike_several(amount, *relevant_activations)
    weights = []
    for act in relevant_activations:
        a = act[sna.REAL_ACTIVATION]
        weights.append(a if a is not None and a > 0.02 else 0)
    return schoose.choose_if_non_zero(weights, list(concepts))


def decay_all():
    """Perl: DecayAll: decay every node activation once (slot 0 included), then every link.

    PERL-QUIRK: ``for (@_)`` vivifies holes in ``@ACTIVATIONS``, so an undef slot or a
    vivified short array ends up as ``[raw, undef, PRECALCULATED[raw]]`` (raw at least 2).
    Returns "" (the value of the final loop)."""
    for i, act in enumerate(ACTIVATIONS):
        if act is None:
            act = ACTIVATIONS[i] = []
        if len(act) < 3:
            act.extend([None] * (3 - len(act)))
    sna.decay_many_times(1, *ACTIVATIONS)
    for link in LINKS:
        sla.decay(link)
    return ""


# ---- getters ----

def get_raw_activations_for_indices(index_ref):
    """Perl: GetRawActivationsForIndices(\\@indices)."""
    return [_field(_activation(i), sna.RAW_ACTIVATION) for i in index_ref]


def get_real_activations_for_indices(index_ref):
    """Perl: GetRealActivationsForIndices(\\@indices)."""
    return [_field(_activation(i), sna.REAL_ACTIVATION) for i in index_ref]


def get_real_activations_for_concepts(concepts):
    """Perl: GetRealActivationsForConcepts(\\@concepts) (inserts missing concepts)."""
    return [_field(_activation(get_memory_index(c)), sna.REAL_ACTIVATION) for c in concepts]


def get_real_activations_for_one_concept(concept):
    """Perl: GetRealActivationsForOneConcept($concept) (inserts it if missing)."""
    return _field(_activation(get_memory_index(concept)), sna.REAL_ACTIVATION)


# ---- choosers ----

_chooser_given_indices = schoose.create(
    map=lambda i: _field(_activation(i), sna.REAL_ACTIVATION))
# $SLTM::MEMORY{$_}: no insertion; an unknown concept reads the dummy slot 0.
_chooser_given_concepts = schoose.create(
    map=lambda c: _field(_activation(memory_index(c) or 0), sna.REAL_ACTIVATION))


def choose_index_given_index(indices):
    """Perl: ChooseIndexGivenIndex(\\@indices), weighted by real activation."""
    return _chooser_given_indices(indices)


def choose_concept_given_index(indices):
    """Perl: ChooseConceptGivenIndex(\\@indices). Nothing chosen reads ``$MEMORY[undef]``,
    the "!!!" placeholder."""
    return _memory_at(_chooser_given_indices(indices))


def choose_index_given_concept(concepts):
    """Perl: ChooseIndexGivenConcept(\\@concepts)."""
    return memory_index(_chooser_given_concepts(concepts))


def choose_concept_given_concept(concepts):
    """Perl: ChooseConceptGivenConcept(\\@concepts). Unknown concepts are not inserted."""
    return _chooser_given_concepts(concepts)


def get_top_concepts(n=None):
    """Perl: GetTopConcepts($N), a dummy: ``[concept, real, raw]`` for every node (N is
    ignored)."""
    return [[MEMORY[i], _field(ACTIVATIONS[i], sna.REAL_ACTIVATION),
             _field(ACTIVATIONS[i], sna.RAW_ACTIVATION)] for i in range(1, NodeCount + 1)]


def find_active_followers(concept):
    """Perl: FindActiveFollowers($concept): for each of the concept's categories, apply
    every mapping it has a FOLLOWS link to; each result goes into a Set::Weighted with
    weight 1 (a dummy value in Perl), then equal keys are merged. A mapping that dies or
    gives a false value is skipped (``eval { ... } or next``)."""
    from seqsee.mapping import apply_mapping
    from seqsee.set.weighted import SetWeighted
    ret = SetWeighted()
    for cat in list(concept.get_categories()):
        node_id = get_memory_index(cat)
        for relation_type_index, _link in list(_out_links_of_type(node_id, LTM_FOLLOWS).items()):
            relation_type = sla.perl_index(MEMORY, relation_type_index)
            try:
                possible_next_object = apply_mapping(relation_type, concept)
            except Exception:  # Perl: eval { ... }
                continue
            if not util.perl_true(possible_next_object):
                continue
            ret.insert([possible_next_object, 1])
    ret.merge_keys()
    return ret


def find_active_categories(concept):
    """Perl: FindActiveCategories($concept): the categories the concept has IS links to,
    minus its current categories, weighted by their real activation."""
    from seqsee.set.weighted import SetWeighted
    current_categories = list(concept.get_categories())
    ret = SetWeighted()
    for category_index, _link in list(_out_links_of_type(get_memory_index(concept), LTM_IS).items()):
        category = sla.perl_index(MEMORY, category_index)
        if any(category is c for c in current_categories):   # $category ~~ @current_categories
            continue
        ret.insert([category, _field(_activation(category_index), sna.REAL_ACTIVATION)])
    return ret


# ---- init, logging and printing ----

_NODES_ALREADY_PRINTED = set()   # Perl: the closure's %NodesAlreadyPrinted (Clear keeps it)


def reset():
    """Test hook: Clear, plus forget which nodes LogActivations has already named."""
    clear()
    _NODES_ALREADY_PRINTED.clear()


def init():
    """Perl: SLTM->init: announce, and open the activations log if the LogActivations
    feature is on and no log is open yet (line-buffered; Perl sets autoflush). A failed
    open leaves the handle undef, as in Perl."""
    from seqsee import global_ as Global
    print("Initializing SLTM...")
    if Global.Feature.get("LogActivations"):
        print("\tActivation logging requested.")
        if not Global.ActivationsLogHandle:
            print(f"\tOpening file for write: {util.perl_str(Global.ActivationsLogfile)}")
            try:
                handle = open(Global.ActivationsLogfile, "w", buffering=1)
            except OSError:
                handle = None
            Global.ActivationsLogHandle = handle


def log_activations():
    """Perl: LogActivations (only called with the LogActivations feature on): one line of
    ``step index activation ...`` for nodes above 0.01, naming each node the first time."""
    from seqsee import global_ as Global
    to_print = [Global.Steps_Finished]
    for i in range(1, NodeCount + 1):
        activation = _field(ACTIVATIONS[i], sna.REAL_ACTIVATION)
        if not util.perl_num(activation) > 0.01:
            continue
        if i not in _NODES_ALREADY_PRINTED:
            print_node(i, MEMORY[i].as_text())
            _NODES_ALREADY_PRINTED.add(i)
        to_print += [i, activation]
    Global.ActivationsLogHandle.write(" ".join(util.perl_str(x) for x in to_print) + "\n")


def print_node(id_, name):
    """Perl: PrintNode($id, $name)."""
    from seqsee import global_ as Global
    Global.ActivationsLogHandle.write(f"NewNode\t{util.perl_str(id_)}\t{util.perl_str(name)}\n")


def _links_of_type(links_ref, type_):
    """Perl ``$links_ref->[$type]`` (undef past the end or for a hole)."""
    if isinstance(links_ref, list) and type_ < len(links_ref):
        return links_ref[type_]
    return None


def print_ltm():
    """Perl: Print: every node with its depth reciprocal and text, then its links by
    type, to stdout."""
    for index in range(1, NodeCount + 1):
        pure, activation = MEMORY[index], ACTIVATIONS[index]
        depth_reciprocal = _field(activation, sna.DEPTH_RECIPROCAL)
        print(f"=== {index}: {util.perl_ref(pure)} {util.perl_str(depth_reciprocal)}\n"
              f"{pure.as_text()}")
        links_ref = OUT_LINKS[index]
        for type_ in range(1, LTM_TYPE_COUNT + 1):
            links_of_this_type = _links_of_type(links_ref, type_) or {}
            if not links_of_this_type:
                continue
            print(f"\t{LinkType2Str[type_]}")
            for to_node, link in list(links_of_this_type.items()):
                modifier_index = _field(link, sla.MODIFIER_NODE_INDEX)
                significance = _field(link, sla.RAW_SIGNIFICANCE)
                stability = _field(link, sla.STABILITY_RECIPROCAL)
                modifier_name = ""
                if util.perl_true(modifier_index):
                    modifier_name = _memory_object(modifier_index).as_text()
                to_name = _memory_object(to_node).as_text()
                print(f"\t\tTo: {to_name}\n\t\tModifier: {modifier_name}")
                print(f"\t\tSig: {util.perl_str(significance)}, \tStab: {util.perl_str(stability)}")


# ---- persistence ----

_WS = " \t\n\r\f\v"   # Perl \s on byte strings


class _NullHandle:
    def write(self, _text):
        pass

    def close(self):
        pass


def dump(file):
    """Perl: SLTM->Dump($file): write every node (``=== index: Class depth`` and its
    serialized form), then ``#####`` and one line per link (Perl: hash order).

    *file* is a filename, or an open file object (Perl: a File::Temp object); either way
    the handle is closed afterwards. Files are written byte-for-byte (latin-1), as Perl
    does. A filename that can't be opened writes nothing (Perl doesn't check open)."""
    print(f"Dumping LTM to file {util.perl_str(file)}")
    if hasattr(file, "write"):
        filehandle = file
    elif util.perl_ref(file) and not isinstance(file, str):
        raise Confess("Dump must be called either with an unblessed filename or a File::Temp object")
    else:
        try:
            filehandle = open(util.perl_str(file), "w", encoding="latin-1", newline="")
        except OSError:
            filehandle = _NullHandle()

    for index in range(1, NodeCount + 1):
        pure, activation = MEMORY[index], ACTIVATIONS[index]
        depth_reciprocal = _field(activation, sna.DEPTH_RECIPROCAL)
        filehandle.write(f"=== {index}: {util.perl_ref(pure)} {util.perl_str(depth_reciprocal)}\n"
                         f"{util.perl_str(pure.serialize())}\n")

    filehandle.write("#####\n")
    for from_node in range(1, NodeCount + 1):
        links_ref = OUT_LINKS[from_node]
        for type_ in range(1, LTM_TYPE_COUNT + 1):
            for to_node, link in list((_links_of_type(links_ref, type_) or {}).items()):
                modifier_index = _field(link, sla.MODIFIER_NODE_INDEX)
                if not util.perl_true(modifier_index):
                    modifier_index = 0
                significance = util.perl_num(_field(link, sla.RAW_SIGNIFICANCE))
                stability = util.perl_num(_field(link, sla.STABILITY_RECIPROCAL))
                filehandle.write("%4s %4s %2s %4s %7.4f %7.5f\n" % (
                    from_node, util.perl_str(to_node), type_, util.perl_str(modifier_index),
                    significance, stability))
    filehandle.close()


def load(filename):
    """Perl: SLTM->Load($filename) ("Safe, non-throwing"). An SErr::LTM_LoadFailure is
    warned about (with a trace) and the program exits; other errors are re-thrown.

    PERL-QUIRK: after a successful load, ``Exception::Class->caught()`` returns the empty
    ``$@`` and Load does ``die ""``, which dies "Died". So Load always dies."""
    import sys
    import traceback
    try:
        load_helper(filename)
    except LTM_LoadFailure as e:
        trace = "".join(traceback.format_tb(e.__traceback__))
        sys.stderr.write(f"Failure loading LTM: {util.perl_str(e.what)}\n{trace}\n")
        raise SystemExit(0)   # Perl: exit
    raise Confess("Died")


_DESERIALIZER_MODULES = (
    "seqsee.sltm_platonic", "seqsee.constants", "seqsee.smetonym_type", "seqsee.sint",
    "seqsee.categories.ascending", "seqsee.categories.descending", "seqsee.categories.even",
    "seqsee.categories.odd", "seqsee.categories.number", "seqsee.categories.prime",
    "seqsee.categories.mountain", "seqsee.categories.sameness", "seqsee.categories.alternating",
    "seqsee.categories.interlaced", "seqsee.categories.mapping_based",
    "seqsee.mapping", "seqsee.mapping.numeric", "seqsee.mapping.structural",
    "seqsee.mapping.position", "seqsee.mapping.meto_type", "seqsee.mapping.dir",
)


def _perl_package(name):
    """The class for a Perl package name (``ref($pure)`` in a dump), or None."""
    import importlib
    for module_name in _DESERIALIZER_MODULES:
        module = importlib.import_module(module_name)
        for value in vars(module).values():
            if isinstance(value, type) and (value.__dict__.get("perl_name") or value.__name__) == name:
                return value
    return None


def _deserialize(type_, val):
    """Perl ``$type->deserialize($val)``."""
    cls = _perl_package(type_)
    if cls is None:
        raise Confess(f'Can\'t locate object method "deserialize" via package "{type_}" '
                      f'(perhaps you forgot to load "{type_}"?)')
    if not hasattr(cls, "deserialize"):
        raise Confess(f'Can\'t locate object method "deserialize" via package "{type_}"')
    return cls.deserialize(val)


def _read_file(filename):
    """File::Slurp read_file: the bytes, as latin-1 text."""
    try:
        with open(util.perl_str(filename), encoding="latin-1", newline="") as fh:
            return fh.read()
    except OSError as e:
        raise Confess(f"read_file '{util.perl_str(filename)}' - open: {e.strerror}") from None


def load_helper(filename):
    """Perl: SLTM->Load_Helper($filename): Clear, then rebuild the LTM from a Dump file.
    May raise SErr::LTM_LoadFailure.

    Nodes must come out at consecutive indices (the ``=== N:`` numbers are ignored). The
    depth reciprocal is stored as the string read (everything after the first whitespace
    of the header line). Link fields are stored as strings; an existing link keeps its
    modifier but gets the new significance and stability."""
    print(f"Loading LTM from {util.perl_str(filename)}")
    clear()
    string = _read_file(filename)
    parts = re.split("#####", string, maxsplit=2)   # Perl: implicit limit 3 for ($a, $b)
    nodes = parts[0]
    links = parts[1] if len(parts) > 1 else ""

    nodes_added = 0
    for chunk in re.split(r"=== \d+:", nodes, flags=re.ASCII):
        chunk = chunk.lstrip(_WS).rstrip(_WS)
        if chunk == "":
            continue
        type_and_sig, _, val = chunk.partition("\n")
        if "\n" not in chunk:
            val = None
        type_sig = re.split(r"\s", type_and_sig, maxsplit=1, flags=re.ASCII)
        type_ = type_sig[0]
        depth_reciprocal = type_sig[1] if len(type_sig) > 1 else None
        try:
            pure = _deserialize(type_, val)
        except Exception as e:
            msg = f"Unable to deserialize >>{util.perl_str(val)}<< of type >>{type_}<<\n"
            msg += str(e)
            msg += f"\nNodes inserted so far: {NodeCount}."
            raise LTM_LoadFailure(what=msg) from e

        if pure is None:
            raise LTM_LoadFailure(
                what=f"Could not find pure: type='{type_}', val='{util.perl_str(val)}'")

        index = insert_unless_present(pure)
        nodes_added += 1
        if nodes_added != NodeCount:
            raise LTM_LoadFailure(
                what=f"Should have only added {nodes_added} nodes by now, but looks like "
                     f"{NodeCount}. Was trying to add '{util.perl_ref_string(pure)}'. "
                     f"Its index is now {index}.")
        set_depth_reciprocal_for_index(index, depth_reciprocal)

    for line in re.split(r"\n+", links):
        line = line.lstrip(_WS).rstrip(_WS)
        if line == "":
            continue
        fields = re.split(r"\s+", line, flags=re.ASCII) + [None] * 6
        frm, to, type_, modifier_index, significance, stability = fields[:6]
        activation = insert_link_unless_present(frm, to, modifier_index, type_)
        _store(activation, sla.RAW_SIGNIFICANCE, significance)
        _store(activation, sla.STABILITY_RECIPROCAL, stability)


def _encode_one(x, in_hash):
    if isinstance(x, dict):
        if in_hash:
            raise Confess("Recursive hash cannot be encoded")
        return encode_hash(x)
    cls = util.perl_ref(x)
    if cls == "SInt":
        return CHAR3 + util.perl_str(x.get_mag())
    if cls:
        index = memory_index(x)
        if in_hash and not index:
            raise Confess(f"unrecognized reference to '{util.perl_ref_string(x)}'")
        return CHAR1 + ("" if index is None else str(index))
    return util.perl_str(x)


def encode(*objects):
    """Perl: SLTM::encode(@objects). Objects not in the LTM encode as a bare CHAR1
    (Perl: an undef index, with a warning)."""
    return SEP1.join(_encode_one(x, False) for x in objects)


def encode_hash(hash_ref):
    """Perl: encode_hash(\\%hash): CHAR2 then keys and values joined by SEP2 (Perl: hash
    order; here insertion order). Flattening resets the hash's each-iterator."""
    util.perl_hash_reset(hash_ref)
    flat =[part for pair in hash_ref.items() for part in pair]
    return CHAR2 + SEP2.join(_encode_one(x, True) for x in flat)


_RX1 = re.compile("^" + CHAR1 + "(.*)", re.S)
_RX2 = re.compile("^" + CHAR2 + "(.*)", re.S)
_RX3 = re.compile("^" + CHAR3 + "(.*)", re.S)


def _perl_split(sep, string):
    """Perl ``split($sep, $str)``: trailing empty fields are dropped."""
    fields = string.split(sep)
    while fields and fields[-1] == "":
        fields.pop()
    return fields


def _memory_at(index_string):
    """Perl ``$MEMORY[$1]``: a non-numeric or empty index is 0 (the "!!!" placeholder);
    past the end is undef."""
    i = int(util.perl_num(index_string))
    try:
        return MEMORY[i]
    except IndexError:
        return None


def _sint_mag(string):
    """SInt->new($1): numeric strings become numbers (the port keeps magnitudes numeric)."""
    if util.looks_like_number(string):
        num = util.perl_num(string)
        if util.perl_str(num) == string:
            return num
    return string


def decode(string):
    """Perl: SLTM::decode($str): the list of decoded objects."""
    from seqsee.sint import SInt
    out = []
    for part in _perl_split(SEP1, string):
        if m := _RX1.match(part):
            out.append(_memory_at(m.group(1)))
        elif m := _RX2.match(part):
            out.append(decode_hash(m.group(1)))
        elif m := _RX3.match(part):
            out.append(SInt(_sint_mag(m.group(1))))
        else:
            out.append(part)
    return out


def decode_hash(string):
    """Perl: decode_hash($str), as the hash ``{ decode_hash($1) }`` that decode builds
    (an odd list gives the last key an undef value)."""
    items = decode(string.replace(SEP2, SEP1))
    if len(items) % 2:
        items.append(None)
    return {util.perl_str(k): v for k, v in zip(items[::2], items[1::2])}
