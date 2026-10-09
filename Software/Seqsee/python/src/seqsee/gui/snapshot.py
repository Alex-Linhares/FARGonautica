"""An immutable, plain-data snapshot of what the GUI views read from the model.

The drawing code (``seqsee.gui.draw``) reads only snapshots, never live model objects, so the
model can keep running on its worker thread while the GUI draws. ``take()`` must be called
on the thread that owns the model (between steps); the result can be sent to any thread.

Everything in a snapshot is a frozen dataclass, a tuple, or a scalar (int, float, str, bool,
None), so it is hashable and can never change after it is taken.

Perl's SGUI modules key their bookkeeping (``%AnchorsForRelations``, ``%RelationsToHide``,
``%Global::Hilit``) by the stringified object. The snapshot gives each object an integer
``oid`` instead, unique within one snapshot: elements first (left to right), then groups (in
``GetGroups`` order), then relations, then any other object a relation or group refers to
(e.g. a relation end that is not in the workspace). ``Snapshot.obj(oid)`` returns the
element, group or relation snapshot, or None for such an outside object.

This is Snapshot I (the workspace), with each object's attention (item 006) and the slipnet
(``SLTM::GetTopConcepts``, item 007), the coderack (item 008), the main stream (item 009) and
the relations' end bounds and Mapping ancestry (item 010), and the objects' categories as
identities plus each fringe component's list label (item 012). SGUI::List::Rules reads
nothing: the SRule methods it calls don't exist, so it dies before reading the model.
"""
import dataclasses
import functools
from typing import Optional

from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scoderack, sltm, sstream2, sworkspace, util
from seqsee.multimethods import perl_isa


@dataclasses.dataclass(frozen=True)
class ObjectSnap:
    """An element or a group (a ``Seqsee::Anchored``)."""
    oid: int
    is_element: bool
    index: Optional[int]          # position in GetElements (elements only)
    mag: object                   # get_mag (elements only)
    left: object                  # get_edges
    right: object
    span: object                  # get_span
    items: tuple                  # oids of @$self (an element's items are itself)
    strength: object              # get_strength
    group_p: bool                 # get_group_p
    metonym_active: bool          # get_metonym_activeness
    structure_string: str         # get_structure_string
    starred_structure_string: Optional[str]  # GetEffectiveObject->get_structure_string, if meto
    categories: tuple             # get_categories, as names (None for unregistered ones)
    category_kind: Optional[str]  # SGUI::Workspace::find_group_style's chain: 'ascending',
                                  # 'descending', 'sameness' (the first that matches) or None
    categories_as_string: str     # get_categories_as_string (Perl refs; SGUI::List::Groups)
    is_locked: bool               # get_is_locked_against_deletion
    bounds_string: str            # get_bounds_string
    hilit: int                    # $Global::Hilit{$obj} || 0
    attention: float = 0          # SCoderack->AttentionDistribution->{$obj} || 0
    category_ids: tuple = ()      # cid (Snapshot.categories) of each of get_categories


@dataclasses.dataclass(frozen=True)
class CategorySnap:
    """A category some workspace object belongs to (SGUI::List::Categories keys its rows by
    the category object; two categories may share a name)."""
    cid: int                      # unique within the snapshot
    name: Optional[str]           # get_name (None for an unregistered category: Perl's undef)


@dataclasses.dataclass(frozen=True)
class RelationSnap:
    """An ``SRelation`` in ``%SWorkspace::relations``."""
    oid: int
    ends: tuple                   # (oid, oid) of get_ends
    strength: object              # get_strength
    type_text: str                # get_type->as_text
    complexity: object            # get_type->get_complexity
    perl_class: str               # ref($reln)
    hilit: int                    # $Global::Hilit{$reln} || 0
    attention: float = 0          # SCoderack->AttentionDistribution->{$reln} || 0
    end_bounds: tuple = ()        # get_bounds_string of each end (also for outside ends)
    isa_numeric: bool = False     # UNIVERSAL::isa($reln, 'Mapping::Numeric') (SGUI::Relations)
    isa_structural: bool = False  # UNIVERSAL::isa($reln, 'Mapping::Structural')


@dataclasses.dataclass(frozen=True)
class ConceptSnap:
    """One entry of ``SLTM::GetTopConcepts(10)``, ``[concept, activation, raw_activation,
    raw_significance]`` in SGUI::Slipnet's DrawNode. The model gives only three fields, so
    ``raw_significance`` is None (Perl's undef)."""
    text: Optional[str]           # $concept->as_text; None if the class has no as_text
                                  # (Mapping::Dir: calling it dies in Perl)
    activation: object            # the node's real activation
    raw_activation: object
    raw_significance: object = None
    perl_class: str = ""          # ref($concept)


@dataclasses.dataclass(frozen=True)
class CoderackSnap:
    """What SGUI::Coderack reads from SCoderack."""
    codelets: tuple = ()          # (family, urgency) per @SCoderack::CODELETS, in rack order
    urgencies_sum: object = 0     # $SCoderack::URGENCIES_SUM
    history: tuple = ()           # (key, count) per %SCoderack::HistoryOfRunnable (keys are
                                  # 'Seqsee::SCF::<family>'), in insertion order


@dataclasses.dataclass(frozen=True)
class ComponentSnap:
    """One ``[component, activation]`` entry of a thought's stored_fringe."""
    text: str                     # "$component" (perl_string: most objects are ref strings)
    activation: object
    hit_intensity: object = None  # $MainStream->{hit_intensity}{$component}
    label: Optional[str] = None   # SGUI::List::Stream's text: as_text if the component can,
                                  # else "$component"


@dataclasses.dataclass(frozen=True)
class ThoughtSnap:
    """A thought of the main stream, as SGUI::Stream's DrawThought reads it."""
    text: str                     # $tht->as_text
    hit_intensity: object = None  # $MainStream->{thought_hit_intensity}{$tht}
    fringe: Optional[tuple] = None  # ComponentSnap (None for a false entry) per stored_fringe
                                    # entry, all of them; None if stored_fringe is false
    perl_class: str = ""          # ref($tht)


@dataclasses.dataclass(frozen=True)
class StreamSnap:
    """What SGUI::Stream reads from ``$Global::MainStream``."""
    current: Optional[ThoughtSnap] = None  # CurrentThought (None if false)
    older: tuple = ()             # ThoughtSnap per OlderThoughts entry (None for a false one)


@dataclasses.dataclass(frozen=True)
class Snapshot:
    steps: int                    # $Global::Steps_Finished
    current_runnable: str         # $Global::CurrentRunnableString
    debug: bool                   # $Global::Feature{debug}
    element_count: int            # $SWorkspace::ElementCount
    elements: tuple               # ObjectSnap per GetElements, left to right
    groups: tuple                 # ObjectSnap per GetGroups (longest span first)
    relations: tuple              # RelationSnap per values %SWorkspace::relations
    bar_lines: tuple              # GetBarLines
    slipnet: tuple = ()           # ConceptSnap per SLTM::GetTopConcepts(10): every node, in
                                  # memory-index order (the N is ignored)
    coderack: CoderackSnap = CoderackSnap()
    stream: StreamSnap = StreamSnap()
    categories: tuple = ()        # CategorySnap per category of the groups (in GetGroups
                                  # order), then of the elements, first seen first

    @functools.cached_property
    def _by_oid(self):
        return {x.oid: x for x in self.elements + self.groups + self.relations}

    def obj(self, oid):
        """The element, group or relation snapshot with this oid (None if outside)."""
        return self._by_oid.get(oid)

    def category(self, cid):
        """The CategorySnap with this cid."""
        return next(c for c in self.categories if c.cid == cid)

    @property
    def largest_group(self):
        """The group SGUI::Workspace draws with ``$is_largest`` (the first of GetGroups)."""
        return self.groups[0] if self.groups else None


def _str(v):
    return None if v is None else util.perl_str(v)


def _category_kind(obj):
    for cat, kind in ((S.ASCENDING, "ascending"), (S.DESCENDING, "descending"),
                      (S.SAMENESS, "sameness")):
        if util.perl_true(obj.is_of_category_p(cat)):
            return kind
    return None


def _concept(entry):
    concept = entry[0]
    as_text = getattr(concept, "as_text", None)
    activation, raw_activation, raw_significance = (list(entry[1:4]) + [None])[:3]
    return ConceptSnap(_str(as_text()) if as_text else None, activation, raw_activation,
                       raw_significance,
                       perl_class=util.perl_ref(concept))


def perl_string(x):
    """Perl's ``"$x"``: scalars as perl_str, SInt by its ``""`` overload (the only fringe
    component class with one), other objects as ``Class=HASH(0x<id>)`` (Perl prints the
    object's address; Class::Std objects are SCALAR refs there)."""
    from seqsee.sint import SInt
    if x is None or isinstance(x, bool) or util._is_scalar(x):
        return util.perl_str(x)
    if isinstance(x, SInt):
        return x.as_text()
    return util.perl_ref_string(x)


def _label(component):
    """``UNIVERSAL::can($component, 'as_text') ? $component->as_text() : $component``."""
    as_text = getattr(component, "as_text", None)
    return _str(as_text()) if callable(as_text) else perl_string(component)


def _thought(stream, thought):
    if not util.perl_true(thought):
        return None
    fringe = thought.stored_fringe()
    if util.perl_true(fringe):
        fringe = tuple(
            ComponentSnap(perl_string(entry[0]), entry[1],
                          stream.hit_intensity.get(sstream2._key(entry[0])),
                          _label(entry[0]))
            if util.perl_true(entry) else None
            for entry in fringe)
    else:
        fringe = None
    return ThoughtSnap(_str(thought.as_text()), stream.thought_hit_intensity.get(thought),
                       fringe, util.perl_ref(thought))


def _stream():
    stream = Global.MainStream
    if stream is None:
        return StreamSnap()
    return StreamSnap(_thought(stream, stream.current_thought),
                      tuple(_thought(stream, t) for t in stream.older_thoughts))


class _Taker:
    def __init__(self, object_ids):
        self.ids = object_ids if object_ids is not None else {}
        self.keep = []            # keeps id()s valid while the snapshot is being taken
        self.categories = {}      # id(category) (None for undef) -> CategorySnap
        # SCoderack->AttentionDistribution, by id() of the object (scalar keys are dropped).
        self.attention = {id(k): v for k, v in scoderack.attention_distribution().items()
                          if not isinstance(k, str)}

    def oid(self, obj):
        key = id(obj)
        if key not in self.ids:
            self.ids[key] = len(self.ids)
            self.keep.append(obj)
        return self.ids[key]

    def cid(self, cat):
        key = None if cat is None else id(cat)
        if key not in self.categories:
            self.categories[key] = CategorySnap(
                len(self.categories), None if cat is None else _str(cat.get_name()))
            self.keep.append(cat)
        return self.categories[key].cid

    def anchored(self, obj, index):
        is_element = index is not None
        meto = util.perl_true(obj.get_metonym_activeness())
        cats = obj.get_categories()
        return ObjectSnap(
            oid=self.oid(obj),
            is_element=is_element,
            index=index,
            mag=obj.get_mag() if is_element else None,
            left=obj.get_left_edge(),
            right=obj.get_right_edge(),
            span=obj.get_span(),
            items=tuple(self.oid(x) for x in obj),
            strength=obj.get_strength(),
            group_p=bool(util.perl_true(obj.get_group_p())),
            metonym_active=bool(meto),
            structure_string=_str(obj.get_structure_string()),
            starred_structure_string=(
                _str(obj.get_effective_object().get_structure_string()) if meto else None),
            categories=tuple(None if c is None else _str(c.get_name()) for c in cats),
            category_kind=_category_kind(obj),
            categories_as_string=obj.get_categories_as_string(),
            is_locked=bool(util.perl_true(obj.get_is_locked_against_deletion())),
            bounds_string=obj.get_bounds_string(),
            hilit=Global.Hilit.get(obj) or 0,
            attention=self.attention.get(id(obj)) or 0,
            category_ids=tuple(self.cid(c) for c in cats),
        )

    def relation(self, reln):
        first, second = reln.get_ends()
        rtype = reln.get_type()
        return RelationSnap(
            oid=self.oid(reln),
            ends=(self.oid(first), self.oid(second)),
            strength=reln.get_strength(),
            type_text=_str(rtype.as_text()),
            complexity=rtype.get_complexity(),
            perl_class=util.perl_ref(reln),
            hilit=Global.Hilit.get(reln) or 0,
            attention=self.attention.get(id(reln)) or 0,
            end_bounds=(_str(first.get_bounds_string()), _str(second.get_bounds_string())),
            isa_numeric=perl_isa(reln, "Mapping::Numeric"),
            isa_structural=perl_isa(reln, "Mapping::Structural"),
        )


def take(object_ids=None, live=None):
    """Snapshot the current model state.

    ``object_ids``, if given, is a dict filled with ``{id(live_object): oid}`` (for tests).
    ``live``, if given, is filled with the live objects by the tags the views use for them:
    ``"obj<oid>"`` for elements, groups, relations and the other objects they refer to,
    ``"cat<cid>"`` for categories (the runner maps a list click back to the model with it).
    """
    t = _Taker(object_ids)
    live_elements = sworkspace.get_elements()
    live_groups = sworkspace.get_groups()
    live_relations = list(sworkspace.relations.values())
    # Assign oids in the documented order before descending into items and ends.
    for o in live_elements + live_groups + live_relations:
        t.oid(o)
    groups = tuple(t.anchored(g, None) for g in live_groups)
    elements = tuple(t.anchored(e, i) for i, e in enumerate(live_elements))
    snap = Snapshot(
        steps=Global.Steps_Finished or 0,
        current_runnable=_str(Global.CurrentRunnableString) or "",
        debug=bool(util.perl_true(Global.Feature.get("debug"))),
        element_count=sworkspace.ElementCount,
        elements=elements,
        groups=groups,
        relations=tuple(t.relation(r) for r in live_relations),
        bar_lines=tuple(sworkspace.get_bar_lines()),
        slipnet=tuple(_concept(c) for c in sltm.get_top_concepts(10)),
        coderack=CoderackSnap(
            codelets=tuple((c[0], c[1]) for c in scoderack.CODELETS),
            urgencies_sum=scoderack.URGENCIES_SUM,
            history=tuple(scoderack.HistoryOfRunnable.items()),
        ),
        stream=_stream(),
        categories=tuple(t.categories.values()),
    )
    if live is not None:
        by_id = {id(o): o for o in t.keep}
        live.update(("obj%d" % oid, by_id[key]) for key, oid in t.ids.items() if key in by_id)
        live.update(("cat%d" % c.cid, by_id[key]) for key, c in t.categories.items()
                    if key is not None and key in by_id)
    return snap
