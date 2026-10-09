"""Port of SRuleApp.pm (``SRuleApp``): an application of an SRule to a run of workspace
objects (its ``items``), each the rule's transform applied to the one before.

A Moose class. ``SRuleApp({...})`` or kwargs is Moose ``new``: one pass over the
attributes in name order (direction, item, rule), each with its required check and then
its type check (``item``, init_arg ``items``: ArrayRef, default []). BUILD confesses
unless the direction is $DIR::RIGHT.

Naming: ``CheckConsitencyOfGroup`` → ``check_consitency_of_group`` (Perl's typo kept),
``FindExtension`` → ``find_extension``, ``_ExtendOneStep`` → module function
``_extend_one_step``, ``_ExtendSeveralSteps`` → ``_extend_several_steps``,
``ExtendForward/Backward/Right/Left/LeftMaximally`` → ``extend_forward`` etc. The Array
trait handles are ``get_all_items`` (a copy), ``push_item`` and ``unshift_item``;
``get_items`` returns the list itself (Perl's array ref).

Hooks, looked up at call time (tests monkeypatch them):
``_get_something_like(opts)`` and ``_check_at_location(opts)`` (SWorkspace, item 033),
``_element_count()`` ($SWorkspace::ElementCount; delegates to anchored's hook, item 031),
``_sltm_spike_by(amount, concept)`` (SLTM::SpikeBy), ``_scodelet_new(family,
urgency, args)`` (SCodelet->new, item 035) and ``_coderack_add_codelet(codelet)``
(SCoderack->add_codelet, item 036). ``_plonk_into_place`` is not a stub: it is the
PERL-QUIRK below and must keep dying.

PERL-QUIRKs (oracle-confirmed):
- SRuleApp.pm declares only the ApplyMapping/FindMapping multimethods, so the
  ``__PlonkIntoPlace(...)`` call in _ExtendOneStep is an undefined sub. Every extension
  step that gets past check_at_location dies "Undefined subroutine
  &SRuleApp::__PlonkIntoPlace called", and the rest of _ExtendOneStep never runs.
- _ExtendSeveralSteps always extends in the app's own direction (RIGHT, enforced by
  BUILD) with the unflipped transform, so ExtendBackward/ExtendLeft look to the right of
  the *first* item.
- On SErr::ElementsBeyondKnownSought the MaybeAskTheseTerms codelet gets ``exception =>
  $_``, which is the step counter of the enclosing ``for (1 .. $steps)``, not the error.
- get_span is right edge of the last item minus left edge of the first, + 1, so it can
  be ≤ 0 when the items aren't in order.
"""
from seqsee import global_ as Global
from seqsee import util
from seqsee.errors import Confess, ElementsBeyondKnownSought
from seqsee.multimethods import perl_isa


def _get_something_like(opts):
    """Perl: SWorkspace->GetSomethingLike(\\%opts)."""
    from seqsee import sworkspace
    return sworkspace.get_something_like(opts)


def _check_at_location(opts):
    """Perl: SWorkspace->check_at_location(\\%opts)."""
    from seqsee import sworkspace
    return sworkspace.check_at_location(opts)


def _element_count():
    """Perl: $SWorkspace::ElementCount (via anchored's hook, item 031)."""
    from seqsee.objects import anchored
    return anchored._element_count()


def _plonk_into_place(pos, direction, obj):
    """Perl: ``__PlonkIntoPlace(...)`` as seen from package SRuleApp.

    PERL-QUIRK: the multimethod is never imported into SRuleApp, so this always dies.
    """
    raise Confess("Undefined subroutine &SRuleApp::__PlonkIntoPlace called")


def _sltm_spike_by(amount, concept):
    """Perl: SLTM::SpikeBy($amount, $concept)."""
    from seqsee import sltm
    return sltm.spike_by(amount, concept)


def _scodelet_new(family, urgency, args):
    """Perl: SCodelet->new($family, $urgency, \\%args)."""
    from seqsee.scodelet import SCodelet
    return SCodelet(family, urgency, args)


def _coderack_add_codelet(codelet):
    """Perl: SCoderack->add_codelet($codelet)."""
    from seqsee import scoderack
    scoderack.add_codelet(codelet)


def _check_items(value):
    from seqsee.objects.object import _type_error
    if not isinstance(value, list):
        raise _type_error("item", "ArrayRef", value)
    return value


class SRuleApp:
    """Perl: SRuleApp."""

    perl_name = "SRuleApp"

    # (attribute, init_arg, required); visited in attribute-name order by the constructor.
    _ATTRS = (("direction", "direction", True), ("item", "items", False), ("rule", "rule", True))

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess(f"{self.perl_name}: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        for attr, init_arg, required in self._ATTRS:
            if init_arg in kwargs:
                if attr == "item":
                    _check_items(kwargs[init_arg])
            elif required:
                raise Confess(f"Attribute ({attr}) is required")
        self._rule = kwargs["rule"]
        self._item = kwargs["items"] if "items" in kwargs else []
        self._direction = kwargs["direction"]
        self._build()

    def _build(self):
        """Perl: BUILD."""
        from seqsee.constants import DIR
        if self._direction is not DIR.RIGHT:
            raise Confess("Expected direction to be right!")

    # --- attributes ----------------------------------------------------------------------

    def get_rule(self):
        return self._rule

    def set_rule(self, value):
        self._rule = value

    def get_items(self):
        return self._item

    def set_items(self, value):
        self._item = _check_items(value)

    def get_all_items(self):
        return list(self._item)

    def push_item(self, *items):
        self._item.extend(items)
        return len(self._item)

    def unshift_item(self, *items):
        self._item[0:0] = items
        return len(self._item)

    def get_direction(self):
        return self._direction

    def set_direction(self, value):
        self._direction = value

    # --- methods -------------------------------------------------------------------------

    def check_consitency_of_group(self, group):
        """Perl: CheckConsitencyOfGroup($group): 0 only for a group inside the app's span
        that is neither its own underlying reln's group, an item, nor part of an item."""
        left, right = self.get_edges()
        gp_left, gp_right = group.get_edges()
        if not (util.perl_num(gp_left) >= util.perl_num(left)
                and util.perl_num(gp_right) <= util.perl_num(right)):
            return 1
        if group.get_underlying_reln() is self:
            return 1
        for item in self.get_all_items():
            if group is item:
                return 1
            if util.perl_true(item.has_as_part_deep(group)):
                return 1
        return 0

    def find_extension(self, opts):
        """Perl: FindExtension({direction_to_extend_in =>, skip_this_many_elements =>}):
        what SWorkspace->GetSomethingLike finds where the next object should be, or None."""
        from seqsee.constants import DIR
        from seqsee.mapping import apply_mapping
        from seqsee.objects.object import _perl_string
        rule = self.get_rule()
        items = self.get_all_items()
        direction_to_extend_in = opts.get("direction_to_extend_in")
        if not util.perl_true(direction_to_extend_in):
            raise Confess("need direction_to_extend_in")
        skip = opts.get("skip_this_many_elements")
        skip = skip if util.perl_true(skip) else 0
        if skip >= len(items):
            return None
        if direction_to_extend_in is DIR.RIGHT:
            last_object = items[-1 - skip]
            relation_to_use = rule.get_transform()
        else:
            last_object = items[skip]
            relation_to_use = rule.get_flipped_transform()
        if relation_to_use is None:
            return None
        if not perl_isa(relation_to_use, "Mapping"):
            raise Confess(f"Strange transform: {_perl_string(relation_to_use)}")
        next_pos = last_object.get_next_pos_in_dir(direction_to_extend_in)
        if next_pos is None:
            return None
        expected_next_object = apply_mapping(relation_to_use, last_object.get_effective_object())
        if not util.perl_true(expected_next_object):
            return None
        if not len(expected_next_object):
            return None
        return _get_something_like({
            "object": expected_next_object,
            "start": next_pos,
            "direction": direction_to_extend_in,
            "trust_level": 50 * util.perl_num(self.get_span()) / (util.perl_num(_element_count()) + 1),
            "reason": "",
            "hilit_set": list(items),
        })

    def _extend_several_steps(self, extend_at_start_or_end, steps=None):
        """Perl: _ExtendSeveralSteps($start_or_end, $steps): 1 after ``steps`` successful
        steps (then the items are updated), else None."""
        if steps is None:
            steps = 1
        index_of_end = -1 if extend_at_start_or_end == "end" else 0
        direction_to_extend_in = self.get_direction()
        items = self.get_all_items()
        rule = self.get_rule()
        transform = rule.get_transform()
        for step in util.perl_range(1, steps):
            current_end = items[index_of_end] if items else None
            try:
                success = _extend_one_step({
                    "items_ref": items,
                    "direction_to_extend_in": direction_to_extend_in,
                    "object_at_end": current_end,
                    "transform": transform,
                    "extend_at_start_or_end": extend_at_start_or_end,
                })
            except ElementsBeyondKnownSought:
                trust_level = (0.5 * sum(util.perl_num(i.get_span()) for i in items)
                               / util.perl_num(_element_count()))
                if not util.toss(trust_level):
                    return None
                # PERL-QUIRK: $_ here is the loop counter, not the exception.
                _coderack_add_codelet(_scodelet_new("MaybeAskTheseTerms", 10000, {
                    "core": self,
                    "exception": step,
                }))
                return None
            if not util.perl_true(success):
                return None
        self.set_items(list(items))
        Global.update_group_strength_by_consistency()
        return 1

    def extend_forward(self, steps=None):
        """Perl: ExtendForward($steps)."""
        return self._extend_several_steps("end", steps)

    def extend_backward(self, steps=None):
        """Perl: ExtendBackward($steps)."""
        return self._extend_several_steps("start", steps)

    def extend_right(self, steps=None):
        """Perl: ExtendRight($steps)."""
        return self.extend_forward(steps)

    def extend_left(self, steps=None):
        """Perl: ExtendLeft($steps)."""
        return self.extend_backward(steps)

    def extend_left_maximally(self):
        """Perl: ExtendLeftMaximally."""
        while self.extend_left(1):
            pass
        return None

    def as_text(self):
        return "SRuleApp " + util.perl_ref_string(self)

    def get_span(self):
        left, right = self.get_edges()
        return util.perl_num(right) - util.perl_num(left) + 1

    def get_edges(self):
        items = self.get_all_items()
        if not items:
            raise Confess('Can\'t call method "get_left_edge" on an undefined value')
        return items[0].get_left_edge(), items[-1].get_right_edge()


def _extend_one_step(opts):
    """Perl: SRuleApp::_ExtendOneStep(\\%opts): 1 if the next object was found and
    added (with its relation), else None. Dies at __PlonkIntoPlace (see PERL-QUIRKs)."""
    from seqsee.mapping import apply_mapping, find_mapping
    from seqsee.srelation import SRelation
    items_ref = opts.get("items_ref")
    if not util.perl_true(items_ref):
        raise Confess("need items_ref")
    direction_to_extend_in = opts.get("direction_to_extend_in")
    if not util.perl_true(direction_to_extend_in):
        raise Confess("need direction_to_extend_in")
    object_at_end = opts.get("object_at_end")
    if not util.perl_true(object_at_end):
        raise Confess("need object_at_end")
    transform = opts.get("transform")
    if not util.perl_true(transform):
        raise Confess("need transform")
    extend_at_start_or_end = opts.get("extend_at_start_or_end")
    if not util.perl_true(extend_at_start_or_end):
        raise Confess("need extend_at_start_or_end")

    next_pos = object_at_end.get_next_pos_in_dir(direction_to_extend_in)
    if next_pos is None:
        return None
    next_object = apply_mapping(transform, object_at_end.get_effective_object())
    is_this_what_is_present = _check_at_location({
        "start": next_pos,
        "direction": direction_to_extend_in,
        "what": next_object,
    })
    if not util.perl_true(is_this_what_is_present):
        return None

    plonk_result = _plonk_into_place(next_pos, direction_to_extend_in, next_object)
    if not util.perl_true(plonk_result.plonk_was_successful()):
        raise Confess("__PlonkIntoPlace failed. Shouldn't have, I think")
    wso = plonk_result.resultant_object()
    if extend_at_start_or_end == "end":
        items_ref.append(wso)
        found = find_mapping(object_at_end, wso)
        if not util.perl_true(found):
            return None
        reln = SRelation({"first": object_at_end, "second": wso, "type": found})
    elif extend_at_start_or_end == "start":
        items_ref.insert(0, wso)
        found = find_mapping(wso, object_at_end)
        if not util.perl_true(found):
            return None
        reln = SRelation({"first": wso, "second": object_at_end, "type": found})
    else:
        raise Confess("Huh?")
    if not util.perl_true(reln):
        return None
    reln.insert()
    _sltm_spike_by(200, reln)
    return 1
