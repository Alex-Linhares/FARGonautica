"""Port of Seqsee/Anchored.pm (``Seqsee::Anchored``, extends Seqsee::Object): an object
with a place in the workspace (left and right edges).

Moose attributes: ``left_edge`` and ``right_edge`` (required, untyped) and
``is_locked_against_deletion`` (Bool). Constructor checks: see SeqseeObject._ATTRS.

Naming: ``IsFlushRight`` → ``is_flush_right``, ``IsFlushLeft`` → ``is_flush_left``,
``_CheckValidity`` → ``_check_validity``, ``Extend`` → ``extend``, ``SafeExtend`` →
``safe_extend``, ``Update`` → ``update``, ``FindExtension`` → ``find_extension``.

SWorkspace's pieces go through hooks here, looked up at call time (tests monkeypatch them;
they call seqsee.sworkspace):
``_element_count`` ($SWorkspace::ElementCount), ``_find_groups_conflicting_with``
(SWorkspace::__FindGroupsConflictingWith), ``_get_super_groups``
(SWorkspace->GetSuperGroups, a list), ``_delete_group`` (SWorkspace::__DeleteGroup),
``_remove_gp`` (SWorkspace->remove_gp) and ``_update_group`` (SWorkspace::__UpdateGroup).

PERL-QUIRKs (oracle-confirmed):
- create of a single item returns that item itself (after UpdateStrength), even when
  called on a subclass.
- SafeExtend returns 1 when Extend gives up (unresolved conflict, or supergroups that
  survive the toss): only CouldNotCreateExtendedGroup gives 0.
- get_bounds_string has a space on both sides (" <l, r> "), so as_text and the
  "Extended to become" history line have double spaces.
"""
from seqsee import util
from seqsee.errors import SErr, Confess, CouldNotCreateExtendedGroup
from seqsee.multimethods import perl_isa
from seqsee.objects.object import SeqseeObject, _check, _perl_string


def _element_count():
    """Perl: $SWorkspace::ElementCount."""
    from seqsee import sworkspace
    return sworkspace.ElementCount


def _find_groups_conflicting_with(group):
    """Perl: SWorkspace::__FindGroupsConflictingWith($group)."""
    from seqsee import sworkspace
    return sworkspace.find_groups_conflicting_with(group)


def _get_super_groups(group):
    """Perl: SWorkspace->GetSuperGroups($group): a list."""
    from seqsee import sworkspace
    return sworkspace.get_super_groups(group)


def _delete_group(group):
    """Perl: SWorkspace::__DeleteGroup($group)."""
    from seqsee import sworkspace
    return sworkspace.delete_group(group)


def _remove_gp(group):
    """Perl: SWorkspace->remove_gp($group)."""
    from seqsee import sworkspace
    return sworkspace.remove_gp(group)


def _update_group(group):
    """Perl: SWorkspace::__UpdateGroup($group)."""
    from seqsee import sworkspace
    return sworkspace.update_group(group)


class Anchored(SeqseeObject):
    """Perl: Seqsee::Anchored."""

    perl_name = "Seqsee::Anchored"

    _ATTRS = SeqseeObject._ATTRS + (
        ("is_locked_against_deletion", "is_locked_against_deletion", False),
        ("left_edge", "left_edge", True),
        ("right_edge", "right_edge", True),
    )

    def _init_attributes(self, kwargs):
        super()._init_attributes(kwargs)
        self._left_edge = kwargs["left_edge"]
        self._right_edge = kwargs["right_edge"]
        self._is_locked_against_deletion = kwargs.get("is_locked_against_deletion")

    # --- attributes ----------------------------------------------------------------------

    def get_left_edge(self):
        return self._left_edge

    def set_left_edge(self, value):
        self._left_edge = value

    def get_right_edge(self):
        return self._right_edge

    def set_right_edge(self, value):
        self._right_edge = value

    def get_is_locked_against_deletion(self):
        return self._is_locked_against_deletion

    def set_is_locked_against_deletion(self, value):
        self._is_locked_against_deletion = _check("is_locked_against_deletion", value)

    # --- edges ---------------------------------------------------------------------------

    def set_edges(self, left, right):
        """Perl: set_edges($left, $right); returns self."""
        self.set_left_edge(left)
        self.set_right_edge(right)
        return self

    def get_edges(self):
        """Perl: get_edges: the list (left, right)."""
        return (self.get_left_edge(), self.get_right_edge())

    def get_bounds_string(self):
        """Perl: get_bounds_string: " <l, r> "."""
        return f" <{util.perl_str(self.get_left_edge())}, {util.perl_str(self.get_right_edge())}> "

    def get_span(self):
        return util.perl_num(self.get_right_edge()) - util.perl_num(self.get_left_edge()) + 1

    def as_text(self):
        """Perl: as_text: "Seqsee::Anchored <u><bounds> <annotated structure>"."""
        bounds_string = self.get_bounds_string()
        structure_string = util.perl_str(self.get_annotated_structure_string())
        ruleapp = "u" if util.perl_true(self.get_underlying_reln()) else ""
        return f"Seqsee::Anchored {ruleapp}{bounds_string} {structure_string}"

    def get_next_pos_in_dir(self, direction):
        """Perl: get_next_pos_in_dir: right edge + 1 for RIGHT; left edge - 1 for LEFT, or
        None at the left end; confesses for any other direction."""
        from seqsee.constants import DIR
        if direction is DIR.RIGHT:
            return util.perl_num(self.get_right_edge()) + 1
        if direction is DIR.LEFT:
            le = util.perl_num(self.get_left_edge())
            if not le > 0:
                return None
            return le - 1
        raise Confess("funny direction to extnd in!!")

    def spans(self, other):
        """Perl: spans: other lies within self."""
        sl, sr = self.get_edges()
        ol, or_ = other.get_edges()
        return sl <= ol and or_ <= sr

    def overlaps(self, other):
        sl, sr = self.get_edges()
        ol, or_ = other.get_edges()
        return (sr <= or_ and sr >= ol) or (or_ <= sr and or_ >= sl)

    def is_flush_right(self):
        """Perl: IsFlushRight: 1 if the right edge is the workspace's last element."""
        return 1 if self.get_right_edge() == _element_count() - 1 else 0

    def is_flush_left(self):
        """Perl: IsFlushLeft."""
        return 1 if self.get_left_edge() == 0 else 0

    def recalculate_edges(self):
        """Perl: recalculate_edges: from the first and last items."""
        subobjects = self.get_items()
        self.set_left_edge(subobjects[0].get_left_edge())
        self.set_right_edge(subobjects[-1].get_right_edge())

    # --- create ----------------------------------------------------------------------------

    @staticmethod
    def _check_validity(*items):
        """Perl: _CheckValidity(@items): 1 if the items are anchored, adjacent and in order;
        None otherwise. Throws SErr "EmptyCreate" for no items, and confesses for an
        unanchored item (checked as it is reached)."""
        if not items:
            SErr.throw("EmptyCreate")
        first_item, rest = items[0], items[1:]
        if not perl_isa(first_item, "Seqsee::Anchored"):
            raise Confess(f"Unanchored object {_perl_string(first_item)}")
        most_recent_right_edge = first_item.get_right_edge()
        for next_item in rest:
            if not util.perl_true(next_item):  # while (my $next_item = shift(@items))
                break
            if not perl_isa(next_item, "Seqsee::Anchored"):
                raise Confess(f"Unanchored object {_perl_string(next_item)}")
            if not next_item.get_left_edge() == most_recent_right_edge + 1:
                return None
            most_recent_right_edge = next_item.get_right_edge()
        return 1

    @classmethod
    def create(cls, *items):
        """Perl: create(@items): None unless _check_validity; a single item is returned
        itself; else a new group (group_p 1) spanning the items. UpdateStrength either way."""
        if not Anchored._check_validity(*items):
            return None
        if len(items) == 1:
            anchored_object = items[0]
        else:
            anchored_object = cls({
                "left_edge": items[0].get_left_edge(),
                "right_edge": items[-1].get_right_edge(),
                "items": list(items),
                "group_p": 1,
            })
        anchored_object.update_strength()
        return anchored_object

    # --- extension ---------------------------------------------------------------------------

    def extend(self, *args):
        """Perl: Extend($to_insert, $insert_at_end_p): 1 on success, None if a conflict
        can't be resolved or the supergroups survive; throws CouldNotCreateExtendedGroup
        if the extended items aren't a valid group."""
        if len(args) != 2:
            raise Confess("Need 3 arguments")
        to_insert, insert_at_end_p = args
        current_subobjects = self.get_items_array()
        if util.perl_true(insert_at_end_p):
            new_subobjects = current_subobjects + [to_insert]
        else:
            new_subobjects = [to_insert] + current_subobjects

        potential_new_group = Anchored.create(*new_subobjects)
        if not util.perl_true(potential_new_group):
            # Perl: ->new("...")->throw(); throw on an instance rethrows it.
            raise CouldNotCreateExtendedGroup("Extended group creation failed")
        conflicts = _find_groups_conflicting_with(potential_new_group)
        if util.perl_true(conflicts):
            if not util.perl_true(conflicts.resolve({"IgnoreConflictWith": self})):
                return None

        # If there are supergroups, they must die. Kludge, for now:
        supergps = list(_get_super_groups(self))
        if supergps:
            if util.toss(0.5):
                for gp in supergps:
                    _delete_group(gp)
            else:
                return None

        # If we get here, all conflicting incumbents are dead.
        self.get_parts_ref()[:] = new_subobjects
        self.update()
        self.add_history("Extended to become " + self.get_bounds_string())
        return 1

    def safe_extend(self, *args):
        """Perl: SafeExtend: Extend, but 0 for CouldNotCreateExtendedGroup. Other errors
        propagate. PERL-QUIRK: 1 whenever Extend doesn't throw, even if it gave up."""
        if len(args) != 2:
            raise Confess("Need 3 arguments")
        try:
            self.extend(*args)
        except CouldNotCreateExtendedGroup:
            return 0
        return 1

    def update(self):
        """Perl: Update: recalculate edges, categories, relations and strength; refresh the
        underlying rule app (on failure the group is removed from the workspace and None
        returned); then SWorkspace::__UpdateGroup, whose value is returned."""
        self.recalculate_edges()
        self.recalculate_categories()
        self.recalculate_relations()
        self.update_strength()
        underlying_reln = self.get_underlying_reln()
        if util.perl_true(underlying_reln):
            error = False
            try:
                self.set_underlying_ruleapp(underlying_reln.get_rule())
            except NotImplementedError:
                raise  # an unported stub, not a Perl die: don't hide it
            except Exception:  # Try::Tiny catches every die
                _remove_gp(self)
                error = True
            if error:
                return None
            if not util.perl_true(self.get_underlying_reln()):
                raise Confess("underlying_reln lost here")
        return _update_group(self)

    def find_extension(self, *args):
        """Perl: FindExtension($direction_to_extend_in, $skip): the underlying rule app's
        FindExtension, or None without one."""
        if len(args) != 2:
            raise Confess("FindExtension for an object requires 3 args")
        direction_to_extend_in, skip = args
        underlying_ruleapp = self.get_underlying_reln()
        if not util.perl_true(underlying_ruleapp):
            return None
        return underlying_ruleapp.find_extension({
            "direction_to_extend_in": direction_to_extend_in,
            "skip_this_many_elements": skip,
        })
