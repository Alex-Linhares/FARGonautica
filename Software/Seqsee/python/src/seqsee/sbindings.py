"""Port of lib/SBindings.pm: how an object is an instance of a category.

Holds the attribute bindings (e.g. ``start``, ``end``, ``length``) and the raw
slippages (absolute position -> SMetonym), from which BUILD derives the
metonymy mode, position mode, position and metonymy type.
"""
from seqsee.constants import METO_MODE, POS_MODE
from seqsee.errors import Confess
from seqsee.smetonym import SMetonym
from seqsee.spos import SPos
from seqsee.util import perl_num

_MISSING = object()


def _check(attr, value, cls):
    """Moose ``isa`` check for the rw attributes (undef is rejected too)."""
    if not isinstance(value, cls):
        raise Confess(f"Attribute ({attr}) does not pass the type constraint because: "
                      f"Validation failed for '{cls.__name__}' with value {value!r}")
    return value


class SBindings:
    """Perl: SBindings.

    ``SBindings(bindings=..., raw_slippages=...)`` or ``SBindings({...})``
    (Perl ``new``). Unknown init args (MappingBased passes ``object``) are
    ignored, as Moose does. The two hashes are kept, not copied.
    """

    def __init__(self, *args, **kwargs):
        if len(args) == 1 and isinstance(args[0], dict) and not kwargs:
            kwargs = dict(args[0])
        elif args:
            raise Confess("SBindings: odd arguments to constructor")
        for init_arg in ("bindings", "raw_slippages"):
            if init_arg not in kwargs:
                attr = "bindings_ref" if init_arg == "bindings" else "squinting_raw"
                raise Confess(f"Attribute ({attr}) is required")
            if not isinstance(kwargs[init_arg], dict):
                raise Confess(f"Attribute ({init_arg}) does not pass the type constraint "
                              f"because: Validation failed for 'HashRef[Any]'")
        self._bindings_ref = kwargs["bindings"]
        self._squinting_raw = kwargs["raw_slippages"]
        self._metonymy_mode = self._position_mode = self._position = None
        self._metonymy_type = None
        for attr in ("metonymy_mode", "position_mode", "position", "metonymy_type"):
            if kwargs.get(attr, _MISSING) is not _MISSING:
                setattr(self, attr, kwargs[attr])
        self._build()

    def _build(self):
        """Perl: BUILD."""
        if self.slippages_count() == 0:
            self.metonymy_mode = METO_MODE.NONE
            return
        # PERL-QUIRK: `$self->metonymy_type(SMetonym->intersection(...))` is called in
        # list context, so a failed intersection (`return;`) calls the accessor with
        # no args, i.e. as a getter: any constructor-supplied type is kept.
        typ = SMetonym.intersection(*self.all_slippages())
        if typ is not None:
            self.metonymy_type = typ
        if self.slippages_count() == 1:
            self.metonymy_mode = METO_MODE.SINGLE
            self.position_mode = POS_MODE.FORWARD
            # Perl hash keys are strings: "2.0" + 1 is 3, "abc" + 1 is 1.
            pos = perl_num(self.slippage_positions()[0]) + 1
            if isinstance(pos, float) and pos.is_integer():
                pos = int(pos)
            self.position = SPos(pos)

    @classmethod
    def create(cls, slippage_ref=None, bindings_ref=None, obj=None):
        """Perl: SBindings->create($slippage_ref, $bindings_ref, $object). The object is ignored."""
        if slippage_ref is None or bindings_ref is None:
            raise Confess("Need two args (plus possibly a third arg (ignored)!")
        return cls(raw_slippages=slippage_ref, bindings=bindings_ref)

    # --- bindings_ref ---------------------------------------------------------
    def get_bindings_ref(self):
        return self._bindings_ref

    def get_binding_for_attribute(self, attribute):
        """Perl: GetBindingForAttribute."""
        return self._bindings_ref.get(attribute)

    # --- squinting_raw ----------------------------------------------------------
    def get_squinting_raw(self):
        return self._squinting_raw

    def slippages_count(self):
        return len(self._squinting_raw)

    def all_slippages(self):
        return list(self._squinting_raw.values())

    def slippage_positions(self):
        return list(self._squinting_raw.keys())

    # --- rw attributes (Perl writer `metonymy_mode(x)`, reader `get_metonymy_mode`) ---
    @property
    def metonymy_mode(self):
        return self._metonymy_mode

    @metonymy_mode.setter
    def metonymy_mode(self, value):
        self._metonymy_mode = _check("metonymy_mode", value, METO_MODE)

    def get_metonymy_mode(self):
        return self._metonymy_mode

    @property
    def position_mode(self):
        return self._position_mode

    @position_mode.setter
    def position_mode(self, value):
        self._position_mode = _check("position_mode", value, POS_MODE)

    def get_position_mode(self):
        return self._position_mode

    @property
    def position(self):
        return self._position

    @position.setter
    def position(self, value):
        self._position = _check("position", value, SPos)

    def get_position(self):
        return self._position

    @property
    def metonymy_type(self):
        return self._metonymy_type

    @metonymy_type.setter
    def metonymy_type(self, value):
        self._metonymy_type = value  # isa Any

    def get_metonymy_type(self):
        return self._metonymy_type

    def _delegate(self, method):
        typ = self._metonymy_type
        if typ is None:
            raise Confess(f"Cannot delegate to {method} because the value of "
                          f"metonymy_type is not defined")
        fn = getattr(typ, method, None)
        if fn is None:
            raise Confess(f'Can\'t call method "{method}" on {typ!r}')
        return fn()

    def get_metonymy_cat(self):
        """Delegates to metonymy_type->get_category."""
        return self._delegate("get_category")

    def get_metonymy_name(self):
        """Delegates to metonymy_type->get_name."""
        return self._delegate("get_name")

    # --- no-ops ---------------------------------------------------------------------
    def tell_directed_story(self, *args):
        """Perl: TellDirectedStory (NO-OP)."""

    def tell_backward_story(self, *args):
        """NO-OP."""

    def tell_forward_story(self, *args):
        """NO-OP."""
