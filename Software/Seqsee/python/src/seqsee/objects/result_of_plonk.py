"""Port of Seqsee/ResultOfPlonk.pm (``Seqsee::ResultOfPlonk``): what
SWorkspace::__PlonkIntoPlace returns.

A Moose class. ``ResultOfPlonk({...})``/kwargs is Moose ``new``: one pass over the
attributes in name order, each with its required check and then its type check:
``attribute_copy_result`` (required, a ResultOfAttributeCopy), ``object_being_plonked``
(required, a Seqsee::Object, weak) and ``resultant_object`` (a Seqsee::Object, weak,
with the predicate ``has_resultant_object``). Each accessor reads with no argument and
type-checks and sets with one, like the Perl rw accessors.

Naming: ``Failed`` keeps its Perl name, ``PlonkWasSuccessful`` → ``plonk_was_successful``,
``AttributeCopyWasSuccessful`` → ``attribute_copy_was_successful`` (a delegation to the
copy result's ``success``, so with an argument it sets it). The ``bool`` overload is
``has_resultant_object``; with ``fallback => 1`` the string form is that value too
("1" or "").

PERL-QUIRK (oracle-confirmed): the predicate stays true after a weakly held resultant
object has been freed, so the plonk still counts as successful with an undef result.
"""
from seqsee import util
from seqsee.errors import Confess
from seqsee.multimethods import perl_isa
from seqsee.objects.object import _moose_new_args, _moose_weaken, _type_error

_MISSING = object()


def _required(attr):
    return Confess(f"Attribute ({attr}) is required")


def _check_object(attr, value):
    if not perl_isa(value, "Seqsee::Object"):
        raise _type_error(attr, "Seqsee::Object", value)


def _check_copy_result(value):
    from seqsee.objects.result_of_attribute_copy import ResultOfAttributeCopy
    if not isinstance(value, ResultOfAttributeCopy):
        raise _type_error("attribute_copy_result", "Seqsee::ResultOfAttributeCopy", value)


class ResultOfPlonk:
    """Perl: Seqsee::ResultOfPlonk."""

    perl_name = "Seqsee::ResultOfPlonk"

    def __init__(self, *args, **kwargs):
        kwargs = _moose_new_args(self.perl_name, args, kwargs)
        for attr in ("attribute_copy_result", "object_being_plonked"):
            if attr not in kwargs:
                raise _required(attr)
            if attr == "attribute_copy_result":
                _check_copy_result(kwargs[attr])
            else:
                _check_object(attr, kwargs[attr])
        if "resultant_object" in kwargs:
            _check_object("resultant_object", kwargs["resultant_object"])
        self._attribute_copy_result = kwargs["attribute_copy_result"]
        self._object_being_plonked = _moose_weaken(kwargs["object_being_plonked"])
        self._resultant_object = (_moose_weaken(kwargs["resultant_object"])
                                  if "resultant_object" in kwargs else _MISSING)

    @classmethod
    def Failed(cls, object_being_plonked):  # noqa: N802 (Perl name)
        """Perl: Failed($object): no resultant object, and a failed attribute copy."""
        from seqsee.objects.result_of_attribute_copy import ResultOfAttributeCopy
        return cls(object_being_plonked=object_being_plonked,
                   attribute_copy_result=ResultOfAttributeCopy.Failed())

    def object_being_plonked(self, *args):
        if args:
            _check_object("object_being_plonked", args[0])
            self._object_being_plonked = _moose_weaken(args[0])
            return args[0]
        return self._object_being_plonked()

    def resultant_object(self, *args):
        if args:
            _check_object("resultant_object", args[0])
            self._resultant_object = _moose_weaken(args[0])
            return args[0]
        return None if self._resultant_object is _MISSING else self._resultant_object()

    def has_resultant_object(self):
        """Perl: the predicate (true once set, even if the object has since been freed)."""
        return self._resultant_object is not _MISSING

    def attribute_copy_result(self, *args):
        if args:
            _check_copy_result(args[0])
            self._attribute_copy_result = args[0]
        return self._attribute_copy_result

    def attribute_copy_was_successful(self, *args):
        """Perl: AttributeCopyWasSuccessful (handles => the copy result's ``success``)."""
        return self._attribute_copy_result.success(*args)

    def plonk_was_successful(self):
        """Perl: PlonkWasSuccessful."""
        return self.has_resultant_object()

    def __bool__(self):
        return self.has_resultant_object()

    def __str__(self):
        return util.perl_str(self.has_resultant_object())
