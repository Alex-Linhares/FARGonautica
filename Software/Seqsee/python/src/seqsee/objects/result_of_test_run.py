"""Port of Seqsee/ResultOfTestRun.pm: the outcome of one test run of Seqsee.

Three Class::Std::Storable classes:

- ``TestOutputStatus`` (``:name<status_string>``) with the six shared statuses
  ``Successful``, ``RanOutOfTerms``, ``InitialBlemish``, ``ExtendedABit``,
  ``NotEvenExtended`` and ``Crashed`` (Perl ``$TestOutputStatus::Successful`` …), and
  ``IsSuccess``/``IsAtLeastAnExtension``/``IsACrash`` → ``is_success``/
  ``is_at_least_an_extension``/``is_a_crash`` (1 or 0, compared by status string).
- ``ResultOfTestRun`` (Perl ``Seqsee::ResultOfTestRun``): ``status``, ``steps``, ``error``.
- ``ResultsOfTestRuns``: ``times``, ``results``, ``rate``, ``terms``, ``features``,
  ``version`` (required), plus ``is_ltm_result`` (default 0) and ``context`` (default '')
  set by BUILD from the constructor args.

``:name<x>`` attributes are required init args (``exists`` is enough, undef is fine) with
``get_x`` and ``set_x`` (returns the old value; dies without a value). Class::Std's
constructor errors are reproduced as in ``result_of_get_something_like``. Storable
(freeze/thaw) is Python's pickle.
"""
from seqsee.errors import Confess
from seqsee.objects.result_of_get_something_like import _mislabelled
from seqsee.util import perl_true


class _ClassStd:
    """A Class::Std class whose ``_ATTRS`` are ``:name<...>`` attributes."""

    perl_name = None
    _ATTRS = ()

    def __init__(self, *args):
        if args and not isinstance(args[0], dict):
            raise Confess(f"Argument to {self.perl_name}->new() must be hash reference")
        arg_ref = args[0] if args else {}
        arg_set = {**arg_ref, **(arg_ref.get(self.perl_name) or {})}
        missing, suss_keys = [], []
        self._values = {}
        for attr in self._ATTRS:
            if attr in arg_set:
                self._values[attr] = arg_set[attr]
            else:
                missing.append(f"Missing initializer label for {self.perl_name}: '{attr}'.")
                suss_keys.extend(arg_set)
        if missing:
            raise Confess("\n".join(missing + _mislabelled(suss_keys)
                                    + ["Fatal error in constructor call"]))
        self._build(arg_set)

    def _build(self, arg_set):
        """Perl BUILD."""

    def _get(self, attr):
        return self._values.get(attr)

    def _set(self, attr, args):
        if not args:
            raise Confess(f"Missing new value in call to 'set_{attr}' method")
        old = self._values.get(attr)
        self._values[attr] = args[0]
        return old


def _accessors(cls):
    """Add ``get_x``/``set_x`` for each attribute in ``cls._ATTRS``."""
    for attr in cls._ATTRS:
        setattr(cls, f"get_{attr}", lambda self, *a, _attr=attr: self._get(_attr))
        setattr(cls, f"set_{attr}", lambda self, *a, _attr=attr: self._set(_attr, a))
    return cls


@_accessors
class TestOutputStatus(_ClassStd):
    """Perl: TestOutputStatus."""

    __test__ = False  # not a pytest class
    perl_name = "TestOutputStatus"
    _ATTRS = ("status_string",)

    def is_success(self):
        """Perl: IsSuccess."""
        return 1 if self.get_status_string() == "Successful" else 0

    def is_at_least_an_extension(self):
        """Perl: IsAtLeastAnExtension."""
        status_string = self.get_status_string()
        for s in ("Successful", "RanOutOfTerms", "InitialBlemish", "ExtendedABit"):
            if status_string == s:
                return 1
        return 0

    def is_a_crash(self):
        """Perl: IsACrash."""
        return 1 if self.get_status_string() == "Crashed" else 0


Successful = TestOutputStatus({"status_string": "Successful"})
RanOutOfTerms = TestOutputStatus({"status_string": "RanOutOfTerms"})
InitialBlemish = TestOutputStatus({"status_string": "InitialBlemish"})
ExtendedABit = TestOutputStatus({"status_string": "ExtendedABit"})
NotEvenExtended = TestOutputStatus({"status_string": "NotEvenExtended"})
Crashed = TestOutputStatus({"status_string": "Crashed"})


@_accessors
class ResultOfTestRun(_ClassStd):
    """Perl: Seqsee::ResultOfTestRun."""

    perl_name = "Seqsee::ResultOfTestRun"
    _ATTRS = ("status", "steps", "error")


@_accessors
class ResultsOfTestRuns(_ClassStd):
    """Perl: ResultsOfTestRuns."""

    perl_name = "ResultsOfTestRuns"
    _ATTRS = ("times", "results", "rate", "terms", "features", "version")

    def _build(self, arg_set):
        ltm, context = arg_set.get("is_ltm_result"), arg_set.get("context")
        self._values["is_ltm_result"] = ltm if perl_true(ltm) else 0
        self._values["context"] = context if perl_true(context) else ""

    def get_is_ltm_result(self, *args):
        return self._get("is_ltm_result")

    def set_is_ltm_result(self, *args):
        return self._set("is_ltm_result", args)

    def get_context(self, *args):
        return self._get("context")

    def set_context(self, *args):
        return self._set("context", args)
