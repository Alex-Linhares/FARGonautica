"""Port of Seqsee/ResultOfGetSomethingLike.pm (``Seqsee::ResultOfGetSomethingLike``): what
SWorkspace->GetSomethingLike returns.

A Class::Std class with four ``:name<...>`` attributes (to_ask, literally_present,
probable_matches, potential_matches): each is a required init arg with ``get_X`` and
``set_X`` accessors. ``set_X`` returns the old value and dies without a new value.

``ResultOfGetSomethingLike({...})`` is Class::Std's ``new``, including its error
messages. Like Class::Std, it also reads args nested under the class name, and its
"Did you mislabel" hint lists the duplicates of the passed keys, once per missing
attribute (Class::Std's ``uniq`` keeps duplicates, not distinct names). The keys come in
dict order here and in Perl hash order there.
"""
from seqsee.errors import Confess

_ATTRS = ("to_ask", "literally_present", "probable_matches", "potential_matches")


def _mislabelled(keys):
    """Class::Std::_mislabelled(@keys), with its ``uniq`` (which returns repeats)."""
    seen = {}
    repeats = []
    for k in keys:
        if seen.get(k):
            repeats.append(k)
        seen[k] = seen.get(k, 0) + 1
    names = [f"'{k}'" for k in repeats]
    if not names:
        return []
    if len(names) == 1:
        arglist = names[0]
    elif len(names) == 2:
        arglist = " or ".join(names)
    else:
        arglist = ", ".join(names[:-1]) + f", or {names[-1]}"
    return [f"(Did you mislabel one of the args you passed: {arglist}?)"]


class ResultOfGetSomethingLike:
    """Perl: Seqsee::ResultOfGetSomethingLike."""

    perl_name = "Seqsee::ResultOfGetSomethingLike"

    def __init__(self, *args):
        if args and not isinstance(args[0], dict):
            raise Confess(f"Argument to {self.perl_name}->new() must be hash reference")
        arg_ref = args[0] if args else {}
        arg_set = {**arg_ref, **(arg_ref.get(self.perl_name) or {})}
        missing, suss_keys = [], []
        self._values = {}
        for attr in _ATTRS:
            if attr in arg_set:
                self._values[attr] = arg_set[attr]
            else:
                missing.append(f"Missing initializer label for {self.perl_name}: '{attr}'.")
                suss_keys.extend(arg_set)
        if missing:
            raise Confess("\n".join(missing + _mislabelled(suss_keys)
                                    + ["Fatal error in constructor call"]))

    def _set(self, attr, args):
        if not args:
            raise Confess(f"Missing new value in call to 'set_{attr}' method")
        old = self._values[attr]
        self._values[attr] = args[0]
        return old

    def get_to_ask(self, *args):
        return self._values["to_ask"]

    def set_to_ask(self, *args):
        return self._set("to_ask", args)

    def get_literally_present(self, *args):
        return self._values["literally_present"]

    def set_literally_present(self, *args):
        return self._set("literally_present", args)

    def get_probable_matches(self, *args):
        return self._values["probable_matches"]

    def set_probable_matches(self, *args):
        return self._set("probable_matches", args)

    def get_potential_matches(self, *args):
        return self._values["potential_matches"]

    def set_potential_matches(self, *args):
        return self._set("potential_matches", args)
