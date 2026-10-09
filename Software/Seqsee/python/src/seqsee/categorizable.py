"""Port of Categorizable.pm (Moose role): the categories an object belongs to.

Perl keeps ``categories`` as a hash keyed by the stringified category ref, with the
bindings as values. A module-level registry maps those strings back to the category
objects. Here the per-object hash and the registry are keyed by the category object
itself (identity), which is what the ref string stands for. ``category_list_as_strings``
still produces Perl-style ``Class=HASH(0x…)`` strings (``util.perl_ref_string``).

Consumers must provide ``add_history(msg)`` (Perl: AddHistory), as Seqsee::Object does,
and, for ``copy_categories_to``, the target must provide ``describe_as(cat)``.
"""
from seqsee import util
from seqsee.errors import Confess

# Perl: %category_registry ($cat stringified => $cat). Keyed by id(); holding the
# category keeps the id unique. Categories are registered for the life of the program.
_category_registry = {}

_MISSING = object()


def register_category(cat):
    """Perl: Categorizable->RegisterCategory($cat). SCategory calls it after BUILD."""
    _category_registry[id(cat)] = cat


def _registered(cat):
    """Perl: $category_registry{$cat}; None (undef) when the category was never registered."""
    return _category_registry.get(id(cat))


class Categorizable:
    """Perl role Categorizable. A mixin; the categories hash is created on first use."""

    def get_cats_hash(self):
        """Perl: get_cats_hash (the ``categories`` attribute): {category: bindings}, live."""
        try:
            return self._categories
        except AttributeError:
            self._categories = {}
            return self._categories

    def add_category(self, cat, bindings=_MISSING):
        """Perl: add_category (Hash 'set'), then AddHistory "Added category <name>".

        Returns the bindings. Moose dies without a value, before anything is set."""
        if bindings is _MISSING:
            raise Confess("You must pass an even number of arguments to set")
        self.get_cats_hash()[cat] = bindings
        self.add_history("Added category " + util.perl_str(cat.get_name()))
        return bindings

    def remove_category(self, cat):
        """Perl: AddHistory "Removed category <name>" (even if absent), then Hash 'delete'.

        Returns the removed bindings, or None."""
        self.add_history("Removed category " + util.perl_str(cat.get_name()))
        return self.get_cats_hash().pop(cat, None)

    def is_of_category_p(self, cat):
        """Perl: is_of_category_p (Hash 'get'): the bindings, or None."""
        return self.get_cats_hash().get(cat)

    def get_binding_for_category(self, cat):
        """Perl: GetBindingForCategory (Hash 'get'), same as is_of_category_p."""
        return self.get_cats_hash().get(cat)

    def category_list_as_strings(self):
        """Perl: category_list_as_strings (Hash 'keys'): stringified category refs.

        Perl returns them in hash order; this uses insertion order."""
        return [util.perl_ref_string(c) for c in self.get_cats_hash()]

    def get_categories(self):
        """Perl: get_categories: registry lookups, so unregistered categories give None."""
        return [_registered(c) for c in self.get_cats_hash()]

    def get_categories_as_string(self):
        """Perl: get_categories_as_string (the ref strings joined with ', ')."""
        return ", ".join(self.category_list_as_strings())

    def has_non_ad_hoc_category(self):
        """Perl: HasNonAdHocCategory: 1 if some category's ref string lacks 'Interlaced'.

        The test is on the Perl class name inside the ref string, so Interlaced categories
        need a ``perl_name`` containing 'Interlaced'."""
        for s in self.category_list_as_strings():
            if "Interlaced" not in s:
                return 1
        return 0

    def copy_categories_to(self, to):
        """Perl: CopyCategoriesTo: ``to.describe_as`` every category; 1 if all succeeded, else 0."""
        any_failure_so_far = 0
        for category in self.get_categories():
            if not util.perl_true(to.describe_as(category)):
                any_failure_so_far += 1
        return 0 if any_failure_so_far else 1

    def get_common_categories(self, *others):
        """Perl: ``$object->get_common_categories(@others)``, the module function
        called as a method (the object is the first argument)."""
        return get_common_categories(self, *others)


def get_common_categories(*objects):
    """Perl: Categorizable::get_common_categories(@objects): categories shared by all.

    Confesses on a non-object argument and on a shared category that was never
    registered. Perl returns hash order; this returns first-seen order."""
    count = len(objects)
    key_count = {}
    seen = {}
    for obj in objects:
        if util._is_scalar(obj):
            raise Confess(f"Funny arg {util.perl_str(obj)}")
        for cat in obj.get_cats_hash():
            key_count[id(cat)] = key_count.get(id(cat), 0) + 1
            seen[id(cat)] = cat
    result = []
    for key, n in key_count.items():
        if n != count:
            continue
        cat = _category_registry.get(key)
        if cat is None:
            known = ", ".join(util.perl_ref_string(c) for c in _category_registry.values())
            raise Confess(f"not a cat: {util.perl_ref_string(seen[key])}\ncats known:\n{known}")
        result.append(cat)
    return result
