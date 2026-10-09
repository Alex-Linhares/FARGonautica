"""Port of SChoose.pm: weighted random choice.

Perl calls look like ``SChoose->choose(\\@weights, \\@names)``; here they are
module functions (``schoose.choose(weights, names)``). Every draw is one
``util.rand()`` call, made in the same order as the Perl, so seeded runs match
the Perl draw for draw.
"""
from seqsee.errors import Confess
from seqsee.util import perl_num, perl_true, rand


def _sum(numbers):
    """List::Util::sum: None (undef) for an empty list."""
    if not numbers:
        return None
    return sum(numbers)


def _pick(numbers, total):
    """The shared loop of choose/choose_if_non_zero/choose_a_few_nonzero:
    returns the index chosen (the last index if nothing is chosen earlier)."""
    random = rand() * total
    idx = -1
    for x in numbers:
        idx += 1
        if x > random:
            break
        random -= x
    return idx


def _at(items, idx):
    """Perl ``$ref->[$idx]``: undef (None) past the end."""
    return items[idx] if idx < len(items) else None


def create(opts=None, **kwargs):
    """Perl: SChoose->create({ map => ..., grep => ... }).

    Returns a chooser: a function taking a list of objects and returning one,
    chosen with likelihood ``map(obj)`` (or the object itself, if no map), among
    the objects for which ``grep(obj)`` is true. ``map``/``grep`` must be
    callables taking the object. The Perl also accepts strings of Perl code
    using ``$_``; callers of the port pass an equivalent callable instead.

    Behaviour (faithful to the generated Perl sub):
      * empty list -> None, no draw;
      * with grep: no object passes -> None, no draw; some pass but the
        likelihood sum is 0 -> uniform among the passing objects (one draw);
      * likelihood sum non-zero -> first object whose partial sum is >= the
        draw. PERL-QUIRK: with negative likelihoods no partial sum may qualify;
        the Perl sub then falls off its end and returns "" (the last failed
        comparison), and so does the port;
      * likelihood sum 0 -> uniform among all objects.
    """
    opts = dict(opts or {}, **kwargs)
    map_fn = opts.get("map")
    grep_fn = opts.get("grep")
    for name, fn in (("map", map_fn), ("grep", grep_fn)):
        if fn is not None and not callable(fn):
            raise Confess(f"SChoose.create: {name} must be a callable, got {fn!r}")

    def chooser(objects):
        if not objects:
            return None
        likelihood_sum = 0
        partial_sums = []
        grep_pass_count = 0
        grep_pass_array = []
        for obj in objects:
            likelihood = perl_num(map_fn(obj) if map_fn is not None else obj)
            if grep_fn is not None:
                if perl_true(grep_fn(obj)):
                    grep_pass_count += 1
                else:
                    likelihood = 0
                grep_pass_array.append(grep_pass_count)
            likelihood_sum += likelihood
            partial_sums.append(likelihood_sum)

        if grep_fn is not None:
            if grep_pass_count and not likelihood_sum:
                random = rand() * grep_pass_count
                for idx, count in enumerate(grep_pass_array):
                    if count >= random:
                        return objects[idx]
            elif not grep_pass_count:
                return None

        if likelihood_sum:
            random = rand() * likelihood_sum
            for idx, partial in enumerate(partial_sums):
                if partial >= random:
                    return objects[idx]
            return ""  # PERL-QUIRK: falls off the end of the Perl sub
        idx = int(rand() * len(partial_sums))
        return objects[idx]

    return chooser


def choose(numbers, names=None):
    """Perl: SChoose->choose(\\@numbers, \\@names).

    Picks an index with probability proportional to ``numbers`` and returns
    ``names[idx]`` (``numbers[idx]`` if no names). If nothing is picked earlier
    (e.g. all weights 0) the last index is returned. Empty -> None, no draw."""
    if not numbers:
        return None
    if names is None:
        names = numbers
    nums = [perl_num(x) for x in numbers]
    return _at(names, _pick(nums, _sum(nums)))


def choose_a_few_nonzero(how_many, numbers, names=None):
    """Perl: SChoose->choose_a_few_nonzero($how_many, \\@numbers, \\@names).

    Chooses up to ``how_many`` entries without replacement (each chosen weight is
    zeroed), stopping once the remaining sum is not positive. Returns a list.
    PERL-QUIRK: a float residue can keep the sum positive after every non-zero
    weight is used; the loop then picks the last entry again (faithfully ported)."""
    nums = [perl_num(x) for x in numbers]
    names = list(numbers if names is None else names)
    total = _sum(nums)
    chosen = []
    still_to_choose = how_many
    while still_to_choose and total is not None and total > 0:
        idx = _pick(nums, total)
        chosen.append(_at(names, idx))
        total -= nums[idx]
        nums[idx] = 0
        still_to_choose -= 1
    return chosen


def choose_if_non_zero(numbers, names=None):
    """Perl: SChoose->choose_if_non_zero(\\@numbers, \\@names).

    Like ``choose`` but returns None (without drawing) when the sum is 0."""
    if not numbers:
        return None
    if names is None:
        names = numbers
    nums = [perl_num(x) for x in numbers]
    total = _sum(nums)
    if not total:
        return None
    return _at(names, _pick(nums, total))


def using_fascination(objects, fasc):
    """Perl: SChoose->using_fascination(\\@objects, $fasc).

    PERL-QUIRK: the Perl calls ``choose($array_ref, \\@imp)``, i.e. the objects
    are the weights and the fascination values the names. Ported as written
    (unused in the Perl code base)."""
    imp = [obj.get_fascination(fasc) for obj in objects]
    return choose(objects, imp)


def uniform(items):
    """Perl: SChoose->uniform(\\@items). One draw even when empty (-> None)."""
    return _at(items, int(rand() * len(items)))
