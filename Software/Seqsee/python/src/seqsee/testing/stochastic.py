"""Port of Test::Stochastic 0.03 (CPAN, by Abhijit Mahabal), used by Test::Seqsee.

A random sub is called up to ``TIMES`` (1000) times and its results (stringified, so undef
is "") are checked:

- ``stochastic_ok(sub, {value: probability})``: each value is seen within
  ``_get_acceptable_range(p, TIMES, TOLERENCE)`` times (TOLERENCE 0.2);
- ``stochastic_all_seen_ok(sub, [values])``: every value is seen (stops as soon as all are);
- ``stochastic_all_and_only_ok(sub, [values])``: every value, and nothing else, is seen
  (always 1000 calls unless an unexpected value appears);
- the ``_nok`` forms expect the check to fail.

The sub and the expectation may come in either order. ``setup(times=…, tolerence=…)``
changes the module settings (``reset()`` restores them).

PERL-QUIRKs (oracle-confirmed, ported):
- ``stochastic_nok`` always passes: "stochastic_nok -- unexpectedly in range" when in range.
- all_and_only's "missing" message lists every expected value, not only the missing ones.
- A value never seen reads as undef: ``_check_probabilities`` says "saw it  times".
- The probability check walks the expectation hash with ``each`` (``util.perl_each``, dict
  order here) and dies at the first value out of range, leaving the hash's iterator there:
  checking the same hash again resumes after that value, so it can pass.
"""
from seqsee.errors import Confess
from seqsee.testing.more import ok
from seqsee.util import perl_each, perl_str

TIMES = 1000
TOLERENCE = 0.2


def reset():
    global TIMES, TOLERENCE
    TIMES = 1000
    TOLERENCE = 0.2


def _err_text(e):
    return str(e)


def _sub_and_arg(arg1, arg2):
    """Test::Stochastic: the CODE ref may come first or second."""
    return (arg1, arg2) if callable(arg1) else (arg2, arg1)


def _check_probabilities(arg1, arg2):
    sub, expected = _sub_and_arg(arg1, arg2)
    seen = {}
    for _ in range(TIMES):
        key = perl_str(sub())
        seen[key] = seen.get(key, 0) + 1
    for k, v in perl_each(expected):
        k = perl_str(k)
        lo, hi = _get_acceptable_range(v, TIMES, TOLERENCE)
        count = seen.get(k)
        if lo <= (count or 0) <= hi:
            continue
        raise Confess(f"Value out of range for '{k}': expected to see it between {lo} and "
                      f"{hi} times, but instead saw it {perl_str(count)} times\n")
    return 1


def _check_all_present(arg1, arg2):
    sub, arr = _sub_and_arg(arg1, arg2)
    to_see = {perl_str(x): 1 for x in arr}
    for _ in range(TIMES):
        to_see.pop(perl_str(sub()), None)
        if not to_see:
            return 1
    raise Confess("Not all expected outputs seen: missing " + ", ".join(to_see) + "\n")


def _check_all_and_only_present(arg1, arg2):
    sub, arr = _sub_and_arg(arg1, arg2)
    to_see = {perl_str(x): 1 for x in arr}
    still_to_see = dict(to_see)
    for _ in range(TIMES):
        val = perl_str(sub())
        if val not in to_see:
            raise Confess(f"unexpected value {val}\n")
        still_to_see.pop(val, None)
    if not still_to_see:
        return 1
    # PERL-QUIRK: lists %to_see, not %still_to_see
    raise Confess("Not all expected outputs seen: missing " + ", ".join(to_see) + "\n")


def _run(check, arg1, arg2):
    """eval { check(...) }: the error text, or None."""
    try:
        check(arg1, arg2)
    except Exception as e:  # noqa: BLE001 - Perl's eval catches every death
        return _err_text(e)
    return None


def stochastic_ok(arg1, arg2, msg=None):
    """Perl: stochastic_ok."""
    msg = msg or "stochastic_ok"
    err = _run(_check_probabilities, arg1, arg2)
    return ok(0, err) if err else ok(1, msg)


def stochastic_nok(arg1, arg2, msg=None):
    """Perl: stochastic_nok. PERL-QUIRK: passes either way."""
    msg = msg or "stochastic_nok"
    err = _run(_check_probabilities, arg1, arg2)
    return ok(1, msg) if err else ok(1, "stochastic_nok -- unexpectedly in range")


def stochastic_all_seen_ok(arr, sub, msg=None):
    """Perl: stochastic_all_seen_ok."""
    err = _run(_check_all_present, arr, sub)
    return ok(0, err) if err else ok(1, msg or "stochastic_all_seen_ok")


def stochastic_all_seen_nok(arr, sub, msg=None):
    """Perl: stochastic_all_seen_nok."""
    err = _run(_check_all_present, arr, sub)
    if err:
        return ok(1, msg or "stochastic_all_seen_nok")
    return ok(0, "stochastic_all_seen_nok: unexpectedly saw everything")


def stochastic_all_and_only_ok(arr, sub, msg=None):
    """Perl: stochastic_all_and_only_ok."""
    err = _run(_check_all_and_only_present, arr, sub)
    return ok(0, err) if err else ok(1, msg or "stochastic_all_and_only_ok")


def stochastic_all_and_only_nok(arr, sub, msg=None):
    """Perl: stochastic_all_and_only_nok."""
    err = _run(_check_all_and_only_present, arr, sub)
    if err:
        return ok(1, msg or "stochastic_all_and_only_nok")
    return ok(0, "stochastic_all_and_only_nok")


def setup(**options):
    """Perl: Test::Stochastic::setup(times => N, tolerence => T)."""
    global TIMES, TOLERENCE
    for k, v in options.items():
        if k == "times":
            TIMES = v
        elif k == "tolerence":
            TOLERENCE = v
        else:
            raise Confess(f"unknown option {k} passed to setup\n")


def _get_acceptable_range(p, times, tolerence):
    """Perl: _get_acceptable_range: (int((p - t) * times), int((p + t) * times + 0.999))."""
    return int((p - tolerence) * times), int((p + tolerence) * times + 0.999)
