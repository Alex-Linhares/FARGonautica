"""Port of SUtil.pm, plus the central RNG.

Every random draw in the port goes through ``RNG`` (module functions ``srand``,
``rand``, ``shuffle``). It reproduces Perl's ``rand``/``srand`` (drand48, Perl
>= 5.20) and ``List::Util::shuffle`` (which draws from the same state), so a
seeded Python run matches a seeded Perl run draw for draw.

Also holds a few helpers for Perl scalar semantics (``perl_str``, ``perl_num``,
``perl_true``) that SUtil's functions need.
"""
import glob as _glob
import inspect
import math
import os
import re
import sys
import threading

from seqsee.errors import Confess

# --- central RNG ---------------------------------------------------------------

_A = 0x5DEECE66D
_C = 0xB
_MASK48 = (1 << 48) - 1
_SEED_LOW = 0x330E
_UV_MAX = (1 << 64) - 1


class Drand48:
    """Perl's drand48: ``srand(s)`` sets ``x = (s << 16) | 0x330E``; each draw does
    ``x = (0x5DEECE66D * x + 0xB) mod 2**48`` and returns ``x / 2**48``.

    Like Perl, an unseeded generator seeds itself on the first draw.
    """

    def __init__(self, seed=None):
        self._x = None
        if seed is not None:
            self.srand(seed)

    def srand(self, seed=None):
        """Perl: srand. Returns the seed (Perl returns "0 but true" for 0).

        Perl parses the seed's string form as an unsigned integer: the sign and
        fraction are dropped (-42.9 -> 42) and overflow gives UV_MAX. Only the low
        32 bits seed the generator.
        """
        if seed is None:
            anum = int.from_bytes(os.urandom(4), "little")
        else:
            num = perl_num(seed)
            if isinstance(num, float) and not math.isfinite(num):
                anum = _UV_MAX
            else:
                anum = abs(int(num))
                if anum > _UV_MAX:
                    anum = _UV_MAX
        self._x = (((anum & 0xFFFFFFFF) << 16) | _SEED_LOW) & _MASK48
        return anum

    def _next(self):
        if self._x is None:
            self.srand()
        self._x = (_A * self._x + _C) & _MASK48
        return self._x / 2**48

    def rand(self, n=1):
        """Perl: rand(n). A float in [0, n); ``rand(0)`` behaves like ``rand(1)``."""
        if n == 0:
            n = 1
        return n * self._next()

    def shuffle(self, items):
        """Perl: List::Util::shuffle (XS, 1.63). Returns a new shuffled list."""
        out = list(items)
        index = len(out)
        while index > 1:
            swap = int(self._next() * index)
            index -= 1
            out[swap], out[index] = out[index], out[swap]
        return out

    def getstate(self):
        return self._x

    def setstate(self, state):
        self._x = state


RNG = Drand48()


def srand(seed=None):
    """Perl: srand, on the central RNG."""
    return RNG.srand(seed)


def rand(n=1):
    """Perl: rand, on the central RNG."""
    return RNG.rand(n)


def shuffle(items):
    """Perl: List::Util::shuffle, on the central RNG."""
    return RNG.shuffle(items)


# --- Perl scalar semantics -------------------------------------------------------

_SCALARS = (str, int, float, type(None))
_NUM_RE = re.compile(r"\s*([+-]?(?:\d+\.?\d*(?:[eE][+-]?\d+)?|\.\d+(?:[eE][+-]?\d+)?))")


def _is_scalar(x):
    return isinstance(x, _SCALARS)


def perl_str(x):
    """Stringify like Perl: floats use %.15g, undef is "", false is ""."""
    if x is None or x is False:
        return ""
    if x is True:
        return "1"
    if isinstance(x, float):
        if math.isnan(x):
            return "NaN"
        if math.isinf(x):
            return "Inf" if x > 0 else "-Inf"
        return "%.15g" % x
    return str(x)


def perl_num(x):
    """Numify like Perl: strings use their leading number (or 0), undef is 0."""
    if x is None:
        return 0
    if isinstance(x, bool):
        return int(x)
    if isinstance(x, (int, float)):
        return x
    if isinstance(x, str):
        m = _NUM_RE.match(x)
        if not m:
            return 0
        text = m.group(1)
        if re.fullmatch(r"[+-]?\d+", text):
            return int(text)
        return float(text)
    return x


def perl_true(x):
    """Perl truthiness: undef, "", "0" and 0 are false; everything else is true.

    Refs are true, except that a class with a Perl ``bool`` overload (a Python
    ``__bool__``, e.g. Seqsee::ResultOfCanBeSeenAs, EXTENDIBILE) decides for itself.
    Lists and dicts are always true (Perl array/hash refs)."""
    if x is None:
        return False
    if isinstance(x, str):
        return x not in ("", "0")
    if isinstance(x, (int, float)):
        return x != 0
    if getattr(type(x), "__bool__", None) is not None:
        return bool(x)
    return True


def perl_ref_string(obj):
    """The Perl stringification of a blessed hashref, ``Class=HASH(0x…)``.

    The class is ``obj.perl_name`` when set, else the Python class name. The address is
    ``id(obj)``, so it is only stable while the object is alive. Use it where Perl code
    inspects a stringified ref (e.g. Categorizable's /Interlaced/ check), never for
    comparing against Perl output."""
    name = getattr(obj, "perl_name", None) or type(obj).__name__
    return f"{name}=HASH(0x{id(obj):x})"


def perl_ref(x):
    """Perl ``ref($x)``: "" for scalars, "ARRAY"/"HASH" for lists/dicts, else the class
    name (``perl_name`` when set, so a subclass that sets its own name stops matching)."""
    if _is_scalar(x) or isinstance(x, bool):
        return ""
    if isinstance(x, (list, tuple)):
        return "ARRAY"
    if isinstance(x, dict):
        return "HASH"
    return getattr(x, "perl_name", None) or type(x).__name__


def looks_like_number(x):
    """Scalar::Util::looks_like_number for the decimal forms Seqsee uses, plus Perl's
    Inf/Infinity/NaN (any case, signed) and the special string "0 but true"
    (oracle-confirmed in sworkspace_basic). Hex strings don't count."""
    if isinstance(x, (int, float)) and not isinstance(x, bool):
        return True
    if not isinstance(x, str):
        return False
    return (_NUM_RE.fullmatch(x.rstrip()) is not None or x == "0 but true"
            or _INF_NAN_RE.fullmatch(x) is not None)


_INF_NAN_RE = re.compile(r"\s*[+-]?(?:inf(?:inity)?|nan)\s*", re.IGNORECASE)


def smartmatch_in(needle, items):
    """Perl ``$needle ~~ @items`` for a string needle: true if any element matches.

    Per element: undef never matches, a number (int/float, not a numeric string)
    compares numerically (so "first" ~~ [0] is true), anything else compares as a
    string (oracle-confirmed in mapping_based)."""
    for item in items:
        if item is None:
            continue
        if isinstance(item, (int, float)) and not isinstance(item, bool):
            if perl_num(needle) == item:
                return True
        elif perl_str(item) == needle:
            return True
    return False


def perl_range(start, end):
    """Perl ``$start .. $end`` on numbers: both ends are truncated toward zero.

    Not ported: Perl's magic string ranges ("a".."c"), used when the start is a
    non-empty string that doesn't look like a number. Those raise Confess."""
    for x in (start, end):
        if isinstance(x, str) and x != "" and not looks_like_number(x):
            raise Confess(f"perl_range: magic string ranges are not ported ({x!r})")
    return list(range(int(perl_num(start)), int(perl_num(end)) + 1))


def _has_as_text(x):
    return not isinstance(x, (str, int, float, type(None), list, tuple, dict)) and callable(
        getattr(x, "as_text", None))


# --- SUtil -------------------------------------------------------------------------

def uniq(*items):
    """Perl: uniq. Keys on the Perl string form (objects by identity); the last item
    with a given key wins. Perl returns hash values in no particular order; this
    returns them in first-seen order."""
    seen = {}
    for x in items:
        key = perl_str(x) if _is_scalar(x) else ("ref", id(x))
        seen[key] = x
    return list(seen.values())


def compare_deep(*args):
    """Perl: compare_deep. Numeric == on scalars, recursive on (nested) lists."""
    if len(args) != 2:
        raise Confess("compare_deep needs exactly two arguments")
    a, b = args
    ref_a, ref_b = not _is_scalar(a), not _is_scalar(b)
    if not ref_a and not ref_b:
        return perl_num(a) == perl_num(b)
    if ref_a and ref_b:
        if not (isinstance(a, (list, tuple)) and isinstance(b, (list, tuple))):
            raise Confess("compare_deep: only array refs can be compared")
        if len(a) != len(b):
            return False
        return all(compare_deep(x, y) for x, y in zip(a, b))
    return False


def equal_when_flattened(obj1, obj2):
    """Perl: equal_when_flattened. Numbers compare with ==, objects by ``flatten()``."""
    if _is_scalar(obj1):
        if not _is_scalar(obj2):
            return False
        return perl_num(obj1) == perl_num(obj2)
    if _is_scalar(obj2):
        return False
    flat1, flat2 = list(obj1.flatten()), list(obj2.flatten())
    if len(flat1) != len(flat2):
        return False
    return all(perl_num(x) == perl_num(y) for x, y in zip(flat1, flat2))


def generate_blemished(cat, blemish, pos, **args):
    """Perl: generate_blemished(cat => ..., blemish => ..., pos => ..., %build_args)."""
    built = cat.build(dict(args))
    return built.apply_blemish_at(blemish, pos)


def oddman(*objects):
    """Perl: oddman.

    PERL-QUIRK: dead code. It needs ``$SCat::ascending::ascending``,
    ``$SCat::mountain::mountain`` and ``$SBlemishType::*``, which no longer exist,
    so it dies in Perl.
    """
    raise Confess("oddman uses $SCat::ascending::ascending etc., which no longer exist")


def myall(*items):
    """Perl: myall. True if every item is Perl-true."""
    return all(perl_true(x) for x in items)


def odd_position(*items):
    """Perl: odd_position. Index of the single item that differs (string eq) from
    all the others, which must be equal; None if there isn't exactly one."""
    if len(items) < 3:
        raise Confess("need at least three arguments")
    s = [perl_str(x) for x in items]
    odd_pos = odd_value = repeated = None
    if s[0] == s[1]:
        repeated = s[0]
        for i in range(2, len(s)):
            if s[i] != s[0]:
                odd_pos, odd_value = i, s[i]
                break
    elif s[0] == s[2]:
        odd_pos, odd_value, repeated = 1, s[1], s[0]
    else:
        odd_pos, odd_value, repeated = 0, s[0], s[1]
    if odd_pos is None:
        return None
    for i, x in enumerate(s):
        if x != (odd_value if i == odd_pos else repeated):
            return None
    return odd_pos


def naive_brittle_chunking(items):
    """Perl: naive_brittle_chunking. Runs of equal (==) neighbours become lists."""
    items = list(items)
    ret = []
    while len(items) > 1:
        nxt = items.pop(0)
        run = [nxt]
        while items and perl_num(items[0]) == perl_num(nxt):
            run.append(items.pop(0))
        ret.append(run if len(run) > 1 else nxt)
    ret.extend(items)
    return ret


def next_available_file_number(directory):
    """Perl: next_available_file_number. 1 + the largest number formed by the
    digits of any ``directory/*`` path (1 if none).

    PERL-QUIRK: digits are stripped from the whole path, so digits in
    ``directory`` itself count (``dir9/a1`` -> 91). Paths whose digits are "" or
    "0" are ignored.
    """
    numbers = [re.sub(r"[^0-9]", "", p) for p in sorted(_glob.glob(f"{directory}/*"))]
    numbers = [n for n in numbers if perl_true(n)]
    if not numbers:
        return 1
    return 1 + max(int(n) for n in numbers)


def toss(prob):
    """Perl: toss. 1 with probability ``prob`` (one draw: ``rand() <= prob``), else 0."""
    if prob is None:
        raise Confess("uninitialized prob as argument!")
    return 1 if rand() <= prob else 0


def clear_all():
    """Perl: clear_all. Clears the workspace, then the stream and coderack."""
    from seqsee import sworkspace
    sworkspace.clear()
    clear_all_but_workspace()


def clear_all_but_workspace():
    """Perl: clear_all_but_workspace. Clears $Global::MainStream and SCoderack."""
    from seqsee import global_ as Global
    from seqsee import scoderack
    Global.MainStream.clear()
    scoderack.clear()


def minmax(*items):
    """Perl: minmax. Returns (min, max) using numeric comparison."""
    if not items:
        raise Confess("undefined!")
    lo = hi = items[0]
    for x in items[1:]:
        if lo > x:
            lo = x
        if hi < x:
            hi = x
    return lo, hi


def all_(code, *items):
    """Perl: all (&@). True if ``code(item)`` is Perl-true for every item."""
    return all(perl_true(code(x)) for x in items)


def significant(x):
    """Perl: significant. 1 if x > 0.7, else 0."""
    return 1 if x > 0.7 else 0


def structure_to_string(structure):
    """Perl: StructureToString. Nested lists -> "[1, [2, 3]]"; scalars unchanged."""
    if isinstance(structure, (list, tuple)):
        return "[" + ", ".join(perl_str(structure_to_string(x)) for x in structure) + "]"
    return structure


def stringify_deep_array(item):
    """Perl: StringifyDeepArray. Same output as StructureToString."""
    if isinstance(item, (list, tuple)):
        return "[" + ", ".join(perl_str(stringify_deep_array(x)) for x in item) + "]"
    return item


def trim(s):
    """Perl: trim. Perl trims each argument in place; this returns the trimmed string.

    PERL-QUIRK: ``s#\\s+# #`` has no /g, so only the first inner run of
    whitespace is squeezed ("a   b   c" -> "a b   c").
    """
    s = re.sub(r"^\s*", "", s, count=1, flags=re.ASCII)
    s = re.sub(r"\s*$", "", s, count=1, flags=re.ASCII)
    return re.sub(r"\s+", " ", s, count=1, flags=re.ASCII)


def _text_or_str(x):
    return x.as_text() if _has_as_text(x) else perl_str(x)


def stringify_for_carp(arg):
    """Perl: StringifyForCarp. Objects with as_text become «text»; dicts become
    "{ k => v, ... }" (Perl hash order there, insertion order here); lists are
    comma-joined; other references give "reftype=..."; None gives "undef".
    Other scalars come back unchanged.
    """
    if _has_as_text(arg):
        return "\xab" + arg.as_text() + "\xbb"
    if isinstance(arg, dict):
        out = "{ "
        for k, v in arg.items():
            if v is None:
                v = "undef"
            out += f"{perl_str(k)} => "
            if _has_as_text(v):
                out += "\xab" + v.as_text() + "\xbb"
            elif isinstance(v, (list, tuple)):
                out += "[ " + ", ".join(_text_or_str(x) for x in v) + " ]"
            else:
                out += perl_str(v)
            out += ", "
        return out + " }"
    if isinstance(arg, (list, tuple)):
        return ", ".join(_text_or_str(x) for x in arg)
    if inspect.isroutine(arg):
        return "reftype=CODE"
    if arg is None:
        return "undef"
    if not _is_scalar(arg):
        return f"reftype={type(arg).__name__}"
    return arg


def hash_sorted_as_array(mapping):
    """Perl: hash_sorted_as_array(%hash). Flat [k1, v1, k2, v2, ...], keys string-sorted."""
    out = []
    for k in sorted(mapping, key=perl_str):
        out.extend((k, mapping[k]))
    return out


# Perl's per-hash `each` iterator. A `while (my ($k, $v) = each %h)` loop that returns
# early leaves the iterator part-way, and the next `each` on the same hash resumes there.
# `keys`, `values` and flattening the hash in list context reset it. State is keyed by
# id(dict); the dict itself is kept alive alongside, so the id can't be reused.
_EACH = {}


def perl_each(d):
    """Iterate ``d``'s items like a Perl ``each`` loop, sharing the hash's iterator."""
    _, pos = _EACH.pop(id(d), (d, 0))
    items = list(d.items())
    while pos < len(items):
        item = items[pos]
        pos += 1
        _EACH[id(d)] = (d, pos)
        yield item
    _EACH.pop(id(d), None)


def perl_hash_reset(d):
    """Reset ``d``'s ``each`` iterator (what Perl's keys/values/list flattening do)."""
    if isinstance(d, dict):
        _EACH.pop(id(d), None)


def perl_keys(d):
    """Perl ``keys %h``: the keys, resetting the hash's ``each`` iterator."""
    perl_hash_reset(d)
    return list(d)


def reset_each_iterators():
    """Forget every pending ``each`` position (test isolation)."""
    _EACH.clear()


# --- recursion headroom (item 050b) ---------------------------------------------------
# Perl has no recursion limit, and some Seqsee recursions go hundreds of levels deep (the
# FindMapping/sameness recursion of item 050b), each level costing several Python frames.
# So the main loop runs its steps through call_with_deep_stack: in a worker thread with a
# large C stack and a raised Python recursion limit. Recursive paths must also avoid
# C-level calls such as an instance's __call__, which CPython 3.12 counts against a fixed
# C recursion limit; see Multimethod.call.
# ~7 frames per FindMapping/sameness level; the deepest seen (Alternating, seed 1) was 3638
# levels. C-level recursion stays bounded by CPython's own C limit.
DEEP_RECURSION_LIMIT = 500_000
DEEP_STACK_SIZE = 256 * 1024 * 1024
_deep = threading.local()


def call_with_deep_stack(fn, *args, **kwargs):
    """Return ``fn(*args, **kwargs)``, run with at least ``DEEP_RECURSION_LIMIT`` Python
    frames and a ``DEEP_STACK_SIZE`` C stack. Any exception (BaseException included) is
    re-raised in the caller. A nested call runs inline in the already-deep thread."""
    if getattr(_deep, "active", False):
        return fn(*args, **kwargs)
    outcome = {}

    def work():
        _deep.active = True
        try:
            outcome["value"] = fn(*args, **kwargs)
        except BaseException as e:  # noqa: BLE001 - handed back to the caller
            outcome["error"] = e

    # daemon: a Ctrl-C in the waiting caller must not leave the process waiting on it
    worker = threading.Thread(target=work, name="seqsee-deep-stack", daemon=True)
    old_limit = sys.getrecursionlimit()
    sys.setrecursionlimit(max(old_limit, DEEP_RECURSION_LIMIT))
    try:
        old_size = threading.stack_size(DEEP_STACK_SIZE)
        try:
            worker.start()
        finally:
            threading.stack_size(old_size)
        worker.join()
    finally:
        sys.setrecursionlimit(old_limit)
    if "error" in outcome:
        raise outcome["error"]
    return outcome["value"]
