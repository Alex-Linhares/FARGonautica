"""The fixed Scheme -> Python name mapping (docs/python-translation-plan.md, "Names").

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026); the mapping was decided in loop0002 item 01
and moved into the package in item 03.  python/tests/test_name_mapping.py checks
that it is valid and injective on every name the original defines.
"""
from __future__ import annotations

import builtins
import keyword
import re

# Names no rule handles well: digits first, or a lone operator.
EXCEPTIONS = {
    "1st": "first", "2nd": "second", "3rd": "third", "4th": "fourth",
    "5th": "fifth", "6th": "sixth", "7th": "seventh", "8th": "eighth",
    "1-": "one_minus",            # (- 1 x)
    "10-": "ten_minus", "100-": "hundred_minus",
    "100*": "times_100",          # (round (* 100 x))
    "%": "percent",               # (/ n 100)
    "20%": "percent_20", "40%": "percent_40", "80%": "percent_80",
    "^2": "square", "^3": "cube",
    "~": "rough",                 # roughly n (draws)
    "?": "theme_help",            # themes.ss: prints the theme abbreviations
    "180/pi": "degrees_per_radian", "pi/180": "radians_per_degree",
}

# Python names a Scheme name must not take: keywords and the builtins.
RESERVED = set(keyword.kwlist) | set(keyword.softkwlist) | set(dir(builtins))


def scheme_to_python(name: str) -> str:
    """The fixed mapping, applied in this order:

    1. EXCEPTIONS
    2. markers around the whole name: *x* -> g_x (global variable), %x% -> p_x
       (parameter, Marshall's tunable constants), =x= -> c_x (colour)
    3. a trailing * (the extend-syntax forms) -> _star
    4. inside the name: -> to _to_, ? to _p, ! to _bang, : to __, / to _or_,
       . and - to _
    5. a Python keyword or builtin gets a trailing _
    """
    if name in EXCEPTIONS:
        return EXCEPTIONS[name]
    prefix = ""
    for mark, pre in (("*", "g_"), ("%", "p_"), ("=", "c_")):
        if len(name) > 2 and name.startswith(mark) and name.endswith(mark):
            name, prefix = name[1:-1], pre
            break
    suffix = ""
    if name.endswith("*") and len(name) > 1:
        name, suffix = name[:-1], "_star"
    name = name.replace("->", "-to-")
    name = re.sub(r"\?", "-p", name)
    name = name.replace("!", "-bang")
    name = name.replace(":", "--")
    name = name.replace("/", "-or-")
    name = name.replace(".", "-")
    out = prefix + name.replace("-", "_") + suffix
    if out in RESERVED:
        out += "_"
    return out
