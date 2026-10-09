"""Compare Python draw ops with Perl canvas dumps (python/oracle/CanvasDump.pm).

Items match by type; coords within ``tol`` px (default ±0.5); tags as a set; options exactly
after normalising both sides: colours → ``#RRGGBB`` (seqsee.gui.draw.colors), numbers →
floats, lists → tuples, '' → unset, and options equal to Tk's default for the item type are
dropped (so ``outline='#000000'`` on a rectangle equals the default ``black``).

``ordered=True`` compares draw order item by item; ``ordered=False`` compares multisets
(for Perl draw orders that depend on hash order).
"""

from dataclasses import fields

from seqsee.gui.draw import colors
from seqsee.gui.draw.ops import OP_TYPES, Op

COLOUR_OPTS = {"fill", "outline", "activefill", "activeoutline", "disabledfill", "disabledoutline"}


def _norm_value(key, value):
    if value is None or value == "":
        return None
    if key in COLOUR_OPTS:
        return colors.to_hex(value)
    if isinstance(value, bool):
        return float(value)
    if isinstance(value, (int, float)):
        return float(value)
    if isinstance(value, (list, tuple)):
        return tuple(_norm_value(None, v) for v in value)
    return value


def _defaults(type_):
    cls = OP_TYPES[type_]
    return {
        f.name: _norm_value(f.name, f.default)
        for f in fields(cls)
        if f.name not in ("coords", "tags")
    }


def normalize(item):
    """A comparable dict for an Op or a dumped item dict."""
    d = item.to_dict() if isinstance(item, Op) else item
    type_ = d["type"]
    defaults = _defaults(type_) if type_ in OP_TYPES else {}
    opts = {}
    for key, value in (d.get("opts") or {}).items():
        v = _norm_value(key, value)
        if key in defaults and v == defaults[key]:
            continue
        if key not in defaults and v is None:
            continue
        opts[key] = v
    return {
        "type": type_,
        "coords": tuple(float(c) for c in d.get("coords", ())),
        "opts": opts,
        "tags": frozenset(d.get("tags") or ()),
    }


def item_diff(actual, expected, tol=0.5):
    """None if the two items match, else a short description of the first difference."""
    a, e = normalize(actual), normalize(expected)
    if a["type"] != e["type"]:
        return f"type {a['type']} != {e['type']}"
    if len(a["coords"]) != len(e["coords"]) or any(
        abs(x - y) > tol for x, y in zip(a["coords"], e["coords"])
    ):
        return f"coords {list(a['coords'])} != {list(e['coords'])}"
    if a["opts"] != e["opts"]:
        keys = sorted(set(a["opts"]) | set(e["opts"]))
        bad = {k: (a["opts"].get(k), e["opts"].get(k)) for k in keys
               if a["opts"].get(k) != e["opts"].get(k)}
        return f"opts (actual, expected) {bad}"
    if a["tags"] != e["tags"]:
        return f"tags {sorted(a['tags'])} != {sorted(e['tags'])}"
    return None


def _brief(item):
    n = normalize(item)
    return f"{n['type']} {list(n['coords'])} {n['opts']} tags={sorted(n['tags'])}"


def compare_ops(actual, expected, tol=0.5, ordered=True):
    """List of differences between two item lists (empty if they match)."""
    actual, expected = list(actual), list(expected)
    problems = []
    if ordered:
        for i, (a, e) in enumerate(zip(actual, expected)):
            d = item_diff(a, e, tol)
            if d:
                problems.append(f"item {i}: {d}")
        problems += [f"extra actual item {i}: {_brief(a)}"
                     for i, a in enumerate(actual[len(expected):], len(expected))]
        problems += [f"missing item {i}: {_brief(e)}"
                     for i, e in enumerate(expected[len(actual):], len(actual))]
        return problems
    unmatched = list(range(len(actual)))
    for j, e in enumerate(expected):
        hit = next((i for i in unmatched if item_diff(actual[i], e, tol) is None), None)
        if hit is None:
            problems.append(f"missing item {j}: {_brief(e)}")
        else:
            unmatched.remove(hit)
    problems += [f"extra actual item {i}: {_brief(actual[i])}" for i in unmatched]
    return problems


def assert_ops_match(actual, expected, tol=0.5, ordered=True):
    problems = compare_ops(actual, expected, tol, ordered)
    assert not problems, "draw ops differ from the Perl canvas:\n  " + "\n  ".join(problems)
