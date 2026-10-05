"""A minimal Scheme datum scanner: enough to list a battery's top-level forms.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Used only to count a battery's (test NAME EXPR) forms independently of Chez
(tests/test_fixtures.py).  Atoms stay strings; lists become Python lists.
Handles ; and #| |# comments, #; datum comments, strings, characters (#\\(),
quote prefixes, vectors and brackets: what tests/diff/*.scm use.
"""
from __future__ import annotations

DELIMS = set("()[]\";'`, \t\n\r\f")


def read_all(text: str) -> list:
    forms = []
    pos = skip(text, 0)
    while pos < len(text):
        form, pos = read(text, pos)
        if form is not COMMENT:
            forms.append(form)
        pos = skip(text, pos)
    return forms


COMMENT = object()


def skip(text: str, pos: int) -> int:
    """Skip whitespace, ; comments and (nested) #| |# comments."""
    while pos < len(text):
        c = text[pos]
        if c.isspace():
            pos += 1
        elif c == ";":
            end = text.find("\n", pos)
            pos = len(text) if end < 0 else end + 1
        elif text.startswith("#|", pos):
            depth, pos = 1, pos + 2
            while depth:
                if text.startswith("#|", pos):
                    depth, pos = depth + 1, pos + 2
                elif text.startswith("|#", pos):
                    depth, pos = depth - 1, pos + 2
                elif pos >= len(text):
                    raise SyntaxError("unterminated #| comment")
                else:
                    pos += 1
        else:
            break
    return pos


def read(text: str, pos: int):
    """Read the datum at pos (after skip); returns (datum, next position)."""
    c = text[pos]
    if c in "([":
        items, pos = [], skip(text, pos + 1)
        while text[pos] not in ")]":
            item, pos = read(text, pos)
            if item is not COMMENT:
                items.append(item)
            pos = skip(text, pos)
        return items, pos + 1
    if c in ")]":
        raise SyntaxError(f"unexpected {c} at {pos}")
    if c == '"':
        end = pos + 1
        while text[end] != '"':
            end += 2 if text[end] == "\\" else 1
        return text[pos : end + 1], end + 1
    if c in "'`,":
        pos += 2 if text.startswith(",@", pos) else 1
        datum, pos = read(text, skip(text, pos))
        return ["quote", datum], pos
    if text.startswith("#;", pos):
        _, pos = read(text, skip(text, pos + 2))
        return COMMENT, pos
    if text.startswith("#'", pos):
        datum, pos = read(text, skip(text, pos + 2))
        return ["syntax", datum], pos
    if text.startswith("#(", pos):
        return read(text, pos + 1)
    end = pos + 3 if text.startswith("#\\", pos) else pos + 1
    while end < len(text) and text[end] not in DELIMS:
        end += 1
    return text[pos:end], end


def test_names(text: str) -> list[str]:
    """The NAMEs of the top-level (test NAME EXPR) forms, in order."""
    return [f[1] for f in read_all(text) if isinstance(f, list) and f and f[0] == "test"]
