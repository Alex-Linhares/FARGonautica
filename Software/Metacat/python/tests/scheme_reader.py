"""A small Scheme reader: Scheme text -> Python data in metacat/chez.py's representation.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

For tests that take quoted Scheme data from a battery or a fixture file (the SGL
battery's picture expressions, python/oracle/sgl-fixture.scm).  A symbol is a str,
a string chez.String, an integer int, a ratio Fraction, a decimal float, #t/#f
bool, a list a Python list, 'x (quote x).  Dotted pairs, vectors and characters
are not needed and raise.
"""
from __future__ import annotations

from fractions import Fraction

from metacat import chez

DELIMS = set("()[]\"; \t\n\r\f'")


def _skip(text, pos):
    while pos < len(text):
        if text[pos].isspace():
            pos += 1
        elif text[pos] == ";":
            end = text.find("\n", pos)
            pos = len(text) if end < 0 else end + 1
        else:
            break
    return pos


def _atom(tok):
    if tok == "#t":
        return True
    if tok == "#f":
        return False
    if tok.startswith("#"):
        raise ValueError("unsupported datum " + tok)
    try:
        if "/" in tok:
            return Fraction(tok)
        return int(tok)
    except ValueError:
        pass
    try:
        if any(c.isdigit() for c in tok) and tok not in ("+", "-", "..."):
            return float(tok)
    except ValueError:
        pass
    return tok


def _read(text, pos):
    pos = _skip(text, pos)
    if pos >= len(text):
        raise EOFError
    c = text[pos]
    if c in "([":
        close = ")" if c == "(" else "]"
        out, pos = [], pos + 1
        while True:
            pos = _skip(text, pos)
            if pos >= len(text):
                raise EOFError("unclosed list")
            if text[pos] == close:
                return out, pos + 1
            if text[pos] == "." and text[pos + 1] in DELIMS:
                raise ValueError("dotted pairs are not supported")
            item, pos = _read(text, pos)
            out.append(item)
    if c in ")]":
        raise ValueError("unexpected " + c)
    if c == "'":
        item, pos = _read(text, pos + 1)
        return ["quote", item], pos
    if c == '"':
        out, pos = [], pos + 1
        while text[pos] != '"':
            if text[pos] == "\\":
                pos += 1
                out.append({"n": "\n", "t": "\t"}.get(text[pos], text[pos]))
            else:
                out.append(text[pos])
            pos += 1
        return chez.String("".join(out)), pos + 1
    end = pos
    while end < len(text) and text[end] not in DELIMS:
        end += 1
    return _atom(text[pos:end]), end


def read(text: str):
    """The first datum of text."""
    return _read(text, 0)[0]


def read_all(text: str) -> list:
    """Every datum of text, in order."""
    out, pos = [], _skip(text, 0)
    while pos < len(text):
        item, pos = _read(text, pos)
        out.append(item)
        pos = _skip(text, pos)
    return out
