"""Every name the original defines, for test_name_mapping.py (item 01).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The mapping itself moved into the package in item 03 (metacat/names.py); this
module re-exports it and reads the original's names.
"""
from __future__ import annotations

import re
from pathlib import Path

from metacat.names import EXCEPTIONS, RESERVED, scheme_to_python  # noqa: F401

ORIGINAL = Path(__file__).resolve().parents[2] / "chez_scheme" / "original"


def original_names() -> dict[str, str]:
    """Every name the original defines, with the file that first defines it:
    (define name ...), (define (name ...) ...), extend-syntax keywords, codelet types
    (codelet-type-list*) and slipnodes (slipnet-node-list*)."""
    names: dict[str, str] = {}

    def add(name, f):
        names.setdefault(name, f)
    for path in sorted(ORIGINAL.glob("*.ss")):
        text = path.read_text(errors="replace")
        text = re.sub(r";[^\n]*", "", text)
        for m in re.finditer(r"\(define\s+\(?([^\s()]+)", text):
            add(m.group(1), path.name)
        for m in re.finditer(r"\(extend-syntax\s+\(([^\s()]+)", text):
            add(m.group(1), path.name)
        for block in ("codelet-type-list\\*", "slipnet-node-list\\*"):
            m = re.search(r"\(%s(.*?)\)\)\s*\n\s*\n" % block, text, re.S)
            if m and "extend-syntax" not in text[max(0, m.start() - 40):m.start()]:
                for item in re.finditer(r"\(\s*([^\s()\"]+)", m.group(1)):
                    add(item.group(1), path.name)
    return names
