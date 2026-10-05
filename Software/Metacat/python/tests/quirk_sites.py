"""The `# chez:` and `# 1.2:` sites of the package, listed in
docs/python-translation-plan.md (item 17).

    python3 python/tests/quirk_sites.py           # print the list
    python3 python/tests/quirk_sites.py --write   # rewrite it in the plan

test_quirk_sites.py checks that the plan's list is the code's.  A site is named by
its module and its enclosing function (not its line), so that edits elsewhere
leave the list alone.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026).
"""

from __future__ import annotations

import ast
import io
import re
import sys
import tokenize
from collections import Counter
from pathlib import Path

PACKAGE = Path(__file__).resolve().parent.parent / "metacat"
PLAN = PACKAGE.parent.parent / "docs" / "python-translation-plan.md"
BEGIN = "<!-- quirk-sites:begin (python/tests/quirk_sites.py --write) -->"
END = "<!-- quirk-sites:end -->"
MARK = re.compile(r"#\s*(chez|1\.2):\s*(.*)")

# the kinds of `# chez:` site, by the first matching pattern of the comment text
KINDS = [
    ("truthiness (only #f is false)", re.compile(r"#f only|only #f|boolean|bool is an int", re.I)),
    ("map's order", re.compile(r"\bmap\b|map_|andmap|ormap|for-each|tell-all|delegate-to-all", re.I)),
    ("stochastic-if* coin first", re.compile(r"stochastic-if|coin", re.I)),
    ("sort", re.compile(r"\bsort", re.I)),
    ("recursion and sequence order", re.compile(r"first|before|walked", re.I)),
    ("evaluation order", re.compile(r"evaluat|right to left|left to right|argument|\blet\b|append|order", re.I)),
    ("record-case and case", re.compile(r"record-case|\bcase\b", re.I)),
    ("characters, strings and symbols", re.compile(r"char|string|symbol|constituent", re.I)),
    ("numbers and printing", re.compile(r"exact|flonum|float|round|rational|print|format|number|eq\?|\(\*", re.I)),
]


def enclosing_names(tree):
    """line -> qualified name of the innermost def/class around it."""
    names = {}

    def walk(node, prefix):
        for child in ast.iter_child_nodes(node):
            if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                name = f"{prefix}.{child.name}" if prefix else child.name
                for line in range(child.lineno, child.end_lineno + 1):
                    names[line] = name
                walk(child, name)
            else:
                walk(child, prefix)

    walk(tree, "")
    return names


def sites():
    """[(module, function, tag, text)] in file and line order.  Only real comments
    count (tokenize), not the words in a docstring."""
    out = []
    for path in sorted(PACKAGE.rglob("*.py")):
        source = path.read_text()
        names = enclosing_names(ast.parse(source))
        lines = source.splitlines()
        comments = [(tok.start[0], tok.string) for tok in
                    tokenize.generate_tokens(io.StringIO(source).readline)
                    if tok.type == tokenize.COMMENT]
        module = path.relative_to(PACKAGE.parent).with_suffix("").as_posix().replace("/", ".")
        for k, (line, comment) in enumerate(comments):
            m = MARK.match(comment)
            if not m:
                continue
            text = m.group(2).strip()
            # a comment continued on the following comment-only lines
            for next_line, next_comment in comments[k + 1:]:
                if next_line != line + 1 or MARK.match(next_comment) \
                        or not lines[next_line - 1].lstrip().startswith("#"):
                    break
                text += " " + next_comment.lstrip("#").strip()
                line = next_line
            if len(text) > 110:
                text = text[:107].rstrip() + "..."
            out.append((module, names.get(comments[k][0], "(module level)"),
                        m.group(1), text))
    return out


def kind(text):
    for name, pattern in KINDS:
        if pattern.search(text):
            return name
    return "other"


def render():
    found = sites()
    chez = [s for s in found if s[2] == "chez"]
    orig = [s for s in found if s[2] == "1.2"]
    lines = [BEGIN, "",
             f"{len(found)} sites: {len(chez)} `# chez:` (Chez's semantics that Python must "
             f"reproduce) and {len(orig)} `# 1.2:` (Metacat 1.2's own quirks, kept).", "",
             "| `# chez:` kind | Sites |", "|---|---|"]
    for name, n in sorted(Counter(kind(s[3]) for s in chez).items(), key=lambda kv: -kv[1]):
        lines.append(f"| {name} | {n} |")
    lines += ["", "| Module | `# chez:` | `# 1.2:` |", "|---|---|---|"]
    per = {}
    for module, _, tag, _ in found:
        per.setdefault(module, Counter())[tag] += 1
    for module, c in per.items():
        lines.append(f"| `{module}` | {c['chez']} | {c['1.2']} |")
    for tag, group in (("1.2", orig), ("chez", chez)):
        lines += ["", f"`# {tag}:` sites (module, function: comment):", ""]
        for module, function, _, text in group:
            lines.append(f"- `{module}`, `{function}`: {text.replace('|', '/')}")
    lines += ["", END]
    return "\n".join(lines) + "\n"


def plan_section(text):
    start, end = text.index(BEGIN), text.index(END) + len(END) + 1
    return text[start:end]


def write():
    text = PLAN.read_text()
    PLAN.write_text(text.replace(plan_section(text), render()))


if __name__ == "__main__":
    if sys.argv[1:] == ["--write"]:
        write()
    else:
        sys.stdout.write(render())
