#!/usr/bin/env python3
"""Draw the citation audit's random sample (`docs/citation-audit.md`), reproducibly.

    python3 tools/audit_sample.py [--seed 31] [--img 24] [--plain 24] [--layouts 10] [--json]

The inventory: every page citation (`[F95 p. N]`, `[F95 pp. N-M]`, `[FCCA ...]`, `[FH91 ...]`,
with or without `(img)`) in SPEC.md, DECISIONS.md and RESULTS.md, in that order, line by line,
and then in every docstring of the package (`tabletop/**/*.py`, sorted by path; module, class
and function docstrings, read with `ast`). Two strata: the `(img)` citations, which the
quotation checker never reads, and the plain ones. The sample: with one splitmix64 generator
seeded by `--seed` (the package's own `tabletop/rng.py`, loaded by file), `--img` draws
without replacement from the `(img)` stratum, then `--plain` from the plain one, then
`--layouts` problem stems from `problems/` (sorted). The draws are `rng.below(len(pool))`
on the remaining pool, in that order, so the sample depends on the inventory's size and
order: the audit records both, and a tree with the same inventory gives the same sample.

Standard library only; no `sys.path` change (the generator is loaded by file). Prints a
Markdown table per stratum (location, citation, the text before the tag) and the layouts;
`--json` prints the same as JSON.
"""

from __future__ import annotations

import argparse
import ast
import importlib.util
import json
import re
from pathlib import Path

PYTHON_DIR = Path(__file__).resolve().parent.parent
MARKDOWN = ("SPEC.md", "DECISIONS.md", "RESULTS.md")
TAG = re.compile(r"\[(F95|FCCA|FH91) (pp?\. [0-9]+(?:-[0-9]+)?)(?: \(img\))?(?:;[^\]]*)?\]")


def load_rng():
    """The package's generator, loaded from its file (no import of the package)."""
    path = PYTHON_DIR / "tabletop" / "rng.py"
    spec = importlib.util.spec_from_file_location("tabletop_rng", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.Rng


def citations_in_lines(name: str, lines: list[str], first_line: int, kind: str) -> list[dict]:
    out = []
    for offset, line in enumerate(lines):
        for m in TAG.finditer(line):
            out.append({"file": name, "line": first_line + offset, "tag": m.group(1),
                        "pages": m.group(2), "img": "(img)" in m.group(0), "kind": kind,
                        "cite": m.group(0), "claim": line[:m.start()].strip()[-200:]})
    return out


def docstring_citations(path: Path, name: str) -> list[dict]:
    """The citations of every docstring of a module, with the line each sits on."""
    tree = ast.parse(path.read_text())
    out = []
    nodes = [tree] + [n for n in ast.walk(tree)
                      if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef))]
    for node in nodes:
        doc = ast.get_docstring(node, clean=False)
        if not doc or not node.body:
            continue
        out += citations_in_lines(name, doc.split("\n"), node.body[0].lineno, "docstring")
    return out


def inventory(root: Path = PYTHON_DIR) -> list[dict]:
    """Every page citation of the three Markdown files and of the package's docstrings."""
    items = []
    for md in MARKDOWN:
        items += citations_in_lines(md, (root / md).read_text().split("\n"), 1, "md")
    for py in sorted((root / "tabletop").rglob("*.py")):
        items += docstring_citations(py, str(py.relative_to(root)))
    return items


def problem_stems(root: Path = PYTHON_DIR) -> list[str]:
    return sorted(p.stem for p in (root / "problems").glob("*.json"))


def draw(rng, pool: list, count: int) -> list:
    """`count` items without replacement, by `rng.below` on the shrinking pool."""
    pool = list(pool)
    out = []
    for _ in range(min(count, len(pool))):
        out.append(pool.pop(rng.below(len(pool))))
    return out


def sample(seed: int, img: int, plain: int, layouts: int, root: Path = PYTHON_DIR) -> dict:
    items = inventory(root)
    rng = load_rng()(seed)
    img_pool = [i for i in items if i["img"]]
    plain_pool = [i for i in items if not i["img"]]
    return {"seed": seed, "inventory": len(items), "img_total": len(img_pool),
            "plain_total": len(plain_pool), "img": draw(rng, img_pool, img),
            "plain": draw(rng, plain_pool, plain),
            "layouts": draw(rng, problem_stems(root), layouts)}


def markdown(s: dict) -> str:
    lines = [f"Seed {s['seed']}; inventory {s['inventory']} citations ({s['img_total']} with "
             f"(img), {s['plain_total']} plain); {len(s['layouts'])} layouts.", ""]
    for stratum, title in (("img", "(img) citations"), ("plain", "plain citations")):
        lines += [f"### {title}", "", "| # | Where | Citation | Before the tag |", "|---|---|---|---|"]
        for n, i in enumerate(s[stratum], 1):
            claim = i["claim"][-70:].replace("|", "\\|")
            lines.append(f"| {n} | `{i['file']}:{i['line']}` | `{i['cite']}` | ...{claim} |")
        lines.append("")
    lines += ["### layouts", "", ", ".join(f"`{p}`" for p in s["layouts"]), ""]
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--seed", type=int, default=31)
    ap.add_argument("--img", type=int, default=24)
    ap.add_argument("--plain", type=int, default=24)
    ap.add_argument("--layouts", type=int, default=10)
    ap.add_argument("--json", action="store_true")
    a = ap.parse_args(argv)
    s = sample(a.seed, a.img, a.plain, a.layouts)
    print(json.dumps(s, indent=1) if a.json else markdown(s))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
