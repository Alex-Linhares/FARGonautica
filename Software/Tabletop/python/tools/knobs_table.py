#!/usr/bin/env python3
"""Print the table of knobs in `python/README.md` from `tabletop/config.py` (D-066).

Every field of `Config` is commented, in the source, with the page or the decision that
gives its value; this script reads those comments and prints one Markdown row per field, in
the order of the dataclass, under its group heading, with the default of every profile that
differs from `standard`:

    python3 tools/knobs_table.py            # the table, as README.md carries it
    python3 tools/knobs_table.py --check    # exit 1 when README.md's table is stale

`tests/test_readme.py` runs the check, so the README's table is never out of date. Standard
library only; the script reads the source text, not the dataclass, so that the comments are
kept word for word.
"""

import re
import sys
from pathlib import Path

PYTHON_DIR = Path(__file__).resolve().parent.parent
CONFIG = PYTHON_DIR / "tabletop" / "config.py"
README = PYTHON_DIR / "README.md"
BEGIN = "<!-- begin knobs -->"
END = "<!-- end knobs -->"
GROUP = re.compile(r"^    # ---- (.*?) -+$")
FIELD = re.compile(r"^    ([a-z_][a-z0-9_]*): [^=]+ = (.*)$")
COMMENT = re.compile(r"^    # (.*)$")


def fields_of(source: str):
    """(group, field, default, comment) for every field of `Config`, in source order."""
    body = source[source.index("class Config"):]
    body = body[:body.index("\ndef ")] if "\ndef " in body else body
    rows, group, comment = [], "", []
    for line in body.splitlines():
        m = GROUP.match(line)
        if m:
            group, comment = m.group(1), []
            continue
        m = COMMENT.match(line)
        if m:
            comment.append(m.group(1).strip())
            continue
        m = FIELD.match(line)
        if m:
            text = " ".join(comment).replace("DECISION D-", "D-")
            rows.append((group, m.group(1), m.group(2).strip(), text))
            comment = []
            continue
        if line.strip() == "":
            continue
        comment = []
    return rows


def profile_differences():
    """{field: {profile: repr(value)}} for the fields where a profile differs from standard."""
    import importlib.util
    spec = importlib.util.spec_from_file_location("tabletop_config", CONFIG)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    PROFILES = module.PROFILES
    standard = PROFILES["standard"]
    out = {}
    for name, config in PROFILES.items():
        if name == "standard":
            continue
        for field, value in config.differences(standard).items():
            out.setdefault(field, {})[name] = repr(value)
    return out


def cell(text: str) -> str:
    return text.replace("|", "\\|")


def table() -> str:
    rows = fields_of(CONFIG.read_text(encoding="utf-8"))
    differences = profile_differences()
    lines, group = [], None
    for g, field, default, comment in rows:
        if g != group:
            group = g
            lines.append("")
            lines.append(f"**{group}**")
            lines.append("")
            lines.append("| Knob | Default | Other profiles | Source or decision |")
            lines.append("|---|---|---|---|")
        others = ", ".join(f"`{p}` {v}" for p, v in differences.get(field, {}).items())
        default_text = default if len(default) <= 40 else default[:37] + "..."
        lines.append(f"| `{field}` | `{cell(default_text)}` | {others} | {cell(comment)} |")
    return "\n".join(lines[1:]) + "\n"


def readme_table() -> str:
    text = README.read_text(encoding="utf-8")
    start = text.index(BEGIN) + len(BEGIN)
    end = text.index(END)
    return text[start:end].strip("\n") + "\n"


def main(argv) -> int:
    if "--check" in argv:
        if readme_table() == table():
            print("README.md's table of knobs is current")
            return 0
        print("README.md's table of knobs is stale: python3 tools/knobs_table.py > table.md "
              "and paste it between the markers", file=sys.stderr)
        return 1
    sys.stdout.write(table())
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
