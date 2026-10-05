"""The module rules of docs/python-translation-plan.md ("The global top level and
Python modules", and Risk 6), checked on the engine as built (item 17):

- every engine module imports alone, in a fresh interpreter, without loading the
  engine: no cross-module work at import time, and no random draw;
- engine modules `from`-import names only from chez, objects, sugar, utilities and
  names, which nothing rebinds.  Any other `from metacat.x import y` copies a binding
  that a wrapper (trace_writer.py, the batteries' fakes) or engine.set_global may
  replace.  The exceptions below are pure drawing helpers that nothing rebinds.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026).
"""

import ast
import subprocess
import sys
from pathlib import Path

PACKAGE = Path(__file__).resolve().parent.parent / "metacat"
ENGINE = sorted(p for p in PACKAGE.glob("*.py") if p.name not in ("__init__.py", "__main__.py"))
DIRECT = {"chez", "objects", "sugar", "utilities", "names"}

# module -> {source module: names}, the from-imports among engine modules as built
ALLOWED = {
    "bridge_graphics": {
        "general_graphics": {"circular_arc", "dashed_circular_arc", "dashed_elliptical_arc",
                             "dashed_line", "dashed_line_points", "dotted_circular_arc",
                             "dotted_elliptical_arc", "dotted_line", "dotted_line_points",
                             "elliptical_arc", "centered_zigzag_line", "zigzag_line_points"},
        "workspace_objects": {"both_spanning_groups_p"},
    },
    "group_graphics": {
        "general_graphics": {"arrowhead", "dashed_box", "dotted_box", "outline_box",
                             "zigzag_line_points"},
    },
    "rule_graphics": {"general_graphics": {"dashed_box", "outline_box"}},
}


def from_imports(path):
    """[(source module, name)] of `from metacat.x import y` with x not a direct module."""
    found = []
    for node in ast.walk(ast.parse(path.read_text())):
        if isinstance(node, ast.ImportFrom):
            assert node.level == 0, f"{path.name}: relative import"
            parts = (node.module or "").split(".")
            if parts[0] == "metacat" and len(parts) > 1 and parts[1] not in DIRECT:
                found += [(parts[1], alias.name) for alias in node.names]
    return found


def test_engine_modules_from_import_only_what_nothing_rebinds():
    for path in ENGINE:
        allowed = ALLOWED.get(path.stem, {})
        for source, name in from_imports(path):
            assert name in allowed.get(source, ()), \
                f"{path.name}: from metacat.{source} import {name}"


def test_the_allowed_names_exist_and_are_not_wrapped():
    from metacat import engine, trace_writer
    import importlib
    wrapped = Path(trace_writer.__file__).read_text()
    for source_names in ALLOWED.values():
        for source, names in source_names.items():
            module = importlib.import_module("metacat." + source)
            for name in names:
                assert callable(getattr(module, name)), (source, name)
                assert name not in wrapped, (source, name)
    assert engine  # the engine's module list is what ENGINE walks
    assert {p.stem for p in ENGINE} >= {m.__name__.split(".")[1]
                                        for m in engine.translated_modules()}


def test_every_engine_module_imports_alone():
    code = (
        "import importlib, sys\n"
        "name = sys.argv[1]\n"
        "importlib.import_module('metacat.' + name)\n"
        "from metacat import chez, engine\n"
        "assert chez._S == 1, 'import drew a random number'\n"
        "assert not engine._loaded, 'import loaded the engine'\n"
        "print('ok')\n")
    procs = {path.stem: subprocess.Popen([sys.executable, "-c", code, path.stem],
                                         cwd=PACKAGE.parent, stdout=subprocess.PIPE,
                                         stderr=subprocess.PIPE, text=True)
             for path in ENGINE}
    failed = {}
    for name, proc in procs.items():
        out, err = proc.communicate(timeout=120)
        if proc.returncode != 0 or out.strip() != "ok":
            failed[name] = err.strip().splitlines()[-1:] if err else out
    assert not failed, failed
