"""Every Franz built-in of the census is covered by numbo.franz or is plain Python.

The census is read from the oracle's own files, so a built-in added there
can't be forgotten here:
  - the table "Franz built-ins used by the source (census)" in
    lisp/src/PORTING_NOTES.md (first column, every `name`);
  - *franz-builtins-used* in lisp/tests/franz-compat-tests.lisp;
  - every defun/defmacro of lisp/src/franz-compat.lisp (this adds max, min and
    the helpers the table doesn't list);
  - the functions lisp/src/oracle.lisp shadows (random, float, sqrt).
"""

import re

from conftest import LISP_DIR, REPO_DIR
from numbo import franz, rng


def census_table():
    text = (LISP_DIR / "src" / "PORTING_NOTES.md").read_text(encoding="utf-8")
    section = text.split("#### Franz built-ins used by the source (census)", 1)[1]
    section = section.split("\n####", 1)[0]
    names = set()
    for line in section.splitlines():
        if line.startswith("| `"):
            first_cell = line.split("|")[1]
            names.update(re.findall(r"`([^`]+)`", first_cell))
    return names


def census_test_list():
    text = (LISP_DIR / "tests" / "franz-compat-tests.lisp").read_text(encoding="utf-8")
    body = re.search(r"\(defparameter \*franz-builtins-used\*\s*'\((.*?)\)\)",
                     text, re.S).group(1)
    return set(body.split())


def franz_compat_definitions():
    text = (LISP_DIR / "src" / "franz-compat.lisp").read_text(encoding="utf-8")
    return set(re.findall(r"^\(def(?:un|macro) (\S+)", text, re.M))


def oracle_shadows():
    text = (LISP_DIR / "src" / "oracle.lisp").read_text(encoding="utf-8")
    body = re.search(r"\(shadow '\(([^)]*)\)", text).group(1)
    return {s.strip('"').lower() for s in body.split()}


def test_census_sources_parse():
    assert {"quotient", "sortcar", "concat", "*mod", "if", "defun"} <= census_table()
    assert {"nequal", "uconcat", "/"} <= census_test_list()
    assert {"max", "min", "invert-case", "franz-pname"} <= franz_compat_definitions()
    assert oracle_shadows() == {"random", "float", "sqrt"}


def test_every_builtin_is_covered_or_plain_python():
    census = (census_table() | census_test_list() | franz_compat_definitions()
              | oracle_shadows())
    covered = set(franz.LISP_NAMES) | set(franz.PLAIN_PYTHON) | {"random"}
    missing = sorted(census - covered)
    assert not missing, f"not covered and not noted as plain Python: {missing}"
    assert not set(franz.LISP_NAMES) & set(franz.PLAIN_PYTHON)


def test_covered_names_exist():
    for lisp, py in franz.LISP_NAMES.items():
        fn = getattr(franz, py, None)
        assert callable(fn), (lisp, py)
        assert f"franz-compat.lisp: {lisp}" in fn.__doc__ or \
            f"oracle.lisp: {lisp}" in fn.__doc__, (lisp, py)
    for lisp, how in franz.PLAIN_PYTHON.items():
        assert isinstance(how, str) and how, lisp
    assert callable(rng.Rng.random)
