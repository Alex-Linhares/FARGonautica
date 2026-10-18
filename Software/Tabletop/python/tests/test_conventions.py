"""The conventions that bind every item of the reconstruction (TASK.md, "Conventions"):

- every `D-NNN` mentioned under `tabletop/` is a heading in DECISIONS.md, and the headings
  are unique;
- every `Config` field has a comment with a source tag or a decision number;
- every public module, class and function docstring under `tabletop/` cites a source tag
  (`[F95 p. N]`, `[FCCA p. N]`, `[FH91 p. N]`) or a decision number;
- no problem name or object id appears in engine code outside `problems.py`, `reported.py`,
  `bench.py` (or a `bench/` package) and `__main__.py` (docstrings and comments may);
- no file under `Software/Tabletop/` names the unpublished engineering book whose
  conventions the code follows (ANSWERS.md 10): the conventions are stated in the package's
  own words, with no attribution;
- no file under `Software/Tabletop/` names the paper the roles come from, or its project
  (the roles loop's ANSWERS.md Q5): roles, the re-cast and the consumers are described in
  the package's own words, with French's pages where he supports them;
- nothing can turn roles off (the roles loop's ANSWERS.md Q1, Q3 and C4; D-078): no
  `tabletop/roles/` subpackage, no `Config` field, profile or command-line option that
  switches roles off or makes them optional, no boolean about roles, and every role knob
  rejected at zero or below. Roles may change during a run; the mechanism may not be
  turned off.

They pass on the skeleton and fail as soon as a later item forgets one.
"""

import ast
import io
import re
import tokenize
from pathlib import Path

import pytest

import tabletop
from test_standalone import is_generated          # the build artefacts every scanner skips

PACKAGE = Path(tabletop.__file__).resolve().parent
PYTHON_DIR = PACKAGE.parent
DECISIONS = PYTHON_DIR / "DECISIONS.md"

DECISION = re.compile(r"\bD-\d{3}\b")
SOURCE_TAG = re.compile(r"\[(F95|FCCA|FH91)\s+pp?\.\s*\d+(?:\s*[-–]\s*\d+)?\]")
PROBLEM_NAME = re.compile(r"(surround|blockage|buridan|figure[-_ ]?ground|ch2[-_ ]?fig)[-_ ]?\d",
                          re.I)
NAME_ALLOWED_FILES = {"problems.py", "reported.py", "bench.py", "__main__.py"}


def modules() -> list[Path]:
    # tabletop/gui/ (loop0006) is scanned by tests/gui/test_gui_conventions.py instead.
    return sorted(p for p in PACKAGE.rglob("*.py") if p.relative_to(PACKAGE).parts[0] != "gui")


def cites(text: str | None) -> bool:
    return bool(text) and bool(SOURCE_TAG.search(text) or DECISION.search(text))


def decision_headings() -> list[str]:
    return re.findall(r"^###\s+(D-\d{3})\b", DECISIONS.read_text(), flags=re.M)


def test_decisions_file_has_the_entry_format():
    text = DECISIONS.read_text()
    assert "### D-064: " in text
    for field in ("**Kind:**", "**Sources:**", "**Choice:**", "**Reason:**", "**Knob:**",
                  "**Alternatives:**"):
        assert field in text, field


def test_every_decision_in_the_code_is_a_heading_in_decisions_md():
    headings = set(decision_headings())
    missing = {}
    for path in modules():
        used = set(DECISION.findall(path.read_text()))
        if used - headings:
            missing[path.name] = sorted(used - headings)
    assert not missing, f"decisions mentioned in the code but absent from DECISIONS.md: {missing}"


def test_decision_numbers_are_unique_headings():
    numbers = decision_headings()
    assert len(numbers) == len(set(numbers)), "a decision number is used twice"


def comments_by_line(source: str) -> dict[int, str]:
    out = {}
    for tok in tokenize.generate_tokens(io.StringIO(source).readline):
        if tok.type == tokenize.COMMENT:
            out[tok.start[0]] = tok.string
    return out


def config_fields(tree: ast.Module) -> list[ast.AnnAssign]:
    for node in tree.body:
        if isinstance(node, ast.ClassDef) and node.name == "Config":
            return [n for n in node.body if isinstance(n, ast.AnnAssign)]
    raise AssertionError("config.py has no class Config")


def test_every_config_field_is_commented_with_a_tag_or_decision():
    source = (PACKAGE / "config.py").read_text()
    tree = ast.parse(source)
    comments = comments_by_line(source)
    uncited = []
    for field in config_fields(tree):
        # The comment may sit on the field's lines or on the lines just above them.
        lines = range(field.end_lineno, field.lineno - 4, -1)
        text = " ".join(comments.get(n, "") for n in lines)
        if not cites(text):
            uncited.append(field.target.id)
    assert not uncited, f"Config fields without a source tag or decision in a comment: {uncited}"


def test_config_is_a_frozen_dataclass_with_defaults_and_named_profiles():
    from dataclasses import fields, is_dataclass
    from tabletop.config import PROFILES, Config
    assert is_dataclass(Config) and Config.__dataclass_params__.frozen
    Config()  # every field has a default
    assert isinstance(fields(Config), tuple)
    assert "standard" in PROFILES
    assert all(isinstance(cfg, Config) for cfg in PROFILES.values())


def public_definitions(tree: ast.Module):
    """Every public class and function, at module level or inside a public class."""
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            if node.name.startswith("_"):
                continue
            yield node
            if isinstance(node, ast.ClassDef):
                for sub in node.body:
                    if (isinstance(sub, (ast.FunctionDef, ast.AsyncFunctionDef))
                            and not sub.name.startswith("_")):
                        yield sub


def test_every_public_docstring_cites_a_source():
    uncited = []
    for path in modules():
        tree = ast.parse(path.read_text(), filename=str(path))
        if path.read_text().strip() and not cites(ast.get_docstring(tree)):
            uncited.append(f"{path.name}: module docstring")
        for node in public_definitions(tree):
            if not cites(ast.get_docstring(node)):
                uncited.append(f"{path.name}: {node.name}")
    assert not uncited, "public docstrings without a source tag or decision:\n" \
        + "\n".join(uncited)


def docstring_ids(tree: ast.Module) -> set[int]:
    return {id(n.body[0].value) for n in ast.walk(tree)
            if isinstance(n, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef))
            and n.body and isinstance(n.body[0], ast.Expr)
            and isinstance(n.body[0].value, ast.Constant)}


def test_no_problem_names_in_engine_code():
    """The engine never special-cases a problem or an object (TASK.md, "Never tune")."""
    offenders = []
    for path in modules():
        rel = path.relative_to(PACKAGE)
        if path.name in NAME_ALLOWED_FILES or "bench" in rel.parts[:-1]:
            continue
        tree = ast.parse(path.read_text(), filename=str(path))
        skip = docstring_ids(tree)
        for node in ast.walk(tree):
            if (isinstance(node, ast.Constant) and isinstance(node.value, str)
                    and id(node) not in skip and PROBLEM_NAME.search(node.value)):
                offenders.append(f"{rel}:{node.lineno} {node.value[:40]!r}")
            elif isinstance(node, ast.Name) and PROBLEM_NAME.search(node.id):
                offenders.append(f"{rel}:{node.lineno} {node.id}")
    assert not offenders, "problem names in engine code:\n" + "\n".join(offenders)


# The unpublished book is never named under Software/Tabletop/ (ANSWERS.md 10). The patterns
# are assembled so that this file does not match itself.
FORBIDDEN_WORDS = re.compile("|".join(["how to co" + "de", "farg" + "book", "bo" + "ok/"]), re.I)
FOLDER = PYTHON_DIR.parent


# The paper the roles come from, and its project, are never named either (the roles loop's
# ANSWERS.md Q5). The patterns are built from pieces so that this file does not match itself;
# the word boundary keeps "FARGonautica" and `FARGONAUTICA` legal.
PAPER_WORDS = re.compile("|".join([r"\b" + "ar" + "go" + r"\b", "loo" + "py",
                                   "abstract roles " + "work"]), re.I)


def offending_lines(pattern: re.Pattern) -> list:
    """Every line of every text file under Software/Tabletop/ that matches `pattern`."""
    offenders = []
    for path in sorted(FOLDER.rglob("*")):
        if not path.is_file() or any(part.startswith(".") for part in path.relative_to(FOLDER).parts):
            continue
        if "__pycache__" in path.parts or is_generated(path):
            continue
        try:
            text = path.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            continue
        for n, line in enumerate(text.splitlines(), 1):
            if pattern.search(line):
                offenders.append(f"{path.relative_to(FOLDER)}:{n}: {line.strip()[:80]}")
    return offenders


def test_the_unpublished_book_is_never_named():
    offenders = offending_lines(FORBIDDEN_WORDS)
    assert not offenders, "the book is named under Software/Tabletop/:\n" + "\n".join(offenders)


def test_the_role_paper_is_never_named():
    offenders = offending_lines(PAPER_WORDS)
    assert not offenders, "the role paper or its project is named under Software/Tabletop/:\n" \
        + "\n".join(offenders)


# ---- no roles-off option (the roles loop's ANSWERS.md Q1, Q3 and C4; D-078) --------------

# The names that would switch roles off or make them optional, as the roles loop's gate
# matches them against Config fields, profile names and command-line options.
SWITCH_NAME = re.compile(
    r"(^|[_-])(no|without|disable[ds]?|enable[ds]?|skip|use|with)[_-]roles?($|[_-])"
    r"|(^|[_-])roles?[_-](off|on|enabled?|disabled?|switch|mode|flag|ablat\w*|lesion\w*)($|[_-])"
    r"|^-*roles?$|^extensions?$|^-*v1$|^-*pre[_-]?roles?$",
    re.I)
SWITCH_PROFILE = re.compile(r"role|v1|baseline|legacy|no[-_]", re.I)
PROBE_HOMES = ("tests", "tools")      # the lesion probes live there and only there


def cli_option_strings() -> list:
    """(file, option) for every `add_argument` string and `dest` under `tabletop/`: the
    engine's, the benches' and the GUI's command lines, read statically so that the GUI's
    toolkit need not be importable."""
    out = []
    for path in sorted(PACKAGE.rglob("*.py")):
        for node in ast.walk(ast.parse(path.read_text(), filename=str(path))):
            if (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
                    and node.func.attr == "add_argument"):
                for a in node.args:
                    if isinstance(a, ast.Constant) and isinstance(a.value, str):
                        out.append((path.relative_to(PYTHON_DIR).as_posix(), a.value))
                for kw in node.keywords:
                    if kw.arg == "dest" and isinstance(kw.value, ast.Constant):
                        out.append((path.relative_to(PYTHON_DIR).as_posix(), str(kw.value.value)))
    return out


def role_knobs() -> list:
    """The `Config` fields about roles that carry a value: (name, a copy of the default
    with every number set to 0, the same at -1). Vacuous until `tabletop/roles.py` exists."""
    import dataclasses
    from tabletop.config import Config
    if not (PACKAGE / "roles.py").exists():
        return []

    def with_every_number(value, bad):
        if isinstance(value, bool):
            return value
        if isinstance(value, (int, float)):
            return bad
        if isinstance(value, tuple):
            return tuple(with_every_number(v, bad) for v in value)
        return value

    out = []
    for field in dataclasses.fields(Config):
        if "role" not in field.name.lower():
            continue
        default = getattr(Config(), field.name)
        zeroed = with_every_number(default, 0)
        if zeroed != default:
            out.append((field.name, zeroed, with_every_number(default, -1)))
    return out


def test_there_is_no_roles_off_option():
    import dataclasses
    from tabletop.config import PROFILES, Config
    assert not (PACKAGE / "roles").is_dir(), "tabletop/roles/ is a subpackage: roles are not separable"
    names = [f.name for f in dataclasses.fields(Config)]
    switches = [n for n in names if SWITCH_NAME.search(n)]
    assert not switches, f"Config fields that read as a roles switch: {switches}"
    booleans = [n for n in names if "role" in n.lower() and isinstance(getattr(Config(), n), bool)]
    assert not booleans, f"boolean Config fields about roles: {booleans}"
    if "group_position_match" in names:
        assert "answer_from_group_position" not in names, "D-058's boolean survives beside group_position_match"
    profiles = [p for p in PROFILES if SWITCH_PROFILE.search(p)]
    assert not profiles, f"profiles that read as roles-off or baseline: {profiles}"
    options = [(f, o) for f, o in cli_option_strings()
               if SWITCH_NAME.search(o.strip("-").replace("-", "_"))]
    assert not options, f"command-line options that read as a roles switch: {options}"
    probes = [p.relative_to(PYTHON_DIR).as_posix() for p in PYTHON_DIR.rglob("role_probes.py")
              if p.relative_to(PYTHON_DIR).parts[0] not in PROBE_HOMES]
    assert not probes, f"the probe helpers live only in tests/ and tools/: {probes}"
    for name, zeroed, negative in role_knobs():
        for bad in (zeroed, negative):
            with pytest.raises(ValueError):
                dataclasses.replace(Config(), **{name: bad})
