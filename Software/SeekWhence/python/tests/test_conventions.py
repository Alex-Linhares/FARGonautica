"""The conventions that bind every item of the reconstruction (TASK.md, "Conventions"):

- every `D-NNN` mentioned under `seekwhence/` is a heading in DECISIONS.md;
- every `Config` field has a comment with a source tag or a decision number;
- every public module, class and function docstring under `seekwhence/` cites a source tag
  (`[M86 p. N]`, `[FCCA p. N]`, `[MHN p. N]`) or a decision number.

They pass trivially on the skeleton and fail as soon as a later item forgets a citation.
"""

import ast
import io
import re
import tokenize
from pathlib import Path

import seekwhence

PACKAGE = Path(seekwhence.__file__).resolve().parent
ROOT = PACKAGE.parent
DECISIONS = ROOT / "DECISIONS.md"

DECISION = re.compile(r"\bD-\d{3}\b")
SOURCE_TAG = re.compile(r"\[(M86|FCCA|MHN)\s+pp?\.\s*[ivxlc\d]+(?:\s*[-–]\s*[ivxlc\d]+)?\]")


def modules():
    # seekwhence/gui/ (the optional PySide6 GUI) is checked by tests/gui/; the engine
    # never imports it (loop0004, ANSWERS.md 1).
    return sorted(p for p in PACKAGE.rglob("*.py") if p.relative_to(PACKAGE).parts[0] != "gui")


def cites(text: str | None) -> bool:
    return bool(text) and bool(SOURCE_TAG.search(text) or DECISION.search(text))


def decision_headings() -> set[str]:
    text = DECISIONS.read_text()
    return set(re.findall(r"^###\s+(D-\d{3})\b", text, flags=re.M))


def test_decisions_file_exists_with_the_entry_format():
    text = DECISIONS.read_text()
    assert "### D-007: Taskrack sampling rule" in text  # the format example
    for field in ("**Sources:**", "**Choice:**", "**Reason:**", "**Knob:**", "**Alternatives:**"):
        assert field in text


def test_every_decision_in_the_code_exists_in_decisions_md():
    headings = decision_headings()
    missing = {}
    for path in modules():
        used = set(DECISION.findall(path.read_text()))
        if used - headings:
            missing[path.name] = sorted(used - headings)
    assert not missing, f"decisions mentioned in the code but absent from DECISIONS.md: {missing}"


def test_decision_numbers_are_unique_headings():
    text = DECISIONS.read_text()
    numbers = re.findall(r"^###\s+(D-\d{3})\b", text, flags=re.M)
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
        # The comment may sit on the field's line or on the lines just above it, up to the
        # previous field or statement.
        lines = range(field.lineno, field.lineno - 4, -1)
        text = " ".join(comments.get(n, "") for n in lines)
        if not cites(text):
            uncited.append(field.target.id)
    assert not uncited, f"Config fields without a source tag or decision in a comment: {uncited}"


def test_config_is_a_frozen_dataclass():
    from dataclasses import fields, is_dataclass
    from seekwhence.config import Config
    assert is_dataclass(Config) and Config.__dataclass_params__.frozen
    Config()  # every field has a default
    assert isinstance(fields(Config), tuple)


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
        if not cites(ast.get_docstring(tree)):
            uncited.append(f"{path.name}: module docstring")
        for node in public_definitions(tree):
            if not cites(ast.get_docstring(node)):
                uncited.append(f"{path.name}: {node.name}")
    assert not uncited, "public docstrings without a source tag or decision:\n" \
        + "\n".join(uncited)
