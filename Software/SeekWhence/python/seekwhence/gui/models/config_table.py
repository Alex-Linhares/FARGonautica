"""The configuration pane's model, Qt-free (PANES.md P13; ANSWERS.md 5; G-050, G-051).

A run is its seed and its configuration (DECISION D-069): every knob of the
reconstruction is a field of `seekwhence.config.Config`, a frozen dataclass whose every
field is commented with the source tag that gives its value or the decision (`D-NNN`,
DECISIONS.md) that chose it. `read_fields()` reads those comments with `tokenize`, as the
engine's `tests/test_conventions.py` does, and gives one `FieldNote` per field: its name,
the kind of its value (`int`, `float`, `bool` or `tuple`), its default, its source tags,
its decision numbers and the comment itself, with the section heading above it
(`the taskrack and the run`, `the cytoplasm`, ...). `decision_titles()` reads the
decisions' headings from DECISIONS.md, so that hovering `D-007` can show
`D-007: Taskrack sampling rule`.

`ConfigTable` holds the user's edits as a dict of field name to value and builds the next
run's configuration with `dataclasses.replace(Config(), **edits)`, the way ANSWERS.md 5
says; `conform(value, default)` validates a value against the field's type, nested tables
included, and `parse_text(text, default)` reads what the user typed (an integer, a
decimal, `true`/`false`, or a Python tuple literal for a table), raising `ConfigError`
with a message for anything else. The three fields the controls pane sets (the two caps,
DECISION D-011, and the trace, DECISION D-055) are `CONTROLLED`: the table shows them and
never edits them (G-051). `rows(shown)` gives one `FieldRow` per field for the view, with
the value of the configuration `shown` (the next run's, or a replay's) and whether it
differs from the default. `to_settings`/`from_settings` are the QSettings round trip
(`config/edits`, a JSON text of `[name, value]` pairs, every pair checked; Numbo GUI,
`main_window.py`'s `_restore_settings`).
"""

import ast
import dataclasses
import io
import json
import re
import tokenize
from pathlib import Path

from seekwhence import config as config_module
from seekwhence.config import Config

__all__ = ["CONTROLLED", "SETTINGS_KEY", "ConfigError", "ConfigTable", "FieldNote", "FieldRow",
           "conform", "decision_titles", "default_config", "filter_rows", "kind_of",
           "parse_text", "read_fields", "row_tooltip", "value_text"]

DECISION = re.compile(r"\bD-\d{3}\b")
SOURCE_TAG = re.compile(r"\[(?:M86|FCCA|MHN)\s+pp?\.\s*[ivxlc\d]+(?:\s*[-–]\s*[ivxlc\d]+)?"
                        r"(?:\s*,\s*[ivxlc\d]+(?:\s*[-–]\s*[ivxlc\d]+)?)*\]")
SECTION = re.compile(r"^#\s*-{3,}\s*(.*?)\s*-{3,}\s*$")
# The fields the controls pane (P11) sets: the two caps (DECISION D-011) and the trace
# (DECISION D-055); the configuration pane shows them and does not edit them (G-051).
CONTROLLED = ("max_tasks_per_term", "max_tasks", "verbose")
SETTINGS_KEY = "config/edits"
DECISIONS_PATH = Path(config_module.__file__).resolve().parent.parent / "DECISIONS.md"


class ConfigError(ValueError):
    """A value is not valid for its field (ANSWERS.md 5; G-050)."""


# ---- reading config.py and DECISIONS.md (PANES.md P13) ------------------------------------------------

@dataclasses.dataclass(frozen=True)
class FieldNote:
    """One field of `Config` as `config.py` documents it (PANES.md P13; G-050): its
    `name`, its `kind` (`int`, `float`, `bool` or `tuple`), its `default`, the source
    `tags` (`[M86 p. 105]`) and `decisions` (`D-007`) of its comment, the `comment`
    itself (the `#` and the `DECISION` word stripped, lines joined) and the `section`
    heading above it."""
    name: str
    kind: str
    default: object
    tags: tuple
    decisions: tuple
    comment: str
    section: str


def kind_of(default) -> str:
    """`bool`, `int`, `float` or `tuple`: the kind of a field's value, from its default
    (`bool` before `int`, since a bool is an int) (G-050)."""
    if isinstance(default, bool):
        return "bool"
    if isinstance(default, int):
        return "int"
    if isinstance(default, float):
        return "float"
    if isinstance(default, tuple):
        return "tuple"
    raise ConfigError(f"a Config field of an unknown kind: {type(default).__name__}")


def _comments_by_line(source: str) -> dict:
    out = {}
    for tok in tokenize.generate_tokens(io.StringIO(source).readline):
        if tok.type == tokenize.COMMENT:
            out[tok.start[0]] = tok.string
    return out


def _config_fields(tree: ast.Module) -> list:
    for node in tree.body:
        if isinstance(node, ast.ClassDef) and node.name == "Config":
            return node, [n for n in node.body if isinstance(n, ast.AnnAssign)]
    raise ConfigError("config.py has no class Config")


def _clean(comment: str) -> str:
    text = comment.lstrip("#").strip()
    return re.sub(r"^DECISION\s+", "", text)


def read_fields(path=None) -> tuple:
    """One `FieldNote` per field of `Config`, in field order, read from `config.py`
    (`path`, the engine's by default) with `tokenize` and `ast`, the engine's
    `test_conventions.py` reading done read-only (PANES.md P13; G-050). A field's
    comment is every comment line between the previous field (or the class's docstring)
    and the field, section headings (`# ---- ... ----`) apart; the heading last seen is
    the field's section."""
    source = Path(path or config_module.__file__).read_text()
    tree = ast.parse(source)
    comments = _comments_by_line(source)
    klass, fields = _config_fields(tree)
    defaults = Config()
    notes = []
    floor = klass.body[0].end_lineno if klass.body else klass.lineno
    section = ""
    for field in fields:
        lines = []
        for n in range(floor + 1, field.lineno + 1):
            comment = comments.get(n)
            if comment is None:
                continue
            heading = SECTION.match(comment)
            if heading:
                section = heading.group(1)
            else:
                lines.append(_clean(comment))
        text = " ".join(lines)
        name = field.target.id
        notes.append(FieldNote(name=name, kind=kind_of(getattr(defaults, name)),
                               default=getattr(defaults, name),
                               tags=tuple(dict.fromkeys(SOURCE_TAG.findall(text))),
                               decisions=tuple(dict.fromkeys(DECISION.findall(text))),
                               comment=text, section=section))
        floor = field.end_lineno
    return tuple(notes)


def decision_titles(path=DECISIONS_PATH) -> dict:
    """`D-007` to `D-007: Taskrack sampling rule`: the `###` headings of DECISIONS.md
    (PANES.md P13: hovering a decision number shows its heading; G-050)."""
    text = Path(path).read_text()
    out = {}
    for number, title in re.findall(r"^###\s+(D-\d{3})\s*:\s*(.*?)\s*$", text, flags=re.M):
        out.setdefault(number, f"{number}: {title}")
    return out


def default_config() -> Config:
    """`Config()`, the defaults shown first (ANSWERS.md 5; G-050)."""
    return Config()


# ---- values (ANSWERS.md 5; G-050) ------------------------------------------------------------------

def conform(value, default):
    """`value` checked against the type of `default`, nested tables included, and
    returned in the engine's types (a JSON list back as a tuple, an integer where the
    default is a float as a float); `ConfigError` for anything else (ANSWERS.md 5:
    each value is validated before it is accepted; G-050). A table is a tuple whose
    every entry has the shape of the default's first entry; an empty table is a
    tuple."""
    if isinstance(default, bool):
        if isinstance(value, bool):
            return value
        raise ConfigError(f"expected true or false, got {value!r:.60}")
    if isinstance(default, int):
        if isinstance(value, int) and not isinstance(value, bool):
            return value
        raise ConfigError(f"expected an integer, got {value!r:.60}")
    if isinstance(default, float):
        if isinstance(value, (int, float)) and not isinstance(value, bool):
            return float(value)
        raise ConfigError(f"expected a number, got {value!r:.60}")
    if isinstance(default, str):
        if isinstance(value, str):
            return value
        raise ConfigError(f"expected a name, got {value!r:.60}")
    if isinstance(default, tuple):
        if not isinstance(value, (tuple, list)):
            raise ConfigError(f"expected a tuple, got {value!r:.60}")
        if not default:
            return tuple(value)
        shape = default[0]
        out = []
        for entry in value:
            if isinstance(shape, tuple):
                if not isinstance(entry, (tuple, list)) or len(entry) != len(shape):
                    raise ConfigError(f"expected an entry of {len(shape)} items like "
                                      f"{shape!r}, got {entry!r:.60}")
                out.append(tuple(conform(v, d) for v, d in zip(entry, shape)))
            else:
                out.append(conform(entry, shape))
        return tuple(out)
    raise ConfigError(f"a field of an unknown kind: {type(default).__name__}")


def parse_text(text: str, default):
    """The value the text means for a field with this `default`: an integer, a decimal
    number, `true`/`false` (also `yes`/`no`, `1`/`0`), or a Python literal for a table
    (`(("Same", 10), ("Succ", 8))`, lists allowed), conformed to the field's type;
    `ConfigError` for anything else (ANSWERS.md 5; G-050)."""
    word = str(text).strip()
    if not word:
        raise ConfigError("empty")
    if isinstance(default, bool):
        if word.lower() in ("true", "yes", "1", "on"):
            return True
        if word.lower() in ("false", "no", "0", "off"):
            return False
        raise ConfigError(f"expected true or false, got {word!r:.60}")
    if isinstance(default, int):
        try:
            return int(word)
        except ValueError:
            raise ConfigError(f"expected an integer, got {word!r:.60}") from None
    if isinstance(default, float):
        try:
            return float(word)
        except ValueError:
            raise ConfigError(f"expected a number, got {word!r:.60}") from None
    try:
        value = ast.literal_eval(word)
    except (ValueError, SyntaxError, TypeError, MemoryError, RecursionError):
        raise ConfigError(f"not a Python literal: {word!r:.60}") from None
    return conform(value, default)


def value_text(value) -> str:
    """The text of a value as the pane shows it and as `parse_text` reads it back: `true`
    or `false`, a number, or a tuple literal one entry per line for a table (G-050)."""
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, float)):
        return repr(value)
    if isinstance(value, tuple) and value and all(isinstance(v, tuple) for v in value):
        return "(" + ",\n ".join(repr(v) for v in value) + ")"
    return repr(value)


def _plain(value):
    if isinstance(value, (list, tuple)):
        return tuple(_plain(v) for v in value)
    return value


# ---- the table (ANSWERS.md 5; G-050, G-051) ---------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class FieldRow:
    """One row of the pane (PANES.md P13): the `note` (name, kind, default, tags,
    decisions, comment, section), the `value` of the configuration shown, whether it
    is `edited` (differs from the default), and whether it is `controlled` by the
    controls pane (G-051)."""
    note: FieldNote
    value: object
    edited: bool
    controlled: bool

    @property
    def name(self) -> str:
        """The field's name (G-050)."""
        return self.note.name

    @property
    def source_text(self) -> str:
        """`D-007` or `[M86 p. 105]  D-046`: the tags and decisions, the source column
        (PANES.md P13)."""
        return "  ".join(self.note.tags + self.note.decisions)


class ConfigTable:
    """The user's edits of the configuration, and the rows that show them (ANSWERS.md
    5; G-050, G-051). `edits` maps a field name to its value; `config()` is the next
    run's configuration, `dataclasses.replace(Config(), **edits)`; the `CONTROLLED`
    fields are never in `edits`. `listeners` are called after every change."""

    def __init__(self, notes=None, titles=None):
        self.notes = tuple(notes) if notes is not None else read_fields()
        self.by_name = {note.name: note for note in self.notes}
        self.titles = dict(titles) if titles is not None else decision_titles()
        self.edits = {}
        self.listeners = []

    # -- the edits --------------------------------------------------------------------------

    def config(self, base=None) -> Config:
        """The configuration the edits give: `dataclasses.replace(Config(), **edits)`;
        with a `base`, its `CONTROLLED` fields (the controls' caps and trace) are kept
        and every other field is the default or its edit (ANSWERS.md 5; G-051)."""
        kept = {} if base is None else {name: getattr(base, name) for name in CONTROLLED}
        return dataclasses.replace(Config(), **kept, **self.edits)

    def set_value(self, name: str, value) -> bool:
        """Edit a field: the value conformed to its type (`ConfigError` if it cannot be,
        the edits unchanged); a value equal to the default removes the edit. A
        controlled field is refused with a `ConfigError` (G-051). Returns whether the
        edits changed."""
        note = self._note(name)
        if name in CONTROLLED:
            raise ConfigError(f"{name} is set in the Controls pane")
        value = conform(value, note.default)
        before = dict(self.edits)
        if _plain(value) == _plain(note.default):
            self.edits.pop(name, None)
        else:
            self.edits[name] = value
        return self._changed(before)

    def set_text(self, name: str, text: str) -> bool:
        """Edit a field from the text typed: `parse_text`, then `set_value` (G-050)."""
        return self.set_value(name, parse_text(text, self._note(name).default))

    def reset(self, name: str) -> bool:
        """Reset one field to its default (ANSWERS.md 5; G-050)."""
        self._note(name)
        before = dict(self.edits)
        self.edits.pop(name, None)
        return self._changed(before)

    def reset_all(self) -> bool:
        """Reset every field to its default: the configuration is `Config()` again
        (ANSWERS.md 5; G-050)."""
        before = dict(self.edits)
        self.edits.clear()
        return self._changed(before)

    def set_config(self, config) -> bool:
        """The edits that make `config` out of the defaults: every field but the
        controlled ones that differs from `Config()` (a session file's configuration
        restored, G-051)."""
        before = dict(self.edits)
        defaults = Config()
        self.edits.clear()
        for note in self.notes:
            value = getattr(config, note.name)
            if note.name not in CONTROLLED and _plain(value) != _plain(note.default):
                self.edits[note.name] = conform(value, getattr(defaults, note.name))
        return self._changed(before)

    @property
    def edited_names(self) -> tuple:
        """The fields edited, in field order (G-050)."""
        return tuple(note.name for note in self.notes if note.name in self.edits)

    def _note(self, name):
        try:
            return self.by_name[name]
        except KeyError:
            raise ConfigError(f"Config has no field {name!r}") from None

    def _changed(self, before) -> bool:
        if before == self.edits:
            return False
        for listener in list(self.listeners):
            listener()
        return True

    # -- the rows -----------------------------------------------------------------------------

    def rows(self, shown=None) -> tuple:
        """One `FieldRow` per field, in field order, with the values of `shown` (a
        `Config`: the next run's, or a replay's) or, with none, of the edits over the
        defaults (PANES.md P13; G-050)."""
        config = self.config() if shown is None else shown
        out = []
        for note in self.notes:
            value = getattr(config, note.name)
            out.append(FieldRow(note, value, _plain(value) != _plain(note.default),
                                note.name in CONTROLLED))
        return tuple(out)

    def title_of(self, decision: str) -> str:
        """`D-007: Taskrack sampling rule`, or the number alone if DECISIONS.md lacks
        it (PANES.md P13)."""
        return self.titles.get(decision, decision)

    # -- QSettings (Numbo GUI) ---------------------------------------------------------------

    def to_settings(self) -> dict:
        """`{"config/edits": "[[name, value], ...]"}`, the edits as a JSON text (Numbo
        GUI, `main_window.py`; G-050)."""
        pairs = [[name, self.edits[name]] for name in self.edited_names]
        return {SETTINGS_KEY: json.dumps(pairs)}

    def from_settings(self, values: dict) -> bool:
        """Restore the edits from the settings' `config/edits`: every pair checked
        against its field's type, a bad or unknown one dropped, a text that is not JSON
        ignored (Numbo GUI's `_restore_settings`; G-050). Returns whether any edit was
        restored."""
        text = values.get(SETTINGS_KEY)
        if not isinstance(text, str):
            return False
        try:
            pairs = json.loads(text)
        except ValueError:
            return False
        if not isinstance(pairs, list):
            return False
        before = dict(self.edits)
        self.edits.clear()
        for pair in pairs:
            if not (isinstance(pair, list) and len(pair) == 2 and isinstance(pair[0], str)):
                continue
            name, value = pair
            note = self.by_name.get(name)
            if note is None or name in CONTROLLED:
                continue
            try:
                value = conform(value, note.default)
            except ConfigError:
                continue
            if _plain(value) != _plain(note.default):
                self.edits[name] = value
        self._changed(before)
        return bool(self.edits)


# ---- the view's pure helpers (G-051) -----------------------------------------------------------------

def filter_rows(rows, text: str) -> tuple:
    """The rows whose name, section, source or comment contains `text` (case
    ignored); all of them for an empty text (PANES.md P13: filter by name; G-051)."""
    word = text.strip().lower()
    if not word:
        return tuple(rows)
    return tuple(r for r in rows
                 if word in r.name.lower() or word in r.note.section.lower()
                 or word in r.source_text.lower() or word in r.note.comment.lower())


def row_tooltip(row: FieldRow, titles: dict) -> str:
    """The field's comment, its section, its decisions' headings from DECISIONS.md and,
    when edited, its default (PANES.md P13; G-051)."""
    lines = [f"{row.name} ({row.note.kind})"]
    if row.note.section:
        lines.append(f"section: {row.note.section}")
    lines.append(row.note.comment)
    for decision in row.note.decisions:
        lines.append(titles.get(decision, decision))
    if row.controlled:
        lines.append("set in the Controls pane (P11)")
    if row.edited:
        lines.append(f"default: {value_text(row.note.default)}")
    return "\n".join(lines)
