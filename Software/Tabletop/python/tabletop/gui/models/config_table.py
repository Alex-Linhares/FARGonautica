"""The configuration pane's model, Qt-free (PANES.md P13; ANSWERS.md 3; Seek-Whence GUI,
`models/config_table.py`; G-046, G-047).

A run is its problem, its seed and its `Config` (D-064, D-066): every knob of the
reconstruction is a field of the frozen dataclass `tabletop.config.Config`, commented in
the source with the page tag that gives its value or the decision (`D-NNN`) that chose
it. `read_fields()` reads those comments from `config.py` the way `tools/knobs_table.py`
reads them for the README's table of knobs (the same three patterns: the group heading,
the comment lines and the field line; the `DECISION` word dropped), and gives one
`FieldNote` per field: its name, the kind of its value (`bool`, `int`, `float`, `str` or
`tuple`), the `standard` default, the source tags (`[F95 p. 66; F95 p. 93]`, a bracket
kept whole), the decision numbers, the comment and the group heading above it.
`decision_titles()` reads the `### D-NNN: title` headings of DECISIONS.md, so that
hovering `D-061` can show `D-061: The run cap`.

`ConfigTable` holds the chosen profile and the user's edits (a dict of field name to
value) and builds the next run's configuration as ANSWERS.md 3 says:
`dataclasses.replace(PROFILES[profile], **edits)`. `conform(value, default)` validates a
value against its field's type, the tables entry by entry, and `parse_text(text,
default)` reads what the user typed (an integer, a decimal, `true`/`false`, a name, or a
Python tuple literal for a table), raising `ConfigError` with a message for anything
else. The `profile` field is set by the profile selector, never typed (`PROFILE_FIELD`).
`rows()` gives one `FieldRow` per field for the view: the value of the next run's
configuration, the profile's own value, whether the field is edited (differs from the
profile's value), whether the profile differs from `standard` there
(`Config.differences`), whether it is one of French's faults (`FAULT_KNOBS`, fixed in
`standard` and restored in `french-faults`) and whether it is a calibrated knob
(`CALIBRATION_KNOBS`, D-076). `to_settings`/`from_settings` are the QSettings round trip
of the edits (`config/edits`, one JSON text of `[name, value]` pairs, every pair checked;
the profile itself is the controls' `controls/profile`, G-044). `edited_fields(config)`
and `config_note_of(config)` say how a started run's configuration differs from its
profile, for the title and the stats pane (ANSWERS.md 3).
"""

import ast
import dataclasses
import json
import re
from pathlib import Path

from tabletop import config as config_module
from tabletop.config import CALIBRATION_KNOBS, FAULT_KNOBS, PROFILES, Config
from tabletop.gui.models.run_stats import config_note

__all__ = ["DECISIONS_PATH", "PROFILE_FIELD", "SETTINGS_KEY", "STANDARD", "ConfigError",
           "ConfigTable", "FieldNote", "FieldRow", "config_note_of", "conform",
           "decision_titles", "edited_fields", "filter_rows", "kind_of", "parse_text",
           "read_fields", "row_tooltip", "value_text"]

# `tools/knobs_table.py`'s three patterns, which read the comments of `config.py` for the
# README's table of knobs (D-066); kept the same here so that the pane shows what the
# README shows.
GROUP = re.compile(r"^    # ---- (.*?) -+$")
FIELD = re.compile(r"^    ([a-z_][a-z0-9_]*): [^=]+ = (.*)$")
COMMENT = re.compile(r"^    # (.*)$")
SOURCE_TAG = re.compile(r"\[(?:F95|FCCA|FH91)[^\]]*\]")
DECISION = re.compile(r"\bD-\d{3}\b")
HEADING = re.compile(r"^###\s+(D-\d{3})\s*:\s*(.*?)\s*$", flags=re.M)
PROFILE_FIELD = "profile"
STANDARD = "standard"
SETTINGS_KEY = "config/edits"
DECISIONS_PATH = Path(config_module.__file__).resolve().parent.parent / "DECISIONS.md"
FAULT_NUMBERS = {name: n for n, (name, _value) in enumerate(FAULT_KNOBS, 1)}
FAULT_VALUES = dict(FAULT_KNOBS)
CALIBRATED = {field: profile for profile, field in CALIBRATION_KNOBS}


class ConfigError(ValueError):
    """A value is not valid for its field, or a field cannot be edited (ANSWERS.md 3;
    G-046)."""


# ---- reading config.py and DECISIONS.md (PANES.md P13) -----------------------------------

@dataclasses.dataclass(frozen=True)
class FieldNote:
    """One field of `Config` as `config.py` documents it (PANES.md P13; G-046): its
    `name`, its `kind` (`bool`, `int`, `float`, `str` or `tuple`), the `standard`
    `default`, the source `tags` (each bracket whole, `[F95 p. 66; F95 p. 93]`), the
    `decisions` (`D-032`), the `comment` as `tools/knobs_table.py` joins it and the
    `section`, the group heading above the field."""
    name: str
    kind: str
    default: object
    tags: tuple
    decisions: tuple
    comment: str
    section: str


def kind_of(default) -> str:
    """`bool`, `int`, `float`, `str` or `tuple`: the kind of a field's value, from its
    default (`bool` before `int`, since a bool is an int) (G-046)."""
    if isinstance(default, bool):
        return "bool"
    if isinstance(default, int):
        return "int"
    if isinstance(default, float):
        return "float"
    if isinstance(default, str):
        return "str"
    if isinstance(default, tuple):
        return "tuple"
    raise ConfigError(f"a Config field of an unknown kind: {type(default).__name__}")


def _class_body(source: str) -> str:
    body = source[source.index("class Config"):]
    return body[:body.index("\ndef ")] if "\ndef " in body else body


def read_fields(path=None) -> tuple:
    """One `FieldNote` per field of `Config`, in field order, read from `config.py`
    (`path`, the engine's by default) as `tools/knobs_table.py`'s `fields_of` reads it:
    a field's comment is the `#` lines just above it, a `# ---- group ----` line is its
    section, and a blank line between does not break the comment (PANES.md P13;
    G-046). The values are the dataclass's own defaults."""
    source = Path(path or config_module.__file__).read_text(encoding="utf-8")
    defaults = Config()
    notes, section, comment = [], "", []
    for line in _class_body(source).splitlines():
        m = GROUP.match(line)
        if m:
            section, comment = m.group(1), []
            continue
        m = COMMENT.match(line)
        if m:
            comment.append(m.group(1).strip())
            continue
        m = FIELD.match(line)
        if m:
            text = " ".join(comment).replace("DECISION D-", "D-")
            name = m.group(1)
            default = getattr(defaults, name)
            notes.append(FieldNote(name=name, kind=kind_of(default), default=default,
                                   tags=tuple(dict.fromkeys(SOURCE_TAG.findall(text))),
                                   decisions=tuple(dict.fromkeys(DECISION.findall(text))),
                                   comment=text, section=section))
            comment = []
            continue
        if line.strip() == "":
            continue
        comment = []
    return tuple(notes)


def decision_titles(path=DECISIONS_PATH) -> dict:
    """`D-061` to `D-061: The run cap`: the `###` headings of DECISIONS.md, for the hover
    text (PANES.md P13; G-046)."""
    out = {}
    for number, title in HEADING.findall(Path(path).read_text(encoding="utf-8")):
        out.setdefault(number, f"{number}: {title}")
    return out


# ---- values (ANSWERS.md 3; G-046) ------------------------------------------------------------

def conform(value, default, top=True):
    """`value` checked against the type of `default` and returned in the engine's types,
    `ConfigError` otherwise (ANSWERS.md 3: each edit is validated against the field's
    type; G-046): a bool must be a bool, an int an int (not a bool), a float a number
    (an integer becomes a float at the top level, and stays an integer inside a table,
    as the engine's own tables mix them), a name a string, a table a tuple (or a list,
    as JSON gives it) whose every entry has the shape of the default's first entry."""
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
            return float(value) if top else value
        raise ConfigError(f"expected a number, got {value!r:.60}")
    if isinstance(default, str):
        if isinstance(value, str):
            return value
        raise ConfigError(f"expected a name, got {value!r:.60}")
    if isinstance(default, tuple):
        return _conform_table(value, default)
    raise ConfigError(f"a field of an unknown kind: {type(default).__name__}")


def _conform_table(value, default) -> tuple:
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
            out.append(tuple(conform(v, d, top=False) for v, d in zip(entry, shape)))
        else:
            out.append(conform(entry, shape, top=False))
    return tuple(out)


def parse_text(text: str, default):
    """The value the text means for a field with this `default`: an integer, a decimal
    number, `true`/`false` (also `yes`/`no`, `1`/`0`, `on`/`off`), a name as typed, or a
    Python literal for a table (`(("diagonal", 1.0), ("opposite", 0.7))`, lists
    allowed), conformed to the field's type; `ConfigError` otherwise (G-046)."""
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
    if isinstance(default, str):
        return word
    try:
        value = ast.literal_eval(word)
    except (ValueError, SyntaxError, TypeError, MemoryError, RecursionError):
        raise ConfigError(f"not a Python literal: {word!r:.60}") from None
    return conform(value, default)


def value_text(value) -> str:
    """The text of a value as the pane shows it and as `parse_text` reads it back: `true`
    or `false`, a number, a name, or a tuple literal one entry per line for a table of
    pairs (G-046)."""
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, float)):
        return repr(value)
    if isinstance(value, str):
        return value
    if isinstance(value, tuple) and value and all(isinstance(v, tuple) for v in value):
        return "(" + ",\n ".join(repr(v) for v in value) + ")"
    return repr(value)


def _plain(value):
    if isinstance(value, (list, tuple)):
        return tuple(_plain(v) for v in value)
    return value


# ---- a started run's configuration (ANSWERS.md 3; G-047) -------------------------------------

def edited_fields(config) -> tuple:
    """The fields on which `config` differs from its own profile (`config.profile`, the
    name `dataclasses.replace` keeps), in field order and without `profile`; empty when
    the profile is unknown (ANSWERS.md 3: the title and the stats pane mark a run whose
    configuration differs from its profile; G-047)."""
    base = PROFILES.get(config.profile)
    if base is None:
        return ()
    return tuple(name for name in config.differences(base) if name != PROFILE_FIELD)


def config_note_of(config) -> str:
    """The configuration line the stats pane prints for a run of `config`, from the
    fields `RunStarted.changed` would report, which the engine takes against `standard`
    (D-066; G-045, G-047): `standard (the defaults)`, `human: type_weight=50`,
    `standard: max_codelets=400`."""
    changed = tuple(sorted((k, repr(v)) for k, v in
                           config.differences(PROFILES[STANDARD]).items()))
    return config_note(config.profile, changed)


# ---- the table (ANSWERS.md 3; G-046, G-047) ------------------------------------------------

@dataclasses.dataclass(frozen=True)
class FieldRow:
    """One row of the pane (PANES.md P13; G-046): the `note`, the `value` of the next
    run's configuration, the `profile_value` of the chosen profile, whether the field is
    `edited` (the value differs from the profile's), whether the profile `differs` from
    `standard` there (`Config.differences`), the `fault` number when it is one of French's
    seven faults (`FAULT_KNOBS`, the faulty value in `fault_value`), the profile it is
    `calibrated` for (`CALIBRATION_KNOBS`, D-076) and whether it is `fixed`, the `profile`
    field the selector sets."""
    note: FieldNote
    value: object
    profile_value: object
    edited: bool
    differs: bool
    fault: int | None
    fault_value: object
    calibrated: str | None
    fixed: bool

    @property
    def name(self) -> str:
        """The field's name (G-046)."""
        return self.note.name

    @property
    def source_text(self) -> str:
        """`[F95 p. 66; F95 p. 93]  D-032`: the tags and the decisions, the source
        column (PANES.md P13)."""
        return "  ".join(self.note.tags + self.note.decisions)

    @property
    def marks_text(self) -> str:
        """The marks column: `French's fault 3 (french-faults: 15)`, `calibrated for
        human (D-076)`, `differs from standard`, joined by `; ` (ANSWERS.md 3; G-046)."""
        parts = []
        if self.fault is not None:
            parts.append(f"French's fault {self.fault} (french-faults: "
                         f"{value_text(self.fault_value)})")
        if self.calibrated is not None:
            parts.append(f"calibrated for {self.calibrated} (D-076)")
        if self.differs:
            parts.append("differs from standard")
        if self.fixed:
            parts.append("set by the profile selector")
        return "; ".join(parts)


class ConfigTable:
    """The chosen profile and the user's edits, and the rows that show them (ANSWERS.md
    3; G-046, G-047). `edits` maps a field name to its value; `config()` is the next
    run's configuration, `dataclasses.replace(PROFILES[profile], **edits)`; `listeners`
    are called after every change."""

    def __init__(self, notes=None, titles=None, profile=STANDARD):
        self.notes = tuple(notes) if notes is not None else read_fields()
        self.by_name = {note.name: note for note in self.notes}
        self.titles = dict(titles) if titles is not None else decision_titles()
        self.profile = STANDARD
        self.edits = {}
        self.listeners = []
        self.set_profile(profile)

    # -- the profile and the edits ------------------------------------------------------------

    @property
    def base(self) -> Config:
        """The chosen profile's configuration, `PROFILES[profile]` (D-066)."""
        return PROFILES[self.profile]

    def config(self) -> Config:
        """The next run's configuration: `dataclasses.replace(PROFILES[profile],
        **edits)` (ANSWERS.md 3; G-046)."""
        return dataclasses.replace(self.base, **self.edits)

    def set_profile(self, name: str) -> bool:
        """Choose the profile whose values the pane shows and the edits apply to; the
        edits stay as they are (G-047). `ConfigError` for an unknown name. Returns
        whether anything changed."""
        if name not in PROFILES:
            raise ConfigError(f"no profile named {name!r}")
        if name == self.profile:
            return False
        self.profile = name
        self._changed()
        return True

    def set_value(self, name: str, value) -> bool:
        """Edit a field: the value conformed to its type (`ConfigError` if it cannot be,
        the edits unchanged); a value equal to the profile's removes the edit; the
        `profile` field is refused (G-046). Returns whether the edits changed."""
        note = self._note(name)
        if name == PROFILE_FIELD:
            raise ConfigError("the profile is chosen with the selector")
        value = conform(value, note.default)
        before = dict(self.edits)
        if _plain(value) == _plain(getattr(self.base, name)):
            self.edits.pop(name, None)
        else:
            self.edits[name] = value
        return self._changed(before)

    def set_text(self, name: str, text: str) -> bool:
        """Edit a field from the text typed: `parse_text`, then `set_value` (G-046)."""
        return self.set_value(name, parse_text(text, self._note(name).default))

    def reset(self, name: str) -> bool:
        """Reset one field to the profile's value (ANSWERS.md 3; G-046)."""
        self._note(name)
        before = dict(self.edits)
        self.edits.pop(name, None)
        return self._changed(before)

    def reset_all(self) -> bool:
        """Reset every field: the configuration is the profile's again (ANSWERS.md 3;
        G-046)."""
        before = dict(self.edits)
        self.edits.clear()
        return self._changed(before)

    def set_config(self, config: Config) -> bool:
        """The profile and the edits that make `config`: its own profile when it is one
        of `PROFILES` (else the profile stays), and every field that differs from that
        profile (a session file's configuration restored, G-047)."""
        before = (self.profile, dict(self.edits))
        if config.profile in PROFILES:
            self.profile = config.profile
        self.edits.clear()
        for name in edited_fields(dataclasses.replace(config, profile=self.profile)):
            self.edits[name] = conform(getattr(config, name), self.by_name[name].default)
        if before == (self.profile, self.edits):
            return False
        self._changed()
        return True

    @property
    def edited_names(self) -> tuple:
        """The fields whose edit differs from the profile's value, in field order
        (G-046)."""
        return tuple(note.name for note in self.notes if note.name in self.edits
                     and _plain(self.edits[note.name]) != _plain(getattr(self.base, note.name)))

    def edited_text(self) -> str:
        """`edited: max_codelets, type_weight`, or nothing (ANSWERS.md 3; G-047)."""
        names = self.edited_names
        return f"edited: {', '.join(names)}" if names else ""

    def _note(self, name):
        try:
            return self.by_name[name]
        except KeyError:
            raise ConfigError(f"Config has no field {name!r}") from None

    def _changed(self, before=None) -> bool:
        if before is not None and before == self.edits:
            return False
        for listener in list(self.listeners):
            listener()
        return True

    # -- the rows -------------------------------------------------------------------------------

    def rows(self, shown=None) -> tuple:
        """One `FieldRow` per field, in field order, for the next run's configuration or
        for `shown` (a `Config`: a replay's), the profile's values and marks beside each
        (PANES.md P13; ANSWERS.md 3; G-046)."""
        config = self.config() if shown is None else shown
        base = PROFILES.get(config.profile, self.base)
        standard = PROFILES[STANDARD]
        out = []
        for note in self.notes:
            name = note.name
            value, profile_value = getattr(config, name), getattr(base, name)
            out.append(FieldRow(note, value, profile_value,
                                _plain(value) != _plain(profile_value),
                                _plain(profile_value) != _plain(getattr(standard, name)),
                                FAULT_NUMBERS.get(name), FAULT_VALUES.get(name),
                                CALIBRATED.get(name), name == PROFILE_FIELD))
        return tuple(out)

    def title_of(self, decision: str) -> str:
        """`D-061: The run cap`, or the number alone if DECISIONS.md lacks it
        (PANES.md P13)."""
        return self.titles.get(decision, decision)

    # -- QSettings (Numbo GUI; ANSWERS.md 3) ------------------------------------------------

    def to_settings(self) -> dict:
        """`{"config/edits": "[[name, value], ...]"}`, the edits as one JSON text (Numbo
        GUI, `main_window.py`; G-046)."""
        pairs = [[name, self.edits[name]] for name in self.edits]
        return {SETTINGS_KEY: json.dumps(pairs)}

    def from_settings(self, values: dict) -> bool:
        """Restore the edits from the settings' `config/edits`: every pair checked
        against its field's type, a bad or unknown one dropped, a text that is not a
        JSON list ignored (Numbo GUI's `_restore_settings`; G-046). Returns whether any
        edit was restored."""
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
            if note is None or name == PROFILE_FIELD:
                continue
            try:
                self.edits[name] = conform(value, note.default)
            except ConfigError:
                continue
        self._changed(before)
        return bool(self.edits)


# ---- the view's pure helpers (G-047) -------------------------------------------------------

def filter_rows(rows, text: str) -> tuple:
    """The rows whose name, section, source, marks or comment contains `text` (case
    ignored); all of them for an empty text (PANES.md P13: a filter; G-047)."""
    word = text.strip().lower()
    if not word:
        return tuple(rows)
    return tuple(r for r in rows
                 if word in r.name.lower() or word in r.note.section.lower()
                 or word in r.source_text.lower() or word in r.marks_text.lower()
                 or word in r.note.comment.lower())


def row_tooltip(row: FieldRow, titles: dict) -> str:
    """The field's comment, its section, its decisions' headings from DECISIONS.md, its
    marks and, when edited, the profile's value (PANES.md P13; G-047)."""
    lines = [f"{row.name} ({row.note.kind})"]
    if row.note.section:
        lines.append(f"section: {row.note.section}")
    lines.append(row.note.comment)
    for decision in row.note.decisions:
        lines.append(titles.get(decision, decision))
    if row.marks_text:
        lines.append(row.marks_text)
    if row.edited:
        lines.append(f"profile's value: {value_text(row.profile_value)}")
    return "\n".join(lines)
