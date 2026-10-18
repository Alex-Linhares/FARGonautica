"""The controls pane's model, Qt-free (PANES.md P11; Numbo GUI, `controls.py` and
`run_stats.py`'s parsers; G-048, G-049).

A run is its seed and its configuration (DECISION D-069), plus the console lines fed
and the answers to the guesses [M86 pp. 164-179]. The controls hold what the user chooses
before pressing Play, as plain text, in a `ControlValues`: the `preset` (an appendix run
with Meredith's inputs, commands and answers [M86 pp. 164-179]; a sequence of the catalogue
[M86 pp. 6-7, 38, 88, 145-150; FCCA pp. 53-54] (DECISION D-070); or `custom`, free terms),
the free `terms`, the `seed`, the two caps `cap_term` and `cap_total` (DECISION D-011), the
`answering` mode (`interactive`: "a user presents to the system numbers" [M86 p. 37] and
answers its guesses [M86 p. 166]; `scripted`: the preset's lines fed on request and the
guesses answered from the script, then judged against the terms fed (DECISIONS D-033,
D-053); `judged`: the bench's knowing user, who judges a guess against the true
continuation (DECISION D-070)), the trace (`verbose`, DECISION D-055) and the `speed`, an
index into `DELAYS`, Numbo's stops. `run_inputs` turns the values into the `RunInputs` the
window starts a `Session` from, validating each field (`InputError`), and `values_of` does
the reverse for the inputs a preset action or a session file gives (G-049). `to_settings`
and `from_settings` are the QSettings round trip under `controls/*`, every value read
checked and a bad one replaced by its default (Numbo GUI, `main_window.py`).
"""

import dataclasses
import random

from seekwhence import bench, sequences
from seekwhence.config import Config
from seekwhence.gui.models import presets
from seekwhence.gui.models.presets import RunInputs
from seekwhence.session import COMMANDS, QUIT

__all__ = ["ANSWERING", "ANSWERING_TEXT", "CUSTOM", "DEFAULT_DELAY", "DELAYS", "GROUP_TITLES",
           "SETTINGS_KEYS", "ControlValues", "InputError", "PresetEntry", "catalogue",
           "delay_text", "entry_of", "parse_cap", "parse_seed", "parse_terms", "preset_lines",
           "random_seed", "run_inputs", "values_of"]

# The speed slider's stops: the delay after each event while playing, in seconds, slowest
# first; 0 is "as fast as redrawing allows" (Numbo GUI, `controls.py`).
DELAYS = (1.0, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0)
DEFAULT_DELAY = 0.01

CUSTOM = "custom"
# The answering modes (PANES.md P11; G-048).
ANSWERING = ("interactive", "scripted", "judged")
ANSWERING_TEXT = {
    "interactive": "Interactive: you type the terms and answer the guesses",
    "scripted": "Scripted: the terms fed on request, guesses answered from the script",
    "judged": "Continuation judge: guesses judged against the true continuation",
}
# The groups of `seekwhence.sequences`, with their citations (DECISION D-070).
GROUP_TITLES = {
    "blackburn": "The Blackburn dozen [M86 pp. 6-7, 145-147]",
    "p146": "Three more the program solves [M86 p. 146]",
    "p88": "The nine sequences solved [M86 p. 88]",
    "p38": "Typical problems of the domain [M86 p. 38]",
    "p150": "Unconquered sequences [M86 p. 150]",
    "fcca": "Hofstadter's sampler [FCCA pp. 53-54]",
}
SETTINGS_KEYS = ("preset", "terms", "seed", "cap_term", "cap_total", "answering", "verbose",
                 "speed")


class InputError(ValueError):
    """A control's text is not a valid input (Numbo GUI, `run_stats.py`)."""


def delay_text(seconds) -> str:
    """`1 s`, `10 ms`, `max` (Numbo GUI, `controls.py`)."""
    if seconds == 0:
        return "max"
    if seconds >= 1:
        return f"{seconds:g} s"
    return f"{seconds * 1000:g} ms"


# ---- the presets (G-048) ------------------------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class PresetEntry:
    """One entry of the preset list (G-048): its `id` (an appendix run id, `custom`, or a
    sequence's name), its `label`, its `group` (`appendix`, `custom`, or a group of
    `seekwhence.sequences`), its `citation` tag, Meredith's `reported` result (`solved`,
    `unsolved`, `not attempted`, or empty) [M86 pp. 145-147], the `parsed` form the source
    prints [M86 p. 7], the `remarks` in brief, the default `seed` and whether the trace is
    `verbose` by default (the printout's mode for an appendix run, DECISION D-055)."""
    id: str
    label: str
    group: str
    citation: str = ""
    reported: str = ""
    parsed: str = ""
    remarks: str = ""
    seed: int = 1
    verbose: bool = True

    @property
    def result_text(self) -> str:
        """`[M86 pp. 6-7, 145]  reported: unsolved`, the line under the preset list
        [M86 pp. 145-147] (G-048)."""
        parts = [self.citation] if self.citation else []
        if self.reported:
            parts.append(f"reported: {self.reported}")
        return "  ".join(parts)

    @property
    def tooltip(self) -> str:
        """The entry's label, citation, parsed form, result and remarks (G-048)."""
        lines = [self.label]
        if self.citation:
            lines.append(self.citation)
        if self.parsed:
            lines.append(f"parsed: {self.parsed}")
        if self.reported:
            lines.append(f"Meredith's result: {self.reported}")
        if self.remarks:
            lines.append(self.remarks)
        return "\n".join(lines)


def _appendix_entries() -> list:
    out = []
    for run_id in presets.appendix_ids():
        inputs = presets.appendix_inputs(run_id)
        out.append(PresetEntry(run_id, inputs.title, "appendix", _tag_of(inputs.title),
                               "solved", "", "Meredith's console inputs, commands and "
                               "answers, on the golden seed", inputs.seed,
                               inputs.config.verbose))
    return out


def _tag_of(title: str) -> str:
    start = title.find("[")
    return title[start:] if start >= 0 else ""


def catalogue() -> tuple:
    """Every preset in the list's order (G-048): the three appendix runs [M86 pp. 164-179],
    `custom`, then the groups of `seekwhence.sequences` in the catalogue's order, each
    sequence with its citation and reported result (DECISION D-070)."""
    out = _appendix_entries()
    out.append(PresetEntry(CUSTOM, "Custom: your own terms", "custom", "[M86 p. 37]"))
    for seq in sequences.CATALOGUE:
        out.append(PresetEntry(seq.name, presets.sequence_title(seq), seq.group, seq.tag,
                               seq.reported, seq.parsed, seq.remarks))
    return tuple(out)


_CATALOGUE = None


def entry_of(preset: str) -> PresetEntry:
    """The entry of a preset id; `KeyError` for none (G-048)."""
    global _CATALOGUE
    if _CATALOGUE is None:
        _CATALOGUE = {e.id: e for e in catalogue()}
    return _CATALOGUE[preset]


def preset_lines(preset: str) -> tuple:
    """The console lines a preset feeds when scripted: an appendix run's own, terms and
    commands [M86 pp. 164-179]; a sequence's first `SEQUENCE_TERMS` terms (DECISION
    D-070); none for `custom` (G-048)."""
    if preset in presets.APPENDIX_SEEDS:
        return tuple(str(x) for x in presets.appendix_inputs(preset).lines)
    if preset == CUSTOM:
        return ()
    return tuple(str(t) for t in sequences.find(preset).prefix(presets.SEQUENCE_TERMS))


# ---- the parsers (Numbo GUI, `run_stats.py`) ------------------------------------------------------------

def parse_seed(text) -> int:
    """A nonnegative integer (Numbo GUI, `run_stats.py`)."""
    word = str(text).strip()
    try:
        seed = int(word)
    except ValueError:
        raise InputError(f"seed: {word!r} is not an integer") from None
    if seed < 0:
        raise InputError(f"seed: must be at least 0, got {seed}")
    return seed


def parse_cap(text, what="cap") -> int:
    """An integer, at least 1 (DECISION D-011)."""
    word = str(text).strip()
    try:
        n = int(word)
    except ValueError:
        raise InputError(f"{what}: {word!r} is not an integer") from None
    if n < 1:
        raise InputError(f"{what}: must be at least 1, got {n}")
    return n


def parse_terms(text) -> tuple:
    """The console lines of the free terms field, one per word: nonnegative integers, the
    show commands and `bye`, what the console takes (DECISION D-054) [M86 p. 164]."""
    words = str(text).replace(",", " ").split()
    for word in words:
        if not (word.isdigit() or word.lower() in COMMANDS or word.lower() == QUIT):
            raise InputError(f"terms: {word!r} is not a term (a nonnegative integer) "
                             f"or a console command")
    return tuple(words)


def random_seed(rng=None) -> int:
    """A seed for the "random seed" button, from Python's `random` (never the engine's
    `Rng`) (G-049)."""
    return (rng or random).randrange(1, 1_000_000_000)


# ---- the values (G-049) -----------------------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class ControlValues:
    """What the controls show, as text (G-049): `preset`, the free `terms`, the `seed`,
    `cap_term` and `cap_total` (DECISION D-011), `answering` (one of `ANSWERING`),
    `verbose` (DECISION D-055) and `speed` (an index into `DELAYS`)."""
    preset: str = presets.DEFAULT_PRESET
    terms: str = ""
    seed: str = str(presets.APPENDIX_SEEDS[presets.DEFAULT_PRESET])
    cap_term: str = str(Config().max_tasks_per_term)
    cap_total: str = str(Config().max_tasks)
    answering: str = "scripted"
    verbose: bool = True
    speed: int = DELAYS.index(DEFAULT_DELAY)

    def to_settings(self) -> dict:
        """The values under `controls/*` keys, strings and integers (Numbo GUI)."""
        return {f"controls/{key}": getattr(self, key) for key in SETTINGS_KEYS}

    @classmethod
    def from_settings(cls, values: dict) -> "ControlValues":
        """The values read back from `controls/*` keys: every value checked, a missing or
        bad one replaced by the default (Numbo GUI, `main_window.py`'s `_restore_settings`;
        G-049)."""
        default = cls()
        got = {}
        preset = values.get("controls/preset")
        try:
            entry_of(preset)
            got["preset"] = preset
        except (KeyError, TypeError):
            preset = default.preset
        if preset != default.preset and "preset" in got:
            got["seed"] = str(entry_of(preset).seed)
            got["verbose"] = entry_of(preset).verbose
        for key in ("terms", "seed", "cap_term", "cap_total"):
            value = values.get(f"controls/{key}")
            if isinstance(value, (str, int)) and not isinstance(value, bool):
                got[key] = str(value)
        answering = values.get("controls/answering")
        if answering in ANSWERING:
            got["answering"] = answering
        verbose = values.get("controls/verbose")
        if isinstance(verbose, bool):
            got["verbose"] = verbose
        elif isinstance(verbose, (str, int)) and str(verbose).lower() in ("true", "false", "0", "1"):
            got["verbose"] = str(verbose).lower() in ("true", "1")
        speed = values.get("controls/speed")
        if isinstance(speed, str) and speed.strip().lstrip("-").isdigit():
            speed = int(speed)
        if isinstance(speed, int) and not isinstance(speed, bool) and 0 <= speed < len(DELAYS):
            got["speed"] = speed
        # a text that does not parse is replaced by its default
        for key, parser, fallback in (("seed", parse_seed, str(entry_of(preset).seed)),
                                      ("cap_term", parse_cap, default.cap_term),
                                      ("cap_total", parse_cap, default.cap_total),
                                      ("terms", parse_terms, "")):
            if key in got:
                try:
                    parser(got[key])
                except InputError:
                    got[key] = fallback
        return dataclasses.replace(default, **got)


def run_inputs(values: ControlValues, config=None) -> RunInputs:
    """The `RunInputs` of the values (G-048, G-049): the configuration is `config`
    (`Config()` by default; item 15's edits) with the trace and the two caps from the
    controls; the lines are the preset's (`preset_lines`) or the free terms; the answers
    and the judge follow the answering mode. `InputError` for a text that does not
    parse."""
    seed = parse_seed(values.seed)
    config = dataclasses.replace(
        Config() if config is None else config, verbose=bool(values.verbose),
        max_tasks_per_term=parse_cap(values.cap_term, "cap per term"),
        max_tasks=parse_cap(values.cap_total, "cap for the run"))
    if values.answering not in ANSWERING:
        raise InputError(f"answering: {values.answering!r} is not one of {ANSWERING}")
    try:
        entry = entry_of(values.preset)
    except KeyError:
        raise InputError(f"no preset {values.preset!r}") from None
    interactive = values.answering == "interactive"
    judged = values.answering == "judged"
    if entry.group == "appendix":
        inputs = presets.appendix_inputs(entry.id, seed)
        lines = inputs.lines
        answers = inputs.answers
        continuation = tuple(x for x in lines if isinstance(x, int))
    elif entry.id == CUSTOM:
        lines = parse_terms(values.terms)
        answers = ()
        continuation = tuple(int(w) for w in lines if w.isdigit())
    else:
        lines = preset_lines(entry.id)
        answers = ()
        continuation = tuple(sequences.find(entry.id).terms)
    if judged and not continuation:
        raise InputError("the continuation judge needs terms to judge against")
    title = entry.label
    if entry.id == CUSTOM:
        title = "custom: " + " ".join(lines) if lines else "custom"
        if interactive:
            title = "interactive: type the terms yourself"
    return RunInputs(preset=entry.id, config=config, seed=seed,
                     lines=None if interactive else tuple(lines),
                     answers=() if interactive or judged else tuple(answers),
                     interactive=interactive, title=title,
                     continuation=continuation if judged else None)


def values_of(inputs: RunInputs, previous: ControlValues | None = None) -> ControlValues:
    """The controls that show `inputs` (G-049): the preset (an interactive run of item
    03, or any preset not in the list, shows as `custom`), the free terms (the lines when
    custom), the seed, the caps and the trace from the configuration, the answering mode
    (`interactive`, `judged` when there is a continuation, else `scripted`); the speed is
    `previous`'s."""
    previous = previous or ControlValues()
    try:
        entry_of(inputs.preset)
        preset = inputs.preset
    except KeyError:
        preset = CUSTOM
    terms = previous.terms
    if preset == CUSTOM:
        terms = " ".join(str(x) for x in inputs.lines) if inputs.lines else ""
    answering = ("interactive" if inputs.interactive or inputs.lines is None
                 else "judged" if inputs.continuation is not None else "scripted")
    return ControlValues(preset=preset, terms=terms, seed=str(inputs.seed),
                         cap_term=str(inputs.config.max_tasks_per_term),
                         cap_total=str(inputs.config.max_tasks), answering=answering,
                         verbose=bool(inputs.config.verbose), speed=previous.speed)
