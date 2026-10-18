"""The controls pane's model, Qt-free (PANES.md P9; Numbo GUI, `controls.py` and
`run_stats.py`'s parsers, through the Seek-Whence GUI's `models/controls.py`; G-044).

A run is its problem, its profile and its seed (D-064, D-066): "Almost all of the
configurations were run fifty times." [F95 p. 114], each run from its own seed. The
controls hold what the user chooses before pressing Play, as plain text, in a
`ControlValues`: the `problem` (one of the 53 digitized configurations, `problems/`,
D-003), the `profile` (one of `config.PROFILES`, D-066), the `seed` as typed, and the
`speed`, an index into `DELAYS`, Numbo's stops. The cap is shown read-only
(`cap_text`): `max_codelets` of the next run's `Config` (D-061), editable in the
configuration pane (ANSWERS.md 3). `catalogue()` lists the problems as the selector
shows them: the chapter 4 configuration first (`DEFAULT_PROBLEM`, `surround-05` [F95
p. 118]), then every family in the library's order [F95 p. 114], each problem with its
title and page. `run_inputs` validates the values (`InputError`) into the (problem name,
profile name, seed) triple the window starts a run from, and `values_of` does the
reverse for a run started any other way (the Problem menu, the command line, a replayed
session), so that the selector always shows the run actually started (PANES.md P9;
loop0004 ANSWERS 10.3). `to_settings` and `from_settings` are the QSettings round trip
under `controls/*`, every value read checked and a bad one replaced by its default
(Numbo GUI, `main_window.py`'s `_restore_settings`).
"""

import dataclasses

from tabletop.config import PROFILES
from tabletop.problems import FAMILIES, by_family, load_problem

__all__ = ["DEFAULT_DELAY", "DEFAULT_PROBLEM", "DEFAULT_PROFILE", "DEFAULT_SEED", "DELAYS",
           "FAMILY_TITLES", "SETTINGS_KEYS", "ControlValues", "InputError", "ProblemEntry",
           "cap_text", "catalogue", "delay_text", "entry_of", "parse_seed", "run_inputs",
           "values_of"]

# The speed slider's stops: the delay after each event while playing, in seconds, slowest
# first; 0 is "as fast as redrawing allows" (Numbo GUI, `controls.py`; ANSWERS.md 5).
DELAYS = (1.0, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0)
DEFAULT_DELAY = 0.01

# The chapter 4 configuration is the default problem and the first of the selector
# (PANES.md P9; [F95 p. 118]); `standard` and seed 1 are the CLI's defaults.
DEFAULT_PROBLEM = "surround-05"
DEFAULT_PROFILE = "standard"
DEFAULT_SEED = 1

# The families as the selector heads them [F95 p. 114]; chapter 2's figures are the
# examples of the domain [F95 pp. 23-45].
FAMILY_TITLES = {
    "surround": "Surround (ch. 5)",
    "blockage": "Blockage (ch. 5)",
    "buridan": "Buridan (ch. 5)",
    "figure-ground": "Figure-ground (ch. 5)",
    "ch2": "The examples of chapter 2",
}
SETTINGS_KEYS = ("problem", "profile", "seed", "speed")


class InputError(ValueError):
    """A control's text is not a valid input (Numbo GUI, `run_stats.py`)."""


def delay_text(seconds) -> str:
    """`1 s`, `10 ms`, `max` (Numbo GUI, `controls.py`)."""
    if seconds == 0:
        return "max"
    if seconds >= 1:
        return f"{seconds:g} s"
    return f"{seconds * 1000:g} ms"


# ---- the problems (G-044) ------------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class ProblemEntry:
    """One entry of the problem selector (G-044): the problem's `name`, `title`,
    `family`, the `page` of its figure (None when the file gives none) and `tag`, its
    source's tag (`F95`); `label` is the selector's text, `tooltip` the hover text."""
    name: str
    title: str
    family: str
    page: int | None = None
    tag: str = "F95"

    @property
    def label(self) -> str:
        """`Surround 5 (surround-05, p. 118)` (G-044)."""
        page = f", p. {self.page}" if self.page else ""
        return f"{self.title} ({self.name}{page})"

    @property
    def citation(self) -> str:
        """`[F95 p. 118]`, or empty when the file gives no page (G-044)."""
        return f"[{self.tag} p. {self.page}]" if self.page else ""

    @property
    def tooltip(self) -> str:
        """The title, the name, the family and the figure's page (G-044)."""
        lines = [f"{self.title}: {self.name}, family {self.family}"]
        if self.citation:
            lines.append(f'the figure is on page {self.page} {self.citation}')
        if self.name == DEFAULT_PROBLEM:
            lines.append('the configuration of the chapter 4 run [F95 pp. 95-107]')
        return "\n".join(lines)


def _entry(name: str) -> ProblemEntry:
    problem = load_problem(name)
    source = problem.source if isinstance(problem.source, dict) else {}
    page = source.get("page")
    return ProblemEntry(name, problem.title, problem.family,
                        int(page) if isinstance(page, int) else None,
                        str(source.get("tag") or "F95"))


_CATALOGUE = None


def catalogue() -> tuple:
    """Every problem of the library as the selector lists it (G-044): the chapter 4
    configuration first [F95 p. 118], then each family in `problems.FAMILIES`' order
    [F95 p. 114], each problem in the library's order, with its title and page; loaded
    once."""
    global _CATALOGUE
    if _CATALOGUE is None:
        names = [DEFAULT_PROBLEM]
        for family in FAMILIES:
            names += [name for name in by_family(family) if name != DEFAULT_PROBLEM]
        _CATALOGUE = tuple(_entry(name) for name in names)
    return _CATALOGUE


def entry_of(name: str) -> ProblemEntry:
    """The entry of a problem name; `KeyError` for none (G-044)."""
    for entry in catalogue():
        if entry.name == name:
            return entry
    raise KeyError(name)


# ---- the parsers (Numbo GUI, `run_stats.py`) ----------------------------------------------------

def parse_seed(text) -> int:
    """An integer, what the CLI's `--seed` takes (Numbo GUI, `run_stats.py`)."""
    word = str(text).strip()
    try:
        return int(word)
    except ValueError:
        raise InputError(f"seed: {word!r} is not an integer") from None


def cap_text(config) -> str:
    """The read-only cap line of the controls (PANES.md P9): `max_codelets` of the next
    run's `Config` (D-061) and where to edit it (ANSWERS.md 3)."""
    return f"{config.max_codelets} codelets (max_codelets; edit in Edit > Configuration)"


# ---- the values (G-044) -------------------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class ControlValues:
    """What the controls show, as text (G-044): the `problem` name, the `profile` name,
    the `seed` as typed and the `speed`, an index into `DELAYS`."""
    problem: str = DEFAULT_PROBLEM
    profile: str = DEFAULT_PROFILE
    seed: str = str(DEFAULT_SEED)
    speed: int = DELAYS.index(DEFAULT_DELAY)

    def to_settings(self) -> dict:
        """The values under `controls/*` keys (Numbo GUI, `main_window.py`): the problem
        and the profile as strings, the seed as an integer when it parses (else the
        text), the speed as its index."""
        seed = self.seed
        try:
            seed = parse_seed(self.seed)
        except InputError:
            pass
        return {"controls/problem": self.problem, "controls/profile": self.profile,
                "controls/seed": seed, "controls/speed": self.speed}

    @classmethod
    def from_settings(cls, values: dict) -> "ControlValues":
        """The values read back from `controls/*` keys: every value checked, a missing or
        bad one replaced by its default (Numbo GUI, `main_window.py`'s
        `_restore_settings`; G-044)."""
        got = {}
        problem = values.get("controls/problem")
        if isinstance(problem, str):
            try:
                entry_of(problem)
                got["problem"] = problem
            except KeyError:
                pass
        profile = values.get("controls/profile")
        if isinstance(profile, str) and profile in PROFILES:
            got["profile"] = profile
        seed = values.get("controls/seed")
        if isinstance(seed, (str, int)) and not isinstance(seed, bool):
            try:
                got["seed"] = str(parse_seed(seed))
            except InputError:
                pass
        speed = values.get("controls/speed")
        if isinstance(speed, str) and speed.strip().lstrip("-").isdigit():
            speed = int(speed)
        if isinstance(speed, int) and not isinstance(speed, bool) and 0 <= speed < len(DELAYS):
            got["speed"] = speed
        return dataclasses.replace(cls(), **got)


def run_inputs(values: ControlValues) -> tuple:
    """The (problem name, profile name, seed) of the values, each checked: a problem of
    the library, a profile of `PROFILES`, an integer seed; `InputError` otherwise
    (G-044; Numbo GUI's `inputs`)."""
    try:
        entry_of(values.problem)
    except KeyError:
        raise InputError(f"no problem named {values.problem!r}") from None
    if values.profile not in PROFILES:
        raise InputError(f"no profile named {values.profile!r}")
    return values.problem, values.profile, parse_seed(values.seed)


def values_of(problem: str, profile: str, seed, previous: ControlValues | None = None) \
        -> ControlValues:
    """The controls that show a run started any other way (G-044): the problem (a name
    not in the library, an edited table of item 14, shows as the previous problem), the
    profile (a configuration that is no profile shows as the previous profile, the stats
    pane saying what changed), the seed; the speed is `previous`'s."""
    previous = previous or ControlValues()
    try:
        entry_of(problem)
    except KeyError:
        problem = previous.problem
    if profile not in PROFILES:
        profile = previous.profile
    return ControlValues(problem=problem, profile=profile, seed=str(int(seed)),
                         speed=previous.speed)
