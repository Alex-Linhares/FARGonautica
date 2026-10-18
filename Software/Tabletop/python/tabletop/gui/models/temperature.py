"""The temperature panes' model (P2 Thermometer, P2b Temperature plot; G-027 to G-029),
Qt-free: the readings of `TemperatureUpdated`, the marks of `CompetitionHeld` and
`WorldviewChanged`, the three terms of T = alpha dQ/Q + beta dWV - gamma Tcp [F95 p. 91
(img)], French's chapter 4 thermometer [F95 pp. 98-107 (img)], the thermometer's form
and the plot's geometry.

Everything comes from the events (D-065): a `RunStarted` begins a run and gives the
run's `Config` back from its `profile` and `changed` (`config_of_start`), every
`TemperatureUpdated` is a `Reading`, every competition and Worldview change a `Mark`.
The model is exact at every event index, so seeking, which republishes the history,
rebuilds it exactly (G-027). The thermometer shows the last reading and nothing newer:
no update follows the find-answer codelet that ends the run, so the value at the answer
is up to four codelets old (D-050), and the pane says how old. Before the first reading
the column is empty, as French's screen drew it at 0 codelets [F95 p. 96 (img)]
(D-049). The plot is the GUI's design, not French's (ANSWERS.md 4, 11): its geometry,
`plot_layout`, is a pure function of the model, the size and the options, so the view
only draws polylines and the tests check the picture without Qt (G-028). French's
chapter 4 readings are a constant here, as the engine keeps French's numbers in
`tabletop/reported.py`, and the tests check them against `tests/data/ch4.json` (G-029).
"""

import ast
import dataclasses
from dataclasses import dataclass

from tabletop import events as ev
from tabletop.config import PROFILES
from tabletop.temperature import clamp

__all__ = ["CH4_PROBLEM", "FRENCH_CH4", "NOT_FRENCHS", "REASONS", "SERIES", "Area",
           "Band", "Mark", "MarkLine", "PlotLayout", "Point", "Polyline", "Reading",
           "TemperatureModel", "Terms", "Thermometer", "config_of_start", "plot_layout",
           "reading_at", "reading_text", "series_of", "terms_of", "thermometer_of"]

NOT_FRENCHS = "GUI design, not French's"
# Who asked for the update (D-050; `TemperatureUpdated.by`).
REASONS = ("start", "schedule", "competition", "change")
# The problem French's chapter 4 run is (`tests/data/ch4.json`, "same_as") [F95 p. 118].
CH4_PROBLEM = "surround-05"
# French's thermometer at the chapter 4 dumps, (figure, codelets run, T) [F95 pp. 98-107
# (img)]: Figs. 4.1 and 4.3 drew the column empty (0 and 12 codelets), and Figs. 4.7 and
# 4.8 are both at 47 codelets. Checked against `tests/data/ch4.json` (G-029).
FRENCH_CH4 = (("4.2", 2, 80.0), ("4.4", 22, 59.0), ("4.5", 33, 43.2), ("4.6", 36, 31.3),
              ("4.7", 47, 35.8), ("4.8", 47, 13.6), ("4.9", 55, 2.0))
# The plot's five series: name, label, the band they are drawn in (G-028).
SERIES = (("temperature", "T", "temperature"), ("q", "Q", "fraction"),
          ("dq", "ΔQ", "fraction"), ("dwv", "Δ_WV", "count"), ("tcp", "T_cp", "count"))


@dataclass(frozen=True)
class Reading:
    """One `TemperatureUpdated` (D-050): the event's `index` in the run, the `clock`, T
    and the formula's inputs [F95 p. 91 (img)], the exponent `a`, `held` (D-049) and
    `by`."""
    index: int
    clock: int
    temperature: float
    q: float
    dq: float
    dwv: int
    tcp: int
    a: float
    held: bool
    by: str


@dataclass(frozen=True)
class Mark:
    """A competition (`CompetitionHeld`) or a Worldview change (`WorldviewChanged`) to
    mark on the plot (G-028): the event's `index`, the `clock`, the `kind` and a short
    `detail` (the newcomer, or the names added and removed)."""
    index: int
    clock: int
    kind: str
    detail: str


@dataclass(frozen=True)
class Terms:
    """The three terms of T = alpha dQ/Q + beta dWV - gamma Tcp at one reading [F95
    p. 91 (img)] (D-047): `dq_over_q`, `changes`, `competitions` (each as the term's
    size, the last one subtracted), `raw` their signed sum before the clamp, and
    `applied`, whether T was computed from them (False while held, D-049, and under
    `temperature_fixed`, D-047)."""
    dq_over_q: float
    changes: float
    competitions: float
    raw: float
    applied: bool


@dataclass(frozen=True)
class Thermometer:
    """What the thermometer draws (G-027): the `value` (None before the first reading:
    the column empty, as at 0 codelets [F95 p. 96 (img)]), `fill` on 0-1, the `text`
    printed beside the fill's top as French's screen prints it ("80.0" [F95 p. 98
    (img)]), `held` (the grey band, D-049), `by` and `clock` of the reading, `age` in
    codelets since it (D-050) and a `note` in words."""
    value: object
    fill: float
    text: str
    held: bool
    by: str
    clock: int
    age: int
    note: str


def config_of_start(start):
    """The run's `Config` from a `RunStarted`: the profile's, with the `changed` fields
    put back (their values are reprs, D-065; `profile` itself is skipped) (G-027)."""
    config = PROFILES[start.profile]
    edits = {}
    for field, value in start.changed:
        if field != "profile":
            edits[field] = ast.literal_eval(value)
    return dataclasses.replace(config, **edits) if edits else config


def terms_of(reading, config) -> Terms:
    """The three terms at a reading, from its inputs and the run's coefficients [F95
    p. 91 (img)] (D-047 to D-049): Q floored at `q_floor` as the engine floors it."""
    q = max(config.q_floor, reading.q)
    first = config.temperature_alpha * reading.dq / q
    second = config.temperature_beta * reading.dwv
    third = config.temperature_gamma * reading.tcp
    applied = not reading.held and config.temperature_fixed < 0
    return Terms(first, second, third, first + second - third, applied)


class TemperatureModel:
    """The temperature of a run as the events tell it (G-027): `readings`, `marks`, the
    `config` of the run, the `problem`, the `clock` of the last event, `ended` (the
    `RunEnded`'s T) and `events`, how many events of the run were seen; `on_event` feeds
    it, `last` is the last reading, `thermometer()` the thermometer's form."""

    def __init__(self):
        self.reset()

    def reset(self) -> None:
        """Forget the run (a `RunStarted` begins a new one; D-065)."""
        self.problem = None
        self.config = None
        self.readings = []
        self.marks = []
        self.clock = 0
        self.ended = None
        self.events = 0

    @property
    def last(self):
        """The last reading, or None before the first (G-027)."""
        return self.readings[-1] if self.readings else None

    @property
    def french_offered(self) -> bool:
        """Whether French's chapter 4 thermometer applies: the run is `surround-05`
        (PANES.md P2b; G-029)."""
        return self.problem == CH4_PROBLEM

    def on_event(self, event) -> None:
        """Take one event: a reading, a mark, the start or the end (D-065; G-027)."""
        if isinstance(event, ev.RunStarted):
            self.reset()
            self.problem = event.problem
            self.config = config_of_start(event)
        index = self.events
        self.events += 1
        self.clock = event.clock
        if isinstance(event, ev.TemperatureUpdated):
            self.readings.append(Reading(index, event.clock, event.temperature, event.q,
                                         event.dq, event.dwv, event.tcp, event.a,
                                         event.held, event.by))
        elif isinstance(event, ev.CompetitionHeld):
            self.marks.append(Mark(index, event.clock, "competition", event.newcomer))
        elif isinstance(event, ev.WorldviewChanged):
            detail = f"{event.reason}: +{', '.join(event.added)} -{', '.join(event.removed)}"
            self.marks.append(Mark(index, event.clock, "change", detail))
        elif isinstance(event, ev.RunEnded):
            self.ended = event.temperature

    def terms(self, reading=None) -> Terms:
        """The three terms at `reading` (the last by default) (G-028)."""
        reading = self.last if reading is None else reading
        return terms_of(reading, self.config)

    def thermometer(self) -> Thermometer:
        """The thermometer's form at the current event (G-027)."""
        return thermometer_of(self)

    def french_points(self) -> tuple:
        """French's chapter 4 readings when the run is `surround-05`, else nothing
        (G-029)."""
        return FRENCH_CH4 if self.french_offered else ()


def thermometer_of(model) -> Thermometer:
    """The thermometer's form (G-027): the last reading, its value printed to one decimal
    as French's screen prints it [F95 p. 98 (img)], grey while held (D-049), and how
    many codelets old it is (D-050)."""
    reading = model.last
    if reading is None:
        note = "no reading yet" if model.problem else "no run"
        return Thermometer(None, 0.0, "", False, "", 0, 0, note)
    age = max(0, model.clock - reading.clock)
    note = f"{reading.temperature:.1f} by {reading.by} at codelet {reading.clock}"
    if reading.held:
        note += f"; held at the initial {reading.temperature:g} until the first change"
    if age:
        note += f"; {age} codelet{'s' if age != 1 else ''} old"
    if model.ended is not None:
        note += "; the run has ended: no update follows the answer"
    return Thermometer(reading.temperature, clamp(reading.temperature) / 100.0,
                       f"{reading.temperature:.1f}", reading.held, reading.by,
                       reading.clock, age, note)


def series_of(model) -> dict:
    """The five series as (clock, value) points, one per reading, keyed by `SERIES`
    name (G-028)."""
    return {name: tuple((r.clock, getattr(r, name)) for r in model.readings)
            for name, _label, _band in SERIES}


# ---------------------------------------------------------------------------------------------
# The plot's geometry (G-028): a pure function of the model, the size and the options.

# Margins in pixels: the note above, the x labels below, the y labels at the left.
TOP, BOTTOM, LEFT, RIGHT, GAP = 20, 18, 36, 10, 12
# The bands' shares of the height: T gets half, the fractions and the counts a quarter.
BAND_SHARES = (("temperature", 0.5), ("fraction", 0.25), ("count", 0.25))
BAND_LABELS = {"temperature": "T", "fraction": "Q, ΔQ", "count": "Δ_WV, T_cp"}
AREA_LABELS = (("dq_over_q", "α ΔQ/Q"), ("changes", "β Δ_WV"), ("competitions", "− γ T_cp"))


@dataclass(frozen=True)
class Polyline:
    """A series drawn as a line: its name, label and (x, y) pixel points (G-028)."""
    name: str
    label: str
    points: tuple


@dataclass(frozen=True)
class Area:
    """A stacked area of one term (G-028): `upper` and `lower` pixel polylines, one
    point per reading; `negative` for the subtracted term."""
    name: str
    label: str
    upper: tuple
    lower: tuple
    negative: bool


@dataclass(frozen=True)
class Band:
    """A horizontal strip of the plot sharing the x axis (G-028): its `kind`, its label,
    pixel `top` and `bottom`, value range `low` to `high`, the `lines` in it and the
    y `ticks` as (y, label)."""
    kind: str
    label: str
    top: float
    bottom: float
    low: float
    high: float
    lines: tuple
    ticks: tuple


@dataclass(frozen=True)
class MarkLine:
    """A vertical mark across the bands (G-028): `x`, `kind` (competition or change)
    and the mark's `detail`."""
    x: float
    kind: str
    detail: str


@dataclass(frozen=True)
class Point:
    """One of French's labelled readings on the T band (G-029): `x`, `y`, the label
    "Fig. 4.4: 59.0" and the codelets."""
    x: float
    y: float
    label: str
    clock: int


@dataclass(frozen=True)
class PlotLayout:
    """The plot's geometry (G-028): the pixel `width` and `height`, the plot's `left`
    and `right`, `x_max` (the clock at the right edge), the `bands`, the `marks`, the
    stacked `areas` (empty unless asked), French's `french` points (empty unless asked
    and offered), the x `ticks` as (x, label), the `note` and `empty` before any
    reading."""
    width: int
    height: int
    left: float
    right: float
    x_max: int
    bands: tuple
    marks: tuple
    areas: tuple
    french: tuple
    ticks: tuple
    note: str
    empty: bool

    def x_of(self, clock) -> float:
        """The pixel x of a clock (G-028)."""
        return self.left + (self.right - self.left) * clock / self.x_max

    def band(self, kind):
        """The band of a kind (G-028)."""
        return next(b for b in self.bands if b.kind == kind)


def _y_of(band, value) -> float:
    """The pixel y of a value in a band, clipped to the band."""
    share = (value - band.low) / (band.high - band.low)
    return band.bottom - (band.bottom - band.top) * min(1.0, max(0.0, share))


def _band_ranges(model) -> dict:
    """The value range of each band kind: T 0-100, fractions 0-1, counts 0 to the
    largest count seen, at least 2 (G-028)."""
    counts = [r.dwv for r in model.readings] + [r.tcp for r in model.readings]
    return {"temperature": (0.0, 100.0), "fraction": (0.0, 1.0),
            "count": (0.0, float(max([2] + counts)))}


def _band_rows(height) -> dict:
    """The pixel top and bottom of each band kind for a height (G-028)."""
    room = max(10.0, height - TOP - BOTTOM - GAP * (len(BAND_SHARES) - 1))
    rows, y = {}, float(TOP)
    for kind, share in BAND_SHARES:
        rows[kind] = (y, y + room * share)
        y += room * share + GAP
    return rows


def _x_max(model, french) -> int:
    """The clock at the right edge: the last event's clock, French's last dump when his
    points are shown, at least 10 (G-028)."""
    far = max(10, model.clock, *(r.clock for r in model.readings))
    if french and model.french_offered:
        far = max(far, max(c for _f, c, _t in FRENCH_CH4))
    return far


def _x_ticks(x_max, x_of) -> tuple:
    """Clock ticks every 5, 10, 20, 50 or 100 codelets, about six across (G-028)."""
    step = next((s for s in (5, 10, 20, 50, 100, 200, 500) if x_max / s <= 8), 1000)
    return tuple((x_of(c), str(c)) for c in range(0, x_max + 1, step))


def _bands(model, height, x_of) -> tuple:
    """The three bands with their lines (G-028)."""
    ranges, rows = _band_ranges(model), _band_rows(height)
    points = series_of(model)
    bands = []
    for kind, _share in BAND_SHARES:
        low, high = ranges[kind]
        top, bottom = rows[kind]
        band = Band(kind, BAND_LABELS[kind], top, bottom, low, high, (), ())
        lines = tuple(Polyline(name, label, tuple((x_of(c), _y_of(band, v))
                                                  for c, v in points[name]))
                      for name, label, where in SERIES if where == kind)
        ticks = tuple((_y_of(band, v), f"{v:g}") for v in (low, high))
        bands.append(dataclasses.replace(band, lines=lines, ticks=ticks))
    return tuple(bands)


def _areas(model, band, x_of) -> tuple:
    """The three terms stacked on the T band (G-028): the two positive terms from 0 up,
    the subtracted one from the top of the stack down; their net is the T line while
    the formula applies and T is inside 0-100."""
    bottoms, firsts, seconds, thirds = [], [], [], []
    for r in model.readings:
        t, x = terms_of(r, model.config), x_of(r.clock)
        bottoms.append((x, _y_of(band, 0.0)))
        firsts.append((x, _y_of(band, t.dq_over_q)))
        seconds.append((x, _y_of(band, t.dq_over_q + t.changes)))
        thirds.append((x, _y_of(band, t.dq_over_q + t.changes - t.competitions)))
    names = AREA_LABELS
    return (Area(names[0][0], names[0][1], tuple(firsts), tuple(bottoms), False),
            Area(names[1][0], names[1][1], tuple(seconds), tuple(firsts), False),
            Area(names[2][0], names[2][1], tuple(seconds), tuple(thirds), True))


def _french(model, band, x_of) -> tuple:
    """French's labelled points on the T band (G-029)."""
    return tuple(Point(x_of(c), _y_of(band, t), f"Fig. {f}: {t:g}", c)
                 for f, c, t in model.french_points())


def _note(model, terms, french) -> str:
    """The plot's caption: the design's status, the options on, the state (G-028)."""
    parts = [NOT_FRENCHS]
    if terms:
        parts.append("terms stacked")
    if french:
        parts.append("French's ch. 4 thermometer" if model.french_offered
                     else f"French's ch. 4 points apply to {CH4_PROBLEM} only")
    if model.problem is None:
        parts.append("no run")
    elif not model.readings:
        parts.append("no reading yet")
    else:
        parts.append(f"{len(model.readings)} readings to codelet {model.last.clock}")
    return " · ".join(parts)


def plot_layout(model, width, height, terms=False, french=False) -> PlotLayout:
    """The plot's geometry for a size and the options (G-028): the T band with the T
    line, the stacked terms when `terms`, French's points when `french` and the run is
    `surround-05` (G-029); the fraction band with Q and ΔQ; the count band with Δ_WV
    and T_cp; the competitions and changes as vertical marks."""
    x_max = _x_max(model, french)
    left, right = float(LEFT), float(max(LEFT + 10, width - RIGHT))

    def x_of(clock):
        return left + (right - left) * clock / x_max

    bands = _bands(model, height, x_of)
    t_band = next(b for b in bands if b.kind == "temperature")
    marks = tuple(MarkLine(x_of(m.clock), m.kind, m.detail) for m in model.marks)
    return PlotLayout(width, height, left, right, x_max, bands, marks,
                      _areas(model, t_band, x_of) if terms and model.readings else (),
                      _french(model, t_band, x_of) if french else (),
                      _x_ticks(x_max, x_of), _note(model, terms, french),
                      not model.readings)


def reading_at(model, layout, x):
    """The reading nearest to a pixel x, for the hover text; None without readings
    (G-028)."""
    if not model.readings:
        return None
    return min(model.readings, key=lambda r: abs(layout.x_of(r.clock) - x))


def reading_text(reading, terms) -> str:
    """The hover text of a reading (G-028): T, the inputs, the exponent, the terms and
    the reason."""
    lines = [f"codelet {reading.clock}: T {reading.temperature:.1f} by {reading.by}"
             + (" (held, D-049)" if reading.held else ""),
             f"Q {reading.q:.3f}  ΔQ {reading.dq:.4f}  Δ_WV {reading.dwv}  T_cp {reading.tcp}"
             f"  a {reading.a:.2f}",
             f"α ΔQ/Q {terms.dq_over_q:.1f}  + β Δ_WV {terms.changes:.1f}"
             f"  − γ T_cp {terms.competitions:.1f}  = {terms.raw:.1f}"
             + ("" if terms.applied else " (not applied)")]
    return "\n".join(lines)
