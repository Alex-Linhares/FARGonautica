"""Regenerate the pictures of the front page (`../README.md`) in python/docs/, offscreen
and deterministically.

    cd python
    python3 -m scripts.docs_figures [--out DIR] [--only NAME ...]

This is a documentation tool, not part of the package: it imports `tabletop` (standard
library only) and draws with PySide6, which only this script needs
(`pip install PySide6`; tested with 6.11). Without PySide6 it says so and exits 1. It
renders with `QT_QPA_PLATFORM=offscreen`, so no window opens.

The pictures, all drawn from data in this folder, never from French's figures:

The results figures have three columns, as RESULTS.md's tables do since the roles (D-078):
French's program, this reconstruction before roles (v1, the records of `results/v1/`,
commit `e38d6e0`) and with them on the defaults of the day (the records of `results/`:
since 2026-10-17 v4, the second optimization's defaults of D-115, labelled "v4" (v3, the
optimized defaults of D-114, from 2026-10-16); the code calls them
the roles engine), both at 500 runs per problem under the `standard` profile.

- table-surround-05.png: Surround 5 (the chapter 4 configuration [F95 p. 118]) drawn from
  `problems/surround-05.json`, with the object Henry touches, French's answer labels and
  the answer of the run `python3 -m tabletop surround-05 --seed 3`; his frequencies, v1's
  and the roles'.
- tables-gallery.png: Surround 1, Blockage 6 and Buridan 1 drawn from their files, each
  with French's frequencies [F95 pp. 115, 126, 129], v1's (`results/v1/families.json`) and
  the roles' (`results/families.json`).
- results-main-answer.png: French's main answer in each of the 35 result sets of chapter
  5, his frequency beside v1's and the roles' (the table of RESULTS.md section 2.1 with
  its v1 column, and section 11.1).
- results-sets.png: every answer of four result sets, French's bars beside v1's and the
  roles'.
- temperature.png: the temperature against the codelets run on Surround 5 (the roles'
  mean and quartiles over 500 runs, `results/temperature.json`, and v1's mean,
  `results/v1/temperature.json`, RESULTS.md section 6.4), French's thermometer in the
  chapter 4 dumps (`tests/data/ch4.json`) [F95 pp. 96-107 (img)] and the one run of seed 3.
- terminal.png: the command line's real output for
  `python3 -m tabletop surround-05 --seed 3 --table`, drawn as a terminal, its middle
  elided.

The object icons are new vector drawings, the GUI's (`tabletop/gui/icons.py` paints the
shapes of `tabletop/gui/models/icon_shapes.py`; French's own icons are never copied); each
object is drawn in its digitized footprint box (D-002), at its digitized position (D-001,
D-003). Each picture must stay under 200 KB.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

try:
    from PySide6.QtCore import QPointF, QRectF, Qt
    from PySide6.QtGui import (QBrush, QColor, QFont, QFontMetricsF, QGuiApplication, QImage,
                               QPainter, QPainterPath, QPen, QPolygonF)
    from tabletop.gui.icons import paint_icon   # the GUI's icons, shared (ANSWERS.md 4)
except ImportError:                                  # the package itself never needs it
    QGuiApplication = None

PYTHON_DIR = Path(__file__).resolve().parents[1]
DOCS = PYTHON_DIR / "docs"
RESULTS = PYTHON_DIR / "results"
LIMIT = 200 * 1024

# The colours: a light surface, text in ink tones, and three series hues (French, v1, roles).
SURFACE = "#fcfcfb"
INK = "#0b0b0b"
INK2 = "#52514e"
MUTED = "#8a8984"
GRID = "#e4e2dc"
FRENCH = "#eb6834"          # French's program
V1 = "#a39f96"              # this reconstruction before roles (v1)
ROLES = "#2a78d6"           # with the roles (D-078)
TABLE_FILL = "#f4efe4"
TABLE_EDGE = "#a89f8c"
SANS = "DejaVu Sans"
MONO = "DejaVu Sans Mono"


# ---- drawing helpers ----------------------------------------------------------------------

def colour(hex_: str, alpha: int = 255) -> QColor:
    c = QColor(hex_)
    c.setAlpha(alpha)
    return c


def font(size: float, bold: bool = False, family: str = SANS) -> QFont:
    f = QFont(family)
    f.setPixelSize(round(size))
    f.setBold(bold)
    f.setHintingPreference(QFont.PreferNoHinting)
    return f


def new_image(width: int, height: int, background: str = SURFACE):
    img = QImage(width, height, QImage.Format_RGB32)
    img.fill(QColor(background))
    p = QPainter(img)
    p.setRenderHint(QPainter.Antialiasing)
    p.setRenderHint(QPainter.TextAntialiasing)
    return img, p


def text(p, x, y, s, size=13, bold=False, ink=INK, align="left", family=SANS):
    """Draw `s` with its baseline at y; x is the left, centre or right per `align`."""
    f = font(size, bold, family)
    p.setFont(f)
    p.setPen(QColor(ink))
    w = QFontMetricsF(f).horizontalAdvance(s)
    if align == "center":
        x -= w / 2
    elif align == "right":
        x -= w
    p.drawText(QPointF(x, y), s)
    return w


def save(img, painter, path: Path) -> int:
    painter.end()
    path.parent.mkdir(parents=True, exist_ok=True)
    if not img.save(str(path), "PNG", 9):
        raise SystemExit(f"could not write {path}")
    size = path.stat().st_size
    flag = "" if size <= LIMIT else "  OVER 200 KB"
    print(f"{path.name:26} {img.width()} x {img.height()}  {size / 1024:6.1f} KB{flag}")
    return size


# ---- the object icons (new design) ---------------------------------------------------------
# The icons are the GUI's: `tabletop/gui/models/icon_shapes.py` describes each type as
# primitives in its footprint box and `tabletop/gui/icons.py` paints them, so that the
# README's pictures and the GUI share one set of new drawings, never French's bitmaps
# (ANSWERS.md 4). `paint_icon` draws one icon centred on the painter's origin, turned to its
# orientation.


def draw_table(p, problem, x0: float, y0: float, scale: float, labels: dict,
               rings: dict, names: bool = True):
    """The table of `problem` with its upper left at (x0, y0), `scale` px per unit; Henry's
    name above, Eliza's below; `labels` id -> text written beside the object, `rings`
    id -> colour of a ring drawn around it."""
    from tabletop.objects import footprint
    tw, th = problem.width * scale, problem.height * scale
    if names:
        text(p, x0 + tw / 2, y0 - 9, "Henry", 15, True, INK2, "center")
        text(p, x0 + tw / 2, y0 + th + 21, "Eliza", 15, True, INK2, "center")
    p.setPen(QPen(QColor(TABLE_EDGE), 2))
    p.setBrush(QColor(TABLE_FILL))
    p.drawRoundedRect(QRectF(x0, y0, tw, th), 6, 6)
    dash = QPen(colour(TABLE_EDGE, 140), 1, Qt.DashLine)
    p.setPen(dash)
    p.drawLine(QPointF(x0 + 6, y0 + th / 2), QPointF(x0 + tw - 6, y0 + th / 2))
    placed = []                                      # label boxes already drawn
    for o in problem.objects:
        cx, cy = x0 + o.x * scale, y0 + o.y * scale
        w, h = footprint(o.type)                     # the box pointing up (D-002)
        rw, rh = footprint(o.type, o.orientation)    # as it lies on the table
        if o.id in rings:
            pad = 1.6 * scale
            p.setPen(QPen(QColor(rings[o.id]), 3))
            p.setBrush(colour(rings[o.id], 40))
            p.drawRoundedRect(QRectF(cx - rw * scale / 2 - pad, cy - rh * scale / 2 - pad,
                                     rw * scale + 2 * pad, rh * scale + 2 * pad), 5, 5)
        p.save()
        p.translate(cx, cy)
        paint_icon(p, o.type, w * scale, h * scale, o.orientation)
        p.restore()
        if o.id in labels:
            above = o.y < problem.height / 2
            ly = cy - rh * scale / 2 - 1.6 * scale - 6 if above else cy + rh * scale / 2 + 1.6 * scale + 17
            lw = QFontMetricsF(font(14, True)).horizontalAdvance(labels[o.id]) + 6
            while any(abs(cx - px) < (lw + pw) / 2 and abs(ly - py) < 16
                      for px, py, pw in placed):
                ly += -16 if above else 16
            placed.append((cx, ly, lw))
            text(p, cx, ly, labels[o.id], 14, True, INK, "center")


# ---- the data -------------------------------------------------------------------------------

def families(engine: str = "roles"):
    """The `families` records of the roles engine (`results/`) or of v1
    (`results/v1/`, `Results.v1`, D-078)."""
    from tabletop.bench.results import Results
    res = Results(RESULTS)
    return (res if engine == "roles" else res.v1)["families"]


def family_record(problem: str, program: str = "original", engine: str = "roles") -> dict:
    for r in families(engine):
        if r["problem"] == problem and r["program"] == program:
            return r
    raise KeyError(problem)


def three_columns(problem: str, program: str = "original") -> list:
    """(row name label, French's frequency, v1's, the roles') for every row of a result
    set, in the roles record's order (D-078)."""
    from tabletop.bench.results import row_named
    v1 = family_record(problem, program, "v1")
    return [(r["label"], r["french"]["frequency"], row_named(v1, r["label"])["ours"]["frequency"],
             r["ours"]["frequency"]) for r in family_record(problem, program)["rows"]]


def one_run(name: str, seed: int):
    """The run of `python3 -m tabletop NAME --seed SEED`: its result and its events."""
    from tabletop.config import PROFILES
    from tabletop.events import Collector
    from tabletop.problems import load_problem
    from tabletop.run import run_problem
    log = Collector()
    result = run_problem(load_problem(name), PROFILES["standard"], seed=seed, observers=[log])
    return result, log.events


def answer_labels(problem) -> dict:
    """id -> "H", "E1", "E1 = H" ... as French's figures label them [F95 p. 114]."""
    labels = {}
    for label, oid in sorted(problem.answers.items()):
        labels[oid] = f"{labels[oid]}, {label}" if oid in labels else label
    t = problem.touched
    labels[t] = f"H = {labels[t]}" if t in labels else "H"
    return labels


def row_name(problem, row: dict) -> str:
    """A short name for a result row: its label and the object's type."""
    if row["label"] == "other":
        return "other answers"
    oids = [problem.answers[l] for l in row["label"].split("+")]
    kinds = sorted({problem.object(o).type for o in oids})
    what = " or ".join(kinds)
    if oids == [problem.touched]:
        what = "touched " + what
    elif len(oids) > 1:
        what = "either " + what
    return f"{row['label']} {what}"


# ---- 1. the table settings -------------------------------------------------------------------

def fig_hero(out: Path):
    from tabletop.problems import load_problem
    pr = load_problem("surround-05")
    result, _ = one_run("surround-05", 3)
    scale = 6.0
    img, p = new_image(1120, 700)
    x0, y0 = 40, 66
    labels = answer_labels(pr)
    draw_table(p, pr, x0, y0, scale, labels, {pr.touched: FRENCH, result.answer: ROLES})
    px = x0 + pr.width * scale + 40
    y = 70
    text(p, px, y, "Surround 5", 24, True)
    y += 24
    text(p, px, y, "drawn from problems/surround-05.json", 13, False, INK2)
    y += 18
    text(p, px, y, "new vector icons, not French's figure", 13, False, INK2)
    y += 46

    def swatch(c, yy):
        p.setPen(QPen(QColor(c), 3))
        p.setBrush(colour(c, 40))
        p.drawRoundedRect(QRectF(px, yy - 13, 22, 16), 4, 4)

    swatch(FRENCH, y)
    text(p, px + 32, y, "H  Henry touches his cup", 15, True)
    y += 34
    swatch(ROLES, y)
    text(p, px + 32, y, "Eliza touches the cup across", 15, True)
    y += 20
    text(p, px + 32, y, f"the table: {result.answer} ({result.label}), the run", 13, False, INK2)
    y += 18
    text(p, px + 32, y, "python3 -m tabletop surround-05 --seed 3", 12, False, INK2, family=MONO)
    y += 18
    text(p, px + 32, y, f"(structure {result.structure_value:.0f}, {result.codelets} codelets)",
         13, False, INK2)
    y += 44
    text(p, px, y, "French's answers [F95 p. 118]", 15, True)
    y += 24
    columns = {label: (f, v1, roles) for label, f, v1, roles in three_columns("surround-05")}
    for label, what in (("E1", "the glass inside the matching row"),
                        ("E2", "the isolated cup")):
        f, v1, roles = columns[label]
        text(p, px, y, f"{label}  {what}", 13, False, INK)
        y += 20
        text(p, px + 26, y, f"French {f}%   v1 {v1:.1f}%   roles {roles:.1f}% of 500 runs",
             13, False, INK2)
        y += 26
    _f, v1, roles = columns["other"]
    text(p, px, y, f"other objects: v1 {v1:.1f}%, roles {roles:.1f}%", 13, False, INK)
    y += 20
    text(p, px + 26, y, "(French prints no bar for them)", 13, False, INK2)
    return save(img, p, out / "table-surround-05.png")


def fig_gallery(out: Path):
    from tabletop.problems import load_problem
    sets = (("surround-01", "original", 115), ("blockage-06", "original", 126),
            ("buridan-01", "original", 129))
    scale = 3.5
    panel = 110 * scale
    gap = 36
    width = round(3 * panel + 4 * gap)
    img, p = new_image(width, 560)
    for i, (name, program, page) in enumerate(sets):
        pr = load_problem(name)
        x0 = gap + i * (panel + gap)
        text(p, x0, 30, f"{pr.title}  [F95 p. {page}]", 17, True)
        draw_table(p, pr, x0, 64, scale, answer_labels(pr), {pr.touched: FRENCH})
        y = 64 + 100 * scale + 56
        text(p, x0, y, "answer", 12, True, INK2)
        text(p, x0 + panel - 130, y, "French", 12, True, INK2, "right")
        text(p, x0 + panel - 65, y, "v1", 12, True, INK2, "right")
        text(p, x0 + panel, y, "v4", 12, True, INK2, "right")
        for label, f, v1, roles in three_columns(name, program):
            y += 20
            text(p, x0, y, row_name(pr, {"label": label}), 13)
            text(p, x0 + panel - 130, y, "-" if f is None else f"{f}%", 13, False, INK, "right")
            text(p, x0 + panel - 65, y, f"{v1:.1f}%", 13, False, INK, "right")
            text(p, x0 + panel, y, f"{roles:.1f}%", 13, False, INK, "right")
    return save(img, p, out / "tables-gallery.png")


# ---- 2. the results against French's --------------------------------------------------------

def legend(p, x, y, items):
    for c, label in items:
        p.setPen(QPen(QColor(SURFACE), 2))
        p.setBrush(QColor(c))
        p.drawEllipse(QPointF(x + 6, y - 5), 6, 6)
        x += 18 + text(p, x + 18, y, label, 13, False, INK) + 26


def fig_main_answers(out: Path):
    recs = families()
    row_h, head_h = 19, 30
    families_seen = []
    for r in recs:
        fam = r["problem"].rsplit("-", 1)[0]
        if fam not in families_seen:
            families_seen.append(fam)
    height = 120 + len(recs) * row_h + len(families_seen) * head_h + 50
    width = 900
    img, p = new_image(width, height)
    text(p, 24, 32, "French's main answer in each of the 35 result sets of chapter 5", 18, True)
    text(p, 24, 54, "how often it was given: French's program, this reconstruction before "
         "roles (v1) and with roles on v4's defaults (500 runs each)", 13, False, INK2)
    legend(p, 24, 82, ((FRENCH, "French's program [F95 pp. 115-139]"),
                       (V1, "v1, before roles"), (ROLES, "v4, second optimization")))
    lx, ax0, ax1 = 24, 300, width - 40
    top = 110

    def xpos(v):
        return ax0 + (ax1 - ax0) * v / 100

    y = top
    rows_y = []
    current = None
    for r in recs:
        fam = r["problem"].rsplit("-", 1)[0]
        if fam != current:
            current = fam
            y += head_h
            title = {"surround": "Surround", "blockage": "Blockage", "buridan": "Buridan",
                     "figure-ground": "Figure/Ground"}[fam]
            text(p, lx, y - 8, title, 14, True)
        y += row_h
        rows_y.append((r, y - row_h / 2))
    bottom = y + 8
    for v in range(0, 101, 20):
        p.setPen(QPen(QColor(GRID), 1))
        p.drawLine(QPointF(xpos(v), top + 10), QPointF(xpos(v), bottom))
        text(p, xpos(v), bottom + 18, f"{v}%", 12, False, INK2, "center")
    for r, yc in rows_y:
        from tabletop.bench.results import main_row, row_named
        m = main_row(r)
        v1 = row_named(family_record(r["problem"], r["program"], "v1"), m["label"])
        f, o, o1 = m["french"]["frequency"], m["ours"]["frequency"], v1["ours"]["frequency"]
        name = r["title"].replace(" (original program)", " (original)") \
                         .replace(" (modified program)", " (modified)")
        text(p, lx + 12, yc + 5, name, 13)
        text(p, ax0 - 14, yc + 5, m["label"], 12, False, INK2, "right")
        p.setPen(QPen(QColor(MUTED), 2))
        p.drawLine(QPointF(xpos(min(f, o, o1)), yc), QPointF(xpos(max(f, o, o1)), yc))
        for v, c, radius in ((o1, V1, 4.5), (f, FRENCH, 5.5), (o, ROLES, 5.5)):
            p.setPen(QPen(QColor(SURFACE), 2))
            p.setBrush(QColor(c))
            p.drawEllipse(QPointF(xpos(v), yc), radius, radius)
    text(p, lx, bottom + 44, "Data: results/families.json and results/v1/families.json "
         "(RESULTS.md sections 2.1 and 11.1). The labels E1, E2, E3 are French's.",
         12, False, INK2)
    return save(img, p, out / "results-main-answer.png")


def fig_sets(out: Path):
    from tabletop.problems import load_problem
    sets = (("surround-01", "original"), ("surround-05", "original"),
            ("blockage-06", "original"), ("buridan-01", "original"))
    pw, row_h, bar_h = 470, 60, 14
    ph = 46 + max(len(three_columns(name, program)) for name, program in sets) * row_h
    width, height = 2 * pw + 40, 110 + 2 * ph + 30
    img, p = new_image(width, height)
    text(p, 24, 32, "Every answer of four result sets, in percent of the runs", 18, True)
    text(p, 24, 54, "French's program (about 50 runs) beside this reconstruction before "
         "roles (v1) and with roles on v4's defaults (500 runs each)", 13, False, INK2)
    legend(p, 24, 82, ((FRENCH, "French's program"), (V1, "v1, before roles"),
                       (ROLES, "v4, second optimization")))
    for i, (name, program) in enumerate(sets):
        pr = load_problem(name)
        rec = family_record(name, program)
        rows = three_columns(name, program)
        x0 = 24 + (i % 2) * (pw + 10)
        y0 = 110 + (i // 2) * ph
        text(p, x0, y0 + 20, f"{pr.title}  [F95 p. {rec['page']}]", 15, True)
        bx0, bx1 = x0 + 190, x0 + pw - 60
        y = y0 + 40
        p.setPen(QPen(QColor(GRID), 1))
        p.drawLine(QPointF(bx0, y - 4), QPointF(bx0, y + len(rows) * row_h - 4))
        for label, f, v1, roles in rows:
            text(p, x0, y + 29, row_name(pr, {"label": label}), 13)
            for j, (v, c) in enumerate(((f, FRENCH), (v1, V1), (roles, ROLES))):
                yy = y + 2 + j * (bar_h + 4)
                if v is None:
                    text(p, bx0 + 6, yy + 12, "no bar", 12, False, MUTED)
                    continue
                w = (bx1 - bx0) * v / 100
                p.setPen(Qt.NoPen)
                p.setBrush(QColor(c))
                p.drawRoundedRect(QRectF(bx0, yy, max(w, 1.5), bar_h), 3, 3)
                text(p, bx0 + w + 6, yy + 12, f"{v:.0f}%" if c == FRENCH else f"{v:.1f}%",
                     12, False, INK)
            y += row_h
    return save(img, p, out / "results-sets.png")


# ---- 3. the temperature ---------------------------------------------------------------------

def fig_temperature(out: Path):
    from tabletop.bench.ch4 import french_data, french_dumps
    curves = {}
    for engine, path in (("roles", RESULTS / "temperature.json"),
                         ("v1", RESULTS / "v1" / "temperature.json")):
        for line in path.read_text().splitlines():
            rec = json.loads(line)
            if rec["variant"] == "intact" and rec["survey"]["problem"] == "surround-05":
                curves[engine] = rec["curve"]
    curve = curves["roles"]
    french = [(d[2], d[3]) for d in french_dumps(french_data()) if d[3] is not None]
    result, events = one_run("surround-05", 3)
    updates = [(e.clock, e.temperature) for e in events if e.name == "TemperatureUpdated"]

    width, height = 900, 660               # the legend under the axis since v3 (D-114)
    img, p = new_image(width, height)
    text(p, 24, 32, "The temperature over a run of Surround 5", 18, True)
    text(p, 24, 54, "T against the codelets run: French's one run beside this "
         "reconstruction's 500, before roles (v1) and with roles on v4's defaults", 13, False, INK2)
    ax0, ax1, ay0, ay1 = 80, width - 40, 110, height - 190

    def pt(n, t):
        return QPointF(ax0 + (ax1 - ax0) * n / 100, ay1 - (ay1 - ay0) * t / 100)

    for t in range(0, 101, 20):
        p.setPen(QPen(QColor(GRID), 1))
        p.drawLine(pt(0, t), pt(100, t))
        text(p, ax0 - 10, pt(0, t).y() + 4, str(t), 12, False, INK2, "right")
    for n in range(0, 101, 10):
        text(p, pt(n, 0).x(), ay1 + 20, str(n), 12, False, INK2, "center")
    text(p, (ax0 + ax1) / 2, ay1 + 42, "codelets run", 13, False, INK2, "center")
    p.save()
    p.translate(30, (ay0 + ay1) / 2)
    p.rotate(-90)
    text(p, 0, 0, "temperature", 13, False, INK2, "center")
    p.restore()

    rows = [r for r in curve if r[1] is not None]
    band = QPolygonF([pt(r[0], r[2]) for r in rows] + [pt(r[0], r[4]) for r in reversed(rows)])
    p.setPen(Qt.NoPen)
    p.setBrush(colour(ROLES, 45))
    p.drawPolygon(band)
    p.setPen(QPen(QColor(ROLES), 2.5))
    p.setBrush(Qt.NoBrush)
    p.drawPolyline(QPolygonF([pt(r[0], r[1]) for r in rows]))
    p.setPen(QPen(QColor(V1), 2.5))
    p.drawPolyline(QPolygonF([pt(r[0], r[1]) for r in curves["v1"] if r[1] is not None]))

    steps = []
    last = updates[0][1]
    for n, t in updates:
        if n > 100:                    # the chart stops where the bench's curve does
            break
        steps += [pt(n, last), pt(n, t)]
        last = t
    steps.append(pt(min(result.codelets, 100), last))
    p.setPen(QPen(QColor(INK2), 1.5, Qt.DashLine))
    p.drawPolyline(QPolygonF(steps))

    p.setPen(QPen(QColor(FRENCH), 2))
    p.drawPolyline(QPolygonF([pt(n, t) for n, t in french]))
    for n, t in french:
        p.setPen(QPen(QColor(SURFACE), 2))
        p.setBrush(QColor(FRENCH))
        p.drawEllipse(pt(n, t), 5.5, 5.5)

    legend_top = ay1 + 78
    lx, ly = ax0, legend_top
    for k, (draw, label) in enumerate((
            ("french", "French's thermometer, his chapter 4 run [F95 pp. 96-107]"),
            ("v1", "v1, before roles: mean of the runs still going"),
            ("mean", "v4: mean of the runs still going"),
            ("band", "v4: middle half of the runs (quartiles)"),
            ("seed", f"v4: the one run of seed 3 (answer at {result.codelets})"))):
        if k == 3:                         # the second column
            lx, ly = ax0 + 430, legend_top
        if draw == "french":
            p.setPen(QPen(QColor(FRENCH), 2))
            p.drawLine(QPointF(lx - 30, ly - 5), QPointF(lx - 6, ly - 5))
            p.setPen(QPen(QColor(SURFACE), 2))
            p.setBrush(QColor(FRENCH))
            p.drawEllipse(QPointF(lx - 18, ly - 5), 5, 5)
        elif draw in ("mean", "v1"):
            p.setPen(QPen(QColor(ROLES if draw == "mean" else V1), 2.5))
            p.drawLine(QPointF(lx - 30, ly - 5), QPointF(lx - 6, ly - 5))
        elif draw == "band":
            p.setPen(Qt.NoPen)
            p.setBrush(colour(ROLES, 45))
            p.drawRect(QRectF(lx - 30, ly - 12, 24, 14))
        else:
            p.setPen(QPen(QColor(INK2), 1.5, Qt.DashLine))
            p.drawLine(QPointF(lx - 30, ly - 5), QPointF(lx - 6, ly - 5))
        text(p, lx, ly, label, 12)
        ly += 22
    text(p, ax0, height - 18, "Data: results/temperature.json, results/v1/temperature.json "
         "(RESULTS.md section 6.4), tests/data/ch4.json; the curves stop at 100.",
         12, False, INK2)
    return save(img, p, out / "temperature.png")


# ---- 4. the terminal ------------------------------------------------------------------------

TERMINAL_COMMAND = ["surround-05", "--seed", "3", "--table"]
HEAD_TRACE_LINES = 8          # the codelet lines shown after the map and its legend
TAIL_LINES = 4
COLUMNS = 104


def terminal_lines() -> list:
    proc = subprocess.run([sys.executable, "-m", "tabletop", *TERMINAL_COMMAND],
                          cwd=PYTHON_DIR, capture_output=True, text=True, check=True)
    lines = proc.stdout.splitlines()
    start = next(i for i, l in enumerate(lines) if l.startswith("tabletop "))
    head = lines[:start + 1 + HEAD_TRACE_LINES]
    tail = lines[-TAIL_LINES:]
    skipped = len(lines) - len(head) - len(tail)
    shown = [l if len(l) <= COLUMNS else l[:COLUMNS - 1] + "…" for l in head]
    shown.append(f"      ... {skipped} more lines ...")
    shown += [l if len(l) <= COLUMNS else l[:COLUMNS - 1] + "…" for l in tail]
    return shown


def fig_terminal(out: Path):
    lines = terminal_lines()
    f = font(13, family=MONO)
    fm = QFontMetricsF(f)
    cw, lh = fm.horizontalAdvance("M"), 17
    pad, bar = 18, 30
    width = round(pad * 2 + cw * COLUMNS)
    height = round(bar + pad * 2 + lh * (len(lines) + 1))
    img, p = new_image(width, height, "#1d1f21")
    p.setPen(Qt.NoPen)
    p.setBrush(QColor("#2d2f31"))
    p.drawRect(QRectF(0, 0, width, bar))
    for i, c in enumerate(("#ed6a5e", "#f5bf4f", "#61c554")):
        p.setBrush(QColor(c))
        p.drawEllipse(QPointF(18 + i * 20, bar / 2), 6, 6)
    text(p, width / 2, 20, "python/  (Tabletop)", 12, False, "#a0a3a6", "center")
    y = bar + pad + 12
    x = pad
    w = text(p, x, y, "$ ", 13, True, "#8bc34a", family=MONO)
    text(p, x + w, y, "python3 -m tabletop " + " ".join(TERMINAL_COMMAND), 13, False,
         "#ffffff", family=MONO)
    for line in lines:
        y += lh
        ink = "#d6d6d6"
        bold = False
        if line.startswith("Eliza touches") or line.startswith("tabletop "):
            ink, bold = "#ffffff", True
        elif line.strip().startswith("..."):
            ink = "#8a8f94"
        elif line.strip() in ("Henry", "Eliza"):
            ink, bold = "#f5bf4f", True
        text(p, x, y, line, 13, bold, ink, family=MONO)
    return save(img, p, out / "terminal.png")


FIGURES = {
    "table-surround-05": fig_hero,
    "tables-gallery": fig_gallery,
    "results-main-answer": fig_main_answers,
    "results-sets": fig_sets,
    "temperature": fig_temperature,
    "terminal": fig_terminal,
}


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--out", type=Path, default=DOCS, help="where to write (python/docs/)")
    parser.add_argument("--only", nargs="+", choices=sorted(FIGURES), help="these pictures only")
    args = parser.parse_args(argv)
    if QGuiApplication is None:
        print("docs_figures.py draws with PySide6, which is not installed: "
              "pip install PySide6 (the tabletop package itself needs nothing).",
              file=sys.stderr)
        return 1
    try:
        import tabletop  # noqa: F401
    except ImportError:
        print("run it from python/ as `python3 -m scripts.docs_figures`, or install the "
              "package first (pip install -e .)", file=sys.stderr)
        return 1
    app = QGuiApplication.instance() or QGuiApplication(["docs_figures"])  # noqa: F841
    too_big = 0
    for name in args.only or FIGURES:
        too_big += FIGURES[name](args.out) > LIMIT
    return 1 if too_big else 0


if __name__ == "__main__":
    sys.exit(main())
