"""Tk canvas command streams, replayed into any canvas, and normalised display lists.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The reference for the Qt canvas (metacat/qt/canvas.py, loop0003 item 02) is a real
tkinter Canvas given the same commands.  This module:

- reads the frozen streams of python/fixtures/sgl-tcl/ (what the original's SGL
  interpreter sends Tk: `(tcl v1 create rectangle ... \\x2D;fill (rgb 0 0 0) ...)`)
  into the arguments a panel passes to its canvas's ``tcl`` (symbols as str,
  strings as chez.String, exact ratios as Fraction, colours as Rgb, fonts as
  (face size style...) tuples);
- adds SYNTHETIC, a stream that reaches what the fixture streams don't: arrows,
  smoothing, justification, lower, raise above an item, ids as targets, tag
  lists, scale, hidden and empty items in bbox, and many arcs, lines, polygons
  and rectangles at fractional coordinates, for Tk's bbox rules;
- replays a stream into canvases (anything with ``tcl``), collecting the answers
  of create, bbox and canvasx;
- dumps a canvas as a normalised display list through the Tk commands find,
  type, coords, itemcget, gettags and bbox, which the Qt canvas answers too;
- records Tk's own metrics of every text (font measure, font metrics
  -linespace), so that the Qt canvas's bbox rules can be checked exactly,
  apart from the fonts.

  env -u WAYLAND_DISPLAY xvfb-run -a python3 python/tests/canvas_streams.py OUT.json

writes the dumps of every stream replayed into tkinter Canvases (Tk's scaling at
96 dpi): python/tests/data/tk-display-lists.json.  The script exits by itself.
"""
from __future__ import annotations

import json
import random
import re
import sys
from fractions import Fraction
from pathlib import Path

HERE = Path(__file__).resolve().parent
PY = HERE.parent
if str(PY) not in sys.path:
    sys.path.insert(0, str(PY))

from metacat import chez  # noqa: E402
from metacat.gui import colors  # noqa: E402

STREAMS = PY / "fixtures" / "sgl-tcl"
REFERENCE = HERE / "data" / "tk-display-lists.json"

# Tk's option names per item kind, compared in the display lists
OPTIONS = {
    "line": ["-fill", "-width", "-dash", "-arrow", "-smooth", "-state"],
    "rectangle": ["-fill", "-outline", "-width", "-dash", "-state"],
    "oval": ["-fill", "-outline", "-width", "-dash", "-state"],
    "arc": ["-fill", "-outline", "-width", "-dash", "-start", "-extent", "-style", "-state"],
    "polygon": ["-fill", "-outline", "-width", "-dash", "-smooth", "-state"],
    "text": ["-anchor", "-fill", "-font", "-text", "-justify", "-state"],
}
COLOR_OPTIONS = {"-fill", "-outline"}
NUMBER_OPTIONS = {"-width", "-start", "-extent"}


# ---------------------------------------------------------------------------
# Reading the fixture streams

_TOKEN = re.compile(r'\s*(?:(\()|(\))|"((?:[^"\\]|\\.)*)"|([^\s()"]+))')


def _parse(text):
    stack = [[]]
    pos = 0
    while True:
        m = _TOKEN.match(text, pos)
        if not m or m.end() == pos:
            break
        pos = m.end()
        if m.group(1):
            stack.append([])
        elif m.group(2):
            done = stack.pop()
            stack[-1].append(done)
        elif m.group(3) is not None:
            stack[-1].append(chez.String(m.group(3).replace('\\"', '"').replace("\\\\", "\\")))
        else:
            stack[-1].append(_atom(m.group(4)))
    if text[pos:].strip() or len(stack) != 1:
        raise ValueError("cannot parse %r" % text)
    return stack[0]


def _atom(word):
    word = word.replace("\\x2D;", "-")
    if re.fullmatch(r"-?\d+", word):
        return int(word)
    if re.fullmatch(r"-?\d+/\d+", word):
        return Fraction(word)
    if re.fullmatch(r"-?\d+\.\d*", word):
        return float(word)
    return word


def _argument(x):
    """a parsed argument as a panel passes it"""
    if isinstance(x, list) and x and x[0] == "rgb":
        return colors.Rgb(*x[1:])
    if isinstance(x, list) and x and x[0] == "font":
        return (x[1], x[2], *x[3])
    return x


def read_stream(path):
    """[(kind, canvas name, args)]: kind is 'tcl' (a canvas command) or 'swl' (a
    canvas method: set-background-color!, whose colour is a name)."""
    commands = []
    for line in Path(path).read_text().splitlines():
        (form,) = _parse(line)
        kind, name, *args = form
        if kind == "swl":
            commands.append(("swl", name, [args[0], colors.swl_color(args[1])]))
        else:
            commands.append(("tcl", name, [_argument(a) for a in args]))
    return commands


# ---------------------------------------------------------------------------
# The synthetic stream

def _synthetic():
    c = []

    def t(*args):
        c.append(("tcl", "s1", list(args)))

    red, blue, black = colors.Rgb(255, 0, 0), colors.Rgb(0, 0, 255), colors.Rgb(0, 0, 0)
    helv, times = ("helvetica", -12), ("times", 14, "bold", "italic")
    t("create", "line", 10, 10, 90, 40, "-arrow", "last", "-width", 2, "-tags", "arrows")
    t("create", "line", 10, 50, 90, 50, "-arrow", "first", "-fill", red, "-tags", "arrows")
    t("create", "line", 10, 60, 50, 90, 90, 60, "-arrow", "both", "-width", 3)
    t("create", "line", 100, 10, 130, 60, 160, 10, 190, 60, "-smooth", 1, "-fill", blue)
    t("create", "line", 100, 70, 150, 95, 190, 70, "-smooth", "true", "-dash", ". ")
    t("create", "polygon", 200, 10, 260, 10, 280, 50, 230, 70, "-smooth", 1,
      "-outline", black, "-fill", "light blue")
    t("create", "polygon", 300, 10, 340, 60, 260, 60, "-fill", "", "-outline", "dark green",
      "-width", 3)
    t("create", "text", 100, 120, "-text", "left\nlonger line", "-anchor", "nw",
      "-font", helv, "-justify", "left", "-tags", "texts")
    t("create", "text", 250, 120, "-text", "centre\nlonger line", "-anchor", "n",
      "-font", helv, "-justify", "center", "-tags", ("texts", "centred"))
    t("create", "text", 400, 120, "-text", "right\nlonger line", "-anchor", "ne",
      "-font", helv, "-justify", "right", "-fill", red, "-tags", "texts")
    for anchor in ["n", "ne", "e", "se", "s", "sw", "w", "nw", "center"]:
        t("create", "text", 300, 200, "-text", "anchor " + anchor, "-anchor", anchor,
          "-font", times, "-tags", "anchors")
    t("create", "text", 51.5, 200.5, "-text", "", "-anchor", "s", "-font", times)
    t("create", "text", 61, 210, "-text", "no fill", "-fill", "", "-font", helv)
    t("create", "text", 71.25, 220.75, "-text", "hidden", "-state", "hidden", "-font", helv)
    t("create", "rectangle", 20, 300, 60, 340, "-fill", "gold", "-tags", ("a", "b"))
    t("create", "rectangle", 30, 310, 70, 350, "-fill", "grey40", "-tags", "b")
    t("create", "oval", 40, 320, 80, 360, "-fill", "SteelBlue", "-outline", "", "-tags", "c")
    t("create", "rectangle", 50, 330, 90, 370, "-fill", "#abc", "-outline", "#00ff00",
      "-tags", "c")
    t("lower", "c")
    t("raise", "a", "c")
    t("raise", "18")
    t("lower", "b", "19")
    t("create", "rectangle", 120, 300, 160, 340, "-tags", "scaled", "-width", 2)
    t("create", "oval", 120, 300, 160, 340, "-tags", "scaled")
    t("create", "text", 140, 320, "-text", "s", "-font", helv, "-tags", "scaled")
    t("create", "line", 120, 300, 160, 340, "-tags", "scaled")
    t("scale", "scaled", 100, 300, 1.5, Fraction(1, 2))
    t("scale", "scaled", 0, 0, -1, 1)
    t("move", "all", 3, -2)
    t("move", "1", Fraction(1, 2), 0.25)
    t("itemconfigure", "b", "-state", "hidden", "-width", 4)
    t("itemconfigure", "c", "-tags", ("x", "y", "z"), "-fill", "orange")
    t("create", "rectangle", 200, 300, 200, 300)
    t("create", "rectangle", 210, 320, 205, 310, "-outline", "")
    t("create", "oval", 230, 300, 240, 300.4)
    t("delete", "2")
    t("delete", "no-such-tag")
    t("itemconfigure", "no-such-tag", "-state", "hidden")
    t("move", "no-such-tag", 5, 5)
    t("bbox", "no-such-tag")
    t("bbox", "arrows")
    t("bbox", "b")
    t("bbox", "texts", "c")
    t("bbox", "all")
    t("canvasx", 10)
    t("canvasy", 7.5)
    t("itemconfigure", "b", "-state", "normal")
    t("itemconfigure", "anchors", "-state", "disabled")
    t("bbox", "b")
    t("bbox", "anchors")
    t("create", "rectangle", 400, 400, 410, 410, "-fill", "#abc", "-outline", "DarkViolet")
    t("create", "oval", 420, 400, 430, 410, "-fill", "LightGoldenrod1", "-outline", "gray")
    t("create", "line", 440, 400, 450, 410, "-fill", "#123456789", "-dash", "-")
    t("create", "polygon", 460, 400, 470, 400, 465, 410, "-fill", "#1234abcd5678",
      "-outline", "Navy Blue", "-dash", "_ ,", "-width", 2)
    rng = random.Random(1)
    for i in range(160):
        x, y = rng.randint(-50, 600) + rng.choice([0, 0.5, 0.25, 0.75, 0.4]), \
            rng.randint(-50, 500) + rng.choice([0, 0.5, 0.6])
        w, h = rng.choice([0, 1, 2, 3, 4, 7, 30, 61]), rng.choice([0, 1, 2, 5, 40])
        width = rng.choice([0, 1, 1.5, 2, 3, 4, 5])
        kind = ["arc", "arc", "rectangle", "oval", "line", "polygon"][i % 6]
        if kind == "arc":
            start = rng.choice([0, 30, 90, 180, 270, 315, -90, -45, 400, 12.5])
            extent = rng.choice([90, 180, 225, 300, 360, -90, -200, 1, 359, 400, 720])
            style = rng.choice(["arc", "pieslice", "chord"])
            outline = rng.choice([black, ""])
            t("create", "arc", x, y, x + w, y + h, "-start", start, "-extent", extent,
              "-style", style, "-width", width, "-outline", outline, "-fill", red)
        elif kind in ("rectangle", "oval"):
            t("create", kind, x + w, y + h, x, y, "-width", width,
              "-outline", rng.choice([black, ""]))
        elif kind == "line":
            pts = [v for _ in range(rng.randint(2, 4))
                   for v in (rng.randint(-40, 600) + rng.choice([0, 0.5, -0.5]),
                             rng.randint(-40, 500) + rng.choice([0, 0.5, -0.5]))]
            t("create", "line", *pts, "-width", width, "-arrow",
              rng.choice(["none", "none", "first", "last", "both"]))
        else:
            pts = [v for _ in range(rng.randint(3, 5))
                   for v in (rng.randint(-40, 600) + rng.choice([0, 0.5, -0.5]),
                             rng.randint(-40, 500) + rng.choice([0, 0.5, -0.5]))]
            t("create", "polygon", *pts, "-width", width,
              "-outline", rng.choice([black, ""]))
    return c


SYNTHETIC = _synthetic()


def streams():
    """{name: commands}: the two fixture streams and the synthetic one"""
    out = {p.stem: read_stream(p) for p in sorted(STREAMS.glob("*.txt"))}
    out["synthetic"] = SYNTHETIC
    return out


# ---------------------------------------------------------------------------
# Replay and dump

def replay(commands, canvases):
    """Send each command to canvases[name].  A bbox on the hidden canvas goes to
    the id of its latest create (the fixture's fake hidden canvas answered "1"
    to every create).  Returns the answers of create, bbox,
    canvasx and canvasy: [[canvas, command, answer]]."""
    answers = []
    last_id = {}
    for kind, name, args in commands:
        canvas = canvases[name]
        if kind == "swl":
            getattr(canvas, args[0].replace("-", "_").replace("!", "_bang"))(*args[1:])
            continue
        if args[0] == "bbox" and name == "hidden" and name in last_id:
            args = ["bbox", str(last_id[name])]
        result = canvas.tcl(*args)
        if args[0] == "create":
            last_id[name] = int(result)
            answers.append([name, "create", int(result)])
        elif args[0] in ("bbox", "canvasx", "canvasy"):
            answers.append([name, " ".join(map(str, args)), norm_answer(result)])
    return answers


def norm_answer(x):
    if isinstance(x, str) and not str(x):
        return None
    if isinstance(x, (list, tuple)):
        return [int(v) for v in x]
    return float(x)


def tcl_split(x):
    """a Tcl list (a string, or a tuple from tkinter) as a list of strings"""
    if isinstance(x, (list, tuple)):
        return [str(a) for a in x]
    s, out, i = str(x), [], 0
    while i < len(s):
        if s[i].isspace():
            i += 1
        elif s[i] == "{":
            depth, j = 1, i + 1
            while depth:
                depth += {"{": 1, "}": -1}.get(s[j], 0)
                j += 1
            out.append(s[i + 1:j - 1])
            i = j
        else:
            j = i
            while j < len(s) and not s[j].isspace():
                j += 1
            out.append(s[i:j])
            i = j
    return out


def norm_option(option, value, rgb):
    value = str(value) if not isinstance(value, (list, tuple)) else value
    if option in COLOR_OPTIONS:
        return "#%02x%02x%02x" % rgb(value) if value else ""
    if option in NUMBER_OPTIONS:
        return float(value)
    if option == "-smooth":
        return value not in ("0", "false", "no", "off", "")
    if option == "-font":
        return tcl_split(value)
    return value


def dump(canvas, rgb):
    """The canvas's display list, bottom to top: id, type, coords, options,
    tags and bbox of each item, through Tk's own query commands.  rgb(name)
    gives a colour's 8-bit (r, g, b)."""
    items = []
    for i in canvas.tcl("find", "all") or []:
        i = int(i)
        kind = str(canvas.tcl("type", i))
        items.append({
            "id": i,
            "type": kind,
            "coords": [round(float(v), 4) for v in canvas.tcl("coords", i)],
            "options": {o: norm_option(o, canvas.tcl("itemcget", i, o), rgb)
                        for o in OPTIONS[kind]},
            "tags": tcl_split(canvas.tcl("gettags", i)),
            "bbox": norm_answer(canvas.tcl("bbox", i)),
        })
    return {"items": items, "bbox all": norm_answer(canvas.tcl("bbox", "all"))}


def font_key(font):
    """a font word (a tuple, or a Tcl list string) as one string"""
    return " ".join(tcl_split(font))


def text_metrics(commands, width, linespace):
    """Tk's metrics of every text a stream creates: {"widths": {font key + "|" +
    line: pixels}, "linespace": {font key: pixels}}, from width(font, line) and
    linespace(font)."""
    from metacat.gui import swl
    widths, spaces = {}, {}
    for kind, _, args in commands:
        if kind != "tcl" or args[:2] != ["create", "text"]:
            continue
        opts = dict(zip(args[4::2], args[5::2]))
        font = swl.tcl_word(opts["-font"])
        key = font_key(font)
        spaces[key] = int(linespace(font))
        for line in str(opts.get("-text", "")).split("\n"):
            widths[key + "|" + line] = int(width(font, line))
    return {"widths": widths, "linespace": spaces}


def run(make_canvas, rgb, measure=None):
    """{stream: {"answers": ..., canvas name: dump}} for every stream"""
    out = {}
    for sname, commands in streams().items():
        canvases = {}
        for _, name, _ in commands:
            if name not in canvases:
                canvases[name] = make_canvas(name)
        answers = replay(commands, canvases)
        out[sname] = {"answers": answers,
                      **{name: dump(c, rgb) for name, c in sorted(canvases.items())}}
        if measure:
            out[sname]["metrics"] = text_metrics(commands, *measure)
    return out


# ---------------------------------------------------------------------------
# The Tk reference (under Xvfb)

TK_VERSION = []


def tk_run():
    import tkinter
    from metacat.gui import swl
    root = tkinter.Tk()
    root.withdraw()
    root.tk.call("tk", "scaling", 96 / 72)

    def make_canvas(name):
        top = tkinter.Toplevel(root)
        widget = tkinter.Canvas(top, width=640, height=480, highlightthickness=0,
                                borderwidth=0)
        widget.pack()
        return swl.TkCanvas(widget)

    def rgb(name):
        return tuple(v >> 8 for v in root.winfo_rgb(name))

    def width(font, line):
        return root.tk.call("font", "measure", font, line)

    def linespace(font):
        return root.tk.call("font", "metrics", font, "-linespace")

    root.update()
    TK_VERSION.append(str(root.tk.call("info", "patchlevel")))
    try:
        return run(make_canvas, rgb, (width, linespace))
    finally:
        root.destroy()


if __name__ == "__main__":
    dumps = tk_run()
    result = {"about": "Every stream of python/tests/canvas_streams.py replayed into "
                       "tkinter Canvases under Xvfb (scaling 96 dpi): the answers and the "
                       "normalised display lists.  Do not edit by hand.",
              "tk": TK_VERSION[0],
              "streams": dumps}
    Path(sys.argv[1]).write_text(json.dumps(result, indent=1, sort_keys=True) + "\n")
    print("wrote", sys.argv[1])
