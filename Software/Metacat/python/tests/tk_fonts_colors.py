"""Tk's fonts and colours, the reference of the Qt fonts and colours (loop0003 item 03).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Run under Xvfb (``xvfb-run -a python3 tk_fonts_colors.py OUT.json``), with Tk's
scaling at 96 dpi as the GUIs fix it, this records:

- for every font word of FONTS: Tk's ``font metrics`` (ascent, descent,
  linespace), ``font actual -family`` and ``font measure`` of each of SAMPLES;
- for every colour name of metacat/gui/colors.py: Tk's ``winfo rgb``, as 8-bit
  (r, g, b).

data/tk-fonts-colors.json is this file's output; test_qt_fonts.py compares the
Qt mapping with it, and its slow tier checks that Tk still says the same.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))

FAMILIES = ("helvetica", "times", "courier")
SIZES = (-5, -6, -7, -8, -9, -10, -11, -12, -14, -16, -18, -20, -22, -24, -30,
         8, 10, 12, 14, 18, 24)
STYLES = ((), ("bold",), ("italic",), ("bold", "italic"))
FONTS = [(f, s, *st) for f in FAMILIES for s in SIZES for st in STYLES]

# the Coderack's codelet types, a Slipnet label, digits and Latin-1's tallest
SAMPLES = ("Bond builders", "Whole-string", "Description", "illil", "MWM",
           "opposite", "0123456789", "Å É ç")


def font_key(font):
    return " ".join(str(w) for w in font)


def tk_reference():
    import tkinter
    from metacat.gui import colors
    root = tkinter.Tk()
    root.withdraw()
    root.tk.call("tk", "scaling", 96 / 72)
    try:
        fonts = {}
        for font in FONTS:
            metrics = {k: int(root.tk.call("font", "metrics", font, "-" + k))
                       for k in ("ascent", "descent", "linespace")}
            metrics["family"] = str(root.tk.call("font", "actual", font, "-family"))
            metrics["widths"] = {s: int(root.tk.call("font", "measure", font, s))
                                 for s in SAMPLES}
            fonts[font_key(font)] = metrics
        rgb = {name: [v >> 8 for v in root.winfo_rgb(name)]
               for name, *_ in colors._COLOR_TABLE}
        return {"about": "Tk's font metrics and colours under Xvfb (scaling 96 dpi), "
                         "from python/tests/tk_fonts_colors.py.  Do not edit by hand.",
                "tk": str(root.tk.call("info", "patchlevel")),
                "fonts": fonts, "colors": rgb}
    finally:
        root.destroy()


if __name__ == "__main__":
    Path(sys.argv[1]).write_text(json.dumps(tk_reference(), indent=1, sort_keys=True) + "\n")
    print("wrote", sys.argv[1])
