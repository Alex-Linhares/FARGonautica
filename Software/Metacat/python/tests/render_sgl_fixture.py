"""Draw python/oracle/sgl-fixture.scm on a real tkinter Canvas and save it as PNG.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  xvfb-run -a python3 python/tests/render_sgl_fixture.py OUT.png [--check]

Never on the owner's screen: run it under xvfb-run (tests/test_sgl.py does).  The
fixture is drawn by metacat/gui/sgl.py on a 640 x 480 Canvas (viewport v1),
with fonts measured by Tk on the logo window's hidden canvas (fonts.ss's
create-mcat-logo) and Tk's scaling fixed at 96 dpi, as the Racket port fixes it.
The canvas window is then grabbed from the X server (Xlib's XGetImage, through
ctypes) and written to OUT.png (for inspection, next to
racket/tests/snapshots/sgl-fixture.png).  --check tests pixels whose colours only
a faithful drawing gives (hidden items, raise, delete, retag, move, erase, clear).
The script exits by itself.
"""
from __future__ import annotations

import ctypes
import ctypes.util
import struct
import zlib
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent))
sys.path.insert(0, str(HERE.parent / "oracle"))

import tkinter  # noqa: E402

from metacat.gui import colors, fonts, sgl, swl  # noqa: E402

import test_sgl as t  # noqa: E402

W, H = 640, 480

# (x, y) in the fixture's coordinates (y up), the colour expected there, and why
CHECKS = [
    ((5, 470), "ivory", "clear's background colour"),
    ((290, 360), "gold", "a filled rectangle"),
    ((515, 50), "ivory", "a hidden rectangle stays hidden"),
    ((595, 50), "dark violet", "unhide shows the second one"),
    ((398, 45), "ivory", "delete removes the tagged disc"),
    ((350, 45), "black", "the discs without the tag stay"),
    ((230, 50), "blue", "raise puts blue above the later green"),
    ((260, 70), "light green", "green where blue is not"),
    ((75, 60), "black", "move takes the square right and up"),
    ((25, 30), "grey70", "the grey square stays"),
    ((205, 160), "ivory", "erase paints the background colour"),
    ((180, 185), "orange", "around the erased square"),
    ((420, 380), "dark orange", "a pie slice"),
    ((445, 380), "ivory", "outside the slice's sweep"),
    ((66, 238), "light blue", "an open polygon's fill"),
    ((265, 160), "white", "a thick line erased in white"),
]


def render(out_png: Path, check: bool) -> int:
    root = tkinter.Tk()
    root.withdraw()
    root.tk.call("tk", "scaling", 96 / 72)
    swl.g_tk_root = root
    fonts.load()
    fonts.create_mcat_logo(root)
    fonts.g_mcat_logo.widget.winfo_toplevel().geometry("+700+0")   # out of the way
    sgl.load()
    top = tkinter.Toplevel(root)
    top.geometry("+0+0")
    widget = tkinter.Canvas(top, width=W, height=H, highlightthickness=0, borderwidth=0)
    widget.pack()
    sections, viewports = t.fixture_forms()
    spec = next(v for v in viewports if v[0] == "v1")
    vp = t.make_viewport(sgl, swl.TkCanvas(widget), *spec[1:])
    t.run_ops(sgl, fonts, colors, vp, sections["ops"], t.make_fonts(fonts, sections))
    for _ in range(3):
        root.update()
    pixels = grab(widget.winfo_id(), W, H)
    root.destroy()
    write_png(out_png, W, H, pixels)
    print("rendered %s: %d x %d" % (out_png, W, H))
    if not check:
        return 0
    failures = 0
    for (x, y), name, why in CHECKS:
        want = colors.swl_color(t.C.String(name))
        got = pixels[(H - y) * W + x]
        ok = got == (want.r, want.g, want.b)
        print("%-4s (%d, %d) %s: %s, want %s (%s)" % ("ok" if ok else "FAIL", x, y, name,
                                                     got, (want.r, want.g, want.b), why))
        failures += not ok
    return 1 if failures else 0


def grab(window_id: int, width: int, height: int):
    """The window's pixels, row by row, as (r, g, b): XGetImage on a TrueColor visual."""
    x11 = ctypes.CDLL(ctypes.util.find_library("X11"))
    x11.XOpenDisplay.restype = ctypes.c_void_p
    x11.XOpenDisplay.argtypes = [ctypes.c_char_p]
    x11.XGetImage.restype = ctypes.c_void_p
    x11.XGetImage.argtypes = [ctypes.c_void_p, ctypes.c_ulong, ctypes.c_int, ctypes.c_int,
                              ctypes.c_uint, ctypes.c_uint, ctypes.c_ulong, ctypes.c_int]
    x11.XGetPixel.restype = ctypes.c_ulong
    x11.XGetPixel.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.c_int]
    x11.XDestroyImage = getattr(x11, "XDestroyImage", None)
    x11.XCloseDisplay.argtypes = [ctypes.c_void_p]
    display = x11.XOpenDisplay(None)
    if not display:
        raise SystemExit("no X display")
    all_planes, z_pixmap = (1 << 64) - 1, 2
    image = x11.XGetImage(display, window_id, 0, 0, width, height, all_planes, z_pixmap)
    if not image:
        raise SystemExit("XGetImage failed")
    pixels = []
    for y in range(height):
        for x in range(width):
            p = x11.XGetPixel(image, x, y)
            pixels.append(((p >> 16) & 255, (p >> 8) & 255, p & 255))
    x11.XCloseDisplay(display)
    return pixels


def write_png(path: Path, width: int, height: int, pixels) -> None:
    """A minimal 8-bit RGB PNG writer (zlib, no filtering)."""
    raw = bytearray()
    for y in range(height):
        raw.append(0)
        for r, g, b in pixels[y * width:(y + 1) * width]:
            raw += bytes((r, g, b))

    def chunk(kind, data):
        return (struct.pack(">I", len(data)) + kind + data
                + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF))

    path.write_bytes(b"\x89PNG\r\n\x1a\n"
                     + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
                     + chunk(b"IDAT", zlib.compress(bytes(raw), 9))
                     + chunk(b"IEND", b""))


if __name__ == "__main__":
    sys.exit(render(Path(sys.argv[1]), "--check" in sys.argv[2:]))
