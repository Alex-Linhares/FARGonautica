#!/usr/bin/env python3
"""Measure a table figure extracted from the book, in pixels (the digitization of D-003).

The layouts of chapter 5 are embedded images, one per result set, 489 x 205 pixels at
135 ppi [F95 pp. 115-139 (img)]; chapter 2's are of the same kind [F95 pp. 27-35 (img)].
Each shows the table as a thin square frame at the left with "Henry" above it and "Eliza"
below it, the objects as small icons inside it, the touched object's H arrow and the E
arrows of Eliza's answers [F95 p. 114]. This tool, standard library only, does three things:

- `read_png` / `write_png`: a minimal PNG reader and writer (8-bit grey, RGB, with or
  without alpha; the filters of the PNG specification; `zlib` for the streams);
- `find_frame`: the table's frame as (left, top, right, bottom) pixel coordinates, found as
  the longest dark vertical and horizontal runs in the left part of the image;
- `components`: the 8-connected clusters of dark pixels inside the frame (the frame's own
  line erased), each with its bounding box, pixel count and centre, in pixels and as
  fractions of the frame's inner width and height (D-001: the table is the unit square, the
  origin at Henry's upper-left corner, y downwards).

Run it on an image extracted with `pdfimages -png` under /tmp:

    python3 tools/measure_figure.py /tmp/tt/s/img-008.png            # frame and components
    python3 tools/measure_figure.py /tmp/tt/s/img-008.png --scale 3 --out /tmp/tt/big.png
    python3 tools/measure_figure.py /tmp/tt/b/img-046.png --frame 11,14,205,189   # faint lines

`--scale N --out PATH` writes the frame's region enlarged N times (nearest neighbour) for
reading by eye. Nothing here is ever committed as an image: the figures are copyrighted and
are read in place (`docs/problems.md`).
"""

from __future__ import annotations

import argparse
import struct
import sys
import zlib
from pathlib import Path

DARK = 128  # a pixel is "ink" below this grey level (the JPEG figures are near-binary)


# ---------------------------------------------------------------------------------------------
# PNG reading and writing (the standard library has no image module).


def _paeth(a: int, b: int, c: int) -> int:
    p = a + b - c
    pa, pb, pc = abs(p - a), abs(p - b), abs(p - c)
    if pa <= pb and pa <= pc:
        return a
    return b if pb <= pc else c


def _unfilter(kind: int, line: bytearray, prev: bytes, bpp: int) -> bytearray:
    out = bytearray(line)
    n = len(out)
    if kind == 0:
        return out
    for i in range(n):
        a = out[i - bpp] if i >= bpp else 0
        b = prev[i]
        c = prev[i - bpp] if i >= bpp else 0
        if kind == 1:
            out[i] = (out[i] + a) & 255
        elif kind == 2:
            out[i] = (out[i] + b) & 255
        elif kind == 3:
            out[i] = (out[i] + ((a + b) >> 1)) & 255
        elif kind == 4:
            out[i] = (out[i] + _paeth(a, b, c)) & 255
        else:
            raise ValueError(f"unknown PNG filter {kind}")
    return out


def read_png(path: str | Path) -> tuple[int, int, list[list[int]]]:
    """Read an 8-bit PNG and return (width, height, rows of grey levels 0-255).

    Colour is reduced to grey as the rounded mean of R, G and B; alpha is ignored; palette
    images are refused (none of the extracted figures is one). The reader exists so that a
    figure can be measured with the standard library alone (D-003).
    """
    data = Path(path).read_bytes()
    if data[:8] != b"\x89PNG\r\n\x1a\n":
        raise ValueError(f"{path}: not a PNG")
    pos, idat, width = 8, bytearray(), 0
    height = depth = ctype = 0
    while pos < len(data):
        length, tag = struct.unpack(">I4s", data[pos:pos + 8])
        body = data[pos + 8:pos + 8 + length]
        pos += 12 + length
        if tag == b"IHDR":
            width, height, depth, ctype = struct.unpack(">IIBB", body[:10])
            if body[12] != 0:
                raise ValueError(f"{path}: interlaced PNGs are not supported")
        elif tag == b"IDAT":
            idat += body
        elif tag == b"IEND":
            break
    if depth != 8:
        raise ValueError(f"{path}: only 8-bit PNGs are supported (depth {depth})")
    channels = {0: 1, 2: 3, 4: 2, 6: 4}.get(ctype)
    if channels is None:
        raise ValueError(f"{path}: colour type {ctype} is not supported")
    raw = zlib.decompress(bytes(idat))
    stride = width * channels
    rows, prev, p = [], bytes(stride), 0
    for _ in range(height):
        kind = raw[p]
        line = _unfilter(kind, bytearray(raw[p + 1:p + 1 + stride]), prev, channels)
        p += 1 + stride
        if channels == 1:
            rows.append(list(line))
        elif channels == 2:
            rows.append(list(line[0::2]))
        else:
            rows.append([(line[i] + line[i + 1] + line[i + 2] + 1) // 3
                         for i in range(0, stride, channels)])
        prev = line
    return width, height, rows


def write_png(path: str | Path, rows: list[list[tuple[int, int, int]]]) -> None:
    """Write rows of (r, g, b) pixels as an 8-bit RGB PNG (filter 0 on every line).

    Used only for review images under /tmp (D-003); never for anything committed.
    """
    height, width = len(rows), len(rows[0])
    raw = bytearray()
    for row in rows:
        raw.append(0)
        for r, g, b in row:
            raw += bytes((r, g, b))

    def chunk(tag: bytes, body: bytes) -> bytes:
        return (struct.pack(">I", len(body)) + tag + body
                + struct.pack(">I", zlib.crc32(tag + body) & 0xFFFFFFFF))

    out = b"\x89PNG\r\n\x1a\n"
    out += chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
    out += chunk(b"IDAT", zlib.compress(bytes(raw), 9))
    out += chunk(b"IEND", b"")
    Path(path).write_bytes(out)


# ---------------------------------------------------------------------------------------------
# The frame and the ink inside it.


def find_frame(rows: list[list[int]], dark: int = DARK) -> tuple[int, int, int, int]:
    """The table's frame as (left, top, right, bottom) pixel coordinates of its lines.

    The frame is the thin square at the left of every chapter 5 figure [F95 p. 115 (img)]
    and fills the whole of a chapter 2 figure, which draws the table alone [F95 p. 27
    (img)]. In the JPEG images its one-pixel lines are smeared to a faint grey, so they are
    found as the darkest column of the left eighth of the image (left), the darkest column
    between three-tenths and twelve-twenty-fifths of the width (right: the bar graph lies
    further right) or, for a figure less than one and a half times wider than tall, of the
    right eighth, and the darkest rows of the top and bottom quarters (top, bottom), each
    mean taken over the span the other pair of lines encloses. A figure with no line darker
    than the page raises `ValueError`. `dark` is unused here (kept for the CLI's symmetry).
    """
    height, width = len(rows), len(rows[0])
    table_alone = width * 2 < height * 3  # chapter 2's figures: no bar graph to the right

    def col_mean(x: int, y0: int, y1: int) -> float:
        return sum(rows[y][x] for y in range(y0, y1)) / (y1 - y0)

    def row_mean(y: int, x0: int, x1: int) -> float:
        return sum(rows[y][x0:x1]) / (x1 - x0)

    y0, y1 = height // 8, height - height // 8
    left = min(range(0, width // 8), key=lambda x: col_mean(x, y0, y1))
    right_span = range(width - width // 8, width) if table_alone \
        else range(width * 3 // 10, width * 12 // 25)
    right = min(right_span, key=lambda x: col_mean(x, y0, y1))
    x0, x1 = left + 4, right - 4
    top = min(range(0, height // 4), key=lambda y: row_mean(y, x0, x1))
    bottom = min(range(height * 3 // 4, height), key=lambda y: row_mean(y, x0, x1))
    if max(col_mean(left, y0, y1), col_mean(right, y0, y1),
           row_mean(top, x0, x1), row_mean(bottom, x0, x1)) > 230:
        raise ValueError("no frame found")
    return left, top, right, bottom


def components(rows: list[list[int]], frame: tuple[int, int, int, int], dark: int = DARK,
               margin: int = 2) -> list[dict]:
    """The 8-connected clusters of dark pixels strictly inside the frame.

    `margin` pixels inside each side of the frame are ignored (the frame line and its
    JPEG halo). Each cluster is a dict with `box` (minx, miny, maxx, maxy, inclusive),
    `pixels`, `cx`, `cy` (the centroid, in pixels) and `fx`, `fy` (the centroid as fractions
    of the frame's inner width and height, D-001). Clusters are listed top to bottom, then
    left to right, by their box's upper-left corner.
    """
    left, top, right, bottom = frame
    x0, y0, x1, y1 = left + 1 + margin, top + 1 + margin, right - 1 - margin, bottom - 1 - margin
    seen = set()
    out = []
    for sy in range(y0, y1 + 1):
        for sx in range(x0, x1 + 1):
            if rows[sy][sx] >= dark or (sx, sy) in seen:
                continue
            stack, members = [(sx, sy)], []
            seen.add((sx, sy))
            while stack:
                x, y = stack.pop()
                members.append((x, y))
                for dx in (-1, 0, 1):
                    for dy in (-1, 0, 1):
                        nx, ny = x + dx, y + dy
                        if (x0 <= nx <= x1 and y0 <= ny <= y1 and (nx, ny) not in seen
                                and rows[ny][nx] < dark):
                            seen.add((nx, ny))
                            stack.append((nx, ny))
            xs = [x for x, _ in members]
            ys = [y for _, y in members]
            cx = sum(xs) / len(members)
            cy = sum(ys) / len(members)
            out.append({"box": (min(xs), min(ys), max(xs), max(ys)), "pixels": len(members),
                        "cx": cx, "cy": cy,
                        "fx": (cx - left) / (right - left), "fy": (cy - top) / (bottom - top)})
    out.sort(key=lambda c: (c["box"][1], c["box"][0]))
    return out


def merge_close(clusters: list[dict], frame: tuple[int, int, int, int],
                gap: int = 2) -> list[dict]:
    """Merge clusters whose boxes lie within `gap` pixels of each other.

    The JPEG figures break an icon's thin lines into several clusters (a fork's tines and
    handle, a cup's body and handle); icons sit further apart than their own fragments, so
    merging boxes closer than two pixels rebuilds the icons while keeping neighbours apart
    (D-003; the exceptions, an arrow touching its object, are resolved by eye and recorded
    in `docs/problems.md`). Merged clusters carry the summed pixel count and the box's
    centre (`cx`, `cy`, `fx`, `fy`), not the fragments' centroid, so that a thin handle
    does not pull the centre away from the icon's body.
    """
    left, top, right, bottom = frame
    boxes = [dict(c) for c in clusters]
    merged = True
    while merged:
        merged = False
        for i in range(len(boxes)):
            for j in range(i + 1, len(boxes)):
                a, b = boxes[i]["box"], boxes[j]["box"]
                if (a[0] - gap <= b[2] and b[0] - gap <= a[2]
                        and a[1] - gap <= b[3] and b[1] - gap <= a[3]):
                    box = (min(a[0], b[0]), min(a[1], b[1]), max(a[2], b[2]), max(a[3], b[3]))
                    boxes[i] = {"box": box, "pixels": boxes[i]["pixels"] + boxes[j]["pixels"]}
                    del boxes[j]
                    merged = True
                    break
            if merged:
                break
    for c in boxes:
        x0, y0, x1, y1 = c["box"]
        c["cx"], c["cy"] = (x0 + x1) / 2, (y0 + y1) / 2
        c["fx"], c["fy"] = (c["cx"] - left) / (right - left), (c["cy"] - top) / (bottom - top)
    boxes.sort(key=lambda c: (c["box"][1], c["box"][0]))
    return boxes


def enlarge(rows: list[list[int]], frame: tuple[int, int, int, int], scale: int,
            pad: int = 4) -> list[list[tuple[int, int, int]]]:
    """The frame's region, `pad` pixels around it, enlarged `scale` times (nearest neighbour),
    as RGB rows for `write_png` (D-003: review images are written under /tmp only)."""
    left, top, right, bottom = frame
    out = []
    for y in range(max(0, top - pad), min(len(rows), bottom + pad + 1)):
        line = []
        for x in range(max(0, left - pad), min(len(rows[0]), right + pad + 1)):
            v = rows[y][x]
            line.extend([(v, v, v)] * scale)
        out.extend([line] * scale)
    return out


def parse_frame(text: str) -> tuple[int, int, int, int]:
    """`L,T,R,B` from the command line into a frame; the lines must make a box (D-003).

    Two chapter 5 figures (Blockage 9, Buridan 12) print frame lines too faint for
    `find_frame`'s profile, and were given by hand from the profile's darkest candidates
    (`docs/problems.md` 7).
    """
    parts = [int(v) for v in text.split(",")]
    if len(parts) != 4 or not (parts[0] < parts[2] and parts[1] < parts[3]):
        raise ValueError(f"--frame wants L,T,R,B with L < R and T < B, not {text!r}")
    return parts[0], parts[1], parts[2], parts[3]


def main(argv: list[str] | None = None) -> int:
    """Print a figure's frame and components; optionally write an enlarged copy (D-003)."""
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("image", help="a PNG extracted with pdfimages, under /tmp")
    ap.add_argument("--scale", type=int, default=0, help="enlarge the frame N times")
    ap.add_argument("--out", help="where to write the enlarged frame (under /tmp)")
    ap.add_argument("--dark", type=int, default=DARK, help="ink threshold (grey level)")
    ap.add_argument("--min-pixels", type=int, default=3, help="hide smaller clusters")
    ap.add_argument("--merge", type=int, default=0,
                    help="merge clusters within N pixels of each other (icons from fragments)")
    ap.add_argument("--whole", action="store_true",
                    help="measure the whole image (no frame search; the palette)")
    ap.add_argument("--frame", metavar="L,T,R,B",
                    help="the frame's lines by hand, when a line is too faint for the profile "
                         "(the darkest columns and rows of the image's left part, read off a dump)")
    args = ap.parse_args(argv)
    width, height, rows = read_png(args.image)
    if args.frame:
        frame = parse_frame(args.frame)
    elif args.whole:
        frame = (-1, -1, width, height)
    else:
        frame = find_frame(rows, args.dark)
    left, top, right, bottom = frame
    print(f"{args.image}: {width} x {height} px; frame left={left} top={top} right={right} "
          f"bottom={bottom} (inner {right - left - 1} x {bottom - top - 1} px)")
    print(f"{'#':>3} {'minx':>5} {'miny':>5} {'maxx':>5} {'maxy':>5} {'w':>3} {'h':>3} "
          f"{'px':>4} {'cx':>7} {'cy':>7} {'fx':>6} {'fy':>6}")
    found = components(rows, frame, args.dark, margin=0 if args.whole else 2)
    if args.merge:
        found = merge_close(found, frame, args.merge)
    for i, c in enumerate(found):
        if c["pixels"] < args.min_pixels:
            continue
        x0, y0, x1, y1 = c["box"]
        print(f"{i:>3} {x0:>5} {y0:>5} {x1:>5} {y1:>5} {x1 - x0 + 1:>3} {y1 - y0 + 1:>3} "
              f"{c['pixels']:>4} {c['cx']:>7.1f} {c['cy']:>7.1f} {c['fx']:>6.3f} {c['fy']:>6.3f}")
    if args.scale and args.out:
        write_png(args.out, enlarge(rows, frame, args.scale))
        print(f"wrote {args.out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
