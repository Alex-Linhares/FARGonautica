#!/usr/bin/env python3
"""Render a problem file beside the figure it was digitized from, for review by eye (D-003).

    python3 tools/render_problem.py problems/surround-05.json:/tmp/tt/s/img-008.png \
        [more problem.json:figure.png pairs] --out /tmp/tt/review.png [--scale 3]

For each pair, one row of the output: on the left, the figure's table frame enlarged
`scale` times with every object's `box_px` outlined in its type's colour; on the right, the
layout drawn from the file alone (the 110 x 100 table at the same size, the 3 x 3 region
lines faint, each object as a filled rectangle of its footprint, D-002, in its type's
colour; the touched object with a thick black border, the answers with a red one). A file
without a figure (`problems/x.json` alone) gets only the right panel. The two panels must
show the same picture; `docs/problems.md` records the review of every file.

Standard library only; the PNG reading and writing is `measure_figure.py`'s. The output is
written under /tmp and never committed (the figures are copyrighted).
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

import measure_figure as mf

COLOURS = {  # one colour per type, printed as a legend
    "fork": (200, 0, 0), "knife": (0, 130, 0), "spoon": (0, 0, 210), "cup": (230, 120, 0),
    "saucer": (130, 0, 170), "plate": (0, 150, 150), "big glass": (90, 90, 90),
    "small glass": (240, 0, 170), "pepper shaker": (40, 40, 40), "salt shaker": (160, 160, 0),
    "soup bowl": (0, 90, 130),
}
FOOTPRINTS_PX = {  # the same boxes as tabletop/objects.py (D-002), pointing up
    "fork": (6, 17), "knife": (3, 17), "spoon": (6, 17), "cup": (14, 7), "saucer": (14, 13),
    "plate": (19, 19), "big glass": (11, 15), "small glass": (8, 10), "pepper shaker": (12, 15),
    "salt shaker": (12, 15), "soup bowl": (21, 12),
}
WHITE, BLACK, RED, GREY = (255, 255, 255), (0, 0, 0), (220, 0, 0), (200, 200, 200)


def blank(width: int, height: int, colour=WHITE) -> list[list[tuple[int, int, int]]]:
    return [[colour] * width for _ in range(height)]


def rect(img, x0: int, y0: int, x1: int, y1: int, colour, fill: bool = False,
         thickness: int = 1) -> None:
    """Draw a rectangle (outline of `thickness`, or filled) clipped to the image."""
    h, w = len(img), len(img[0])
    for y in range(max(0, y0), min(h, y1 + 1)):
        for x in range(max(0, x0), min(w, x1 + 1)):
            edge = (x - x0 < thickness or x1 - x < thickness
                    or y - y0 < thickness or y1 - y < thickness)
            if fill or edge:
                img[y][x] = colour


def left_panel(figure: Path, frame, objects: list[dict], scale: int, pad: int):
    """The figure's frame enlarged, with each object's pixel box outlined (D-003)."""
    _, _, rows = mf.read_png(figure)
    img = mf.enlarge(rows, frame, scale, pad)
    ox, oy = frame[0] - pad, frame[1] - pad
    for o in objects:
        x0, y0, x1, y1 = o["box_px"]
        rect(img, (x0 - ox) * scale - 1, (y0 - oy) * scale - 1,
             (x1 - ox + 1) * scale, (y1 - oy + 1) * scale, COLOURS[o["type"]])
    return img


def right_panel(problem: dict, scale: int, pad: int):
    """The layout drawn from the file alone: table, regions, footprints, markers (D-003)."""
    left, top, right, bottom = problem["table"]["frame_px"]
    fw, fh = (right - left + 2 * pad + 1) * scale, (bottom - top + 2 * pad + 1) * scale
    img = blank(fw, fh)
    tx0, ty0 = pad * scale, pad * scale
    tx1, ty1 = (right - left + pad) * scale, (bottom - top + pad) * scale
    width, height = problem["table"]["width"], problem["table"]["height"]

    def px(x: float, y: float) -> tuple[int, int]:
        return (round(tx0 + x / width * (tx1 - tx0)), round(ty0 + y / height * (ty1 - ty0)))

    for k in (1, 2):  # the nine regions [F95 p. 65], drawn faint
        rect(img, px(width * k / 3, 0)[0], ty0, px(width * k / 3, 0)[0], ty1, GREY)
        rect(img, tx0, px(0, height * k / 3)[1], tx1, px(0, height * k / 3)[1], GREY)
    rect(img, tx0, ty0, tx1, ty1, BLACK)
    answers = set(problem["answers"].values())
    for o in problem["objects"]:
        w_px, h_px = FOOTPRINTS_PX[o["type"]]
        if o["type"] in ("fork", "knife", "spoon") and o["orientation"] in ("left", "right"):
            w_px, h_px = h_px, w_px
        cx, cy = px(o["x"], o["y"])
        hw, hh = w_px * scale / 2, h_px * scale / 2
        rect(img, round(cx - hw), round(cy - hh), round(cx + hw), round(cy + hh),
             COLOURS[o["type"]], fill=True)
        if o["id"] == problem["touched"]:
            rect(img, round(cx - hw) - 3, round(cy - hh) - 3, round(cx + hw) + 3,
                 round(cy + hh) + 3, BLACK, thickness=2)
        if o["id"] in answers:
            rect(img, round(cx - hw) - 3, round(cy - hh) - 3, round(cx + hw) + 3,
                 round(cy + hh) + 3, RED, thickness=2)
    return img


def render_pair(problem_file: Path, figure: Path | None, scale: int, pad: int = 4):
    problem = json.loads(problem_file.read_text())
    right = right_panel(problem, scale, pad)
    if figure is None:
        return right
    left = left_panel(figure, problem["table"]["frame_px"], problem["objects"], scale, pad)
    height = max(len(left), len(right))
    gap = 10
    rows = []
    for y in range(height):
        l = left[y] if y < len(left) else [WHITE] * len(left[0])
        r = right[y] if y < len(right) else [WHITE] * len(right[0])
        rows.append(l + [WHITE] * gap + r)
    return rows


def stack(panels):
    """Panels one above the other, separated by a grey line, padded to the widest."""
    width = max(len(p[0]) for p in panels)
    out = []
    for p in panels:
        for row in p:
            out.append(row + [WHITE] * (width - len(row)))
        out.append([GREY] * width)
    return out


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("pairs", nargs="+", help="problem.json or problem.json:figure.png")
    ap.add_argument("--out", required=True, help="the PNG to write (under /tmp)")
    ap.add_argument("--scale", type=int, default=3)
    args = ap.parse_args(argv)
    panels = []
    for pair in args.pairs:
        problem_file, _, figure = pair.partition(":")
        panels.append(render_pair(Path(problem_file), Path(figure) if figure else None,
                                  args.scale))
        print(f"{problem_file}" + (f" beside {figure}" if figure else ""))
    mf.write_png(args.out, stack(panels))
    print(f"wrote {args.out}; legend: " + ", ".join(f"{t} {c}" for t, c in COLOURS.items())
          + "; touched = black border, answers = red border")
    return 0


if __name__ == "__main__":
    sys.exit(main())
