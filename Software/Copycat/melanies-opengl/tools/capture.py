"""Capture screenshots of a replayed trace in Hager's window, on whatever DISPLAY is set.

    DISPLAY=:94 python tools/capture.py TRACE.jsonl OUT-PREFIX N1 N2 ...

steps the run through the window and saves OUT-PREFIX-N.png when the codelet count first
reaches each N (and OUT-PREFIX-final.png at the answer). Quantized to 256 colours."""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))

from PIL import ImageGrab                       # noqa: E402
from melanies_opengl import lisprun, gui        # noqa: E402


def settle(window):
    """Let Hager's eased animations (node sizes, the background's temperature colour)
    converge, without stepping the run: 60 small frames with the speed set so low that
    update() never reaches a step."""
    speed, done = window.speed, window.done
    window.speed = 1e-9
    for _ in range(60):
        window.done = False          # update() returns early once the answer is in
        window.update(1 / 30.0)
    window.speed, window.done = speed, done


def grab(window, path):
    settle(window)
    window.switch_to()
    window.on_draw()
    window.flip()
    window.dispatch_events()
    x, y = window.get_location()
    w, h = window.get_size()
    im = ImageGrab.grab(xdisplay=os.environ.get("DISPLAY"), bbox=(x, y, x + w, y + h))
    im.convert("RGB").quantize(256).save(path)
    print("saved", path, im.size)


def main():
    trace, prefix = sys.argv[1], sys.argv[2]
    marks = [int(n) for n in sys.argv[3:]]
    run = lisprun.replay_run(trace)
    window = gui.MelaniesWindow(run)
    window.playing = True
    window.speed = 10 ** 9
    window.switch_to()
    window.dispatch_events()
    pending = list(marks)
    for _ in range(200000):
        window.update(1.0)
        window.dispatch_events()
        n = run.coderack.time
        while pending and n >= pending[0]:
            grab(window, "%s-%d.png" % (prefix, pending.pop(0)))
        if window.done or run.finished:
            break
    window.update(1.0)
    grab(window, "%s-final.png" % prefix)
    window.close()


if __name__ == "__main__":
    main()
