"""Smoke test of Hager's window over a replayed trace of Mitchell's program, on a virtual
display. Needs pyglet (pip install -e '.[gui]') and Xvfb; skipped cleanly without either.
Starts its own Xvfb on display :94 and never touches the real screen."""
import os
import shutil
import subprocess
import sys
import time

import pytest

from melanies_opengl import lisprun

HERE = os.path.dirname(os.path.abspath(__file__))
FIXTURE = os.path.join(HERE, "fixtures", "ijk-seed1.jsonl")
DISPLAY = ":94"


def _can_run():
    if sys.platform != "linux" or not shutil.which("Xvfb"):
        return False
    try:
        import pyglet  # noqa: F401
    except ImportError:
        return False
    return True


pytestmark = pytest.mark.skipif(not _can_run(), reason="needs pyglet and Xvfb")


@pytest.fixture(scope="module")
def xvfb():
    proc = subprocess.Popen(["Xvfb", DISPLAY, "-screen", "0", "1100x700x24", "+extension", "GLX"],
                            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(1.0)
    if proc.poll() is not None:
        pytest.skip("Xvfb would not start on %s" % DISPLAY)
    os.environ["DISPLAY"] = DISPLAY
    os.environ["WAYLAND_DISPLAY"] = ""
    os.environ.setdefault("LIBGL_ALWAYS_SOFTWARE", "1")
    yield
    proc.terminate()
    proc.wait(timeout=10)


def test_replay_in_the_window(xvfb):
    from melanies_opengl import gui
    run = lisprun.replay_run(FIXTURE)
    window = gui.MelaniesWindow(run)
    try:
        assert window.caption.startswith("Copycat: Melanie Mitchell's original")
        window.playing = True
        window.speed = 10 ** 9
        window.switch_to()
        for _ in range(200):
            window.update(1.0)
            window.dispatch_events()
            if window.done:
                break
        window.on_draw()
        window.flip()
        assert window.done
        assert run.answer == "ijl"
        assert window.rule.text == "Replace letter-category of rmost letter by successor"
        assert window.workspace.translated_label.text.startswith("Replace ")
        assert len(window.workspace.strings) == 4          # the answer was drawn
        kinds = {kind for kind, _, _, _ in window.workspace.structures.current()}
        assert kinds >= {"bond", "group", "corr"}
        assert len(window.workspace.structures.items) > 0
        assert window.timer.text == str(389 // run.timestep)
    finally:
        window.close()
