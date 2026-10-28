"""Smoke test of the pyglet client: open the window on a virtual display, step the run with
the structures drawn, and check that the window changed nothing in the engine.

Needs pyglet (pip install -e '.[gui]') and Xvfb; skipped cleanly without either. The test
starts its own Xvfb on display :93 and never touches the real screen.
"""
import os
import shutil
import subprocess
import sys
import time

import pytest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, ROOT)

DISPLAY = ":93"
STEPS = 400


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


def test_window_runs_and_draws(xvfb):
    import pyglet
    from copycat.run import Run
    from clients.opengl import Window

    # the same problem and seed, with and without the window
    reference = Run("abc", "abd", "iijjkk", 4)
    for _ in range(STEPS):
        reference.step()

    run = Run("abc", "abd", "iijjkk", 4)
    window = Window(run)
    try:
        window.playing = True
        window.speed = 10 ** 9            # one engine step per update
        window.switch_to()
        for _ in range(STEPS):
            window.update(1.0)
            window.dispatch_events()
        window.on_draw()
        window.flip()

        # the structures layer drew something: bonds, groups or correspondences exist by
        # now on this seed, and each has at least one shape or label
        drawn = window.workspace.structures.items
        assert len(drawn) > 0
        kinds = {kind for kind, _, _, _ in window.workspace.structures.current()}
        assert kinds & {"bond", "group", "corr"}
        assert window.timer.text == str(STEPS // run.timestep)

        # the GUI only observes: same codelet count, temperature and structures as headless
        assert run.coderack.time == reference.coderack.time
        assert run.workspace.temperature == reference.workspace.temperature
        assert (len(run.workspace.bonds()), len(run.workspace.groups()),
                len(run.workspace.correspondences())) == \
               (len(reference.workspace.bonds()), len(reference.workspace.groups()),
                len(reference.workspace.correspondences()))
    finally:
        window.close()


def test_runs_to_an_answer_with_the_window(xvfb):
    from copycat.run import Run
    from clients.opengl import Window

    run = Run("abc", "abd", "ijk", 1)
    window = Window(run)
    try:
        window.playing = True
        window.speed = 10 ** 9
        window.switch_to()
        for _ in range(5000):
            window.update(1.0)
            if window.done:
                break
        assert window.done
        assert run.workspace.answer_string.name == "ijl"
        assert window.rule.text.startswith("Replace ")
        assert window.workspace.translated_label.text.startswith("Replace ")
        # the answer string is drawn once, as the fourth string
        assert len(window.workspace.strings) == 4
    finally:
        window.close()
