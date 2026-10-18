"""The determinism contract: `run(problem, config, seed, window=False) -> dict` (TASK.md;
the loop's gate calls it on six cases: the four golden fingerprints and one run under each
of the other profiles; G-009).

The dict has `records`, the engine's events as `tabletop.events.record_of` dicts in order,
taken from the GUI's own history (the event log the tap fills); `draws`, the generator's
draw count at the end (`Result.draws`, D-064); and `report`, French's report line
(`tabletop.run.report_line` of the run's result [F95 p. 145]). With `window=False` the
run goes through the Qt-free run controller on its worker thread, waited for here, so the
call is synchronous: this module must not import PySide6. With `window=True` the real
`MainWindow` is built offscreen with `settings=None`, the run started through the
window's own run controller with `run_to_end()`, every event delivered to the GUI thread
and drawn before it is acknowledged, and the result is what the window recorded (its
history, the tap's log) (G-009).

A display only observes: the tap and the gate read the run and never change it, which is
what this contract lets the loop's gate check on every iteration (ANSWERS.md 6).
"""

from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.run_controller import RunController
from tabletop.gui.models.tap import Tap
from tabletop.run import report_line

__all__ = ["run", "run_through_controller"]


def run(problem, config, seed, window=False) -> dict:
    """Run a problem through the GUI's observers and return what they recorded (TASK.md's
    determinism contract; Numbo GUI, `test_run_controller.py`'s plain runs). `problem` is
    a problem name or a `Problem`, `config` a `Config` (G-009)."""
    if window:
        return _run_through_window(problem, config, seed)
    return run_through_controller(problem, config, seed)


def run_through_controller(problem, config, seed) -> dict:
    """The run through `Tap`, `EventLog` and the Qt-free `RunController`, with
    `run_to_end()`, waited for (G-009; Seek-Whence GUI, `headless.py`)."""
    log = EventLog()
    tap = Tap(log)
    controller = RunController(observers=[tap], on_world=tap.attach, on_step=tap.after_step)
    controller.start_run(problem, config, seed)
    controller.run_to_end()
    controller.wait()
    outcome = controller.outcome
    if outcome.error is not None:
        raise outcome.error
    return {"records": list(log.records), "draws": outcome.result.draws,
            "report": report_line(outcome.result)}


def _run_through_window(problem, config, seed) -> dict:
    """The same run through the real `MainWindow`, offscreen, with `settings=None` and
    the package's default layout: started with `start_run`, run with `run_to_end()`, the
    Qt event loop turned until the window has shown the outcome (Numbo GUI,
    `test_gui_shell.py`'s runs through the window; Seek-Whence GUI, `headless.py`;
    G-009)."""
    import os
    os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
    from PySide6.QtCore import QEventLoop, QTimer
    from PySide6.QtWidgets import QApplication
    from tabletop.gui.main_window import MainWindow
    app = QApplication.instance() or QApplication([])
    window = MainWindow(settings=None)
    window.show()
    app.processEvents()
    loop = QEventLoop()
    window.run_finished.connect(lambda outcome: loop.quit())
    poll = QTimer()
    poll.setInterval(20)
    poll.timeout.connect(lambda: loop.quit() if window.outcome is not None else None)
    try:
        window.start_run(problem, config, seed)
        window.controller.run_to_end()
        poll.start()
        while window.outcome is None:
            loop.exec()
        poll.stop()
        outcome = window.outcome
        if outcome.error is not None:
            raise outcome.error
        return {"records": list(window.history.records), "draws": outcome.result.draws,
                "report": report_line(outcome.result)}
    finally:
        poll.stop()
        window.close()
        app.processEvents()
