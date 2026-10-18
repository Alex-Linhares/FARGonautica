"""The determinism contract: `run(config, seed, lines, answers, window=False) -> dict`
(TASK.md; the gate calls it for the three appendix runs and two Blackburn sequences,
[M86 pp. 164-179], [M86 pp. 6-7]).

The dict has `records`, the engine's events as `seekwhence.trace.event_record` dicts in
order, taken from the GUI's own history (the event log the tap fills); `draws`,
`world.rng.draws` at the end; and `outcome`, `Session.outcome`. With `window=False` the
run goes through the Qt-free run controller on its worker thread, waited for here, so the
call is synchronous: this module must not import PySide6. With `window=True` the real
`MainWindow` is built offscreen with `settings=None`, the scripted session runs through
the window's own run controller with `run_to_end()`, every event delivered to the GUI
thread and drawn before it is acknowledged, and the result is what the window recorded
(its history, the tap's log).

A display only observes: the tap and the gate read the run and
never change it, which is what this contract lets the loop's gate check on every
iteration.
"""

import io

from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.run_controller import RunController
from seekwhence.gui.models.tap import Tap
from seekwhence.trace import event_record


def run(config, seed, lines, answers, window=False) -> dict:
    """Run a scripted session through the GUI's observers and return what they recorded
    (TASK.md's determinism contract; Numbo GUI, `test_run_controller.py`'s plain runs).
    `lines` and `answers` mean what they mean for `Session`: scripted console lines,
    scripted answers, then guesses judged against the scripted terms (DECISION D-069)."""
    if window:
        return _run_through_window(config, seed, lines, answers)
    log = EventLog()
    tap = Tap(log)
    controller = RunController(observers=[tap], on_session=tap.attach_session)
    controller.start_session(config, seed, lines, answers, out=io.StringIO())
    controller.run_to_end()
    controller.wait()
    outcome = controller.outcome
    if outcome.error is not None:
        raise outcome.error
    return {"records": [event_record(e) for e in log.events],
            "draws": outcome.result["draws"], "outcome": outcome.result["outcome"]}


def _run_through_window(config, seed, lines, answers) -> dict:
    """The same run through the real MainWindow, offscreen, with `settings=None`: started
    with `start_session`, run with `run_to_end()`, the Qt event loop turned until the
    window has shown the outcome (Numbo GUI, `test_gui_shell.py`'s runs through the
    window; G-008)."""
    import os
    os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
    from PySide6.QtCore import QEventLoop, QTimer
    from PySide6.QtWidgets import QApplication
    from seekwhence.gui.main_window import MainWindow
    from seekwhence.gui.models.presets import custom_inputs
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
        window.start_session(custom_inputs(config, seed, lines, answers))
        window.controller.run_to_end()
        poll.start()
        while window.outcome is None:
            loop.exec()
        poll.stop()
        outcome = window.outcome
        if outcome.error is not None:
            raise outcome.error
        return {"records": [event_record(e) for e in window.history.events],
                "draws": outcome.result["draws"], "outcome": outcome.result["outcome"]}
    finally:
        poll.stop()
        window.close()
        app.processEvents()
