"""Headless runs: the oracle's chez_scheme/oracle/run.ss, in Python.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
Translated to Python (2026) from the oracle's prelude.ss (install-headless-windows!)
and run.ss (the driver), with racket/headless.rkt as a worked translation.

- `install_headless_windows()`: prelude.ss's null windows, plus the Racket port's
  headless Commentary window (commentary-graphics.ss's make-comment-window logic
  without its text window), which prints each paragraph as run.ss's
  $commentary-hook does.
- `run_problem(strings, seed, max_codelets, keep_going, trace_port)`: run.ss's
  driver.  It prints the Problem line, the commentary, the answers and the
  summary to sys.stdout, as the oracle does, and returns (reason, answers).
  break and quiet-break are replaced by `headless_break`, which ends the run
  (StopRun) or, with keep-going, returns at once as (go) would;
  report-error-and-halt prints "Ooops: ..." and ends the run.

A run changes the engine for good (the Memory and the codelet count outlive it:
anomalies_and_quirks.md, "The Memory outlives a run"), and the oracle runs one
problem per process; so run one problem per process, or per fork of a process
where `prepare()` has run.
"""
from __future__ import annotations

import sys

import metacat as _metacat
from metacat import chez, engine, run, setup, trace_writer
from metacat.objects import Lambda, SchemeObject, tell


# ---------------------------------------------------------------------------
# Headless windows (prelude.ss: install-headless-windows!)

def make_null_window(wname, messages):
    """prelude.ss: make-null-window"""
    def fn(self, msg, *args):
        if msg in messages:
            return "done"
        raise chez.SchemeError("headless-window", "~s received unexpected message ~s",
                               wname, msg)
    return Lambda(fn)


class HeadlessCommentWindow(SchemeObject):
    """commentary-graphics.ss: make-comment-window's closure, with its text window
    replaced by the oracle's recording text window (prelude.ss), as the Racket
    port's headless.rkt has it: each add-comment draws one paragraph, the eliza
    or the non-eliza one, and the trace records it."""

    def __init__(this):
        this.eliza_paragraphs = []
        this.non_eliza_paragraphs = []

    def otherwise(this, self, msg, args):
        if msg == "object-type":
            return "comment-window"
        if msg == "new-problem":
            initial_sym, modified_sym, target_sym, answer_sym = args
            f = chez.format_
            if setup.p_justify_mode is not False:
                lines1 = [f('Let\'s see... "~a" changes to "~a", and', initial_sym, modified_sym),
                          f(' "~a" changes to "~a".  Hmm...', target_sym, answer_sym)]
                lines2 = [f('Beginning justify run:  "~a" changes to "~a", and',
                            initial_sym, modified_sym),
                          f(' "~a" changes to "~a"...', target_sym, answer_sym)]
            else:
                lines1 = [f('Okay, if "~a" changes to "~a", what', initial_sym, modified_sym),
                          f(' does "~a" change to?  Hmm...', target_sym)]
                lines2 = [f('Beginning run:  If "~a" changes to "~a", what',
                            initial_sym, modified_sym),
                          f(' does "~a" change to?', target_sym)]
            tell(self, "add-comment", lines1, lines2)
            return "done"
        if msg == "add-comment":
            lines1, lines2 = args
            paragraph1 = "".join(lines1)
            paragraph2 = "".join(lines2)
            this.eliza_paragraphs = [1, paragraph1] + this.eliza_paragraphs
            this.non_eliza_paragraphs = [1, paragraph2] + this.non_eliza_paragraphs
            paragraph = paragraph1 if setup.p_eliza_mode is not False else paragraph2
            # the oracle's $commentary-hook, set by run.ss and wrapped by trace.ss
            trace_writer.emit("comment", ("text", paragraph))
            chez.printf("Comment: ~a~%", paragraph)
            return "done"
        if msg == "clear":
            this.eliza_paragraphs = []
            this.non_eliza_paragraphs = []
            return "done"
        if msg == "initialize":
            tell(self, "clear")
            return "done"
        raise chez.SchemeError("headless-window",
                               "comment window received unexpected message ~s", msg)


VERBOSE = False


def _memory_window_fn(self, msg, *args):
    # add-memory-icon gives each answer or snag description its icon drawing
    # procedures (memory-graphics.ss), which memory.ss calls even when nothing
    # is displayed; here they draw nothing.
    if msg == "add-memory-icon":
        tell(args[0], "set-graphics-info", lambda activation: "no-icon", "no-icon")
        return "done"
    if msg == "draw":
        return "done"
    raise chez.SchemeError("headless-window", "~s received unexpected message ~s",
                           "memory", msg)


def _control_panel_fn(self, msg, *args):
    if msg == "set-verbose-step-mode":
        # as in gui.ss, with the verbose checkbox off unless --verbose
        setup.p_verbose = args[0] if args[0] is not False else VERBOSE
        return "done"
    raise chez.SchemeError("headless-window",
                           "control panel received unexpected message ~s", msg)


def install_headless_windows():
    """prelude.ss: install-headless-windows!"""
    s = engine.set_global
    s("%workspace-graphics%", False)
    s("%slipnet-graphics%", False)
    s("%coderack-graphics%", False)
    s("*workspace-window*", make_null_window("workspace", ("garbage-collect", "caching-on",
                                                           "flush")))
    s("*slipnet-window*", make_null_window("slipnet", ("clear",)))
    s("*coderack-window*", make_null_window("coderack", ("clear",)))
    s("*themespace-window*", make_null_window(
        "themespace", ("erase-all-themes", "update-thematic-pressure", "update-graphics",
                       "set-theme-graphics-parameters-and-draw", "garbage-collect")))
    s("*top-themes-window*", make_null_window("top-themes", ()))
    s("*bottom-themes-window*", make_null_window("bottom-themes", ()))
    s("*vertical-themes-window*", make_null_window("vertical-themes", ()))
    s("*memory-window*", Lambda(_memory_window_fn))
    s("*trace-window*", make_null_window("trace", ("initialize", "add-event")))
    s("*temperature-window*", make_null_window("temperature", ("initialize",
                                                               "update-graphics")))
    # port: plot-current-values too, which the model sends with workspace
    # graphics on (a run with only the Workspace window attached, as
    # racket/headless.rkt's null windows accept it)
    s("*EEG-window*", make_null_window("EEG", ("initialize", "plot-current-values")))
    # Each codelet type keeps its own reference to the Coderack window, set by the
    # window (coderack-graphics.ss); a codelet's 'run tells it
    # 'set-last-codelet-type whatever the graphics switches say.
    coderack_graphics = make_null_window("coderack-graphics", ("set-last-codelet-type",))
    for t in _metacat.coderack.g_codelet_types:
        tell(t, "set-graphics-parameters", coderack_graphics,
             False, False, False, False, False, False, False, False)
    s("*control-panel*", Lambda(_control_panel_fn))
    s("*comment-window*", HeadlessCommentWindow())


# ---------------------------------------------------------------------------
# The driver (chez_scheme/oracle/run.ss)

class StopRun(Exception):
    """run.ss's stop-run continuation, called with the reason."""

    def __init__(self, reason):
        super().__init__(reason)
        self.reason = reason


ANSWERS: list = []
KEEP_GOING = [False]


def headless_break():
    """chez_scheme/oracle/run.ss: headless-break"""
    run.g_running_p = False
    at_cap = run.g_break_time is not False and run.g_break_time == setup.g_codelet_count
    if KEEP_GOING[0] and not at_cap:
        run.g_running_p = True
        return "ignore"
    raise StopRun("cap" if at_cap else "suspend")


def on_answer(answer_event):
    """chez_scheme/oracle/run.ss's wrapper of abstract-answer-description"""
    answer = tell(tell(answer_event, "get-answer-string"), "print-name")
    quality = tell(answer_event, "get-quality")
    ANSWERS.append(answer)
    chez.printf("Answer: ~a  quality ~a  codelet ~a  temperature ~a~%",
                answer, quality, setup.g_codelet_count, setup.g_temperature)


def on_halt(message, obj):
    """chez_scheme/oracle/run.ss's report-error-and-halt"""
    chez.printf('Ooops: bad message "~a" sent to object of type ~a~%',
                message[1], tell(obj, "object-type"))
    raise StopRun("halt")


_prepared = False


def prepare():
    """Load the engine, install the headless windows and the trace wrappers, and
    replace break.  Once per process (fork after it)."""
    global _prepared
    if _prepared:
        return
    _prepared = True
    engine.load()
    install_headless_windows()
    trace_writer.on_answer = on_answer
    trace_writer.on_halt = on_halt
    trace_writer.install_trace()
    run.break_ = headless_break
    run.quiet_break = headless_break


def wrap_comment_window():
    """racket/headless.rkt's install-recorders! for the Commentary: a wrapper of
    the views' Commentary window that prints and emits each paragraph, as
    HeadlessCommentWindow does.  The wrapper is the window's self, so the
    window's own (tell self 'add-comment ...) in new-problem is recorded too."""
    window = setup.g_comment_window

    def comment_window_fn(self, msg, *args):
        if msg == "add-comment":
            lines1, lines2 = args
            paragraph = "".join(lines1 if setup.p_eliza_mode is not False else lines2)
            trace_writer.emit("comment", ("text", paragraph))
            chez.printf("Comment: ~a~%", paragraph)
        return window(self, msg, *args)

    setup.g_comment_window = Lambda(comment_window_fn)


_recorded = []


def install_recorders():
    """racket/headless.rkt: install-recorders!, around windows the views
    installed after prepare(); the headless windows record by themselves."""
    if (not isinstance(setup.g_comment_window, HeadlessCommentWindow)
            and setup.g_comment_window not in _recorded):
        wrap_comment_window()
        _recorded.append(setup.g_comment_window)
    if setup.g_trace_window not in _recorded:
        _recorded.append(trace_writer.wrap_trace_window())


def run_problem(strings, seed, max_codelets=False, keep_going=False, trace_port=None,
                verbose=False, views=None):
    """chez_scheme/oracle/run.ss's driver, after its argument checks: strings are
    3 or 4 symbols, seed a valid seed, max_codelets a positive integer or #f.
    Prints what run.ss prints and returns (reason, answers).  An error of the
    run propagates (the trace written so far is flushed).  views, if given, is
    called before the run to attach windows (racket/headless.rkt's #:views),
    e.g. metacat.gui.views.attach_views."""
    global VERBOSE
    prepare()
    if views is not None:
        views()
        install_recorders()
    VERBOSE = verbose
    setup.p_verbose = verbose
    ANSWERS.clear()
    KEEP_GOING[0] = keep_going
    trace_writer.PORT = trace_port
    try:
        trace_writer.trace_start(strings, seed, max_codelets, keep_going)
        initial, modified, target = strings[:3]
        answer = strings[3] if len(strings) == 4 else False
        setup.p_justify_mode = answer is not False
        chez.printf("Problem: ~a -> ~a; ~a -> ~a  seed ~a~%",
                    initial, modified, target, "?" if answer is False else answer, seed)
        try:
            run.init_mcat(initial, modified, target, answer, seed)
            run.g_break_time = max_codelets
            run.run_mcat()
        except StopRun as stop:
            reason = stop.reason
        trace_writer.trace_end(reason, ANSWERS)
    finally:
        if trace_port is not None:
            trace_port.flush()
        trace_writer.PORT = None
    chez.printf("Stopped: ~a~%", reason)
    chez.printf("Codelets: ~a~%", setup.g_codelet_count)
    chez.printf("Temperature: ~a~%", setup.g_temperature)
    chez.printf("Answers: ~a~%", "none" if not ANSWERS else list(ANSWERS))
    sys.stdout.flush()
    return reason, list(ANSWERS)
