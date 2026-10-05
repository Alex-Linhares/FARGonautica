"""run.ss: the run loop (init-mcat, run-mcat, step-mcat, update-everything ...)
and the REPL commands that drive it (ss, runtil, break, go, rerun ...).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from run.ss, with
racket/engine/run.rktl as a worked translation.

One function per definition, under the mapped names.  The globals are module
attributes (`*break-time*` is `g_break_time` ...); the rest of the engine reads
them through the package (_metacat.run.suspend, .g_temperature_clamped_p ...), so
a driver may replace `break_`, `quiet_break` or `suspend`, as the oracle's run.ss
does with set!.

break and go (docs/python-translation-plan.md, "Continuations").  break captures
a continuation, which may be deep inside a codelet (suspend, answers.ss), then
calls (reset), which returns to the REPL; go calls that continuation, so the run
goes on from the break as if break had returned 'ignore.  Python has no
re-entrant continuations, so a command that may break runs in an engine thread
(`toplevel(thunk)`, the REPL's evaluation of one command): at the break, (reset)
parks that thread and `toplevel` returns to its caller; `go` hands control back to
the parked thread and waits, as the REPL waits, until the run breaks again or the
command ends.  Only one thread runs model code at a time.  Outside `toplevel`
(the headless drivers, which replace break anyway) a break's (reset) raises
objects.Reset, as Chez's reset ends a `scheme --script` run.

Evaluation order: init-workspace's let makes the strings last first (Chez's let);
nothing in them draws.  update-everything's stochastic-if* draws its coin first.
"""
from __future__ import annotations

import queue
import sys
import threading

import metacat as _metacat
from metacat import chez, coderack, formulas, objects, setup, slipnet, sugar, utilities
from metacat import workspace, workspace_strings
from metacat.objects import tell

p_update_cycle_length = 15
p_initial_slipnode_clamp_cycles = 50
p_garbage_collect_cycles = 100

g_this_run = False
g_running_p = False
g_interrupt_p = False
g_breakpoint_continuation = False
g_break_time = False
g_step_mode_p = False
p_step_cycles = 1
g_display_mode_p = False
# created by init-mcat's set! in the original (no define)
g_temperature_clamped_p = False
g_initial_slipnode_unclamp_time = False


# ---------------------------------------------------------------------------
# The REPL's side of break and go (no counterpart in run.ss: Chez's call/cc and
# reset)

class _Abandoned(BaseException):
    """Unwinds a parked engine thread whose breakpoint can no longer be resumed
    (a new run replaced it); no model code catches it."""


_engine = threading.local()   # .holder: [the queue of the command waiting for it]


def toplevel(thunk):
    """The REPL evaluating one command: run thunk in an engine thread until it
    returns (its value), raises (re-raised here) or resets (None: a break, or
    report-error-and-halt).  A break parks the thread; go resumes it."""
    if getattr(_engine, "holder", None) is not None:
        return thunk()   # already in an engine thread: a nested command
    back = queue.SimpleQueue()
    holder = [back]

    def body():
        _engine.holder = holder
        try:
            result = ("value", thunk())
        except objects.Reset:
            result = ("reset", None)
        except _Abandoned:
            return
        except BaseException as e:   # noqa: BLE001 - re-raised by the waiting command
            result = ("error", e)
        holder[0].put(result)

    threading.Thread(target=body, daemon=True, name="metacat-engine").start()
    return _wait(back)


def _wait(back):
    kind, value = back.get()
    if kind == "error":
        raise value
    return value


class Breakpoint:
    """break's continuation: calling it with a value resumes the parked run, which
    sees break return that value; the caller waits as toplevel does."""

    def __init__(self, holder):
        self.holder = holder           # the parked thread's holder, or None
        self.resume = threading.Event()
        self.value = None
        self.abandoned = False
        self.parked = False

    def __call__(self, value=None):
        if not self.parked or self.resume.is_set():
            # chez: a continuation can be re-entered; a thread cannot
            raise chez.SchemeError("go", "this break can no longer be resumed")
        back = queue.SimpleQueue()
        self.holder[0] = back
        self.value = value
        self.resume.set()
        return _wait(back)

    def abandon(self):
        if self.parked and not self.resume.is_set():
            self.abandoned = True
            self.resume.set()


def _breakpoint_point(body):
    """break's (continuation-point* breakpoint ... (reset)): body receives the
    Breakpoint and ends with (reset), which raises objects.Reset.  In an engine
    thread the thread parks there and, once resumed, break returns the value given
    to the continuation; elsewhere Reset propagates."""
    holder = getattr(_engine, "holder", None)
    point = Breakpoint(holder)
    try:
        return body(point)
    except objects.Reset:
        if holder is None:
            raise
        point.parked = True
        holder[0].put(("reset", None))
        point.resume.wait()
        if point.abandoned:
            raise _Abandoned() from None
        return point.value


def _drop_breakpoint():
    """Setting *breakpoint-continuation* to #f leaves the parked run, if any, to the
    garbage collector in Chez; here its thread is unwound."""
    global g_breakpoint_continuation
    if isinstance(g_breakpoint_continuation, Breakpoint):
        g_breakpoint_continuation.abandon()
    g_breakpoint_continuation = False


def reset():
    """Chez: (reset), back to the REPL (parks an engine thread at a break)."""
    raise objects.Reset()


# ---------------------------------------------------------------------------
# run.ss

def ss(*args):
    """run.ss: ss (step mode: (ss n) breaks every n codelets, (ss 0) turns it off)"""
    global p_step_cycles
    if args:
        n = args[0]
        if n > 0:
            p_step_cycles = n
            step_mode_on()
        else:
            step_mode_off()
    chez.printf("step mode ~a~a~%",
                "on" if g_step_mode_p is not False else "off",
                chez.format_(", step size ~a", p_step_cycles) if g_step_mode_p is not False
                else "")


def step_mode_on():
    """run.ss: step-mode-on"""
    global g_step_mode_p
    g_step_mode_p = True
    return tell(setup.g_control_panel, "set-verbose-step-mode", not p_step_cycles > 10)


def step_mode_off():
    """run.ss: step-mode-off"""
    global g_step_mode_p
    g_step_mode_p = False
    return tell(setup.g_control_panel, "set-verbose-step-mode", False)


def runtil(*args):
    """run.ss: runtil (sets or clears the breakpoint *break-time*)"""
    global g_break_time
    if not args:
        if g_break_time is not False:
            return ["breaktime", "set", "at", g_break_time]
        return ["no", "breaktime", "set"]
    breaktime = args[0]
    if breaktime == 0:
        g_break_time = False
        tell(setup.g_control_panel, "clear-breakpoint-message")
        return ["breaktime", "cleared"]
    g_break_time = breaktime
    tell(setup.g_control_panel, "display-breakpoint-message")
    return ["breaktime", "set", "at", g_break_time]


def clear_breakpoint():
    """run.ss: clear-breakpoint"""
    _drop_breakpoint()
    return "done"


# prompt and no-prompt: Marshall's workaround for SWL 0.9u's REPL prompts.
# swl:version is "0.9x" (the oracle's prelude.ss), so break never uses them.
prompt = False   # run.ss: (waiter-prompt-and-read), the REPL's prompt procedure


def no_prompt(n):
    """run.ss: no-prompt (SWL 0.9u's REPL read without a prompt; never used, since
    swl:version is not "0.9u")"""
    if not (isinstance(n, int) and not isinstance(n, bool) and n >= 0):
        raise chez.SchemeError("no-prompt", "~s is not a nonnegative exact integer", n)
    line = sys.stdin.readline()
    return line if line else None


def break_():
    """run.ss: break.  Stops the run until go (see the module docstring)."""
    def body(breakpoint):
        global g_breakpoint_continuation, g_running_p
        g_breakpoint_continuation = breakpoint
        # (swl:sync-display): nothing to synchronize without the views
        chez.printf("stopped~%")
        # swl:version is "0.9x": waiter-prompt-and-read no-prompt is not called
        g_running_p = False
        tell(setup.g_control_panel, "switch-to-input-mode")
        return reset()
    return _breakpoint_point(body)


def quiet_break():
    """run.ss: quiet-break.  break without "stopped"."""
    def body(breakpoint):
        global g_breakpoint_continuation, g_running_p
        g_breakpoint_continuation = breakpoint
        g_running_p = False
        tell(setup.g_control_panel, "switch-to-input-mode")
        return reset()
    return _breakpoint_point(body)


def go():
    """run.ss: go.  Resumes the run stopped by the last break, and waits until it
    breaks again (see the module docstring)."""
    global g_interrupt_p, g_running_p
    if not utilities.exists_p(g_breakpoint_continuation):
        chez.printf("No previous break.~%")
        return None
    tell(setup.g_control_panel, "switch-to-run-mode")
    g_interrupt_p = False
    g_running_p = True
    if g_display_mode_p is not False:
        _metacat.view_globals.restore_current_state()
    return g_breakpoint_continuation("ignore")


def suspend():
    """run.ss: suspend"""
    chez.printf("Type (go) or click on the Workspace to continue...~%")
    return _metacat.run.break_()


def rerun():
    """run.ss: rerun"""
    if not utilities.exists_p(g_this_run):
        chez.printf("No current problem.~%")
        return None
    return tell(setup.g_control_panel, "run-new-problem", g_this_run)


def run_mcat():
    """run.ss: run-mcat (the terminate escape is never taken).  break_ and
    update_everything are read through the package, so that a driver can replace
    them."""
    while True:
        step_mcat()
        if setup.g_codelet_count == g_initial_slipnode_unclamp_time:
            sugar.say("Unclamping initially-clamped slipnodes...")
            for node in slipnet.g_initially_clamped_slipnodes:
                tell(node, "unfreeze")
        if tell(coderack.g_coderack, "empty?") is not False:
            post_initial_codelets()
            clamp_initial_slipnodes()
        if chez.modulo(setup.g_codelet_count, p_update_cycle_length) == 0:
            _metacat.run.update_everything()
        if (g_interrupt_p is not False
                or (g_step_mode_p is not False
                    and chez.modulo(setup.g_codelet_count, p_step_cycles) == 0)
                or (g_break_time is not False and g_break_time == setup.g_codelet_count)):
            update_all_graphics()
            chez.printf("Codelets run: ~a~n", setup.g_codelet_count)
            _metacat.run.break_()
        if chez.modulo(setup.g_codelet_count, p_garbage_collect_cycles) == 0:
            tell(setup.g_themespace_window, "garbage-collect")
            tell(setup.g_workspace_window, "garbage-collect")


def step_mcat():
    """run.ss: step-mcat"""
    codelet = tell(coderack.g_coderack, "choose-codelet")
    tell(codelet, "run")
    setup.g_codelet_count = 1 + setup.g_codelet_count
    return "done"


def init_mcat(initial_sym, modified_sym, target_sym, answer_sym, seed):
    """run.ss: init-mcat"""
    global g_breakpoint_continuation, g_interrupt_p, g_running_p, g_display_mode_p
    global g_this_run, g_initial_slipnode_unclamp_time, g_temperature_clamped_p
    _drop_breakpoint()   # (set! *breakpoint-continuation* #f)
    g_interrupt_p = False
    g_running_p = True
    g_display_mode_p = False
    step_mode_off()
    chez.random_seed(seed)
    if setup.p_justify_mode is not False:
        g_this_run = [initial_sym, modified_sym, target_sym, answer_sym, seed]
    else:
        g_this_run = [initial_sym, modified_sym, target_sym, seed]
    tell(coderack.g_coderack, "initialize")
    setup.g_codelet_count = 0
    g_initial_slipnode_unclamp_time = 0
    setup.g_temperature = 100
    g_temperature_clamped_p = False
    for node in slipnet.g_slipnet_nodes:
        tell(node, "reset")
    _metacat.view_globals.g_fg_color = _metacat.view_globals.p_default_fg_color
    tell(setup.g_temperature_window, "initialize")
    tell(setup.g_temperature_window, "update-graphics", 100)
    tell(setup.g_EEG_window, "initialize")
    tell(setup.g_slipnet_window, "clear")
    tell(setup.g_coderack_window, "clear")
    tell(setup.g_comment_window, "clear")
    tell(_metacat.trace.g_trace, "initialize")
    tell(_metacat.memory.g_memory, "clear-activations")
    tell(_metacat.memory.g_memory, "unhighlight-all-answers")
    tell(_metacat.themes.g_themespace, "initialize")
    init_workspace(initial_sym, modified_sym, target_sym, answer_sym)
    add_string_position_descriptions_to_letters(workspace.g_initial_string)
    add_string_position_descriptions_to_letters(workspace.g_modified_string)
    add_string_position_descriptions_to_letters(workspace.g_target_string)
    if setup.p_justify_mode is not False:
        add_string_position_descriptions_to_letters(workspace.g_answer_string)
    if (tell(workspace.g_initial_string, "get-length") == 1
            or tell(workspace.g_modified_string, "get-length") == 1
            or tell(workspace.g_target_string, "get-length") == 1
            or (setup.p_justify_mode is not False
                and tell(workspace.g_answer_string, "get-length") == 1)):
        tell(slipnet.plato_object_category, "set-activation", slipnet.p_max_activation)
    for obj in tell(workspace.g_workspace, "get-objects"):
        for descriptor in utilities.tell_all(tell(obj, "get-descriptions"), "get-descriptor"):
            tell(descriptor, "set-activation", slipnet.p_max_activation)
    update_workspace_values()
    clamp_initial_slipnodes()
    if setup.p_slipnet_graphics is not False:
        tell(setup.g_slipnet_window, "update-graphics")
    post_initial_codelets()
    if setup.p_coderack_graphics is not False:
        tell(setup.g_coderack_window, "update-graphics")
    # (collect 4): a garbage collection, nothing to do


def init_workspace(initial_sym, modified_sym, target_sym, answer_sym):
    """run.ss: init-workspace"""
    mws = workspace_strings.make_workspace_string
    # chez: let inits last first; none of them draws
    answer_string = mws("answer", answer_sym) if setup.p_justify_mode is not False else False
    target_string = mws("target", target_sym)
    modified_string = mws("modified", modified_sym)
    initial_string = mws("initial", initial_sym)
    tell(workspace.g_workspace, "initialize",
         initial_string, modified_string, target_string, answer_string)
    workspace.g_initial_string = initial_string
    workspace.g_modified_string = modified_string
    workspace.g_target_string = target_string
    workspace.g_answer_string = answer_string
    # Order of strings is important:
    workspace.g_top_strings = [initial_string, modified_string]
    workspace.g_bottom_strings = [target_string, answer_string]
    workspace.g_vertical_strings = [initial_string, target_string]
    workspace.g_non_answer_strings = [initial_string, modified_string, target_string]
    workspace.g_all_strings = [initial_string, modified_string, target_string, answer_string]
    if setup.p_workspace_graphics is not False:
        tell(setup.g_workspace_window, "draw-problem",
             initial_string, modified_string, target_string, answer_string)
    return tell(setup.g_comment_window, "new-problem",
                initial_sym, modified_sym, target_sym, answer_sym)


def clamp_initial_slipnodes():
    """run.ss: clamp-initial-slipnodes"""
    global g_initial_slipnode_unclamp_time
    for node in slipnet.g_initially_clamped_slipnodes:
        tell(node, "clamp", slipnet.p_max_activation)
    g_initial_slipnode_unclamp_time = (setup.g_codelet_count
                                       + p_initial_slipnode_clamp_cycles * p_update_cycle_length)


def post_initial_codelets():
    """run.ss: post-initial-codelets"""
    for _ in range(2 * len(tell(workspace.g_workspace, "get-objects"))):
        tell(coderack.g_coderack, "add-deferred-codelet",
             tell(coderack.bottom_up_bond_scout, "make-codelet", coderack.p_very_low_urgency))
        tell(coderack.g_coderack, "add-deferred-codelet",
             tell(coderack.bottom_up_bridge_scout, "make-codelet", coderack.p_very_low_urgency))
    return tell(coderack.g_coderack, "post-deferred-codelets")


def add_string_position_descriptions_to_letters(string):
    """run.ss: add-string-position-descriptions-to-letters"""
    string_length = tell(string, "get-length")
    leftmost_letter = tell(string, "get-letter", 0)
    if string_length == 1:
        return tell(leftmost_letter, "new-description",
                    slipnet.plato_string_position_category, slipnet.plato_single)
    rightmost_letter = tell(string, "get-letter", string_length - 1)
    tell(leftmost_letter, "new-description",
         slipnet.plato_string_position_category, slipnet.plato_leftmost)
    tell(rightmost_letter, "new-description",
         slipnet.plato_string_position_category, slipnet.plato_rightmost)
    if string_length % 2 == 1:
        middle_letter = tell(string, "get-letter", string_length // 2)
        return tell(middle_letter, "new-description",
                    slipnet.plato_string_position_category, slipnet.plato_middle)
    return None


def update_everything():
    """run.ss: update-everything"""
    tell(workspace.g_workspace, "check-if-rules-possible")
    update_workspace_values()
    trace = _metacat.trace.g_trace
    if tell(trace, "within-snag-period?") is not False:
        progress_achieved = tell(trace, "progress-since-last-snag")
        # chez: the coin first (stochastic-if*)
        sugar.stochastic_if_star(lambda: utilities.percent(progress_achieved),
                                 lambda: tell(trace, "undo-snag-condition"))
    if tell(trace, "clamp-period-expired?") is not False:
        tell(trace, "undo-last-clamp")
    tell(workspace.g_workspace, "spread-activation-to-themespace")
    tell(_metacat.themes.g_themespace, "spread-activation")
    slipnet.update_slipnet_activations()
    formulas.update_temperature()
    coderack.add_bottom_up_codelets()
    coderack.add_top_down_codelets()
    tell(coderack.g_coderack, "post-deferred-codelets")
    if setup.p_workspace_graphics is not False:
        tell(_metacat.eeg_graphics.g_EEG, "record-current-values")
        tell(setup.g_EEG_window, "plot-current-values")
    return update_all_graphics()


def update_workspace_values():
    """run.ss: update-workspace-values"""
    ws = workspace.g_workspace
    for structure in tell(ws, "get-structures"):
        tell(structure, "update-strength")
    objects = tell(ws, "get-objects")
    for obj in objects:
        tell(obj, "update-raw-importance")
    tell(workspace.g_initial_string, "update-all-relative-importances")
    tell(workspace.g_modified_string, "update-all-relative-importances")
    tell(workspace.g_target_string, "update-all-relative-importances")
    if setup.p_justify_mode is not False:
        tell(workspace.g_answer_string, "update-all-relative-importances")
    for obj in objects:
        tell(obj, "update-object-values")
    tell(workspace.g_initial_string, "update-average-intra-string-unhappiness")
    tell(workspace.g_modified_string, "update-average-intra-string-unhappiness")
    tell(workspace.g_target_string, "update-average-intra-string-unhappiness")
    if setup.p_justify_mode is not False:
        tell(workspace.g_answer_string, "update-average-intra-string-unhappiness")
    return tell(ws, "update-average-unhappiness-values")


def update_all_graphics():
    """run.ss: update-all-graphics"""
    if setup.p_workspace_graphics is not False:
        tell(setup.g_workspace_window, "update-graphics")
        tell(setup.g_temperature_window, "update-graphics", setup.g_temperature)
    if setup.p_slipnet_graphics is not False:
        tell(setup.g_slipnet_window, "update-graphics")
    if setup.p_coderack_graphics is not False:
        tell(setup.g_coderack_window, "update-graphics")
