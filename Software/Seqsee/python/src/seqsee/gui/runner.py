"""The model runner: Seqsee runs on a worker thread and talks to the GUI thread through Qt
signals.

Perl runs the model inside Tk's event loop: a button or key calls one of Seqsee.pl's
``Interaction_*`` subs, which calls ``Seqsee::Interaction_step_n``, and ``update_display``
(UI/Graphical.pm) redraws the canvas every ``update_after`` steps. Questions are modal Tk
widgets that wait inside the codelet that asks them. Here:

- Commands (``step``, ``step_n``, ``crawl``, ``continue_``, ``new_sequence``, ``call``) are
  queued from the GUI thread and run one at a time on the worker thread. The run commands are
  Seqsee.pl's ``Interaction_step`` / ``Interaction_step_n`` / ``Interaction_crawl`` /
  ``Interaction_continue``: the same ``n``, ``update_after`` and ``InterstepSleep``.
- ``pause`` is GUI_sparse.conf's Pause: ``$Global::Break_Loop = 1``. ``Interaction_step_n``
  clears Break_Loop before every step, so the runner also wraps ``seqsee_main.seqsee_step``
  to set it again after the step while a pause is pending; a pause can't fall between two
  steps and be lost. A pause also drops the run commands still queued.
- ``update_display`` (the model's hooks in seqsee_main, sworkspace, all_mx and
  user_interaction) takes a ``snapshot.take()`` on the worker thread and emits it with
  ``snapshot``, at most once per ``min_interval`` seconds (~30 per second). Every command ends
  with a snapshot, and one is sent before every question, so the GUI always ends up showing the
  final state and the state being asked about (e.g. the hilit objects).
- Responsiveness: the worker and the GUI thread share the GIL, and every Qt virtual call on a
  Python-made scene or item takes it, so a GUI thread drawing while the worker steps waits
  for the GIL again and again (frames of hundreds of ms, a frozen event loop). With
  ``pace_frames`` (the window sets it) the step after a snapshot waits, at most
  ``FRAME_WAIT``, until the GUI calls ``frame_drawn``; and the GIL switch interval is
  ``SWITCH_INTERVAL`` while the runner runs. ``snapshot_times`` keeps what the last
  snapshots cost. Only timing changes: the model takes the same steps.
- Questions: ``user_interaction.install`` gets ``boolean_response``
  (MessageRequiringBooleanResponse; UI/Graphical's ask_user_extension asks through it) and
  ``response`` (MessageRequiringAResponse); ``all_mx._ask_for_more_terms`` is
  SGUI::ask_for_more_terms + waitWindow. Each sends a ``Question`` with ``question`` and blocks
  the worker until ``answer(question, value)`` is called from the GUI. A more-terms answer is
  the text typed: it is split on commas and spaces (``seqentry.split_terms``) and inserted
  into the workspace, as the Perl Entry's <Return> handler does (an insert error asks again,
  as the Tk window stays open); None (window closed) inserts nothing.
- ``list_action`` is a list popup's button (SGUI::List::CreatePopupWidget): the action
  (``listactions``) runs on the worker on the live object behind a snapshot tag; every
  snapshot sent keeps its live objects by tag (``snapshot.take(live=)``), for the last
  ``LIVE_SNAPSHOTS``.
- ``accept_sequence`` is SGUI::ask_seq's accepted input: workspace, coderack and stream
  cleared and the terms inserted, without a restart (before the first sequence, INITIALIZE
  runs first). ``Question.parts``
  are the Tk insert arguments the Commentary shows for it (``gui.commentary``).
- UI/Graphical.pm's ``main::message($msg, $no_break)``: the model modules' ``_message`` hooks
  (``MESSAGE_HOOK_MODULES``). With $no_break, the message is sent with ``message(parts)`` and
  the model goes on; without, it is a ``response`` question with the single choice
  'continue' (MessageRequiringAResponse), so the model waits, as in Perl.
- A model exception ends the command and is sent with ``error(message, traceback)``; the
  runner keeps going (Perl's default_error_handler shows a tkdie dialog).
- ``new_sequence`` starts a fresh run, as ``cli.run_headless`` does: ``s.reset_all()``,
  ``read_config``, ``srand(seed)`` and Seqsee.pl's INITIALIZE. Called during a run, it pauses
  the run and cancels a pending question first. ``defaults`` (the entry point's command line)
  fills the read_config options it isn't given and turns the -f features on again after
  reset_all.

The GUI thread never touches model objects: it only receives snapshots and questions. The
hooks are module attributes of the model (looked up at call time), installed by ``start`` and
restored by ``quit``; headless behaviour is unchanged.
"""
import collections
import contextlib
import io
import sys
import threading
import time
import traceback

from PySide6.QtCore import QObject, Signal

from seqsee import global_ as Global
from seqsee import mapping, s, sanity, scodelet_base, scoderack, seqsee_main, sworkspace, \
    user_interaction, util
from seqsee.categories import alternating
from seqsee.codelets import all_mx, large_gp
from seqsee.codelets import ui as ui_codelets
from seqsee.gui import commentary, listactions, seqentry, snapshot
from seqsee.scripts import describe_solution

RUN_COMMANDS = ("step", "step_n", "crawl", "continue")
MORE_TERMS_TEXT = seqentry.MORE_TERMS_PROMPT
LIVE_SNAPSHOTS = 16     # list_action can name an object of one of the last 16 snapshots
SWITCH_INTERVAL = 0.001  # sys.setswitchinterval while the runner runs (Python's is 0.005)
FRAME_WAIT = 0.25       # with pace_frames: the longest a step waits for the last frame

# (module, attribute) of every model hook the runner replaces.
_HOOKS = (
    (seqsee_main, "seqsee_step"),
    (seqsee_main, "_update_display"),
    (sworkspace, "_update_display"),
    (all_mx, "_update_display"),
    (all_mx, "_ask_for_more_terms"),
    (user_interaction, "_update_display"),
    (user_interaction, "boolean_response"),
    (user_interaction, "response"),
)
# The modules whose ``_message`` is main::message (sstream2 goes through scodelet_base's).
MESSAGE_HOOK_MODULES = (all_mx, large_gp, ui_codelets, describe_solution, sanity, mapping,
                        alternating, scodelet_base)
_HOOKS += tuple((mod, "_message") for mod in MESSAGE_HOOK_MODULES)


class Question:
    """A question from the model, waiting for ``Runner.answer``.

    ``kind``: ``"boolean"`` (yes/no; answer 1/0/None), ``"response"`` (answer one of
    ``choices``) or ``"more_terms"`` (answer the typed text, or None). ``extra`` holds the
    other arguments of MessageRequiringBooleanResponse (``'', $msg_suffix, ['debug']``).
    ``cancelled`` is set if the runner gave up waiting (new sequence, quit). ``parts``: what
    the Commentary inserts for it (Tk insert arguments; default ``(text,)``)."""

    def __init__(self, kind, text, choices=(), extra=(), parts=None):
        self.kind = kind
        self.text = text
        self.choices = tuple(choices)
        self.extra = tuple(extra)
        self.parts = tuple(parts) if parts is not None else (text,)
        self.value = None
        self.cancelled = False
        self._event = threading.Event()

    @property
    def answered(self):
        return self._event.is_set()

    def __repr__(self):
        return f"Question({self.kind!r}, {self.text!r})"


class Runner(QObject):
    """Runs Seqsee on a worker thread. Create it on the GUI thread, connect the signals,
    then ``start()``; ``quit()`` stops it and restores the model's hooks."""

    snapshot = Signal(object)             # a snapshot.Snapshot
    question = Signal(object)             # a Question; call answer() to unblock the model
    question_closed = Signal(object)      # a Question cancelled before it was answered
    message = Signal(object)              # main::message($msg, 1): Tk insert arguments
    error = Signal(str, str)              # message, formatted traceback
    command_done = Signal(str, object)    # command name, its result
    state_changed = Signal(str)           # "idle", "running", "waiting", "stopped"

    def __init__(self, min_interval=1 / 30, parent=None, defaults=None):
        super().__init__(parent)
        self.min_interval = min_interval
        # Seqsee.pl's command line (the entry point, gui.app): read_config options (e.g.
        # ``update_interval``, ``view``) for a fresh run that doesn't give them, and
        # ``features`` (the -f features, which reset_all clears) turned on again in each.
        self.defaults = dict(defaults or {})
        self.options = None               # the run's options (read_config), once a sequence is set
        self.max_steps = None             # set_max_steps: overrides options["max_steps"]
        self._cond = threading.Condition()
        self._commands = collections.deque()
        self._pause = threading.Event()
        self._pending = None
        self._quitting = False
        self._thread = None
        self._saved_hooks = None
        self._last_snapshot = float("-inf")
        self.snapshot_times = collections.deque(maxlen=100)  # seconds per snapshot.take()
        # pace_frames (set by the window): after sending a snapshot, the next step waits
        # (at most FRAME_WAIT) until the GUI calls frame_drawn().
        self.pace_frames = False
        self._frame_drawn = threading.Event()
        self._frame_drawn.set()
        # id(snapshot) -> (snapshot, its live objects by tag), for the last few sent
        self._live = collections.OrderedDict()
        self._state = "idle"

    # ---- GUI-thread API -----------------------------------------------------------------
    def start(self):
        """Install the hooks and start the worker thread."""
        if self._thread is not None:
            return
        self._saved_hooks = [(mod, name, getattr(mod, name)) for mod, name in _HOOKS]
        self._orig_step = seqsee_main.seqsee_step
        self._saved_switch_interval = sys.getswitchinterval()
        sys.setswitchinterval(SWITCH_INTERVAL)
        self._install_hooks()
        self._thread = threading.Thread(target=self._work, name="seqsee-runner", daemon=True)
        self._thread.start()

    def is_alive(self):
        return self._thread is not None and self._thread.is_alive()

    @property
    def pending_question(self):
        return self._pending

    @property
    def state(self):
        return self._state

    def step(self):
        """Seqsee.pl Interaction_step: one step, display after it."""
        self._enqueue("step")

    def step_n(self, n):
        """Seqsee.pl Interaction_step_n({n => $n}): n steps, display at the end."""
        self._enqueue("step_n", n)

    def crawl(self, ms):
        """Seqsee.pl Interaction_crawl($ms): InterstepSleep = ms, run to max_steps, display
        after every step."""
        self._enqueue("crawl", ms)

    def continue_(self):
        """Seqsee.pl Interaction_continue: InterstepSleep = 0, run to max_steps, display every
        update_interval steps."""
        self._enqueue("continue")

    def call(self, fn, *args, **kwargs):
        """Run ``fn(*args, **kwargs)`` on the worker thread (with deep-recursion headroom);
        its result comes back with ``command_done("call", result)``. For GUI actions that
        change the model (e.g. the list popups)."""
        self._enqueue("call", fn, args, kwargs)

    def list_action(self, snap, part, action, key):
        """A list popup's button (SGUI::List::CreatePopupWidget): run ``action`` of list
        ``part`` (``listactions``) on the worker, on the live object drawn with tag ``key``
        in ``snap`` (one of the last snapshots sent); main::message goes through the usual
        hook. The command's closing snapshot is Tk::Seqsee::Update."""
        self._enqueue("list_action", snap, part, action, key)

    def new_sequence(self, seq, seed=None, max_steps=None, update_interval=None):
        """Start a fresh run on ``seq`` (a string of integers, or a list). Pauses a running
        command and cancels a pending question first. Drops a ``set_max_steps`` limit: the
        run's ``max_steps`` option holds."""
        self._interrupt()
        self.max_steps = None
        self._enqueue("new_sequence", seq, seed, max_steps, update_interval)

    def accept_sequence(self, terms, seed=None, max_steps=None):
        """SGUI::ask_seq's accepted input (``seqentry.check_sequence``'s terms): clear the
        workspace, coderack and stream and insert ``terms``. Not a restart: the steps, the
        random state and the rest of the model go on. Before the first sequence, Seqsee.pl's
        INITIALIZE (without a sequence, with ``seed`` if given) runs first. ``max_steps``, if
        given, is ``set_max_steps``. Pauses a running command and cancels a pending question
        first, like ``new_sequence``."""
        self._interrupt()
        if max_steps is not None:
            self.max_steps = max_steps
        self._enqueue("accept_sequence", list(terms), seed)

    def set_max_steps(self, n):
        """The window's max-steps field: the following run commands stop at ``n`` steps
        instead of the run's ``max_steps`` option (None: the option again). A running
        command keeps the limit it started with."""
        self.max_steps = n

    def pause(self):
        """GUI_sparse.conf Pause: ``$Global::Break_Loop = 1``; queued runs are dropped."""
        with self._cond:
            self._pause.set()
            Global.Break_Loop = 1
            self._commands = collections.deque(
                c for c in self._commands if c[0] not in RUN_COMMANDS)

    def set_interstep_sleep(self, ms):
        """The [Scale]'s ``-variable => \\$Global::InterstepSleep``: takes effect at the next
        step, also in a running crawl. (One attribute write, read by the worker between steps,
        as Tk writes it between steps in Perl.)"""
        Global.InterstepSleep = ms

    def set_debug_max(self, value):
        """GUI_sparse.conf's m binding writes ``$Global::debugMAX`` (``1 - $debugMAX``); like
        ``set_interstep_sleep``, a single write the model reads when it next looks."""
        Global.debugMAX = value

    def frame_drawn(self):
        """The GUI has drawn the snapshots sent so far: with ``pace_frames``, the model goes
        on."""
        self._frame_drawn.set()

    def answer(self, question, value):
        """Answer a question (from the GUI thread); the worker goes on."""
        if question.answered:
            return
        question.value = value
        question._event.set()

    def quit(self, timeout=10):
        """Stop the worker (pausing the run and cancelling any question) and restore the
        model's hooks. Returns True if the worker has stopped. Idempotent."""
        if self._thread is None:
            return True
        with self._cond:
            self._quitting = True
        self._interrupt()
        with self._cond:
            self._commands.clear()
            self._commands.append(("quit",))
            self._cond.notify()
        self._thread.join(timeout)
        if self._thread.is_alive():
            return False
        if self._saved_hooks is not None:
            for mod, name, value in self._saved_hooks:
                setattr(mod, name, value)
            self._saved_hooks = None
            sys.setswitchinterval(self._saved_switch_interval)
        return True

    # ---- internals ----------------------------------------------------------------------
    def _enqueue(self, *command):
        with self._cond:
            if self._quitting:
                return
            self._commands.append(command)
            self._cond.notify()

    def _interrupt(self):
        """Pause, and cancel the pending question (if any)."""
        self.pause()
        self._frame_drawn.set()
        q = self._pending
        if q is not None and not q.answered:
            q.cancelled = True
            self.answer(q, None)
            self.question_closed.emit(q)

    def _set_state(self, state):
        if state != self._state:
            self._state = state
            self.state_changed.emit(state)

    def _install_hooks(self):
        seqsee_main.seqsee_step = self._seqsee_step
        for mod in (seqsee_main, sworkspace, all_mx, user_interaction):
            mod._update_display = self._update_display
        all_mx._ask_for_more_terms = self._ask_for_more_terms
        user_interaction.install(boolean_response=self._boolean_response,
                                 response=self._response)
        for mod in MESSAGE_HOOK_MODULES:
            mod._message = self._message

    def _work(self):
        while True:
            with self._cond:
                while not self._commands:
                    self._cond.wait()
                command = self._commands.popleft()
                if command[0] in RUN_COMMANDS:
                    self._pause.clear()
            name = command[0]
            if name == "quit":
                self._set_state("stopped")
                return
            self._set_state("running")
            result = None
            try:
                result = getattr(self, "_do_" + name)(*command[1:])
            except Exception as e:  # noqa: BLE001 - shown in the GUI; the runner goes on
                self.error.emit(str(e).split("\n")[0] or type(e).__name__,
                                traceback.format_exc())
            self._send_snapshot(force=True)
            with self._cond:
                if not self._commands:
                    self._set_state("idle")
            self.command_done.emit(name, result)

    def _need_options(self):
        if self.options is None:
            raise RuntimeError("No sequence: start a new sequence first")
        max_steps = self.max_steps
        if max_steps is not None:
            return dict(self.options, max_steps=max_steps)
        return self.options

    def _do_step(self):
        options = self._need_options()
        return seqsee_main.interaction_step_n(
            {"n": 1, "update_after": 1, "max_steps": options["max_steps"]})

    def _do_step_n(self, n):
        options = self._need_options()
        return seqsee_main.interaction_step_n({"n": n, "max_steps": options["max_steps"]})

    def _do_crawl(self, ms):
        options = self._need_options()
        Global.InterstepSleep = ms
        return seqsee_main.interaction_step_n(
            {"n": options["max_steps"], "update_after": 1, "max_steps": options["max_steps"]})

    def _do_continue(self):
        options = self._need_options()
        Global.InterstepSleep = 0
        return seqsee_main.interaction_step_n(
            {"n": options["max_steps"], "update_after": options["update_interval"],
             "max_steps": options["max_steps"]})

    def _do_call(self, fn, args, kwargs):
        return util.call_with_deep_stack(fn, *args, **kwargs)

    def _do_list_action(self, snap, part, action, key):
        _, live = self._live.get(id(snap), (None, {}))
        item = live.get(key) if key is not None else None
        if item is None:
            raise RuntimeError(f"{action}: the item is no longer in the workspace")
        return util.call_with_deep_stack(listactions.run, part, action, item, self._message)

    def _do_accept_sequence(self, terms, seed=None):
        if self.options is None:
            self._do_new_sequence("", seed, None, None, allow_empty=True)
        # $check_and_accept_input_sequence (lib/SGUI.pm); its Update() is the snapshot that
        # ends every command.
        sworkspace.clear()
        scoderack.clear()
        Global.MainStream.clear()
        sworkspace.insert_elements(*terms)
        return terms

    def _do_new_sequence(self, seq, seed, max_steps, update_interval, allow_empty=False):
        from seqsee import cli
        self.options = None
        s.load()
        s.reset_all()
        self._install_hooks()  # reset_all restored the default answer callbacks
        features = self.defaults.get("features") or {}
        Global.Feature.update(features)
        if "debugMAX" in features:     # as _read_commandline does
            Global.debugMAX = 1
        if not isinstance(seq, str):
            seq = " ".join(util.perl_str(x) for x in seq)
        given = {"seq": seq}
        for key, value in (("seed", seed), ("max_steps", max_steps),
                           ("update_interval", update_interval)):
            if value is not None:
                given[key] = value
        for key, value in self.defaults.items():
            if key != "features" and key not in given and value is not None:
                given[key] = value
        quiet = io.StringIO()  # "View: 1!", "Initializing Coderack..."
        with contextlib.redirect_stdout(quiet):
            options = seqsee_main.read_config(**given)
            if not options["seq"] and not allow_empty:
                raise ValueError('No sequence given: type one, e.g. "1 1 2 1 2 3"')
            Global.Options_ref = options
            util.srand(int(util.perl_num(options["seed"])))
            util.call_with_deep_stack(cli._initialize, options)
        self.options = options
        return options

    # ---- the model's hooks (worker thread) ----------------------------------------------
    def _seqsee_step(self):
        if not self._frame_drawn.is_set():
            self._frame_drawn.wait(FRAME_WAIT)
        result = self._orig_step()
        if self._pause.is_set():
            Global.Break_Loop = 1
        return result

    def _update_display(self, *args):
        self._send_snapshot()

    def _send_snapshot(self, force=False):
        now = time.monotonic()
        if not force and now - self._last_snapshot < self.min_interval:
            return
        self._last_snapshot = now
        try:
            live = {}
            snap = snapshot.take(live=live)
            self.snapshot_times.append(time.monotonic() - now)
            self._live[id(snap)] = (snap, live)
            while len(self._live) > LIVE_SNAPSHOTS:
                self._live.popitem(last=False)
        except Exception as e:  # noqa: BLE001 - a display problem must not kill the model
            self.error.emit(f"snapshot failed: {e}", traceback.format_exc())
            return
        if self.pace_frames:
            self._frame_drawn.clear()
        self.snapshot.emit(snap)

    def _ask(self, kind, text, choices=(), extra=(), parts=None):
        q = Question(kind, text, choices, extra, parts)
        with self._cond:
            if self._quitting:
                return None
            self._pending = q
        self._send_snapshot(force=True)
        self._set_state("waiting")
        self.question.emit(q)
        q._event.wait()
        with self._cond:
            self._pending = None
        self._set_state("running")
        return q.value

    def _boolean_response(self, question, *rest):
        return self._ask("boolean", util.perl_str(question), extra=rest,
                         parts=(question, *rest))

    def _response(self, choices, question):
        return self._ask("response", util.perl_str(question), choices=choices,
                         parts=(question,))

    def _message(self, msg, no_break=None, *rest):
        """UI/Graphical.pm's main::message."""
        parts, choices = commentary.message_request(msg, no_break)
        if choices is None:
            self.message.emit(parts)
            return None
        return self._ask("response", commentary.parts_text(parts), choices=choices,
                         parts=parts)

    def _ask_for_more_terms(self):
        """SGUI::ask_for_more_terms + waitWindow. The Entry's <Return> inserts the terms, even
        none (insert_elements still sets TimeOfLastNewElement), then Update() and destroy.
        If the insert dies (e.g. "a"), Tk reports the error and the window stays open (the
        terms before the bad one are in): here the error is sent and the question asked
        again."""
        while True:
            text = self._ask("more_terms", MORE_TERMS_TEXT)
            if text is None:
                return
            try:
                sworkspace.insert_elements(*seqentry.split_terms(str(text)))
            except Exception as e:  # noqa: BLE001 - Tk's background error; the window stays
                self.error.emit(str(e).split("\n")[0] or type(e).__name__,
                                traceback.format_exc())
                self._send_snapshot(force=True)
                continue
            self._send_snapshot(force=True)
            return
