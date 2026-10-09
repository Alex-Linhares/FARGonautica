"""The main window (lib/Tk/Seqsee.pm's Populate, as lib/SGUI.pm's CreateWidgets builds it with
config/GUI_sparse.conf): a menu bar and the canvas showing one of the 11 composite views.

Perl's Tk::Seqsee widget is a Tk::Menustrip (View, Save, Help) above a fixed 780 × 450 canvas
(GUI_sparse.conf's ``-width``/``-height``; its ``background => white`` reaches the frame
option database only, so the canvas keeps Tk's default #D9D9D9, as the oracle shows).

- View: the 11 ``@ViewOptions`` in order. Choosing one is the menu callback: ``@Parts =
  @{ $vo->[1] }; SetupParts(); Update()``. SetupParts resets the lists' pages (PERL-QUIRK, see
  ``views``), so ``pages`` is emptied on every view change.
- Save: Perl's "as EPS" (``$Canvas->postscript``) becomes "as PNG..." and "as SVG...": the
  canvas as shown, at its current size, on its background.
- Help: Perl's "About..." has no action (Tk::Menustrip's default prints "[About...]"); here it
  opens an About box. Perl's "Help On..." (also without an action) is left out.
- Run (not in Perl's Menustrip), the toolbar, the key bindings, the InterstepSleep slider,
  the codelet count and the status bar's run labels: ``controls`` (GUI_sparse.conf's button
  frame and [bindings]). They call ``run_command``, which drives the attached runner.
- The Commentary (lib/Tk/SCommentary.pm, packed below the canvas by GUI_sparse.conf): a dock
  under the canvas, ``commentary``. The runner's messages are logged there and its questions
  answered there (``show_question``), with AttentionNeeded set while one waits; model errors
  are logged too.
- SGUI::ask_seq (the x binding) and SGUI::ask_for_more_terms (the runner's more-terms
  questions): non-modal windows from ``seqentry`` (``ask_seq``, ``ask_for_more_terms``).
- SGUI::List's bindings: button 1 on a list's page squares pages it; on a row it is
  ProcessClickOnItem (``process_click_on_item``: the list's "Actions for …" popup from
  ``listpopup``, and a pause, as ``$Global::Break_Loop = 1``); a popup button runs the action
  on the worker (``Runner.list_action``). The panes do the same with their own pages.
- Hover tooltips on workspace objects (not in Perl): ``tooltip_at`` (``hover.tooltip``).

The modern shell (not in Perl): the Panes menu shows each drawing module as a dock of its own
(``panes``; hidden at first), next to the fixed views; a second toolbar row holds Seqsee.pl's
``--seed`` and ``--max_steps`` as fields, and Run → Restart (a fresh run of the last sequence
with them). Given ``settings`` (a QSettings; ``default_settings()`` for the user's), the window
remembers its geometry, the toolbar and dock layout, the view and the fields on close and
restores them when built (the options, Seqsee.pl's command line, win over the settings).

Unlike the Tk canvas, the Qt canvas follows the window's size: a resize recomposes the view
(SetupParts' fractions of the new size) once per event-loop turn. The canvas is never scrolled
or scaled, so scene coordinates are canvas pixels, as in Tk.

The window draws only snapshots (``show_snapshot``; ``attach_runner`` connects a
``runner.Runner``'s snapshots, errors, state and finished commands). A drawing error is shown
in the status bar and the last good picture stays.

Responsiveness (not in Perl, whose model runs inside Tk's event loop): the runner's snapshots
go through ``queue_snapshot``, which keeps only the newest and draws it at the next turn of
the event loop, so snapshots that arrive faster than the window draws are dropped
(``frames_dropped``) rather than queued up. A question, a cancelled question and the end of
a command draw the pending snapshot first (``flush_snapshot``): the window shows the state
asked about, and the final one. ``frame_times`` holds the last frames' cost (compose +
render, window and visible panes); a frame of a 30-term workspace stays within
``FRAME_BUDGET``.
"""
import collections
import contextlib
import time
from pathlib import Path

import PySide6
from PySide6.QtCore import QByteArray, QRect, QRectF, QSettings, QSize, Qt, QTimer
from PySide6.QtGui import QAction, QActionGroup, QPainter
from PySide6.QtSvg import QSvgGenerator
from PySide6.QtWidgets import QFileDialog, QFrame, QGraphicsScene, QGraphicsView, \
    QMainWindow, QMessageBox, QToolTip

import seqsee
from seqsee import util
from seqsee.gui import commentary as cm
from seqsee.gui import hover, seqentry
from seqsee.gui.draw import coderack, lists, views

from . import commentary as qcommentary
from . import controls, listpopup, panes, render
from . import seqentry as qseqentry

CANVAS_WIDTH = 780              # GUI_sparse.conf [Seqsee] -width
CANVAS_HEIGHT = 450             # GUI_sparse.conf [Seqsee] -height
CANVAS_BACKGROUND = "#D9D9D9"   # Tk's default canvas background (oracle/gui_window.pl)
PNG_FILTER = "PNG image (*.png)"
SVG_FILTER = "SVG image (*.svg)"
_SUFFIXES = {PNG_FILTER: ".png", SVG_FILTER: ".svg"}
SETTINGS_VERSION = 1            # saveState/restoreState's version
FRAME_BUDGET = 0.050            # seconds per frame (snapshot + compose + render), 30 terms
FRAME_TIMES = 100               # how many frame costs ``frame_times`` keeps


def default_settings():
    """The user's settings (organization and application "Seqsee"), for the entry point;
    a MainWindow without ``settings`` remembers nothing."""
    return QSettings("Seqsee", "Seqsee")


def about_text():
    return (f"Seqsee {seqsee.__version__}\n\n"
            "Abhijit Mahabal's Seqsee: a model of how people see patterns in integer "
            "sequences and extend them (a Fluid Analogies Research Group architecture).\n\n"
            "Python port of the Perl original, with a Qt (PySide6 "
            f"{PySide6.__version__}) GUI that draws the views of the Perl/Tk GUI.")


class Canvas(QGraphicsView):
    """The Tk canvas: a scene in canvas pixels, top-left aligned, no scroll bars, no frame.
    Calls ``on_resize`` after each resize, ``on_click(x, y)`` on a button-1 press (Tk's
    '<1>') and sets its tooltip to ``tooltip_at(x, y)`` as the pointer moves."""

    def __init__(self, width=CANVAS_WIDTH, height=CANVAS_HEIGHT, on_resize=None, parent=None,
                 on_click=None, tooltip_at=None):
        super().__init__(parent)
        self.setScene(QGraphicsScene(self))
        self._hint = QSize(width, height)
        self._on_resize = on_resize
        self._on_click = on_click
        self._tooltip_at = tooltip_at
        self.setMouseTracking(True)
        self.setFrameShape(QFrame.NoFrame)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setRenderHints(QPainter.Antialiasing | QPainter.TextAntialiasing)
        self.setMinimumSize(120, 80)

    def sizeHint(self):
        return self._hint

    def size_px(self):
        vp = self.viewport()
        return vp.width(), vp.height()

    def resizeEvent(self, event):
        super().resizeEvent(event)
        if self._on_resize is not None:
            self._on_resize()

    def mousePressEvent(self, event):
        if event.button() == Qt.LeftButton and self._on_click is not None:
            p = self.mapToScene(event.position().toPoint())
            self._on_click(p.x(), p.y())
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event):
        if self._tooltip_at is not None:
            p = self.mapToScene(event.position().toPoint())
            text = self._tooltip_at(p.x(), p.y())
            if text != self.toolTip():
                self.setToolTip(text)
                self.viewport().setToolTip(text)    # the viewport gets the ToolTip events
                if not text:
                    QToolTip.hideText()
        super().mouseMoveEvent(event)


class MainWindow(QMainWindow):
    """The Seqsee window: View / Save / Help menus and the canvas."""

    def __init__(self, options=None, width=CANVAS_WIDTH, height=CANVAS_HEIGHT, settings=None,
                 parent=None):
        super().__init__(parent)
        self.setWindowTitle("Seqsee")
        self.options = dict(options or {})
        self.settings = settings
        self.view = views.initial_view(self.options)
        self.snapshot = None
        self.attention_needed = False
        self.pages = {}
        self.known_families = ()
        self.composition = None
        self.composed_size = None
        self.redraw_count = 0
        self.frame_times = collections.deque(maxlen=FRAME_TIMES)
        self.frames_dropped = 0         # snapshots replaced by a newer one before drawing
        self._queued_snapshot = None
        self._frame_timer = QTimer(self)
        self._frame_timer.setSingleShot(True)
        self._frame_timer.setInterval(0)
        self._frame_timer.timeout.connect(self.flush_snapshot)
        self.measure = render.QtMeasure()
        self.runner = None
        self.run_state = None           # the runner's state, or "paused" after a pause
        self._pause_requested = False
        self.interstep_sleep = 0        # $Global::InterstepSleep as the GUI last set it
        self.debug_max = 0              # $Global::debugMAX as the GUI last set it
        self.last_sequence = ""
        self.seq_dialog = None          # SGUI::ask_seq's window, once opened
        self.more_terms_dialog = None   # SGUI::ask_for_more_terms' window, once opened
        self._more_terms_question = None
        self._redraw_timer = QTimer(self)
        self._redraw_timer.setSingleShot(True)
        self._redraw_timer.setInterval(0)
        self._redraw_timer.timeout.connect(self.redraw)
        self.selected_items = {}        # list part -> (snapshot, tag): SelectedItem
        self.list_popups = listpopup.ListPopups(self)
        self.list_popups.chosen.connect(self._list_action)
        self.canvas = Canvas(width, height, on_resize=self._redraw_timer.start,
                             on_click=self.canvas_clicked, tooltip_at=self.tooltip_at)
        self.canvas.scene().setBackgroundBrush(render.qcolor(CANVAS_BACKGROUND))
        self.setCentralWidget(self.canvas)
        self.commentary_dock = qcommentary.CommentaryDock(self)
        self.commentary = self.commentary_dock.widget()
        self.addDockWidget(Qt.BottomDockWidgetArea, self.commentary_dock)
        self.commentary.answered.connect(self._answered)
        self.commentary.debug_clicked.connect(self._start_debug)
        self.pane_docks = panes.make_docks(self, Canvas, CANVAS_BACKGROUND)
        self._make_controls()
        self._make_menus()
        self.run_status = controls.RunStatus(self.statusBar())
        self.status_labels = self.run_status.labels
        # show() would cap the size at 2/3 of the screen; start with the whole canvas.
        self.resize(self.sizeHint())
        self.restore_settings()
        self._apply_run_options()
        self.redraw()

    # ---- menus ------------------------------------------------------------------------
    def _make_menus(self):
        bar = self.menuBar()
        view_menu = bar.addMenu("View")
        group = QActionGroup(self)
        group.setExclusive(True)
        self.view_actions = []
        for k, v in enumerate(views.VIEW_OPTIONS):
            action = QAction(v.title, self, checkable=True)
            action.setChecked(k == self.view)
            action.triggered.connect(lambda _=False, k=k: self.set_view(k))
            group.addAction(action)
            view_menu.addAction(action)
            self.view_actions.append(action)
        self.pane_menu = bar.addMenu("Panes")
        for pane in panes.PANES:
            self.pane_menu.addAction(self.pane_docks[pane.part].toggleViewAction())
        self.pane_menu.addSeparator()
        self.pane_menu.addAction(self.commentary_dock.toggleViewAction())
        run_menu = bar.addMenu("Run")
        controls.fill_run_menu(run_menu, self.run_actions)
        run_menu.addAction(self.restart_action)
        save_menu = bar.addMenu("Save")
        self.save_actions = []
        for label, flt in (("as PNG...", PNG_FILTER), ("as SVG...", SVG_FILTER)):
            action = QAction(label, self)
            action.triggered.connect(lambda _=False, flt=flt: self._ask_save(flt))
            save_menu.addAction(action)
            self.save_actions.append(action)
        help_menu = bar.addMenu("Help")
        self.about_action = QAction("About...", self)
        self.about_action.triggered.connect(self.about)
        help_menu.addAction(self.about_action)

    def _make_controls(self):
        self.run_actions, self.button_actions = controls.make_actions(self, self.run_command)
        self.sleep_widget = controls.SleepSlider()
        self.sleep_slider = self.sleep_widget.slider
        self.sleep_value_label = self.sleep_widget.value_label
        self.sleep_widget.moved.connect(self._sleep_moved)
        self.count_label = controls.CodeletCount()
        self.toolbar = self.addToolBar("Controls")
        self.toolbar.setObjectName("controls")
        self.count_action = controls.fill_toolbar(self.toolbar, self.run_actions,
                                                  self.button_actions, self.count_label)
        self.menuBar().setCornerWidget(self.sleep_widget, Qt.TopRightCorner)
        self.run_settings = controls.RunSettings()
        self.seed_edit = self.run_settings.seed_edit
        self.max_steps_box = self.run_settings.max_steps_box
        self.run_settings.max_steps_changed.connect(self._max_steps_changed)
        self.restart_action = QAction("Restart", self)
        self.restart_action.setToolTip(
            "A fresh run of the last sequence with this seed and max steps")
        self.restart_action.triggered.connect(self.restart)
        self.addToolBarBreak()          # its own row: the controls fill the 780 px width
        self.settings_toolbar = self.addToolBar("Run settings")
        self.settings_toolbar.setObjectName("run_settings")
        self.settings_toolbar.addWidget(self.run_settings)
        self.settings_toolbar.addAction(self.restart_action)

    def menu_titles(self):
        return [a.text() for a in self.menuBar().actions()]

    # ---- state ------------------------------------------------------------------------
    def canvas_size(self):
        return self.canvas.size_px()

    def set_view(self, view):
        """The View menu's callback: show ``view`` (index or title); the lists go back to
        page 0 (SetupParts)."""
        self.view = views.view_index(view)
        self.view_actions[self.view].setChecked(True)
        self.pages = {}
        self.redraw()

    def set_page(self, part, page):
        """Show list ``part`` at ``page`` in the current view."""
        self.pages[part] = page
        self.redraw()

    # ---- list interaction and hover (SGUI::List's bindings) ------------------------------
    def canvas_clicked(self, x, y):
        """Button 1 on the canvas at (x, y)."""
        self.handle_list_click(self.canvas.scene(), self.composition, self.pages,
                               self.set_page, x, y)

    def handle_list_click(self, scene, comp, pages, set_page, x, y):
        """SGUI::List's '<1>' bindings on the topmost item at (x, y) of ``scene`` (the
        window's canvas or a pane's, with its composition, pages and ``set_page(part,
        page)``): the page squares page the list; a row opens its popup."""
        click = listpopup.list_click(scene, x, y)
        if click is None:
            return
        if click.kind == "item":
            self.process_click_on_item(click.part, click.key)
            return
        state = comp.lists.get(click.part) if comp is not None else None
        page = state.page_number if state is not None else pages.get(click.part, 0)
        set_page(click.part, lists.page_after(click.kind, page))

    def process_click_on_item(self, part, key):
        """ProcessClickOnItem: remember the SelectedItem, show the list's popup (with the
        entry's details) and set $Global::Break_Loop (the run pauses)."""
        snap = self.snapshot
        self.selected_items[part] = (snap, key)
        details = hover.describe_entry(snap, part, key) if snap is not None else ""
        self.list_popups.popup(part).show_item(details)
        if self.runner is not None:
            self.run_command("pause")

    def _list_action(self, part, action):
        """A popup button: the action on the list's SelectedItem, on the model's thread."""
        snap, key = self.selected_items.get(part, (None, None))
        if self.runner is None:
            self.statusBar().showMessage("No model attached", 3000)
            return
        self.runner.list_action(snap, part, action, key)

    def tooltip_at(self, x, y):
        """The hover tooltip at (x, y) of the canvas ('' if no workspace object is there)."""
        if self.snapshot is None or self.composed_size is None:
            return ""
        w, h = self.composed_size
        try:
            return hover.tooltip(self.snapshot, self.view, w, h, x, y, measure=self.measure)
        except Exception:  # noqa: BLE001 - a hover must never break the window
            return ""

    def set_attention_needed(self, needed):
        """Tk::Seqsee's AttentionNeeded / AttentionNoLongerNeeded."""
        self.attention_needed = bool(needed)
        self.redraw()

    def show_snapshot(self, snap):
        """Draw ``snap`` now (the window and the visible panes); a queued older snapshot is
        dropped."""
        start = time.perf_counter()
        if self._queued_snapshot is not None and self._queued_snapshot is not snap:
            self.frames_dropped += 1
        self._queued_snapshot = None
        self._frame_timer.stop()
        self.snapshot = snap
        self.count_label.update_from(snap)
        self.run_status.show_snapshot(snap)
        self.redraw()
        for dock in self.pane_docks.values():
            dock.redraw()
        # Paint now, while the model waits for the frame (the runner's pace_frames), rather
        # than later in the event loop, fighting the worker for the GIL.
        for canvas in [self.canvas] + [d.canvas for d in self.pane_docks.values()]:
            if canvas.isVisible():
                canvas.viewport().repaint()
        self.frame_times.append(time.perf_counter() - start)
        frame_drawn = getattr(self.runner, "frame_drawn", None)
        if frame_drawn is not None:
            QTimer.singleShot(0, frame_drawn)

    def queue_snapshot(self, snap):
        """A snapshot from the runner: drawn at the next turn of the event loop, unless a
        newer one replaces it first."""
        if self._queued_snapshot is not None:
            self.frames_dropped += 1
        self._queued_snapshot = snap
        self._frame_timer.start()

    def flush_snapshot(self):
        """Draw the queued snapshot now, if any."""
        snap = self._queued_snapshot
        if snap is not None:
            self.show_snapshot(snap)

    def reset_run(self):
        """Forget the Coderack rows of the last run (SCoderack->clear empties
        %HistoryOfRunnable)."""
        self.known_families = ()

    def attach_runner(self, runner):
        """Show ``runner``'s snapshots and state; the controls drive it; its messages and
        questions go to the Commentary; its model errors to the Commentary and status bar."""
        self.runner = runner
        runner.snapshot.connect(self.queue_snapshot)
        runner.pace_frames = True       # each step waits until the last frame is drawn
        runner.error.connect(self._model_error)
        runner.message.connect(lambda parts: self.commentary.insert(*parts))
        runner.question.connect(self.show_question)
        runner.question_closed.connect(self.question_closed)
        runner.state_changed.connect(self._state_changed)
        runner.command_done.connect(self._command_done)
        self._show_state(runner.state)

    # ---- the commentary -------------------------------------------------------------
    def show_question(self, question):
        """MessageRequiringAResponse / MessageRequiringBooleanResponse: the Commentary shows
        the question and its buttons; AttentionNeeded until it is answered. (More-terms
        requests are SGUI::ask_for_more_terms' window: ``ask_for_more_terms``.)"""
        self.flush_snapshot()           # the state asked about (the hilit objects)
        if question.kind == "more_terms":
            self.ask_for_more_terms(question)
            return
        if question.kind not in ("boolean", "response"):
            return
        self.commentary.ask(question)
        self.set_attention_needed(True)

    def question_closed(self, question):
        """The runner cancelled ``question`` before it was answered."""
        self.flush_snapshot()
        if question is self._more_terms_question:
            self._more_terms_question = None
            self.more_terms_dialog.close_quietly()
            return
        if self.commentary.pending is question:
            self.commentary.close_question(question)
            self.set_attention_needed(False)

    def _answered(self, question, value):
        self.set_attention_needed(False)
        if self.runner is not None:
            self.runner.answer(question, value)

    def ask_for_more_terms(self, question):
        """SGUI::ask_for_more_terms (+ waitWindow): the "Request for more terms" window; the
        text typed at Return answers ``question`` (None if the window is closed)."""
        if self.more_terms_dialog is not None:
            self.more_terms_dialog.close_quietly()
        dialog = qseqentry.MoreTermsDialog(self)
        dialog.setWindowFlag(Qt.Window, True)
        self.more_terms_dialog = dialog
        self._more_terms_question = question
        dialog.entered.connect(lambda text: self._more_terms_answer(question, text))
        dialog.dismissed.connect(lambda: self._more_terms_answer(question, None))
        dialog.show()
        dialog.raise_()
        dialog.activateWindow()

    def _more_terms_answer(self, question, text):
        if self._more_terms_question is question:
            self._more_terms_question = None
        if self.runner is not None:
            self.runner.answer(question, text)

    def _start_debug(self):
        """SCommentary's "Start Debug": debugMAX = 1 - debugMAX, and say so."""
        self.run_command("debug_max")
        self.commentary.insert(*cm.debug_message(self.debug_max))

    def _model_error(self, message, tb=""):
        self.statusBar().showMessage(f"Model error: {message}")
        self.commentary.insert(f"Model error: {message}\n", [cm.ERROR_TAG])

    # ---- controls ---------------------------------------------------------------------
    def run_command(self, command, arg=None):
        """What a button, key binding or Run entry does (GUI_sparse.conf): ``step``,
        ``step_n`` (arg = n), ``crawl`` (arg = ms), ``continue``, ``pause``, ``new_sequence``,
        ``debug_max`` (arg = 1/0; None toggles), ``quit``."""
        if command == "quit":
            self.close()
            return
        if command == "new_sequence":
            self.ask_seq()
            return
        if command == "debug_max":
            self.debug_max = controls.toggled_debug_max(self.debug_max) if arg is None else arg
            self.run_actions["m"].setChecked(bool(self.debug_max))
        elif command == "crawl":            # Interaction_crawl: InterstepSleep = ms
            self._show_sleep(arg)
        elif command == "continue":         # Interaction_continue: InterstepSleep = 0
            self._show_sleep(0)
        if self.runner is None:
            self.statusBar().showMessage("No model attached", 3000)
            return
        if command == "pause":
            self._pause_requested = True
            self.runner.pause()
            return
        if command == "debug_max":
            self.runner.set_debug_max(self.debug_max)
            return
        self._pause_requested = False
        if command == "step":
            self.runner.step()
        elif command == "step_n":
            self.runner.step_n(arg)
        elif command == "crawl":
            self.runner.crawl(arg)
        elif command == "continue":
            self.runner.continue_()
        else:
            raise ValueError(f"unknown command {command!r}")

    def ask_seq(self):
        """The x binding (SGUI::ask_seq): open the "Seqsee Sequence Entry" window (or raise
        the open one) and log "New Sequence Started: " (PERL-QUIRK: without the sequence,
        see ``seqentry.NEW_SEQUENCE_MESSAGE``). Its accepted input goes to
        ``accept_sequence``. Not modal: a run goes on until the input is accepted."""
        if self.seq_dialog is not None and self.seq_dialog.isVisible():
            self.seq_dialog.raise_()
            self.seq_dialog.activateWindow()
            return
        dialog = qseqentry.SequenceDialog(parent=self)
        dialog.setWindowFlag(Qt.Window, True)
        dialog.accepted_terms.connect(self.accept_sequence)
        self.seq_dialog = dialog
        dialog.show()
        dialog.raise_()
        dialog.activateWindow()
        self.commentary.insert(*seqentry.NEW_SEQUENCE_MESSAGE)

    def accept_sequence(self, text, terms):
        """The closure's accept: the runner clears the workspace, coderack and stream and
        inserts ``terms`` (``Runner.accept_sequence``); the Coderack rows are forgotten."""
        self.last_sequence = text
        self.reset_run()
        if self.runner is None:
            self.statusBar().showMessage("No model attached", 3000)
            return
        self._pause_requested = False
        self.runner.accept_sequence(list(terms), seed=self.seed(), max_steps=self.max_steps())

    def start_sequence(self, seq, seed=None, max_steps=None):
        """A fresh run on ``seq`` (``Runner.new_sequence``: reset, seed, INITIALIZE), with
        the seed and max-steps fields' values unless given."""
        self.last_sequence = seq
        self.reset_run()
        if self.runner is None:
            self.statusBar().showMessage("No model attached", 3000)
            return
        self._pause_requested = False
        self.runner.new_sequence(seq, seed=self.seed() if seed is None else seed,
                                 max_steps=self.max_steps() if max_steps is None else max_steps)

    def restart(self):
        """Run → Restart: a fresh run of the last sequence with the fields' seed and max
        steps (no sequence yet: ask for one, as x does)."""
        if not str(self.last_sequence).strip():
            self.ask_seq()
            return
        self.start_sequence(self.last_sequence)

    # ---- seed and max steps (Seqsee.pl's --seed, --max_steps) ------------------------------
    def seed(self):
        """The seed field: an int, or None (empty: Seqsee.pl's random default)."""
        return self.run_settings.seed()

    def max_steps(self):
        return self.run_settings.max_steps()

    def _apply_run_options(self):
        """The options (Seqsee.pl's command line) fill the fields; they beat the settings."""
        for key, setter in (("seed", self.run_settings.set_seed),
                            ("max_steps", self.run_settings.set_max_steps)):
            value = self.options.get(key)
            if value is not None and str(value).strip() != "":
                with contextlib.suppress(ValueError, TypeError):
                    setter(int(util.perl_num(value)))

    def _max_steps_changed(self, n):
        if self.runner is not None:
            self.runner.set_max_steps(n)

    # ---- remembered layout (QSettings) -------------------------------------------------
    def save_settings(self):
        """Remember the window's geometry, the toolbars' and docks' layout, the view and the
        fields (nothing without ``settings``)."""
        s = self.settings
        if s is None:
            return
        s.setValue("geometry", self.saveGeometry())
        s.setValue("state", self.saveState(SETTINGS_VERSION))
        s.setValue("view", self.view)
        s.setValue("seed", self.seed_edit.text().strip())
        s.setValue("max_steps", self.max_steps())
        s.sync()

    def restore_settings(self):
        """Restore what ``save_settings`` remembered; unreadable values are ignored."""
        s = self.settings
        if s is None:
            return
        geometry = s.value("geometry")
        if isinstance(geometry, QByteArray):
            self.restoreGeometry(geometry)
        state = s.value("state")
        if isinstance(state, QByteArray):
            self.restoreState(state, SETTINGS_VERSION)
        if "view" not in self.options:
            with contextlib.suppress(ValueError, TypeError, IndexError, KeyError):
                self.set_view(views.view_index(int(s.value("view", 0))))
        seed = s.value("seed")
        if seed is not None and str(seed).strip().isdigit():
            self.run_settings.set_seed(int(str(seed).strip()))
        with contextlib.suppress(ValueError, TypeError):
            n = int(s.value("max_steps"))
            if 1 <= n <= controls.MAX_STEPS_MAX:
                self.run_settings.set_max_steps(n)

    def _show_sleep(self, ms):
        self.interstep_sleep = ms
        self.sleep_widget.show_value(ms)

    def _sleep_moved(self, ms):
        self.interstep_sleep = ms
        if self.runner is not None:
            self.runner.set_interstep_sleep(ms)

    def _state_changed(self, state):
        self.flush_snapshot()           # a command's closing snapshot comes before its end
        if state == "idle" and self._pause_requested:
            state = "paused"
        self._show_state(state)

    def _show_state(self, state):
        self.run_state = state
        self.run_status.show_state(state)

    def _command_done(self, name, result):
        """SGUI::ask_seq keeps InterstepSleep and debugMAX; the port's fresh run resets
        them (reset_all; so does the first accept_sequence), so set them again."""
        self.flush_snapshot()
        if name in ("new_sequence", "accept_sequence") and self.runner is not None:
            self.runner.set_interstep_sleep(self.interstep_sleep)
            self.runner.set_debug_max(self.debug_max)

    def closeEvent(self, event):
        self.save_settings()
        self.list_popups.close_all()
        if self.seq_dialog is not None:
            self.seq_dialog.close()
        if self.more_terms_dialog is not None:
            self.more_terms_dialog.close_quietly()
        if self.runner is not None:
            self.runner.quit(timeout=5)
        super().closeEvent(event)

    # ---- drawing ----------------------------------------------------------------------
    def redraw(self):
        """Update: compose the current view at the canvas's size and show it."""
        self._redraw_timer.stop()
        self.redraw_count += 1
        w, h = self.canvas_size()
        scene = self.canvas.scene()
        if self.snapshot is None:
            self.composition = None
            render.render([], scene=scene, width=w, height=h)
            self.composed_size = (w, h)
            return
        try:
            comp = views.compose(self.view, self.snapshot, w, h, self.attention_needed,
                                 self.pages, measure=self.measure,
                                 known_families=self.known_families)
            render.render(comp.ops, scene=scene, width=w, height=h)
        except Exception as e:  # a drawing bug must not kill the window
            self.statusBar().showMessage(f"Drawing failed: {type(e).__name__}: {e}")
            return
        self.composition = comp
        self.composed_size = (w, h)
        self.remember_coderack_rows(comp, self.view)
        if comp.died:
            first = str(comp.died).strip().splitlines()[0] if str(comp.died).strip() else ""
            self.statusBar().showMessage(f"{comp.died_part} died: {first}")
        else:
            self.statusBar().clearMessage()

    def remember_coderack_rows(self, comp, view):
        """DrawIt's ``$HistoryOfRunnable{$_} ||= 0`` stays in the model: once the Coderack
        has drawn (in ``view``, the window's or a pane's), its rack families keep a row
        (``coderack.known_families``)."""
        parts = [p[0] for p in views.part_rects(view, 1, 1)]
        if views.CODERACK not in parts:
            return
        if comp.died_part is not None and parts.index(comp.died_part) < parts.index(
                views.CODERACK):
            return
        self.known_families = tuple(dict.fromkeys(
            self.known_families + coderack.rack_families(self.snapshot)))

    # ---- saving -----------------------------------------------------------------------
    def _ops(self):
        return self.composition.ops if self.composition is not None else ()

    def save_image(self, path):
        """Save the canvas as shown: ``.png`` or ``.svg``."""
        path = Path(path)
        w, h = self.canvas_size()
        suffix = path.suffix.lower()
        if suffix == ".png":
            render.save_png(self._ops(), path, w, h, background=CANVAS_BACKGROUND)
        elif suffix == ".svg":
            self._save_svg(path, w, h)
        else:
            raise ValueError(f"can only save .png or .svg, not {path.name!r}")

    def _save_svg(self, path, w, h):
        gen = QSvgGenerator()
        gen.setFileName(str(path))
        gen.setSize(QSize(w, h))
        gen.setViewBox(QRect(0, 0, w, h))
        gen.setTitle("Seqsee: " + views.VIEW_OPTIONS[self.view].title)
        scene = render.render(self._ops(), width=w, height=h)
        painter = QPainter(gen)
        painter.fillRect(QRectF(0, 0, w, h), render.qcolor(CANVAS_BACKGROUND))
        scene.render(painter, QRectF(0, 0, w, h), QRectF(0, 0, w, h))
        painter.end()
        scene.clear()

    def _ask_save(self, flt):
        name, chosen = QFileDialog.getSaveFileName(
            self, "Save view", "seqsee" + _SUFFIXES[flt], f"{flt};;{_other(flt)}", flt)
        if not name:
            return
        path = Path(name)
        if path.suffix.lower() not in (".png", ".svg"):
            path = path.with_name(path.name + _SUFFIXES.get(chosen, _SUFFIXES[flt]))
        try:
            self.save_image(path)
        except (OSError, ValueError) as e:
            QMessageBox.warning(self, "Save failed", str(e))
            return
        self.statusBar().showMessage(f"Saved {path}", 5000)

    def about(self):
        QMessageBox.about(self, "About Seqsee", about_text())


def _other(flt):
    return SVG_FILTER if flt == PNG_FILTER else PNG_FILTER
