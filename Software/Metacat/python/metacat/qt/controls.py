"""The control strip of the Qt GUI: gui.ss's control panel, in the main window.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from gui.ss's make-control-panel, its menus and
dialogs, with metacat/gui/gui.py (its tkinter translation) as a worked
translation: loop0003 item 05 (the run controls), item 06 (the menus and
dialogs).

`QtControlPanel` is gui.ss's control panel object: the same messages (switch-
to-run-mode, switch-to-input-mode, init-new-problem, resume-current-problem,
display-breakpoint-message, ...), with the same effects on the model, and the
widgets of a strip at the top of the main window: the info label, the command
line, the speed slider, Step, Go, Stop and Reset, the breakpoint label and the
self-watching warning.  Enter and the buttons run gui.py's own actions
(go-button-action, ...), which talk to this object through
setup.g_control_panel; the speed slider runs gui.py's speed-slider-action.

The menus are gui.ss's, in the main window's menu bar (docs/qt-gui-plan.md
1.3, 2.3): Help, Demos, View (the Windows menu: one checkable action per pane,
made by `attach_windows` once the panes exist), Options and Memory.  SWL's menu
items are QActions; an item's SWL font is its "swl-font" property, its SWL
kind its "swl-kind" property, and a highlighted item (gui.ss's
set-menu-item-color: the current demo and commentary font) is a checked one.
The dialogs are gui.ss's: input-dialog, confirm-dialog (Clear Memory, the
theme-edit dialog) and the Help window, as Qt dialogs; Save commentary uses
Qt's file dialog.

Threads (docs/qt-gui-plan.md 2.5): the engine sends some of these messages
from its own thread (switch-to-run-mode in go, switch-to-input-mode in break,
the breakpoint messages, engine-error).  What touches a widget is posted to
the GUI thread (engine_bridge.GuiInvoker.post) and the message returns at
once; no caller uses the value.  Queries answer from Python attributes.
"""
from __future__ import annotations

from PySide6.QtCore import QEvent, QObject, QTimer, Qt
from PySide6.QtGui import QGuiApplication
from PySide6.QtWidgets import (QDialog, QFileDialog, QHBoxLayout, QLabel, QLineEdit, QMenu,
                               QPushButton, QSlider, QTextEdit, QVBoxLayout, QWidget)

import metacat as _metacat
from metacat import chez, demos, run, setup, utilities, view_globals
from metacat.gui import constants as K
from metacat.gui import fonts as gfonts
from metacat.gui import gui, sgl, swl
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.qt import fonts
from metacat.qt.canvas import _qcolor
from metacat.qt.engine_bridge import GuiInvoker
from metacat.utilities import base_object, exists_p


class _ReturnKey(QObject):
    """Tk's <Key-Return> binding on an entry: the Return key with any modifiers,
    not the keypad's Enter (KP_Enter, another keysym), which QLineEdit's
    returnPressed would also take"""

    def __init__(self, widget, action):
        super().__init__(widget)
        self.action = action

    def eventFilter(self, obj, event):
        if event.type() == QEvent.KeyPress and event.key() == Qt.Key_Return:
            self.action()
            return True
        return False


def bind_return(line_edit, action):
    """port: (bind entry <Key-Return> action), as gui.ss binds its entries"""
    line_edit.installEventFilter(_ReturnKey(line_edit, action))


def qfont(font):
    """an SWL font (gui/fonts.py's SwlFont) as a QFont"""
    return fonts.qfont(swl.tcl_word(font))


def css_color(color):
    return _qcolor(swl.tcl_word(color)).name()


# --------------------------------------------------------------------------------
# fonts (gui.ss: select-control-panel-fonts)

p_gui_header_font = p_gui_command_line_font = p_gui_run_mode_font = False
p_gui_speed_controls_font = p_gui_speed_controls_italic_font = False
p_gui_input_dialog_font = p_gui_menubar_font = p_gui_menu_item_font = False
p_gui_warning_font = p_gui_instructions_font = p_gui_help_window_font = False


def select_control_panel_fonts():
    """gui.ss: select-control-panel-fonts"""
    global p_gui_header_font, p_gui_command_line_font, p_gui_run_mode_font
    global p_gui_speed_controls_font, p_gui_speed_controls_italic_font
    global p_gui_input_dialog_font, p_gui_menubar_font, p_gui_menu_item_font
    global p_gui_warning_font, p_gui_instructions_font, p_gui_help_window_font
    screen = QGuiApplication.primaryScreen()
    screen_height = screen.geometry().height() if screen is not None else 1080
    big = 14 if screen_height > 1024 else 12
    medium = 12 if screen_height > 1024 else 10
    small = 10 if screen_height > 1024 else 8
    ss = gfonts.sans_serif or "helvetica"
    p_gui_header_font = gfonts.swl_font(ss, big, "bold")
    p_gui_command_line_font = gfonts.swl_font(ss, big, "bold")
    p_gui_run_mode_font = gfonts.swl_font(ss, big, "bold", "italic")
    p_gui_speed_controls_font = gfonts.swl_font(ss, small, "bold")
    p_gui_speed_controls_italic_font = gfonts.swl_font(ss, small, "italic")
    p_gui_input_dialog_font = gfonts.swl_font(ss, big, "bold")
    p_gui_menubar_font = gfonts.swl_font(ss, medium)
    p_gui_menu_item_font = gfonts.swl_font(ss, medium, "bold")
    p_gui_warning_font = gfonts.swl_font(ss, big, "bold")
    p_gui_instructions_font = gfonts.swl_font(ss, big)
    p_gui_help_window_font = gfonts.swl_font("courier", big)


# --------------------------------------------------------------------------------
# the input dialog (gui.ss: input-dialog)

class InputDialog(QDialog):
    """gui.ss: input-dialog's toplevel (its field's get-parent)"""

    def raise_(self):
        super().raise_()
        self.activateWindow()


class _InputField:
    """port: the input dialog's SWL <entry>, whose get-parent is the dialog"""

    def __init__(self, dialog, entry):
        self.dialog = dialog
        self.entry = entry

    def get_parent(self):
        return self.dialog

    def set_focus(self):
        self.entry.setFocus()


def input_dialog(x, y, default, message_, input_action, destroy_action):
    """gui.ss: input-dialog.  Enter reads the field: empty closes the dialog,
    a number below 1 or no number shows "Invalid input!" for 700 ms, else
    input_action gets the number and the dialog closes."""
    dialog = InputDialog()
    dialog.setWindowTitle("Input")
    dialog.setAttribute(Qt.WA_DeleteOnClose)
    dialog.setStyleSheet("InputDialog { background: %s; }" % css_color(K.c_white))
    state = {"closed": False}
    message_label = QLabel(message_)
    message_label.setFont(qfont(p_gui_input_dialog_font))
    message_label.setAlignment(Qt.AlignCenter)
    input_field = QLineEdit()
    input_field.setFont(qfont(p_gui_command_line_font))
    input_field.setMaxLength(64)
    layout = QVBoxLayout(dialog)
    layout.setContentsMargins(15, 15, 15, 15)
    layout.addWidget(message_label)
    layout.addSpacing(20)
    layout.addWidget(input_field, 0, Qt.AlignHCenter)
    dialog.setMinimumWidth(230)

    def black():
        message_label.setStyleSheet("color: %s;" % css_color(K.c_black))
    black()

    def destroyed(_result):
        if not state["closed"]:
            state["closed"] = True
            destroy_action(dialog)
    dialog.finished.connect(destroyed)

    def action():
        input_ = input_field.text()
        if input_ == "":
            dialog.close()
            return
        value = chez.string_to_number(input_)
        if value is False or value < 1:
            message_label.setStyleSheet("color: %s;" % css_color(K.c_red))
            message_label.setText("Invalid input!")

            def back():   # port: (pause 700) becomes a timer
                if not state["closed"]:
                    black()
                    message_label.setText(message_)
            QTimer.singleShot(700, back)
        else:
            input_action(value)
            dialog.close()
    bind_return(input_field, action)
    if exists_p(default):
        input_field.setText(default)
        input_field.selectAll()
    cp = setup.g_control_panel
    frame = tell(cp, "get-widgets")["frame"] if cp else None
    if frame is not None:
        corner = frame.mapToGlobal(frame.rect().topLeft())
        dialog.move(corner.x() + x, corner.y() + y)
    dialog.show()
    input_field.setFocus()
    return _InputField(dialog, input_field)


_breakpoint_input_field = [False]


def set_breakpoint_action(item=None):
    """gui.ss: set-breakpoint-action"""
    if exists_p(_breakpoint_input_field[0]):
        _breakpoint_input_field[0].get_parent().raise_()
        _breakpoint_input_field[0].set_focus()
        return

    def input_action(timestep):
        run.g_break_time = timestep
        tell(setup.g_control_panel, "display-breakpoint-message")

    def destroy(toplevel):
        _breakpoint_input_field[0] = False
        return True
    _breakpoint_input_field[0] = input_dialog(
        20, 80,
        chez.number_to_string(run.g_break_time) if exists_p(run.g_break_time) else "",
        "Enter new breakpoint:", input_action, destroy)


def clear_breakpoint_action(item=None):
    """gui.ss: clear-breakpoint-action"""
    run.g_break_time = False
    tell(setup.g_control_panel, "clear-breakpoint-message")


_step_interval_input_field = [False]


def set_step_interval_action(item=None):
    """gui.ss: set-step-interval-action"""
    if exists_p(_step_interval_input_field[0]):
        _step_interval_input_field[0].get_parent().raise_()
        _step_interval_input_field[0].set_focus()
        return

    def input_action(interval):
        run.p_step_cycles = interval

    def destroy(toplevel):
        _step_interval_input_field[0] = False
        return True
    _step_interval_input_field[0] = input_dialog(
        80, 80, chez.number_to_string(run.p_step_cycles), "Enter new step interval:",
        input_action, destroy)


# --------------------------------------------------------------------------------
# the help viewer (gui.ss: help-action)

class SwlDialog(QDialog):
    """port: SWL's <toplevel> for the dialogs and the Help window: closing it
    (destroy, the window manager's close, Escape) runs its destroy-request
    handler once, as gui.py's SwlToplevel does"""

    def __init__(self, title, destroy_request_handler, background=None):
        super().__init__()
        self.setWindowTitle(title)
        self.handler = destroy_request_handler
        self.destroyed_p = False
        if background is not None:
            self.setProperty("swl-background", css_color(background))
            self.setStyleSheet("SwlDialog { background: %s; }" % css_color(background))

    def closeEvent(self, event):
        if not self.destroyed_p:
            if self.handler(self) is False:
                event.ignore()
                return
            self.destroyed_p = True
        # not QDialog's closeEvent, which would reject() (a close again)
        event.accept()
        # deleted by the GUI thread's event loop: never by Python's cycle
        # collector, which may run in the engine thread (a crash), and never
        # inside its own event (the handler may have dropped the last
        # reference, so it is kept until the loop's next turn)
        self.deleteLater()
        _closed_dialogs.append(self)
        QTimer.singleShot(0, _closed_dialogs.clear)

    def reject(self):           # Escape: as the window manager's close
        self.close()

    def destroy(self):
        self.close()

    def raise_(self):
        super().raise_()
        self.activateWindow()

    def set_focus(self):
        self.setFocus()


_closed_dialogs = []


def _place(dialog, x, y):
    """the control panel's get-relative-position: x, y from the strip's corner"""
    cp = setup.g_control_panel
    frame = tell(cp, "get-widgets")["frame"] if cp else None
    if frame is not None:
        corner = frame.mapToGlobal(frame.rect().topLeft())
        dialog.move(corner.x() + x, corner.y() + y)


_help_window = [False]


def help_action(item=None):
    """gui.ss: help-action: help.txt in a word-wrapping, read-only text (one
    window; Help again raises it)"""
    if exists_p(_help_window[0]):
        _help_window[0].raise_()
        _help_window[0].set_focus()
        return

    def destroy(toplevel):
        _help_window[0] = False
        return True
    window = SwlDialog("Help", destroy)
    window.setSizeGripEnabled(True)
    window.setProperty("swl-font", list(swl.tcl_word(p_gui_help_window_font)))
    text = QTextEdit()
    text.setReadOnly(True)
    text.setLineWrapMode(QTextEdit.WidgetWidth)
    text.setFont(qfont(p_gui_help_window_font))
    text.setStyleSheet("QTextEdit { background: %s; color: %s; padding-left: 10px; }"
                       % (css_color(K.p_gui_help_window_color), css_color(K.c_black)))
    with open(gui.HELP_FILE, encoding="latin-1") as port:     # read-file
        text.setPlainText(port.read())
    layout = QVBoxLayout(window)
    layout.setContentsMargins(0, 0, 0, 0)
    layout.addWidget(text)
    # Tk's text: 80 columns by 24 lines
    metrics = text.fontMetrics()
    window.resize(metrics.horizontalAdvance("0") * 80 + 40, metrics.lineSpacing() * 24 + 20)
    _help_window[0] = window
    window.show()


# --------------------------------------------------------------------------------
# pop-up dialogs (gui.ss: confirm-dialog)

# fg-color specifies the color of the dialog text.  If bg-color is #f
# the dialog text appears on a white background surrounded by a grey
# border, otherwise there is no border and the entire dialog
# background is bg-color.

def confirm_dialog(x, y, font, fg_color, bg_color, justify, message_, yes_label, no_label,
                   yes_action, no_action, destroy_action):
    """gui.ss: confirm-dialog"""
    border_bg_color = (bg_color if exists_p(bg_color)
                       else gfonts_color("grey85"))
    dialog = SwlDialog("Confirm", destroy_action, border_bg_color)
    message_label = QLabel(str(message_))
    message_label.setFont(qfont(font))
    message_label.setAlignment(Qt.AlignLeft if justify == "left" else Qt.AlignHCenter)
    message_label.setStyleSheet("QLabel { color: %s; background: %s; }"
                                % (css_color(fg_color),
                                   css_color(bg_color if exists_p(bg_color) else K.c_white)))
    yes_button = QPushButton(yes_label)
    no_button = QPushButton(no_label)
    yes_button.setAutoDefault(False)
    no_button.setAutoDefault(False)
    yes_button.clicked.connect(lambda: yes_action(yes_button))
    no_button.clicked.connect(lambda: no_action(no_button))
    buttons = QHBoxLayout()
    buttons.addWidget(yes_button)
    buttons.addSpacing(20)
    buttons.addWidget(no_button)
    layout = QVBoxLayout(dialog)
    layout.setContentsMargins(15, 15, 15, 15)
    layout.setSpacing(0)
    layout.addWidget(message_label)
    layout.addSpacing(20)
    layout.addLayout(buttons)
    layout.setAlignment(buttons, Qt.AlignHCenter)
    dialog.setFixedSize(dialog.sizeHint())
    _place(dialog, x, y)
    dialog.show()
    return dialog


def gfonts_color(name):
    from metacat.gui import colors
    return colors.swl_color(chez.String(name))


# --------------------------------------------------------------------------------
# Save commentary (gui.ss: save-commentary-action)

def _default_file_dialog(title, mode, default_dir):
    """port: SWL's swl:file-dialog, as Qt's save dialog"""
    name, _filter = QFileDialog.getSaveFileName(None, title, default_dir)
    return name if name else False


file_dialog = _default_file_dialog


def set_file_dialog(f):
    """port: replace swl:file-dialog (tests save without a modal dialog)"""
    global file_dialog
    file_dialog = f


def save_commentary_action(item=None):
    """gui.ss: save-commentary-action"""
    import os
    filename = file_dialog("Save Commentary to File", "save", gui.g_file_dialog_directory)
    if exists_p(filename):
        if os.path.exists(filename):
            os.remove(filename)
        with open(filename, "w") as op:
            for line in tell(setup.g_comment_window, "get-lines"):
                if isinstance(line, str):
                    op.write(chez.format_("~a~%", line))
                else:
                    for _ in range(line):
                        op.write("\n")


# --------------------------------------------------------------------------------
# menus (gui.ss: create-menu, menu-item, check-menu-item, ...)

def _font_of(action):
    """port: an SWL menu item's font (get-font), from its "swl-font" property"""
    face, size, *style = action.property("swl-font")
    return gfonts.swl_font(face, size, list(style))


def _set_font(action, font):
    """port: an SWL menu item's set-font!"""
    action.setFont(qfont(font))
    action.setProperty("swl-font", list(swl.tcl_word(font)))


def menu_item(menu, text, action_proc, font=None, kind="command"):
    """gui.ss: menu-item, added to menu; kind is the SWL item's kind
    ("command", "check"), or "pane" for the View menu's actions"""
    action = menu.addAction(text)
    action.setProperty("swl-kind", kind)
    _set_font(action, font or p_gui_menu_item_font)
    action.triggered.connect(lambda checked=False: action_proc(action))
    return action


def highlight_item(menu, text, action_proc, font=None):
    """port: a command item that gui.ss highlights (set-menu-item-color): a
    demo or a commentary font; checked when highlighted"""
    action = menu_item(menu, text, action_proc, font)
    action.setCheckable(True)
    return action


def check_menu_item(menu, text, selected_p, action_proc):
    """gui.ss: check-menu-item"""
    action = menu_item(menu, text, action_proc, p_gui_menu_item_font, "check")
    action.setCheckable(True)
    action.setChecked(bool(selected_p))
    return action


def create_submenu(menu, text, font=None):
    """gui.ss: create-submenu (its cascade item added to menu)"""
    sub = menu.addMenu(text)
    _set_font(sub.menuAction(), font or p_gui_menu_item_font)
    return sub


def set_menu_item_color(action):
    """gui.ss: set-menu-item-color (highlight the item)"""
    action.setChecked(True)


def unhighlight_menu_items(menu):
    """gui.ss: unhighlight-menu-items"""
    for action in menu.actions():
        if action.menu() is not None:
            unhighlight_menu_items(action.menu())
        elif action.isCheckable() and action.property("swl-kind") == "command":
            action.setChecked(False)


def demo_menu_item(menu, text, problem):
    """gui.ss: demo-menu-item"""
    def action(item):
        tell(setup.g_control_panel, "init-new-problem", problem, False)
        set_menu_item_color(item)
    item = highlight_item(menu, text, action)
    item.setData(list(problem))
    return item


def clamp_codelets_menu_item(menu, text, structure_type):
    """gui.ss: clamp-codelets-menu-item"""
    def action(item):
        # port: the pattern is looked up here, not when the menu is made, so
        # that a control panel can be made before the engine is loaded (the
        # patterns are trace.ss's constants)
        if not tell(setup.g_control_panel, "problem-exists?"):
            return tell(setup.g_control_panel, "display-error", "No current problem!")
        pattern = gui.clamp_codelets_pattern(structure_type)
        trace = _metacat.trace.g_trace
        clamp_event = _metacat.trace.make_clamp_event(
            "manual-clamp", [pattern], [],
            "workspace" if structure_type in ("top-down", "bottom-up", "bridge")
            else structure_type)
        if setup.p_coderack_graphics:
            tell(setup.g_coderack_window, "raise-window")
        tell(trace, "undo-last-clamp")
        tell(trace, "add-event", clamp_event)
        tell(clamp_event, "activate")
    return menu_item(menu, text, action)


def comment_font_menu_item(menu, highlight_p, text, face, size, *style):
    """gui.ss: comment-font-menu-item"""
    item = highlight_item(menu, text, lambda item: None,
                          gfonts.swl_font(face, size, list(style)))
    item.setChecked(bool(highlight_p))
    return item


def set_comment_font_menu_actions(face_menu, size_menu):
    """gui.ss: set-comment-font-menu-actions"""
    from metacat.gui import views

    def face_action(item):
        f = _font_of(item)
        font = gfonts.make_mfont(f.face, f.size, f.style)
        views.set_view_global("%comment-window-font%", font)
        tell(setup.g_comment_window, "new-font", font)
        update_menu_fonts(size_menu, f.face, "same", f.style)
        unhighlight_menu_items(face_menu)
        set_menu_item_color(item)

    def size_action(item):
        f = _font_of(item)
        font = gfonts.make_mfont(f.face, f.size, f.style)
        views.set_view_global("%comment-window-font%", font)
        tell(setup.g_comment_window, "new-font", font)
        update_menu_fonts(face_menu, "same", f.size, "same")
        unhighlight_menu_items(size_menu)
        set_menu_item_color(item)
    for menu, proc in ((face_menu, face_action), (size_menu, size_action)):
        for item in menu.actions():
            if not item.isSeparator():
                item.triggered.disconnect()
                item.triggered.connect(lambda checked=False, item=item, proc=proc: proc(item))


def update_menu_fonts(menu, new_face, new_size, new_style):
    """gui.ss: update-menu-fonts"""
    for item in menu.actions():
        if not item.isSeparator():
            font = _font_of(item)
            face = font.face if new_face == "same" else new_face
            size = font.size if new_size == "same" else new_size
            style = font.style if new_style == "same" else new_style
            _set_font(item, gfonts.swl_font(face, size, list(style)))


# --------------------------------------------------------------------------------
# the View menu (gui.ss: create-windows-menu, window-controller)

# the panes, with the Windows menu's titles, in its order (no Logo: not a pane)
PANE_TITLES = [("workspace", "Workspace"), ("slipnet", "Slipnet"), ("coderack", "Coderack"),
               ("temperature", "Temperature"), ("trace", "Temporal Trace"),
               ("commentary", "Commentary"), ("memory", "Episodic Memory"),
               ("top-themes", "Top Themes"), ("bottom-themes", "Bottom Themes"),
               ("vertical-themes", "Vertical Themes"), ("EEG", "EEG")]


class WindowController(SchemeObject):
    """gui.ss: window-controller, for a pane: its View action is checked when
    the pane is shown (the original's "Hide X" / "Show X" titles)"""

    def __init__(this, text, name, main_window, visible_p, action):
        this.text = text
        this.name = name
        this.main_window = main_window
        this.visible_p = visible_p
        this.menu_item = action

    @message("object-type")
    def object_type(this, self):
        return "window-controller"

    @message("get-menu-item")
    def get_menu_item(this, self):
        return this.menu_item

    @message("get-text")
    def get_text(this, self):
        """port: for tests"""
        return this.text

    @message("get-toplevel")
    def get_toplevel(this, self):
        return this.main_window.hosts[this.name]

    @message("visible?")
    def visible_p_(this, self):
        """port: for tests"""
        return this.visible_p

    @message("initialize")
    def initialize(this, self):
        return tell(self, "update")

    @message("toggle")
    def toggle(this, self):
        if this.visible_p:
            return tell(self, "hide")
        return tell(self, "show")

    @message("show")
    def show_(this, self):
        this.visible_p = True
        tell(self, "update")
        return tell(setup.g_control_panel, "raise")

    @message("hide")
    def hide_(this, self):
        this.visible_p = False
        return tell(self, "update")

    @message("update")
    def update(this, self):
        on = this.visible_p

        def body():
            this.menu_item.setChecked(on)
            this.main_window.set_pane_visible(this.name, on)
        tell(setup.g_control_panel, "on-gui-thread", body)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


# --------------------------------------------------------------------------------

def create_slider(parent, text, min_text, max_text, init_val, slide_action):
    """gui.ss: create-slider: the scale over its Slow and Fast labels, and its
    name under them.  Returns the frame and the QSlider."""
    frame = QWidget(parent)
    slider = QSlider(Qt.Horizontal)
    slider.setRange(0, 100)
    slider.setValue(init_val)
    slider.setFixedWidth(gui.p_gui_slider_length + 40)
    slider.valueChanged.connect(lambda value: slide_action(slider, value))
    labels = QHBoxLayout()
    labels.setContentsMargins(0, 0, 0, 0)
    for t, align in ((min_text, Qt.AlignLeft), (text, Qt.AlignCenter), (max_text, Qt.AlignRight)):
        label = QLabel(t)
        label.setFont(qfont(p_gui_speed_controls_font if t == text
                            else p_gui_speed_controls_italic_font))
        labels.addWidget(label, 1, align)
    layout = QVBoxLayout(frame)
    layout.setContentsMargins(0, 0, 0, 0)
    layout.setSpacing(0)
    layout.addWidget(slider)
    layout.addLayout(labels)
    return frame, slider


def make_control_panel(invoker=None):
    """gui.ss: make-control-panel"""
    return QtControlPanel(invoker)


class QtControlPanel(SchemeObject):
    """gui.ss: make-control-panel (the control panel object), as a strip"""

    def __init__(this, invoker=None):
        select_control_panel_fonts()
        gui.load()
        this.invoker = invoker or GuiInvoker()
        this.parked_hook = None       # the main window's sync, when the engine parks
        white = K.c_white
        strip = QWidget()
        strip.setObjectName("control-strip")
        strip.setAutoFillBackground(True)
        strip.setStyleSheet("QWidget#control-strip { background: %s; }" % css_color(white))
        info_label = QLabel("Please enter a problem:")
        info_label.setFont(qfont(p_gui_header_font))
        # wide enough for a problem and its seed (display widens it further)
        info_label.setMinimumWidth(max(330, info_label.fontMetrics().horizontalAdvance(
            " abcdef -> abcdeg; mrrjjj -> mrrjjjj       seed:  3852097033 ") + 10))
        command_line = QLineEdit()
        command_line.setMinimumWidth(380)
        this.command_line_action = gui.go_button_action
        bind_return(command_line, lambda: this.command_line_action(command_line))
        speed_controls = QWidget()
        speed_controls.setObjectName("speed-controls")
        speed_controls.setStyleSheet("QWidget#speed-controls { background: %s; }"
                                     % css_color(K.p_gui_speed_controls_color))
        slider_frame, speed_slider = create_slider(speed_controls, "Speed", "Slow", "Fast",
                                                   gui.p_initial_speed, gui.speed_slider_action)

        def button(title, action):
            b = QPushButton(title)
            b.setFont(qfont(p_gui_speed_controls_font))
            b.setEnabled(False)
            b.action = action           # for the tests (drive_qt_audit.py)
            b.clicked.connect(lambda: b.action(b))
            return b
        step_button = button("Step", gui.step_button_action)
        go_button = button("Go", gui.go_button_action)
        stop_button = button("Stop", gui.stop_button_action)
        reset_button = button("Reset", gui.reset_button_action)
        breakpoint_label = QLabel("")
        breakpoint_label.setFont(qfont(p_gui_speed_controls_font))
        breakpoint_label.setStyleSheet("color: %s;" % css_color(K.c_red))
        breakpoint_label.setMinimumWidth(220)
        self_watching_warning_label = QLabel("Warning: Self-watching is disabled")
        self_watching_warning_label.setFont(qfont(p_gui_header_font))
        self_watching_warning_label.setStyleSheet("color: %s;" % css_color(K.c_red))
        menus = this._make_menus(self_watching_warning_label)
        controls = QHBoxLayout(speed_controls)
        controls.setContentsMargins(6, 3, 6, 3)
        controls.addWidget(slider_frame)
        controls.addSpacing(10)
        for b in (step_button, go_button, stop_button, reset_button):
            controls.addWidget(b)
        layout = QHBoxLayout(strip)
        layout.setContentsMargins(15, 5, 15, 5)
        layout.addWidget(info_label)
        layout.addWidget(command_line)
        layout.addSpacing(15)
        layout.addWidget(speed_controls)
        layout.addSpacing(15)
        # the two red messages one above the other, as gui.ss packs them under
        # the speed controls, so that the strip fits a 1920-pixel window
        messages = QVBoxLayout()
        messages.setContentsMargins(0, 0, 0, 0)
        messages.setSpacing(0)
        messages.addWidget(breakpoint_label)
        messages.addWidget(self_watching_warning_label)
        layout.addLayout(messages)
        layout.addStretch(1)
        self_watching_warning_label.setVisible(setup.p_self_watching_enabled is False)
        # port: the slider's initial value sets the speed, as gui.py does
        gui.speed_slider_action(speed_slider, gui.p_initial_speed)
        this.strip = strip
        this.info_label = info_label
        this.info_text = "Please enter a problem:"
        this.command_line = command_line
        this.step_button, this.go_button = step_button, go_button
        this.stop_button, this.reset_button = stop_button, reset_button
        this.breakpoint_label = breakpoint_label
        this.self_watching_warning_label = self_watching_warning_label
        this.speed_slider = speed_slider
        this.verbose_mode_p = setup.p_verbose
        this.problem = False
        this.clearmem_dialog = False
        this.theme_edit_dialog = False
        this.edited_theme_types = []
        this.saved_theme_states = []
        this._set_command_line_look(p_gui_command_line_font, K.c_black,
                                    K.p_gui_command_line_color, "left")
        command_line.setFocus()
        this.widgets = {
            "frame": strip, "info-label": info_label, "command-line": command_line,
            "speed-slider": speed_slider, "step-button": step_button,
            "go-button": go_button, "stop-button": stop_button, "reset-button": reset_button,
            "breakpoint-label": breakpoint_label,
            "self-watching-warning-label": self_watching_warning_label,
            "menu-bar": this.menu_bar, "window-controllers": this.window_controllers,
            **menus}

    def _make_menus(this, self_watching_warning_label):
        """gui.ss's menus (make-control-panel's demos-menu, options-menu and
        main-menu), as QMenus; the View menu waits for the panes
        (attach_windows).  Returns them by name, for get-widgets."""
        D = demos
        help_menu = QMenu("Help")
        menu_item(help_menu, "Metacat help", help_action)
        demos_menu = QMenu("Demos")
        for text, name in gui.DEMO_ITEMS:
            demo_menu_item(demos_menu, text, getattr(D, name))
        demos_menu.addSeparator()
        sub = create_submenu(demos_menu, "Answer comparison and reminding")
        for text, name in (("abc / xyd", "abc_xyd"), ("abc / wyz", "abc_wyz"),
                           ("abc / dyz", "abc_dyz"), ("rst / xyu", "rst_xyu"),
                           ("rst / wyz", "rst_wyz"), ("rst / uyz", "rst_uyz"),
                           ("abc / mrrkkk", "abc_mrrkkk"), ("abc / mrrjjjj", "abc_mrrjjjj"),
                           ("xqc / mrrkkk", "xqc_mrrkkk"), ("xqc / mrrjjjj", "xqc_mrrjjjj"),
                           ("eqe / baaab", "eqe_baaab"), ("eqe / aaabaaa", "eqe_aaabaaa"),
                           ("eqe / qeeeq", "eqe_qeeeq"), ("eqe / aaabccc", "eqe_aaabccc")):
            demo_menu_item(sub, text, getattr(D, name))
        demos_menu.addSeparator()
        sub = create_submenu(demos_menu, "Implausible rules")
        for args, name in (((5, 4, "top"), "fig5_4_top"), ((5, 4, "bottom"), "fig5_4_bottom"),
                           ((5, 5, "top"), "fig5_5_top"), ((5, 5, "bottom"), "fig5_5_bottom")):
            demo_menu_item(sub, gui.figure(*args), getattr(D, name))
        sub = create_submenu(demos_menu, "Poor thematic characterizations")
        for args, name in (((5, 7), "fig5_7"), ((5, 8), "fig5_8"), ((5, 10), "fig5_10"),
                           ((5, 11), "fig5_11")):
            demo_menu_item(sub, gui.figure(*args), getattr(D, name))
        sub = create_submenu(demos_menu, "Other sample runs")
        for text, name in (("abc -> cba; mrrjjj -> mmmrrj", "misc1"),
                           ("abc -> abd; ijk -> abd", "misc2"),
                           ("abc -> aabbcc; kkjjii -> ?", "misc3"),
                           ("a -> b; z -> ?", "misc4"), ("abc -> abd; glz -> ?", "misc5")):
            demo_menu_item(sub, text, getattr(D, name))
        view_menu = QMenu("View")
        this.window_controllers = []
        this.theme_window_controllers = []

        options_menu = QMenu("Options")
        menu_item(options_menu, "Set breakpoint", set_breakpoint_action)
        menu_item(options_menu, "Clear breakpoint", clear_breakpoint_action)
        menu_item(options_menu, "Step mode interval", set_step_interval_action)
        options_menu.addSeparator()

        def eliza(item):
            setup.p_eliza_mode = not setup.p_eliza_mode
            tell(setup.g_comment_window, "switch-modes")

        def slipnet_graphics(item):
            setup.p_slipnet_graphics = not setup.p_slipnet_graphics
            if not run.g_display_mode_p:
                if setup.p_slipnet_graphics:
                    tell(setup.g_slipnet_window, "restore-current-state")
                else:
                    tell(setup.g_slipnet_window, "blank-window")

        def coderack_graphics(item):
            setup.p_coderack_graphics = not setup.p_coderack_graphics
            if not run.g_display_mode_p:
                if setup.p_coderack_graphics:
                    tell(setup.g_coderack_window, "restore-current-state")
                else:
                    tell(setup.g_coderack_window, "blank-window", chez.String("Coderack"))

        def codelet_counts(item):
            setup.p_codelet_count_graphics = not setup.p_codelet_count_graphics
            tell(setup.g_coderack_window, "initialize")

        def last_codelet(item):
            setup.p_highlight_last_codelet = not setup.p_highlight_last_codelet
            if setup.p_coderack_graphics and not run.g_display_mode_p:
                if setup.p_highlight_last_codelet:
                    tell(setup.g_coderack_window, "highlight-last-codelet")
                else:
                    tell(setup.g_coderack_window, "unhighlight-last-codelet")

        def self_watching(item):
            setup.p_self_watching_enabled = not setup.p_self_watching_enabled
            if setup.p_self_watching_enabled:
                self_watching_warning_label.hide()
                for a in clamp_items:
                    a.setEnabled(True)
                for controller in this.theme_window_controllers:
                    tell(controller, "show")
            else:
                self_watching_warning_label.show()
                for a in clamp_items:
                    a.setEnabled(False)
                tell(_metacat.trace.g_trace, "undo-last-clamp")
                _metacat.themes.delete_themes()
                for controller in this.theme_window_controllers:
                    tell(controller, "hide")
        check_menu_item(options_menu, "Eliza mode", setup.p_eliza_mode, eliza)
        check_menu_item(options_menu, "Slipnet graphics", setup.p_slipnet_graphics,
                        slipnet_graphics)
        check_menu_item(options_menu, "Coderack graphics", setup.p_coderack_graphics,
                        coderack_graphics)
        check_menu_item(options_menu, "Show codelet counts", setup.p_codelet_count_graphics,
                        codelet_counts)
        check_menu_item(options_menu, "Show last codelet type", setup.p_highlight_last_codelet,
                        last_codelet)
        check_menu_item(options_menu, "Self-watching mode", setup.p_self_watching_enabled,
                        self_watching)
        check_menu_item(options_menu, "Verbose mode", setup.p_verbose,
                        lambda item: tell(setup.g_control_panel, "toggle-verbose-mode"))
        options_menu.addSeparator()
        clamp_themes_menu_item = menu_item(
            options_menu, "Clamp theme pattern",
            lambda item: tell(setup.g_control_panel, "theme-edit-mode-on"))
        clamp_codelets_menu = create_submenu(options_menu, "Clamp codelet pattern")
        for text, structure_type in (("Top-down codelet pattern", "top-down"),
                                     ("Bottom-up codelet pattern", "bottom-up"),
                                     ("Rule codelet pattern", "rule"),
                                     ("Bridge codelet pattern", "bridge"),
                                     ("Group codelet pattern", "group")):
            clamp_codelets_menu_item(clamp_codelets_menu, text, structure_type)
        undo_clamp_menu_item = menu_item(
            options_menu, "Undo last clamp",
            lambda item: tell(_metacat.trace.g_trace, "undo-last-clamp"))
        clamp_items = [clamp_themes_menu_item, clamp_codelets_menu.menuAction(),
                       undo_clamp_menu_item]
        options_menu.addSeparator()
        # these menus assume %comment-window-font% is sans-serif 12 (bold italic)
        face_menu = create_submenu(options_menu, "Commentary font face")
        # (fonts.ss's faces; Tk's names before fonts.install has chosen them)
        ss = gfonts.sans_serif or "helvetica"
        for i, (name, face) in enumerate((("serif", gfonts.serif or "times"), ("sans-serif", ss),
                                          ("fancy", gfonts.fancy or "palatino"))):
            if i:
                face_menu.addSeparator()
            for suffix, style in (("", ()), (" italic", ("italic",)), (" bold", ("bold",)),
                                  (" bold italic", ("bold", "italic"))):
                comment_font_menu_item(face_menu, name + suffix == "sans-serif bold italic",
                                       name + suffix, face, 12, *style)
        size_menu = create_submenu(options_menu, "Commentary font size")
        for text, size in (("tiny", 8), ("small", 10), ("medium", 12), ("large", 18),
                           ("larger", 24), ("huge", 34)):
            comment_font_menu_item(size_menu, text == "medium", text, ss, size, "bold", "italic")
        menu_item(options_menu, "Save commentary to file", save_commentary_action)
        memory_menu = QMenu("Memory")
        menu_item(memory_menu, "Clear Memory",
                  lambda item: tell(setup.g_control_panel, "clear-memory"))
        if setup.p_self_watching_enabled is False:
            for a in clamp_items:
                a.setEnabled(False)
        set_comment_font_menu_actions(face_menu, size_menu)
        this.menu_bar = [help_menu, demos_menu, view_menu, options_menu, memory_menu]
        for menu in this.menu_bar:
            _set_font(menu.menuAction(), p_gui_menubar_font)
        this.demos_menu = demos_menu
        this.view_menu = view_menu
        this.menus = [demos_menu, options_menu, memory_menu]   # disabled while running
        return {"help-menu": help_menu, "demos-menu": demos_menu, "view-menu": view_menu,
                "options-menu": options_menu, "memory-menu": memory_menu,
                "clamp-codelets-menu": clamp_codelets_menu,
                "comment-font-face-menu": face_menu, "comment-font-size-menu": size_menu}

    def attach_windows(this, main_window):
        """gui.ss: the window controllers and create-windows-menu, for the panes
        of main_window: the View menu (main window's GUI thread)"""
        view_menu = this.view_menu
        controllers = []
        for name, text in PANE_TITLES:
            action = menu_item(view_menu, text, lambda item: None, kind="pane")
            action.setCheckable(True)
            visible_p = name not in main_window.hidden_at_start()
            controller = WindowController(text, name, main_window, visible_p, action)
            action.triggered.disconnect()
            action.triggered.connect(lambda checked=False, c=controller: tell(c, "toggle"))
            controllers.append(controller)

        def show_all(item):
            for controller in controllers:
                tell(controller, "show")

        def hide_all(item):
            for controller in controllers:
                tell(controller, "hide")

        def reset_layout(item):
            """port: View > Reset layout (docs/qt-gui-plan.md 2.3)"""
            for controller in controllers:
                shown = (controller.name not in main_window.hidden_at_start()
                         and (setup.p_self_watching_enabled is not False
                              or controller not in this.theme_window_controllers))
                tell(controller, "show" if shown else "hide")
            main_window.reset_layout()
        view_menu.addSeparator()
        menu_item(view_menu, "Show all panes", show_all)
        menu_item(view_menu, "Hide all panes", hide_all)
        view_menu.addSeparator()
        menu_item(view_menu, "Reset layout", reset_layout)
        for controller in controllers:
            tell(controller, "initialize")
        this.window_controllers[:] = controllers
        this.theme_window_controllers[:] = controllers[7:10]   # (sublist ... 7 10)

    def _gui(this, fn):
        """port: fn on the GUI thread (now there; posted from another thread)"""
        this.invoker.post(fn)

    @message("on-gui-thread")
    def on_gui_thread(this, self, fn):
        """port: fn on the GUI thread (the window controllers' widget changes)"""
        this._gui(fn)

    # control panel object:
    @message("object-type")
    def object_type(this, self):
        return "control-panel"

    @message("get-widgets")
    def get_widgets(this, self):
        """port: the widgets, for the main window and tests"""
        return this.widgets

    @message("problem-exists?")
    def problem_exists_p(this, self):
        return exists_p(this.problem)

    @message("get-current-problem")
    def get_current_problem(this, self):
        return this.problem

    @message("get-command-line-string")
    def get_command_line_string(this, self):
        return this.invoker.call(this.command_line.text)

    @message("update-current-problem")
    def update_current_problem(this, self, tokens):
        if all(isinstance(t, str) for t in tokens):   # (andmap symbol? tokens)
            utilities.randomize()
        if len(tokens) == 5:
            this.problem = list(tokens)
        elif len(tokens) == 3:
            this.problem = [*tokens, False, chez.random_seed()]
        elif isinstance(tokens[3], str):
            this.problem = [*tokens, chez.random_seed()]
        elif isinstance(tokens[3], int):
            this.problem = [tokens[0], tokens[1], tokens[2], False, tokens[3]]
        problem = this.problem
        setup.p_justify_mode = exists_p(problem[3])
        tell(self, "display", chez.format_(
            " ~a -> ~a; ~a -> ~a       seed:  ~a ", problem[0], problem[1], problem[2],
            problem[3] if setup.p_justify_mode else "?", problem[4]))
        def body():
            this.command_line.clear()
            unhighlight_menu_items(this.demos_menu)
        this._gui(body)

    @message("init-new-problem")
    def init_new_problem(this, self, tokens, step_mode_p):
        tell(self, "update-current-problem", tokens)
        tell(self, "switch-to-input-mode")
        problem = this.problem

        def thunk():
            run.init_mcat(*problem)
            if step_mode_p:
                run.step_mode_on()
            run.quiet_break()
            run.run_mcat()
        gui.thread_break(setup.g_repl_thread, False, thunk)

    @message("run-new-problem")
    def run_new_problem(this, self, tokens):
        tell(self, "update-current-problem", tokens)
        tell(self, "switch-to-run-mode")
        problem = this.problem

        def thunk():
            run.init_mcat(*problem)
            run.run_mcat()
        gui.thread_break(setup.g_repl_thread, False, thunk)

    @message("run-demo")
    def run_demo(this, self, problem):
        """port: gui.ss's demo-menu-item action (item 06 highlights the item)"""
        return tell(self, "init-new-problem", problem, False)

    @message("resume-current-problem")
    def resume_current_problem(this, self):
        from metacat import view_globals
        if not exists_p(this.problem):
            return tell(self, "display-error", "No current problem!")
        if run.g_display_mode_p:
            view_globals.restore_current_state()
        gui.thread_break(setup.g_repl_thread, False, run.go)

    @message("reset-current-problem")
    def reset_current_problem(this, self):
        if not exists_p(this.problem):
            return tell(self, "display-error", "No current problem!")
        problem = this.problem

        def thunk():
            run.init_mcat(*problem)
            run.quiet_break()
            run.run_mcat()
        gui.thread_break(setup.g_repl_thread, False, thunk)

    @message("verbose-mode?")
    def verbose_mode_p(this, self):
        return this.verbose_mode_p

    @message("toggle-verbose-mode")
    def toggle_verbose_mode(this, self):
        this.verbose_mode_p = not this.verbose_mode_p
        setup.p_verbose = this.verbose_mode_p

    @message("set-verbose-step-mode")
    def set_verbose_step_mode(this, self, value):
        setup.p_verbose = value or this.verbose_mode_p
        return "done"

    def _set_command_line_look(this, font, fg, bg, justify):
        # a disabled line keeps these colours, as gui.py's Tk entry does
        this.command_line_look = (font, fg, bg, justify)
        this.command_line.setFont(qfont(font))
        this.command_line.setStyleSheet("QLineEdit { color: %s; background: %s; }"
                                        % (css_color(fg), css_color(bg)))
        this.command_line.setAlignment(Qt.AlignHCenter if justify == "center" else Qt.AlignLeft)

    def _enable_all(this, command_line, step, go, stop, reset, menus):
        this.command_line.setEnabled(command_line)
        this.step_button.setEnabled(step)
        this.go_button.setEnabled(go)
        this.stop_button.setEnabled(stop)
        this.reset_button.setEnabled(reset)
        for menu in this.menus:
            # the cascade only, as gui.ss's set-enabled! on the menu bar's item
            # (QMenu.setEnabled would disable its submenus' items too)
            menu.menuAction().setEnabled(menus)

    @message("switch-to-run-mode")
    def switch_to_run_mode(this, self):
        def body():
            this.command_line_action = gui.nop_event_handler
            this._set_command_line_look(p_gui_run_mode_font, K.c_green, K.c_black, "center")
            this.command_line.setText("running...")
            this._enable_all(False, False, False, True, False, False)
        this._gui(body)

    @message("switch-to-input-mode")
    def switch_to_input_mode(this, self):
        def body():
            this.command_line_action = gui.go_button_action
            this.command_line.clear()
            this._set_command_line_look(p_gui_command_line_font, K.c_black,
                                        K.p_gui_command_line_color, "left")
            this._enable_all(True, True, True, False, True, True)
            this.command_line.setFocus()
            # port: the engine parked: every pane shows its final picture now
            if this.parked_hook is not None:
                this.parked_hook()
        this._gui(body)

    @message("switch-to-disabled-mode")
    def switch_to_disabled_mode(this, self):
        def body():
            this.command_line_action = gui.nop_event_handler
            this._enable_all(False, False, False, False, False, False)
        this._gui(body)

    @message("ready-to-edit?")
    def ready_to_edit_p(this, self, theme_type):
        return (not utilities.member_p(theme_type, this.edited_theme_types)
                and (bool(setup.p_justify_mode) or theme_type != "bottom-bridge"))

    @message("edit-theme-type")
    def edit_theme_type(this, self, theme_type):
        this.edited_theme_types = [theme_type] + this.edited_theme_types
        _metacat.themes.set_themes(theme_type, 0)

    @message("raise-theme-edit-dialog")
    def raise_theme_edit_dialog(this, self):
        this.theme_edit_dialog.raise_()
        this.theme_edit_dialog.set_focus()

    @message("theme-edit-mode-on")
    def theme_edit_mode_on(this, self):
        from metacat.gui import views
        themespace = _metacat.themes.g_themespace
        if not exists_p(this.problem):
            return tell(self, "display-error", "No current problem!")
        if exists_p(this.theme_edit_dialog):
            return tell(self, "raise-theme-edit-dialog")
        if run.g_display_mode_p:
            view_globals.restore_current_state()
        tell(self, "switch-to-disabled-mode")
        views.set_view_global("*theme-edit-mode?*", True)
        this.edited_theme_types = []
        this.saved_theme_states = [
            tell(themespace, "get-partial-state", "top-bridge"),
            tell(themespace, "get-partial-state", "vertical-bridge"),
            tell(themespace, "get-partial-state", "bottom-bridge")]
        tell(setup.g_themespace_window, "set-background-colors",
             K.p_theme_background_color__thematic_pressure_on, K.p_theme_edit_mode_color)
        tell(setup.g_themespace_window, "clear")
        _metacat.themes.delete_themes()
        tell(themespace, "thematic-pressure-off")
        tell(setup.g_themespace_window, "display-edit-mode-message")
        tell(setup.g_themespace_window, "raise-window")
        if sgl.g_platform == "macintosh":
            text = chez.format_("~a~%~a~%~a~%~%~a~%~a~%~a~%~a",
                                "To clamp a theme-pattern, click on one or more",
                                "theme windows, select the themes to include in",
                                "the pattern, and then click Clamp Themes.",
                                "Clicking on a theme selects maximum positive",
                                "theme activation.  Shift-clicking selects maximum",
                                "negative activation.  Clicking on an already-selected",
                                "theme unselects it.")
        else:
            text = chez.format_("~a~%~a~%~a~%~%~a~%~a~%~a~%~a",
                                "To clamp a theme-pattern, click on one or more",
                                "theme windows, select the themes to include in",
                                "the pattern, and then click Clamp Themes.",
                                "Left-clicking on a theme selects maximum",
                                "positive theme activation.  Right-clicking selects",
                                "maximum negative activation.  Clicking on an",
                                "already-selected theme unselects it.")

        def yes(button):
            tell(self, "theme-edit-mode-off", True)
            this.theme_edit_dialog.destroy()

        def no(button):
            tell(self, "theme-edit-mode-off", False)
            this.theme_edit_dialog.destroy()

        def destroy(toplevel):
            tell(self, "theme-edit-mode-off", False)
            this.theme_edit_dialog = False
            tell(self, "switch-to-input-mode")
            return True
        this.theme_edit_dialog = confirm_dialog(
            10, 20, p_gui_instructions_font, K.c_black, K.c_yellow, "left", text,
            "Clamp Themes", "Cancel", yes, no, destroy)

    @message("theme-edit-mode-off")
    def theme_edit_mode_off(this, self, clamp_patterns_p):
        from metacat.gui import theme_graphics, views
        themespace = _metacat.themes.g_themespace
        trace = _metacat.trace.g_trace
        if theme_graphics.g_theme_edit_mode_p:
            views.set_view_global("*theme-edit-mode?*", False)
            tell(setup.g_themespace_window, "set-background-colors",
                 K.p_theme_background_color__thematic_pressure_on,
                 K.p_theme_background_color__thematic_pressure_off)
            theme_types_to_clamp = this.edited_theme_types if clamp_patterns_p else []
            for state in this.saved_theme_states:
                theme_type = state[0][0]
                if not utilities.member_p(theme_type, theme_types_to_clamp):
                    tell(themespace, "restore-state", state)
            if theme_types_to_clamp:
                # chez: map's order (the patterns only read)
                patterns = chez.map_(
                    lambda type_: tell(themespace, "get-nonzero-theme-pattern", type_),
                    theme_types_to_clamp)
                clamp_event = _metacat.trace.make_clamp_event("manual-clamp", patterns, [],
                                                              "workspace")
                tell(trace, "undo-last-clamp")
                tell(trace, "add-event", clamp_event)
                tell(clamp_event, "activate")

    @message("clear-memory")
    def clear_memory(this, self):
        if exists_p(this.clearmem_dialog):
            this.clearmem_dialog.raise_()
            this.clearmem_dialog.set_focus()
            return
        tell(self, "switch-to-disabled-mode")

        def yes(button):
            tell(_metacat.memory.g_memory, "clear")
            this.clearmem_dialog.destroy()

        def no(button):
            this.clearmem_dialog.destroy()

        def destroy(toplevel):
            this.clearmem_dialog = False
            tell(setup.g_control_panel, "switch-to-input-mode")
            return True
        this.clearmem_dialog = confirm_dialog(
            20, 70, p_gui_warning_font, K.c_red, False, "center",
            chez.format_("Really delete all answers~%from the Episodic Memory?"),
            "Yes", "Cancel", yes, no, destroy)

    @message("hide-window")
    def hide_window(this, self, toplevel):
        for controller in this.window_controllers:
            if tell(controller, "get-toplevel") is toplevel:
                tell(controller, "hide")

    @message("raise")
    def raise_(this, self):
        if exists_p(this.clearmem_dialog):
            this.clearmem_dialog.raise_()

    @message("display-breakpoint-message")
    def display_breakpoint_message(this, self):
        text = chez.format_("Breakpoint set for time step ~a", run.g_break_time)
        this._gui(lambda: this.breakpoint_label.setText(text))

    @message("clear-breakpoint-message")
    def clear_breakpoint_message(this, self):
        this._gui(lambda: this.breakpoint_label.setText(""))

    @message("display")
    def display(this, self, message_):
        def body():
            this.info_text = message_
            this.info_label.setText(message_)
            # port: the strip sits in a tool bar, which follows minimum sizes
            hint = this.info_label.sizeHint().width()
            if hint > this.info_label.minimumWidth():
                this.info_label.setMinimumWidth(hint)
        this._gui(body)

    @message("display-error")
    def display_error(this, self, message_):
        def body():
            this.info_label.setStyleSheet("color: %s;" % css_color(K.c_red))
            this.info_label.setText(message_)

            def back():   # port: (pause 700) in the GUI thread becomes a timer
                # port: the message under the error, not the label's text, which
                # may be an earlier error still showing (anomalies: "Two errors
                # within 700 ms leave the first one on the control panel")
                this.info_label.setStyleSheet("color: %s;" % css_color(K.c_black))
                this.info_label.setText(this.info_text)
            QTimer.singleShot(700, back)
        this._gui(body)

    @message("engine-error")
    def engine_error(this, self, message_):
        """port: an error in the model, which ended the engine thread's thunk (in the
        original it went to the REPL, leaving the control panel in run mode)"""
        tell(self, "switch-to-input-mode")
        tell(self, "display", chez.format_("Error: ~a", message_))

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)
