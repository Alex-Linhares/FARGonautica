"""gui.ss: the control panel, its menus and dialogs, and the window controllers.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from gui.ss, with
racket/gui/gui.rktl as a worked translation.

The command-line parser, the button, menu and speed actions, the control panel
object (its messages and their effects) and the window controllers are the
original's, one function or class per definition.  SWL's widgets were Tk's
widgets under another name, so they become tkinter's own: <toplevel> is a
Toplevel, <label> a Label, <entry> an Entry, <scale> a Scale, <button> a
Button, <frame> a Frame, packed as gui.ss packs them, with the same options
(fonts, colours, relief, traversal thickness).  What SWL added on top of Tk is
here, marked "port:":

- `MenuItem` and `Menu`: SWL's menu items were objects created before their
  menu, with their own title, font, colours and action.  Here an item keeps
  them and passes them to its Tk menu entry once the menu is made (when the
  main menu is attached to the control panel), and from then on.
- `SwlToplevel`: a Toplevel whose `destroy` runs its destroy-request handler
  first, as SWL's did (the dialogs rely on it), and so does closing it from the
  window manager.
- `thread_break(thread, ignore, thunk)`: SWL's thread-break, which interrupted the
  REPL thread to run a thunk.  The REPL thread is gui/app.py's engine thread:
  thunks queue there and run one at a time, each until run.ss's break returns
  to it (run.toplevel).
- (pause 700) in the GUI thread (display-error, the input dialog) becomes a Tk
  timer, so that the GUI keeps answering.

This module imports tkinter only inside the functions that make widgets.  The
control panel is made and used in Tk's main thread; the engine thread sends it
the messages run.ss sends (switch-to-input-mode ...), and tkinter hands those
Tk calls to the main thread.
"""
from __future__ import annotations

import sys

import metacat as _metacat
from metacat import chez, run, setup, sugar, utilities, view_globals
from metacat import demos
from metacat.gui import colors, fonts, sgl
from metacat.gui import constants as K
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import base_object, exists_p

p_gui_header_font = False
p_gui_command_line_font = False
p_gui_run_mode_font = False
p_gui_speed_controls_font = False
p_gui_speed_controls_italic_font = False
p_gui_menubar_font = False
p_gui_menu_item_font = False
p_gui_warning_font = False
p_gui_instructions_font = False
p_gui_input_dialog_font = False
p_gui_help_window_font = False


# ---------------------------------------------------------------------------
# port: SWL on tkinter

def _tk():
    import tkinter
    return tkinter


def _w(x):
    """port: a Tcl word for a colour or font option (gui/swl.py's tcl_word)"""
    from metacat.gui import swl
    return swl.tcl_word(x)


def _root():
    from metacat.gui import swl
    return swl.g_tk_root


def thread_break(thread, ignore, thunk):
    """port: SWL's thread-break: interrupt the REPL thread to run thunk (here: queue
    it on gui/app.py's engine thread)"""
    return thread.send(thunk)


def nop_event_handler(*args):
    """port: SWL's nop-event-handler"""
    return None


def show(w):
    """port: SWL's show generic: a window host, a toplevel or a packed widget"""
    if hasattr(w, "show_window"):
        return w.show_window()
    tk = _tk()
    if isinstance(w, tk.Wm):
        return w.deiconify()
    return w.pack(**getattr(w, "_swl_pack", {}))


def hide(w):
    """port: SWL's hide generic"""
    if hasattr(w, "hide_window"):
        return w.hide_window()
    tk = _tk()
    if isinstance(w, tk.Wm):
        return w.withdraw()
    return w.pack_forget()


def pack(w, **options):
    """port: SWL's pack (Tk's pack; the options are kept for show)"""
    w._swl_pack = options
    w.pack(**options)


def set_enabled(w, on):
    """port: SWL's set-enabled! on a widget"""
    w.configure(state="normal" if on else "disabled")


class SwlToplevel:
    """port: SWL's <toplevel> for the dialogs and the Help window: destroy runs the
    destroy-request handler first, and destroys the window only if it answers
    true; closing it from the window manager does the same."""

    def __init__(self, title, destroy_request_handler, background=None):
        tk = _tk()
        self.top = tk.Toplevel(_root())
        self.top.title(title)
        if background is not None:
            self.top.configure(background=_w(background))
        self.handler = destroy_request_handler
        self.destroyed = False
        self.top.protocol("WM_DELETE_WINDOW", self.destroy)

    def destroy(self):
        if not self.destroyed and self.handler(self) is not False:
            self.destroyed = True
            self.top.destroy()

    def raise_(self):
        self.top.lift()

    def set_focus(self):
        self.top.focus_set()


def _menu_bg():
    return K.p_gui_menu_background_color


class MenuItem:
    """port: an SWL menu item (<command-menu-item>, <check-menu-item>,
    <cascade-menu-item>, <separator-menu-item>): its options, passed to its Tk
    entry once its menu exists"""

    def __init__(self, kind, **options):
        self.kind = kind
        self.options = {}
        self.font = None
        self.action = None
        self.submenu = None
        self.selected = False
        self.variable = None
        self.menu = None      # the Tk menu, once made
        self.index = None
        for k, v in options.items():
            self._set(k, v)

    def _set(self, option, value):
        if option == "font":
            self.font = value
            value = _w(value) if value else None
        elif option in ("background", "foreground", "activeforeground", "selectcolor"):
            value = _w(value)
        if value is None:
            return
        self.options[option] = value
        if self.menu is not None and self.kind != "separator":
            self.menu.entryconfigure(self.index, **{option: value})

    def _command(self):
        if self.kind == "check":
            self.selected = bool(self.variable.get())
        if self.action is not None:
            self.action(self)

    def realize(self, menu):
        """add this item's entry to the Tk menu"""
        tk = _tk()
        self.menu = menu
        if self.kind == "separator":
            menu.add_separator()
        elif self.kind == "cascade":
            sub = self.submenu.realize(menu)
            menu.add_cascade(menu=sub, **self.options)
        elif self.kind == "check":
            self.variable = tk.IntVar(menu, value=1 if self.selected else 0)
            menu.add_checkbutton(variable=self.variable, command=self._command,
                                 **self.options)
        else:
            menu.add_command(command=self._command, **self.options)
        self.index = menu.index("end")

    # SWL's methods on menu items
    def set_title_bang(self, title):
        self._set("label", str(title))

    def get_title(self):
        return self.options.get("label", "")

    def set_enabled_bang(self, on):
        self._set("state", "normal" if on else "disabled")

    def set_foreground_color_bang(self, color):
        self._set("foreground", color)

    def set_active_foreground_color_bang(self, color):
        self._set("activeforeground", color)

    def set_background_color_bang(self, color):
        self._set("background", color)

    def get_font(self):
        return self.font

    def set_font_bang(self, font):
        self._set("font", font)

    def set_action_bang(self, action):
        self.action = action

    def get_selected(self):
        return self.selected

    def get_menu(self):
        return self.submenu


class Menu:
    """port: an SWL <menu>: its items, made into a Tk menu by realize"""

    def __init__(self, items):
        self.items = list(items)
        self.tk_menu = None

    def get_menu_items(self):
        return self.items

    def realize(self, parent):
        tk = _tk()
        self.tk_menu = tk.Menu(parent, tearoff=0)
        for item in self.items:
            item.realize(self.tk_menu)
        return self.tk_menu

    def set_enabled_bang(self, on):
        for item in self.items:
            item.set_enabled_bang(on)


def isa_separator_p(item):
    """port: (isa? item <separator-menu-item>)"""
    return item.kind == "separator"


def isa_cascade_p(item):
    """port: (isa? item <cascade-menu-item>)"""
    return item.kind == "cascade"


# ---------------------------------------------------------------------------

# bug workaround: on mac OS X, set-background-color! actually changes
# the text *foreground color* instead of the background color, so we
# use =blue= instead of =white= for better visibility:
def set_menu_item_color(item):
    """gui.ss: set-menu-item-color"""
    if sgl.g_platform == "macintosh":
        # highlight item by setting foreground color to blue
        item.set_background_color_bang(K.c_blue)
    else:
        # highlight item by setting background color to white
        item.set_background_color_bang(K.c_white)


def select_control_panel_fonts():
    """gui.ss: select-control-panel-fonts"""
    global p_gui_header_font, p_gui_command_line_font, p_gui_run_mode_font
    global p_gui_speed_controls_font, p_gui_speed_controls_italic_font, p_gui_menubar_font
    global p_gui_menu_item_font, p_gui_warning_font, p_gui_instructions_font
    global p_gui_input_dialog_font, p_gui_help_window_font
    screen_height = _root().winfo_screenheight()   # (swl:screen-height)
    big = 14 if screen_height > 1024 else 12
    medium = 12 if screen_height > 1024 else 10
    small = 10 if screen_height > 1024 else 8
    ss = fonts.sans_serif
    p_gui_header_font = fonts.swl_font(ss, big, "bold")
    p_gui_command_line_font = fonts.swl_font(ss, big, "bold")
    p_gui_run_mode_font = fonts.swl_font(ss, big, "bold", "italic")
    p_gui_speed_controls_font = fonts.swl_font(ss, small, "bold")
    p_gui_speed_controls_italic_font = fonts.swl_font(ss, small, "italic")
    p_gui_menubar_font = fonts.swl_font(ss, medium)
    p_gui_menu_item_font = fonts.swl_font(ss, medium, "bold")
    p_gui_warning_font = fonts.swl_font(ss, big, "bold")
    p_gui_instructions_font = fonts.swl_font(ss, big)
    p_gui_input_dialog_font = fonts.swl_font(ss, big, "bold")
    p_gui_help_window_font = fonts.swl_font("courier", big)


p_gui_slider_length = 80
p_gui_slider_thickness = 12

p_initial_speed = 50


# --------------------------------------------------------------------------------

def pack_hspace(parent, width, side, *color):
    """gui.ss: pack-hspace"""
    space = _tk().Frame(parent, width=width)
    if color:
        space.configure(background=_w(color[0]))
    pack(space, side=side)


def pack_vspace(parent, height, side, *color):
    """gui.ss: pack-vspace"""
    space = _tk().Frame(parent, height=height)
    if color:
        space.configure(background=_w(color[0]))
    pack(space, side=side)


# --------------------------------------------------------------------------------
# command line parser for control panel

def _alphabetic_p(c):
    # chez: char-alphabetic? is Unicode's Alphabetic property, str.isalpha the
    # letter categories (anomalies: "str.isalpha is close to, but not, Chez's
    # char-alphabetic?"); equal on the battery's characters
    return c.isalpha()


def _numeric_p(c):
    # chez: char-numeric? is Unicode's Numeric property (½ and Arabic-Indic digits
    # are numeric; fixture char-noise), as str.isnumeric
    return c.isnumeric()


def _downcase(c):
    # chez: char-downcase maps one character to one
    d = c.lower()
    return d if len(d) == 1 else c


def tokenize_string(input_):
    """gui.ss: tokenize-string.  A list of symbols and numbers, or the symbol
    error."""
    chars = [_downcase(c) for c in str(input_)]   # (map char-downcase (string->list input))
    tokens = []
    buffer = []
    i = 0
    n = len(chars)
    # consume-noise, consume-letters and consume-digits, as one loop over the
    # state (the original's mutual tail calls)
    mode = "noise"
    while True:
        if mode == "noise":
            if i == n:
                return tokens
            c = chars[i]
            if char_noise_p(c):
                i += 1
            elif _alphabetic_p(c):
                buffer, mode, i = [c], "letters", i + 1
            elif _numeric_p(c):
                buffer, mode, i = [c], "digits", i + 1
            else:
                return "error"
        elif mode == "letters":
            if i == n:
                return tokens + ["".join(buffer)]
            c = chars[i]
            if char_noise_p(c):
                tokens.append("".join(buffer))
                buffer, mode, i = [], "noise", i + 1
            elif _alphabetic_p(c):
                buffer.append(c)
                i += 1
            else:
                return "error"
        else:
            if i == n:
                return tokens + [chez.string_to_number("".join(buffer))]
            c = chars[i]
            if char_noise_p(c):
                tokens.append(chez.string_to_number("".join(buffer)))
                buffer, mode, i = [], "noise", i + 1
            elif _numeric_p(c):
                buffer.append(c)
                i += 1
            else:
                return "error"


def char_noise_p(char):
    """gui.ss: char-noise?"""
    return not _alphabetic_p(char) and not _numeric_p(char)


def step_button_action(ignore):
    """gui.ss: step-button-action"""
    input_ = tell(setup.g_control_panel, "get-command-line-string")
    if len(input_) == 0:
        run.step_mode_on()
        return tell(setup.g_control_panel, "resume-current-problem")
    tokens = tokenize_string(input_)
    if sugar.valid_token_list_p(tokens):
        return tell(setup.g_control_panel, "init-new-problem", tokens, True)
    return tell(setup.g_control_panel, "display-error", "Invalid input!")


def go_button_action(ignore):
    """gui.ss: go-button-action"""
    input_ = tell(setup.g_control_panel, "get-command-line-string")
    if len(input_) == 0:
        run.step_mode_off()
        return tell(setup.g_control_panel, "resume-current-problem")
    tokens = tokenize_string(input_)
    if sugar.valid_token_list_p(tokens):
        return tell(setup.g_control_panel, "init-new-problem", tokens, False)
    return tell(setup.g_control_panel, "display-error", "Invalid input!")


def stop_button_action(ignore):
    """gui.ss: stop-button-action"""
    run.g_interrupt_p = True


def reset_button_action(ignore):
    """gui.ss: reset-button-action"""
    input_ = tell(setup.g_control_panel, "get-command-line-string")
    if len(input_) == 0:
        return tell(setup.g_control_panel, "reset-current-problem")
    tokens = tokenize_string(input_)
    if sugar.valid_token_list_p(tokens):
        return tell(setup.g_control_panel, "init-new-problem", tokens, False)
    return tell(setup.g_control_panel, "display-error", "Invalid input!")


# ------------------------------------------------------------------
# help viewer

# port: help.txt ships with the package, a copy of the original's
# (tests/test_install.py checks that it is unchanged); the original read it from
# the current directory, Metacat's
HELP_FILE = __import__("pathlib").Path(__file__).resolve().parent / "help.txt"


def read_file(filename, text_widget):
    """gui.ss: read-file"""
    with open(filename, encoding="latin-1") as port:
        while True:
            x = port.read(2048)
            if not x:
                break
            text_widget.insert("end", x)


_help_window = [False]


def help_action(item):
    """gui.ss: help-action"""
    if exists_p(_help_window[0]):
        _help_window[0].raise_()
        _help_window[0].set_focus()
        return

    def destroy(toplevel):
        _help_window[0] = False
        return True
    _help_window[0] = SwlToplevel("Help", destroy)
    tk = _tk()
    # port: SWL's <scrollframe> holding a <text>
    sf = tk.Frame(_help_window[0].top)
    txt = tk.Text(sf, background=_w(K.p_gui_help_window_color),
                  font=_w(p_gui_help_window_font), wrap="word", padx=10)
    sb = tk.Scrollbar(sf, orient="vertical", command=txt.yview)
    txt.configure(yscrollcommand=sb.set)
    sb.pack(side="right", fill="y")
    txt.pack(side="left", expand=True, fill="both")
    pack(sf, expand=True, fill="both")
    read_file(HELP_FILE, txt)
    txt.mark_set("insert", "1.0")
    txt.configure(state="disabled")


# ------------------------------------------------------------------------------------
# pop-up dialogs

# fg-color specifies the color of the dialog text.  If bg-color is #f
# the dialog text appears on a white background surrounded by a grey
# border, otherwise there is no border and the entire dialog
# background is bg-color.

def confirm_dialog(x, y, font, fg_color, bg_color, justify, message_, yes_label, no_label,
                   yes_action, no_action, destroy_action):
    """gui.ss: confirm-dialog"""
    tk = _tk()
    border_bg_color = bg_color if exists_p(bg_color) else colors.swl_color(chez.String("grey85"))
    dialog = SwlToplevel("Confirm", destroy_action, border_bg_color)
    top = dialog.top
    top.resizable(False, False)
    top.geometry(tell(setup.g_control_panel, "get-relative-position", x, y))
    message_label = tk.Label(top, text=str(message_), foreground=_w(fg_color),
                             background=_w(bg_color if exists_p(bg_color) else K.c_white),
                             justify=justify, font=_w(font))
    button_frame = tk.Frame(top, background=_w(border_bg_color))
    yes_button = tk.Button(button_frame, text=yes_label, command=lambda: yes_action(yes_button))
    no_button = tk.Button(button_frame, text=no_label, command=lambda: no_action(no_button))
    pack(yes_button, side="left")
    pack_hspace(button_frame, 20, "left", border_bg_color)
    pack(no_button, side="right")
    pack_vspace(top, 15, "top", border_bg_color)
    pack_vspace(top, 15, "bottom", border_bg_color)
    pack_hspace(top, 15, "left", border_bg_color)
    pack_hspace(top, 15, "right", border_bg_color)
    pack(message_label, side="top", fill="both")
    pack_vspace(top, 20, "top", border_bg_color)
    pack(button_frame, side="top")
    return dialog


class _InputField:
    """port: the input dialog's SWL <entry>, whose get-parent is the dialog"""

    def __init__(self, dialog, entry):
        self.dialog = dialog
        self.entry = entry

    def get_parent(self):
        return self.dialog

    def set_focus(self):
        self.entry.focus_set()


def input_dialog(x, y, default, message_, input_action, destroy_action):
    """gui.ss: input-dialog"""
    tk = _tk()
    dialog = SwlToplevel("Input", destroy_action)
    top = dialog.top
    top.resizable(False, False)
    top.geometry(tell(setup.g_control_panel, "get-relative-position", x, y))
    top_border = tk.Frame(top, width=200, height=15)
    message_label = tk.Label(top, text=message_, foreground=_w(K.c_black),
                             background=_w(K.c_white), font=_w(p_gui_input_dialog_font))

    def action(event):
        input_ = input_field.get()
        if input_ == "":
            dialog.destroy()
            return
        value = chez.string_to_number(input_)
        if value is False or value < 1:
            message_label.configure(foreground=_w(K.c_red), text="Invalid input!")

            def back():   # port: (pause 700) becomes a timer
                if not dialog.destroyed:
                    message_label.configure(foreground=_w(K.c_black), text=message_)
            top.after(700, back)
        else:
            input_action(value)
            dialog.destroy()
    input_field = tk.Entry(top, width=10, font=_w(p_gui_command_line_font),
                           background=_w(K.c_white))
    input_field.bind("<Return>", action)
    pack(top_border, side="top")
    pack_vspace(top, 15, "bottom")
    pack_hspace(top, 15, "left")
    pack_hspace(top, 15, "right")
    pack(message_label, side="top")
    pack_vspace(top, 20, "top")
    pack(input_field, side="top")
    if exists_p(default):
        input_field.insert(0, default)
        input_field.select_range(0, len(default))
    input_field.focus_set()
    return _InputField(dialog, input_field)


_breakpoint_input_field = [False]


def set_breakpoint_action(item):
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


def clear_breakpoint_action(item):
    """gui.ss: clear-breakpoint-action"""
    run.g_break_time = False
    tell(setup.g_control_panel, "clear-breakpoint-message")


_step_interval_input_field = [False]


def set_step_interval_action(item):
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


def _default_file_dialog(title, mode, default_dir):
    """port: SWL's swl:file-dialog (Tk's save dialog)"""
    from tkinter import filedialog
    name = filedialog.asksaveasfilename(title=title, initialdir=default_dir)
    return name if name else False


file_dialog = _default_file_dialog


def set_file_dialog(f):
    """port: replace swl:file-dialog (tests save without a modal dialog)"""
    global file_dialog
    file_dialog = f


# port: metacat.ss's *file-dialog-directory* ("~/Desktop/" in its comment)
g_file_dialog_directory = __import__("os").path.expanduser("~")


def save_commentary_action(item):
    """gui.ss: save-commentary-action"""
    import os
    filename = file_dialog("Save Commentary to File", "save", g_file_dialog_directory)
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

class _Slider:
    """port: create-slider's <frame>, with its <scale>"""

    def __init__(self, frame, scale):
        self.frame = frame
        self.scale = scale

    def pack(self, **options):
        self.frame.pack(**options)


def create_slider(parent, text, min_text, max_text, len_, init_val, color, slide_action):
    """gui.ss: create-slider"""
    tk = _tk()
    slider = tk.Frame(parent, background=_w(color))
    min_label = tk.Label(slider, text=min_text, font=_w(p_gui_speed_controls_italic_font),
                         background=_w(color))
    max_label = tk.Label(slider, text=max_text, font=_w(p_gui_speed_controls_italic_font),
                         background=_w(color))
    main_label = tk.Label(slider, text=text, font=_w(p_gui_speed_controls_font),
                          background=_w(color))
    scale = tk.Scale(slider, orient="horizontal", length=len_,
                     width=p_gui_slider_thickness, showvalue=0, background=_w(color),
                     activebackground=_w(color), highlightthickness=0)
    scale.set(init_val)
    # port: Tk passes the value as a string
    scale.configure(command=lambda value: slide_action(scale, int(float(value))))
    pack(scale, side="top", fill="both")
    pack(min_label, side="left", fill="both")
    pack(max_label, side="right", fill="both")
    pack(main_label, side="bottom", fill="both")
    return _Slider(slider, scale)


# ------------------------------------------------------------------------------
# speed controls

p_max_num_of_flashes = 5
p_max_flash_pause = 100
p_max_snag_pause = 5000
# port: the engine reads these two from metacat/view_globals.py; load() puts
# them there
p_text_scroll_pause = 20
p_codelet_highlight_pause = 100


def speed_slider_action(scale, value):
    """gui.ss: speed-slider-action"""
    def range_(low, high):
        return chez.max_(low, utilities.round_(chez.mul(utilities.percent(100 - value), high)))
    if value == 100:
        view_globals.p_num_of_flashes = 1
        view_globals.p_flash_pause = 1
        view_globals.p_snag_pause = 1
        view_globals.p_text_scroll_pause = 1
    else:
        view_globals.p_num_of_flashes = range_(2, p_max_num_of_flashes)
        view_globals.p_flash_pause = range_(10, p_max_flash_pause)
        view_globals.p_snag_pause = range_(250, p_max_snag_pause)
        view_globals.p_text_scroll_pause = 20


def load():
    """port: gui.ss's definitions of the speed settings the engine reads"""
    view_globals.p_text_scroll_pause = p_text_scroll_pause
    view_globals.p_codelet_highlight_pause = p_codelet_highlight_pause


# ------------------------------------------------------------------------------

DEMO_ITEMS = [
    ("Run 1:  abc -> abd; mrrjjj -> mrrjjjj", "run1"),
    ("Run 2:  xqc -> xqd; mrrjjj -> mrrkkk", "run2"),
    ("Run 3:  rst -> rsu; xyz -> uyz", "run3"),
    ("Run 4:  abc -> abd; xyz -> dyz", "run4"),
    ("Run 5:  xqc -> xqd; mrrjjj -> mrrjjjj", "run5"),
    ("Run 6:  eqe -> qeq; abbbc -> aaabccc", "run6"),
    ("Run 7:  abc -> abd; xyz -> ?", "run7"),
    ("Run 8:  eqe -> qeq; abbbc -> ?", "run8"),
]


def make_control_panel():
    """gui.ss: make-control-panel"""
    return ControlPanel()


class ControlPanel(SchemeObject):
    """gui.ss: make-control-panel (the control panel object)"""

    def __init__(this):
        tk = _tk()
        from metacat.gui import swl
        swl.swl_sync_display()
        select_control_panel_fonts()
        root = _root()
        D = demos
        white = K.c_white
        control_panel = tk.Toplevel(root)
        control_panel.title("Metacat Control Panel")
        control_panel.configure(background=_w(white))
        control_panel.resizable(False, False)
        # (destroy-request-handler: (lambda (toplevel) (exit)))
        control_panel.protocol("WM_DELETE_WINDOW", _exit)
        top_border = tk.Canvas(control_panel, width=350, height=5, highlightthickness=0,
                               background=_w(white))
        self_watching_warning_label = tk.Label(
            control_panel, text="Warning: Self-watching is disabled",
            font=_w(p_gui_header_font), foreground=_w(K.c_red), background=_w(white))
        info_label = tk.Label(control_panel, text="Please enter a problem:",
                              font=_w(p_gui_header_font), background=_w(white))
        command_line = tk.Entry(control_panel, width=40, font=_w(p_gui_command_line_font),
                                background=_w(K.p_gui_command_line_color),
                                disabledbackground=_w(K.p_gui_command_line_color))
        this.command_line_action = go_button_action
        command_line.bind("<Return>", lambda event: this.command_line_action(command_line))
        speed_controls = tk.Frame(control_panel,
                                  background=_w(K.p_gui_speed_controls_color))
        speed_slider = create_slider(speed_controls, "Speed", "Slow", "Fast",
                                     p_gui_slider_length, p_initial_speed,
                                     K.p_gui_speed_controls_color, speed_slider_action)

        def button(title, active, action):
            b = tk.Button(speed_controls, text=title, font=_w(p_gui_speed_controls_font),
                          activebackground=_w(active), highlightthickness=3,
                          highlightbackground=_w(K.p_gui_speed_controls_color),
                          state="disabled")
            b.configure(command=lambda: action(b))
            return b
        step_button = button("Step", K.c_green, step_button_action)
        go_button = button("Go", K.c_green, go_button_action)
        stop_button = button("Stop", K.c_red, stop_button_action)
        reset_button = button("Reset", K.c_green, reset_button_action)
        breakpoint_label = tk.Label(control_panel, text="", font=_w(p_gui_speed_controls_font),
                                    foreground=_w(K.c_red), background=_w(white))
        demos_menu = create_menu(
            *[demo_menu_item(t, getattr(D, n)) for t, n in DEMO_ITEMS],
            menu_item_separator(),
            create_submenu("Answer comparison and reminding",
                           demo_menu_item("abc / xyd", D.abc_xyd),
                           demo_menu_item("abc / wyz", D.abc_wyz),
                           demo_menu_item("abc / dyz", D.abc_dyz),
                           demo_menu_item("rst / xyu", D.rst_xyu),
                           demo_menu_item("rst / wyz", D.rst_wyz),
                           demo_menu_item("rst / uyz", D.rst_uyz),
                           demo_menu_item("abc / mrrkkk", D.abc_mrrkkk),
                           demo_menu_item("abc / mrrjjjj", D.abc_mrrjjjj),
                           demo_menu_item("xqc / mrrkkk", D.xqc_mrrkkk),
                           demo_menu_item("xqc / mrrjjjj", D.xqc_mrrjjjj),
                           demo_menu_item("eqe / baaab", D.eqe_baaab),
                           demo_menu_item("eqe / aaabaaa", D.eqe_aaabaaa),
                           demo_menu_item("eqe / qeeeq", D.eqe_qeeeq),
                           demo_menu_item("eqe / aaabccc", D.eqe_aaabccc)),
            menu_item_separator(),
            create_submenu("Implausible rules",
                           demo_menu_item(figure(5, 4, "top"), D.fig5_4_top),
                           demo_menu_item(figure(5, 4, "bottom"), D.fig5_4_bottom),
                           demo_menu_item(figure(5, 5, "top"), D.fig5_5_top),
                           demo_menu_item(figure(5, 5, "bottom"), D.fig5_5_bottom)),
            create_submenu("Poor thematic characterizations",
                           demo_menu_item(figure(5, 7), D.fig5_7),
                           demo_menu_item(figure(5, 8), D.fig5_8),
                           demo_menu_item(figure(5, 10), D.fig5_10),
                           demo_menu_item(figure(5, 11), D.fig5_11)),
            create_submenu("Other sample runs",
                           demo_menu_item("abc -> cba; mrrjjj -> mmmrrj", D.misc1),
                           demo_menu_item("abc -> abd; ijk -> abd", D.misc2),
                           demo_menu_item("abc -> aabbcc; kkjjii -> ?", D.misc3),
                           demo_menu_item("a -> b; z -> ?", D.misc4),
                           demo_menu_item("abc -> abd; glz -> ?", D.misc5)))
        window_controllers = [
            window_controller("Workspace", setup.g_workspace_window, True),
            window_controller("Slipnet", setup.g_slipnet_window, True),
            window_controller("Coderack", setup.g_coderack_window, True),
            window_controller("Temperature", setup.g_temperature_window, True),
            window_controller("Temporal Trace", setup.g_trace_window, True),
            window_controller("Commentary", setup.g_comment_window, True),
            window_controller("Episodic Memory", setup.g_memory_window, True),
            window_controller("Top Themes", setup.g_top_themes_window, True),
            window_controller("Bottom Themes", setup.g_bottom_themes_window, True),
            window_controller("Vertical Themes", setup.g_vertical_themes_window, True),
            window_controller("EEG", setup.g_EEG_window, False),
            window_controller("Logo", fonts.g_mcat_logo, False)]
        windows_menu = create_windows_menu(window_controllers)
        theme_window_controllers = window_controllers[7:10]   # (sublist ... 7 10)
        ss = fonts.sans_serif
        # these menus assume %comment-window-font% is sans-serif 12 (bold italic)
        options__comment_font_face_menu = create_menu(
            comment_font_menu_item(False, "serif", fonts.serif, 12),
            comment_font_menu_item(False, "serif italic", fonts.serif, 12, "italic"),
            comment_font_menu_item(False, "serif bold", fonts.serif, 12, "bold"),
            comment_font_menu_item(False, "serif bold italic", fonts.serif, 12, "bold", "italic"),
            menu_item_separator(),
            comment_font_menu_item(False, "sans-serif", ss, 12),
            comment_font_menu_item(False, "sans-serif italic", ss, 12, "italic"),
            comment_font_menu_item(False, "sans-serif bold", ss, 12, "bold"),
            comment_font_menu_item(True, "sans-serif bold italic", ss, 12, "bold", "italic"),
            menu_item_separator(),
            comment_font_menu_item(False, "fancy", fonts.fancy, 12),
            comment_font_menu_item(False, "fancy italic", fonts.fancy, 12, "italic"),
            comment_font_menu_item(False, "fancy bold", fonts.fancy, 12, "bold"),
            comment_font_menu_item(False, "fancy bold italic", fonts.fancy, 12, "bold", "italic"))
        options__comment_font_size_menu = create_menu(
            comment_font_menu_item(False, "tiny", ss, 8, "bold", "italic"),
            comment_font_menu_item(False, "small", ss, 10, "bold", "italic"),
            comment_font_menu_item(True, "medium", ss, 12, "bold", "italic"),
            comment_font_menu_item(False, "large", ss, 18, "bold", "italic"),
            comment_font_menu_item(False, "larger", ss, 24, "bold", "italic"),
            comment_font_menu_item(False, "huge", ss, 34, "bold", "italic"))
        clamp_themes_menu_item = menu_item(
            "Clamp theme pattern",
            lambda item: tell(setup.g_control_panel, "theme-edit-mode-on"))
        options__clamp_codelets_menu = create_submenu(
            "Clamp codelet pattern",
            clamp_codelets_menu_item("Top-down codelet pattern", "top-down"),
            clamp_codelets_menu_item("Bottom-up codelet pattern", "bottom-up"),
            clamp_codelets_menu_item("Rule codelet pattern", "rule"),
            clamp_codelets_menu_item("Bridge codelet pattern", "bridge"),
            clamp_codelets_menu_item("Group codelet pattern", "group"))
        undo_clamp_menu_item = menu_item(
            "Undo last clamp", lambda item: tell(_metacat.trace.g_trace, "undo-last-clamp"))

        def self_watching(item):
            setup.p_self_watching_enabled = not setup.p_self_watching_enabled
            if setup.p_self_watching_enabled:
                hide(self_watching_warning_label)
                clamp_themes_menu_item.set_enabled_bang(True)
                options__clamp_codelets_menu.set_enabled_bang(True)
                undo_clamp_menu_item.set_enabled_bang(True)
                for controller in theme_window_controllers:
                    tell(controller, "show")
            else:
                show(self_watching_warning_label)
                clamp_themes_menu_item.set_enabled_bang(False)
                options__clamp_codelets_menu.set_enabled_bang(False)
                undo_clamp_menu_item.set_enabled_bang(False)
                tell(_metacat.trace.g_trace, "undo-last-clamp")
                _metacat.themes.delete_themes()
                for controller in theme_window_controllers:
                    tell(controller, "hide")
        self_watching_mode_menu_item = check_menu_item(
            "Self-watching mode", setup.p_self_watching_enabled, self_watching)

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
        options_menu = create_options_menu(
            menu_item("Set breakpoint", set_breakpoint_action),
            menu_item("Clear breakpoint", clear_breakpoint_action),
            menu_item("Step mode interval", set_step_interval_action),
            menu_item_separator(),
            check_menu_item("Eliza mode", setup.p_eliza_mode, eliza),
            check_menu_item("Slipnet graphics", setup.p_slipnet_graphics, slipnet_graphics),
            check_menu_item("Coderack graphics", setup.p_coderack_graphics, coderack_graphics),
            check_menu_item("Show codelet counts", setup.p_codelet_count_graphics,
                            codelet_counts),
            check_menu_item("Show last codelet type", setup.p_highlight_last_codelet,
                            last_codelet),
            self_watching_mode_menu_item,
            check_menu_item("Verbose mode", setup.p_verbose,
                            lambda item: tell(setup.g_control_panel, "toggle-verbose-mode")),
            menu_item_separator(),
            clamp_themes_menu_item,
            options__clamp_codelets_menu,
            undo_clamp_menu_item,
            menu_item_separator(),
            submenu_anchor("Commentary font face", options__comment_font_face_menu),
            submenu_anchor("Commentary font size", options__comment_font_size_menu),
            menu_item("Save commentary to file", save_commentary_action))
        if sgl.g_platform == "macintosh":
            main_menu = create_menu(
                submenu_anchor("Demos", demos_menu, p_gui_menubar_font),
                submenu_anchor("Windows", windows_menu, p_gui_menubar_font),
                submenu_anchor("Options", options_menu, p_gui_menubar_font))
        else:
            main_menu = create_menu(
                menu_item("Help", help_action, p_gui_menubar_font),
                submenu_anchor("Demos", demos_menu, p_gui_menubar_font),
                submenu_anchor("Windows", windows_menu, p_gui_menubar_font),
                submenu_anchor("Options", options_menu, p_gui_menubar_font),
                nop_menu_item(),
                clear_memory_menu_item("Clear Memory", p_gui_menubar_font))
        control_panel.configure(menu=main_menu.realize(control_panel))   # set-menu!
        pack(top_border, fill="x")
        pack_hspace(control_panel, 15, "left", white)
        pack_hspace(control_panel, 15, "right", white)
        pack(info_label)
        pack_vspace(control_panel, 5, "top", white)
        pack(command_line)
        pack_vspace(control_panel, 15, "top", white)
        pack(speed_slider.frame, side="left", anchor="n")
        pack_hspace(speed_controls, 10, "left", K.p_gui_speed_controls_color)
        pack(step_button, side="left", anchor="n")
        pack(go_button, side="left", anchor="n")
        pack(stop_button, side="left", anchor="n")
        pack(reset_button, side="left", anchor="n")
        show(speed_controls)
        pack(breakpoint_label)
        pack(self_watching_warning_label)
        if setup.p_self_watching_enabled:
            hide(self_watching_warning_label)
        else:
            clamp_themes_menu_item.set_enabled_bang(False)
            options__clamp_codelets_menu.set_enabled_bang(False)
            undo_clamp_menu_item.set_enabled_bang(False)
        set_comment_font_menu_actions(options__comment_font_face_menu,
                                      options__comment_font_size_menu)
        # port: the slider's initial value sets the speed, as Tk's scale command
        # does once the scale exists
        speed_slider_action(speed_slider.scale, p_initial_speed)
        command_line.focus_set()
        this.root = root
        this.control_panel = control_panel
        this.top_border = top_border
        this.self_watching_warning_label = self_watching_warning_label
        this.info_label = info_label
        this.command_line = command_line
        this.step_button, this.go_button = step_button, go_button
        this.stop_button, this.reset_button = stop_button, reset_button
        this.breakpoint_label = breakpoint_label
        this.speed_slider = speed_slider
        this.demos_menu = demos_menu
        this.window_controllers = window_controllers
        this.main_menu = main_menu
        this.demos_button = get_demos_button(main_menu)
        this.options_button = get_options_button(main_menu)
        this.clearmem_button = get_clearmem_button(main_menu)
        this.clearmem_dialog = False
        this.theme_edit_dialog = False
        this.edited_theme_types = []
        this.saved_theme_states = []
        this.verbose_mode_p = setup.p_verbose
        this.problem = False
        # port: for tests
        this.widgets = {
            "frame": control_panel, "info-label": info_label, "command-line": command_line,
            "speed-slider": speed_slider.scale, "step-button": step_button,
            "go-button": go_button, "stop-button": stop_button, "reset-button": reset_button,
            "breakpoint-label": breakpoint_label,
            "self-watching-warning-label": self_watching_warning_label,
            "main-menu": main_menu.tk_menu, "demos-menu": demos_menu.tk_menu,
            "windows-menu": windows_menu.tk_menu, "options-menu": options_menu.tk_menu,
            "clamp-codelets-menu": options__clamp_codelets_menu.get_menu().tk_menu,
            "comment-font-size-menu": options__comment_font_size_menu.tk_menu,
            "comment-font-face-menu": options__comment_font_face_menu.tk_menu,
            "window-controllers": window_controllers}

    # control panel object:
    @message("object-type")
    def object_type(this, self):
        return "control-panel"

    @message("get-widgets")
    def get_widgets(this, self):
        """port: the widgets, for tests and the window layout"""
        return this.widgets

    @message("problem-exists?")
    def problem_exists_p(this, self):
        return exists_p(this.problem)

    @message("get-current-problem")
    def get_current_problem(this, self):
        return this.problem

    @message("set-position")
    def set_position(this, self, x, y):
        this.control_panel.geometry(chez.format_("+~a+~a", x, y))

    @message("get-relative-position")
    def get_relative_position(this, self, x_offset, y_offset):
        geometry = this.control_panel.geometry()
        len_ = len(geometry)
        ix = geometry.index("+")
        iy = ix + 1 + geometry[ix + 1:len_].index("+")
        x = chez.string_to_number(geometry[ix:iy])
        y = chez.string_to_number(geometry[iy:len_])
        return chez.format_("+~a+~a", x + x_offset, y + y_offset)

    @message("get-command-line-string")
    def get_command_line_string(this, self):
        return this.command_line.get()

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
        this.command_line.delete(0, "end")
        unhighlight_menu_items(this.demos_menu)

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
        thread_break(setup.g_repl_thread, False, thunk)

    @message("run-new-problem")
    def run_new_problem(this, self, tokens):
        tell(self, "update-current-problem", tokens)
        tell(self, "switch-to-run-mode")
        problem = this.problem

        def thunk():
            run.init_mcat(*problem)
            run.run_mcat()
        thread_break(setup.g_repl_thread, False, thunk)

    @message("resume-current-problem")
    def resume_current_problem(this, self):
        if not exists_p(this.problem):
            return tell(self, "display-error", "No current problem!")
        if run.g_display_mode_p:
            view_globals.restore_current_state()
        thread_break(setup.g_repl_thread, False, run.go)

    @message("reset-current-problem")
    def reset_current_problem(this, self):
        if not exists_p(this.problem):
            return tell(self, "display-error", "No current problem!")
        problem = this.problem

        def thunk():
            run.init_mcat(*problem)
            run.quiet_break()
            run.run_mcat()
        thread_break(setup.g_repl_thread, False, thunk)

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
        # port: Tk 8.4+ draws a disabled entry in its disabled colours, which
        # SWL's Tk did not have: they follow the entry's own
        this.command_line.configure(font=_w(font), foreground=_w(fg),
                                    disabledforeground=_w(fg), background=_w(bg),
                                    disabledbackground=_w(bg), justify=justify)

    def _enable_all(this, command_line, step, go, stop, reset, menus):
        set_enabled(this.command_line, command_line)
        set_enabled(this.step_button, step)
        set_enabled(this.go_button, go)
        set_enabled(this.stop_button, stop)
        set_enabled(this.reset_button, reset)
        this.demos_button.set_enabled_bang(menus)
        this.options_button.set_enabled_bang(menus)
        this.clearmem_button.set_enabled_bang(menus)
        this.stop_button.configure(relief="raised")

    @message("switch-to-run-mode")
    def switch_to_run_mode(this, self):
        this.command_line.delete(0, "end")
        this._set_command_line_look(p_gui_run_mode_font, K.c_green, K.c_black, "center")
        this.command_line.insert(0, "running...")
        this.command_line_action = nop_event_handler
        this._enable_all(False, False, False, True, False, False)

    @message("switch-to-input-mode")
    def switch_to_input_mode(this, self):
        set_enabled(this.command_line, True)
        this.command_line_action = go_button_action
        this.command_line.delete(0, "end")
        this._set_command_line_look(p_gui_command_line_font, K.c_black,
                                    K.p_gui_command_line_color, "left")
        this._enable_all(True, True, True, False, True, True)

    @message("switch-to-disabled-mode")
    def switch_to_disabled_mode(this, self):
        this.command_line_action = nop_event_handler
        this._enable_all(False, False, False, False, False, False)

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

    @message("display-breakpoint-message")
    def display_breakpoint_message(this, self):
        this.breakpoint_label.configure(
            text=chez.format_("Breakpoint set for time step ~a", run.g_break_time))

    @message("clear-breakpoint-message")
    def clear_breakpoint_message(this, self):
        this.breakpoint_label.configure(text="")

    @message("display")
    def display(this, self, message_):
        this.info_label.configure(text=message_)

    @message("display-error")
    def display_error(this, self, message_):
        current_message = this.info_label.cget("text")
        current_width = this.top_border.winfo_width()
        # temporarily freeze the top border at the current width to
        # avoid window shrinkage in case title is longer than usual
        this.top_border.configure(width=current_width)
        this.info_label.configure(foreground=_w(K.c_red), text=message_)

        def back():   # port: (pause 700) in the GUI thread becomes a Tk timer
            this.info_label.configure(foreground=_w(K.c_black), text=current_message)
            this.top_border.configure(width=350)
        this.control_panel.after(700, back)

    @message("engine-error")
    def engine_error(this, self, message_):
        """port: an error in the model, which ended the engine thread's thunk (in the
        original it went to the REPL, leaving the control panel in run mode)"""
        tell(self, "switch-to-input-mode")
        tell(self, "display", chez.format_("Error: ~a", message_))

    @message("hide-window")
    def hide_window(this, self, toplevel):
        for controller in this.window_controllers:
            if tell(controller, "get-toplevel") is toplevel:
                tell(controller, "hide")

    @message("raise")
    def raise_(this, self):
        this.control_panel.lift()
        if exists_p(this.clearmem_dialog):
            this.clearmem_dialog.raise_()

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def _exit():
    """port: (exit) from the control panel's destroy-request handler: Tk's main
    loop ends, and with it python3 -m metacat.gui"""
    root = _root()
    if root:
        root.quit()
    else:
        sys.exit(0)


# ------------------------------------------------------------------------------------
# menus

def create_menu(*items):
    """gui.ss: create-menu"""
    return Menu(items)


def create_submenu(text, *items):
    """gui.ss: create-submenu"""
    return submenu_anchor(text, create_menu(*items))


def submenu_anchor(text, submenu, *font):
    """gui.ss: submenu-anchor"""
    item = MenuItem("cascade", label=text, background=_menu_bg(),
                    font=p_gui_menu_item_font if not font else font[0])
    item.submenu = submenu
    return item


def menu_item_separator():
    """gui.ss: menu-item-separator"""
    return MenuItem("separator")


def nop_menu_item():
    """gui.ss: nop-menu-item"""
    return MenuItem("command", label="  ", background=_menu_bg(), state="disabled")


def menu_item(text, action_proc, *font):
    """gui.ss: menu-item"""
    item = MenuItem("command", label=text, background=_menu_bg(),
                    font=p_gui_menu_item_font if not font else font[0])
    item.action = action_proc
    return item


def check_menu_item(text, selected_p, action_proc):
    """gui.ss: check-menu-item"""
    item = MenuItem("check", label=text,
                    foreground=(K.p_gui_menu_item_on_color if selected_p
                                else K.p_gui_menu_item_off_color),
                    background=_menu_bg(), selectcolor=K.p_gui_checkbox_select_color,
                    font=p_gui_menu_item_font)
    item.selected = bool(selected_p)

    def action(item):
        item.set_foreground_color_bang(K.p_gui_menu_item_on_color if item.get_selected()
                                       else K.p_gui_menu_item_off_color)
        action_proc(item)
    item.action = action
    return item


def clear_memory_menu_item(text, font):
    """gui.ss: clear-memory-menu-item"""
    item = MenuItem("command", label=text, font=font, background=_menu_bg(),
                    activeforeground=K.c_red)
    item.action = lambda item: tell(setup.g_control_panel, "clear-memory")
    return item


def demo_menu_item(text, problem):
    """gui.ss: demo-menu-item"""
    item = MenuItem("command", label=text, background=_menu_bg(), font=p_gui_menu_item_font)

    def action(item):
        tell(setup.g_control_panel, "init-new-problem", problem, False)
        set_menu_item_color(item)
    item.action = action
    return item


# in Mac OS X, periods in menu labels don't show up for some reason
def figure(m, n, *opt):
    """gui.ss: figure"""
    separator = "-" if sgl.g_platform == "macintosh" else "."
    return chez.format_("Figure ~a~a~a~a", m, separator, n,
                        "" if not opt else chez.format_(" (~a)", opt[0]))


def get_demos_button(main_menu):
    """gui.ss: get-demos-button"""
    if sgl.g_platform == "macintosh":
        return main_menu.get_menu_items()[0]
    return main_menu.get_menu_items()[1]


def get_options_button(main_menu):
    """gui.ss: get-options-button"""
    if sgl.g_platform == "macintosh":
        return main_menu.get_menu_items()[2]
    return main_menu.get_menu_items()[3]


def get_clearmem_button(main_menu):
    """gui.ss: get-clearmem-button"""
    if sgl.g_platform == "macintosh":
        return main_menu.get_menu_items()[2].get_menu().get_menu_items()[0]
    return main_menu.get_menu_items()[5]


def create_options_menu(*items):
    """gui.ss: create-options-menu"""
    if sgl.g_platform == "macintosh":
        all_options = [clear_memory_menu_item("Clear Memory", p_gui_menu_item_font),
                       menu_item("Help", help_action, p_gui_menu_item_font), *items]
    else:
        all_options = list(items)
    return create_menu(*all_options)


def create_windows_menu(window_controllers):
    """gui.ss: create-windows-menu"""
    def show_all(item):
        for controller in window_controllers:
            tell(controller, "show")

    def hide_all(item):
        for controller in window_controllers:
            tell(controller, "hide")
    all_menu_items = (utilities.tell_all(window_controllers, "get-menu-item")
                      + [menu_item_separator(),
                         menu_item("Show all windows", show_all),
                         menu_item("Hide all windows", hide_all)])
    for controller in window_controllers:
        tell(controller, "initialize")
    return Menu(all_menu_items)


def window_controller(text, window, visible_p):
    """gui.ss: window-controller"""
    return WindowController(text, window, visible_p)


class WindowController(SchemeObject):
    """gui.ss: window-controller (the closure)"""

    def __init__(this, text, window, visible_p):
        this.text = text
        this.window = window
        this.visible_p = visible_p
        if window is fonts.g_mcat_logo:
            # (send (send *mcat-logo* get-parent) get-parent): the logo's toplevel
            this.toplevel = window.widget.winfo_toplevel()
        else:
            this.toplevel = tell(window, "get-toplevel")
        this.menu_item = MenuItem("command", font=p_gui_menu_item_font,
                                  background=_menu_bg())

    @message("object-type")
    def object_type(this, self):
        return "window-controller"

    @message("get-menu-item")
    def get_menu_item(this, self):
        return this.menu_item

    @message("get-toplevel")
    def get_toplevel(this, self):
        return this.toplevel

    @message("visible?")
    def visible_p_(this, self):
        """port: for tests"""
        return this.visible_p

    @message("initialize")
    def initialize(this, self):
        this.menu_item.set_action_bang(lambda item: tell(self, "toggle"))
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
        if this.visible_p:
            this.menu_item.set_title_bang(chez.format_("Hide ~a", this.text))
            this.menu_item.set_foreground_color_bang(K.p_gui_menu_item_on_color)
            this.menu_item.set_active_foreground_color_bang(K.p_gui_menu_item_on_color)
            if this.window is not fonts.g_mcat_logo:
                tell(this.window, "restore-position")
            show(this.toplevel)
        else:
            this.menu_item.set_title_bang(chez.format_("Show ~a", this.text))
            this.menu_item.set_foreground_color_bang(K.p_gui_menu_item_off_color)
            this.menu_item.set_active_foreground_color_bang(K.p_gui_menu_item_off_color)
            if this.window is not fonts.g_mcat_logo:
                tell(this.window, "remember-position")
            hide(this.toplevel)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def comment_font_menu_item(highlight_p, text, face, size, *style):
    """gui.ss: comment-font-menu-item"""
    item = MenuItem("command", background=_menu_bg(),
                    font=fonts.swl_font(face, size, list(style)), label=text)
    if highlight_p:
        set_menu_item_color(item)
    return item


def set_comment_font_menu_actions(face_menu, size_menu):
    """gui.ss: set-comment-font-menu-actions"""
    from metacat.gui import views

    def face_action(item):
        f = item.get_font()
        font = fonts.make_mfont(f.face, f.size, f.style)
        views.set_view_global("%comment-window-font%", font)
        tell(setup.g_comment_window, "new-font", font)
        update_menu_fonts(size_menu, f.face, "same", f.style)
        unhighlight_menu_items(face_menu)
        set_menu_item_color(item)

    def size_action(item):
        f = item.get_font()
        font = fonts.make_mfont(f.face, f.size, f.style)
        views.set_view_global("%comment-window-font%", font)
        tell(setup.g_comment_window, "new-font", font)
        update_menu_fonts(face_menu, "same", f.size, "same")
        unhighlight_menu_items(size_menu)
        set_menu_item_color(item)
    for face_item in face_menu.get_menu_items():
        if not isa_separator_p(face_item):
            face_item.set_action_bang(face_action)
    for size_item in size_menu.get_menu_items():
        size_item.set_action_bang(size_action)


def update_menu_fonts(menu, new_face, new_size, new_style):
    """gui.ss: update-menu-fonts"""
    for item in menu.get_menu_items():
        if not isa_separator_p(item):
            font = item.get_font()
            face = font.face if new_face == "same" else new_face
            size = font.size if new_size == "same" else new_size
            style = font.style if new_style == "same" else new_style
            item.set_font_bang(fonts.swl_font(face, size, list(style)))


def unhighlight_menu_items(menu):
    """gui.ss: unhighlight-menu-items"""
    for item in menu.get_menu_items():
        if isa_cascade_p(item):
            unhighlight_menu_items(item.get_menu())
        else:
            item.set_background_color_bang(_menu_bg())


def clamp_codelets_pattern(structure_type):
    """gui.ss: clamp-codelets-menu-item's case on the structure type"""
    trace = _metacat.trace
    if structure_type == "top-down":
        return trace.p_top_down_codelet_pattern
    if structure_type == "bottom-up":
        return trace.p_bottom_up_codelet_pattern
    if structure_type == "group":
        return trace.against_background(_metacat.coderack.p_very_low_urgency, trace.p_group_codelet_pattern)
    if structure_type == "bridge":
        return trace.against_background(_metacat.coderack.p_very_low_urgency,
                                        trace.p_bridge_codelet_pattern)
    if structure_type == "rule":
        return trace.against_background(_metacat.coderack.p_very_low_urgency, trace.p_rule_codelet_pattern)
    return None


def clamp_codelets_menu_item(text, structure_type):
    """gui.ss: clamp-codelets-menu-item"""
    pattern = clamp_codelets_pattern(structure_type)

    def action(item):
        trace = _metacat.trace.g_trace
        if not tell(setup.g_control_panel, "problem-exists?"):
            return tell(setup.g_control_panel, "display-error", "No current problem!")
        clamp_event = _metacat.trace.make_clamp_event(
            "manual-clamp", [pattern], [],
            "workspace" if structure_type in ("top-down", "bottom-up", "bridge")
            else structure_type)
        if setup.p_coderack_graphics:
            tell(setup.g_coderack_window, "raise-window")
        tell(trace, "undo-last-clamp")
        tell(trace, "add-event", clamp_event)
        tell(clamp_event, "activate")
    return menu_item(text, action)
