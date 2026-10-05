"""Inventory the live tkinter GUI, under Xvfb (loop0003 item 00).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  env -u WAYLAND_DISPLAY xvfb-run -a -s "-screen 0 1920x1200x24" \\
      python3 python/tests/tk_gui_inventory.py python/tests/data/tk-gui-inventory.json

Never on the owner's screen.  The GUI is made as `python3 -m metacat.gui` makes it
(metacat.gui.app.setup); the script then walks what Tk actually has:

- every graphics window: its title, size, position, whether it is resizable
  (and its aspect ratio and minimum size), its scrolling, the module that draws
  it, the Tk bindings on its canvas and the viewport's left and right press
  handlers (shift-left goes to the right one, sgl.Viewport.mouse_press);
- the control panel's widgets, and the state of the command line, the buttons
  and the menus in each run state (initial, input, run, disabled, and with
  self-watching off), with the command line's Enter action;
- the speed slider and what its action sets at a few values;
- every menu, item by item (kind, label, state, font, colours, and what it does:
  its action, a demo's problem, a font, a window), from SWL's menu-item objects
  and their Tk entries;
- the dialogs the menus open (Help, the two input dialogs, Clear Memory, the
  theme-clamp dialog, the commentary file dialog), opened and walked, then
  closed;
- every application keyboard and mouse binding on every widget.

It writes the inventory as JSON (sorted keys) and exits by itself.  Values that
depend on the machine's fonts (the faces fonts.load() picked) are kept under
keys named "font"; tests/test_tk_gui_inventory.py ignores them when it compares
a fresh inventory with the committed one.
"""
from __future__ import annotations

import json
import os
import sys
import threading
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))

OUT = Path(sys.argv[1]) if len(sys.argv) > 1 else HERE / "data" / "tk-gui-inventory.json"

from metacat import engine, run, setup, view_globals  # noqa: E402
from metacat.objects import tell  # noqa: E402


def watchdog(secs=300):
    def kill():
        time.sleep(secs)
        sys.stderr.write("tk_gui_inventory: watchdog\n")
        os._exit(3)
    threading.Thread(target=kill, daemon=True).start()


watchdog()
engine.load()
from metacat.gui import app, constants as K, fonts, gui, sgl  # noqa: E402

app.setup()
root = app.g_root
CP = setup.g_control_panel
W = tell(CP, "get-widgets")


def pump(secs=0.3):
    t = time.time()
    while time.time() - t < secs:
        root.update()
        time.sleep(0.01)


def wait_idle(secs=60):
    t = time.time()
    while app.engine_busy_p():
        root.update()
        time.sleep(0.01)
        if time.time() - t > secs:
            raise RuntimeError("the engine did not stop")
    pump(0.2)


def plain(x):
    """a JSON value for a Tk option or a Scheme value"""
    if isinstance(x, (list, tuple)):
        return [plain(a) for a in x]
    if isinstance(x, (bool, int, float)) or x is None:
        return x
    if isinstance(x, fonts.SwlFont):
        return [str(x.face), x.size, *[str(s) for s in x.style]]
    return str(x)


def fn_name(f):
    if f is None or f is False:
        return None
    return "%s.%s" % (getattr(f, "__module__", "?"), getattr(f, "__qualname__", repr(f)))


def closure(f):
    """a function's free variables by name"""
    code = getattr(f, "__code__", None)
    if code is None or not f.__closure__:
        return {}
    out = {}
    for name, cell in zip(code.co_freevars, f.__closure__):
        try:
            out[name] = cell.cell_contents
        except ValueError:
            pass
    return out


def bindings(widget):
    """the widget's own Tk bindings (sequence -> the Python callback's name)"""
    out = {}
    for seq in widget.bind():
        script = widget.bind(seq)
        out[seq] = "python callback" if "if {" in script or "]" in script else script
    return out


# --- the graphics windows ------------------------------------------------------------

WINDOW_GLOBALS = [
    ("Workspace", "g_workspace_window"), ("Slipnet", "g_slipnet_window"),
    ("Coderack", "g_coderack_window"), ("Temperature", "g_temperature_window"),
    ("Temporal Trace", "g_trace_window"), ("Commentary", "g_comment_window"),
    ("Episodic Memory", "g_memory_window"), ("Top Themes", "g_top_themes_window"),
    ("Bottom Themes", "g_bottom_themes_window"),
    ("Vertical Themes", "g_vertical_themes_window"), ("EEG", "g_EEG_window")]


def graphics_window(window):
    """the GraphicsWindow under a panel object (panels delegate to it)"""
    seen = set()
    w = window
    while id(w) not in seen:
        seen.add(id(w))
        if type(w).__name__ == "GraphicsWindow":
            return w
        w = getattr(w, "graphics_window", None) or getattr(w, "text_window", None)
        if w is None:
            break
    raise TypeError("no graphics window in %r" % (window,))


def resize_method(panel):
    """the resize message of the panel or of what it delegates to (a text window)"""
    w = panel
    while w is not None:
        if "resize" in type(w).__dict__:
            return fn_name(type(w).__dict__["resize"])
        if type(w).__name__ == "GraphicsWindow":
            return None
        w = getattr(w, "graphics_window", None) or getattr(w, "text_window", None)
    return None


def window_entry(name, panel, controller):
    gw = graphics_window(panel)
    host = gw.top
    top = host.top
    vp = gw.vp
    aspect = top.aspect()
    entry = {
        "name": name,
        "global": None,
        "title": top.title(),
        "panel_class": "%s.%s" % (type(panel).__module__, type(panel).__name__),
        "draws_in": type(panel).__module__,
        "default_size": [gw.visible_w, gw.visible_h] if isinstance(gw.visible_w, int)
        else plain([gw.visible_w, gw.visible_h]),
        "canvas_size": plain([gw.canvas_w, gw.canvas_h]),
        "geometry": top.geometry(),
        "scrolling": gw.scrolling,
        "scrollbars": sorted(host.scrollbars),
        "resizable": list(top.resizable()),
        "min_size": list(top.minsize()),
        "aspect": list(aspect) if aspect else None,
        "resize_method": resize_method(panel),
        "visible_at_start": bool(tell(controller, "visible?")),
        "canvas_bindings": bindings(host.widget),
        "left_press": fn_name(vp.left_press_handler),
        "right_press": fn_name(vp.right_press_handler),
        "shift_left_press": fn_name(vp.right_press_handler),
        "background": plain(host.canvas.get_background_color()),
    }
    if entry["left_press"] == "metacat.gui.sgl.nop_event_handler":
        entry["left_press"] = None
    for k in ("right_press", "shift_left_press"):
        if entry[k] == "metacat.gui.sgl.nop_event_handler":
            entry[k] = None
    for k in ("left_press", "right_press", "shift_left_press"):
        h = getattr(vp, {"left_press": "left_press_handler"}.get(k, "right_press_handler"))
        cl = closure(h)
        if "theme_type" in cl and entry[k]:
            entry[k] += "(%s)" % cl["theme_type"]
    return entry


def windows():
    controllers = {tell(c, "get-menu-item").get_title().split(" ", 1)[1]: c
                   for c in W["window-controllers"]}
    out = []
    for name, g in WINDOW_GLOBALS:
        e = window_entry(name, getattr(setup, g), controllers[name])
        e["global"] = g
        out.append(e)
    logo = fonts.g_mcat_logo.widget.winfo_toplevel()
    out.append({"name": "Logo", "global": "fonts.g_mcat_logo", "title": logo.title(),
                "draws_in": "metacat.gui.fonts", "geometry": logo.geometry(),
                "resizable": list(logo.resizable()),
                "visible_at_start": bool(tell(controllers["Logo"], "visible?")),
                "canvas_bindings": bindings(fonts.g_mcat_logo.widget)})
    return out


# --- menus -------------------------------------------------------------------------

def item_does(item):
    """what a menu item does, from its action closure"""
    a = item.action
    name = fn_name(a)
    cl = closure(a)
    if name is None:
        return {"action": None}
    if name.endswith("check_menu_item.<locals>.action"):
        return {"action": fn_name(cl["action_proc"])}
    if name.endswith("demo_menu_item.<locals>.action"):
        return {"action": "init-new-problem", "problem": plain(cl["problem"])}
    if name.endswith("clamp_codelets_menu_item.<locals>.action"):
        return {"action": "clamp codelet pattern", "structure_type": cl["structure_type"]}
    if name.endswith("WindowController.initialize.<locals>.<lambda>"):
        return {"action": "toggle window"}
    if name.endswith("clear_memory_menu_item.<locals>.<lambda>"):
        return {"action": "clear-memory"}
    return {"action": name}


def menu_tree(menu):
    out = []
    for item in menu.get_menu_items():
        tkm, i = item.menu, item.index
        e = {"kind": item.kind}
        if item.kind != "separator":
            e["label"] = tkm.entrycget(i, "label")
            e["state"] = str(tkm.entrycget(i, "state"))
            e["foreground"] = str(tkm.entrycget(i, "foreground"))
            e["background"] = str(tkm.entrycget(i, "background"))
            e["activeforeground"] = str(tkm.entrycget(i, "activeforeground"))
            e["font"] = plain(item.font) if item.font else None
            e["accelerator"] = str(tkm.entrycget(i, "accelerator"))
        if item.kind == "check":
            e["selected"] = bool(item.variable.get())
        if item.kind == "cascade":
            e["items"] = menu_tree(item.submenu)
        elif item.kind != "separator":
            e.update(item_does(item))
        out.append(e)
    return out


def menu_states(menu):
    """label -> state, over the whole tree"""
    out = {}
    for item in menu.get_menu_items():
        if item.kind == "separator":
            continue
        label = item.menu.entrycget(item.index, "label")
        out[label] = str(item.menu.entrycget(item.index, "state"))
        if item.kind == "cascade":
            for k, v in menu_states(item.submenu).items():
                out[label + " > " + k] = v
    return out


# --- the control panel -------------------------------------------------------------

def widget_tree(w):
    e = {"class": w.winfo_class(), "path": str(w), "mapped": bool(w.winfo_ismapped())}
    for opt in ("text", "state", "width", "height", "relief", "justify", "orient",
                "from", "to", "length", "showvalue", "resolution"):
        try:
            e[opt] = plain(w.cget(opt))
        except Exception:   # noqa: BLE001 - not an option of this widget
            pass
    try:
        e["font"] = plain(w.cget("font"))
    except Exception:   # noqa: BLE001
        pass
    for opt in ("background", "foreground"):
        try:
            e[opt] = str(w.cget(opt))
        except Exception:   # noqa: BLE001
            pass
    b = bindings(w)
    if b:
        e["bindings"] = b
    kids = [widget_tree(k) for k in w.winfo_children() if k.winfo_class() != "Menu"]
    if kids:
        e["children"] = kids
    return e


CONTROLS = ["command-line", "step-button", "go-button", "stop-button", "reset-button",
            "speed-slider"]


def control_state():
    cl = W["command-line"]
    out = {k: str(W[k].cget("state")) for k in CONTROLS}
    out["command-line-text"] = cl.get()
    out["command-line-justify"] = str(cl.cget("justify"))
    out["command-line-foreground"] = str(cl.cget("foreground"))
    out["command-line-background"] = str(cl.cget("background"))
    out["command-line-font"] = plain(cl.cget("font"))
    out["enter-action"] = fn_name(CP.command_line_action)
    out["info-label"] = W["info-label"].cget("text")
    out["breakpoint-label"] = W["breakpoint-label"].cget("text")
    out["self-watching-warning-visible"] = bool(W["self-watching-warning-label"].winfo_ismapped())
    out["menus"] = menu_states(CP.main_menu)
    return out


def speed_table():
    out = {}
    for v in (0, 1, 25, 50, 75, 99, 100):
        gui.speed_slider_action(W["speed-slider"], v)
        out[str(v)] = {k: plain(getattr(view_globals, k)) for k in
                       ("p_num_of_flashes", "p_flash_pause", "p_snag_pause",
                        "p_text_scroll_pause", "p_codelet_highlight_pause")}
    gui.speed_slider_action(W["speed-slider"], gui.p_initial_speed)
    return out


# --- dialogs -----------------------------------------------------------------------

def toplevels():
    return [w for w in root.winfo_children() if w.winfo_class() == "Toplevel"]


def new_toplevels(before):
    return [w for w in toplevels() if w not in before]


def all_widgets(w):
    yield w
    for k in w.winfo_children():
        yield from all_widgets(k)


def dialog_entry(top, opened_by):
    e = {"opened_by": opened_by, "title": top.title(), "resizable": list(top.resizable()),
         "geometry_offset_from_control_panel": None, "widgets": widget_tree(top)}
    texts = [w for w in all_widgets(top) if w.winfo_class() == "Text"]
    if texts:
        t = texts[0]
        e["text_first_line"] = t.get("1.0", "1.end")
        e["text_lines"] = int(t.index("end-1c").split(".")[0])
        e["text_wrap"] = str(t.cget("wrap"))
    return e


def open_dialog(opened_by, action, close):
    before = toplevels()
    action()
    pump()
    (top,) = new_toplevels(before)
    e = dialog_entry(top, opened_by)
    DIALOG_BINDINGS.extend(widget_bindings(top, opened_by))
    close(top)
    pump()
    assert not new_toplevels(before), opened_by
    return e


def destroy_via_wm(top):
    # the window manager's close button (WM_DELETE_WINDOW)
    top.tk.call(top.protocol("WM_DELETE_WINDOW"))


def dialogs():
    out = []
    out.append(open_dialog("Help", lambda: gui.help_action(None), destroy_via_wm))
    out[-1]["geometry_offset_from_control_panel"] = None
    for label, fn, offset in (("Options > Set breakpoint", gui.set_breakpoint_action, [20, 80]),
                              ("Options > Step mode interval", gui.set_step_interval_action,
                               [80, 80])):
        e = open_dialog(label, lambda fn=fn: fn(None), destroy_via_wm)
        e["geometry_offset_from_control_panel"] = offset
        e["enter"] = ("empty: close; not a number >= 1: 'Invalid input!' in red for "
                      "700 ms; otherwise set the value and close")
        out.append(e)
    e = open_dialog("Clear Memory", lambda: tell(CP, "clear-memory"), destroy_via_wm)
    e["geometry_offset_from_control_panel"] = [20, 70]
    e["buttons"] = {"Yes": "memory clear, close", "Cancel": "close"}
    e["while_open"] = "control panel in disabled mode; back to input mode on close"
    out.append(e)
    e = open_dialog("Options > Clamp theme pattern",
                    lambda: tell(CP, "theme-edit-mode-on"), destroy_via_wm)
    e["geometry_offset_from_control_panel"] = [10, 20]
    e["buttons"] = {"Clamp Themes": "theme-edit-mode-off #t, close",
                    "Cancel": "theme-edit-mode-off #f, close"}
    e["while_open"] = ("control panel in disabled mode; *theme-edit-mode?* on; theme windows "
                       "take left (+100) and right/shift-left (-100) clicks")
    out.append(e)
    calls = []

    def recorder(title, mode, directory):
        calls.append({"title": title, "mode": mode})
        return False
    gui.set_file_dialog(recorder)
    gui.save_commentary_action(None)
    gui.set_file_dialog(gui._default_file_dialog)
    out.append({"opened_by": "Options > Save commentary to file", "title": calls[0]["title"],
                "kind": "file dialog (tkinter.filedialog.asksaveasfilename)",
                "mode": calls[0]["mode"],
                "writes": "the Commentary's get-lines, one per line; a number n is n "
                          "blank lines"})
    out.append({"opened_by": "control panel display-error", "kind": "inline",
                "title": None,
                "does": "the info label shows the message in red for 700 ms "
                        "('Invalid input!', 'No current problem!')"})
    return out


# --- keyboard and mouse bindings ---------------------------------------------------

DIALOG_BINDINGS = []


def widget_bindings(top, dialog=None):
    out = []
    for w in all_widgets(top):
        for seq in bindings(w):
            e = {"widget": str(w), "class": w.winfo_class(),
                 "toplevel": w.winfo_toplevel().title(), "sequence": seq}
            if dialog:
                e["dialog"] = dialog
            out.append(e)
    return out


def all_bindings():
    out = widget_bindings(root) + DIALOG_BINDINGS
    out.sort(key=lambda e: (e["toplevel"], e.get("dialog", ""), e["widget"], e["sequence"]))
    return out


# --- the walk ----------------------------------------------------------------------

def main():
    pump(0.5)
    inv = {"generated_by": "python/tests/tk_gui_inventory.py",
           "screen": [root.winfo_screenwidth(), root.winfo_screenheight()],
           "platform": sgl.g_platform}
    inv["windows"] = windows()
    inv["control_panel"] = {"title": W["frame"].title(),
                            "resizable": list(W["frame"].resizable()),
                            "close": fn_name(W["frame"].protocol("WM_DELETE_WINDOW")) and
                            "gui._exit (quits the application)",
                            "widgets": widget_tree(W["frame"])}
    states = {"initial": control_state()}
    inv["menus"] = menu_tree(CP.main_menu)
    inv["speed_slider"] = {
        "from": plain(W["speed-slider"].cget("from")), "to": plain(W["speed-slider"].cget("to")),
        "initial": gui.p_initial_speed, "length": gui.p_gui_slider_length,
        "labels": ["Speed", "Slow", "Fast"], "action": fn_name(gui.speed_slider_action),
        "sets": speed_table()}
    inv["buttons"] = {
        "Step": fn_name(gui.step_button_action), "Go": fn_name(gui.go_button_action),
        "Stop": fn_name(gui.stop_button_action), "Reset": fn_name(gui.reset_button_action),
        "semantics": {
            "Step": "empty line: step mode on, resume; valid problem: init it in step mode; "
                    "else 'Invalid input!'",
            "Go": "empty line: step mode off, resume; valid problem: init it and park "
                  "(quiet-break); else 'Invalid input!'",
            "Enter": "same as Go in input mode; nothing in run and disabled modes",
            "Stop": "sets *interrupt?*; the run breaks at the next codelet",
            "Reset": "empty line: re-init the current problem (same seed); valid problem: "
                     "init it; else 'Invalid input!'"}}
    # a problem, so that every state and dialog can be reached
    CP.command_line.insert(0, "abc abd xyz 7")
    gui.go_button_action(None)
    wait_idle()
    states["input"] = control_state()
    states["input"]["note"] = "after init-new-problem 'abc abd xyz 7' (parked)"
    tell(CP, "switch-to-disabled-mode")
    pump()
    states["disabled"] = control_state()
    states["disabled"]["note"] = "from input mode (the look of the command line is kept)"
    tell(CP, "switch-to-input-mode")
    tell(CP, "switch-to-run-mode")
    pump()
    states["run"] = control_state()
    tell(CP, "switch-to-input-mode")
    pump()
    sw = [i for i in CP.main_menu.items[3].submenu.items
          if i.kind == "check" and i.get_title() == "Self-watching mode"][0]
    sw.menu.invoke(sw.index)
    pump()
    states["self-watching-off"] = control_state()
    states["self-watching-off"]["hidden_windows"] = [
        tell(c, "get-menu-item").get_title()[5:] for c in W["window-controllers"]
        if not tell(c, "visible?")]
    sw.menu.invoke(sw.index)
    pump()
    inv["states"] = states
    inv["dialogs"] = dialogs()
    inv["bindings"] = all_bindings()
    inv["state_messages"] = {
        "switch-to-input-mode": "command line, Step, Go, Reset and the menus enabled; Stop "
                                "disabled; Enter = Go",
        "switch-to-run-mode": "only Stop enabled; the command line shows 'running...' in "
                              "green on black, centred; Enter does nothing",
        "switch-to-disabled-mode": "everything disabled (dialogs open)"}
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(inv, indent=1, sort_keys=True, ensure_ascii=False) + "\n")
    print("wrote", OUT)


try:
    main()
    status = 0
except BaseException:   # noqa: BLE001
    import traceback
    traceback.print_exc()
    status = 1
sys.stdout.flush()
os._exit(status)
