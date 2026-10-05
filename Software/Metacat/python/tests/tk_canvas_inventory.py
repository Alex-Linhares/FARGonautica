"""The Tk canvas commands and options the panels send (loop0003 item 01).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

    python3 tests/tk_canvas_inventory.py tests/data/tk-canvas-commands.json

writes the list the Qt canvas (metacat/qt/canvas.py) must implement, built from
three sources, each kept as a part of the JSON:

- "code": the calls in python/metacat/ that send a canvas command, read with
  ast: sgl.py's tcl_eval (and _tcl_eval_8_0), swl.swl_tcl_eval, and a
  canvas's .tcl.  The command (and a create's item kind) must be a literal; the
  options are the literal words "-name".  A call whose command is computed is a
  problem, unless it is one of the FORWARDERS, which pass on what they are given;
- "fixtures": python/fixtures/sgl-tcl/*.txt, the original's own Tcl stream
  (captured from Chez by python/oracle/capture_sgl_tcl.py);
- "runs": the commands that reach the canvases in real runs with every view
  attached (views.attach_views, offscreen hosts with recording canvases, and a
  recording hidden canvas for fonts.ss's text measurement): RUNS, each in a
  fresh process (`--record OUT problem... seed`).

Per command ("create <kind>" for create) the options used; per enumerated
option (ENUMERATED) the values seen; per command the forms of its first
argument seen in the streams ("id", "all" or "tag"); the canvas methods
called besides tcl; and the words given as -tags ("(list)" if a list ever is).  tests/test_tk_canvas_inventory.py rebuilds each part and
compares it with the committed JSON.
"""
from __future__ import annotations

import ast
import json
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
PY = HERE.parent
METACAT = PY / "metacat"
SGL_TCL = PY / "fixtures" / "sgl-tcl"

# run7 of the screenshots, and a justify run (the Bottom Themes draw only there)
RUNS = [(["abc", "abd", "xyz"], 3852097033), (["abc", "abd", "xyz", "wyz"], 1)]

# the options whose values the Qt canvas must interpret word by word
ENUMERATED = ("-anchor", "-arrow", "-capstyle", "-dash", "-joinstyle", "-justify",
              "-smooth", "-state", "-style")

# commands whose first argument is not an item (an id, "all" or a tag)
NOT_TARGETED = ("create", "canvasx", "canvasy")

# the calls that pass a command on without naming it: (file, function)
FORWARDERS = {("metacat/gui/swl.py", "swl_tcl_eval"),
              ("metacat/gui/sgl.py", "_tcl_eval_8_0"),
              ("metacat/gui/sgl.py", "tcl"),
              ("metacat/qt/canvas.py", "tcl")}     # the Qt canvas itself (item 02)

# name of the callable -> index of the command among its arguments
SENDERS = {"tcl_eval": 1, "_tcl_eval_8_0": 1, "swl_tcl_eval": 1, "tcl": 0}

METHODS = ("get_background_color", "set_background_color_bang")

ABOUT = ("The Tk canvas commands and options the panels send, from the code, the "
         "sgl-tcl fixtures and recorded runs (python/tests/tk_canvas_inventory.py; "
         "checked by test_tk_canvas_inventory.py).  Keys are commands, 'create <kind>' "
         "for create.  Do not edit by hand.")


class Found:
    """what one source sends"""

    def __init__(self, source):
        self.source = source
        self.commands = {}      # key -> set of options
        self.values = {}        # enumerated option -> set of values
        self.targets = {}       # key -> set of "id", "all", "tag"
        self.methods = set()
        self.tags = set()       # the words given as -tags ("(list)" for a list)
        self.problems = []
        self.lines = 0

    def command(self, key, options=()):
        self.commands.setdefault(key, set()).update(options)

    def value(self, option, value):
        if option in ENUMERATED:
            self.values.setdefault(option, set()).add(value)

    def target(self, key, word):
        self.targets.setdefault(key, set()).add(target_form(word))

    def record(self, args):
        """one command as the panels pass it to a canvas's tcl"""
        self.lines += 1
        words = [w if isinstance(w, (int, float)) else str(w) if isinstance(w, str) else w
                 for w in args]
        verb = str(words[0])
        key = verb + " " + str(words[1]) if verb == "create" else verb
        options = []
        for i, w in enumerate(words):
            if isinstance(w, str) and re.fullmatch(r"-[a-z]+", w) and i > 0:
                options.append(w)
                if w == "-tags" and i + 1 < len(words):
                    tag = words[i + 1]
                    self.tags.add(tag if isinstance(tag, str) else "(list)")
                if i + 1 < len(words) and isinstance(words[i + 1], str):
                    self.value(w, words[i + 1])
        self.command(key, options)
        if verb not in NOT_TARGETED and len(words) > 1:
            self.target(key, words[1])

    def as_json(self):
        return {"commands": {k: sorted(v) for k, v in sorted(self.commands.items())},
                "values": {k: sorted(v) for k, v in sorted(self.values.items())},
                "targets": {k: sorted(v) for k, v in sorted(self.targets.items())},
                "methods": sorted(self.methods),
                "tags": sorted(self.tags)}


def target_form(word):
    word = str(word)
    if word == "all":
        return "all"
    return "id" if word.isdigit() else "tag"


# ------------------------------------------------------------------------------
# code

def _callee(func):
    if isinstance(func, ast.Name):
        return func.id
    if isinstance(func, ast.Attribute):
        return func.attr
    return None


def _scan_file(found, path, rel):
    tree = ast.parse(path.read_text(), str(path))

    def visit(node, function):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            function = node.name
        if isinstance(node, ast.Call):
            name = _callee(node.func)
            if name in METHODS:
                found.methods.add(name)
            if name in SENDERS:
                check_call(node, name, function)
        for child in ast.iter_child_nodes(node):
            visit(child, function)

    def check_call(node, name, function):
        index = SENDERS[name]
        if name == "tcl" and not isinstance(node.func, ast.Attribute):
            return
        args = node.args
        literal = (len(args) > index and not any(isinstance(a, ast.Starred) for a in args[:index + 1])
                   and isinstance(args[index], ast.Constant) and isinstance(args[index].value, str))
        if not literal:
            if (rel, function) not in FORWARDERS:
                found.problems.append("%s:%d: a canvas command that is not a literal (in %s)"
                                      % (rel, node.lineno, function))
            return
        words = []
        for a in args[index:]:
            if isinstance(a, ast.Constant) and isinstance(a.value, (str, int, float)):
                words.append(a.value)
            else:
                words.append(None)
        if words[0] == "create" and not (len(words) > 1 and isinstance(words[1], str)):
            found.problems.append("%s:%d: create of an item kind that is not a literal"
                                  % (rel, node.lineno))
            return
        verb = words[0]
        key = verb + " " + words[1] if verb == "create" else verb
        options = []
        for i, w in enumerate(words):
            if i > 0 and isinstance(w, str) and re.fullmatch(r"-[a-z]+", w):
                options.append(w)
                if i + 1 < len(words) and isinstance(words[i + 1], str):
                    found.value(w, words[i + 1])
        found.command(key, options)
        found.lines += 1

    visit(tree, None)


def scan_code(paths=None):
    """the canvas commands sent by the modules (default: every python/metacat
    module, the engine's, the tkinter GUI's and the Qt GUI's)"""
    found = Found("code")
    if paths is None:
        paths = sorted(METACAT.rglob("*.py"))
    for path in paths:
        path = Path(path)
        try:
            rel = path.resolve().relative_to(PY).as_posix()
        except ValueError:
            rel = path.name
        _scan_file(found, path, rel)
    return found


# ------------------------------------------------------------------------------
# fixtures

_TOKEN = re.compile(r'"(?:[^"\\]|\\.)*"|[()]|[^\s()"]+')


def _tokens(text):
    """the top-level words of a stream line's arguments: strings unquoted,
    parenthesised groups as lists"""
    stack = [[]]
    for t in _TOKEN.findall(text):
        if t == "(":
            stack.append([])
        elif t == ")":
            done = stack.pop()
            stack[-1].append(done)
        elif t.startswith('"'):
            stack[-1].append(t[1:-1])
        else:
            stack[-1].append(t.replace("\\x2D;", "-"))
    return stack[0]


def scan_fixtures(directory=SGL_TCL):
    found = Found("fixtures")
    for path in sorted(Path(directory).glob("*.txt")):
        for line in path.read_text().splitlines():
            m = re.fullmatch(r"\((tcl|swl) (\S+) (.*)\)", line)
            if not m:
                raise ValueError("%s: not a stream line: %r" % (path.name, line))
            words = _tokens(m.group(3))
            if m.group(1) == "swl":
                found.lines += 1
                found.methods.add(words[0].replace("-", "_").replace("!", "_bang"))
            else:
                found.record([w if isinstance(w, str) else "(list)" for w in words])
    return found


# ------------------------------------------------------------------------------
# runs

def record_run(strings, seed, out):
    """in this (fresh) process: one run with every view attached, its canvases
    recording; the Found written to out as JSON"""
    import io
    from contextlib import redirect_stdout
    if str(PY) not in sys.path:
        sys.path.insert(0, str(PY))
    from metacat import headless
    from metacat.gui import fonts, hosts, views

    found = Found("runs")

    class RecordingCanvas(hosts.OffscreenCanvas):
        def tcl(self, *args):
            found.record(args)
            return super().tcl(*args)

        def get_background_color(self):
            found.methods.add("get_background_color")
            return super().get_background_color()

        def set_background_color_bang(self, color):
            found.methods.add("set_background_color_bang")
            return super().set_background_color_bang(color)

    class RecordingHidden(hosts.OffscreenHiddenCanvas):
        def tcl(self, *args):
            found.record(args)
            return super().tcl(*args)

    class RecordingHost(hosts.OffscreenHost):
        def make_canvas(self, visible_w, visible_h, bg_color):
            self.width, self.height = visible_w, visible_h
            self.canvas = RecordingCanvas(bg_color)
            return self.canvas

    hosts.set_window_host_maker(RecordingHost)
    hosts.install_offscreen_fonts()             # the fixed metric and families
    fonts.g_hidden_canvas = RecordingHidden()
    with redirect_stdout(io.StringIO()):
        headless.run_problem(strings, seed, views=views.attach_views)
    Path(out).write_text(json.dumps(dict(found.as_json(), lines=found.lines)))


def scan_runs(runs=RUNS):
    """RUNS recorded, each in a fresh process, in parallel"""
    import tempfile
    found = Found("runs")
    with tempfile.TemporaryDirectory() as tmp:
        def one(i_run):
            i, (strings, seed) = i_run
            out = Path(tmp) / ("%d.json" % i)
            subprocess.run([sys.executable, str(Path(__file__).resolve()), "--record",
                            str(out), *strings, str(seed)], cwd=PY, check=True, timeout=600)
            return json.loads(out.read_text())
        with ThreadPoolExecutor(len(runs)) as pool:
            for part in pool.map(one, enumerate(runs)):
                found.lines += part["lines"]
                for k, v in part["commands"].items():
                    found.command(k, v)
                for k, v in part["values"].items():
                    found.values.setdefault(k, set()).update(v)
                for k, v in part["targets"].items():
                    found.targets.setdefault(k, set()).update(v)
                found.methods.update(part["methods"])
                found.tags.update(part["tags"])
    return found


# ------------------------------------------------------------------------------
# the list

def build_from_parts(parts):
    commands, values, targets, methods, tags = {}, {}, {}, set(), set()
    for source, part in parts.items():
        for k, opts in part["commands"].items():
            entry = commands.setdefault(k, {"options": set(), "sources": set()})
            entry["options"].update(opts)
            entry["sources"].add(source)
        for k, v in part["values"].items():
            values.setdefault(k, set()).update(v)
        for k, v in part["targets"].items():
            targets.setdefault(k, set()).update(v)
        methods.update(part["methods"])
        tags.update(part["tags"])
    return {
        "about": ABOUT,
        "commands": {k: {"options": sorted(e["options"]), "sources": sorted(e["sources"])}
                     for k, e in sorted(commands.items())},
        "values": {k: sorted(v) for k, v in sorted(values.items())},
        "targets": {k: sorted(v) for k, v in sorted(targets.items())},
        "methods": sorted(methods),
        "tags": sorted(tags),
        "parts": {source: parts[source] for source in sorted(parts)},
    }


def build(founds):
    return build_from_parts({f.source: f.as_json() for f in founds})


def part(committed, source):
    return committed["parts"][source]


def missing(found, committed):
    """what found sends that the committed list lacks, one string per entry"""
    out = []
    commands = committed["commands"]
    for key, options in sorted(found.commands.items()):
        if key not in commands:
            out.append(key)
            out.extend(key + " " + o for o in sorted(options))
        else:
            out.extend(key + " " + o for o in sorted(options - set(commands[key]["options"])))
    for option, values in sorted(found.values.items()):
        listed = set(committed["values"].get(option, ()))
        out.extend("%s=%r" % (option, v) for v in sorted(values - listed))
    for key, forms in sorted(found.targets.items()):
        listed = set(committed["targets"].get(key, ()))
        out.extend("%s target %s" % (key, f) for f in sorted(forms - listed))
    out.extend("method " + m for m in sorted(found.methods - set(committed["methods"])))
    out.extend("tag " + t for t in sorted(found.tags - set(committed["tags"])))
    return out


def main(argv):
    if argv[:1] == ["--record"]:
        out, *words = argv[1:]
        record_run(words[:-1], int(words[-1]), out)
        return 0
    code = scan_code()
    if code.problems:
        print("\n".join(code.problems), file=sys.stderr)
        return 1
    data = build([code, scan_fixtures(), scan_runs()])
    Path(argv[0]).write_text(json.dumps(data, indent=1) + "\n")
    print("%d commands -> %s" % (len(data["commands"]), argv[0]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
