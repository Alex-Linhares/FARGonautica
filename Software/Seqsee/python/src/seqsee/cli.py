"""Headless port of Seqsee.pl: ``python3 -m seqsee --seq "1 1 2 1 2 3" --seed N --max-steps M``.

Seqsee.pl reads the options (``_read_config(_read_commandline())``), runs INITIALIZE (coderack,
stream and workspace clear/init, LTM load if Feature LTM, SLTM init) and hands control to Tk,
whose "continue" button calls Interaction_continue (``Interaction_step_n`` for max_steps steps).
A yes from the user inserts terms and sets Break_Loop, which stops that call; the user then
presses continue again. ``run_headless`` does the same without Tk: it calls
``interaction_step_n`` again until it returns true (max_steps reached) or a solution is
accepted, and returns what happened.

Questions go where UI/Graphical.pm sends them, ``$SGUI::Commentary``; here a ``_RunState``
answers them (``user_interaction.boolean_response``/``response``, with UI/Graphical's
``ask_user_extension``):

- ``--continuation "4 5 6"``: the known next terms. A yes/no question about terms is answered
  yes iff they are the next known terms; asking past them ends the run (status
  ``out_of_terms``). The asked terms come from the SErr::ElementsBeyondKnownSought being asked
  (``user_interaction.ask``) or ``ask_user_extension``'s items, not from the question text.
- otherwise ``--answer yes`` (default: every question is answered yes), ``--answer no``, or
  ``--answer ask`` (read y/n from stdin).

"Does this generate the sequence you had in mind?" (DescribeSolution) is answered "Yes" in
``yes`` and continuation modes. Accepting a solution sets Break_Loop, so the run ends after
that step (status ``accepted``). The report gives the extension found: the workspace elements
beyond the input.

Deliberate additions (no Perl counterpart), all CLI-layer:
- ``util.srand(seed)``: Seqsee.pl never seeds its RNG with ``--seed`` (PERL-QUIRK).
- Hyphenated spellings ``--max-steps``/``--update-interval`` are translated to Seqsee.pm's
  ``--max_steps``/``--update_interval`` before Getopt (``translate_argv``).
- ``--continuation``, ``--answer`` and ``--json`` are taken out before Getopt
  (``split_cli_options``).
- Every run starts with ``s.reset_all()``, as a fresh Seqsee.pl process would.
- Seqsee's own stdout (``View: 1!``, ``Initializing Coderack...``) is swallowed.
- No ``--seq``: Seqsee.pl opens SGUI->ask_seq; here it is a usage error (exit 2).
- ``--gui`` (``split_gui_flag``) hands the command line to the Qt GUI, ``seqsee.gui.app``
  (imported only then; ``python3 -m seqsee.gui`` is the same). Without it nothing changes.

Oracle: oracle/cli.pl (golden ``cli``) runs the same loop in Perl.
"""
import contextlib
import importlib
import io
import json
import re
import sys
from pathlib import Path

from seqsee import global_ as Global
from seqsee import s, scoderack, seqsee_main, sltm, sworkspace, user_interaction, util
from seqsee.errors import Confess
from seqsee.user_interaction import SolutionConfirmation

_REPO = Path(__file__).resolve().parents[3]
_LTM_FILE = _REPO / "memory_dump.dat"

_HYPHENATED = {"max-steps": "max_steps", "update-interval": "update_interval"}
_ANSWERS = ("yes", "no", "ask")


class UsageError(Exception):
    """Bad command line."""


class OutOfTerms(Exception):
    """Seqsee asked about terms beyond the known continuation."""


def translate_argv(argv):
    """Rename ``--max-steps``/``--update-interval`` (with ``-``/``--`` prefix, optionally
    ``=value``) to Seqsee.pm's underscore names. Stops at ``--``."""
    out = []
    args = iter(argv)
    for arg in args:
        if arg == "--":
            out.append(arg)
            out.extend(args)
            break
        for prefix in ("--", "-"):
            if arg.startswith(prefix):
                name, eq, value = arg[len(prefix):].partition("=")
                if name in _HYPHENATED:
                    arg = prefix + _HYPHENATED[name] + eq + value
                break
        out.append(arg)
    return out


def split_gui_flag(argv):
    """Does ``argv`` ask for the GUI? Returns (the rest, bool). Seqsee.pm's ``gui=s`` is a
    synonym of ``gui_config=s``: ``--gui X`` and ``--gui=X`` ask for the GUI and stay for
    Getopt (which gives gui_config); a bare ``--gui`` (last, or before another option) is
    taken out. Any of ``--``, ``-``, ``+``; case-insensitive, as Getopt; stops at ``--``."""
    rest, gui = [], False
    i = 0
    while i < len(argv):
        arg = argv[i]
        if arg == "--":
            rest.extend(argv[i:])
            break
        m = re.match(r"(--|-|\+)gui(=.*)?\Z", arg, re.I | re.S)
        if m:
            gui = True
            nxt = argv[i + 1] if i + 1 < len(argv) else None
            if m.group(2) is None and (nxt is None or nxt[:1] in ("-", "+")):
                i += 1
                continue
        rest.append(arg)
        i += 1
    return rest, gui


def split_cli_options(argv):
    """Take the CLI's own options (``--continuation X``, ``--answer X``, ``--json``) out of
    ``argv``. Returns (the rest, {option: value})."""
    rest, opts = [], {}
    i = 0
    while i < len(argv):
        arg = argv[i]
        if arg == "--":
            rest.extend(argv[i:])
            break
        name, eq, value = arg.lstrip("-").partition("=")
        if arg.startswith("-") and name in ("continuation", "answer"):
            if not eq:
                i += 1
                if i >= len(argv):
                    raise UsageError(f"Option {name} requires an argument")
                value = argv[i]
            if name == "answer" and value not in _ANSWERS:
                raise UsageError(f"--answer must be one of {', '.join(_ANSWERS)}; got '{value}'")
            opts[name] = value
        elif arg.startswith("-") and name == "json" and not eq:
            opts["json"] = True
        else:
            rest.append(arg)
        i += 1
    return rest, opts


class _RunState:
    """The stand-in for $SGUI::Commentary during one run: answers questions and records them."""

    def __init__(self, known=None, answer="yes", stdin=None, prompt_out=None):
        self.known = known          # list of term strings (input + continuation), or None
        self.answer = answer
        self.stdin = stdin if stdin is not None else sys.stdin
        self.prompt_out = prompt_out if prompt_out is not None else sys.stderr
        self.pending = []           # the terms being asked about
        self.asked = []
        self.responses = []
        self.accepted = False

    def _ask_stdin(self, question):
        self.prompt_out.write(f"{question} [y/n] ")
        self.prompt_out.flush()
        return self.stdin.readline().strip().lower() in ("y", "yes")

    def boolean_response(self, question, *rest):
        """$SGUI::Commentary->MessageRequiringBooleanResponse($question, ...)."""
        question = util.perl_str(question)
        self.asked.append(question)
        if self.known is None:
            if self.answer == "ask":
                return 1 if self._ask_stdin(question) else 0
            return 1 if self.answer == "yes" else 0
        terms = list(self.pending)
        if not terms:
            raise Confess(f"No pending terms: {question}")
        at = sworkspace.ElementCount
        if at + len(terms) > len(self.known):
            raise OutOfTerms(question)
        for i, term in enumerate(terms):
            if util.perl_num(self.known[at + i]) != util.perl_num(term):
                return 0
        return 1

    def response(self, choices, question):
        """$SGUI::Commentary->MessageRequiringAResponse([choices], $question)."""
        self.responses.append(util.perl_str(question))
        if self.known is None and self.answer == "ask":
            return "Yes" if self._ask_stdin(question) else "No"
        if self.known is None and self.answer == "no":
            return "No"
        return "Yes"


@contextlib.contextmanager
def _installed(state):
    """Install the run's answerers, the pending-terms wrappers and the accept hook; restore
    the previous ones afterwards."""
    ui = user_interaction
    saved = (ui.boolean_response, ui.response, ui.ask_user_extension, ui.ask)
    saved_accept = SolutionConfirmation.__dict__["set_accepted_solution"]
    orig_ask, orig_accept = ui.ask, SolutionConfirmation.set_accepted_solution
    gui_ask_user_extension = ui.gui_ask_user_extension

    def ask(err, *args):
        before, state.pending = state.pending, list(err.next_elements)
        try:
            return orig_ask(err, *args)
        finally:
            state.pending = before

    def ask_user_extension(items, msg_suffix=None):
        before, state.pending = state.pending, list(items)
        try:
            return gui_ask_user_extension(items, msg_suffix)
        finally:
            state.pending = before

    def set_accepted_solution(cls, rule, position_structure):
        state.accepted = True
        Global.Break_Loop = 1
        return orig_accept(rule, position_structure)

    ui.install(boolean_response=state.boolean_response, response=state.response,
               ask_user_extension=ask_user_extension)
    ui.ask = ask
    SolutionConfirmation.set_accepted_solution = classmethod(set_accepted_solution)
    try:
        yield
    finally:
        ui.boolean_response, ui.response, ui.ask_user_extension, ui.ask = saved
        SolutionConfirmation.set_accepted_solution = saved_accept


def _initialize(options):
    """Seqsee.pl's INITIALIZE, without the display."""
    scoderack.clear()
    scoderack.init(options)
    Global.MainStream.clear()
    Global.MainStream.init()
    sworkspace.clear()
    sworkspace.init(options)
    if util.perl_true(Global.Feature.get("LTM")):
        sltm.load(str(_LTM_FILE))
    sltm.init()


def run_headless(argv, stdin=None, prompt_out=None):
    """Run headless Seqsee.pl on ``argv`` and return the result: seed, seq, max_steps,
    continuation, elements, extension, asked, responses, steps, status (accepted /
    max_steps / out_of_terms / error) and error. Raises UsageError on a bad command line."""
    rest, cli_opts = split_cli_options(translate_argv(list(argv)))
    s.load()
    s.reset_all()
    quiet = io.StringIO()
    with contextlib.redirect_stdout(quiet):
        try:
            options = seqsee_main.read_config(**seqsee_main.read_commandline(rest))
        except Confess as e:
            raise UsageError(str(e).split("\n")[0]) from None
    if not options["seq"] or options["seq"] == [""]:
        raise UsageError("No sequence given: use --seq \"1 1 2 1 2 3\"")
    Global.Options_ref = options

    cont = cli_opts.get("continuation")
    known = None
    if cont is not None:
        known = list(options["seq"]) + cont.replace(",", " ").split()
    state = _RunState(known=known, answer=cli_opts.get("answer", "yes"), stdin=stdin,
                      prompt_out=prompt_out)
    seq_text = " ".join(options["seq"])
    max_steps = options["max_steps"]
    result = {
        "seed": int(util.perl_num(options["seed"])),
        "seq": seq_text,
        "max_steps": int(util.perl_num(max_steps)) if max_steps is not None else None,
        "continuation": cont,
    }

    error = None
    with contextlib.redirect_stdout(quiet), _installed(state):
        try:
            util.srand(int(util.perl_num(options["seed"])))
            _initialize(options)
            Global.InterstepSleep = 0
            while not (util.perl_true(seqsee_main.interaction_step_n(
                    {"n": max_steps, "update_after": options["update_interval"],
                     "max_steps": max_steps})) or state.accepted):
                pass
        except OutOfTerms:
            error = "out_of_terms"
        except Exception as e:  # noqa: BLE001 - the run's death is reported, as Perl's eval
            error = e

    mags = [int(util.perl_num(e.get_mag())) for e in sworkspace.get_elements()]
    initial = len(options["seq"])
    result["elements"] = mags
    result["extension"] = mags[initial:]
    result["asked"] = state.asked
    result["responses"] = state.responses
    result["steps"] = Global.Steps_Finished
    if error == "out_of_terms":
        result["status"] = "out_of_terms"
    elif error is not None:
        result["status"] = "error"
    elif state.accepted:
        result["status"] = "accepted"
    else:
        result["status"] = "max_steps"
    result["error"] = (str(error).split("\n")[0] or type(error).__name__) \
        if result["status"] == "error" else None
    return result


def format_report(result):
    """The human-readable report of a run."""
    lines = [f"Sequence: {result['seq']}"]
    if result["extension"]:
        lines.append("Extension found: " + " ".join(str(x) for x in result["extension"]))
    else:
        lines.append("Extension found: (none)")
    status = result["status"]
    detail = {
        "accepted": "solution accepted",
        "max_steps": "reached max_steps",
        "out_of_terms": "asked beyond the known continuation",
        "error": f"error: {result['error']}",
    }[status]
    lines.append(f"Status: {status} ({detail}) after {result['steps']} steps, seed {result['seed']}")
    for q in result["asked"]:
        lines.append(f"  asked: {q.strip()}")
    return "\n".join(lines) + "\n"


def main(argv=None, out=None, err=None):
    """Entry point. Returns the exit status: 0 for a finished run, 1 if Seqsee died, 2 for a
    usage error."""
    out = out if out is not None else sys.stdout
    err = err if err is not None else sys.stderr
    argv = sys.argv[1:] if argv is None else argv
    if split_gui_flag(list(argv))[1]:
        # Imported only on request: the headless package never loads the GUI (or Qt).
        return importlib.import_module("seqsee.gui.app").main(list(argv), err=err)
    try:
        cli_opts = split_cli_options(translate_argv(list(argv)))[1]
        with contextlib.redirect_stderr(err):
            result = run_headless(argv)
    except UsageError as e:
        err.write(f"seqsee: {e}\n")
        return 2
    if cli_opts.get("json"):
        out.write(json.dumps(result, sort_keys=True) + "\n")
    else:
        out.write(format_report(result))
    return 1 if result["status"] == "error" else 0
