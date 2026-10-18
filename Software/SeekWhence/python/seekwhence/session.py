"""The session: the console protocol of the transcripts, for a user at a terminal or for a
script [M86 pp. 37, 164-179] (DECISIONS D-053, D-054, D-064, D-069).

The program is shown patterns "expressed as sequences of nonnegative integers, presented one
term at a time" [MHN p. 3]: it prints `please enter a term:` and the user types one integer, or one of the
console commands `show`, `show-hypothesis`, `show-seq`, `show-parse` [M86 pp. 166-179], or
`bye` to quit (DECISION D-054). When the program has a guess it prints the block and
`enter no if wrong, ok if right`; `ok` ends the run with `bye`, anything else sends the
system on [M86 pp. 166, 175-176] (DECISION D-053). A `Session` is one run seen through
that protocol: it makes the `World`, prints the prompts, reads the user's lines from a
`Console` (a terminal, a piped stream or a script of lines), feeds each term to the engine
(`run.enter_term`) and runs the tasks until the program asks again
(`run.run_until_prompt`), answers the engine's guesses through a `ConsoleAnswerer`, and
ends with an `outcome` (DECISION D-069). The engine never imports this module; the session
only drives it, and a run driven by a session gives the same trace as one driven by the
engine's tests from the same seed (displays and drivers only observe).
"""

import sys

from seekwhence import cytoplasm as cy
from seekwhence import gnoths
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.filtering import OK, judge
from seekwhence.forms import value_text
from seekwhence.run import enter_term, new_world, run_until_prompt
from seekwhence.slipping import bonds
from seekwhence.trace import Trace

# The two prompts, as printed with the user's input on the same line [M86 pp. 164, 166].
PROMPT = "please enter a term: "
ANSWER_PROMPT = "enter no if wrong, ok if right "

# The console commands [M86 pp. 166-179] and the word that quits (DECISION D-054).
COMMANDS = ("show", "show-hypothesis", "show-seq", "show-parse")
QUIT = "bye"

# The reconstruction's own line for an input that is neither a term nor a command; the
# transcripts never show one (DECISION D-054).
HINT = "(enter a nonnegative integer, show, show-hypothesis, show-seq, show-parse or bye)"

# The outcomes of a session (DECISION D-069): `ok` (a guess accepted, `bye` [M86 p. 166]),
# `bye` (the user quit), `eof` (the input ended), `no-terms` (the scripted terms ran out),
# `capped` (the whole-run cap of DECISION D-011).
OUTCOMES = ("ok", "bye", "eof", "no-terms", "capped")


def parse_line(text: str) -> tuple:
    """What one console line is: `("term", 3)` for a nonnegative integer, `("command",
    "show")` for a console command, `("bye", None)`, or `("unknown", text)` for anything
    else, which is re-prompted (DECISION D-054). Case and surrounding spaces are ignored."""
    word = text.strip()
    if word.lower() == QUIT:
        return ("bye", None)
    if word.lower() in COMMANDS:
        return ("command", word.lower())
    if word.isdigit():
        return ("term", int(word))
    return ("unknown", word)


class Console:
    """Where the session reads the user's lines and writes its prompts (DECISION D-069).

    The lines come from a script (`lines`, the non-interactive mode's `--terms`) or from a
    text stream (`inp`, the terminal or a pipe; standard input when None). `ask` writes a
    prompt, reads one line and, when `echo` is on, writes the line after the prompt so that
    the output reads as the transcripts do (`please enter a term: 3` [M86 p. 164]); the echo
    is on for a script and for a stream that is not a terminal (a terminal echoes by
    itself), unless given. At the end of the script or the stream `ask` ends the prompt's
    line and returns None. `asked` keeps every line read.
    """

    def __init__(self, out=None, lines=None, inp=None, echo: bool | None = None):
        self.out = out if out is not None else sys.stdout
        self.lines = None if lines is None else [str(line) for line in lines]
        self.inp = inp
        if echo is None:
            stream = self._stream()
            echo = self.lines is not None or not (hasattr(stream, "isatty") and stream.isatty())
        self.echo = echo
        self.asked: list = []

    def _stream(self):
        return self.inp if self.inp is not None else sys.stdin

    @property
    def scripted(self) -> bool:
        """Whether the lines come from a script rather than a stream (DECISION D-069)."""
        return self.lines is not None

    def ask(self, prompt: str) -> str | None:
        """Write the prompt and read one line, echoing it when `echo` is on; None at the end
        of the input (DECISION D-069)."""
        self.out.write(prompt)
        self.out.flush()
        if self.lines is not None:
            if not self.lines:
                self.out.write("\n")
                return None
            text = self.lines.pop(0)
        else:
            line = self._stream().readline()
            if line == "":
                self.out.write("\n")
                return None
            text = line.rstrip("\n")
        if self.echo:
            self.out.write(text + "\n")
        self.asked.append(text)
        return text

    def tell(self, prompt: str, text: str) -> None:
        """Write a prompt with an answer that was not read from the user on the same line,
        `enter no if wrong, ok if right ok` [M86 p. 166], for a scripted or judged answer
        (DECISION D-069)."""
        self.out.write(prompt + text + "\n")
        self.out.flush()


class ConsoleAnswerer:
    """The session's `Answerer` (DECISION D-064): a guess is answered by the next scripted
    answer, else, when the whole sequence is scripted, by judging the guess against it
    (`ok` when its terms agree with the sequence as far as both go, `no` otherwise,
    DECISIONS D-033, D-053), else by asking the user at the console: a blank line, `enter
    no if wrong, ok if right ` and the user's words on the same line [M86 p. 166]. A
    scripted or judged answer is written after the prompt so the output reads as the
    transcripts do. At the end of the input the answer is the empty word, a refusal.
    `given` keeps every answer."""

    def __init__(self, console: Console, answers=(), terms=None):
        self.console = console
        self.answers = list(answers)
        self.terms = None if terms is None else tuple(terms)
        self.given: list = []

    def answer(self, world, form: str, block: str, terms: tuple) -> str:
        """The answer to one guess (DECISIONS D-053, D-064, D-069)."""
        del world, form, block
        if self.answers:
            answer = self.answers.pop(0)
            self.console.tell("\n" + ANSWER_PROMPT, answer)
        elif self.terms is not None:
            answer = OK if judge(terms, self.terms) else "no"
            self.console.tell("\n" + ANSWER_PROMPT, answer)
        else:
            asked = self.console.ask("\n" + ANSWER_PROMPT)
            answer = "" if asked is None else asked
        self.given.append(answer)
        return answer


class Session:
    """One run of the program as the user sees it [M86 pp. 164-179] (DECISION D-069).

    `Session(config, seed, out, lines, answers, inp, observers)`: with `lines` (terms and
    console commands, as strings or integers) the session is scripted, the terms are fed on
    request and a guess with no scripted answer left is judged against the scripted terms;
    without `lines` the user types at `inp` (standard input by default). `out` receives the
    transcript-style trace (`Trace`, verbose as `Config.verbose` says, DECISION D-055) and
    the prompts. `recorder` keeps every event; extra `observers` are subscribed too.

    `run` is the console loop; `enter` feeds one term and runs the engine until it asks
    again; `handle` takes one console line; the `show*` methods give the commands' output
    as lines; `outcome` is None while the session goes on and one of `OUTCOMES` after;
    `summary` is the one-line account of the run (DECISION D-069). An `answerer` given
    (a function of the console returning an `Answerer`, DECISION D-064) replaces the
    `ConsoleAnswerer`: the bench's judge against a sequence's true continuation
    (DECISION D-070) is one.
    """

    def __init__(self, config: Config | None = None, seed: int = 0, out=None, lines=None,
                 answers=(), inp=None, observers=(), answerer=None):
        self.config = config if config is not None else Config()
        self.seed = seed
        self.out = out if out is not None else sys.stdout
        self.console = Console(self.out, lines=lines, inp=inp)
        scripted_terms = None
        if lines is not None:
            scripted_terms = [parse_line(str(x))[1] for x in lines
                              if parse_line(str(x))[0] == "term"]
        if answerer is None:
            self.answerer = ConsoleAnswerer(self.console, answers, scripted_terms)
        else:
            self.answerer = answerer(self.console)
        self.trace = Trace(self.out, verbose=self.config.verbose, prompts=False)
        self.recorder = Recorder()
        self.world = new_world(self.config, seed,
                               observers=[self.recorder, self.trace, *observers],
                               answerer=self.answerer)
        self.outcome: str | None = None
        self.reasons: list = []

    # ---- driving the engine ------------------------------------------------------------------

    def enter(self, term: int) -> str:
        """Feed one term and run the tasks until the program asks for the next ("presented
        one term at a time" [MHN p. 3]; `run.enter_term`, `run.run_until_prompt`). Returns
        the loop's reason (`call-term`, `empty`, `term-cap`, `done`, `capped`); `done` ends
        the session with the outcome `ok` and `capped` with `capped` (DECISIONS D-011,
        D-012, D-069). A per-term cap prompts again, as D-011 says."""
        if self.outcome is not None:
            raise RuntimeError(f"the session has ended ({self.outcome})")
        enter_term(self.world, term)
        reason = run_until_prompt(self.world)
        self.reasons.append(reason)
        if reason == "done":
            self.outcome = "ok"
        elif reason == "capped":
            self.outcome = "capped"
        return reason

    def handle(self, text: str) -> str:
        """Take one console line: a term is entered, a command's lines are written, `bye`
        ends the session, anything else writes the hint and prompts again (DECISION D-054).
        Returns the loop's reason for a term, the command's name, `bye` or `unknown`."""
        kind, value = parse_line(text)
        if kind == "term":
            return self.enter(value)
        if kind == "bye":
            self.outcome = "bye"
            return "bye"
        if kind == "command":
            for line in self.command_lines(value):
                self.write(line)
            return value
        self.write(HINT)
        return "unknown"

    def run(self) -> str:
        """The console loop: prompt, read, handle, until the session ends; the end of the
        input is the outcome `eof` (a stream) or `no-terms` (a script) (DECISION D-069).
        Returns the outcome."""
        while self.outcome is None:
            text = self.console.ask("\n" + PROMPT)
            if text is None:
                self.outcome = "no-terms" if self.console.scripted else "eof"
                break
            self.handle(text)
        return self.outcome

    def write(self, line: str) -> None:
        """Write one line of a command's output (DECISION D-054)."""
        self.out.write(line + "\n")

    # ---- the console commands [M86 pp. 166-179] (DECISION D-054) ---------------------------------

    def command_lines(self, name: str) -> list:
        """The lines a console command prints (DECISION D-054)."""
        if name == "show":
            return self.show_lines()
        if name == "show-hypothesis":
            return [self.show_hypothesis()]
        if name == "show-seq":
            return [self.show_seq()]
        if name == "show-parse":
            return [self.show_parse()]
        raise ValueError(f"unknown command {name!r}")

    def show_hypothesis(self) -> str:
        """`show-hypothesis`: the hypothesis's form, `(Cycle 3 2 (3 7))` [M86 p. 166];
        `nil` when there is none (the transcripts never ask before one exists; Lisp's
        empty answer, DECISION D-054)."""
        hypothesis = self.world.hypothesis
        return "nil" if hypothesis is None else hypothesis.show_form()

    def show_seq(self) -> str:
        """`show-seq`: the terms so far, `3 7 3 3` [M86 p. 169]."""
        return " ".join(str(t) for t in self.world.terms)

    def show_parse(self) -> str:
        """`show-parse`: the parenthesization, `((1 2) (1))` [M86 p. 174]
        (`gnoths.parse_text`, DECISION D-051)."""
        return gnoths.parse_text(self.world)

    def show_lines(self) -> list:
        """`show`: `terms of the sequence:` and the terms, a blank line, `bonds:` with one
        line per bond, a blank line, `gloms:` with one line per glom in creation order,
        nested gloms included and glints left out, a blank line, and the `gnoths:` block
        [M86 pp. 167-168, 171, 174-175, 178-179] (DECISION D-054). A section with nothing
        in it keeps its heading."""
        world = self.world
        lines = ["terms of the sequence:", self.show_seq(), "", "bonds:"]
        lines.extend(bond.show_line() for bond in bonds(world))
        lines.extend(["", "gloms:"])
        lines.extend(glom_line(glom) for glom in world.cytoplasm
                     if isinstance(glom, cy.Glom))
        lines.append("")
        lines.extend(gnoths.show_gnoths_lines(world))
        return lines

    # ---- the end of a session (DECISION D-069) ----------------------------------------------

    def summary(self) -> str:
        """One line on how the run ended, the reconstruction's own, for the non-interactive
        mode: `outcome: ok after 4 terms, 57 tasks (seed 1): (Cycle 3 2 (3 7))`
        (DECISION D-069)."""
        world = self.world
        form = self.show_hypothesis()
        return (f"outcome: {self.outcome} after {len(world.terms)} terms, {world.clock} tasks "
                f"(seed {self.seed}): {form if form != 'nil' else 'no hypothesis'}")


def glom_line(glom: cy.Glom) -> str:
    """One glom as `show` prints it: `glom1 (Same print-value fence) --> (3 7) terms 1 to 2`
    for a bottom-up glom and `glom3 pseudo --> ((3 7))  terms 1 to 2` for a pseudo-glom
    [M86 p. 167] (two spaces before `terms` on every pseudo-glom line of the transcripts
    [M86 pp. 167, 171, 175, 178] and one on the others; DECISION D-054)."""
    gap = "  " if glom.pseudo else " "
    return (f"{glom.name} {glom.type_text()} --> {value_text(glom.print_value)}{gap}"
            f"terms {glom.start_position} to {glom.end_position}")
