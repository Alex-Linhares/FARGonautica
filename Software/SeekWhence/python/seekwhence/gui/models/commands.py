"""The console commands rebuilt from a snapshot: `show`, `show-hypothesis`, `show-seq`
and `show-parse`, formatted exactly as `Session.command_lines` prints them
[M86 pp. 166-179] (DECISION D-054; PANES.md P1 and P8; G-017). Qt-free.

The transcripts' four commands are typed at the prompt and print the state then
[M86 pp. 166-179]. In the GUI they are buttons (item 03) and, later, console commands
(item 04) that show the state *at the history's cursor*, which may be in the past, so
they cannot ask the live `Session`: they read the `Snapshot` in force (G-003) and print
it as the session would. `tests/gui/test_commands.py` checks the lines against
`Session.command_lines` at every prompt of the appendix runs.
"""

from seekwhence.forms import value_text
from seekwhence.session import COMMANDS

__all__ = ["COMMANDS", "command_lines", "glom_line", "show_lines"]


def command_lines(snapshot, name: str) -> list:
    """The lines the command prints for the snapshot (`Session.command_lines`, DECISION
    D-054); None, before any snapshot, is the empty world (G-017)."""
    if name == "show":
        return show_lines(snapshot)
    if name == "show-hypothesis":
        return [show_hypothesis(snapshot)]
    if name == "show-seq":
        return [show_seq(snapshot)]
    if name == "show-parse":
        return [show_parse(snapshot)]
    raise ValueError(f"unknown command {name!r}")


def show_hypothesis(snapshot) -> str:
    """`show-hypothesis`: the form, `(Cycle 3 2 (3 7))` [M86 p. 166], or `nil`
    (DECISION D-054)."""
    if snapshot is None or snapshot.hypothesis is None:
        return "nil"
    return snapshot.hypothesis.form


def show_seq(snapshot) -> str:
    """`show-seq`: the terms so far, `3 7 3 3` [M86 p. 169]."""
    if snapshot is None:
        return ""
    return " ".join(str(t) for t in snapshot.terms)


def show_parse(snapshot) -> str:
    """`show-parse`: the parenthesization, `((1 2) (1))` [M86 p. 174] (DECISION D-051);
    `()` for the empty world."""
    return "()" if snapshot is None else snapshot.parse


def glom_line(glom) -> str:
    """One glom as `show` prints it, `glom1 (Same print-value fence) --> (3 7) terms 1 to
    2`, with two spaces before `terms` on a pseudo-glom's line [M86 p. 167]
    (`session.glom_line`, DECISION D-054)."""
    gap = "  " if glom.pseudo else " "
    return (f"{glom.name} {glom.type_text} --> {value_text(glom.print_value)}{gap}"
            f"terms {glom.start} to {glom.end}")


def show_lines(snapshot) -> list:
    """`show`: the terms, the bonds, the gloms and the gnoths blocks, as
    `Session.show_lines` prints them [M86 pp. 167-168, 174-175, 178-179] (DECISION
    D-054)."""
    lines = ["terms of the sequence:", show_seq(snapshot), "", "bonds:"]
    if snapshot is not None:
        lines.extend(bond.show_line for bond in snapshot.bonds)
    lines.extend(["", "gloms:"])
    if snapshot is not None:
        lines.extend(glom_line(glom) for glom in snapshot.gloms)
    lines.extend(["", "gnoths:", ""])
    if snapshot is not None:
        for i, gnoth in enumerate(snapshot.gnoths):
            if i:
                lines.append("")
            lines.extend(f"{field}: {text}" for field, text in gnoth.fields)
    return lines
