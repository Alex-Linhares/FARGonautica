"""Sequence entry, the pure part: lib/SGUI.pm's ask_seq and ask_for_more_terms without Tk.

- ``read_sequence_list``: SGUI.pm's BEGIN block (config/sequence.list, chomped, the lines with
  a digit); ``combo_list``: what Tk::ComboEntry's listbox shows (SelectionList sorts it).
- ``check_sequence``: the ``$check_and_accept_input_sequence`` closure. A character other
  than a digit, white space, comma or '-' sets the label to "Illformed input: $v"; text
  without a digit is ignored (the label keeps its text); anything else is accepted and split
  into terms (the caller then clears the workspace, coderack and stream and inserts them).
  The terms are not checked further: "1-2" or a leading comma's empty field pass, and
  SWorkspace's insert dies on them (oracle/gui_seqentry.pl).
- ``split_terms``: both subs' ``s/^\\s+//; s/\\s+$//; split(/[,\\s]+/, $v)`` (a leading
  empty field stays, trailing ones go; '' gives no terms). ask_for_more_terms inserts
  whatever it gives, without the check, even no terms at all.
"""
import re
from collections import namedtuple
from pathlib import Path

SEQUENCE_LIST = Path(__file__).resolve().parents[4] / "config" / "sequence.list"

TITLE = "Seqsee Sequence Entry"
PROMPT = "Enter sequence(space separated): "
GO = "Go"
COMBO_WIDTH = 40                  # the ComboEntry's -width (characters)
MORE_TERMS_TITLE = "Request for more terms"
MORE_TERMS_PROMPT = "I am stuck! Please provide more terms! (space separated): "
MORE_TERMS_WIDTH = 20             # Tk::Entry's default -width

# PERL-QUIRK: ask_seq logs 'New Sequence Started: ', [], "$seq\n" right after building the
# Toplevel, while $seq is still undef: the line never shows the sequence.
NEW_SEQUENCE_MESSAGE = ("New Sequence Started: ", [], "\n")

Check = namedtuple("Check", "accepted terms message")

# Perl's \s and \d here (byte strings from Tk; ASCII is what can be typed meaningfully).
_ILLFORMED = re.compile(r"[^\d\s,-]", re.ASCII)
_DIGIT = re.compile(r"\d", re.ASCII)


def read_sequence_list(path=SEQUENCE_LIST):
    """SGUI.pm's BEGIN: ``@seq = grep { /\\d/ } <$in>``, chomped."""
    lines = Path(path).read_text().split("\n")
    if lines and lines[-1] == "":
        lines.pop()
    return [line for line in lines if _DIGIT.search(line)]


def combo_list(seqs):
    """Tk::ComboEntry's SelectionList: ``sort``ed (string order), each chomped."""
    return sorted(s[:-1] if s.endswith("\n") else s for s in seqs)


def strip(v):
    """``$v =~ s/^\\s+//; $v =~ s/\\s+$//``."""
    return re.sub(r"\s+\Z", "", re.sub(r"\A\s+", "", v, flags=re.ASCII), flags=re.ASCII)


def split_terms(v):
    """``split(/[,\\s]+/, $v)`` after stripping: a leading empty field is kept, trailing
    empty fields dropped; an empty string gives no fields."""
    v = strip(v)
    if v == "":
        return []
    fields = re.split(r"[,\s]+", v, flags=re.ASCII)
    while fields and fields[-1] == "":
        fields.pop()
    return fields


def check_sequence(v):
    """``$check_and_accept_input_sequence->($v, $label)``: a ``Check(accepted, terms,
    message)``; ``message`` is the label's new text, or None if the label keeps its text."""
    if _ILLFORMED.search(v):
        return Check(False, None, f"Illformed input: {v}")
    if not _DIGIT.search(v):
        return Check(False, None, None)
    return Check(True, split_terms(v), None)
