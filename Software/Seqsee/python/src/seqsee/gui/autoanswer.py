"""An automatic answerer for GUI runs: what a user who knows the sequence would answer to the
model's questions, judged from the question's text alone (no model objects).

The questions are those the Commentary shows (lib/Tk/SCommentary.pm): lib/Seqsee.pm's ask
(". Is the next term 4??", ". Are the next 4 terms 1, 2, 3, and 4?") and UI/Graphical.pm's
ask_user_extension ("Is the next term 4?", "Are the next terms: 7 8?") through
MessageRequiringBooleanResponse; DescribeSolution's "Does this generate the sequence you had
in mind?" and main::message's 'continue' through MessageRequiringAResponse; and
SGUI::ask_for_more_terms' window.

``Answerer(known).decide(kind, text, choices, element_count)``: with the known terms (the
sequence and its continuation), a question about terms is answered yes only if they are the
known terms after the ``element_count`` elements on screen (beyond the known terms: no), as
``cli._RunState`` answers with ``--continuation``; other boolean questions yes; a response
question its first choice ("Yes", "continue"); more terms: the next known terms, up to the
next one that doesn't go up (for ``1 1 2 1 2 3 …``, a whole block). Without known terms: yes
to everything (``--answer yes``) and no more terms.
"""
import re

SOLUTION_QUESTION = "Does this generate the sequence you had in mind?"
_TERM_WORD = re.compile(r"\bterms?\b:?")
_INT = re.compile(r"-?\d+")


def asked_terms(text):
    """The terms a question asks about (the integers after its last "term"/"terms"), or None
    if it doesn't ask about terms."""
    parts = _TERM_WORD.split(text)
    if len(parts) < 2:
        return None
    terms = [int(t) for t in _INT.findall(parts[-1])]
    return terms or None


def _split(known):
    if known is None:
        return None
    if isinstance(known, str):
        known = known.replace(",", " ").split()
    return [str(t) for t in known]


class Answerer:
    """Decides the answers; remembers the questions asked beyond the known terms."""

    def __init__(self, known):
        self.known = _split(known)
        self.beyond_known = []

    @staticmethod
    def is_solution_question(text):
        return text.strip() == SOLUTION_QUESTION

    def decide(self, kind, text, choices, element_count):
        """The answer: "yes"/"no" (boolean), one of ``choices`` (response), the terms to type
        (more_terms; None closes the window)."""
        if kind == "boolean":
            terms = asked_terms(text)
            if self.known is None or terms is None:
                return "yes"
            at = int(element_count)
            if at + len(terms) > len(self.known):
                self.beyond_known.append(text)
                return "no"
            ok = all(int(self.known[at + i]) == t for i, t in enumerate(terms))
            return "yes" if ok else "no"
        if kind == "response":
            if self.is_solution_question(text):
                return "Yes" if "Yes" in choices else choices[0]
            return choices[0] if choices else None
        if kind == "more_terms":
            if self.known is None:
                return None
            at = int(element_count)
            nxt = self.known[at:at + self._next_block(at)]
            return " ".join(nxt) if nxt else None
        raise ValueError(f"unknown question kind {kind!r}")

    def _next_block(self, at):
        """How many terms to give from ``at``: up to the next term that is not greater than
        the one before it (at least one)."""
        n = 1
        while at + n < len(self.known) and int(self.known[at + n]) > int(self.known[at + n - 1]):
            n += 1
        return n
