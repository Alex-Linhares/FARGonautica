"""find-answer (D-056, D-057): "to announce the current object corresponding to the touched
object and to stop the program" [F95 p. 105].

"The run ends only when a find-answer codelet is run." [F95 p. 105] The codelet reads the
answer off the Worldview (`answer.answer_for`) and the run loop stops the run on its
outcome.
"""

from tabletop import answer as AN
from tabletop import roles as R
from tabletop.codelets.common import ANSWER, Outcome


def find_answer(world, codelet, a: float) -> Outcome:
    """find-answer (D-057, D-063): the answer as the Worldview stands; the outcome carries
    it and the run ends [F95 p. 94; F95 p. 105]. A `blocked` answer (D-101, consumer C4)
    records the CLAIMED tag it read as consumed; an answer matched on the touched object's
    roles (D-097, consumer C3) records those tags as consumed."""
    answer = AN.answer_for(world)
    if answer.roles:
        AN.consume_roles(world, answer, codelet.type)
    if answer.through == AN.BLOCKED:
        R.consumed(world, AN.claimed_counterpart(world), R.C4, "the answer recorded as blocked",
                   codelet.type)
    label = f" ({answer.label})" if answer.label else " (none)"
    detail = f"Eliza touches {answer.object}{label}"
    if answer.literal:
        detail += " literal"
    if answer.note:
        detail += f"; {answer.note}"
    return Outcome(ANSWER, detail, answer=answer)
