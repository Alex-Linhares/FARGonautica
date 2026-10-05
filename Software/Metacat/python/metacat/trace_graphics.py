"""trace-graphics.ss: the Temporal Trace window (the engine's part).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from trace-graphics.ss, with
racket/engine/trace-graphics.rktl as a worked translation.

Only group-event-pexp-text-string is translated here, verbatim, because the
model calls it: trace.ss's make-group-event names every group event with it,
and that print name is in the golden traces ("name":">a-b-c>").  The rest of
trace-graphics.ss (the Trace window, the event icons and their pexps) is
translated in the panels item, as group_graphics.py's precedent
(anomalies_and_quirks.md, "Trace events and the EEG reach into graphics
files").  group-event-pexp-text-string only reads the group and the slipnet;
it never draws a random number and never imports tkinter.
"""
from __future__ import annotations

from metacat import chez, slipnet
from metacat.objects import tell
from metacat.utilities import adjacency_map, first, group_p, letter_p, tell_all

String = chez.String


def _string_append(strings):
    """(apply string-append strings) as a Scheme string; like Chez, every
    argument must be a string."""
    for s in strings:
        if not isinstance(s, str):
            raise chez.SchemeError("string-append", "~s is not a string", s)
    return String("".join(strings))


def group_event_pexp_text_string(group):
    """trace-graphics.ss: group-event-pexp-text-string"""
    # the let*, in order
    bond_facet = tell(group, "get-bond-facet")
    constituent_objects = tell(group, "get-constituent-objects")
    descriptors = tell_all(constituent_objects, "get-descriptor-for", bond_facet)

    def descriptor_string(object_, descriptor):
        if slipnet.platonic_number_p(descriptor):
            return String(chez.format_("~a", slipnet.platonic_number_to_number(descriptor)))
        if letter_p(object_):
            return tell(descriptor, "get-lowercase-name")
        if group_p(object_):
            return tell(descriptor, "get-uppercase-name")
        return None       # 1.2: a cond without else
    # chez: map's order of application (the procedure only reads)
    descriptor_strings = chez.map_(descriptor_string, constituent_objects, descriptors)
    return _string_append(
        [first(descriptor_strings)]
        + adjacency_map(lambda x, y: String(chez.format_("-~a", y)), descriptor_strings))
