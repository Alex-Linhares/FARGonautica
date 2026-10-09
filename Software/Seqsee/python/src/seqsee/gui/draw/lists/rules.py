"""The Rules list (lib/SGUI/List/Rules.pm): one row per rule, its as_text anchored nw.

PERL-QUIRK (confirmed by the oracle): GetItemList calls ``SRule->GetListOfSimpleRules`` and
``SRule->GetListOfCompoundRules``, which lib/SRule.pm doesn't define (neither does the port),
so DrawIt dies inside GetEntriesOnCurrentPage before anything is drawn and before the paging
state is set. ``draw_layers`` does the same unless it is given the rules' texts (``rules=``),
which is how the tests check DrawOneItem against the oracle's fake rules.

Rows are tagged ``rule<K>`` (K = the rule's index in the list).
"""
from .. import ops
from . import MARGIN, draw_list

NAME = "SGUI::List::Rules"
HEIGHT_PER_ROW = 15
NO_RULES = 'Can\'t locate class method "GetListOfSimpleRules" via package "SRule"'


def item_list(snap):
    """GetItemList: dies, as in Perl (the model has no rule lists)."""
    raise ops.DrawDied(NO_RULES, [])


def draw_one(left, top, entry):
    """DrawOneItem for one ``(K, as_text)`` entry; undef or '' leaves Tk's empty text."""
    _, text = entry
    if text:
        return [ops.text(left, top, anchor="nw", text=text)]
    return [ops.text(left, top, anchor="nw")]


def _tag(entry):
    return "rule%d" % entry[0]


def draw_layers(snap, x, y, w, h, page=0, rules=None, name=NAME):
    """Setup(canvas, x, y, w, h); PageNumber = page; DrawIt, as ``(lowered, others, state)``.
    ``rules``: the simple then compound rules' as_text, or None for the real SRule (dies)."""
    entries = item_list(snap) if rules is None else tuple(enumerate(rules))
    return draw_list(entries, draw_one, x, y, w, h, name, HEIGHT_PER_ROW, page=page,
                     tag_of=_tag, margin=MARGIN)


def draw(snap, x, y, w, h, page=0, rules=None, name=NAME):
    """The list of draw ops for page ``page``."""
    lowered, out, _ = draw_layers(snap, x, y, w, h, page, rules, name)
    return lowered + out
