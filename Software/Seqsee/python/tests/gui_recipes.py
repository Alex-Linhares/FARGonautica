"""Named, deterministic workspace states for the GUI tests (loop0002).

Each recipe builds a model state directly (inserting elements, creating groups, relations,
metonyms, bar lines, highlights), never by running the stochastic model. The GUI drawing
oracles (``oracle/gui_*.pl``, through ``oracle/GuiRecipes.pm``) build the same states under
the same names, so a Python
snapshot of a recipe can be drawn and compared with the Perl canvas dump of that recipe.

``build(name)`` resets the workspace and returns a dict of the named objects it created.
"""
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scoderack, sltm, sworkspace
from seqsee import snode_activation as sna
from seqsee.categories.interlaced import Interlaced
from seqsee.categories.mapping_based import MappingBased
from seqsee.constants import METO_MODE
from seqsee.mapping import dir as mapping_dir
from seqsee.mapping.dir import MappingDir
from seqsee.mapping.numeric import MappingNumeric
from seqsee.mapping.structural import MappingStructural
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.scodelet import SCodelet
from seqsee.sbindings import SBindings
from seqsee.sint import SInt
from seqsee.srelation import SRelation, SRelationStructural
from seqsee.sstream2 import _key as _component_key
from seqsee.sthought.relations import SThoughtSRelation
from seqsee.sthought.scat import SThoughtSCat
from seqsee.sthought.sobject import SThoughtSeqseeAnchored, SThoughtSeqseeElement


def _init(*seq):
    Global.Feature.clear()
    Global.Steps_Finished = 0
    Global.CurrentRunnableString = ""
    Global.Hilit.clear()
    sltm.clear()
    sworkspace.init({"seq": list(seq)})
    sworkspace.clear_bar_lines()
    scoderack.clear()
    Global.MainStream.clear()
    Global.MainStream.hit_intensity.clear()
    Global.MainStream.thought_hit_intensity.clear()
    return sworkspace.get_elements()


def _group(cat, *items):
    g = Anchored.create(*items)
    if cat is not None:
        g.describe_as(cat)
    sworkspace.add_group(g)
    return g


def _reln(first, second, name, cat=None):
    r = SRelation({"first": first, "second": second,
                   "type": MappingNumeric.create(name, S.NUMBER if cat is None else cat)})
    r.insert()
    return r


def _struct_reln(first, second):
    rtype = MappingStructural.create({
        "category": S.ASCENDING, "meto_mode": METO_MODE.NONE,
        "direction_reln": MappingDir.create("Same"),
        "changed_bindings": {"start": MappingNumeric.create("succ", S.NUMBER)},
        "slippages": {}})
    r = SRelationStructural({"first": first, "second": second, "type": rtype})
    r.insert()
    return r


def _codelet(family, urgency, **args):
    scoderack.add_codelet(SCodelet(family, urgency, args))


def empty():
    _init()
    return {}


def one_element():
    e = _init(7)
    return {"e": e}


def six_elements():
    e = _init(1, 1, 2, 1, 2, 3)
    return {"e": e}


def twenty_elements():
    e = _init(*range(1, 21))
    return {"e": e}


def bar_lines():
    """Six elements and bar lines, added out of order (add_bar_lines sorts them); 6 is after
    the last element."""
    e = _init(1, 1, 2, 1, 2, 3)
    sworkspace.add_bar_lines(3, 0)
    sworkspace.add_bar_lines(6)
    return {"e": e}


def hilit_debug():
    """Highlighted elements (Hilit 1, 2 and 3), the debug feature (index labels), steps and a
    codelet as the last runnable (its family has no $NAME, so the label is empty)."""
    e = _init(1, 1, 2, 1, 2, 3)
    Global.Feature["debug"] = 1
    Global.hilit(1, e[1])
    Global.hilit(2, e[3])
    Global.hilit(3, e[5])
    Global.Steps_Finished = 42
    Global.CurrentRunnableString = "Seqsee::SCF::FocusOn"
    return {"e": e}


def runnable_thought():
    """A last-runnable string whose package does define $NAME (an SThought class), and one
    bar line."""
    e = _init(4, 5)
    Global.CurrentRunnableString = "SThought::Seqsee::Element"
    sworkspace.add_bar_lines(1)
    return {"e": e}


def groups_relations():
    """``1 2 3 2 2 2 4 5``: an ascending group (1 2 3) inside a larger group, a sameness group
    (2 2 2) with an active "each" metonym, relations inside and outside groups, two bar lines,
    a highlighted group and relation, 42 steps and a current runnable."""
    e = _init(1, 2, 3, 2, 2, 2, 4, 5)
    asc = _group(S.ASCENDING, e[0], e[1], e[2])
    same = _group(S.SAMENESS, e[3], e[4], e[5])
    same.annotate_with_metonym(S.SAMENESS, "each")
    same.set_metonym_activeness(1)
    big = _group(None, asc, same)
    r_in = _reln(e[0], e[1], "succ")      # inside asc: hidden unless highlighted
    r_out = _reln(e[6], e[7], "succ")     # not inside any group
    r_hi = _reln(e[1], e[2], "succ")      # inside asc, but highlighted
    sworkspace.add_bar_lines(0, 6)
    Global.hilit(1, asc)
    Global.hilit(2, r_hi)
    Global.Steps_Finished = 42
    Global.CurrentRunnableString = "Seqsee::SCF::FocusOn"
    return {"e": e, "asc": asc, "same": same, "big": big,
            "r_in": r_in, "r_out": r_out, "r_hi": r_hi}


def large():
    """30 elements ``1 2 3 1 2 3 …`` with a group per triple, a relation between each pair of
    neighbouring elements and between neighbouring triples (for timing)."""
    e = _init(*([1, 2, 3] * 10))
    groups = [_group(S.ASCENDING, *e[i:i + 3]) for i in range(0, 30, 3)]
    rels = [_reln(e[i], e[i + 1], "succ") for i in range(0, 30, 3)]
    rels += [_reln(e[i + 1], e[i + 2], "succ") for i in range(0, 30, 3)]
    sworkspace.add_bar_lines(*range(0, 30, 3))
    return {"e": e, "groups": groups, "rels": rels}


def nested_groups():
    """``1 2 1 2 3 1 2 3 4``: three ascending groups A, B, C; S1 = (A B) and S2 = (S1 C),
    three levels deep with distinct spans (so GetGroups' order is the same in Perl and
    Python). Highlighted groups (Hilit 1 on B, 3 on S2) are raised above the others.
    Relations: A→B and e0→e1 are hidden (neighbours in a group), S1→C is hidden but
    highlighted, B→C, A→C and e1→e2 are drawn."""
    e = _init(1, 2, 1, 2, 3, 1, 2, 3, 4)
    a = _group(S.ASCENDING, e[0], e[1])
    b = _group(S.ASCENDING, e[2], e[3], e[4])
    c = _group(S.ASCENDING, e[5], e[6], e[7], e[8])
    s1 = _group(None, a, b)
    s2 = _group(None, s1, c)
    rels = {"ab": _reln(a, b, "succ"), "e01": _reln(e[0], e[1], "succ"),
            "s1c": _reln(s1, c, "succ"), "bc": _reln(b, c, "succ"),
            "ac": _reln(a, c, "succ"), "e12": _reln(e[1], e[2], "pred")}
    Global.hilit(1, b)
    Global.hilit(3, s2)
    Global.hilit(2, rels["s1c"])
    return {"e": e, "a": a, "b": b, "c": c, "s1": s1, "s2": s2, **rels}


def metonyms():
    """``2 2 2 2 3 3 5 3 2 1``: the largest group M (2 2 2 2) has an active "each" metonym;
    P (3 3) has one that is not active; element 6 (5) has an active metonym and is squinted
    as a group (group_p); element 5 is squinted only; D (3 2 1) is descending. Relations
    start from the anchors of squinted elements."""
    e = _init(2, 2, 2, 2, 3, 3, 5, 3, 2, 1)
    m = _group(S.SAMENESS, e[0], e[1], e[2], e[3])
    m.annotate_with_metonym(S.SAMENESS, "each")
    m.set_metonym_activeness(1)
    p = _group(S.SAMENESS, e[4], e[5])
    p.annotate_with_metonym(S.SAMENESS, "each")
    d = _group(S.DESCENDING, e[7], e[8], e[9])
    e[6].annotate_with_metonym(S.SAMENESS, "each")
    e[6].set_metonym_activeness(1)
    e[6].set_group_p(1)
    e[5].set_group_p(1)
    rels = {"mp": _reln(m, p, "succ"), "e67": _reln(e[6], e[7], "pred"),
            "e56": _reln(e[5], e[6], "succ")}
    return {"e": e, "m": m, "p": p, "d": d, **rels}


def overlapping_relations():
    """``1 2 3 4 5 6 7`` with an ascending group G (1 2 3) and overlapping relations: two
    hidden ones inside G, e2→e3, e3→e4, e2→e4, e0→e6 (long), e6→e5 (leftwards), G→e5, and
    one whose second end is an element outside the workspace (not drawn). Bar lines 3, 7."""
    e = _init(1, 2, 3, 4, 5, 6, 7)
    g = _group(S.ASCENDING, e[0], e[1], e[2])
    outside = Element.create(9, 9)
    rels = [_reln(e[0], e[1], "succ"), _reln(e[1], e[2], "succ"),
            _reln(e[2], e[3], "succ"), _reln(e[3], e[4], "succ"),
            _reln(e[2], e[4], "succ"), _reln(e[0], e[6], "succ"),
            _reln(e[6], e[5], "pred"), _reln(g, e[5], "succ"),
            _reln(e[4], outside, "succ")]
    sworkspace.add_bar_lines(3, 7)
    return {"e": e, "g": g, "outside": outside, "rels": rels}


def squint_no_groups():
    """``4 4 4``: element 1 squinted (group_p), element 2 with an active metonym, but no
    groups. PERL-QUIRK: DrawGroups returns before drawing such elements when there are no
    groups. Relation e0→e1 uses the plain element anchors."""
    e = _init(4, 4, 4)
    e[1].set_group_p(1)
    e[2].annotate_with_metonym(S.SAMENESS, "each")
    e[2].set_metonym_activeness(1)
    r = _reln(e[0], e[1], "succ")
    return {"e": e, "r": r}


def attention_elements():
    """Attention (SGUI::Workspace_Attention): codelets whose arguments are elements (and a
    string), urgencies summing to 100: e0 0.8 (clamped), e1 0.2, e2 0.05, e4 0.1, e3 and e5
    0. Hilit 1 on e1, the debug feature, bar lines 0 and 3, a current runnable."""
    e = _init(1, 1, 2, 1, 2, 3)
    _codelet("AreRelated", 20, a=e[0], b=e[1])
    _codelet("Reader", 5, core=e[2])
    _codelet("Bigger", 60, core=e[0])
    _codelet("Mid", 10, core=e[4])
    _codelet("Str", 5, s="x")
    Global.Feature["debug"] = 1
    Global.hilit(1, e[1])
    sworkspace.add_bar_lines(0, 3)
    Global.Steps_Finished = 7
    Global.CurrentRunnableString = "Seqsee::SCF::FocusOn"
    return {"e": e}


def attention_groups():
    """``1 2 1 2 3 1 2 3 4 5``: groups A, B, C, S1 = (A B), S2 = (S1 C) (distinct spans),
    element 9 squinted with an active metonym, no relations. A FocusOn codelet spreads
    attention by the reader's distribution; the others point at B, S2 and element 9.
    Hilit 1 on B, 3 on S2."""
    e = _init(1, 2, 1, 2, 3, 1, 2, 3, 4, 5)
    a = _group(S.ASCENDING, e[0], e[1])
    b = _group(S.ASCENDING, e[2], e[3], e[4])
    c = _group(S.ASCENDING, e[5], e[6], e[7], e[8])
    s1 = _group(None, a, b)
    s2 = _group(None, s1, c)
    e[9].annotate_with_metonym(S.SAMENESS, "each")
    e[9].set_metonym_activeness(1)
    e[9].set_group_p(1)
    _codelet("FocusOn", 40)
    _codelet("G", 30, core=b)
    _codelet("H", 30, core=s2, other=e[9])
    Global.hilit(1, b)
    Global.hilit(3, s2)
    return {"e": e, "a": a, "b": b, "c": c, "s1": s1, "s2": s2}


def attention_relations():
    """groups_relations plus codelets on the reader, two elements and a relation:
    Workspace_Attention dies on the first relation (SReln::draw_attention is not
    SRelation's)."""
    r = groups_relations()
    _codelet("FocusOn", 50)
    _codelet("AreRelated", 25, a=r["e"][0], b=r["e"][3])
    _codelet("Rel", 25, core=r["r_out"])
    return r


def _activate(index, value):
    sltm.ACTIVATIONS[index][sna.REAL_ACTIVATION] = value


def slipnet_small():
    """Slipnet (SGUI::Slipnet): elements 1 2 3 (nodes 1-3), then ascending, sameness, the
    succ mapping (with its dependencies) and descending. Real activations set directly: plat1
    0.9, plat2 exactly 0.01 (not shown: the test is >), plat3 0.5, ascending 1, sameness left
    at the initial 0.003 (not shown), succ 0.0101, descending 0.25; number spiked by 40 (raw
    10, 0.0145)."""
    e = _init(1, 2, 3)
    asc = sltm.get_memory_index(S.ASCENDING)
    sltm.get_memory_index(S.SAMENESS)
    succ = sltm.get_memory_index(MappingNumeric.create("succ", S.NUMBER))
    desc = sltm.get_memory_index(S.DESCENDING)
    _activate(1, 0.9)
    _activate(2, 0.01)
    _activate(3, 0.5)
    _activate(asc, 1)
    _activate(succ, 0.0101)
    _activate(desc, 0.25)
    sltm.spike_by(40, S.NUMBER)
    return {"e": e}


def slipnet_full():
    """40 nodes (elements 1..40); node i has activation (i % 7 + 1) / 8, except every fifth,
    0.005 (not shown). 32 are shown: the 31st lands in a fourth column (col 3), then DrawIt
    stops."""
    e = _init(*range(1, 41))
    for i in range(1, sltm.NodeCount + 1):
        _activate(i, (i % 7 + 1) / 8 if i % 5 else 0.005)
    return {"e": e}


def slipnet_long_text():
    """Long concept texts (cut to MaxTextWidth = 30): 1..12, and the ascending groups (1..6)
    and (7..12) inside a larger group. Node i has activation 0.02 + (i % 4) * 0.3."""
    e = _init(*range(1, 13))
    g1 = _group(S.ASCENDING, *e[0:6])
    g2 = _group(S.ASCENDING, *e[6:12])
    _group(None, g1, g2)
    for i in range(1, sltm.NodeCount + 1):
        _activate(i, 0.02 + (i % 4) * 0.3)
    return {"e": e}


def slipnet_dir():
    """Mapping::Dir nodes, which have no as_text: elements 4 5, then Dir Same (0.005, not
    shown) and Dir Different (0.8, shown). Activations: plat4 0.6, plat5 0.3."""
    e = _init(4, 5)
    same = sltm.get_memory_index(mapping_dir.SAME)
    diff = sltm.get_memory_index(mapping_dir.DIFFERENT)
    _activate(1, 0.6)
    _activate(2, 0.3)
    _activate(same, 0.005)
    _activate(diff, 0.8)
    return {"e": e}


def _history(**counts):
    for family, count in counts.items():
        scoderack.HistoryOfRunnable["Seqsee::SCF::" + family] = count


def coderack_small():
    """Coderack (SGUI::Coderack): five codelets of four families (urgencies sum 120) and a
    history of runs: Reader 12, FocusOn 6 and AttemptExtension 2 (not on the rack).
    AreRelated and Bigger are on the rack but not in the history (DrawIt adds them with 0)."""
    e = _init(1, 2, 3)
    _codelet("Reader", 30, core=e[0])
    _codelet("Reader", 10)
    _codelet("AreRelated", 20, a=e[0], b=e[1])
    _codelet("FocusOn", 15)
    _codelet("Bigger", 45, core=e[2])
    _history(Reader=12, FocusOn=6, AttemptExtension=2)
    return {"e": e}


def coderack_zero_urgency():
    """Codelets whose urgencies are all 0 (URGENCIES_SUM 0: the sums become '---'), no
    history (no run so far: no red bars)."""
    _init()
    _codelet("Reader", 0)
    _codelet("FocusOn", 0)
    _codelet("Reader", 0)
    return {}


def coderack_history_only():
    """No codelet on the rack, only a history (one family with a count of 0)."""
    _init()
    _history(Reader=3, FocusOn=1, AreRelated=0)
    return {}


def coderack_many():
    """55 families: 25 codelets F00..F24 (urgency i + 1; the rack's maximum), and a history of
    F00..F04 (5 - i) and H00..H29 (i + 1). Three columns of 16 rows are drawn, then DrawIt
    stops."""
    _init()
    for i in range(25):
        _codelet("F%02d" % i, i + 1)
    _history(**{"F%02d" % i: 5 - i for i in range(5)})
    _history(**{"H%02d" % i: i + 1 for i in range(30)})
    return {}


def _stream(current, *older):
    """Sets the stream's thoughts directly (add_thought would run get_actions and choose at
    random)."""
    stream = Global.MainStream
    stream.current_thought = current
    stream.older_thoughts = list(older)
    stream.older_thought_count = len(older)
    for t in (current,) + older:
        if t:
            stream.thoughts_set[t] = t


def _thought_hits(*pairs):
    for thought, hit in pairs:
        Global.MainStream.thought_hit_intensity[thought] = hit


def _component_hits(hits):
    for component, hit in hits.items():
        Global.MainStream.hit_intensity[_component_key(component)] = hit


def stream_current():
    """Stream (SGUI::Stream): only a current thought, with its real fringe (get_fringe of an
    element: literal platonics and absolute positions) and no hit intensity (undef)."""
    e = _init(3, 5, 7)
    t = SThoughtSeqseeElement({"core": e[1]})
    t.stored_fringe(t.get_fringe())
    _stream(t)
    return {"e": e, "t": t}


def stream_small():
    """A current thought and four older ones of the four thought classes, with hand-made
    fringes: 4 components (3 drawn: a string, a number, an SInt; then a category), 1, none
    ([]), undef (never thought), 2 (a float and a string). Thought hit intensities 100, 500,
    2500 (clamped to 2000), none (undef) and 0; component hit intensities for two components."""
    e = _init(1, 2, 3, 4)
    g = _group(S.ASCENDING, e[0], e[1], e[2])
    r = _reln(e[2], e[3], "succ")
    cur = SThoughtSeqseeElement({"core": e[3]})
    cur.stored_fringe([["absolute_position_3", 80], [4, 100], [SInt(5), 30], [S.ASCENDING, 50]])
    tg = SThoughtSeqseeAnchored({"core": g})
    tg.stored_fringe([[S.ASCENDING, 100]])
    tr = SThoughtSRelation({"core": r})
    tr.stored_fringe([])
    tc = SThoughtSCat({"core": S.ASCENDING})
    te = SThoughtSeqseeElement({"core": e[0]})
    te.stored_fringe([[0.5, 20], ["x", 10]])
    _stream(cur, tg, tr, tc, te)
    _thought_hits((cur, 100), (tg, 500), (tr, 2500), (te, 0))
    _component_hits({"absolute_position_3": 80, 4: 100})
    return {"e": e, "g": g, "r": r, "cur": cur, "tg": tg, "tr": tr, "tc": tc, "te": te}


def stream_full():
    """No current thought; 12 older element thoughts and a '' (skipped; antiquating with no
    current thought leaves one): rows 1, 2, then 0..2 per column; the 12th is in a fifth
    column, right of the rectangle. Hit intensities 150 * i; i % 5 components each."""
    e = _init(*range(1, 13))
    t = []
    for i in range(12):
        th = SThoughtSeqseeElement({"core": e[i]})
        th.stored_fringe([["c%d_%d" % (i, j), 10 * j] for j in range(1, i % 5 + 1)])
        t.append(th)
    _stream("", *t[0:5], "", *t[5:12])
    _thought_hits(*((t[i], 150 * i) for i in range(12)))
    return {"e": e, "t": t}


def stream_real():
    """Real fringes (get_fringe) of a group, a relation, a category and an element: most
    components are objects (platonics, categories, mappings, elements), which Perl draws as
    stringified refs."""
    e = _init(1, 2, 3, 2, 2, 2)
    asc = _group(S.ASCENDING, e[0], e[1], e[2])
    r = _reln(e[0], e[1], "succ")
    cur = SThoughtSeqseeAnchored({"core": asc})
    tr = SThoughtSRelation({"core": r})
    tc = SThoughtSCat({"core": S.SAMENESS})
    te = SThoughtSeqseeElement({"core": e[4]})
    for th in (cur, tr, tc, te):
        th.stored_fringe(th.get_fringe())
    _stream(cur, tr, tc, te)
    _thought_hits((tr, 40), (te, 1999))
    return {"e": e, "asc": asc, "r": r}


def relations_pane():
    """``1 2 3 4 5 6 7 8``, groups A (1 2 3) and B (4 5 6): relations of every kind the
    Relations pane shows, with strengths set after insert (0, 5.555, 100, 123456.789,
    -3.14159, 0.005, 42): succ, pred and same on NUMBER, succ on EVEN, an
    SRelationStructural between the groups, a group to an element, and one to an element
    outside the workspace."""
    e = _init(1, 2, 3, 4, 5, 6, 7, 8)
    a = _group(S.ASCENDING, e[0], e[1], e[2])
    b = _group(S.ASCENDING, e[3], e[4], e[5])
    outside = Element.create(9, 9)
    r = [_reln(e[0], e[1], "succ"), _reln(e[2], e[1], "pred"), _reln(e[6], e[7], "same"),
         _reln(e[6], e[0], "succ", S.EVEN), _struct_reln(a, b), _reln(a, e[6], "succ"),
         _reln(e[7], outside, "succ")]
    for reln, strength in zip(r, (0, 5.555, 100, 123456.789, -3.14159, 0.005, 42)):
        reln.set_strength(strength)
    return {"e": e, "a": a, "b": b, "r": r}


def relations_many():
    """27 elements, 26 relations e_i → e_(i+1) (more than RowCount = 22 rows), strength
    3.7 i."""
    e = _init(*(1 + i % 9 for i in range(27)))
    r = [_reln(e[i], e[i + 1], "succ") for i in range(26)]
    for i, reln in enumerate(r):
        reln.set_strength(3.7 * i)
    return {"e": e, "r": r}


def groups_list():
    """``1 2 3 4 5 6 7 7 7 8``: groups for the Groups list. A = ascending (1 2 3), locked,
    strength 55.555; B = ascending (4 5 6) with a second category (EVEN, empty bindings),
    strength 0; C = sameness (7 7 7), locked, strength 100; D = (A B), no category, strength
    99.999."""
    e = _init(1, 2, 3, 4, 5, 6, 7, 7, 7, 8)
    a = _group(S.ASCENDING, e[0], e[1], e[2])
    b = _group(S.ASCENDING, e[3], e[4], e[5])
    b.add_category(S.EVEN, SBindings.create({}, {}, b))
    c = _group(S.SAMENESS, e[6], e[7], e[8])
    d = _group(None, a, b)
    a.set_is_locked_against_deletion(1)
    c.set_is_locked_against_deletion(1)
    a.set_strength(55.555)
    b.set_strength(0)
    c.set_strength(100)
    d.set_strength(99.999)
    return {"e": e, "a": a, "b": b, "c": c, "d": d}


def groups_many():
    """60 elements, 30 disjoint pair groups (no category; equal spans, so hash order in
    Perl), strength 3.3 i, every 7th locked: more groups than one page holds."""
    e = _init(*(1 + i % 5 for i in range(60)))
    g = [_group(None, e[2 * i], e[2 * i + 1]) for i in range(30)]
    for i, gp in enumerate(g):
        gp.set_strength(3.3 * i)
        if i % 7 == 0:
            gp.set_is_locked_against_deletion(1)
    return {"e": e, "g": g}


def _add_category(obj, cat):
    obj.add_category(cat, SBindings.create({}, {}, obj))


def categories_many():
    """Categories list (SGUI::List::Categories): 28 elements (category number), 14 disjoint
    pair groups G0..G13, Gi with the i-th of ascending, descending, mountain, sameness, prime,
    odd, even, interlaced 2..8 (added with empty bindings), G0 also descending, and BIG =
    (G0 G1) ascending: 15 categories, more than a 780x450 page holds (10)."""
    e = _init(*(1 + i % 9 for i in range(28)))
    cats = [S.ASCENDING, S.DESCENDING, S.MOUNTAIN, S.SAMENESS, S.PRIME, S.ODD, S.EVEN] + [
        Interlaced.create(n) for n in range(2, 9)]
    g = [_group(None, e[2 * i], e[2 * i + 1]) for i in range(14)]
    for gp, cat in zip(g, cats):
        _add_category(gp, cat)
    _add_category(g[0], S.DESCENDING)
    big = _group(None, g[0], g[1])
    _add_category(big, S.ASCENDING)
    return {"e": e, "g": g, "big": big}


def stream_list_many():
    """Stream list (SGUI::List::Stream): a current thought with four components (activations
    1, 3, 2, 3: sorted 3, 3, 2, 1, stable) and 14 older element thoughts Ti (i = 1..14): no
    stored fringe when i % 4 == 0, an empty one when i % 4 == 1, else [["f<i>", i], ["g<i>",
    0.5]]. Hit intensities: none when i % 3 == 0, 2.5 for T7, else 10 * (i % 5) (ties)."""
    e = _init(*range(1, 16))
    cur = SThoughtSeqseeElement({"core": e[0]})
    cur.stored_fringe([["a", 1], ["b", 3], ["c", 2], [SInt(5), 3]])
    t = []
    for i in range(1, 15):
        th = SThoughtSeqseeElement({"core": e[i]})
        if i % 4 == 1:
            th.stored_fringe([])
        elif i % 4:
            th.stored_fringe([["f%d" % i, i], ["g%d" % i, 0.5]])
        t.append(th)
    _stream(cur, *t)
    _thought_hits(*((t[i - 1], 2.5 if i == 7 else 10 * (i % 5)) for i in range(1, 15) if i % 3))
    return {"e": e, "cur": cur, "t": t}


def stream_hole():
    """A current thought, then older T1 (hit 7), a '' and T2 (no hit): the '' sorts between
    them and its row dies in DrawOneItem (as_text on '')."""
    e = _init(1, 2, 3)
    t = [SThoughtSeqseeElement({"core": x}) for x in e]
    for th in t:
        th.stored_fringe([["x", 1]])
    _stream(t[0], t[1], "", t[2])
    _thought_hits((t[1], 7))
    return {"e": e, "t": t}


def solution():
    """The end of a solved run on ``1 1 2 1 2 3`` (item 022; modelled on the Python GUI run
    with seed 7): ``1 1 2 1 2 3 1 2 3 4 1 2 3 4 5``, ascending blocks A (1 2), B (1 2 3),
    C (1 2 3 4), D (1 2 3 4 5), the first element squinted, BIG = (e0 A B C D) with a
    mapping-based category (strengths set: 74.4, blocks 56.5 .. 68.5); no relations
    (DescribeSolution deleted them); Hilit 2 on C and D. Slipnet: succ 0.956, ascending 0.95,
    number 0.166, mountain 0.079. Coderack: 7 codelets (one on D), a history of 8 families.
    Stream: the current thought on BIG (hit 22500), older thoughts on D (hit 500), e9 (no
    hit) and ascending (no fringe). 677 steps; the last runnable was DescribeSolution."""
    e = _init(1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4, 5)
    a = _group(S.ASCENDING, *e[1:3])
    b = _group(S.ASCENDING, *e[3:6])
    c = _group(S.ASCENDING, *e[6:10])
    d = _group(S.ASCENDING, *e[10:15])
    e[0].set_group_p(1)
    big = _group(None, e[0], a, b, c, d)
    rtype = MappingStructural.create({
        "category": S.ASCENDING, "meto_mode": METO_MODE.NONE,
        "direction_reln": MappingDir.create("Same"),
        "changed_bindings": {"end": MappingNumeric.create("succ", S.NUMBER)},
        "slippages": {}})
    _add_category(big, MappingBased.create(rtype))
    for g, strength in zip((a, b, c, d, big), (56.5, 60.5, 64.5, 68.5, 74.4)):
        g.set_strength(strength)
    Global.hilit(2, c)
    Global.hilit(2, d)
    _activate(sltm.get_memory_index(MappingNumeric.create("succ", S.NUMBER)), 0.956)
    _activate(sltm.get_memory_index(S.ASCENDING), 0.95)
    _activate(sltm.get_memory_index(S.NUMBER), 0.166)
    _activate(sltm.get_memory_index(S.MOUNTAIN), 0.079)
    _codelet("FocusOn", 50)
    _codelet("ActOnOverlappingThoughts", 100)
    _codelet("ConvulseEnd", 10)
    _codelet("FocusOn", 50)
    _codelet("AttemptExtensionOfGroup", 80, core=d)
    _codelet("CheckProgress", 100)
    _codelet("FocusOn", 50)
    _history(FocusOn=219, ActOnOverlappingThoughts=134, AttemptExtensionOfRelation=117,
             AttemptExtensionOfGroup=81, MergeGroups=33, CheckProgress=27, CreateGroup=14,
             DescribeSolution=2)
    cur = SThoughtSeqseeAnchored({"core": big})
    cur.stored_fringe([["plat[1, [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]", 100],
                       ["[ascending] end => succ", 50], ["Gp based on [ascending]", 100]])
    td = SThoughtSeqseeAnchored({"core": d})
    td.stored_fringe([["plat[1, 2, 3, 4, 5]", 100], ["ascending", 100]])
    te = SThoughtSeqseeElement({"core": e[9]})
    te.stored_fringe([["plat4", 100], ["absolute_position_9", 80]])
    tc = SThoughtSCat({"core": S.ASCENDING})
    _stream(cur, td, te, tc)
    _thought_hits((cur, 22500), (td, 500))
    _component_hits({"ascending": 100})
    Global.Steps_Finished = 677
    Global.CurrentRunnableString = "Seqsee::SCF::DescribeSolution"
    return {"e": e, "a": a, "b": b, "c": c, "d": d, "big": big}


RECIPES = {f.__name__: f for f in
           (empty, one_element, six_elements, twenty_elements, bar_lines, hilit_debug,
            runnable_thought, groups_relations, large, nested_groups, metonyms,
            overlapping_relations, squint_no_groups, attention_elements, attention_groups,
            attention_relations, slipnet_small, slipnet_full, slipnet_long_text,
            slipnet_dir, coderack_small, coderack_zero_urgency, coderack_history_only,
            coderack_many, stream_current, stream_small, stream_full, stream_real,
            relations_pane, relations_many, groups_list, groups_many, categories_many,
            stream_list_many, stream_hole, solution)}


def build(name):
    return RECIPES[name]()
