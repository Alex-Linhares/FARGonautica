"""The adapter: JSON trace lines -> objects shaped like the ones Hager's pyglet client reads.

Hager's client (copycat-opengl/clients/opengl/) touches, and only touches:

  run.step(), run.timestep
  run.workspace.initial_string / modified_string / target_string / answer_string
      .get_letters() -> letters with .name, .is_changed, .left_string_position
      .get_bonds(), .get_proposed_bonds() (filtered by bond.string == string),
      .get_groups(), .get_proposed_groups()
  run.workspace.rule / translated_rule (.to_string()), .temperature,
      .correspondences(), .get_proposed_correspondences()
  bond.bond_category.name / .short_name, .left_object, .right_object, .direction_category
  group.group_category.name / .short_name, .direction_category, .left_string_position,
      .right_string_position, .type_name == 'group', .objects
  correspondence.object1, .object2, .concept_mappings[].is_distinguishing(),
      .descriptor1.short_name, .descriptor2.short_name
  run.slipnet.slipnodes[].short_name, .activation, .is_active(), .clamp
  run.coderack.time, .last_chosen.__class__.__name__, .codelets()[].__class__.__name__

The structure objects keep their identity from one line to the next (they are cached by a
key), so the client's redraw-on-change test (id-based) only fires when a structure appears,
disappears or gets built."""

import json
import re

# Mitchell's slipnode pname -> Hager's label, where her own cm-name is missing or long.
SHORT_NAMES = {
    'identity': 'ident', 'opposite': 'oppos', 'letter': 'letter', 'group': 'group',
    'object-category': 'obj-c', 'letter-category': 'ltr-c', 'length': 'length',
    'alphabetic-position-category': 'alph-c', 'string-position-category': 'str-c',
    'direction-category': 'dir-c', 'bond-category': 'bnd-c', 'group-category': 'grp-c',
    'bond-facet': 'facet', 'middle': 'middle', 'predgrp': 'predgr', 'succgrp': 'succgr',
    'samegrp': 'samegr',
}

# Mitchell's category pnames -> the names Hager's COLORS table knows.
HAGER_CATEGORY_NAMES = {
    'predgrp': 'predecessor group', 'succgrp': 'succesor group', 'samegrp': 'sameness group',
}

# Mitchell's codelet types -> the class names of Hager's codelets (his Coderack view lists them).
CODELET_CLASSES = {
    'bottom-up-bond-scout': 'BondBottomUpScout',
    'bond-builder': 'BondBuilder',
    'bond-strength-tester': 'BondStrengthTester',
    'top-down-bond-scout--category': 'BondTopDownCategoryScout',
    'top-down-bond-scout--direction': 'BondTopDownDirectionScout',
    'breaker': 'Breaker',
    'bottom-up-correspondence-scout': 'CorrespondenceBottomUpScout',
    'correspondence-builder': 'CorrespondenceBuilder',
    'important-object-correspondence-scout': 'CorrespondenceImportantObjectScout',
    'correspondence-strength-tester': 'CorrespondenceStrengthTester',
    'bottom-up-description-scout': 'DescriptionBottomUpScout',
    'description-builder': 'DescriptionBuilder',
    'description-strength-tester': 'DescriptionStrengthTester',
    'top-down-description-scout': 'DescriptionTopDownScout',
    'group-builder': 'GroupBuilder',
    'group-strength-tester': 'GroupStrengthTester',
    'top-down-group-scout--category': 'GroupTopDownCategoryScout',
    'top-down-group-scout--direction': 'GroupTopDownDirectionScout',
    'group-scout--whole-string': 'GroupWholeStringScout',
    'replacement-finder': 'ReplacementFinder',
    'rule-builder': 'RuleBuilder',
    'rule-scout': 'RuleScout',
    'rule-strength-tester': 'RuleStrengthTester',
    'rule-translator': 'RuleTranslator',
}

BUILT = 3   # Mitchell's %built% proposal level; 1 and 2 are proposed


class Concept(object):
    """A slipnode seen as a category or descriptor: a name and a label."""

    def __init__(self, pname, short_name=None):
        self.pname = pname
        self.name = HAGER_CATEGORY_NAMES.get(pname, pname)
        if short_name is None:
            short_name = SHORT_NAMES.get(pname, pname)
        self.short_name = short_name.lower() if len(short_name) == 1 else short_name

    def __repr__(self):
        return "Concept(%r)" % self.pname


class Slipnode(Concept):
    def __init__(self, pname, cm_name):
        Concept.__init__(self, pname, cm_name)
        self.activation = 0
        self.clamp = False

    def is_active(self):
        return self.activation >= 100


class Slipnet(object):
    def __init__(self, nodes):
        self.slipnodes = [Slipnode(pname, cm) for pname, cm in nodes]
        self.by_name = dict((n.pname, n) for n in self.slipnodes)

    def concept(self, pname):
        """The node of that name, or a bare Concept when the trace names one we lack."""
        if pname is None:
            return None
        return self.by_name.get(pname) or Concept(pname)

    def apply(self, line):
        for node, act, clamp in zip(self.slipnodes, line['act'], line['clamp']):
            node.activation = act
            node.clamp = clamp


class Letter(object):
    type_name = 'letter'

    def __init__(self, string, record):
        self.string = string
        self.name = record['p']
        self.left_string_position = self.right_string_position = record['pos']
        self.is_changed = record['ch']
        self.descriptions = [tuple(d) for d in record['d']]
        self.objects = []

    def __repr__(self):
        return "Letter(%s:%d)" % (self.name, self.left_string_position)


class Group(object):
    type_name = 'group'

    def __init__(self, string, left, right, category, direction):
        self.string = string
        self.left_string_position = left
        self.right_string_position = right
        self.group_category = category
        self.direction_category = direction
        self.objects = []
        self.built = False


class Bond(object):
    def __init__(self, string, left_object, right_object, category, direction):
        self.string = string
        self.left_object = left_object
        self.right_object = right_object
        self.bond_category = category
        self.direction_category = direction
        self.built = False


class ConceptMapping(object):
    def __init__(self, descriptor1, descriptor2, distinguishing):
        self.descriptor1 = descriptor1
        self.descriptor2 = descriptor2
        self.distinguishing = distinguishing

    def is_distinguishing(self):
        return self.distinguishing


class Correspondence(object):
    def __init__(self, object1, object2):
        self.object1 = object1
        self.object2 = object2
        self.concept_mappings = []
        self.built = False


RULE_RELATION = re.compile(
    r'^Replace (?P<oc1>[\w-]+) with (?P<facet>[\w-]+) "(?P<d1>[^"]+)" by (?P<oc2>[\w-]+) '
    r'with (?P<type>[\w-]+): (?P<rel>[\w-]+) of ')
RULE_PLAIN = re.compile(
    r'^Replace (?P<oc1>[\w-]+) with (?P<facet>[\w-]+) "(?P<d1>[^"]+)" by (?P<oc2>[\w-]+) '
    r'with (?P<type>[\w-]+) "(?P<d2>[^"]+)"')


def compact_rule(text):
    """Mitchell's (rule :print) line, shortened for the blackboard.

    'Replace letter with string-position-category "rmost" by letter with letter-category:
    successor of letter-category of letter with string-position-category: rmost'
        -> 'Replace letter-category of rmost letter by successor'
    'Replace letter with string-position-category "rmost" by letter with letter-category "D"'
        -> 'Replace letter-category of rmost letter by "D"'
    The full text stays in Rule.full."""
    m = RULE_RELATION.match(text)
    if m:
        return "Replace %s of %s %s by %s" % (m.group('type'), m.group('d1'), m.group('oc1'),
                                               m.group('rel'))
    m = RULE_PLAIN.match(text)
    if m:
        return 'Replace %s of %s %s by "%s"' % (m.group('type'), m.group('d1'), m.group('oc1'),
                                                 m.group('d2'))
    return text


class Rule(object):
    """A rule as Mitchell prints it (full) and as the GUI shows it (to_string)."""

    def __init__(self, text):
        self.full = text
        self.text = compact_rule(text)

    def to_string(self):
        return self.text


class WString(object):
    """One of the four workspace strings."""

    def __init__(self, key, records):
        self.key = key
        self.letters = [Letter(self, r) for r in records]
        self.name = "".join(l.name for l in self.letters)
        self.bonds = []
        self.proposed_bonds = []
        self.groups = []
        self.proposed_groups = []

    def get_letters(self):
        return self.letters

    def get_bonds(self):
        return self.bonds

    def get_proposed_bonds(self):
        return self.proposed_bonds

    def get_groups(self):
        return self.groups

    def get_proposed_groups(self):
        return self.proposed_groups

    def __repr__(self):
        return "WString(%s=%s)" % (self.key, self.name)


class Workspace(object):
    def __init__(self, slipnet, line):
        self.slipnet = slipnet
        strings = line['strings']
        self.initial_string = WString('i', strings['i'])
        self.modified_string = WString('m', strings['m'])
        self.target_string = WString('t', strings['t'])
        self.answer_string = None
        self.rule = None
        self.translated_rule = None
        self.temperature = line['temp']
        self.built_correspondences = []
        self.proposed_correspondences = []
        self.snag = False
        self._cache = {}
        self.strings = {'i': self.initial_string, 'm': self.modified_string,
                        't': self.target_string}

    def correspondences(self):
        return self.built_correspondences

    def get_proposed_correspondences(self):
        return self.proposed_correspondences

    # -- rebuilding from a line ---------------------------------------------------------

    def _cached(self, key, make):
        obj = self._cache.get(key)
        if obj is None:
            obj = make()
            self._cache[key] = obj
        return obj

    def _object(self, ref, groups_by_span):
        """Resolve an object reference [string, left, right, kind]."""
        skey, left, right, kind = ref
        string = self.strings.get(skey)
        if string is None:
            raise KeyError(ref)
        if kind == 'l':
            return string.letters[left]
        group = groups_by_span.get((skey, left, right))
        if group is None:
            raise KeyError(ref)
        return group

    def apply(self, line):
        self.temperature = line['temp']
        self.snag = line.get('snag', False)
        self.rule = Rule(line['rule']) if line['rule'] else None
        self.translated_rule = Rule(line['trule']) if line['trule'] else None
        if line['strings']['a'] and self.answer_string is None:
            self.answer_string = WString('a', line['strings']['a'])
            self.strings['a'] = self.answer_string
        for key, string in self.strings.items():          # the letters' flags move
            records = line['strings'].get(key)
            if records:
                for letter, r in zip(string.letters, records):
                    letter.is_changed = r['ch']
                    letter.descriptions = [tuple(d) for d in r['d']]
        for s in self.strings.values():
            s.bonds, s.proposed_bonds, s.groups, s.proposed_groups = [], [], [], []
        concept = self.slipnet.concept

        # groups first, built before proposed, so that members can be looked up
        groups_by_span = {}
        records = sorted(line['groups'], key=lambda g: (g['lvl'] < BUILT, g['r'] - g['l']))
        pending = []
        for g in records:
            key = ('g', g['s'], g['l'], g['r'], g['cat'], g['dir'])
            group = self._cached(key, lambda: Group(self.strings[g['s']], g['l'], g['r'],
                                                    concept(g['cat']), concept(g['dir'])))
            group.built = g['lvl'] >= BUILT
            pending.append((g, group))
            if g['lvl'] >= BUILT or (g['s'], g['l'], g['r']) not in groups_by_span:
                groups_by_span[(g['s'], g['l'], g['r'])] = group
        for g, group in pending:
            string = self.strings[g['s']]
            try:
                group.objects = [self._object(r, groups_by_span) for r in g['objs']]
            except KeyError:
                group.objects = []
            (string.groups if group.built else string.proposed_groups).append(group)

        for b in line['bonds']:
            string = self.strings.get(b['s'])
            if string is None:
                continue
            try:
                lo = self._object(b['lo'], groups_by_span)
                ro = self._object(b['ro'], groups_by_span)
            except KeyError:
                continue
            key = ('b', b['s'], tuple(b['lo']), tuple(b['ro']), b['cat'], b['dir'])
            bond = self._cached(key, lambda: Bond(string, lo, ro, concept(b['cat']),
                                                  concept(b['dir'])))
            bond.left_object, bond.right_object = lo, ro
            bond.built = b['lvl'] >= BUILT
            (string.bonds if bond.built else string.proposed_bonds).append(bond)

        self.built_correspondences, self.proposed_correspondences = [], []
        for c in line['corrs']:
            try:
                o1 = self._object(c['o1'], groups_by_span)
                o2 = self._object(c['o2'], groups_by_span)
            except KeyError:
                continue
            key = ('c', tuple(c['o1']), tuple(c['o2']))
            corr = self._cached(key, lambda: Correspondence(o1, o2))
            corr.object1, corr.object2 = o1, o2
            corr.concept_mappings = [ConceptMapping(concept(d1), concept(d2), dist)
                                     for d1, d2, dist in c['cms']]
            corr.built = c['lvl'] >= BUILT
            (self.built_correspondences if corr.built
             else self.proposed_correspondences).append(corr)


class Codelet(object):
    """A stand-in whose class name is what Hager's Coderack view reads."""


_codelet_classes = {}


def codelet_instance(mitchell_type):
    name = CODELET_CLASSES.get(mitchell_type, mitchell_type)
    cls = _codelet_classes.get(name)
    if cls is None:
        cls = type(name, (Codelet,), {'mitchell_type': mitchell_type})
        _codelet_classes[name] = cls
    return _instance_of(cls)


_instances = {}


def _instance_of(cls):
    inst = _instances.get(cls)
    if inst is None:
        inst = cls()
        _instances[cls] = inst
    return inst


class Coderack(object):
    def __init__(self):
        self.time = 0
        self.last_chosen = None
        self.counts = {}

    def apply(self, line):
        self.time = line['n']
        self.counts = line['rack']
        self.last_chosen = codelet_instance(line['codelet']) if line['codelet'] else None

    def codelets(self):
        out = []
        for mitchell_type, count in self.counts.items():
            out.extend([codelet_instance(mitchell_type)] * count)
        return out


class TraceRun(object):
    """A run rebuilt from trace lines. SOURCE yields parsed lines (dicts); the first must be
    the "init" line. step() applies the next line; at the end of the trace it does nothing."""

    def __init__(self, source):
        self.source = source
        first = next(source)
        if first.get('k') != 'init':
            raise ValueError("the trace does not start with an init line")
        self.problem = first['problem']
        self.seed = first.get('seed')
        self.timestep = first.get('timestep', 15)
        self.slipnet = Slipnet(first['nodes'])
        self.workspace = Workspace(self.slipnet, first)
        self.coderack = Coderack()
        self.lines = 0
        self.finished = False
        self.last = None
        self._apply(first)

    def _apply(self, line):
        self.last = line
        self.lines += 1
        self.slipnet.apply(line)
        self.workspace.apply(line)
        self.coderack.apply(line)
        if line.get('k') == 'final':
            self.finished = True

    def step(self):
        if self.finished:
            return False
        try:
            line = next(self.source)
        except StopIteration:
            self.finished = True
            return False
        if line is None:          # the live source had nothing yet
            return False
        self._apply(line)
        return True

    def run_to_end(self):
        while self.step():
            pass
        return self

    @property
    def answer(self):
        return self.workspace.answer_string.name if self.workspace.answer_string else None


def parse_lines(lines):
    """Parsed JSON objects from an iterable of text lines; blank lines are skipped."""
    for text in lines:
        text = text.strip()
        if text:
            yield json.loads(text)
