"""Port of SRule.pm (``SRule``): a rule, i.e. a transform (a Mapping) together with its
flipped version, that can be checked against a list of objects to give an SRuleApp.

A Class::Std class. ``SRule({...})`` is Class::Std's ``new``: both ``transform`` and
``flipped_transform`` must be present (no type checks). ``SRule.create(x)`` is the
``createRule`` multimethod: an SRelation gives the rule of its type; a Mapping gives a
rule memoized by transform identity (Perl: its address). The memo lives for the whole
process in Perl; ``reset()`` clears it for test isolation.

Naming: ``CreateApplication`` → ``create_application``, ``CheckApplicability`` →
``check_applicability``.

Hooks, looked up at call time (tests monkeypatch them):
``_find_object_set_direction(*objects)`` is SWorkspace::__FindObjectSetDirection. The
Perl reads the workspace's %LeftEdge_of; for live objects that equals
``get_left_edge()``, which this port uses. (``sworkspace.find_object_set_direction`` is
the faithful port; it requires live objects, which the srule oracle doesn't have.)

PERL-QUIRKs (oracle-confirmed):
- create dies with an empty message (bare ``confess``) when the flipped transform fails
  CheckSanity, e.g. an ascending structural mapping that changes only ``start``. It
  returns empty when the transform has no flipped version.
- CheckApplicability ends with SRuleApp->new, whose BUILD confesses unless the direction
  is RIGHT. So objects in leftward order die ("Expected direction to be right!") instead
  of giving a leftward rule app. With fewer than 2 objects it confesses "Need at least 2".
"""
from seqsee import util
from seqsee.errors import Confess
from seqsee.multimethods import Multimethod

_MEMO = {}   # id(transform) -> (transform, rule); the transform is kept alive so ids stay unique


def reset():
    """Clear createRule's memo (Perl: ``state %MEMO``, never cleared)."""
    _MEMO.clear()


def _find_object_set_direction(*objects):
    """Perl: SWorkspace::__FindObjectSetDirection(@objects): RIGHT/LEFT if the left edges
    strictly increase/decrease, UNKNOWN if two in a row are equal, NEITHER if mixed."""
    from seqsee.constants import DIR
    left_edges = [None if o is None else o.get_left_edge() for o in objects]
    if len(objects) <= 1:
        raise Confess("Need at least 2")
    leftward = rightward = 0
    for i in range(len(objects) - 1):
        diff = util.perl_num(left_edges[i + 1]) - util.perl_num(left_edges[i])
        if diff > 0:
            rightward += 1
        elif diff < 0:
            leftward += 1
        else:
            return DIR.UNKNOWN
    if leftward and rightward:
        return DIR.NEITHER
    if leftward:
        return DIR.LEFT
    if rightward:
        return DIR.RIGHT
    raise Confess("huh?")


class SRule:
    """Perl: SRule."""

    perl_name = "SRule"

    def __init__(self, args):
        missing = [k for k in ("transform", "flipped_transform") if k not in args]
        if missing:
            lines = [f"Missing initializer label for SRule: '{k}'." for k in missing]
            raise Confess("\n".join(lines + ["Fatal error in constructor call"]))
        self._transform = args["transform"]
        self._flipped_transform = args["flipped_transform"]

    @classmethod
    def create(cls, *args):
        """Perl: SRule->create(...), i.e. createRule(...)."""
        return CREATE_RULE.call(*args)

    def get_transform(self):
        return self._transform

    def set_transform(self, value):
        self._transform = value

    def get_flipped_transform(self):
        return self._flipped_transform

    def set_flipped_transform(self, value):
        self._flipped_transform = value

    def create_application(self, opts):
        """Perl: CreateApplication({start =>, direction =>}): a rule app over just start."""
        from seqsee.srule_app import SRuleApp
        start = opts.get("start")
        if not util.perl_true(start):
            raise Confess("need start")
        direction = opts.get("direction")
        if not util.perl_true(direction):
            raise Confess("need direction")
        return SRuleApp({"rule": self, "items": [start], "direction": direction})

    def check_applicability(self, opts):
        """Perl: CheckApplicability({objects => [...]}): an SRuleApp if each object is the
        transform applied to the one before (compared by structure string), else None."""
        from seqsee.mapping import apply_mapping
        from seqsee.srule_app import SRuleApp
        objects = opts.get("objects")
        if not util.perl_true(objects):
            raise Confess("need objects")
        to_account_for = list(objects)
        accounted_for = [to_account_for.pop(0) if to_account_for else None]
        transform = self._transform
        while to_account_for:
            last_accounted_for = accounted_for[-1].get_effective_object()
            expected_next = apply_mapping(transform, last_accounted_for)
            if not util.perl_true(expected_next):
                return None
            actual_next = to_account_for.pop(0)
            if (util.perl_str(expected_next.get_structure_string())
                    != util.perl_str(actual_next.get_effective_object().get_structure_string())):
                return None
            accounted_for.append(actual_next)
        direction = _find_object_set_direction(*accounted_for)
        if not direction.is_left_or_right():
            return None
        return SRuleApp({"rule": self, "items": accounted_for, "direction": direction})

    def as_text(self):
        from seqsee.mapping.numeric import _method_target
        return "Rule: " + _method_target(self._transform, "as_text").as_text()


CREATE_RULE = Multimethod("createRule")


@CREATE_RULE.variant("SRelation")
def _create_rule_from_relation(rel):
    return CREATE_RULE.call(rel.get_type())


@CREATE_RULE.variant("Mapping")
def _create_rule_from_mapping(transform):
    flipped_transform = transform.flipped_version()
    if not util.perl_true(flipped_transform):
        return None
    if not util.perl_true(flipped_transform.check_sanity()):
        raise Confess("")
    entry = _MEMO.get(id(transform))
    if entry is None:
        entry = _MEMO[id(transform)] = (transform, SRule({
            "transform": transform,
            "flipped_transform": flipped_transform,
        }))
    return entry[1]
