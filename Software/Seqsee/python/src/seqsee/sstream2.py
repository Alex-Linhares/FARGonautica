"""Port of lib/SStream2.pm: a stream of thoughts.

Seqsee uses one stream, ``Global.MainStream``. The last few thoughts (``max_older_thoughts``)
are the context: when a new thought's fringe shares components with the fringes of older
thoughts, an ActOnOverlappingThoughts codelet is scheduled. Then the thought's actions run
(SActions) or go to the coderack (at most two SCodelets, chosen by urgency).

Perl hash keys stringify. Here: thoughts are keyed by identity (no thought class overloads
``""``); fringe components are keyed by ``_key``: scalars by their Perl string (5 and "5"
share a key), SInt by its ``""`` overload ("SInt(3)"), other objects by identity.

PERL-QUIRKs (oracle-confirmed, ported):
- Revisiting an older thought calls ``_recalculate_Compstrength()`` as a plain function,
  without ``$self``, so it works on a throwaway hash. ComponentOwnership_of and vivify stay
  as they were. The revisited thought may still own components there, so it can hit itself
  (undampened): ActOnOverlappingThoughts with a == b.
- thoughtTypeMatch's table has "A:B" keys, but the lookup builds "A;B" (and the names are not
  thought classes anyway), so different thought classes always match 0.
- ``antiquate_current_thought`` with no current thought pushes '' onto OlderThoughts; the
  recalculation then dies on '' after clearing ComponentOwnership_of and vivify.
- A get_actions entry that is neither an SCodelet nor an SAction (e.g. the undef/0 that
  ExtendFromMemory returns) confesses "Huh? non-codelet '…'". SActions before it have run,
  and no codelet is added.
- ``clear`` leaves hit_intensity and thought_hit_intensity alone.

When several older thoughts are hit, Perl's SChoose->choose gets them in hash order; here
they come in first-hit order, so the picked thought can differ for a given seed.
"""
from seqsee import global_ as Global
from seqsee import schoose
from seqsee.errors import Confess
from seqsee.util import _is_scalar, perl_num, perl_ref, perl_ref_string, perl_str, perl_true

_MEMO = {}

# Perl: the %Mapping table of thoughtTypeMatch (never reached, see the module docstring).
_TYPE_MAPPING = {
    "Seqsee::Element:Seqsee::Anchored": 0.9,
    "Seqsee::Anchored:Seqsee::Element": 0.9,
}


def _key(x):
    """The Perl hash key of a fringe component."""
    from seqsee.sint import SInt
    if x is None or isinstance(x, bool) or _is_scalar(x):
        return perl_str(x)
    if isinstance(x, SInt):
        return x.as_text()
    return x


def _message(msg):
    """Perl: main::message (debugMAX output)."""
    from seqsee.scodelet_base import _message as message
    message(msg)


class SStream2:
    """A stream of thoughts (Perl ``SStream2``). Build it with ``SStream2.create_new``."""

    perl_name = "SStream2"

    @classmethod
    def create_new(cls, name, opts=None):
        """Perl ``CreateNew($name, {DiscountFactor, MaxOlderThoughts})``, memoized on the name only."""
        if not perl_true(name):
            raise Confess("Missing name!")
        if name in _MEMO:
            return _MEMO[name]
        self = cls.__new__(cls)
        _MEMO[name] = self
        opts = opts or {}
        self.name = name
        self.discount_factor = opts.get("DiscountFactor") if perl_true(opts.get("DiscountFactor")) else 0.8
        self.max_older_thoughts = opts.get("MaxOlderThoughts") if perl_true(opts.get("MaxOlderThoughts")) else 10
        self.older_thought_count = 0
        self.older_thoughts = []
        self.thoughts_set = {}
        self.component_strength = {}
        self.component_ownership_of = {}
        self.current_thought = ""
        self.vivify = {}
        self.hit_intensity = {}
        self.thought_hit_intensity = {}
        return self

    def clear(self):
        """Perl ``clear``. Leaves hit_intensity and thought_hit_intensity alone, as the Perl does."""
        self.older_thought_count = 0
        self.older_thoughts = []
        self.thoughts_set = {}
        self.component_strength = {}
        self.component_ownership_of = {}
        self.current_thought = ""
        self.vivify = {}

    def init(self):
        """Perl ``init``: an empty sub."""

    def add_thought(self, *args):
        """Perl: add_thought($thought).

        A thought with a false core is ignored; the current thought is ignored. An older
        thought becomes current again (and leaves OlderThoughts); a new one antiquates the
        current thought and may expel the oldest. Then the thought is thought about.
        """
        if len(args) != 1:
            raise Confess("new thought takes two arguments")
        thought = args[0]
        if not perl_true(thought.core()):
            return None
        if perl_true(Global.debugMAX):
            from seqsee.util import stringify_for_carp
            _message("Added thought: " + stringify_for_carp(thought))
        if perl_true(Global.Feature.get("CodeletTree")):
            Global.CodeletTreeLogHandle.write(f"Chose {perl_ref_string(thought)}\n")
        if thought is self.current_thought:
            return None
        if thought in self.thoughts_set:
            if perl_true(self.current_thought):
                self.older_thoughts.insert(0, self.current_thought)
            self.older_thoughts = [t for t in self.older_thoughts if t is not thought]
            self.current_thought = thought
            # PERL-QUIRK: _recalculate_Compstrength() is called without $self: no effect.
            self.older_thought_count = len(self.older_thoughts)
        else:
            if perl_true(self.current_thought):
                self.antiquate_current_thought()
            self.current_thought = thought
            self.thoughts_set[thought] = thought
            self._maybe_expell_thoughts()
        self._think_the_current_thought()
        return None

    def _think_the_current_thought(self):
        """Perl: _think_the_current_thought."""
        from seqsee import scoderack
        from seqsee.saction import SAction
        from seqsee.scodelet import SCodelet
        thought = self.current_thought
        if not perl_true(thought):
            return None
        Global.CurrentCodelet = thought
        Global.CurrentCodeletFamily = perl_ref(thought)

        fringe = thought.get_fringe()
        thought.stored_fringe(fringe)

        hit_with = self._is_there_a_hit(fringe)
        if perl_true(hit_with):
            SCodelet("ActOnOverlappingThoughts", 100, {"a": hit_with, "b": thought}).schedule()

        codelets = []
        for x in thought.get_actions():
            x_type = perl_ref(x)
            if x_type == "SCodelet":
                codelets.append(x)
            elif x_type == "SAction":
                if perl_true(Global.Feature.get("CodeletTree")):
                    Global.CodeletTreeLogHandle.write(
                        f"\t{perl_ref_string(x)}\t{perl_str(x.family)}\t{perl_str(x.urgency)}\n")
                x.conditionally_run()
            else:
                shown = perl_ref_string(x) if x_type else perl_str(x)
                raise Confess(f"Huh? non-codelet '{shown}' returned by get_actions")

        if len(codelets) > 2:
            choose2 = schoose.choose_a_few_nonzero(2, [c[1] for c in codelets], codelets)
        else:
            choose2 = codelets
        for c in choose2:
            if perl_true(Global.debugMAX):
                from seqsee.util import stringify_for_carp
                _message([c[0], ["codelet_family"],
                          " codelet added by thought: " + stringify_for_carp(c)])
            scoderack.add_codelet(c)
        return None

    def _maybe_expell_thoughts(self):
        """Perl: _maybe_expell_thoughts. Drops the oldest thoughts past max_older_thoughts."""
        if not self.older_thought_count > perl_num(self.max_older_thoughts):
            return None
        for _ in range(int(self.older_thought_count - perl_num(self.max_older_thoughts))):
            gone = self.older_thoughts.pop() if self.older_thoughts else None
            if gone is not None and not _is_scalar(gone):
                self.thoughts_set.pop(gone, None)
        self.older_thought_count = self.max_older_thoughts
        self._recalculate_compstrength()
        return None

    def _recalculate_compstrength(self):
        """Perl: _recalculate_Compstrength. Rebuilds ComponentOwnership_of and vivify."""
        ownership = self.component_ownership_of
        vivify = self.vivify
        ownership.clear()
        vivify.clear()
        for t in self.older_thoughts:
            if _is_scalar(t):
                # Only '' gets here (antiquating with no current thought).
                raise Confess('Can\'t call method "stored_fringe" without a package or object '
                              'reference')
            for comp, act in t.stored_fringe():
                k = _key(comp)
                vivify[k] = comp
                ownership.setdefault(k, {})[t] = act
        return None

    def antiquate_current_thought(self):
        """Perl: antiquate_current_thought. The current thought becomes the newest older one."""
        self.older_thoughts.insert(0, self.current_thought)
        self.current_thought = ""
        self.older_thought_count += 1
        self._recalculate_compstrength()
        return None

    def _is_there_a_hit(self, fringe):
        """Perl: _is_there_a_hit. Picks an older thought sharing fringe components, or None.

        Each hit thought scores sum(its activation × the new intensity) over shared
        components, discounted by DiscountFactor per step back and by thoughtTypeMatch.
        """
        components_hit = {}
        hit_intensity = self.hit_intensity
        hit_intensity.clear()
        ownership = self.component_ownership_of
        for comp, intensity in fringe:
            k = _key(comp)
            if k not in ownership:
                continue
            components_hit[k] = comp
            hit_intensity[k] = intensity

        thought_hit_intensity = self.thought_hit_intensity
        thought_hit_intensity.clear()
        for k in components_hit:
            if k not in ownership:
                continue
            owner = ownership[k]
            intensity = hit_intensity[k]
            for tht, act in owner.items():
                thought_hit_intensity[tht] = (thought_hit_intensity.get(tht, 0)
                                              + perl_num(act) * perl_num(intensity))

        if not thought_hit_intensity:
            return None

        dampen_by = 1
        for i in range(int(self.older_thought_count)):
            dampen_by *= self.discount_factor
            thought = self.older_thoughts[i] if i < len(self.older_thoughts) else None
            if thought is None or _is_scalar(thought) or thought not in thought_hit_intensity:
                continue
            thought_hit_intensity[thought] *= dampen_by
            thought_hit_intensity[thought] *= self.thought_type_match(thought, self.current_thought)

        chosen = schoose.choose(list(thought_hit_intensity.values()),
                                list(thought_hit_intensity.keys()))
        return self.thoughts_set.get(chosen)

    def thought_type_match(self, othertht, cur_tht):
        """Perl: thoughtTypeMatch. 1 for the same thought class, else 0 (see PERL-QUIRKs)."""
        type1, type2 = perl_ref(othertht), perl_ref(cur_tht)
        if type1 == type2:
            return 1
        key = f"{type1};{type2}"
        if key in _TYPE_MAPPING:
            return _TYPE_MAPPING[key]
        return 0


def reset():
    """Forget all memoized streams (for tests; Perl reloads the process)."""
    _MEMO.clear()
