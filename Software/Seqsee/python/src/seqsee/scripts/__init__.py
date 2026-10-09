"""Port of lib/Seqsee/Scripts.pm: scripts, i.e. codelet families whose body is a list of
steps run in order, which can call other scripts and return to their caller.

A script is a ``Script`` (a ``CodeletFamily`` registered in ``family.FAMILIES`` under its
Perl name, package ``Seqsee::SCF::<Name>``) with ``steps`` (the ``step`` class attribute)
and ``attributes`` (the ``attributes`` class attribute: the MooseX::Params::Validate spec).
``run`` is Seqsee::Scripts::run. A step can:

- raise ``ScriptReturn`` (``RETURN()``): stop. If the stack has a caller frame, its script
  is scheduled (urgency 10000) to resume at the saved step;
- raise ``CallSubscript`` (``SCRIPT(name, args)``): push a frame ``[next step, args,
  family]`` and schedule the subscript (urgency 10000), then stop.

A script codelet's arguments are either the script's own (start at step 0, empty stack)
or ``{__S_T_E_P__, __A_R_G_S__, __S_T_A_C_K__}``.

PERL-QUIRK (oracle-confirmed): MooseX::Params::Validate caches a spec per calling sub
(``refaddr(caller_cv(2))``). Every script validates from the same place
(Seqsee::Scripts::run), so the first script validated in a process fixes the spec, and the
order of its positional arguments, for all later scripts. That is ``_cached_spec``.
``reset()``/``clear_spec_cache()`` forget it. In a real run DescribeSolution goes first and
its spec (``group``) suits DescribeInitialBlemish and DescribeBlocks too. A script with
other argument names (DescribeRule, ...) dies "...not listed in the validation options".
"""
import logging

from seqsee import util
from seqsee.codelets.family import FAMILIES, CodeletFamily, hash_deref, _normalize_attributes
from seqsee.errors import CallSubscript, Confess, ScriptReturn

_log = logging.getLogger(__name__)

# PERL-QUIRK: MooseX::Params::Validate's cached spec for Seqsee::Scripts::run.
_cached_spec = None


def clear_spec_cache():
    """Forget the cached validation spec (Perl: empty MooseX::Params::Validate's
    %CACHED_SPECS)."""
    global _cached_spec
    _cached_spec = None


def reset():
    """Test-isolation hook."""
    clear_spec_cache()


def RETURN():
    """Perl: Seqsee::Scripts::RETURN. Throws SErr::ScriptReturn."""
    _log.debug("RETURN")
    raise ScriptReturn()


def SCRIPT(name, arguments):
    """Perl: Seqsee::Scripts::SCRIPT($name, $arguments). Throws SErr::CallSubscript."""
    _log.debug("SCRIPT(%s)", name)
    raise CallSubscript(name=name, arguments=arguments)


class Script(CodeletFamily):
    """One script package (Perl: ``Seqsee::SCF::<Name>``, extends Seqsee::Scripts)."""

    def __init__(self, name, attributes, steps):
        super().__init__(name, _normalize_attributes(attributes), body=None)
        self.steps = list(steps)
        self.called = "Seqsee::Scripts::run"

    def expected_attributes(self):
        """Perl: expected_attributes, the flattened ``attributes`` list."""
        return [x for name, spec in self.attributes for x in (name, spec)]

    def number_of_steps(self):
        return len(self.steps)

    def get_step(self, i):
        return self.steps[i]

    def run(self, action_object, args):
        return run(action_object, args)


def define_script(name, attributes, steps):
    """Register a script (Perl: a package extending Seqsee::Scripts with ``+step`` and
    ``+attributes`` defaults). A script defined again replaces the earlier one."""
    FAMILIES[name] = Script(name, attributes, steps)


def _validated_list(package, opts_ref):
    """Perl: ``validated_list([%$opts_ref], $package->expected_attributes())``, with the
    call site's spec cached on first use (PERL-QUIRK, see the module docstring)."""
    global _cached_spec
    if _cached_spec is None:
        _cached_spec = list(package.attributes)
    validator = CodeletFamily(package.name, _cached_spec, None)
    validator.called = "Seqsee::Scripts::run"
    return validator.validated_list(hash_deref(opts_ref))


def run(action_object, args_ref):
    """Perl: Seqsee::Scripts::run($action_object, $args_ref)."""
    from seqsee.scodelet import SCodelet
    if args_ref is None:
        args_ref = {}  # `exists $args_ref->{...}` autovivifies
    args_ref = hash_deref(args_ref)
    if "__S_T_A_C_K__" in args_ref:
        stack = args_ref.get("__S_T_A_C_K__")
        step_ = args_ref.get("__S_T_E_P__")
        opts_ref = args_ref.get("__A_R_G_S__")
    else:
        stack, step_, opts_ref = [], 0, args_ref

    if action_object is None:
        raise Confess('Can\'t call method "family" on an undefined value')
    family = util.perl_str(action_object.family)
    package = FAMILIES.get(family)
    if not isinstance(package, Script):
        raise Confess(f'Can\'t locate object method "expected_attributes" via package '
                      f'"Seqsee::SCF::{family}"')

    arguments = _validated_list(package, opts_ref)

    step_ = int(util.perl_num(step_))
    while step_ < package.number_of_steps():
        step = package.get_step(step_)
        _log.debug("Step#: %s", step_)
        try:
            step(*arguments)
        except ScriptReturn:
            new_stack = list(stack)
            if not new_stack:
                return None
            step_no, args, name = new_stack.pop()
            SCodelet(name, 10000, {
                "__S_T_E_P__": step_no,
                "__A_R_G_S__": args,
                "__S_T_A_C_K__": new_stack,
            }).schedule()
            return None
        except CallSubscript as e:
            new_stack = [*stack, [step_ + 1, opts_ref, action_object.family]]
            _log.debug("SUBSCRIPT: %s", e.name)
            SCodelet(e.name, 10000, {
                "__S_T_E_P__": 0,
                "__A_R_G_S__": e.arguments,
                "__S_T_A_C_K__": new_stack,
            }).schedule()
            return None
        step_ += 1
    return None
