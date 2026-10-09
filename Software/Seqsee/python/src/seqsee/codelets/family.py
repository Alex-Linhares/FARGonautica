"""Port of lib/MooseX/SCF.pm: ``Codelet_Family`` (the family registry) and ``ACTION``.

In Perl each family is a package ``Seqsee::SCF::<Name>`` whose ``Codelet_Family(attributes
=> [...], body => sub {...})`` installs a ``run`` sub. Here families live in ``FAMILIES``
(name → ``CodeletFamily``) and are defined with a decorator::

    @codelet_family("AttemptExtensionOfGroup", attributes=("object", "direction"))
    def attempt_extension_of_group(object, direction): ...

``attributes`` is a list of names (each with the empty spec ``{}``, i.e. mandatory) or of
``(name, spec)`` pairs, the Params::Validate specs used in Seqsee/SCF_MX: ``{}``,
``{"required": 1}``, ``{"optional": 1}`` and ``{"default": value}``. The body gets the
arguments as positional values in attribute order (MooseX::Params::Validate's
``validated_list``).
"""
from seqsee import util
from seqsee.errors import Confess

FAMILIES = {}


def hash_deref(x):
    """Perl ``%{ $x }`` under strict refs: the dict, or Perl's error."""
    if isinstance(x, dict):
        return x
    if x is None:
        raise Confess("Can't use an undefined value as a HASH reference")
    if util._is_scalar(x) or isinstance(x, bool):
        raise Confess(f'Can\'t use string ("{util.perl_str(x)}") as a HASH ref '
                      'while "strict refs" in use')
    raise Confess("Not a HASH reference")


def _is_optional(spec):
    """Params::Validate: a spec is optional if it has a true ``optional`` or a ``default``.

    PERL-QUIRK: General.pm's typo ``{ defualt => "" }`` is therefore mandatory.
    """
    return util.perl_true(spec.get("optional")) or "default" in spec


class CodeletFamily:
    """One ``Seqsee::SCF::<name>`` package: its attributes and body."""

    def __init__(self, name, attributes, body):
        self.name = name
        self.attributes = attributes
        self.body = body
        self.called = f"Seqsee::SCF::{name}::run"

    def validated_list(self, args):
        """MooseX::Params::Validate ``validated_list([%args], @attributes)``.

        Unknown parameters are reported first (the XS validator names only the first one it
        finds, in hash order), then missing mandatory ones (sorted). Absent optional
        parameters are undef unless they have a default; a given undef stays undef.
        """
        specs = dict(self.attributes)
        unknown = [k for k in args if k not in specs]
        if unknown:
            raise Confess(f"The following parameter was passed in the call to {self.called} "
                          f"but was not listed in the validation options: {unknown[0]}")
        missing = sorted(name for name, spec in self.attributes
                         if name not in args and not _is_optional(spec))
        if missing:
            listed = ", ".join(f"'{m}'" for m in missing)
            plural = "s" if len(missing) > 1 else ""
            raise Confess(f"Mandatory parameter{plural} {listed} missing in call to {self.called}")
        values = []
        for name, spec in self.attributes:
            if name in args:
                values.append(args[name])
            else:
                values.append(spec.get("default"))
        return values

    def run(self, action_object, args):
        """The installed ``run`` sub: validate the args hash, then call the body."""
        return self.body(*self.validated_list(hash_deref(args)))


def _normalize_attributes(attributes):
    out = []
    for a in attributes:
        if isinstance(a, str):
            out.append((a, {}))
        else:
            name, spec = a
            out.append((name, dict(spec)))
    return out


def define_codelet_family(name, attributes=None, body=None):
    """Perl: ``Codelet_Family(attributes => [...], body => sub {...})`` in package
    ``Seqsee::SCF::<name>``. A family defined again replaces the earlier one."""
    if attributes is None:
        raise Confess("Require attributes")
    if body is None:
        raise Confess("Require body")
    FAMILIES[name] = CodeletFamily(name, _normalize_attributes(attributes), body)


def codelet_family(name, attributes):
    """Decorator form of ``define_codelet_family``; returns the body unchanged."""
    def register(body):
        define_codelet_family(name, attributes=attributes, body=body)
        return body
    return register


# The modules defining families (Perl: Seqsee/SCF_MX/*.pm, loaded by ``use S``).
FAMILY_MODULES = ("seqsee.codelets.general", "seqsee.codelets.all_mx",
                  "seqsee.codelets.all_mx2", "seqsee.codelets.large_gp",
                  "seqsee.codelets.ui", "seqsee.scripts.describe_solution")


def load_families():
    """Import every family module, so their families are registered."""
    import importlib
    for name in FAMILY_MODULES:
        importlib.import_module(name)


def family_run(family, action_object, args):
    """Perl: ``"Seqsee::SCF::$family::run"->($action_object, $args)``."""
    fam = FAMILIES.get(util.perl_str(family))
    if fam is None:
        load_families()
        fam = FAMILIES.get(util.perl_str(family))
    if fam is None:
        raise Confess(f"Undefined subroutine &Seqsee::SCF::{util.perl_str(family)}::run called")
    return fam.run(action_object, args)


def action(urgency, family, options):
    """Perl: ACTION($urgency, $family, $options). Runs the family now with probability
    urgency/100 (one draw)."""
    from seqsee.saction import SAction
    return SAction({"family": family, "urgency": urgency,
                    "arguments": options}).conditionally_run()
