"""Port of lib/Seqsee.pm: the main loop, configuration and command line.

Perl names: ``run`` → ``run``, ``do_background_activity`` → ``do_background_activity``,
``Seqsee_Step`` → ``seqsee_step``, ``Interaction_step_n`` → ``interaction_step_n``,
``_read_commandline`` → ``read_commandline``, ``_read_config`` → ``read_config``,
``%DEFAULTS`` → ``DEFAULTS``. ``already_rejected_by_user`` lives in
seqsee.user_interaction (item 044).

config/seqsee.conf is read by ``read_config``; config/start_codelets.conf by
``scoderack.init`` (item 036). Both use scoderack's minimal Config::Std reader. Perl reads
them relative to the current directory; the port reads the repo's config/ directory.

Hooks (module attributes, looked up at call time): ``_update_display`` (main::update_display,
a no-op headless), ``_sanity_check`` (SanityCheck()) and ``_sleep`` (Time::HiRes::sleep).

The main loop never clears ``scripts._cached_spec`` (MooseX::Params::Validate's per-process
spec cache, see item 045); only ``scripts.reset()`` does.

PERL-QUIRKs (oracle-confirmed):
- ``run`` is dead code. It passes the sequence itself to SWorkspace->init, which wants an
  options hash, so ``run(1, 2, 3)`` dies in init. Given an options hash it gets through the
  inits, then calls ``_SeqseeMainLoop``, which is defined nowhere.
- ``Seqsee_Step`` always returns undef, so ``Interaction_step_n`` never sees the program
  finish. A run ends only at max_steps, on Break_Loop or by an exception.
- ``--sanity``/``--nosanity`` are parsed but ignored: ``_read_config`` never copies them and
  ``$Global::Sanity`` is untouched.
- The default seed is drawn at load time and is never passed to srand.
"""
import re
import sys
import time
from pathlib import Path

from seqsee import global_ as Global
from seqsee import scoderack, sltm, sworkspace, util
from seqsee.errors import Confess, Fatal
from seqsee.scodelet import SCodelet
from seqsee.util import perl_num, perl_str, perl_true

_SEQSEE_CONF = Path(__file__).resolve().parents[3] / "config" / "seqsee.conf"

# Perl: my %DEFAULTS, used if not set in the config file or on the command line. Perl draws
# the seed at load time, before any srand; a private generator keeps the central stream intact.
DEFAULTS = {
    "seed": int(util.Drand48().rand() * 32000),
    "update_interval": 0,  # If default used, carps when interactive
}

# Perl: the closure lexical of do_background_activity.
_TimeLastProgressCheckerLaunched = 0


def reset():
    """Restore the module state of a fresh process (the do_background_activity closure)."""
    global _TimeLastProgressCheckerLaunched
    _TimeLastProgressCheckerLaunched = 0


def _update_display():
    """Perl: main::update_display (GUI). Headless: nothing."""


def _sanity_check():
    """Perl: SanityCheck() (Sanity.pm, all groups and relations)."""
    from seqsee import sanity
    return sanity.SANITY_CHECK.call()


def _sleep(seconds):
    """Perl: Time::HiRes::sleep."""
    time.sleep(seconds)


def _workspace_init(options):
    """SWorkspace->init($options) with Perl's errors for a non-hash or seq-less argument."""
    if options is None:
        raise Confess("Can't use an undefined value as an ARRAY reference")
    if not isinstance(options, dict):
        raise Confess(f'Can\'t use string ("{perl_str(options)}") as a HASH ref '
                      'while "strict refs" in use')
    if options.get("seq") is None:
        raise Confess("Can't use an undefined value as an ARRAY reference")
    sworkspace.init(options)


def run(*sequence):
    """Perl: run(@sequence). See the PERL-QUIRK above: it always dies."""
    from seqsee import s
    s.load()
    sworkspace.clear()
    _workspace_init(sequence[0] if sequence else None)
    Global.MainStream.clear()
    Global.MainStream.init()
    scoderack.clear()
    scoderack.init()
    sltm.init()
    raise Confess("Undefined subroutine &Seqsee::_SeqseeMainLoop called")


def do_background_activity():
    """Perl: do_background_activity. Maybe a FocusOn (0.3), maybe a CheckProgress (if the
    last one is over 20 steps old, with probability steps-since-new-structure / 150), and
    every 10 steps LTM decay plus a strength update."""
    global _TimeLastProgressCheckerLaunched
    if perl_true(Global.Feature.get("CodeletTree")):
        Global.CodeletTreeLogHandle.write("Background\n")

    if util.toss(0.3):
        scoderack.add_codelet(SCodelet("FocusOn", 50, {}))

    time_since_last_addn = Global.Steps_Finished - Global.TimeOfNewStructure
    time_since_last_checker = Global.Steps_Finished - _TimeLastProgressCheckerLaunched

    if time_since_last_checker > 20 and util.toss(time_since_last_addn / 150):
        _TimeLastProgressCheckerLaunched = Global.Steps_Finished
        SCodelet("CheckProgress", 100, {}).schedule()

    if Global.Steps_Finished % 10 == 0:
        sltm.decay_all()
        sworkspace.update_object_strengths()


def seqsee_step():
    """Perl: Seqsee_Step. Runs one codelet. Always returns None (Perl: undef)."""
    Global.Steps_Finished += 1
    if perl_true(Global.Feature.get("LogActivations")) and not Global.Steps_Finished % 10:
        sltm.log_activations()
    if not Global.Steps_Finished % 100:
        Global.AcceptableTrustLevel -= 0.002
        if not Global.Steps_Finished % 1000:
            print(f"@{Global.Steps_Finished}")
    if perl_true(Global.InterstepSleep):
        _sleep(perl_num(Global.InterstepSleep) / 1000)

    do_background_activity()

    runnable = scoderack.get_next_runnable()
    if not runnable:
        return None  # prog not yet finished!

    if isinstance(runnable, SCodelet):
        if perl_true(Global.Feature.get("CodeletTree")):
            Global.CodeletTreeLogHandle.write(f"Chose {util.perl_ref_string(runnable)}\n")
        Global.CurrentRunnableString = "Seqsee::SCF::" + perl_str(runnable[0])
        runnable.run()
    else:
        raise Fatal(f"Runnable object is {perl_str(runnable)}: expected a SCodelet")
    if perl_true(Global.Sanity):
        _sanity_check()
    return None


def interaction_step_n(opts):
    """Perl: Interaction_step_n($opts_ref). Takes up to ``n`` steps (never past
    ``max_steps``), updating the display every ``update_after`` steps.

    Returns 1 if there is nothing left to do, else the last Seqsee_Step result (None).
    Dies "Need n" if n is false.

    The steps run through ``util.call_with_deep_stack``: Perl has no recursion limit, and
    some codelets recurse hundreds of levels deep (FindMapping on sameness, item 050b).
    """
    return util.call_with_deep_stack(_interaction_step_n, opts)


def _interaction_step_n(opts):
    steps_left_to_take = opts.get("n")
    if not perl_true(steps_left_to_take):
        raise Confess("Need n")
    steps_left_to_take = min(perl_num(steps_left_to_take),
                             perl_num(opts.get("max_steps")) - Global.Steps_Finished)
    if not steps_left_to_take > 0:
        return 1  # i.e, okay to stop now!

    update_after = opts.get("update_after")
    update_after = int(perl_num(update_after)) if perl_true(update_after) else int(
        steps_left_to_take)

    change_after_last_display = 0  # to prevent repeats at end
    program_finished = 0

    for steps_executed in range(1, int(steps_left_to_take) + 1):
        Global.Break_Loop = 0
        program_finished = seqsee_step()
        change_after_last_display = 1

        if not steps_executed % update_after:
            _update_display()
            change_after_last_display = 0
        if perl_true(program_finished):
            break
        if perl_true(Global.Break_Loop):
            break

    if change_after_last_display:
        _update_display()
    return program_finished


# ---- command line (Getopt::Long) --------------------------------------------------------------
# Seqsee.pm's GetOptions spec: name → type ("i" integer, "s" string, "!" negatable flag).
_GETOPT_SPEC = (("seed", "i"), ("seq", "s"), ("update_interval", "i"), ("max_steps", "i"),
                ("n", "i"), ("f", "s"), ("gui_config", "s"), ("gui", "s"), ("sanity", "!"),
                ("view", "i"))
_INT = re.compile(r"[-+]?[0-9]+\Z")


def _getopt_table():
    """Every accepted spelling → (canonical name, type, negated), as Getopt::Long builds it."""
    table = {}
    for name, type_ in _GETOPT_SPEC:
        table[name] = (name, type_, False)
        if type_ == "!":
            table["no" + name] = (name, type_, True)
            table["no-" + name] = (name, type_, True)
    return table


def _feature_callback(feature_name):
    """Seqsee.pm's ``f`` handler: turn a feature on, or exit on a typo."""
    print(f"{feature_name} will be turned on")
    if not perl_true(Global.PossibleFeatures.get(feature_name)):
        print(f"No feature {feature_name}. Typo?")
        sys.exit()
    Global.Feature[feature_name] = 1


def _get_options(argv):
    """The subset of Getopt::Long's GetOptions (default configuration) Seqsee.pm needs.

    Options start with ``--``, ``-`` or ``+``; ``name=value`` works with any of them. Names
    are case-insensitive and may be abbreviated uniquely. Non-options are kept in order (permute)
    and ``--`` ends option processing. Problems are warned on stderr and the option skipped.
    ``argv`` is left holding the non-options.
    """
    table = _getopt_table()
    options = {}
    remaining = []
    pending = list(argv)
    while pending:
        arg = pending.pop(0)
        if arg == "--":
            remaining.extend(pending)
            pending = []
            break
        m = re.match(r"(--|-|\+)(.+)\Z", arg, re.S)
        if not m:
            remaining.append(arg)
            continue
        opt, optarg = m.group(2), None
        pos = opt.find("=", 1)
        if pos > 0:
            opt, optarg = opt[:pos], opt[pos + 1:]
        tryopt = opt.lower()
        if tryopt not in table:
            hits = [k for k in table if k.startswith(tryopt)]
            kinds = sorted({("no" if table[k][2] else "") + table[k][0] for k in hits})
            if len(kinds) > 1:
                sys.stderr.write(f"Option {opt} is ambiguous ({', '.join(kinds)})\n")
                continue
            if not hits:
                sys.stderr.write(f"Unknown option: {opt}\n")
                continue
            tryopt = hits[0]
        name, type_, negated = table[tryopt]
        if type_ == "!":
            if optarg is not None:
                sys.stderr.write(f"Option {tryopt} does not take an argument\n")
                continue
            options[name] = 0 if negated else 1
            continue
        if (optarg == "") if optarg is not None else not pending:
            sys.stderr.write(f"Option {tryopt} requires an argument\n")
            continue
        value = optarg if optarg is not None else pending.pop(0)
        if type_ == "i":
            if not _INT.match(value):
                sys.stderr.write(f'Value "{value}" invalid for option {tryopt} '
                                 "(number expected)\n")
                continue
            value = int(value)
        if name == "f":
            _feature_callback(value)
        else:
            options[name] = value
    argv[:] = remaining + pending
    return options


def read_commandline(argv=None):
    """Perl: _read_commandline. Parses ``argv`` (default: ``sys.argv[1:]``, i.e. @ARGV, which
    is left holding the non-options) and returns the options dict.

    ``-f FEATURE`` turns ``Global.Feature[FEATURE]`` on (and exits on an unknown feature);
    Perl's hash also holds the ``f`` callback itself, which the port leaves out. ``n`` fills a
    false ``max_steps`` and ``gui`` a false ``gui_config``.
    """
    if argv is None:
        args = sys.argv[1:]
        options = _get_options(args)
        sys.argv[1:] = args
    else:
        options = _get_options(argv)
    if "n" in options and not perl_true(options.get("max_steps")):
        options["max_steps"] = options["n"]
    if "gui" in options and not perl_true(options.get("gui_config")):
        options["gui_config"] = options["gui"]
    if "debugMAX" in Global.Feature:
        Global.debugMAX = 1
    return options


# ---- config ------------------------------------------------------------------------------------
_CONFIG_KEYS = ("seed", "max_steps", "update_interval", "UseScheduledThoughtProb",
                "ScheduledThoughtVanishProb", "DecayRate", "view", "gui_config")


def read_config(**options):
    """Perl: _read_config(%options). Each setting comes from ``options``, else the
    ``[seqsee]`` section of config/seqsee.conf, else ``DEFAULTS``. ``seq`` must be a space or
    comma separated list of integers; it becomes a list of strings. Prints "View: …!".
    """
    config = scoderack._read_config(_SEQSEE_CONF)
    section = config.get("seqsee", {})
    result = {}
    for key in _CONFIG_KEYS:
        if key in options:
            val = options[key]
        elif key in section:
            val = section[key]
        elif key in DEFAULTS:
            val = DEFAULTS[key]
        else:
            raise Confess(f"Option '{key}' not set either on command line, conf file or defauls")
        result[key] = val

    seq = options.get("seq")
    text = "" if seq is None else perl_str(seq)
    if not re.match(r"[\d\s,]*\Z", text, re.ASCII):
        raise Confess("The option --seq must be a space or comma separated list of integers; "
                      f"I got '{text}' instead")
    text = re.sub(r"\s*\Z", "", re.sub(r"\A\s*", "", text, flags=re.ASCII), flags=re.ASCII)
    fields = re.split(r"[\s,]+", text, flags=re.ASCII)
    while fields and fields[-1] == "":
        fields.pop()  # Perl's split drops trailing empty fields
    result["seq"] = fields

    print(f"View: {perl_str(result['view'])}!")
    return result
