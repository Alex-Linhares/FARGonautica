"""Port of lib/Global.pm: run-wide global state (``$Global::…``).

Use as ``from seqsee import global_ as Global`` and read attributes at call time
(``Global.Steps_Finished``) so ``reset()`` and test assignments are seen.
Variable names keep their Perl spelling. Perl subs become snake_case functions:
``ClearHilit`` → ``clear_hilit``, ``Hilit`` → ``hilit`` (the hash stays ``Hilit``),
``SetRuleAppAsBest`` → ``set_rule_app_as_best``, etc.

``clear()`` is Perl's between-runs reset. ``reset()`` is the test-isolation hook that
restores every load-time value (conftest calls it around each test).
Hashes keyed by workspace objects (``Hilit``, ``GroupStrengthByConsistency``) are dicts
keyed by the object itself (Perl: its stringified ref).
"""
import re

from seqsee import sstream2
from seqsee.util import perl_str

Steps_Finished = 0                 # Number of codelets/thoughts run.
Break_Loop = None                  # Boolean: break out of main loop after this iteration?
CurrentCodelet = None              # Current codelet.
CurrentCodeletFamily = None        # Family of current codelet.
CurrentRunnableString = ""         # Perl: undef in Global.pm, then SHistory.pm sets it to ''.
AtLeastOneUserVerification = None  # Bool: has the user ever said 'yes' to 'is this next?'
TestingOptionsRef = None           # Global options when in testing mode.
TestingMode = None                 # Bool: are we running in testing mode?
ExtensionRejectedByUser = {}       # Rejected continuations ("1, 2, 3" → 1).
LogString = ""                     # Generated log string.
PossibleFeatures = {}              # Possible features, to catch typos in -f option.
Feature = {}                       # Features turned on from the commandline with -f.
Options_ref = None                 # Global options: defaults, configs and commandline.
RealSequence = []                  # The real sequence; Seqsee may not know all of it in test mode.
InitialTermCount = None            # Number of terms at start.
TimeOfLastNewElement = 0           # When was the last element added?
TimeOfNewStructure = 0             # When was the last group created or element added?

InterstepSleep = 0                 # In milliseconds.
Sanity = 1                         # Do sanity check after each step?
Hilit = {}                         # Objects to highlight; values 1 or 2.

BestRule = None                    # Best rule seen so far.
BestRuleApp = None
RecentPromisingRule = None         # A recently seen rule that might be right.
RecentPromisingRuleApp = None
GroupStrengthByConsistency = {}    # Strength conferred on groups by consistency with rules.

AcceptableTrustLevel = 0.5         # Trust level above which questions can be asked (adjusted at runtime).

debugMAX = None                    # The highest debug setting.

CodeletTreeLogfile = "codelet_tree.log"
CodeletTreeLogHandle = None
ActivationsLogfile = "activations.log"
ActivationsLogHandle = None

MainStream = None                  # SStream2 'MainStream', created by reset() below.

_POSSIBLE_FEATURES = ("debug", "LTM", "CodeletTree", "NoGpOverlap", "LogActivations", "AllowSquinting",
                      "LTM_expt", "Primes", "Parity", "Alternating", "debugMAX", "NoInterlaced")


def clear():
    """Perl ``clear``: reset per-run state.

    PERL-QUIRK: BestRuleApp/RecentPromisingRuleApp are not cleared, so the next
    ``update_group_strength_by_consistency()`` brings the old strengths back.
    """
    global Steps_Finished, AtLeastOneUserVerification, LogString, BestRule, RecentPromisingRule
    Steps_Finished = 0
    AtLeastOneUserVerification = 0
    ExtensionRejectedByUser.clear()
    LogString = ""
    Hilit.clear()
    BestRule = None
    RecentPromisingRule = None
    GroupStrengthByConsistency.clear()


def clear_hilit():
    """Perl ``ClearHilit``."""
    Hilit.clear()


def hilit(value, *objects):
    """Perl ``Hilit($value, @objects)``: mark each object with value (1 or 2)."""
    for obj in objects:
        Hilit[obj] = value


def set_rule_app_as_best(ruleapp):
    """Perl ``SetRuleAppAsBest``."""
    global BestRuleApp, BestRule
    BestRuleApp = ruleapp
    BestRule = ruleapp.get_rule()
    update_group_strength_by_consistency()


def set_rule_app_as_recent(ruleapp):
    """Perl ``SetRuleAppAsRecent``."""
    global RecentPromisingRuleApp, RecentPromisingRule
    RecentPromisingRuleApp = ruleapp
    RecentPromisingRule = ruleapp.get_rule()
    update_group_strength_by_consistency()


def update_group_strength_by_consistency():
    """Perl ``UpdateGroupStrengthByConsistency``: +40 per item of each rule app (repeats add up)."""
    GroupStrengthByConsistency.clear()
    for ruleapp in (BestRuleApp, RecentPromisingRuleApp):
        if ruleapp:
            for item in ruleapp.get_items():
                GroupStrengthByConsistency[item] = GroupStrengthByConsistency.get(item, 0) + 40


def set_future_terms(*terms):
    """Perl ``Global->SetFutureTerms(@terms)`` (the package argument is dropped)."""
    RealSequence.extend(terms)


def update_extensions_rejected_by_user(*new_magnitudes):
    """Perl ``UpdateExtensionsRejectedByUser(@new_magnitudes)``.

    Keeps the rejected continuations that start with the new terms, minus those terms.
    PERL-QUIRK (oracle-confirmed): the prefix is interpolated into the regex unescaped and
    without a boundary. A key that matches ``^prefix`` but not ``^prefix, `` is kept whole
    (prefix "1, 2" keeps "1, 23, 4"; prefix "1" keeps "10, 11"), and "." in a magnitude
    matches any character. An empty prefix keeps every non-empty key, stripping a leading ", ".
    """
    prefix = ", ".join(perl_str(m) for m in new_magnitudes)
    new_keys = []
    for key in ExtensionRejectedByUser:
        if not re.match(prefix, key):
            continue
        if key == prefix:
            continue
        new_keys.append(re.sub("^" + prefix + ", ", "", key, count=1))
    ExtensionRejectedByUser.clear()
    ExtensionRejectedByUser.update((k, 1) for k in new_keys)


def reset():
    """Restore every load-time value (for tests; Perl reloads the process).

    Containers are cleared in place. The SStream2 memo is forgotten and a fresh
    MainStream is created.
    """
    global Steps_Finished, Break_Loop, CurrentCodelet, CurrentCodeletFamily, CurrentRunnableString
    global AtLeastOneUserVerification, TestingOptionsRef, TestingMode, LogString, Options_ref
    global InitialTermCount, TimeOfLastNewElement, TimeOfNewStructure, InterstepSleep, Sanity
    global BestRule, BestRuleApp, RecentPromisingRule, RecentPromisingRuleApp, AcceptableTrustLevel
    global debugMAX, CodeletTreeLogfile, CodeletTreeLogHandle, ActivationsLogfile, ActivationsLogHandle
    global MainStream
    Steps_Finished = 0
    Break_Loop = None
    CurrentCodelet = None
    CurrentCodeletFamily = None
    CurrentRunnableString = ""
    AtLeastOneUserVerification = None
    TestingOptionsRef = None
    TestingMode = None
    ExtensionRejectedByUser.clear()
    LogString = ""
    PossibleFeatures.clear()
    PossibleFeatures.update((f, 1) for f in _POSSIBLE_FEATURES)
    Feature.clear()
    Options_ref = None
    RealSequence.clear()
    InitialTermCount = None
    TimeOfLastNewElement = 0
    TimeOfNewStructure = 0
    InterstepSleep = 0
    Sanity = 1
    Hilit.clear()
    BestRule = None
    BestRuleApp = None
    RecentPromisingRule = None
    RecentPromisingRuleApp = None
    GroupStrengthByConsistency.clear()
    AcceptableTrustLevel = 0.5
    debugMAX = None
    CodeletTreeLogfile = "codelet_tree.log"
    CodeletTreeLogHandle = None
    ActivationsLogfile = "activations.log"
    ActivationsLogHandle = None
    sstream2.reset()
    MainStream = sstream2.SStream2.create_new("MainStream")


reset()
