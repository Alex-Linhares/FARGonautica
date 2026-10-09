"""Port of lib/SCoderack.pm: the coderack.

Module-level state mirrors the Perl package variables: ``CODELETS`` (@CODELETS),
``URGENCIES_SUM``, ``CODELET_COUNT``, ``LastSelectedRunnable`` and ``HistoryOfRunnable``.
``clear()`` is SCoderack->clear (it leaves LastSelectedRunnable alone, as the Perl does);
``reset()`` also clears LastSelectedRunnable, for test isolation.

Codelets are read as ``cl[0..3]`` (family, urgency, creation_time, args), via SCodelet's
``@{}`` view. Urgencies are numified the Perl way (strings, undef).

PERL-QUIRK: if the random number exceeds the urgency mass (fractional urgencies, or a
negative remaining sum), Perl's ``_choose_codelet`` walks past the end of @CODELETS,
autovivifying urgency-0 entries forever, and never returns. The port raises Confess there.
"""
from pathlib import Path

from seqsee import global_ as Global
from seqsee.errors import Confess
from seqsee.util import perl_num, perl_ref_string, perl_str, perl_true, rand

MAX_CODELETS = 25
CODELET_COUNT = 0
CODELETS = []
URGENCIES_SUM = 0
LastSelectedRunnable = None
HistoryOfRunnable = {}

_START_CODELETS_CONF = Path(__file__).resolve().parents[3] / "config" / "start_codelets.conf"


def clear():
    """Perl: clear. Makes it all empty."""
    global CODELET_COUNT, URGENCIES_SUM
    CODELET_COUNT = 0
    URGENCIES_SUM = 0
    CODELETS.clear()
    HistoryOfRunnable.clear()


def reset():
    """clear() plus LastSelectedRunnable (not touched by Perl's clear)."""
    global LastSelectedRunnable
    clear()
    LastSelectedRunnable = None


def _read_config(path):
    """A minimal Config::Std ``read_config``: ``{section: {key: value or [values]}}``.

    Repeated keys in a section become a list, as Config::Std does. Comments start with
    ``#`` or ``;``.
    """
    config = {}
    section = config.setdefault("", {})
    for line in Path(path).read_text().splitlines():
        text = line.strip()
        if not text or text[0] in "#;":
            continue
        if text.startswith("[") and text.endswith("]"):
            section = config.setdefault(text[1:-1].strip(), {})
            continue
        key, sep, value = text.partition("=")
        if not sep:
            key, sep, value = text.partition(":")
        key, value = key.strip(), value.strip()
        if key in section:
            old = section[key]
            section[key] = old + [value] if isinstance(old, list) else [old, value]
        else:
            section[key] = value
    return config


def init(options=None):
    """Perl: init. Ignores ``options``; adds the start codelets from config/start_codelets.conf."""
    from seqsee.scodelet import SCodelet
    print("Initializing Coderack...")
    if Global.Steps_Finished is None:
        Global.Steps_Finished = 0
    if perl_true(Global.Feature.get("CodeletTree")):
        handle = open(Global.CodeletTreeLogfile, "w", buffering=1)
        Global.CodeletTreeLogHandle = handle
        handle.write("Initial\n")
    launch_config = _read_config(_START_CODELETS_CONF)
    for family, section in launch_config.items():
        if not perl_true(family):
            continue
        urgencies = section.get("urgency")
        urgencies = urgencies if isinstance(urgencies, list) else [urgencies]
        for urgency in urgencies:
            add_codelet(SCodelet(family, urgency, {}))


def _log_tree(text):
    if perl_true(Global.Feature.get("CodeletTree")):
        Global.CodeletTreeLogHandle.write(text)


def add_codelet(codelet):
    """Perl: add_codelet. Adds the codelet, expunging the least urgent one past MAX_CODELETS."""
    from seqsee.scodelet import SCodelet
    global CODELET_COUNT, URGENCIES_SUM
    if codelet is None:
        raise Confess('Can\'t call method "isa" on an undefined value')
    if isinstance(codelet, (dict, list)):
        raise Confess('Can\'t call method "isa" on unblessed reference')
    if isinstance(codelet, str) and codelet == "SCodelet":
        # PERL-QUIRK: "SCodelet"->isa("SCodelet") is true; the push happens, then ->[1] dies.
        CODELET_COUNT += 1
        CODELETS.append(codelet)
        raise Confess('Can\'t use string ("SCodelet") as an ARRAY ref while "strict refs" in use')
    if not isinstance(codelet, SCodelet):
        raise Confess("A non codelet is being added")
    CODELET_COUNT += 1
    CODELETS.append(codelet)
    _log_tree(f"\t{perl_ref_string(codelet)}\t{perl_str(codelet[0])}\t{perl_str(codelet[1])}\n")
    URGENCIES_SUM += perl_num(codelet[1])
    if CODELET_COUNT > MAX_CODELETS:
        expunge_codelet()


def choose_codelet():
    """Perl: _choose_codelet. Returns the index of a codelet, chosen by urgency (one draw)."""
    if not CODELET_COUNT:
        return None
    if not perl_true(URGENCIES_SUM):
        raise Confess("In Coderack: urgencies sum 0, but codelet count non-zero")
    random_number = 1 + int(rand(URGENCIES_SUM))
    index = 0
    while True:
        if index >= len(CODELETS):
            # PERL-QUIRK: Perl autovivifies urgency-0 entries here and never terminates.
            raise Confess("In Coderack: random number exceeds the urgency mass "
                          "(Perl loops forever here)")
        urgency = perl_num(CODELETS[index][1])
        if not random_number > urgency:
            return index
        random_number -= urgency
        index += 1


def get_urgencies_sum():
    """Perl: get_urgencies_sum."""
    return URGENCIES_SUM


def get_codelet_count():
    """Perl: get_codelet_count."""
    return CODELET_COUNT


def get_next_runnable():
    """Perl: get_next_runnable. Removes and returns a codelet chosen by urgency.

    With an empty coderack it returns a fresh FocusOn codelet (urgency 100) and leaves
    LastSelectedRunnable alone.
    """
    from seqsee.scodelet import SCodelet
    global URGENCIES_SUM, CODELET_COUNT, LastSelectedRunnable
    Global.LogString = "\n\n=======\nLogged Message:\n===\n"
    if not CODELET_COUNT:
        new_reader = SCodelet("FocusOn", 100, {})
        _log_tree(f"Background\n\t{perl_ref_string(new_reader)}\tFocusOn\t100\n")
        return new_reader
    idx = choose_codelet()
    to_return = CODELETS.pop(idx)
    key = "Seqsee::SCF::" + perl_str(to_return[0])
    HistoryOfRunnable[key] = HistoryOfRunnable.get(key, 0) + 1
    URGENCIES_SUM -= perl_num(to_return[1])
    CODELET_COUNT -= 1
    LastSelectedRunnable = to_return
    return to_return


def expunge_codelet():
    """Perl: expunge_codelet. Drops the least urgent codelet (the last among ties, stable sort)."""
    global CODELET_COUNT, URGENCIES_SUM
    CODELETS.sort(key=lambda c: perl_num(c[1]), reverse=True)
    cl = CODELETS.pop()
    _log_tree(f"Expunge {perl_ref_string(cl)}\n")
    CODELET_COUNT -= 1
    URGENCIES_SUM -= perl_num(cl[1])


def _dist_key(v):
    """Perl hash keys stringify: scalars by value, objects by identity."""
    if v is None or isinstance(v, (str, int, float)):
        return perl_str(v)
    if isinstance(v, list):
        return f"ARRAY(0x{id(v):x})"
    if isinstance(v, dict):
        return f"HASH(0x{id(v):x})"
    return v


def attention_distribution():
    """Perl: AttentionDistribution. Maps objects (and scalar args, by string) to the chance of
    being looked at next. FocusOn codelets count through the workspace's reader distribution.
    """
    from seqsee import sworkspace
    if not perl_true(URGENCIES_SUM):
        return {}
    dist = {}
    reader_urgencies_sum = 0
    for cl in CODELETS:
        if perl_str(cl[0]) == "FocusOn":
            reader_urgencies_sum += perl_num(cl[1])
            continue
        urgency = perl_num(cl[1])
        for v in cl[3].values():
            k = _dist_key(v)
            dist[k] = dist.get(k, 0) + urgency
    if perl_true(reader_urgencies_sum):
        probs, objects = sworkspace.get_object_or_relation_choice_probability_distribution()
        for obj, prob in zip(objects, probs):
            k = _dist_key(obj)
            dist[k] = dist.get(k, 0) + prob * reader_urgencies_sum
    total = perl_num(URGENCIES_SUM)
    return {k: v / total for k, v in dist.items()}
