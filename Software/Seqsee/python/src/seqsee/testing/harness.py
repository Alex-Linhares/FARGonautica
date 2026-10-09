"""Port of Test/Seqsee.pm (Test::Seqsee): helpers for testing Seqsee, and RunSeqsee.

Test::Seqsee has no ``package`` line, so all of this lives in ``main`` in Perl. Loading it
sets ``$Global::TestingMode = 1`` and ``CurrentRunnableString = ""`` and calls
INITIALIZE_for_testing; here ``load()`` does that (call ``s.load()`` first). The
failed-request counter (ResetFailedRequests …) and the testing ``main::ask_user_extension``
were ported with UserInteraction (``user_interaction.reset_failed_requests`` …,
``testing_ask_user_extension``).

Test::More assertions go through ``seqsee.testing.more.ok`` (results in
``more.details``; each helper returns the ok value):

- ``undef_ok``, ``instance_of_cat_ok``, ``throws_thought_ok``, ``throws_no_thought_ok``;
- ``code_throws_stochastic_ok``/``_nok``/``_all_and_only_ok``/``_all_and_only_nok``, built on
  ``_wrap_to_get_payload_type`` and Test::Stochastic (``seqsee.testing.stochastic``);
- ``stochastic_test_codelet(setup=…, throws=…, post_run=…, codefamily=…)``;
- ``output_contains(sub, msg=…, always=[…], never=[…], sometimes=[…],
  sometimes_but_not_always=[…])`` (5 calls) and its ``output_*_contains`` wrappers;
  ``fringe_contains``, ``extended_fringe_contains``, ``action_contains`` (object or setup sub).

Runs: ``run_seqsee`` (RunSeqsee → ResultOfTestRun), ``reg_test_helper`` (RegTestHelper:
a (status, steps) tuple), ``reg_stat``, ``reg_stat_shell``, ``reg_harness`` (RegHarness),
``parse_seq_`` (ParseSeq_), ``TOO_LOW``/``PLEASANTLY_HIGH``. As in e2e_run.perl, seed the
RNG (``util.srand``) before ``run_seqsee``; ``s.reset_all()`` + ``load()`` give the state of
a fresh Perl process.

Port notes:
- main::message/debug_message/update_display/default_error_handler/ask_for_more_terms,
  which INITIALIZE_for_testing replaces by no-ops, are already headless no-ops/logging
  hooks in the port; only ``ask_user_extension`` is installed.
- Payloads: Perl's ``$e->payload`` method becomes a ``payload`` attribute or method of the
  exception; ``ref`` and ``isa`` use ``perl_name`` (``util.perl_ref``, the MRO).
- The LTM file is ``LTM_FILE`` (the repo root's memory_dump.dat, like the CLI; Perl uses the
  cwd); ``_sltm_dump`` is SLTM->Dump.
- RegStatShell writes the options to a file "foo", runs ``perl -e '…RegStat()'`` with the
  current features, and RegStat reads "foo", runs 10 trials and writes its outputs back to
  "foo" with Data::Dumper. Here ``reg_stat(opts_ref)`` takes the options directly and
  ``reg_stat_shell`` runs it in-process after ``s.reset_all()`` (keeping Global.Feature)
  and ``load()``. No "foo" file.

PERL-QUIRKs (oracle-confirmed, ported):
- RegHarness reads its config file from its *second* argument (``my $file = shift`` after
  ``$arg1``), so ``RegHarness($file)`` as util/regtest.pl calls it dies "Can't open config
  file ''". The result files are named after ``$_`` (undef → ".last_res"/".log_res"): the
  ``name`` argument here. ``.last_res`` gets "RESULTS = ARRAY(0x…)". The earlier GotIt count
  is a string from the file; an unknown count (missing, or > 10) has no TooLow /
  PleasantlyHigh entry, which reads as 0.
- RegHarness's ``||=`` defaults replace 0 too (min_extension 0 → 2).
- RegStat counts only GotIt/Extended (with their steps) and deaths (UnnaturalDeath); other
  statuses only add "Different result!" to RESULTS.
- RegTestHelper rethrows a FinishedTest without got_it, and any other death unless there
  were more than max_false failed requests (then ("TooManyFalseQueries", 0)).
- RunSeqsee confesses on a FinishedTest without got_it, skipping the LTM dump.
- throws_thought_ok/_wrap_to_get_payload_type expect thoughts as exception payloads, which
  current Seqsee never throws: a codelet that dies with a string makes throws_thought_ok
  die ("Can't locate object method "payload" …"), and the wrapper rethrows it.
- output_contains appends its failure text straight after the message
  ("output_containshalf was not always seen. …") and stops at the first failure.
- code_throws_stochastic_all_and_only_ok passes the check sub on as Test::Stochastic's
  message argument (a true check sub is used as the test name in Perl).
"""
import inspect
import os
import re
import sys
import time
from pathlib import Path

from seqsee import global_ as Global
from seqsee import scoderack, seqsee_main, sltm, sworkspace, user_interaction, util
from seqsee.errors import Confess, FinishedTest, FinishedTestBlemished, NotClairvoyant
from seqsee.objects import result_of_test_run as rotr
from seqsee.testing import stochastic
from seqsee.testing.more import ok
from seqsee.util import perl_num, perl_ref, perl_str, perl_true

LTM_FILE = str(Path(__file__).resolve().parents[4] / "memory_dump.dat")


def _sltm_dump(filename):
    """Perl: SLTM->Dump($filename)."""
    sltm.dump(filename)


def _err_text(e):
    return str(e)


def _perl_isa(obj, name):
    """Perl: $obj->isa($name), by perl_name along the MRO."""
    return any(c.__dict__.get("perl_name", c.__name__) == name for c in type(obj).__mro__)


def _payload(e):
    """Perl: $e->payload."""
    if not hasattr(e, "payload"):
        what = perl_ref(e) if hasattr(e, "perl_name") else perl_str(_err_text(e))
        raise Confess(f'Can\'t locate object method "payload" via package "{what}" '
                      f'(perhaps you forgot to load "{what}"?)\n')
    p = e.payload
    return p() if callable(p) else p


# ------------------------------------------------------------------ load
def load():
    """What ``use Test::Seqsee`` does at load: TestingMode, CurrentRunnableString and
    INITIALIZE_for_testing (prints "View: 1!")."""
    Global.TestingMode = 1
    Global.CurrentRunnableString = ""
    initialize_for_testing()


def initialize_for_testing():
    """Perl: INITIALIZE_for_testing."""
    Global.TestingOptionsRef = seqsee_main.read_config(seq="0")  # Random
    Global.Steps_Finished = 0
    Global.CurrentRunnableString = ""
    user_interaction.install(ask_user_extension=user_interaction.testing_ask_user_extension)


def reset():
    """Forget Test::More results and Test::Stochastic settings (for tests)."""
    from seqsee.testing import more
    more.reset()
    stochastic.reset()


# ------------------------------------------------------------------ simple assertions
def undef_ok(what, msg=None):
    """Perl: undef_ok."""
    if what is None:
        return ok(1, msg or "is undefined")
    return ok(0, msg or f"expected undef, got {perl_str(what)}")


def instance_of_cat_ok(what, cat, msg=None):
    """Perl: instance_of_cat_ok."""
    msg = msg or f"{perl_str(what)} is an instance of {perl_str(cat)}"
    return ok(what.instance_of_cat(cat), msg)


def throws_thought_ok(cl, type_):
    """Perl: throws_thought_ok: running the codelet must die with a thought payload of one
    of the types (names without "SThought::" get it). Returns the payload on success."""
    types = list(type_) if isinstance(type_, (list, tuple)) else [type_]
    types = [t if t.startswith("SThought::") else "SThought::" + t for t in types]
    try:
        cl.run()
    except Exception as e:  # noqa: BLE001
        err = e
    else:
        ok(0, "No thought returned")
        return None
    payload = _payload(err)
    if not perl_true(payload):
        ok(0, "Died without payload")
        return None
    for t in types:
        if _perl_isa(payload, t):
            ok(1, f"{perl_str(payload)} returned")
            return payload
    return ok(0, f"Wrong type: {perl_str(payload)}. Expected one of: " + ", ".join(types))


def throws_no_thought_ok(cl):
    """Perl: throws_no_thought_ok."""
    try:
        cl.run()
    except Exception as e:  # noqa: BLE001
        return ok(0, f"Should return no thought! {_err_text(e)}")
    return ok(1, "Lived Ok")


# ------------------------------------------------------------------ stochastic codelet checks
def _wrap_to_get_payload_type(subr, check_sub=None):
    """Perl: _wrap_to_get_payload_type: a sub returning the payload type after
    SThought::/Seqsee::SCF:: ("" when subr lives and check_sub, if any, is true)."""
    def wrapped():
        try:
            subr()
        except Exception as e:
            if hasattr(e, "payload"):
                m = re.match(r"(Seqsee::SCF|SThought)::(.*)", perl_ref(_payload(e)), re.S)
                if m:
                    return m.group(2)
            raise
        if check_sub is not None and not perl_true(check_sub()):
            raise Confess("Failed Check in check_sub")
        return ""
    return wrapped


def code_throws_stochastic_ok(subr, arr_ref, check_sub=None):
    """Perl: code_throws_stochastic_ok."""
    return stochastic.stochastic_all_seen_ok(_wrap_to_get_payload_type(subr, check_sub), arr_ref)


def code_throws_stochastic_nok(subr, arr_ref):
    """Perl: code_throws_stochastic_nok."""
    return stochastic.stochastic_all_seen_nok(_wrap_to_get_payload_type(subr), arr_ref)


def code_throws_stochastic_all_and_only_ok(subr, arr_ref, check_sub=None):
    """Perl: code_throws_stochastic_all_and_only_ok. PERL-QUIRK: check_sub also goes to
    Test::Stochastic as the message (and is the test name when true)."""
    msg = f"CODE(0x{id(check_sub):x})" if check_sub is not None else None
    return stochastic.stochastic_all_and_only_ok(
        _wrap_to_get_payload_type(subr, check_sub), arr_ref, msg)


def code_throws_stochastic_all_and_only_nok(subr, arr_ref):
    """Perl: code_throws_stochastic_all_and_only_nok."""
    return stochastic.stochastic_all_and_only_nok(_wrap_to_get_payload_type(subr), arr_ref)


def stochastic_test_codelet(setup=None, throws=None, post_run=None, codefamily=None):
    """Perl: stochastic_test_codelet(setup => …, throws => […], post_run => …,
    codefamily => …)."""
    from seqsee.scodelet import SCodelet

    def trial():
        util.clear_all()
        opts_ref = setup()
        cl = SCodelet(codefamily, 100, opts_ref)
        cl.run()

    code_throws_stochastic_ok(trial, throws)
    if post_run:
        return ok(post_run(), "checking the after effects")
    return ok(1, "No check_sub: nothing to check")


# ------------------------------------------------------------------ output_contains & co.
def output_contains(subr, msg=None, **scope):
    """Perl: output_contains($subr, %scope): call subr 5 times; each call returns a list,
    and each distinct item counts once per call. Quantifiers: always, never, sometimes,
    sometimes_but_not_always."""
    msg = msg or "output_contains"
    seen = {}
    times = 5
    for _ in range(times):
        seen_here = {}
        for x in subr():
            seen_here[perl_str(x)] = 1
        for k in seen_here:
            seen[k] = seen.get(k, 0) + 1

    problems_found = 0
    for k, v in scope.items():
        for item in v:
            item = perl_str(item)
            n = seen.setdefault(item, 0)
            if k == "always":
                bad = n != times
                text = f"{item} was not always seen. Seen {n} times out of {times}"
            elif k == "never":
                bad = n != 0
                text = f"{item} was not never seen. Seen {n} times out of {times}"
            elif k == "sometimes":
                bad = not n > 0
                text = f"{item} was not seen anytime. Seen {n} times out of {times}"
            elif k == "sometimes_but_not_always":
                bad = not (0 < n < times)
                text = (f"Expected to see {item} sometimes but not always, but it was "
                        + ("always" if n else "never") + " seen")
            else:
                raise Confess(f"unknown quantifier {k}")
            if bad:
                problems_found = 1
                msg += text
                break
        else:
            continue
        break
    return ok(1 - problems_found, msg)


def _as_list(arg):
    return arg if isinstance(arg, list) else [arg]


def output_always_contains(subr, arg):
    """Perl: output_always_contains."""
    return output_contains(subr, always=_as_list(arg))


def output_never_contains(subr, arg):
    """Perl: output_never_contains."""
    return output_contains(subr, never=_as_list(arg))


def output_sometimes_contains(subr, arg):
    """Perl: output_sometimes_contains."""
    return output_contains(subr, sometimes=_as_list(arg))


def output_sometimes_but_not_always_contains(subr, arg):
    """Perl: output_sometimes_but_not_always_contains."""
    return output_contains(subr, sometimes_but_not_always=_as_list(arg))


def _contains(target, getter, msg, options):
    """fringe_contains & co.: target is an object, or a setup sub (called after
    SUtil::clear_all for each of the 5 calls)."""
    if inspect.isfunction(target) or inspect.ismethod(target):  # Perl: ref($self) eq "CODE"
        def subr():
            util.clear_all()
            return getter(target())
    else:
        def subr():
            return getter(target)
    return output_contains(subr, msg=msg, **options)


def fringe_contains(target, **options):
    """Perl: fringe_contains."""
    return _contains(target, lambda o: [x[0] for x in o.get_fringe()], "fringe_contains  ",
                     options)


def extended_fringe_contains(target, **options):
    """Perl: extended_fringe_contains."""
    return _contains(target, lambda o: [x[0] for x in o.get_extended_fringe()],
                     "extended_fringe_contains  ", options)


def action_contains(target, **options):
    """Perl: action_contains (the actions' class names)."""
    return _contains(target, lambda o: [perl_ref(a) for a in o.get_actions()],
                     "action_contains  ", options)


# ------------------------------------------------------------------ runs
def _step_until_done(max_steps):
    while not perl_true(seqsee_main.interaction_step_n(
            {"n": max_steps, "max_steps": max_steps, "update_after": max_steps})):
        pass


def _init_run(seq, continuation):
    user_interaction.reset_failed_requests()
    sworkspace.init({**Global.TestingOptionsRef, "seq": seq})
    Global.set_future_terms(*continuation)
    scoderack.init(Global.TestingOptionsRef)
    Global.MainStream.init()


def reg_test_helper(opts_ref):
    """Perl: RegTestHelper(\\%opts): one run; returns (status, steps) with status GotIt,
    Extended, BlemishedGotIt, TooManyFalseQueries, ExtendedWithoutGettingIt or
    NotEvenExtended."""
    for k in ("seq", "continuation", "max_false", "max_steps", "min_extension"):
        if k not in opts_ref:
            raise Confess(f"Missing option {k}")
    seq = opts_ref["seq"]
    continuation = opts_ref["continuation"]
    max_false_continuations = opts_ref["max_false"]
    max_steps = opts_ref["max_steps"]
    min_extension = opts_ref["min_extension"]

    _init_run(seq, continuation)
    sworkspace.ReadHead = 0
    Global.clear()
    sys.stderr.write("\n****** BEGIN ANOTHER RUN: ")
    try:
        _step_until_done(max_steps)
    except FinishedTest as e:
        if perl_true(e.got_it):
            sys.stderr.write("+GOT IT\n")
            return "GotIt", Global.Steps_Finished
        raise
    except NotClairvoyant:
        sys.stderr.write("+EXTENDED (NO MORE KNOWN TERMS)\n")
        return "Extended", Global.Steps_Finished
    except FinishedTestBlemished:
        sys.stderr.write("+BLEMISHED GOT IT\n")
        return "BlemishedGotIt", Global.Steps_Finished
    except Exception:
        failed_requests = user_interaction.get_failed_requests()
        if perl_num(failed_requests) > perl_num(max_false_continuations):
            sys.stderr.write(f"+TOO MANY FAILED QUERIES ({perl_str(failed_requests)} > "
                             f"{perl_str(max_false_continuations)})!\n")
            sys.stderr.write("; ".join(util.perl_keys(Global.ExtensionRejectedByUser)) + "\n")
            return "TooManyFalseQueries", 0
        raise
    if sworkspace.ElementCount - len(seq) > perl_num(min_extension):
        sys.stderr.write("+EXTENDED A BIT\n")
        return "ExtendedWithoutGettingIt", Global.Steps_Finished
    sys.stderr.write("+NOT EVEN EXTENDED ONCE\n")
    return "NotEvenExtended", Global.Steps_Finished


def reg_stat(opts_ref):
    """Perl: RegStat (with the options passed in instead of read from "foo"): 10 trials of
    RegTestHelper; prints the options and the outcome counts; returns the counts plus
    RESULTS (one line per trial) and avgcc (mean steps of the successes)."""
    outputs = {}
    errors = {}
    successful_codelet_count = []
    results = []
    for _ in range(10):
        out = step_count = None
        error = None
        try:
            out, step_count = reg_test_helper(opts_ref)
        except Exception as e:  # noqa: BLE001 - Perl's eval
            error = _err_text(e)
        if out in ("GotIt", "Extended"):
            successful_codelet_count.append(step_count)
            results.append(f"SUCCESS: {out}\t{perl_str(step_count)}")
            outputs[out] = outputs.get(out, 0) + 1
            continue
        if error:
            results.append(f"Error: {error}")
            errors[error] = errors.get(error, 0) + 1
            outputs["UnnaturalDeath"] = outputs.get("UnnaturalDeath", 0) + 1
            continue
        m = re.match(r"UnnaturalDeath:\s*(.*)$", perl_str(out))
        if m:
            errors[m.group(1)] = errors.get(m.group(1), 0) + 1
            results.append(f"Error: {m.group(1)}")
            outputs["UnnaturalDeath"] = outputs.get("UnnaturalDeath", 0) + 1
            sys.stderr.write(f"\nERROR:\n{m.group(1)}\n=================\n")
            continue
        results.append("Different result!")

    sys.stdout.write("============\n")
    for k, v in opts_ref.items():
        v2 = ", ".join(perl_str(x) for x in v) if isinstance(v, list) else perl_str(v)
        sys.stdout.write(f"{k}\t=>  {v2}\n")
    sys.stdout.write("============\n")
    for k, v in outputs.items():
        sys.stdout.write(f"{v}\t times: {k[:50]}\n")

    outputs["RESULTS"] = results
    if successful_codelet_count:
        avg = sum(successful_codelet_count) / len(successful_codelet_count)
        outputs["avgcc"] = int(avg) if avg == int(avg) else avg
    return outputs


def reg_stat_shell(opts_ref):
    """Perl: RegStatShell: RegStat in a fresh process with the current features. Here:
    in-process, after ``s.reset_all()`` (Global.Feature kept) and ``load()``."""
    from seqsee import s
    from seqsee.testing import more
    features = dict(Global.Feature)
    details = list(more.details)  # the parent process's Test::More results stay
    s.reset_all()
    more.details.extend(details)
    Global.Feature.clear()
    Global.Feature.update(features)
    load()
    return reg_stat(opts_ref)


TOO_LOW = {"0": -1, "1": 0, "2": 0, "3": 1, "4": 2, "5": 3, "6": 4, "7": 5, "8": 6, "9": 7,
           "10": 9}
PLEASANTLY_HIGH = {"0": 1, "1": 3, "2": 4, "3": 5, "4": 6, "5": 7, "6": 8, "7": 9, "8": 9,
                   "9": 10, "10": 11}


class ConfigError(Exception):
    """Config::Std's read_config death."""


def _read_config_std(filename):
    """Config::Std's read_config: {section: {key: value}}; dies on a line that is not a
    comment, a [section] or a key = value / key: value pair."""
    try:
        if perl_str(filename) == "":
            raise FileNotFoundError(2, "No such file or directory")
        text = Path(perl_str(filename)).read_text()
    except OSError as e:
        raise ConfigError(f"Can't open config file '{perl_str(filename)}' "
                          f"({(e.strerror or '').lower()})\n") from None
    config = {"": {}}
    section = config[""]
    for line in text.splitlines():
        stripped = line.strip()
        if not stripped or stripped[0] in "#;":
            continue
        m = re.fullmatch(r"\[\s*(.*?)\s*\]", stripped)
        if m:
            section = config.setdefault(m.group(1), {})
            continue
        m = re.fullmatch(r"([^=:\s][^=:]*?)\s*[=:]\s*(.*)", stripped)
        if not m:
            raise ConfigError(f"Error in config file '{perl_str(filename)}' near:\n\n"
                              f"\t{line}\n")
        section[m.group(1)] = m.group(2)
    return config


def _perl_value_string(v):
    if isinstance(v, list):
        return f"ARRAY(0x{id(v):x})"
    if isinstance(v, dict):
        return f"HASH(0x{id(v):x})"
    return perl_str(v)


def reg_harness(arg1, file=None, name=None):
    """Perl: RegHarness($arg1, $file): run RegStatShell on one sequence (a hash, or the
    config file given as the second argument), compare GotIt with ``name.last_res`` and log
    to ``name.log_res``. Returns (improved, became_worse, results, opts)."""
    improved, became_worse, results = [], [], []
    if isinstance(arg1, dict):
        opts = dict(arg1)
    else:
        config = _read_config_std(file)
        opts = dict(config[""])
    opts["seq"], opts["continuation"] = parse_seq_(opts.get("seq"))
    for k, default in (("max_false", 10), ("max_steps", 10000), ("min_extension", 2)):
        if not perl_true(opts.get(k)):
            opts[k] = default
    start_time = time.time()
    output = reg_stat_shell(opts)
    if not perl_true(output.get("avgcc")):
        output["avgcc"] = 0
    results.extend(output["RESULTS"])
    total_time = int(time.time() - start_time)
    sys.stdout.write(f"Processing time: {total_time}\n")
    if not perl_true(output.get("GotIt")):
        output["GotIt"] = 0
    current = output["GotIt"]

    earlier = 0
    prefix = perl_str(name)
    last_res_file = prefix + ".last_res"
    log_file = prefix + ".log_res"
    if os.path.exists(last_res_file):
        try:
            earlier = _read_config_std(last_res_file)[""].get("GotIt")
        except ConfigError:
            earlier = 0

    with open(log_file, "a") as log, open(last_res_file, "w") as current_fh:
        log.write(f"[{int(time.time())}]\n")
        for k, v in output.items():
            k = re.sub(r"\W", "", k)
            log.write(f"{k} = {_perl_value_string(v)}\n")
            current_fh.write(f"{k} = {_perl_value_string(v)}\n")

    if perl_num(current) <= perl_num(TOO_LOW.get(perl_str(earlier))):
        sys.stdout.write(f"##########\n# PERFORMANCE WORSE!\n Had Got It {perl_str(earlier)} "
                         f"times, now just {perl_str(current)}")
        became_worse.append([opts["seq"], earlier, current])
    elif perl_num(current) >= perl_num(PLEASANTLY_HIGH.get(perl_str(earlier))):
        sys.stdout.write(f"!!!!!!!!!\n# PERFORMANCE BETTER!\n Had Got It {perl_str(earlier)} "
                         f"times, now it is {perl_str(current)}")
        improved.append([opts["seq"], earlier, current])
    return improved, became_worse, results, opts


def parse_seq_(seq):
    """Perl: ParseSeq_("1 2 3 | 4 5") → ([1, 2, 3], [4, 5]) (strings). Text after a second
    "|" is dropped."""
    parts = perl_str(seq).split("|")
    s, c = (parts + [""])[:2]
    return s.split(), c.split()


def run_seqsee(seq, continuation, max_steps, max_false, min_extension):
    """Perl: RunSeqsee(\\@seq, \\@continuation, $max_steps, $max_false, $min_extension) →
    ResultOfTestRun. ``max_false`` is unused, as in Perl."""
    _init_run(seq, continuation)
    if perl_true(Global.Feature.get("LTM")):
        try:
            sltm.load(LTM_FILE)
        except Exception as e:  # noqa: BLE001 - Perl's eval (an exit still exits)
            return rotr.ResultOfTestRun({
                "status": rotr.Crashed,
                "steps": Global.Steps_Finished,
                "error": "Unable to load memory file! " + _err_text(e),
            })
    else:
        print("LTM not passed in as an option.")
    sltm.init()

    sworkspace.ReadHead = 0
    Global.clear()

    result = None
    try:
        _step_until_done(max_steps)
    except FinishedTest as err:
        if not perl_true(err.got_it):
            raise Confess("A SErr::FinishedTest thrown without getting it. Bad.") from err
        result = rotr.ResultOfTestRun(
            {"status": rotr.Successful, "steps": Global.Steps_Finished, "error": None})
    except NotClairvoyant:
        result = rotr.ResultOfTestRun(
            {"status": rotr.RanOutOfTerms, "steps": Global.Steps_Finished, "error": None})
    except FinishedTestBlemished:
        result = rotr.ResultOfTestRun(
            {"status": rotr.InitialBlemish, "steps": Global.Steps_Finished, "error": None})
    except Exception as err:  # noqa: BLE001 - Perl's eval
        result = rotr.ResultOfTestRun({"status": rotr.Crashed, "steps": Global.Steps_Finished,
                                       "error": f"Crashed!\n{_err_text(err)}"})

    if perl_true(Global.Feature.get("LTM")):
        _sltm_dump(LTM_FILE)
    else:
        print("LTM not passed in as an option, so no need to save.")
    if result is not None:
        return result

    # So did not die.
    status = (rotr.ExtendedABit if sworkspace.ElementCount - len(seq) > perl_num(min_extension)
              else rotr.NotEvenExtended)
    return rotr.ResultOfTestRun({"status": status, "steps": Global.Steps_Finished, "error": None})
