"""The Pnet data of python/numbo/pnet_def.py against the oracle's dump,
python/fixtures/pnet.json (lisp/tests/oracle/pnet.lisp), field by field and in
order (loop0002 item 4)."""

import io
import json
import subprocess
import sys

import pytest

from conftest import PYTHON_DIR, load_fixture, lisp_data

from numbo import pnet_def
from numbo.franz import Symbol, intern

PNET = load_fixture("pnet.json")


def _slot(lisp_name):
    return lisp_name.replace("-", "_")


@pytest.fixture
def holders():
    """A fresh registry of the 1987 holder variables, after init-pnet."""
    h = {}
    pnet_def.init_pnet(h)
    return h


def test_slots_in_defflavor_order():
    assert list(pnet_def.SLOTS) == [_slot(s) for s in PNET["slots"]]
    assert len(pnet_def.SLOTS) == 19


def test_new_pnode_has_every_slot_nil():
    p = pnet_def.Pnode()
    assert all(getattr(p, s) is None for s in pnet_def.SLOTS)


def test_holders_in_init_pnet_order(holders):
    assert len(holders) == 91
    assert [h.name for h in holders] == [e["holder"] for e in PNET["holders"]]
    assert all(isinstance(h, Symbol) and h.package == "NUMBO" for h in holders)
    assert all(isinstance(p, pnet_def.Pnode) for p in holders.values())


@pytest.mark.parametrize("index", range(91))
def test_pnode_fields(holders, index):
    entry = PNET["holders"][index]
    pnode = holders[intern(entry["holder"])]
    expected = entry["pnode"]
    assert list(expected) == PNET["slots"]
    for slot, value in expected.items():
        assert json.dumps(lisp_data(getattr(pnode, _slot(slot)))) == json.dumps(value), \
            (entry["holder"], slot)


def test_pnet_is_the_88_holders_in_order(holders):
    pnet = pnet_def.pnet_list(holders)
    assert len(pnet) == 88
    assert [h.name for h in pnet_def.PNET] == PNET["pnet"]
    for holder, pnode in zip(pnet_def.PNET, pnet):
        assert pnode is holders[holder]
    # plus, minus and times are created but are not in *pnet*.
    left_out = [h.name for h in holders if h not in pnet_def.PNET]
    assert left_out == ["PLUS", "MINUS", "TIMES"]


def test_neighbors_name_holders(holders):
    """initialize-pnet-2 evaluates each neighbor pair; the oracle's resolved
    pairs (config-start) are the holders pnet_def names, in order."""
    for entry in PNET["config-start"]:
        pnode = holders[intern(entry["holder"])]
        raw = [[n.name, l.name] for n, l in (pnode.neighbors or [])]
        assert raw == (entry["neighbors"] or []), entry["holder"]
        for n, l in pnode.neighbors or []:
            assert n in holders and l in holders


def test_codelet_thresholds_and_urgencies(holders):
    """Each codelet's threshold and urgency symbols, evaluated with the
    init-chiffre parameter values, give what the oracle's populate-coderack
    would see at the start of config."""
    params = PNET["parameters"]["init-chiffre"]
    for entry in PNET["config-start"]:
        pnode = holders[intern(entry["holder"])]
        got = [[fn.name, params[thr.name], params[urg.name], args]
               for fn, thr, urg, args in (pnode.codelets or [])]
        assert json.dumps(got) == json.dumps(entry["codelets"] or []), entry["holder"]


def test_parameters_recorded():
    """The decay rates, thresholds, urgencies and initial activation the
    later items read; init-chiffre changes only these three."""
    defvar = PNET["parameters"]["defvar"]
    chiffre = PNET["parameters"]["init-chiffre"]
    assert list(defvar) == list(chiffre)
    changed = {k for k in defvar if defvar[k] != chiffre[k]}
    assert changed == {"%UPPER-THRESHOLD%", "%SECOND-URGENCY%"}
    assert isinstance(chiffre["%INITIAL-ACTIVATION%"], float)


def test_init_pnet_lists_are_fresh():
    """Each World gets its own neighbor and codelet lists (one SBCL process
    runs init-pnet once), but the symbols in them are the interned ones."""
    a, b = {}, {}
    pnet_def.init_pnet(a)
    pnet_def.init_pnet(b)
    one = intern("NODE-1")
    assert a[one] is not b[one]
    assert a[one].neighbors == b[one].neighbors
    assert a[one].neighbors is not b[one].neighbors
    assert a[one].neighbors[0] is not b[one].neighbors[0]
    assert a[one].neighbors[0][0] is b[one].neighbors[0][0]
    sub = intern("NODE-SUBTRACT")
    assert a[sub].codelets[0] is not b[sub].codelets[0]


def _config_start_pnet(holders):
    pnet = pnet_def.pnet_list(holders)
    for entry, pnode in zip(PNET["config-start"], pnet):
        assert entry["activation"] == 0.0
        pnode.activation = entry["activation"]
    holders[intern("NODE-5")].activation = 50.5
    holders[intern("PLUS2-3")].activation = 24.0
    holders[intern("OPERATION")].activation = 7
    return pnet


def test_print_pnet_all(holders):
    out = io.StringIO()
    pnet_def.print_pnet_all(_config_start_pnet(holders), out)
    assert out.getvalue() == PNET["print-pnet-all"]


def test_print_pnet(holders):
    out = io.StringIO()
    pnet_def.print_pnet(_config_start_pnet(holders), 24, out)
    assert out.getvalue() == PNET["print-pnet"]


def test_generated_table_is_current():
    """pnet_def.py's table is what python/scripts/gen_pnet_def.py makes from
    lisp/src/pnet-def.lisp today."""
    proc = subprocess.run(
        [sys.executable, str(PYTHON_DIR / "scripts" / "gen_pnet_def.py"), "--check"],
        capture_output=True, text=True, timeout=60)
    assert proc.returncode == 0, proc.stdout + proc.stderr
