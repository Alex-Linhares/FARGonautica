"""The slow tests of `tests/test_benches.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

from test_benches import (
    CFG,
    CLI,
    catalogue,
    every_table_and_its_json,
    faults,
    lesions,
    pytest,
    rho,
    run_cli,
)

pytestmark = pytest.mark.slow


def test_rho_lesions_and_faults_at_sample_size():
    for module in (rho, lesions, faults):
        only = [module.SETS[0]]
        results = module.run(CFG, 1, 1, only)
        lines = module.report(results)
        assert lines[0].startswith("## ") and any(l.startswith("### " + only[0]) for l in lines)
        records = module.records(results)
        assert len(records) == module.planned(module.SETS, only) == len(results["variants"])
        assert all(r["bench"] == module.__name__.split(".")[-1] for r in records)


@pytest.mark.slow
def test_all_writes_every_table_and_its_json(tmp_path):
    """Every bench at one run through the command line (about 40 s on this machine under
    load: the lesions' capped runs and the variants' 474 surveys), so a slow test;
    `test_all_writes_every_table_and_its_json_on_the_quick_benches` checks the `all` path
    by default."""
    out = tmp_path / "results"
    proc = run_cli("all", "--runs", "1", "--out", str(out), "--quiet")
    assert proc.returncode == 0, proc.stderr
    every_table_and_its_json(out, proc.stdout, list(CLI.BENCHES))
    assert len((out / "ch4.json").read_text().splitlines()) == 1
    assert len((out / "potelbat.json").read_text().splitlines()) == len(catalogue())
