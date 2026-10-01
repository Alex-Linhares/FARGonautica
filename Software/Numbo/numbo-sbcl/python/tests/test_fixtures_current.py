"""Committed fixtures are exactly what the oracle writes today, and
regeneration is deterministic (loop0002 item 2)."""

import filecmp

from conftest import FIXTURES_DIR, regenerate_fixtures, requires_sbcl


def _files(d):
    return sorted(p.name for p in d.iterdir() if p.is_file())


@requires_sbcl
def test_regeneration_is_deterministic_and_matches_committed(tmp_path):
    runs = []
    for i in (1, 2):
        out = tmp_path / f"run{i}"
        out.mkdir()
        proc = regenerate_fixtures(out)
        assert proc.returncode == 0, proc.stdout + proc.stderr
        assert _files(out), "regeneration wrote no fixtures"
        runs.append(out)
    first, second = runs
    # Running it twice gives identical files ...
    assert _files(first) == _files(second)
    for name in _files(first):
        assert filecmp.cmp(first / name, second / name, shallow=False), name
    # ... and the committed fixtures are up to date (and none is stale).
    assert _files(first) == _files(FIXTURES_DIR), \
        "python/fixtures/ has missing or stale files: run python/scripts/regen_fixtures.sh"
    for name in _files(first):
        assert filecmp.cmp(first / name, FIXTURES_DIR / name, shallow=False), \
            f"{name} is out of date: run python/scripts/regen_fixtures.sh"
