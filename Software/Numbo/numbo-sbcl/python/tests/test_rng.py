"""The shared RNG matches the oracle's splitmix64 (src/oracle.lisp) bit for bit.

Expected values: python/fixtures/rng_vectors.json, written by
tests/oracle/rng-vectors.lisp.
"""

import pytest

from conftest import load_fixture

VECTORS = load_fixture("rng_vectors.json")


def test_fixture_shape():
    assert VECTORS["algorithm"] == "splitmix64"
    assert sorted(VECTORS["outputs"]) == ["0", "1", "18"]
    assert all(len(v) == 20 for v in VECTORS["outputs"].values())
    # Seed 0's first output is Vigna's published splitmix64 value.
    assert VECTORS["outputs"]["0"][0] == 0xE220A8397B1DCDAF
    for case in VECTORS["random"]:
        assert len(case["n"]) == len(case["values"])


@pytest.mark.parametrize("seed", ["0", "1", "18"])
def test_raw_outputs(seed):
    from numbo import rng
    r = rng.Rng(int(seed))
    assert [r.next_u64() for _ in VECTORS["outputs"][seed]] == VECTORS["outputs"][seed]


def test_random_n_sequences():
    from numbo import rng
    for case in VECTORS["random"]:
        r = rng.Rng(case["seed"])
        got = [r.random(n) for n in case["n"]]
        assert got == case["values"]
        assert r.draws == case["draws"]
