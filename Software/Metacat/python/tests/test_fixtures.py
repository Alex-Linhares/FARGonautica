"""Item 00: the fixture pipeline (python/oracle/capture.py, python/fixtures/).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Fast tests check the committed fixtures without Chez: one fixture per test of
every battery, the split/join round trip, and that no input of the capture
changed since (SOURCES).  The slow test (marked `slow`; run by run-tests.sh in
the full tier, which the gate runs) captures every battery again under Chez
into a temporary directory and requires byte-identical files.
"""
from __future__ import annotations

import filecmp

import pytest

import capture
import scheme_forms
from chez_fixtures import FIXTURES, chez, manifest, values

BATTERIES = capture.batteries()


def test_every_battery_is_captured():
    frozen = [b for b in BATTERIES if capture.battery_path(b).parent == capture.DIFF]
    local = [b for b in BATTERIES if capture.battery_path(b).parent == capture.LOCAL]
    assert len(frozen) == 10              # tests/diff/*-battery.scm
    assert local == ["bridge-extra", "chez", "codelet-extra", "coderack-extra", "gui", "rule-extra",
                     "slipnet-extra", "trace-extra", "utilities-extra",
                     "workspace-extra"]   # python/oracle/batteries/ (items 02-10, 15)
    # extra-seeds/ holds whole runs, not a battery (capture_extra_seeds.py, item 12);
    # sgl-tcl/ holds the SGL interpreter's Tcl stream (capture_sgl_tcl.py, item 13)
    assert sorted(p.name for p in FIXTURES.iterdir()
                  if p.is_dir() and p.name not in ("extra-seeds", "sgl-tcl")) == BATTERIES


@pytest.mark.parametrize("battery", BATTERIES)
def test_one_fixture_per_test(battery):
    """The fixture count equals the battery's test count (read in Python, not Chez)."""
    names = scheme_forms.test_names(capture.battery_path(battery).read_text())
    assert list(manifest(battery)) == names
    files = sorted(p.name for p in (FIXTURES / battery).glob("*.txt"))
    assert files == sorted(capture.file_name(i, n) for i, n in enumerate(names))


@pytest.mark.parametrize("battery", BATTERIES)
def test_split_join_round_trip(battery):
    names = list(manifest(battery))
    vals = [values(battery)[n].encode() for n in names]
    raw = capture.join(names, vals)
    assert capture.split(raw, names) == vals


def test_split_multiline_values():
    raw = b"a => (1\n 2)\nb? => ERROR\nc => x\ny => z\n"
    assert capture.split(raw, ["a", "b?", "c"]) == [b"(1\n 2)", b"ERROR", b"x\ny => z"]
    with pytest.raises(capture.CaptureError):
        capture.split(raw + b"junk", ["a", "b?", "c"])
    with pytest.raises(capture.CaptureError):
        capture.split(raw, ["a", "b?", "d"])


def test_file_names():
    assert capture.file_name(5, "prob?") == "005-prob%3F.txt"
    assert capture.file_name(91, "~") == "091-%7E.txt"
    assert capture.file_name(0, "number->string-exact") == "000-number-%3Estring-exact.txt"


def test_battery_names():
    assert capture.battery_name("utilities") == "utilities"
    assert capture.battery_name("sgl-battery") == "sgl"
    assert capture.battery_name("tests/diff/rule-battery.scm") == "rule"
    with pytest.raises(capture.CaptureError):
        capture.battery_name("nonesuch")


def test_scheme_forms_reader():
    text = '; c\n#| x #| y |# |#\n(test a "q\\")" #\\( #;(test z 1))\n#;(test b 2)\n(define x 1)\n(test c [1 #(2)] \'(3))'
    assert scheme_forms.test_names(text) == ["a", "c"]


@pytest.mark.parametrize("battery", BATTERIES)
def test_sources_unchanged(battery):
    """No input of the capture changed since the fixture was made."""
    recorded = (FIXTURES / battery / "SOURCES").read_text().splitlines()
    assert recorded[0].startswith("chez ")
    current = capture.sources(battery, recorded[0][len("chez "):]).splitlines()
    assert current == recorded


def test_fixture_lookup():
    """A known value, as a sanity check on the loader."""
    assert chez("utilities", "~").startswith("((((0 . (0 . (0 . (0 . ()))))")
    assert all("\n" not in n for n in manifest("utilities"))


@pytest.mark.slow
def test_recapture_is_byte_identical(tmp_path):
    """Freshness: capturing every battery again under Chez gives the same files."""
    counts = capture.capture_all(BATTERIES, tmp_path)
    assert counts == {b: len(manifest(b)) for b in BATTERIES}
    for battery in BATTERIES:
        ours, fresh = FIXTURES / battery, tmp_path / battery
        assert sorted(p.name for p in ours.iterdir()) == sorted(p.name for p in fresh.iterdir())
        match, mismatch, errors = filecmp.cmpfiles(
            ours, fresh, [p.name for p in ours.iterdir()], shallow=False
        )
        assert (mismatch, errors) == ([], []), battery
