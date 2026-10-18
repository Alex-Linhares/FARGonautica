"""The slow tests of `tests/gui/test_tap.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_tap import (
    Collector,
    DENSE_CODELETS,
    KEYFRAME_STEPS,
    STANDARD,
    World,
    dataclasses,
    load_problem,
    pytest,
    tapped,
)

pytestmark = pytest.mark.slow


@pytest.mark.slow
def test_the_forced_capped_run_takes_keyframes_and_stays_the_same_run():
    """The measurement of G-007: `surround-05` seed 1 with the find-answer postings
    switched off (`answer_post_rate=0`, for the measurement only) reaches the cap of
    3000 codelets (D-061); the tap takes about one snapshot per 65 events (470 of its
    30,692 since v5's fix, D-116 to D-120; 25,263 in v4; 24,774 since item 06 (recounted at the fast-engine loop's item 06; 24,744 was stale); 25,753 at item 10; 24,291 at item 09; 24,389 at item 08; 24,638 at item 07; 48 per snapshot at item 05)."""
    forced = dataclasses.replace(STANDARD, answer_post_rate=0.0)
    plain = Collector()
    world = World(load_problem("surround-05"), forced, 1, observers=[plain])
    while not world.finished:
        world.step()
    log, result, _, _, _ = tapped("surround-05", 1, config=forced)
    assert result.capped and result.codelets == 3000
    assert tuple(log.events) == tuple(plain.events) and result.draws == world.rng.draws
    assert len(log.snapshots) < len(log.events) / 40
    assert len(log.snapshots) == 1 + DENSE_CODELETS + (3000 - DENSE_CODELETS) // KEYFRAME_STEPS + 1
