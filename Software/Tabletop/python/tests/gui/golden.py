"""The four golden runs, plain, as the reference every GUI observer is compared with
(Numbo GUI, `test_run_controller.py`'s `reference`; Seek-Whence GUI, `tests/gui/golden.py`):
the cases of `tests/data/golden/ENGINE/fingerprints.json`, the set of the engine that
is running (D-107), the loop's gate's cases (G-009). Not a
test file: the test files and the conftest import it.
"""

import dataclasses
import importlib.util
import json
from pathlib import Path

from tabletop import accel
from tabletop.config import PROFILES
from tabletop.events import Collector, record_of
from tabletop.problems import load_problem
from tabletop.run import report_line, run_problem

# The v2 defaults (D-114), from tests/v2_defaults.py, loaded by its path.
_spec = importlib.util.spec_from_file_location(
    "v2_defaults", Path(__file__).resolve().parents[1] / "v2_defaults.py")
_v2 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_v2)
V2, V2_PROFILES = _v2.V2, _v2.V2_PROFILES

# The set of the engine that is running (D-107): tests/data/golden/{compiled,python}/.
GOLDEN = Path(__file__).resolve().parents[1] / "data" / "golden" / accel.info()["engine"] \
    / "fingerprints.json"


@dataclasses.dataclass(frozen=True)
class GoldenRun:
    """One run of the engine alone (`run_problem` with a `Collector`, D-065): the
    `problem` name, the `seed`, the `events`, their `records`, the `draws`, the `result`
    and French's `report` line."""
    problem: str
    seed: int
    events: tuple
    records: tuple
    draws: int
    result: object
    report: str


def golden_cases() -> list:
    """The (problem, seed) pairs of the golden fingerprints, in the file's order."""
    return [(key.split()[0], int(key.split()[1])) for key in json.loads(GOLDEN.read_text())]


def plain_run(problem: str, seed: int, profile="standard") -> GoldenRun:
    """A run of the engine alone, with a `Collector` as its only observer; `profile` is a
    profile's name or a `Config`."""
    log = Collector()
    config = PROFILES[profile] if isinstance(profile, str) else profile
    result = run_problem(load_problem(problem), config, seed, [log])
    return GoldenRun(problem, seed, tuple(log.events),
                     tuple(record_of(e) for e in log.events), result.draws, result,
                     report_line(result))
