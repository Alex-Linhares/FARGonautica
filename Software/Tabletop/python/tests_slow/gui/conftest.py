"""The GUI's fixtures for the slow suite (ANSWERS.md D2): the same `window`,
`module_window`, `thread_errors`, `ini` and `golden_runs` as `tests/gui/conftest.py`,
re-exported from that file (loaded by path: a second module named `conftest` cannot be
imported by name), so that the two suites share one definition of each fixture and the
offscreen setting (G-002)."""

import importlib.util
from pathlib import Path

_SHARED = Path(__file__).resolve().parents[2] / "tests" / "gui" / "conftest.py"
_spec = importlib.util.spec_from_file_location("tests_gui_conftest", _SHARED)
_module = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_module)
globals().update({name: value for name, value in vars(_module).items()
                  if not name.startswith("__")})
