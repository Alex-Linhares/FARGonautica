"""The engine seam: the one place that chooses between the compiled engine and the pure one
(D-107). Read once at import from the environment variable `TABLETOP_ENGINE`:

- unset, empty or `compiled`: the compiled engine (`tabletop/_native/`), **required**. If it
  is not built, fails to import, or its embedded build stamp does not match the sources
  beside it (D-110), `EngineError` is raised here, at import, and every entry point fails at
  start with the build command. There is no fallback to the pure engine, ever.
- `python`: the pure engine (`tabletop/rng.py`, `tabletop/slipnet.py`), the specification.
  Every module of `tabletop/_native/` (`MODULES`) must import and carry the sources' stamp.
- anything else (`pure`, `auto`, `1`, `Python`): `EngineError`, so that a typo never selects
  an engine.

The rest of the package constructs `Rng` and `Slipnet`, derives batch seeds (`run_seeds`)
and works out the Coderack's chances (`chances_of`, the typed hot spot of D-112) through
this module and never imports `tabletop._native` itself; `info()` says which engine
is running (`ENGINE`, `compiled` or `python`), the generator's name, and for the compiled
engine its build stamp, compiler, Cython version and pinned flags.
"""

import os

from tabletop import _native as native

VARIABLE = "TABLETOP_ENGINE"
ENGINES = ("compiled", "python")


class EngineError(ImportError):
    """The engine cannot start: not built, stale, or misnamed (D-107, D-110)."""


def _not_built(reason: str) -> EngineError:
    return EngineError(f"the compiled Tabletop engine is not built ({reason}): run "
                       f"`{native.BUILD_COMMAND}` from the python/ folder, or set "
                       f"{VARIABLE}=python to run the pure engine explicitly")


_requested = os.environ.get(VARIABLE, "")
if _requested in ("", "compiled"):
    ENGINE = "compiled"
elif _requested == "python":
    ENGINE = "python"
else:
    raise EngineError(f"{VARIABLE}={_requested!r} names no engine: leave it unset (or set it to "
                      f"'compiled') for the compiled engine, or set it to 'python' for the pure "
                      f"engine; the legal values are {ENGINES}")

if ENGINE == "python":
    from tabletop.rng import Rng, run_seeds
    from tabletop.slipnet import Slipnet
    from tabletop.temperature import chances_of
    _INFO = {"engine": "python", "rng": "splitmix64"}
else:
    import importlib
    _loaded = {}
    for _name in native.MODULES:
        try:
            _loaded[_name] = importlib.import_module(f"tabletop._native.{_name}")
        except ImportError as error:
            raise _not_built(f"{type(error).__name__}: {error}") from error
        if _loaded[_name].BUILD_STAMP != native.stamp():
            raise _not_built(f"the build of {_name} is stale: its stamp "
                             f"{_loaded[_name].BUILD_STAMP[:12]} is not the sources' "
                             f"{native.stamp()[:12]}")
    _rng, _slipnet = _loaded["rng"], _loaded["slipnet"]
    Rng, run_seeds = _rng.Rng, _rng.run_seeds
    Slipnet = _slipnet.Slipnet                   # the native Slipnet (D-111)
    chances_of = _loaded["coderack"].chances_of  # the Coderack's chances in C (D-112)
    _INFO = {"engine": "compiled", "rng": "splitmix64", "build_stamp": _rng.BUILD_STAMP,
             "cflags": _rng.BUILD_FLAGS, "compiler": _rng.BUILD_COMPILER,
             "cython": _rng.CYTHON_VERSION, "modules": list(native.MODULES)}

def info() -> dict:
    """The engine that is running (D-107): `engine` (`compiled` or `python`), `rng` (the
    generator), and for the compiled engine `build_stamp`, `cflags`, `compiler`, `cython`
    and `modules`. A copy: the record is fixed at import."""
    return dict(_INFO)
