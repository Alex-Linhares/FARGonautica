"""The compiled engine's sources (Cython), built in place by `python3 tools/build_fast.py`
or by `pip install .` (D-107, D-110).

This module is plain Python so that the build (`setup.py`) and the seam (`tabletop.accel`)
can read the pinned flags and work out the build stamp without the extension. Nothing here
imports an extension module: `tabletop.accel` alone does, and compares the stamp embedded in
the extension with `stamp()` before the engine starts, so a stale build refuses to run.

Each `.pyx` mirrors a module of the pure engine function for function (`rng.pyx` mirrors
`tabletop/rng.py`, `slipnet.pyx` mirrors `tabletop/slipnet.py`, `coderack.pyx` the Coderack's chances of
`tabletop/temperature.py`), with a comment at every signature naming the Python it
implements.
"""

import hashlib
from pathlib import Path

HERE = Path(__file__).resolve().parent

MODULES = ("rng", "slipnet", "coderack")
"""The extension modules, one per `.pyx`, each importable as `tabletop._native.NAME`."""

CFLAGS = ("-O2", "-ffp-contract=off")
"""The pinned C flags (D-110), added to the interpreter's own: `-O2`; `-ffp-contract=off`
so that no `a * b + c` is fused into an FMA, whatever the compiler's default; never
`-ffast-math` (the distances use `math.inf` and the arithmetic must stay IEEE) and never
`-march=native` (the goldens must not depend on one CPU family)."""

BUILD_COMMAND = "python3 tools/build_fast.py"
"""How to build the engine, run from the `python/` folder (named in every refusal)."""


def sources() -> list:
    """The Cython sources, `.pyx` and `.pxd`, in name order (D-110)."""
    return sorted(p for p in HERE.iterdir() if p.suffix in (".pyx", ".pxd"))


def stamp() -> str:
    """The build stamp (D-110): the SHA-256 of the sources' names and bytes and of the
    pinned flags, as the build embeds it in every extension module. A changed source or
    flag changes the stamp, and `tabletop.accel` then refuses the old build."""
    digest = hashlib.sha256()
    for path in sources():
        digest.update(path.name.encode("utf-8") + b"\0" + path.read_bytes() + b"\0")
    digest.update(" ".join(CFLAGS).encode("utf-8"))
    return digest.hexdigest()
