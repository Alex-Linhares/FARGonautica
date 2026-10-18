#!/usr/bin/env python3
"""Build the compiled engine in place (D-107, D-110).

    python3 tools/build_fast.py [--jobs N] [--cython PATH]

Run from the `python/` folder. It compiles `tabletop/_native/` with at most 4 jobs through
`setup.py build_ext --inplace` (so `pip install .` builds the very same extension), with the
pinned flags of `tabletop/_native/__init__.py` and the build stamp (a hash of the `.pyx` and
`.pxd` sources and the flags) embedded in the extension; then it starts the engine once to
check that `tabletop.accel` accepts the build. The Cython used: `--cython PATH` (a `cython`
executable), else `$TABLETOP_CYTHON`, else the `Cython` module of this Python. Exit 0 when
built; without Cython or a C compiler, a message says what is missing and how to get it, the
exit status is 2, and no half-built extension is left behind.
"""

import argparse
import json
import os
import shutil
import subprocess
import sys
import sysconfig
from pathlib import Path

HERE = Path(__file__).resolve().parent
PYTHON_DIR = HERE.parent
NATIVE = PYTHON_DIR / "tabletop" / "_native"
MAX_JOBS = 4
ARTEFACT_SUFFIXES = (".c", ".html", ".so", ".pyd")


def say(text: str) -> None:
    print(f"build_fast: {text}", flush=True)


def remove_artefacts() -> None:
    for p in NATIVE.iterdir():
        if p.suffix in ARTEFACT_SUFFIXES:
            p.unlink()
    shutil.rmtree(PYTHON_DIR / "build", ignore_errors=True)


def cython_source(given) -> tuple:
    """(how Cython is found, the TABLETOP_CYTHON value to pass, or None when none found)."""
    for origin, path in (("--cython", given), ("TABLETOP_CYTHON", os.environ.get("TABLETOP_CYTHON"))):
        if path:
            if Path(path).is_file() and os.access(path, os.X_OK):
                return f"the executable {path} ({origin})", path
            return f"no Cython: {origin} names {path!r}, which is not an executable", None
    probe = subprocess.run([sys.executable, "-c", "import Cython; print(Cython.__version__)"],
                           capture_output=True, text=True)
    if probe.returncode == 0:
        return f"the Cython module {probe.stdout.strip()} of {sys.executable}", ""
    found = shutil.which("cython")
    if found:
        return f"the executable {found} (on PATH)", found
    return "no Cython: neither --cython, nor TABLETOP_CYTHON, nor the Cython module, nor a " \
           "cython on PATH", None


def compiler() -> tuple:
    cc = (os.environ.get("CC") or sysconfig.get_config_var("CC") or "cc").split()[0]
    return cc, shutil.which(cc)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--jobs", type=int, default=MAX_JOBS,
                        help=f"parallel compile jobs, at most {MAX_JOBS} (default)")
    parser.add_argument("--cython", default=None, help="a cython executable to generate the C")
    args = parser.parse_args(argv)
    jobs = max(1, min(args.jobs, MAX_JOBS))
    if args.jobs > MAX_JOBS:
        say(f"--jobs {args.jobs} capped at {MAX_JOBS} (the machine is shared)")
    missing = []
    how, cython = cython_source(args.cython)
    if cython is None:
        missing.append(f"{how}; get it with `pip install cython` (in any virtual "
                       "environment: pass its bin/cython with --cython PATH)")
    cc, cc_path = compiler()
    if not cc_path:
        missing.append(f"no C compiler: {cc!r} is not on PATH; install gcc or clang "
                       "(Debian/Ubuntu: `apt install build-essential`)")
    if missing:
        for m in missing:
            say("cannot build the compiled engine: " + m)
        remove_artefacts()
        return 2
    say(f"Cython: {how}; compiler: {cc_path}; jobs: {jobs}")
    remove_artefacts()
    env = dict(os.environ)
    env.pop("TABLETOP_ENGINE", None)
    env["TABLETOP_CYTHON"] = cython
    cmd = [sys.executable, "setup.py", "build_ext", "--inplace", "--parallel", str(jobs)]
    proc = subprocess.run(cmd, cwd=PYTHON_DIR, env=env)
    if proc.returncode != 0:
        remove_artefacts()
        say(f"the build failed (exit {proc.returncode}); nothing was left behind")
        return proc.returncode or 1
    check = subprocess.run([sys.executable, "-c",
                            "import json; from tabletop import accel; print(json.dumps(accel.info()))"],
                           cwd=PYTHON_DIR, env=env, capture_output=True, text=True)
    if check.returncode != 0:
        remove_artefacts()
        say("the engine refused the fresh build; nothing was left behind:\n" + check.stderr)
        return 1
    info = json.loads(check.stdout.strip().splitlines()[-1])
    shutil.rmtree(PYTHON_DIR / "build", ignore_errors=True)
    say(f"built: engine {info.get('engine')}, stamp {str(info.get('build_stamp'))[:12]}, "
        f"flags {info.get('cflags')}, {info.get('compiler')}, Cython {info.get('cython')}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
