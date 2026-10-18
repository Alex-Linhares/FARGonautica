"""Build the compiled engine, `tabletop/_native/` (D-107, D-110).

    python3 setup.py build_ext --inplace --parallel 4      # what tools/build_fast.py runs
    pip install .                                          # the same extension, installed

The pure engine needs nothing to run (`pyproject.toml`: `dependencies = []`); the compiled
engine needs Cython and a C compiler to build. Cython: the `Cython` module of this Python,
else the executable named by `TABLETOP_CYTHON` (`tools/build_fast.py --cython PATH` sets it),
else a `cython` on PATH; it only generates C, which setuptools then compiles with the
interpreter's own compiler. Without either, the build stops with a message saying what is
missing and leaves no extension behind (never a half-built engine that `tabletop.accel`
would accept). The flags are the pinned ones of `tabletop/_native/__init__.py` (CFLAGS),
and the build stamp, the flags, the compiler and the Cython version are embedded in every
module as C string macros, which `tabletop.accel` reads and checks at start.
"""

import importlib.util
import os
import shutil
import subprocess
import sys
import sysconfig
from pathlib import Path

from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext
from setuptools.errors import CompileError, ExecError, LinkError

HERE = Path(__file__).resolve().parent
NATIVE = HERE / "tabletop" / "_native"
HOW_TO_BUILD = "then run `python3 tools/build_fast.py` from the python/ folder"
NO_CYTHON = ("Cython is not installed, so the compiled engine cannot be built: `pip install "
             "cython` (or point TABLETOP_CYTHON at a cython executable, or pass --cython PATH "
             f"to tools/build_fast.py), {HOW_TO_BUILD}.")
NO_COMPILER = ("no working C compiler, so the compiled engine cannot be built: install gcc or "
               f"clang (Debian/Ubuntu: `apt install build-essential`), {HOW_TO_BUILD}.")


def native_meta():
    """`tabletop/_native/__init__.py` loaded by file (CFLAGS, MODULES, stamp()), so that
    the build never imports the package it is building."""
    spec = importlib.util.spec_from_file_location("tabletop_native_meta", NATIVE / "__init__.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def artefacts() -> list:
    """What a build leaves under tabletop/_native/: generated C, annotations, extensions."""
    return sorted(p for p in NATIVE.iterdir() if p.suffix in (".c", ".html", ".so", ".pyd"))


def remove_artefacts() -> None:
    for p in artefacts():
        p.unlink()


def cython_executable():
    given = os.environ.get("TABLETOP_CYTHON")
    if given:
        return given if Path(given).is_file() and os.access(given, os.X_OK) else None
    return shutil.which("cython")


def generate_c(pyx_files: list) -> str:
    """Generate NAME.c beside every .pyx; returns the Cython version used."""
    try:
        import Cython
        from Cython.Build import cythonize
    except ImportError:
        exe = cython_executable()
        if not exe:
            raise SystemExit("tabletop: " + NO_CYTHON)
        version = subprocess.run([exe, "--version"], capture_output=True, text=True)
        version = (version.stdout + version.stderr).strip().split()[-1]
        for pyx in pyx_files:
            proc = subprocess.run([exe, "-3", "--fast-fail", "-o", str(pyx.with_suffix(".c")),
                                   str(pyx)], text=True)
            if proc.returncode != 0:
                raise SystemExit(f"tabletop: Cython failed on {pyx.name} (see above)")
        return version
    cythonize([str(p) for p in pyx_files], quiet=True, nthreads=1,
              compiler_directives={"language_level": "3"})
    return Cython.__version__


def compiler_name() -> str:
    cc = os.environ.get("CC") or sysconfig.get_config_var("CC") or "cc"
    exe = shutil.which(cc.split()[0])
    if not exe:
        return cc.split()[0] + " (not found)"
    proc = subprocess.run([exe, "--version"], capture_output=True, text=True)
    first = (proc.stdout or proc.stderr).strip().splitlines()[:1]
    return first[0] if first else cc


def extensions(meta, cython_version: str) -> list:
    """One Extension per module, from the generated C, with the pinned flags and the
    stamp, the flags, the compiler and the Cython version as string macros."""
    def macro(value: str) -> str:
        return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'
    macros = [("TABLETOP_BUILD_STAMP", macro(meta.stamp())),
              ("TABLETOP_BUILD_FLAGS", macro(" ".join(meta.CFLAGS))),
              ("TABLETOP_BUILD_COMPILER", macro(compiler_name())),
              ("TABLETOP_CYTHON_VERSION", macro(cython_version))]
    return [Extension(f"tabletop._native.{name}", [str(NATIVE / f"{name}.c")],
                      extra_compile_args=list(meta.CFLAGS), define_macros=macros)
            for name in meta.MODULES]


class BuildNative(build_ext):
    """build_ext that generates the C first, refuses to run without a compiler, and leaves
    no artefact behind when anything fails."""

    def run(self):
        meta = native_meta()
        remove_artefacts()
        cc = (os.environ.get("CC") or sysconfig.get_config_var("CC") or "cc").split()[0]
        if not shutil.which(cc):
            raise SystemExit(f"tabletop: {NO_COMPILER} (CC is {cc!r}, not found)")
        try:
            version = generate_c([NATIVE / f"{name}.pyx" for name in meta.MODULES])
            for ext, spec in zip(self.extensions, extensions(meta, version)):
                # fill in the Extension setup() declared, which setuptools has finalized
                ext.sources, ext.extra_compile_args = spec.sources, spec.extra_compile_args
                ext.define_macros = spec.define_macros
            super().run()
        except BaseException:
            remove_artefacts()
            raise
        for p in artefacts():         # the generated C is not needed once compiled
            if p.suffix in (".c", ".html"):
                p.unlink()

    def build_extension(self, ext):
        try:
            super().build_extension(ext)
        except (CompileError, LinkError, ExecError) as error:
            raise SystemExit(f"tabletop: {NO_COMPILER} ({error})") from error


if __name__ == "__main__" or "setuptools" in sys.modules:
    setup(cmdclass={"build_ext": BuildNative},
          ext_modules=[Extension(f"tabletop._native.{name}", [])
                       for name in native_meta().MODULES])
