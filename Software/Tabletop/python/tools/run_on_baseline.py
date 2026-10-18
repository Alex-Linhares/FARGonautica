#!/usr/bin/env python3
"""Run a bench against the engine of another commit, from a throwaway worktree (D-102).

    python3 tools/run_on_baseline.py COMMIT WORKTREE BENCH [--runs N] [--seed S] [--profile P]
        [--problems NAME ...] [--json FILE] [--report FILE] [--worktree-bench]
        [--engine compiled|python] [--quiet]

"Before roles" is a commit, not a flag (D-078): a comparison with the engine as it stood
before the roles re-runs that commit. This tool adds a detached worktree of COMMIT at
WORKTREE (a path outside the repository, `/tmp/tabletop-v1-...`), runs the named bench
against the worktree's `tabletop` package, writes the bench's JSON records to `--json` and
its tables to `--report` (stdout when not given), and removes the worktree again, whether
or not the bench succeeded. The caller names the commit and the path: nothing here does.

Which bench module runs: by default the *current* tree's `tabletop/bench/BENCH.py`, loaded
by file into the worktree's `tabletop.bench` package, so that a bench written after the
commit (the variants bench of D-102) measures the old engine; with `--worktree-bench`, the
worktree's own module, for a bench the commit already had. Either way the engine is the
worktree's: the subprocess puts the worktree's `python/` first on `PYTHONPATH`, runs from
there, and refuses to go on if `tabletop` resolves anywhere else. The package is found
inside the worktree at this folder's path relative to the git top level, so the tool works
both in FARGonautica (`Software/Tabletop/python/`) and in the standalone repository
(`python/`). Exit status: 0 when the bench ran, 1 when the worktree could not be made or the
bench failed, 2 on a usage error.

The worktree's engine (D-107): a fresh worktree has no build of the compiled engine, so when
the compiled engine is wanted (the default, or `--engine compiled`, which also overrides an
inherited request) and the commit has `tools/build_fast.py`, the tool builds it there first
(the worktree's own build tool, at most 4 jobs; Cython as that tool finds it) and stops with
the build's message if it cannot, never running the pure engine in its place; a commit before
the compiled engine has only the plain package, the pure engine, and the tool says so. With
`--engine python` the inner process is started with the explicit request, the one way the
pure engine runs. The bench's records carry the engine that ran (`accel.info()`).
"""

import argparse
import importlib
import importlib.util
import os
import subprocess
import sys
from pathlib import Path

PYTHON_DIR = Path(__file__).resolve().parents[1]
BENCH_DIR = PYTHON_DIR / "tabletop" / "bench"
USAGE_ERROR = 2


def parser() -> argparse.ArgumentParser:
    """The options (D-102)."""
    p = argparse.ArgumentParser(prog="python3 tools/run_on_baseline.py",
                                description="Run a bench against another commit's engine from "
                                            "a throwaway worktree.")
    p.add_argument("commit", help="the commit whose engine runs (e.g. the pre-roles baseline)")
    p.add_argument("worktree", help="where to put the detached worktree, outside the repository")
    p.add_argument("bench", help="the bench's name (tabletop/bench/NAME.py)")
    p.add_argument("--runs", type=int, default=100, help="runs per problem (default 100)")
    p.add_argument("--seed", type=int, default=1, help="the bench's seed (default 1)")
    p.add_argument("--profile", default="standard", help="the configuration profile")
    p.add_argument("--problems", nargs="+", metavar="NAME", help="only these problems")
    p.add_argument("--json", metavar="FILE", help="write the bench's JSON records here")
    p.add_argument("--report", metavar="FILE", help="write the bench's tables here (else stdout)")
    p.add_argument("--worktree-bench", action="store_true",
                   help="run the worktree's own bench module, not the current tree's")
    p.add_argument("--engine", choices=["compiled", "python"],
                   help="the worktree's engine (D-107): compiled, built there first (the "
                        "default, unless the environment requests python), or python, the "
                        "pure engine by explicit request")
    p.add_argument("--quiet", action="store_true", help="no progress lines on stderr")
    p.add_argument("--inner", action="store_true", help=argparse.SUPPRESS)
    p.add_argument("--module-file", help=argparse.SUPPRESS)
    p.add_argument("--package-dir", help=argparse.SUPPRESS)
    return p


def git(top: Path, *args: str) -> subprocess.CompletedProcess:
    """One git command at the repository's top level, output captured (D-102)."""
    return subprocess.run(["git", "-C", str(top), *args], capture_output=True, text=True)


def top_level(python_dir: Path = PYTHON_DIR) -> Path | None:
    """The git top level this folder is in, or None outside a repository (D-102)."""
    proc = subprocess.run(["git", "-C", str(python_dir), "rev-parse", "--show-toplevel"],
                          capture_output=True, text=True)
    return Path(proc.stdout.strip()).resolve() if proc.returncode == 0 else None


def package_dir(worktree: Path, top: Path, python_dir: Path = PYTHON_DIR) -> Path:
    """Where `python/` lies inside the worktree: at this folder's path relative to the git
    top level, in FARGonautica or in the standalone repository (D-102)."""
    return worktree / python_dir.resolve().relative_to(top)


def add_worktree(top: Path, commit: str, worktree: Path) -> str | None:
    """Add a detached worktree of the commit; the error text when git refuses (D-102)."""
    proc = git(top, "worktree", "add", "--detach", str(worktree), commit)
    return None if proc.returncode == 0 else (proc.stderr.strip() or "git worktree add failed")


def remove_worktree(top: Path, worktree: Path) -> None:
    """Remove the worktree, by force, and prune git's list of it (D-102)."""
    git(top, "worktree", "remove", "--force", str(worktree))
    git(top, "worktree", "prune")


def inner_command(args, package: Path, module_file: Path | None) -> list:
    """The subprocess's command line: this script again, in its inner mode (D-102)."""
    cmd = [sys.executable, str(Path(__file__).resolve()), args.commit, args.worktree, args.bench,
           "--inner", "--package-dir", str(package), "--runs", str(args.runs),
           "--seed", str(args.seed), "--profile", args.profile]
    if args.problems:
        cmd += ["--problems", *args.problems]
    if args.json:
        cmd += ["--json", str(Path(args.json).resolve())]
    if args.report:
        cmd += ["--report", str(Path(args.report).resolve())]
    if args.quiet:
        cmd.append("--quiet")
    if module_file is not None:
        cmd += ["--module-file", str(module_file)]
    return cmd


ENGINE_VARIABLE = "TABLETOP_ENGINE"
BUILD_JOBS = 4


def engine_wanted(args, environ) -> str | None:
    """`compiled` or `python`: the `--engine` option, else the environment's request, else
    compiled (D-107); None when the environment holds a value that names no engine."""
    if args.engine:
        return args.engine
    value = environ.get(ENGINE_VARIABLE, "")
    return "compiled" if value in ("", "compiled") else "python" if value == "python" else None


def prepare_engine(engine: str, package: Path, env: dict, quiet: bool) -> str | None:
    """Make the worktree's engine the one wanted (D-107, D-110): build the compiled
    engine with the worktree's own `tools/build_fast.py` when the commit has it (the
    error text when the build fails: no fallback), or note that a commit before the
    compiled engine has the plain package alone; for `python`, put the explicit request
    in the inner process's environment. Returns the error text, or None."""
    build_tool = package / "tools" / "build_fast.py"
    if engine == "python":
        env[ENGINE_VARIABLE] = "python"
        note = "the pure engine, by explicit request"
    else:
        env.pop(ENGINE_VARIABLE, None)
        if build_tool.is_file():
            proc = subprocess.run([sys.executable, str(build_tool), "--jobs", str(BUILD_JOBS)],
                                  cwd=package, env=env, capture_output=True, text=True)
            if proc.returncode != 0:
                return ("the compiled engine did not build in the worktree (no fallback to the "
                        "pure engine, D-107; pass --engine python to request it):\n"
                        + (proc.stdout + proc.stderr).strip())
            note = "the compiled engine, built in the worktree"
        else:
            note = "the plain package of a commit before the compiled engine (the pure engine)"
    if not quiet:
        print(f"run_on_baseline: engine: {note}", file=sys.stderr)
    return None


def engine_info() -> dict:
    """The worktree engine's `accel.info()` (D-107); a commit before the engine seam has
    no `tabletop.accel`, and its plain package is the pure engine."""
    if importlib.util.find_spec("tabletop.accel") is None:
        return {"engine": "python", "note": "a commit before the engine seam: the plain package"}
    from tabletop import accel
    return accel.info()


def run_inner(args) -> int:
    """The inner mode: with the worktree's `python/` first on PYTHONPATH, import its
    `tabletop`, load the bench module (the worktree's own, or the current tree's file into
    the worktree's `tabletop.bench` package), run it and write the records and tables
    (D-072, D-102)."""
    import tabletop   # noqa: E402  (resolved through PYTHONPATH: the worktree's package)
    found = Path(tabletop.__file__).resolve().parent
    expected = Path(args.package_dir).resolve() / "tabletop"
    if found != expected:
        print(f"run_on_baseline: tabletop resolved to {found}, not {expected}", file=sys.stderr)
        return 1
    import tabletop.bench  # noqa: E402
    from tabletop.bench.__main__ import progress_printer, write_json
    from tabletop.config import PROFILES
    if args.module_file:
        name = f"tabletop.bench.{args.bench}"
        spec = importlib.util.spec_from_file_location(name, args.module_file)
        module = importlib.util.module_from_spec(spec)
        sys.modules[name] = module
        spec.loader.exec_module(module)
    else:
        module = importlib.import_module(f"tabletop.bench.{args.bench}")
    total = module.planned(module.SETS, args.problems)
    results = module.run(PROFILES[args.profile], args.seed, args.runs, args.problems,
                         progress_printer(total, args.quiet))
    lines, records = module.report(results), module.records(results)
    for record in records:            # every record names the engine that ran it (D-107)
        record["engine"] = engine_info()
    if args.json:
        with open(args.json, "w", encoding="utf-8") as handle:
            write_json(records, handle)
    text = "\n".join(lines) + "\n"
    if args.report:
        Path(args.report).write_text(text, encoding="utf-8")
    else:
        sys.stdout.write(text)
    return 0


def main(argv=None) -> int:
    """Add the worktree, run the bench against it in a subprocess, remove the worktree
    (D-102)."""
    args = parser().parse_args(sys.argv[1:] if argv is None else list(argv))
    if args.inner:
        return run_inner(args)
    if args.runs < 1:
        print("run_on_baseline: --runs wants a positive count", file=sys.stderr)
        return USAGE_ERROR
    module_file = None if args.worktree_bench else BENCH_DIR / f"{args.bench}.py"
    if module_file is not None and not module_file.is_file():
        print(f"run_on_baseline: no bench module {module_file}", file=sys.stderr)
        return USAGE_ERROR
    top = top_level()
    if top is None:
        print("run_on_baseline: this folder is not inside a git repository", file=sys.stderr)
        return 1
    worktree = Path(args.worktree).resolve()
    if top in worktree.parents or worktree == top:
        print("run_on_baseline: the worktree must lie outside the repository", file=sys.stderr)
        return USAGE_ERROR
    error = add_worktree(top, args.commit, worktree)
    if error:
        print(f"run_on_baseline: {error}", file=sys.stderr)
        return 1
    try:
        package = package_dir(worktree, top)
        if not (package / "tabletop" / "__init__.py").is_file():
            print(f"run_on_baseline: no tabletop package at {package}", file=sys.stderr)
            return 1
        env = dict(os.environ)
        env["PYTHONPATH"] = str(package) + (os.pathsep + env["PYTHONPATH"]
                                           if env.get("PYTHONPATH") else "")
        env["PYTHONDONTWRITEBYTECODE"] = "1"
        engine = engine_wanted(args, os.environ)
        if engine is None:
            print(f"run_on_baseline: {ENGINE_VARIABLE}={os.environ[ENGINE_VARIABLE]!r} names "
                  "no engine: compiled or python (D-107)", file=sys.stderr)
            return USAGE_ERROR
        error = prepare_engine(engine, package, env, args.quiet)
        if error:
            print(f"run_on_baseline: {error}", file=sys.stderr)
            return 1
        proc = subprocess.run(inner_command(args, package, module_file), cwd=package, env=env)
        if proc.returncode != 0:
            print(f"run_on_baseline: the bench failed (exit {proc.returncode})", file=sys.stderr)
            return 1
        if not args.quiet:
            what = "its own" if args.worktree_bench else "the current tree's"
            print(f"run_on_baseline: {args.bench} ({what} module) ran on {args.commit} "
                  f"({package}); worktree removed", file=sys.stderr)
        return 0
    finally:
        remove_worktree(top, worktree)


if __name__ == "__main__":
    sys.exit(main())
