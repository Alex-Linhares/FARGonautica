"""python3 -m melanies_opengl INITIAL MODIFIED TARGET [--seed N] [--every N] [--sbcl PATH]
                              [--speed N] [--record FILE] [--replay FILE] [--no-gui]"""

import argparse
import sys

from . import lisprun


def main(argv=None):
    p = argparse.ArgumentParser(prog="python3 -m melanies_opengl",
                                description="Run Melanie Mitchell's original Copycat (SBCL) "
                                            "and watch it in Hager's OpenGL GUI.")
    p.add_argument("initial", nargs="?", default="abc")
    p.add_argument("modified", nargs="?", default="abd")
    p.add_argument("target", nargs="?", default="ijk")
    p.add_argument("--seed", type=int, default=None, help="SBCL random seed; none = from the clock")
    p.add_argument("--every", type=int, default=1, help="one trace line every N codelets")
    p.add_argument("--sbcl", default=None, help="path to sbcl (default: the one on PATH)")
    p.add_argument("--speed", type=float, default=None, help="codelets per second in the GUI (Hager's default 15)")
    p.add_argument("--record", default=None, help="save the trace (JSON lines) to this file")
    p.add_argument("--replay", default=None, help="replay a recorded trace instead of running SBCL")
    p.add_argument("--no-gui", action="store_true", help="no window: run (or replay) and print the summary")
    args = p.parse_args(argv)

    if args.replay:
        run = lisprun.replay_run(args.replay)
    elif args.no_gui:
        run = lisprun.blocking_run(args.initial, args.modified, args.target, args.seed,
                                   args.every, args.sbcl, args.record)
    else:
        run = lisprun.live_run(args.initial, args.modified, args.target, args.seed,
                               args.every, args.sbcl, args.record)

    if args.no_gui:
        run.run_to_end()
        if getattr(run, "lisp", None) is not None:
            run.lisp.wait()
        ws = run.workspace
        print("Problem: %s -> %s :: %s -> ?%s" % tuple(run.problem + [
            "  (seed %s)" % run.seed if run.seed is not None else ""]))
        print("Rule: %s" % (ws.rule.to_string() if ws.rule else "none"))
        print("Translated rule: %s" % (ws.translated_rule.to_string() if ws.translated_rule else "none"))
        print("Answer: %s" % (run.answer or "none"))
        print("Temperature: %s" % ws.temperature)
        print("Codelets run: %d" % run.coderack.time)
        print("Trace lines: %d" % run.lines)
        return 0 if run.answer else 2

    from . import gui
    gui.show(run, args.speed)
    return 0


if __name__ == "__main__":
    sys.exit(main())
