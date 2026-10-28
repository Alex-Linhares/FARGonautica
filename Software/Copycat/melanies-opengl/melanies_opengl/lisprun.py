"""Running Mitchell's Copycat in SBCL as a subprocess, and replaying recorded traces."""

import os
import queue
import shutil
import subprocess
import sys
import threading

from . import MELANIES_COPYCAT, EMITTER
from .trace import TraceRun, parse_lines


def sbcl_command(initial, modified, target, seed=None, every=1, sbcl=None):
    sbcl = sbcl or shutil.which("sbcl")
    if not sbcl:
        raise RuntimeError("sbcl is not on PATH (pass --sbcl)")
    load = os.path.join(MELANIES_COPYCAT, "load.lisp")
    if not os.path.exists(load):
        raise RuntimeError("melanies-copycat/load.lisp is missing: %s" % load)
    form = '(copycat::mo-run "%s" "%s" "%s" :seed %s :every %d)' % (
        initial, modified, target, "nil" if seed is None else int(seed), int(every))
    return [sbcl, "--noinform", "--non-interactive", "--no-userinit", "--no-sysinit",
            "--load", load, "--load", EMITTER, "--eval", form]


class LispSource(object):
    """Starts SBCL and yields its JSON lines as they arrive. Iterating blocks until a line
    is available or the process ends; poll() returns None instead of blocking."""

    def __init__(self, initial, modified, target, seed=None, every=1, sbcl=None,
                 record=None, timeout=120.0):
        self.cmd = sbcl_command(initial, modified, target, seed, every, sbcl)
        self.timeout = timeout
        self.proc = subprocess.Popen(self.cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                     text=True, bufsize=1)
        self.queue = queue.Queue()
        self.record = open(record, "w") if record else None
        self.stderr = []
        self.done = False
        self.reader = threading.Thread(target=self._read, daemon=True)
        self.reader.start()
        threading.Thread(target=self._read_err, daemon=True).start()

    def wait(self, timeout=30.0):
        """Wait for SBCL to end and the reader to write every line (the record file in
        particular): without this, exiting right after the final line can lose the tail of
        a recording, since the reader thread is a daemon."""
        self.reader.join(timeout)
        try:
            self.proc.wait(timeout)
        except subprocess.TimeoutExpired:
            self.proc.kill()

    def _read(self):
        for text in self.proc.stdout:
            if self.record:
                self.record.write(text)
                self.record.flush()      # a recording must be complete even if we exit early
            text = text.strip()
            if text:
                self.queue.put(text)
        if self.record:
            self.record.close()
        self.queue.put(None)

    def _read_err(self):
        for text in self.proc.stderr:
            self.stderr.append(text)

    def __iter__(self):
        return self

    def __next__(self):
        if self.done:
            raise StopIteration
        text = self.queue.get(timeout=self.timeout)
        if text is None:
            self.done = True
            raise StopIteration
        import json
        return json.loads(text)

    def poll(self):
        """The next line if one is waiting, else None (StopIteration at the end)."""
        if self.done:
            raise StopIteration
        try:
            text = self.queue.get_nowait()
        except queue.Empty:
            return None
        if text is None:
            self.done = True
            raise StopIteration
        import json
        return json.loads(text)

    def close(self):
        if self.proc.poll() is None:
            self.proc.terminate()
            try:
                self.proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.proc.kill()


class LiveSource(object):
    """Wraps a LispSource so that the GUI's step() never blocks: the first line (init) is
    waited for, later ones are polled."""

    def __init__(self, lisp):
        self.lisp = lisp
        self.first = True

    def __iter__(self):
        return self

    def __next__(self):
        if self.first:
            self.first = False
            return next(self.lisp)
        return self.lisp.poll()


def live_run(initial, modified, target, seed=None, every=1, sbcl=None, record=None):
    """A TraceRun fed by a fresh SBCL process."""
    lisp = LispSource(initial, modified, target, seed, every, sbcl, record)
    try:
        run = TraceRun(LiveSource(lisp))
    except StopIteration:
        lisp.close()
        raise RuntimeError("SBCL produced no trace:\n" + "".join(lisp.stderr[-20:]))
    run.lisp = lisp
    return run


def replay_run(path):
    """A TraceRun fed by a recorded trace file."""
    return TraceRun(parse_lines(open(path)))


def blocking_run(initial, modified, target, seed=None, every=1, sbcl=None, record=None):
    """A TraceRun that reads every line as it comes (for tests and recording, no GUI)."""
    lisp = LispSource(initial, modified, target, seed, every, sbcl, record)
    run = TraceRun(lisp)
    run.lisp = lisp
    return run
