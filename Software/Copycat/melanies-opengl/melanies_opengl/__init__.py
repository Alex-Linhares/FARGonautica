"""melanies_opengl: Melanie Mitchell's original Copycat (running on SBCL), displayed live in
Joseph Aaron Hager's pyglet (OpenGL) GUI.

The Lisp side (lisp/trace-emitter.lisp) writes one JSON line per codelet; this package reads
them (lisprun), rebuilds the workspace, slipnet and coderack as plain objects with the
attributes Hager's client reads (trace), and opens his window on them (gui)."""

import os

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
COPYCAT_DIR = os.path.dirname(ROOT)                       # Software/Copycat
MELANIES_COPYCAT = os.path.join(COPYCAT_DIR, "melanies-copycat")
COPYCAT_OPENGL = os.path.join(COPYCAT_DIR, "copycat-opengl")
EMITTER = os.path.join(ROOT, "lisp", "trace-emitter.lisp")
