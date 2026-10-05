"""python3 -m metacat.gui [SCALE]: Metacat's windows and control panel.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
Type a problem such as "abc abd xyz" (optionally an answer and a seed: "abc abd
xyz 7") and press Enter, then Go or Step; or pick a run from the Demos menu.
"""
import sys

from metacat.gui import app

sys.exit(app.main(sys.argv[1:]))
