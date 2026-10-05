"""python3 -m metacat.qt: Metacat in one window (Qt).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
`--quit-after MS` closes the window and quits after MS milliseconds (for tests
and screenshots, with QT_QPA_PLATFORM=offscreen).
"""
import sys

from metacat.qt import app

sys.exit(app.main(sys.argv[1:]))
