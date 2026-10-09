"""Load golden data produced by the Perl oracle (see oracle/regen.py)."""
import json
from pathlib import Path

GOLDEN_DIR = Path(__file__).resolve().parent / "golden"


def load(name):
    """Return the list of cases recorded by oracle/NAME.pl."""
    return json.loads((GOLDEN_DIR / f"{name}.json").read_text())
