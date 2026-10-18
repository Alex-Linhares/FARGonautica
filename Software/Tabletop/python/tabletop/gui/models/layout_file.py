"""The default layout file, `tabletop/gui/default_layout.json`: a dock arrangement that
travels with the code (ANSWERS.md 2; G-010; Seek-Whence GUI, `layout_file.py`). Qt-free.

`QSettings` remembers a user's layout per user and per machine (Numbo GUI,
`main_window.py`). The author wants a default that forks and clones get too, so it is a
small text file inside the package, committed with the code: the window's
`QMainWindow.saveState()` bytes as base64 (the arrangement Qt restores), beside a plain,
readable record of each dock's visibility, area and tab group, the format name and
version, and the Qt version that wrote it. No window geometry: screens differ between
machines. This module reads and writes that file and knows nothing of Qt; the window
turns the record into `restoreState` and back. A file that is missing, unreadable, of
another format or version, or that names a dock the window no longer has makes the
window fall back to the next layout (the built-in one) and say so; it never crashes.
"""

import base64
import binascii
import dataclasses
import json
from pathlib import Path

__all__ = ["DEFAULT_PATH", "FORMAT", "MAX_BYTES", "VERSION", "DockPlacement", "LayoutError",
           "LayoutRecord", "decode", "encode", "missing_docks", "read_layout", "write_layout"]

FORMAT = "tabletop-gui-layout"
VERSION = 1
# The shipped file, inside the package (ANSWERS.md 2).
DEFAULT_PATH = Path(__file__).resolve().parent.parent / "default_layout.json"
# ANSWERS.md 2 and iterations.md item 02: the file stays under 20 KB.
MAX_BYTES = 20_000


class LayoutError(ValueError):
    """The file is not a layout this GUI can use: wrong format, version or shape
    (G-010)."""


@dataclasses.dataclass(frozen=True)
class DockPlacement:
    """Where one dock is, readably: its `objectName`, whether it is shown, its `area`
    (`left`, `right`, `top`, `bottom`, `floating`) and the names of the docks it is tabbed
    with (G-010)."""
    name: str
    visible: bool
    area: str
    tabbed_with: tuple = ()


@dataclasses.dataclass(frozen=True)
class LayoutRecord:
    """A layout as the file holds it (G-010): the format `version`, the `qt` version that
    wrote it, the `state` bytes of `QMainWindow.saveState()`, the `docks` placements and
    the `toolbars` as (name, visible) pairs."""
    version: int
    qt: str
    state: bytes
    docks: tuple
    toolbars: tuple = ()


def encode(record: LayoutRecord) -> str:
    """The file's text for a record: JSON, the state as base64, indented so that a diff
    of the committed file reads (G-010)."""
    data = {"format": FORMAT, "version": record.version, "qt": record.qt,
            "docks": [dataclasses.asdict(d) | {"tabbed_with": list(d.tabbed_with)}
                      for d in record.docks],
            "toolbars": [{"name": name, "visible": bool(visible)}
                         for name, visible in record.toolbars],
            "state": base64.b64encode(bytes(record.state)).decode("ascii")}
    return json.dumps(data, indent=1) + "\n"


def decode(text: str) -> LayoutRecord:
    """The record in the file's text; `LayoutError` if it is not one (G-010)."""
    try:
        data = json.loads(text)
    except ValueError as error:
        raise LayoutError(f"not JSON: {error}") from None
    if not isinstance(data, dict) or data.get("format") != FORMAT:
        raise LayoutError(f"not a {FORMAT} file")
    version = data.get("version")
    if version != VERSION:
        raise LayoutError(f"layout format version {version!r}, this GUI reads {VERSION}")
    try:
        state = base64.b64decode(data["state"], validate=True)
        docks = tuple(DockPlacement(str(d["name"]), bool(d["visible"]), str(d["area"]),
                                    tuple(str(n) for n in d.get("tabbed_with", ())))
                      for d in data["docks"])
        toolbars = tuple((str(t["name"]), bool(t["visible"])) for t in data.get("toolbars", ()))
    except (KeyError, TypeError, ValueError, binascii.Error) as error:
        raise LayoutError(f"malformed layout: {error!r}") from None
    if not state:
        raise LayoutError("the layout has no state")
    return LayoutRecord(version, str(data.get("qt", "")), state, docks, toolbars)


def missing_docks(record: LayoutRecord, known) -> tuple:
    """The dock names of the record that the window does not have (G-010)."""
    known = set(known)
    return tuple(d.name for d in record.docks if d.name not in known)


def read_layout(path=DEFAULT_PATH):
    """The record in the file at `path`, or None if there is no file; `LayoutError` if it
    cannot be read or is not a layout (G-010)."""
    path = Path(path)
    if not path.exists():
        return None
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError) as error:
        raise LayoutError(f"cannot read {path.name}: {error}") from None
    return decode(text)


def write_layout(path, record: LayoutRecord) -> int:
    """Write the record to `path`; returns the size written. `OSError` if the place is not
    writable (the window then offers another place, ANSWERS.md 2) (G-010)."""
    text = encode(record)
    if len(text.encode("utf-8")) > MAX_BYTES:
        raise LayoutError(f"the layout is over {MAX_BYTES} bytes")
    Path(path).write_text(text, encoding="utf-8")
    return len(text)
