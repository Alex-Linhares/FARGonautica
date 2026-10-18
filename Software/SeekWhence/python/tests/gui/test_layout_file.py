"""Loop0004 item 02: the default layout file codec (`seekwhence/gui/models/layout_file.py`),
Qt-free (ANSWERS.md 8; G-010).

- A record round-trips through the file's text; the text is JSON with the state as
  base64 and no geometry.
- Not JSON, another format, another version, a malformed dock, no state: `LayoutError`.
- `missing_docks` names the docks the window lacks; `read_layout` is None for no file and
  raises for a corrupt one; `write_layout` refuses a file over the size limit.
"""

import base64
import json

import pytest

from seekwhence.gui.models import layout_file as lf

STATE = bytes(range(256)) * 3


def record(docks=None, toolbars=(("timeline-toolbar", True),)):
    docks = docks if docks is not None else (
        lf.DockPlacement("controls-dock", True, "left"),
        lf.DockPlacement("hypothesis-dock", True, "right", ("reformulation-dock",)),
        lf.DockPlacement("config-dock", False, "right"))
    return lf.LayoutRecord(lf.VERSION, "6.11.2", STATE, tuple(docks), tuple(toolbars))


def test_a_record_round_trips_through_the_text():
    text = lf.encode(record())
    data = json.loads(text)
    assert data["format"] == lf.FORMAT and data["version"] == lf.VERSION
    assert base64.b64decode(data["state"]) == STATE
    assert "geometry" not in text
    assert data["docks"][1] == {"name": "hypothesis-dock", "visible": True, "area": "right",
                                "tabbed_with": ["reformulation-dock"]}
    assert lf.decode(text) == record()


@pytest.mark.parametrize("text, reason", [
    ("{not json", "not JSON"),
    (json.dumps([1, 2]), "not a"),
    (json.dumps({"format": "numbo-layout", "version": 1}), "not a"),
    (json.dumps({"format": lf.FORMAT, "version": 2, "state": "AAAA", "docks": []}), "version"),
    (json.dumps({"format": lf.FORMAT, "version": 1, "state": "AAAA", "docks": [{"name": "x"}]}),
     "malformed"),
    (json.dumps({"format": lf.FORMAT, "version": 1, "state": "!!!", "docks": []}), "malformed"),
    (json.dumps({"format": lf.FORMAT, "version": 1, "state": "", "docks": []}), "no state"),
    (json.dumps({"format": lf.FORMAT, "version": 1, "docks": []}), "malformed"),
])
def test_a_bad_text_raises_layout_error(text, reason):
    with pytest.raises(lf.LayoutError, match=reason):
        lf.decode(text)


def test_missing_docks_names_what_the_window_lacks():
    assert lf.missing_docks(record(), ["controls-dock", "hypothesis-dock", "config-dock"]) == ()
    assert lf.missing_docks(record(), ["controls-dock"]) == ("hypothesis-dock", "config-dock")


def test_read_and_write_a_file(tmp_path):
    path = tmp_path / "default_layout.json"
    assert lf.read_layout(path) is None
    size = lf.write_layout(path, record())
    assert path.stat().st_size == size < lf.MAX_BYTES
    assert lf.read_layout(path) == record()
    path.write_text("junk")
    with pytest.raises(lf.LayoutError):
        lf.read_layout(path)
    with pytest.raises(lf.LayoutError, match="over"):
        lf.write_layout(path, lf.LayoutRecord(lf.VERSION, "6", b"x" * lf.MAX_BYTES, ()))


def test_the_default_path_is_inside_the_gui_package():
    assert lf.DEFAULT_PATH.parent.name == "gui" and lf.DEFAULT_PATH.name == "default_layout.json"
    assert lf.DEFAULT_PATH.exists()
    assert lf.read_layout().docks            # the shipped file reads
