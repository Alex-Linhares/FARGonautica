"""The selection shared by every pane (item 04; `tabletop/gui/models/selection.py`;
G-022), Qt-free, after the Seek-Whence GUI's test:

- `select` holds the kind, the name and the members the selection implies; `includes`
  is true for the element and its members; `toggle` on the same element clears.
- Listeners are called after every change, once, with the selection; `clear` on an empty
  selection says nothing.
- The selection holds names only.
"""

from tabletop.gui.models.selection import KINDS, Selection


def test_select_includes_the_element_and_its_members():
    s = Selection()
    assert s.empty and s.kind is None and s.name is None and s.members == frozenset()
    s.select("group", "g1", ("fork1", "fork2", "knife1"))
    assert (s.kind, s.name) == ("group", "g1") and not s.empty
    assert s.members == {"g1", "fork1", "fork2", "knife1"}
    assert s.includes("g1") and s.includes("knife1") and not s.includes("cup1")
    s.select("object", "cup1")
    assert s.members == {"cup1"} and not s.includes("g1")


def test_toggle_and_clear():
    s = Selection()
    s.toggle("object", "cup1")
    assert s.name == "cup1"
    s.toggle("object", "cup1")
    assert s.empty and s.members == frozenset()
    s.toggle("object", "cup1")
    s.toggle("group", "cup1")            # another kind: a new selection, not a clear
    assert (s.kind, s.name) == ("group", "cup1")
    s.clear()
    assert s.empty


def test_listeners_hear_every_change_once():
    s = Selection()
    heard = []
    s.subscribe(lambda sel: heard.append((sel.kind, sel.name, sorted(sel.members))))
    s.clear()                            # nothing selected: nothing to say
    assert heard == []
    s.select("correspondence", "cp1", ("cup2", "cup1"))
    s.clear()
    s.clear()
    assert heard == [("correspondence", "cp1", ["cp1", "cup1", "cup2"]), (None, None, [])]


def test_the_selection_holds_names_only():
    s = Selection()
    s.select("proposal", "p1", ["fork4", "knife2"])
    assert all(isinstance(n, str) for n in s.members)
    assert isinstance(s.members, frozenset)
    assert KINDS == ("object", "group", "proposal", "correspondence", "node")
