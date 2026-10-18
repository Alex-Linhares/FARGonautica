"""Loop0004 item 06: the selection shared by every pane (`seekwhence/gui/models/selection.py`),
Qt-free (G-025).

- `select` holds the kind, the name and the members the selection implies; `includes`
  is true for the element and its members; `toggle` on the same element clears.
- Listeners are called after every change, once, with the selection; `clear` on an empty
  selection says nothing.
- The selection holds names only.
"""

from seekwhence.gui.models.selection import Selection


def test_select_includes_the_element_and_its_members():
    s = Selection()
    assert s.empty and s.kind is None and s.name is None and s.members == frozenset()
    s.select("glom", "glom7", ("glom6", "glint4", "glint5"))
    assert (s.kind, s.name) == ("glom", "glom7") and not s.empty
    assert s.members == {"glom7", "glom6", "glint4", "glint5"}
    assert s.includes("glom7") and s.includes("glint5") and not s.includes("glint1")
    s.select("glint", "glint1")
    assert s.members == {"glint1"} and not s.includes("glom7")


def test_toggle_and_clear():
    s = Selection()
    s.toggle("glint", "glint1")
    assert s.name == "glint1"
    s.toggle("glint", "glint1")
    assert s.empty and s.members == frozenset()
    s.toggle("glint", "glint1")
    s.toggle("glom", "glint1")           # another kind: a new selection, not a clear
    assert (s.kind, s.name) == ("glom", "glint1")
    s.clear()
    assert s.empty


def test_listeners_hear_every_change_once():
    s = Selection()
    heard = []
    s.subscribe(lambda sel: heard.append((sel.kind, sel.name, sorted(sel.members))))
    s.clear()                            # nothing selected: nothing to say
    assert heard == []
    s.select("bond", "bond1", ("glint1", "glint3"))
    s.clear()
    s.clear()
    assert heard == [("bond", "bond1", ["bond1", "glint1", "glint3"]), (None, None, [])]


def test_the_selection_holds_names_only():
    s = Selection()
    s.select("glom", "glom1", ["glint1", "glint2"])
    assert all(isinstance(n, str) for n in s.members)
    assert isinstance(s.members, frozenset)
