"""Loop0006 item 15: the palette and editor's model (`tabletop/gui/models/palette.py`;
G-050, G-051), Qt-free (PANES.md P7's tests).

- A table built by the model validates (`problems.validate` returns nothing), becomes a
  `Problem` through `problems.problem_from_dict`, round-trips through Save and Load (the
  JSON file, every object, the touch, the answers, the names) and runs, the same seed
  giving the same run.
- An invalid table (empty; no touch; no answer) shows `validate`'s own messages with the
  GUI's hints, and `problem()` refuses it.
- The counts equal the table's, every type listed in the palette's order.
- A library table loaded into the editor keeps its objects, touch and answers, names the
  library problem in `based_on`, and run again gives the library run's answer, structure
  and codelets.
- `problems/` is untouched after Save, and a path inside it is refused.
- Editing: add (the next free id; clamped inside the table), move, rotate (silverware
  only, the footprint turning with it), delete (the touch and the answers closing up),
  touch, toggle an answer, the names and the family; every change refused while
  `enabled` is False; the listeners called.
- The realism faults [F95 p. 38]: an overlap and an object between the bands are said,
  and such a table can still run.
- The file's boxes give the positions back exactly (`position_from_box`).
"""

import hashlib
import json
from pathlib import Path

import pytest

from tabletop import objects as O
from tabletop import problems as P
from tabletop.config import PROFILES
from tabletop.gui.models import palette as pm
from tabletop.run import report_line, run_problem
from golden import V2, V2_PROFILES


def built_table() -> pm.TableEditor:
    """A small table: a cup and a fork in Henry's row, a cup and a spoon in Eliza's,
    the cup in front of Henry touched, the cup in front of Eliza the expected answer."""
    e = pm.TableEditor()
    e.add("cup", 30, 15)
    e.add("fork", 20, 15)
    e.add("cup", 80, 82)
    e.add("spoon", 90, 82)
    e.touch("cup1")
    e.toggle_answer("cup2")
    return e


def library_hashes() -> dict:
    return {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(P.PROBLEMS_DIR.iterdir())}


def test_a_built_table_validates_becomes_a_problem_and_runs():
    e = built_table()
    data = e.to_dict()
    assert P.validate(data) == [] and e.messages() == () and e.status().runnable
    problem = e.problem()
    assert isinstance(problem, P.Problem)
    assert list(problem.ids()) == ["cup1", "fork1", "cup2", "spoon1"]
    assert problem.touched == "cup1" and problem.answers == {"E1": "cup2"}
    assert problem.object("fork1").orientation == "up" and problem.object("cup1").orientation is None
    assert problem.name == "edited" and problem.family in P.FAMILIES
    # The provenance says the table was set in the GUI, in words (G-050).
    assert "GUI" in problem.source["figure"] and "GUI" in problem.method
    assert all("GUI" in o.provenance.figure and "pdfimages" in o.provenance.image
               for o in problem.objects)
    a = run_problem(problem, V2_PROFILES["standard"], 1)
    b = run_problem(e.problem(), V2_PROFILES["standard"], 1)
    assert a == b and a.codelets > 0 and report_line(a).startswith("Eliza touches")


def test_a_built_table_round_trips_through_save_and_load(tmp_path):
    e = built_table()
    e.set_names("mytable", "My table", "blockage")
    path = e.save(tmp_path / "mytable.json")
    data = json.loads(Path(path).read_text(encoding="utf-8"))
    assert P.validate(data) == [] and data["name"] == "mytable" and data["family"] == "blockage"
    assert data["title"] == "My table" and [k for k in data][:3] == ["name", "title", "family"]
    other = pm.TableEditor()
    other.load_file(path)
    assert other.objects == e.objects and other.touched == e.touched
    assert other.answers == e.answers and (other.name, other.title, other.family) == \
        ("mytable", "My table", "blockage")
    assert other.to_dict() == e.to_dict() and other.problem() == e.problem()


def test_the_boxes_give_the_positions_back_exactly():
    e = built_table()
    e.add("knife", 33.3, 47.7, "left")
    e.add("soup bowl", 12.7, 88.1)
    data = e.to_dict()
    table = data["table"]
    for o in data["objects"]:
        assert P.position_from_box(o["box_px"], table["frame_px"], table["width"],
                                   table["height"]) == (o["x"], o["y"])
        assert table["frame_px"][0] < o["box_px"][0] and o["box_px"][2] < table["frame_px"][2]


def test_an_invalid_table_shows_validates_messages_and_cannot_run():
    e = pm.TableEditor()
    messages = e.messages()
    assert "no objects" in messages and "no answers" in messages
    assert any("touched" in m for m in messages)
    assert any("drag an object" in m for m in messages)          # the GUI's hints (G-051)
    assert e.status().text == "0 objects; cannot run: no objects"
    with pytest.raises(ValueError, match="cannot run"):
        e.problem()
    e.add("cup", 30, 15)
    assert "no objects" not in e.messages() and not e.status().runnable
    e.touch("cup1")
    assert e.messages() == ("no answers", "  " + pm.HINTS["no answers"])
    e.toggle_answer("cup1")
    assert e.messages() == () and e.status().runnable
    assert e.status().text == "1 object; can run; a table French would set"


def test_the_counts_equal_the_tables():
    e = pm.TableEditor()
    assert list(e.counts()) == list(O.TYPES) and set(e.counts().values()) == {0}
    e.load(P.load_problem("surround-05"))
    counts = e.counts()
    assert counts == {"fork": 4, "knife": 2, "spoon": 4, "cup": 2, "saucer": 0, "plate": 0,
                      "big glass": 0, "small glass": 1, "pepper shaker": 0, "salt shaker": 0,
                      "soup bowl": 0}
    assert sum(counts.values()) == 13 == len(e.objects)
    e.delete("fork1")
    e.add("plate", 55, 50)
    assert e.counts()["fork"] == 3 and e.counts()["plate"] == 1


def test_a_library_table_loaded_and_run_again_gives_the_library_run():
    library = P.load_problem("surround-05")
    e = pm.TableEditor()
    e.load(library)
    assert [(o.id, o.type, o.x, o.y, o.orientation) for o in e.objects] == \
        [(o.id, o.type, o.x, o.y, o.orientation) for o in library.objects]
    assert e.touched == library.touched and e.answers == library.answers
    assert e.based_on == "surround-05" and e.name == "surround-05" and e.family == "surround"
    assert e.to_dict()["based_on"] == "surround-05" and e.faults() == ()
    ours = run_problem(e.problem(), V2_PROFILES["standard"], 3)
    theirs = run_problem(library, V2_PROFILES["standard"], 3)
    assert (ours.answer, ours.label, ours.structure_value, ours.codelets) == \
        (theirs.answer, theirs.label, theirs.structure_value, theirs.codelets)
    assert report_line(ours) == "Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6."   # surround-05 seed 3 on the v2 defaults since v5's fix, D-116 to D-120 (cup2 (E2), structure 75, 44 codelets, T 41.0 since item 08)
    # An edited table saved and loaded keeps the library problem it came from.
    e.move("cup1", 40, 15)
    other = pm.TableEditor()
    other.load(e.problem())
    assert other.based_on == "surround-05" and other.object("cup1").x == 40


def test_the_library_is_untouched_after_save_and_refused_as_a_target(tmp_path):
    before = library_hashes()
    e = built_table()
    e.save(tmp_path / "t.json")
    with pytest.raises(ValueError, match="library"):
        e.save(P.PROBLEMS_DIR / "zz-edited.json")
    with pytest.raises(ValueError, match="library"):
        e.save(P.PROBLEMS_DIR / "sub" / "t.json")
    assert library_hashes() == before
    assert pm.under_problems_dir(P.PROBLEMS_DIR / "surround-05.json")
    assert not pm.under_problems_dir(tmp_path / "surround-05.json")
    assert pm.problem_dir_of(None) == str(P.PROBLEMS_DIR)
    assert pm.problem_dir_of(tmp_path / "t.json") == str(tmp_path)


def test_editing_adds_moves_rotates_deletes_touches_and_marks():
    e = pm.TableEditor()
    seen = []
    e.listeners.append(lambda: seen.append(len(e.objects)))
    assert e.add("fork", 20, 15) == "fork1" and e.add("fork", 25, 15) == "fork2"
    e.delete("fork1")
    assert e.add("fork", 20, 15) == "fork1"                 # the smallest free number
    assert e.add("small glass", 50, 15) == "small-glass1"
    assert seen == [1, 2, 1, 2, 3]
    # Clamped inside the table, to a tenth (G-050).
    e.add("plate", -5, 200)
    plate = e.object("plate1")
    w, h = plate.footprint
    assert plate.x == pytest.approx(w / 2 + pm.EDGE, abs=0.05)
    assert plate.y == pytest.approx(100 - h / 2 - pm.EDGE, abs=0.05)
    assert e.move("plate1", 55.57, 50.04) == e.object("plate1")
    assert (e.object("plate1").x, e.object("plate1").y) == (55.6, 50.0)
    # Rotate: silverware only, the footprint turning with it.
    assert e.object("fork1").footprint == O.footprint("fork", "up")
    e.rotate("fork1")
    assert e.object("fork1").orientation == "down"
    e.rotate("fork1")
    assert e.object("fork1").orientation == "left"
    assert e.object("fork1").footprint == O.footprint("fork", "left")
    e.rotate("plate1")
    assert e.object("plate1").orientation is None
    # Touch and answers.
    e.touch("fork1")
    e.toggle_answer("fork2")
    e.toggle_answer("plate1")
    assert e.touched == "fork1" and e.answers == {"E1": "fork2", "E2": "plate1"}
    assert e.label_of("plate1") == "E2" and e.label_of("fork1") == ""
    e.toggle_answer("fork2")
    assert e.answers == {"E1": "plate1"}                   # the labels close up
    e.toggle_answer("fork2")
    e.delete("plate1")
    assert e.answers == {"E1": "fork2"}
    e.delete("fork1")
    assert e.touched is None
    with pytest.raises(KeyError):
        e.move("nobody", 1, 1)
    with pytest.raises(ValueError):
        e.add("teapot", 1, 1)
    with pytest.raises(ValueError):
        e.set_names(family="ch2")
    e.set_names("", "", "buridan")
    assert (e.name, e.family) == ("edited", "buridan") and e.title.startswith("Edited table")
    e.clear()
    assert e.objects == [] and e.touched is None and e.answers == {} and e.family == "surround"


def test_every_change_is_refused_while_a_run_is_live():
    e = built_table()
    before = e.to_dict()
    e.enabled = False
    for action in (lambda: e.add("cup", 50, 50), lambda: e.move("cup1", 50, 50),
                   lambda: e.rotate("fork1"), lambda: e.delete("cup1"),
                   lambda: e.touch("cup2"), lambda: e.toggle_answer("cup1"),
                   lambda: e.set_names("x"), e.clear,
                   lambda: e.load(P.load_problem("surround-05"))):
        with pytest.raises(ValueError, match="live"):
            action()
    assert e.to_dict() == before
    e.enabled = True
    e.clear()
    assert e.objects == []


def test_the_realism_faults_are_said_and_such_a_table_still_runs():
    e = built_table()
    e.add("cup", 31, 16)                          # on top of cup1
    e.add("saucer", 55, 32)                       # between Henry's row and the middle band
    faults = e.faults()
    assert "cup1 and cup3 overlap" in faults
    assert "saucer1 lies between the rows and the middle band" in faults
    status = e.status()
    assert status.runnable and status.faults == faults
    assert status.text.startswith("6 objects; can run, but not a table French would set:")
    assert run_problem(e.problem(), V2_PROFILES["standard"], 2).codelets > 0
    # The hover text and the drawn forms (G-051).
    assert "touched by Henry (H)" in e.hover_text("cup1")
    assert "expected answer E1" in e.hover_text("cup2") and e.hover_text("nobody") == ""
    assert [a.letter for a in e.arrows()] == ["H", "E"]
    assert {o.id for o in e.placed()} == {o.id for o in e.objects}


def test_a_bad_file_is_refused(tmp_path):
    e = pm.TableEditor()
    bad = tmp_path / "bad.json"
    bad.write_text("not json", encoding="utf-8")
    with pytest.raises(ValueError, match="not a problem file"):
        e.load_file(bad)
    bad.write_text(json.dumps({"name": "x"}), encoding="utf-8")
    with pytest.raises(ValueError, match="missing key"):
        e.load_file(bad)
    bad.write_text("[]", encoding="utf-8")
    with pytest.raises(ValueError, match="not a problem file"):
        e.load_file(bad)
    assert e.objects == []
    e.load_file(P.PROBLEMS_DIR / "blockage-06.json")      # the library's files load
    assert e.based_on == "blockage-06" and len(e.objects) == len(P.load_problem("blockage-06").objects)
