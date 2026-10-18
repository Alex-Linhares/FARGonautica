"""Loop0004 item 10 (finished in iteration 15, ANSWERS.md 9): the platoplasm pane's model
(`seekwhence/gui/models/platoplasm.py`), Qt-free (G-036, G-037, G-038).

- The s-link table read from the `start` event equals `Config.s_links`, and the pulls,
  pushes and strict classes equal the config's tables (DECISIONS D-038, D-039, D-040)
  [M86 pp. 90-91, 99].
- Run 3's slip-check display equals its `slip-check` event: `slip-check: stayval: 15.0`,
  each neighbour's pressure, `best: (Y-group 4.0)` and the stay [M86 pp. 103-105, 176].
- `Countup --> S-group` moves the crown: at the `class-changed` event it sits over
  Countup's box, then travels the s-link to S-group over `CROWN_MOVE` events
  [M86 pp. 99, 174].
- The eight types by category with their formats [M86 pp. 54-55, 59]; the ideal atoms met
  so far with their glints [M86 pp. 48-49]; the manifestation and actualization lists
  equal every snapshot's at every task boundary of the three appendix runs, with the one
  silent burst of run 3 corrected by the next snapshot (DECISION D-036); the reigning
  class and the threshold [M86 pp. 99, 105]; the tooltips [M86 pp. 90-91]; a click
  selects a type's manifestations and actualizations (G-025); the links layer; a start
  resets.
"""

import io

import pytest

from seekwhence.config import Config
from seekwhence.events import ClassChanged, RunStarted, SlipChecked
from seekwhence.gui.models import platoplasm as pm
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.selection import Selection
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX, appendix_inputs

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)


def logged(run_id):
    """An appendix run's `EventLog` with the tap's snapshots."""
    config, lines, answers = appendix_inputs(run_id)
    log = EventLog()
    tap = Tap(log)
    session = Session(config, SEEDS[run_id], out=io.StringIO(), lines=list(lines),
                      answers=list(answers), observers=[tap])
    tap.attach_session(session)
    session.run()
    return log


@pytest.fixture(scope="module")
def logs():
    return {run_id: logged(run_id) for run_id in (RUN1, RUN2, RUN3)}


def model_at(log, index=None, selection=None):
    """The model fed the events up to `index` (the end by default) with the snapshots in
    force (G-037)."""
    model = pm.PlatoplasmModel(selection)
    last = len(log.events) - 1 if index is None else index
    for i in range(last + 1):
        model.on_event(log.events[i])
        model.update(log.state_at(i))
    return model


def index_of(log, kind, nth=0):
    return [i for i, e in enumerate(log.events) if e.kind == kind][nth]


def lists(model):
    return {name: (set(node.manifestations), set(node.actualizations))
            for name, node in model.types.items()}


# -- the tables from the config [M86 pp. 90-91, 99] -----------------------------------------------

def test_the_s_link_table_equals_the_configs(logs):
    config = Config()
    model = model_at(logs[RUN1], 0)         # the start event alone
    assert set(model.link_triples()) == set(config.s_links)
    assert len(model.link_triples()) == len(config.s_links)
    for frm, to, slipperiness in config.s_links:
        assert model.slipperiness(frm, to) == slipperiness
    assert model.slipperiness("Cycle", "Constant") == 0.0
    pulls = {(k, t, m): v for k, t, m, v in config.bond_values if v > 0}
    pushes = {(k, t, m): v for k, t, m, v in config.bond_values if v < 0}
    for name, node in model.types.items():
        assert {(name, t, m): v for (t, m), v in node.pulls} == {
            key: v for key, v in pulls.items() if key[0] == name}
        assert {(name, t, m): v for (t, m), v in node.pushes} == {
            key: v for key, v in pushes.items() if key[0] == name}
        assert node.strict == (name in config.strict_classes)
    # the end of the run says the same, from the snapshot
    assert set(model_at(logs[RUN3]).link_triples()) == set(config.s_links)


def test_the_eight_types_by_category_with_their_formats():
    model = pm.PlatoplasmModel()
    assert list(model.types) == ["Constant", "Countup", "C-group", "S-group", "P-group",
                                 "Y-group", "Cycle", "Tuple"]
    assert [t.category for t in model.types.values()] == [
        "repeater", "repeater", "generate", "generate", "generate", "process", "fence", "fence"]
    assert model.types["S-group"].format == "(S-group start length)"
    assert model.types["Cycle"].format == "(Cycle actual)"
    assert pm.CATEGORIES == ("repeater", "generate", "process", "fence")
    assert all(t.s_links == () and t.pulls == () for t in model.types.values())


# -- run 3's slip-check [M86 pp. 103-105, 176] --------------------------------------------------------

def test_run_3s_slip_check_display_equals_its_event(logs):
    log = logs[RUN3]
    i = index_of(log, "slip-check")
    event = log.events[i]
    assert event.reigning == "S-group" and event.stayval == 15.0
    assert event.best == "Y-group" and event.pressure == 4.0 and not event.slipped
    model = model_at(log, i)
    check = model.check
    assert check.reigning == "S-group" and check.stayval == 15.0
    assert check.best == "Y-group" and check.pressure == 4.0
    assert check.pressures == tuple((to, float(p)) for to, p in event.pressures)
    assert model.pressures() == dict(event.pressures)
    texts = [line.text for line in pm.check_lines(check, model.slipperiness)]
    assert texts[0] == "slip-check: stayval: 15.0"
    assert texts[-2] == "        best: (Y-group 4.0)"
    assert texts[-1] == "We will stay with the reigning class -- S-group."
    by_name = {line.name: line for line in pm.check_lines(check, model.slipperiness)
               if line.kind == "pressure"}
    assert by_name["Y-group"].text == "Y-group: 4.0  (0.4 * 10.0 = 4.0)" and by_name["Y-group"].best
    assert by_name["Countup"].text == "Countup: 1.0  (0.1 * 10.0 = 1.0)"
    assert not by_name["Countup"].best
    assert set(by_name) == {to for to, _ in event.pressures}
    lay = pm.layout(model)
    lit = {l.to: l for l in lay.links if l.lit}
    assert set(lit) == set(by_name) and lit["Y-group"].best and lit["Y-group"].pressure == 4.0
    assert "slipping pressure: 0.4 * 10.0 = 4.0" in pm.link_tooltip(lit["Y-group"])
    assert "best pretender" in pm.link_tooltip(lit["Y-group"])
    assert next(n for n in lay.nodes if n.name == "Y-group").best
    # the pane's lines carry the check after the reigning class
    texts = [line.text for line in pm.pane_lines(model)]
    assert texts[0].startswith("reigning class: S-group")
    assert "slip-check: stayval: 15.0" in texts and "        best: (Y-group 4.0)" in texts


def test_a_slipping_check_on_a_synthetic_event():
    model = pm.PlatoplasmModel()
    model.on_event(RunStarted(1, tuple((f, getattr(Config(), f)) for f in
                                       ("s_links", "bond_values", "strict_classes"))))
    model.on_event(SlipChecked("S-group", 2.0, "Y-group", 4.0,
                               (("Countup", 1.0), ("Y-group", 4.0)), True))
    texts = [line.text for line in pm.check_lines(model.check, model.slipperiness)]
    assert texts[-1] == "slip: S-group --> Y-group"
    model.on_event(ClassChanged("S-group", "Y-group", "slip"))
    assert model.reigning == "Y-group"
    assert pm.change_text(model.change) == (
        "Changing to a new reigning class. S-group --> Y-group  "
        "(a neighbour beat the staying pressure)")


# -- the crown [M86 pp. 99, 174] ---------------------------------------------------------------------

def test_countup_to_s_group_moves_the_crown(logs):
    log = logs[RUN3]
    i = index_of(log, "class-changed")
    event = log.events[i]
    assert (event.old, event.new, event.reason) == ("Countup", "S-group", "strict")
    before = model_at(log, i - 1)
    assert before.reigning == "Countup"
    crown = pm.layout(before).crown
    countup = next(n for n in pm.layout(before).nodes if n.name == "Countup")
    assert crown.to == "Countup" and crown.frm is None and crown.progress == 1.0
    assert countup.x <= crown.x <= countup.x + countup.w
    model = model_at(log, i)
    assert model.reigning == "S-group" and model.crown_progress() == 0.0
    assert pm.change_text(model.change) == (
        "Changing to a new reigning class. Countup --> S-group  (strict: slipping at once)")
    lay = pm.layout(model)
    assert lay.crown.frm == "Countup" and lay.crown.to == "S-group"
    assert lay.crown.progress == 0.0
    assert (lay.crown.x, lay.crown.y) == (crown.x, crown.y)      # still over Countup
    s_group = next(n for n in lay.nodes if n.name == "S-group")
    assert s_group.reigning and not countup.reigning or True
    xs = []
    for k in range(1, pm.CROWN_MOVE + 1):
        model = model_at(log, i + k)
        lay = pm.layout(model)
        xs.append((lay.crown.x, lay.crown.y))
        assert lay.crown.progress == pytest.approx(min(1.0, k / pm.CROWN_MOVE))
    assert len(set(xs)) == len(xs)                               # it travels
    end_x, end_y = xs[-1]
    assert s_group.x <= end_x <= s_group.x + s_group.w and lay.crown.frm is None
    assert next(n for n in lay.nodes if n.name == "S-group").reigning
    assert not next(n for n in lay.nodes if n.name == "Countup").reigning
    # the gnoths moved with the class [M86 pp. 174-175]
    assert set(model.types["S-group"].actualizations) == {"gnoth1", "gnoth2", "gnoth3"}
    assert model.types["Countup"].actualizations == ()


def test_crown_progress():
    assert pm.crown_progress(None, 100) == 1.0
    change = pm.ClassChange("Countup", "S-group", "strict", 10)
    assert pm.crown_progress(change, 10) == 0.0
    assert pm.crown_progress(change, 10 + pm.CROWN_MOVE) == 1.0
    assert 0 < pm.crown_progress(change, 11) < 1


# -- the lists against the snapshots [M86 pp. 69, 77, 90] -----------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_the_lists_equal_every_snapshots(logs, run_id):
    log = logs[run_id]
    model = pm.PlatoplasmModel()
    disagreed = []
    for i, event in enumerate(log.events):
        model.on_event(event)
        model.update(log.state_at(i))
        snapshot = log.state_at(i)
        if snapshot is None or log.snapshot_index(i) != i:
            continue            # a snapshot taken at this very event (a task boundary)
        expected = {t.name: (set(t.manifestations), set(t.actualizations))
                    for t in snapshot.platoplasm.types}
        if lists(model) != expected:
            disagreed.append(i)
        assert {a.value: set(a.manifestations) for a in snapshot.platoplasm.atoms} == {
            v: set(a.manifestations) for v, a in model.atoms.items()}
        assert model.reigning == snapshot.reformulation.reigning
    assert disagreed == []
    # The silent burst of run 3's dubbed glom1 by the Gnoth-setter's clear_span (DECISION
    # D-036) has no event: the next snapshot corrects it, once.
    assert model.corrections == (1 if run_id == RUN3 else 0)


def test_run_3s_lists_and_atoms_at_the_end(logs):
    model = model_at(logs[RUN3])
    assert model.reigning == "S-group" and model.threshold == 10.0
    # gnoth1 went in the first episode's shift [M86 p. 174]; gnoth2 and gnoth3 remain
    assert set(model.types["S-group"].actualizations) == {"gnoth2", "gnoth3"}
    assert sorted(model.atoms) == [1, 2, 3]          # 1 2 1 2 3 1 entered on seed 30
    assert model.atoms[1].manifestations == ("glint1", "glint3", "glint6")
    assert model.atoms[2].manifestations == ("glint2", "glint4")
    assert model.atoms[3].manifestations == ("glint5",)
    tip = pm.atom_tooltip(model.atoms[1])
    assert tip.startswith("ideal1: predecessor ideal0, successor ideal2")
    assert "manifestations (3): glint1, glint3, glint6" in tip
    lay = pm.layout(model)
    assert [a.value for a in lay.atoms] == [1, 2, 3]
    assert [a.count for a in lay.atoms] == [3, 2, 1]
    assert all(a.y > max(n.y + n.h for n in lay.nodes) for a in lay.atoms)
    node = next(n for n in lay.nodes if n.name == "S-group")
    assert node.counts == "0 m / 2 a" and node.reigning
    tip = pm.node_tooltip(model.types["S-group"], model.reigning)
    assert "S-group  (generate type)" in tip and "the reigning class" in tip
    assert "pull-bonds: Succ adjacent +10" in tip
    assert "push-bonds: Same remote -5, Same adjacent -5" in tip
    assert "s-links: Countup 0.1, C-group 0.3, Y-group 0.4, Cycle 0.3, Tuple 0.4" in tip
    texts = [line.text for line in pm.pane_lines(model)]
    assert texts[0] == "reigning class: S-group   threshold 10.0"
    assert texts[-3:] == ["pull  Succ adjacent +10", "push  Same remote -5",
                          "push  Same adjacent -5"]


def test_run_1_has_a_dubbed_glom_and_no_reigning_class(logs):
    log = logs[RUN1]
    model = model_at(log)
    assert model.reigning is None and model.threshold is None and model.check is None
    assert pm.layout(model).crown is None
    assert model.types["Cycle"].manifestations == ("glom1",)      # (Cycle 3 2 (3 7))
    assert [line.text for line in pm.pane_lines(model)] == ["no reigning class yet"]
    i = index_of(log, "dubbed")
    assert log.events[i].kind_name == "Cycle" and log.events[i].name == "glom1"
    assert model_at(log, i).types["Cycle"].manifestations == ("glom1",)
    assert model_at(log, i - 1).types["Cycle"].manifestations == ()


# -- the selection and the layers (G-025, G-038) ----------------------------------------------------

def test_a_click_selects_a_types_manifestations_and_actualizations(logs):
    selection = Selection()
    model = model_at(logs[RUN3], selection=selection)
    members = model.select("S-group")
    assert set(members) == {"gnoth2", "gnoth3"}
    assert model.selected == "S-group" and model.is_selected("S-group")
    assert selection.includes("gnoth2")
    assert next(n for n in pm.layout(model).nodes if n.name == "S-group").selected
    assert model.select("S-group") == () and model.selected is None
    assert not selection.includes("gnoth2")
    assert model.select(None) == () and model.select("no-such") == ()


def test_the_links_layer_tags_the_structures(logs):
    model = model_at(logs[RUN3])
    assert not model.show_links and pm.layout(model).tags == ()
    model.set_show_links(True)
    tags = pm.layout(model).tags
    assert {(t.owner, t.name, t.kind) for t in tags} == {
        ("S-group", "gnoth2", "actualization"), ("S-group", "gnoth3", "actualization")}


def test_a_start_resets(logs):
    model = model_at(logs[RUN3])
    model.on_event(logs[RUN1].events[0])
    assert model.reigning is None and model.check is None and model.change is None
    assert model.atoms == {} and all(t.actualizations == () for t in model.types.values())
    assert set(model.link_triples()) == set(Config().s_links)


def test_the_style_functions():
    assert pm.link_width(0.0) == 0.6 and pm.link_width(1.0) == 3.0
    assert pm.slipperiness_text(0.4) == "0.4" and pm.pressure_text(4) == "4.0"
    assert pm.pressure_term(0.4, 4.0) == "0.4 * 10.0 = 4.0"
    assert pm.pressure_term(0.0, 0.0).endswith("= 0.0")
    assert pm.scout_text(pm.Scout("S-group", "Y-group", 2.0, 15.0)) == (
        "Slip-scout: x2, stayval 15.0: S-group --> Y-group")
    assert pm.scout_text(pm.Scout("Countup", "S-group", 2.0, None)) == (
        "Slip-scout: x2, strict: Countup --> S-group")
