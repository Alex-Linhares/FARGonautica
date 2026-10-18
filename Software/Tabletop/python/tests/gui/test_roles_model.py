"""The roles pane's model and the roles the other panes read (item 14 of the roles loop;
`models/roles.py`, the roles layer of `models/overlays.py`, the role-proposed mark of
`models/structures.py` and `models/worldview.py`, the role marker of `models/timeline.py`,
the roles' band of `models/slipnet_grid.py` and the role dots and links of
`models/slipnet_network.py`; G-060 to G-062), Qt-free:

- at every snapshot index of `blockage-07` seed 9 (a run that reads every family: the
  midline, a gap, images, a loner, a figure and a ground, an echo, the axis, the re-cast
  and a barrier with its claimed counterpart; seed 16 from item 10 of the roles loop to
  v4, which reads no barrier since v5's fix, D-116 to D-120) the standing tags equal
  `Snapshot.roles`, the re-cast `Snapshot.recast` and the axis `Snapshot.axis`;
- an item's history keeps its lost tags with when and why they went (the author's answer
  D1 of the roles loop), and the readers come from `RoleConsumed`;
- the lines: the banner, the axis, the re-cast's tiers, five family headings, a standing
  tag solid and a past one dashed, the fresh mark at the tag's own event; the hover
  text; what a click selects;
- the overlays' roles layer: badges with the registry's codes, the midline band, the
  gaps between their clusters, the touched object's images, the re-cast's tiers, the
  barrier with its claim, the note and the hover lines; nothing with the layer off; Eliza's
  view flips every box and is its own inverse;
- a correspondence a role consumer proposed is marked in the structures and the
  Worldview entries; the timeline marks every role change and re-cast as a violet dot;
  the grid's roles' band and the network's role dots and links are flagged.
"""

import dataclasses

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop import roles as R
from tabletop import slipnet_def as sd
from tabletop.config import PROFILES
from tabletop.gui.models import overlays as ov
from tabletop.gui.models import roles as rm
from tabletop.gui.models import slipnet_grid as sg
from tabletop.gui.models import slipnet_network as sn
from tabletop.gui.models import structures as st
from tabletop.gui.models import table as tm
from tabletop.gui.models import timeline as tl
from tabletop.gui.models import worldview as wm
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.tap import Tap
from tabletop.gui.models.worldview import DETAIL, HEADING, MUTED, SHADOW, WORLDVIEW
from tabletop.problems import load_problem
from tabletop.world import World

RICH = ("blockage-07", 9)       # every family read; a barrier and its claim (item 10 of the roles loop)
QUICK = ("surround-05", 43)     # first-sight tags only, no role change
# Since v5's fix (D-116 to D-120): blockage-07 seed 16, the RICH run from item 10 of the
# roles loop to v4, no longer reads a barrier, so RICH moved to seed 9, the first seed that
# reads every family with the barrier standing at the end (fork1 claiming fork2 through
# cp1) and images on the touched object (seed 104, the engine tests' run, reads every
# family too but its cup1 has no image left by the end); and surround-05 seed 3, the quick
# run and QUICK here from item 08 to v4, reads a midline, a gap, images, a loner, an echo
# and twins, so QUICK moved to seed 43, the first seed whose tags are first-sight only
# (COLUMN, CORNER and MEMBER; 23 codelets, a literal answer).
STANDARD = V2_PROFILES["standard"]


def stepped(problem, seed):
    """The run stepped by hand with the tap: the log and the index of the last event of
    the start and of each step."""
    log = EventLog()
    tap = Tap(log)
    world = World(load_problem(problem), STANDARD, seed, observers=[tap])
    tap.attach(world)
    boundaries = [len(log.events) - 1]
    while not world.finished:
        world.step()
        tap.after_step(world)
        boundaries.append(len(log.events) - 1)
    return log, boundaries


def fed(events, upto=None):
    model = rm.RolesModel()
    for event in events if upto is None else events[:upto + 1]:
        model.on_event(event)
    return model


def standing_view(model):
    return sorted((t.name, t.item, t.role, t.args, t.weight,
                   tuple((c, e) for c, e, _by, _clock in t.consumed))
                  for t in model.standing())


def snapshot_view(snapshot):
    return sorted((d["name"], d["item"], d["role"], tuple(d["args"]), d["weight"],
                   tuple((c[0], c[1]) for c in d["consumed"]))
                  for d in snapshot.roles)


# -- the model against the snapshots (G-060) ---------------------------------------------------

def test_the_standing_tags_the_recast_and_the_axis_equal_the_snapshots_at_every_index():
    log, boundaries = stepped(*RICH)
    seen = set()
    for index in boundaries:
        snapshot = log.state_at(index)
        model = fed(log.events, index)
        assert standing_view(model) == snapshot_view(snapshot), index
        if snapshot.recast is None:
            assert model.recast is None
        else:
            assert {k: model.recast[k] for k in ("touched", "signature", "tiers", "ambiguous", "clock")} \
                == dict(snapshot.recast)
        axis = model.axis()
        assert (axis.name if axis is not None else None) == \
            (snapshot.axis["name"] if snapshot.axis is not None else None)
        seen |= {t.role for t in model.standing()}
    assert {R.MIDLINE, R.GAP, R.MIRROR_IMAGE, R.POINT_IMAGE, R.LONER, R.FIGURE, R.GROUND, R.ECHO,
            R.SYMMETRY_AXIS, R.COUNTERPART, R.BARRIER, R.CLAIMED, R.ROW, R.COLUMN} <= seen
    assert model.events == len(log.events) and model.index == len(log.events) - 1


def test_an_items_history_keeps_its_lost_tags_with_when_and_why():
    run = plain_run(*RICH, V2)
    model = fed(run.events)
    drops = [e for e in run.events if isinstance(e, ev.RoleDiscarded)]
    assert drops, "the run changes roles (D1 of the roles loop)"
    for drop in drops:
        tag = model.tags[drop.tag]
        assert not tag.standing and tag.item == drop.item and tag.role == drop.role
        assert tag.reason == drop.reason and tag.discarded_by == drop.by
        assert tag.discarded_at == drop.clock and run.events[tag.discarded_index] is drop
        assert tag in model.history_of(drop.item) and tag not in model.tags_of(drop.item)
    consumed = [e for e in run.events if isinstance(e, ev.RoleConsumed)]
    assert {e.consumer for e in consumed} >= {"C1", "C2", "C4", "C6"}
    readers = {c for t in model.tags.values() for c in t.consumers}
    assert readers == {e.consumer for e in consumed}
    barrier = [t for t in model.tags.values() if t.role == R.BARRIER]
    assert barrier and "C4" in barrier[0].consumers
    assert model.recasts == sum(isinstance(e, ev.RoleRecast) for e in run.events) > 0
    assert model.touched == "cup1" and "g1" in model.groups


def test_the_lines_are_the_banner_the_axis_the_recast_and_the_families():
    run = plain_run(*RICH, V2)
    model = fed(run.events)
    lines = model.lines()
    assert lines[0].role == HEADING and lines[0].text.startswith(f"{len(model.standing())} roles standing on")
    assert "re-cast" in lines[0].text
    assert lines[1].name == R.WORLDVIEW_ITEM and lines[1].text.startswith("axis: point symmetry")
    recast = next(line for line in lines if line.text.startswith("re-cast of cup1"))
    assert recast.role == HEADING and "signature [" in recast.text
    tiers = [line for line in lines if line.text.startswith("tier ")]
    assert tiers and all(line.role == DETAIL for line in tiers)
    headings = [line for line in lines if line.role == HEADING and ": " in line.text
                and "standing, " in line.text]
    assert [h.text.split(":")[0] for h in headings] == [rm.FAMILY_TITLES[f] for f in R.FAMILIES]
    solid = [line for line in lines if line.role == WORLDVIEW]
    dashed = [line for line in lines if line.role == SHADOW]
    assert len(solid) == len(model.standing())
    assert len(dashed) == len(model.tags) - len(model.standing()) > 0
    first_past = dashed[0]
    tag = next(t for t in model.tags.values() if rm.tag_text(t) == first_past.text)
    detail = lines[lines.index(first_past) + 1]
    assert detail.role == DETAIL and f"{tag.reason} by {tag.discarded_by}" in detail.text
    # The tag's own event tints its lines; another event does not (G-060).
    at_build = fed(run.events, tag.built_index)
    fresh = [line for line in at_build.lines() if line.fresh]
    assert fresh and all(line.name == tag.item for line in fresh)
    assert not any(line.fresh for line in fed(run.events, tag.built_index + 1).lines()
                   if line.name == tag.item and rm.tag_text(tag) == line.text)
    # Before any tag: the banner says so and every family is empty.
    empty = rm.RolesModel()
    texts = [line.text for line in empty.lines()]
    assert texts[0] == "no role read yet" and "axis: none read yet" in texts
    assert texts.count("none read") == len(R.FAMILIES)
    assert [line.role for line in empty.lines() if line.text == "none read"] == [MUTED] * 5


def test_the_hover_text_and_the_selection(golden_runs):
    model = fed(plain_run(*RICH, V2).events)
    text = model.tooltip("cup1")
    assert text.startswith("cup1: ") and "standing" in text and "past" in text
    assert all(line.startswith("  r") for line in text.splitlines()[1:])
    assert model.tooltip("no-such-object") == ""
    assert model.selection_of("cup1") == ("object", "cup1", ())
    assert model.selection_of("g1") == ("group", "g1", model.groups["g1"])
    assert model.selection_of(R.TABLE) is None and model.selection_of(R.WORLDVIEW_ITEM) is None
    quick = fed(golden_runs[("surround-05", 1)].events)
    assert quick.standing() and quick.banner().startswith(f"{len(quick.standing())} role")


# -- the overlays' roles layer (G-061) -----------------------------------------------------------

def table_with_roles(problem, seed, roles=True):
    run = plain_run(problem, seed, V2)
    model = tm.TableModel()
    model.set_problem(load_problem(problem))
    model.overlays.layers["roles"] = roles
    for event in run.events:
        model.on_event(event)
    return model, run


def test_the_roles_layer_draws_badges_the_midline_the_gaps_the_images_the_tiers_and_the_block():
    model, run = table_with_roles(*RICH)
    lay = model.layout()
    o = lay.overlays
    assert o.roles_shown and not o.empty and "roles (" in o.note and ov.NOT_FRENCHS in o.note
    badges = {b.item: b for b in o.badges}
    roles = model.overlays.roles
    for item, badge in badges.items():
        tags = roles.tags_of(item)
        assert tags and badge.codes == tuple((rm.code_of(t.role), t.role, t.args) for t in tags)
        assert badge.box == (lay.object(item).box if lay.object(item) else lay.structure(item).box)
    assert set(badges) == {t.item for t in roles.standing()
                           if t.item not in (R.TABLE, R.WORLDVIEW_ITEM)}
    assert all(rm.code_of(r) == R.spec(r).code for r in R.NAMES)
    assert o.midline == (0.0, lay.height / 3, lay.width, 2 * lay.height / 3)
    assert o.axis == "point"
    gaps = {(g.row, g.a, g.b) for g in o.gaps}
    assert gaps == {tuple(t.args[:3]) for t in roles.tags_of(R.TABLE) if t.role == R.GAP}
    for gap in o.gaps:
        left, top, right, bottom = gap.box
        assert left < right and top < bottom
    images = {(i.kind, i.source, i.image) for i in o.images}
    expected = {("mirror" if t.role == R.MIRROR_IMAGE else "point", "cup1", t.args[0])
                for t in roles.tags_of("cup1") if t.role in (R.MIRROR_IMAGE, R.POINT_IMAGE)}
    assert images == expected and images
    tiers = {(t.candidate, t.tier) for t in o.tiers}
    assert tiers == {(t.item, t.args[1]) for t in roles.standing() if t.role == R.COUNTERPART}
    assert tiers and all(t.weight > 0 for t in o.tiers)
    blocks = [(b.barrier, b.claimed, b.correspondence) for b in o.blocks]
    assert blocks == [(t.item, t.args[0], t.args[1]) for t in roles.standing()
                      if t.role == R.BARRIER] and blocks
    lines = model.overlays.hover_lines("cup1")
    assert lines and lines[0].startswith("roles: ") and any("L4 LONER" in l for l in lines)
    assert "roles:" in model.tooltip("cup1")
    # The focused object's images follow the focus (G-061).
    model.overlays.focus = blocks[0][1]
    focused = model.layout().overlays.images
    assert all(i.source == blocks[0][1] for i in focused)


def test_nothing_is_drawn_with_the_layer_off_and_elizas_view_flips_every_box():
    off, _run = table_with_roles(*RICH, roles=False)
    o = off.layout().overlays
    assert o.empty and not o.roles_shown and not o.badges and o.midline is None
    assert not o.gaps and not o.images and not o.tiers and not o.blocks and o.note == ""
    assert off.overlays.hover_lines("cup1") == []
    on, _run = table_with_roles(*RICH)
    henry = on.layout()
    on.eliza = True
    eliza = on.layout()
    assert eliza.eliza and eliza.overlays.roles_shown
    w, h = henry.width, henry.height
    for a, b in zip(henry.overlays.badges, eliza.overlays.badges):
        assert b.box == ov.flip_box(a.box, w, h) and b.codes == a.codes
    assert eliza.overlays.midline == ov.flip_box(henry.overlays.midline, w, h)
    for a, b in zip(henry.overlays.blocks, eliza.overlays.blocks):
        assert b.barrier_box == ov.flip_box(a.barrier_box, w, h)
        assert b.claimed_box == ov.flip_box(a.claimed_box, w, h)
    for a, b in zip(henry.overlays.images, eliza.overlays.images):
        assert b.image_box == ov.flip_box(a.image_box, w, h)
    back = ov.eliza_view(eliza)
    for a, b in zip(henry.overlays.gaps + henry.overlays.tiers, back.overlays.gaps + back.overlays.tiers):
        assert all(abs(x - y) < 1e-9 for x, y in zip(a.box, b.box))


def test_the_quick_run_has_first_sight_badges_and_no_role_change():
    model, run = table_with_roles(*QUICK)
    o = model.layout().overlays
    assert o.badges and o.midline is None and not o.gaps and not o.images and not o.blocks
    assert all(code in ("L2", "L3", "A1") for b in o.badges for code, _r, _a in b.codes)
    assert not [e for e in run.events if isinstance(e, ev.RoleDiscarded)]


# -- the other panes (G-061, G-062) -----------------------------------------------------------------

def test_a_role_proposed_correspondence_is_marked_in_the_structures_and_the_worldview():
    run = plain_run(*RICH, V2)
    structures = st.StructuresModel()
    worldview = wm.WorldviewModel()
    for event in run.events:
        structures.on_event(event)
        worldview.on_event(event)
    effects = {frozenset(e.effect.split()[1:]): (e.consumer, e.role, e.by)
               for e in run.events if isinstance(e, ev.RoleConsumed)
               and e.effect.startswith("create-cp ")}
    marked = {c.name: c for c in structures.correspondences.values() if c.role_proposed}
    assert marked, "the run has a correspondence a role consumer proposed (D-095)"
    for c in marked.values():
        consumer, role, by = effects[frozenset((c.henry, c.eliza))]
        assert c.role_proposed == f"{consumer} {role} by {by}"
        assert "role-proposed:" in structures.tooltip(c.name)
    for c in structures.correspondences.values():
        if frozenset((c.henry, c.eliza)) not in effects:
            assert not c.role_proposed
    entries = {e.name: e for e in worldview.entries()[0] + worldview.entries()[1]}
    listed = [name for name in marked if name in entries]     # the removed ones are not listed
    assert listed
    for name in listed:
        assert entries[name].role_proposed == marked[name].role_proposed
    texts = [line.text for line in worldview.lines()]
    assert any("role-proposed (" in text for text in texts)


def test_the_correspondence_line_carries_the_mark():
    model, _run = table_with_roles(*RICH, roles=False)
    lines = {c.name: c for c in model.layout().correspondences}
    states = model.structures.correspondences
    assert any(line.role_proposed for line in lines.values())
    for name, line in lines.items():
        assert line.role_proposed == bool(states[name].role_proposed)


def test_the_timeline_marks_every_role_change_and_recast(golden_runs):
    run = plain_run(*RICH, V2)
    log = EventLog()
    for event in run.events:
        log.on_event(event)
    model = tl.TimelineModel(log)
    model.sync()
    role = model.markers_of("role")
    changes = [i for i, e in enumerate(run.events) if isinstance(e, (ev.RoleDiscarded, ev.RoleRecast))]
    assert [m.index for m in role] == changes and changes
    for marker in role:
        style = tl.marker_style(marker)
        assert (style.color, style.shape, style.row) == ("#5b2a86", "dot", 0)
        event = run.events[marker.index]
        if isinstance(event, ev.RoleDiscarded):
            assert marker.label == "r" and event.item in marker.text and event.reason in marker.text
        else:
            assert marker.label == "R" and marker.text.startswith(f"re-cast of {event.touched}")
    assert "role" in tl.MARKER_KINDS
    quiet = EventLog()
    for event in golden_runs[("surround-05", 1)].events:
        quiet.on_event(event)
    other = tl.TimelineModel(quiet)
    other.sync()
    assert len(other.markers_of("role")) == sum(isinstance(e, (ev.RoleDiscarded, ev.RoleRecast))
                                                for e in golden_runs[("surround-05", 1)].events)


def test_the_grids_roles_band_and_the_networks_role_dots_and_links_are_flagged():
    grid = sg.SlipnetGridModel()
    lay = grid.layout(280, 300)
    band = [b for b in lay.boxes if b.role]
    assert {b.name for b in band} == set(sd.ROLE_NODES) and len(band) == 15
    assert all(not b.on_screen and b.row >= sg.FIRST_ROLE_ROW for b in band)
    assert lay.role_caption == sg.ROLE_CAPTION == \
        "not on French's screen (re-interpretation): the 15 role nodes (D-080)"
    assert "roles' band: not on French's screen (re-interpretation; D-080)" in grid.tooltip("barrier")
    assert "seventh row" in grid.tooltip("isolated") and "roles' band" not in grid.tooltip("cup")
    network = sn.SlipnetNetworkModel()
    nl = network.layout(600, 400)
    assert {d.name for d in nl.dots if d.role} == set(sd.ROLE_NODES)
    assert {l.name for l in nl.lines if l.role} == {link.name for link in sd.ROLE_LINKS}
    assert len([l for l in nl.lines if l.role]) == 29
    assert "a role node: not on French's screen (re-interpretation; D-080)" in network.tooltip("key")
    assert "a role node" not in network.tooltip("opposite")
    assert "violet" in sn.CAPTION
    assert dataclasses.replace(nl.dots[0], role=True).role
