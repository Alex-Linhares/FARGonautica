"""One run's world: the problem, the configuration, the generator, the Slipnet, the
Workspace, the Coderack and the temperature, built fresh for every run, and `step`, which
runs one codelet with everything that follows it (D-047 to D-050, D-053, D-056, D-061,
D-064, D-066).

"Initially, the program explicitly knows only the location and object type of the touched
object." [F95 p. 64] The problem's objects are "the physical situation itself" [FCCA p. 386],
which the codelets discover piece by piece; the `World` holds that situation and everything
the run builds from it. Nothing is shared between two worlds, and every random draw of a run
goes through `World.rng`, so a run is reproducible from the problem, the profile and the
seed alone. A step chooses a codelet by urgency under the temperature [F95 p. 77], runs it,
posts its follow-ups [F95 p. 63], updates the Slipnet (D-019) and lets active concepts post
their codelets [F95 p. 64], recomputes the temperature when due (D-050), and hangs the
find-answer codelets the temperature calls for, "The lower the temperature, the more such
codelets are hung on the Coderack." [F95 p. 94] "The run ends only when a find-answer
codelet is run." [F95 p. 105], or at the cap (D-061).
"""

from dataclasses import dataclass, field

from tabletop import answer as AN
from tabletop import coderack as CR
from tabletop import context as CX
from tabletop import events as EV
from tabletop import roles as R
from tabletop import worldview as WV
from tabletop.codelets.dispatch import run_codelet
from tabletop.config import PROFILES, Config
from tabletop.problems import Problem
from tabletop.accel import ENGINE, Rng, Slipnet
from tabletop.slipnet_def import node_for_type
from tabletop.temperature import Temperature
from tabletop.workspace import Workspace

# Who asked for a temperature update (D-050; `docs/engine-api.md`, `TemperatureUpdated.by`).
START, SCHEDULE, COMPETITION, CHANGE = "start", "schedule", "competition", "change"
FOCUS = "focus"   # the `by` of the jolts that keep the touched type and "opposite" active


@dataclass(eq=False)
class Step:
    """What one call of `World.step` did, for the trace and the events of a later item
    (D-065): the `clock` after the codelet ran, the `codelet` chosen, the exponent `a` and
    the `temperature` at the choice, the `outcome` (`codelets.common.Outcome`), the
    codelets `removed` for age or capacity as (codelet, reason) pairs (D-053), the
    Slipnet's `update` report or None (D-019), the `top_down` postings as (node, codelet)
    pairs (D-020), the `temperature_update` or None (D-050), the `answer_codelets` the
    schedule posted (D-056), `pushed` and `popped`, the context push and pop that happened
    before the codelet ran, as `context.Push` and `context.Pop` or None (D-021), and
    `finished`."""

    clock: int
    codelet: CR.Codelet
    a: float
    temperature: float
    outcome: object
    removed: list = field(default_factory=list)
    update: object = None
    top_down: list = field(default_factory=list)
    temperature_update: object = None
    answer_codelets: list = field(default_factory=list)
    pushed: object = None
    popped: object = None
    finished: bool = False


class World:
    """The state of one run (D-064, D-066): `problem`, `config`, `rng` (the one seeded
    generator), `slipnet` (started on the touched object's type, D-017), `workspace` (the
    structures built so far), `coderack` ("loaded up with its standard set of starting
    codelets" [F95 p. 97], D-055), `temperature` (at its initial value, D-049), `clock`
    (codelets run), `context` (the `context.Context` the program is in, None at the top
    level, D-021), and, once the run has ended, `answer` (`answer.Answer`), `capped`
    (D-061) and `finished`. Building a `World` performs the start of a run, with the
    Slipnet's first update, so that "some activation has already begun to spread" [F95
    p. 97]. `observers` receive every event of the run through `events`, the run's
    `events.Publisher` (D-065); they cannot change it."""

    def __init__(self, problem: Problem, config: Config, seed: int, observers=()):
        self.problem = problem
        self.config = config
        self.seed = seed
        self.rng = Rng(seed)
        self.clock = 0
        self.events = EV.Publisher(self, observers)
        self.slipnet = Slipnet(config)
        self.slipnet.events = self.events
        self.workspace = Workspace(problem, config)
        self.workspace.events = self.events
        self.temperature = Temperature(config)
        self.context: CX.Context | None = None
        self.answer: AN.Answer | None = None
        self.capped = False
        self.finished = False
        self.steps = 0
        touched = problem.touched_object()
        changed = tuple(sorted((k, repr(v)) for k, v in
                               config.differences(PROFILES["standard"]).items()))
        self.events.record(EV.RunStarted, problem=problem.name, seed=seed, profile=config.profile,
                           engine=ENGINE, changed=changed, touched=touched.id,
                           objects=tuple(problem.ids()))
        self.coderack = CR.initial_rack(config, self.clock)
        self._announce_posted(self.coderack.codelets)
        report = self.slipnet.start(touched.type)
        self._announce_update(report)
        # "the program explicitly knows only the location and object type of the touched
        # object" [F95 p. 64]: it is the one object in view at the start.
        self.workspace.notice(touched.id, self.rng, by="start")
        self.update_temperature(START)

    # ---- publishing (D-065) ---------------------------------------------------------------

    def _announce_posted(self, codelets) -> None:
        for c in codelets:
            self.events.record(EV.CodeletPosted, codelet=c.type, args=tuple(c.args), raw=c.raw,
                               generation=c.generation, urgency=c.urgency, by=c.by)

    def _announce_removed(self, removed) -> None:
        for c, reason in removed:
            self.events.record(EV.CodeletRemoved, codelet=c.type, args=tuple(c.args),
                               posted_at=c.posted_at, reason=reason)

    def _announce_update(self, report) -> None:
        if self.events.listening:   # the report is worked out only for a listener (D-108)
            self.events.record(EV.SlipnetUpdated,
                               changed=tuple(tuple(x) for x in report.changed),
                               lengths=tuple(tuple(x) for x in report.lengths))

    def post(self, codelets) -> list:
        """Hang codelets on the Coderack, publishing each posting and whatever the
        capacity pushes off (D-053, D-065). Returns the (codelet, reason) pairs removed."""
        removed = self.coderack.post_all(codelets)
        self._announce_posted(codelets)
        self._announce_removed(removed)
        return removed

    # ---- the temperature's inputs (D-049, D-050) ------------------------------------------

    def quality(self) -> float:
        """Q, "the average quality of the correspondences comprising" the Worldview "as a
        fraction of an optimally good correspondence" [F95 p. 90]: the mean rating of its
        correspondences over 100, clipped to [`q_floor`, 1]; `q_floor` for an empty
        Worldview (D-049)."""
        cps = self.workspace.worldview()
        if not cps:
            return self.config.q_floor
        mean = WV.current(self).strength / len(cps)
        return max(self.config.q_floor, min(1.0, mean / 100.0))

    def update_temperature(self, by: str):
        """Recompute T from the Worldview's quality and the two logs (D-048 to D-050)."""
        update = self.temperature.update(self.quality(),
                                         [c.clock for c in self.workspace.competitions],
                                         [c.clock for c in self.workspace.changes], by, self.clock)
        self.events.record(EV.TemperatureUpdated, temperature=update.temperature, q=update.q,
                           dq=update.dq, dwv=update.dwv, tcp=update.tcp, a=update.a,
                           held=update.held, by=by)
        return update

    # ---- the find-answer schedule (D-056) -------------------------------------------------

    def idle(self) -> int:
        """The codelets run since the Worldview last changed, or since the start while it
        never has (D-120): what the time pressure reads."""
        changes = self.workspace.changes
        return self.clock - (changes[-1].clock if changes else 0)

    def time_pressure(self) -> float:
        """The time pressure on the answer, 0 to 1 (D-120, v5): 0 while the Worldview has
        changed within the last `answer_patience` codelets, then rising linearly to 1 over
        the next `answer_patience`, "a competing pressure - time pressure - that pushes for
        the program to finish within a reasonable amount of time" [FCCA p. 387]; it does not
        depend on T, so that the default answer [F95 p. 105] stays reachable on a table
        where no structure can form and T never falls. Draws nothing."""
        patience = self.config.answer_patience
        return max(0.0, min(1.0, (self.idle() - patience) / patience))

    def schedule_find_answer(self) -> list:
        """Hang the find-answer codelets the temperature calls for: `answer_post_rate`
        x ((100 - T) / 100) ^ `answer_post_power` of them per cycle, the integer part always
        and the rest with one perceptual coin (D-056, D-060), each at raw urgency `answer_urgency_max`
        x ((100 - T) / 100) ^ `answer_urgency_power`, at least 1, generation 0 (D-054), so
        that an early answer is "unlikely, but theoretically possible" [F95 p. 105]. "The lower the temperature, the more
        such codelets are hung on the Coderack." [F95 p. 94] Under time pressure (D-120) the
        two laws read T x (1 - pressure) in place of T, the same laws at a temperature the
        pressure lowers; with no pressure, nothing differs. Returns the codelets posted."""
        cfg = self.config
        t = self.temperature.value * (1.0 - self.time_pressure())
        expected = cfg.answer_post_rate * ((100.0 - t) / 100.0) ** cfg.answer_post_power
        count = int(expected)
        if expected > count and self.rng.coin(expected - count):
            count += 1
        raw = max(1, round(cfg.answer_urgency_max * ((100.0 - t) / 100.0) ** cfg.answer_urgency_power))
        posted = [CR.make_codelet("find-answer", cfg, self.clock, by=CR.ANSWER_SCHEDULE, raw=raw)
                  for _ in range(count)]
        for c in posted:
            self.events.record(EV.AnswerCodeletPosted, urgency=c.urgency, temperature=t)
        return posted

    # ---- one codelet (D-050, D-053, D-061) ------------------------------------------------

    def drop_orphans(self) -> list:
        """Remove every correspondence with an end at a group that has left the Workspace
        (lost a fight or absorbed, D-025, D-026), since the correspondence has nothing to
        stand on (D-041). Returns the correspondences removed."""
        ws = self.workspace
        orphans = [cp for cp in ws.correspondences
                   if any(end not in self.problem.ids() and not ws.has_group(end)
                          for end in cp.ends)]
        if not orphans:
            return []
        return WV.remove_all(self, orphans)

    def step(self) -> Step | None:
        """Run one codelet and everything that follows it; None once the run has ended.
        The oldest codelets are aged out (D-053); one codelet is chosen by urgency under
        a(T) [F95 p. 77; F95 p. 92]; the context it carries is entered, with the pop and
        the push that takes (D-021); it is run; its follow-ups are posted [F95 p. 63]; the
        Slipnet is updated (D-019) and its active concepts post their codelets [F95 p. 64]
        (D-020); the temperature is recomputed after a competition or a change, and
        otherwise every `temperature_update_every` codelets (D-050); then, unless the run
        has ended, the find-answer codelets due are hung (D-056). A find-answer codelet
        ends the run with its answer [F95 p. 105]; the cap ends it with the answer as it
        stands (D-061). An empty rack is reloaded with the starting set (D-055)."""
        if self.finished:
            return None
        cfg, ws, events = self.config, self.workspace, self.events
        removed = self.coderack.remove_aged(self.clock)
        self._announce_removed(removed)
        a = self.temperature.exponent
        t_before = self.temperature.value
        rack_size = len(self.coderack)
        codelet = self.coderack.choose(a, self.rng)
        if codelet is None:
            self.post(CR.initial_rack(cfg, self.clock).codelets)
            rack_size = len(self.coderack)
            codelet = self.coderack.choose(a, self.rng)
        self.clock += 1
        events.actor = codelet.type
        events.record(EV.CodeletChosen, codelet=codelet.type, args=tuple(codelet.args),
                      generation=codelet.generation, urgency=codelet.urgency,
                      chance=codelet.chance, a=a, temperature=t_before, rack_size=rack_size)
        competitions_before, changes_before = len(ws.competitions), len(ws.changes)
        noticed_before = len(ws.noticed)
        pushed, popped = CX.enter(self, codelet)
        self._announce_context(pushed, popped)
        outcome = run_codelet(self, codelet, a)
        events.record(EV.CodeletRan, codelet=codelet.type, args=tuple(codelet.args),
                      generation=codelet.generation, action=outcome.action, detail=outcome.detail,
                      posted=tuple(c.type for c in outcome.posted))
        if outcome.fizzled:
            events.record(EV.CodeletFizzled, codelet=codelet.type, args=tuple(codelet.args),
                          reason=outcome.detail)
        removed += self.post(outcome.posted)
        self.drop_orphans()
        self.perceive_roles(noticed_before, codelet.type, a)
        update, top_down = None, []
        if self.clock % cfg.slipnet_update_every == 0:
            self.focus()
            update = self.slipnet.update()
            self._announce_update(update)
            for node, codelet_type in self.slipnet.top_down_postings(self.rng):
                posted = CR.make_codelet(codelet_type, cfg, self.clock, by=CR.SLIPNET)
                removed += self.post([posted])
                top_down.append((node, posted))
        temperature_update = None
        if len(ws.changes) > changes_before:
            temperature_update = self.update_temperature(CHANGE)
        elif len(ws.competitions) > competitions_before:
            temperature_update = self.update_temperature(COMPETITION)
        elif self.clock % cfg.temperature_update_every == 0:
            temperature_update = self.update_temperature(SCHEDULE)
        answer_codelets = []
        if outcome.answer is not None:
            self.finish(outcome.answer, capped=False)
        elif self.clock >= cfg.max_codelets:
            self.finish(AN.answer_for(self), capped=True)
        else:
            answer_codelets = self.schedule_find_answer()
            removed += self.post(answer_codelets)
        self.steps += 1
        return Step(self.clock, codelet, a, t_before, outcome, removed, update, top_down,
                    temperature_update, answer_codelets, pushed, popped, self.finished)

    def perceive_roles(self, noticed_before: int, by: str, exponent: float) -> list:
        """Read the roles of the objects the codelet just brought into view (D-079): each
        new object's roles through `roles.perceive`, which asks the built detectors and
        proposes a tag for each reading; an object with no role gets no tag and nothing
        changes. Returns the tags that stand. The perception that needs structures built
        later (groups, neighbours, the Worldview) is the find-roles codelet's, from the
        item that builds it."""
        tags = []
        for id_ in list(self.workspace.noticed[noticed_before:]):
            tags.extend(R.perceive(self, id_, by, exponent))
        return tags

    def focus(self) -> None:
        """Keep the touched object's category and "opposite" active (D-017): "the concept
        'opposite' and the category of the touched object ... are active initially" [F95
        p. 66], and the dumps hold them near the ceiling for the whole run [F95 pp. 98-107
        (img)]; one `focus_jolt` to each before every Slipnet update, published `by`
        `focus`. Nothing when the jolt is 0."""
        if self.config.focus_jolt <= 0:
            return
        actor = self.events.actor
        self.events.actor = FOCUS
        self.slipnet.jolt(node_for_type(self.problem.touched_object().type), self.config.focus_jolt)
        self.slipnet.jolt("opposite", self.config.focus_jolt)
        self.events.actor = actor

    def _announce_context(self, pushed, popped) -> None:
        if popped is not None:
            self.events.record(EV.ContextPopped, correspondence=popped.correspondence,
                               reinitialized=tuple(popped.reinitialized),
                               spread=tuple(tuple(x) for x in popped.spread),
                               fixed=tuple(tuple(x) for x in popped.fixed),
                               restored=tuple(tuple(x) for x in popped.restored))
        if pushed is not None:
            self.events.record(EV.ContextPushed, correspondence=pushed.correspondence,
                               reset=tuple(tuple(x) for x in pushed.reset))

    def finish(self, answer: AN.Answer, capped: bool) -> None:
        """End the run with this answer [F95 p. 105]; `capped` marks a run the cap stopped
        (D-061). Publishes `AnswerGiven` and then `RunEnded` (D-065)."""
        self.answer = answer
        self.capped = capped
        self.finished = True
        self.events.record(EV.AnswerGiven, answer=answer.object, label=answer.label,
                           literal=answer.literal, through=answer.through, via=answer.via,
                           note=answer.note)
        self.events.record(EV.RunEnded, answer=answer.object, label=answer.label,
                           literal=answer.literal, capped=capped,
                           structure_value=self.structure_value(), codelets=self.clock,
                           temperature=self.temperature.value, worldview=tuple(WV.summary(self)))

    def snapshot(self):
        """A frozen copy of the run's state as plain data (`snapshot.snapshot`; D-065),
        which draws nothing and changes nothing."""
        from tabletop.snapshot import snapshot
        return snapshot(self)

    def structure_value(self) -> float:
        """The Worldview's rating as it stands, the structure value a run reports (D-062)
        [F95 pp. 113-114; FCCA p. 392]."""
        return WV.current(self).total
