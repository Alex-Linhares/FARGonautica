"""The Slipnet's behaviour: activations in 1-150, jolts, label-controlled link lengths, the
parallel-path distance, spreading, the label trickle, decay, the initial state and the
top-down posting hook (D-011 to D-020), and the reset and the reconstruction that the
context push and pop of `context.py` use (D-021, D-070).

"During a run, nothing is built or destroyed in the Slipnet, but there is activity of another
sort: the waxing and waning of conceptual activations, and the concomitant stretching and
shrinking of conceptual distances" [FCCA p. 381]. A node's activation comes from three
sources: jolts from codelets that notice an instance of the concept, spreading from nearby
nodes, and decay at a rate set by the node's depth [F95 pp. 55-56]; "Activation values range
from 1 to 150." [F95 p. 95] The length of a labeled link "varies dynamically as a function of
the activation of the link's label node" [F95 p. 56], and the distance between two nodes is
the parallel combination of the lengths of the pathways joining them [F95 p. 61 (img)].

One `Slipnet` per run, built from a `Config` and the definition in `slipnet_def.py`; it holds
no module state. Its only randomness is the posting hook's coins, through the run's generator
passed in (D-064). Every update is synchronous: the amounts spread and trickled are buffered
and added after every node has been read, so the order of nodes never matters (D-013).
"""

import math
from dataclasses import dataclass
from functools import cached_property

from tabletop import events as EV
from tabletop import slipnet_def as D
from tabletop.config import Config

ACTIVATION_MIN = 1.0    # "Activation values range from 1 to 150." [F95 p. 95]; dormancy (D-015)
ACTIVATION_MAX = 150.0  # the ceiling, reached by "right" and "opposite" [F95 p. 107 (img)]


def clip(value: float) -> float:
    """An activation kept in [1, 150] (D-015) [F95 p. 95]."""
    return min(ACTIVATION_MAX, max(ACTIVATION_MIN, value))


def parallel_distance(path_lengths) -> float:
    """The distance over several pathways: the reciprocal of the sum of the reciprocals of
    the pathways' lengths, as for resistors in parallel, Distance(A,B) = 1 / (1/(d1 + d1') +
    1/(d2 + d2') + 1/d3) [F95 p. 61 (img)]; `math.inf` when there is no pathway (D-012)."""
    total = 0.0
    for length in path_lengths:
        total += 1.0 / length
    return 1.0 / total if total > 0 else math.inf


@dataclass
class UpdateReport:
    """What one Slipnet update changed, for the `SlipnetUpdated` event (D-019): the nodes
    whose activation moved (name, new value) and the labeled links whose effective length
    changed (link name, new length), both rounded to a tenth, in definition order. It keeps
    the activations and lengths before and after the update and works `changed` and
    `lengths` out when they are first read (D-108): a run nobody observes never reads them."""

    nodes: tuple
    links: tuple
    activation_before: list
    lengths_before: list
    activation_after: list
    lengths_after: list

    @cached_property
    def changed(self) -> list:
        """The nodes whose activation moved to a tenth, (name, new value) (D-019)."""
        return [(node.name, round(after, 1))
                for node, before, after in zip(self.nodes, self.activation_before,
                                               self.activation_after)
                if round(after, 1) != round(before, 1)]

    @cached_property
    def lengths(self) -> list:
        """The links whose effective length moved to a tenth, (name, new length) (D-019)."""
        return [(link.name, round(after, 1))
                for link, before, after in zip(self.links, self.lengths_before,
                                               self.lengths_after)
                if round(after, 1) != round(before, 1)]


class Slipnet:
    """The concept network of one run (D-007 to D-020) [F95 pp. 55-63].

    Fields: `config`; `nodes` (the definitions, in order); `activation`, a list parallel to
    `nodes`; `depth`, the depth of each node from its level (D-008); `links` (the definitions)
    and `effective`, the current length of each link (D-011); `distances`, the distance
    matrix, recomputed after every update (D-012); `updates`, how many updates have run.
    """

    def __init__(self, config: Config, nodes=D.NODES, links=D.LINKS):
        self.config = config
        self.nodes = tuple(nodes)
        self.index = {node.name: i for i, node in enumerate(self.nodes)}
        self.links = tuple(links)
        self.activation = [ACTIVATION_MIN] * len(self.nodes)
        self.depth = [config.depth_by_level[node.level - 1] for node in self.nodes]
        self.effective = [float(link.length) for link in self.links]
        self.updates = 0
        self._posted_at = list(self.activation)   # activations at the last posting (D-020)
        self.events = EV.Publisher()               # the run's publisher, set by the World (D-065)
        self._paths = self._enumerate_paths()
        self._neighbours = self._enumerate_neighbours()
        self.distances = []   # the n x n matrix of D-012, flat: d(i, j) at i * n + j
        self._recompute_lengths()
        self._recompute_distances()

    # ---- reading ------------------------------------------------------------------------

    def activation_of(self, name: str) -> float:
        """A node's activation, in [1, 150] (D-015)."""
        return self.activation[self.index[name]]

    def activations(self) -> list:
        """Every node's activation as (name, value), in definition order, for snapshots and
        the trace (D-015)."""
        return [(node.name, self.activation[i]) for i, node in enumerate(self.nodes)]

    def distance(self, a: str, b: str) -> float:
        """The conceptual distance between two nodes over every pathway of at most
        `max_path_links` links, at the links' current lengths [F95 p. 61 (img)]; 0 from a
        node to itself, `math.inf` when no pathway joins them (D-012)."""
        return self.distances[self.index[a] * len(self.nodes) + self.index[b]]

    def effective_length(self, link: D.LinkDef) -> float:
        """A link's current length: the intrinsic one for ISA and identity links, the shrunk
        one for a labeled link whose label is active [F95 p. 56] (D-011). The link is found
        by identity in the table, as the update indexes it."""
        return self.effective[next(i for i, own in enumerate(self.links) if own is link)]

    def links_of(self, name: str) -> list:
        """Every link of this Slipnet's table that touches a node, in table order (D-009)."""
        return D.links_of(name, self.links)

    def categories_of(self, type_node: str) -> list:
        """The categories a type ISA in this Slipnet's table, the nearest first (D-009)."""
        return D.categories_of(type_node, self.links)

    def labels_between(self, a: str, b: str) -> tuple:
        """The labels of the link joining two nodes in this Slipnet's table, `()` when there
        is none or it is an ISA link [F95 p. 62] (D-009, D-030)."""
        return D.labels_between(a, b, self.links)

    def neighbours(self, name: str) -> list:
        """The nodes within `spread_radius` of a node with their distances, nearest first
        (D-013), for the network view."""
        pairs = [(other.name, self.distance(name, other.name)) for other in self.nodes
                 if other.name != name]
        return sorted([p for p in pairs if p[1] <= self.config.spread_radius],
                      key=lambda p: p[1])

    def level(self, name: str) -> int:
        """A node's level of abstractness, 1 to 3 [F95 pp. 61-62] (D-008)."""
        return self.nodes[self.index[name]].level

    # ---- changing activations -----------------------------------------------------------

    def set_activation(self, name: str, value: float) -> float:
        """Set a node's activation outright, clipped (D-015); the initial state (D-017) and
        the context reset (D-021) use it. Returns the value set."""
        i = self.index[name]
        self.activation[i] = clip(float(value))
        return self.activation[i]

    def jolt(self, name: str, amount: float) -> float:
        """Add a jolt of activation to a node, "a standard jolt" from a codelet that noticed
        an instance of the concept [F95 p. 97], clipped at once (D-015, D-016). Returns the
        new activation."""
        i = self.index[name]
        self.activation[i] = clip(self.activation[i] + amount)
        self.events.record(EV.NodeJolted, node=name, amount=float(amount), by=self.events.actor,
                           activation=self.activation[i])
        return self.activation[i]

    def reset(self, names) -> list:
        """Set the named nodes to the initial value 1, "the activation slate really is wiped
        clean" [F95 p. 87] (the context push, D-021, D-070), and their posting levels with
        them, so that a rise inside the subworld counts from 1 (D-020). Returns the names
        reset."""
        for name in names:
            self.restore(name, ACTIVATION_MIN)
        return list(names)

    def restore(self, name: str, value: float) -> float:
        """Set a node's activation to a reconstructed level (the context pop, D-021),
        clipped (D-015), and its posting level to the same, so that the reconstruction
        itself posts no codelet (D-020). Returns the value set."""
        i = self.index[name]
        self.activation[i] = clip(float(value))
        self._posted_at[i] = self.activation[i]
        return self.activation[i]

    def spread_step_into(self, names) -> list:
        """What one spreading step (D-013) would send into the named nodes from every other
        node at the levels as they stand: the pop's fix, "taking into account the activation
        levels of the nodes connected to the reinitialized nodes" [F95 p. 89] (D-021).
        Returns (name, amount) pairs in the names' order, amounts of 0 included; changes
        nothing."""
        buffer = [0.0] * len(self.nodes)
        self._spread_into(buffer)
        return [(name, buffer[self.index[name]]) for name in names]

    def start(self, touched_type: str) -> UpdateReport:
        """The initial state (D-017): every node at 1, "opposite" at `initial_opposite` and
        the touched object's type at `initial_touched_type` ("the concept 'opposite' and the
        category of the touched object ... are active initially" [F95 p. 66]), then one
        update, so that "some activation has already begun to spread" [F95 p. 97] before the
        first codelet runs. Returns the update's report."""
        for i in range(len(self.activation)):
            self.activation[i] = ACTIVATION_MIN
        self.set_activation("opposite", self.config.initial_opposite)
        self.set_activation(D.node_for_type(touched_type), self.config.initial_touched_type)
        report = self.update()
        # The initial state is not a rise: the posting levels of D-020 start from it, so
        # that the first update posts only for what the first codelets add (the starting
        # rack of D-055 already holds the pick codelets of [F95 p. 65]).
        self._posted_at = list(self.activation)
        # The first report says what moved from the all-1 state, for `SlipnetUpdated` (D-065).
        return UpdateReport(self.nodes, self.links, [ACTIVATION_MIN] * len(self.nodes),
                            report.lengths_before, report.activation_after, report.lengths_after)

    # ---- the update ---------------------------------------------------------------------

    def update(self) -> UpdateReport:
        """One cycle of the Slipnet (D-019): spreading (D-013) and the label trickle (D-018)
        are computed from the current activations into a buffer and added together; then
        decay by depth (D-014) and the clip (D-015); then the lengths (D-011) and distances
        (D-012) are recomputed. Returns what moved, read against the state before."""
        activation_before = list(self.activation)
        lengths_before = list(self.effective)
        buffer = [0.0] * len(self.nodes)
        self._spread_into(buffer)
        self._trickle_into(buffer)
        for i in range(len(self.nodes)):
            self.activation[i] += buffer[i]
        self._decay()
        for i in range(len(self.nodes)):
            self.activation[i] = clip(self.activation[i])
        self._recompute_lengths()
        self._recompute_distances()
        self.updates += 1
        return UpdateReport(self.nodes, self.links, activation_before, lengths_before,
                            list(self.activation), list(self.effective))

    def _spread_into(self, buffer):
        """Spreading (D-013): activation "radiates" from the "warmer" node to its neighbour
        [F95 p. 60], "inversely proportional to the distance between the two concepts" [F95
        p. 59]: node A sends `spread_constant` x (a_A - a_B) / d(A, B) to every node B within
        `spread_radius` that is cooler than A, the total it sends capped at its excess over
        dormancy, since "the node cannot spread more activation than it possesses" [F95
        p. 59]. The sender keeps its activation. Only a node a pathway joins to A can be
        in reach (the others are at distance infinity, D-012), so A's neighbours are the
        nodes read, in definition order."""
        c, radius = self.config.spread_constant, self.config.spread_radius
        if c <= 0:
            return
        activation, distances, n = self.activation, self.distances, len(self.nodes)
        for i in range(n):
            a_i = activation[i]
            excess = a_i - ACTIVATION_MIN
            if excess <= 0:
                continue
            row = i * n                       # A's row of the distance matrix (D-012)
            receivers, amounts = [], []       # the cooler nodes in reach, and A's amount to each
            for j in self._neighbours[i]:
                a_j = activation[j]
                if a_j >= a_i:
                    continue
                d = distances[row + j]
                if d <= radius:
                    receivers.append(j)
                    amounts.append(c * (a_i - a_j) / d)
            total = 0.0
            for amount in amounts:
                total += amount
            scale = excess / total if total > excess else 1.0
            for j, amount in zip(receivers, amounts):
                buffer[j] += amount * scale

    def _trickle_into(self, buffer):
        """The label trickle (D-018): "whenever both of the concepts joined by a labeled link
        are active, a small amount of activation is also sent to the label node" [F95
        p. 62]: each label of a labeled link whose two concepts are both above
        `label_trickle_threshold` receives `label_trickle` times their mean activation.
        Identity links join one concept and never trickle."""
        rate, threshold = self.config.label_trickle, self.config.label_trickle_threshold
        if rate <= 0:
            return
        for link in self.links:
            if link.kind != D.LABELED:
                continue
            a, b = self.activation[self.index[link.a]], self.activation[self.index[link.b]]
            if a > threshold and b > threshold:
                for label in link.labels:
                    buffer[self.index[label]] += rate * (a + b) / 2

    def _decay(self):
        """Decay (D-014): "The greater the conceptual depth of a node, the less its activation
        will decay with each cycle." [F95 p. 62] Each node loses (a - 1) x (100 - depth) /
        100 x `decay_rate` of its excess over dormancy, so a concrete concept returns to
        dormancy quickly unless resupplied [F95 p. 56; F95 p. 62]."""
        rate = self.config.decay_rate
        for i in range(len(self.nodes)):
            excess = self.activation[i] - ACTIVATION_MIN
            self.activation[i] -= excess * (100 - self.depth[i]) / 100 * rate

    # ---- lengths and distances ----------------------------------------------------------

    def _recompute_lengths(self):
        """Label-controlled lengths (D-011): a labeled link's effective length is its
        intrinsic length times (1 - `label_shrink` x a_label / 150), with the most active of
        its labels; "This will then shrink the length of all of the links labeled by the
        label node." [F95 p. 62] ISA and identity links keep their lengths."""
        s = self.config.label_shrink
        for j, link in enumerate(self.links):
            if link.kind == D.LABELED and link.labels and s > 0:
                a_label = ACTIVATION_MIN
                for label in link.labels:
                    a_label = max(a_label, self.activation[self.index[label]])
                self.effective[j] = link.length * (1 - s * a_label / ACTIVATION_MAX)
            else:
                self.effective[j] = float(link.length)

    def _enumerate_paths(self) -> list:
        """Every pathway of at most `max_path_links` links between two distinct nodes, as
        tuples of link indices, grouped by pair (D-012): the direct links, and the two-link
        paths through a shared neighbour, "regardless of their component links' types" [F95
        p. 60]. Identity links join a node to itself and are not pathways. Computed once; the
        lengths are summed at each update."""
        by_node = {i: [] for i in range(len(self.nodes))}
        for j, link in enumerate(self.links):
            if link.kind == D.IDENTITY:
                continue
            ia, ib = self.index[link.a], self.index[link.b]
            by_node[ia].append((j, ib))
            by_node[ib].append((j, ia))
        paths = {}
        for i in range(len(self.nodes)):
            for j1, mid in by_node[i]:
                if mid > i:
                    paths.setdefault((i, mid), []).append((j1,))
                if self.config.max_path_links < 2:
                    continue
                for j2, end in by_node[mid]:
                    if end > i and j2 != j1:
                        paths.setdefault((i, end), []).append((j1, j2))
        return sorted(paths.items())

    def _enumerate_neighbours(self) -> list:
        """For every node, the nodes some pathway of `_paths` joins it to, in definition
        order (D-012): the only nodes at a finite distance from it, the ones spreading
        reads (D-013). Computed once."""
        neighbours = [[] for _ in self.nodes]
        for (i, j), _ in self._paths:
            neighbours[i].append(j)
            neighbours[j].append(i)
        return [sorted(own) for own in neighbours]

    def _recompute_distances(self):
        """The distance matrix (D-012): for every pair with a pathway, the parallel
        combination of the pathways' current lengths [F95 p. 61 (img)], `parallel_distance`
        summed in place; 0 on the diagonal and `math.inf` for the pairs no pathway joins.
        A flat list, d(i, j) at `i * n + j`, both ways round: `distance` and `_spread_into`
        read it by index."""
        n, effective = len(self.nodes), self.effective
        distances = [math.inf] * (n * n)
        for i in range(n):
            distances[i * n + i] = 0.0
        for (i, j), paths in self._paths:
            reciprocals = 0.0
            for path in paths:
                length = 0.0
                for link in path:
                    length += effective[link]
                reciprocals += 1.0 / length
            distances[i * n + j] = distances[j * n + i] = 1.0 / reciprocals
        self.distances = distances

    # ---- the top-down posting hook ------------------------------------------------------

    def top_down_postings(self, rng) -> list:
        """The codelets the Slipnet posts after an update (D-020): "An active concept in the
        Slipnet dispatches codelets to look for instances of itself in the Workspace." [F95
        p. 64], and the dispatch follows the concept's rise, "as the rate of group-finding
        falls (i.e., the rate of change of activation of 'group' decreases) and less
        activation gets sent to 'group' - and thus the activation of 'group' decays - fewer
        codelets will be sent out to look for groups" [F95 p. 64]. For every node of
        `top_down_table`, in the table's order, the rise of its activation since the last
        posting, divided by `top_down_rise`, is the number of codelets of its type posted:
        the integer part always, the remainder with one coin; a node that fell or held posts
        nothing and draws nothing. Returns (node, codelet type) pairs; the run loop posts
        them at the type's raw urgency and generation 0."""
        postings = []
        for node, codelet_type in self.config.top_down_table:
            i = self.index[node]
            rise = self.activation[i] - self._posted_at[i]
            self._posted_at[i] = self.activation[i]
            if rise <= 0:
                continue
            count = rise / self.config.top_down_rise
            whole = int(count)
            if count > whole and rng.coin(count - whole):
                whole += 1
            postings.extend([(node, codelet_type)] * whole)
        return postings
