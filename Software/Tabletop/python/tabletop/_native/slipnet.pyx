# cython: language_level=3
# cython: boundscheck=False, wraparound=False, cdivision=False
"""The compiled Slipnet: `tabletop/slipnet.py` with the activations, depths, effective
lengths and the distance matrix in C `double` arrays, the pathways and the neighbour lists
flattened into C index arrays, and C loops for spreading, the trickle, decay, the clip, the
lengths and the distances (D-111). It mirrors `slipnet.py` method for method, in the same
order of floating-point operations, so the two engines give the same activations (the
identity digests of D-108 are reproduced bit for bit, and `tests/test_native_slipnet.py`
checks a run of updates on both classes); the mechanism is specified by `slipnet.py` and its
decisions (D-011 to D-021, D-070), which are not repeated here.

What differs from the Python, and why (D-111): the per-update scratch buffers (the amounts a
node sends, the nodes that receive them, the spreading buffer) are allocated once with the
network instead of at every update; the paths are walked through flat `start`/`index`
arrays instead of nested tuples; the change report (`UpdateReport`) copies the activations
and lengths into C arrays and, as D-108 has it, works `changed` and `lengths` out with
Python's `round` only when they are first read. The reading interface is the Python's:
`activation`, `effective` and `distances` are writable views on the C arrays, `depth` the
list of the Python, `nodes`, `links`, `index`, `updates`, `events` and `config` as there.

The build embeds its stamp, flags, compiler and Cython version as C string macros (set by
`setup.py`; D-110), read below into module attributes for `tabletop.accel`.
"""

import math

from libc.stdlib cimport free, malloc
from libc.string cimport memcpy

from tabletop import events as EV
from tabletop import slipnet_def as D

cdef extern from *:
    """
    #ifndef TABLETOP_BUILD_STAMP
    #define TABLETOP_BUILD_STAMP "unstamped"
    #endif
    #ifndef TABLETOP_BUILD_FLAGS
    #define TABLETOP_BUILD_FLAGS ""
    #endif
    #ifndef TABLETOP_BUILD_COMPILER
    #define TABLETOP_BUILD_COMPILER ""
    #endif
    #ifndef TABLETOP_CYTHON_VERSION
    #define TABLETOP_CYTHON_VERSION ""
    #endif
    """
    const char *TABLETOP_BUILD_STAMP
    const char *TABLETOP_BUILD_FLAGS
    const char *TABLETOP_BUILD_COMPILER
    const char *TABLETOP_CYTHON_VERSION

cdef const char *_stamp = TABLETOP_BUILD_STAMP
cdef const char *_flags = TABLETOP_BUILD_FLAGS
cdef const char *_compiler = TABLETOP_BUILD_COMPILER
cdef const char *_cython = TABLETOP_CYTHON_VERSION
BUILD_STAMP = _stamp.decode("utf-8")
BUILD_FLAGS = _flags.decode("utf-8")
BUILD_COMPILER = _compiler.decode("utf-8")
CYTHON_VERSION = _cython.decode("utf-8")

cdef double ACT_MIN = 1.0      # slipnet.py: ACTIVATION_MIN, "Activation values range from 1 to 150." [F95 p. 95]
cdef double ACT_MAX = 150.0    # slipnet.py: ACTIVATION_MAX
ACTIVATION_MIN = ACT_MIN       # slipnet.py: ACTIVATION_MIN (D-015)
ACTIVATION_MAX = ACT_MAX       # slipnet.py: ACTIVATION_MAX


cdef inline double clip_c(double value) noexcept:
    # slipnet.py: clip, as C: min(ACTIVATION_MAX, max(ACTIVATION_MIN, value)) (D-015)
    cdef double v = value if value > ACT_MIN else ACT_MIN
    return v if v < ACT_MAX else ACT_MAX


def clip(value):
    """An activation kept in [1, 150] (D-015) [F95 p. 95]. Mirrors slipnet.py, clip."""
    return clip_c(float(value))


def parallel_distance(path_lengths):
    """The distance over several pathways, the reciprocal of the sum of the reciprocals
    [F95 p. 61 (img)]; `math.inf` with no pathway (D-012). Mirrors slipnet.py,
    parallel_distance, the same sum in the same order."""
    cdef double total = 0.0
    for length in path_lengths:
        total += 1.0 / length
    return 1.0 / total if total > 0 else math.inf


cdef double *dalloc(Py_ssize_t count) except NULL:
    # a C helper of slipnet.pyx (no counterpart in slipnet.py): a double array, or MemoryError
    cdef double *p = <double *> malloc(sizeof(double) * (count if count > 0 else 1))
    if p == NULL:
        raise MemoryError()
    return p


cdef Py_ssize_t *ialloc(Py_ssize_t count) except NULL:
    # a C helper of slipnet.pyx (no counterpart in slipnet.py): an index array, or MemoryError
    cdef Py_ssize_t *p = <Py_ssize_t *> malloc(sizeof(Py_ssize_t) * (count if count > 0 else 1))
    if p == NULL:
        raise MemoryError()
    return p


cdef class UpdateReport:
    """What one Slipnet update changed, for the `SlipnetUpdated` event (D-019). Mirrors
    slipnet.py, UpdateReport: the same fields (`nodes`, `links`, `activation_before`,
    `lengths_before`, `activation_after`, `lengths_after`) and the same `changed` and
    `lengths`, worked out with Python's `round` when first read (D-108); the four lists are
    held as C arrays, which `Slipnet.update` fills by one copy each."""

    cdef readonly tuple nodes
    cdef readonly tuple links
    cdef Py_ssize_t n, m
    cdef double *act_before
    cdef double *len_before
    cdef double *act_after
    cdef double *len_after
    cdef object _changed
    cdef object _lengths

    def __cinit__(self, *args, **kwargs):
        # slipnet.py: UpdateReport (the dataclass's fields; the arrays are set by `_fill`)
        self._changed = None
        self._lengths = None

    def __init__(self, nodes, links, activation_before, lengths_before, activation_after,
                 lengths_after):
        # mirrors slipnet.py, UpdateReport.__init__ (the dataclass's), from Python lists
        cdef Py_ssize_t i
        self._fill(tuple(nodes), tuple(links), len(activation_before), len(lengths_before))
        for i in range(self.n):
            self.act_before[i] = activation_before[i]
            self.act_after[i] = activation_after[i]
        for i in range(self.m):
            self.len_before[i] = lengths_before[i]
            self.len_after[i] = lengths_after[i]

    cdef void _fill(self, tuple nodes, tuple links, Py_ssize_t n, Py_ssize_t m) except *:
        # a C helper of slipnet.pyx (no counterpart in slipnet.py): allocate the four arrays
        self.nodes, self.links, self.n, self.m = nodes, links, n, m
        self.act_before = dalloc(n)
        self.act_after = dalloc(n)
        self.len_before = dalloc(m)
        self.len_after = dalloc(m)

    def __dealloc__(self):
        # slipnet.py has no counterpart: the C arrays of this UpdateReport are freed
        free(self.act_before)
        free(self.act_after)
        free(self.len_before)
        free(self.len_after)

    @property
    def activation_before(self):
        # slipnet.py: UpdateReport.activation_before, as a list
        return [self.act_before[i] for i in range(self.n)]

    @property
    def lengths_before(self):
        # slipnet.py: UpdateReport.lengths_before, as a list
        return [self.len_before[i] for i in range(self.m)]

    @property
    def activation_after(self):
        # slipnet.py: UpdateReport.activation_after, as a list
        return [self.act_after[i] for i in range(self.n)]

    @property
    def lengths_after(self):
        # slipnet.py: UpdateReport.lengths_after, as a list
        return [self.len_after[i] for i in range(self.m)]

    @property
    def changed(self):
        """The nodes whose activation moved to a tenth, (name, new value) (D-019). Mirrors
        slipnet.py, UpdateReport.changed: the same `round`, kept once computed (D-108)."""
        cdef Py_ssize_t i
        if self._changed is None:
            self._changed = [(self.nodes[i].name, round(self.act_after[i], 1))
                             for i in range(self.n)
                             if round(self.act_after[i], 1) != round(self.act_before[i], 1)]
        return self._changed

    @property
    def lengths(self):
        """The links whose effective length moved to a tenth, (name, new length) (D-019).
        Mirrors slipnet.py, UpdateReport.lengths, kept once computed (D-108)."""
        cdef Py_ssize_t i
        if self._lengths is None:
            self._lengths = [(self.links[i].name, round(self.len_after[i], 1))
                             for i in range(self.m)
                             if round(self.len_after[i], 1) != round(self.len_before[i], 1)]
        return self._lengths


cdef class Slipnet:
    """The concept network of one run (D-007 to D-020) [F95 pp. 55-63]. Mirrors slipnet.py,
    class Slipnet, with its state in C arrays (D-111): `activation`, `effective` and
    `distances` are views on them, `depth` the Python's list of ints; `nodes`, `links`,
    `index`, `updates`, `events` and `config` as in the Python."""

    cdef readonly object config
    cdef readonly tuple nodes
    cdef readonly tuple links
    cdef readonly dict index
    cdef readonly list depth
    cdef public object events
    cdef readonly Py_ssize_t updates
    cdef readonly list _paths          # slipnet.py: Slipnet._paths, the same list, for reading
    cdef readonly list _neighbours     # slipnet.py: Slipnet._neighbours, the same list
    cdef Py_ssize_t n, m
    cdef double *act                   # slipnet.py: Slipnet.activation
    cdef double *dep                   # slipnet.py: Slipnet.depth, as doubles for the decay
    cdef double *eff                   # slipnet.py: Slipnet.effective
    cdef double *dist                  # slipnet.py: Slipnet.distances, n x n flat
    cdef double *posted_at             # slipnet.py: Slipnet._posted_at
    cdef double *buf                   # the update's buffer, allocated once
    cdef double *amounts               # spreading's amounts, allocated once
    cdef Py_ssize_t *receivers         # spreading's receivers, allocated once
    cdef Py_ssize_t *nb_start          # the neighbour lists, flat: node i's are nb[nb_start[i]:nb_start[i+1]]
    cdef Py_ssize_t *nb
    cdef Py_ssize_t npairs             # the pathways, flat: pair k joins pair_i[k] and pair_j[k]
    cdef Py_ssize_t *pair_i            # by the paths pair_start[k]..pair_start[k+1], each path p
    cdef Py_ssize_t *pair_j            # being the links path_links[path_start[p]:path_start[p+1]]
    cdef Py_ssize_t *pair_start
    cdef Py_ssize_t *path_start
    cdef Py_ssize_t *path_links
    cdef int *link_labeled             # 1 for a labeled link (the trickle, the lengths)
    cdef Py_ssize_t *link_a            # the links' end nodes, by index
    cdef Py_ssize_t *link_b
    cdef double *link_length           # the intrinsic lengths
    cdef Py_ssize_t *label_start       # link j's labels, by node index: labels[label_start[j]:label_start[j+1]]
    cdef Py_ssize_t *labels
    cdef double c_spread, radius, trickle, threshold, decay_rate, shrink   # the Config's scalars
    cdef Py_ssize_t max_path_links

    def __init__(self, config, nodes=D.NODES, links=D.LINKS):
        # mirrors slipnet.py, Slipnet.__init__: the same fields, then the tables flattened
        cdef Py_ssize_t i
        self.config = config
        self.nodes = tuple(nodes)
        self.index = {node.name: i for i, node in enumerate(self.nodes)}
        self.links = tuple(links)
        self.n, self.m = len(self.nodes), len(self.links)
        self.depth = [config.depth_by_level[node.level - 1] for node in self.nodes]
        self.updates = 0
        self.events = EV.Publisher()               # the run's publisher, set by the World (D-065)
        self.c_spread, self.radius = config.spread_constant, config.spread_radius
        self.trickle, self.threshold = config.label_trickle, config.label_trickle_threshold
        self.decay_rate, self.shrink = config.decay_rate, config.label_shrink
        self.max_path_links = config.max_path_links
        self.act = dalloc(self.n)
        self.dep = dalloc(self.n)
        self.posted_at = dalloc(self.n)
        self.buf = dalloc(self.n)
        self.amounts = dalloc(self.n)
        self.receivers = ialloc(self.n)
        self.eff = dalloc(self.m)
        self.dist = dalloc(self.n * self.n)
        for i in range(self.n):
            self.act[i] = ACT_MIN
            self.posted_at[i] = ACT_MIN            # activations at the last posting (D-020)
            self.dep[i] = self.depth[i]
        self._flatten_links()
        self._paths = self._enumerate_paths()
        self._neighbours = self._enumerate_neighbours()
        self._flatten_paths()
        self._recompute_lengths()
        self._recompute_distances()

    def __dealloc__(self):
        # slipnet.py has no counterpart: the C arrays of this Slipnet are freed
        free(self.act); free(self.dep); free(self.posted_at); free(self.buf)
        free(self.amounts); free(self.receivers); free(self.eff); free(self.dist)
        free(self.nb_start); free(self.nb)
        free(self.pair_i); free(self.pair_j); free(self.pair_start)
        free(self.path_start); free(self.path_links)
        free(self.link_labeled); free(self.link_a); free(self.link_b); free(self.link_length)
        free(self.label_start); free(self.labels)

    cdef void _flatten_links(self) except *:
        # the link table of slipnet.py (Slipnet.links, read by name there) as C arrays
        cdef Py_ssize_t j, k, count = 0
        self.link_labeled = <int *> malloc(sizeof(int) * (self.m if self.m > 0 else 1))
        if self.link_labeled == NULL:
            raise MemoryError()
        self.link_a = ialloc(self.m)
        self.link_b = ialloc(self.m)
        self.link_length = dalloc(self.m)
        self.label_start = ialloc(self.m + 1)
        for j, link in enumerate(self.links):
            count += len(link.labels)
        self.labels = ialloc(count)
        k = 0
        for j, link in enumerate(self.links):
            self.link_labeled[j] = 1 if link.kind == D.LABELED else 0
            self.link_a[j] = self.index[link.a]
            self.link_b[j] = self.index[link.b]
            self.link_length[j] = float(link.length)
            self.label_start[j] = k
            for label in link.labels:
                self.labels[k] = self.index[label]
                k += 1
        self.label_start[self.m] = k

    cdef void _flatten_paths(self) except *:
        # the pathways and neighbours of slipnet.py (Slipnet._paths, Slipnet._neighbours) as C arrays
        cdef Py_ssize_t k, p, q, npaths = 0, nlinks = 0, nnb = 0
        for (i, j), paths in self._paths:
            npaths += len(paths)
            for path in paths:
                nlinks += len(path)
        for own in self._neighbours:
            nnb += len(own)
        self.npairs = len(self._paths)
        self.pair_i = ialloc(self.npairs)
        self.pair_j = ialloc(self.npairs)
        self.pair_start = ialloc(self.npairs + 1)
        self.path_start = ialloc(npaths + 1)
        self.path_links = ialloc(nlinks)
        self.nb_start = ialloc(self.n + 1)
        self.nb = ialloc(nnb)
        p = q = 0
        for k, ((i, j), paths) in enumerate(self._paths):
            self.pair_i[k], self.pair_j[k], self.pair_start[k] = i, j, p
            for path in paths:
                self.path_start[p] = q
                for link in path:
                    self.path_links[q] = link
                    q += 1
                p += 1
        self.pair_start[self.npairs] = p
        self.path_start[npaths] = q
        q = 0
        for k, own in enumerate(self._neighbours):
            self.nb_start[k] = q
            for j in own:
                self.nb[q] = j
                q += 1
        self.nb_start[self.n] = q

    # ---- reading ------------------------------------------------------------------------

    @property
    def activation(self):
        # slipnet.py: Slipnet.activation, the list; here a writable view on the C array
        return <double[:self.n]> self.act

    @property
    def effective(self):
        # slipnet.py: Slipnet.effective, the list; here a writable view on the C array
        return <double[:self.m]> self.eff

    @property
    def distances(self):
        # slipnet.py: Slipnet.distances, the flat n x n list; here a view on the C array
        return <double[:self.n * self.n]> self.dist

    @property
    def _posted_at(self):
        # slipnet.py: Slipnet._posted_at, the list; here a view on the C array
        return <double[:self.n]> self.posted_at

    def activation_of(self, name):
        """A node's activation, in [1, 150] (D-015). Mirrors slipnet.py, Slipnet.activation_of."""
        return self.act[<Py_ssize_t> self.index[name]]

    def activations(self):
        """Every node's activation as (name, value), in definition order (D-015). Mirrors
        slipnet.py, Slipnet.activations."""
        cdef Py_ssize_t i
        return [(node.name, self.act[i]) for i, node in enumerate(self.nodes)]

    def distance(self, a, b):
        """The conceptual distance between two nodes [F95 p. 61 (img)]; 0 to itself,
        `math.inf` when no pathway joins them (D-012). Mirrors slipnet.py, Slipnet.distance."""
        return self.dist[<Py_ssize_t> self.index[a] * self.n + <Py_ssize_t> self.index[b]]

    def effective_length(self, link):
        """A link's current length (D-011), the link found by identity in the table. Mirrors
        slipnet.py, Slipnet.effective_length."""
        return self.eff[next(i for i, own in enumerate(self.links) if own is link)]

    def links_of(self, name):
        """Every link of this Slipnet's table that touches a node (D-009). Mirrors
        slipnet.py, Slipnet.links_of."""
        return D.links_of(name, self.links)

    def categories_of(self, type_node):
        """The categories a type ISA, the nearest first (D-009). Mirrors slipnet.py,
        Slipnet.categories_of."""
        return D.categories_of(type_node, self.links)

    def labels_between(self, a, b):
        """The labels of the link joining two nodes, `()` when none or ISA [F95 p. 62]
        (D-009, D-030). Mirrors slipnet.py, Slipnet.labels_between."""
        return D.labels_between(a, b, self.links)

    def neighbours(self, name):
        """The nodes within `spread_radius` of a node with their distances, nearest first
        (D-013), for the network view. Mirrors slipnet.py, Slipnet.neighbours."""
        pairs = [(other.name, self.distance(name, other.name)) for other in self.nodes
                 if other.name != name]
        return sorted([p for p in pairs if p[1] <= self.config.spread_radius],
                      key=lambda p: p[1])

    def level(self, name):
        """A node's level of abstractness, 1 to 3 [F95 pp. 61-62] (D-008). Mirrors
        slipnet.py, Slipnet.level."""
        return self.nodes[self.index[name]].level

    # ---- changing activations -----------------------------------------------------------

    def set_activation(self, name, value):
        """Set a node's activation outright, clipped (D-015, D-017, D-021). Mirrors
        slipnet.py, Slipnet.set_activation."""
        cdef Py_ssize_t i = self.index[name]
        self.act[i] = clip_c(float(value))
        return self.act[i]

    def jolt(self, name, amount):
        """Add a jolt of activation to a node [F95 p. 97], clipped at once (D-015, D-016),
        and record it. Mirrors slipnet.py, Slipnet.jolt."""
        cdef Py_ssize_t i = self.index[name]
        self.act[i] = clip_c(self.act[i] + <double> amount)
        self.events.record(EV.NodeJolted, node=name, amount=float(amount), by=self.events.actor,
                           activation=self.act[i])
        return self.act[i]

    def reset(self, names):
        """Set the named nodes to 1 [F95 p. 87] and their posting levels with them (D-020,
        D-021, D-070). Mirrors slipnet.py, Slipnet.reset."""
        for name in names:
            self.restore(name, ACT_MIN)
        return list(names)

    def restore(self, name, value):
        """Set a node's activation to a reconstructed level, clipped, and its posting level
        to the same (D-020, D-021). Mirrors slipnet.py, Slipnet.restore."""
        cdef Py_ssize_t i = self.index[name]
        self.act[i] = clip_c(float(value))
        self.posted_at[i] = self.act[i]
        return self.act[i]

    def spread_step_into(self, names):
        """What one spreading step (D-013) would send into the named nodes [F95 p. 89]
        (D-021); changes nothing. Mirrors slipnet.py, Slipnet.spread_step_into."""
        cdef Py_ssize_t i
        for i in range(self.n):
            self.buf[i] = 0.0
        self._spread_into(self.buf)
        return [(name, self.buf[<Py_ssize_t> self.index[name]]) for name in names]

    def start(self, touched_type):
        """The initial state (D-017) [F95 p. 66; F95 p. 97]: every node at 1, "opposite" and
        the touched type raised, then one update; the posting levels start from the result
        (D-020). Mirrors slipnet.py, Slipnet.start, the first report built the same way."""
        cdef Py_ssize_t i
        cdef UpdateReport report, first
        for i in range(self.n):
            self.act[i] = ACT_MIN
        self.set_activation("opposite", self.config.initial_opposite)
        self.set_activation(D.node_for_type(touched_type), self.config.initial_touched_type)
        report = self.update()
        memcpy(self.posted_at, self.act, sizeof(double) * self.n)
        first = UpdateReport.__new__(UpdateReport)
        first._fill(self.nodes, self.links, self.n, self.m)
        for i in range(self.n):
            first.act_before[i] = ACT_MIN
        memcpy(first.len_before, report.len_before, sizeof(double) * self.m)
        memcpy(first.act_after, report.act_after, sizeof(double) * self.n)
        memcpy(first.len_after, report.len_after, sizeof(double) * self.m)
        return first

    # ---- the update ---------------------------------------------------------------------

    def update(self):
        """One cycle of the Slipnet (D-019): spreading (D-013) and the trickle (D-018) into
        a buffer, added together; decay (D-014) and the clip (D-015); the lengths (D-011)
        and distances (D-012). Mirrors slipnet.py, Slipnet.update, operation for operation."""
        cdef Py_ssize_t i
        cdef UpdateReport report = UpdateReport.__new__(UpdateReport)
        report._fill(self.nodes, self.links, self.n, self.m)
        memcpy(report.act_before, self.act, sizeof(double) * self.n)
        memcpy(report.len_before, self.eff, sizeof(double) * self.m)
        for i in range(self.n):
            self.buf[i] = 0.0
        self._spread_into(self.buf)
        self._trickle_into(self.buf)
        for i in range(self.n):
            self.act[i] += self.buf[i]
        self._decay()
        for i in range(self.n):
            self.act[i] = clip_c(self.act[i])
        self._recompute_lengths()
        self._recompute_distances()
        self.updates += 1
        memcpy(report.act_after, self.act, sizeof(double) * self.n)
        memcpy(report.len_after, self.eff, sizeof(double) * self.m)
        return report

    cdef void _spread_into(self, double *buffer) noexcept:
        # mirrors slipnet.py, Slipnet._spread_into (D-013): the same amounts in the same order,
        # the sender's neighbours read from the flat lists, the scratch arrays reused
        cdef Py_ssize_t i, j, p, q, k, row, n = self.n
        cdef double a_i, a_j, d, excess, total, scale
        cdef double c = self.c_spread, radius = self.radius
        if c <= 0:
            return
        for i in range(n):
            a_i = self.act[i]
            excess = a_i - ACT_MIN
            if excess <= 0:
                continue
            row = i * n                       # A's row of the distance matrix (D-012)
            k = 0                             # the cooler nodes in reach, and A's amount to each
            for p in range(self.nb_start[i], self.nb_start[i + 1]):
                j = self.nb[p]
                a_j = self.act[j]
                if a_j >= a_i:
                    continue
                d = self.dist[row + j]
                if d <= radius:
                    self.receivers[k] = j
                    self.amounts[k] = c * (a_i - a_j) / d
                    k += 1
            total = 0.0
            for q in range(k):
                total += self.amounts[q]
            scale = excess / total if total > excess else 1.0
            for q in range(k):
                buffer[self.receivers[q]] += self.amounts[q] * scale

    cdef void _trickle_into(self, double *buffer) noexcept:
        # mirrors slipnet.py, Slipnet._trickle_into (D-018): links in table order, labels in theirs
        cdef Py_ssize_t j, q
        cdef double a, b, rate = self.trickle, threshold = self.threshold
        if rate <= 0:
            return
        for j in range(self.m):
            if not self.link_labeled[j]:
                continue
            a, b = self.act[self.link_a[j]], self.act[self.link_b[j]]
            if a > threshold and b > threshold:
                for q in range(self.label_start[j], self.label_start[j + 1]):
                    buffer[self.labels[q]] += rate * (a + b) / 2

    cdef void _decay(self) noexcept:
        # mirrors slipnet.py, Slipnet._decay (D-014) [F95 p. 62]: (a - 1)(100 - depth)/100 x rate
        cdef Py_ssize_t i
        cdef double excess, rate = self.decay_rate
        for i in range(self.n):
            excess = self.act[i] - ACT_MIN
            self.act[i] -= excess * (100 - self.dep[i]) / 100 * rate

    # ---- lengths and distances ----------------------------------------------------------

    cdef void _recompute_lengths(self) noexcept:
        # mirrors slipnet.py, Slipnet._recompute_lengths (D-011): the most active label shrinks
        cdef Py_ssize_t j, q
        cdef double a_label, s = self.shrink
        for j in range(self.m):
            if self.link_labeled[j] and self.label_start[j + 1] > self.label_start[j] and s > 0:
                a_label = ACT_MIN
                for q in range(self.label_start[j], self.label_start[j + 1]):
                    if self.act[self.labels[q]] > a_label:   # max(a_label, a), the first kept on a tie
                        a_label = self.act[self.labels[q]]
                self.eff[j] = self.link_length[j] * (1 - s * a_label / ACT_MAX)
            else:
                self.eff[j] = self.link_length[j]

    def _enumerate_paths(self):
        """Every pathway of at most `max_path_links` links between two distinct nodes, as
        tuples of link indices, grouped by pair (D-012) [F95 p. 60]. Mirrors slipnet.py,
        Slipnet._enumerate_paths, the same list in the same order."""
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

    def _enumerate_neighbours(self):
        """For every node, the nodes some pathway joins it to, in definition order (D-012),
        the ones spreading reads (D-013). Mirrors slipnet.py, Slipnet._enumerate_neighbours."""
        neighbours = [[] for _ in self.nodes]
        for (i, j), _ in self._paths:
            neighbours[i].append(j)
            neighbours[j].append(i)
        return [sorted(own) for own in neighbours]

    cdef void _recompute_distances(self) except *:
        # mirrors slipnet.py, Slipnet._recompute_distances (D-012): for every pair with a
        # pathway, the reciprocals summed path by path, link by link, in the Python's order
        cdef Py_ssize_t i, j, k, p, q, n = self.n
        cdef double reciprocals, length, inf = math.inf
        for i in range(n * n):
            self.dist[i] = inf
        for i in range(n):
            self.dist[i * n + i] = 0.0
        for k in range(self.npairs):
            i, j = self.pair_i[k], self.pair_j[k]
            reciprocals = 0.0
            for p in range(self.pair_start[k], self.pair_start[k + 1]):
                length = 0.0
                for q in range(self.path_start[p], self.path_start[p + 1]):
                    length += self.eff[self.path_links[q]]
                reciprocals += 1.0 / length
            self.dist[i * n + j] = self.dist[j * n + i] = 1.0 / reciprocals

    # ---- the top-down posting hook ------------------------------------------------------

    def top_down_postings(self, rng):
        """The codelets the Slipnet posts after an update (D-020) [F95 p. 64]: for every node
        of `top_down_table`, its rise since the last posting over `top_down_rise`, the whole
        part always and the remainder with one coin. Mirrors slipnet.py,
        Slipnet.top_down_postings, draw for draw."""
        cdef Py_ssize_t i
        cdef double rise, count
        cdef long long whole
        postings = []
        top_down_rise = self.config.top_down_rise
        for node, codelet_type in self.config.top_down_table:
            i = self.index[node]
            rise = self.act[i] - self.posted_at[i]
            self.posted_at[i] = self.act[i]
            if rise <= 0:
                continue
            count = rise / top_down_rise
            whole = <long long> count
            if count > whole and rng.coin(count - whole):
                whole += 1
            postings.extend([(node, codelet_type)] * whole)
        return postings
