"""The sequences the sources name, with their continuations, for the bench's `dozen` command
[M86 pp. 6-7, 38, 88, 145-147, 150; FCCA pp. 53-54] (DECISION D-070).

The thesis lists "a dozen sequences" presented to "twenty-five students at Blackburn College"
[M86 p. 6], the Blackburn dozen, in raw and parsed form [M86 pp. 6-7], and reports the
program's performance on each [M86 pp. 145-147]: "the current Seek-Whence program can solve
only three of the Blackburn dozen -- problems 2, 8, and 12" [M86 p. 147]. It names three
more sequences the system can solve beside them [M86 pp. 145-146], the nine sequences solved
[M86 p. 88], ten typical problems of the domain [M86 p. 38] and three unconquered sequences
[M86 p. 150]; FCCA adds Hofstadter's sampler of fifteen sequences of the domain
[FCCA pp. 53-54]. Each is a `Sequence` here, with the rule that continues it (the one the
parsed form prints, [M86 p. 7]: "the solver must complete the current template and fill out
the next one") so that the bench can judge a guess against the true continuation
(DECISION D-070), the number of terms the source prints, its citation, Meredith's reported
result where the thesis gives one, and her remarks in brief. Every page cited was read on
the page image (items 01, 04 and 22). Nothing here is part of the engine: the engine never
imports this module.
"""

from dataclasses import dataclass
from itertools import count


@dataclass(frozen=True)
class Sequence:
    """One sequence of the sources (DECISION D-070): `name` (`blackburn-1`, `p88-3`, ...),
    `group` (`blackburn`, `p146`, `p88`, `p38`, `p150`, `fcca`), `terms` (the continuation,
    `LENGTH` terms or fewer when the rule ends), `shown` (how many terms the source prints),
    `parsed` (the parsed form as the source prints or describes it), `tag` (the citation),
    `reported` (`solved`, `unsolved`, `not attempted` or `` when the source gives no
    verdict), `remarks` (the source's words in brief) and `also` (other places the same
    sequence is listed)."""
    name: str
    group: str
    terms: tuple
    shown: int
    parsed: str
    tag: str
    reported: str = ""
    remarks: str = ""
    also: str = ""

    def prefix(self, n: int) -> tuple:
        """The first n terms (DECISION D-070)."""
        return self.terms[:n]

    @property
    def printed(self) -> str:
        """The terms the source prints, as text (DECISION D-070)."""
        return " ".join(str(t) for t in self.terms[:self.shown])


# ---- the rules [M86 pp. 6-7, 38, 88, 150; FCCA pp. 53-54] ---------------------------------------

def cycle(*block):
    """A cycle of a block of terms: `3 7 3 7 ...` [M86 p. 88]."""
    while True:
        yield from block


def countup(start: int = 1):
    """`1 2 3 4 ...` [M86 p. 88], Blackburn 2 [M86 p. 7]."""
    yield from count(start)


def scales(first: int = 1):
    """Blackburn 1, `1 * 1 2 * 1 2 3 * 1 2 3 4 * ...` [M86 p. 7]; with `first` 2 the
    running example `1 2 * 1 2 3 * 1 2 3 4 ...` [M86 p. 88]."""
    for n in count(first):
        yield from range(1, n + 1)


def theme_song():
    """Blackburn 3, `2 1 2 * 2 2 2 * 2 3 2 * 2 4 2 * ...` [M86 p. 7]."""
    for n in count(1):
        yield from (2, n, 2)


def n_times_n(start: int = 1):
    """Blackburn 4, `1 * 2 2 * 3 3 3 * 4 4 4 4 * ...` [M86 p. 7], from `start`."""
    for n in count(start):
        yield from [n] * n


def n_times_k(k: int):
    """`1 1 1 2 2 2 3 3 3 ...` [M86 p. 88]: each integer k times."""
    for n in count(1):
        yield from [n] * k


def n_times_n_plus_1():
    """`1 1 2 2 2 3 3 3 3 ...` [M86 p. 146]: each integer n, n + 1 times."""
    for n in count(1):
        yield from [n] * (n + 1)


def two_n():
    """Blackburn 6, `2 1 * 2 2 * 2 3 * 2 4 * 2 5 ...` [M86 p. 7]."""
    for n in count(1):
        yield from (2, n)


def two_three_n():
    """Blackburn 7, `2 3 (1) * 2 3 (2 2) * 2 3 (3 3 3) * ...` [M86 p. 7]."""
    for n in count(1):
        yield from (2, 3)
        yield from [n] * n


def n_n_plus_1():
    """Blackburn 8, `1 2 * 2 3 * 3 4 * 4 5 * 5 6 ...` [M86 p. 7]."""
    for n in count(1):
        yield from (n, n + 1)


def pairs_of_lengths():
    """Blackburn 9, `1 * 2 * 3 3 * 4 4 * 5 5 5 * 6 6 6 ...` [M86 p. 7]: each length
    twice."""
    for n in count(1):
        yield from [n] * ((n + 1) // 2)


def nine_n():
    """Blackburn 10, `9 1 * 9 2 * 9 3 * 9 4 ...` [M86 p. 7]."""
    for n in count(1):
        yield from (9, n)


def both_ends():
    """Blackburn 11, `1 8 1 * (2 1) 8 (1 2) * (3 2 1) 8 (1 2 3) * ...` [M86 p. 7]."""
    for n in count(1):
        yield from range(n, 0, -1)
        yield 8
        yield from range(1, n + 1)


def triples_up(start: int):
    """`16 15 14 17 16 15 18 17 16 ...` [M86 p. 88]: descending triples that climb."""
    for k in count(0):
        yield from (start + k, start + k - 1, start + k - 2)


def triples_down(start: int):
    """`16 15 14 15 14 13 14 13 12 ...` [M86 p. 88], "as well as possible, given a
    non-infinite pattern": descending triples that sink, until the pattern reaches 0 (the
    domain has no negative integers [M86 p. 38])."""
    for k in count(0):
        if start - k - 2 < 0:
            return
        yield from (start - k, start - k - 1, start - k - 2)


def one_then_zeros():
    """`1 0 1 0 0 1 0 0 0 1 ...` [M86 p. 38]."""
    for n in count(1):
        yield 1
        yield from [0] * n


def pairs_and(marker: int):
    """`1 2 8 3 4 8 5 6 8 ...` [M86 p. 38] and `1 2 10 3 4 10 5 6 10 ...` [M86 p. 150]:
    pairs of a countup with a marker after each."""
    for n in count(0):
        yield from (2 * n + 1, 2 * n + 2, marker)


def runs_and_eight():
    """`1 2 8 3 4 5 8 6 7 8 9 8 ...` [M86 p. 38]: the countup goes on through runs that
    lengthen, an 8 after each."""
    counter = count(1)
    for length in count(2):
        for _ in range(length):
            yield next(counter)
        yield 8


def one_n_two():
    """`1 1 2 1 2 2 1 3 2 ...` [M86 p. 38]: `(1 n 2)`."""
    for n in count(1):
        yield from (1, n, 2)


def one_n_one():
    """`1 1 1  1 2 1  1 3 1 ...` [M86 p. 150]."""
    for n in count(1):
        yield from (1, n, 1)


def alternate_copies():
    """FCCA's `1 0 0 1 1 1 0 0 0 0 1 1 1 1 1 ...`, "n copies of either 1 or 0,
    alternately" [FCCA p. 53]."""
    for n in count(1):
        yield from [1 if n % 2 else 0] * n


def every_fifth_five():
    """The same "with every fifth term replaced by '5'" [FCCA p. 53]."""
    for i, term in enumerate(alternate_copies(), 1):
        yield 5 if i % 5 == 0 else term


def interleaved():
    """"interleaving the first two sequences" of the sampler [FCCA p. 53]: the alternate
    copies and the hemiolic bi-cycle, the copies first."""
    a, b = alternate_copies(), cycle(2, 2, 2, 3, 3)
    while True:
        yield next(a)
        yield next(b)


def scales_minus_one():
    """`2 1 3 1 2 4 1 2 3 5 ...`, "ascending scales with the next-to-last note omitted"
    [FCCA p. 54], n from 2."""
    for n in count(2):
        yield from range(1, n - 1)
        yield n


def descending_scales():
    """`1 2 1 3 2 1 4 3 2 1 ...`, "descending scales of length n" [FCCA p. 54]."""
    for n in count(1):
        yield from range(n, 0, -1)


def sawtooth_plateaus():
    """`0 0 0 1 1 2 4 5 5 6 6 6 ...`: "the multiplicity of each pitch being dictated by
    the 'sawtooth sequence'" [FCCA p. 54]."""
    for pitch, times in enumerate(cycle(3, 2, 1, 0, 1, 2)):
        yield from [pitch] * times


def mountain_chain():
    """`1 1 2 1 1 2 3 2 1 ...`: in Hofstadter's words, a mountain rising from 1 to height
    n and then redescending symmetrically to 1 [FCCA p. 54] (a paraphrase: the text layer
    of the page does not match the printed words closely enough for the checker)."""
    for n in count(1):
        yield from range(1, n + 1)
        yield from range(n - 1, 0, -1)


def continuation(rule, n: int) -> tuple:
    """The first n terms a rule gives (fewer when the rule ends, as `triples_down` does)
    (DECISION D-070)."""
    out = []
    for term in rule:
        if len(out) >= n:
            break
        out.append(term)
    return tuple(out)


# ---- the catalogue [M86 pp. 6-7, 38, 88, 145-147, 150; FCCA pp. 53-54] -----------------------------

# How many terms of each continuation are kept: enough to judge a guess beyond the terms
# the source prints (DECISION D-070).
LENGTH = 40


def _seq(name, group, rule, shown, parsed, tag, reported="", remarks="", also=""):
    return Sequence(name, group, continuation(rule, LENGTH), shown, parsed, tag, reported,
                    remarks, also)


# The Blackburn dozen, raw and parsed [M86 pp. 6-7], with the reported results
# [M86 pp. 145-147] (`tests/data/expectations.json` holds the same data, transcribed in
# item 04; the remarks here are in brief).
BLACKBURN = (
    _seq("blackburn-1", "blackburn", scales(), 15,
         "1 * 1 2 * 1 2 3 * 1 2 3 4 * 1 2 3 4 5 * ...", "[M86 pp. 6-7, 145]", "unsolved",
         "thrashes hopelessly; the initial C-group interferes with finding the lengthening "
         "S-groups, which reach the template level but not a hypothesis [M86 p. 145]",
         also="FCCA's ascending scales [FCCA p. 53]"),
    _seq("blackburn-2", "blackburn", countup(1), 7,
         "1 * 2 * 3 * 4 * 5 * 6 * 7 ...", "[M86 pp. 6-7, 145]", "solved",
         "solved quite readily, in just under one minute [M86 p. 145]",
         also="`1 2 3 4 ...` [M86 p. 88]"),
    _seq("blackburn-3", "blackburn", theme_song(), 15,
         "2 1 2 * 2 2 2 * 2 3 2 * 2 4 2 * 2 5 2 ...", "[M86 pp. 6-7, 145-146]", "not attempted",
         "hopeless as yet, not even attempted: far too much interference, terms with "
         "multiple potential roles; a distant goal [M86 pp. 145-146]",
         also="the unconquered `2 1 2 2 2 2 2 3 2 2 4 2 ...` [M86 p. 150]; the project's theme "
              "song [FCCA pp. 51-52]"),
    _seq("blackburn-4", "blackburn", n_times_n(1), 10,
         "1 * 2 2 * 3 3 3 * 4 4 4 4 * ...", "[M86 pp. 6-7, 146]", "unsolved",
         "the initial S-groups (1 2) and (2 3) interfere with the budding C-group notion; the "
         "correct notion is discovered but cannot break through into a hypothesis [M86 p. 146]",
         also="the typical problem `1 2 2 3 3 3 ...` [M86 p. 38]"),
    _seq("blackburn-5", "blackburn", cycle(1, 8, 5, 8), 12,
         "1 8 5 8 * 1 8 5 8 * 1 8 5 8 * ...", "[M86 pp. 6-7, 146]", "unsolved",
         "finds the Y-group `1 8 5 8 1` and doggedly clings to it; stopped after fifteen "
         "minutes [M86 p. 146]"),
    _seq("blackburn-6", "blackburn", two_n(), 10,
         "2 1 * 2 2 * 2 3 * 2 4 * 2 5 ...", "[M86 pp. 6-7, 146]", "unsolved",
         "too much interference combined with an alternation of terms; beyond the current "
         "system [M86 p. 146]"),
    _seq("blackburn-7", "blackburn", two_three_n(), 18,
         "2 3 (1) * 2 3 (2 2) * 2 3 (3 3 3) * 2 3 (4 4 4 4) * ...", "[M86 pp. 6-7, 146]",
         "unsolved",
         "far beyond the current system: interference, interleaving and growing group "
         "lengths [M86 p. 146]"),
    _seq("blackburn-8", "blackburn", n_n_plus_1(), 10,
         "1 2 * 2 3 * 3 4 * 4 5 * 5 6 ...", "[M86 pp. 6-7, 146]", "solved",
         "solved within three minutes [M86 p. 146]",
         also="FCCA's `[n n+1]` template [FCCA p. 53]"),
    _seq("blackburn-9", "blackburn", pairs_of_lengths(), 12,
         "1 * 2 * 3 3 * 4 4 * 5 5 5 * 6 6 6 ...", "[M86 pp. 6-7, 146-147]", "unsolved",
         "difficult for a while yet: the subtle pattern of growth in group lengths taxes the "
         "representation; group lengths would have to be manifestations [M86 pp. 146-147]"),
    _seq("blackburn-10", "blackburn", nine_n(), 8,
         "9 1 * 9 2 * 9 3 * 9 4 ...", "[M86 pp. 6-7, 147]", "unsolved",
         "the only problem is the alternation of terms of which at least one is "
         "non-constant; with slight extension it should solve this one [M86 p. 147]"),
    _seq("blackburn-11", "blackburn", both_ends(), 24,
         "1 8 1 * (2 1) 8 (1 2) * (3 2 1) 8 (1 2 3) * ...", "[M86 pp. 6-7, 147]", "unsolved",
         "hard: interference between the groups, which grow at both ends; the system gets "
         "bogged down in spurious relationships [M86 p. 147]"),
    _seq("blackburn-12", "blackburn", cycle(1, 8, 5, 5, 8, 1), 12,
         "1 8 5 5 8 1 * 1 8 5 5 8 1 * ...", "[M86 pp. 6-7, 147]", "solved",
         "solved, but can take up to twenty minutes; often finds the Y-group "
         "`(1 8 (5 (5 8 1 1 8 5) 5) 8 1)` rather than the one we would like [M86 p. 147]"),
)

# The three sequences the thesis names as solved beside the dozen [M86 pp. 145-146].
RELATED = (
    _seq("p146-1", "p146", n_times_n(2), 9,
         "2 2 * 3 3 3 * 4 4 4 4 * ...", "[M86 p. 146]", "solved",
         "Blackburn 4 without its first term: the system can solve it [M86 p. 146]"),
    _seq("p146-2", "p146", n_times_n_plus_1(), 9,
         "1 1 * 2 2 2 * 3 3 3 3 * ...", "[M86 p. 146]", "solved",
         "each integer one more time than its value, `(C-group (Countup 1) (Countup 2))`: "
         "the system can solve it [M86 p. 146]"),
    _seq("p146-3", "p146", cycle(1, 8, 5, 8, 1), 10,
         "1 8 5 8 1 * 1 8 5 8 1 * ...", "[M86 p. 146]", "solved",
         "Blackburn 5's Y-group repeated: the system can solve it [M86 p. 146]"),
)

# The nine sequences solved [M86 p. 88]; the three appendix runs are among them.
P88 = (
    _seq("p88-1", "p88", cycle(1), 3, "1 1 1 ...", "[M86 p. 88]", "solved"),
    _seq("p88-2", "p88", countup(1), 4, "1 2 3 4 ...", "[M86 p. 88]", "solved",
         also="Blackburn 2"),
    _seq("p88-3", "p88", n_times_k(3), 9, "1 1 1 2 2 2 3 3 3 ...", "[M86 p. 88]", "solved"),
    _seq("p88-4", "p88", cycle(3, 4), 6, "3 4 3 4 3 4 ...", "[M86 p. 88]", "solved"),
    _seq("p88-5", "p88", cycle(3, 7), 6, "3 7 3 7 3 7 ...", "[M86 p. 88]", "solved",
         also="appendix run 1 [M86 pp. 164-166]; a typical problem [M86 p. 38]"),
    _seq("p88-6", "p88", cycle(3, 7, 3), 9, "3 7 3 3 7 3 3 7 3 ...", "[M86 p. 88]", "solved",
         also="appendix run 2 [M86 pp. 167-169]"),
    _seq("p88-7", "p88", triples_up(16), 9, "16 15 14 17 16 15 18 17 16 ...", "[M86 p. 88]",
         "solved"),
    _seq("p88-8", "p88", triples_down(16), 9, "16 15 14 15 14 13 14 13 12 ...", "[M86 p. 88]",
         "solved", "as well as possible, given a non-infinite pattern [M86 p. 88]"),
    _seq("p88-9", "p88", scales(2), 9, "1 2 1 2 3 1 2 3 4 ...", "[M86 p. 88]", "solved",
         also="appendix run 3 [M86 pp. 170-179]; readily solved [M86 p. 145]; a typical "
              "problem [M86 p. 38]"),
)

# The typical problems [M86 p. 38] not listed above (the thesis gives no verdict on them).
P38 = (
    _seq("p38-1", "p38", cycle(2), 3, "2 2 2 ...", "[M86 p. 38]"),
    _seq("p38-2", "p38", n_times_k(2), 6, "1 1 2 2 3 3 ...", "[M86 p. 38]"),
    _seq("p38-5", "p38", one_then_zeros(), 10, "1 0 1 0 0 1 0 0 0 1 ...", "[M86 p. 38]"),
    _seq("p38-6", "p38", pairs_and(8), 9, "1 2 8 3 4 8 5 6 8 ...", "[M86 p. 38]"),
    _seq("p38-7", "p38", runs_and_eight(), 12, "1 2 8 3 4 5 8 6 7 8 9 8 ...", "[M86 p. 38]"),
    _seq("p38-9", "p38", cycle(8, 0, 8), 9, "8 0 8 8 0 8 8 0 8 ...", "[M86 p. 38]"),
    _seq("p38-10", "p38", one_n_two(), 9, "1 1 2 1 2 2 1 3 2 ...", "[M86 p. 38]"),
)

# The unconquered sequences [M86 p. 150] not listed above.
P150 = (
    _seq("p150-1", "p150", pairs_and(10), 9, "1 2 10 3 4 10 5 6 10 ...", "[M86 p. 150]",
         "unsolved",
         "independent interleaved sequences whose components are more complicated than "
         "constants [M86 p. 150]"),
    _seq("p150-3", "p150", one_n_one(), 9, "1 1 1  1 2 1  1 3 1 ...", "[M86 p. 150]",
         "unsolved", "a good deal of interference [M86 p. 150]"),
)

# Hofstadter's sampler [FCCA pp. 53-54] (SPEC 3.4), the ascending scales and `[n n+1]`
# being Blackburn 1 and 8 above; the pre-parsed "doubler/singler spliceroo" is left out, its
# continuation not being given.
FCCA = (
    _seq("fcca-1", "fcca", cycle(2, 2, 2, 3, 3), 15, "a trio of 2's alternating with a duo of 3's",
         "[FCCA p. 53]"),
    _seq("fcca-2", "fcca", alternate_copies(), 15, "n copies of either 1 or 0, alternately",
         "[FCCA p. 53]"),
    _seq("fcca-3", "fcca", every_fifth_five(), 20, "the same, every fifth term replaced by 5",
         "[FCCA p. 53]"),
    _seq("fcca-4", "fcca", interleaved(), 20, "interleaving the first two sequences",
         "[FCCA p. 53]"),
    _seq("fcca-7", "fcca", scales_minus_one(), 15,
         "ascending scales with the next-to-last note omitted", "[FCCA p. 54]"),
    _seq("fcca-8", "fcca", descending_scales(), 15, "descending scales, [n n-1 ... 2 1]",
         "[FCCA p. 54]"),
    _seq("fcca-9", "fcca", cycle(3, 2, 1, 0, 1, 2), 13, "sawtooth, [3 2 1 0 1 2]", "[FCCA p. 54]"),
    _seq("fcca-10", "fcca", sawtooth_plateaus(), 18, "sawtooth plateaus", "[FCCA p. 54]"),
    _seq("fcca-11", "fcca", mountain_chain(), 16, "mountain chain, [1 2 ... n ... 2 1]",
         "[FCCA p. 54]"),
    _seq("fcca-12", "fcca", cycle(1, 1, 2, 3, 1, 2, 2, 3, 1, 2, 3, 3), 16, "the marching doubler",
         "[FCCA p. 54]"),
    _seq("fcca-13", "fcca", cycle(1, 1, 2, 3, 1, 2, 2, 3, 1, 2, 3, 3, 1, 2, 2, 3), 20,
         "the bouncing doubler", "[FCCA p. 54]"),
    _seq("fcca-14", "fcca", cycle(1, 2, 2, 3, 3, 1, 1, 2, 3, 3, 1, 1, 2, 2, 3), 20,
         "the marching singler", "[FCCA p. 54]"),
)

GROUPS = {
    "blackburn": BLACKBURN, "p146": RELATED, "p88": P88, "p38": P38, "p150": P150, "fcca": FCCA,
}

CATALOGUE = BLACKBURN + RELATED + P88 + P38 + P150 + FCCA


def find(name: str) -> Sequence:
    """A sequence by its name, `blackburn-2` (DECISION D-070)."""
    for seq in CATALOGUE:
        if seq.name == name:
            return seq
    raise KeyError(f"no sequence {name!r}")


def of_groups(groups) -> list:
    """The sequences of the named groups, in catalogue order (DECISION D-070)."""
    groups = list(groups)
    for group in groups:
        if group not in GROUPS:
            raise KeyError(f"no group {group!r}; the groups are {', '.join(GROUPS)}")
    return [seq for seq in CATALOGUE if seq.group in groups]
