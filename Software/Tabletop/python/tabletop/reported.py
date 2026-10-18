"""French's reported results, as data: the measurement targets of RESULTS.md.

Every number French published about Tabletop's behaviour, and about his human subjects,
transcribed from the pages so that the benches can print our numbers beside his. Nothing here
is ever used to set a default (RESULTS.md, "Never tune"); every entry is a target.

- The 36 result sets of chapter 5 [F95 pp. 115-139]: for each problem a bar graph of the
  frequency of each answer (an image; `img=True`) and a table of "the average structure
  value at the end of the runs and the average number of codelets run in runs giving the
  answer in question" [F95 p. 115] (the text layer). Buridan 1 appears twice, under the
  original and the modified program [F95 pp. 129-130]. About fifty runs each [F95 p. 114].
- FCCA's reprint, Figures IX-1 to IX-3 [FCCA pp. 394-399]: eighteen of the same problems,
  with the frequencies, structure-values and run-lengths redrawn; two sets differ from the
  book's (SPEC.md §7).
- Tables 5.1-5.3 [F95 pp. 143-144]: thirty-four subjects against a second set of runs
  after the object-type weight was raised [F95 pp. 142-143], and Figure 5.4's order.
- The other human data: [F95 pp. 86, 118, 122], [FCCA pp. 396-397], [FH91 p. 181].
- The derived observations French states about his own numbers [F95 pp. 131, 136, 142].

Frequencies were read from the bar graphs' printed labels and checked against the bars'
pixel heights (every bar agrees with its label to within one percentage point); the structure
values and codelet counts were read from the text layer and checked on the page images.
Where the sources disagree with each other, every version is kept (SPEC.md §7).
"""

from dataclasses import dataclass


@dataclass(frozen=True)
class Answer:
    """One answer of a result set: its printed label, what it names, and its three numbers.

    `label` is the label printed under the bar ("E1", "E2", "E3", "E1+E2"; the structure
    table prints "E1 or E2" for the joint bar [F95 p. 123]). `frequency` is the percentage of
    runs, read from the bar graph [F95 p. 115 (img)]. `structure` and `codelets` are the
    averages over the runs ending in that answer [F95 p. 115]; both are None where the table
    prints a dash because the answer never came up (for example Surround 6 [F95 p. 119]).
    """

    label: str
    names: str
    frequency: int
    structure: int | None
    codelets: int | None


@dataclass(frozen=True)
class ResultSet:
    """French's statistics for one problem under one program setting [F95 p. 115].

    `problem` is the file stem the problem has in `problems/` ("surround-05"); `title` is the
    heading as printed; `program` is "original" or, for Buridan 1's second set and all of
    Buridan 2-13, "modified" (the raised incoherence penalty [F95 p. 130]). `source` is
    "F95" or "FCCA", `page` the printed page of the bar graph, `figure` FCCA's figure label.
    `img` says that the frequencies were read from an image. `text` records where the
    running text confirms a number, with its page.
    """

    problem: str
    title: str
    family: str
    source: str
    page: int
    answers: tuple[Answer, ...]
    program: str = "original"
    figure: str | None = None
    img: bool = True
    text: str = ""
    note: str = ""

    def answer(self, label: str) -> Answer:
        """The answer printed with this label [F95 p. 115]."""
        for a in self.answers:
            if a.label == label:
                return a
        raise KeyError(label)

    def frequencies_sum(self) -> int:
        """The sum of the printed frequencies; 100 in every set of chapter 5 [F95 p. 115]."""
        return sum(a.frequency for a in self.answers)


# ---------------------------------------------------------------------------------------------
# What the labels name, family by family [F95 pp. 115-139 (img)].

GLASS = "Eliza's small glass, lower left, directly opposite the touched cup"
CUP = "Eliza's cup, lower right, diagonally opposite the touched cup"
TOUCHED = "the touched cup itself, on Henry's side (literal sameness)"
GLASSES = "either of Eliza's two small glasses, lower left"
SPOON = "Eliza's spoon, lower left"
FORK = "Eliza's fork, lower right"


def _answers(*rows: tuple) -> tuple[Answer, ...]:
    """Build the answers of one set from (label, names, frequency, structure, codelets) rows
    [F95 p. 115]."""
    return tuple(Answer(*row) for row in rows)


def _f95(problem: str, title: str, page: int, answers: tuple[Answer, ...],
         program: str = "original", text: str = "", note: str = "") -> ResultSet:
    """One result set of chapter 5, its family taken from the problem's name [F95 p. 114]."""
    family = problem.rsplit("-", 1)[0]
    return ResultSet(problem, title, family, "F95", page, answers, program, None, True,
                     text, note)


# ---------------------------------------------------------------------------------------------
# The Surround family [F95 pp. 115-121]. Henry's cup is touched, upper left; E1 is Eliza's
# glass (or its replacement) lower left, E2 her cup lower right; Surround 6 swaps them.

SURROUND = (
    _f95("surround-01", "Surround 1", 115, _answers(
        ("E1", GLASS, 5, 58, 52),
        ("E2", CUP, 95, 89, 50)),
        text="95 percent of the time Eliza opts to touch the cup [F95 p. 115]"),
    _f95("surround-02", "Surround 2", 116, _answers(
        ("E1", GLASS + ", in a spoon-spoon-glass-fork group", 10, 98, 68),
        ("E2", CUP, 90, 83, 50)),
        text="increases from 5 percent ... to 10 percent here [F95 p. 116]"),
    _f95("surround-03", "Surround 3", 117, _answers(
        ("E1", GLASS + ", in a spoon-spoon-glass-fork group", 15, 112, 89),
        ("E2", CUP, 85, 84, 64)),
        text="112 as opposed to 84 for the cup [F95 p. 117]",
        note="the bar graph is on p. 116, the table on p. 117"),
    _f95("surround-04", "Surround 4", 117, _answers(
        ("E1", GLASS + ", beside a spoon pair", 45, 186, 64),
        ("E2", CUP, 55, 107, 56)),
        text="touches the small glass 45 percent of the time; 186 against 107 [F95 pp. 117-118]"),
    _f95("surround-05", "Surround 5", 118, _answers(
        ("E1", GLASS + ", inside the spoon-spoon-glass-fork-fork-knife group", 86, 248, 101),
        ("E2", CUP + " (a loner)", 14, 146, 94)),
        text="touched the small glass 86 percent of the time [F95 p. 120]"),
    _f95("surround-06", "Surround 6", 119, _answers(
        ("E1", "Eliza's cup, lower left, inside the group (the cup and glass of Surround 5 "
               "swapped)", 100, 237, 62),
        ("E2", "Eliza's isolated small glass, lower right", 0, None, None)),
        text="the program ... simply does not do it very often [F95 p. 119]"),
    _f95("surround-07", "Surround 7", 119, _answers(
        ("E1", "Eliza's saucer in place of the glass of Surround 5", 80, 231, 109),
        ("E2", CUP, 20, 127, 107)),
        text="she touches the saucer 80 percent of the time [F95 p. 120]"),
    _f95("surround-08", "Surround 8", 120, _answers(
        ("E1", "Eliza's plate in place of the glass of Surround 5", 10, 214, 204),
        ("E2", CUP, 90, 151, 102))),
    _f95("surround-09", "Surround 9", 121, _answers(
        ("E1", "Eliza's salt shaker in place of the glass of Surround 5", 2, 266, 445),
        ("E2", CUP, 98, 150, 99)),
        text="in fifty runs of the program, Eliza touches the saltshaker only once [F95 p. 121]",
        note="the bar graph is on p. 120, the table on p. 121"),
    _f95("surround-10", "Surround 10", 121, _answers(
        ("E1", GLASS + ", as Surround 5 with junk added mid-table", 73, 259, 88),
        ("E2", CUP, 27, 141, 78)),
        text="only 13 percent less often than in the Surround 5 configuration [F95 p. 121]",
        note="the figure shows five added objects: salt and pepper shakers mid-table, a fork, "
             "a knife and a glass mid-right [F95 p. 121 (img)]; FCCA says six [FCCA p. 395]"),
)

# ---------------------------------------------------------------------------------------------
# The Blockage family [F95 pp. 122-129]. Henry's cup is touched, upper left; Eliza's glass (or
# two glasses) lower left; objects added upper right. Blockage 1-2 label the touched cup E2;
# from Blockage 3 the two glasses are E1 and E2 (one bar, "E1+E2") and the cup is E3.

BLOCKAGE = (
    _f95("blockage-01", "Blockage 1", 122, _answers(
        ("E1", GLASS, 100, 66, 67),
        ("E2", TOUCHED, 0, None, None)),
        text="a great deal of pressure to touch the small glass, and Eliza always does so "
             "[F95 p. 122]"),
    _f95("blockage-02", "Blockage 2", 123, _answers(
        ("E1", GLASS, 88, 71, 106),
        ("E2", TOUCHED + ", preserving a glass-glass correspondence", 12, 94, 158)),
        text="Eliza decides, 12 percent of the time, ... to touch the cup [F95 p. 123]",
        note="the bar graph is on p. 122, the table on p. 123"),
    _f95("blockage-03", "Blockage 3", 123, _answers(
        ("E1+E2", GLASSES, 100, 78, 101),
        ("E3", TOUCHED, 0, None, None)),
        text="As a result, Eliza never touches the cup [F95 p. 124]"),
    _f95("blockage-04", "Blockage 4", 125, _answers(
        ("E1+E2", GLASSES, 80, 64, 125),
        ("E3", TOUCHED, 20, 88, 179)),
        text="about 40 percent higher ... about 45 percent longer to build [F95 p. 125]",
        note="the bar graph is on p. 124, the table on p. 125"),
    _f95("blockage-05", "Blockage 5", 126, _answers(
        ("E1+E2", GLASSES, 58, 58, 95),
        ("E3", TOUCHED, 42, 144, 190)),
        text="more than twice as often as she did in the previous configuration [F95 p. 126]",
        note="the bar graph is on p. 125, the table on p. 126"),
    _f95("blockage-06", "Blockage 6", 126, _answers(
        ("E1+E2", GLASSES, 22, 57, 66),
        ("E3", TOUCHED, 78, 177, 94)),
        text="she touches the cup in front of Henry 78 percent of the time [F95 p. 127]"),
    _f95("blockage-07", "Blockage 7", 127, _answers(
        ("E1+E2", GLASSES + ", with a fork and knife beside them", 24, 59, 59),
        ("E3", TOUCHED, 76, 196, 119)),
        text="the addition of the silverware has virtually no effect [F95 p. 127]"),
    _f95("blockage-08", "Blockage 8", 128, _answers(
        ("E1+E2", GLASSES, 38, 73, 142),
        ("E3", TOUCHED, 62, 153, 124)),
        text="from 78 percent (Blockage 6) to 62 percent (Blockage 8) [F95 p. 128]",
        note="the bar graph is on p. 127, the table on p. 128"),
    _f95("blockage-09", "Blockage 9", 128, _answers(
        ("E1+E2", GLASSES, 80, 81, 130),
        ("E3", TOUCHED, 20, 165, 124)),
        text="Eliza touches Henry's cup only 20 percent of the time [F95 p. 128]"),
    _f95("blockage-10", "Blockage 10", 129, _answers(
        ("E1+E2", GLASSES, 100, 83, 106),
        ("E3", TOUCHED, 0, None, None)),
        text="Eliza always chooses one of the glasses on her side of the table [F95 p. 129]"),
)

# ---------------------------------------------------------------------------------------------
# The Buridan family [F95 pp. 129-139]. Henry's spoon is touched, top centre, beside its
# "sidekicks"; E1 is Eliza's spoon lower left, E2 her fork lower right (or another object).

BURIDAN_MODIFIED_NOTE = ("all of Buridan 2-13 were run with the modified program, whose "
                         "incoherence penalty was raised until Buridan 1 came out 50-50 "
                         "[F95 p. 130]")


def _buridan(problem: str, title: str, page: int, answers: tuple[Answer, ...],
             program: str = "modified", text: str = "", note: str = "") -> ResultSet:
    """One Buridan result set; modified program unless said otherwise [F95 p. 130]."""
    return _f95(problem, title, page, answers, program, text, note)


BURIDAN = (
    _buridan("buridan-01", "Buridan 1 (original program)", 129, _answers(
        ("E1", SPOON + ", beside a glass pair", 72, 170, 88),
        ("E2", FORK + ", beside a cup pair", 28, 201, 78)),
        program="original",
        text="goes for the spoon on Eliza's side of the table about 72 percent of the time "
             "[F95 p. 130]"),
    _buridan("buridan-01", "Buridan 1 (modified program)", 130, _answers(
        ("E1", SPOON + ", beside a glass pair", 50, 246, 86),
        ("E2", FORK + ", beside a cup pair", 50, 280, 75)),
        text="the spoon and the fork on her side of the table with exactly the same frequency "
             "[F95 p. 131]",
        note="the title prints 'Buridan I'; the calibration point of the buridan profile"),
    _buridan("buridan-02", "Buridan 2", 131, _answers(
        ("E1", SPOON + ", beside one glass", 98, 96, 62),
        ("E2", FORK + ", beside one cup", 2, 192, 105)),
        text="she goes for her spoon overwhelmingly more often than her fork [F95 p. 131]"),
    _buridan("buridan-03", "Buridan 3", 132, _answers(
        ("E1", SPOON + ", beside one glass", 48, 95, 74),
        ("E2", FORK + ", beside a cup pair", 52, 272, 77))),
    _buridan("buridan-04", "Buridan 4", 132, _answers(
        ("E1", SPOON + ", beside a glass and a cup", 52, 122, 89),
        ("E2", FORK + ", beside a cup pair", 48, 279, 113)),
        text="the two arguments were almost perfectly balanced [F95 p. 133]"),
    _buridan("buridan-05", "Buridan 5", 133, _answers(
        ("E1", SPOON + ", beside a glass and a cup", 40, 138, 96),
        ("E2", "Eliza's spoon, lower right, beside a glass pair", 60, 237, 83)),
        text="the spoon associated with the sameness group 60 percent of the time [F95 p. 134]"),
    _buridan("buridan-06", "Buridan 6", 134, _answers(
        ("E1", SPOON + ", beside a salt shaker and a glass", 40, 75, 77),
        ("E2", FORK + ", beside a cup pair", 60, 277, 88)),
        text="60/40 percent as opposed to 50/50 percent [F95 p. 134]"),
    _buridan("buridan-07", "Buridan 7", 135, _answers(
        ("E1", SPOON + ", alone", 8, 74, 113),
        ("E2", FORK + ", beside a cup pair", 92, 285, 95)),
        text="chooses the fork ... about 20 percent faster than the spoon [F95 p. 135]"),
    _buridan("buridan-08", "Buridan 8", 136, _answers(
        ("E1", SPOON + ", beside a salt-shaker pair", 44, 74, 80),
        ("E2", FORK + ", beside a cup pair", 56, 283, 91)),
        text="44 percent as opposed to 40 percent [F95 p. 136]",
        note="the bar graph is on p. 135, the table on p. 136"),
    _buridan("buridan-09", "Buridan 9", 136, _answers(
        ("E1", SPOON + ", beside a glass pair", 96, 187, 98),
        ("E2", "Eliza's salt shaker, lower right, beside a cup pair", 4, 248, 139)),
        text="Eliza almost always touches the spoon in front of her [F95 p. 136]"),
    _buridan("buridan-10", "Buridan 10", 137, _answers(
        ("E1", SPOON + ", alone", 100, 65, 85),
        ("E2", FORK + ", alone", 0, None, None)),
        text="she always chooses the spoon in front of her [F95 p. 137]"),
    _buridan("buridan-11", "Buridan 11", 137, _answers(
        ("E1", SPOON + ", alone", 94, 57, 84),
        ("E2", FORK + ", beside one cup", 6, 199, 93)),
        text="Most of the time, though, she still goes for the spoon [F95 p. 138]"),
    _buridan("buridan-12", "Buridan 12", 138, _answers(
        ("E1", SPOON + ", beside a cup pair and a glass", 75, 311, 81),
        ("E2", FORK + ", beside a cup pair", 25, 264, 105))),
    _buridan("buridan-13", "Buridan 13", 139, _answers(
        ("E1", SPOON + ", beside a plate pair", 98, 173, 98),
        ("E2", "Eliza's salt shaker, lower right, beside a cup pair", 2, 161, 155)),
        text="Eliza opts overwhelmingly to touch the spoon [F95 p. 139]"),
)

# ---------------------------------------------------------------------------------------------
# Figure/Ground 1 [F95 pp. 139-142]: Henry reaches across and touches Eliza's lone fork,
# beside her two glasses; on his side are two forks and a lone glass.

FIGURE_GROUND = (
    _f95("figure-ground-01", "Figure/ground 1", 139, _answers(
        ("E1+E2", "either of Henry's two forks", 100, 92, 57),
        ("E3", "Henry's lone glass, the 'figure' answer", 0, None, None)),
        text="the program never selected Henry's glass [F95 p. 140]"),
)

CHAPTER5 = SURROUND + BLOCKAGE + BURIDAN + FIGURE_GROUND


# ---------------------------------------------------------------------------------------------
# FCCA's reprint, Figures IX-1 to IX-3 [FCCA pp. 394-399 (img)]: "Each problem was run a total
# of 50 times." [FCCA p. 392]. The frequencies, structure-values and run-lengths were read
# from the redrawn figures; two sets differ from the book's (SPEC.md §7).

def _fcca(figure: str, page: int, book: ResultSet, answers: tuple[Answer, ...] | None = None,
          note: str = "") -> ResultSet:
    """FCCA's version of a chapter 5 set: the book's answers unless FCCA's figure differs
    [FCCA pp. 394-399]."""
    return ResultSet(book.problem, book.title, book.family, "FCCA", page,
                     answers or book.answers, book.program, figure, True, "", note)


def _book(problem: str, program: str | None = None) -> ResultSet:
    """The chapter 5 set for a problem; Buridan 1 needs the program setting named
    [F95 pp. 115-139]."""
    for rs in CHAPTER5:
        if rs.problem == problem and program in (None, rs.program):
            return rs
    raise KeyError((problem, program))


FCCA = (
    _fcca("IX-1a", 394, _book("surround-01")),
    _fcca("IX-1b", 394, _book("surround-02")),
    _fcca("IX-1c", 394, _book("surround-04")),
    _fcca("IX-1d", 395, _book("surround-05"), _answers(
        ("E1", GLASS, 86, 253, 107),
        ("E2", CUP, 14, 154, 85)),
        note="structure and codelets differ from the book's 248/146 and 101/94 [F95 p. 118]"),
    _fcca("IX-1e", 395, _book("surround-08")),
    _fcca("IX-1f", 395, _book("surround-10"), _answers(
        ("E1", GLASS, 82, 242, 91),
        ("E2", CUP, 18, 148, 127)),
        note="every number differs from the book's 73/27, 259/141, 88/78 [F95 p. 121]; the "
             "caption says six objects were added [FCCA p. 395], the book's figure shows five"),
    _fcca("IX-2a", 396, _book("blockage-01")),
    _fcca("IX-2b", 396, _book("blockage-02")),
    _fcca("IX-2c", 396, _book("blockage-04")),
    _fcca("IX-2d", 397, _book("blockage-05")),
    _fcca("IX-2e", 397, _book("blockage-06")),
    _fcca("IX-2f", 397, _book("blockage-09")),
    _fcca("IX-3a", 398, _book("buridan-10")),
    _fcca("IX-3b", 398, _book("buridan-11")),
    _fcca("IX-3c", 398, _book("buridan-01", "modified")),
    _fcca("IX-3d", 399, _book("buridan-04")),
    _fcca("IX-3e", 399, _book("buridan-05")),
    _fcca("IX-3f", 399, _book("buridan-06")),
)


# ---------------------------------------------------------------------------------------------
# Tables 5.1-5.3 [F95 pp. 143-144]: thirty-four subjects [F95 p. 142] against a second set of
# fifty runs per problem after "I slightly increased the relative importance that the program
# attached to object-type matches" [F95 p. 143].

@dataclass(frozen=True)
class SubjectRow:
    """One row of Tables 5.1-5.3: percentages of subjects and of Tabletop's runs for the
    two answers the table names [F95 pp. 143-144].

    `labels` are the table's column headings ("E1", "E2", or Table 5.3's "Either small
    glass" and "Originally touched cup"); `subjects` and `tabletop` the two percentages
    for each, in that order; `footnote` the table's footnote where the row carries its
    asterisk or is the only row it can refer to (SPEC.md §7).
    """

    table: str
    problem: str
    page: int
    labels: tuple[str, str]
    names: tuple[str, str]
    subjects: tuple[int, int]
    tabletop: tuple[int, int]
    footnote: str = ""

    def most_frequent_for_subjects(self) -> int:
        """The index of the answer the subjects chose most often [F95 p. 143]."""
        return 0 if self.subjects[0] >= self.subjects[1] else 1


def _rows(table: str, page: int, prefix: str, labels: tuple[str, str],
          names: tuple[str, str], rows: tuple) -> tuple[SubjectRow, ...]:
    """The rows of one table from (number, subjects E1, TT E1, subjects E2, TT E2[, footnote])
    tuples [F95 pp. 143-144]."""
    out = []
    for number, s1, t1, s2, t2, *rest in rows:
        out.append(SubjectRow(table, f"{prefix}-{number:02d}", page, labels, names,
                              (s1, s2), (t1, t2), rest[0] if rest else ""))
    return tuple(out)


CUP_NEAR_E1 = "TT also touched the cup close to E1: 3%"
OTHER_CUP = "The other cup on Henry's side of the table"

TABLE_5_1 = _rows("5.1", 143, "buridan", ("E1", "E2"), (SPOON, FORK), (
    (1, 75, 75, 25, 25, CUP_NEAR_E1),
    (2, 66, 100, 31, 0),
    (3, 66, 84, 34, 16),
    (4, 66, 72, 34, 25, CUP_NEAR_E1),
    (5, 22, 47, 78, 53),
    (6, 53, 59, 47, 41),
    (7, 62, 72, 38, 28),
    (8, 62, 75, 38, 25),
    (9, 100, 100, 0, 0),
    (10, 97, 100, 3, 0),
    (11, 75, 100, 25, 0),
    (12, 56, 84, 44, 16),
    (13, 97, 100, 3, 0),
))

TABLE_5_2 = _rows("5.2", 143, "surround", ("E1", "E2"), (GLASS, CUP), (
    (1, 0, 0, 100, 100),
    (2, 0, 3, 100, 97),
    (3, 16, 16, 81, 84, "3% touched the touched cup"),
    (4, 22, 75, 73, 25),
    (5, 33, 81, 72, 19),
))

TABLE_5_3 = _rows("5.3", 144, "blockage", ("Either small glass", "Originally touched cup"),
                  (GLASSES, TOUCHED), (
    (1, 67, 100, 33, 0),
    (2, 38, 47, 62, 53),
    (5, 16, 28, 34, 47, OTHER_CUP),
    (6, 26, 19, 72, 81),
    (7, 22, 28, 78, 72),
))

SUBJECT_ROWS = TABLE_5_1 + TABLE_5_2 + TABLE_5_3

# Figure 5.4 [F95 p. 144 (img)]: for each of the 23 problems, the subjects' most frequent
# answer against Tabletop's frequency for it, "arranged in decreasing order of goodness of
# fit" [F95 p. 143]. The labels as printed, left to right; "Bur18" is read as Buridan 9, the
# one Buridan problem without a bar of its own and the only 100/100 row.
FIGURE_5_4 = (
    ("Bur18", "buridan-09"), ("Bur1", "buridan-01"), ("Sur1", "surround-01"),
    ("Sur3", "surround-03"), ("Bur13", "buridan-13"), ("Bur10", "buridan-10"),
    ("Sur2", "surround-02"), ("Bur4", "buridan-04"), ("Bur6", "buridan-06"),
    ("Blk7", "blockage-07"), ("Blk6", "blockage-06"), ("Blk2", "blockage-02"),
    ("Bur7", "buridan-07"), ("Bur8", "buridan-08"), ("Bur3", "buridan-03"),
    ("Bur11", "buridan-11"), ("Blk5", "blockage-05"), ("Bur5", "buridan-05"),
    ("Bur12", "buridan-12"), ("Blk1", "blockage-01"), ("Bur2", "buridan-02"),
    ("Sur4", "surround-04"), ("Sur5", "surround-05"),
)


def subject_row(problem: str) -> SubjectRow:
    """The table row of a problem [F95 pp. 143-144]."""
    for row in SUBJECT_ROWS:
        if row.problem == problem:
            return row
    raise KeyError(problem)


def fit(row: SubjectRow) -> int:
    """Figure 5.4's goodness of fit: the gap between the subjects' most frequent answer and
    Tabletop's frequency for the same answer [F95 p. 143]. For Blockage 5 the subjects' most
    frequent answer is the other cup, the remainder of both columns (SPEC.md §7)."""
    if row.problem == "blockage-05":
        return abs((100 - sum(row.subjects)) - (100 - sum(row.tabletop)))
    i = row.most_frequent_for_subjects()
    return abs(row.subjects[i] - row.tabletop[i])


# ---------------------------------------------------------------------------------------------
# The other human data.

@dataclass(frozen=True)
class HumanDatum:
    """One statement about human subjects, with its source: the book [F95 p. 142], FCCA
    [FCCA p. 396] or the paper [FH91 p. 181]. `percent` is None where the source gives no
    number."""

    problem: str
    source: str
    page: int
    what: str
    percent: int | None
    note: str = ""


HUMAN_DATA = (
    HumanDatum("blockage-01", "F95", 122, "subjects who touched the touched cup", 33,
               "agrees with Table 5.3's 33 [F95 p. 144]"),
    HumanDatum("surround-05", "F95", 118, "subjects who touched the isolated cup, in the "
               "chapter 5 experiment", None, "'over two-thirds'; Table 5.2 gives 72"),
    HumanDatum("surround-05", "F95", 118, "subjects of an initial informal survey who drew "
               "the group correspondences yet chose the isolated cup", None,
               "'in more than half of the cases'"),
    HumanDatum("mirror-symmetry-survey", "F95", 86, "subjects preferring the object-type "
               "mapping over the size mapping, without the cups", 62,
               "against 38 for size; a 'small, preliminary survey'"),
    HumanDatum("mirror-symmetry-survey", "F95", 86, "the same, with the mirror-symmetric "
               "cup-cup mapping present", 56, ""),
    HumanDatum("blockage-02", "FCCA", 396, "subjects who saw the glasses as counterparts "
               "(and so touched Henry's cup)", 40,
               "cites French 1992; Table 5.3 gives 62 for the touched cup [F95 p. 144]"),
    HumanDatum("blockage-06", "FCCA", 397, "subjects who chose Henry's cup", 66,
               "cites FH91; Table 5.3 gives 72 [F95 p. 144]"),
    HumanDatum("blockage-06", "FH91", 181, "subjects who touched the touched cup in the "
               "third of the paper's three problems", 66,
               "thirty-one subjects; 'twice as many ... (66% vs. 33%)' against the first"),
    HumanDatum("blockage-01", "FH91", 181, "subjects who touched the touched cup in the "
               "first of the paper's three problems", 33, "thirty-one subjects"),
)

SUBJECTS_F95 = 34   # "thirty-four students in an introductory experimental psychology class
                    # at Willamette University" [F95 p. 142]
SUBJECTS_FH91 = 31  # "Thirty-one human subjects were given these three problems" [FH91 p. 181]
RUNS_PER_PROBLEM = 50   # "Almost all of the configurations were run fifty times. No
                        # configuration was ever run fewer than forty times." [F95 p. 114];
                        # "a total of 50 times" [FCCA p. 392]


# ---------------------------------------------------------------------------------------------
# Derived observations: what French says his numbers show [F95 pp. 131, 136, 142].

def two_answer_sets(sets: tuple[ResultSet, ...] = CHAPTER5) -> list[ResultSet]:
    """The sets in which both answers came up, so both have a structure value and a run
    length [F95 p. 115]."""
    return [rs for rs in sets if all(a.structure is not None for a in rs.answers)]


def best_structured(rs: ResultSet) -> Answer:
    """The answer with the higher average structure value [F95 p. 115]."""
    return max((a for a in rs.answers if a.structure is not None), key=lambda a: a.structure)


def most_frequent(rs: ResultSet) -> Answer | None:
    """The answer chosen most often, or None when the frequencies tie (Buridan 1 modified,
    50-50 [F95 p. 131])."""
    top = max(a.frequency for a in rs.answers)
    winners = [a for a in rs.answers if a.frequency == top]
    return winners[0] if len(winners) == 1 else None


def better_structure_takes_longer(sets: tuple[ResultSet, ...] = CHAPTER5) -> list[ResultSet]:
    """The two-answer sets in which the better-structured answer took more codelets:
    "Answers involving better structure generally take more time before they are selected."
    [F95 p. 142]. The rest are the exceptions, among them Buridan 7, where the fork is
    chosen "about 20 percent faster than the spoon" [F95 p. 135]."""
    out = []
    for rs in two_answer_sets(sets):
        best = best_structured(rs)
        other = [a for a in rs.answers if a is not best][0]
        if best.codelets > other.codelets:
            out.append(rs)
    return out


def frequency_against_structure(sets: tuple[ResultSet, ...] = CHAPTER5) -> list[ResultSet]:
    """The sets in which the most frequent answer is not the best structured: "This type of
    discrepancy between frequency and structure value crops up in some problems" [F95
    p. 131]; French names Surrounds 3, 4, 8 and 9 and Blockages 2 and 5 [F95 p. 142] and
    Buridans 9 and 11 [F95 p. 131]. Ties in frequency are left out."""
    out = []
    for rs in two_answer_sets(sets):
        top = most_frequent(rs)
        if top is not None and top is not best_structured(rs):
            out.append(rs)
    return out


def result_sets(problem: str) -> list[ResultSet]:
    """Every reported set for a problem, the book's first [F95 p. 115], then FCCA's
    [FCCA p. 392]."""
    return [rs for rs in CHAPTER5 + FCCA if rs.problem == problem]
