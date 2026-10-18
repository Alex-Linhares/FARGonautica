"""French's numbers as data (`tabletop.reported`): the counts, the sums, the pages, the
labels, the derived observations, and the arithmetic behind French's own ratios.

The numbers were read on the page images [F95 pp. 115-144 (img); FCCA pp. 394-399 (img)];
these tests pin the transcription so that a later edit cannot move a target silently.
"""

import re

import pytest

from tabletop import reported as r

PAGE_RANGES = {"F95": (115, 144), "FCCA": (394, 399), "FH91": (181, 181)}


def test_counts():
    assert len(r.CHAPTER5) == 35          # 10 + 10 + 14 (Buridan 1 twice) + 1
    assert len(r.SURROUND) == 10 and len(r.BLOCKAGE) == 10
    assert len(r.BURIDAN) == 14 and len(r.FIGURE_GROUND) == 1
    assert len(r.FCCA) == 18
    assert len(r.SUBJECT_ROWS) == 23 and len(r.FIGURE_5_4) == 23
    assert [len(t) for t in (r.TABLE_5_1, r.TABLE_5_2, r.TABLE_5_3)] == [13, 5, 5]


def test_every_entry_has_a_page_in_its_source():
    for rs in r.CHAPTER5 + r.FCCA:
        lo, hi = PAGE_RANGES[rs.source]
        assert lo <= rs.page <= hi, rs.title
        assert rs.img is True
    for row in r.SUBJECT_ROWS:
        assert row.page in (143, 144)
    for datum in r.HUMAN_DATA:
        lo, hi = PAGE_RANGES[datum.source] if datum.source != "F95" else (86, 144)
        assert lo <= datum.page <= hi, datum


def test_chapter5_problems_and_programs():
    names = [rs.problem for rs in r.CHAPTER5]
    assert names.count("buridan-01") == 2
    assert len(set(names)) == 34
    assert names[:3] == ["surround-01", "surround-02", "surround-03"]
    assert names[-1] == "figure-ground-01"
    programs = {rs.title: rs.program for rs in r.BURIDAN}
    assert programs["Buridan 1 (original program)"] == "original"
    assert programs["Buridan 1 (modified program)"] == "modified"
    assert all(rs.program == "modified" for rs in r.BURIDAN[1:])
    assert all(rs.program == "original" for rs in r.SURROUND + r.BLOCKAGE + r.FIGURE_GROUND)
    assert all(rs.family == rs.problem.rsplit("-", 1)[0] for rs in r.CHAPTER5)


def test_frequencies_sum_to_100_in_every_bar_graph():
    """Every bar graph of chapter 5 and of FCCA sums to exactly 100 [F95 pp. 115-139 (img);
    FCCA pp. 394-399 (img)]. The exceptions are all in the tables (test below)."""
    for rs in r.CHAPTER5 + r.FCCA:
        assert rs.frequencies_sum() == 100, rs.title


def test_absent_answers_have_no_structure_or_codelets():
    """A dash in the table goes with a frequency of 0 [F95 p. 119]."""
    for rs in r.CHAPTER5 + r.FCCA:
        for a in rs.answers:
            assert (a.frequency == 0) == (a.structure is None) == (a.codelets is None), rs.title
    zero = sorted(rs.problem for rs in r.CHAPTER5 if any(a.frequency == 0 for a in rs.answers))
    assert zero == ["blockage-01", "blockage-03", "blockage-10", "buridan-10",
                    "figure-ground-01", "surround-06"]


def test_labels_as_printed():
    """Blockage 1-2 label the touched cup E2; from Blockage 3 the glasses share one bar,
    E1+E2, and the cup is E3 [F95 pp. 122-129 (img)]."""
    labels = {rs.problem: tuple(a.label for a in rs.answers) for rs in r.BLOCKAGE}
    assert labels["blockage-01"] == labels["blockage-02"] == ("E1", "E2")
    for n in range(3, 11):
        assert labels[f"blockage-{n:02d}"] == ("E1+E2", "E3")
    assert tuple(a.label for a in r.FIGURE_GROUND[0].answers) == ("E1+E2", "E3")
    for rs in r.SURROUND + r.BURIDAN:
        assert tuple(a.label for a in rs.answers) == ("E1", "E2")
    assert r.SURROUND[4].answer("E1").frequency == 86


def test_spot_checks_against_the_text():
    """Numbers the running text states outright [F95 pp. 115-139]."""
    book = {(rs.problem, rs.program): rs for rs in r.CHAPTER5}
    assert book["surround-01", "original"].answer("E2").frequency == 95      # p. 115
    assert book["surround-04", "original"].answer("E1").frequency == 45      # p. 117
    assert book["surround-05", "original"].answer("E1").frequency == 86      # p. 120
    assert book["surround-07", "original"].answer("E1").frequency == 80      # p. 120
    assert book["surround-09", "original"].answer("E1").frequency == 2       # p. 121, once
    assert book["blockage-02", "original"].answer("E2").frequency == 12      # p. 123
    assert book["blockage-06", "original"].answer("E3").frequency == 78      # p. 127
    assert book["blockage-08", "original"].answer("E3").frequency == 62      # p. 128
    assert book["blockage-09", "original"].answer("E3").frequency == 20      # p. 128
    assert book["buridan-01", "original"].answer("E1").frequency == 72       # p. 130
    assert book["buridan-01", "modified"].answer("E1").frequency == 50       # p. 131
    assert book["buridan-05", "modified"].answer("E2").frequency == 60       # p. 134
    assert book["buridan-08", "modified"].answer("E1").frequency == 44       # p. 136
    assert book["surround-03", "original"].answer("E1").structure == 112     # p. 117
    assert book["surround-04", "original"].answer("E2").structure == 107     # p. 118


# French's own ratios, each with its page, against the arithmetic of the tables. The claims
# are approximate ("about", "approximately"), so each is checked within a stated tolerance.
RATIOS = [
    # problem, program, numerator (label, field), denominator (label, field), claim, lo, hi, page
    ("surround-05", "original", ("E1", "codelets"), ("E2", "codelets"),
     "about 10 percent longer", 1.05, 1.15, 118),
    ("blockage-02", "original", ("E2", "structure"), ("E1", "structure"),
     "rated 30 percent higher", 1.25, 1.35, 123),
    ("blockage-02", "original", ("E2", "codelets"), ("E1", "codelets"),
     "approximately 50 percent longer", 1.45, 1.55, 123),
    ("blockage-04", "original", ("E3", "structure"), ("E1+E2", "structure"),
     "about 40 percent higher", 1.35, 1.45, 125),
    ("blockage-04", "original", ("E3", "codelets"), ("E1+E2", "codelets"),
     "about 45 percent longer", 1.40, 1.50, 125),
    ("blockage-06", "original", ("E3", "structure"), ("E1+E2", "structure"),
     "approximately three times better", 2.8, 3.3, 127),
    ("blockage-06", "original", ("E3", "codelets"), ("E1+E2", "codelets"),
     "about 40 percent longer", 1.35, 1.45, 127),
    ("buridan-01", "original", ("E1", "frequency"), ("E2", "frequency"),
     "approximately two and a half times as often", 2.3, 2.7, 130),
    ("buridan-07", "modified", ("E1", "codelets"), ("E2", "codelets"),
     "about 20 percent faster (the fork)", 1.15, 1.25, 135),
]


@pytest.mark.parametrize("problem, program, num, den, claim, lo, hi, page", RATIOS)
def test_frenchs_ratios_follow_from_his_tables(problem, program, num, den, claim, lo, hi, page):
    rs = next(x for x in r.CHAPTER5 if x.problem == problem and x.program == program)
    ratio = getattr(rs.answer(num[0]), num[1]) / getattr(rs.answer(den[0]), den[1])
    assert lo <= ratio <= hi, f"{rs.title}: {claim} [F95 p. {page}] but the ratio is {ratio:.2f}"


def test_blockage_5_more_than_twice_blockage_4():
    """"Eliza now chooses the cup in front of Henry more than twice as often as she did in
    the previous configuration" [F95 p. 126]: 42 against 20."""
    b4 = next(x for x in r.BLOCKAGE if x.problem == "blockage-04").answer("E3").frequency
    b5 = next(x for x in r.BLOCKAGE if x.problem == "blockage-05").answer("E3").frequency
    assert b5 > 2 * b4


def test_surround_10_is_13_points_below_surround_5():
    """"Eliza touches the small glass only 13 percent less often than in the Surround 5
    configuration" [F95 p. 121]; FCCA's 82 would be four points below [FCCA p. 395]."""
    s5 = r.SURROUND[4].answer("E1").frequency
    s10 = r.SURROUND[9].answer("E1").frequency
    assert s5 - s10 == 13
    fcca_s10 = next(x for x in r.FCCA if x.figure == "IX-1f").answer("E1").frequency
    assert s5 - fcca_s10 == 4


def test_fcca_reprint_matches_the_book_except_two_sets():
    """FCCA's eighteen figures carry the book's numbers except Surround 5 and 10
    [FCCA pp. 395 (img); F95 pp. 118, 121]."""
    book = {(rs.problem, rs.program): rs for rs in r.CHAPTER5}
    differ = []
    for rs in r.FCCA:
        if rs.answers != book[rs.problem, rs.program].answers:
            differ.append(rs.figure)
    assert differ == ["IX-1d", "IX-1f"]
    s5 = next(x for x in r.FCCA if x.figure == "IX-1d")
    assert [(a.structure, a.codelets) for a in s5.answers] == [(253, 107), (154, 85)]
    s10 = next(x for x in r.FCCA if x.figure == "IX-1f")
    assert [(a.frequency, a.structure, a.codelets) for a in s10.answers] == \
        [(82, 242, 91), (18, 148, 127)]


def test_fcca_figures_in_order_with_pages():
    figures = [rs.figure for rs in r.FCCA]
    assert figures == [f"IX-{i}{c}" for i in (1, 2, 3) for c in "abcdef"]
    pages = [rs.page for rs in r.FCCA]
    assert pages == [394] * 3 + [395] * 3 + [396] * 3 + [397] * 3 + [398] * 3 + [399] * 3
    problems = [rs.problem for rs in r.FCCA]
    assert problems[:6] == ["surround-01", "surround-02", "surround-04", "surround-05",
                            "surround-08", "surround-10"]
    assert problems[6:12] == ["blockage-01", "blockage-02", "blockage-04", "blockage-05",
                              "blockage-06", "blockage-09"]
    assert problems[12:] == ["buridan-10", "buridan-11", "buridan-01", "buridan-04",
                             "buridan-05", "buridan-06"]
    assert next(x for x in r.FCCA if x.figure == "IX-3c").program == "modified"


def test_result_sets_by_problem():
    sets = r.result_sets("surround-05")
    assert [rs.source for rs in sets] == ["F95", "FCCA"]
    assert [rs.program for rs in r.result_sets("buridan-01")] == ["original", "modified",
                                                                  "modified"]
    assert r.result_sets("blockage-03")[0].source == "F95" and len(r.result_sets("blockage-03")) == 1


# ---------------------------------------------------------------------------------------------
# Tables 5.1-5.3 and Figure 5.4

def test_table_rows_name_the_23_problems_french_lists():
    """"The entire Buridan series was tested, and five representative problems were chosen
    from each of the Surround and the Blockage series" [F95 p. 143]."""
    problems = [row.problem for row in r.SUBJECT_ROWS]
    assert problems[:13] == [f"buridan-{n:02d}" for n in range(1, 14)]
    assert problems[13:18] == [f"surround-{n:02d}" for n in range(1, 6)]
    assert problems[18:] == ["blockage-01", "blockage-02", "blockage-05", "blockage-06",
                             "blockage-07"]
    assert {row.table for row in r.TABLE_5_1} == {"5.1"}
    assert r.TABLE_5_3[0].labels == ("Either small glass", "Originally touched cup")


# Rows whose columns do not sum to 100 ± 2, with the reason (SPEC.md §7).
TABLE_EXCEPTIONS = {
    ("buridan-02", "subjects"): "66 + 31 = 97 as printed",
    ("buridan-04", "tabletop"): "72 + 25 = 97, plus the footnote's 3 for the cup close to E1",
    ("surround-03", "subjects"): "16 + 81 = 97; the footnote's 3 touched the touched cup",
    ("surround-04", "subjects"): "22 + 73 = 95 as printed",
    ("surround-05", "subjects"): "33 + 72 = 105 as printed",
    ("blockage-05", "subjects"): "16 + 34 = 50; the other cup on Henry's side took the rest",
    ("blockage-05", "tabletop"): "28 + 47 = 75; the other cup on Henry's side took the rest",
}


def test_table_columns_sum_to_100_except_the_known_rows():
    off = {}
    for row in r.SUBJECT_ROWS:
        for column in ("subjects", "tabletop"):
            total = sum(getattr(row, column))
            if abs(total - 100) > 2:
                off[row.problem, column] = total
    assert set(off) == set(TABLE_EXCEPTIONS), off
    assert off["surround-05", "subjects"] == 105 and off["surround-04", "subjects"] == 95
    assert off["surround-03", "subjects"] == 97 == off["buridan-02", "subjects"]
    assert off["buridan-04", "tabletop"] == 97
    assert off["blockage-05", "subjects"] == 50 and off["blockage-05", "tabletop"] == 75


def test_buridan_1_tabletop_sums_to_103_with_its_footnote():
    row = r.subject_row("buridan-01")
    assert row.footnote == r.CUP_NEAR_E1
    assert sum(row.tabletop) + 3 == 103
    assert r.subject_row("buridan-04").footnote == r.CUP_NEAR_E1
    assert sum(r.subject_row("buridan-04").tabletop) + 3 == 100


def test_rows_within_rounding_of_34_subjects():
    """One of thirty-four subjects is 2.9 points, so sums of 97-98 are rounding or a
    non-response; 105 is not [F95 p. 142]."""
    sums = {row.problem: sum(row.subjects) for row in r.SUBJECT_ROWS}
    assert sums["buridan-02"] == 97 and sums["blockage-06"] == 98
    assert sums["surround-05"] == 105


def test_figure_5_4_is_in_decreasing_order_of_fit():
    """"The problems are arranged in decreasing order of goodness of fit." [F95 p. 143]"""
    fits = [r.fit(r.subject_row(problem)) for _, problem in r.FIGURE_5_4]
    assert fits == sorted(fits)
    assert fits[0] == 0 and fits[-1] == 53
    assert r.FIGURE_5_4[0] == ("Bur18", "buridan-09")
    assert [label for label, _ in r.FIGURE_5_4][-2:] == ["Sur4", "Sur5"]
    assert len({p for _, p in r.FIGURE_5_4}) == 23


def test_french_s_summary_of_the_fit():
    """"on over half of these problems, Tabletop's answers differ from those of subjects by
    no more than 10 percentage points" [F95 p. 144]; the worst are Surrounds 4 and 5."""
    fits = {problem: r.fit(r.subject_row(problem)) for _, problem in r.FIGURE_5_4}
    assert sum(1 for f in fits.values() if f <= 10) > len(fits) / 2
    worst = sorted(fits, key=fits.get)[-2:]
    assert set(worst) == {"surround-04", "surround-05"}


def test_human_data():
    assert r.SUBJECTS_F95 == 34 and r.SUBJECTS_FH91 == 31 and r.RUNS_PER_PROBLEM == 50
    by = {(d.problem, d.source): d.percent for d in r.HUMAN_DATA}
    assert by["blockage-01", "F95"] == 33 == by["blockage-01", "FH91"]
    assert by["blockage-02", "FCCA"] == 40
    assert by["blockage-06", "FCCA"] == 66 == by["blockage-06", "FH91"]
    assert r.subject_row("blockage-06").subjects[1] == 72       # the book's version
    assert r.subject_row("blockage-02").subjects[1] == 62
    assert r.subject_row("blockage-01").subjects == (67, 33)
    survey = [d.percent for d in r.HUMAN_DATA if d.problem == "mirror-symmetry-survey"]
    assert survey == [62, 56]


# ---------------------------------------------------------------------------------------------
# The derived observations

def test_better_structure_takes_longer_on_20_of_29():
    """[F95 p. 142]; the nine exceptions include Buridan 7 [F95 p. 135]."""
    two = r.two_answer_sets()
    assert len(two) == 29
    holds = r.better_structure_takes_longer()
    assert len(holds) == 20
    exceptions = sorted((rs.problem, rs.program) for rs in two if rs not in holds)
    assert exceptions == [("blockage-08", "original"), ("blockage-09", "original"),
                          ("buridan-01", "modified"), ("buridan-01", "original"),
                          ("buridan-05", "modified"), ("buridan-07", "modified"),
                          ("buridan-12", "modified"), ("buridan-13", "modified"),
                          ("surround-01", "original")]


def test_frequency_against_structure_in_14_sets():
    """French names Surrounds 3, 4, 8, 9 and Blockages 2, 5 [F95 p. 142] and Buridans 9
    and 11 [F95 p. 131]; the data give fourteen."""
    sets = r.frequency_against_structure()
    names = [rs.problem for rs in sets]
    assert len(names) == 14
    for named in ("surround-03", "surround-04", "surround-08", "surround-09", "blockage-02",
                  "blockage-05", "buridan-09", "buridan-11"):
        assert named in names
    assert names == ["surround-02", "surround-03", "surround-04", "surround-08", "surround-09",
                     "blockage-02", "blockage-04", "blockage-05", "blockage-09", "buridan-01",
                     "buridan-02", "buridan-04", "buridan-09", "buridan-11"]
    assert [rs.program for rs in sets if rs.problem == "buridan-01"] == ["original"]


def test_most_frequent_and_best_structured():
    modified = r.BURIDAN[1]
    assert r.most_frequent(modified) is None                      # 50-50
    assert r.best_structured(modified).label == "E2"
    s5 = r.SURROUND[4]
    assert r.most_frequent(s5).label == "E1" and r.best_structured(s5).label == "E1"


def test_frequencies_are_not_all_multiples_of_two():
    """Exactly fifty runs give even percentages; the odd ones (5, 95, 15, 85, 45, 55, 73,
    27) agree with "approximately fifty runs" [F95 p. 114] rather than exactly fifty."""
    odd = [rs.title for rs in r.CHAPTER5 if any(a.frequency % 2 for a in rs.answers)]
    assert odd == ["Surround 1", "Surround 3", "Surround 4", "Surround 10", "Buridan 12"]


def test_citations_are_well_formed():
    """Every tag in the module names a page of the source's range."""
    import tabletop.reported
    text = open(tabletop.reported.__file__, encoding="utf-8").read()
    tags = re.findall(r"\[(F95|FCCA|FH91) pp?\. (\d+)", text)
    assert len(tags) >= 30
    for source, page in tags:
        lo, hi = {"F95": (1, 190), "FCCA": (319, 399), "FH91": (175, 182)}[source]
        assert lo <= int(page) <= hi, (source, page)
