# Results: the six classic problems, 100 seeds each

Mitchell's archived code run through `copycat:run` with seeds 1 to 100
(`./run-copycat.sh --sweep 100 abc abd TARGET 1 results/abc-abd-TARGET.log`;
the per-run lines are in `results/`, the tables in `results/sweep-summary.txt`).
Every one of the 600 runs ended with an answer and no Lisp error. SBCL
2.6.0, October 2026.

The comparison column is Hofstadter and Mitchell's chapter 5 of *Fluid
Concepts and Creative Analogies* (FCCA, 1995; the chapter is Mitchell &
Hofstadter 1990 revised), whose bar graphs summarize 1,000 runs per problem
of this same version of the program ("the differences between the
frequencies given in this figure and those given for the same problem in
Mitchell & Hofstadter, 1990b are due to several improvements in the
program", footnote 4). The graphs are pictures; the figures quoted are the
ones that appear in the text and in the graphs' top bars.

| Problem | This port, 100 seeds | FCCA, 1,000 runs |
|---|---|---|
| abc → abd, ijk → ? | **ijl 97**, ijd 2, ijk 1 | ijl by far the most common, ijd second; "in 1,000 runs, the program never gave an answer involving a replacement of the leftmost letter" (Problem 1, Figure V-1) |
| abc → abd, xyz → ? | **xyd 78**, wyz 12, yyz 7, xyz 2, dyz 1 | xyd 771; wyz "a distant second in frequency, but the answer with the lowest average final temperature"; yyz next; dyz and three others on the fringe (Problem 6, Figure V-6) |
| abc → abd, iijjkk → ? | **iijjll 80**, iijjkl 18, iijjkd 1, iijjkk 1 | not in FCCA chapter 5 (its Problem 2 is aabc → aabd, ijkk → ?: ijll 612, ijkl second, then jjkk and hjkk); the 1990 Physica D paper discusses iijjkk with iijjll as the straightforward answer |
| abc → abd, kji → ? | **kjh 61**, kjj 22, lji 16, kjd 1 | kjh 547, "kjj and lji almost tying for second place (the latter being a bit less common, but having a much lower average final temperature)", kjd a very distant fourth, dji and kji once each (Problem 3, Figure V-3) |
| abc → abd, mrrjjj → ? | **mrrkkk 76**, mrrjjk 18, mrrddd 2, mrrjkk 2, mrrjjjj 2 | mrrkkk "by far the most common", mrrjjk "a fairly distant second", then mrrjkk and mrrjjjj "with roughly equal frequencies" (mrrjjjj 39), mrrddd and mrrjjd on the fringe (Problem 4, Figure V-4) |
| abc → abd, xyd → ? | **xye 98**, xyd 2 | not a published problem; the rightmost letter is replaced by its successor, as in Problem 1 |

Mean codelets and final temperature over the 100 runs: ijk 296 / 18;
xyz 3,350 / 22; iijjkk 551 / 32; kji 381 / 21; mrrjjj 958 / 44; xyd 582 / 44.

The ranking and the proportions agree with the book's in every problem
where the book gives them: xyd at 78% against 77%, kjh at 61% against 55%
with kjj and lji close behind, mrrkkk far ahead with mrrjjjj at 2% against
4%. The point FCCA makes about mrrjjjj, that the program finds the "deep"
answer rarely but at a much lower temperature than the obvious ones, is
visible in the log: the two mrrjjjj runs end at temperatures 24 and 21
against a mean of 44 (`grep mrrjjjj results/abc-abd-mrrjjj.log`).

Footnote 3 of the chapter, that ijd ends at nearly the same temperature as
ijl, holds here too (ijd runs: temperatures 17 and 24; ijl mean 18).

100 runs per problem is enough to see the shape of a distribution, not the
fringe answers; the book's 1,000-run graphs list answers that 100 seeds did
not produce (hjkk-type answers in Problem 2, uryz's cousins in Problem 6).
A 1,000-seed sweep is `./run-copycat.sh --sweep 1000 abc abd xyz` (roughly
ten minutes for xyz, less for the others).
