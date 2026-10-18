"""The benches: French's experiments replayed, our numbers beside his (D-072; ANSWERS.md 1).

    python3 -m tabletop.bench families                 # the 35 chapter 5 result sets, 500 runs each
    python3 -m tabletop.bench fcca --runs 50           # FCCA's 18 reprinted sets, French's fifty runs
    python3 -m tabletop.bench families --problems surround-05 blockage-06 --runs 20 --json out.json

Every result set French reports is a measurement target: "Almost all of the configurations
were run fifty times." [F95 p. 114], and for each answer he prints its frequency, "the
average structure value at the end of the runs and the average number of codelets run in
runs giving the answer in question" [F95 p. 115]. A bench runs the same problems from one
seed, tallies the same three numbers per answer and prints them beside his, with a
two-sample z for every frequency and a chi-square per problem (`stats.py`). No number of
his is ever used to set a default (ANSWERS.md 1); the benches only measure.
"""
