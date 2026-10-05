"""demos.ss: the demo runs from Chapter 5 of Marshall's dissertation.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from demos.ss.

Demo runs from Chapter 5 of Jim Marshall's PhD thesis, "Metacat: A
Self-Watching Cognitive Architecture for Analogy-Making and High-Level
Perception", Department of Computer Science, Indiana University, 1999.

Each problem is a list of symbols (strings) and a seed, as the control panel's
command line gives them.  The control panel's Demos menu (gui/gui.py) runs them.
This module imports nothing from the GUI.
"""
from __future__ import annotations

from metacat import setup
from metacat.objects import tell


def demo(problem):
    """demos.ss: demo"""
    return tell(setup.g_control_panel, "run-new-problem", problem)


# Type (demo run1) to run the first demo, etc.

# -------------------------------------------------------
# Sample runs (section 5.2)

# Note: To reproduce Runs 1-8 exactly as described in
# Chapter 5, the Episodic Memory should be cleared at
# the beginning of each run.

run1 = ["abc", "abd", "mrrjjj", "mrrjjjj", 1092119323]
run2 = ["xqc", "xqd", "mrrjjj", "mrrkkk", 1248075611]
run3 = ["rst", "rsu", "xyz", "uyz", 2330176791]
run4 = ["abc", "abd", "xyz", "dyz", 2836825623]
run5 = ["xqc", "xqd", "mrrjjj", "mrrjjjj", 3729474543]
run6 = ["eqe", "qeq", "abbbc", "aaabccc", 789090523]
run7 = ["abc", "abd", "xyz", 3852097033]
run8 = ["eqe", "qeq", "abbbc", 3557912874]

# -------------------------------------------------------
# Answer comparison and reminding (section 5.2.3)

# xyz family
abc_xyd = ["abc", "abd", "xyz", "xyd", 1760747975]
abc_wyz = ["abc", "abd", "xyz", "wyz", 3100511611]
abc_dyz = ["abc", "abd", "xyz", "dyz", 2107869027]
rst_xyu = ["rst", "rsu", "xyz", "xyu", 939480183]
rst_wyz = ["rst", "rsu", "xyz", "wyz", 720286361]
rst_uyz = ["rst", "rsu", "xyz", "uyz", 2330176791]

# mrrjjj family
abc_mrrkkk = ["abc", "abd", "mrrjjj", "mrrkkk", 4211806334]
abc_mrrjjjj = ["abc", "abd", "mrrjjj", "mrrjjjj", 1092119323]
xqc_mrrkkk = ["xqc", "xqd", "mrrjjj", "mrrkkk", 1248075611]
xqc_mrrjjjj = ["xqc", "xqd", "mrrjjj", "mrrjjjj", 3729474543]

# eqe family
eqe_baaab = ["eqe", "qeq", "abbba", "baaab", 3635369418]
eqe_aaabaaa = ["eqe", "qeq", "abbba", "aaabaaa", 4209674874]
eqe_qeeeq = ["eqe", "qeq", "abbbc", 2302461154]
eqe_aaabccc = ["eqe", "qeq", "abbbc", "aaabccc", 789090523]

# -------------------------------------------------------
# Implausible rules (section 5.3.1)

fig5_4_top = ["eeqee", "qeeq", "xxixx", 698282038]
fig5_4_bottom = ["eeqee", "qeeq", "xxixx", 175910650]
fig5_5_top = ["eeqee", "qeeq", "xxixx", 698282038]
fig5_5_bottom = ["eeqee", "qeeq", "xxixx", 4109591222]

# -------------------------------------------------------
# Poor thematic characterizations (section 5.3.2)

fig5_7 = ["aabc", "aabd", "ijkk", "ijll", 2351730219]
fig5_8 = ["aabc", "aabd", "ijkk", "hjkk", 1810079903]
fig5_10 = ["abc", "abd", "xyz", 3009318743]
fig5_11 = ["abc", "abd", "xyz", 2006188493]

# -------------------------------------------------------
# Other sample runs, not discussed in the dissertation.

# Justifies mmmrrj (7794 time steps).
misc1 = ["abc", "cba", "mrrjjj", "mmmrrj", 3538780671]

# Describes a and b as "changing" in abc->abd (1126 time steps).
misc2 = ["abc", "abd", "ijk", "abd", 3386544399]

# Answers kji at time 1240 (applying succgrp=>predgrp slippage),
# kkkjjjiii at time 1470 (not applying the slippage), kkjjii at
# 1485 (using a literal rule).
misc3 = ["abc", "aabbcc", "kkjjii", 912835776]

# First answers b, then y on account of a coattail slippage
# induced by a first=>last slippage (945 time steps).
misc4 = ["a", "b", "z", 3861033416]

# Answers flz due to a coattail slippage, then dlz, then hlz
# at time step 1721.
misc5 = ["abc", "abd", "glz", 1108779034]

# -------------------------------------------------------
# not used (commented out in demos.ss)

# answers qbbbq due to lack of a middle description,
# then qeeeq, then qcccb due to a stupid rule (time 2782)
# misc6 = ["eqe", "qeq", "abbbc", 4089168737]

# justifies xbbbx (1945 time steps)
# misc7 = ["eqe", "qeq", "bxxxb", "xbbbx", 3227071918]

# clamps several times, settles for unjustified bbbxbbb (time 5888)
# misc8 = ["eqe", "qeq", "bxxxb", "bbbxbbb", 4244517374]

# snags, maps abc-xyz symmetrically, answers dyz (time 2257)
# misc9 = ["abc", "abd", "xyz", 692549763]
