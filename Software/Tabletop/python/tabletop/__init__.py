"""Tabletop (French, 1992), reconstructed from the specification in Python.

Robert M. French's Tabletop is a model of analogy-making in a microworld of table settings.
French asks us to imagine "two people, Henry and Eliza, seated at a small table facing each
other" [F95 p. 25]: Henry touches an object and says "Do this!", and Eliza must respond by doing
"the same thing" from her side of the table [F95 p. 25]. The program plays Eliza. Its
architecture is Copycat's: it "consists of three main components: the Slipnet, the Workspace,
and the Coderack" [F95 p. 55]. No source code survives, so this package is a reconstruction from
French's book (F95), FCCA chapters 8-9 and the 1991 paper (FH91): every module cites the pages
it implements, and every choice the sources leave open is a numbered decision in DECISIONS.md.

The engineering conventions: standard library only; one seeded generator per run
(`tabletop.rng`, D-064); a fresh world per run; typed events.
"""

__version__ = "0.0.1"
