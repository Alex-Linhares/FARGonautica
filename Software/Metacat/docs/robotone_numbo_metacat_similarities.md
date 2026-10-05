# Robotone, Numbo and Metacat: the similarities

A short comparison of three systems:
- **Robotone**, the program in M. Ganesalingam & W. T. Gowers, *A fully automatic problem
  solver with human-style output* (arXiv:1309.4501, 2013; expanded as *A Fully Automatic
  Theorem Prover with Human-Style Output*, J. Automated Reasoning, 2017). "Robotone" is
  the name its output file uses.
- **Numbo**, by Daniel Defays (1987); see `~/dev/numbo`.
- **Metacat**, by James Marshall; see this repository.

This document covers only what the three share. The differences are large and deliberately
left out.

Sources: the arXiv paper, Numbo's Python engine (`~/dev/numbo/python/numbo/`) and its
README, and the Metacat source in `chez_scheme/original/`. Written 2026-10-02.

---

**1. The same goal: model how people think, not how to compute an answer.** All three
refuse the brute-force route. Gowers calls his approach the "extreme human" end of the
spectrum. Numbo and Metacat come from the Fluid Analogies Research Group (FARG), which
studies how people perceive and reason.

**2. Toy domains on purpose.** The problems are trivial for a machine. Any prover handles
elementary metric-space lemmas, and five bricks and a target can be solved by enumerating
every combination. Letter-string analogies are tiny. In all three the problem is a
microscope for watching a process, and *how* the answer is reached is the whole point.

**3. Long-term memory and working memory are kept apart.**

| | Long-term knowledge | Working memory |
|---|---|---|
| Robotone | library of definitions, facts, rewrite rules (the paper says it "models … long-term memory") | hypotheses and targets |
| Numbo | Pnet (arithmetic facts such as 20×6=120, landmark numbers) | cytoplasm (bricks, blocks, targets) |
| Metacat | Slipnet (letter and relation concepts) | Workspace (letters, bonds, groups, bridges) |

**4. Knowledge acts only when the situation calls for it.** Robotone applies a library
result only when the current hypotheses match its premises. In Numbo, a Pnet node activated
by the bricks or the target posts its own codelets. In Metacat, activated slipnodes post
top-down codelets. In all three, adding knowledge shouldn't slow the system down, because
nothing fires unless it's relevant.

**5. Working backwards from the target by creating subgoals.** Robotone's backwards
reasoning replaces a target with simpler targets. Numbo's decomposition codelets do the
same with numbers: 114 is near 120, so it builds 120 and creates a secondary target of 6.

**6. Notes about statements that logic doesn't need but thinking does.** Robotone tags
statements as "used" or "vulnerable", and the paper calls this "logically unnecessary" but
"indispensable in human reasoning". Numbo's nodes carry activation, status and misfortune.
Metacat's structures carry strength, salience and importance.

**7. Letting go of what's no longer useful.** Robotone's deletion moves discard hypotheses
that are used up or can't match anything, as a model of attention. Numbo kills blocks and
secondary targets, and when the run stays hot for too long it starts taking its own
structures apart. Metacat's breaker codelets and competing structures do the same. All
three treat forgetting as part of thinking.

**8. Graded preferences for some actions over others.** Robotone ranks its move types from
safe to risky: tidying first, expansion later, suspension last. Numbo and Metacat express
the same idea through codelet urgencies. Cheap, safe probes come first and expensive
commitments later, which is Metacat's terraced scan. The mechanisms differ (a fixed
priority list versus probabilities), but the underlying idea is the same.

**9. A single measure of how things are going.** Robotone has no explicit one. Gowers's
stated open problem, that humans "somehow" keep search under control, is what temperature
in Numbo and Metacat is for. In Numbo, temperature is computed from the misfortune of
secondary nodes and decides when to dismantle and start over.

**10. Explaining one's own reasoning.** Robotone's write-up must be "faithful to its thought
processes", narrating moves in the order they happened. Gowers also plans a separate
"proof-discovery account". Metacat's Temporal Trace and commentary are that idea, aimed at
its own processing. Numbo's traces record step by step how a solution was built.

**11. Judged by how human the output looks.** Gowers tested whether blog readers could tell
Robotone's proofs from students' proofs. The FARG models are judged the same way: by
whether their answers, preferences and paths look like people's.
