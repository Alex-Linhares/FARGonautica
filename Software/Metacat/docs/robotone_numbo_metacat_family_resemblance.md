# Why Robotone looks like a cousin of Numbo and Metacat

*A philosophical companion to [`robotone_numbo_metacat_similarities.md`](robotone_numbo_metacat_similarities.md).
Robotone is the program in Ganesalingam & Gowers, "A fully automatic problem solver with
human-style output" (arXiv:1309.4501, 2013). Written 2026-10-02.*

---

"Family resemblance" is Wittgenstein's phrase, and it fits these three precisely. In the
*Philosophical Investigations* he asks what all games have in common and answers that nothing
does. There is no single essence of "game". Instead there is "a complicated network of
similarities overlapping and criss-crossing", the way family members share a nose here, a
gait there and a temperament elsewhere, with no one feature common to all.

Robotone, Numbo and Metacat are related in exactly this way. Try to find one defining trait
and it slips away:
- **Codelets and stochastic competition** link Numbo and Metacat but not Robotone, which is
  a deterministic priority list.
- **Subgoals** link Robotone and Numbo, but Metacat has none in that sense.
- **A faithful account of its own reasoning** links Robotone and Metacat, but Numbo only
  leaves a trace.
- **Temperature** is shared by the two FARG models, and Robotone has nothing like it.

No strand runs through all three, yet anyone who reads the three descriptions side by side
feels at once that they're relatives. The interesting question is why.

## Not descent, but convergence

The resemblance doesn't come from ancestry. Ganesalingam and Gowers's paper cites Polya,
Newell and Simon, Bledsoe, Boyer and Moore, and Bundy. It doesn't cite Hofstadter, Mitchell,
Defays or Marshall. Robotone isn't a descendant of Copycat. It grew in a different
tradition, automated theorem proving, from different soil.

That makes the likeness more telling. When unrelated lineages evolve the same eye, biologists
conclude that the problem shaped the solution. Something similar seems to have happened
here: researchers who took one particular question seriously were pushed to similar answers.
The question was not "how can a machine get the right answer?" but **"how does a mind arrive
at an answer?"**

## The shared commitment: the process is the result

Most of AI is judged by outputs: did the prover prove it, did the solver solve it? All three
projects reject that standard, and they do it in the same telling way: they choose problems
a machine finds trivial.
- Five bricks and a target can be enumerated in microseconds.
- Elementary metric-space lemmas are easy for any resolution prover.
- Letter-string analogies are tiny.

Picking such problems is a philosophical statement. It says the answer is already cheap and
the only thing worth studying is the *path*. Gowers forbids his program to "exploit the
speed of computers". Hofstadter's group builds models that could brute-force their domains
and deliberately don't.

This turns the usual relationship between constraint and capability upside down. Normally a
constraint limits what a system can do. Here the constraint, "do only what a human would
do", is the instrument of discovery. Gowers says so directly: by submitting to the
restriction, "we will force ourselves to develop a number of useful and important
techniques." The FARG view is the same: microdomains are microscopes, not toys. Adopting a
constraint as a method is one of the family's features.

## The common enemy: relevance

Once brute force is ruled out, every such system faces the same problem. Out of everything it
knows and everything it could do, how does it find what *matters* here? Philosophers call
this the frame problem, or the problem of relevance. Gowers names it without solving it:
humans "somehow" keep search under control and "somehow" select relevant facts from the mass
of what they know.

The three systems meet this problem with recognisably related moves, and that is where most
of the resemblance lives:
- **Knowledge waits to be called.** Robotone's library results fire only when hypotheses
  match their premises. Numbo's Pnet nodes and Metacat's slipnodes post codelets only when
  activated by what's in front of them.
- **Attention is finite and must be released.** Robotone deletes used-up hypotheses. Numbo
  kills blocks. Metacat breaks structures.
- **Annotations matter that logic doesn't need.** Tags like "used", activation and salience
  are, as Gowers puts it, "logically unnecessary" but "indispensable in human reasoning".
- **Safe moves come before risky ones.** Robotone's priority list and the FARG codelet
  urgencies differ in mechanism but express the same intuition: commit cautiously.

These aren't four separate inventions. They are four faces of one idea: **intelligence
consists largely in not thinking about most things.** In practice, a mind is defined as much
by what it ignores, forgets and defers as by what it computes.

## Cognition as recognition

Defays subtitled his Numbo chapter *A Study in Cognition and Recognition*, and that phrase
marks the deepest kinship. For FARG, perception isn't a front end that hands clean symbols to
a reasoner. It is reasoning, all the way down. Seeing 114 as "near 120" or seeing `abc` as a
successor group is already the creative act.

Robotone looks like a logical system, and its moves are sound inferences. Yet the paper's
account of mathematics is perceptual through and through. Most proofs, Gowers writes, have a
"story", built around "key ideas". Humans see that a statement is "just obviously false".
They recognise when a hypothesis is "used up". They reach intermediate statements "not by
brute-force search but by approximation". This is mathematics described as *seeing what the
situation calls for*. Robotone's priority list is an attempt to write down a mathematician's
trained eye. In that sense it is a recognition system wearing the clothes of a deduction
system, and that's why it sits comfortably beside two models whose whole subject is
high-level perception.

## Introspection as evidence

There's a methodological likeness too. All three were built partly from first-person
evidence:
- Gowers and Ganesalingam chose and ordered their moves "by examining our own reactions to
  many different problems".
- Hofstadter's group catalogued slips, errors, and how analogies feel from the inside.
- Defays drew on how people actually talk through number puzzles.

This is introspective cognitive science, in the line of Polya and of Newell and Simon's
protocol analysis. It accepts that the felt texture of thinking (the "aha", the sense of
being stuck, the urge to try something else) is real data about how thinking works.

From this comes the family's most distinctive trait: **the demand that a system's account of
itself be honest.** Robotone's write-ups must be "faithful to its thought processes",
narrated in the order the moves were made, not polished afterwards. Metacat's Temporal Trace
and self-commentary push the same demand further, aiming to make the system aware of its own
processing. Both reject the common practice of hiding how an idea was discovered behind a
clean final product. Gowers complains that mathematical papers are written "in a style that
appears to do its best to conceal how the ideas they contain were discovered". Marshall
could have written that sentence about AI systems.

## So what is the resemblance?

Not a shared mechanism, but a shared question, together with a shared refusal to answer it
cheaply. Each project asks how a mind finds its way through a space too large to search.
Each forbids itself the cheap answer. Each builds a model of relevance, attention and
commitment out of introspection and careful observation. Each then insists that the model be
able to account for its own path.

Projects that accept those commitments tend to converge in this way, even when they start in
different traditions. Like a family, they share no single trait, but enough overlapping ones
that they're clearly related.
