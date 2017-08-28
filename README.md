# FARGlexandria
**the lost library of Fluid Concepts & Creative Analogies projects**



Don't you just hate it when you get a new computer and you have to scour the entire internet in search of all the FARG projects yet once again? All those long days of suffering by googling for everything; finding out that your old files are corrupt; and email spamming the authors? Aren't you tired of having your wife and kids furiously screaming at you just because they need to run Letter Spirit with the boat style? And when was the last time you checked the integrity of pcsv84.exe to make sure you were ready to run chez scheme?  Haven't you always dreamt of a magical forward-looking place where you could get every single FARG project?  Well do we have good news for you! As from now on with this exclusive, limited-time offer, all your problems have been solved! Now you can scream out the window that those days of excruciating agony are finally gone. Congratulations, sir, for you have made it to the very center of the internet--the very place where the smart and the fine-looking congregate.  Open up that big smile and tap yourself on the back to fully enjoy your modern conveniences of cloning this repo in your notebook, in your boss' desktop unbeknownst to him, in public libraries, Apple stores, and more!  Never forget: having all FARG projects safely backed-up, sharply organized, and always ready to run is the very recipe for a modern happy family.

This repository has a number of goals:  

    i) we want to collect all the (open-source) computational architectures that have been developed in
    Douglas Hofstadter's group's research in Fluid Concepts & Creative Analogies; from Numbo to Copycat to
    Tabletop to Letter Spirit, Metacat, Musicat, etc, alongside the info on how to install and run it;
    ii) we want to collect all the literature (except for published books, of course):
     - unpublished papers,
     - CRCC reports,
     - published papers,
     - Theses, etc
    iii) we want to archive all materials for an online course on Fluid Concepts & Creative Analogies
    (also called "Fluid Concepts", "FARG architectures", "Copycat Architecture", "Active Symbols", etc).


Welcome!  Join us.  This will be insanely fun.  




PROJECTS TO JOIN HERE (desiderata)
---

The ambition is to catalogue and archive the sourcecode and literature involved in building new Fluid Concepts
Architectures to new domains.  Eventually we hope to have a complete set of archived projects (or linked from
here when being maintained).

 | Project | Author | Language | Available at [FARGlexandria](https://github.com/Alex-Linhares/FARGlexandria)? |
 |---------| --------- | ----------| ----------|
 | Seek-Whence | [Marsha Meredith](https://scholar.google.com.br/scholar?hl=en&q=marsha+meredith&btnG=&as_sdt=1%2C5&as_sdtp=) | ?? | Not Yet |
 | Numbo | [Daniel Defays](http://www.ulg.ac.be/cms/c_6099593/fr/repertoires?uid=U011369) | Lisp | **Yes (coming soon)** |
 | Copycat | [Melanie Mitchell](http://web.cecs.pdx.edu/~mm/) | Lisp (also Java & Python) | **Yes** |  
 |Tabletop | [Robert M. French](http://leadserv.u-bourgogne.fr/en/members/robert-m-french) | ?? |  Not yet |
 | Letter Spirit | [Gary McGraw](https://www.garymcgraw.com/) | Scheme |  Not yet |
 | Letter Spirit | John Rehling | ?? | Not yet |
 | Metacat | [James Marshall](https://www.sarahlawrence.edu/faculty/marshall-james.html) | Chez Scheme |  **Yes** |
 | Phaeaco | [Harry Foundalis](http://www.foundalis.com/) | C++ | Not open-source currently |
 | Musicat | [Eric Nichols](https://github.com/eraoul) | C# | **Yes** |
 | Capyblanca | [Alex Linhares](https://github.com/Alex-Linhares) | Delphi | **Yes** |
 | George | [Francisco Lara-Dammer](https://www.linkedin.com/in/francisco-lara-dammer-ba5a3ab2/) | Java | Not yet |
 | SeqSee | [Abhijit Mahabal](https://github.com/amahabal) | Perl | **Yes** |


PROJECT: COPYCAT
---
![Copycat](https://camo.githubusercontent.com/fce9eca50ac44e256082460f36bca5fd48c56e46/687474703a2f2f692e696d6775722e636f6d2f6c484d776e2e706e67)

[Melanie Mitchell](http://web.cecs.pdx.edu/~mm/) & [Douglas Hofstadter](http://www.cogs.indiana.edu/people/profile.php?u=dughof)

Find Dr. Mitchell in  [Homepage](http://web.cecs.pdx.edu/~mm/)  |  [Wikipedia](https://en.wikipedia.org/wiki/Melanie_Mitchell)  |  [Google Scholar](https://scholar.google.com.br/citations?user=k4gbv2AAAAAJ&hl=en&oi=ao)  |  [Academia.edu](https://pdx.academia.edu/MelanieMitchell)

Copycat is a model of analogy making and human cognition based on the concept of the parallel terraced scan, developed
in 1988 by Douglas Hofstadter, Melanie Mitchell, and others at the Center for Research on Concepts and Cognition,
Indiana University Bloomington.

Copycat produces answers to such problems as "abc is to abd as ijk is to what?" (abc:abd :: ijk:?). Hofstadter and
Mitchell consider analogy making as the core of high-level cognition, or high-level perception, as Hofstadter calls it,
basic to recognition and categorization. High-level perception emerges from the spreading activity of many independent
processes, called codelets, running in parallel, competing or cooperating. They create and destroy temporary perceptual
constructs, probabilistically trying out variations to eventually produce an answer. The codelets rely on an associative
network, slipnet, built on pre-programmed concepts and their associations (a long-term memory). The changing activation
levels of the concepts make a conceptual overlap with neighboring concepts.

Copycat's architecture is tripartite, consisting of a slipnet, a working area (also called workspace, similar to
blackboard systems), and the coderack (with the codelets). The slipnet is a network composed of nodes, which represent
permanent concepts, and weighted links, which are relations, between them. It differs from traditional semantic networks
as the effective weight associated with a particular link may vary through time according to the activation level of
specific concepts (nodes). The codelets build structures in the working area and modify activations in the slipnet
accordingly (bottom-up processes), and the current state of slipnet determines probabilistically which codelets must be run (top-down influences).

Implementations of the Copycat project:

| Author  |  Language/Sourcecode |  Note  |
|--------|------------------------|-------|
| Melanie Mitchell |  [LISP](http://web.cecs.pdx.edu/~mm/how-to-get-copycat.html) | Bitrotten |  
| Scott Boland  | [Java](/Software/Copycat/Scott-Boland-implementation%20in%20JAVA/JavaCopycat.zip) | (SB's email lost?) |
| ["speakeasy"](https://github.com/speakeasy)  |  [Java](https://github.com/speakeasy/CopyCat)  |  fork of Scott Boland  code |
| [Greg Detre](https://github.com/gregdetre) | [Clojure](https://github.com/gregdetre/copycat-clojure)  | Not tested  |
| Multiple Authors | [Python 2](https://github.com/Quuxplusone/co.py.cat)  | Multiple forks, no graphics  |
| [Lucas Saldyt](https://github.com/LSaldyt) | [Python 3](https://github.com/LSaldyt/copycat/commit/bc848e8f2d7dd922b96bd0118ba4adf2fc033e55)  | Above repo patch to Python 3  |
| [Joseph Aaron Hager](https://github.com/ajhager) | [Python 3](https://github.com/ajhager/copycat) | [OpenGL graphics](https://youtu.be/v7Eotp-XKVA) |


Please see [Mitchell](http://web.cecs.pdx.edu/~mm/)'s [Copycat page](http://web.cecs.pdx.edu/~mm/how-to-get-copycat.html). Scott Boland's re-implementation in Java is 404'ing at The University of Queensland, but it is available [here in this repo](/Software/Copycat/Scott-Boland-implementation%20in%20JAVA/JavaCopycat.zip) and in [archive.org](https://archive.org/details/JavaCopycat).



PROJECT: METACAT
---
![Metacat](/Software/Metacat/metacat.png)

[James Marshall](https://www.sarahlawrence.edu/faculty/marshall-james.html) & [Douglas Hofstadter](http://www.cogs.indiana.edu/people/profile.php?u=dughof)

Find Dr. Marshall in [Sarah Lawrence College](https://www.sarahlawrence.edu/faculty/marshall-james.html)  |  [Personal Website](http://science.slc.edu/~jmarshall/)  |  [Google Scholar](https://scholar.google.com.br/scholar?q=james+marshall+metacat&btnG=&hl=en&as_sdt=0%2C5&oq=james+marshall+)

Dr. Marshall is maintaining [the Metacat Project page](http://science.slc.edu/~jmarshall/metacat/).



PROJECT: NUMBO
---

Find Dr. Defays in [Homepage]()  |  [Google Scholar]()

Daniel Defays sent in 2006 a printed version of the sourcecode to Alex Linhares.  This will be scanned
and archived in this repo---eventually.


PROJECT: SEQSEE
---
A Concept-centered Architecture for Sequence Perception

[Abhijit A. Mahabal](https://github.com/amahabal) & [Douglas Hofstadter](http://www.cogs.indiana.edu/people/profile.php?u=dughof)

Find Dr. Mahabal in  [Google Scholar](https://scholar.google.com.br/citations?user=67MnndUAAAAJ&hl=en&oi=ao)  |  [Academia.edu](https://google.academia.edu/AbhijitMahabal)

One of the goals of this project is to design and implement a computer program
that can extend integer sequences intelligently, and the project has resulted in the
creation of the program named ―Seqsee‖ (pronounced ―sexy‖). Seqsee can extend a wide
range of cognitively interesting sequences, including the following sequence (Seqsee is
presented the sequence without the groupings indicated by the parentheses):

    ( (1) ) ( (1) (1 2) ) ( (1) (1 2) (1 2 3) )

If people are shown this sequence (without the parentheses), they quickly form a
group consisting of the three initial ―1‖s, but then realize that each plays a slightly
different role in the sequence. Like people, Seqsee is initially distracted by the three
consecutive ―1‖s, but gradually figures out that the second ―1‖ is an ascending group,
and that the initial ―1‖ is an ascending group made up of one ascending group.

Architecturally, Seqsee is a descendant of Hofstadter & Mitchell‘s computer
program Copycat, and adds several novel features that allow it to easily modify behavior
in response to its recent perceptions, to form specific expectations such as ―an
ascending group is likely to be located here‖, to more quickly understand sequences
having previously seen similar sequences, to see an entity as something else, and to do
all this without the use of brute force.

Seqsee uses several ideas in achieving its goals: William James‘ notions of the
fringe and the stream of thought; analogies between objects; categorization and labeling
of objects and of situations, and the detection of categories without using brute-force
tests for all sorts of categories; the notion of context which influences and is influenced
by perception; the notion, similar to affordances, of the ―action fringe‖ of an object; and
a category-based long-term memory.


The dissertation (Available here and in [Dr. Mahabal's website](http://www.amahabal.com/files/Seqsee--doublesided.pdf)
describes the program and its principles, which are much more general
than integer-sequence extrapolation, and compares its performance with human
performance.

Seqsee is written in Perl and a new version is written in Python and the code is maintained
by Dr. Mahabal---which is why we do not provide the code in this git repo.  For the latest
versions, see:

[Perl version, original project](https://github.com/amahabal/Seqsee)

[Python version being rewritten](https://github.com/amahabal/Pyhttps://scholar.google.com.br/scholar?hl=en&q=Francisco+Lara-Dammer&btnG=&as_sdt=1%2C5&as_sdtp=&oq=francisco+Seqsee)

PROJECT: George
---
Modeling Human Discoverativity in Geometry

[Francisco Lara-Dammer](https://www.linkedin.com/in/francisco-lara-dammer-ba5a3ab2/) &  [Douglas Hofstadter](http://www.cogs.indiana.edu/people/profile.php?u=dughof)

Find Dr. Lara-Dammer at [LinkedIn](https://www.linkedin.com/in/francisco-lara-dammer-ba5a3ab2/)  |  [Homepage at IU](https://www.cs.indiana.edu/~flaradam/)  |  [Google Scholar](https://scholar.google.com.br/scholar?hl=en&q=Francisco+Lara-Dammer&btnG=&as_sdt=1%2C5&as_sdtp=&oq=francisco+)  |  [Academia.edu (?)](https://independent.academia.edu/FranciscoLaraDammer) 

Publications: 
- Modeling Human Discoverativity in Geometry, PhD Thesis, Indiana University, 
- Lara-Dammer & Hofstadter, [*Perception of Direction and its Influence on Geometric Discoveries*](https://cogsci.indiana.edu/pub/lara-dammer.perception-of-direction-2006.pdf)
- [Lara-Dammer, Hofstadter & Goldstone (2017) A computer model of context-dependent perception in a very simple world, *Journal of Experimental and Theoretical Artificial Intelligence* 1--36](http://www.tandfonline.com/eprint/hNcJzAjrviqTaWjzR235/full)


PROJECT: CAPYBLANCA
---
![Capyblanca](/Software/Capyblanca/Capyblanca.Screenshot.png)

[Alex Linhares](https://github.com/Alex-Linhares) & [Douglas Hofstadter](http://www.cogs.indiana.edu/people/profile.php?u=dughof)

Find Linhares in  [Homepage](http://ebape.fgv.br/en/faculty/alexandre-linhares) |  [Academia.edu](https://fgv.academia.edu/AlexandreLinhares)  |  [Google Scholar](https://scholar.google.com.br/citations?user=jzP1_8QAAAAJ&hl=en&oi=sra)  

A chess system based on Active Symbols / Fluid Concepts.  [Here](https://youtu.be/5h67WwfuQDg) is the sample run of it described in the [Information Sciences paper](https://github.com/Alex-Linhares/Fluid-Concepts-and-Creative-Analogies/blob/master/Literature/Chess-Capyblanca-2014-Linhares-Information%20Sciences.pdf).

Most chess programs consist of nothing but tree search. Can a program have ideas; perceive abstractions;
understand situations? If so, how?

This project is based on the computational cognitive models of Fluid Concepts, such as Copycat, Letter Spirit,
Phaeaco, Tabletop, Numbo, Seeqsee, etc. These projects have been developed by Douglas Hofstadter and his PhD students.

Capyblanca has been developed in Delphi. As of May 7th, 2008, the codebase compiles and runs in free (as in beer)
turbodelphi for windows.

Sourcecode:
 - previously on https://code.google.com/archive/p/capyblanca/
 - now available here, in the [Software folder](/Software/Capyblanca).

 Publications in Capyblanca & Analogies in Chess:
  - [Linhares, A. (2005) An Active Symbols Theory of Chess Intuition.  *Minds and Machines* 15 131--181.](/Literature/Chess-Capyblanca-2005-Linhares-Minds_and_Machines.pdf)
  - [Linhares, A., & Brum, P. (2007) Understanding our understanding of strategic scenarios: What roles do chunks play?  *Cognitive Science* 31 989--1007](/Literature/Chess-Capyblanca-Linhares.and.Brum-2007-Cognitive%20Science.pdf)
  - [Linhares, A., & Brum, P. (2009) How can experts see the invisible? Reply to Bilali & Gobet, *Cognitive Science* 33 748--751](/Literature/Chess-Capyblanca-Linhares.and.Brum-2009-Cognitive%20Science.pdf)
  - [Linhares, A. & A.E.T.A. Freitas (2010) Questioning Chase and Simon's (1973) "Perception in Chess": The "experience recognition" hypothesis, *New Ideas in Psychology* 28 64--78](/Literature/Chess-Capyblanca-Linhares.and.Freitas-2010-New%20Ideas%20in%20Psychology.pdf)
  - [Linhares, A., A.E.T.A. Freitas, A. Mendes, & J.S. Silva (2012) Entanglement of Perception and Reasoning in Chess: differential errors of strategic reconstruction *Cognitive Systems Research* 13 72--86](/Literature/Chess-Capyblanca-Linhares%20et%20al-2012-Cognitive%20Systems%20Research.pdf)
  - [Linhares, A., and Chada, D. (2013) What is the nature of the mind's pattern recognition in chess? *New Ideas in Psychology* 31 108--121](/Literature/Chess-Capyblanca-Linhares.Chada-2013-New%20Ideas%20in%20Psychology.pdf)
  - [Linhares, A. (2014) The Emergence of Choice: decision-making and strategic thinking through analogies, *Information Sciences* 259 36--56](/Literature/Chess-Capyblanca-2014-Linhares-Information%20Sciences.pdf)

Future and ongoing activities:
  - A review paper, containing the major findings in analogies in chess and of the Capyblanca architecture, is being written--perhaps for publication as a chapter in the sequel of *Fluid Concepts and Creative Analogies*.
  - Linhares plans to rewrite a number of AI/cogsci projects that may be of relevance to a future architecture, such as [ENTROPICA](https://www.ted.com/talks/alex_wissner_gross_a_new_equation_for_intelligence), [KT-Forms](http://www.pnas.org/content/105/31/10687), and complex Clusterings.
  - A rewrite of Daniel Defays "Numbo" in [Dr. Mahabal's Fluid Concepts framework](https://github.com/amahabal/PySeqsee) is in the plans
  - A rewrite of Capyblanca in [Dr. Mahabal's Fluid Concepts framework](https://github.com/amahabal/PySeqsee) is in the plans
  - An original, Fluid-Concepts-based, re-write of Kemp and Tenembaum's Forms in Mahabal's Framework is in the plans
  - ...as is the idea of a book tentatively entitled "Fluid concepts: a course"
  - The long-term goal is to create a computational architecture that, without knowing the rules or goals of a game, can come to understand and play combinatorics games like checkers, chess, or go.
  - Another long-term goal is to create a computational architecture that is able to solve Bongard problems and Raven's matrices. Extending Phaeaco beyond what is currently possible.
  - A paper on a "measure of human intuition" is in the plans, after Dr. Eric Nichols agreed that the idea and the methodology seems to make sense.
  - Some ideas on a technical, mathematical, definition of what is commonly called *strong AI* or *AI-Complete* are being sketched. They use mostly combinatorics and information theory... but they stem from the solving of Bongard problems.

If you are interested in joining these future activities, please get in touch!


PROJECT: MUSICAT
---
![Musicat](http://ericpnichols.com/images/musicat.png)

[Eric Paul Nichols](https://github.com/eraoul) & [Douglas Hofstadter](http://www.cogs.indiana.edu/people/profile.php?u=dughof)

Find Nichols in  [Google Scholar](https://scholar.google.com.br/citations?user=Q37xBqwAAAAJ&hl=en)  |  [Academia.edu](https://indiana.academia.edu/EricNichols)

What happens when people listen to music? What sorts of mental structures are formed? How do we make sense of a melody
as its notes fly by in rapid succession? Can we model the experience of listening to music in real time?

Musicat is a model of real-time melody perception by people. The program “listens” to monophonic Western
tonal melodies one note at a time (presented not as audio recordings, but rather in a symbolic form much like sheet
music) and generates an internal representation of the musical structures it “hears”. These structures include groups of
adjacent notes, meta-groups comprised of smaller groups, expectations for upcoming structures, and, most importantly,
analogies between groups (and meta-groups) of various sizes. In the model, listening is not a passive process; instead,
it is an active, dynamic process of creating mental structures. Thus when Musicat listens to a melody, I consider such
an act to be creative, and I call it a “listening performance”.

PhD Thesis of Dr. Eric Paul Nichols is available here: http://ericpnichols.com/musicat/pdf/MusicatDissertation.pdf, and
the sourcecode is available at http://ericpnichols.com/musicat/ (and also archived here).  


PROJECT: PHAEACO
---
![Phaeaco](phaeaco-the-solver.gif)

[Harry Foundalis](http://www.foundalis.com/) & [Douglas Hofstadter](http://www.cogs.indiana.edu/people/profile.php?u=dughof)

Find Dr. Foundalis in [homepage](http://www.foundalis.com/)  |  [Google Scholar](https://scholar.google.com.br/scholar?hl=en&q=foundalis+harry&btnG=&as_sdt=1%2C5&as_sdtp=&oq=harr)

Phaeaco is an architecture that is able to solve some Bongard-problems in a psychologically plausible way.  Numerous
people believe that Bongard problems are one of the quintessential challenges of AI and Cognitive Science.  Here is what
Dr. Foundalis said about them:

    "I see Bongard problems not as a mere collection of cute visual puzzles, but as a gateway that
    allows us to get a glimpse at the foundations of cognition: a set of principles that are as
    fundamental for cognitive science as Newton’s laws are for physics. Bongard problems themselves
    are solved primarily as a consequence of following some fundamental principles of cognition."

Dr. Foundalis is not sharing the code at this time, but his thesis is found in [the Software Folder](/Software).


PROJECT: LETTER SPIRIT
---
Defcon 1. 404 file not found so far...

If you have any information (screenshots, sourcecode, homepage / github page of authors, email, etc.), please let us solve issues #5 and #6 of this repository.

[Gary McGraw](https://www.garymcgraw.com/)

Find Dr. McGraw in  [Google Scholar](https://scholar.google.com.br/scholar?q=mcgraw+letter+spirit&btnG=&hl=en&as_sdt=0%2C5)  |  [Amazon Author's page](https://www.amazon.com/Gary-McGraw/e/B000APFZ2S/ref=sr_ntt_srch_lnk_1?qid=1503736392&sr=8-1)  




John (Andrew?) Rehling

Find Dr. Rehling in  [Google Scholar](https://scholar.google.com.br/scholar?hl=en&q=john+rehling&btnG=&as_sdt=1%2C5&as_sdtp=)

2017-08-26 Sent an email to reputation.com.. I think he published a patent there.
