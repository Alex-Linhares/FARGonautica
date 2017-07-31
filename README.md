# Fluid Concepts & Creative Analogies

Congratulations you have made it to the center of the internet!

This repository has a number of goals:  

    i) we want to collect all the (open-source) computational architectures that have been developed in 
    Douglas Hofstadter's group's research in Fluid Concepts & Creative Analogies; from Numbo to Copycat to 
    Tabletop to Letter Spirit, Metacat, Musicat, etc, alongside the info on how to install and run it;
    ii) we want to collect all the literature (unpublished papers, CRCC reports, published papers, 
    Theses, etc);
    iii) we want to archive all materials for an online course on Fluid Concepts & Creative Analogies 
    (also called "Fluid Concepts", "FARG architectures", "Copycat Architecture", "Active Symbols", etc).
    
    
     
    

Welcome!  Join us.  This will be fun.  




PROJECTS TO JOIN HERE (desiderata)
---

The ambition is to catalogue and archive the sourcecode and literature involved in building new Fluid Concepts 
Architectures to new domains.  Eventually we hope to have a complete set of archived projects (or linked from 
here when being maintained).

Numbo  
Copycat  
Tabletop  
Letter Spirit  
Metacat  
Phaeaco  
Musicat  
Capyblanca  
George  
SeqSee  


PROJECT: COPYCAT
---
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
accordingly (bottom-up processes), and the current state of slipnet determines probabilistically which codelets must be 
run (top-down influences).

Copycat was implemented in LISP, and there are multiple ports available.  Scott Boland did one of the first re-implementations 
in Java (available in this repo and in https://archive.org/details/JavaCopycat). This implementation has served for another 
port, to Python, which is available here: https://github.com/Quuxplusone/co.py.cat. 

AJ Hager has also independently developed a python version of copycat, with some amazing OpenGL graphics, available 
(and maintained) at https://github.com/ajhager/copycat



PROJECT: METACAT
---



PROJECT: NUMBO
---

Daniel Defays sent in 2006 a printed version of the sourcecode to Alex Linhares.  This will be scanned and archived in this 
repo---eventually.


PROJECT: SEQSEE
---
A Concept-centered Architecture for Sequence Perception
Abhijit A. Mahabal

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


The dissertation (Aivailable here and in http://www.amahabal.com/files/Seqsee--doublesided.pdf) 
describes the program and its principles, which are much more general 
than integer-sequence extrapolation, and compares its performance with human
performance.

Seqsee is written in Perl and a new version is written in Python and the code is maintained 
by Dr. Mahabal---which is why we do not provide the code in this git repo.  For the latest
versions, see:

Perl version: https://github.com/amahabal/Seqsee 
Python version: https://github.com/amahabal/PySeqsee 


PROJECT: CAPYBLANCA
---
A chess system based on Active Symbols / Fluid Concepts

Most chess programs consist of nothing but tree search. Can a program have ideas; perceive abstractions; 
understand situations? If so, how?

This project is based on the computational cognitive models of Fluid Concepts, such as Copycat, Letter Spirit, 
Phaeaco, Tabletop, Numbo, Seeqsee, etc. These projects have been developed by Douglas Hofstadter and his PhD students.

Capyblanca has been developed in Delphi. As of May 7th, 2008, the codebase compiles and runs in free (as in beer) 
turbodelphi for windows.

With this initiative, we hope to contribute to the scientific community by letting those interested not only replicate 
the results in detail, but also improve the architecture and explore different design decisions which we have not been 
able to. (A major task, discussed in the conclusion, concerns the integration of learning algorithms.)

A major point concerning FARG architectures is that programs do not rely on procedure calling; they do, instead, launch 
tasks which are to be handled asynchronously (Hofstadter and FARG, 1995). As mentioned, the task scheduler is known as 
the coderack, for processes may be triggered from any part of the task queue. It is impossible to detail the thousands 
of lines of the whole code here, but by focusing on some of the units involved, we may have a better grasp of how the 
system is developed, how it works, and how one might be able to further develop it and extend the architecture.

Pascal code is divided over units. The major units involved in the current implementation of Capyblanca are detailed 
below.

• MainUnit: Creates the graphical user interface; loads particular chess positions into memory; initializes working 
memory, external memory, and the slipnet; lets the user test the system; includes some basic tests.

• Slipnet Unit: Creates basic semantic nodes and their associations (for example, a piece can be a guardian or an 
attacker--each of these roles has a corresponding node)

• ExternalMemoryUnit: Creates a chess position; includes basic code for piece movement, potential trajectories that 
each piece might access, and the piece's "spheres of influence". Includes code that will attach abstract roles to pieces 
(or detach them, eventually--see fig. 8)

• WorkingMemoryUnit: Creates the representation for the "ImaginedBoard", that is, the board in the system's "mind's eye". 
Notice that this is not equivalent to external memory. For example, external memory starts with a complete position with all 
pieces, while working memory is gradually filled with bottom-up, data-driven information glanced from external memory, or 
"imagined", top-down, expectation-driven trajectories and roles.

• SpecialFunctions Unit: GUI-related; displays only parts of graphics associated with the current representation of 
trajectories.

• BasicImpulse Unit: Implementation of an abstract class "TImpulse", which has the basic data structures and associated 
methods for use on more specific subclasses.

• ImpulseSquareAndPiece Unit: The impulses implemented here work at a low level, "looking" at squares in the board, 
creating structures in STM if a piece is found, and finding trajectories and relations between a piece and either a 
square or a piece.

• AbstractImpulses Unit: Handles the processing of abstract roles, such as the role of attacker, or guardian, or, 
in the case of a pawn, of a potential promotion, etc. Creates the corresponding roles for further processing.

• ImpulseConsiderations Unit: This is the most abstract level of processing in Capyblanca. This unit attempts to model 
"abstract thoughts", i.e., considerations which are NOT tied to an specific piece or square, and are in their most 
general form. One example is: "the only solution to a double check is to escape with the king". In this example, nothing 
is said about the types of pieces attacking the king, their positions, the color that is under check, etc. Another example, 
that of a piece that finds itself having to juggle between two different, incompatible roles, are presented in the sample 
run detailed below (in the case of the black king of position 6).


Sourcecode previously on https://code.google.com/archive/p/capyblanca/, now available here, in the /Software folder.



PROJECT: MUSICAT
---
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
