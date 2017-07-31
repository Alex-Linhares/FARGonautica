# Fluid Concepts & Creative Analogies

Congratulations you have made it to the center of the internet!

This repository has a number of goals:  

    i) we want to collect **all** the (open-source) computational architectures that have been developed in 
    Douglas Hofstadter's group's research in Fluid Concepts & Creative Analogies; from Numbo to Copycat to 
    Tabletop to Letter Spirit, Metacat, Musicat, &tc, alongside the info on how to install and run it;
    ii) we want to collect **all** the literature (unpublished papers, CRCC reports, published papers, 
    Theses, &tc);
    iii) we want to create an online course, and perhaps a collaborative course book, on Fluid Concepts 
    & Creative Analogies (also called "Fluid Concepts", "FARG architectures", "Copycat Architecture", &tc).    
     
    

Welcome!  Join us.  This will be fun.  




PROJECTS TO JOIN HERE (desiderata)
---



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


Source code previously on https://code.google.com/archive/p/capyblanca/, now available here, in the /Software folder.





 
