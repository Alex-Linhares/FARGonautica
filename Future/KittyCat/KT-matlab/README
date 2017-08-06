July 2008. Charles Kemp. (ckemp@cmu.edu)

Code for the form discovery model described in:

Kemp, C., &  Tenenbaum, J. B. (2008). The discovery of structural form.
PNAS.

See also the supporting information, which includes a detailed description
of the model and helps to explain how the code works.

Structure Learning Code
-----------------------

Input:	features (missing data allowed), similarity ratings, or relations

Goal :  fit a graph structure to the data. Return the best structure and the
        posterior probability of that structure given the data.

Structures: partitions, chains, orders, rings, hierarchies, trees, grids,
	    and cylinders. We include versions with and without self-links, 
	    and directed and undirected versions for many of these structures. 
	    For instance,
		    ring:	     directed ring with self links
		    dirringnoself:   directed ring with no self links
		    undirring:	     undirected ring with self links
		    undirringnoself: undirected ring with no self links

Usage
-----

The top level file is masterrun.m. Type 

>> masterrun

and the code will fit three forms (chain, ring and tree) to three data
sets. If 'neato' is installed (part of the GraphViz package -- see below)
then one figure window will show the progress of the current search, and a
second will show the best solution found for the data set most recently
analyzed.

To run other analyses, edit masterrun.m to specify the structures and data
sets you want to explore, and the number of times to run each analysis.

Data:
-----

Three kinds of data are included: 

i)  small feature data sets: 
    demo_chain_feat, demo_ring_feat, demo_tree_feat

ii) small relational data sets:
    demo_ring_rel_bin, demo_hierarchy_rel_bin, demo_order_rel_freq
    
    The first two are binary relations, and the third is a frequency matrix.

iii) larger synthetic data sets (see Fig S5 in the Supporting Information
			         for the PNAS paper)
     synthpartition, synthchain, synythring, synthtree, synthgrid

iv) real world data (feature, relational and similarity) 
    feature data: animals, judges
    similarity:   colors, faces, cities 
    relational:   mangabeys, bushcabinet,  kularing, prisoners

To add more data sets, edit setpsexport.m (ps.data contains the names for each
data set, and ps.dlocs contains locations of each data set).

Output:
-------
Results are saved in the file specified in masterrun.m 

STRUCTURE{i,j}: the best instance of form I for dataset J
MODELLIKE(i,j): posterior probability of best instance of form I for
		dataset J
NAMES{J}:	names of the entities in data set J


The program also creates a results directory which stores some of the
states visited during each search. The files in this directory can be
ignored.

Data structures:
----------------
Each graph is represented as a Matlab structure with many fields. The most
important fields in a structure g are described here:

g.objcount:	    number of objects in the structure
g.adjcluster:	    a cluster graph represented as an adjacency matrix (a
		    cluster graph is a graph where the nodes correspond to 
		    clusters, not individual objects)
g.adjclustersym:    symmetric version of g.adjcluster
g.Wcluster:	    a weighted cluster graph. The weight of an edge is the
		    reciprocal of its length
g.adj:		    a graph including object nodes and cluster nodes. The
		    first g.objcount nodes are object nodes, and the rest
		    are cluster nodes.
g.W:		    a weighted version of g.adj
g.z:		    cluster assignments for the objects
g.ncomp:	    number of graph components. Will be 2 for direct
		    products (e.g. a cylinder has two components -- a chain and			    a ring)
g.components:	    cell array storing the graph components 
g.extlen:	    length of all external branches (branches that connect an
		    object to a cluster). Used only if all external branch
		    lengths are tied.
g.intlen:	    length of internal branches (branches that connect a
		    cluster to a cluster. Used only if all internal branch 
		    lengths are tied.

Displaying results:
------------------

This function may be useful for visualizing the structures found:

draw_dot.m:	    show a graph (takes an adjacency matrix as input);

It relies on 'neato' which isn't part of this release, but is part of the
GraphViz package:

http://www.graphviz.org/

Algorithm
---------

Each structure is associated with one or more node replacement graph
grammars.  We initialize the structure search with a single-node graph. At
each stage we try splitting each node according to the active graph
grammars (see loop in structurefit.m). Each split creates two children: we
seed the two children with random members of the parent node, and greedily
assign all remaining members of the parent node to the children nodes (see
best_split.m). We choose the best of these splits, try to improve the
resulting graph using a series of heuristics (see gibbs_clean.m) and
continue.

Generative model for features: 
------------------------------

Assume that the underlying structural form F is known, and we want to
generate a feature matrix (n objects by m features). First a graph of form
F is sampled from the set of F-consistent graphs, where each graph is
weighted by the number of nodes it contains.  Features are then generated
over this graph using a Gaussian process (see Zhu, Ghahramani and
Lafferty).  (Technical detail: we combine a Gaussian random field with a
prior that expects the variance at each object node to be sigma (drawn
from an exponential prior with parameter sigbeta)).

Only the covariance matrix of the feature data turns out to matter. We can
therefore fit structures given only a covariance (or a similarity) matrix.

We integrate over edge lengths in the graph by finding the modal set of
lengths (see graph_like_conn.m) then using the Laplace approximation. We
use an exponential prior on the edgelengths (parameter lbeta).

Initializing the search for relational structures:
--------------------------------------------------
As described in the PNAS paper, we initialize the search in two ways when
analyzing relational data:
    (i) with all the entities assigned to one cluster
    (ii) using a graph built from the best instance of structure
	'partitionnoself'

Currently, however, strategy (ii) is not fully automated. To use this
strategy, first find the best instance of 'partitionnoself' for your data
set. Then use zinit_rel.m to put this result in a folder
(ps.reloutsideinit) that can be accessed when using strategy (ii).  Put
the path to this directory in  ps.relinitdir, set ps.reloutsideinit to
'external', and run the model again. 


Nauty:
------

graphsig.m requires Brendan McKay's nauty program to be installed:

http://cs.anu.edu.au/~bdm/nauty/

This function will only be called if ps.nauty = 1 (set to 0 by default).
It's fine to stick with the default setting, but using nauty might mean
that large data sets get processed a little faster. 

Disclaimers: 
------------

(i) this code was not written to be highly efficient. I've run it on data
sets with up to 100 entities but don't know how well it scales beyond
that.  Missing data make the algorithm substantially slower.

(ii) Parts of this code -- especially the search heuristics used to clean
up the graph after each split -- are messy and may be hard to follow.


Acknowledgements:
-----------------
The code includes functions written by several other people:

Kevin Murphy's BNT:
    subv2ind.m, mysetdiff.m 
Thomas Minka's lightspeed toolbox:
    vec.m inv_posdef.m inv_triu.m logdet.m
Leon Peshkin:
    draw_dot, graph_to_dot.m, dot_to_graph.m, graph_draw.m
Carl Rasmussen:
    checkgrad.m
John Burkardt:
    stirling2.m
Michael Kay
    dijkstra.m

