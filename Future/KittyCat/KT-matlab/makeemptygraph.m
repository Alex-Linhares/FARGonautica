function graph  = makeemptygraph(ps)

% Make a graph with one cluster and no objects.

graph.type = ps.runps.structname;
graph.objcount= ps.runps.nobjects;
graph.sigma = ps.sigmainit;
graph.adjcluster = [0]; graph.adjclustersym = [0];
graph.adj=expand_graph([0], {1:ps.runps.nobjects}, ps.runps.type);
graph.Wcluster = [0];
graph.W = graph.adj;
graph.z = ones(1, graph.objcount);
graph.leaflengths = ones(1, graph.objcount);
graph.extlen = 1; graph.intlen = 1;
switch ps.runps.structname 
  case{'partition', 'chain', 'ring', 'tree', 'hierarchy', 'order',...
       'dirchain', 'dirring', 'dirhierarchy', 'domtree',...
       'connected', 'partitionnoself', 'dirringnoself', 'dirchainnoself',...
       'ordernoself', 'dirhierarchynoself', 'dirdomtreenoself',...
       'undirchain', 'undirchainnoself', 'undirring', 'undirringnoself',...
       'undirhierarchy', 'undirhierarchynoself', 'undirdomtree',...
       'undirdomtreenoself', 'connectednoself'}
      graph.ncomp = 1;
      graph.components{1}.type = ps.runps.structname;
      switch ps.runps.structname
        case{'partition', 'chain', 'ring', 'order', ...
             'dirchain','dirring', 'connected', 'partitionnoself',...
             'dirchainnoself', 'dirringnoself', 'ordernoself', ...
	     'undirchain', 'undirchainnoself', 'undirring',...
	     'undirringnoself','connectednoself'} 
          graph.components{1}.prodcount = 1;
        case{'tree'} 
          graph.components{1}.prodcount = 2;
        case{'hierarchy','dirhierarchy', 'domtree', 'dirhierarchynoself', ...
             'dirdomtreenoself', 'undirhierarchy', 'undirhierarchynoself',...
	     'undirdomtree', 'undirdomtreenoself'} 
          graph.components{1}.prodcount = 3;
      end
  case{'grid'}
      graph.ncomp = 2;
      graph.components{1}.type = 'chain';
      graph.components{1}.prodcount=1;

      graph.components{2}.type = 'chain';
      graph.components{2}.prodcount=1;

  case{'cylinder'}
      graph.ncomp = 2;
      graph.components{1}.type = 'ring';
      graph.components{1}.prodcount=1;

      graph.components{2}.type = 'chain';
      graph.components{2}.prodcount=1;
end

for j=1:graph.ncomp
  graph.components{j}.adj = [0]; graph.components{j}.W = [0];
  graph.components{j}.adjsym = [0]; graph.components{j}.Wsym = [0];
  graph.components{j}.nodecount=   1; graph.components{j}.nodemap  =   1;
  graph.components{j}.edgecount=   0; graph.components{j}.edgemap  =  [0];
  graph.components{j}.edgecountsym= 0; graph.components{j}.edgemapsym  =  [0];
  graph.components{j}.z	     =   ones(1, graph.objcount);
  graph.components{j}.illegal=[];
end
graph = combinegraphs(graph, ps);

