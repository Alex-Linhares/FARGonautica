function [graph currscore overallchange nearmscores nearmgraphs] = ...
	    collapsedims(graph, data, ps, epsilon, currscore, ...
		 overallchange, loopmax, nearmscores, nearmgraphs, varargin); 

% take graph with multiple components and squeeze out dimensions wherever
% possible (move objects to the nearest possible vacant nodes)

args	  = varargin;
debug     = 0;
for i=1:2:length(args)
  switch args{i}
   case 'debug',      debug   = args{i+1};     
  end
end

nmissflag = length(nearmscores) > 0;

change = 1; loopcount = 0;
while (change && loopcount < loopmax) 
  change = 0; loopcount = loopcount+1;
  if loopcount == loopmax 
    disp('loopcount exceeded in gibbs_clean: collapsedims');
  end
  ds = dijkstra(graph.Wclustersym);
  for i=1:graph.ncomp
    % occupied nodes for this component
    occnodescomp = get_occnodescomp(graph, i);
    for jind = 1:length(occnodescomp) 
      if jind > length(occnodescomp) continue; end
      j = occnodescomp(jind);
      testgraph = graph;
      [occ, unocc] = getocc(testgraph, i, j);
      zj = occ(find(testgraph.compinds(occ,i)==j));
      if length(zj) <= length(unocc)
        rp = randperm(length(zj));
	zj = zj(rp);
	for zjinst = zj 
	  [m mind] = min(ds(zjinst,unocc));
          testgraph = zassign(zjinst, unocc(mind), testgraph, i, j); 
	  unocc = setdiff(unocc, unocc(mind));
        end
	testgraph = combinegraphs(testgraph, ps, 'zonly', 1);
        testgraph = simplify_graph(testgraph, ps);
        [testscore, newgraph]=graph_like(data, testgraph, ps);
        testscore = testscore + graph_prior(testgraph, ps);
        if testscore -  currscore  > epsilon
          if debug keyboard; end
          change = 1; overallchange = 1;
          graph = testgraph; currscore = testscore;
          occnodescomp = get_occnodescomp(graph, i);
        elseif nmissflag % add graph to list of nearmisses
          if testscore > nearmscores(end)
    	    [nearmscores nearmgraphs]=addnearmiss(nearmscores, nearmgraphs,...
	    testgraph, testscore, graph, currscore, epsilon);
          end
        end
      end
    end
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get occupied and unoccupied nodes
function [occ unocc] = getocc(graph, compind, j)

occ = unique(graph.z); 
unocc = setdiff(1:size(graph.Wcluster,1), occ);
% empty nodes with value j on COMPIND are no good
unocc = setdiff(unocc, find(graph.compinds(:, compind) == j));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get occupied and unoccupied nodes

function occnodescomp = get_occnodescomp(graph, i);
occnodescomp = unique(graph.components{i}.z);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% move zjinst to newnode in graph
function graph = zassign(zjinst, newnode, graph, compind, j, ps)
 
objind = find(graph.z == zjinst);
for i = 1:graph.ncomp
  graph.components{i}.z(objind) = graph.compinds(newnode,i);
end

