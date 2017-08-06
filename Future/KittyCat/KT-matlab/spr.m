function [graph currscore overallchange nearmscores nearmgraphs] = ...
	   spr(graph, data, ps, i, epsilon, currscore, overallchange, debug,...
	    nearmscores, nearmgraphs)

% "Sub-tree pruning and regrafting": try changing a tree structured graph
% by snipping off pieces and attaching them elsewhere 

% I: component of graph to tweak

nmissflag = length(nearmscores) > 0;

if strcmp(graph.components{i}.type, 'tree') 
  oflags = 0:1;
else 
  oflags = 0; % if tree is not latent, snipping off an object will be the same
	      % as moving it from cluster to cluster -- and we've done that
end

for objflag = oflags	    % snip off an object or a cluster node?
  rp = makerp(graph, i, objflag);
  for jind=1:length(rp)
    if jind > length(rp) continue; end
    j = rp(jind);
    [rs cs] = makers(graph, j, i, objflag);
    if ~isempty(rs)
      for eind = 1:length(rs)
        if eind > length(rs) continue; end
        testgraph = subtreeattach(graph, j, rs(eind),cs(eind), i, ps, ...
				  'objflag', objflag); 
        testgraph = simplify_graph(testgraph, ps);
        [testscore, newgraph]=graph_like(data, testgraph, ps);
        testscore = testscore + graph_prior(testgraph, ps);
        if testscore -  currscore  > epsilon
          if debug keyboard; end
          overallchange = 1;
          graph = testgraph;
          currscore = testscore;
	  rp = makerp(graph, i, objflag);
	  if jind > length(rp) break; end 
	  j = rp(jind);
  	  [rs cs] = makers(graph, j, i, objflag);
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function rp = makerp(graph, i, objflag)

if objflag == 0
  rp = randperm(graph.components{i}.nodecount);
else
  rp = randperm(graph.objcount);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [rs cs] = makers(graph, j, i, objflag)

% J is node; I is graph component
% 'tree'    : Rs and Cs indicate edges we'll attach j to 
% 'hierarchy' : Rs (== Cs) is list of nodes we'll attach j to

if objflag
  switch graph.components{i}.type
    case{'tree'} 
      edges = graph.components{i}.adj; 
      [rs cs] = find(edges);
    case{'hierarchy', 'dirhierarchy', 'domtree', 'dirhierarchynoself',...
          'undirhierarchy', 'undirhierarchynoself'}
      rs = 1:graph.components{i}.nodecount;
      rs = setdiff(rs,graph.components{i}.z(j));   
      cs = rs;
  end
else   % j is cluster node
  jparent = find(graph.components{i}.adj(:,j));
  if isempty(jparent)
    rs = []; cs = []; return;
  end
  descendants = find_descendants(graph.components{i}.adj);
  jds = [j, descendants{j}];
  switch graph.components{i}.type
      case{'tree'} 
        edges = graph.components{i}.adj; edges(jds, jds)=0;
        [rs cs] = find(edges);
        % if j's parent is one of the edge nodes the swap changes nothing 
        includeind = find(sum([rs,cs] == jparent, 2) == 0);
        rs = rs(includeind); cs = cs(includeind);
      case{'hierarchy', 'dirhierarchy', 'domtree', 'dirhierarchynoself',...
	   'undirhierarchy', 'undirhierarchynoself'}
        rs = 1:graph.components{i}.nodecount;
        rs = setdiff(rs, [jds, jparent]); cs = rs;
  end
end
