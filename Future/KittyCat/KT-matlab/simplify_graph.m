function graph = simplify_graph(graph, ps)

% Try cleaning up GRAPH by removing unnecessary cluster nodes.

% remove  cluster nodes we don't want
%	1) dangling cluster nodes (any node that's not an object node, but has
%				    exactly one (or zero) cluster neighbors and
%				    no object neighbors)
%	2) any cluster node with exactly two neighbors, one of which is a
%	    cluster node

nobj = graph.objcount;
overallchange = 0;
origgraph = graph;

for i=1:graph.ncomp
  % dangling cluster nodes
  adj = graph.components{i}.adj; W = graph.components{i}.W; 
  imap = 1:size(adj,1);
  z = graph.components{i}.z; 
  illegal = graph.components{i}.illegal;
  cont = ones(1,3);
  while (sum(cont)) 
    for caseind = 1:3
      ntot = size(adj,1);
      occ = zeros(1,ntot);
      occ(z) = 1;
      [adj W z includeind] = redundantinds(caseind, graph, i, adj, W, z, ...
					   occ, ps);
      if length(includeind) == ntot
        cont(caseind) = 0; 
      else
        cont(caseind) = 1;
        overallchange = 1;
	map = zeros(1, ntot); map(includeind) = 1:length(includeind);
        z = map(z);
        illegal = map(illegal); illegal = illegal(illegal>0);
        imap = imap(includeind);
      end
    end
  end

  if overallchange
    Wtr = W';
    graph.components{i}.adj = adj;  graph.components{i}.W = W; 
    graph.components{i}.adjsym = adj|adj';
    graph.components{i}.Wsym = W; 
    graph.components{i}.Wsym(logical(adj')) = Wtr(logical(adj'));
    graph.components{i}.z = z; 
    graph.components{i}.edgecount = sum(sum(adj));
    graph.components{i}.edgecountsym = sum(sum(graph.components{i}.adjsym))/2;
    graph.components{i}.illegal = illegal;
    graph.components{i}.nodecount = size(adj,1); 
    graph = combinegraphs(graph, ps, 'origgraph', origgraph, 'compind', i,...
			  'imap', imap);
    overallchange = 0;
  end
end

if graph.objcount < 30
  return
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [adj W z includeind]= redundantinds(caseind, graph, i, adj, W, z,...
					     occ, ps)

% XXX: should really apply these only if we haven't tied branch lengths. But
% since we only tie branches as a search heuristic right now, it doesn't matter
% too much.

cflag = 0;
switch caseind
  case{1}
    % dangling cluster nodes (any unoccupied node with zero or exactly one
    %	cluster neighbor 
    removeind = find(sum(adj)+sum(adj')<=1 & occ == 0);
  case{2}
    % any unoccupied node with exactly two neighbors 
    % (but don't remove root of tree
    switch graph.components{i}.type
      case{'tree'} %XXX: remove root for sure
        if ps.cleanstrong
          removeind= find(sum(adj)+sum(adj')==2 & occ ==0 ) ;
        else % don't remove root
          removeind= find(sum(adj)+sum(adj')==2 & sum(adj) == 1 & occ ==0 ) ;
        end
      otherwise
        removeind= find(sum(adj)+sum(adj')==2 & occ ==0 ) ;
    end
    if isempty(removeind) includeind = 1:size(adj,1); return;  end
    removeind = removeind(1);
    nbs = [find(adj(:, removeind))', find(adj(removeind,:))];
    if adj(nbs(2),removeind) % nbs(2) is a parent
      nbs = fliplr(nbs);
    end
    if nbs(1) ~= nbs(2) % special case when we simplify a 2 cluster ring
      adj(nbs(1),nbs(2)) = 1;
      oldWind = sub2ind(size(W), [nbs(1:2), removeind, removeind], ...
				 [removeind, removeind, nbs(1:2)]);
      oldWs = W(oldWind); 
      oldWs = oldWs(oldWs>0);
      W(nbs(1), nbs(2)) = 1/sum(1./oldWs); 
    end
  case{3} 
    if strcmp(graph.components{i}.type, 'tree') %&& ~ps.cleanstrong
      if (ps.fixedall || ps.fixedinternal || ps.fixedexternal)
        removeind = [];
      else
	% join pairs that have been split
        zcnts = hist(z, 1:size(adj,1));
	singletons = find(zcnts == 1);
	twosingleneighbors = ... 
	  find(sum(adj(singletons,:),1) + sum(adj(:, singletons),2)' == 2);
	if ~isempty(twosingleneighbors)
	  parent = twosingleneighbors(1);
	  children = intersect(singletons, ...
			find((adj(parent,:) | adj(:, parent)')));
	  z(z==children(2)) = children(1);
	  removeind = [children(2)];
	else
          removeind = [];
	end
      end
     else
       if ~strcmp(ps.runps.type, 'rel') && ... % not for relational data
	  graph.ncomp == 1 % don't want to do this for single dimensions of
			   % grids
         zcnts = hist(z, 1:size(adj,1));
         % any occupied node with one cluster parent and no cluster children
         removeindpar = find(sum(adj)==1 & sum(adj')==0 & zcnts == 1);
         % any occupied node with one cluster child and no cluster parents
         removeindch = find(sum(adj')==1 & sum(adj)==0 & zcnts == 1);
         if isempty([removeindpar, removeindch]) 
 	    includeind = 1:size(adj,1); return;  
 	end
 	if ~isempty(removeindpar)
           removeind = removeindpar(1);
           newz = find(adj(:, removeind));
 	else 
           removeind = removeindch(1);
           newz = find(adj(removeind,:));
         end
         obj = find(z == removeind);
         z(obj) = newz;
       else
         removeind = [];
       end
    end
end

includeind = mysetdiff(1:size(adj,1), removeind);
adj = adj(includeind, includeind);
W = W(includeind, includeind);

