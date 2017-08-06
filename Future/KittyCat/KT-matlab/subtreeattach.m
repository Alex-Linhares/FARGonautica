function graph = subtreeattach(graph, j, edgep, edgec, comp, ps, varargin);

% attach subtree rooted at j to edge between edgep and edgec

origgraph = graph;
args = varargin;
objflag = 0;
for i=1:2:length(args)
  switch args{i}
   case 'objflag', objflag=args{i+1};
  end
end

adj = graph.components{comp}.adj; W   = graph.components{comp}.W; 
newadj = adj; newW = W;
if objflag == 0
  oldp = find(adj(:, j));
  if isempty(oldp) % j is a cluster node with no parent
    return
  end
end

switch graph.components{comp}.type
  case{'tree'}
    % attach subtree rooted at j to edge between edgep and edgec
    oldweight= W(edgep, edgec); newweight = 2*oldweight;
    newp = size(adj,1)+1;
    newadj(edgep, newp) = 1; newadj(newp, edgec) = 1; newadj(edgep, edgec) = 0;
    newW(edgep, newp) = newweight; 
    newW(newp, edgec) = newweight; 
    newW(edgep, edgec) = 0;
    graph.components{comp}.illegal = [graph.components{comp}.illegal, newp]; 
    if objflag
      newpc = size(adj,1)+2;
      newadj(newp, newpc) = 1; newW(newp, newpc) = 2 * graph.leaflengths(j);
      newadj(newpc, :) = 0; newW(newpc, :) = 0;
      graph.leaflengths(j) = 2*graph.leaflengths(j);
      graph.components{comp}.z(j) = newpc;
      graph.components{comp}.nodecount = graph.components{comp}.nodecount+2;
    else 
      newadj(newp, j) = 1; newW(newp, j) = W(oldp, j);
      newadj(oldp, j) = 0; newW(oldp, j) = 0; 
      graph.components{comp}.nodecount = graph.components{comp}.nodecount+1;
    end

  case{'hierarchy', 'dirhierarchy', 'domtree', 'dirhierarchynoself', ...
       'domtreenoself', 'undirhierarchy', 'undirhierarchynoself',...
       'undirdomtree', 'undirdomtreenoself'}
    if objflag % attach object j to edgep
      newp = size(adj,1)+1;
      newadj(edgep, newp) = 1;  newadj(newp,:) = 0;
      newW(edgep, newp) = 2*graph.leaflengths(j); newW(newp,:) = 0;
      graph.leaflengths(j) = 2*graph.leaflengths(j);
      graph.components{comp}.z(j) = newp;
      graph.components{comp}.nodecount = graph.components{comp}.nodecount+1;
    else % attach subtree rooted at j to edgep 
      newadj(oldp, j) = 0; newW(oldp, j) = 0;
      newadj(edgep, j) = 1; newW(edgep, j) = W(oldp, j);
    end
  otherwise
    error('unexpected structure');
end

graph.components{comp}.adj = newadj;
graph.components{comp}.W = newW;
graph.components{comp}.adjsym = newadj + newadj' - (newadj & newadj');
graph.components{comp}.Wsym = newW + newW' - newW.*(newadj & newadj');

% make sure new node doesn't map to a a neighbor of edgep or edgec. If this is
% true it can map to anything.
empties  = [find(~adj(edgep,:) & ~adj(edgec,:)), 1, 1, 1];
imap = [1:size(adj,1), empties(1:(size(newadj,1)-size(adj,1)))];

graph = combinegraphs(graph, ps, 'origgraph', origgraph, 'compind', comp, ...
		      'imap', imap);
