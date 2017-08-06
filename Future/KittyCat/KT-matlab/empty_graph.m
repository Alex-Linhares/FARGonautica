function graph = empty_graph(graph, compind, c1, c2);

% Remove all members of cluster C1 or cluster C2 from component COMPIND of
% GRAPH

if compind < 0
 removeind = find(graph.z == c1 | graph.z == c2);
else
  removeind = find(graph.components{compind}.z == c1 | ...
		 graph.components{compind}.z == c2  );
end

graph.z(removeind) = -1;

includeind = setdiff(1:size(graph.adj,1), removeind);
graph.adj = graph.adj(includeind, includeind);
graph.W = graph.W(includeind, includeind);
graph.objcount = graph.objcount - length(removeind);
