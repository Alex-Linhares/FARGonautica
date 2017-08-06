function [nearmscores nearmgraphs] = addnearmiss(nearmscores, nearmgraphs,...
				  graph, score, currgraph, currscore, epsilon)

% Add GRAPH to NEARMGRAPHS -- the list of "near miss graphs"

if norm(currscore - score) < epsilon
  return;
end

lsize = length(nearmscores); newind = 1:lsize;
lower = find(nearmscores < score); l = lower(1);
newind = setdiff(1:lsize,l);
nearmscores(newind) = nearmscores(1:end-1);
nearmgraphs(newind) = nearmgraphs(1:end-1);
nearmscores(l) = score; nearmgraphs{l} = graph;

