function graph = filloutrelgraph(graph)

% make graph transitive

switch graph.type
  case{'connected', 'connectednoself'}
    graph.adjcluster = graph.adjcluster | graph.adjcluster';
  otherwise % make graph transitive
    nclass = size(graph.adjcluster, 1);
    descendants = find_descendants(graph.adjcluster);
    for i=1:nclass
      graph.adjcluster(i,descendants{i}) = 1;
    end
end




