function [logI graph] = graph_like(data, graph, ps)

% graph_like(data, adj, beta, sigma): compute log p(DATA|GRAPH)

origgraph = graph;

currobj = find(graph.z > 0);
if strcmp(ps.runps.type, 'sim')
  data = data(currobj, currobj);
elseif strcmp(ps.runps.type, 'feat')
  data = data(currobj,:);
elseif strcmp(ps.runps.type, 'rel')
  %data.R = data.R(currobj,currobj,:);
end

if strcmp(ps.runps.type, 'rel')
  [logI newgraph] = graph_like_rel(data, graph, ps);
else
   [logI newgraph] = graph_like_conn(data, graph, ps);
end

graph = newgraph;

