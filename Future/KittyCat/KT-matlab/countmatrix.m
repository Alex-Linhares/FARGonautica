function counts = countmatrix(data, graph)

% GRAPH includes entities that are assigned to clusters. COUNTS specifies
% the number of links between pairs of clusters.

nclass = size(graph.adjcluster,1) ;
nobj = graph.objcount;
counts=zeros(nclass);
for i=1:nclass
  clustmembers{i}=find(graph.z==i);
end

counts = size(graph.adj,1)-nobj;

% XXX vectorize this
for i=1:nclass
  for j=1:nclass
    counts(i,j) = sum(sum(data(clustmembers{i}, clustmembers{j})));
  end
end

