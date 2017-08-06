function graph = relgraphinit(data, z, ps)

% Create initial graph for relational data set DATA by using various
% heuristics


switch ps.runps.structname
    case{'undirchain', 'undirring', 'undirhierarchy', ...
         'undirchainnoself', 'undirringnoself', 'undirhierarchynoself'} 
      % so that our greedy algorithm considers counts in both directions along
      % each edge
      data = data + data';
end


lc = makelcfreq(data, z);
nclust = length(unique(z));
counts = hist(z, unique(z));
totobs = repmat(counts, nclust, 1).*repmat(counts', 1, nclust);
% counts on diagonal shouldn't influence the structure we choose 
lc(sub2ind(size(lc), 1:nclust, 1:nclust)) = 0;
lcprop = lc./totobs;

for i = unique(z)
  zs{i} = find(z == i);
end

graph = makeemptygraph(ps);
graph.z = z;
graph.adjcluster = zeros(nclust); graph.adjclustersym = zeros(nclust);
graph.adj=expand_graph(zeros(nclust), zs, ps.runps.type);
graph.Wcluster = zeros(nclust);
graph.W = graph.adj;

[head, tail, used] = chooseinithead(lc, lcprop, graph);
for i = 2:nclust
  [graph, head, tail, used] = growgraph(graph, head, tail, used, lc, lcprop);
end
graph = finishgraph(graph, head, tail);

graph.components{1}.adj = graph.adjcluster;
graph.components{1}.adjsym = graph.adjcluster | graph.adjcluster';
graph.components{1}.W   = graph.adjcluster;
graph.components{1}.Wsym   = graph.adjclustersym;
graph.components{1}.z= z;
graph.components{1}.nodecount = nclust;
graph.components{1}.nodemap= 1:nclust;
graph.components{1}.edgemap= get_edgemap(graph.components{1}.adj);
graph = combinegraphs(graph, ps);

%------------------------------------------------------------------------------ 
function [head, tail, used] = chooseinithead(lc, lcprop, graph)

if 0
head = [];
[m,mind] = max(sum(lcprop, 2));
tail = mind;
head = tail;
used(tail) = 1;
end

used = zeros(size(lc,1),1);
switch graph.type
  case{'order', 'ordernoself', 'domtree', 'dirdomtreenoself',...
       'undirdomtree', 'undirdomtreenoself'}
    head = [];
    [m,mind] = max(sum(lcprop, 2));
    tail = mind;
    used(tail) = 1;
  otherwise
    [head, tail] = find(lcprop == max(lcprop(:)));
    head = head(1);
    tail = head;
    used(head) = 1;
end
    
%------------------------------------------------------------------------------ 
function [graph, head, tail, used] = ...
			    growgraph(graph, head, tail, used, lc, lcprop)

unused = find(used == 0);
switch graph.type
  case{'partition', 'partitionnoself'}
  case{'dirchain', 'dirchainnoself', 'dirring', 'dirringnoself',...
       'dirhierarchy', 'dirhierarchynoself',...
       'undirchain', 'undirchainnoself', 'undirring', 'undirringnoself',...
       'undirhierarchy', 'undirhierarchynoself'}
      headlinks = lcprop(unused, head); hlmax = max(headlinks(:));
      taillinks = lcprop(tail, unused); tlmax = max(taillinks(:));
      if hlmax > tlmax
        [h1, t1] = find(headlinks == hlmax);
	h1 = h1(1); t1 = t1(1);
        h = unused(h1); t = head(t1);
        graph.adjcluster(h, t) = 1;
        newhead = h; newtail = [];
      else
       [h1, t1] = find(taillinks== tlmax);
	h1 = h1(1); t1 = t1(1);
        h = tail(h1); t = unused(t1);
        graph.adjcluster(h, t) = 1;
        newhead = []; newtail = t;
      end
      used([newhead, newtail]) = 1;
      switch graph.type
        case{'dirhierarchy', 'dirhierarchynoself', 'undirhierarchy', ...
	     'undirhierarchynoself'}
	  if ~isempty(newhead) % only one head allowed to avoid mult connected
	    head = newhead;
	    tail = [tail, head];
          else 
	    tail = [tail, newtail];
          end
	otherwise
	  if isempty(newhead)
	     tail = newtail;
	  else
	     head = newhead;
	  end
      end
  case{'order', 'ordernoself'}
    [m,mind] = max(sum(lcprop(unused, unused), 2));
    t = unused(mind);
    graph.adjcluster(tail, t) = 1;
    used(t) = 1;
    tail = t;
  case{'domtree', 'dirdomtreenoself', 'undirdomtree', 'undirdomtreenoself'}
    % greedy search should work OK for domtree
    error('init not implemented for domtree');
  otherwise
    error('unexpected structure type');
end

%------------------------------------------------------------------------------ 
function graph = finishgraph(graph, head, tail);
switch graph.type
  case{'dirring', 'dirringnoself', 'undirring', 'undirringnoself'}
    if tail ~= head
      graph.adjcluster(tail,head)=1;
    end
  otherwise
end

