function descendants = find_descendants(adj)

% Create cell array specifying descendants of each node in ADJ 

leaves = find(sum(adj,2) == 0)'; 
% nodes marked once they've been in queue
marked = zeros(size(adj,1), 1);
processed= zeros(size(adj,1), 1);
queue = [];
for l = leaves
  descendants{l} = [];
  marked(l) = 1;
  processed(l) = 1;
  parents = find(adj(:, l))';
  queue = [queue, parents(marked(parents)==0)];
  marked(parents(marked(parents)==0)) = 1;
end

% XXX inefficient
while ~isempty(queue)
  node = queue(1);  queue = queue(2:end);
  children = find(adj(node,:));
  if sum(processed(children)==0) >= 1
    queue = [queue, node];
    continue;
  else
    ds = children;
    for c = children
      ds = union(ds, descendants{c});
    end
    descendants{node} = ds;
    parents = find(adj(:, node))';
    queue = [queue, parents(marked(parents)==0)];
    marked(parents(marked(parents)==0)) = 1;
    processed(node) = 1;
  end
end

