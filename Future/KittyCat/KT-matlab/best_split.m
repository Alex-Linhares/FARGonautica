function [ll, part1, part2, newgraph]=...
			best_split(graph, compind, c, pind, data, seedpairs, ps)

% Choose the best split of cluster node C.

% SEEDPAIRS: objects to seed the new children nodes
% COMPIND:   graph component (-1 for high level split)
% PIND :     which grammar to use

if ps.speed > 1 ps.fast = 1; end

if compind < 0
  partmembers = find(graph.z == c);
  c1 = c; c2 = pind;
  e_graph = graph;
else
  cgraph	= graph.components{compind};
  partmembers	= find(cgraph.z==c); 
  [e_graph c1 c2] = split_node(graph, compind, c, pind, ...
			     partmembers(1), partmembers(2:end), ps);
end

if c1 == -inf % if we can't apply the current production
  ll = -inf; part1 = []; part2 = []; newgraph = []; return;
end
e_graph = empty_graph(e_graph, compind, c1, c2);

for i=1:size(seedpairs, 1)
  disp(i)
  g = e_graph;
  g = add_element(g, compind, c1, seedpairs(i,1), ps);
  g = add_element(g, compind, c2, seedpairs(i,2), ps);

  membout = setdiff(partmembers, seedpairs(i,:));
  rp = randperm(length(membout));
  membout = membout(rp);

  d = data; 
  if strcmp(ps.runps.type, 'rel')
  else
    d(membout, :)= inf;
  end

  [l g] = graph_like(d, g, ps);
  l = l + graph_prior(g, ps);

  % go through the remaining cluster members, greedily choosing which child
  % node to put them in

  while ~isempty(membout) 
    newobj = membout(1); membout = membout(2:end);
    g1 = g; 
    g1 = add_element(g, compind, c1, newobj, ps);
    g2 = add_element(g, compind, c2, newobj, ps);
   
    d = data; 
    if strcmp(ps.runps.type, 'rel')
      d.ys(:, membout)=inf; d.ys(membout, :)=inf;
      d.ns(:, membout)=inf; d.ns(membout, :)=inf;
    else
      d(membout, :)= inf;
    end

    [g1l g1new] = graph_like(d, g1, ps);
    g1l = g1l + graph_prior(g1, ps);

    [g2l g2new] = graph_like(d, g2, ps);
    g2l = g2l + graph_prior(g2, ps);

    [l choice] = max([g1l, g2l]);
    if (choice == 1) 
      g = g1new; 
    else
      g = g2new; 
    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% display the best split
if 0 
  clf
  ns = ps.runps.names;
  ns = ns(g.z>0);
  % replace newgraph with g
  for i=length(ns)+1:size(g.adj,1)
    ns{i}='';
  end
  draw_dot(g.adj, ns);
  keyboard
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  end

%  pstmp = ps.cleanstrong; ps.cleanstrong = 1;
%  gsimp = simplify_graph(g, ps);
%  ps.cleanstrong = pstmp;
%  % chain splits: sometimes splitting the cluster at the end of a chain makes
%  % no real difference.
%  if length(unique(gsimp.z(partmembers))) == 1 l = -inf; end
  gs{i}=g;
  ls(i)=l;
end


[s sind] = sort(ls, 2, 'descend');
pstmp = ps.cleanstrong; ps.cleanstrong = 1;
% chain splits: sometimes splitting the cluster at the end of a chain makes
%		 no real difference. Try not to choose splits like these
for ind = 1:length(ls)
  gsimp = simplify_graph(gs{sind(ind)}, ps);
  if length(unique(gsimp.z(partmembers))) == 1 
    ls(sind(ind)) = -inf; 
  else     
    break;
  end
end
ps.cleanstrong = pstmp;

switch ps.speed
  case{'1,2'} % slower version : optimize lengths for each potential split
    ps.fast = 0;
    for i=1:length(ls)
      [gl gnew]=graph_like(d, gs{i}, ps);
      gl = gl + graph_prior(gnew, ps);
      ls(i)=gl;
      gs{i}=gnew;
    end
    [ll mind]=max(ls);
    newgraph = gs{mind};
  case{3} % optimize once per split  
    ps.fast = 0;
    [ll mind]=max(ls);
    [gl gnew]=graph_like(d, gs{mind}, ps);
    ll = gl + graph_prior(gnew, ps);
    newgraph = gnew;
  case{4,5}  % optimize about once per depth
  [ll mind]=max(ls);
  newgraph = gs{mind};
end

if ls(mind) == -inf ll = -inf; end
if compind < 0
  part1 = find(graph.z == c1);
  part2 = find(graph.z == c2);
else
  part1 = find(newgraph.components{compind}.z == c1);
  part2 = find(newgraph.components{compind}.z == c2);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% display the best split
if ps.showbestsplit 
  figure(3); clf
  ns = ps.runps.names;
  % replace newgraph with g
  for i=length(ns)+1:size(newgraph.adj,1)
    ns{i}='';
  end
  draw_dot(newgraph.adj, ns);
  title(num2str(ll));
  drawnow
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


