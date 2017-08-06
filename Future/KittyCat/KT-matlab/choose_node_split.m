function [ll, part1, part2, newgraph]=choose_node_split(graph, compind,...
						    splitind, pind, data, ps)

% split node SPLITIND in component COMPIND using production PIND  

if compind < 0
  partmembers = find(graph.z == splitind);
else
  partmembers = find(graph.components{compind}.z == splitind);
end
disp('Splitting..'); disp(ps.runps.names(partmembers));

if length(partmembers) == 1
  ll = -inf; part1 = splitind; part2 = []; newgraph = [];
else
   seedpairs = choose_seedpairs(graph, compind, splitind, pind, ps);
  [ll, part1, part2, newgraph]=best_split(graph, compind, splitind, pind, ...
					    data, seedpairs, ps);
end

if isnan(ll)
  keyboard
end
