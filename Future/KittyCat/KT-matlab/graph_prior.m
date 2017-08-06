function gp = graph_prior(graph, ps)

% Compute prior on graph GRAPH.

% GP = log P(GRAPH). 

% if some objects are out of play, gp will be proportional to the true prior
% but the actual number will be wrong

nclusternodes = size(graph.adjcluster,1);

switch graph.type
 case {'partition', 'connected', 'partitionnoself', 'connectednoself'}
   index = 1;     
 case {'chain', 'undirchain', 'undirchainnoself'}
   index = 2; 
 case {'ring', 'undirring', 'undirringnoself'}
   index = 3;
 case {'tree'}
   index = 4;
   nclusternodes = nclusternodes - length(graph.illegal);
 case {'hierarchy', 'undirhierarchy', 'undirhierarchynoself', 'undirdomtree', 'undirdomtreenoself'}
   index = 5;
 case {'domtree', 'dirhierarchy', 'dirhierarchynoself', 'dirdomtreenoself'}
   index = 6;
 case {'order', 'dirchain', 'dirchainnoself', 'ordernoself'}
   index = 7;
 case {'dirring', 'dirringnoself'}
   index = 8;
 case {'grid'}
   index = 9;
 case {'cylinder'}
   index = 10;
 otherwise
   error('Unexpected structure');
end

gp = ps.logps{index}(nclusternodes);

