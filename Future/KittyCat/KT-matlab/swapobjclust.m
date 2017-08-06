function [graph currscore overallchange nearmscores nearmgraphs] = ...
	    swapobjclust(graph, data, ps, comp, epsilon, currscore, ...
		 overallchange, loopmax, nearmscores, nearmgraphs, varargin); 

% Try improving GRAPH by swapping clusters or individual objects

args	  = varargin;
objflag   = 0;
debug     = 0;
whole     = 0;
fastflag  = 0; % only try swaps locally in graph
for i=1:2:length(args)
  switch args{i}
   case 'objflag',    objflag = args{i+1};     % move objects around
   case 'debug',      debug   = args{i+1};     
   case 'fastflag',  fastflag = args{i+1};     
  end
end
if isempty(comp)
  whole = 1;	% swaps at level of entire graph
end
graphngb = 3;   % neighborhood within which to try swaps (fast mode)

nmissflag = length(nearmscores) > 0;

change = 1; loopcount = 0;
while (change && loopcount < loopmax) 
  change = 0; loopcount = loopcount+1;
  if loopcount == loopmax 
    disp('loopcount exceeded in gibbs_clean: swapobjclust');
  end
  [sw1 sw2]= chooseswaps(graph, whole, objflag, comp, fastflag, graphngb);
  rp = randperm(size(sw1,1));
  for jind=1:length(rp)
    j = rp(jind);
    if j > size(sw1,1) continue; end
    testgraph = doswap(graph, sw1(j,:), sw2(j,:), objflag, ps); 
    testgraph = simplify_graph(testgraph, ps);
    [testscore, newgraph]=graph_like(data, testgraph, ps);
    testscore = testscore + graph_prior(testgraph, ps);
    %if debug clf; draw_dot(testgraph.adj); keyboard; end
    %XXX
    %if sw1(j,1) == 1 && sw1(j,2) == 21 && isnan(sw2(j,1)) keyboard; end
    %XXX
    if testscore -  currscore  > epsilon
      if debug keyboard; end
      change = 1; overallchange = 1;
      graph = testgraph; currscore = testscore;
      [sw1 sw2]= chooseswaps(graph, whole, objflag, comp, fastflag, graphngb);
    elseif nmissflag % add graph to list of nearmisses
      if testscore > nearmscores(end)
	[nearmscores nearmgraphs]=addnearmiss(nearmscores, nearmgraphs,...
	    testgraph, testscore, graph, currscore, epsilon);
      end
    end
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sw1 and sw2 indicate  swaps/moves to make
%	format: [c, j, z1, z2, z3, ..., zn]
%	        Either c: graph component, j: node
%	            or c: nan, j: node (at top level)
%	            or c: nan, j:object
%	sw2   : may contain nans (for rows that are moves)   
%

function [sw1 sw2]= chooseswaps(graph, whole, oflag, comp, fastflag, graphngb)

if oflag
  nnode = size(graph.adjcluster, 1);
  % cluster nodes that are free to accept objects
  clegal = setdiff(1:nnode, graph.illegal); 
  clegalv = zeros(1,nnode); clegalv(clegal) = 1;
  % objects that are free to move
  objmovable = sourceobjs(graph);
  objmovablev= zeros(1,graph.objcount); objmovablev(objmovable) = 1;
  if fastflag
    dijk = dijkstra(graph.adjclustersym);
    col1 = []; col2 = [];
    for c=unique(graph.z)
      ds = find(dijk(c,:) <= graphngb & dijk(c,:) > 0 & clegalv);
      cmembers = find(graph.z == c & objmovablev);
      memmat = repmat(cmembers, length(ds), 1);
      col1 = [col1; memmat(:)]; 
      col2 = [col2; repmat(ds', length(cmembers),1)]; 
    end
    sw1 = [nan*ones(length(col1), 1), col1, graph.compinds(col2,:)];
    sw2 = nan*ones(size(sw1));
  else
    col1 = repmat(objmovable, length(clegal), 1);
    sw1 = [nan*ones(prod(size(col1)),1), col1(:), ...
	 repmat(graph.compinds(clegal,:), length(objmovable), 1)];
    sw2 = nan*ones(size(sw1));
  end
elseif whole    
  nnode = size(graph.adjcluster,1);
  % cluster nodes that are free to accept objects
  clegal = setdiff(1:nnode, graph.illegal); 
  clegalv = zeros(1,nnode); clegalv(clegal) = 1;
  % cluster nodes whose objects we can steal 
  csource = sourcecls(graph);
  csourcev= zeros(1,nnode); csourcev(csource) = 1;
  if fastflag
    dijk = dijkstra(graph.adjclustersym);
    col1 = []; col2 = []; col1a= []; col2a = [];
    for c = csource
      ds = find(dijk(c,:) <= graphngb & dijk(c,:) > 0 & clegalv);
      col1 = [col1; c*ones(length(ds),1)]; col2 = [col2; ds']; 
      swopts = find(dijk(c,:) <= graphngb & dijk(c,:) > 0 & csourcev);
      % don't want to try swaps twice
      swopts = swopts(swopts > c);
      col1a = [col1a; c*ones(length(swopts),1)]; col2a = [col2a; swopts'];
    end
    sw1 = [nan*ones(length(col1), 1), col1, graph.compinds(col2,:)];
    sw2 = nan*ones(size(sw1));
    sw1 = [sw1;nan*ones(size(col1a)), col1a, graph.compinds(col2a,:)];
    sw2 = [sw2;nan*ones(size(col1a)), col2a,  graph.compinds(col1a,:)];
  else
    % moves
    col1 = repmat(csource, length(clegal),1);
    sw1 = [nan*ones(prod(size(col1)), 1), col1(:), ...
  	 repmat(graph.compinds(clegal,:), length(csource), 1)];
    sw2 = nan*ones(size(sw1));
    % swaps (only bother with swaps of occupied nodes)
    pairs = nchoosek(csource, 2);
    col1 = pairs(:,1); col2 = pairs(:,2);
    sw1 = [sw1;nan*ones(size(col1)), col1,  graph.compinds(col2,:)];
    sw2 = [sw2;nan*ones(size(col1)), col2,  graph.compinds(col1,:)];
  end
else % within component moves/swaps
  % cluster nodes that are free to accept objects
  clegal = setdiff(1:graph.components{comp}.nodecount,...
		 graph.components{comp}.illegal);
  clegalv = zeros(1,graph.components{comp}.nodecount); clegalv(clegal) = 1;
  % cluster nodes whose objects we can steal 
  csourcemove = sourcecls(graph, comp);
  csourcemovev= zeros(1,graph.components{comp}.nodecount);
  csourcemovev(csourcemove) = 1;
  csourceswap = unique(graph.components{comp}.z);
  csourceswapv= zeros(1,graph.components{comp}.nodecount);
  csourceswapv(csourceswap) = 1;
  if fastflag
    dijk = dijkstra(graph.components{comp}.adjsym);
    col1 = []; col2 = []; 
    for c = csourcemove
      ds = find(dijk(c,:) <= graphngb & dijk(c,:) > 0 & clegalv);
      col1 = [col1; c*ones(length(ds),1)]; col2 = [col2; ds']; 
    end
    sw1 =[comp*ones(length(col1),1),col1,nan*ones(length(col1), graph.ncomp)];
    if isempty(sw1) 
      sw1 = zeros(0, graph.ncomp+2);
    else
      sw1(:, 2+comp) = col2; 
    end
    sw2 = nan*ones(size(sw1));

    col1 = []; col2 = [];
    for c = csourceswap
      ds = find(dijk(c,:) <= graphngb & dijk(c,:) > 0 & csourceswapv);
      % don't want to try swaps twice
      ds = ds(ds > c);
      col1 = [col1; c*ones(length(ds),1)]; col2 = [col2; ds']; 
    end
    sw1b =[comp*ones(length(col1),1),col1,nan*ones(length(col1), graph.ncomp)];
    sw2b =[comp*ones(length(col1),1),col2,nan*ones(length(col1), graph.ncomp)];
    if isempty(sw1b) 
      sw1b = zeros(0, graph.ncomp+2); sw2b = sw1b;
    else
      sw1b(:, 2+comp) = col2; 
      sw2b(:, 2+comp) = col1; 
    end
    sw1 = [sw1; sw1b];
    sw2 = [sw2; sw2b];
  else 
    % moves
    col1 = repmat(csourcemove, 1, length(clegal));
    sw1 = [comp*ones(size(col1')), col1', ...
           nan*ones(length(col1), graph.ncomp)];
    col2 = repmat(clegal, length(csourcemove), 1);
    sw1(:, 2+comp) = col2(:); 
    sw2 = nan*ones(size(sw1));
  
    % swaps (only bother with swaps of occupied nodes)
    if length(csourceswap) > 1
      pairs = nchoosek(csourceswap, 2);
    else
      pairs = [1 1];
    end
    col1 = pairs(:,1); col2 = pairs(:,2);
    sw1b = [comp*ones(size(col1)), col1, nan*ones(length(col1), graph.ncomp)];
    sw1b(:, 2+comp) = col2;
    sw2b = [comp*ones(size(col1)), col2, nan*ones(length(col1), graph.ncomp)];
    sw2b(:, 2+comp) = col1;
    sw1 = [sw1; sw1b];
    sw2 = [sw2; sw2b];
  end
end 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sw1 and sw2 indicate  swaps/moves to make
%	format: [c, j, z1, z2, z3, ..., zn]
%	sw2   : may contain infs (for rows that are moves)   
       
function graph = doswap(graph, sw1, sw2, oflag, ps); 

if oflag		    % object move
  obj = sw1(2);
  for i=1:graph.ncomp
    graph.components{i}.z(obj) = sw1(2+i);
  end
elseif ~isnan(sw1(1))	    % within component move/swap
  c = sw1(1); cl = sw1(2);
  oldz = graph.components{c}.z; newz = oldz;
  newz(oldz == cl) = sw1(2+c); 
  c2 = sw2(1); cl2 = sw2(2);
  if ~isnan(c2)
    newz(oldz == cl2) = sw2(2+c); 
  end
  graph.components{c}.z = newz;
else			    % move/swap at highest level
  cl = sw1(2); oldz = graph.z; 
  clmembers = find(oldz == cl);
  for i=1:graph.ncomp
    graph.components{i}.z(clmembers) = sw1(2+i);
  end
  cl2 = sw2(2);
  if ~isnan(cl2)
    clmembers = find(oldz == cl2);
    for i=1:graph.ncomp
      graph.components{i}.z(clmembers) = sw2(2+i);
    end
  end
end
graph = combinegraphs(graph, ps, 'zonly', 1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find objects that can be moved
function js = sourceobjs(graph)
switch graph.type
  case{'hierarchy', 'dirhierarchynoself', 'dirchain', 'dirring',...
       'dirhierarchy', 'dirchainnoself', 'dirringnoself', 'undirintree',...
       'undirhierarchynoself', 'undirchain', 'undirchainnoself', ...
       'undirring', 'undirringnoself'}
    [extcls intcls]= cltypes(graph,1);
    inds = ones(1, graph.objcount);
    for i = intcls
      if sum(graph.z == i) == 1
        inds(graph.z == i) = 0;
      end
    end
    js = find(inds);
  otherwise
    js = 1:graph.objcount;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find clusters that can be moved
function csource = sourcecls(graph, i)

if nargin < 2
  csource = unique(graph.z);
else
  csource = unique(graph.components{i}.z);
end

switch graph.type
  case{'hierarchy', 'dirtree', 'dirhierarchynoself', 'undirhierarchy',...
       'undirhierarchynoself'}
    [extcls intcls]= cltypes(graph,1);
    csource = intersect(csource, extcls);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find external clusters and internal clusters in component i
function [extcls intcls]= cltypes(graph, i)
% find clusters that can be moved.
adj = graph.components{i}.adjsym;
extcls = find(sum(adj) <= 1);
intcls = setdiff(1:graph.components{i}.nodecount, extcls);


