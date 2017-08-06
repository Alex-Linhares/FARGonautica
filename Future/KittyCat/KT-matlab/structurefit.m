function [ll, graph, bestgraphlls, bestgraph] = ...
		    structurefit(data, ps, graph, savefile) 

% Fit a given structure to matrix DATA using parameters PS 

%  Graph representation:
%   graph.adj: adjacency matrix
%   graph.objcount: number of object nodes (as distinct from cluster nodes)
%   graph.W: edge weights (w = 1/distance)

loopeps = 1e-2;
bestgraphlls = []; bestgraph = {};

if isempty(graph) 
  % set up initial graph
  graph = makeemptygraph(ps);
end

[currprob graph]= optimizebranches(graph, data, ps);
if ps.speed == 5
  [currprob graph]= graphscorenoopt(graph, data, ps); 
end

stopflag=0;
depth=1;

% continue splitting cluster nodes while score improves
while (stopflag==0)
  m = -inf; 
  for i=1:graph.ncomp
    clegal = unique(graph.components{i}.z);
    for c = 1:graph.components{i}.nodecount
      % if c is cluster node
      if ismember(c, clegal) 
        for pind = 1:graph.components{i}.prodcount
	  % split node c in component i using production pind
          [lls{depth,i,c, pind}, part{depth,i,c,pind, 1}, ...
	   part{depth,c,pind,2}, newgraph{depth,i,c, pind}] = ...
	    choose_node_split(graph, i, c, pind, data, ps);   
	end
      end
    end
  end

  % for combinations: try moving objects to vacant neighbors
  if graph.ncomp > 1
    i = graph.ncomp+1; lls{depth,i,1,1}= -inf; newgraph{depth,i,1,1} = [];
    nclusternodes = size(graph.adjcluster,1);
    nodecounts = hist(graph.z, 1:nclusternodes);
    c = 0; 
    for nd = 1:size(graph.adjcluster,1);
      if nodecounts(nd) > 1
        nbs = find(graph.adjclustersym(:,nd));
	for nbind = 1:length(nbs)
	  nb = nbs(nbind);
	  if nodecounts(nb) == 0
	    c = c+1;
	    [lls{depth,i,c, 1}, part{depth,i,c,1, 1}, ...
	      part{depth,c,1,2}, newgraph{depth,i,c, 1}] = ...
	        choose_node_split(graph, -1, nd, nb,  data, ps);   
	  end
	end
      end
    end
  end

  [m mi mc mpind] = bestsplit(graph, depth, lls);

  if m == -inf % no splits possible
   lls{depth, mi, mc, mpind} = currprob; 
   newgraph{depth, mi, mc, mpind} = graph;
  end

  if ps.speed == 4
    % optimize branches for best split
    % ps.speed == 3: branches already optimized.
    % ps.speed == 5: don't worry about optimizations except as heuristic
    [lls{depth, mi, mc, mpind}, newgraph{depth, mi, mc, mpind}] = ...
	  optimizebranches(newgraph{depth,mi, mc, mpind}, data, ps);
  end

  newscore = lls{depth, mi, mc, mpind}; newg = newgraph{depth, mi, mc, mpind};

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if ps.showpreclean  && ~isempty(newg)
    figure(1)
    clf
    title(sprintf('pre-clean: %s  %g', graph.type, newscore));
    ns = ps.runps.names;
    for i=length(ns)+1:size(newg.adj,1)
      ns{i}=' ';
    end
    draw_dot(newg.adj, ns);
    drawnow
  end
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  if strcmp(ps.runps.type, 'rel')
    % try swapping objects before removing clusters. This gives a new cluster
    % the chance to establish itself. Mainly added (5/8/06) to allow the
    % algorithm to find the best ring for the Kula data. I haven't tested this
    % for feature/similarity data but suspect it might not be good for two
    % reasons: 
    %	a) for feature data, we initially tie branches together and therefore
    %	encourage the model to introduce more clusters than is correct. We
    %	might not want to make it easier for extra clusters to stick around. 
    %	b) it's probably too expensive on the bigger data sets.
    [newscore, newg] =  gibbs_clean(newg, data, ps, 'loopmax', 2, 'fast', 1,...
    				    'swaptypes', [1,0,0,0,0]);
    [newscore, newg] =  gibbs_clean(newg, data, ps, 'loopmax', 2, 'fast', 1,...
    				    'swaptypes', [0,1,0,0,0]);
    %[newscore, newg] =  gibbs_clean(newg, data, ps, 'loopmax', 2, 'fast', 1);
  else
    % clean new graph using a fast pass
    disp('fastcleaning');
    [newscore, newg] =  gibbs_clean(newg, data, ps, 'loopmax', 2, 'fast', 1);
  end

  switch ps.speed
    case {5} % avoid optimizing branch lengths in many cases
      if depth < 10 
	  disp('small depth: optimizing branch lengths of best split');
          [nll, ng] = optimizebranches(newg, data, ps);
          [newscore, newg] = graphscorenoopt(ng, data, ps);
      end
      
      if newscore - currprob <= loopeps 
        % opt branch lengths as heuristic
        disp('optimizing branch lengths');
        [nll, ng] = optimizebranches(graph, data, ps);
        [newscore, newg] = graphscorenoopt(ng, data, ps);
        if newscore - currprob <= loopeps && depth >= 10 
          disp('optimizing branch lengths of best split');
          [nll, ng] = optimizebranches(newgraph{depth,mi,mc,mpind}, data, ps);
          [newscore, newg] = graphscorenoopt(ng, data, ps);
        end
      end

      if newscore - currprob <= loopeps 
        % clean current graph using a gibbs-style pass
        disp('slow cleaning current graph');
        [newscore, newg] =   gibbs_clean(graph, data, ps, 'loopmax', 2);
      end

      if newscore - currprob <= loopeps 
        % optimize all splits at this depth
        disp('optimize all splits at current depth');
        [lls newgraph]=optimizedepth(graph, depth, lls, newgraph, data,ps);
        [m mi mc mpind] = bestsplit(graph, depth, lls);
        if lls{depth,mi,mc,mpind} > newscore
          newscore = lls{depth,mi,mc,mpind};
          newg = newgraph{depth,mi,mc,mpind};
        end
      end
      
    case{1,2,3,4} 
      if newscore - currprob <= loopeps && ps.speed == 4
        % optimize all splits at this depth
        disp('optimize all splits at current depth');
        [lls newgraph]=optimizedepth(graph, depth, lls, newgraph, data,ps);
        [m mi mc mpind] = bestsplit(graph, depth, lls);
        if lls{depth,mi,mc,mpind} > newscore
          newscore = lls{depth,mi,mc,mpind};
          newg = newgraph{depth,mi,mc,mpind};
        end
      end
     
      if newscore - currprob <= loopeps 
        % clean current graph using a gibbs-style pass
        disp('slow cleaning current graph');
        [newscore, newg] = gibbs_clean(graph, data, ps, 'loopmax', 2);
      end
    
      if newscore - currprob <= loopeps 
        % replace best split with a near miss of current graph if we can find a
        %	good one
        disp('looking for 10 near misses');
        [newscore, newg] = gibbs_clean(graph, data, ps, 'loopmax', 2, ...
    				   'nearmisses', 10,  'loopeps', loopeps);
      end
  end

  
  % if we still can't beat current graph
  if newscore - currprob <= loopeps 
    stopflag=1;
  else % NB: we might go around a few extra times when graph and newgraph are
       % basically the same
    disp(sprintf('improvement: %g', newscore - currprob));
    newgraph{depth,mi, mc, mpind} = newg;
    lls{depth, mi, mc, mpind} = newscore;
    graph = newg;
    currprob = newscore;
    bestgraph{depth} = graph; bestgraphlls(depth) = currprob;
    depth = depth+1;
    save(savefile, 'bestgraphlls', 'bestgraph');
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if ps.showpostclean 
      figure(2)
      clf
      title(sprintf('post-clean: %s  %g', graph.type, currprob));
      ns = ps.runps.names;
      for i=length(ns)+1:size(graph.adj,1)
        ns{i}=' ';
      end
      draw_dot(graph.adj, ns);
      drawnow
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  end
end

ll = currprob;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [m mi mc mpind]= bestsplit(graph, depth, lls) 

% find indices of best split at DEPTH

m = -inf; mi = 1; mc = 1; mpind = 1;
for i=1:graph.ncomp
  clegal = unique(graph.components{i}.z);
  for c = 1:graph.components{i}.nodecount
    % if c is cluster node
    if ismember(c, clegal) 
      for pind = 1:graph.components{i}.prodcount
        if lls{depth, i, c, pind} > m
          m = lls{depth,i,c, pind}; mi = i; mc = c; mpind = pind;
	end
      end
    end
  end
end

if graph.ncomp > 1
  i = graph.ncomp+1;
  for c = 1:size(lls, 3)
      if ~isempty(lls{depth,i,c,1}) && lls{depth, i, c, 1} > m
        m = lls{depth,i,c, pind}; mi = i; mc = c; mpind = pind;
      end
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% approximate graph score (no integrating out)
function [ll graph]= graphscorenoopt(graph, data, ps) 

ps.fast = 1; % fast mode -- don't compute MAP branch lengths, etc
[ll graph] = graph_like(data, graph, ps);
 ll = ll + graph_prior(graph, ps);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [ll graph]= optimizebranches(graph, data, ps) 

ps.fast = 0;
[ll graph] = graph_like(data, graph, ps);
 ll = ll + graph_prior(graph, ps);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [lls newgraph]= optimizedepth(graph, depth, lls,  newgraph, data, ps) 
% optimize all splits at DEPTH

for i=1:graph.ncomp
  clegal = unique(graph.components{i}.z);
  for c = 1:graph.components{i}.nodecount
    % if c is cluster node
    if ismember(c, clegal) 
      for pind = 1:graph.components{i}.prodcount
        % there'll be no splits of nodes with one object
        if ~isempty(newgraph{depth,i,c,pind})
          [lls{depth, i, c, pind}, newgraph{depth, i, c, pind}] = ...
	    optimizebranches(newgraph{depth,i, c, pind}, data, ps);
        end
      end
    end
  end
end

if graph.ncomp > 1
  i = graph.ncomp+1;
  for c = 1:size(lls, 3)
      if ~isempty(newgraph{depth, i, c, 1}) 
          [lls{depth, i, c, 1}, newgraph{depth, i, c, 1}] = ...
	    optimizebranches(newgraph{depth,i, c, 1}, data, ps);
      end
  end
end



