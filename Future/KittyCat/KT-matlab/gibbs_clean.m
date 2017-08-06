function [ll graph] = gibbs_clean(graph, data, ps, varargin)

% SWAPTYPES: which swaps to include
% 1. individual objects
% 2. cluster nodes
% 3. subtreeprune (objects and cluster nodes)
% 4. at level of entire graph
% 5. remove dimensions

swaptypes = [1, 1, 1, 1, 1];
origgraph = graph;
args = varargin;
loopmax = 1;
debug = 0;
epsilon = 1e-4;
loopeps = epsilon;
optlens = 1;	 % optimize branch lengths, and use Hessian based score 
fastflag = 0;    % nearby swaps only

nearmisses = 0;
nearmissesk = 4; % store four times as many as needed.

for i=1:2:length(args)
  switch args{i}
   case 'loopmax',         loopmax = args{i+1};
   case 'debug',             debug = args{i+1};
   case 'nearmisses',   nearmisses = args{i+1};
   case 'loopeps',         loopeps = args{i+1};
   case 'optlens',	   optlens = args{i+1};
   case 'swaptypes',	 swaptypes = args{i+1};
   case 'fast',		  fastflag = args{i+1};
  end
end
nearmisses = nearmisses*nearmissesk;
optlens = ps.speed < 5;

% gold standards: don't accept changes that make these worse!
llgold = -inf;
graphgold = graph;

% XXX: we assume that simplifying graph won't make score worse -- but check this
orig_graph = graph; graph = simplify_graph(graph, ps);
overallchange = 1;
loopoverall = 0;

nearmscores = -inf*ones(1, nearmisses);
nearmgraphs = cell(1, nearmisses);

while overallchange == 1 && loopoverall < loopmax
  overallchange = 0;
  loopoverall = loopoverall+1;
  disp(sprintf('gibbs_clean: iteration %g', loopoverall));

  % run branch length optimization
  if optlens 
    ps.fast = 0; 
    [ll graph] = graph_like(data, graph, ps);
    ll = ll + graph_prior(graph, ps);
    if ps.gibbsclean == 0 break; end
    origll = ll;
    if ll > llgold
      llgold = ll; graphgold = graph;
    else
      disp('gibbs_clean: graph is getting worse');
      graph = graphgold; ll = llgold;
      break;
    end
  end
  if ps.speed > 1 ps.fast = 1; end

  % use approximate scores throughout
  [testl, ng]=graph_like(data, graph, ps);
  currscore= testl + graph_prior(graph, ps);

  for i=1:graph.ncomp
    % cluster swaps
    if swaptypes(2) && graph.components{i}.nodecount > 1 
      [graph currscore overallchange nearmscores nearmgraphs]= ...
  	    swapobjclust(graph, data, ps, i, epsilon, currscore, ...
  			 overallchange, loopmax, nearmscores, nearmgraphs,...
			 'fastflag', fastflag); 
      % finish if we've improved
      if ~optlens && overallchange && ~fastflag  ll = currscore; return; end
    end

    % subtree pruning and regrafting
    if swaptypes(3) && graph.components{i}.nodecount > 1 && ~fastflag
      switch graph.components{i}.type
        case{'tree', 'hierarchy', 'dirhierarchy', 'domtree', ...
	     'dirhierarchynoself', 'undirhierarchy', 'undirhierarchynoself',...
	     'domtreenoself'}
	  [graph currscore overallchange nearmscores nearmgraphs]= ...
	   spr(graph, data, ps, i, epsilon, currscore, overallchange, debug,...
	    nearmscores, nearmgraphs); 
	end
      % finish if we've improved
      if ~optlens && overallchange && ~fastflag ll = currscore; return; end
    end
  end

  if graph.ncomp > 1 && sum(graph.compsizes > 1) > 1
    if swaptypes(5)
      % try removing some dimensions
      [graph currscore overallchange nearmscores nearmgraphs]= ...
  	    collapsedims(graph, data, ps, epsilon, currscore, ...
  			 overallchange, loopmax, nearmscores, nearmgraphs);
    end

    if swaptypes(4) 
      % cluster swaps at level of entire graph
      [graph currscore overallchange nearmscores nearmgraphs]= ...
  	    swapobjclust(graph, data, ps, [], epsilon, currscore, ...
			 overallchange, loopmax, nearmscores, nearmgraphs,...
			 'fastflag', fastflag); 
    end
  end

  % finish if we've improved
  if ~optlens && overallchange && ~fastflag ll = currscore; return; end

  if swaptypes(1)
    % object swaps -- will be slow for large graphs
    [graph currscore overallchange nearmscores nearmgraphs]= ...
	    swapobjclust(graph, data, ps, [], epsilon, currscore,...
			 overallchange, loopmax, nearmscores, nearmgraphs,...
			 'objflag', 1, 'fastflag', fastflag); 
  end
end

if optlens 
  if loopoverall == loopmax % we fell out of loop without optimizing
			    % branch lengths
    ps.fast = 0;
    [ll graph] = graph_like(data, graph, ps);
    ll = ll + graph_prior(graph, ps);
    disp('overall: loop count exceeded');
  end

  if nearmisses > 0
    [optg optscores] = nearmissopts(nearmgraphs, nearmscores, graph, ...
  				    nearmisses, nearmissesk, ps, epsilon);
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if 0 % show nearmisses
      %for i = 1:length(optg)
      for i = 1:8
        %subplot(ceil(sqrt(length(optg))),ceil(sqrt(length(optg))),i)
	clf;
        ps.cleanstrong = 1;
        % sg = simplify_graph(optg{i}, ps);
        sg = optg{i};
        draw_dot(sg.adj, ps.runps.names);
        gsig{i} = graphsig(sg.adjsym, sg.objcount);
        title(num2str(optscores(i)));
        ps.cleanstrong = 0;
	drawnow; 
      end
    end
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
    ps.fast = 0; 
    for i = 1:length(optg)
      disp(sprintf('opt %g', i));
      [llnm graphnm] = graph_like(data, optg{i}, ps);
      llnm = llnm + graph_prior(graphnm, ps);
      if llnm - ll > loopeps
        disp('found a good near-miss');
        graph = graphnm; ll = llnm;  break;
      end
    end
  end
else % don't need to worry about optimizing lengths and computing true score
  ll = currscore;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find  near misses to optimize
function [optg optscores] = nearmissopts(nearmgraphs, nearmscores, graph, ...
				   nearmisses, nearmissesk, ps, epsilon);
nearmisses = nearmisses/nearmissesk;
sigs = [];
ps.cleanstrong = 1;
i = 0; j = 0;
optg = {}; optscores = [];
% so that we don't optimize the current best graph again
nearmgraphs = cat(2, graph, nearmgraphs{:}); nearmscores = [0, nearmscores];
while i < nearmisses & j <= nearmisses*nearmissesk
  j = j+1; 
  if j > length(nearmgraphs)
    break
  end

  sg = nearmgraphs(j);
  if ps.nauty % If nauty installed, make sure that the near misses checked
	      %	    are all different up to isomorphism 
    jsig = graphsig(sg.adjsym, sg.objcount);
    if length(jsig) > size(sigs,2)
      newsig = zeros(j, length(jsig)); 
      newsig(1:j-1, 1:size(sigs,2)) = sigs; sigs = newsig;
    end
    sigs(j, 1:length(jsig)) = transpose(jsig);
    ds = (repmat(sigs(j,:),j-1, 1) - sigs(1:j-1,:));
    % never pick the first graph but that's OK
    if j > 1 && min(sum(abs(ds),2)) > 0 && ...
       min(abs(nearmscores(j) - nearmscores(1:j-1))) > epsilon
      i = i+1;
      optg{i} = nearmgraphs(j);  optscores(i) = nearmscores(j);
    end
  else % consider the best NEARMISSES graphs in NEARMGRAPHS, even if some
       %   are the same up to isomorphism
    i = i+1;
    optg{i} = nearmgraphs(j);  optscores(i) = nearmscores(j);
  end
end
