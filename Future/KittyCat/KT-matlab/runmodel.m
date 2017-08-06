function [ll graph names, bestglls, bestgraph] = runmodel(ps, sind, ...
							  dind, rind, savefile)

% Given data set DIND, find the best instance of form SIND. 
%   PS:	      parameter structure with hyperparameters, etc
%   RIND:     which repeat is this?
%   SAVEFILE: where to save interim results
% Output:
%   LL:	       log probability of the best structure found
%   GRAPH:     the best structure found
%   BESTGLLS:  log probabilities of the structures explored along the way
%   BESTGRAPH: structures explored along the way

ps.runps.structname=ps.structures{sind};


display(sprintf('    Repeat %g', rind));
fileroot=['results\',ps.structures{sind}, 'out\', ps.data{dind}, ...
          num2str(rind)];
fileroot;
currdir=pwd;

% make directory
[s,mess,messdir]=mkdir(fileroot);
cd(fileroot)
savefile = 'growthhistory';

names=[]; 
% load data, names, features into variable space
%fprintf('whos 1 ==================================================');
%whos;
load(ps.dlocs{dind});
fprintf('whos 2 ==================================================');
whos;

[nobjects, ps]= setrunps(data, dind, ps);
%fprintf('whos 3 ==================================================');
%whos;
[data ps]=scaledata(data,ps);
%fprintf('whos 4 ==================================================');
%whos;
if isempty(names)
  for i=1:nobjects names{i}=num2str(i); end
end

ps.runps.names=names;
if ps.showtruegraph 
  figure(1)
  for i=length(names)+1:size(adj,1)
    names{i} = '';
  end
  draw_dot(adj, names);
  title('real structure');
  drawnow
end

graph = [];

if isfield(ps, 'outsideinit') && ~isempty(ps.outsideinit)
  load(ps.outsideinit);
elseif strcmp(ps.runps.type, 'rel') && strcmp(ps.reloutsideinit,'external') 
  switch ps.runps.structname
    % partitionnoself missing: that's what we use to initialize the others
    case {'partition',  'dirchain', 'dirchainnoself', 'dirring',...
          'dirringnoself', 'dirhierarchy','dirhierarchynoself','undirchain',...
	  'undirchainnoself', 'undirring', 'undirringnoself', ...
	  'undirhierarchy', 'undirhierarchynoself', 'order',...
	  'ordernoself'}
      bestz = load([ps.relinitdir, ps.data{dind}, '_bestz']);
      graph = relgraphinit(data.R, bestz, ps);
  end
else
  if strcmp(ps.runps.type, 'rel') && strcmp(ps.reloutsideinit, 'overd')
    switch ps.runps.structname
    % only for certain structures (not partition, domhier)
      case {'dirchain', 'dirchainnoself', 'dirring',...
          'dirringnoself', 'dirhierarchy','dirhierarchynoself','undirchain',...
	  'undirchainnoself', 'undirring', 'undirringnoself', ...
	  'undirhierarchy', 'undirhierarchynoself'}
        bestz = 1:nobjects;
        graph = relgraphinit(data.R, bestz, ps);
    end
  end
end

if ~isfield(ps, 'overrideSS')
  ps.overrideSS = 0;
end
ps.cleanstrong= 0;
ps = structcounts(nobjects, ps);

switch ps.runps.structname
  case{'griddimsearch'} % an alternative way of searching for grids. can
			% be ignored.
    oldps = ps;
    ps.speed = 5; ps.fixedall= 1; ps.fixedall= 1; ps.init = 'none';
    [score, graph] = runmodel(ps, 2, dind, rind);
    graph.ncomp = 2; graph.components{1}.type = 'chain';
    % initialize second component
    graph.components{2} = graph.components{1}; 
    graph.components{2}.type = 'chain'; 
    graph.components{2}.adj = [0]; graph.components{2}.W = [0];
    graph.components{2}.adjsym = [0]; graph.components{2}.Wsym = [0];
    graph.components{2}.nodecount=   1; graph.components{2}.nodemap  =   1;
    graph.components{2}.edgecount=   0; graph.components{2}.edgemap  =  [0];
    graph.components{2}.edgecountsym= 0; graph.components{2}.edgemapsym  =  [0];
    graph.components{2}.z	     =   ones(1, graph.objcount);
    graph.components{2}.illegal=[];
    graph = combinegraphs(graph, ps);
    graph.type = 'grid';
    ps = oldps; ps.runps.structname = 'grid';
  case{'cyldimsearchring'}
    oldps = ps;
    ps.speed = 5; ps.fixedinternal = 1; ps.fixedexternal=1; 
    ps.init = 'none';
    [score, graph] = runmodel(ps, 3, dind, rind);
    graph.ncomp = 2; graph.components{1}.type = 'ring';
    % initialize second component
    graph.components{2} = graph.components{1}; 
    graph.components{2}.type = 'chain'; 
    graph.components{2}.adj = [0]; graph.components{2}.W = [0];
    graph.components{2}.adjsym = [0]; graph.components{2}.Wsym = [0];
    graph.components{2}.nodecount=   1; graph.components{2}.nodemap  =   1;
    graph.components{2}.edgecount=   0; graph.components{2}.edgemap  =  [0];
    graph.components{2}.edgecountsym= 0; graph.components{2}.edgemapsym  =  [0];
    graph.components{2}.z	     =   ones(1, graph.objcount);
    graph.components{2}.illegal=[];
    graph = combinegraphs(graph, ps);
    ps = oldps; ps.runps.structname = 'cylinder';
    graph.type = 'cylinder';
  case{'cyldimsearchchain'}
    oldps = ps;
    ps.speed = 5; ps.fixedinternal = 1; ps.fixedexternal=1; 
    ps.init = 'none';
    [score, graph] = runmodel(ps, 2, dind, rind);
    graph.ncomp = 2; graph.components{1}.type = 'chain';
    % initialize second component
    graph.components{2} = graph.components{1}; 
    graph.components{2}.type = 'ring'; 
    graph.components{2}.adj = [0]; graph.components{2}.W = [0];
    graph.components{2}.adjsym = [0]; graph.components{2}.Wsym = [0];
    graph.components{2}.nodecount=   1; graph.components{2}.nodemap  =   1;
    graph.components{2}.edgecount=   0; graph.components{2}.edgemap  =  [0];
    graph.components{2}.edgecountsym= 0; graph.components{2}.edgemapsym  =  [0];
    graph.components{2}.z	     =   ones(1, graph.objcount);
    graph.components{2}.illegal=[];
    graph = combinegraphs(graph, ps);
    ps = oldps; ps.runps.structname = 'cylinder';
    graph.type = 'cylinder';
end

bestglls = {}; bestgraph = {};
switch ps.speed
  case{1,2,3,4,5}
    disp(sprintf('using speed %g', ps.speed)); 
    [ll, graph, bestglls, bestgraph, ps] = brlencases(data, ps, graph, ...
					    bestglls, bestgraph, savefile);
  case{54}
    ps.speed = 5; disp('starting at speed 5'); 
    [ll, graph, bestglls, bestgraph, ps] = brlencases(data, ps, graph, ...
					    bestglls, bestgraph, savefile);
    
    ps.speed = 4; disp('refining at speed 4'); 
    ps.init = 'none'; % branches have already been untied (XXX: maybe it's
		      % good to tie them again)
    [ll, graph, bestglls, bestgraph, ps] = brlencases(data, ps, graph, ...
					    bestglls, bestgraph, savefile);
  otherwise
    error('Unknown speed value');
end

if strcmp(ps.runps.structname, 'tree')
   ps.init = 'none'; ps.cleanstrong = 1;
   % remove tree root 
   disp('removing tree root');
   graph = simplify_graph(graph,ps);
   [ll, graph, bestglls, bestgraph, ps] = brlencases(data, ps, graph,... 
					    bestglls, bestgraph, savefile);
end

if ps.speed == 5
  disp('finding true score for speed 5');
  ps.fast = 0;
  [ll graph] = graph_like(data, graph, ps);
  ll = ll + graph_prior(graph, ps);
end

% display estimated structure
if ps.showinferredgraph 
  figure(3)
  for i=length(names)+1:size(graph.adj,1)
    names{i} = '';
  end
  clf
  draw_dot(graph.adj, names);
  title(sprintf('%s: estimated structure:  %g', graph.type, ll));
  drawnow
end

cd(currdir);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% deal with different approaches to branchlengths at current speed
function [ll graph, bestglls, bestgraph, ps] = brlencases(data, ps, graph,...
					        bestglls, bestgraph, savefile);

speedstr = num2str(ps.speed);
switch ps.init
  case{'none'}
    [ll, graph, bestglls{1,ps.speed}, bestgraph{1, ps.speed}] = ...
	structurefit(data, ps, graph, [savefile, 'noinit', speedstr]);
  case{'ext'}
    ps.fixedexternal = 1;
    [ll, graph, bestglls{1,ps.speed}, bestgraph{1}] = ...
	    structurefit(data, ps, graph, [savefile, 'exttie', speedstr]);
    disp('untie external...');
    ps.fixedexternal = 0;
    [ll, graph, bestglls{2, ps.speed}, bestgraph{2, ps.speed}] = ...
	    structurefit(data, ps, graph, [savefile, 'notie', speedstr]); 
  case{'int'}
    ps.fixedinternal = 1;
    [ll, graph, bestglls{1, ps.speed}, bestgraph{1, ps.speed}] = ...
	    structurefit(data, ps, graph,  [savefile, 'inttie', speedstr]);
    disp('untie internal...');
    ps.fixedinternal = 0;
    [ll, graph, bestglls{2, ps.speed}, bestgraph{2, ps.speed}] = ...
	structurefit(data, ps, graph, [savefile, 'notie', speedstr]); 
  case{'intext'}
    ps.fixedinternal = 1; ps.fixedexternal = 1;
    [ll, graph, bestglls{1, ps.speed}, bestgraph{1, ps.speed}] = ...
	structurefit(data, ps, graph, [savefile, 'alltie', speedstr]);
    disp('untie internal...');
    ps.fixedinternal = 0; 
    [ll, graph, bestglls{2, ps.speed}, bestgraph{2, ps.speed}] = ...
	structurefit(data, ps, graph, [savefile, 'exttie', speedstr]);
    disp('untie external...');
    ps.fixedexternal = 0; 
    [ll, graph, bestglls{3, ps.speed}, bestgraph{3, ps.speed}] = ...
	structurefit(data, ps, graph, [savefile, 'notie', speedstr]);
end
 
