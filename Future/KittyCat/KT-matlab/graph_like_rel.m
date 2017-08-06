function [logI graph] = graph_like_rel(data, graph, ps)

% graph_like_rel(data, adj, beta, sigma): compute log p(DATA|GRAPH), where
%   D is a relational data set

origgraph = graph;

switch graph.type
  case{'order', 'domtree', 'ordernoself', 'dirdomtreenoself', 'undirdomtree', 'undirdomtreenoself', 'connected', 'connectednoself'}
    % For these forms we only store the "backbone" of a transitive graph
    % -- so reconstruct the full transitive graph
    graph = filloutrelgraph(graph);
end

nobj = graph.objcount;
nclust = size(graph.adj,1)-nobj;
for i=1:nclust
  clustmembers{i}=find(graph.z==i);
end

switch data.type 
  case 'reldom' % Not currently used
    edgepropsteps = 5; edgesumsteps = 5; edgeoffset=0;
    edgeprops = (1:edgepropsteps)/edgepropsteps - 1/(2*edgepropsteps);
    edgesums = 2.^(edgeoffset+1:edgeoffset+edgesumsteps);
    [alphas betas]=makehyps(edgeprops, edgesums);

    noedgeprops = 0.5; noedgesums=2.^(-3:2); 
    [alphasnoedge betasnoedge]=makehyps(noedgeprops, noedgesums);
    
    for i=1:nclust
      for j=1:nclust
        yobs(i,j) = sum(sum(data.R(clustmembers{i}, clustmembers{j},1)));
        nobs(i,j) = sum(sum(data.R(clustmembers{i}, clustmembers{j},2)));
      end
    end

    clustgraph = graph.adjcluster;
    edgeones =  find(clustgraph); edgezeros = find(1-clustgraph);
    orig.yobs=yobs;
    orig.nobs=nobs;
    orig.clustgraph = clustgraph;

    if data.lowdiag
      if sum(diag(data.R(:,:,2)))>0
        error('data not lower diagonal!');
      end

      % cases for which no prediction is made 
      %	    within cluster probs, (a,b)
      %	    relationships when there's no edge between a and b

      doubgraph = clustgraph+clustgraph';
      doubgraph = doubgraph+eps*tril(ones(size(clustgraph)), -1);
      nopreds = find(doubgraph==0 | doubgraph==2);
      diagind = sub2ind(size(clustgraph), 1:nclust, 1:nclust);
      nopredys = yobs; nopredns = nobs;
      nopredys(diagind) = yobs(diagind)/2; nopredns(diagind) = nobs(diagind)/2; 
      yobsnopreds = nopredys(nopreds); nobsnopreds = nopredns(nopreds);
      lognopredI = 0 ;
      if ~isempty(nobsnopreds(nobsnopreds>0));
        lognopredI = bblikesumhyps(yobsnopreds, nobsnopreds, alphasnoedge,...
				   betasnoedge); 
      end

      clustgraphtmp = clustgraph + eps*tril(ones(size(clustgraph)));
      clustgraphtmp(nopreds) = clustgraphtmp(nopreds)+eps;

      edgeones =  find(clustgraphtmp==1); edgezeros = find(clustgraphtmp==0);
      yobs(edgezeros)=nobs(edgezeros)-yobs(edgezeros);
      edgeones = [edgezeros; edgeones]; edgezeros=[];
    end

    ysones = yobs(edgeones); nsones=nobs(edgeones);
    yszeros = yobs(edgezeros); nszeros=nobs(edgezeros);
    
    logI1 = 0; logI2 = 0;
    if ~isempty(nsones(nsones>0));
      logI1 = bblikesumhyps(ysones, nsones, alphas, betas); 
    end
    if ~isempty(nszeros(nszeros>0));
      logI2 = bblikesumhyps(yszeros, nszeros, alphas, betas);
    end
    logI = logI1+logI2;

    if data.lowdiag
      logI = logI+lognopredI;
    end

    % for a two cluster graph, can't get the edge direction right since we have
    % a flat prior over \theta_edge and \theta_noedge
    nclustedges = sum(sum(clustgraph));
    if (size(clustgraph,1)==2 && nclustedges==1) 
      [r c]= find(clustgraph==1);
      if nobs(c,r) > 0 && yobs(c,r)/nobs(c,r) > yobs(r,c)/nobs(r,c)
        graph.adj(nobj+1:end, nobj+1:end) = clustgraph';
      end
    end
  case {'relfreq', 'relbin'}  
    %     RELFREQ: data show frequencies of directed interactions between 
    %	    individuals
    %     RELBIN: a directed matrix (binary)

    z = graph.z; z = z(z>=0);
    classsizes = hist(z, 1:nclust);
    sizematrix= repmat(classsizes, nclust, 1).*repmat(classsizes', 1, nclust);
    % self links  aren't allowed ...
    diagind = sub2ind([nclust, nclust], 1:nclust, 1:nclust);
    sizematrix(diagind) = sizematrix(diagind) - classsizes;
    switch graph.type
      case{'partition', 'order', 'dirchain', 'dirring', 'dirhierarchy',...
	   'domtree', 'connected' 'undirchain', 'undirring', ...
	   'undirhierarchy', 'undirdomtree'}
        % ... but some forms expect links within classes
	graph.adjcluster(diagind) = 1;
    end

    switch graph.type
      case{'undirchain', 'undirring', 'undirhierarchy', 'undirdomtree', ...
      'undirchainnoself', 'undirringnoself', 'undirhierarchynoself', ...
      'undirdomtreenoself'}
        % ... and others expect links to be symmetric
	graph.adjcluster = graph.adjcluster | graph.adjcluster';
    end

    classcounts = countmatrix(data.R, graph); countvec= classcounts(:);
    adjvec = graph.adjcluster(:); 
    sizevec = sizematrix(:);

    %we know that edges in adj encourage links  
    edgepropsteps = 5; edgesumsteps = ps.edgesumsteps;
    edgeoffset=ps.edgeoffset;
    edgesums = ps.edgesumlambda.^(edgeoffset+1:edgeoffset+edgesumsteps);
    edgeprops = (edgepropsteps+1:2*edgepropsteps)/(2*edgepropsteps) -...
		 1/(4*edgepropsteps);
    noedgeprops = (1:edgepropsteps)/(2*edgepropsteps)-1/(4*edgepropsteps);

    [alphasedge betasedge]=makehyps(edgeprops, edgesums);
    [alphasnoedge betasnoedge]=makehyps(noedgeprops, edgesums);

    % for binary data
    edgepropsteps = 10; edgesumsteps = ps.edgesumsteps;
    edgeoffset=ps.edgeoffset;
    mags= ps.edgesumlambda.^(edgeoffset+1:edgeoffset+edgesumsteps);

    thetas  = (1:edgepropsteps)/edgepropsteps-1/(2*edgepropsteps);

    switch data.type
      case{'relbin'}
        logI = rellikebin(countvec, adjvec, sizevec, mags', thetas'); 
      case{'relfreq'}
        logI = rellikefreqs(countvec', adjvec', sizevec', ...
        			    alphasedge', betasedge'); 
    end
  otherwise
    error('unknown relational type')
end

if isinf(logI) || isnan(logI)
 keyboard
end
graph = origgraph;

