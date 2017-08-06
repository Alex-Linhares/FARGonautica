function [logI graph] = graph_like_conn(data, graph, ps)

% Compute log P(DATA|GRAPH)
% D: feature data or similarity data

% convert to log weights
graph.Wsym(graph.adjsym> 0) = log(graph.Wsym(graph.adjsym>0));
graph.sigma = log(graph.sigma);

% FAST MODE
if isfield(ps, 'fast') && ps.fast == 1 % don't optimize branch lengths
  % old, slower approach: make Xinit and call dataprobWsig
  % add value for sigma
  Xinit=mat2vec(graph.Wsym, graph, ps);
  Xinit = [graph.sigma;Xinit];
  logI = -dataprobwsig(Xinit, data, graph, ps);

  %  if 0 % faster approach -- but it can't deal with missing data yet 
  %    Xinit=mat2vec(graph.Wsym, graph);
  %    if size(data,1)==size(data,2)
  %      dim = ps.runps.dim;
  %    else
  %      dim = size(data,2);
  %    end
  %    J = inv_covariance(graph.Wsym, graph.objcount, graph.sigma); G = inv(J);
  %    logI = gplike(data, G, dim, ps)+ weightprior(Xinit, ps.lbeta)+...
  %       weightprior(graph.sigma, ps.sigbeta);
  %  end

  % convert back to original weights
  graph.Wsym(graph.adjsym> 0) = exp(graph.Wsym(graph.adjsym>0));
  graph.sigma = exp(graph.sigma);
  return;
end


% SLOW MODE: run gradient-based optimization, use Laplace approximation
upper_bound = 200;
dprobfun = @dataprobwsig;

Xinit=mat2vec(graph.Wsym, graph, ps);
% add value for sigma
Xinit = [graph.sigma;Xinit];

% Check that gradient is correct
% checkgrad('dataprobwsig', Xinit, 1e-5, data, graph, ps)  
% [a b] = dataprobwsig(Xinit, data, graph, ps);

gradstring = 'on'; lsstring = 'on';
options = optimset('GradObj', gradstring, 'LargeScale', lsstring, ...
    	       'Display', 'off'); 

% Find MAP values of branch lengths

[X, fX]=fminunc(@(x) dataprobwsig(x, data, graph, ps), Xinit, options) ; 

Xorig = X;
graphorig = graph;

switch graph.type
  case{'XXXHIDDENtree'}
    ps.cleanstrong = 1;
    % clean graph before computing Laplace approximation
    graphL = simplify_graph(graph, ps);
    ps.cleanstrong = 0;
    Xinit=mat2vec(graphL.Wsym, graphL, ps);
    Xinit = [graphL.sigma;Xinit];
    [X, fX]=fmincon(dprobfun, Xinit, [], [], [], [], ...
        0.001*ones(size(Xinit)), upper_bound*ones(size(Xinit)), [], ...
        options, data, graphL, ps) ; 
  otherwise
    graphL = graph;
end

ll = -feval(dprobfun, X, data, graphL, ps);
datal = @(x) feval(dprobfun, x, data, graphL, ps);

% Laplace approximation to p(D|S)
% We approximate the integral over branch lengths
% minus sign because datal computes -ll
H=-hessiangrad(datal, X, 1e-5);

includeind = find(X<upper_bound - 5);
H = H(includeind, includeind);
d = length(includeind);
if includeind(1) ~= 1
  disp('WARNING: sigma blows up');
end

logI = (d/2)*log(2*pi)+0.5*mylogdet(inv(-H))+ll;
if ~isreal(logI)
  disp('WARNING: laplacian approx gone awry');
  es = eig(inv(-H));
  dapprox = real(prod(es(es>0)));
  logI = (d/2)*log(2*pi)+0.5*log(dapprox)+ll;
end

% convert back to original weights
Xorig = exp(Xorig);

[graph ps]= combineWs(graphorig, Xorig(2:end), ps);
graph.sigma = Xorig(1);




