function ps = structcounts(nobjects, ps)

% make PS.LOGPS: ps.logps{i}(n) is prior for an i-structure with n clusters 
% We compute these priors in advance and cache them.

maxn = nobjects;
% THETA: parameter for geometric distribution on number of nodes
theta = ps.theta;

%    S2(N,M) represents the number of distinct partitions of N elements
%    into M nonempty sets.  
s2 = stirling2(maxn,maxn);

for i = 1:maxn
  F(i) = factorial(i);
end

% T(n,k): number of ways to put n elements into k parcels
ps.T = repmat(F, maxn, 1).*s2;

% counts(i, n) is number of architectures of type i with n labelled clusters.
% architecture specifies where cluster nodes go but not what they contain

% partition, connected
counts(1,:) = zeros(1, maxn);
% chain
counts(2,:) = gammaln( (1:maxn) +1) - log(2);
counts(2, 1) = 0;
% ring
counts(3,:) = gammaln(1:maxn) - log(2);
counts(3, 1:2) = [0,0];
% unrooted tree
counts(4,:) = gammaln( (1:maxn)-1.5) + ((1:maxn)-2)*log(2)-0.5*log(pi) ;
counts(4,1) = 0;
% hierarchy unrooted
counts(5,:) = ((1:maxn)-2).*log(1:maxn); 
% rooted hierarchy 
counts(6,:) = ((1:maxn)-1).*log(1:maxn); 
% dirchain
counts(7,:) = gammaln((1:maxn)+1); 
% dirring
counts(8,:) = gammaln(1:maxn); 

logcounts = counts;

% we choose among all structures where each dimension contains no holes (but
% multiple objects can end up at the same node). Each structure is weighted
% according to the number of nodes it contains.

% consider the number of ways to partition the objects into each ncluster 
logclustercounts = log(s2(maxn,:));

% combine npartitions with narchitectures to get number of structures
logcounts = logcounts + repmat(logclustercounts, 8,1);
logweights = log(theta) + (1:maxn)*log(1-theta);

for i = 1:8
  totsums(i) = sumlogs(logweights + logcounts(i,:));
end
lcs = repmat(logweights, 8,1) - repmat(totsums', 1,maxn);

for i = 1:8
  logcs{i} = lcs(i,:);
end
logcs{9} = gridpriors(maxn, theta, ps.T, 'grid');
logcs{10} = gridpriors(maxn, theta, ps.T, 'cylinder');
ps.logps= logcs;

