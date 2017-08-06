function [J L] = inv_covariance(W, nobj, sigma, ps);

% Compute inverse covariance matrix defined over weighted graph W. We
% basically follow the approach of Zhu, Ghahramani and Lafferty.

n = size(W,1);
D = diag(sum(W, 2));
L = D-W;

% prior on object nodes
P = zeros(n);

if ps.zglreg
  P(1:n, 1:n)=1/sigma^2*eye(n);
else
  P(1:nobj, 1:nobj)=1/sigma^2*eye(nobj);
end

% inverse covariance
J = L+P;

% identify holes -- a hack to deal with orphaned cluster nodes lying around
% (should remove these)
adj = W~=0;
holes = find(sum(adj)==0);
J(holes, holes)=1;

