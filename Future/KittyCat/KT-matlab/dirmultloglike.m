function ll=dirmultloglike(alpha, counts) 

% LL is the log likelihood of the data (counts) under the Dirichlet-multinomial
%   model with prior alpha
% alpha and counts can be matrices (each row is a single alpha or count vector)

% 5/18/07 -- added this temporarily
%if isempty(counts)
%  ll = 0;
%  return;
%end

countsplusalpha = counts+alpha;
countsplusalpha(countsplusalpha==0)=1;

alphanonz = alpha;
alphanonz(alpha==0)=1;


ll = gammaln(sum(alpha, 2)) + sum(gammaln(countsplusalpha), 2) ...
	   - sum(gammaln( alphanonz), 2) - gammaln(sum(counts+alpha, 2));
