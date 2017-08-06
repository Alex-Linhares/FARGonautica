function lps = gridpriors(maxn, theta, T, type)

% Count number of ways to put objects on a grid or cylinder.

% THETA: parameter for geometric distribution on cluster number
% T(n,k): number of ways to put n elements into k parcels

switch type
  case{'grid'}
    % G(k,l): number of ways of putting maxn labelled objects on an k by l grid.
    % create for k <= l,  k<=maxn 
    for k = 1:maxn
      for l = k:maxn
        % G(k,l): ways of putting maxn objects on a k by l grid
        count = T(maxn,k)*T(maxn,l);
        if l == 1 % no symmetries
        elseif k == l
          onedcount = T(maxn,k);
	                   % subtract cases where k, l dimensions same
          count = (count - 2*onedcount)/8 + onedcount/2;
        elseif k == 1
          count = count/2; % l dimension can be flipped
        else
          count = count/4; % both dimensions can be flipped
        end
        G(k,l) = count;
      end
    end
    occind = find(triu(ones(maxn)));
  case{'cylinder'}
    for k = 1:maxn
      for l = 1:maxn
        % G(k,l): ways of putting maxn objects on a k (line) by l (ring)
	%   cylinder
	kcount = T(maxn,k);
	if k ~=1
	  kcount = kcount/2;   % chain can be reflected
        end
	lcount = T(maxn, l)/l; % ring can be rotated
	if l > 2
	  lcount = lcount/2;   % chain representation of ring can be flipped
	end
        G(k,l) = kcount * lcount;
      end
    end
    occind = find(ones(maxn));
end
  
logG = zeros(maxn);
logG(occind) = log(G(occind));
gridsizes = repmat(1:maxn, maxn,1).*repmat((1:maxn)', 1, maxn);
logweights = log(theta) + (1:maxn^2)*log(1-theta);
loggridweights = logweights(gridsizes);

logtotsum = sumlogs(logG(occind)+loggridweights(occind));
% weights for some impossible nodecounts will be represented (but they don't
% contribute to logtotsum, so this should be OK)
lps = logweights - logtotsum;
