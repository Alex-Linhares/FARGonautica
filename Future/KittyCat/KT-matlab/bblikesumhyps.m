function ll = bblikesumhyps(ys, ns, alphas, betas)

% Compute log probability of data (YS, NS) under beta binomial model 
% LL = log(p(ys|ns)) = log( sum( p(ys|alpha, beta, ns)p(alpha, beta) ) )

if length(ys) == 0
  ll = 0; return;
end

ys = ys(ns>0);
ns = ns(ns>0);

if isempty(ns)
  ll = 0; return;
end

hypcount = length(alphas);
dcount   = length(ys);

allys = repmat(ys, 1, hypcount); allns = repmat(ns, 1, hypcount);
allalphas = repmat(alphas', dcount,1); allbetas = repmat(betas', dcount,1);

% assume flat prior over each pair of hyperparameters
lls = bbloglike(allalphas, allbetas, allns, allys);

offset = max(lls);
ll = log(mean(exp(lls - offset)))+offset;

