function ll = rellikebin(countvec, adjvec, sizevec, mags, thetas)

% Compute probability of the data under the model for binary relations.

%   MAGS: sum of hyperparameters
% THETAS: proportion of 1s

% zs(i): zeroscore. sum_m p(C_0|\theta_i, m)p(m) 
% os(i): onescore.  sum_m p(C_1|\theta_i, m)p(m) 

% matrices: (mags, thetas)
magsmat   = repmat(mags, 1, length(thetas));
thetasmat = repmat(thetas', length(mags), 1);
alphasmat = thetasmat.*magsmat;
betasmat  = magsmat - alphasmat;
alphas = alphasmat(:); betas = betasmat(:); hypcount = length(alphas);

vals = [0 1];
for vind = 1:length(vals)
  v = vals(vind);
  ys = countvec(adjvec==v); ns = sizevec(adjvec==v); dcount = length(ys);
  if isempty(ys)
    llsmat{vind} = 0*magsmat;
  else
    allys = repmat(ys, 1, hypcount); allns = repmat(ns, 1, hypcount);
    allalphas = repmat(alphas', dcount,1); 
    allbetas = repmat(betas', dcount, 1);
    lls = bbloglike(allalphas, allbetas, allns, allys);
    llsmat{vind} = reshape(lls, size(alphasmat));
  end
end
for i = 1:size(llsmat{1},2)
  zs(i) = meanlogs(llsmat{1}(:,i));
  os(i) = meanlogs(llsmat{2}(:,i));
end


[zind oind] = find(triu(ones(length(zs))));
% compute mean p(C|S, \theta0, \theta1)
lls = zs(zind)+os(oind);
ll = meanlogs(lls);
