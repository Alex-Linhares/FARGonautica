function ll = gplike(X, G, dim, ps)

% Compute log probability of data X (assume 0 mean) under a Gaussian model
%   with covariance matrix G 

% X: observation matrix. One object per row. Missing entries represented by inf

nobj = size(X,1);
Gsmall = G(1:nobj, 1:nobj);
invGsmall= inv_posdef(Gsmall);
logdetGsmall= logdet(Gsmall);

if strcmp(ps.runps.type, 'sim')   % similarity data
  ll = dim*(-0.5*logdetGsmall-nobj/2*log(2*pi));
  ll = ll-0.5*trace(dim*X*invGsmall);
else
  nfeat = size(X,2);
  if nobj  == ps.runps.chunkcount && ps.overrideSS == 0
    XX = nfeat*ps.runps.SS; 
  else 
    XX = X*X';
  end
  ll = nfeat*(-0.5*logdetGsmall-nobj/2*log(2*pi));
  %disp(sprintf('ll:%g tr: %g detGsmall:%g', ll, -0.5*trace(XX'*invGsmall), logdetGsmall));
  ll = ll-0.5*trace(XX'*invGsmall);
end

