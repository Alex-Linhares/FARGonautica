function ll=bbloglike(alpha, beta, ns, ys) 

% function ll=loglike(alpha, beta, ns, ys) 
% ll is the log likelihood of the data (ns, ys) under the beta-binomial model 
%    given hyperparameters alpha, beta 
% ns is a vector specifying how many samples there are for each class and
%    feature
% ys is a vector specifying how many of these samples took on the value 1 


ll = sum(   gammaln(alpha+ys)+gammaln(beta+(ns-ys))-gammaln(ns+alpha+beta)...
	   -gammaln(alpha)-gammaln(beta)+gammaln(alpha+beta),1 );

