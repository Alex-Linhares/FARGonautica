function ll = rellikefreqs(countvec, adjvec, sizevec, alphas, betas)

% Compute probability of the data under the (relational) frequency model.

% one parameter for each pair of latent classes
hypcount = length(alphas);
symcount = length(countvec);

allcounts = repmat(countvec, hypcount, 1); 
alladj= repmat(adjvec, hypcount, 1);
allalphas = repmat(alphas', 1, symcount);
allbetas = repmat(betas', 1, symcount);
allsizes = repmat(sizevec, hypcount,1);

allalphas = allsizes.*(alladj.*allalphas + (1-alladj).*allbetas);

% assume flat prior over each pair of hyperparameters
lls = dirmultloglike(allalphas, allcounts);

offset = max(lls);
ll = log(mean(exp(lls - offset)))+offset;

% ll is p(countvec|graph). Now compute p(data|countvec)  
sizevec(sizevec == 0)=1;
ll = ll - sum(countvec.*log(sizevec));


