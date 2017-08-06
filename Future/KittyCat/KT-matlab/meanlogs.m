function L=meanlogs(X)

% L = log(sum(exp(X)));
mx=max(max(X));

Xp=exp(X-mx);
L=log(mean(Xp))+mx;
