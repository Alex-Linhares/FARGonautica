function L=sumlogs(X)

% L = log(sum(exp(X)));
mx=max(max(X));

Xp=exp(X-mx);
L=log(sum(Xp))+mx;
