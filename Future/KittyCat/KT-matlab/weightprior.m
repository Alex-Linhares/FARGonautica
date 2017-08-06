function lp = weightprior(w, beta);

% Compute exponential prior on edge weights.  Remember we're now working
% in log weight space

% w is weight, but we want prior on corresponding log weight v
lp = sum (-log(beta) - 2*log(w)-1./(beta*w) +log(w));

% gamma with shape = 2
%lp = sum (-2*log(beta) - 3*log(w)-1./(beta*w) );
