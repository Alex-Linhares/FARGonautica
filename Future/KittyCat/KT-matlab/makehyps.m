function [alphas betas]=makehyps(props, sums)

% Make a grid of parameters given proportions PROPS and sums SUMS


[Ps Ss]=meshgrid(props, sums);
[alphas, betas] = trans2orig(Ps, Ss);
alphas = alphas(:); betas = betas(:);

