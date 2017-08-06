function [alphas betas]=trans2orig(Ps, Ss)

alphas = Ps.*Ss;
betas = Ss - alphas;
    



