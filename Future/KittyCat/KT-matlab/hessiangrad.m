function H = hessiangrad(f,X,e,varargin)

% PURPOSE: Computes finite difference using gradient 
% -------------------------------------------------------
% Usage:  H = hessian(func,x,varargin)
% Where: func = function name, [fval G] = func(x,varargin)
%           x = vector of parameters (n x 1)
%	    e = small perturbation for computing finite differences
%    varargin = optional arguments passed to the function
% -------------------------------------------------------
% RETURNS:
%           H = finite differnce hessian
% -------------------------------------------------------

[Y dY]= feval(f,X,varargin{:});

H = zeros(length(dY),length(X)) ;
for j = 1:length(X)
  dX = zeros(length(X),1);
  dX(j) = dX(j) + e;                           % perturb a single dimension
  [Y1 dY1]= feval(f,X+dX,varargin{:});
  dX = -dX ;
  [Y2 dY2]= feval(f,X+dX,varargin{:});
  H(:,j) = vec(dY1 - dY2)/(2*e);
end

