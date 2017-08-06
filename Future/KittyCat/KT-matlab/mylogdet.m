function y = logdet(A)
% log(det(A)) where A is positive-definite.
% This is faster and more stable than using log(det(A)).

%  From Tom Minka's lightspeed toolbox

[U p] = chol(A);
if p == 0
  y = 2*sum(log(diag(U)));
else
  y = log(det(A));
end
