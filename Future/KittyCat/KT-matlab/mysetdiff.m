function C = mysetdiff(A,B)
% MYSETDIFF Set difference of two sets of positive integers (much faster than built-in setdiff)
% C = mysetdiff(A,B)
% C = A \ B = { things in A that are not in B }

if isempty(A)
  ma = 0;
else
  ma = max(A);
end

if isempty(B)
  mb = 0;
else
  mb = max(B);
end

if ma==0 
  C = [];
elseif mb==0
  C = A;
else % both non-empty
  %bits = sparse(1, max(ma,mb));
  bits = zeros(1, max(ma,mb));
  bits(A) = 1;
  bits(B) = 0;
  C = A(logical(bits(A)));
end
