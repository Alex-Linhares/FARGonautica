function data = makesimlike(data, ps)

% shift and scale DATA so that the maximum value in covariance is 1, and the
% smallest value is 0 (not always possible, in which case we settle for the
% closest value to 0 we can get). 

origdata = data;
if ps.missingdata
  for i = 1:ps.runps.chunknum
    datac{i} = data(ps.runps.objind{i}, ps.runps.featind{i});
  end 
else
  datac{1} = data;
end

lb = -inf; ub = inf;
count = 0;
for ch = 1:length(datac)
  data = datac{ch};
  nobjects = size(data,1);
  fnum = size(data,2);
  outerproduct = data*data';
  a = fnum;
  for i = 1:nobjects
    for j = 1:nobjects
      count = count+1;
      c = outerproduct(i,j);   
      b = -(sum(data(i,:)) +sum(data(j,:))); 
      delta = sqrt(b^2 - 4*a*c);
      if ~isreal(delta) 
        kmins(count) = -b/(2*a);
        fmins(count) = (4*a*c - b^2)/(4*a);
        continue;  
      end
      % values  of k between newlb and newub will make cov(i,j) < 0
      newlb  = (-b + delta)/(2*a);
      newub  = (-b - delta)/(2*a);
      if newlb > lb
        lb = newlb;
      end
      if newub < ub
        ub = newub;
      end
    end
  end
end

fmins(fmins == 0) = inf;
if ub == inf
  % can't achieve a zero -- take the smallest value we can get
  [m mind] = min(fmins);
  ub = kmins(mind);
end

% setting k = ub or lb will make some cov(i,j) value 0 and all the others
% positive.  we choose ub

if ps.missingdata
  for i = 1:ps.runps.chunknum
    datac{i} = datac{i} - ub;
  end 
else
  datac{1} = origdata - ub;
end

for ch = 1:length(datac)
  dnew1 = datac{ch};
  fnum = size(dnew1, 2);
  op1 = dnew1*dnew1'/fnum;
  maxs(ch) = max(op1(:));
  csize(ch) = size(dnew1,2);
end

%set largest value anywhere to 1
%m = max(maxs);

% set largest value in largest chunk to 1
[mc mcind]=max(csize);
m = maxs(mcind);

data = (origdata - ub)/sqrt(m);

