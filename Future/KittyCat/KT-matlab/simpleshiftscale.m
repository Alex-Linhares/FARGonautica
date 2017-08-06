function data = simpleshiftscale(data, ps)

% shift and scale data so that mean is zero, and largest covariance is 1 

origdata = data;

globmean = sum(sum(data(~isinf(data))))/sum(sum(~isinf(data)));
data = data - globmean;

if ps.missingdata
  for i = 1:ps.runps.chunknum
    datac{i} = data(ps.runps.objind{i}, ps.runps.featind{i});
  end 
else
  datac{1} = data;
end

for ch = 1:length(datac)
  dnew = datac{ch};
  fnum = size(dnew, 2);
  op1 = dnew*dnew'/fnum;
  maxs(ch) = max(op1(:));
  csize(ch) = size(dnew,2);
end


% set largest value in largest chunk to 1
[mc mcind]=max(csize);
m = maxs(mcind);
data = (origdata - globmean)/sqrt(m);

