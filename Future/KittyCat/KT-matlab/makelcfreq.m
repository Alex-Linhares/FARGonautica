function lc = makelcfreq(R, zs);

nclass= length(unique(zs));
lc=zeros(nclass);
[r, c]=ind2sub(size(R), find(R));

for i = 1:length(r)
  lc(zs(r(i)), zs(c(i))) = lc(zs(r(i)), zs(c(i))) + R(r(i), c(i));
end

