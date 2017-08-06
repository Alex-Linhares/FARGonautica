% Pull out results on partitionnoself to initialize searches for relational
% structures

masterfile    = 'resultsdemo.mat';
thisdata = [17:20];
ps = setps;
defaultps(ps);

relinitdir = ps.relinitdir;
currdir = pwd;

load(masterfile);
for i = thisdata
  cd(irmdatadir);
  modellike(modellike==0) = -inf;
  [m,mind]=max(modellike(9,i,:));
  if isinf(m)
    error('no results for partitionnoself');
  end
  g = structure{9,i,mind};
  bestz = g.z;
  fid = fopen([ps.data{i}, '_bestz'], 'w');
  fprintf(fid, '%d ', bestz);
  fclose(fid);
  cd(currdir)
end
