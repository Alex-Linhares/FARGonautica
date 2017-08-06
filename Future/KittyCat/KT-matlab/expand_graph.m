function [newadj, objcount]=expand_graph(adj, zs, type)

% ADJ: a graph over clusters.
% NEWADJ: a graph over objects and clusters. Created by hanging objects
% off ADJ according to the cluster assignments in ZS


if size(adj,1)~=size(adj,2)
    error('expand_graph: adj must be square');
  elseif size(adj,1) ~= length(zs)
   error('expand_graph: adj inconsistent with zs');
end
  
  nclust = size(adj,1);
  objcount = 0;
  for i=1:length(zs)
    objcount = objcount + length(zs{i});
  end

newadj = zeros(objcount+nclust);
newadj(objcount+1:objcount+nclust, objcount+1:objcount+nclust)=adj;

%% hang objects off nodes of adj according to zs (assignments) 
for i=1:length(zs)
  % now directed
  % newadj(zs{i},objcount+i)  = 1;
  newadj(objcount+i, zs{i}) = 1;
end
