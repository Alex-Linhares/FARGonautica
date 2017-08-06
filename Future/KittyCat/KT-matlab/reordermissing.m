function [graph Wvec]=reordermissing(graph, Wvec, obsind, missind, ps)

% shuffle objects OBSIND (entities with missing values) to the end of the
% list.

nobj = graph.objcount;
nlat = size(graph.adjcluster,1);
newind = [obsind, missind];
[s sind]=sort(newind); [t tind] = sort(sind);
leftoutind=find(~sparse(1,newind,1,1,length(graph.z)));

graph.z= [graph.z(newind), graph.z(leftoutind)];
for c = 1:graph.ncomp
  graph.components{c}.z = [graph.components{c}.z(newind), ...
			   graph.components{c}.z(leftoutind)];
end
graph.leaflengths = [graph.leaflengths(newind), graph.leaflengths(leftoutind)];


% first position is for sigma
if ~ps.fixedexternal
  Wvec(2:nobj+1) = Wvec(tind+1);
end

% adjust leaflengths, W, Wsym, adjsym etc
graph.W(1:nobj, nobj+(1:nlat)) = graph.W(tind, nobj+(1:nlat));
graph.W(nobj+(1:nlat), 1:nobj) = graph.W(nobj+(1:nlat), tind);

graph.Wsym(1:nobj, nobj+(1:nlat)) = graph.Wsym(tind, nobj+(1:nlat));
graph.Wsym(nobj+(1:nlat), 1:nobj) = graph.Wsym(nobj+(1:nlat), tind);

graph.adj(1:nobj, nobj+(1:nlat)) = graph.adj(tind, nobj+(1:nlat));
graph.adj(nobj+(1:nlat), 1:nobj) = graph.adj(nobj+(1:nlat), tind);

graph.adjsym(1:nobj, nobj+(1:nlat)) = graph.adjsym(tind, nobj+(1:nlat));
graph.adjsym(nobj+(1:nlat), 1:nobj) = graph.adjsym(nobj+(1:nlat), tind);

