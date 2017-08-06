function graph = combinegraphs(graph, ps, varargin)

% create direct product of components in graph. 
% 1) If given ZONLY flag, adjust class assignments only, or 
% 2) also adjust graph.W
%    a) if given ORIGGRAPH, COMPIND and IMAP and ps.prodtied == 0 and 
%	graph.ncomp > 1, copy across values from ORIGGRAPH.W (COMPIND and
%	IMAP tell us how to find them: IMAP maps nodes in component
%	COMPIND of GRAPH onto their equivalents in ORIGGRAPH.
%    b) else set graph.Wcluster to the graph product of the components.

% If some data are missing, assume they are marked with -1 in the original
% graph.z. 

args = varargin;
origgraph = []; compind = 0; imap = []; 
zonly = 0;
for i=1:2:length(args)
  switch args{i}
   case 'origgraph', origgraph = args{i+1};
   case 'compind',   compind   = args{i+1};
   case 'imap',	     imap      = args{i+1};
   case 'zonly',     zonly     = args{i+1};
  end
end

for i=1:graph.ncomp
  compsizes(i) = size(graph.components{i}.adj,1);
  graph.components{i}.nodemap=(1:size(graph.components{i}.adj,1))';
  graph.components{i}.edgemap=get_edgemap(graph.components{i}.adj);
  graph.components{i}.edgemapsym=get_edgemap(graph.components{i}.adjsym, ...
				 'sym', 1 );
end

graph.compsizes = compsizes;
W = graph.components{1}.W;
adj = graph.components{1}.adj;
z   = graph.components{1}.z;
illegal = graph.components{1}.illegal;

for i = 2:graph.ncomp
  na     = size(W,1);
  Wb     = graph.components{i}.W; adjb = graph.components{i}.adj;  
  nb = size(Wb,1); zb = graph.components{i}.z;
  Wnew   = kron(eye(nb), W); adjnew = kron(eye(nb), adj); 
  z = na*(zb-1)+z; 
  lastcol = repmat(1:nb, na, 1); 
  illegal = na*0:(nb-1)+illegal;

  for j=1:i-1
    graph.components{j}.nodemap=repmat(graph.components{j}.nodemap, nb,1);
    graph.components{j}.edgemap= kron(eye(nb), graph.components{j}.edgemap);
    graph.components{j}.edgemapsym= kron(eye(nb),...
       graph.components{j}.edgemapsym);
  end

  Wnewbunscram = kron(eye(na), Wb); adjnewbunscram = kron(eye(na), adjb);
  sind = reshape(1:na*nb, nb,na)'; sind = sind(:);

  adj = adjnew + adjnewbunscram(sind, sind);
  W = Wnew + Wnewbunscram(sind, sind);

  graph.components{i}.nodemap = repmat(graph.components{i}.nodemap, na,1);
  graph.components{i}.nodemap = graph.components{i}.nodemap(sind);
  newillegal = graph.components{i}.illegal;
  illind = zeros(1, nb*na); illind(nb*0:(na-1)+newillegal) = 1;
  illegal = union(newillegal, find(illind(sind)));

  graph.components{i}.edgemap = kron(eye(na), graph.components{i}.edgemap);
  graph.components{i}.edgemap = graph.components{i}.edgemap(sind,sind);
  graph.components{i}.edgemapsym = ...
			    kron(eye(na),graph.components{i}.edgemapsym);
  graph.components{i}.edgemapsym = graph.components{i}.edgemapsym(sind,sind);
end

graph.compinds= [];
graph.globinds = zeros(compsizes);
for i = 1:graph.ncomp
  % analysis: components of each node at highest level
  graph.compinds(:,i) = graph.components{i}.nodemap;
end
inds = subv2ind(compsizes, graph.compinds);
% synthesis: map component nodes to combined node
graph.globinds(inds) = 1:size(adj,1);

obsind = find(graph.z>=0);
graph.z(obsind) = z(obsind);
graph.illegal = illegal;
nobj = length(obsind);
if ~zonly
  graph.adjcluster = adj;
  graph.adjclustersym = double(graph.adjcluster | graph.adjcluster'); 
  graph.Wcluster = W;
  doubleWcluster = repmat(0, size(graph.Wcluster)); 
  doubleWcluster(graph.adjcluster & graph.adjcluster')= ...
    graph.Wcluster(graph.adjcluster & graph.adjcluster');
  graph.Wclustersym = graph.Wcluster+graph.Wcluster'- ...
		    doubleWcluster;
end

if ~zonly && ~ps.prodtied  && graph.ncomp > 1 &&  ~isempty(origgraph)
  % copy across values from origgraph.Wcluster
  [rind cind] = find(graph.adjcluster);
  oldcomps = graph.compinds;
  oldcomps(:,compind) = imap(oldcomps(:, compind));
  oldedgersind= subv2ind(origgraph.compsizes, oldcomps(rind,:));  
  oldedgers = origgraph.globinds(oldedgersind);
  oldedgecsind= subv2ind(origgraph.compsizes, oldcomps(cind,:));  
  oldedgecs = origgraph.globinds(oldedgecsind);
  oldW= origgraph.Wcluster;
  oldedgelengths = oldW(sub2ind(size(oldW),oldedgers, oldedgecs));
  if size(oldW,1)== 1 oldW = 1; end
  oldedgelengths(oldedgelengths == 0) = median(oldW(oldW > 0));
  newW = graph.adjcluster; 
  newW(find(adj)) = oldedgelengths;
  graph.Wcluster = newW;
end
  
fullW=zeros(size(W,1)+nobj);
leafinds = sub2ind(size(fullW), nobj+z(obsind), 1:nobj);
fullW(leafinds)=graph.leaflengths(obsind);
fullW(nobj+1:end, nobj+1:end)=graph.Wcluster;
graph.W = fullW;
graph.adj = fullW>0;
graph.adjsym = (graph.adj | graph.adj'); 
graph.Wsym = fullW;
Wtr = fullW';
graph.Wsym(graph.adj') = Wtr(graph.adj');

