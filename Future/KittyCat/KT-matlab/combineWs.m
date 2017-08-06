function [graph ps] = combineWs(graph, Wvec, ps) 

% fill in W matrices given the weights in Wvec (graph topology changes nowhere
%						 -- only weights change)

leafinds = sub2ind(size(graph.W), graph.objcount+graph.z(graph.z>=0),...
		   1:graph.objcount);

nobj = graph.objcount;
W = 0*graph.adjcluster;

extlen = 0; intlen = 0;
if ps.fixedall
  extlen = Wvec(1); intlen = Wvec(1); 
elseif ps.fixedexternal && ps.fixedinternal
  extlen = Wvec(1); 
  if sum(sum(graph.adjcluster))
    intlen = Wvec(2);
  else
    intlen = 1;
  end
elseif ps.fixedexternal
  extlen = Wvec(1); Wvec = Wvec(2:end);
elseif ps.fixedinternal
  leafweights = Wvec(1:nobj); 
  if sum(sum(graph.adjcluster)) 
    intlen = Wvec(nobj+1);
  else
    intlen = 1;
  end
else
  leafweights= Wvec(1:nobj);
  Wvec = Wvec(nobj+1:end);
end 
graph.extlen= extlen; graph.intlen= intlen;

% set up internal lengths
if ps.fixedinternal == 0  && ~ps.fixedall
  if ps.prodtied
    for i=1:length(graph.components)
      Ws = Wvec(1:graph.components{i}.edgecountsym);  
      Wvec = Wvec(graph.components{i}.edgecountsym+1:end);
      emap = graph.components{i}.edgemapsym;
      edgeinds = emap(find(emap));
      W(find(emap))= Ws(edgeinds);
      localW = tril(graph.components{i}.adjsym, -1);
      localW(logical(localW)) = Ws;
      graph.components{i}.Wsym = localW+localW';
      graph.components{i}.W = graph.components{i}.adj .* ...
			      graph.components{i}.Wsym;
    end
  else
    W = tril(graph.adjclustersym, -1);
    W(find(W)) = Wvec;
    W = W+W';
    if graph.ncomp == 1 % shouldn't need this once we deal with untied products
		        % properly. But it's essential for now that the
			% component Ws be accurate.
      graph.components{1}.Wsym = W;
      graph.components{1}.W = W.*graph.components{1}.adj;
    end
  end
else
  W = intlen*graph.adjclustersym;
end
graph.Wclustersym = W;
graph.Wcluster = graph.adjcluster.*W;

% set up external lengths
if ps.fixedexternal || ps.fixedall
  leafweights = extlen*ones(1, length(leafinds));
end

fullW=zeros(size(W,1)+nobj);
fullW(leafinds) = leafweights;
fullWtr = fullW';
fullW(graph.adj')= fullWtr(graph.adj');
fullW(nobj+1:end, nobj+1:end)=graph.Wclustersym;
%graph.Wsym = fullW + fullW - graph.adj.*(fullW);
graph.Wsym = fullW;
graph.W = repmat(0, size(fullW));
graph.W(graph.adj) = fullW(graph.adj);
graph.leaflengths(graph.z>0) = leafweights;
