function V = mat2vec(W, graph, ps)

% convert weighted graph W to a vector of weights V

if ps.fixedall
  V = log(graph.extlen);
  return;
elseif ps.fixedexternal && ps.fixedinternal && sum(sum(graph.adjcluster)) == 0
  V = [log(graph.extlen)];
  return
elseif ps.fixedexternal && ps.fixedinternal 
  V = log([graph.extlen; graph.intlen]);
  return;
end

[A B C D]     = matrixpartition(W, graph.objcount);
[mA mB mC mD] = matrixpartition(graph.adjsym, graph.objcount);
mA = tril(mA, -1);

Dvec = [];
if ps.prodtied
  for i=1:graph.ncomp
    W = graph.components{i}.Wsym;
    mW = logical(tril(graph.components{i}.adjsym, -1));
    if sum(sum(W))>0
      Dvec = [Dvec; W(mW)];
    end
  end
else
  if size(mD,1) == 1
    mD = [];
  else 
    mD = logical(tril(mD, -1));
  end
  Dvec = D(mD);
end

if size(C,1) == 1
  C = C';
end

if ps.fixedexternal
  V = [log(graph.extlen); Dvec];
elseif ps.fixedinternal && sum(sum(graph.adjcluster,1)) == 0
  V = [C(find(mC))];
elseif ps.fixedinternal 
  V = [C(find(mC)); log(graph.intlen)];
else
  V  = [A(find(mA)); C(find(mC)); Dvec];
end
