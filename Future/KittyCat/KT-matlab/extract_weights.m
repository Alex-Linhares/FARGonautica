function [dWvec dWvecprior]= extract_weights(dEdlWa, dEdlWb, dEdlWbprior, ...
					     dEdlWddata, dEdlWdprior, graph, ps)

% Go through gradient matrices (dEdlWa, etc) and pull out dWvec (includes
% contributions from data and prior) and dWvecprior (contributions from prior only)

% enforce symmetry of D
nclust = size(graph.adjcluster,1);
dEdlWddatasym = dEdlWddata + dEdlWddata' - eye(nclust).*dEdlWddata;
dEdlWdpriorsym = dEdlWdprior + dEdlWdprior' - eye(nclust).*dEdlWdprior;

[mA mB mC mD] = matrixpartition(graph.adjsym, graph.objcount);

% Bvec should be leafweights in order
dEdlWc = dEdlWb';
dEdlWcprior = dEdlWbprior';
if size(dEdlWc,1) == 1
  dEdlWc = dEdlWc';
  dEdlWcprior = dEdlWcprior';
end
Bvec = dEdlWc(find(mC))+dEdlWcprior(find(mC));
Bvecprior = dEdlWcprior(find(mC));
if size(mD, 1) == 1
  mD = [];
else
  mD = logical(tril(mD, -1));
end
mB = logical(mB);

			 % we need to consider forward and backward edges even
			 % though the matrix is symmetric
extcount = sum(sum(mB)); intcount = 2*sum(sum(mD));
dext = dEdlWb(mB); dextprior = dEdlWbprior(mB); 
dint = dEdlWddatasym(mD); dintprior = dEdlWdpriorsym(mD); 

if ps.fixedall
  dWvecprior = (sum(dextprior) + sum(dintprior)) / (extcount + intcount);
  dWvec = dWvecprior + sum(dext) + sum(dint); 
  return
elseif ps.fixedexternal && ps.fixedinternal
  if intcount == 0
    dWvecprior = [sum(dextprior)/extcount];
    dWvec = dWvecprior + sum(dext);
  else
    dWvecprior = [sum(dextprior)/extcount; sum(dintprior)/intcount];
    dWvec = dWvecprior + [sum(dext); sum(dint)];
  end
  return
elseif ps.fixedinternal
  if intcount == 0
    dWvecprior = Bvecprior;  
    dWvec = dWvecprior + dEdlWc(find(mC));   
  else
    dWvecprior = [Bvecprior; sum(dintprior)/intcount];  
    dWvec = dWvecprior + [dEdlWc(find(mC)); sum(dint)];   
  end
  return
end

Dvec = []; Dvecprior = [];
if ps.prodtied
  for i=1:graph.ncomp
    emap = graph.components{i}.edgemapsym;
    edgeinds = emap(find(emap));
    edgevalsdata = dEdlWddata(find(emap));
    edgevalsprior= dEdlWdprior(find(emap));
    [s sind]=sort(edgeinds);
    counts = sparse(1, edgeinds(sind), 1);
    sumvalsdata = cumsum(edgevalsdata(sind));
    sumvalsprior= cumsum(edgevalsprior(sind));
    sumsdata = diff([0; sumvalsdata(cumsum(counts))]);
    sumsprior= diff([0; sumvalsprior(cumsum(counts))]);
    sumsprior = sumsprior ./full(counts)';
    Dvec = [Dvec;sumsdata+sumsprior];
    Dvecprior = [Dvecprior;sumsprior];
  end
else
   Dvecprior = dintprior/2;  % D is symmetric, but only want the prior on one
			     %   edge
   Dvec = dint + Dvecprior;
end

if ps.fixedexternal
  dWvecprior = [sum(dextprior)/extcount; Dvecprior];
  dWvec = [sum(dextprior)/extcount + sum(dext);  Dvec];
else
  dWvecprior = [Bvecprior; Dvecprior];
  dWvec	     = [Bvec; Dvec];
end
