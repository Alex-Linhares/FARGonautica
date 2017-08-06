function [graph c1 c2] = split_node(graph, compind, c, pind, part1, part2, ps)

% split node C in component CIND using production PIND and put PART1 and PART2
%   in the two children

structname = graph.components{compind}.type;
origgraph = graph;

if pind == 1
  switch structname
    case{'dirchain', 'order', 'dirchainnoself', 'ordernoself',...
         'undirchain', 'undirchainnoself'}
      structname = 'chain';
    case{'dirring', 'dirringnoself', 'undirring', 'undirringnoself'}
      structname = 'ring';
    case{'dirhierarchy', 'domtree', 'dirhierarchynoself', 'undirhierarchy',...
        'undirhierarchynoself', 'dirdomtreenoself', 'undirdomtree', ...
        'undirdomtreenoself'}
      structname = 'hierarchy';
    case{'partitionnoself'}
      structname = 'partition';
    case{'connectednoself'}
      structname = 'connected';

  end
end

if pind == 2
  switch structname
    case{'hierarchy', 'dirhierarchy', 'domtree', 'dirhierarchynoself',...
	 'ordernoself', 'undirhierarchy', 'undirhierarchynoself',...
	 'dirdomtreenoself', 'undirdomtree', 'undirdomtreenoself'}
      structname = 'chain';
      if graph.components{compind}.nodecount == 1 % all productions the same
						  % for a one node graph 
        graph = -inf; c1 = -inf;  c2 = -inf;  return;
      end
      if (sum(graph.components{compind}.adj(:,c)) == 0 && ...
          sum(graph.components{compind}.adj(c,:)) >= 2)
        structname = 'rootchain';
      end
    case{'tree'}
      structname = 'treever2';
      if graph.components{compind}.nodecount <= 3 % all productions the same
        graph = -inf; c1 = -inf;  c2 = -inf;  return;
      end
  end
end

if pind == 3
  switch structname
    case{'hierarchy', 'dirhierarchy', 'domtree', 'dirhierarchynoself', ...
    'undirhierarchy', 'undirhierarchynoself', 'dirdomtreenoself', ...
    'undirdomtree', 'undirdomtreenoself' } 
      structname = 'domtreeflat';
      if sum(graph.components{compind}.adj(:,c)) == 0 % c has no parents
        graph = -inf; c1 = -inf; c2 = -inf; return;
      end
  end
end

disp(structname)

ntot    = graph.components{compind}.nodecount;
origadj = graph.components{compind}.adj;
origW   = graph.components{compind}.W;
% make markers for original edges
origind = find(origadj);
nold    = length(origind);
origadj(origind)=1:nold;
newinternal = [];

switch structname
    case {'partition', 'chain', 'ring', 'hierarchy', 'domtreeflat', ...
	  'connected', 'rootchain'}
      newadj   = zeros(ntot+1);
      minusnew       = setdiff(1:ntot+1, c+1);
      newadj(minusnew, minusnew) = origadj;
      % give the new node all the connections of the previous nodes
      newadj(c+1,:)=newadj(c,:); 
      newadj(:,c+1)=newadj(:,c); 

      % c, c+1 are the new clusters
      c1 = c; c2 = c+1;
      newnodes = [c1 c2];
      oldps=find(newadj(:,c)');
      oldchild=find(newadj(c,:));
      switch structname
        case {'partition'}
        case {'connected'}
	  newadj(c, c+1)=inf; 
        case {'chain', 'ring'}
	  newadj(c, c+1)=inf; newadj(c+1, c)=0;
	  newadj(oldps, c+1)=0; newadj(c, oldchild)=0;
	  if strcmp(structname, 'ring') 
	      if isempty(oldps) && isempty(oldchild) 
	        newadj(c+1, c)=inf;
	      end
	  end
	case {'hierarchy'}
	  newadj(c, c+1)=inf; newadj(c+1, c)=0; 
	  newadj(oldps, c+1)=0; newadj(c+1, oldchild) = 0;
	case {'domtreeflat'}
	  newadj(c, c+1)=0; newadj(c+1, c)=0; 
	  newadj(c+1, oldchild) = 0;
	case {'rootchain'}
	  newadj(c, c+1)=inf; newadj(c+1, c)=0; 
	  newadj(c+1, oldchild(1)) = 0;
	  newadj(c, oldchild(2:end)) = 0;
      end
    case {'tree'}
      newadj   = zeros(ntot+2);
      minusnew       = setdiff(1:ntot+2, [c+1, c+2]);
      newadj(minusnew, minusnew) = origadj;

      % c+1, c+2 new leaf nodes
      c1 = c+1; c2 = c+2;
      newnodes = [c c1 c2];
      newadj(c+1, c)=0; newadj(c, c+1)=inf; 
      newadj(c+2, c)=0; newadj(c, c+2)=inf; 
      newinternal= c; 
    case {'treever2'}
      newadj   = zeros(ntot+2);
      cpar = find(origadj(:,c)); 
      csibs = find(origadj(cpar,:)); csib = csibs(csibs ~=c);

      origadj(cpar, csib) = 0;
      minusnew       = setdiff(1:ntot+2, [c+1, c+2]);
      newadj(minusnew, minusnew) = origadj;

      % c+1, c+2 new leaf nodes
      c1 = c+1; c2 = c+2;
      newnodes = [c c1 c2];
      newadj(c, c+2) = inf; newadj(c, minusnew(csib)) = inf; 
      newadj(minusnew(cpar), c+1) = inf;
      newinternal= c; 
    otherwise
      error('Unknown structure');
end

newind=find(newadj);
[s sind]=sort(newadj(newind));
newW = newadj;  

% replace markers with weights 
if length(origind) > 0
  newW(isinf(newW))=median(origW(origind));
  newW(newind(sind(1:nold)))=origW(origind);
else
  newW(isinf(newW))=1;
end

newadj(newadj>0)=1;

% map(i) tells us what node i in old graph is now labelled 
% imap(j) tells us what node j in new graph corresponds to
map = zeros(1,size(newadj,1)); imap = map;
brandnew = setdiff(newnodes, c);
oldind = setdiff(1:size(newadj,1), brandnew);
map(1:length(oldind))=oldind;
imap(oldind) = 1:length(oldind); imap(brandnew) = c;

oldz = graph.components{compind}.z;
newz = map(oldz);

newillegal= zeros(1,size(newadj,1));
newillegal= map(graph.components{compind}.illegal);
graph.components{compind}.illegal=[newillegal, newinternal];

graph.components{compind}.adj = newadj;
graph.components{compind}.W   = newW;
doubleW = 0*newW; doubleW(newadj & newadj')= newW(newadj & newadj');
graph.components{compind}.Wsym   = newW+newW'-doubleW;
graph.components{compind}.adjsym   = newadj+newadj' - (newadj & newadj');

graph.components{compind}.nodecount = size(newadj,1);
graph.components{compind}.edgecount = sum(sum(newadj)); 

graph.components{compind}.edgecountsym =...
      sum(sum(graph.components{compind}.adjsym))/2; 

% new cluster nodes appear in order at the end of newnodes
newz(part1) = newnodes(end-1);
newz(part2) = newnodes(end);
graph.components{compind}.z = newz;

graph = combinegraphs(graph, ps, 'origgraph', origgraph, ...
		      'compind', compind, 'imap', imap);

%disp(origgraph.Wcluster); disp('---'); disp(graph.Wcluster); 

