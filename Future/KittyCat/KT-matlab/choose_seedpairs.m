function seedpairs = choose_seedpairs(graph, compind,c,pind,  ps)

% Choose pairs to seed split of node C in component PIND.

% C:	   cluster node to split
% COMPIND: current graph component (-1 for top level split)
% PIND : production to try

% pc: paircount -- number of seedpairs to try for each object
pc = 1;

if compind < 0 % high level split
  partmembers = find(graph.z == c);
else 
  partmembers = find(graph.components{compind}.z == c);
end

if length(partmembers) <= 5 % consider all possible seedsplits
  seedpairs = nchoosek(partmembers,2);
else
  for i=1:length(partmembers)
    clustmembers = find(graph.z==graph.z(partmembers(i)));
    clustmembers = clustmembers(clustmembers ~= partmembers(i));
    clustmembers = clustmembers(randperm(length(clustmembers)));
    if isempty(clustmembers)
      clustmembers = partmembers(partmembers~=partmembers(i)); 
    end
    if length(clustmembers) < pc 
      clustmembers = repmat(clustmembers, 1, pc); 
    end
    pair2(i,:) = clustmembers(1:pc);
  end
  pair1 = repmat(partmembers', pc,1);
  seedpairs=[pair1 pair2(:)];
end

if compind < 0 % high level split
  seedpairs = [seedpairs; fliplr(seedpairs)];
else
  switch ps.runps.structname
    case {'partition'} % children are symmetric ...
    case {'tree'} 
      if pind == 2 & size(graph.components{compind}.adj,1)>1
        seedpairs = [seedpairs; fliplr(seedpairs)];
      end
    otherwise 
      if size(graph.components{compind}.adj,1)>1 % unless this is first split
        seedpairs = [seedpairs; fliplr(seedpairs)];
      end
  end
end

