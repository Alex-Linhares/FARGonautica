function g = add_element(g, compind, c, element, ps);

% add entity ELEMENT to cluster C1 of component COMPIND

if compind < 0
  for j = 1:g.ncomp
    g.components{j}.z(element) = g.compinds(c,j);
  end
else 
  g.components{compind}.z(element) = c;
end

g.z(element) = 1;

g.objcount = g.objcount+1;

% XXX: inefficient
g = combinegraphs(g, ps, 'zonly', 1);

