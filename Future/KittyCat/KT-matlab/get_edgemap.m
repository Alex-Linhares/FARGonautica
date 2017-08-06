function emap = get_edgemap(adj, varargin)

% Create an edge map (EMAP) which associates each edge in ADJ with a
% number

args = varargin;
symflag =0;
for i=1:2:length(args)
  switch args{i}
   case 'sym', symflag=args{i+1};
  end
end


emap = zeros(size(adj));
if symflag
  es = find(tril(adj,-1));
  emap(es)=1:length(es);
  emap = emap+emap';
else
  es = find(adj);
  emap(es)=1:length(es);
end

