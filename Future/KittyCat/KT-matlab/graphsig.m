function sig = graphsig(adj, objcnt, varargin)

% call nauty to compute signature for an adjacency matrix

args=varargin;
label = 1;
for i=1:2:length(args)
  switch args{i}
   case 'label', label=args{i+1};
  end
end


n = size(adj,1);
gname = '_GRAPHTMP';
gsigout = '_GRAPHSIG';
fid = fopen(gname, 'w');
fprintf(fid, 'n=%g\n d\n g\n', n);
allcs = '';
for i = 1:n
  par = sprintf('%g: ', i-1);
  child = sprintf('%g, ', find(adj(i,:))-1);
  allcs = [allcs, sprintf('%s %s ;\n', par, child(1:end-2))];
end
allcs = [allcs(1:end-2), '.'];
fprintf(fid, '%s', allcs);

if label
  partstr = 'f = [';
  for i = 1:objcnt
    partstr = [partstr, num2str(i-1), '|'];
  end
  partstr = [partstr(1:end-1), '];', sprintf('\n')];
else
  partstr = [sprintf('\n'), '-f', sprintf('\n')];
end

fprintf(fid, '%s', partstr);

% if we want labelled graph
%lines = {'c', '-a', '-m', 'x', 'b'};
% get identifier for graph (two hexadecimal numbers)

% orig: ckemp
lines = {'c', '-m', 'x', 'z'};
fprintf(fid, '%s\n%s\n%s\n%s\n%s\n', lines{:});
fclose(fid);

[s, sig] = system(['dreadnaut < ', gname, '| tail -1 ']);
[s w] = system(['rm ', gname] );

% Junkyard: extract stuff from dreadnaut output

%[s, sig] = system(['dreadnaut < ', gname, '| perl -i -ne "if (/:/) {print;}" ']);
%
%[s, sig] = system(['dreadnaut < ', gname, '> ' gsigout]); 
%[s, perm] = system(['cat ', gsigout, ' | perl -i -ne "unless (/;/) {print;}" ']);
%[s, sig ] = system(['cat ', gsigout, ' | perl -i -ne "if (/:/) {print;}" ']);
%
%perm = str2num(perm);
%[s,sind] = sort(perm);
%sig = [num2str(perm(1:objcnt)), sprintf('\n'), sig];

%[s, sig ] = system(['cat ', gsigout, ' | perl -i -ne "if (/:/) {print;}" ']);
%system(['rm ', gname, ' ', gsigout]);


