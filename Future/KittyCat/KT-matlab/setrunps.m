function [nobjects, ps]=setrunps(data, dind, ps)

% initialize runps component of ps

if ~isfield(data, 'type')
  if size(data,2)==size(data,1)	&& ~ps.featforce % similarity data
    ps.runps.type = 'sim';  
    nobjects = size(data,1);
  else
    ps.runps.type = 'feat';  
    nobjects=size(data,1);
  end
else
    ps.runps.type = 'rel';  
    nobjects = data.nobj; 
    ps.speed = 5;	           % 54 unnecessary for relational data
    ps.init = 'none';	       % unnecessary to init search with tied 
			                   % branch lengths
end
ps.runps.nobjects = nobjects;


if strcmp(ps.runps.type, 'sim')	% similarity data
  % number of subjects in experiment
  ps.runps.dim = ps.simdim{dind};
end

