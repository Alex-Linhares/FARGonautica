function [data  ps]= scaledata(data, ps)

% scale the data according to several strategies

switch ps.runps.type
  case {'rel'}
    return % don't scale relational data
end

if sum(sum(isinf(data))) > 0
  ps.missingdata = 1;
else
  ps.missingdata = 0;
end

nobjects = size(data,1);
origdata = data;
dmean  = mean(data(~isinf(data)));
stdev = std(data(~isinf(data)));
ps = makechunks(data,ps);

if strcmp(ps.runps.type, 'sim')	% similarity data
  switch ps.simtransform
    case{'center'}
      % Centering matrix
      Z=eye(nobjects)-ones(nobjects)*(1./nobjects);
      data = Z*data*Z;
    end
elseif strcmp(ps.runps.type, 'feat')	% feature data
  switch ps.datatransform
    case{'simpleshiftscale'}	    % make data zero mean, max covar 1
      data = simpleshiftscale(data, ps);
    case{'makesimlike'}		    % make data look like similarity matrix 
				    % (max covar 1, min covar close to 0)
      data = makesimlike(data, ps);
  end
  if ~ps.missingdata
    ps.runps.SS = data*data'/size(data,2);
    ps.runps.chunkcount = size(data,1);
  end
end

ps = makechunks(data,ps);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function ps = makechunks(data, ps);

if ps.missingdata
  % create ps.runps.dataind
  datamask = ~isinf(data);
  [b i j]=unique(datamask', 'rows');
  ps.runps.chunknum = size(b,1);
  for chunk = 1:size(b,1)
    % features and objects included in each chunk
    ps.runps.featind{chunk} = find(j==chunk);
    ps.runps.objind{chunk} = find(b(chunk,:));
    ps.runps.chunksize{chunk} = length(ps.runps.objind{chunk});
    dtemp = data(ps.runps.objind{chunk}, ps.runps.featind{chunk});
    ps.runps.chunkSS{chunk} = dtemp * dtemp' / length(ps.runps.featind{chunk});
  end
end




