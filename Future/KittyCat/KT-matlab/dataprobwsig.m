function [ll dWvec dWvecprior] = dataprobwsig(Wvec, d, graph, ps)

% Compute probability P(Wvec|D), where Wvec specifies the edge lengths for
% graph GRAPH and D is a feature or similarity data set.

% LL = -log(p(d|Wvec, graph)p(Wvec));
% Wvec: first entry is sigma, 
%	next chunk is a set of leaf weights
%	then follows a chunk of weights for each component

% d may contain missing entries, but we'll cycle through in chunks and create
% new data matrices without missing entries 

% Wvec contains log weights 
logWvec = Wvec;
Wvec = exp(Wvec);

% wbeta: parameter for exponential prior on branch lengths
% sigbeta: parameter for exponential prior on inverse sigma 
wbeta = ps.lbeta; sigbeta = ps.sigbeta;

pk = 2; % 2 for exponential: 3 for gamma shape 2

if ps.missingdata
  ps.missingdata = 0;
  wpriors = -(weightprior(Wvec(2:end), wbeta)+weightprior(Wvec(1), sigbeta));
  ll = wpriors; dWvec = 0;
  theseobjs = graph.z>=0;

  for c = 1:ps.runps.chunknum
    obsind = find(theseobjs & sparse(1,ps.runps.objind{c},1,1,length(graph.z)));
    missind =find(theseobjs &~sparse(1,ps.runps.objind{c},1,1,length(graph.z)));
    currobs = [obsind, missind];
    [s sind] = sort(currobs); [t tind] = sort(sind);

    % shuffle the missing objects for this chunk to the end of the list.
    % We can then ignore them when computing probability of the data for
    % this chunk.
    [newgraph newWvec]= reordermissing(graph, Wvec, obsind, missind, ps);
    newdata = d(tind(1:length(obsind)),ps.runps.featind{c});
    ps.runps.SS = ps.runps.chunkSS{c}; 
    ps.runps.chunkcount = ps.runps.chunksize{c}; 
    if nargout > 1
      %checkgrad('dataprobwsig', newWvec, 0.00001, newdata, newgraph, ps)
      [llc dWvecc dWveccprior]=dataprobwsig(log(newWvec),newdata, newgraph, ps);
      llc = llc - wpriors;
      if c > 1 % include dWveccprior for first chunk
        dWvecc = dWvecc - dWveccprior;
      end
      if ~ps.fixedexternal
        dWvecc(2:length(sind)+1) = dWvecc(sind+1);
      end
      dWvec = dWvec + dWvecc; 
    else
      llc=dataprobwsig(log(newWvec), newdata, newgraph, ps)-wpriors;
    end
    ll = ll + llc;
  end
  return
end

origgraph = graph;

if strcmp(ps.runps.type, 'sim') 
  dim =  ps.runps.dim;
  SS = d;
elseif size(d,1) == ps.runps.chunkcount  
  dim = size(d,2);
  SS = ps.runps.SS;
else
  dim = size(d,2);
  SS = 1/dim*d*d';
end

nobs = size(d,1); nmiss = graph.objcount - nobs; nobj = graph.objcount;
nlat = size(graph.adj,1)-graph.objcount;

sigma = Wvec(1);

[graph ps] = combineWs(graph, Wvec(2:end), ps);

J  = inv_covariance(graph.Wsym, nobj, sigma, ps);
G  = inv_posdef(J);
ll = gplike(d, G, dim, ps)+ weightprior(Wvec(2:end), wbeta)+ ...
     weightprior(sigma, sigbeta);

% since the function is - log posterior prob
ll = -ll; 

if 0
disp(sprintf('likelihood: %g prior %g', gplike(d,G, dim, ps), ...
					weightprior(Wvec, wbeta)));
end

if nargout > 1 
  if nmiss == 0
    % partition graph because values observed only at objects
    [A B C D] = matrixpartition(J, nobj);
    [wA wB wC wD] = matrixpartition(graph.Wsym, nobj);
    wAnoz = wA; wAnoz(wAnoz==0)=1; wBnoz = wB; 
    wBnoz(wBnoz==0)=1; 
    wDnoz = wD; wDnoz(wDnoz==0)=1;

    % to deal with mysterious Matlab bug
    Btr = B'; wBnoztr = wBnoz'; wDnoztr = wDnoz';
    
    Dinv = inv_posdef(D);
    DinvB = Dinv*Btr; DinvBtr = DinvB';
    Xinv = (A-DinvBtr*Btr);
    X = inv_posdef(Xinv);
    XXX = X*Xinv*X;
    YY = SS;
    U = dim*(YY-XXX);

    oneobjtr = ones(1,nobj); onelat = ones(nlat,1);
    % coefficients for A, B, D
    c1 = U;
    c2 = -2*DinvB*U;
    c3 = DinvB*U*DinvBtr;
    diagc1tr = diag(c1)';

    % -0.5 out the front is constant from log likelihood
    dEdlWb =  ( -0.5*(onelat*diagc1tr - c2 + diag(c3)*oneobjtr).*wBnoztr)' ;
    dEdlWbprior = transpose(... 
	(-pk./wBnoztr + 1./(wbeta*wBnoztr.^2) + 1./wBnoztr).*wBnoztr ); 

    dEdlWddata =  -0.5*( (onelat*diag(c3)' - c3).*wDnoztr)' ;
    
    % keep these separate because several of the edges in D are really the same,
    % and we only want to put the prior on one of them.
    
    dEdlWdprior= transpose( ...
	(-pk./wDnoztr + 1./(wbeta*wDnoztr.^2) + 1./wDnoztr).*wDnoztr );
    
    dEdWa = zeros(nobj);
    % go through Wa, Wb, Wd and pull out component weights
    [dWvec, dWvecprior] = extract_weights(dEdWa, dEdlWb, dEdlWbprior, ...
			    dEdlWddata, dEdlWdprior, graph, ps);
    
    % 1 not 2 because of the constant out the front!
    
    if ps.zglreg
      dEdsig = 1/sigma^3*(trace(c1)+trace(c3)) * sigma;
    else    % As of August 19: see below
     dEdsig = 1/sigma^3*(trace(c1)) * sigma;
    end

    % with exponential prior on 1/sigma
    dEdsigprior = (- pk/sigma + 1/(sigbeta*sigma^2) + 1/sigma)*sigma ;
    dWvec = [dEdsig+dEdsigprior; dWvec];
    dWvecprior = [dEdsigprior; dWvecprior];
    
    % since the function is - log posterior prob
    dWvec = -dWvec; dWvecprior = -dWvecprior;
  else % compute partial derivatives
    % partition graph because values observed only at objects
    [A1 A2 B1 B2 D] = triplepartition(J, nobs, nmiss);
    [wA1 wA2 wB1 wB2 wD] = triplepartition(graph.Wsym, nobs, nmiss);

    wB1noz = wB1; wB1noz(wB1noz==0)=1;
    wB2noz = wB2; wB2noz(wB2noz==0)=1;
    wDnoz = wD; wDnoz(wDnoz==0)=1;

    % XXX can compute matrix inverses more efficiently (matrices diagonal)
    A1inv = inv_posdef(A1); A2inv = inv_posdef(A2);
    A1invB1 = A1inv*B1; A2invB2 = A2inv*B2;

    % squash mysterious bug
    B2tr = B2'; B1tr = B1'; A1invB1tr = A1invB1'; A2invB2tr = A2invB2';
    wB1noztr = wB1noz'; wB2noztr = wB2noz'; wDnoztr = wDnoz';

    Y = D - B2tr*A2inv*B2 - B1tr*A1inv*B1;
    Yinv = inv_posdef(Y);
    YinvB1 = Yinv*B1tr; YinvB1tr = YinvB1';
    X = A1inv+A1invB1*Yinv*A1invB1tr;
    Xinv = inv_posdef(X);
    XXX = X*Xinv*X;
    YY = SS;				    % NB: YY and Y are unrelated
    U = dim*(YY-XXX);
    U = dim*(Xinv - Xinv*YY*Xinv);
    A1invUA1inv = A1inv*U*A1inv; 
    K = YinvB1*A1invUA1inv*YinvB1tr;

    oneobstr = ones(1,nobs); onemisstr= ones(1,nmiss); onelat = ones(nlat,1); 

    % coefficients for A1, A2, B1, B2, D
    c1 = -A1invUA1inv - 2*A1invUA1inv*YinvB1tr*A1invB1tr - A1invB1*K*A1invB1tr;
    c2 = -A2invB2*K*A2invB2tr;
    c3 = 2*YinvB1*A1invUA1inv + 2*K*A1invB1tr;
    c4 = 2*K*A2invB2tr;
    c5 = -K;

    diagc1tr = diag(c1)'; diagc2tr = diag(c2)'; diagc5tr = diag(c5)';

    % -0.5 out the front is constant from log likelihood
    dEdlWB1 = ( -0.5 * ( onelat*diagc1tr - c3 + diag(c5)*oneobstr).*wB1noztr)';
    dEdlWB1prior = transpose(...
	(-pk./wB1noztr + 1./(wbeta*wB1noztr.^2) + 1./wB1noztr).*wB1noztr) ;

    dEdlWB2 = ( -0.5 * ( onelat*diagc2tr - c4 + diag(c5)*onemisstr).*wB2noztr)'; 
    dEdlWB2prior = transpose(...
	(-pk./wB2noztr + 1./(wbeta*wB2noztr.^2) +1./wB2noztr).*wB2noztr ) ;

    dEdlWDdata =  -0.5*( (onelat*diagc5tr -c5).*wDnoztr )';
    
    % keep these separate because several of the edges in D are really the same,
    % and we only want to put the prior on one of them (this applies to cross
    % products) 
    dEdlWDprior= transpose(...
		(-pk./wDnoztr + 1./(wbeta*wDnoztr.^2) + 1./wDnoztr).*wDnoztr );
    
    % go through Wa, WB1, WB2, WDand pull out component weights
    dEdWa = zeros(nobj);
    [dWvec dWvecprior] = extract_weights(dEdWa, [dEdlWB1; dEdlWB2], ...
				   [dEdlWB1prior; dEdlWB2prior], ...
				   dEdlWDdata, dEdlWDprior, graph, ps);
    
    % with exponential prior on 1/sigma
					    % because we're working with log
					    % sigma

    if ps.zglreg
      dEdsig = 1/sigma^3*(trace(c1)+trace(c2)+trace(c5))*sigma; 
    else % XXX: as of August 19 2005
      dEdsig = 1/sigma^3*(trace(c1)+trace(c2))*sigma; 
    end

							      % likewise
    dEdsigprior = (- pk/sigma + 1/(sigbeta*sigma^2) + 1/sigma)*sigma;

    dWvec = [dEdsig+dEdsigprior; dWvec];
    dWvecprior = [dEdsigprior; dWvecprior];
    
    % since the function is - log posterior prob
    dWvec = -dWvec; dWvecprior = -dWvecprior;
  end
  if sum(isnan(dWvec))
    disp('NaNs in dataprobwsig.m');
    keyboard
  end
end


