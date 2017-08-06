% Charles Kemp, 2008
% Fit different structures to feature, similarity and relational data

addpath(pwd);
cd ('c:\KT');
addpath ('c:\KT');
addpath ('c:\KT\Graphviz\');

sprintf('blah blah blah');
masterfile    = 'danielsresults.mat';
sprintf(masterfile);
% masterfile must have .mat suffix, otherwise exist() won't find it
if ~strcmp(masterfile(end-3:end), ['.mat'])
  error('masterfile must have a .mat suffix'); 
end

ps = setps();
% set default values of all parameters
ps = defaultps(ps);

% change default parameters for this run
[s,w] = system('C:\Program Files (x86)\Graphviz2.20\bin\GVEdit.exe');   %C:\Program Files\Graphviz2.20\bin\GVEdit.exe  
if s ==0 % neato available
ps.showinferredgraph = 1; % show inferred graph 
ps.showpostclean     = 1; % show best graph (post cleaning) at each depth 
end


ps.reloutsideinit    ='overd';   % initialize relational structure with 
				 % one object per group

% Structures for this run. We'll fit the chain model, the ring model and
% the tree model. The indices correspond to form names in setpsexport()
%thisstruct = [2,4,6];	 
% Datasets for this run. Indices correspond to dataset names in  setpsexport() 
%thisdata = [1:3];			

% to run some additional structure/data pairs list them here.
extraspairs = [];
extradpairs = [];

% Use these structure and data indices for analyzing

%APENAS PARA CONSULTA RAPIDA...
% ps.structures = {'partition', 'chain', 'order', 'ring', 'hierarchy',...
% 			 'tree', 'grid', 'cylinder',...
%     'partitionnoself',...
%     'dirchain', 'dirchainnoself', 'undirchain', 'undirchainnoself',...
%     'ordernoself', 'connected', 'connectednoself',...  
%     'dirring', 'dirringnoself', 'undirring', 'undirringnoself',...
%     'dirhierarchy', 'dirhierarchynoself', 'undirhierarchy', 'undirhierarchynoself',...
%    };



% a) synthetic data described in Kemp (2008)
%   thisstruct = [1,2,4,6,7];	 
%   thisdata = [7:11];			

% b) real world feature and similarity data in Kemp (2008)
   %thisstruct = [9,11,13,14,16,18,20,22,24];	 
    thisstruct = [24];	 
   thisdata = [15];			

% c) real world relational data in Kemp (2008)
%   thisstruct = [1,9,10:13, 3,14:16,17:20, 21:24]  
%   thisdata = [17:20];			

% *** REPMAT REPLICA UMA MATRIZ ***
% *** B = repmat(A,m,n)         ***
% creates a large matrix B
% consisting of an m-by-n tiling of copies of A. 
% The size of B is [size(A,1)*m, (size(A,2)*n]. 
% The statement repmat(A,n) creates an n-by-n tiling.
sindpair = repmat(thisstruct', 1, length(thisdata));
dindpair = repmat(thisdata, length(thisstruct), 1);

sindpair = [extraspairs(:); sindpair(:)]';
dindpair = [extradpairs(:); dindpair(:)]';

repeats = 1;
for rind = 1:repeats
  for ind = 1:length(dindpair)
    dind = dindpair(ind);
    sind = sindpair(ind); 
    
    disp(['  ', ps.data{dind}, ' ', ps.structures{sind}]);
    rand('state', rind);
    [mtmp stmp  ntmp ltmp gtmp] = runmodel(ps, sind, dind, rind);
    succ = 0;
    while (succ == 0)
      try
        if exist(masterfile)
          currps = ps; load(masterfile); ps = currps;
	 end
	pss{sind,dind,rind} = ps;
        modellike(sind, dind, rind) = mtmp;  
        structure{sind,dind, rind}  = stmp;
        names{dind} = ntmp;		   
        llhistory{sind, dind, rind} = ltmp;
        save(masterfile, 'modellike', 'structure', 'names', 'pss', ...
			 'llhistory'); 
        succ = 1;
      catch
        succ = 0;
        disp('error reading masterfile');
        pause(10*rand);
      end
    end
  end
end

