function W = wdconv(W)

% Convert from edge weights to distances

W(W>0)=1./W(W>0);

