function [A1 A2 B1 B2 D]=triplepartition(J, nobs, nmiss)

nobj = nobs+nmiss;
A1 = J(1:nobs, 1:nobs);
B1 = J(1:nobs, nobj+1:end);
A2 = J(nobs+1:nobj, nobs+1:nobj);
B2 = J(nobs+1:nobj, nobj+1:end);
D = J(nobj+1:end, nobj+1:end);

