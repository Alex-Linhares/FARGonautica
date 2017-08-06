function [A B C D]=matrixpartition(J, nobjects)

% Chop matrix J into 4 sub matrices

A = J(1:nobjects, 1:nobjects);
B = J(1:nobjects, nobjects+1:end);
C = J(nobjects+1:end, 1:nobjects);
D = J(nobjects+1:end, nobjects+1:end);

