function [FDR, h, crit_p] = fdr_matrix(pValues, StatsParameters)
% applies FDR correction to a N x N matrix of pvalues, by turning them
% first into a vector, then back into the original matrix.

Dim1 = size(pValues, 1);

% turn matrix into vector
Indexes = 1:numel(pValues); % a number for every p value
Indexes = reshape(Indexes, Dim1, []); % shaped like the original p-value matrix
pValues_long = pValues(:);
Indexes_long = Indexes(:);

% remove any nans (relies on the index to then reconstruct position of p values)
Nans = isnan(pValues_long); % there is probably a more elegant way to do this
pValues_long(Nans) = [];
Indexes_long(Nans) = [];

% identify still significant values
[sig, crit_p, ~,  pValues_fdr] = fdr_bh(pValues_long, StatsParameters.Alpha, StatsParameters.ttest.dep);

h = nan(size(pValues));
h(Indexes_long) = sig;

FDR = nan(size(pValues));
FDR(Indexes_long) = pValues_fdr;