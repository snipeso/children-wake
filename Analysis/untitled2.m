load(fullfile(Paths.Metadata, 'SleepScoring.mat'), 'Metadata')
ScoringMetadata = Metadata;

Datasets = Parameters.Datasets;

ScoringMetadata(~ismember(ScoringMetadata.Dataset, Datasets), :) = [];

save(fullfile(Paths.Metadata, 'SleepScoringForWake.mat'), 'ScoringMetadata')
