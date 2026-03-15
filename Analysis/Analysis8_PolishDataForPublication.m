%% Polish data


clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% setup variables and parameters

Parameters = analysisParameters();


%%% set paths
Paths = Parameters.Paths;

% where data can be found
CacheDir = Paths.Cache;
CacheName = 'ProcessedData.mat';

% where to save figures
ResultsFolder = fullfile(Paths.Results, 'MainStatsStandardized');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

%%% load data
load(fullfile(CacheDir, CacheName))

% fixes to metadata
Metadata = basic_metadata_cleanup(Metadata);

% recode dataset names
Datasets = {'SleepLearning', 'Providence', 'ADHD', 'BMSAdults', 'BMS', 'BMSSL'};
DatasetsNew = {'Dataset2008', 'Dataset2009', 'Dataset2010', 'Dataset2016', 'Dataset2017', 'Dataset2019'};

for DatasetIdx = 1:numel(Datasets)
    Metadata.Dataset(strcmp(Metadata.Dataset, Datasets{DatasetIdx})) = ...
        repmat(DatasetsNew(DatasetIdx), nnz(strcmp(Metadata.Dataset, Datasets{DatasetIdx})), 1);
end


%% codex

OriginalTableLabels = Metadata.Properties.VariableNames;
for MeasureIdx = 1:numel(Parameters.OutcomeMeasures.OriginalLabels)
    ColumnIdx = strcmp(OriginalTableLabels, Parameters.OutcomeMeasures.OriginalLabels{MeasureIdx});
    Metadata.Properties.VariableNames(ColumnIdx) = ...
        genvarname(Parameters.OutcomeMeasures.Titles(MeasureIdx));
end

%%

save(fullfile(ResultsFolder, 'AllData.mat'), 'Metadata',  'BurstInformationTopography', 'BurstInformationTopographyBands', ...
    "BurstInformationClusters", 'Frequencies', 'Chanlocs', 'AllFrequencies', 'AverageSpectrograms')
