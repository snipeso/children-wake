% Applies final metadata cleanup, anonymized dataset relabeling, and saves
% the `ExtendedData_*.mat` files.

%% Polish data


clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% setup variables and parameters

Parameters = analysisParameters();
Bands = Parameters.Bands;

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



writetable(Metadata, fullfile(ResultsFolder, 'ExtendedData_3.csv'))


SpectraAverage = SpectraAverage(Metadata.Index, :); % only used data for this study
save(fullfile(ResultsFolder, 'ExtendedData_2.mat'), 'Metadata', 'SpectraAverage', ...
    'Frequencies')

for Field = fieldnames(Topographies)'
    ShortData = Topographies.(Field{1})(Metadata.Index, :);
    Topographies.(Field{1}) = ShortData;
end
save(fullfile(ResultsFolder, 'ExtendedData_4.mat'), 'Metadata',  ...
    'Topographies',  'Chanlocs')

for Field = fieldnames(TopographiesBands)'
    ShortData = TopographiesBands.(Field{1})(Metadata.Index, :, :);
    TopographiesBands.(Field{1}) = ShortData;
end
save(fullfile(ResultsFolder, 'ExtendedData_6.mat'), 'Metadata',  ...
    'TopographiesBands',  'Chanlocs', "Bands")


