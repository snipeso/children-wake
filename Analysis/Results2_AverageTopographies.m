clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Hours = Parameters.Hours;
BandLabels = {'Theta', 'Low Alpha', 'High Alpha'};
TopoPlotProps = Parameters.PlotProps.TopoPlots;
Ages = Parameters.Ages;


MinNaNChannels = 25; % for amplitudes

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', 'Chanlocs')


Metadata.Index = [1:size(Metadata, 1)]'; %#ok<NBRAK1> % add index so can chop up table as needed
Metadata(strcmp(Metadata.Dataset, 'SleepLearning') & ...
    contains(Metadata.Session, {'Session_2', 'Session_3'}), :) = []; % remove repeated measures 1 year later (will average recordings a couple weeks apart)
Metadata(contains(Metadata.Task, {'Learning', 'Alertness', 'Fixation'}), :) = []; % only look at first oddball and alertness
Metadata.Subgroup(strcmp(Metadata.Group, 'HC')) = 5;
Metadata.Globality = Metadata.Globality*100; % make it percentage instead of proportion
Metadata.AgeGroups = string(discretize(Metadata.Age, [Ages(:, 1); Ages(end, 2)]));
Metadata.Task(contains(Metadata.Task, 'Alertness')) = {'Alertness'}; % Fix because different order in task
Metadata.Task(contains(Metadata.Task, 'Oddball')) = {'Oddball'};
MetadataComplete = Metadata;
Metadata(contains(Metadata.Group, 'ADHD'), :) = []; % RODO figure out why theres too few ADHD kids!!
nAges = size(Ages, 1);

