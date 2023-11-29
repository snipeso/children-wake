
clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Hours = Parameters.Hours;

Variables = {''};
VariablesCluster = {};

Frequencies = 4:14;
nFrequencies = numel(Frequencies);
nChans = 109;
nVariables = 3;

Source = fullfile(Paths.AnalyzedData, 'EEG', 'Bursts');

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

if ~exist(CacheDir, 'dir')
    mkdir(CacheDir)
end

[Participants, UniqueIndx] = unique(Metadata.Participant);
UniqueMetadata = Metadata(UniqueIndx, :);

Histograms = nan(numel(Participants), nFrequencies, 2);

for  ParticipantIdx = 1:numel(Participants)
    Dataset = UniqueMetadata.Dataset{ParticipantIdx};
    Participant = Metadata.Participant{ParticipantIdx};
    Session = Metadata.Session{ParticipantIdx};

    Task = Parameters.Tasks.(Dataset){1};

    for Hour = 1:2

        % load in data
        Path = fullfile(Source, Dataset, Task);
        DataOut = load_datafile(Path, Participant, Session, Hour, ...
            {'Bursts', 'BurstClusters', 'EEGMetadata'}, '.mat');
        if isempty(DataOut); continue; end

        Bursts = DataOut{1};
        BurstClusters = DataOut{2};
        EEGMetadata = DataOut{3};
    end
end




