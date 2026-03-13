% creates a massive matrix D x ch x f x v, so that data can then easily be
% indexed as needed.
% TODO: better documenation; rename to Assemble WakeData

clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;

Frequencies = 4:16;
nFrequencies = numel(Frequencies)-1;
nChans = 123;
MinBursts = 10;

Source = fullfile(Paths.AnalyzedData, 'EEG', 'Bursts');
SourcePower =  fullfile(Paths.AnalyzedData, 'EEG', 'Power');
Folder = 'window4s_allt';

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

if ~exist(CacheDir, 'dir')
    mkdir(CacheDir)
end

Metadata = readtable(fullfile(Paths.Metadata, 'Metadata_Children_Wake.csv'));
Metadata = Metadata(contains(Metadata.Dataset, Datasets), :);
nRecordings = size(Metadata, 1); % this does not consider tasks

% average information per recording; for quick statistics
Metadata.Task = repmat({''}, nRecordings, 1);
Metadata.Globality = nan(nRecordings, 1);
Metadata.Amplitude = nan(nRecordings, 1);
Metadata.Duration =  nan(nRecordings, 1);
Metadata.Power =  nan(nRecordings, 1);
Metadata.PeriodicPower = nan(nRecordings, 1);
Metadata.AperiodicPower =  nan(nRecordings, 1);
Metadata.Slope =  nan(nRecordings, 1);
Metadata.Intercept =  nan(nRecordings, 1);
Metadata.Quantity =  nan(nRecordings, 1);
Metadata.Error = nan(nRecordings, 1);
Metadata.RSquared = nan(nRecordings, 1);

% average information across channels, split by frequencies
BurstInformationClusters = struct();
BurstInformationClusters.Amplitude = nan(nRecordings, nFrequencies);
BurstInformationClusters.Quantity =nan(nRecordings, nFrequencies);
BurstInformationClusters.Duration =nan(nRecordings, nFrequencies);
BurstInformationClusters.Globality =nan(nRecordings, nFrequencies);
BurstInformationClusters.Power = nan(nRecordings, nFrequencies);
BurstInformationClusters.PeriodicPower = nan(nRecordings, nFrequencies);


% average information per channel, averaging frequencies into bands
Bands = Parameters.Bands;
BandLabels = fieldnames(Bands);
nBands = numel(BandLabels);
BurstInformationTopographyBands = struct();
BurstInformationTopographyBands.Quantity = nan(nRecordings, nChans, nBands);
BurstInformationTopographyBands.Amplitude = nan(nRecordings, nChans, nBands);
BurstInformationTopographyBands.Power = nan(nRecordings, nChans, nBands);
BurstInformationTopographyBands.PeriodicPower = nan(nRecordings, nChans, nBands);

BurstInformationTopography.Slope = nan(nRecordings, nChans);
BurstInformationTopography.Intercept = nan(nRecordings, nChans);
BurstInformationTopography.Quantity = nan(nRecordings, nChans);
BurstInformationTopography.Amplitude = nan(nRecordings, nChans);
BurstInformationTopography.Power = nan(nRecordings, nChans);
BurstInformationTopography.PeriodicPower = nan(nRecordings, nChans);

AverageSpectrograms = nan(nRecordings, 513);

TaskMetadata = table(); % set up new metadata table that also takes into account task

for RecordingIdx = 1:nRecordings

    Dataset = Metadata.Dataset{RecordingIdx};
    Participant = Metadata.Participant{RecordingIdx};
    Session = replace(Metadata.Session{RecordingIdx}, '_', '');
    Hour = Metadata.Hour{RecordingIdx};

    Tasks = Parameters.Tasks.(Dataset);

    for TaskIdx = 1:numel(Tasks)
        Task = Tasks{TaskIdx};

        % load in data
        Path = fullfile(Source, Dataset, Task);
        DataOut = load_datafile(Path, Participant, Session, Hour, ...
            {'Bursts', 'BurstClusters', 'EEGMetadata'}, '.mat');
    end
    disp(num2str(RecordingIdx))
end

Metadata = TaskMetadata;
Frequencies(end) = []; % remove last edge;

% save
save(fullfile(CacheDir, CacheName), 'Metadata',  'BurstInformationTopography', 'BurstInformationTopographyBands', ...
    "BurstInformationClusters", 'Frequencies', 'Chanlocs', 'AllFrequencies', 'AverageSpectrograms')
