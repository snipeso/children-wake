% creates a massive matrix D x ch x f x v, so that data can then easily be
% indexed as needed.

clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Hours = Parameters.Hours;

Variables = {''};
VariablesCluster = {};

Frequencies = 5:16;
nFrequencies = numel(Frequencies)-1;
nChans = 123;
nVariables = 3;

Source = fullfile(Paths.AnalyzedData, 'EEG', 'Bursts');

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

if ~exist(CacheDir, 'dir')
    mkdir(CacheDir)
end

Metadata = readtable(fullfile(Paths.Metadata, 'Metadata.csv'));
Metadata = Metadata(contains(Metadata.Dataset, Datasets), :);
nRecordings = size(Metadata, 1); % this does not consider tasks
Metadata.Task = repmat({''}, nRecordings, 1);
Metadata.Globality = nan(nRecordings, 1);
Metadata.Amplitude = nan(nRecordings, 1);
Metadata.Duration =  nan(nRecordings, 1);
Metadata.Frequency =  nan(nRecordings, 1);

BurstInformationClusters = struct();
BurstInformationClusters.Amplitude = nan(nRecordings, nFrequencies);
BurstInformationClusters.Quantity =nan(nRecordings, nFrequencies);

BurstInformationTopography = struct();
BurstInformationTopography.Quantity = nan(nRecordings, nChans);
BurstInformationTopography.Amplitude = nan(nRecordings, nChans);
BurstInformationTopography.Frequency = nan(nRecordings, nChans); % average frequency

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
        if isempty(DataOut); continue; end

        Bursts = DataOut{1};
        BurstClusters = DataOut{2};
        EEGMetadata = DataOut{3};
        Chanlocs = EEGMetadata.chanlocs;
        SampleRate = EEGMetadata.srate;
        RecordingDuration = EEGMetadata.times(end)/60; % in minutes

        % load in variables that apply to whole recording
        TaskMetadata = cat(1, TaskMetadata, Metadata(RecordingIdx, :));
        NewIdx = size(TaskMetadata, 1);
        TaskMetadata.Task{NewIdx} = Task;
        TaskMetadata.Globality(NewIdx) = mean([BurstClusters.ClusterGlobality]);
        TaskMetadata.Amplitude(NewIdx) = mean([BurstClusters.ClusterAmplitude]);
        TaskMetadata.Duration(NewIdx) = mean([BurstClusters.ClusterEnd]-[BurstClusters.ClusterStart])/SampleRate;
        TaskMetadata.Frequency(NewIdx) = mean([BurstClusters.BurstFrequency]);


        %%% load in data for topographies
        for Channel = 1:nChans
            BurstsTemp = Bursts([Bursts.ChannelIndex]==Channel);

            if numel(BurstsTemp)>=10
                % average amplitude in that channel
                BurstInformationTopography.Amplitude(NewIdx, Channel) = ...
                    mean([BurstsTemp.Amplitude]);

                % average quantity of bursts in that channel (as % duration recording)
                BurstInformationTopography.Quantity(NewIdx, Channel) = ...
                    sum([BurstsTemp.DurationPoints])/EEGMetadata.pnts;

                % average frequency of burst in that channel
                BurstInformationTopography.Frequency(NewIdx, Channel) = ...
                    mean([BurstsTemp.BurstFrequency]);
            end
        end

        %%% load in data for spectrogram
        BurstFrequencies = discretize([Bursts.BurstFrequency], Frequencies);
        for FrequencyIdx = 1:nFrequencies
            BurstIdx = BurstFrequencies==FrequencyIdx;
            BurstsTemp = Bursts(BurstIdx);

            if numel(BurstsTemp)<10
                continue
            end

            % how many cycles in that channel
            BurstInformationClusters.Quantity(NewIdx, FrequencyIdx) = ...
                sum([BurstsTemp.CyclesCount])/RecordingDuration;

            % amplitude per channels
            BurstInformationClusters.Amplitude(NewIdx, FrequencyIdx) = ...
                mean([BurstsTemp.Amplitude]);
        end
    end
    disp(num2str(RecordingIdx))
end

Metadata = TaskMetadata;
Frequencies(end) = []; % remove last edge;

% save
save(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopography', ...
    "BurstInformationClusters", 'Frequencies', 'Chanlocs')
