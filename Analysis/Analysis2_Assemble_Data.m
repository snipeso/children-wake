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

Metadata = readtable(fullfile(Paths.Metadata, 'Metadata.csv'));
Metadata = Metadata(contains(Metadata.Dataset, Datasets), :);
nRecordings = size(Metadata, 1);
Metadata.Exists = zeros(nRecordings, 1);
Metadata.Task = repmat({''}, nRecordings, 1);
ClusterBlank = nan(nRecordings, nFrequencies); % synthesized data pooled into clusters
BurstBlank = nan(nRecordings, nChans, nFrequencies);  % information for each channel to produce topographies

BurstInformationClusters = struct();
BurstInformationTopography = struct();

BurstInformationClusters.Amplitude = ClusterBlank;
BurstInformationClusters.Quantity = ClusterBlank;
BurstInformationClusters.Globality = ClusterBlank;
BurstInformationClusters.Duration = ClusterBlank;

BurstInformationTopography.Quantity = BurstBlank;
BurstInformationTopography.Amplitude = BurstBlank;
BurstInformationTopography.Frequency = nan(nRecordings, nChans); % average frequency

TaskMetadata = table(); % set up new metadata table that also takes into account task

for RecordingIdx = 1:nRecordings

    Dataset = Metadata.Dataset{RecordingIdx};
    Participant = Metadata.Participant{RecordingIdx};
    Session = Metadata.Session{RecordingIdx};
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
        RecordingDuration = (EEGMetadata.times)/60; % in minutes

        % load in variables
        TaskMetadata = cat(1, TaskMetadata, Metadata(RecordingIdx, :));
        TaskMetadata.Task{end} = Task;

        BurstFrequencies = [Bursts.BurstFrequency];
        BurstChannels = [Bursts.ChannelIndex];

        for Channel = 1:nChans
            for FrequencyIdx = 1:numel(Frequencies)
                Frequency = Frequencies(FrequencyIdx);
                BurstIdx = BurstFrequencies>=Frequency & BurstFrequencies<Frequency+1 & ...
                    BurstChannels==Channel;
                BurstsTemp = Bursts(BurstIdx);
                if numel(BurstTemp)<5
                    continue
                end

                % how many cycles in that channel
                CyclesPerMinute = sum([BurstsTemp.CyclesCount])/RecordingDuration;
                BurstInformationTopography.Quantity(RecordingIdx, Channel, FrequencyIdx) = CyclesPerMinute;

                % amplitude per channels
                Amplitude = mean([BurstsTemp.Amplitude]);
                BurstInformationTopography.Amplitude(RecordingIdx, Channel, FrequencyIdx) = Amplitude;
            end

            % average frequency of burst in that channel
            BurstTemp = Bursts([Bursts.ChannelIndex]==Channel);
            if numel(BurstTemp)>=5
                Frequency = mean([BurstTemp.BurstFrequency]);
                 BurstInformationTopography.Amplitude(RecordingIdx, Channel, FrequencyIdx) = Frequency;
            end
        end
    end
    disp(num2str(RecordingIdx))
end



% reduce to only datasets that exist
Metadata(~Metadata.Exists, :) = [];
BurstInformationClusters(~Metadata.Exists, :, :) = [];
BurstInformationTopography(~Metadata.Exists, :, :, :) = [];

% save
save(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopography', ...
    "BurstInformationClusters", 'Frequencies', 'Chanlocs')