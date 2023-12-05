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
SourcePower =  fullfile(Paths.AnalyzedData, 'EEG', 'Power');
Folder = 'window8s_allt';

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
Metadata.Frequency = nan(nRecordings, 1);
Metadata.Slope =  nan(nRecordings, 1);
Metadata.Intercept =  nan(nRecordings, 1);

BurstInformationClusters = struct();
BurstInformationClusters.Amplitude = nan(nRecordings, nFrequencies);
BurstInformationClusters.Quantity =nan(nRecordings, nFrequencies);
BurstInformationClusters.Globality =nan(nRecordings, nFrequencies);
BurstInformationClusters.Power = nan(nRecordings, nFrequencies);


BurstInformationTopography = struct();
BurstInformationTopography.Quantity = nan(nRecordings, nChans);
BurstInformationTopography.Amplitude = nan(nRecordings, nChans);
BurstInformationTopography.Frequency = nan(nRecordings, nChans); % peak frequency
BurstInformationTopography.Slope = nan(nRecordings, nChans);
BurstInformationTopography.Intercept = nan(nRecordings, nChans);


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

        % load in power spectra
        Path = fullfile(SourcePower, Folder, Dataset, Task);

        DataOut = load_datafile(Path, Participant, Session, Hour, ...
            {'Power', 'Freqs'}, '.mat');
        Power = DataOut{1};
        Freqs = DataOut{2};


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

                % peak frequency of burst in that channel
                FrequencyDistribution = [BurstsTemp.BurstFrequency];
                FrequencyBins = 3:.5:16;
                FrequencyDistribution = discretize(FrequencyDistribution, FrequencyBins);
                PeakIdx = mode(FrequencyDistribution);
                BurstInformationTopography.Frequency(NewIdx, Channel) = FrequencyBins(PeakIdx);
            end

            % slopes and stuff
            [Slope, Intercept] = fooof_spectrum(Power(Channel, :), Freqs);
            BurstInformationTopography.Slope(NewIdx, Channel) = Slope;
            BurstInformationTopography.Intercept(NewIdx, Channel) = Intercept;
        end

        % run fooof
        [Slope, Intercept, WhitenedPower, FooofFrequencies] = fooof_spectrum(mean(Power, 1), Freqs);
        TaskMetadata.Slope(NewIdx) = Slope;
        TaskMetadata.Intercept(NewIdx) = Intercept;

        %%% load in data for spectrogram
        BurstFrequencies = discretize([Bursts.BurstFrequency], Frequencies);
        PowerFrequencies = discretize(FooofFrequencies, Frequencies);
        for FrequencyIdx = 1:nFrequencies
            BurstIdx = BurstFrequencies==FrequencyIdx;
            BurstsTemp = Bursts(BurstIdx);

            if numel(BurstsTemp)<10
                continue
            end

            BurstInformationClusters.Quantity(NewIdx, FrequencyIdx) = ...
                sum([BurstsTemp.CyclesCount])/RecordingDuration;

            BurstInformationClusters.Amplitude(NewIdx, FrequencyIdx) = ...
                mean([BurstsTemp.Amplitude]);

            BurstInformationClusters.Globality(NewIdx, FrequencyIdx) = ...
                mean([BurstsTemp.ClusterGlobality]);

            % power for that frequency
            BurstInformationClusters.Power(NewIdx, FrequencyIdx) = ...
                mean(WhitenedPower(PowerFrequencies==FrequencyIdx));
        end
    end
    disp(num2str(RecordingIdx))
end

Metadata = TaskMetadata;
Frequencies(end) = []; % remove last edge;

% save
save(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopography', ...
    "BurstInformationClusters", 'Frequencies', 'Chanlocs')
