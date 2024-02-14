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
Metadata.Slope =  nan(nRecordings, 1);
Metadata.Intercept =  nan(nRecordings, 1);
Metadata.Quantity =  nan(nRecordings, 1);

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
        NotEdgeChanIndex = labels2indexes(Parameters.Channels.NotEdge, Chanlocs);

        % remove bursts outside of frequency range
        Bursts([Bursts.BurstFrequency]<Frequencies(1) | [Bursts.BurstFrequency]>Frequencies(end)) = [];
        BurstClusters([BurstClusters.BurstFrequency]<Frequencies(1) | [BurstClusters.BurstFrequency]>Frequencies(end)) = [];

        SampleRate = EEGMetadata.srate;
        RecordingDuration = EEGMetadata.times(end)/60; % in minutes

        % load in variables that apply to whole recording
        TaskMetadata = cat(1, TaskMetadata, Metadata(RecordingIdx, :));
        NewIdx = size(TaskMetadata, 1);
        TaskMetadata.Task{NewIdx} = Task;
        TaskMetadata.Globality(NewIdx) = 100*mean([BurstClusters.ClusterGlobality]);
        TaskMetadata.Amplitude(NewIdx) = mean([BurstClusters.ClusterAmplitude]);
        TaskMetadata.Duration(NewIdx) = mean([BurstClusters.ClusterEnd]-[BurstClusters.ClusterStart])/SampleRate; % burst durations
        
        TaskMetadata.Quantity(NewIdx) = 100*sum([BurstClusters.ClusterEnd]-[BurstClusters.ClusterStart])/EEGMetadata.pnts; % number of bursts

        % load in power spectra
        Path = fullfile(SourcePower, Folder, Dataset, Task);

        DataOut = load_datafile(Path, Participant, Session, Hour, ...
            {'Power', 'Freqs'}, '.mat');
        Power = DataOut{1};
        Freqs = DataOut{2};

        FreqRange = dsearchn(Freqs', [Frequencies(1); Frequencies(end)]);
        TaskMetadata.Power(NewIdx) = mean(mean(log(Power(NotEdgeChanIndex, FreqRange(1):FreqRange(2))), 2), 1);


        %%% load in data for topographies
        BurstChannels = [Bursts.ChannelIndex];
        for ChannelIdx = 1:nChans
            for BandIdx = 1:nBands

                Band = Bands.(BandLabels{BandIdx});

                % power stuff
                FreqRange = dsearchn(Freqs', [Band(1); Band(2)]);
                BurstInformationTopographyBands.Power(NewIdx, ChannelIdx, BandIdx) = ...
                    mean(log(Power(ChannelIdx, FreqRange(1):FreqRange(2))), 2);

                % whitened power
                [~, ~, WhitenedPower, FooofFrequencies] = fooof_spectrum(Power(ChannelIdx, :), Freqs);
                FreqRangeFooof = dsearchn(FooofFrequencies', [Band(1); Band(2)]);
                BurstInformationTopographyBands.PeriodicPower(NewIdx, ChannelIdx, BandIdx) = ...
                    mean(WhitenedPower(FreqRangeFooof(1):FreqRangeFooof(2)), 2);

                % average quantity of bursts in that channel (as % duration recording)
                BurstsTemp = Bursts(BurstChannels==ChannelIdx & ...
                    [Bursts.BurstFrequency]>=Band(1) & [Bursts.BurstFrequency]<=Band(2));

               BurstInformationTopographyBands.Quantity(NewIdx, ChannelIdx, BandIdx) = ...
                    100*sum([BurstsTemp.DurationPoints])/EEGMetadata.pnts; % NOT CYCLES PER MINUTE!!

                if numel(BurstsTemp)<MinBursts
                   BurstInformationTopographyBands.Amplitude(NewIdx, ChannelIdx, BandIdx) = nan;
                else
                    % average amplitude in that channel
                  BurstInformationTopographyBands.Amplitude(NewIdx, ChannelIdx, BandIdx) = ...
                        mean([BurstsTemp.Amplitude]);
                end

            end

            %%% all frequencies amplitude and quantity

            % burst stuff
            BurstsTemp = Bursts(BurstChannels==ChannelIdx);

            % average quantity of bursts in that channel (as % duration recording)
            BurstInformationTopography.Quantity(NewIdx, ChannelIdx) = ...
                100*sum([BurstsTemp.DurationPoints])/EEGMetadata.pnts; % NOT CYCLES PER MINUTE!!

            % average amplitude in that channel
            if numel(BurstsTemp)< MinBursts
                BurstInformationTopography.Amplitude(NewIdx, ChannelIdx) = nan;
            else
                BurstInformationTopography.Amplitude(NewIdx, ChannelIdx) = ...
                    mean([BurstsTemp.Amplitude]);
            end

            % power
            FreqRange = dsearchn(Freqs', [Frequencies(1); Frequencies(end)]);
            BurstInformationTopography.Power(NewIdx, ChannelIdx) = mean(log(Power(ChannelIdx, FreqRange(1):FreqRange(2))), 2);

            % slopes and stuff
            [Slope, Intercept, WhitenedPower, FooofFrequencies] = fooof_spectrum(Power(ChannelIdx, :), Freqs);
            BurstInformationTopography.Slope(NewIdx, ChannelIdx) = Slope;
            BurstInformationTopography.Intercept(NewIdx, ChannelIdx) = Intercept;

            FreqRangeFooof = dsearchn(FooofFrequencies', [Frequencies(1); Frequencies(end)]);
            BurstInformationTopography.PeriodicPower(NewIdx, ChannelIdx) = mean(WhitenedPower(FreqRangeFooof(1):FreqRangeFooof(2)), 2);
        end

        % run fooof
        [Slope, Intercept, WhitenedPower, FooofFrequencies] = fooof_spectrum(mean(Power(NotEdgeChanIndex, :), 1), Freqs);
        TaskMetadata.Slope(NewIdx) = Slope;
        TaskMetadata.Intercept(NewIdx) = Intercept;
        FreqRangeFooof = dsearchn(FooofFrequencies', [Frequencies(1); Frequencies(end)]);
        TaskMetadata.PeriodicPower(NewIdx) = mean(WhitenedPower(FreqRangeFooof(1):FreqRangeFooof(2)));

        %%% load in data for spectrogram
        BurstFrequencies = discretize([BurstClusters.BurstFrequency], Frequencies);
        FooofPowerFrequencies = discretize(FooofFrequencies, Frequencies);
        PowerFrequencies = discretize(Freqs, Frequencies);
        for FrequencyIdx = 1:nFrequencies
            BurstIdx = BurstFrequencies==FrequencyIdx;
            BurstsTemp = BurstClusters(BurstIdx);

            BurstInformationClusters.Quantity(NewIdx, FrequencyIdx) = ...
                100*sum([BurstsTemp.ClusterEnd]-[BurstsTemp.ClusterStart])/EEGMetadata.pnts;

            if numel(BurstsTemp)<MinBursts
                BurstInformationClusters.Amplitude(NewIdx, FrequencyIdx) = nan;
                BurstInformationClusters.Globality(NewIdx, FrequencyIdx) = nan;
                BurstInformationClusters.Duration(NewIdx, FrequencyIdx) = nan;
            else
                BurstInformationClusters.Amplitude(NewIdx, FrequencyIdx) = ...
                    mean([BurstsTemp.Amplitude]);

                BurstInformationClusters.Globality(NewIdx, FrequencyIdx) = ...
                    100*mean([BurstsTemp.ClusterGlobality]);

                BurstInformationClusters.Duration(NewIdx, FrequencyIdx) = ...
                    mean([BurstsTemp.ClusterEnd]-[BurstsTemp.ClusterStart])/SampleRate;
            end

            % power for that frequency
            BurstInformationClusters.PeriodicPower(NewIdx, FrequencyIdx) = ...
                mean(WhitenedPower(FooofPowerFrequencies==FrequencyIdx));

            BurstInformationClusters.Power(NewIdx, FrequencyIdx) = ...
                mean(mean(log(Power(NotEdgeChanIndex, PowerFrequencies==FrequencyIdx))));


        end
    end
    disp(num2str(RecordingIdx))
end

Metadata = TaskMetadata;
Frequencies(end) = []; % remove last edge;

% save
save(fullfile(CacheDir, CacheName), 'Metadata',  'BurstInformationTopography', 'BurstInformationTopographyBands', ...
    "BurstInformationClusters", 'Frequencies', 'Chanlocs')
