% creates a massive matrix D x ch x f x v, so that data can then easily be
% indexed as needed.
% TODO: better documenation; rename to Assemble WakeData

clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;

FrequenciesRedux = 4:16;
nFrequencies = numel(FrequenciesRedux)-1;
nChans = 123;
MinBursts = 10;

Source = fullfile(Paths.AnalyzedData, 'EEG', 'Bursts');
SourcePower =  fullfile(Paths.AnalyzedData, 'EEG', 'Power');
Folder = 'window4s_allt';

CacheDir = Paths.Cache;
CacheName = 'ProcessedData.mat';

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
Metadata.Exponent =  nan(nRecordings, 1);
Metadata.Offset =  nan(nRecordings, 1);
Metadata.Density =  nan(nRecordings, 1);
Metadata.Error = nan(nRecordings, 1);
Metadata.RSquared = nan(nRecordings, 1);
Metadata.RecordingDuration = nan(nRecordings, 1);

% average information per channel, averaging frequencies into bands
Bands = Parameters.Bands;
BandLabels = fieldnames(Bands);
nBands = numel(BandLabels);
for BandIdx = 1:numel(BandLabels)
    Metadata.(BandLabels{BandIdx}) = nan(nRecordings, 1);
end

% average information across channels, split by frequencies
SpectraRedux = struct();
SpectraRedux.Amplitude = nan(nRecordings, nFrequencies);
SpectraRedux.Density =nan(nRecordings, nFrequencies);
SpectraRedux.Duration =nan(nRecordings, nFrequencies);
SpectraRedux.Globality =nan(nRecordings, nFrequencies);
SpectraRedux.Power = nan(nRecordings, nFrequencies);
SpectraRedux.PeriodicPower = nan(nRecordings, nFrequencies);

TopographiesBands = struct();
TopographiesBands.Density = nan(nRecordings, nChans, nBands);
TopographiesBands.Amplitude = nan(nRecordings, nChans, nBands);
TopographiesBands.Power = nan(nRecordings, nChans, nBands);
TopographiesBands.PeriodicPower = nan(nRecordings, nChans, nBands);

Topographies.Exponent = nan(nRecordings, nChans);
Topographies.Offset = nan(nRecordings, nChans);
Topographies.Density = nan(nRecordings, nChans);
Topographies.Amplitude = nan(nRecordings, nChans);
Topographies.Power = nan(nRecordings, nChans);
Topographies.PeriodicPower = nan(nRecordings, nChans);

SpectraAverage = nan(nRecordings, 513);

TaskMetadata = table(); % set up new metadata table that also takes into account task

for RecordingIdx = 1:nRecordings
    A = tic;
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
        Bursts([Bursts.BurstFrequency]<FrequenciesRedux(1) | [Bursts.BurstFrequency]>FrequenciesRedux(end)) = [];
        BurstClusters([BurstClusters.BurstFrequency]<FrequenciesRedux(1) | [BurstClusters.BurstFrequency]>FrequenciesRedux(end)) = [];

        SampleRate = EEGMetadata.srate;
        RecordingDuration = EEGMetadata.times(end)/60; % in minutes

        % load in variables that apply to whole recording
        TaskMetadata = cat(1, TaskMetadata, Metadata(RecordingIdx, :));
        NewIdx = size(TaskMetadata, 1);
        TaskMetadata.Task{NewIdx} = Task;
        TaskMetadata.RecordingDuration(NewIdx) = EEGMetadata.pnts/EEGMetadata.srate/60; % in minutes
        TaskMetadata.Globality(NewIdx) = 100*mean([BurstClusters.ClusterGlobality]);
        TaskMetadata.Amplitude(NewIdx) = mean([BurstClusters.ClusterAmplitude]);
        TaskMetadata.Duration(NewIdx) = mean([BurstClusters.ClusterEnd]-[BurstClusters.ClusterStart])/SampleRate; % burst durations

        TaskMetadata.Density(NewIdx) = 100*sum([BurstClusters.ClusterEnd]-[BurstClusters.ClusterStart])/EEGMetadata.pnts; % number of bursts

        % load in power spectra
        Path = fullfile(SourcePower, Folder, Dataset, Task);
        DataOut = load_datafile(Path, Participant, Session, Hour, ...
            {'Power', 'Freqs'}, '.mat');
        Power = DataOut{1};
        Frequencies = DataOut{2};

        FreqRange = dsearchn(Frequencies', [FrequenciesRedux(1); FrequenciesRedux(end)]);
        TaskMetadata.Power(NewIdx) = mean(mean(log10(Power(NotEdgeChanIndex, FreqRange(1):FreqRange(2))), 2), 1);
        for BandIdx = 1:numel(BandLabels)
            Band = Bands.(BandLabels{BandIdx});
            FreqRange = dsearchn(Frequencies', [Band(1); Band(end)]);
            TaskMetadata.(BandLabels{BandIdx})(NewIdx) = ...
                mean(mean(log10(Power(NotEdgeChanIndex, FreqRange(1):FreqRange(2))), 2), 1);
        end


        %%% load in data for topographies
        BurstChannels = [Bursts.ChannelIndex];
        for ChannelIdx = 1:nChans
            for BandIdx = 1:nBands

                Band = Bands.(BandLabels{BandIdx});

                % power stuff
                FreqRange = dsearchn(Frequencies', [Band(1); Band(2)]);
                TopographiesBands.Power(NewIdx, ChannelIdx, BandIdx) = ...
                    mean(log10(Power(ChannelIdx, FreqRange(1):FreqRange(2))), 2);

                % whitened power
                [~, ~, WhitenedPower, FooofFrequencies] = fooof_spectrum(Power(ChannelIdx, :), Frequencies, [2 35]);
                FreqRangeFooof = dsearchn(FooofFrequencies', [Band(1); Band(2)]);
                TopographiesBands.PeriodicPower(NewIdx, ChannelIdx, BandIdx) = ...
                    mean(WhitenedPower(FreqRangeFooof(1):FreqRangeFooof(2)), 2);

                % average quantity of bursts in that channel (as % duration recording)
                BurstsTemp = Bursts(BurstChannels==ChannelIdx & ...
                    [Bursts.BurstFrequency]>=Band(1) & [Bursts.BurstFrequency]<=Band(2));

                TopographiesBands.Density(NewIdx, ChannelIdx, BandIdx) = ...
                    100*sum([BurstsTemp.DurationPoints])/EEGMetadata.pnts; % NOT CYCLES PER MINUTE!!

                if numel(BurstsTemp)<MinBursts
                    TopographiesBands.Amplitude(NewIdx, ChannelIdx, BandIdx) = nan;
                else
                    % average amplitude in that channel
                    TopographiesBands.Amplitude(NewIdx, ChannelIdx, BandIdx) = ...
                        mean([BurstsTemp.Amplitude]);
                end
            end

            %%% all frequencies amplitude and quantity

            % burst stuff
            BurstsTemp = Bursts(BurstChannels==ChannelIdx);

            % average quantity of bursts in that channel (as % duration recording)
            Topographies.Density(NewIdx, ChannelIdx) = ...
                100*sum([BurstsTemp.DurationPoints])/EEGMetadata.pnts; % NOT CYCLES PER MINUTE!!

            % average amplitude in that channel
            if numel(BurstsTemp)< MinBursts
                Topographies.Amplitude(NewIdx, ChannelIdx) = nan;
            else
                Topographies.Amplitude(NewIdx, ChannelIdx) = ...
                    mean([BurstsTemp.Amplitude]);
            end

            % power
            FreqRange = dsearchn(Frequencies', [FrequenciesRedux(1); FrequenciesRedux(end)]);
            Topographies.Power(NewIdx, ChannelIdx) = mean(log10(Power(ChannelIdx, FreqRange(1):FreqRange(2))), 2);

            % slopes and stuff
            [Exponent, Offset, WhitenedPower, FooofFrequencies] = fooof_spectrum(Power(ChannelIdx, :), Frequencies, [2 35]);
            Topographies.Exponent(NewIdx, ChannelIdx) = Exponent;
            Topographies.Offset(NewIdx, ChannelIdx) = Offset;

            FreqRangeFooof = dsearchn(FooofFrequencies', [FrequenciesRedux(1); FrequenciesRedux(end)]);
            Topographies.PeriodicPower(NewIdx, ChannelIdx) = mean(WhitenedPower(FreqRangeFooof(1):FreqRangeFooof(2)), 2);
        end

        % get power for all non-edge channels
        AveragePower = mean(Power(NotEdgeChanIndex, :), 1);
        SpectraAverage(NewIdx, :) = AveragePower;

        % run fooof
        [Exponent, Offset, WhitenedPower, FooofFrequencies, Fit, AperiodicPower] = fooof_spectrum(AveragePower, Frequencies, [2 35]);
        TaskMetadata.Exponent(NewIdx) = Exponent;
        TaskMetadata.Offset(NewIdx) = Offset;
        FreqRangeFooof = dsearchn(FooofFrequencies', [FrequenciesRedux(1); FrequenciesRedux(end)]);
        TaskMetadata.PeriodicPower(NewIdx) = mean(WhitenedPower(FreqRangeFooof(1):FreqRangeFooof(2)));
        TaskMetadata.Error(NewIdx) = Fit(1);
        TaskMetadata.RSquared(NewIdx) = Fit(2);
        TaskMetadata.AperiodicPower(NewIdx) = mean(AperiodicPower(FreqRangeFooof(1):FreqRangeFooof(2))); % TODO, change WhitenedPower to periodicPower

        %%% load in data for spectrogram
        BurstFrequencies = discretize([BurstClusters.BurstFrequency], FrequenciesRedux);
        FooofPowerFrequencies = discretize(FooofFrequencies, FrequenciesRedux);
        PowerFrequencies = discretize(Frequencies, FrequenciesRedux);
        for FrequencyIdx = 1:nFrequencies
            BurstIdx = BurstFrequencies==FrequencyIdx;
            BurstsTemp = BurstClusters(BurstIdx);

            SpectraRedux.Density(NewIdx, FrequencyIdx) = ...
                100*sum([BurstsTemp.ClusterEnd]-[BurstsTemp.ClusterStart])/EEGMetadata.pnts;

            if numel(BurstsTemp)<MinBursts
                SpectraRedux.Amplitude(NewIdx, FrequencyIdx) = nan;
                SpectraRedux.Globality(NewIdx, FrequencyIdx) = nan;
                SpectraRedux.Duration(NewIdx, FrequencyIdx) = nan;
            else
                SpectraRedux.Amplitude(NewIdx, FrequencyIdx) = ...
                    mean([BurstsTemp.Amplitude]);

                SpectraRedux.Globality(NewIdx, FrequencyIdx) = ...
                    100*mean([BurstsTemp.ClusterGlobality]); 

                SpectraRedux.Duration(NewIdx, FrequencyIdx) = ...
                    mean([BurstsTemp.ClusterEnd]-[BurstsTemp.ClusterStart])/SampleRate;
            end

            % power for that frequency
            SpectraRedux.PeriodicPower(NewIdx, FrequencyIdx) = ...
                mean(WhitenedPower(FooofPowerFrequencies==FrequencyIdx));

            SpectraRedux.Power(NewIdx, FrequencyIdx) = ...
                mean(mean(log10(Power(NotEdgeChanIndex, PowerFrequencies==FrequencyIdx))));
        end
    end
    disp(num2str(RecordingIdx))
    disp(['Duration: ', num2str(toc(A))])
end

Metadata = TaskMetadata;
FrequenciesRedux(end) = []; % remove last edge;

% save
save(fullfile(CacheDir, CacheName), 'Metadata',  'Topographies', 'TopographiesBands', ...
    "SpectraRedux", 'FrequenciesRedux', 'Chanlocs', 'Frequencies', 'SpectraAverage')
