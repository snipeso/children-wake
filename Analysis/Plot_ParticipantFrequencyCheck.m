
clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Hours = Parameters.Hours;
PlotProps = Parameters.PlotProps.Manuscript;

Variables = {''};
VariablesCluster = {};

Frequencies = 4:17;
nFrequencies = numel(Frequencies)-1;
nChans = 109;
nVariables = 3;

Source = fullfile(Paths.AnalyzedData, 'EEG', 'Bursts');

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

if ~exist(CacheDir, 'dir')
    mkdir(CacheDir)
end

ResultsFolder = fullfile(Paths.Results, 'Bursts');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

Metadata = readtable(fullfile(Paths.Metadata, 'Metadata.csv'));
Metadata = Metadata(contains(Metadata.Dataset, Datasets), :);

[Participants, UniqueIndx] = unique(Metadata.Participant);
UniqueMetadata = Metadata(UniqueIndx, :);

HistogramsQuantity = nan(numel(Participants), nFrequencies, 2);
HistogramsAmplitude = HistogramsQuantity;

for  ParticipantIdx = 1:numel(Participants)
    Dataset = UniqueMetadata.Dataset{ParticipantIdx};
    Participant = UniqueMetadata.Participant{ParticipantIdx};
    Session = Parameters.Sessions.(Dataset){1};

    Task = Parameters.Tasks.(Dataset){1};

    for HourIdx = 1:2

        % load in data
        Path = fullfile(Source, Dataset, Task);
        DataOut = load_datafile(Path, Participant, Session, Hours{HourIdx}, ...
            {'BurstClusters', 'EEGMetadata'}, '.mat');
        if isempty(DataOut); continue; end

        Bursts = DataOut{1};
        EEGMetadata = DataOut{2};
        RecordingDuration = EEGMetadata.times(end)/60; % in minutes

        BurstFrequencies = [Bursts.BurstFrequency];
        DiscreteFrequencies = discretize(BurstFrequencies, Frequencies);
        Histogram = tabulate(DiscreteFrequencies);

        TooFew = Histogram(:, 2) < 10;

        Quantities = Histogram(:, 2)./RecordingDuration;
        Quantities(TooFew) = nan;
        HistogramsQuantity(ParticipantIdx, round(Histogram(:, 1)), HourIdx) = Quantities;

        for FreqIdx = 1:nFrequencies
            if FreqIdx>numel(TooFew) || TooFew(FreqIdx)
                continue
            end
            HistogramsAmplitude(ParticipantIdx, FreqIdx, HourIdx) = mean([Bursts(DiscreteFrequencies==FreqIdx).Amplitude]);
        end
    end
    disp(['finished ', Participant])
end




%%

[~, Indexes] = sort(UniqueMetadata.Age);

FigureDimentions = [10, 15];

FigLabel = 'IndividualFrequencies';

Idx = 1;
figure('Units','normalized','OuterPosition',[0 0 1 1])
for ParticipantIdx = Indexes'

    Data = squeeze(HistogramsQuantity(ParticipantIdx, :, :));
    subplot(FigureDimentions(1), FigureDimentions(2), Idx)
    chART.plot.overlapping_histograms(Data, Frequencies(1:end-1))
    xlim([3 16])
    title(Participants{ParticipantIdx})
    if ParticipantIdx == 1
        legend(Hours)
    end


    Idx = Idx+1;
end

chART.save_figure(FigLabel, ResultsFolder, PlotProps)


%%

FigureDimentions = [10, 15];

FigLabel = 'IndividualAmplitude';

Idx = 1;
figure('Units','normalized','OuterPosition',[0 0 1 1])
for ParticipantIdx =  Indexes'

    Data = squeeze(HistogramsAmplitude(ParticipantIdx, :, :));
    subplot(FigureDimentions(1), FigureDimentions(2), Idx)
    chART.plot.overlapping_histograms(Data, Frequencies(1:end-1))
    xlim([3 16])
    ylim([10 40])
    if ParticipantIdx == 1
        legend(Hours)
    end
    title(Participants{ParticipantIdx})


    Idx = Idx+1;
end
% chART.save_figure(FigLabel, ResultsFolder, PlotProps)


