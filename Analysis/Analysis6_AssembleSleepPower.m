clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
Paths = Parameters.Paths;
SleepPaths = Parameters.SleepPaths;
Datasets = Parameters.Datasets;
EpochLength = 20; % move to parameters
TimeToKeep = 60*60/EpochLength; % 1 h in epochs
Band = Parameters.PowerBands.Delta;

Source = fullfile(SleepPaths.Power);

CacheDir = Paths.Cache;
CacheName = 'AllSlowWaves.mat';


load(fullfile(CacheDir, CacheName), 'MetadataSleep')
PowerBands = Parameters.PowerBands;
PowerBandLabels = fieldnames(PowerBands);
MetadataSleep.FH_SWA = nan(size(MetadataSleep, 1), 1);
MetadataSleep.LH_SWA = nan(size(MetadataSleep, 1), 1);


MetadataSleep.FH_SWA_raw = nan(size(MetadataSleep, 1), 1);
MetadataSleep.LH_SWA_raw = nan(size(MetadataSleep, 1), 1);


for RecordingIdx = 1:size(MetadataSleep, 1)
    Dataset = MetadataSleep.Dataset{RecordingIdx};
    Participant = MetadataSleep.Participant{RecordingIdx};
    Session = replace(MetadataSleep.Session{RecordingIdx}, '_', '');

    Path = fullfile(Source, Dataset);
    DataOut = load_datafile(Path, Participant, Session, '', ...
        {'EpochPower', 'Frequencies', 'Artefacts', 'Scoring'}, '.mat');
    if isempty(DataOut); continue; end

    Power = DataOut{1};
    Frequencies = DataOut{2};
    Artefacts = DataOut{3};
    Scoring = DataOut{4};

    % remove artefact epochs
    Power(Parameters.Channels.Edge, :, :) = nan;

    for ChannelIdx = 1:size(Artefacts, 1)
        Power(ChannelIdx, Artefacts(ChannelIdx, :)==0 | isnan(Artefacts(ChannelIdx, :)), :) = nan;
    end

    % remove all not N2 or N3
    Power(:, Scoring>-2, :) = nan;

    % select SWA for first and last hour of NREM
    FreqRange = dsearchn(Frequencies', [Band(1); Band(end)]);

    FH_ScoreIndex = find(cumsum(Scoring<=-2) >= TimeToKeep, 1, 'first');
    LH_ScoreIndex =  find(nnz(Scoring<=-2)-cumsum(Scoring<=-2) >=  TimeToKeep, 1, 'last');

    MetadataSleep.FH_SWA(RecordingIdx) =  mean(mean(mean(log10(Power(:, 1:FH_ScoreIndex, FreqRange(1):FreqRange(2))), 2, 'omitnan'), 1, 'omitnan'), 3, 'omitnan');
    MetadataSleep.LH_SWA(RecordingIdx) =  mean(mean(mean(log10(Power(:, LH_ScoreIndex:end, FreqRange(1):FreqRange(2))),  2, 'omitnan'), 1, 'omitnan'), 3, 'omitnan');

    MetadataSleep.FH_SWA_raw(RecordingIdx) =  mean(mean(mean(Power(:, 1:FH_ScoreIndex, FreqRange(1):FreqRange(2)), 2, 'omitnan'), 1, 'omitnan'), 3, 'omitnan');
    MetadataSleep.LH_SWA_raw(RecordingIdx) =  mean(mean(mean(Power(:, LH_ScoreIndex:end, FreqRange(1):FreqRange(2)), 2, 'omitnan'), 1, 'omitnan'), 3, 'omitnan');
    disp(['Finished ', num2str(RecordingIdx)])
end

CacheName = 'MetadataSleep';
save(fullfile(CacheDir, CacheName), 'MetadataSleep')

