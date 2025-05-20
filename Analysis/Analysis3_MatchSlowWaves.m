% wave matching & sorting

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
Paths = Parameters.Paths;
SleepPaths = Parameters.SleepPaths;
Datasets = Parameters.Datasets;

Source = fullfile(SleepPaths.SlowWaves);

CacheDir = Paths.Cache;
CacheName = 'AllSlowWaves.mat';

if ~exist(CacheDir, 'dir')
    mkdir(CacheDir)
end

% adjust channel indices because they got rereferenced to linked mastoids
ChannelIndexes = 1:129;
ChannelIndexes([57 100]) = []; % because rereferenced to linked mastoids, these channels are no longer in the signal

ExcludeChannels = [43 48 49 56 63 68 73 81 88 94 99 107 113 119 120 125 126 127 128];
ExcludedChannelsNewIndexes = find(ismember(ChannelIndexes, ExcludeChannels));


MetadataSleep = readtable(fullfile(Paths.Metadata, 'Metadata_Children_Sleep.csv'));
MetadataSleep = MetadataSleep(contains(MetadataSleep.Dataset, Datasets), :);
nRecordings = size(MetadataSleep, 1); % this does not consider tasks

Blank = nan(nRecordings, 1);

MetadataSleep.FH_Amplitude = Blank;
MetadataSleep.LH_Amplitude = Blank;

MetadataSleep.FH_Amplitude_Matched = Blank;
MetadataSleep.LH_Amplitude_Matched = Blank;

MetadataSleep.FH_Slope = Blank;
MetadataSleep.LH_Slope = Blank;

MetadataSleep.FH_Slope_Matched = Blank;
MetadataSleep.LH_Slope_Matched = Blank;

MetadataSleep.ProportionMatched = Blank;

for RecordingIdx = 1:nRecordings

    %%% load in data
    Dataset = MetadataSleep.Dataset{RecordingIdx};
    Participant = MetadataSleep.Participant{RecordingIdx};
    Session = replace(MetadataSleep.Session{RecordingIdx}, '_', '');

    Path = fullfile(Source, Dataset);
    DataOut = load_datafile(Path, Participant, Session, '', ...
        {'Scoring', 'SW', 'EpochLength'}, '.mat');
    if isempty(DataOut); continue; end

    Scoring = DataOut{1};
    SW = DataOut{2};
    EpochLength = DataOut{3};
    SampleRate = SW.SampRate;

    %     load('I:\Sleep\Final\EEG\SlowWaves_All\ADHD\P008_ADHD_Session1.mat')

    if isempty(fieldnames(SW.waves))
        warning(['No SW data for ', Participant, Session])
        continue
    end

    % find all the waves in first hour and last hour, excluding epochs and channels that were marked as
    % bad


    %%% prepare to go through all channels
    FH_Amplitudes = [];
    LH_Amplitudes = [];
    FH_Slopes = [];
    LH_Slopes = [];

    FH_Amplitudes_Matched = [];
    LH_Amplitudes_Matched = [];
    FH_Slopes_Matched = [];
    LH_Slopes_Matched = [];


    for ChIndx = 1:numel(SW.waves)

        % skip not EEG channels
        if ismember(ChIndx, ExcludedChannelsNewIndexes)
            continue
        end

        % identify the 1h mark of NREM in samples, and the 1h mark of end
        TimeToKeep = 60*60; % seconds
        FH_ScoreIndex = find(cumsum(Scoring<=-2) >= TimeToKeep/EpochLength, 1, 'first');
        FH_SampleRate = FH_ScoreIndex*EpochLength*SampleRate;

        FH_Indexes = SW.waves(ChIndx).ndx_trough<FH_SampleRate;

        LH_ScoreIndex =  find(nnz(Scoring<=-2)-cumsum(Scoring<=-2) >= TimeToKeep/EpochLength, 1, 'last');
        LH_SampleRate = LH_ScoreIndex*EpochLength*SampleRate;
        LH_Indexes = SW.waves(ChIndx).ndx_trough>LH_SampleRate;


        % only keep waves between 0.25 and 1 s half-period of NZC (in the delta
        % frequency range)
        Delta_Indexes = SW.waves(ChIndx).neg_period>=.25 & SW.waves(ChIndx).neg_period<=1;


        % only keep waves within .01 and .99 amplitude percentile (following VJ's
        % procedure to the letter, not really sure why she does it this way but I imagine it comes from blood, sweat and tears)
        FH_Range = quantile(SW.waves(ChIndx).amp_trough(FH_Indexes & Delta_Indexes), [.1 .99]);
        LH_Range = quantile(SW.waves(ChIndx).amp_trough(LH_Indexes & Delta_Indexes), [.1 .99]);
        CommonAmplitudeRange = [max(FH_Range(1), LH_Range(1)), min(FH_Range(2), LH_Range(2))];

        AmplitudeIndexes = SW.waves(ChIndx).amp_trough >= CommonAmplitudeRange(1) & SW.waves(ChIndx).amp_trough <= CommonAmplitudeRange(2);

        FH_Delta_Indexes = FH_Indexes & Delta_Indexes & AmplitudeIndexes;
        LH_Delta_Indexes = LH_Indexes & Delta_Indexes & AmplitudeIndexes;

        FH_Amps = SW.waves(ChIndx).amp_trough(FH_Delta_Indexes);
        LH_Amps = SW.waves(ChIndx).amp_trough(LH_Delta_Indexes);

        FH_Slps = SW.waves(ChIndx).neg_slope(FH_Delta_Indexes);
        LH_Slps = SW.waves(ChIndx).neg_slope(LH_Delta_Indexes);

        [FH_MatchedWaves, LH_MatchedWaves] = match_sw_amplitudes(FH_Amps, LH_Amps);

        FH_Amplitudes = cat(2, FH_Amplitudes, FH_Amps);
        LH_Amplitudes = cat(2, LH_Amplitudes, LH_Amps);
        FH_Slopes = cat(2, FH_Slopes, FH_Slps);
        LH_Slopes = cat(2, LH_Slopes, LH_Slps);

        FH_Amplitudes_Matched = cat(2, FH_Amplitudes_Matched, FH_Amps(FH_MatchedWaves));
        LH_Amplitudes_Matched = cat(2, LH_Amplitudes_Matched, LH_Amps(LH_MatchedWaves));
        FH_Slopes_Matched = cat(2,  FH_Slopes_Matched, FH_Slps(FH_MatchedWaves));
        LH_Slopes_Matched = cat(2,  LH_Slopes_Matched, LH_Slps(LH_MatchedWaves));
    end

    % Add sleep info
    MetadataSleep.FH_Amplitude(RecordingIdx) = mean(FH_Amplitudes);
    MetadataSleep.LH_Amplitude(RecordingIdx) = mean(LH_Amplitudes);

    MetadataSleep.FH_Amplitude_Matched(RecordingIdx) = mean(FH_Amplitudes_Matched);
    MetadataSleep.LH_Amplitude_Matched(RecordingIdx) = mean(LH_Amplitudes_Matched);

    MetadataSleep.FH_Slope(RecordingIdx) = mean(FH_Slopes);
    MetadataSleep.LH_Slope(RecordingIdx) = mean(LH_Slopes);

    MetadataSleep.FH_Slope_Matched(RecordingIdx) = mean(FH_Slopes_Matched);
    MetadataSleep.LH_Slope_Matched(RecordingIdx) = mean(LH_Slopes_Matched);

    MetadataSleep.ProportionMatched(RecordingIdx) = numel(FH_Slopes_Matched)/numel(LH_Slopes);

    disp(['Finished ',num2str(RecordingIdx) '/', num2str(nRecordings) ])
end

save(fullfile(CacheDir, CacheName), 'MetadataSleep')

