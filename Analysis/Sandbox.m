clear
clc
close all

Info = analysisParameters();
Paths = Info.Paths;
Bands = Info.Narrowbands;
BandLabels = fieldnames(Bands);

%%%% Choose a file
%%%%
%%%%
Source = 'X:\Data\Preprocessed\Power\Clean\SleepLearning\3Oddball';
Filename_Source = 'P115_SleepLearning_Session11_mor_3Oddball_n_1.mat';
%%%%
%%%%
%%%%


load(fullfile(Source, Filename_Source), 'EEG')
SampleRate = EEG.srate;



%% Filter all data in all bands

EEGNarrowbands = cycy.filter_eeg_narrowbands(EEG, Bands);

%% Choose criteria


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% criteria to find bursts in single channels
% irregular shaped bursts, few criteria, but needs more cycles
Idx = 1; % this is to make it easier to skip some
CriteriaSets = struct();
CriteriaSets(Idx).PeriodConsistency = .6;
CriteriaSets(Idx).MonotonicityInAmplitude = .6;
CriteriaSets(Idx).FlankConsistency = .6;
CriteriaSets(Idx).AmplitudeConsistency = .6;
CriteriaSets(Idx).MinCyclesPerBurst = 5;
% % without periodneg, to capture bursts that accelerate/decelerate

% short bursts, strict monotonicity requirements
Idx = Idx+1;
CriteriaSets(Idx).PeriodConsistency = .7;
CriteriaSets(Idx).MonotonicityInAmplitude = .9;
CriteriaSets(Idx).PeriodNeg = true;
CriteriaSets(Idx).FlankConsistency = 0.3;
CriteriaSets(Idx).MinCyclesPerBurst = 3;

% relies on shape but low other criteria; gets most of the bursts
Idx = Idx+1;
CriteriaSets(Idx).PeriodConsistency = .5;
CriteriaSets(Idx).MonotonicityInTime = .4;
CriteriaSets(Idx).MonotonicityInAmplitude = .4;
CriteriaSets(Idx).ReversalRatio = 0.6;
CriteriaSets(Idx).ShapeConsistency = .2;
CriteriaSets(Idx).FlankConsistency = .5;
CriteriaSets(Idx).MinCyclesPerBurst = 3;
CriteriaSets(Idx).AmplitudeConsistency = .4;
CriteriaSets(Idx).MinCyclesPerBurst = 4;
CriteriaSets(Idx).PeriodNeg = true;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Run all criteria on all frequencies on a single channel

close all

%%%% Choose a channel
%%%%
%%%%
Channel = labels2indexes(10, EEG.chanlocs);
%%%%
%%%%
%%%%

% profile on % uncomment to see how long different parts of the process take
Bursts = cycy.detect_bursts(EEG, Channel, EEGNarrowbands,...
    Bands, CriteriaSets);
% profile viewer

cycy.plot.plot_all_bursts(EEG, 15, Bursts, 'Band');

figure
cycy.plot.power_without_bursts(EEG.data(Channel, :), SampleRate, Bursts);

cycy.plot.burst_criteriaset_diagnostics(Bursts);
%% single set

%%%% Choose a single criteria set to investigate for one channel
%%%%
%%%%
CriteriaSetIndex = 3;
Sign = 1;
Band = 'Theta';
%%%%
%%%%
%%%%

BurstsSingle = cycy.test_criteria_set(Sign*EEG.data(Channel, :), ...
    SampleRate, Bands.(Band), CriteriaSets(CriteriaSetIndex));


%% Burst detection in all channels, for all frequencies, and all criteria sets
% detect bursts

%%%% if confident in code, run in parallel; if risks breaking, dont
%%%%
%%%%
RunParallel = true; % if there's a lot of data, channels can be run in parallel
%%%%
%%%%
%%%%

Bursts = cycy.detect_bursts_all_channels(EEG, EEGNarrowbands, Bands, ...
    CriteriaSets, RunParallel); 

MinFrequencyRange = 1;

% aggregate bursts across channels
BurstClusters = cycy.aggregate_bursts_into_clusters(Bursts, EEG, MinFrequencyRange);

%% plot final output in entire EEG

cycy.plot.plot_all_bursts(EEG, 20, BurstClusters, 'Band');

%% Plot bar graph of which criteria set was most influential
cycy.plot.burst_criteriaset_diagnostics(BurstClusters)
