% Detects bursts in EEG data, saves them. Can take >6 h to run.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% load in and set parameters for analysis

% set parameters for how you want to run the script this time
RunParallelBurstDetection = true; % true for faster processing
RerunAnalysis = true; % false to skip files already analyzed

%%% criteria to find bursts in single channels
% irregular shaped bursts, few criteria, but needs more cycles
Idx = 1; % this is to make it easier to skip some
CriteriaSets = struct();
CriteriaSets(Idx).PeriodConsistency = .6;
CriteriaSets(Idx).AmplitudeConsistency = .6;
CriteriaSets(Idx).MonotonicityInAmplitude = .6;
CriteriaSets(Idx).FlankConsistency = .6;
CriteriaSets(Idx).MinCyclesPerBurst = 5;
% % without periodneg, to capture bursts that accelerate/decelerate

% short bursts, strict monotonicity requirements
Idx = Idx+1;
CriteriaSets(Idx).PeriodNeg = true;
CriteriaSets(Idx).PeriodConsistency = .7;
CriteriaSets(Idx).FlankConsistency = .3;
CriteriaSets(Idx).MonotonicityInAmplitude = .9;
CriteriaSets(Idx).MinCyclesPerBurst = 3;

% relies on shape but low other criteria; gets most of the bursts
Idx = Idx+1;
CriteriaSets(Idx).PeriodNeg = true;
CriteriaSets(Idx).PeriodConsistency = .5;
CriteriaSets(Idx).AmplitudeConsistency = .4;
CriteriaSets(Idx).FlankConsistency = .5;
CriteriaSets(Idx).ShapeConsistency = .2;
CriteriaSets(Idx).MonotonicityInTime = .4;
CriteriaSets(Idx).MonotonicityInAmplitude = .4;
CriteriaSets(Idx).ReversalRatio = .6;
CriteriaSets(Idx).MinCyclesPerBurst = 4;

MinClusteringFrequencyRange = 1; % to cluster bursts across channels


% load in parameters that are in common across scripts
Parameters = analysisParameters();
Paths = Parameters.Paths;
Task = Parameters.Task;
Sessions = Parameters.Sessions.(Task);
Bands = Parameters.Narrowbands;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analysis

% set paths and files
EEGSource = fullfile(Paths.CleanEEG, Task);
EEGSourceCuts = fullfile(Paths.Data, 'Cutting', 'Cuts', Task); % timepoints marked as artefacts
Destination = fullfile(Paths.AnalyzedData, 'EEG', 'Bursts_Lapse-Causes', Task);
if ~exist(Destination, 'dir')
    mkdir(Destination)
end

Filenames = list_filenames(EEGSource);
Filenames(~contains(Filenames, Sessions)) = [];
Filenames(~contains(Filenames, Participants)) = [];


%%% run

for FilenameSource = Filenames'

    % load data
    FilenameDestination = replace(FilenameSource, '_Clean.mat', '.mat');
    FilenameCuts =  replace(FilenameSource, '_Clean.mat', '_Cuts.mat');

    if exist(fullfile(Destination, FilenameDestination), 'file') && ~RerunAnalysis
        disp(['Skipping ', FilenameDestination])
        continue
    else
        disp(['Loading ', FilenameSource])
    end

    load(fullfile(EEGSource, FilenameSource), 'EEG')
    SampleRate = EEG.srate;

    % get timepoints without noise
    CleanTimepoints = identify_clean_timepoints(fullfile(EEGSourceCuts, FilenameCuts), EEG);

    % get timepoints of the task
    TaskPoints = identify_task_timepoints(EEG, Triggers);

    % only use clean task timepoints
    KeepTimepoints = CleanTimepoints & TaskPoints;

    % filter data into narrowbands
    EEGNarrowbands = cycy.filter_eeg_narrowbands(EEG, Bands);

    % apply burst detection
    Bursts = cycy.detect_bursts_all_channels(EEG, EEGNarrowbands, Bands, ...
        CriteriaSets, RunParallelBurstDetection, KeepTimepoints);

    % aggregate bursts into clusters across channels (not really used)
    BurstClusters = cycy.aggregate_bursts_into_clusters(Bursts, EEG, MinClusteringFrequencyRange);

    % remove from Bursts all bursts that didn't make it into a cluster (means it was only in one channel)
    ClusteredBurstIndexes = unique([BurstClusters.ClusterBurstsIdx]);
    Bursts = Bursts(ClusteredBurstIndexes);

    % keep track of how much data is being used
    EEGMetadata = EEG;
    EEGMetadata.data = [];
    EEGMetadata.pnts = size(EEG.data, 2); % just making sure its correct
    EEGMetadata.CleanTaskTimepoints = KeepTimepoints;
    EEGMetadata.CleanTaskTimepointsCount = nnz(KeepTimepoints);
    EEGMetadata.data = []; % only save the metadata

    % save
    save(fullfile(Destination, FilenameDestination), 'Bursts', 'BurstClusters', 'EEGMetadata')
    disp(['Finished ', FilenameSource])
end




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% functions

function CleanTimepoints = identify_clean_timepoints(CutsPath, EEG)
TimepointsCount = size(EEG.data, 2);

if exist(CutsPath, 'file')
    NoiseEEG = remove_noise(EEG, CutsPath);
    CleanTimepoints = ~isnan(NoiseEEG.data(1, :));
else
    CleanTimepoints = ones(1, TimepointsCount);
end
end