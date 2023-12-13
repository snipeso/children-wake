% Detects bursts in EEG data, saves them. Can take >6 h to run.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% load in and set parameters for analysis

% set parameters for how you want to run the script this time
RunParallelBurstDetection = false; % true for faster processing
RerunAnalysis = false; % false to skip files already analyzed

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
Datasets = Parameters.Datasets;
TaskList = Parameters.Tasks;
Bands = Parameters.Narrowbands;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analysis

for DatasetCell = Datasets
        Dataset = DatasetCell{1};
        Tasks = TaskList.(Dataset);

    for TaskCell = Tasks
        Task = TaskCell{1};

        % set paths and files
        EEGSource = fullfile(Paths.CleanEEG, Dataset, Task);
        Destination = fullfile(Paths.AnalyzedData, 'EEG', 'Bursts', Dataset, Task);
        if ~exist(Destination, 'dir')
            mkdir(Destination)
        end

        %%% run
        Filenames = list_filenames(EEGSource);
        if isempty(Filenames)
            disp(['Skipping ' EEGSource])
            continue
        end

        for Filename = Filenames'

            % load data
            if exist(fullfile(Destination, Filename), 'file') && ~RerunAnalysis
                disp(['Skipping ', Filename])
                continue
            else
                disp(['Loading ', Filename])
            end

            load(fullfile(EEGSource, Filename), 'EEG')
            SampleRate = EEG.srate;
            KeepTimepoints = ones(1, size(EEG.data, 2));

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
            EEGMetadata.data = []; % only save the metadata

            % save
            save(fullfile(Destination, Filename), 'Bursts', 'BurstClusters', 'EEGMetadata')
            disp(['Finished ', Filename])
        end
    end
end
