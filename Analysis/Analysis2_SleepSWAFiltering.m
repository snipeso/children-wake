% slow part that filters sleep data for slow wave detection

clear
clc
close all

% load in parameters that are in common across scripts
Parameters = analysisParameters();
Paths = Parameters.SleepPaths;
Datasets = Parameters.Datasets;
TaskList = Parameters.Tasks;
RerunAnalysis = true; % false to skip files already analyzed


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analysis

for DatasetCell = Datasets
    Dataset = DatasetCell{1};
    Tasks = TaskList.(Dataset);

    % set paths and files
    EEGSource = fullfile(Paths.CleanEEG, Dataset);
    Destination = fullfile(Paths.SlowWaves, Dataset);
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
        
        % load in data
        if exist(fullfile(Destination, Filename), 'file') && ~RerunAnalysis
            disp(['Skipping ', Filename])
            continue
        else
            disp(['Loading ', Filename])
        end

        StartTime = tic;

        load(fullfile(EEGSource, Filename), 'EEG', 'Artefacts', 'Scoring')

        % filter in delta range
        EEG = kispi_delta_filter(EEG);

        % interpolate bad channels
        Artefacts(isnan(Artefacts))= 0; % I set REM and wake to nan; but the scripts assume they are 0s
        EEG = kispi_interpolate_bad_channels(EEG, Artefacts);

        % rereference to linked mastoids
        EEG = pop_reref(EEG, [100 57]); % use net mastoids, since different studies used different location for gold electrodes

        % detect slow waves
        EpochLength = 20;
        CleanEpochs = find(ismember(Scoring, [-2, -3]) & sum(Artefacts)> 100);

        [SW] = kispi_slowwave_detection(EEG.data, EEG.srate, 'NegZeroCrossings', CleanEpochs, EpochLength, 'No');

        save(fullfile(Destination, Filename), 'SW', 'Scoring', 'Artefacts', 'EpochLength')
        disp(['Finished ', Filename, ' in ', num2str(toc(StartTime)), ' s'])
    end
end


