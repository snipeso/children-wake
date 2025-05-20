
clear
clc
close all

% load in parameters that are in common across scripts
Parameters = analysisParameters();
Paths = Parameters.SleepPaths;
Datasets = Parameters.Datasets;
TaskList = Parameters.Tasks;
RerunAnalysis = true; % false to skip files already analyzed
EpochLength = 20;
WelchWindowLength = 4;
WelchOverlap = .5;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analysis

for DatasetCell = Datasets
    Dataset = DatasetCell{1};
    Tasks = TaskList.(Dataset);

    % set paths and files
    EEGSource = fullfile(Paths.CleanEEG, Dataset);
    Destination = fullfile(Paths.Power, Dataset);
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


        % interpolate bad channels
        Artefacts(isnan(Artefacts)) = 0; % I set REM and wake to nan; but the scripts assume they are 0s
        EEG = kispi_interpolate_bad_channels(EEG, Artefacts);

        % reref to average
        EEG = pop_reref(EEG, []);

        % detect power
        [EpochPower, Frequencies] = oscip.compute_power_on_epochs(EEG.data, EEG.srate, EpochLength, WelchWindowLength, WelchOverlap);


        save(fullfile(Destination, Filename), 'Scoring', 'Artefacts', 'EpochLength', 'EpochPower', 'Frequencies')
        disp(['Finished ', char(Filename), ' in ', num2str(toc(StartTime)), ' s'])
    end
end

