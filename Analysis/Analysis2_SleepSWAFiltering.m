% slow part that filters sleep data for slow wave detection

clear
clc
close all

% load in parameters that are in common across scripts
Parameters = analysisParameters();
Paths = Parameters.SleepPaths;
Datasets = Parameters.Datasets;
TaskList = Parameters.Tasks;
RerunAnalysis = false; % false to skip files already analyzed


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analysis

for DatasetCell = Datasets
    Dataset = DatasetCell{1};
    Tasks = TaskList.(Dataset);

    % set paths and files
    EEGSource = fullfile(Paths.CleanEEG, Dataset);
    Destination = fullfile(Paths.DeltaFilter, Dataset);
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

        load(fullfile(EEGSource, Filename), 'EEG')

        % filter in delta range
        EEG = kispi_delta_filter(EEG);

         save(fullfile(Destination, Filename), 'EEG', '-v7.3')
            disp(['Finished ', Filename])
    end
end


