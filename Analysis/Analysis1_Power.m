% Detects bursts in EEG data, saves them. Can take >6 h to run.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% load in and set parameters for analysis

% set parameters for how you want to run the script this time
RerunAnalysis = false; % false to skip files already analyzed


% load in parameters that are in common across scripts
Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
TaskList = Parameters.Tasks;

WelchWindow = 4; % duration of window to do FFT
Overlap = .5; % overlap of hanning windows for FFT

Tag = ['window',num2str(WelchWindow), 's_allt'];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analysis

for DatasetCell = Datasets
    Dataset = DatasetCell{1};
    Tasks = TaskList.(Dataset);


    for TaskCell = Tasks
        Task = TaskCell{1};

        % set paths and files
        EEGSource = fullfile(Paths.CleanEEG, Dataset, Task);
        Destination = fullfile(Paths.AnalyzedData, 'EEG', 'Power', Tag, Dataset, Task);
        if ~exist(Destination, 'dir')
            mkdir(Destination)
        end

        %%% run
        Filenames = list_filenames(EEGSource);
        if isempty(Filenames)
            disp(['Skipping ' EEGSource])
            continue
        end

        parfor FilenameIdx = 1:numel(Filenames)
            Filename = Filenames{FilenameIdx};
            % for Filename = Filenames'

            % load data
            if exist(fullfile(Destination, Filename), 'file') && ~RerunAnalysis
                disp(['Skipping ', Filename])
                continue
            else
                disp(['Loading ', Filename])
            end

            Output = load(fullfile(EEGSource, Filename), 'EEG');
            SampleRate = Output.EEG.srate;
            Chanlocs =  Output.EEG.chanlocs;
            Duration = size(Output.EEG.data, 2)/SampleRate;

            [Power, Freqs] = cycy.utils.compute_power(Output.EEG.data, SampleRate, WelchWindow, Overlap);

            % keep track of how much data is being used
            EEGMetadata = Output.EEG;
            EEGMetadata.data = [];
            EEGMetadata.pnts = size(Output.EEG.data, 2); % just making sure its correct
            EEGMetadata.data = []; % only save the metadata

            % save
            parsave_file(fullfile(Destination, Filename), Power, Freqs, Duration, Chanlocs, EEGMetadata)
            disp(['Finished ', Filename])
        end
    end
end



function parsave_file(Destination, Power, Freqs, Duration, Chanlocs, EEGMetadata)

save(Destination, 'Power', 'Freqs', 'Duration', 'Chanlocs', 'EEGMetadata')

end
