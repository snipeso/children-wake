function Parameters = prepParameters()
% Here is located all the common variables, paths, and parameters that get
% repeatedly called by more than one preprocessing script.

Parameters.Datasets = {'ADHD', 'BMS', 'BMSSL', 'SleepLearning', 'Providence', 'BMSAdults'};
Parameters.LineNoise.ADHD = 50;
Parameters.LineNoise.BMS = 50;
Parameters.LineNoise.BMSSL = 50;
Parameters.LineNoise.SleepLearning = 50;
Parameters.LineNoise.Providence = 60;
Parameters.LineNoise.BMSAdults = 50;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Locations

Paths = struct(); % I make structs of variables so they don't flood the workspace

% get path where these scripts were saved
Paths.Analysis = mfilename('fullpath');
Paths.Analysis = extractBefore(Paths.Analysis, 'Preprocessing');

Core ='D:\Data\AllWake';
% Core ='E:\Data\';

Paths.Datasets = fullfile(Core, 'Raw');
Paths.Preprocessed = fullfile(Core, 'Preprocessed');
Paths.Final = fullfile(Core, 'Final'); % where data gets saved once its been turned into something else
Paths.Core = Core;
Paths.Metadata = fullfile(Core, 'Metadata');
Paths.Errors = fullfile(Core, 'Errors');

Parameters.Paths = Paths;

% add location of subfunctions
addpath(fullfile(Paths.Analysis, 'functions', 'general'))
Subfunctions = list_filenames(fullfile(Paths.Analysis, 'functions'));
for Indx_F = 1:numel(Subfunctions)
    addpath(fullfile(Paths.Analysis, 'functions',Subfunctions{Indx_F}))
end

% run(fullfile(Paths.Analysis, 'functions', 'external', 'addExternalFunctions'))

% if eeglab has not run, run it so all the subdirectories get added
if ~exist('topoplot', 'file')
    eeglab
    close all
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Variables

% EEG channels
EEG_Channels = struct();
EEG_Channels.notEEG = [49, 56, 107, 113, 126, 127];
EEG_Channels.notSourceLoc = [EEG_Channels.notEEG, 48, 119, 17];
EEG_Channels.Edges = [EEG_Channels.notEEG, 48, 63, 68, 73, 81, 88, 94, 99, 119, 125, 128, 8, 25, 17];

Parameters.EEG_Channels = EEG_Channels;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

% Cleaning: data for quickly scanning data and selecting bad timepoints
PreprocessingParameters.Cutting.fs = 125; % new sampling rate
PreprocessingParameters.Cutting.lp = 40; % low pass filter
PreprocessingParameters.Cutting.hp = 0.5; % high pass filter
PreprocessingParameters.Cutting.hp_stopband = 0.25; % high pass filter gradual roll-off to this freuqency

% Power: starting data for properly cleaned wake data
PreprocessingParameters.Power.fs = 250; % new sampling rate
PreprocessingParameters.Power.lp = 40; % low pass filter
PreprocessingParameters.Power.hp = 0.5; % high pass filter
PreprocessingParameters.Power.hp_stopband = 0.25; % high pass filter gradual roll-off

% ICA: heavily filtered data for getting ICA components
PreprocessingParameters.ICA.fs = 500; % new sampling rate
PreprocessingParameters.ICA.lp = 100; % low pass filter
PreprocessingParameters.ICA.hp = 2.5; % high pass filter
PreprocessingParameters.ICA.hp_stopband = 1.5; % high pass filter gradual roll-off

% Scoring: has special script for running this
PreprocessingParameters.Scoring.fs = 128;
PreprocessingParameters.Scoring.SpChannel = 6;
PreprocessingParameters.Scoring.lp = 40; % low pass filter
PreprocessingParameters.Scoring.hp = .5; % high pass filter
PreprocessingParameters.Scoring.hp_stopband = .2; % high pass filter gradual roll-off

% ERP: starting data for properly cleaned ERPs
PreprocessingParameters.ERP.fs = 250; % new sampling rate
PreprocessingParameters.ERP.lp = 40; % low pass filter
PreprocessingParameters.ERP.hp = 0.1; % high pass filter
PreprocessingParameters.ERP.hp_stopband = 0.05; % high pass filter gradual roll-off

PreprocessingParameters.Microsleep.fs = 200; % new sampling rate
PreprocessingParameters.Microsleep.lp = 70; % low pass filter
PreprocessingParameters.Microsleep.hp = 0.3; % high pass filter
PreprocessingParameters.Microsleep.hp_stopband = 0.1; % high pass filter gradual roll-off

% Waves: starting data for properly cleaned wake data
PreprocessingParameters.Waves.fs = 1000; % new sampling rate
PreprocessingParameters.Waves.lp = 40; % low pass filter
PreprocessingParameters.Waves.hp = 0.5; % high pass filter
PreprocessingParameters.Waves.hp_stopband = 0.25; % high pass filter gradual roll-off

Parameters.Parameters = PreprocessingParameters;

% Trigger_Padding = 1; % amount of time in seconds to keep around start and stop triggers


