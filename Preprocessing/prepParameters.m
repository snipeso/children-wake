function P = prepParameters()
% Here is located all the common variables, paths, and parameters that get
% repeatedly called by more than one preprocessing script.

P.Datasets = {'ADHD', 'BMS', 'BMSSL', 'SleepLearning', 'Providence', 'BMSAdults'};
P.LineNoise.ADHD = 50;
P.LineNoise.BMS = 50;
P.LineNoise.BMSSL = 50;
P.LineNoise.SleepLearning = 50;
P.LineNoise.Providence = 60;
P.LineNoise.BMSAdults = 50;

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

P.Paths = Paths;

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

P.EEG_Channels = EEG_Channels;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

% Cleaning: data for quickly scanning data and selecting bad timepoints
Parameters.Cutting.fs = 125; % new sampling rate
Parameters.Cutting.lp = 40; % low pass filter
Parameters.Cutting.hp = 0.5; % high pass filter
Parameters.Cutting.hp_stopband = 0.25; % high pass filter gradual roll-off to this freuqency

% Power: starting data for properly cleaned wake data
Parameters.Power.fs = 250; % new sampling rate
Parameters.Power.lp = 40; % low pass filter
Parameters.Power.hp = 0.5; % high pass filter
Parameters.Power.hp_stopband = 0.25; % high pass filter gradual roll-off

% ICA: heavily filtered data for getting ICA components
Parameters.ICA.fs = 500; % new sampling rate
Parameters.ICA.lp = 100; % low pass filter
Parameters.ICA.hp = 2.5; % high pass filter
Parameters.ICA.hp_stopband = 1.5; % high pass filter gradual roll-off

% Scoring: has special script for running this
Parameters.Scoring.fs = 128;
Parameters.Scoring.SpChannel = 6;
Parameters.Scoring.lp = 40; % low pass filter
Parameters.Scoring.hp = .5; % high pass filter
Parameters.Scoring.hp_stopband = .2; % high pass filter gradual roll-off

% ERP: starting data for properly cleaned ERPs
Parameters.ERP.fs = 250; % new sampling rate
Parameters.ERP.lp = 40; % low pass filter
Parameters.ERP.hp = 0.1; % high pass filter
Parameters.ERP.hp_stopband = 0.05; % high pass filter gradual roll-off

Parameters.Microsleep.fs = 200; % new sampling rate
Parameters.Microsleep.lp = 70; % low pass filter
Parameters.Microsleep.hp = 0.3; % high pass filter
Parameters.Microsleep.hp_stopband = 0.1; % high pass filter gradual roll-off

% Waves: starting data for properly cleaned wake data
Parameters.Waves.fs = 1000; % new sampling rate
Parameters.Waves.lp = 40; % low pass filter
Parameters.Waves.hp = 0.5; % high pass filter
Parameters.Waves.hp_stopband = 0.25; % high pass filter gradual roll-off

P.Parameters = Parameters;

% Trigger_Padding = 1; % amount of time in seconds to keep around start and stop triggers


