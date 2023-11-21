function Parameters = analysisParameters()
% parameters for detecting bursts
% Lapses-Causes

Parameters = struct();


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analysis paramaters

% Who, what, when
Parameters.Datasets = {'ADHD', 'BMS', 'BMSSL', 'Koffein', 'SleepLearning'};
Parameters.Tasks = {}; % if is empty, will do all of them
Parameters.Participants = 156;
% Parameters.Datasets = {'BMS'};
% Parameters.Tasks = {'1GoNoGo'};
% Folder structure: Preprocessed > Power > Clean > ADHD > 1Oddball

%%% labels
Parameters.Labels.logBands = [1 2 4 8 16 32]; % x markers for plot on log scale
Parameters.Labels.Bands = [1 4 8 14 25 35 40]; % normal scale
Parameters.Labels.FreqLimits = [1 40];
Parameters.Labels.zPower = 'PSD z-scored';
Parameters.Labels.Power = 'PSD Amplitude (\muV^2/Hz)';
Parameters.Labels.Frequency = 'Frequency (Hz)';
Parameters.Labels.Amplitude = 'Amplitude (\muV)';
Parameters.Labels.Time = 'Time (s)';
Parameters.Labels.t = 't-values';



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Locations

if exist( 'D:\LSM\Preprocessed', 'dir') % KISPI desktop
    Core = 'D:\LSM\';
    addpath('H:\Code\chART')
    addpath('H:\Code\Matcycle')
    addpath('H:\Code\fooof_mat\fooof_mat')
    addpath('\\nausers01\user\sniso\Dokumente\MATLAB\eeglab2022.0')
elseif exist( 'X:\Data\Raw', 'dir')
    Core = 'X:\Data\';
else
    error('no data disk!')
    % Core = 'E:\'
end

Paths.Data = fullfile(Core, 'Preprocessed');
Paths.CleanEEG = fullfile(Paths.Data, 'Power', 'Clean');
Paths.Core = Core;

Paths.AnalyzedData  = fullfile(Core, 'Final'); % where data gets saved once its been turned into something else
Paths.Cache = fullfile(Core, 'Cache', 'children-wake');
Paths.Results = fullfile(Core, 'Results', 'children-wake');

if ~exist(Paths.Results, 'dir')
    mkdir(Paths.Results)
end

% if eeglab has not run, run it so all the subdirectories get added
if ~exist('topoplot', 'file')
    eeglab
    close all
end

% get path where these scripts were saved
CD = mfilename('fullpath');
Paths.Analysis = fullfile(extractBefore(CD, 'children-wake'), 'children-wake');

% get all folders in functions
Subfolders = deblank(string(ls(fullfile(Paths.Analysis, 'functions')))); % all content
Subfolders(contains(Subfolders, '.')) = []; % remove all files

for Indx_F = 1:numel(Subfolders)
    addpath(fullfile(Paths.Analysis, 'functions', Subfolders{Indx_F}))
end

addExternalFunctions

Parameters.Paths = Paths;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EEG info

% bands used to get bursts
Narrowbands.ThetaLow = [2 6];
Narrowbands.Theta = [4 8];
Narrowbands.ThetaAlpha = [6 10];
Narrowbands.Alpha = [8 12];
Narrowbands.AlphaHigh = [10 14];
Narrowbands.Sigma = [12 16];

Parameters.Narrowbands = Narrowbands;

Bands.Theta = [4 8]; % up to but not including the second edge
Bands.Alpha = [8 14]; 
Parameters.Bands = Bands;


Triggers.SyncEyes = 'S192';
Triggers.Start = 'S  1';
Triggers.End = 'S  2';
Triggers.Stim = 'S  3';
Triggers.Resp = 'S  4';
Triggers.FA = 'S  5';
Triggers.StartBlank = 'S  6';
Triggers.EndBlank = 'S  7';
Triggers.Alarm = 'S  8';
Triggers.LeftBlock = 'S 10';
Triggers.RightBlock = 'S 11';
Triggers.Tones = 'S 12';
Parameters.Triggers = Triggers;

Parameters.PlotProps.Manuscript = chART.load_plot_properties({'LSM', 'Manuscript'});
Parameters.PlotProps.Manuscript.Figure.Width = 22;

Parameters.PlotProps.Powerpoint = chART.load_plot_properties({'LSM', 'Powerpoint'});
Parameters.PlotProps.Poster = chART.load_plot_properties({'LSM', 'Poster'});

%%% channel clusters

Frontspot = [22 15 9 23 18 16 10 3 24 19 11 4 124 20 12 5 118 13 6 112];
Backspot = [66 71 76 84 65 70 75 83 90 69 74 82 89];
Centerspot = [129 7 106 80 55 31 30 37 54 79 87 105 36 42 53 61 62 78 86 93 104 35 41 47  52 92 98 103 110, 60 85 51 97];

Channels.PreROI.Front = Frontspot;
Channels.PreROI.Center = Centerspot;
Channels.PreROI.Back = Backspot;

Parameters.Channels = Channels;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% statistics

Stats = struct();

Stats.ANOVA.ES = 'eta2';
Stats.ANOVA.nBoot = 2000;
Stats.ANOVA.pValue = 'pValueGG';
Stats.ttest.nBoot = 2000;
Stats.ttest.dep = 'pdep'; % use 'dep' for ERPs, pdep for power
Stats.Alpha = .05;
Stats.Trend = .1;
Stats.Paired.ES = 'hedgesg';
Parameters.Stats = Stats;
