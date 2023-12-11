

close all
clc
clear

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

P = prepParameters();
Paths = P.Paths;

Parameters = P.Parameters;
EEG_Channels = P.EEG_Channels;

MinNeighborCorrelation = .5;
MinDataKeep = .15; % proportion of noise in data as either channel or segment, above which the channel/segment is tossed
MinChannels = 25; % maximum number of channels that can be removed
MinTime = 60; % ninimum file durationin seconds

Refresh = false;

Source = 'D:\Data\AllWake\Preprocessed\ICA\Components\Providence\Oddball';
Destination = 'D:\Data\AllWake\Preprocessed\ICA\ManualScoring\Providence\Oddball';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~exist(Destination, 'dir')
    mkdir(Destination)
end

Pix = get(0,'screensize');

AllFiles = list_filenames(Source);

if ~Refresh
    DoneFiles = list_filenames(Destination);
    AllFiles(ismember(AllFiles, DoneFiles)) = [];
    AllFiles = AllFiles(randperm(numel(AllFiles)));
end


for Indx_F = 1:numel(AllFiles)
    File = AllFiles(Indx_F);
    tic

    load(fullfile(Source, File), 'EEG')

    EEG = manuallyRemoveBadComps(EEG);


    EEG.data = [];
    save(fullfile(Destination, File), 'EEG')
    disp(['Finished ', char(File) ' in ' num2str(round(toc)), 's'])
end

