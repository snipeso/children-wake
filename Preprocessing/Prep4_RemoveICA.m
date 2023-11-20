% Removes bad components in EEG automatically, based on ICLabel's
% classification.

close all
clc
clear

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

P = prepParameters();
Paths = P.Paths;
Datasets = P.Datasets;
Datasets = {'SleepLearning'};
Parameters = P.Parameters;
EEG_Channels = P.EEG_Channels;

Refresh = false;

Spread = 0; % how many times more the main component has to be larger than the next largest component
SlopeRange = [8 30];
MuscleSlopeMin = -.5;
RemoveComps = [2, 3, 4, 6]; % 1:Brain, 2:Muscle, 3:Eye, 4:Heart, 5:Line Noise, 6:Channel Noise, 7:Other
MinTime = 60; % minimum time to keep data in seconds
MinNeighborCorrelation = .5;
MinDataKeep = .15; % proportion of noise in data as either channel or segment, above which the channel/segment is tossed
MinChannels = 25; % maximum number of channels that can be removed

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get paths
Source_Power = fullfile(Paths.Preprocessed, 'Power', 'MAT');
Source_Components = fullfile(Paths.Preprocessed, 'ICA', 'Components');

Destination_All = fullfile(Paths.Preprocessed, 'Power', 'Clean');
Destination_All_Rejects = fullfile(Paths.Preprocessed, 'ICA', 'Cleanable');

% prepare final chanlocs, without the external channels
load('StandardChanlocs128.mat', 'StandardChanlocs')
load('Cz.mat', 'CZ')
FinalChanlocs = StandardChanlocs;
FinalChanlocs(ismember({StandardChanlocs.labels}, string(EEG_Channels.notEEG))) = [];
FinalChanlocs(end+1) = CZ;

% loop through folders and files
for Indx_D = 1:numel(Datasets)

    Dataset = Datasets{Indx_D};
    Tasks = getContent(fullfile(Source_Power, Dataset));

    for Indx_T = 1:numel(Tasks)

        Task = Tasks{Indx_T};
        Source = fullfile(Source_Components, Dataset, Task);
        Destination = fullfile(Destination_All, Dataset, Task);
        Destination_Rejects = fullfile(Destination_All_Rejects, Dataset, Task);

        if ~exist(Destination, 'dir')
            mkdir(Destination)
        end


        if ~exist(Destination_Rejects, 'dir')
            mkdir(Destination_Rejects)
        end

        Files = getContent(Source);
        Files(~contains(Files, '.mat'))=  [];

        for Indx_F = 1:numel(Files)

            %%% load data

            File = Files{Indx_F};

            % skip if file already exists
            if ~Refresh && exist(fullfile(Destination, File), 'file')
                disp(['***********', 'Already did ', File, '***********'])
                continue
            end

            % load data from which you want to remove components
            Filepath_Power = fullfile(Source_Power, Dataset, Task, File);
            load(Filepath_Power, 'EEG')
            Data = EEG;

            % load ICA components
            Filepath_ICA = fullfile(Source, File);
            load(Filepath_ICA, 'EEG')



            %%% preprocess data

            % remove bad channels
            Data = pop_select(Data, 'nochannel', ...
                labels2indexes([EEG_Channels.notEEG, EEG.badchans], Data.chanlocs));

            % add CZ
            Data.data(end+1, :) = zeros(1, size(Data.data, 2));
            Data.chanlocs(end+1) = CZ;

            % rereference to average
            Data = pop_reref(Data, []);


            %%% remove major artifact components

            Components = EEG.etc.ic_classification.ICLabel.classifications;

            % assign one category to each component
            Top = topComp(Components, Spread);

            % identify slope in beta-gamma range
            [Slopes, ~] = getSlopes(EEG, SlopeRange, 'ICA', 'fooof');

            % if anything classified as noise or other that has a flat
            % slope, or tilts positive, then its muscle activity.
            Top(ismember(Top, [5 6 7]) & Slopes>=MuscleSlopeMin) = 2;

            % remove Muscle, Eye, and Heart components
            Rejects = ismember(Top, RemoveComps);
            EEG.reject.gcompreject = Rejects';

            % save separate location of removed comps
             save(fullfile(Destination_Rejects, File), 'EEG')


            % create new data structure with ICA metadata, and EEG data
            NewEEG = EEG; % gets everything from IC structure
            NewEEG.data = Data.data; % replace data
            NewEEG.pnts = Data.pnts; % replaces data related fields
            NewEEG.srate = Data.srate;
            NewEEG.xmax = Data.xmax;
            NewEEG.times = Data.times;
            NewEEG.event = Data.event;
            NewEEG.icaact = [];

            % remove components
            badcomps = find(EEG.reject.gcompreject); % get indexes of selected components
            NewEEG = pop_subcomp(NewEEG, badcomps);


            % re-check for bad channels (in lower frequencies)
            [~, BadChannels, BadWindows] = findBadSegments(NewEEG, 5, MinNeighborCorrelation, ...
                EEG_Channels.notEEG, true, MinDataKeep);
            NewEEG.data(:, BadWindows) = [];
            NewEEG = pop_select(NewEEG, 'nochannel', BadChannels);

            if size(NewEEG.data, 1)< 122-MinChannels % min channels other than external ones
                warning(['Removed too many channel in ', File])
                continue
            end


            % strict cleaning of data
            NewEEG = clean_artifacts(NewEEG, ...
                'FlatlineCriterion', 'off', ...
                'Highpass', 'off', ...
                'ChannelCriterion', 'off', ...
                'LineNoiseCriterion', 'off', ...
                'BurstRejection', 'off',...
                'BurstCriterion', 'off', ...
                'BurstCriterionRefMaxBadChns', 'off', ...
                'WindowCriterion', .1); % fairly agressively remove bad data

            if size(NewEEG.data, 2) < NewEEG.srate*MinTime
                warning(['Removed too many timepoints removed in ', File])
                continue
            end

            % interpolate bad channels
            NewEEG = pop_interp(NewEEG, FinalChanlocs);


            % save
            EEG = NewEEG;
            save(fullfile(Destination, File), 'EEG')

            disp(['Finished ', File])
        end
    end
end


