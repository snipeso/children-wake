% script to calculate components used to clean data


close all
clc
clear

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

P = prepParameters();
Paths = P.Paths;
Datasets = P.Datasets;
Datasets = {'Providence', 'BMSAdults'};
Parameters = P.Parameters;
EEG_Channels = P.EEG_Channels;

MinNeighborCorrelation = .5;
MinDataKeep = .15; % proportion of noise in data as either channel or segment, above which the channel/segment is tossed
MinChannels = 25; % maximum number of channels that can be removed
MinTime = 60; % ninimum file duration in seconds

Refresh = false;
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Source_All = fullfile(Paths.Preprocessed, 'ICA', 'MAT');
Destination_All = fullfile(Paths.Preprocessed, 'ICA', 'Components');


for Indx_D = 1:numel(Datasets)
    Dataset = Datasets{Indx_D};
    Tasks = list_filenames(fullfile(Source_All, Dataset));

    for Indx_T = 1:numel(Tasks)
        Task = Tasks{Indx_T};

        Source = fullfile(Source_All, Dataset, Task);
        Destination = fullfile(Destination_All, Dataset, Task);

        if ~exist(Destination, 'dir')
            mkdir(Destination)
        end

        Files = list_filenames(Source);
        Files(~contains(Files, '.mat'))=  [];

        for Indx_F = 1:numel(Files)
            File = Files{Indx_F};

            % skip if file already exists
            if ~Refresh && exist(fullfile(Destination, File), 'file')
                disp(['***********', 'Already did ', File, '***********'])
                continue
            end

            % load data
            load(fullfile(Source, File), 'EEG')
            Chanlocs = EEG.chanlocs;

            % convert to double
            EEG.data = double(EEG.data);

            % remove bad channels and really bad timepoints
            [~, BadChannels, BadWindows] = find_bad_segments(EEG, 5, MinNeighborCorrelation, ...
                EEG_Channels.notEEG, true, MinDataKeep);
            EEG.data(:, BadWindows) = [];
            EEG = eeg_checkset(EEG);

            if numel(BadChannels)> MinChannels
                warning(['Removed too many channel in ', File])
                continue
            end

            if size(EEG.data, 2) < EEG.srate*MinTime
                warning(['Removed too many timepoints removed in ', File])
                continue
            end


            % remove maybe other noise (flatlines, and little bad windows)
            EEGHyperclean = clean_artifacts(EEG, ...
                'Highpass', 'off', ...
                'ChannelCriterion', 'off', ...
                'LineNoiseCriterion', 'off', ...
                'BurstRejection', 'off',...
                'BurstCriterion', 'off', ...
                'BurstCriterionRefMaxBadChns', 'off', ...
                'WindowCriterion', 'off'); % very agressively remove bad data

            FlatChannels = str2double(setdiff({EEG.chanlocs.labels}, {EEGHyperclean.chanlocs.labels}));


            % save info of which are bad channels
            EEG.badchans = [EEG_Channels.notEEG, BadChannels, FlatChannels];
            if numel([EEG.badchans])> MinChannels
                warning(['Removed too many channel in ', File])
                continue
            end

            % remove really bad channels
            EEG = pop_select(EEG, 'nochannel', EEG.badchans);

            % remove mildly bad timepoints
            EEG = clean_artifacts(EEG, ...
                'FlatlineCriterion', 'off', ...
                'Highpass', 'off', ...
                'ChannelCriterion', 'off', ...
                'LineNoiseCriterion', 'off', ...
                'BurstRejection', 'off',...
                'BurstCriterion', 'off', ...
                'BurstCriterionRefMaxBadChns', 'off', ...
                'WindowCriterion', .1); % very agressively remove bad data

            if size(EEG.data, 2) < EEG.srate*MinTime
                warning(['Removed too many timepoints removed in ', File])
                continue
            end

            % add Cz
            EEG = add_cz(EEG);

            % rereference to average
            EEG = pop_reref(EEG, []);

            % run ICA (takes a while)
            Rank = sum(eig(cov(double(EEG.data'))) > 1E-7);
            if Rank ~= size(EEG.data, 1)
                warning(['Applying PCA reduction for ', File])
            end

            % calculate components
            EEG = pop_runica(EEG, 'runica', 'pca', Rank);

            % classify components
            EEG = iclabel(EEG);

            save(fullfile(Destination, File), 'EEG')
            disp(['***********', 'Finished ', File, '***********'])
        end
    end
end






