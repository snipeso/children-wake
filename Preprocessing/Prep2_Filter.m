% Sorts files by relevant folder, and applies selected preprocessing to
% selected task batch.

close all
clc
clear

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

P = prepParameters();
Paths = P.Paths;
Datasets = P.Datasets;
Parameters = P.Parameters;

Refresh = false;

Template = '000';
Ignore = {};
Destination_Formats = {'Cutting', 'ICA', 'Power'}; % chooses which filtering to do
% options: 'Scoring', 'Cutting', 'ICA', 'Power'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ParticipantID = 1;

Dataset_Path = Paths.Datasets;
Preprocessed_Path = Paths.Preprocessed;

DataStruct = struct(); % keep track of participant codes

for Indx_D = 1:numel(Datasets)

    % Folders where raw data is located
    Dataset = Datasets{Indx_D};
    [Subfolders, Participants] = AllFolderPaths(fullfile(Paths.Datasets, Dataset), ...
        Template, false, Ignore);

    % Consider only relevant subfolders
    Subfolders(contains(Subfolders, 'Old_Export')) = [];


    for Indx_P = 1:numel(Participants) % loop through participants
        Participant = Participants{Indx_P};

        Participant_NewID = ['P', num2str(ParticipantID, '%03.f')];
        DataStruct(ParticipantID).OldName = Participant;
        DataStruct(ParticipantID).NewName = Participant_NewID;
        DataStruct(ParticipantID).Dataset = Dataset;



        for Indx_DF = 1:numel(Destination_Formats)
            Destination_Format = Destination_Formats{Indx_DF};
            Params =  Parameters.(Destination_Format);

            % set selected parameters
            new_fs = Parameters.(Destination_Format).fs;
            lowpass = Parameters.(Destination_Format).lp;
            highpass = Parameters.(Destination_Format).hp;
            hp_stopband = Parameters.(Destination_Format).hp_stopband;


            for Indx_SF = 1:size(Subfolders, 1) % loop through all subfolders
%                             parfor Indx_SF = 1:size(Subfolders, 1) % loop through all subfolders

                %%%%%%%%%%%%%%%%%%%%%%%%
                %%% Check if data exists


                Path = fullfile(Dataset_Path, Dataset, Participant, Subfolders{Indx_SF});

                % skip rest if folder not found
                if ~exist(Path, 'dir')
                    warning([deblank(Path), ' does not exist'])
                    continue
                end

                % identify meaningful folders traversed
                Levels = split(Subfolders{Indx_SF}, '\');
                Levels(cellfun('isempty',Levels)) = []; % remove blanks
                Levels{end} = 'n'; % indicate that it's new
                Levels = replace(Levels, '_', '');

                Task = Levels{end-1}; % task is assumed to be the first folder in the sequence

                % if does not contain EEG, then skip
                Content = getContent(Path);
                MAT = Content(contains(string(Content), '.mat'));
                MAT(strcmp(MAT, '.mat')) = []; % weird bug that had some files without anything??

                if numel(MAT)<1
                    warning([Path, ' is missing SET file'])

                    % see if there's an old one
                    Path = replace(Path, 'New_Export', 'Old_Export');

                    Content = getContent(Path);
                    MAT = Content(contains(string(Content), '.mat'));
                    if numel(MAT)<1
                        warning([Path, ' is missing SET file'])
                        continue
                    end

                    Levels{end} = 'o'; % indicate that its old
                end

                % set up destination location
                Destination = fullfile(Preprocessed_Path, Destination_Format, 'MAT', Dataset, Task);
                if ~exist(Destination, 'dir')
                    mkdir(Destination)
                end

                for Indx_F = 1:numel(MAT)

                    Filename_MAT = MAT(Indx_F);
                    Filename_Core = strjoin([Participant_NewID, Dataset, Levels(:)', num2str(Indx_F)], '_');
                    Filename_Destination = [Filename_Core, '.mat'];

                    % skip filtering if file already exists
                    if ~Refresh && exist(fullfile(Destination, Filename_Destination), 'file')
                        disp(['***********', 'Already did ', Filename_Core, '***********'])
                        continue
                    end


                    %%%%%%%%%%%%%%%%%%%
                    %%% process the data

                    M = load(fullfile(Path, Filename_MAT), 'EEG');

                    EEG = M.EEG;

                    if ~isfield(EEG, 'data')
                        warning(['empty file for ' char(fullfile(Path, Filename_MAT))])
                        continue
                    end

                    if size(EEG.data, 2) < EEG.srate*.5*60
                        warning(['Not enough data for ' char(fullfile(Path, Filename_MAT))])
                        continue
                    end

                    % low-pass filter
                    EEG = pop_eegfiltnew(EEG, [], lowpass); % this is a form of antialiasing, but it not really needed because usually we use 40hz with 256 srate

                    % notch filter for line noise
                    EEG = lineFilter(EEG, 50, false);

                    % resample
                    if EEG.srate ~= new_fs
                        EEG = pop_resample(EEG, new_fs);
                    end

                    % high-pass filter
                    % NOTE: this is after resampling, otherwise crazy slow.
                    EEG = hpEEG(EEG, highpass, hp_stopband);


                    % save preprocessing info in eeg structure
                    EEG.setname = Filename_Core;
                    EEG.filename = Filename_Destination;
                    EEG.original.filename = Filename_MAT;
                    EEG.original.filepath = Path;
                    EEG.originl.participant = Participant;
                    EEG.filtering = Params;

                    EEG = eeg_checkset(EEG);

                    % save EEG
                    Filepath = fullfile(Destination, Filename_Destination);
                    parsave(Filepath, EEG)
                end
            end
        end
        disp(['************** Finished ',  Participant, '***************'])
        ParticipantID = ParticipantID+1;
    end
end

DataTable = struct2table(DataStruct);
writetable(DataTable, fullfile(Paths.Analysis, 'ParticipantCodes.csv'))


function parsave(Filepath, EEG)
save(Filepath, 'EEG')
end
