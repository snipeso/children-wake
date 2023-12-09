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
AllParameters = P.Parameters;
OverheadLinenoise = P.LineNoise;

Refresh = false;

Template = '000';
Ignore = {};
Destination_Formats = {'ICA', 'Power'}; % chooses which filtering to do
% options: 'Scoring', 'Cutting', 'ICA', 'Power'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Dataset_Path = Paths.Datasets;
Preprocessed_Path = Paths.Preprocessed;

DataTable = assemble_codes(Paths, Datasets, Template, Ignore);

for Indx_D = 1:numel(Datasets)

    % Folders where raw data is located
    Dataset = Datasets{Indx_D};
    [Subfolders, Participants] = gather_folder_paths(fullfile(Paths.Datasets, Dataset), ...
        Template, false, Ignore);

    % Consider only relevant subfolders
    Subfolders(~(contains(Subfolders, 'New_Export')| contains(Subfolders, 'NewExport'))) = []; % start from new export, will check old if its empty

    for Indx_P = 1:numel(Participants)
    % parfor Indx_P = 1:numel(Participants) % loop through participants
        Participant = Participants{Indx_P};
        DT = DataTable;

        Index = find(strcmpi(DT.OldName, Participant));
        Participant_NewID = DT.NewName{Index};

        for Indx_DF = 1:numel(Destination_Formats)
            Destination_Format = Destination_Formats{Indx_DF};


            for Indx_SF = 1:size(Subfolders, 1) % loop through all subfolders

                AllParams = AllParameters;
                LineNoise = OverheadLinenoise;
                Parameters =  AllParams.(Destination_Format);
                Parameters.line = LineNoise.(Dataset);

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
                Content = list_filenames(Path);
                MAT = Content(contains(string(Content), '.mat'));
                MAT(strcmp(MAT, '.mat')) = []; % weird bug that had some files without anything??

                if numel(MAT)<1
                    warning([Path, ' is missing MAT file'])

                    % see if there's an old one
                    Path = replace(Path, 'New_Export', 'Old_Export');
                    Path = replace(Path, 'NewExport', 'OldExport');

                    Content = list_filenames(Path);
                    MAT = Content(contains(string(Content), '.mat'));
                    if numel(MAT)<1
                        warning([Path, ' is missing MAT file'])
                        continue
                    else
                        warning([Path ' using old export'])
                    end

                    Levels{end} = 'o'; % indicate that its old
                end

                % set up destination location
                Destination = fullfile(Preprocessed_Path, Destination_Format, 'MAT', Dataset, Task);
                if ~exist(Destination, 'dir')
                    mkdir(Destination)
                end


                Filename_Core = strjoin([Participant_NewID, Dataset, Levels(:)', num2str(numel(MAT))], '_');
                Filename_Destination = [Filename_Core, '.mat'];

                % skip filtering if file already exists
                if ~Refresh && exist(fullfile(Destination, Filename_Destination), 'file')
                    disp(['***********', 'Already did ', Filename_Core, '***********'])
                    continue
                end

                % load all mat files in folder, merging them
                ALLEEG = struct();
                for Indx_F = 1:numel(MAT)

                    Filename_MAT = MAT(Indx_F);


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

                    EEG = preprocess_eeg(EEG,  Parameters);
                    EEG.roi = []; % weird problem at some point


                    ALLEEG = cat_struct(ALLEEG, EEG);

                end

                % merge together multiple files in same folder
                if isempty(ALLEEG)
                    continue
                elseif numel(ALLEEG)>1
                    EEG = pop_mergeset(ALLEEG, 1:numel(ALLEEG));
                else
                    EEG = ALLEEG;
                end


                % save preprocessing info in eeg structure
                EEG.setname = Filename_Core;
                EEG.filename = Filename_Destination;
                EEG.original.filename = Filename_MAT;
                EEG.original.filepath = Path;
                EEG.originl.participant = Participant;
                EEG.filtering = Parameters;

                EEG = eeg_checkset(EEG);

                % save EEG
                Filepath = fullfile(Destination, Filename_Destination);
                parsave(Filepath, EEG)
            end
        end
        disp(['************** Finished ',  Participant, '***************'])
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% functions

function DataTable = assemble_codes(Paths, Datasets, Template, Ignore)

% check if there's already a table
if exist(fullfile(Paths.Metadata, 'ParticipantCodes.csv'), 'file')
    OldTable = readtable(fullfile(Paths.Metadata, 'ParticipantCodes.csv'));
    DataStruct = table2struct(OldTable);
    ParticipantIDs = max(str2double(extractAfter(OldTable.NewName, 'P'))); % largest code previously assigned
else
    ParticipantIDs = 0; % creates new sequential code for all participants if first time running
    DataStruct = struct(); % keep track of participant codes
end


% gather all the participants for the current dataset
AllParticipants = [];
AllDatasets = [];
for Indx_D = 1:numel(Datasets)

    % Folders where raw data is located
    Dataset = Datasets{Indx_D};

    [~, Participants] = gather_folder_paths(fullfile(Paths.Datasets, Dataset), ...
        Template, false, Ignore);

    AllParticipants = cat(1, AllParticipants, Participants);
    AllDatasets = cat(1, AllDatasets, repmat(string(Dataset), numel(Participants), 1));
end

for Indx_P = 1:numel(AllParticipants)

    if exist("OldTable", 'var') && any(strcmpi(OldTable.OldName, AllParticipants{Indx_P})) % find existing code
        continue
    else % create new code
        ParticipantIDs = ParticipantIDs + 1; % get a new ID number.
        Participant_NewID = ['P', num2str(ParticipantIDs, '%03.f')];

        DataStruct(ParticipantIDs).OldName = AllParticipants{Indx_P};
        DataStruct(ParticipantIDs).NewName = Participant_NewID;
        DataStruct(ParticipantIDs).Dataset = AllDatasets{Indx_P};
    end
end

DataTable = struct2table(DataStruct);
writetable(DataTable, fullfile(Paths.Metadata, 'ParticipantCodes.csv'))
end




function parsave(Filepath, EEG)
clc
save(Filepath, 'EEG')
disp(['saving ', Filepath])
end


