% saves and filters EEG data

close all
clear
clc


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

P = prepParameters();


Source = 'E:\Raw\EEG\';
Destination = 'E:\Preprocessed\';
Refresh = false;
Task = 'RestingState';

Destination_Formats = {'ICA', 'Power', 'Cutting'}; % chooses which filtering to do


Participants = string(deblank(ls(Source)));
Participants(contains(Participants, '.')) = [];

load('StandardChanlocs128.mat', 'StandardChanlocs')

    % parfor Indx_P = 1:numel(Participants) % loop through participants
for ParticipantIdx = 1:numel(Particiants)

            Participant = Participants{Indx_P};

    FilePath = fullfile(Source, Participants{ParticipantIdx}, 'EEG', 'raw', 'mat_format', 'RestingState.mat');
    if ~exist(Filepath, 'file')
        continue
    end

    Output = load(FilePath);

    EEG = Output.EEG;

    EEG.data(129, :) = []; % its nice that they put it there, but I add it in later
    EEG.chanlocs = StandardChanlocs;
    EEG.ref = 'Cz';

    RawEEG = EEG;

    for Indx_DF = 1:numel(Destination_Formats)
        Destination_Format = Destination_Formats{Indx_DF};

        % set up destination location
        DestinationFolder = fullfile(Destination, Destination_Format, 'MAT', Task);
        DestinationFilename = [Participants{ParticipantIdx}, '_RestingState.mat'];
        Filepath_Destination = fullfile(DestinationFolder, DestinationFilename);
        if ~exist(DestinationFolder, 'dir')
            mkdir(DestinationFolder)
        end

        if ~Refresh && exist(Filepath_Destination, 'file')
            disp(['Already did ', Participants{ParticipantIdx}, ' ', DestinationFormat])
            continue
        end

        EEG = filter_and_downsample_eeg(RawEEG,  Parameters);
        EEG.setname = DestinationFilename;
        EEG.filtering = Parameters;

        EEG = eeg_checkset(EEG);
    end
    disp(['************** Finished ',  Participant, '***************'])
end




function parsave(Filepath, EEG)
clc
save(Filepath, 'EEG')
disp(['saving ', Filepath])
end

