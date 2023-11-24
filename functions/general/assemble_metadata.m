function Metadata = assemble_metadata(MetadataPath, Datasets)
% creates a table with columns:
% Dataset, Participant, Session, Hour (mor/eve), Sex, Handedness, Age,
% Group, Subgroup (Melanie devised split by medication), Condition

ParticipantCodes = readtable(fullfile(MetadataPath, "ParticipantCodes.csv"));

Metadata = table();
for Dataset = Datasets

    % load in CSVs
    ParticipantInfo = readtable(fullfile(MetadataPath, ['Participants_', Dataset{1}, '.csv']));
    SessionInfo = readtable(fullfile(MetadataPath, ['Data_', Dataset{1}, '.csv']));

    for SessionIdx = 1:size(SessionInfo, 1)
        Row = table();
        Row.Dataset = Dataset;
        OldName = SessionInfo.Participant(SessionIdx);
        NewName = ParticipantCodes.NewName(strcmp(ParticipantCodes.OldName, OldName));
        if isempty(NewName)
            warning(['Participant name mistake ', OldName{1}, ' Dataset ', Dataset{1}])
            continue
        end
        Row.Participant = NewName;
        Row.Session = SessionInfo.Session(SessionIdx);
        
        ParticipantInfoIdx = strcmp(ParticipantInfo.Participant, OldName);
        if nnz(ParticipantInfoIdx) ~= 1
            error(['Participant info error ', OldName, ' Dataset ', Dataset{1}])
        end
        Row.Sex = ParticipantInfo.Sex(ParticipantInfoIdx);
        Row.Handedness = lower(ParticipantInfo.Handedness(ParticipantInfoIdx));

        if ~iscell(Row.Handedness) && isnan(Row.Handedness)
            Row.Handedness = {''};
        end

        Birthday = ParticipantInfo.Birthday(ParticipantInfoIdx);
        ExperimentDate = SessionInfo.Date(SessionIdx);
        Row.Age = years(ExperimentDate-Birthday);

        Row.Group = ParticipantInfo.Group(ParticipantInfoIdx);
        Row.Subgroup = ParticipantInfo.SubGroup(ParticipantInfoIdx);
                        Row.Condition = SessionInfo.Condition(SessionIdx);


        % double for mor and eve
        Rows = [Row; Row];
        Rows.Hour = {'eve'; 'mor'};
        
        try
        Metadata = [Metadata; Rows];
        catch
            a=1
        end
    end
end