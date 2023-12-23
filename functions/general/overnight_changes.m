function OvernightMetadata = overnight_changes(Metadata)
% pairs up recordings to create table of overnight changes

OvernightMetadata = table();

OutcomeVariables =  {'Globality', 'Amplitude', 'Duration', 'Quantity',  'Slope', 'Intercept','Power', 'PeriodicPower' };

Metadata.EveningIndexes = Metadata.Index;
Metadata.MorningIndexes = Metadata.EveningIndexes;

EveningMetadata = Metadata(strcmp(Metadata.Hour, 'eve'), :);
MorningMetadata = Metadata(strcmp(Metadata.Hour, 'mor'), :);

for EveRowIdx = 1:size(EveningMetadata, 1)

    % find a morning recording within the same session and task
    try
        MorRowIdx = strcmp(MorningMetadata.Participant, EveningMetadata.Participant(EveRowIdx)) & ...
            strcmp(MorningMetadata.Session, EveningMetadata.Session(EveRowIdx)) & ...
            strcmp(MorningMetadata.Task, EveningMetadata.Task(EveRowIdx));

    catch

        MorRowIdx = strcmp(MorningMetadata.Participant, EveningMetadata.Participant(EveRowIdx)) & ...
            strcmp(MorningMetadata.Session, EveningMetadata.Session(EveRowIdx)) & ...
            cellfun(@check_tasks, MorningMetadata.Task, repmat(EveningMetadata.Task(EveRowIdx), numel(MorningMetadata.Task), 1));
    end
    if nnz(MorRowIdx)<1
        continue
    end

    % add evening data to new metadata table
    OvernightMetadata = cat(1, OvernightMetadata, EveningMetadata(EveRowIdx, :));

    % replace with difference values
    OvernightMetadata(end, OutcomeVariables) = ...
        MorningMetadata(MorRowIdx, OutcomeVariables) - EveningMetadata(EveRowIdx, OutcomeVariables);

    % save morning data index
    OvernightMetadata.MorningIndexes(end) = MorningMetadata.MorningIndexes(MorRowIdx);
end

end

function Ismember = check_tasks(Cell1, Cell2)
Ismember = all(ismember(Cell1{1}, Cell2{1}));

end