function OvernightMetadata = overnight_changes(Metadata)
% pairs up recordings to create table of overnight changes

OvernightMetadata = table();

Metadata.EveningIndexes = [1:size(Metadata, 1)]';
Metadata.MorningIndexes = Metadata.EveningIndexes;

EveningMetadata = Metadata(strcmp(Metadata.Hour, 'eve'), :);
MorningMetadata = Metadata(strcmp(Metadata.Hour, 'mor'), :);

for EveRowIdx = 1:size(EveningMetadata, 1)
    MorRowIdx = strcmp(MorningMetadata.Participant, EveningMetadata.Participant(EveRowIdx)) & ...
        strcmp(MorningMetadata.Session, EveningMetadata.Session(EveRowIdx)) & ...
        strcmp(MorningMetadata.Task, EveningMetadata.Task(EveRowIdx));
    if nnz(MorRowIdx)<1
        continue
    end

    OvernightMetadata = cat(1, OvernightMetadata, EveningMetadata(EveRowIdx, :));
    OvernightMetadata(end, {'Globality', 'Amplitude', 'Duration', 'Frequency'}) = ...
        MorningMetadata(MorRowIdx, {'Globality', 'Amplitude', 'Duration', 'Frequency'}) - ...
        EveningMetadata(EveRowIdx, {'Globality', 'Amplitude', 'Duration', 'Frequency'});

    OvernightMetadata.MorningIndexes(end) = MorningMetadata.MorningIndexes(MorRowIdx);
end