function AverageData = average_by_column(Metadata, Data, ColumnName)
% data is P x whatever
% eventually might just be used for participants

DimsData = size(Data);

NewRows = unique(Metadata.(ColumnName));
nNewRows = numel(NewRows);

AverageData = nan(nNewRows, DimsData(2));


for ParticipantIdx = 1:nNewRows
    Indexes = ismember(Metadata.(ColumnName), NewRows(ParticipantIdx));
    AverageData(ParticipantIdx, :) = mean(Data(Indexes, :), 1, 'omitnan');
end
