function AverageData = average_by_column(Metadata, Indexes, Data, ColumnName)
% data is P x whatever
% eventually might just be used for participants
%WARNING: indexes need to always be in order

DimsData = size(Data);

Metadata = Metadata(ismember(Metadata.Index, Indexes), :);

NewRows = unique(Metadata.(ColumnName));
try
NewRows(isnan(NewRows)) = [];
end
nNewRows = numel(NewRows);

AverageData = nan(nNewRows, DimsData(2));
for ParticipantIdx = 1:nNewRows
    SubIndexes = ismember(Metadata.(ColumnName), NewRows(ParticipantIdx));
    AverageData(ParticipantIdx, :) = mean(Data(SubIndexes, :), 1, 'omitnan');
end




% 
% DimsData = size(Data);
% 
% NewRows = unique(Metadata.(ColumnName));
% try
% NewRows(isnan(NewRows)) = [];
% end
% 
% nNewRows = numel(NewRows);
% 
% AverageData = nan(nNewRows, DimsData(2));
% 
% NewMetadata = table();
% 
% for ParticipantIdx = 1:nNewRows
%     Indexes = ismember(Metadata.(ColumnName), NewRows(ParticipantIdx));
%     AverageData(ParticipantIdx, :) = mean(Data(Indexes, :), 1, 'omitnan');
% 
%     First = find(Indexes, 1, 'first');
%     NewMetadata = cat(1, NewMetadata, Metadata(First, :));
% end
