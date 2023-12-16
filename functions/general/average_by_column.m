function AverageData = average_by_column(Metadata, Data, ColumnName, ExternalIndexes)
% Averages data rows based on whether information in ColunmnName is the
% same in the Metadata table. So for example you can have participants with
% multiple recordings that you want to average together.
% Can either use ExternalIndexes, which is when Data is larger than
% Metadata, and so relies on the indices in the column Index of metadata,
% or indexes Data based on the positions of rows in the Metadata table.
% Sorry for the confusion. 

DimsData = size(Data);

if isempty(ExternalIndexes) && DimsData(1) ~=size(Metadata, 1)
    error('Mismatch of Metadata and Data, need to provide external indices')
elseif ~isempty(ExternalIndexes)
    % only consider metadata information that applies to rows indicated in
    % the external indices
    % Metadata = Metadata(ismember(Metadata.Index, ExternalIndexes), :);
    % TODO
    error('TODO')
else
    Metadata.Index = [1:DimsData(1)]'; %#ok<NBRAK1> % just use the normal indexing the metadata table
end

% set up new data structure
NewRowLabels = unique(Metadata.(ColumnName));
try % uses try because if its not a number this thing complains
NewRowLabels(isnan(NewRowLabels)) = [];
end
nNewRows = numel(NewRowLabels);
AverageData = nan(nNewRows, DimsData(2));

% average the data
for ParticipantIdx = 1:nNewRows
    SubIndexes = Metadata.Index(ismember(Metadata.(ColumnName), NewRowLabels(ParticipantIdx)));
    AverageData(ParticipantIdx, :) = mean(Data(SubIndexes, :), 1, 'omitnan');
end