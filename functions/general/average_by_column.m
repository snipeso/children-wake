function AverageData = average_by_column(Metadata, Data, ColumnName, MetadataIndexes)
% Averages data rows based on whether information in ColunmnName is the
% same in the Metadata table. So for example you can have participants with
% multiple recordings that you want to average together.
% Can either use ExternalIndexes, which is when Data is larger than
% Metadata, and so relies on the indices in the column Index of metadata,
% or indexes Data based on the positions of rows in the Metadata table.
% ExternalIndexes are the indexes in Metadata, from which the indexes in
% Data are derived from the column Index.
% Sorry for the confusion. 

DimsData = size(Data);

if isempty(MetadataIndexes) && DimsData(1) ~=size(Metadata, 1)
    error('Mismatch of Metadata and Data, need to provide external indices')
elseif ~isempty(MetadataIndexes)
    % Will rely on the pre-existing Metadata.Index column to know what data
    % to select.
    Metadata = Metadata(MetadataIndexes, :); % exlude metadata of whatever didnt make it into the index
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