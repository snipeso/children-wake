function UniqueMetadata = unique_metadata(Metadata)

[~, UniqueIndx] = unique(Metadata.Participant);
UniqueMetadata = Metadata(UniqueIndx, :);