function [Metadata1, Metadata2] = split_groups(Metadata, Column, Items)

Metadata = unique_metadata(Metadata, 'Participant');
Metadata1 = Metadata(ismember(Metadata.(Column), Items(1)), :);
Metadata2 = Metadata(ismember(Metadata.(Column), Items(2)), :);


MaxRow = min(size(Metadata1, 1), size(Metadata2, 1));
Metadata1 = Metadata1(1:MaxRow, :);
Metadata2 = Metadata2(1:MaxRow, :);