function Metadata = make_categorical(Metadata, ColumnName, VariableOrder)
% Reorders a metadata column into the category baseline needed for mixed-effects models.
% removes also rows that do not have that category
Metadata(~contains(Metadata.(ColumnName), VariableOrder), :) = [];

NewColumn = zeros(size(Metadata, 1), 1);

for VariableIdx = 1:numel(VariableOrder)
    NewColumn(contains(Metadata.(ColumnName), VariableOrder{VariableIdx})) = VariableIdx;
end

NewColumn = categorical(NewColumn);
Metadata.(ColumnName) = NewColumn;

