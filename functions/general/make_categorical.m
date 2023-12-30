function Metadata = make_categorical(Metadata, ColumnName, VariableOrder)
% removes also rows that do not have that category
Metadata(~contains(Metadata.(ColumnName), VariableOrder), :) = [];

NewColumn = zeros(size(Metadata, 1), 1);

for VariableIdx = 1:numel(VariableOrder)
    NewColumn(strcmp(Metadata.(ColumnName), VariableOrder{VariableIdx})) = VariableIdx;
end

NewColumn = categorical(NewColumn);
Metadata.(ColumnName) = NewColumn;

