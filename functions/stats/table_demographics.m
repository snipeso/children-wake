function table_demographics(Metadata, CategoryColumn, Destination, Filename)
% creates table of 


Categories = unique(Metadata.(CategoryColumn));
Table = table();

for Category = Categories'

    TempMetadata = Metadata(ismember(Metadata.(CategoryColumn), Category{1}), :);
    Table = cat(1, Table, new_row(TempMetadata, Category{1}));
end
Table = cat(1, Table, new_row(Metadata, 'All'));
disp(Table)

writetable(Table, fullfile(Destination, [Filename, '.csv']))
end


function TableRow = new_row(Metadata, Label)
TableRow = table();
TableRow.Label = {Label};
TableRow.N = numel(Metadata.Participant);
TableRow.nFemale = nnz(strcmp(Metadata.Sex, 'f'));
TableRow.nLefties = nnz(strcmp(Metadata.Handedness, 'l'));
TableRow.nADHD = nnz(strcmp(Metadata.Group, 'ADHD'));
TableRow.AgeRange = {[num2str(min(Metadata.Age), '%.1f'),'-', num2str(max(Metadata.Age), '%.1f')]};
TableRow.MeanAge = {[num2str(mean(Metadata.Age), '%.1f'), '(', num2str(std(Metadata.Age), '%.1f'), ')']};
TableRow.nOddball = nnz(contains(Metadata.Task, 'Oddball'));

OutcomeVariables = {'Amplitude', 'Quantity', 'Globality', 'Duration', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
for Variable = OutcomeVariables
    TableRow.(Variable{1}) =  {[num2str(mean(Metadata.(Variable{1})), '%.2f'), ' (', num2str(std(Metadata.(Variable{1})), '%.2f'), ')']};

end

end