function PairedMetadata = pair_recordings(Metadata, ColumnName, Categories)
% pairs up recordings to create table of item 1, with the index to item 2
% ColumnName is where the categories are saved, and Categories is a cell of
% 2 strings, such that the second will be associated to the first like
% {Categories{1}, Categories{2}}.

PairedMetadata = table();

OutcomeVariables =  {'Globality', 'Amplitude', 'Duration', 'Quantity',  'Slope', 'Intercept','Power', 'PeriodicPower' };

Metadata.IndexesCategory1 = Metadata.Index;
Metadata.IndexesCategory2 = Metadata.IndexesCategory1;

Metadata1 = Metadata(strcmp(Metadata.(ColumnName), Categories{1}), :);
Metadata2 = Metadata(strcmp(Metadata.(ColumnName), Categories{2}), :);

for Cat1RowIdx = 1:size(Metadata1, 1)

    % find a morning recording within the same session and task
    Participant = strcmp(Metadata2.Participant, Metadata1.Participant(Cat1RowIdx));
    Hour = strcmp(Metadata2.Hour, Metadata1.Hour(Cat1RowIdx));
    Session =  strcmp(Metadata2.Session, Metadata1.Session(Cat1RowIdx));

    try
        Task = strcmp(Metadata2.Task, Metadata1.Task(Cat1RowIdx));
    catch
        Task = cellfun(@check_tasks, Metadata2.Task, repmat(Metadata1.Task(Cat1RowIdx), numel(Metadata2.Task), 1));
    end

    switch ColumnName
        case 'Hour' % overnight changes
            MorRowIdx = Participant & Session & Task;
        case {'Session', 'Condition'}
            MorRowIdx = Participant & Hour & Task;
        case 'Task'
            MorRowIdx = Participant & Session & Hour;
    end

    if nnz(MorRowIdx)<1
        continue
    end

    % add evening data to new metadata table
    PairedMetadata = cat(1, PairedMetadata, Metadata1(Cat1RowIdx, :));

    % replace with difference values
    PairedMetadata(end, OutcomeVariables) = ...
        Metadata2(MorRowIdx, OutcomeVariables) - Metadata1(Cat1RowIdx, OutcomeVariables);

    % save morning data index
    PairedMetadata.IndexesCategory2(end) = Metadata2.IndexesCategory2(MorRowIdx);
end
end

function Ismember = check_tasks(Cell1, Cell2)
Ismember = all(ismember(Cell1{1}, Cell2{1}));
end