function DataOut = load_datafile(Path, Participant, Session, Hour, Variables, Extention)
% loads a mat file containing the data of a single participant and single
% session. If Variables is a single string, output will be whatever that
% variable was. If its a cell array, dataout will also be a cell array with
% each cell containing the information
% for children wake

% get filename
Filenames = list_filenames(Path);
Filename = Filenames(contains(Filenames, Participant) & ...
    contains(Filenames, Session) & ...
    contains(Filenames, Hour) & contains(Filenames, Extention));

if isempty(Filename)
    warning(['No data in ', Participant, '_' Session])
    DataOut = [];
    return
elseif numel(Filename)>1
    warning(['too many files in ', char(Filename(1))])
    Filename = Filename(1);
end

% load data
if iscell(Variables)
    DataOut = cell();
    for VariablesIdx = 1:numel(Variables)
        Variable = Variables{VariablesIdx};
        Data = load(fullfile(Path, Filename), Variable);
        DataOut(VariablesIdx) = Data.(Variable);
    end

else
    Data = load(fullfile(Path, Filename), Variables);
    DataOut = Data.(Variables);
end