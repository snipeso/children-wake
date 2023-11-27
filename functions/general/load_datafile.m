function DataOut = load_datafile(Path, Participant, Session, Hour, Variable, Extention)
% loads a mat file containing the data of a single participant and single
% session
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
Data = load(fullfile(Path, Filename), Variable);

DataOut = Data.(Variable);