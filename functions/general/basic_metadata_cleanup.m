function Metadata = basic_metadata_cleanup(Metadata, Extras)
arguments
    Metadata
    Extras = [];
end
% applies standard fixes to the metadata so that all scripts update it in
% the same way.

% assign unique index to each entry, so you can find the correct row in the
% matrices of data even if later the metadata strcture gets reduced as data
% is removed.
Metadata.Index = [1:size(Metadata, 1)]'; %#ok<NBRAK1> % add index so can chop up table as needed

% remove 1 year later follow-up sessions
Metadata(contains(Metadata.Dataset, 'SleepLearning') & ...
    contains(Metadata.Session, {'Session_2', 'Session_3'}), :) = [];

% rename to classic session 1 and 2 for SleepLearning
Metadata.Session(strcmp(Metadata.Session, 'Session_1_1')) = {'Session_1'};
Metadata.Session(strcmp(Metadata.Session, 'Session_1_2')) = {'Session_2'};

% adds column SessionUnique for proper statistical nesting
Metadata = assign_unique_session(Metadata);


if isempty(Extras)
    return
end

% TODO: all the special ones