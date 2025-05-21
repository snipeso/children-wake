clear
clc
close all


Parameters = analysisParameters();
Paths = Parameters.Paths;
SleepPaths = Parameters.SleepPaths;
Datasets = Parameters.Datasets;
EpochLength = 20; % move to parameters
TimeToKeep = 60*60/EpochLength; % 1 h in epochs
Band = Parameters.PowerBands.Delta;

Source = fullfile(SleepPaths.Power);

CacheDir = Paths.Cache;
CacheName = 'MetadataSleep.mat';
load(fullfile(CacheDir, CacheName), 'MetadataSleep')

CacheName = 'WakeMetadata.mat';
load(fullfile(CacheDir, CacheName), 'Metadata')


nRecordings = size(Metadata);


SleepVariables = {'SWA', 'SW_Amplitude', 'SW_Slope', 'SW_Slope_Matched'};

for SleepVariableIdx = 1:numel(SleepVariables)
    Metadata.(SleepVariables{SleepVariableIdx}) = nan(nRecordings, 1);
end

for recordingIdx = 1:nRecordings

    Dataset = MetadataSleep.Dataset{RecordingIdx};
    Participant = MetadataSleep.Participant{RecordingIdx};
    Session = replace(MetadataSleep.Session{RecordingIdx}, '_', '');
    Hour = MetadataSleep.Hour{RecordingIdx};

    switch Hour
        case 'eve'
            Prefix = 'FH_';
        case 'mor'
            Prefix = 'LH_';
    end

    SleepIdx = find(strcmp(SleepMetadata.Dataset, Dataset) & ...
        strcmp(SleepMetadata.Participant, Participant) & ...
        strcmp(SleepMetadata.Session, Session));

    for SleepVariableIdx = 1:numel(SleepVariables)
        Metadata.(SleepVariables{SleepVariableIdx})(RecordingIdx) = ...
            SleepMetadata.([Prefix, SleepVariables{SleepVariableIdx}])(SleepIdx);
    end
end

CacheName = 'AllMetadata.mat';
save(fullfile(CacheDir, CacheName), 'Metadata')
writetable(Metadata, fullfile(CacheDir, replace(CacheName, 'mat', 'csv')))



