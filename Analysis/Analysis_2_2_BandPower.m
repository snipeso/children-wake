% creates a massive matrix D x ch x f x v, so that data can then easily be
% indexed as needed.
% TODO: better documenation; rename to Assemble WakeData

clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;

SourcePower =  fullfile(Paths.AnalyzedData, 'EEG', 'Power');
Folder = 'window4s_allt';

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

load(fullfile(CacheDir, CacheName), 'Metadata', 'Chanlocs')
nRecordings = size(Metadata, 1); % this does not consider tasks
        NotEdgeChanIndex = labels2indexes(Parameters.Channels.NotEdge, Chanlocs);
PowerBands = Parameters.PowerBands;

PowerBandLabels = fieldnames(PowerBands);
for Indx_B = 1:numel(PowerBandLabels)
    Metadata.(PowerBandLabels{Indx_B}) = nan(nRecordings, 1);
end

for RecordingIdx = 1:nRecordings

    Dataset = Metadata.Dataset{RecordingIdx};
    Participant = Metadata.Participant{RecordingIdx};
    Session = replace(Metadata.Session{RecordingIdx}, '_', '');
    Hour = Metadata.Hour{RecordingIdx};
    Task = Metadata.Task{RecordingIdx};

    % load in power spectra
    Path = fullfile(SourcePower, Folder, Dataset, Task);
    DataOut = load_datafile(Path, Participant, Session, Hour, ...
        {'Power', 'Freqs'}, '.mat');
    Power = DataOut{1};
    Frequencies = DataOut{2};

    for Indx_B = 1:numel(PowerBandLabels)
        Band = PowerBands.(PowerBandLabels{Indx_B});
        FreqRange = dsearchn(Frequencies', [Band(1); Band(end)]);
        Metadata.(PowerBandLabels{Indx_B})(RecordingIdx) =  mean(mean(log10(Power(NotEdgeChanIndex, FreqRange(1):FreqRange(2))), 2), 1);
    end

    disp(num2str(RecordingIdx))
end

% save
CacheName = 'WakeMetadata.mat';
save(fullfile(CacheDir, CacheName), 'Metadata')
