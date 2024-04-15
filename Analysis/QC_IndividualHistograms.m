
clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Hours = Parameters.Hours;
PlotProps = Parameters.PlotProps.Manuscript;

Variables = {''};
VariablesCluster = {};

Frequencies = 4:12;
nFrequencies = numel(Frequencies);
nChans = 123;
MinBursts = 10;

Source = fullfile(Paths.AnalyzedData, 'EEG', 'Bursts');
SourcePower =  fullfile(Paths.AnalyzedData, 'EEG', 'Power');
Folder = 'window4s_allt';

Task = 'Oddball';
Hour = 'eve';

CacheDir = Paths.Cache;
CacheName = 'AllBursts_ShiftBands.mat';

if ~exist(CacheDir, 'dir')
    mkdir(CacheDir)
end
ResultsFolder = fullfile(Paths.Results, 'AverageTopographies');

Metadata = readtable(fullfile(Paths.Metadata, 'Metadata_Children_Wake.csv'));
Metadata(~contains(Metadata.Hour,  Hour), :) = [];
Metadata = unique_metadata(Metadata);


Metadata = sortrows(Metadata, 'Age');

    nRecordings = size(Metadata, 1);

    figure('Units','normalized','OuterPosition',[0 0 1 1])
    Grid = [];

    for idxRecording = 1:nRecordings

        Dataset = Metadata.Dataset{idxRecording};
        Participant = Metadata.Participant{idxRecording};
        Session = replace(Metadata.Session{idxRecording}, '_', '');
        Hour = Metadata.Hour{idxHour};

        Tasks = Parameters.Tasks.(Dataset);

        for TaskIdx = 1:numel(Tasks)
            Task = Tasks{TaskIdx};

            % load in data
        Path = fullfile(Source, Dataset, Task);
        DataOut = load_datafile(Path, Participant, Session, Hour, ...
            {'Bursts', 'BurstClusters', 'EEGMetadata'}, '.mat');
            if isempty(DataOut); continue; end

        Bursts = DataOut{1};
        BurstClusters = DataOut{2};
        EEGMetadata = DataOut{3};
        Chanlocs = EEGMetadata.chanlocs;
               
        chART.sub_plot([], Grid, [idxRecording, idxFrequency], [], false, '', PlotProps);



        end

    end
    chART.save_figure(['YoungestTopos_', Hours{idxHour}], ResultsFolder, PlotProps)





