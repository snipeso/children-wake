
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
Bands = Parameters.Bands;

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
ResultsFolder = fullfile(Paths.Results, 'Histograms');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

Metadata = readtable(fullfile(Paths.Metadata, 'Metadata_Children_Wake.csv'));
Metadata(~contains(Metadata.Hour,  Hour), :) = [];
Metadata = unique_metadata(Metadata);
Metadata = Metadata(contains(Metadata.Group, 'HC'), :);


Metadata = sortrows(Metadata, 'Age');

nRecordings = size(Metadata, 1);

 figure('Units','normalized','OuterPosition',[0 0 .5 1])
% Grid = [6 10];
Grid = [10 6];
Indx = 1;

for idxRecording = 1:nRecordings

    Dataset = Metadata.Dataset{idxRecording};
    Participant = Metadata.Participant{idxRecording};
    Session = replace(Metadata.Session{idxRecording}, '_', '');

        Tasks = Parameters.Tasks.(Dataset);
        TaskName = Tasks(contains(Tasks, Task));
        if isempty(TaskName)
            continue
        end
        TaskName = TaskName{1};

        % load in data
        Path = fullfile(Source, Dataset, TaskName);
        DataOut = load_datafile(Path, Participant, Session, Hour, ...
            {'Bursts', 'BurstClusters', 'EEGMetadata'}, '.mat');
        if isempty(DataOut)
            continue
        end

        Bursts = DataOut{1};
        BurstClusters = DataOut{2};
        EEGMetadata = DataOut{3};
        Chanlocs = EEGMetadata.chanlocs;


        Frequencies = 4:.333:17;
        [HistogramAmplitude, HistogramQuantities] = assemble_burst_distributions(Bursts, Frequencies, EEGMetadata.pnts);

        subplot(Grid(1), Grid(2), Indx)
        Indx = Indx+1;
        plot_multicolored_histogram(HistogramQuantities, Frequencies(1:end-1), Bands, PlotProps)
        xlim([2 18])
        % xlabel('Frequency (Hz)')
        % ylabel('% recording')
        title([num2str(Metadata.Age(idxRecording), '%.1f'), ' y.o.'])

        if Indx == Grid(1)*Grid(2)
            chART.save_figure(['Histograms_', num2str(idxRecording)], ResultsFolder, PlotProps)
            Indx  = 1;
            figure('Units','normalized','OuterPosition',[0 0 1 1])
        end

end





