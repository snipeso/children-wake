
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

CacheDir = Paths.Cache;
CacheName = 'AllBursts_ShiftBands.mat';

if ~exist(CacheDir, 'dir')
    mkdir(CacheDir)
end
ResultsFolder = fullfile(Paths.Results, 'AverageTopographies');

Metadata = readtable(fullfile(Paths.Metadata, 'Metadata_Children_Wake.csv'));
Metadata(Metadata.Age > 7, :) = [];
Metadata(isnan(Metadata.Age), :) = [];

Metadata = sortrows(Metadata, 'Age');
Hours = unique(Metadata.Hour);

nParticipants = numel(unique(Metadata.Participant));

for idxHour = 1:numel(Hours)
    MetadataHour = Metadata(contains(Metadata.Hour, Hours{idxHour}), :);

    nRecordings = size(MetadataHour, 1);

    figure('Units','normalized','OuterPosition',[0 0 1 1])
    Grid = [nRecordings, nFrequencies];

    for idxRecording = 1:nRecordings

        Dataset = MetadataHour.Dataset{idxRecording};
        Participant = MetadataHour.Participant{idxRecording};
        Session = replace(MetadataHour.Session{idxRecording}, '_', '');
        Hour = MetadataHour.Hour{idxHour};

        Tasks = Parameters.Tasks.(Dataset);

        for TaskIdx = 1:numel(Tasks)
            Task = Tasks{TaskIdx};

            % load in data
            Path = fullfile(SourcePower, Folder, Dataset, Task);

            DataOut = load_datafile(Path, Participant, Session, Hour, ...
                {'Power', 'Freqs', 'Chanlocs'}, '.mat');
            if isempty(DataOut); continue; end

            Power = DataOut{1};
            Freqs = DataOut{2};
            Chanlocs = DataOut{3};

            for idxFrequency = 1:nFrequencies

                Data = log10(Power(:, dsearchn(Freqs', Frequencies(idxFrequency))));
                chART.sub_plot([], Grid, [idxRecording, idxFrequency], [], false, '', PlotProps);
                chART.plot.eeglab_topoplot(Data, Chanlocs, [], [], '', 'Linear', PlotProps);
                colorbar

                if idxRecording == 1
                    title([num2str(Frequencies(idxFrequency)), ' Hz']);
                end

                if idxFrequency == 1
                    chART.plot.vertical_text(num2str(MetadataHour.Age(idxRecording), '%.1f'), .15, .5, PlotProps)
                end
            end


        end

    end
    chART.save_figure(['YoungestTopos_', Hours{idxHour}], ResultsFolder, PlotProps)
end




