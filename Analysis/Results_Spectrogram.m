
clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.TopoPlots;
Paths = Parameters.Paths;
BandLabels = {'Theta', 'Low Alpha', 'High Alpha'};
nBands = numel(BandLabels);
Ages = Parameters.Ages;
Ages = Ages(2:end, :); % exclude youngest group; too few
nAges = size(Ages, 1);
nChannels = 123;
% Tasks = {'Oddball', 'GoNoGo', 'Alertness', 'Fixation'}; % oddball first is important; its the reference
Tasks = {'1Oddball' '3Oddball'}; % oddball for now, when have time, do another model
ColorParameter = 'Estimate';

%%% paths
ResultsFolder = fullfile(Paths.Results, 'Spectrogram');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

%%% load data
load(fullfile(CacheDir, CacheName), 'Metadata', "BurstInformationClusters", 'Frequencies')
Metadata.Task(strcmp(Metadata.Task, 'Oddball')) = {'1Oddball'};
Metadata = basic_metadata_cleanup(Metadata, {'Ages', Ages, 'Tasks', Tasks});


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 

%% Plot spectrogram

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 25;
PlotProps.Axes.yPadding = 20;

Measures = {'Amplitude', 'Quantity', 'Power', 'PeriodicPower'};
Labels = {'\muV', '%', 'log power', 'log power'};
nMeasures = numel(Measures);

EquidistantAges = 4:4:25;

Metadata.EquispacedAges = discretize(Metadata.Age, EquidistantAges);
OvernightMetadata = pair_recordings(Metadata, 'Hour', {'eve', 'mor'});

figure('Units','centimeters','OuterPosition',[0 0 10 22])

for MeasureIdx = 1:nMeasures
    Spectrogram = BurstInformationClusters.(Measures{MeasureIdx});
    Evening = average_by_column(OvernightMetadata, Spectrogram, 'Participant', [1:size(OvernightMetadata, 1)]');
    MetadataTemp = unique_metadata(OvernightMetadata, 'Participant');
    EveningAverage = average_by_column(MetadataTemp, Evening, 'EquispacedAges', []);

    %%% plot average evening values
    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);
    plot_age_by_frequency(EveningAverage, EquidistantAges(1:end-1), Frequencies, 'Linear', Labels{MeasureIdx}, PlotProps)
    ylim([5 15])
    title(Measures{MeasureIdx})
    ylabel('Frequency (Hz)')
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
end
chART.save_figure('FrequencyByAge', ResultsFolder, PlotProps)

% spectrum difference


% figure('Units','normalized','OuterPosition',[0 0 .18 1])
figure('Units','centimeters','OuterPosition',[0 0 10 22])

for MeasureIdx = 1:nMeasures

    Spectrogram = BurstInformationClusters.(Measures{MeasureIdx});
    Evening = average_by_column(OvernightMetadata, Spectrogram, 'Participant', [1:size(OvernightMetadata, 1)]');

    OvernightTemp = OvernightMetadata;
    OvernightTemp.Index = OvernightMetadata.IndexesCategory2;
    Morning = average_by_column(OvernightTemp, Spectrogram, 'Participant', [1:size(OvernightMetadata, 1)]');

    Change = Morning-Evening;
    MetadataTemp = unique_metadata(OvernightMetadata, 'Participant');
    ChangeAverage = average_by_column(MetadataTemp, Change, 'EquispacedAges', []);

    %%% plot differences
    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);
    plot_age_by_frequency(ChangeAverage, EquidistantAges(1:end-1), Frequencies, 'Divergent', 'difference', PlotProps)
    ylim([5 15])

    title([Measures{MeasureIdx}])
    ylabel('Frequency (Hz)')
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
end
chART.save_figure('FrequencyByAgeChange', ResultsFolder, PlotProps)

