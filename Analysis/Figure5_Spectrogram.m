% plots the age x frequency and spectrograms of the data
clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
Paths = Parameters.Paths;

nChannels = 123;
Tasks = {'1Oddball' '3Oddball'}; % oddball for now, when have time, do another model
ColorParameter = 'Estimate';

Ages = Parameters.Ages;
Ages = Ages(2:end, :); % exclude youngest group; too few
nAges = size(Ages, 1);

%%% paths
ResultsFolder = fullfile(Paths.Results, 'Spectrogram');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

%%% load data
load(fullfile(CacheDir, CacheName), 'Metadata', "BurstInformationClusters", 'Frequencies', 'AverageSpectrograms', 'AllFrequencies')

MetadataOddball = Metadata;
MetadataOddball.Task(strcmp(Metadata.Task, 'Oddball')) = {'1Oddball'};
MetadataOddball = basic_metadata_cleanup(MetadataOddball, {'Ages', Ages, 'Tasks', Tasks});


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

%% Plot spectrogram (Figure 5)


PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 28;
PlotProps.Axes.yPadding = 20;

Measures = {'Amplitude', 'Quantity', 'Power', 'PeriodicPower'};
MeasureTitles = {'Amplitude', 'Density', 'Power', 'Periodic power'};
Labels = {'\muV', '%', 'log power', 'log power'};
nMeasures = numel(Measures);

EquidistantAges = 4:4:25;

MetadataOddball.EquispacedAges = discretize(MetadataOddball.Age, EquidistantAges);
OvernightMetadata = pair_recordings(MetadataOddball, 'Hour', {'eve', 'mor'});

figure('Units','centimeters','OuterPosition',[0 0 10 22])

for MeasureIdx = 1:nMeasures
    Spectrogram = BurstInformationClusters.(Measures{MeasureIdx});
    Evening = average_by_column(OvernightMetadata, Spectrogram, 'Participant', [1:size(OvernightMetadata, 1)]');
    MetadataTemp = unique_metadata(OvernightMetadata, 'Participant');
    EveningAverage = average_by_column(MetadataTemp, Evening, 'EquispacedAges', []);

    %%% plot average evening values
    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);
    plot_age_by_frequency(EveningAverage, EquidistantAges(1:end-1), Frequencies, 'Linear', Labels{MeasureIdx}, PlotProps)
    title(MeasureTitles{MeasureIdx}, 'FontSize', PlotProps.Text.TitleSize)
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
end
colormap(PlotProps.Color.Maps.Linear)
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
    title(MeasureTitles{MeasureIdx}, 'FontSize', PlotProps.Text.TitleSize)
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
end
colormap(PlotProps.Color.Maps.Divergent)
chART.save_figure('FrequencyByAgeChange', ResultsFolder, PlotProps)


%% suppl figure frequencies by age group (Suppl. Figure 5-1)

Ages = Parameters.Ages;
nAges = size(Ages, 1);

MetadataSpectro = basic_metadata_cleanup(Metadata, {'Ages', Ages});
MegaGrid = [2, 1];

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Axes.yPadding = 20;

MiniGridA = [1 nAges];

Tasks = {'GoNoGo', 'Alertness', 'Fixation'};
nTasks = numel(Tasks);

MiniGridB = [1 nTasks];

figure('Units','centimeters','OuterPosition',[0 0 25 18])

%%% A: spectrograms by age
Space = chART.sub_figure(MegaGrid, [1 1], [], 'A', PlotProps);
for AgeIdx = 1:nAges

    % assemble data
    TaskIndexes = contains(MetadataSpectro.Task, {'Oddball'});
    AgeIndexes = MetadataSpectro.AgeGroups==AgeIdx;
    HourIndexes = strcmp(MetadataSpectro.Hour, 'eve');
    MetadataTemp = MetadataSpectro(TaskIndexes & AgeIndexes & HourIndexes, :);

    Evening = average_by_column(MetadataTemp, AverageSpectrograms, 'Participant', [1:size(MetadataTemp, 1)]'); % NB: I did not make sure that each recording was paired to another

    HourIndexes = strcmp(MetadataSpectro.Hour, 'mor');
    MetadataTemp = MetadataSpectro(TaskIndexes & AgeIndexes & HourIndexes, :);
    Morning = average_by_column(MetadataTemp, AverageSpectrograms, 'Participant', [1:size(MetadataTemp, 1)]');

    % plot
    chART.sub_plot(Space, MiniGridA, [1 AgeIdx], [], true, '', PlotProps);
    plot_spectrogram(Evening, Morning, AllFrequencies, PlotProps)
    title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2)), ' y.o.'])
    if AgeIdx >1
        legend off
        ylabel('')
    end
    ylim([.02 90])
end


%%% B: spectrograms by task
Space = chART.sub_figure(MegaGrid, [2 1], [], 'B', PlotProps);
for TaskIdx = 1:nTasks

    % assemble data
    TaskIndexes = contains(MetadataSpectro.Task, Tasks{TaskIdx});
    HourIndexes = strcmp(MetadataSpectro.Hour, 'eve');
    MetadataTemp = MetadataSpectro(TaskIndexes & HourIndexes, :);
    Evening = average_by_column(MetadataTemp, AverageSpectrograms, 'Participant', [1:size(MetadataTemp, 1)]'); % NB: I did not make sure that each recording was paired to another

    HourIndexes = strcmp(MetadataSpectro.Hour, 'mor');
    MetadataTemp = MetadataSpectro(TaskIndexes & HourIndexes, :);
    Morning = average_by_column(MetadataTemp, AverageSpectrograms, 'Participant', [1:size(MetadataTemp, 1)]');

    % plot
    chART.sub_plot(Space, MiniGridB, [1 TaskIdx], [], true, '', PlotProps);
    plot_spectrogram(Evening, Morning, AllFrequencies, PlotProps)
    title(Tasks{TaskIdx})
    ylim([.02 90])
    if TaskIdx > 1
        legend off
        ylabel('')
    end
end


chART.save_figure('Spectrograms', ResultsFolder, PlotProps)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% functions


function plot_spectrogram(Evening, Morning, Freqs, PlotProps)
% Power is a P x H x F matrix

Colors = chART.color_picker([2, 3]);
EveningColor = squeeze(Colors([ 2 3], :, 1));
MorningColor = squeeze(Colors([ 2 3], :, 2));

hold on
plot(Freqs, Evening, 'Color', [EveningColor(2, :), .2], 'LineWidth', PlotProps.Line.Width/4, 'HandleVisibility','off')
plot(Freqs, Morning, 'Color', [MorningColor(2, :), .2], 'LineWidth', PlotProps.Line.Width/4, 'HandleVisibility','off')

chART.set_axis_properties(PlotProps)

plot(Freqs, mean(Evening, 1, 'omitnan'), 'Color', EveningColor(1, :), 'LineWidth', PlotProps.Line.Width)
plot(Freqs, mean(Morning, 1, 'omitnan'), 'Color', MorningColor(1, :), 'LineWidth', PlotProps.Line.Width)

set(gca, 'YScale', 'log', 'XScale', 'log', 'XGrid', 'on', 'XMinorTick','off', 'XMinorGrid', 'off')
xticks([2 4 8 16 32])
xlim([1 40])
legend({'Evening', 'Morning'})
xlabel('Frequency (Hz)')
ylabel('Power (\muV^2/Hz)')
set(legend, 'ItemTokenSize', [10 10], 'location', 'southwest')
end




