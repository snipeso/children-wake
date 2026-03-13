% plots the beta estimates for the overnight change split by band
clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.TopoPlots;
Paths = Parameters.Paths;
BandLabels = {'Theta', 'Alpha', 'Beta_{low}'};
nBands = numel(BandLabels);
Ages = Parameters.Ages;
Ages = Ages(2:end, :); % exclude youngest group; too few
nAges = size(Ages, 1);
nChannels = 123;
Tasks = {'Oddball', 'GoNoGo', 'Alertness', 'Fixation'}; % oddball first is important; its the reference. Learning excluded because different in morning
Measures = Parameters.OutcomeMeasures.OriginalLabels;
MeasuresTitles = Parameters.OutcomeMeasures.Titles;
MeaureLabels = append('\beta ',{'\muV', '%', 'a.u.', 'log power', 'log power', 'log power'});
ColorParameter = 'Estimate'; % this is what gets colored in the topoplots, the beta estimates
nMeasures = numel(Measures);

%%% paths
ResultsFolder = fullfile(Paths.Results, 'AverageTopographies');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

%%% load data
load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', 'Chanlocs')
Metadata = basic_metadata_cleanup(Metadata, {'Ages', Ages, 'Tasks', Tasks});

table_demographics(unique_metadata(Metadata), 'AgeGroups', ResultsFolder, 'AgeGroups')


%% fit model for bands

WakeMeasure = 'Quantity';

MetadataStat = Metadata;
MetadataStat = make_categorical(MetadataStat, 'Task', Tasks);
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'});
MetadataStat.Participant = categorical(MetadataStat.Participant);

MetadataStat.Data = nan(size(MetadataStat, 1), 1);

% possible models
BasicFixed = 'Data ~ Hour';
TaskFixed = 'Data ~ Hour + Task';
BasicRandom = ' + (1|Participant)';
SessionRandom = '+ (1|Participant) + (1|Participant:SessionUnique)'; % if there are more than 1 sessions

BandModels = cell([nAges, nBands, nChannels]);
for BandIdx = 1:nBands
    for AgeIdx = 1:nAges
        for ChannelIdx = 1:nChannels
            MetadataTemp = MetadataStat(MetadataStat.AgeGroups==AgeIdx, :);
            MetadataTemp.Data = BurstInformationTopographyBands.(WakeMeasure)(MetadataTemp.Index, ChannelIdx, BandIdx);

            if numel(unique(MetadataTemp.Task)) > 1
                Fixed = TaskFixed;
            else
                Fixed = BasicFixed;
            end

            if numel(unique(MetadataTemp.Session)) > 1
                Random = SessionRandom;
            else
                Random = BasicRandom;
            end

            formula = [Fixed, Random];
            try
                BandModels{AgeIdx, BandIdx, ChannelIdx} = fitlme(MetadataTemp, formula);
            catch
                BandModels{AgeIdx, BandIdx, ChannelIdx} = {};
            end
        end
    end
    disp(['Finished ', BandLabels{BandIdx}])
end


%% Plot bands change (Figure 7)

PlotProps.Color.Background = 'white';
CLims = [-1 1;
    -12 12;
    -2.5 2.5];

Coefficient = 'Hour_2';
Grid = [nBands, nAges+1];

figure('Units','centimeters','Position',[0 0 22 15])

for BandIdx = 1:nBands
    for AgeIdx = 1:nAges

        %%% plot
        chART.sub_plot([], Grid, [BandIdx, AgeIdx], [], false, '', PlotProps);
        mixed_model_topography(squeeze(BandModels(AgeIdx, BandIdx, :)), ColorParameter, Coefficient, Chanlocs, CLims(BandIdx, :), PlotProps)
        colorbar off

        if BandIdx == 1
             title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2)), ' y.o.'], 'FontSize', PlotProps.Text.TitleSize)
        end

        if AgeIdx ==1
            chART.plot.vertical_text(BandLabels{BandIdx}, .12, .5, PlotProps)
        end
    end

    % plot colorbar
    Axes= chART.sub_plot([], Grid, [BandIdx, nAges+1], [], false, '', PlotProps);
    axis off
    Axes.Position(1) = Axes.Position(1)+.02;
    chART.plot.pretty_colorbar('Divergent', CLims(BandIdx, :), MeaureLabels{strcmp(Measures, WakeMeasure)}, PlotProps)
end


chART.save_figure(['TopographyBandChange_',WakeMeasure], ResultsFolder, PlotProps)



%% check relationship with N3

PlotProps.Colorbar.Location = 'eastoutside';
load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', 'Chanlocs')
Metadata = basic_metadata_cleanup(Metadata);

%%% 
WakeMeasure = 'Quantity';

load(fullfile(Paths.Metadata, 'SleepScoring.mat'), 'ScoringMetadata')

MetadataSimple = Metadata;
MetadataSimple(contains(MetadataSimple.Task, {'3Oddball', 'GoNoGo', 'Fixation'}), :) = [];

MetadataSimple(strcmp(MetadataSimple.Hour, 'mor'), :) = [];
MetadataSimple(MetadataSimple.Age < 18, :) = [];

MetadataSimple = combine_metadata_tables(MetadataSimple, ScoringMetadata, {'Participant', 'Session'});

BandIdx = 2;

MetadataTemp = MetadataSimple;
Rs = nan(nChannels, 1);
Ps = Rs;

for ChannelIdx = 1:nChannels
MetadataTemp.Data = BurstInformationTopographyBands.(WakeMeasure)(MetadataTemp.Index, ChannelIdx, BandIdx);

[Rs(ChannelIdx), Ps(ChannelIdx)] = corr(MetadataTemp.timeN3, MetadataTemp.Data, 'Rows','complete');
end

[~, PMask] = fdr(Ps, Parameters.Stats.Alpha);

figure
chART.plot.eeglab_topoplot(Rs, Chanlocs, PMask, [-1 1], 'R', 'Divergent', PlotProps)
title(strjoin([WakeMeasure,  MetadataSimple.Hour(1), BandLabels{BandIdx}], ' '))


%% for codex

WakeMeasure = 'Quantity';
SleepMeasure = 'timeN3';
PlotProps = Parameters.PlotProps.TopoPlots;
PlotProps.Color.Background = 'white';
CLims = [-1 1];
Grid = [nBands, nAges+1];
Hours = {'eve', 'mor'};

%%% load data
load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', 'Chanlocs')

MetadataSimple = basic_metadata_cleanup(Metadata, {'Ages', Ages});
MetadataSimple(contains(MetadataSimple.Task, {'3Oddball', 'GoNoGo', 'Fixation'}), :) = [];
MetadataSimple = combine_metadata_tables(MetadataSimple, ScoringMetadata, {'Participant', 'Session'});

for HourIdx = 1:numel(Hours)
    Hour = Hours{HourIdx};
    MetadataHour = MetadataSimple(strcmp(MetadataSimple.Hour, Hour), :);

    figure('Units','centimeters','Position',[0 0 22 15])

    for BandIdx = 1:nBands
        for AgeIdx = 1:nAges
            MetadataTemp = MetadataHour(MetadataHour.AgeGroups == AgeIdx, :);
            Rs = nan(nChannels, 1);
            Ps = nan(nChannels, 1);

            for ChannelIdx = 1:nChannels
                MetadataTemp.Data = BurstInformationTopographyBands.(WakeMeasure)(MetadataTemp.Index, ChannelIdx, BandIdx);
                [Rs(ChannelIdx), Ps(ChannelIdx)] = corr(MetadataTemp.(SleepMeasure), MetadataTemp.Data, 'Rows', 'complete');
            end

            [~, PMask] = fdr(Ps, Parameters.Stats.Alpha);

            chART.sub_plot([], Grid, [BandIdx, AgeIdx], [], false, '', PlotProps);
            chART.plot.eeglab_topoplot(Rs, Chanlocs, PMask, CLims, 'R', 'Divergent', PlotProps);
            colorbar off

            if BandIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2)), ' y.o.'], 'FontSize', PlotProps.Text.TitleSize)
            end

            if AgeIdx == 1
                chART.plot.vertical_text(BandLabels{BandIdx}, .12, .5, PlotProps)
            end

            topo_corner_text(['N=', num2str(numel(unique(MetadataTemp.Participant(~isnan(MetadataTemp.timeN3)))))], PlotProps)
        end

        Axes = chART.sub_plot([], Grid, [BandIdx, nAges+1], [], false, '', PlotProps);
        axis off
        Axes.Position(1) = Axes.Position(1)+.02;
        chART.plot.pretty_colorbar('Divergent', CLims, 'R', PlotProps)
    end

    chART.save_figure(['Corr_', WakeMeasure, '-', SleepMeasure,'_', Hour], ResultsFolder, PlotProps)
end
