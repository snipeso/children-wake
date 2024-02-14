clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Hours = Parameters.Hours;
Bands = Parameters.Bands;
BandLabels = {'Theta', 'Low Alpha', 'High Alpha'};

MinNaNChannels = 25; % for amplitudes

% Topography plotprops
TopoPlotProps = Parameters.PlotProps.Manuscript;
TopoPlotProps.Text.LegendSize = 10;
TopoPlotProps.Text.AxisSize = 10;
TopoPlotProps.Axes.xPadding = 8;
TopoPlotProps.Axes.yPadding = 5;
TopoPlotProps.Figure.Padding = 20;
TopoPlotProps.Stats.PlotN = true;
TopoFigureSizes = [.4, .11];

% Ages = [2 8; % too few little kids
%     8, 11;% 3 year age jumps, like Kurth et al. 2010
%     11 14;
%     14 17;
%     17, 20;
%     20 25];
% Ages = [3 7;
%     7 10;
%     10 14;
%     14 18;
%     18 25];

Ages = [    7 14;
    14 25];

ResultsFolder = fullfile(Paths.Results, 'Learning');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', "BurstInformationClusters", 'Frequencies', 'Chanlocs')

% select data for the paper
Metadata.Index = [1:size(Metadata, 1)]'; %#ok<NBRAK1> % add index so can chop up table as needed
Metadata(strcmp(Metadata.Dataset, 'SleepLearning') & ...
    contains(Metadata.Session, {'Session_2', 'Session_3'}), :) = []; % remove repeated measures 1 year later (will average recordings a couple weeks apart)
% Metadata(contains(Metadata.Task, {'3Oddball', '1GoNoGo', '2Learning', '3Fixation', '4Fixation'}), :) = []; % only look at first oddball and alertness task
Metadata.Subgroup(strcmp(Metadata.Group, 'HC')) = 5;
Metadata.Globality = Metadata.Globality*100; % make it percentage instead of proportion
Metadata.AgeGroups = string(discretize(Metadata.Age, [Ages(:, 1); Ages(end, 2)]));
Metadata.Task(contains(Metadata.Task, 'Alertness')) = {'Alertness'}; % Fix because different order in task
Metadata.Task(contains(Metadata.Task, 'GoNoGo')) = {'GoNoGo'}; % Fix because different order in task
% Metadata(contains(Metadata.Task, '4Fixation'), :) = [];
% Metadata.Task(contains(Metadata.Task, 'Fixation')) = {'Fixation'}; % Fix because different order in task


% Metadata.Task(contains(Metadata.Task, 'Oddball')) = {'Oddball'};
% MetadataComplete = Metadata;
Metadata(contains(Metadata.Group, 'ADHD'), :) = []; % RODO figure out why theres too few ADHD kids!!
nAges = size(Ages, 1);


%% overtask change by age (pre vs post stim, sham, stim vs sham)

%% plot oddball1 vs oddball3 and learning

CLims = [-1.5 1.5];

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 20;

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);
Conditions = {'base', 'rotation'};

for ConditionIdx = 1:numel(Conditions)

    % MetadataTemp = Metadata(strcmp(Metadata.Hour, 'mor') & strcmp(Metadata.Condition, Conditions{ConditionIdx}), :);
    MetadataTemp = Metadata(strcmp(Metadata.Condition, Conditions{ConditionIdx}), :);

    Metadata1 = pair_recordings(MetadataTemp, 'Task', {'1Oddball', '3Oddball'});
    Metadata2 = Metadata1;
    Metadata2.Index = Metadata1.IndexesCategory2;

    figure('Units','centimeters','OuterPosition',[0 0 15 30])
    for MeasureIdx = 1:nMeasures
        Topographies = BurstInformationTopography.(Measures{MeasureIdx});
        for AgeIdx = 1:nAges
            Indexes = ismember(Metadata1.AgeGroups, string(AgeIdx));

            Category1 = average_by_column(Metadata1, Topographies, 'Participant', Indexes);
            Category2 = average_by_column(Metadata2, Topographies, 'Participant', Indexes);


            chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', TopoPlotProps);
            plot_topography_difference(Category1, Category2, Chanlocs, CLims, Parameters.Stats, TopoPlotProps) %
            colorbar off

            if MeasureIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
            end

            if AgeIdx ==1
                X = get(gca, 'XLim');
                Y = get(gca, 'YLim');
                text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasureIdx}, ...
                    'FontSize', TopoPlotProps.Text.TitleSize, 'FontName', TopoPlotProps.Text.FontName, ...
                    'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
            end
        end
    end

    chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx+1], [nMeasures, 1], false, '', TopoPlotProps);
    chART.plot.pretty_colorbar('Divergent', CLims, "Cohen's d", TopoPlotProps)

    chART.save_figure(['OddballChange', Conditions{ConditionIdx}], ResultsFolder, TopoPlotProps)
end


%%  difference of difference

CLims = [-1.5 1.5];

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);
    % MetadataTemp = Metadata(strcmp(Metadata.Hour, 'mor') & strcmp(Metadata.Condition, Conditions{ConditionIdx}), :);
    % MetadataTemp = Metadata(strcmp(Metadata.Condition, Conditions{ConditionIdx}), :);

    Metadata1 = pair_recordings(Metadata, 'Task', {'1Oddball', '3Oddball'});
    Oddball1Index = Metadata1.IndexesCategory1;
    Oddball3Index = Metadata1.IndexesCategory2;

        Metadata2 = pair_recordings(Metadata1, 'Condition', {'base', 'rotation'});

    Oddball1BaseIndex = Metadata2.IndexesCategory1; % 1oddball base associated with both a 3oddball, and a 1oddball rot
    Oddball3BaseIndex = Metadata1.IndexesCategory2(ismember(Metadata1.IndexesCategory1, Oddball1BaseIndex));
    Oddball1RotIndex = Metadata2.IndexesCategory2;
    Oddball3RotIndex = Metadata1.IndexesCategory2(ismember(Metadata1.IndexesCategory1, Oddball1RotIndex));


    figure('Units','centimeters','OuterPosition',[0 0 15 30])
    for MeasureIdx = 1:nMeasures
        Topographies = BurstInformationTopography.(Measures{MeasureIdx});
                    Base = Topographies(Oddball3BaseIndex, :)-Topographies(Oddball1BaseIndex, :);
            Rotation = Topographies(Oddball3RotIndex, :)-Topographies(Oddball1RotIndex, :);

        for AgeIdx = 1:nAges
            Indexes = ismember(Metadata2.AgeGroups, string(AgeIdx));

            Category1 = average_by_column(Metadata2(Indexes, :), Base(Indexes, :), 'Participant', []);
            Category2 = average_by_column(Metadata2(Indexes, :), Rotation(Indexes, :), 'Participant', []);

            chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', TopoPlotProps);
            plot_topography_difference(Category1, Category2, Chanlocs, CLims, Parameters.Stats, TopoPlotProps) %
            colorbar off

            if MeasureIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
            end

            if AgeIdx ==1
                X = get(gca, 'XLim');
                Y = get(gca, 'YLim');
                text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasureIdx}, ...
                    'FontSize', TopoPlotProps.Text.TitleSize, 'FontName', TopoPlotProps.Text.FontName, ...
                    'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
            end
        end
    end

    chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx+1], [nMeasures, 1], false, '', TopoPlotProps);
    chART.plot.pretty_colorbar('Divergent', CLims, "Cohen's d", TopoPlotProps)

    chART.save_figure('OddballRotvsBase', ResultsFolder, TopoPlotProps)







%% fixation vs gonogo task
% 
% CLims = [-1.5 1.5];
% 
% Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
% nMeasures = numel(Measures);
% 
% MetadataTemp = Metadata(strcmp(Metadata.Hour, 'eve'), :);
% MetadataTemp = Metadata;
% Metadata1 = pair_recordings(MetadataTemp, 'Task', {'3Fixation', '4Fixation'});
% Metadata2 = Metadata1;
% Metadata2.Index = Metadata1.IndexesCategory2;
% 
% figure('Units','centimeters','OuterPosition',[0 0 25 30])
% for MeasureIdx = 1:nMeasures
%     Topographies = BurstInformationTopography.(Measures{MeasureIdx});
%     for AgeIdx = 1:nAges
%         Indexes = ismember(Metadata1.AgeGroups, string(AgeIdx));
% 
%         Category1 = average_by_column(Metadata1, Topographies, 'Participant', Indexes);
%         Category2 = average_by_column(Metadata2, Topographies, 'Participant', Indexes);
% 
% 
%         chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', TopoPlotProps);
%         plot_topography_difference(Category1, Category2, Chanlocs, CLims, Parameters.Stats, TopoPlotProps) %
%         colorbar off
% 
%         if MeasureIdx == 1
%             title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
%         end
% 
%         if AgeIdx ==2
%             X = get(gca, 'XLim');
%             Y = get(gca, 'YLim');
%             text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasureIdx}, ...
%                 'FontSize', TopoPlotProps.Text.TitleSize, 'FontName', TopoPlotProps.Text.FontName, ...
%                 'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
%         end
%     end
% end
% 
% chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx+1], [nMeasures, 1], false, '', TopoPlotProps);
% chART.plot.pretty_colorbar('Divergent', CLims, "Cohen's d", TopoPlotProps)
% 
% chART.save_figure('GopherChange', ResultsFolder, TopoPlotProps)
