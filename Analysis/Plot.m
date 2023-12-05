
clear
clc
close all


Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Hours = Parameters.Hours;

ResultsFolder = fullfile(Paths.Results, 'Main');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end



CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';
%
load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopography', ...
    "BurstInformationClusters", 'Frequencies', 'Chanlocs')

% load(fullfile(CacheDir, CacheName), 'Metadata')

OvernightMetadata = overnight_changes(Metadata);

%% scatterplot of basic information
close all
PlotProps = Parameters.PlotProps.Manuscript;

% PlotProps.Axes.yPadding = 30;
% PlotProps.Axes.xPadding = 30;

Groups = {'HC', 'ADHD'};

Tasks = {'Oddball', 'Alertness'};
YVariables = {'Globality', 'Amplitude', 'Duration', 'Frequency', 'Slope', 'Intercept'};
Grid = [3 numel(YVariables)];
Session = 'Session_1';
Colors = chART.color_picker(2);
Markers = {'o', '^'};
Dashes = {':', '-'};
Limits = [.02 .14;
    7 30;
    .5 1.1;
    9 13;
    1.1 2.1;
    .5 2.5];

HourLabels = {'Evening', 'Morning'};

figure('Units','normalized','OuterPosition',[0 0 .5 .6])
for VariableIdx = 1:numel(YVariables)
    for HourIdx = 1:numel(Hours)
        Hour = Hours(HourIdx);
        chART.sub_plot([], Grid, [HourIdx, VariableIdx], [], true, '', PlotProps);
        hold on
        for GroupIdx = 1:numel(Groups)
            Indexes = strcmp(Metadata.Group, Groups{GroupIdx}) & ...
                strcmp(Metadata.Hour, Hour);
            AverageData = average_by_column(Metadata(Indexes, :),[Metadata.Age(Indexes), Metadata.(YVariables{VariableIdx})(Indexes)], 'Participant');

            scatter(AverageData(:, 1), AverageData(:, 2), 10,  ...
                'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(GroupIdx,:), 'MarkerFaceAlpha',.7)
            chART.set_axis_properties(PlotProps)

            if HourIdx==1
                title(YVariables{VariableIdx})
            end
            if VariableIdx==1
                ylabel(HourLabels{HourIdx}, 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
            end
            xlim([8 24])
            ylim(Limits(VariableIdx, :))
        end
        h = lsline;
        for GroupIdx = 1:numel(Groups)
            set(h(GroupIdx),'color',Colors(numel(Groups)+1-GroupIdx,:), 'LineWidth', 2)
        end
    end

    chART.sub_plot([], Grid, [3, VariableIdx], [], true, '', PlotProps);

    hold on
    for GroupIdx = 1:numel(Groups)
        Indexes = strcmp(OvernightMetadata.Group, Groups{GroupIdx});

        AverageData = average_by_column(OvernightMetadata(Indexes, :), ...
            [OvernightMetadata.Age(Indexes), OvernightMetadata.(YVariables{VariableIdx})(Indexes)], 'Participant');

        scatter(AverageData(:, 1), AverageData(:, 2), 10, ...
            'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(GroupIdx,:), 'MarkerFaceAlpha',.7)

        chART.set_axis_properties(PlotProps)
        if VariableIdx==1
            ylabel('Overnight change', 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
        end
        xlabel('Age')
        xlim([8 24])
    end

    h = lsline;
    for GroupIdx = 1:numel(Groups)
        set(h(GroupIdx),'color',Colors(numel(Groups)+1-GroupIdx,:), 'LineWidth', 2)
    end

end
legend(Groups)
chART.save_figure(['BasicScatterAge', Session], ResultsFolder, PlotProps)


%% correlate measures

YVariables = {'Globality', 'Amplitude', 'Duration', 'Frequency', 'Slope', 'Intercept'};
Grid = [numel(YVariables) numel(YVariables)];
figure('Units','centimeters','OuterPosition',[0 0 30 30])
for Idx1 = 1:numel(YVariables)
    for Idx2 = 1:numel(YVariables)
        chART.sub_plot([], Grid, [Idx2, Idx1], [], true, '', PlotProps);

        scatter(Metadata.(YVariables{Idx1}), Metadata.(YVariables{Idx2}), 10, ...
            'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(1, :), 'MarkerFaceAlpha',.7)
        chART.set_axis_properties(PlotProps)
        lsline;
        if Idx2 == numel(YVariables)
                    xlabel(YVariables{Idx1})
        end
        if Idx1==1
            ylabel(YVariables{Idx2})
        end
    end
end
chART.save_figure(['CorrelateVariables', Session], ResultsFolder, PlotProps)



%% topographies by age, descriptive

Group = 'HC';

Ages = [8, 11;
    11 14;
    14 16;
    16 18];
nAges = size(Ages, 1);
Measures = fieldnames(BurstInformationTopography);
nMeasures = numel(Measures);
MeasureLabels = Measures;

CLims = [0 .4;
    10, 25;
    8 11.6;
    1.3 1.95;
    .8 1.95];

figure('Units','normalized','OuterPosition',[0 0 .4 .7])
for MeasureIdx = 1:nMeasures
    for AgeIdx = 1:nAges
        Indexes = strcmp(Metadata.Group, Group) & Metadata.Age >= Ages(AgeIdx, 1) & Metadata.Age < Ages(AgeIdx, 2);
        Data = BurstInformationTopography.(Measures{MeasureIdx})(Indexes, :);
        AverageData = average_by_column(Metadata(Indexes, :), Data, 'Participant');

        chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', PlotProps);
        chART.plot.eeglab_topoplot(mean(AverageData, 1), Chanlocs, [], CLims(MeasureIdx, :), '', 'Linear', PlotProps);
        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end
    end

    % plot colorbar
    chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, nAges+1], [], false, '', PlotProps);
    chART.plot.pretty_colorbar('Linear', CLims(MeasureIdx, :), MeasureLabels{MeasureIdx}, PlotProps)

end
chART.save_figure('TopographyAverage', ResultsFolder, PlotProps)



%% topographies by age, overnight changes

% Group = 'HC';
PlotProps.Stats.PlotN= true;

Ages = [8, 11;
    11 14;
    14 16;
    16 20];
% Ages = [8, 10;
%     10 12;
%     12 14;
%     14 16;
%     16 18];

nAges = size(Ages, 1);
Measures = fieldnames(BurstInformationTopography);
nMeasures = numel(Measures);
MeasureLabels = {'cyc/min', 'amplitude (\muV)', 'Frequency (Hz)'};
Task = 'Oddball';

% CLims = [-6 6;
%     -10 10
%     -10 10];

CLims = [-1 1;
    -1 1
    -1 1
    -1 1
    -1 1];
% CLims = [-8 8];
CLims = [-.8 .8];

figure('Units','normalized','OuterPosition',[0 0 .4 .7])
for MeasureIdx = 1:nMeasures
    for AgeIdx = 1:nAges
        % Indexes = strcmp(OvernightMetadata.Group, Group) & OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
        Indexes = OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);

        Evening = BurstInformationTopography.(Measures{MeasureIdx})(OvernightMetadata.EveningIndexes(Indexes), :);
        Morning = BurstInformationTopography.(Measures{MeasureIdx})(OvernightMetadata.MorningIndexes(Indexes), :);
        Evening = average_by_column(OvernightMetadata(Indexes, :), Evening, 'Participant');
        Morning = average_by_column(OvernightMetadata(Indexes, :), Morning, 'Participant');

        chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', PlotProps);
        plot_topography_difference(Evening, Morning, Chanlocs, CLims, Parameters.Stats, PlotProps) %
        colorbar off
        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end
    end
end
        chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx+1], [nMeasures, 1], false, '', PlotProps);
    chART.plot.pretty_colorbar('Divergent', CLims, 'g-values', PlotProps)

chART.save_figure('TopographyChange', ResultsFolder, PlotProps)



%% plot Freq x Age plot, descriptive

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 30;
PlotProps.Axes.yPadding = 20;

Measures = fieldnames(BurstInformationClusters);
nMeasures = numel(Measures);
MeasureLabels = {'amplitude (\muV)', 'cyc/min', '% channels', 'log power (\muV^2/Hz)'};

Ages = 8:2:18;

OvernightMetadata.QuantileAge = discretize(OvernightMetadata.Age, Ages);

figure('Units','normalized','OuterPosition',[0 0 .2 .8])
for MeasureIdx = 1:nMeasures
    % Indexes = strcmp(OvernightMetadata.Group, Group) & OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
    % TempMetadata = OvernightMetadata(Indexes, :);
    TempMetadata = OvernightMetadata;

    Evening = BurstInformationClusters.(Measures{MeasureIdx})(TempMetadata.EveningIndexes, :);
    Morning = BurstInformationClusters.(Measures{MeasureIdx})(TempMetadata.MorningIndexes, :);
    [Evening, UniqueMetadata] = average_by_column(TempMetadata, Evening, 'Participant');
    Morning = average_by_column(TempMetadata, Morning, 'Participant');

    Data = average_by_column(UniqueMetadata, Evening, 'QuantileAge');


    %%% evening values
    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);

    contourf(Ages(1:end-1), Frequencies, Data', 100, 'linecolor','none')
    chART.set_axis_properties(PlotProps)
    colormap(PlotProps.Color.Maps.Linear)
    xticks(8:2:20)
    yticks(5:2:15)
    h = colorbar;
    h.TickLength = 0;
    ylabel(h, MeasureLabels{MeasureIdx}, 'FontName', PlotProps.Text.FontName) % text style needs to be specified for label, because its weird

    title([Measures{MeasureIdx}, ' Evening'])
    ylabel('Frequency (Hz)')
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
end

chART.save_figure('FrequencyByAge', ResultsFolder, PlotProps)


%%


PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 30;
PlotProps.Axes.yPadding = 20;

CLims = [-4 4;
    -.08 .08
      -.08 .08
    -.15 .15];
figure('Units','normalized','OuterPosition',[0 0 .2 .8])
for MeasureIdx = 1:nMeasures
    % Indexes = strcmp(OvernightMetadata.Group, Group) & OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
    % TempMetadata = OvernightMetadata(Indexes, :);
    TempMetadata = OvernightMetadata;

    Evening = BurstInformationClusters.(Measures{MeasureIdx})(TempMetadata.EveningIndexes, :);
    Morning = BurstInformationClusters.(Measures{MeasureIdx})(TempMetadata.MorningIndexes, :);
    [Evening, UniqueMetadata] = average_by_column(TempMetadata, Evening, 'Participant');
    Morning = average_by_column(TempMetadata, Morning, 'Participant');


    %%% difference values
    Change = Morning-Evening;
    Data = average_by_column(UniqueMetadata, Change, 'QuantileAge');


    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);

    contourf(Ages(1:end-1), Frequencies, Data', 100, 'linecolor','none')
    chART.set_axis_properties(PlotProps)

    colormap(PlotProps.Color.Maps.Divergent)
    xticks(8:2:20)
    yticks(5:2:15)
    h = colorbar;
    h.TickLength = 0;
    ylabel('Frequency (Hz)')
    title([Measures{MeasureIdx}, ' Overnight change'])

    if MeasureIdx == nMeasures
        xlabel('Age')
    end
    h = colorbar;
    clim(CLims(MeasureIdx, :))
    h.TickLength = 0;
    ylabel(h, 'difference', 'FontName', PlotProps.Text.FontName) % text style needs to be specified for label, because its weird
end
chART.save_figure('FrequencyByAgeChange', ResultsFolder, PlotProps)










