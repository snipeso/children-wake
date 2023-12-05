
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

% Groups = {'HC', 'ADHD'};
Groups = {'m', 'f'};

Tasks = {'Oddball', 'Alertness'};
YVariables = {'Globality', 'Amplitude', 'Duration', 'Frequency'};
Grid = [3 numel(YVariables)];
Session = 'Session_1';
Colors = chART.color_picker(2);
Markers = {'o', '^'};
Dashes = {':', '-'};
Limits = [.02 .14;
    7 30;
    .5 1.1;
    9 13];

HourLabels = {'Evening', 'Morning'};

figure('Units','normalized','OuterPosition',[0 0 .4 .6])
for VariableIdx = 1:numel(YVariables)
    for HourIdx = 1:numel(Hours)
        Hour = Hours(HourIdx);
        chART.sub_plot([], Grid, [HourIdx, VariableIdx], [], true, '', PlotProps);
        hold on
        for GroupIdx = 1:numel(Groups)
            for TaskIdx = 1:numel(Tasks)
                Indexes = strcmp(Metadata.Sex, Groups{GroupIdx}) & contains(Metadata.Task, Tasks{TaskIdx}) & ...
                    strcmp(Metadata.Hour, Hour) & contains(Metadata.Session, Session);
                % Indexes = strcmp(Metadata.Group, Groups{GroupIdx}) & contains(Metadata.Task, Tasks{TaskIdx}) & ...
                %     strcmp(Metadata.Hour, Hour) & contains(Metadata.Session, Session);
                scatter(Metadata.Age(Indexes), Metadata.(YVariables{VariableIdx})(Indexes), 10, Markers{TaskIdx}, ...
                    'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(GroupIdx,:), 'MarkerFaceAlpha',.7)
                chART.set_axis_properties(PlotProps)
                h = lsline;
                set(h(1),'color',Colors(GroupIdx,:), 'LineWidth', 2)
                if HourIdx==1
                    title(YVariables{VariableIdx})
                end
                if VariableIdx==1
                    ylabel(HourLabels{HourIdx}, 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
                end
                xlim([8 24])
                ylim(Limits(VariableIdx, :))
            end
        end
    end

    chART.sub_plot([], Grid, [3, VariableIdx], [], true, '', PlotProps);

    hold on
    for GroupIdx = 1:numel(Groups)
        for TaskIdx = 1:numel(Tasks)
            Indexes = strcmp(OvernightMetadata.Sex, Groups{GroupIdx}) & contains(OvernightMetadata.Task, Tasks{TaskIdx}) & ...
                contains(OvernightMetadata.Session, Session);
            % Indexes = strcmp(OvernightMetadata.Group, Groups{GroupIdx}) & contains(OvernightMetadata.Task, Tasks{TaskIdx}) & ...
            %     contains(OvernightMetadata.Session, Session);
            scatter(OvernightMetadata.Age(Indexes), OvernightMetadata.(YVariables{VariableIdx})(Indexes), 10, Markers{TaskIdx}, ...
                'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(GroupIdx,:), 'MarkerFaceAlpha',.7)
            chART.set_axis_properties(PlotProps)
            h = lsline;
            set(h(1),'color',Colors(GroupIdx,:), 'LineWidth', 2)
            if VariableIdx==1
                ylabel('Overnight change', 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
            end
            xlabel('Age')
            xlim([8 24])

        end
    end

end
chART.save_figure(['BasicScatterAge', Session], ResultsFolder, PlotProps)


%% topographies by age, descriptive

Group = 'HC';

Ages = [8, 11;
    11 14;
    14 16;
    16 18];
nAges = size(Ages, 1);
Measures = fieldnames(BurstInformationTopography);
nMeasures = numel(Measures);
MeasureLabels = {'cyc/min', 'amplitude (\muV)', 'Frequency (Hz)'};

CLims = [0 .4;
    10, 25;
    8 11.6];

figure('Units','normalized','OuterPosition',[0 0 .4 .6])
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
    -1 1];

figure('Units','normalized','OuterPosition',[0 0 .6 .6])
for MeasureIdx = 1:nMeasures
    for AgeIdx = 1:nAges
        % Indexes = strcmp(OvernightMetadata.Group, Group) & OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
        Indexes = OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);

        Evening = BurstInformationTopography.(Measures{MeasureIdx})(OvernightMetadata.EveningIndexes(Indexes), :);
        Morning = BurstInformationTopography.(Measures{MeasureIdx})(OvernightMetadata.MorningIndexes(Indexes), :);
        Evening = average_by_column(OvernightMetadata(Indexes, :), Evening, 'Participant');
        Morning = average_by_column(OvernightMetadata(Indexes, :), Morning, 'Participant');

        chART.sub_plot([], [nMeasures, nAges], [MeasureIdx, AgeIdx], [], false, '', PlotProps);
        plot_topography_difference(Evening, Morning, Chanlocs, [], Parameters.Stats, PlotProps) %
        % colorbar off
        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end
    end
end



%% plot Freq x Age plot, descriptive



Measures = fieldnames(BurstInformationClusters);
nMeasures = numel(Measures);
MeasureLabels = {'amplitude (\muV)', 'cyc/min'};

Ages = 8:2:18;

OvernightMetadata.QuantileAge = discretize(OvernightMetadata.Age, Ages);

figure('Units','normalized','OuterPosition',[0 0 .2 .5])
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
    colormap(PlotProps.Color.Maps.Linear)
    xticks(8:2:20)
    yticks(5:2:15)
    h = colorbar;
    h.TickLength = 0;
    ylabel(h, MeasureLabels{MeasureIdx}, 'FontName', PlotProps.Text.FontName, ...
        'FontSize', PlotProps.Text.LegendSize) % text style needs to be specified for label, because its weird

    title([Measures{MeasureIdx}, ' Evening'])
    ylabel('Frequency (Hz)')
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
chART.set_axis_properties(PlotProps)

end


%%


CLims = [-4 4;
    -1.5 1.5];
figure('Units','normalized','OuterPosition',[0 0 .2 .5])
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
    ylabel(h, 'difference', 'FontName', PlotProps.Text.FontName, ...
        'FontSize', PlotProps.Text.LegendSize) % text style needs to be specified for label, because its weird
chART.set_axis_properties(PlotProps)
end









