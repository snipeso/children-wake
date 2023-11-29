
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

% load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopography', ...
%     "BurstInformationClusters", 'Frequencies', 'Chanlocs')

load(fullfile(CacheDir, CacheName), 'Metadata')

OvernightMetadata = overnight_changes(Metadata);

%% scatterplot of basic information
close all
PlotProps = Parameters.PlotProps.Manuscript;

% PlotProps.Axes.yPadding = 30;
% PlotProps.Axes.xPadding = 30;

Groups = {'HC', 'ADHD'};

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
                Indexes = strcmp(Metadata.Group, Groups{GroupIdx}) & contains(Metadata.Task, Tasks{TaskIdx}) & ...
                    strcmp(Metadata.Hour, Hour) & contains(Metadata.Session, Session);
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

            Indexes = strcmp(OvernightMetadata.Group, Groups{GroupIdx}) & contains(OvernightMetadata.Task, Tasks{TaskIdx}) & ...
                contains(OvernightMetadata.Session, Session);
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
