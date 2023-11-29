
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

Groups = {'HC', 'ADHD'};
Tasks = {'Oddball', 'Alertness'};
YVariables = {'Globality', 'Amplitude', 'Duration'};
Hour = 'mor';
Session = 'Session_1';
Colors = chART.color_picker(2);
Markers = {'o', '^'};

for Variable = YVariables
    figure
    hold on
    for GroupIdx = 1:numel(Groups)
        for TaskIdx = 1:numel(Tasks)

            Indexes = strcmp(Metadata.Group, Groups{GroupIdx}) & contains(Metadata.Task, Tasks{TaskIdx}) & ...
                strcmp(Metadata.Hour, Hour) & contains(Metadata.Session, Session);
            scatter(Metadata.Age(Indexes), Metadata.(Variable{1})(Indexes), 10, Markers{TaskIdx}, ...
                'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(GroupIdx,:))
            lsline
            xlabel('Age')
            ylabel(Variable{1})
            title(Hour)
        end
    end
    chART.save_figure(['BasicScatterAge', Hour, Session,Variable{1}], ResultsFolder, PlotProps)
end


%% Overnight change




