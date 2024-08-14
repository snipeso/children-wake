%%% plot estimates for each outcome variable for each age, to determine how
%%% large the overnight effect is.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
Paths = Parameters.Paths;
BandLabels = {'Theta', 'Alpha', 'Beta_{low}'};
nBands = numel(BandLabels);
Ages = Parameters.Ages;
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


%% make model (this is a bit slow)

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

Models = cell([nAges, nMeasures, nChannels]);

for MeasureIdx = 1:nMeasures
    for AgeIdx = 1:nAges
        for ChannelIdx = 1:nChannels
            MetadataTemp = MetadataStat(MetadataStat.AgeGroups==AgeIdx, :);
            MetadataTemp.Data = BurstInformationTopography.(Measures{MeasureIdx})(MetadataTemp.Index, ChannelIdx);

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
            Models{AgeIdx, MeasureIdx, ChannelIdx} = fitlme(MetadataTemp, formula);
        end
    end
    disp(['Finished ', Measures{MeasureIdx}])
end



%% plot overnight change topographies (Figure 4)

PlotProps = Parameters.PlotProps.TopoPlots;
PlotProps.External.EEGLAB.MarkerSize = 3;
PlotProps.Text.AxisSize = 16;
PlotProps.Colorbar.Location= 'north';
CLims = struct();
CLims.Amplitude = [-4 4];
CLims.Quantity = [-10 10];
CLims.Slope = [-.15 .15];
CLims.Intercept = [-.15 .15];
CLims.Power = [-.4 .4];
CLims.PeriodicPower = [-.08 .08];

Coefficient = 'Hour_2';
Grid = [nAges+1, 1];


for MeasureIdx = 1:nMeasures
    figure('Units','centimeters','Position',[0 0 10 35])
    for AgeIdx = 1:nAges

        %%% plot
        chART.sub_plot([], Grid, [AgeIdx, 1], [], false, '', PlotProps);
        mixed_model_topography(squeeze(Models(AgeIdx, MeasureIdx, :)), ...
            ColorParameter, Coefficient, Chanlocs, CLims.(Measures{MeasureIdx}), PlotProps)
        colorbar off
    end

    % plot colorbar
    Axes= chART.sub_plot([], Grid, [nAges+1, 1], [], false, '', PlotProps);
    
    axis off
    Axes.Position(1) = .15;
    Axes.Position(3) = .7;
    chART.plot.pretty_colorbar('Divergent', CLims.(Measures{MeasureIdx}), MeaureLabels{MeasureIdx}, PlotProps);
    colormap(PlotProps.Color.Maps.Divergent)
    chART.save_figure(['TopographyChange_', Measures{MeasureIdx}], ResultsFolder, PlotProps)
end




