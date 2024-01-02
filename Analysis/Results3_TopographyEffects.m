%%% plot estimates for each outcome variable for each age, to determine how
%%% large the overnight effect is.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.TopoPlots;
Paths = Parameters.Paths;
BandLabels = {'Theta', 'Low Alpha', 'High Alpha'};
Ages = Parameters.Ages;
Ages = Ages(2:end, :); % exclude youngest group; too few
nAges = size(Ages, 1);
nChannels = 123;
Tasks = {'Oddball', 'GoNoGo', 'Alertness', 'Fixation'}; % oddball first is important; its the reference
Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
MeaureLabels = append('\beta ',{'\muV', '%', '', 'log power', 'log power', 'log power'});
ColorParameter = 'Estimate';
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



%% make model

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
            MetadataTemp = MetadataStat(strcmp(MetadataStat.AgeGroups, num2str(AgeIdx)), :);
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



%% Overnight changes

close all
CLims = struct();
CLims.Amplitude = [-4 4];
CLims.Quantity = [-10 10];
CLims.Slope = [-.15 .15];
CLims.Intercept = [-.15 .15];
CLims.Power = [-.4 .4];
CLims.PeriodicPower = [-.08 .08];

Coefficient = 'Hour_2';
Grid = [nMeasures, nAges+1];

figure('Units','centimeters','OuterPosition',[0 0 22 30])

for MeasureIdx = 1:nMeasures
    for AgeIdx = 1:nAges

        %%% plot
        chART.sub_plot([], Grid, [MeasureIdx, AgeIdx], [], false, '', PlotProps);
        mixed_model_topography(squeeze(Models(AgeIdx, MeasureIdx, :)), ColorParameter, Coefficient, Chanlocs, CLims.(Measures{MeasureIdx}), PlotProps)
        colorbar off
        
        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end

        if AgeIdx ==1
            chART.plot.vertical_text(Measures{MeasureIdx}, .15, .5, PlotProps)
        end
    end

    % plot colorbar
    chART.sub_plot([], Grid, [MeasureIdx, nAges+1], [], false, '', PlotProps);
    chART.plot.pretty_colorbar('Divergent', CLims.(Measures{MeasureIdx}), MeaureLabels{MeasureIdx}, PlotProps)
end


% chART.save_figure('TopographyChange', ResultsFolder, TopoPlotProps)

