% plots topographies of ADHD vs HC

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
Paths = Parameters.Paths;
nChannels = 123;
Tasks = {'Oddball', 'Learning', 'GoNoGo', 'Alertness', 'Fixation'}; % oddball first is important; its the reference

Measures =  Parameters.OutcomeMeasures.OriginalLabels;
MeasureTitles = Parameters.OutcomeMeasures.Titles;
MeaureLabels = append('\beta ',{'\muV', '%', 'a.u.', 'log power', 'log power', 'log power'});

nMeasures = numel(Measures);

%%% paths
ResultsFolder = fullfile(Paths.Results, 'MixedModelADHD');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';





%% make model basic values
%%% load data
load(fullfile(CacheDir, CacheName), 'Metadata', ...
    'BurstInformationTopography', 'Chanlocs')
Metadata = basic_metadata_cleanup(Metadata, {'Tasks', Tasks});

%%% run model
MetadataStat = Metadata;
MetadataStat = make_categorical(MetadataStat, 'Task', Tasks);
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'});
MetadataStat.Participant = categorical(MetadataStat.Participant);
MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'});
MetadataStat.Data = nan(size(MetadataStat, 1), 1);

ModelFormula = ' ~ Hour*Age + Task + Group + (1|Participant) + (1|Participant:SessionUnique)';

Models = cell([nMeasures, nChannels]);
for MeasureIdx = 1:nMeasures
    for ChannelIdx = 1:nChannels
        MetadataTemp = MetadataStat;
        MetadataTemp.Data = BurstInformationTopography.(Measures{MeasureIdx})(MetadataTemp.Index, ChannelIdx);
        formula = ['Data', ModelFormula];
        Models{MeasureIdx, ChannelIdx} = fitlme(MetadataTemp, formula);
    end
    disp(['Finished ', Measures{MeasureIdx}])
end



%% Effect of ADHD on outcome measures (Figure 8)

close all
Coefficient = 'Group_2';
ColorParameter = 'Estimate';

CLims = struct();
CLims.Amplitude = [-4 4];
CLims.Quantity = [-10 10];
CLims.Slope = [-.15 .15];
CLims.Intercept = [-.15 .15];
CLims.Power = [-.4 .4];
CLims.PeriodicPower = [-.08 .08];

Grid = [1, nMeasures];

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Color.Steps.Divergent = 20;
PlotProps.Colorbar.Location = 'southoutside';

figure('Units','centimeters','OuterPosition',[0 0 25 10])
for MeasureIdx = 1:nMeasures
    chART.sub_plot([], Grid, [1, MeasureIdx], [], false, '', PlotProps);
    mixed_model_topography(squeeze(Models(MeasureIdx, :)), ...
        ColorParameter, Coefficient, Chanlocs, CLims.(Measures{MeasureIdx}), PlotProps, MeasureLabels{MeasureIdx})
    title(MeasureTitles{MeasureIdx}, 'FontSize',PlotProps.Text.TitleSize)
    CB = get(gca, 'colorbar');
    CB.Position(2) = .22;
    CB.Label.Units = 'normalized';
    CB.Label.Position(2) = -2.5;
end

chART.save_figure('ADHDTopography', ResultsFolder, PlotProps)
