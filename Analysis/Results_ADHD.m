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

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
MeasureLabels = append('\beta ',{'\muV', '%', '', 'log power', 'log power', 'log power'});

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
load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
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



%% Effect of ADHD on outcome measures

close all
CLims = [-3 3];
Coefficient = 'Group_2';
ColorParameter = 'Estimate';


Grid = [1, nMeasures];

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Color.Steps.Divergent = 20;

figure('Units','centimeters','OuterPosition',[0 0 35 8])
for MeasureIdx = 1:nMeasures
    chART.sub_plot([], Grid, [1, MeasureIdx], [], false, '', PlotProps);

    mixed_model_topography(squeeze(Models(MeasureIdx, :)), ...
        ColorParameter, Coefficient, Chanlocs, [], PlotProps)
    h = colorbar;
h.TickLength = 0;
ylabel(h, MeasureLabels{MeasureIdx}) % text style needs to be specified for label, because its weird

    title(Measures{MeasureIdx})
end

chART.save_figure('ADHDTopography', ResultsFolder, PlotProps)



%% make model overnight change values

load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', 'Chanlocs')
Metadata = basic_metadata_cleanup(Metadata);


MetadataStat = pair_recordings(Metadata, 'Hour', {'eve', 'mor'});

MetadataStat = make_categorical(MetadataStat, 'Task', Tasks);
MetadataStat.Participant = categorical(MetadataStat.Participant);
MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'});
MetadataStat.Data = nan(size(MetadataStat, 1), 1);

formula = 'Overnight ~ Age + Task + Group + (1|Participant) + (1|Participant:SessionUnique)';

OvernightModels = cell([nMeasures, nChannels]);
for MeasureIdx = 1:nMeasures
    for ChannelIdx = 1:nChannels
        MetadataTemp = MetadataStat;
        Evening = BurstInformationTopography.(Measures{MeasureIdx})(MetadataTemp.IndexesCategory1, ChannelIdx);
        Morning = BurstInformationTopography.(Measures{MeasureIdx})(MetadataTemp.IndexesCategory2, ChannelIdx);
        MetadataTemp.Overnight = Morning-Evening;
        OvernightModels{MeasureIdx, ChannelIdx} = fitlme(MetadataTemp, formula);
    end
    disp(['Finished ', Measures{MeasureIdx}])
end



%% Overnight changes

close all
CLims = [-3 3];
Coefficient = 'Group_2';
ColorParameter = 'Estimate';

Grid = [1, nMeasures];

PlotProps = Parameters.PlotProps.Manuscript;

figure('Units','centimeters','OuterPosition',[0 0 30 8])
for MeasureIdx = 1:nMeasures
    chART.sub_plot([], Grid, [1, MeasureIdx], [], false, '', PlotProps);

    mixed_model_topography(squeeze(OvernightModels(MeasureIdx, :)), ...
        ColorParameter, Coefficient, Chanlocs, [], PlotProps)
    title(Measures{MeasureIdx})
end

chART.save_figure('ADHDTopographyOvernight', ResultsFolder, PlotProps)