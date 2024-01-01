clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
nChannels = 123;

ResultsFolder = fullfile(Paths.Results, 'MixedModelADHD');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', 'Chanlocs')
Metadata.Index = [1:size(Metadata, 1)]'; %#ok<NBRAK1> % add index so can chop up table as needed

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);


%% make model

MetadataStat = Metadata;
MetadataStat = make_categorical(MetadataStat, 'Task', {'Oddball', 'Learning', 'GoNoGo', 'Alertness', 'Fixation'});
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'});
MetadataStat.Participant = categorical(MetadataStat.Participant);
MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'});
MetadataStat = make_categorical(MetadataStat, 'Sex', {'f', 'm'});
MetadataStat.Data = nan(size(MetadataStat, 1), 1);

ModelFormula = ' ~ Hour*Age + Task + Group + Sex + (1|Participant)';

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



%% Overnight changes

close all
CLims = [-3 3];
Coefficient = 'Group_2';

Grid = [1, nMeasures+1];

PlotProps = Parameters.PlotProps.TopoPlots;

figure('Units','centimeters','OuterPosition',[0 0 30 8])
for MeasureIdx = 1:nMeasures
    chART.sub_plot([], Grid, [1, MeasureIdx], [], false, '', PlotProps);
    mixed_model_topography(Models(MeasureIdx, :), Chanlocs, CLims, Coefficient, PlotProps)
    colorbar off
    title(Measures{MeasureIdx})
end

Axes = chART.sub_plot([], Grid, [1, MeasureIdx+1], [1, 1], false, '', PlotProps);
Axes.Position(1) = Axes.Position(1)+.02;
chART.plot.pretty_colorbar('Divergent', CLims, "t values", PlotProps)

chART.save_figure('ADHDTopographyChange', ResultsFolder, PlotProps)