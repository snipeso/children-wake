clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;

PlotProps = Parameters.PlotProps.TopoPlots;
% PlotProps = Parameters.PlotProps.Manuscript;
nChannels = 123;

% Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept'};
nMeasures = numel(Measures);


Ages = [ 7 14;
    14 25];
nAges = size(Ages, 1);


ResultsFolder = fullfile(Paths.Results, 'MixedModelLearning');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end


CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', 'Chanlocs')
Metadata = basic_metadata_cleanup(Metadata, {'Ages', Ages, 'Datasets', {'SleepLearning'}});


%% make model

MetadataStat = pair_recordings(Metadata, 'Task', {'1Oddball', '3Oddball'});
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'});
MetadataStat.Participant = categorical(MetadataStat.Participant);
MetadataStat = make_categorical(MetadataStat, 'Condition', {'base', 'rotation'});

MetadataStat.Data = nan(size(MetadataStat, 1), 1);

formula = 'Oddball3 ~ Hour + Oddball1 + Condition + (1|Participant) + (1|Participant:SessionUnique)';

Models = cell([nAges, nMeasures, nChannels]);
for MeasureIdx = 1:nMeasures
    for AgeIdx = 1:nAges
        for ChannelIdx = 1:nChannels
            MetadataTemp = MetadataStat(MetadataStat.AgeGroups==AgeIdx, :);
            MetadataTemp.Oddball1 = ...
                BurstInformationTopography.(Measures{MeasureIdx})(MetadataTemp.IndexesCategory1, ChannelIdx);

            MetadataTemp.Oddball3 = ...
                BurstInformationTopography.(Measures{MeasureIdx})(MetadataTemp.IndexesCategory2, ChannelIdx);

            Models{AgeIdx, MeasureIdx, ChannelIdx} = fitlme(MetadataTemp, formula);
        end
    end
    disp(['Finished ', Measures{MeasureIdx}])
end



%% Overnight changes

close all
CLims = [-5 5];
Coefficient = 'Condition_2';
ColorParameter = 'Estimate';

figure('Units','centimeters','OuterPosition',[0 0 10 30])

for AgeIdx = 1:nAges
    for MeasureIdx = 1:nMeasures

        chART.sub_plot([], [nMeasures+1, nAges], [MeasureIdx, AgeIdx], [], false, '', PlotProps);
        mixed_model_topography(squeeze(Models(AgeIdx, MeasureIdx, :))', ...
           ColorParameter, Coefficient, Chanlocs, [], PlotProps)
        % colorbar off
        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end

        if AgeIdx ==1
            chART.plot.vertical_text(Measures{MeasureIdx}, .15, .5, PlotProps)
        end

    end
end
chART.sub_plot([], [nMeasures+1, nAges], [MeasureIdx+1, 1], [1, nAges], false, '', PlotProps);
PlotProps.Colorbar.Location = 'north';
chART.plot.pretty_colorbar('Divergent', CLims, "t values", PlotProps)

% chART.save_figure('TopographyChange', ResultsFolder, TopoPlotProps)