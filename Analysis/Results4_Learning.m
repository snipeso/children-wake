clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Hours = Parameters.Hours;
BandLabels = {'Theta', 'Low Alpha', 'High Alpha'};
TopoPlotProps = Parameters.PlotProps.TopoPlots;
Ages = Parameters.Ages;
nAges = size(Ages, 1);
nChannels = 123;

ResultsFolder = fullfile(Paths.Results, 'AverageTopographies');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

MinNaNChannels = 25; % for amplitudes

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', 'Chanlocs')
Metadata.Index = [1:size(Metadata, 1)]'; %#ok<NBRAK1> % add index so can chop up table as needed

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);


%% make model

MetadataStat = Metadata;
MetadataStat = make_categorical(MetadataStat, 'Task', {'1Oddball', '3Oddball'});
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'});
MetadataStat.Participant = categorical(MetadataStat.Participant);
MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'});
MetadataStat = make_categorical(MetadataStat, 'Condition', {'base', 'rotation'});

MetadataStat.Data = nan(size(MetadataStat, 1), 1);

ModelFormula = ' ~ Age*Hour + Task*Condition + (1|Participant)';

Models = cell([nMeasures, nChannels]);
for MeasureIdx = 1:nMeasures
    for ChannelIdx = 1:nChannels

        MetadataStat.Data = BurstInformationTopography.(Measures{MeasureIdx})(MetadataStat.Index, ChannelIdx);
        formula = ['Data', ModelFormula];
        Models{MeasureIdx, ChannelIdx} = fitlme(MetadataStat, formula);

    end
    disp(['Finished ', Measures{MeasureIdx}])
end



%% Overnight changes

CLims = [-2 2];

EveningMetadata = pair_recordings(Metadata, 'Hour', {'eve', 'mor'});
MorningMetadata = EveningMetadata;
MorningMetadata.Index = EveningMetadata.IndexesCategory2;



% figure('Units','normalized','Position', [0 0 TopoFigureSizes(1) TopoFigureSizes(2)*nMeasures])
figure('Units','centimeters','OuterPosition',[0 0 25 30])

for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    for AgeIdx = 2:nAges
        Indexes = ismember(EveningMetadata.AgeGroups, string(AgeIdx));

        Evening = average_by_column(EveningMetadata, Topographies, 'Participant', Indexes);
        Morning = average_by_column(MorningMetadata, Topographies, 'Participant', Indexes);

        chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', TopoPlotProps);
        plot_topography_difference(Evening, Morning, Chanlocs, CLims, Parameters.Stats, TopoPlotProps) %
        colorbar off

        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end

        if AgeIdx ==2
            X = get(gca, 'XLim');
            Y = get(gca, 'YLim');
            text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasureIdx}, ...
                'FontSize', TopoPlotProps.Text.TitleSize, 'FontName', TopoPlotProps.Text.FontName, ...
                'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
        end
    end
end

chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx+1], [nMeasures, 1], false, '', TopoPlotProps);
chART.plot.pretty_colorbar('Divergent', CLims, "Cohen's d", TopoPlotProps)

chART.save_figure('TopographyChange', ResultsFolder, TopoPlotProps)