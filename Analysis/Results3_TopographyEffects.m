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
Metadata.AgeGroups = discretize(Metadata.Age, [Ages(:, 1); Ages(end, 2)]);
Metadata.Session(strcmp(Metadata.Session, 'Session_1_1')) = {'Session_1'};
Metadata.Session(strcmp(Metadata.Session, 'Session_1_2')) = {'Session_2'};


Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);




%% make model

MetadataStat = Metadata;
MetadataStat = make_categorical(MetadataStat, 'Task', {'Oddball', 'Learning', 'GoNoGo', 'Alertness', 'Fixation'});
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'});
MetadataStat.Participant = categorical(MetadataStat.Participant);
% MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'});
MetadataStat = make_categorical(MetadataStat, 'Sex', {'f', 'm'});
MetadataStat = make_categorical(MetadataStat, 'Session', {'Session_1', 'Session_2'});


MetadataStat.Data = nan(size(MetadataStat, 1), 1);

ModelFormula = ' ~ Hour + Task + Session + Sex + (1|Participant)';
% ModelFormula = ' ~ Hour + (1|Participant)';
% ModelFormula = ' ~ Age*Hour + Task*Condition + (1|Participant)';
% ModelFormula = ' ~ Hour + Task + Group + Sex + (1|Participant)';

Models = cell([nAges, nMeasures, nChannels]);
for MeasureIdx = 1:nMeasures
    for AgeIdx = 1:nAges
        for ChannelIdx = 1:nChannels
            MetadataTemp = MetadataStat(MetadataStat.AgeGroups==AgeIdx, :);
            MetadataTemp.Data = BurstInformationTopography.(Measures{MeasureIdx})(MetadataTemp.Index, ChannelIdx);
            formula = ['Data', ModelFormula];
            Models{AgeIdx, MeasureIdx, ChannelIdx} = fitlme(MetadataTemp, formula);
        end
    end
    disp(['Finished ', Measures{MeasureIdx}])
end



%% Overnight changes

close all
% CLims = [-15 15];
CLims = [];
Coefficient = 'Hour_2';

figure('Units','centimeters','OuterPosition',[0 0 25 30])

for AgeIdx = 1:nAges
    for MeasureIdx = 1:nMeasures

        chART.sub_plot([], [nMeasures+1, nAges], [MeasureIdx, AgeIdx], [], false, '', PlotProps);
        mixed_model_topography(squeeze(Models(AgeIdx, MeasureIdx, :)), Chanlocs, CLims, Coefficient, PlotProps)
        colorbar off
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

