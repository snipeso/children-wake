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
Metadata(~contains(Metadata.Task, {'Oddball', 'GoNoGo'}), :) = []; % only look at first oddball and GoNoGo, since they are most similar
Metadata.Subgroup(strcmp(Metadata.Group, 'HC')) = 5;
Metadata.AgeGroups = string(discretize(Metadata.Age, [Ages(:, 1); Ages(end, 2)]));
Metadata.Task(contains(Metadata.Task, 'GoNoGo')) = {'GoNoGo'}; % pool datasets to same task
Metadata.Task(contains(Metadata.Task, 'Oddball')) = {'Oddball'};
MetadataComplete = Metadata;
nAges = size(Ages, 1);

table_demographics(unique_metadata(Metadata), 'AgeGroups', ResultsFolder, 'DemographicsAgeGroups')


%% Average topographies

CLims = struct();
CLims.Quantity = [5 40];
CLims.Amplitude = [10, 35];
CLims.Power = [-1 1.7];
CLims.PeriodicPower = [0.04 .44];
CLims.Slope = [1.3 2.1];
CLims.Intercept = [.8 2.3];

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);

figure('Units','centimeters','OuterPosition',[0 0 25 30])
for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    for AgeIdx = 1:nAges
        Indexes = ismember(Metadata.AgeGroups, string(AgeIdx));
        AverageSessions = average_by_column(Metadata, Topographies, 'Participant', Indexes);
        TooFewChannels = sum(isnan(AverageSessions), 2) > MinNaNChannels;
        AverageSessions(TooFewChannels, :) = nan; % make nan all channels, too sparse data % TODO, move to assemble data?

        nParticipants = nnz(~TooFewChannels);
        AverageData = mean(AverageSessions, 1, 'omitnan'); % average across participants since its not a stat.

        chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', TopoPlotProps);
        chART.plot.eeglab_topoplot(AverageData, Chanlocs, [], CLims.(Measures{MeasureIdx}), '', 'Linear', TopoPlotProps);
        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end

        if AgeIdx ==1
            chART.plot.vertical_text(Measures{MeasureIdx})
        end
topo_corner_text(['N=', num2str(nParticipants)],TopoPlotProps)
    end

    % plot colorbar
    chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, nAges+1], [], false, '', TopoPlotProps);
    chART.plot.pretty_colorbar('Linear', CLims.(Measures{MeasureIdx}), Measures{MeasureIdx}, TopoPlotProps)

end
chART.save_figure('TopographyAverage', ResultsFolder, TopoPlotProps)

