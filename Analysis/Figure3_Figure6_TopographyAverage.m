% plots the average topographies for different ages

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

Parameters = analysisParameters();
Paths = Parameters.Paths;
BandLabels = {'Theta', 'Alpha', 'Beta_{low}'};
PlotProps = Parameters.PlotProps.TopoPlots;
Ages = Parameters.Ages;
nAges = size(Ages, 1);

MinNaNChannels = 25; % exclude participants from average if missing too many channels
Tasks = {'Oddball'};


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

% fixes to metadata
Metadata = basic_metadata_cleanup(Metadata, {'Ages', Ages, 'Tasks', Tasks});
Metadata(~contains(Metadata.Task, Tasks), :) = []; % only look at first oddball and GoNoGo, since they are most similar

% tabulate the age groups
table_demographics(unique_metadata(Metadata), 'AgeGroups', ResultsFolder, 'AgeGroupsAverage')


%% Average topographies (Figure 3)

PlotProps = Parameters.PlotProps.TopoPlots;
PlotProps.External.EEGLAB.MarkerSize = 3;
PlotProps.Text.AxisSize = 16;
PlotProps.Colorbar.Location= 'north';

CLims = struct();
CLims.Quantity = [5 40];
CLims.Amplitude = [12, 30];
CLims.Slope = [1.3 2.1];
CLims.Intercept = [.8 2.3];
CLims.Power = [-.7  1.7];
CLims.PeriodicPower = [0.1 .44];

Grid = [nAges+1, 1];

Measures = Parameters.OutcomeMeasures.OriginalLabels;
MeasuresTitles = Parameters.OutcomeMeasures.Titles;

MeasureUnits = {'\muV', '% recording', 'a.u.', 'log power', 'log power', 'log power'};
nMeasures = numel(Measures);

for MeasureIdx = 1:nMeasures
        figure('Units','centimeters','Position',[0 0 10 35])

    Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    for AgeIdx = 1:nAges

        % assemble and average data
        Indexes = Metadata.AgeGroups==AgeIdx;
        AverageSessions = average_by_column(Metadata, Topographies, 'Participant', Indexes);
        TooFewChannels = sum(isnan(AverageSessions), 2) > MinNaNChannels;
        AverageSessions(TooFewChannels, :) = nan; % make nan all channels, too sparse data % TODO, move to assemble data?

        nParticipants = nnz(~TooFewChannels);
        AverageData = mean(AverageSessions, 1, 'omitnan'); % average across participants since its not a stat.

        %%% plot
        chART.sub_plot([], Grid, [AgeIdx, 1], [], false, '', PlotProps);
        chART.plot.eeglab_topoplot(AverageData, Chanlocs, [], CLims.(Measures{MeasureIdx}), '', 'Linear', PlotProps);
        
    end

    % plot colorbar
    Axes= chART.sub_plot([], Grid, [nAges+1, 1], [], false, '', PlotProps);
        axis off
    Axes.Position(1) = .15;
    Axes.Position(3) = .7;
    chART.plot.pretty_colorbar('Linear', CLims.(Measures{MeasureIdx}), MeasureUnits{MeasureIdx}, PlotProps)
  chART.save_figure(['Topography_', Measures{MeasureIdx}], ResultsFolder, PlotProps)

end





%% Average topographies, split by band (Figure 6)

CLims = struct();
CLims.Quantity = [0 5; 3 30; 0 6.5];
CLims.Amplitude = [-1, 18; 10, 30; 1, 16];
CLims.Power = [-.5 2.5; -.25 2.25; -1.5 .5];
CLims.PeriodicPower = [0.05 .3; .2 .8; -.05 .4];

Measures = Parameters.OutcomeMeasures.OriginalLabels([1, 2, 5, 6]); % exclude aperiodic labels
MeasuresTitles = Parameters.OutcomeMeasures.Titles([1, 2, 5, 6]);
MeasureUnits = {'\muV', '% recording', 'log power', 'log power'};
nMeasures = numel(Measures);

for MeasureIdx = 2 %1:nMeasures
    Topographies = BurstInformationTopographyBands.(Measures{MeasureIdx});
    nBands = size(Topographies, 3);

    figure('Units','centimeters','OuterPosition',[0 0 25 16])
    for BandIdx = 1:nBands
        for AgeIdx = 1:nAges

            % gather data
            Indexes = Metadata.AgeGroups==AgeIdx;
            AverageSessions = average_by_column(Metadata, ...
                Topographies(:, :, BandIdx), 'Participant', Indexes);

            TooFewChannels = sum(isnan(AverageSessions), 2) > MinNaNChannels;
            nParticipants = nnz(~TooFewChannels);
            if nParticipants<2 % handle too little data and titles
                if BandIdx == 1
                    chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', PlotProps);
                    title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2)), ' y.o.'], 'FontSize', PlotProps.Text.TitleSize)
                    axis off
                end

                if AgeIdx == 1
                    chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', PlotProps);
                    axis off
                    chART.plot.vertical_text(BandLabels{BandIdx}, .1, .5, PlotProps)
                end
                continue
            end
            AverageSessions(TooFewChannels, :) = nan; % make nan all channels, too sparse data % TODO, move to assemble data?
            AverageData = mean(AverageSessions, 1, 'omitnan'); % average across participants since its not a stat.

            %%% plot
            chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', PlotProps);
            chART.plot.eeglab_topoplot(AverageData, Chanlocs, [], CLims.(Measures{MeasureIdx})(BandIdx, :), '', 'Linear', PlotProps);
            if BandIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2)), ' y.o.'])
            end

            if AgeIdx == 1
                chART.plot.vertical_text(BandLabels{BandIdx}, .1, .5, PlotProps)
            end

            topo_corner_text(['N=', num2str(nParticipants)],PlotProps)
        end

        % plot colorbar
        chART.sub_plot([], [nBands, nAges+1], [BandIdx, nAges+1], [], false, '', PlotProps);
        chART.plot.pretty_colorbar('Linear', CLims.(Measures{MeasureIdx})(BandIdx, :), MeasureUnits{MeasureIdx}, PlotProps)
    end
    chART.save_figure(['TopographyBandAverage_', Measures{MeasureIdx}], ResultsFolder, PlotProps)
end

