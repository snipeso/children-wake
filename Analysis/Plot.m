clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Hours = Parameters.Hours;
Bands = Parameters.Bands;
BandLabels = {'Theta', 'Low Alpha', 'High Alpha'};


% Topography plotprops
TopoPlotProps = Parameters.PlotProps.Manuscript;
TopoPlotProps.Text.LegendSize = 10;
TopoPlotProps.Text.AxisSize = 10;
TopoPlotProps.Axes.xPadding = 8;
TopoPlotProps.Axes.yPadding = 5;
TopoPlotProps.Figure.Padding = 20;
TopoPlotProps.Stats.PlotN = true;
TopoFigureSizes = [.4, .11];

% Ages = [2 8; % too few little kids
%     8, 11;% 3 year age jumps, like Kurth et al. 2010
%     11 14;
%     14 17;
%     17, 20;
%     20 25];
Ages = [3 7;
    7 10;
    10 14;
    14 18;
    18 25];

ResultsFolder = fullfile(Paths.Results, 'Main');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', "BurstInformationClusters", 'Frequencies', 'Chanlocs')

% select data for the paper
Metadata.Index = [1:size(Metadata, 1)]'; %#ok<NBRAK1> % add index so can chop up table as needed
Metadata(strcmp(Metadata.Dataset, 'SleepLearning') & ...
    contains(Metadata.Session, {'Session_2', 'Session_3'}), :) = []; % remove repeated measures 1 year later (will average recordings a couple weeks apart)
Metadata(contains(Metadata.Task, {'3Oddball', '1GoNoGo', '2Learning', '3Fixation', '4Fixation'}), :) = []; % only look at first oddball and alertness task
Metadata.Subgroup(strcmp(Metadata.Group, 'HC')) = 5;
Metadata.Globality = Metadata.Globality*100; % make it percentage instead of proportion
Metadata.AgeGroups = string(discretize(Metadata.Age, [Ages(:, 1); Ages(end, 2)]));
Metadata.Task(contains(Metadata.Task, 'Alertness')) = {'Alertness'}; % Fix because different order in task

MetadataComplete = Metadata;
Metadata(contains(Metadata.Group, 'ADHD'), :) = [];
nAges = size(Ages, 1);


%% Demographics

% disp_demographics(unique_metadata(Metadata), 'Dataset')
table_demographics(unique_metadata(Metadata), 'Dataset', ResultsFolder, 'DemographicsDatasets')


%% scatterplot of basic information
close all
PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 20;
YVariables = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
Grid = [3 numel(YVariables)];

YLimits = [5, 42; % amplitudes
    70, 550; % quantities
    .7 2.25; % slope
    .3, 2.5; % intercept
    -1.6, 2; % power
    -.05, .705; % periodic power
    ];
XLim = [3 25];

HourLabels = {'Evening', 'Morning'};
OvernightMetadata = overnight_changes(Metadata);

% GroupColumns = {'', 'Sex', 'Dataset'};
GroupColumns = {''};

for GC = GroupColumns
    GroupColumn = GC{1};
    % figure('Units','normalized','OuterPosition',[0 0 .4 .5])
    figure('Units','centimeters','OuterPosition',[0 0 25 18])
    for VariableIdx = 1:numel(YVariables)

        %%% plot age x v split by evening and morning, averaged across sessions
        for HourIdx = 1:numel(Hours)
            Hour = Hours(HourIdx);

            Indexes = strcmp(Metadata.Hour, Hour);
            TempMetadata = Metadata(Indexes, :);
            AverageMetadata = unique_metadata(TempMetadata, 'Participant');

            chART.sub_plot([], Grid, [HourIdx, VariableIdx], [], true, '', PlotProps);
            plot_scattercloud(AverageMetadata, 'Age', YVariables{VariableIdx}, ...
                PlotProps, GroupColumn, false, XLim, YLimits(VariableIdx, :))
            legend off

            if HourIdx==1
                title(YVariables{VariableIdx})
            end
            if VariableIdx==1
                ylabel(HourLabels{HourIdx}, 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
            end
        end

        %%% plot overnight change
        chART.sub_plot([], Grid, [3, VariableIdx], [], true, '', PlotProps);
        AverageMetadata = unique_metadata(OvernightMetadata, 'Participant');

        plot_scattercloud(AverageMetadata, 'Age', YVariables{VariableIdx}, ...
            PlotProps, GroupColumn, true, XLim)
        if VariableIdx ~=numel(YVariables)
            legend off
        end
        if VariableIdx==1
            ylabel('Overnight change', 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
        end
        xlabel('Age')
    end
    chART.save_figure(['BasicScatterAge', GroupColumn], ResultsFolder, PlotProps)
end

%% correlate measures

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 25;
PlotProps.Text.TitleSize = 10;
PlotProps.Axes.yPadding = 5;
PlotProps.Axes.xPadding = 5;
PlotProps.Scatter.Size = 5;
PlotProps.Scatter.Alpha = .4;

YVariables = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
Grid = [numel(YVariables) numel(YVariables)];
% figure('Units','centimeters','InnerPosition',[0 0 18 18])
figure('Units','centimeters','OuterPosition',[0 0 18 18])
for Idx1 = 1:numel(YVariables)
    for Idx2 = 1:numel(YVariables)
        chART.sub_plot([], Grid, [Idx2, Idx1], [], false, '', PlotProps);

        if Idx1==Idx2
            if Idx1==1
                chART.set_axis_properties(PlotProps)
                title(YVariables{Idx1})
                ylabel(YVariables{Idx2})
            end
            axis off
            continue
        end

        plot_scattercloud(correct_for_age(Metadata), YVariables{Idx1}, YVariables{Idx2}, PlotProps, '', false)
        set(gca, 'XTick' ,[], 'YTick', [])
        axis square
        if Idx2 == numel(YVariables)
            xlabel(YVariables{Idx1})
        elseif Idx2==1
            title(YVariables{Idx1})
        end
        if Idx1==1
            ylabel(YVariables{Idx2})
        end
    end
end
chART.save_figure('CorrelateVariables', ResultsFolder, PlotProps)




%% save demographic data for each age range

table_demographics(unique_metadata(Metadata), 'AgeGroups', ResultsFolder, 'DemographicsAgeGroups')


%% Average topographies

CLims = struct();
CLims.Quantity = [5 40];
CLims.Amplitude = [10, 35];
CLims.Power = [-1 1.7];
CLims.PeriodicPower = [0.04 .44];
CLims.Slope = [1.3 2.1];
CLims.Intercept = [.8 2.3];


Measures = fieldnames(BurstInformationTopography);
Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);

% figure('Units','normalized','Position', [0 0 TopoFigureSizes(1) TopoFigureSizes(2)*nMeasures])
figure('Units','centimeters','OuterPosition',[0 0 25 30])
for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    for AgeIdx = 1:nAges
        Indexes = ismember(Metadata.AgeGroups, string(AgeIdx));
        AverageSessions = average_by_column(Metadata, Topographies, 'Participant', Indexes);
        nParticipants = nnz(~isnan(mean(AverageSessions, 2)));
        AverageData = mean(AverageSessions, 1, 'omitnan'); % average across participants since its not a stat.

        chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', TopoPlotProps);
        chART.plot.eeglab_topoplot(AverageData, Chanlocs, [], CLims.(Measures{MeasureIdx}), '', 'Linear', TopoPlotProps);
        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end

        if AgeIdx ==1
            X = get(gca, 'XLim');
            Y = get(gca, 'YLim');
            text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasureIdx}, ...
                'FontSize', TopoPlotProps.Text.TitleSize, 'FontName', TopoPlotProps.Text.FontName, ...
                'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
        end

        text(.4, .5, ['N=', num2str(nParticipants)], 'FontName', TopoPlotProps.Text.FontName, 'FontSize', TopoPlotProps.Text.LegendSize)
    end

    % plot colorbar
    chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, nAges+1], [], false, '', TopoPlotProps);
    chART.plot.pretty_colorbar('Linear', CLims.(Measures{MeasureIdx}), Measures{MeasureIdx}, TopoPlotProps)

end
chART.save_figure('TopographyAverage', ResultsFolder, TopoPlotProps)


%% Average topographies, split by band

CLims = struct();
CLims.Quantity = [0 5; 3 30; 0 6.5];
CLims.Amplitude = [-1, 18; 10, 30; 1, 16];
CLims.Power = [-.5 2.5; -.25 2.25; -1.5 .5];
CLims.PeriodicPower = [0.05 .3; .2 .8; -.05 .4];

% Measures = fieldnames(BurstInformationTopographyBands);
Measures = {'Amplitude', 'Quantity', 'Power', 'PeriodicPower'};
MeasureLabels = {'\muV', '% recording', 'Log power', 'Log power'};
nMeasures = numel(Measures);

for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopographyBands.(Measures{MeasureIdx});
    nBands = size(Topographies, 3);
    figure('Units','centimeters','OuterPosition',[0 0 25 16])

    % figure('Units','normalized','Position', [0 0 TopoFigureSizes(1) TopoFigureSizes(2)*nMeasures])
    for BandIdx = 1:nBands
        for AgeIdx = 1:nAges
            Indexes = ismember(Metadata.AgeGroups, string(AgeIdx));
            AverageSessions = average_by_column(Metadata, ...
                Topographies(:, :, BandIdx), 'Participant', Indexes);
            nParticipants = nnz(~isnan(mean(AverageSessions, 2)));

            AverageData = mean(AverageSessions, 1, 'omitnan'); % average across participants since its not a stat.

            chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', TopoPlotProps);
            chART.plot.eeglab_topoplot(AverageData, Chanlocs, [], CLims.(Measures{MeasureIdx})(BandIdx, :), '', 'Linear', TopoPlotProps);
            if BandIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
            end

            if AgeIdx ==1
                X = get(gca, 'XLim');
                Y = get(gca, 'YLim');
                text(X(1)-diff(X)*.1, Y(1)+diff(Y)*.5, BandLabels{BandIdx}, ...
                    'FontSize', TopoPlotProps.Text.TitleSize, 'FontName', TopoPlotProps.Text.FontName, ...
                    'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
            end

            text(.4, .5, ['N=', num2str(nParticipants)], 'FontName', TopoPlotProps.Text.FontName, 'FontSize', TopoPlotProps.Text.LegendSize)
        end

        % plot colorbar
        chART.sub_plot([], [nBands, nAges+1], [BandIdx, nAges+1], [], false, '', TopoPlotProps);
        chART.plot.pretty_colorbar('Linear', CLims.(Measures{MeasureIdx})(BandIdx, :), MeasureLabels{MeasureIdx}, TopoPlotProps)
    end
    chART.save_figure(['TopographyBandAverage_', Measures{MeasureIdx}], ResultsFolder, TopoPlotProps)
end


%% topography differences

CLims = [-2 2];

EveningMetadata = overnight_changes(Metadata);
MorningMetadata = EveningMetadata;
MorningMetadata.Index = EveningMetadata.MorningIndexes;

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);

figure('Units','normalized','Position', [0 0 TopoFigureSizes(1) TopoFigureSizes(2)*nMeasures])
figure('Units','centimeters','OuterPosition',[0 0 25 30])

for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    for AgeIdx = 1:nAges
        Indexes = ismember(EveningMetadata.AgeGroups, string(AgeIdx));

        Evening = average_by_column(EveningMetadata, Topographies, 'Participant', Indexes);
        Morning = average_by_column(MorningMetadata, Topographies, 'Participant', Indexes);

        chART.sub_plot([], [nMeasures, nAges+1], [MeasureIdx, AgeIdx], [], false, '', TopoPlotProps);
        plot_topography_difference(Evening, Morning, Chanlocs, CLims, Parameters.Stats, TopoPlotProps) %
        colorbar off

        if MeasureIdx == 1
            title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
        end

        if AgeIdx ==1
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


%% Overnight topographies split by band


EveningMetadata = overnight_changes(Metadata);
MorningMetadata = EveningMetadata;
MorningMetadata.Index = EveningMetadata.MorningIndexes;


CLims = [-1.5 1.5];

Measures = fieldnames(BurstInformationTopographyBands);
nMeasures = numel(Measures);

for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopographyBands.(Measures{MeasureIdx});
    nBands = size(Topographies, 3);

    % figure('Units','normalized','Position', [0 0 TopoFigureSizes(1) TopoFigureSizes(2)*nMeasures])
    figure('Units','centimeters','OuterPosition',[0 0 25 16])

    for BandIdx = 1:nBands
        for AgeIdx = 1:nAges
            Indexes = ismember(EveningMetadata.AgeGroups, string(AgeIdx));

            Evening = average_by_column(EveningMetadata, Topographies(:, :, BandIdx), 'Participant', Indexes);
            Morning = average_by_column(MorningMetadata, Topographies(:, :, BandIdx), 'Participant', Indexes);

            chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', TopoPlotProps);
            plot_topography_difference(Evening, Morning, Chanlocs, CLims, Parameters.Stats, TopoPlotProps) %
            colorbar off

            if BandIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
            end

            if AgeIdx ==1
                X = get(gca, 'XLim');
                Y = get(gca, 'YLim');
                text(X(1)-diff(X)*.1, Y(1)+diff(Y)*.5, BandLabels{BandIdx}, ...
                    'FontSize', TopoPlotProps.Text.TitleSize, 'FontName', TopoPlotProps.Text.FontName, ...
                    'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
            end
        end
    end

    chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx+1], [nBands, 1], false, '', TopoPlotProps);
    chART.plot.pretty_colorbar('Divergent', CLims, strjoin([Measures{MeasureIdx}, "Cohen's d"], ' '), TopoPlotProps)

    chART.save_figure(['TopographyBandChange_', Measures{MeasureIdx}], ResultsFolder, TopoPlotProps)
end



%% plot Freq x Age plot

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 25;
PlotProps.Axes.yPadding = 20;

Measures = {'Amplitude', 'Quantity', 'Power', 'PeriodicPower'};
Labels = {'\muV', '%', 'log power', 'log power'};
nMeasures = numel(Measures);

EquidistantAges = 4:4:25;

Metadata.EquispacedAges = discretize(Metadata.Age, EquidistantAges);
OvernightMetadata = overnight_changes(Metadata);

% figure('Units','normalized','OuterPosition',[0 0 .18 1])
    figure('Units','centimeters','OuterPosition',[0 0 10 22])

for MeasureIdx = 1:nMeasures
    Spectrogram = BurstInformationClusters.(Measures{MeasureIdx});
    Evening = average_by_column(OvernightMetadata, Spectrogram, 'Participant', [1:size(OvernightMetadata, 1)]');
    MetadataTemp = unique_metadata(OvernightMetadata, 'Participant');
    EveningAverage = average_by_column(MetadataTemp, Evening, 'EquispacedAges', []);

    %%% plot average evening values
    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);
    plot_age_by_frequency(EveningAverage, EquidistantAges(1:end-1), Frequencies, 'Linear', Labels{MeasureIdx}, PlotProps)

    title(Measures{MeasureIdx})
    ylabel('Frequency (Hz)')
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
end
chART.save_figure('FrequencyByAge', ResultsFolder, PlotProps)

% spectrum difference


% figure('Units','normalized','OuterPosition',[0 0 .18 1])
    figure('Units','centimeters','OuterPosition',[0 0 10 22])

for MeasureIdx = 1:nMeasures

    Spectrogram = BurstInformationClusters.(Measures{MeasureIdx});
    Evening = average_by_column(OvernightMetadata, Spectrogram, 'Participant', [1:size(OvernightMetadata, 1)]');

    OvernightTemp = OvernightMetadata;
    OvernightTemp.Index = OvernightMetadata.MorningIndexes;
    Morning = average_by_column(OvernightTemp, Spectrogram, 'Participant', [1:size(OvernightMetadata, 1)]');

    Change = Morning-Evening;
    MetadataTemp = unique_metadata(OvernightMetadata, 'Participant');
    ChangeAverage = average_by_column(MetadataTemp, Change, 'EquispacedAges', []);

    %%% plot differences
    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);
    plot_age_by_frequency(ChangeAverage, EquidistantAges(1:end-1), Frequencies, 'Divergent', 'difference', PlotProps)

    title([Measures{MeasureIdx}])
    ylabel('Frequency (Hz)')
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
end
chART.save_figure('FrequencyByAgeChange', ResultsFolder, PlotProps)



%% ADHD vs HC

% paired t-tests across channels for ADHD and controls
Ages = [8 14];
Group = 'ADHD';
GroupingColumn = 'Group';
TempMetadata = MetadataComplete(MetadataComplete.Age >=Ages(1) & MetadataComplete.Age<=Ages(2), :);

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);

% match recordings and participants
OvernightMetadata = overnight_changes(TempMetadata);

[OvernightMetadataPatients, OvernightMetadataControls] = match_participants(...
    OvernightMetadata, strcmp(OvernightMetadata.(GroupingColumn), Group));


HourLabels = {'Evening', 'Morning'};
CLims = [-1.5 1.5];
PlotProps.Stats.PlotN = true;
StatsParameters = Parameters.Stats;
StatsParameters.Unpaired = true;

% figure('Units','normalized','Position', [0 0 .21 .12*nMeasures])
figure('Units','centimeters','OuterPosition',[0 0 13 30])

for MeasuresIdx = 1:nMeasures

    Topographies = BurstInformationTopography.(Measures{MeasuresIdx});

    %%% group differences at each hour
    for HourIdx = 1:numel(Hours)

        HourMetadata = TempMetadata(strcmp(TempMetadata.Hour, Hours{HourIdx}), :);

        [MetadataPatients, MetadataControls] = match_participants(...
            HourMetadata, strcmp(HourMetadata.(GroupingColumn), Group));

        Control = Topographies(MetadataControls.Index, :);

        % average multiple sessions together
        ADHD = average_by_column(MetadataPatients, Topographies, 'Participant', [1:size(MetadataPatients, 1)]');
        Control = average_by_column(MetadataControls, Topographies, 'Participant',  [1:size(MetadataControls, 1)]');

        % plot
        chART.sub_plot([], [nMeasures+1, 3], [MeasuresIdx, HourIdx], [], false, '', PlotProps);
        plot_topography_difference(Control, ADHD, Chanlocs, CLims, StatsParameters, PlotProps)
        colorbar off

        if HourIdx ==1
            X = get(gca, 'XLim');
            Y = get(gca, 'YLim');
            text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasuresIdx}, ...
                'FontSize', PlotProps.Text.TitleSize, 'FontName', PlotProps.Text.FontName, ...
                'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
        end
        if MeasuresIdx ==1
            title(HourLabels{HourIdx})
        end
    end

    %%% overnight differences patients and controls
    ChangeADHD = Topographies(OvernightMetadataPatients.MorningIndexes, :)- ...
        Topographies(OvernightMetadataPatients.EveningIndexes, :);
    ChangeControls = Topographies(OvernightMetadataControls.MorningIndexes, :)-...
        Topographies(OvernightMetadataControls.EveningIndexes, :);

    ADHD = average_by_column(OvernightMetadataPatients, ChangeADHD, 'Participant', []);
    Control = average_by_column(OvernightMetadataControls, ChangeControls, 'Participant', []);

    chART.sub_plot([], [nMeasures+1, 3], [MeasuresIdx, 3], [], false, '', PlotProps);
    plot_topography_difference(Control, ADHD, Chanlocs, CLims, StatsParameters, PlotProps)
    colorbar off
    % plot
    if MeasuresIdx ==1
        title('ADHD vs Controls')
    end
end


ADHDTopoPlotProps = TopoPlotProps;
ADHDTopoPlotProps.Colorbar.Location = 'north';
chART.sub_plot([], [nMeasures+1, 3], [MeasuresIdx+1, 1], [1, 3], false, '', PlotProps);
chART.plot.pretty_colorbar('Divergent', CLims, "Cohen's d", ADHDTopoPlotProps)

chART.save_figure('ADHDvsControls', ResultsFolder, PlotProps)

% TODO
%% front to back overnight change index

Ages = [7, 12;
    17 22];

% Male vs female
