clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Hours = Parameters.Hours;
BandLabels = {'Theta', 'Low Alpha', 'High Alpha'};

MinNaNChannels = 25; % for amplitudes

% Topography plotprops
TopoPlotProps = Parameters.PlotProps.TopoPlots;
Ages = Parameters.Ages;

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
Metadata(~contains(Metadata.Task, {'Oddball', 'GoNoGo'}), :) = []; % only look at first oddball and alertness task
Metadata.Subgroup(strcmp(Metadata.Group, 'HC')) = 5;
Metadata.Globality = Metadata.Globality*100; % make it percentage instead of proportion
Metadata.AgeGroups = string(discretize(Metadata.Age, [Ages(:, 1); Ages(end, 2)]));
% Metadata.Task(contains(Metadata.Task, 'Alertness')) = {'Alertness'}; % Fix because different order in task
% Metadata.Task(contains(Metadata.Task, 'Oddball')) = {'Oddball'};
MetadataComplete = Metadata;
Metadata(contains(Metadata.Group, 'ADHD'), :) = []; % RODO figure out why theres too few ADHD kids!!
nAges = size(Ages, 1);



table_demographics(unique_metadata(Metadata), 'AgeGroups', ResultsFolder, 'DemographicsAgeGroups')


%% topography differences

CLims = [-2 2];

EveningMetadata = pair_recordings(Metadata, 'Hour', {'eve', 'mor'});
MorningMetadata = EveningMetadata;
MorningMetadata.Index = EveningMetadata.IndexesCategory2;

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);

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


%% Overnight topographies split by band


EveningMetadata = pair_recordings(Metadata, 'Hour', {'eve', 'mor'});
MorningMetadata = EveningMetadata;
MorningMetadata.Index = EveningMetadata.IndexesCategory2;


CLims = [-1.5 1.5];

Measures = fieldnames(BurstInformationTopographyBands);
nMeasures = numel(Measures);

for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopographyBands.(Measures{MeasureIdx});
    nBands = size(Topographies, 3);

    % figure('Units','normalized','Position', [0 0 TopoFigureSizes(1) TopoFigureSizes(2)*nMeasures])
    figure('Units','centimeters','OuterPosition',[0 0 25 16])

    for BandIdx = 1:nBands
        for AgeIdx = 2:nAges
            Indexes = ismember(EveningMetadata.AgeGroups, string(AgeIdx));

            Evening = average_by_column(EveningMetadata, Topographies(:, :, BandIdx), 'Participant', Indexes);
            Morning = average_by_column(MorningMetadata, Topographies(:, :, BandIdx), 'Participant', Indexes);

            chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', TopoPlotProps);
            plot_topography_difference(Evening, Morning, Chanlocs, CLims, Parameters.Stats, TopoPlotProps) %
            colorbar off

            if BandIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
            end

            if AgeIdx ==2
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
OvernightMetadata = pair_recordings(Metadata, 'Hour', {'eve', 'mor'});

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
    OvernightTemp.Index = OvernightMetadata.IndexesCategory2;
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




%% kids vs adults

% paired t-tests across channels for ADHD and controls
% Ages = [8 14];

GroupingColumn = 'AgeGroup2';
Items = [ 1 3];
Metadata.AgeGroup2 = discretize(Metadata.Age, [7, 13, 18, 25]);

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);

% match recordings and participants
OvernightMetadata = pair_recordings(Metadata, 'Hour', {'eve', 'mor'});

[OvernightMetadataKids, OvernightMetadataAdults] = split_groups(OvernightMetadata, GroupingColumn, Items);


HourLabels = {'Evening', 'Morning'};
CLims = [-3 3];
PlotProps.Stats.PlotN = true;
StatsParameters = Parameters.Stats;
StatsParameters.Unpaired = true;

figure('Units','centimeters','OuterPosition',[0 0 13 30])

for MeasuresIdx = 1:nMeasures

    Topographies = BurstInformationTopography.(Measures{MeasuresIdx});

    %%% group differences at each hour
    for HourIdx = 1:numel(Hours)

        HourMetadata = Metadata(strcmp(Metadata.Hour, Hours{HourIdx}), :);
        [MetadataKids, MetadataAdults] = split_groups(HourMetadata, GroupingColumn, Items);

        % average multiple sessions together
        Kids = average_by_column(MetadataKids, Topographies, 'Participant', [1:size(MetadataKids, 1)]');
        Adults = average_by_column(MetadataAdults, Topographies, 'Participant',  [1:size(MetadataAdults, 1)]');

        % plot
        chART.sub_plot([], [nMeasures+1, 3], [MeasuresIdx, HourIdx], [], false, '', PlotProps);
        plot_topography_difference(Adults, Kids, Chanlocs, CLims, StatsParameters, PlotProps)
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
    ChangeKids = Topographies(OvernightMetadataKids.IndexesCategory2, :)- ...
        Topographies(OvernightMetadataKids.IndexesCategory1, :);
    ChangeAdults = Topographies(OvernightMetadataAdults.IndexesCategory2, :)-...
        Topographies(OvernightMetadataAdults.IndexesCategory1, :);

    Kids = average_by_column(OvernightMetadataKids, ChangeKids, 'Participant', []);
    Adults = average_by_column(OvernightMetadataAdults, ChangeAdults, 'Participant', []);

    chART.sub_plot([], [nMeasures+1, 3], [MeasuresIdx, 3], [], false, '', PlotProps);
    plot_topography_difference(Adults, Kids, Chanlocs, CLims, StatsParameters, PlotProps)
    colorbar off
    % plot
    if MeasuresIdx ==1
        title('Kids vs Adults')
    end
end


ADHDTopoPlotProps = TopoPlotProps;
ADHDTopoPlotProps.Colorbar.Location = 'north';
chART.sub_plot([], [nMeasures+1, 3], [MeasuresIdx+1, 1], [1, 3], false, '', PlotProps);
chART.plot.pretty_colorbar('Divergent', CLims, "Cohen's d", ADHDTopoPlotProps)

chART.save_figure('KidsvsAdults', ResultsFolder, PlotProps)


%% check if task differs


% paired t-tests across channels for ADHD and controls
% Ages = [8 14];
Group = 1; % 0 oddball, 1 attention
GroupingColumn = 'TaskType';

Metadata.TaskType = ~contains(Metadata.Task, 'Oddball');
% TempMetadata = MetadataComplete(MetadataComplete.Age >=Ages(1) & MetadataComplete.Age<=Ages(2), :);
TempMetadata = Metadata;

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
nMeasures = numel(Measures);

HourLabels = {'Evening', 'Morning'};
CLims = [-1.5 1.5];
PlotProps.Stats.PlotN = true;
StatsParameters = Parameters.Stats;
StatsParameters.Unpaired = true;

figure('Units','centimeters','OuterPosition',[0 0 13 30])

for MeasuresIdx = 1:nMeasures

    Topographies = BurstInformationTopography.(Measures{MeasuresIdx});

    %%% group differences at each hour
    for HourIdx = 1:numel(Hours)

        HourMetadata = TempMetadata(strcmp(TempMetadata.Hour, Hours{HourIdx}), :);

        [MetadataPatients, MetadataControls] = match_participants(...
            HourMetadata, ismember(HourMetadata.(GroupingColumn), Group));

        % average multiple sessions together
        GroupOfInterest = average_by_column(MetadataPatients, Topographies, 'Participant', [1:size(MetadataPatients, 1)]');
        Control = average_by_column(MetadataControls, Topographies, 'Participant',  [1:size(MetadataControls, 1)]');

        % plot
        chART.sub_plot([], [nMeasures+1, 2], [MeasuresIdx, HourIdx], [], false, '', PlotProps);
        plot_topography_difference(Control, GroupOfInterest, Chanlocs, CLims, StatsParameters, PlotProps)
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
end


ADHDTopoPlotProps = TopoPlotProps;
ADHDTopoPlotProps.Colorbar.Location = 'north';
chART.sub_plot([], [nMeasures+1, 2], [MeasuresIdx+1, 1], [1, 2], false, '', PlotProps);
chART.plot.pretty_colorbar('Divergent', CLims, "Cohen's d", ADHDTopoPlotProps)

chART.save_figure('OddballVsAttention', ResultsFolder, PlotProps)
