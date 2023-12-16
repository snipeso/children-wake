clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Hours = Parameters.Hours;
Bands = Parameters.Bands;
BandLabels = fieldnames(Bands);

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


%% Demographics

% disp_demographics(unique_metadata(Metadata), 'Dataset')
table_demographics(unique_metadata(Metadata), 'Dataset', ResultsFolder, 'DemographicsDatasets')


%% scatterplot of basic information
close all
PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 20;

% PlotProps.Axes.yPadding = 30;
% PlotProps.Axes.xPadding = 30;

Groups = {'HC', 'ADHD'};

Tasks = {'Oddball', 'Alertness'};
YVariables = {'Amplitude', 'Quantity', 'Globality', 'Duration', 'Slope', 'Intercept', 'PeriodicPower'};
Grid = [3 numel(YVariables)];

YLimits = [5, 42; % amplitudes
    70, 550; % quantities
    2, 20; % globality
    .5, 1.45; % duration
    .7 2.25; % slope
    .3, 2.5; % intercept
    -.05, .705; % periodic power
    ];
XLim = [3 25];

HourLabels = {'Evening', 'Morning'};
OvernightMetadata = overnight_changes(Metadata);

% GroupColumn = 'Group'; % or '';
GroupColumn = '';


figure('Units','normalized','OuterPosition',[0 0 .4 .5])
for VariableIdx = 1:numel(YVariables)
    for HourIdx = 1:numel(Hours)
        Hour = Hours(HourIdx);

        Indexes = strcmp(Metadata.Hour, Hour);
        TempMetadata = Metadata(Indexes, :);
        AverageMetadata =  unique_metadata(TempMetadata, 'Participant');


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
% chART.save_figure(['BasicScatterAge', GroupColumn], ResultsFolder, PlotProps)


%% correlate measures

YVariables = {'Amplitude', 'Quantity', 'Globality',  'Duration', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
Grid = [numel(YVariables) numel(YVariables)];
figure('Units','centimeters','OuterPosition',[0 0 30 30])
for Idx1 = 1:numel(YVariables)
    for Idx2 = 1:numel(YVariables)
        chART.sub_plot([], Grid, [Idx2, Idx1], [], true, '', PlotProps);

        scatter(Metadata.(YVariables{Idx1}), Metadata.(YVariables{Idx2}), 10, ...
            'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(1, :), 'MarkerFaceAlpha',.7)
        chART.set_axis_properties(PlotProps)
        lsline;
        if Idx2 == numel(YVariables)
            xlabel(YVariables{Idx1})
        end
        if Idx1==1
            ylabel(YVariables{Idx2})
        end
    end
end
chART.save_figure('CorrelateVariables', ResultsFolder, PlotProps)



%% topographies by age, descriptive

% TODO, split by burst frequency
Group = 'HC';

Ages = [2, 5;
    5 8;
    8, 11;
    11 14;
    14 17;
    17, 20;
    20 25];

nAges = size(Ages, 1);

Measures = fieldnames(BurstInformationTopographyBands);
nMeasures = numel(Measures);
MeasureLabels = Measures;
Tasks = {'Oddball', 'Alertness'};


CLims = struct();
% CLims.Quantity = [0.005 .07; 0.03 .27; 0.01 .11];
CLims.Quantity = [0 4.2; 5 30; .5 6];
CLims.Amplitude = [-1, 18; 7, 30; 2, 16];
CLims.Power = [-.5 2.5; -.5 2.5; -1.6 1.2];
CLims.PeriodicPower = [0.05 .3; .2 .8; 0 .4];
CLims.Slope = [1.3 2];
CLims.Intercept = [.8 2];

Labels = struct();
Labels.Quantity = append( '% ', BandLabels);
Labels.Amplitude = append(BandLabels, ' amp');
Labels.Slope = "Slope";
Labels.Intercept = "Intercept";
Labels.Power =  append( 'Power (log) ', BandLabels);
Labels.PeriodicPower = append( 'Periodic Power (log) ', BandLabels);

%% save demographic data for each age range

Metadata.AgeGroups = string(discretize(Metadata.Age, [Ages(:, 1); Ages(end, 2)]));
table_demographics(unique_metadata(Metadata), 'AgeGroups', ResultsFolder, 'DemographicsAgeGroups')

%%


for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopographyBands.(Measures{MeasureIdx});
    nBands = size(Topographies, 3);

    % figure('Units','normalized','Position', [0 0 .4 .15*nBands])
    figure('Units','normalized', 'Position',[0 0 1 1])
    for BandIdx = 1:nBands
        for AgeIdx = 1:nAges
            Indexes = contains(Metadata.Task, Tasks) & Metadata.Age >= Ages(AgeIdx, 1) & Metadata.Age < Ages(AgeIdx, 2) ...
                & strcmp(Metadata.Group, 'HC');
            % Indexes = Metadata.Age >= Ages(AgeIdx, 1) & Metadata.Age < Ages(AgeIdx, 2);
            Data = Topographies(Indexes, :, BandIdx);
            AverageData = mean(average_by_column(Metadata(Indexes, :), Data, 'Participant'), 1, 'omitnan');

            chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', PlotProps);
            chART.plot.eeglab_topoplot(AverageData, Chanlocs, [], CLims.(Measures{MeasureIdx})(BandIdx, :), '', 'Linear', PlotProps);
            % chART.plot.eeglab_topoplot(AverageData, Chanlocs, [], [], '', 'Linear', PlotProps);
            % colorbar
            if BandIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
            end
        end

        % plot colorbar
        chART.sub_plot([], [nBands, nAges+1], [BandIdx, nAges+1], [], false, '', PlotProps);
        chART.plot.pretty_colorbar('Linear', CLims.(Measures{MeasureIdx})(BandIdx, :), Labels.(Measures{MeasureIdx}){BandIdx}, PlotProps)

    end
    % chART.save_figure(['TopographyAverage_', Measures{MeasureIdx}], ResultsFolder, PlotProps)
end



%% topographies by age, overnight changes

close all

% Group = 'HC';
PlotProps.Stats.PlotN= true;

% Measures = fieldnames(BurstInformationTopographyBands);
Measures = fieldnames(BurstInformationTopography);

CLims = [-1 1];

for MeasureIdx =  1:nMeasures
    % Topographies = BurstInformationTopographyBands.(Measures{MeasureIdx});
    Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    nBands = size(Topographies, 3);
    nBands = 1;

    figure('Units','normalized','Position', [0 0 .5 .15*nBands])
    % figure('Units','normalized', 'Position',[0 0 1 1])

    for BandIdx = 1:nBands
        for AgeIdx = 1:nAges
            % Indexes = strcmp(OvernightMetadata.Group, Group) & OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
            % Indexes = OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
            Indexes =  contains(OvernightMetadata.Task, Tasks) & ...
                OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2) ...
                & strcmp(OvernightMetadata.Group, Group);

            Evening = Topographies(OvernightMetadata.EveningIndexes(Indexes), :, BandIdx);
            Morning = Topographies(OvernightMetadata.MorningIndexes(Indexes), :, BandIdx);
            Evening = average_by_column(OvernightMetadata(Indexes, :), Evening, 'Participant');
            Morning = average_by_column(OvernightMetadata(Indexes, :), Morning, 'Participant');

            chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', PlotProps);
            plot_topography_difference(Evening, Morning, Chanlocs, CLims, Parameters.Stats, PlotProps) %
            colorbar off
            if BandIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
            end
        end
    end
    chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx+1], [nBands, 1], false, '', PlotProps);
    chART.plot.pretty_colorbar('Divergent', CLims, [Measures{MeasureIdx} 'g-values'], PlotProps)

    % chART.save_figure(['TopographyChange_', Measures{MeasureIdx}], ResultsFolder, PlotProps)
end


%% TODO
% front to occipital ratio for amplitudes

%% plot Freq x Age plot, descriptive

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 30;
PlotProps.Axes.yPadding = 20;

Measures = fieldnames(BurstInformationClusters);
nMeasures = numel(Measures);
MeasureLabels = {'amplitude (\muV)', '%', '% channels', 'log power (\muV^2/Hz)'};

Ages = 8:3:20;

OvernightMetadata.QuantileAge = discretize(OvernightMetadata.Age, Ages);

figure('Units','normalized','OuterPosition',[0 0 .2 .8])
for MeasureIdx = 1:nMeasures
    % Indexes = strcmp(OvernightMetadata.Group, Group) & OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
    % TempMetadata = OvernightMetadata(Indexes, :);
    TempMetadata = OvernightMetadata;

    Evening = BurstInformationClusters.(Measures{MeasureIdx})(TempMetadata.EveningIndexes, :);
    Morning = BurstInformationClusters.(Measures{MeasureIdx})(TempMetadata.MorningIndexes, :);
    [Evening, UniqueMetadata] = average_by_column(TempMetadata, Evening, 'Participant');
    Morning = average_by_column(TempMetadata, Morning, 'Participant');

    Data = average_by_column(UniqueMetadata, Evening, 'QuantileAge');


    %%% evening values
    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);
    Colors = chART.color_picker([1, size(Data, 1)]);
    hold on
    for AgeIdx = 1:size(Data, 1)
        plot(Frequencies, Data(AgeIdx, :), 'Color', Colors(AgeIdx, :), 'LineWidth',1.5)
    end
    % contourf(Ages(1:end-1), Frequencies, Data', 100, 'linecolor','none')
    chART.set_axis_properties(PlotProps)
    % colormap(PlotProps.Color.Maps.Linear)
    % xticks(8:2:20)
    % yticks(5:2:15)
    % h = colorbar;
    % h.TickLength = 0;
    % ylabel(h, MeasureLabels{MeasureIdx}, 'FontName', PlotProps.Text.FontName) % text style needs to be specified for label, because its weird

    title([Measures{MeasureIdx}, ' Evening'])
    ylabel('Frequency (Hz)')
    if MeasureIdx == nMeasures
        xlabel('Age')
    end
end

chART.save_figure('FrequencyByAge', ResultsFolder, PlotProps)


%%


PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 30;
PlotProps.Axes.yPadding = 20;

CLims = [-4 4;
    -.08 .08
    -.08 .08
    -.15 .15];
figure('Units','normalized','OuterPosition',[0 0 .2 .8])
for MeasureIdx = 1:nMeasures
    % Indexes = strcmp(OvernightMetadata.Group, Group) & OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
    % TempMetadata = OvernightMetadata(Indexes, :);
    TempMetadata = OvernightMetadata;

    Evening = BurstInformationClusters.(Measures{MeasureIdx})(TempMetadata.EveningIndexes, :);
    Morning = BurstInformationClusters.(Measures{MeasureIdx})(TempMetadata.MorningIndexes, :);
    [Evening, UniqueMetadata] = average_by_column(TempMetadata, Evening, 'Participant');
    Morning = average_by_column(TempMetadata, Morning, 'Participant');


    %%% difference values
    Change = Morning-Evening;
    Data = average_by_column(UniqueMetadata, Change, 'QuantileAge');


    chART.sub_plot([], [nMeasures, 1], [MeasureIdx, 1], [], true, '', PlotProps);
    Colors = chART.color_picker([1, size(Data, 1)], '', 'red');
    hold on
    for AgeIdx = 1:size(Data, 1)
        plot(Frequencies, Data(AgeIdx, :), 'Color', Colors(AgeIdx, :), 'LineWidth',1.5)
    end
    % contourf(Ages(1:end-1), Frequencies, Data', 100, 'linecolor','none')
    chART.set_axis_properties(PlotProps)

    % colormap(PlotProps.Color.Maps.Divergent)
    % xticks(8:2:20)
    % yticks(5:2:15)
    % h = colorbar;
    % h.TickLength = 0;
    ylabel('Frequency (Hz)')
    title([Measures{MeasureIdx}, ' Overnight change'])

    if MeasureIdx == nMeasures
        xlabel('Age')
        legend(string(Ages(1:end-1)))
    end
    % h = colorbar;
    % clim(CLims(MeasureIdx, :))
    % h.TickLength = 0;
    % ylabel(h, 'difference', 'FontName', PlotProps.Text.FontName) % text style needs to be specified for label, because its weird
end
chART.save_figure('FrequencyByAgeChange', ResultsFolder, PlotProps)




%% ADHD vs HC

% paired t-tests across channels for ADHD and controls
Ages = [7.5 14];
TempMetadata = Metadata(Metadata.Age >=Ages(1) & Metadata.Age<=Ages(2), :);

Measures = fieldnames(BurstInformationTopography);
nMeasures = numel(Measures);

% match recordings and participants
OvernightMetadata = overnight_changes(TempMetadata);
[OvernightMetadataPatients, OvernightMetadataControls] = match_participants(...
    OvernightMetadata, strcmp(OvernightMetadata.Group, 'ADHD'));


Groups = {'HC', 'ADHD'};
HourLabels = {'Evening', 'Morning'};
CLims = [-1 1];
PlotProps.Stats.PlotN = true;
StatsParameters = Parameters.Stats;
StatsParameters.Unpaired = true;

figure('Units','normalized','Position', [0 0 .21 .12*nMeasures])
for MeasuresIdx = 1:nMeasures

    Topographies = BurstInformationTopography.(Measures{MeasuresIdx});

    %%% group differences at each hour
    for HourIdx = 1:numel(Hours)

        HourMetadata = TempMetadata(strcmp(TempMetadata.Hour, Hours{HourIdx}), :);

        [MetadataPatients, MetadataControls] = match_participants(...
            HourMetadata, strcmp(HourMetadata.Group, 'ADHD'));

        ADHD = Topographies(MetadataPatients.Index, :);
        Control = Topographies(MetadataControls.Index, :);

        % average multiple sessions together
        ADHD = average_by_column(MetadataPatients, MetadataPatients.Index, ADHD, 'Participant');
        Control = average_by_column(MetadataControls, MetadataControls.Index, Control, 'Participant');

        % plot
        chART.sub_plot([], [nMeasures, 3], [MeasuresIdx, HourIdx], [], false, '', PlotProps);
        plot_topography_difference(Control, ADHD, Chanlocs, CLims, StatsParameters, PlotProps)
        colorbar off

        X = get(gca, 'XLim');
        Y = get(gca, 'YLim');
        if HourIdx ==1
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

    ADHD = average_by_column(OvernightMetadataPatients, OvernightMetadataPatients.Index, ChangeADHD, 'Participant');
    Control = average_by_column(OvernightMetadataControls, OvernightMetadataControls.Index, ChangeControls, 'Participant');



    chART.sub_plot([], [nMeasures, 3], [MeasuresIdx, 3], [], false, '', PlotProps);
    plot_topography_difference(Control, ADHD, Chanlocs, CLims, StatsParameters, PlotProps)
    colorbar off
    % plot
    if MeasuresIdx ==1
        title('ADHD vs Controls')
    end
end
chART.save_figure('ADHDvsControls', ResultsFolder, PlotProps)




