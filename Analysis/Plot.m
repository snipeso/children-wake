
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
%
load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopography', ...
    "BurstInformationClusters", 'Frequencies', 'Chanlocs')

% load(fullfile(CacheDir, CacheName), 'Metadata')

OvernightMetadata = overnight_changes(Metadata);

%% scatterplot of basic information
close all
PlotProps = Parameters.PlotProps.Manuscript;

% PlotProps.Axes.yPadding = 30;
% PlotProps.Axes.xPadding = 30;

Groups = {'HC', 'ADHD'};

Tasks = {'Oddball', 'Alertness'};
YVariables = {'Globality', 'Amplitude', 'Duration', 'Frequency', 'Slope', 'Intercept'};
Grid = [3 numel(YVariables)];
Colors = chART.color_picker(2);
Markers = {'o', '^'};
Dashes = {':', '-'};
Limits = [.02 .14;
    7 30;
    .5 1.1;
    9 13;
    1.1 2.1;
    .5 2.5];

HourLabels = {'Evening', 'Morning'};

figure('Units','normalized','OuterPosition',[0 0 .5 .6])
for VariableIdx = 1:numel(YVariables)
    for HourIdx = 1:numel(Hours)
        Hour = Hours(HourIdx);
        chART.sub_plot([], Grid, [HourIdx, VariableIdx], [], true, '', PlotProps);
        hold on
        for GroupIdx = 1:numel(Groups)
            Indexes = strcmp(Metadata.Group, Groups{GroupIdx}) & ...
                strcmp(Metadata.Hour, Hour);
            AverageData = average_by_column(Metadata(Indexes, :),[Metadata.Age(Indexes), Metadata.(YVariables{VariableIdx})(Indexes)], 'Participant');

            scatter(AverageData(:, 1), AverageData(:, 2), 10,  ...
                'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(GroupIdx,:), 'MarkerFaceAlpha',.7)
            chART.set_axis_properties(PlotProps)

            if HourIdx==1
                title(YVariables{VariableIdx})
            end
            if VariableIdx==1
                ylabel(HourLabels{HourIdx}, 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
            end
            xlim([8 24])
            ylim(Limits(VariableIdx, :))
        end
        h = lsline;
        for GroupIdx = 1:numel(Groups)
            set(h(GroupIdx),'color',Colors(numel(Groups)+1-GroupIdx,:), 'LineWidth', 2)
        end
    end

    chART.sub_plot([], Grid, [3, VariableIdx], [], true, '', PlotProps);

    hold on
    for GroupIdx = 1:numel(Groups)
        Indexes = strcmp(OvernightMetadata.Group, Groups{GroupIdx});

        AverageData = average_by_column(OvernightMetadata(Indexes, :), ...
            [OvernightMetadata.Age(Indexes), OvernightMetadata.(YVariables{VariableIdx})(Indexes)], 'Participant');

        scatter(AverageData(:, 1), AverageData(:, 2), 10, ...
            'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(GroupIdx,:), 'MarkerFaceAlpha',.7)

        chART.set_axis_properties(PlotProps)
        if VariableIdx==1
            ylabel('Overnight change', 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
        end
        xlabel('Age')
        xlim([8 24])
    end

    h = lsline;
    for GroupIdx = 1:numel(Groups)
        set(h(GroupIdx),'color',Colors(numel(Groups)+1-GroupIdx,:), 'LineWidth', 2)
    end

end
legend(Groups)
chART.save_figure('BasicScatterAge', ResultsFolder, PlotProps)


%% correlate measures

YVariables = {'Globality', 'Amplitude', 'Duration', 'Slope', 'Intercept'};
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

Ages = [8, 11;
    11 14;
    14 16;
    16 20; 
    20 25];
nAges = size(Ages, 1);
Measures = fieldnames(BurstInformationTopography);
nMeasures = numel(Measures);
MeasureLabels = Measures;

CLims = struct();
CLims.Quantity = [0.005 .07; 0.03 .27; 0.01 .11];
CLims.Amplitude = [2, 20; 10, 25; 8, 18];
CLims.Slope = [1.3 2];
CLims.Intercept = [.8 2];

Labels = struct();
Labels.Quantity = append( '# ', BandLabels);
Labels.Amplitude = append(BandLabels, ' amp');
Labels.Slope = "Slope";
Labels.Intercept = "Intercept";

%%

for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    nBands = size(Topographies, 3);

    figure('Units','normalized','Position', [0 0 .4 .15*nBands])
    for BandIdx = 1:nBands
        for AgeIdx = 1:nAges
            % Indexes = strcmp(Metadata.Group, Group) & Metadata.Age >= Ages(AgeIdx, 1) & Metadata.Age < Ages(AgeIdx, 2);
            Indexes = Metadata.Age >= Ages(AgeIdx, 1) & Metadata.Age < Ages(AgeIdx, 2);
            Data = Topographies(Indexes, :, BandIdx);
            AverageData = mean(average_by_column(Metadata(Indexes, :), Data, 'Participant'), 1, 'omitnan');

            chART.sub_plot([], [nBands, nAges+1], [BandIdx, AgeIdx], [], false, '', PlotProps);
            chART.plot.eeglab_topoplot(AverageData, Chanlocs, [], CLims.(Measures{MeasureIdx})(BandIdx, :), '', 'Linear', PlotProps);
            if BandIdx == 1
                title([num2str(Ages(AgeIdx, 1)),'-' num2str(Ages(AgeIdx, 2))])
            end
        end

        % plot colorbar
        chART.sub_plot([], [nBands, nAges+1], [BandIdx, nAges+1], [], false, '', PlotProps);
        chART.plot.pretty_colorbar('Linear', CLims.(Measures{MeasureIdx})(BandIdx, :), Labels.(Measures{MeasureIdx}){BandIdx}, PlotProps)

    end
    chART.save_figure(['TopographyAverage_', Measures{MeasureIdx}], ResultsFolder, PlotProps)
end



%% topographies by age, overnight changes

% Group = 'HC';
PlotProps.Stats.PlotN= true;


CLims = [-1 1];

for MeasureIdx = 1:nMeasures
    Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    nBands = size(Topographies, 3);

    figure('Units','normalized','Position', [0 0 .4 .15*nBands])
    for BandIdx = 1:nBands
        for AgeIdx = 1:nAges
            % Indexes = strcmp(OvernightMetadata.Group, Group) & OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);
            Indexes = OvernightMetadata.Age >= Ages(AgeIdx, 1) & OvernightMetadata.Age < Ages(AgeIdx, 2);

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
    chART.plot.pretty_colorbar('Divergent', CLims, 'g-values', PlotProps)

    chART.save_figure(['TopographyChange_', Measures{MeasureIdx}], ResultsFolder, PlotProps)
end


%% TODO
% front to occipital ratio for amplitudes

%% plot Freq x Age plot, descriptive

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 30;
PlotProps.Axes.yPadding = 20;

Measures = fieldnames(BurstInformationClusters);
nMeasures = numel(Measures);
MeasureLabels = {'amplitude (\muV)', 'cyc/min', '% channels', 'log power (\muV^2/Hz)'};

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

OvernightMetadata = overnight_changes(Metadata);

OvernightMetadata.Subgroup(strcmp(OvernightMetadata.Group, 'HC')) = 5;
[Participants, UniqueIndx] = unique(OvernightMetadata.Participant);
UniqueMetadata = OvernightMetadata(UniqueIndx, :);

MetadataPatients = UniqueMetadata(ismember(UniqueMetadata.Subgroup, [1, 2]), :);

MetadataControls = OvernightMetadata(OvernightMetadata.Subgroup==5, :);

 [MetadataPatients] = match_participants(MetadataPatients, MetadataControls);
 % 
 % MetadataControls = OvernightMetadata(contains(OvernightMetadata.Participant, MetadataPatients.ControlParticipant), :);

 MetadataControls = UniqueMetadata(contains(UniqueMetadata.Participant, MetadataPatients.ControlParticipant), :);


 [~, p, ci, stats] = ttest2(MetadataPatients.Amplitude, MetadataControls.Amplitude);


 OvernightMetadata = OvernightMetadata(contains(OvernightMetadata.Participant, [MetadataPatients.ControlParticipant, MetadataPatients.Participant]), :);

 Measures = fieldnames(BurstInformationTopography);
 MeasureIdx = 3;
 BandIdx = 1;
     Evening = BurstInformationTopography.(Measures{MeasureIdx})(OvernightMetadata.EveningIndexes, :, BandIdx);
    Morning = BurstInformationTopography.(Measures{MeasureIdx})(OvernightMetadata.MorningIndexes, :, BandIdx);
    [Evening, UniqueMetadata] = average_by_column(OvernightMetadata, Evening, 'Participant');
    Morning = average_by_column(OvernightMetadata, Morning, 'Participant');
    % Change = Morning-Evening;
    Change = Evening;z

    Patients = strcmp(UniqueMetadata.Group, 'ADHD');
    Controls = strcmp(UniqueMetadata.Group, 'HC'); 

figure
Parameters.Stats.Unpaired = true;
 plot_topography_difference(Change(Patients, :), Change(Controls, :), Chanlocs, [], Parameters.Stats, PlotProps) %





