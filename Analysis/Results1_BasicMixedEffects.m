clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Hours = Parameters.Hours;


CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

ResultsFolder = fullfile(Paths.Results, 'MainStats');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end


load(fullfile(CacheDir, CacheName), 'Metadata')
% Metadata(contains(Metadata.Group, 'ADHD'), :) = []; % RODO figure out why theres too few ADHD kids!!

Metadata.Index = [1:size(Metadata, 1)]'; %#ok<NBRAK1> % add index so can chop up table as needed
Metadata(strcmp(Metadata.Dataset, 'SleepLearning') & ...
    contains(Metadata.Session, {'Session_2', 'Session_3'}), :) = []; % remove repeated measures 1 year later (will average recordings a couple weeks apart)
Metadata.Globality = Metadata.Globality*100; % make it percentage instead of proportion

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
OvernightMetadata = pair_recordings(Metadata, 'Hour', {'eve', 'mor'});

% GroupColumns = {'', 'Sex', 'Dataset'};
GroupColumns = {''};

for GC = GroupColumns
    GroupColumn = GC{1};
    figure('Units','centimeters','OuterPosition',[0 0 25 18])
    for VariableIdx = 1:numel(YVariables)

        %%% plot age x v split by evening and morning, averaged across sessions
        for HourIdx = 1:numel(Hours)
            Hour = Hours(HourIdx);

            Indexes = strcmp(Metadata.Hour, Hour);
            TempMetadata = Metadata(Indexes, :);
            AverageMetadata = unique_metadata(TempMetadata, 'Participant'); % average all tasks and sessions

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


%% run mixed modesl
MetadataStat = Metadata;
MetadataStat = make_categorical(MetadataStat, 'Task', {'Oddball', 'Learning', 'GoNoGo', 'Alertness', 'Fixation'});

MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'});
MetadataStat.Participant = categorical(MetadataStat.Participant);
MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'});
clc

OutcomeMeasures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};
for MeasureIdx = 1:numel(OutcomeMeasures)
    formula = [OutcomeMeasures{MeasureIdx}, ' ~ Age*Hour + Group + Task + (1|Participant)'];

    % mdl = fitlme(MetadataStat, formula,  'DummyVarCoding', 'effects');
    mdl = fitlme(MetadataStat, formula);

    % Display the model summary
    disp(['____________________ ', OutcomeMeasures{MeasureIdx}, ' ____________________'])
    disp(mdl);
    % writetable
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



