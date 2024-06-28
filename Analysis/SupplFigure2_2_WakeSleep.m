% this script relates sleep slow wave amplitudes and slopes (controlling
% for amplitude) to wake outcome measures.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% setup variables and parameters

Parameters = analysisParameters();
Hours = Parameters.Hours;

OutcomeMeasures = [Parameters.OutcomeMeasures.OriginalLabels, 'SWASlope', 'SWAAmp'];
OutcomeMeasuresTitles = [Parameters.OutcomeMeasures.Titles, 'SW Slope', 'SW Amplitude'];
MeasureUnits = [Parameters.OutcomeMeasures.Units, 'A.U.', '\muV'];

%%% set paths
Paths = Parameters.Paths;

% where data can be found
CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

% where to save figures
ResultsFolder = fullfile(Paths.Results, 'SleepWakeStats');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end


%%% load data
load(fullfile(CacheDir, CacheName), 'Metadata')

SlopeCSV = readtable(fullfile(Paths.Core, 'ValeriaSlopes', 'RecodedSlopes.csv'));
AmplitudeCSV = readtable(fullfile(Paths.Core, 'ValeriaSlopes', 'RecodedAmplitudesUnmatched.csv'));


% fixes to metadata
Metadata = basic_metadata_cleanup(Metadata);
Metadata(~ismember(Metadata.Participant, SlopeCSV.subject), :) = []; % only subjects for which sleep data is available
Metadata(~ismember(Metadata.Condition, 'base'), :) = []; % only baseline nights were included in sleep analysis
Metadata(~contains(Metadata.Task, {'Alertness', 'Oddball'}), :) = []; % only use oddball-like tasks

AmplitudeCSV = AmplitudeCSV(ismember(AmplitudeCSV.channel, Parameters.Channels.NotEdge), :); % exclude edge channels (already done for slopes)

 KeepChannels = unique(AmplitudeCSV.channel);

%%% add slope info to metadata table

for RowIdx = 1:size(Metadata)

    Participant = Metadata.Participant(RowIdx);
    Session = Metadata.Session(RowIdx);
    Hour = Metadata.Hour(RowIdx);

    % convert string to index
    if strcmp(Hour, 'eve')
        HourIdx = 1;
    else
        HourIdx = 2;
    end

    % slopes
    Slope = SlopeCSV{strcmp(SlopeCSV.subject, Participant) & ...
        strcmp(SlopeCSV.session, Session) & SlopeCSV.time==HourIdx, 6:end-1};

    Metadata.SWASlope(RowIdx) =  mean(Slope(:, KeepChannels), 'all', 'omitnan');

    % amplitudes
    Amp = power(exp(1), AmplitudeCSV.amp(strcmp(AmplitudeCSV.subject, Participant) & ...
        strcmp(AmplitudeCSV.session, Session) & AmplitudeCSV.time==HourIdx)); % the data was saved log-transformed, so switching it back here

    Metadata.SWAAmp(RowIdx) =  mean(Amp, 'all', 'omitnan');
end



%% save data used for stats to publish

MetadataPublish = Metadata;

OriginalTableLables = MetadataPublish.Properties.VariableNames;
for Idx = 1:numel(OutcomeMeasuresTitles)

    IdxTable = strcmp(OriginalTableLables, OutcomeMeasures{Idx});
    MetadataPublish.Properties.VariableNames(IdxTable) = genvarname(OutcomeMeasuresTitles(Idx));
end

% recode dataset names
Datasets = {'SleepLearning', 'Providence', 'ADHD', 'BMSAdults', 'BMS', 'BMSSL'};
DatasetsNew = {'Dataset2008', 'Dataset2009', 'Dataset2010', 'Dataset2016', 'Dataset2017', 'Dataset2019'};

for DatasetIdx = 1:numel(Datasets)
    MetadataPublish.Dataset(strcmp(MetadataPublish.Dataset, Datasets{DatasetIdx})) = ...
        repmat(DatasetsNew(DatasetIdx), nnz(strcmp(MetadataPublish.Dataset, Datasets{DatasetIdx})), 1);
end


writetable(MetadataPublish, fullfile(ResultsFolder, 'WakeSleepAllData.csv'))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analyses

%% demographics

UniqueMetadata = unique_metadata(Metadata, 'Participant');

disp(['n female: ', num2str(nnz(strcmp(UniqueMetadata.Sex, 'f')))])
disp(['n: ', num2str(size(UniqueMetadata, 1))])
disp(['age: ', num2str(mean(UniqueMetadata.Age, 1))])
disp(['age min: ', num2str(min(UniqueMetadata.Age))])
disp(['age max: ', num2str(max(UniqueMetadata.Age))])

%% run mixed models

clc
if exist(fullfile(ResultsFolder, "SleepWakeModel_AllStats.txt"), 'file')
    delete(fullfile(ResultsFolder, "SleepWakeModel_AllStats.txt"))
end

diary(fullfile(ResultsFolder, "SleepWakeModel_AllStats.txt"))
diary on

%%% setup metadata for statistics
MetadataStat = Metadata;

% make categorical (removes the interpetable names, but makes sure the
% order is correct, so that the first category is the "baseline" kind).
% Also selects the categories to be considered.
MetadataStat.Participant = categorical(MetadataStat.Participant);

MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'}); % compare morning to evening
MetadataStat = make_categorical(MetadataStat, 'Sex', {'f', 'm'}); % compare males to females
MetadataStat = make_categorical(MetadataStat, 'Task', {'Oddball', '2Alertness'}); % compare males to females


%%% mixed model to correct for multiple recordings etc.

FormulaFixed = '~ Hour*Age + Task +';
FormulaRandom = '+ (1|Participant)';

Stats = table();
Stats.OutcomeMeasures = OutcomeMeasures';
TValues = Stats;
nMeasures = numel(OutcomeMeasures);
AllT = nan(nMeasures, nMeasures);
for Idx1 = 1:numel(OutcomeMeasures)
    for Idx2 = 1:numel(OutcomeMeasures)

        if Idx1==Idx2
            continue
        end

        formula = [OutcomeMeasures{Idx1}, FormulaFixed, OutcomeMeasures{Idx2}, FormulaRandom];
        Model = fitlme(MetadataStat, formula);

        RowIdx = strcmp(Model.Coefficients.Name, OutcomeMeasures{Idx2});

        beta = Model.Coefficients.Estimate(RowIdx);
        t = Model.Coefficients.tStat(RowIdx);
        AllT(Idx2, Idx1) = t;
        df = Model.Coefficients.DF(RowIdx);
        p = Model.Coefficients.pValue(RowIdx);
        pString = extractAfter(num2str(p, '%.3f'), '.');
        if p < .001
            pString = ' < .001';
        else
            pString = [' = .',pString];
        end

         StatString = ['beta = ', num2str(beta, '%.2f'), ', t = ', num2str(t, '%.1f'), ', p', pString, ', df = ', num2str(df)];
        % StatString = ['b=', num2str(beta, '%.2f'), '; t=', num2str(t, '%.1f'), '; p', pString, '; df=', num2str(df)];
        Stats.(OutcomeMeasures{Idx1})(Idx2) = {StatString};
        TValues.(OutcomeMeasures{Idx1})(Idx2) = t;

            % Display the model
            if strcmp(OutcomeMeasures{Idx1}, 'SWASlope') || strcmp(OutcomeMeasures{Idx1}, 'SWAAmp')
    disp('   ')
disp('   ')
    disp(['____________________ ',OutcomeMeasuresTitles{Idx2} ' vs ' OutcomeMeasuresTitles{Idx1}, ' ____________________'])
    disp(Model);
            end
    end
end

diary off

disp(Stats)
writetable(Stats, fullfile(ResultsFolder, 'CorrelationsOutcomeVariables.xlsx'))
writetable(TValues, fullfile(ResultsFolder, 'CorrelationsOutcomeVariables_TValues.csv'))


%% scatterplot of basic information (quality check)


close all
PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 20;
PlotProps.Axes.xPadding = 18;
Grid = [3 numel(OutcomeMeasures)];

% PlotProps.Text.FontName = 'Tw Cen MT';

% fix y lims, so same for mor and eve
YLimits = [5, 42; % amplitudes
    70, 550; % quantities
    .7 2.25; % slope
    .3, 2.5; % intercept
    -1.6, 2; % power
    -.05, .705; % periodic power
    100 500;
    15 100];

XLim = [3 25];

HourLabels = {'Evening', 'Morning'};

% select only some of the data
MetadataScatter = Metadata;

OvernightMetadata = pair_recordings(MetadataScatter, 'Hour', {'eve', 'mor'});

clc
figure('Units','centimeters','OuterPosition',[0 0 30 18])

for VariableIdx = 1:numel(OutcomeMeasures)

    %%% plot age x v split by evening and morning, averaged across sessions
    for HourIdx = 1:numel(Hours)

        % select data of either evening or morning
        MetadataHour = MetadataScatter(strcmp(MetadataScatter.Hour, Hours(HourIdx)), :);

        % average sessions and multiple tasks (1oddball and 3 oddball)
        MetadataAverage = unique_metadata(MetadataHour, 'Participant');

        % plot
        chART.sub_plot([], Grid, [HourIdx, VariableIdx], [], true, '', PlotProps);
        plot_scattercloud(MetadataAverage, 'Age', OutcomeMeasures{VariableIdx}, ...
            PlotProps, '', false, XLim, YLimits(VariableIdx, :))
        ylabel(MeasureUnits{VariableIdx})
        legend off

        if HourIdx==1
            title(OutcomeMeasuresTitles{VariableIdx})
        end
        if VariableIdx==1
            chART.plot.vertical_text(HourLabels{HourIdx}, .55, .5, PlotProps)
        end
        disp([ Hours{HourIdx}, OutcomeMeasures{VariableIdx}, ...
            'N=', num2str(numel(unique(MetadataAverage.Participant)))])

    end

    %%% plot overnight change
    chART.sub_plot([], Grid, [3, VariableIdx], [], true, '', PlotProps);
    MetadataAverage = unique_metadata(OvernightMetadata, 'Participant');

    plot_scattercloud(MetadataAverage, 'Age', OutcomeMeasures{VariableIdx}, ...
        PlotProps, '', true, XLim)
    ylabel(MeasureUnits{VariableIdx})
    xlabel('Age')
    if VariableIdx ~=numel(OutcomeMeasures)
        legend off
    end

    if VariableIdx==1
        chART.plot.vertical_text('Overnight change', .55, .5, PlotProps)
        xlabel('Age (years)')
    end

    disp(['Overnight', OutcomeMeasures{VariableIdx}, ...
        'N=', num2str(numel(unique(MetadataAverage.Participant)))])
end
chART.save_figure('BasicScatterAge', ResultsFolder, PlotProps)






%% correlate measures (Suppl. Figure 2-2)

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 25;
PlotProps.Text.TitleSize = 10;
PlotProps.Axes.yPadding = 5;
PlotProps.Axes.xPadding = 5;
PlotProps.Scatter.Size = 5;
PlotProps.Scatter.Alpha = .4;

Grid = [numel(OutcomeMeasures) numel(OutcomeMeasures)];
figure('Units','centimeters','OuterPosition',[0 0 20 20])
for Idx1 = 1:numel(OutcomeMeasures)
    for Idx2 = 1:numel(OutcomeMeasures)
        chART.sub_plot([], Grid, [Idx2, Idx1], [], false, '', PlotProps);

        if Idx1==Idx2
            if Idx1==1
                chART.set_axis_properties(PlotProps)
                title(OutcomeMeasuresTitles{Idx1})
                set(gca, 'YTick', [], 'XTick', [])
                axis square
                ax = gca;
axis(ax,'off')
ylabel(ax,OutcomeMeasuresTitles{Idx2})
ax.YLabel.Visible = 'on';
                ylabel(OutcomeMeasures{Idx2})
            end
            axis off
            continue
        end


        plot_scattercloud(Metadata, OutcomeMeasures{Idx1}, OutcomeMeasures{Idx2}, PlotProps, 'Dataset', false)
        set(gca, 'XTick' ,[], 'YTick', [])
        legend off
        axis square
        if Idx2 == numel(OutcomeMeasures)
            xlabel(OutcomeMeasuresTitles{Idx1})
        elseif Idx2==1
            title(OutcomeMeasuresTitles{Idx1})
        end
        if Idx1==1
            ylabel(OutcomeMeasuresTitles{Idx2})
        end
    end
end
chART.save_figure('CorrelateVariables', ResultsFolder, PlotProps)

%% Correlate overnight changes

MetadataScatter = Metadata;

OvernightMetadata = pair_recordings(MetadataScatter, 'Hour', {'eve', 'mor'});


Grid = [numel(OutcomeMeasures) numel(OutcomeMeasures)];
figure('Units','centimeters','OuterPosition',[0 0 20 20])
for Idx1 = 1:numel(OutcomeMeasures)
    for Idx2 = 1:numel(OutcomeMeasures)
        chART.sub_plot([], Grid, [Idx2, Idx1], [], false, '', PlotProps);

        if Idx1==Idx2
            if Idx1==1
                chART.set_axis_properties(PlotProps)
                title(OutcomeMeasuresTitles{Idx1})
                ylabel(OutcomeMeasures{Idx2})
            end
            axis off
            continue
        end

        % MetadataAverage = unique_metadata(OvernightMetadata, 'Participant');
        CorrectedMetadata = correct_for_age(OvernightMetadata);

        plot_scattercloud(CorrectedMetadata, OutcomeMeasures{Idx1}, OutcomeMeasures{Idx2}, PlotProps, '', false)
        set(gca, 'XTick' ,[], 'YTick', [])
        axis square
        if Idx2 == numel(OutcomeMeasures)
            xlabel(OutcomeMeasuresTitles{Idx1})
        elseif Idx2==1
            title(OutcomeMeasuresTitles{Idx1})
        end
        if Idx1==1
            ylabel(OutcomeMeasuresTitles{Idx2})
        end
    end
end
chART.save_figure('CorrelateVariablesOvernight', ResultsFolder, PlotProps)


%% run models for each measure (quality check)
clc

FormulaString = ' ~ Hour*Age + (1|Participant)'; % MAIN ONE

for MeasureIdx = 1:numel(OutcomeMeasures)
    formula = [OutcomeMeasures{MeasureIdx}, FormulaString];
    Model = fitlme(MetadataStat, formula);

    % Display the model summary
    disp(['____________________ ', OutcomeMeasures{MeasureIdx}, ' ____________________'])
    disp(Model);
    disp_mixed_stat(Model, 'Age')
    disp_mixed_stat(Model, 'Hour_2')
    disp_mixed_stat(Model, 'Age:Hour_2')


    save_model(Model, fullfile(ResultsFolder, ['BasicModel_', OutcomeMeasures{MeasureIdx}, '.txt']))
end
