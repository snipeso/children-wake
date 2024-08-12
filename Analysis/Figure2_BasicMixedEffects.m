% script to run main mixed effects models to determine the significance of
% the most important factors for the paper's analysis on the wake EEG data.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% setup variables and parameters

Parameters = analysisParameters();

Hours = Parameters.Hours;

OutcomeMeasures = Parameters.OutcomeMeasures.OriginalLabels;
OutcomeMeasuresTitles = Parameters.OutcomeMeasures.Titles;
MeasureUnits = Parameters.OutcomeMeasures.Units;

ErrorMeasures = {'Error', 'RSquared'};
ErrorMeasuresTitles = ErrorMeasures;
ErrorUnits = {'error', 'R^2'};

%%% set paths
Paths = Parameters.Paths;

% where data can be found
CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

% where to save figures
ResultsFolder = fullfile(Paths.Results, 'MainStats');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

%%% load data
load(fullfile(CacheDir, CacheName), 'Metadata')

% fixes to metadata
Metadata = basic_metadata_cleanup(Metadata);

% overview of final dataset
Patients = Metadata(contains(Metadata.Group, 'ADHD'), :);
table_demographics(unique_metadata(Patients), 'Subgroup', ResultsFolder, 'SubgroupPatients')
Controls = Metadata(contains(Metadata.Group, 'HC'), :);
table_demographics(unique_metadata(Controls), 'Subgroup', ResultsFolder, 'SubgroupControls')
table_demographics(unique_metadata(Metadata), 'Dataset', ResultsFolder, 'Dataset')
table_demographics(Metadata, 'Hour', ResultsFolder, 'Hour')


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analyses


%% save table to publish

MetadataPublish = Metadata;

OriginalTableLables = MetadataPublish.Properties.VariableNames;
for Idx = 1:numel(OutcomeMeasuresTitles)

    IdxTable = strcmp(OriginalTableLables, OutcomeMeasures{Idx});
    MetadataPublish.Properties.VariableNames(IdxTable) = genvarname(OutcomeMeasuresTitles(Idx));
end

writetable(MetadataPublish, fullfile(ResultsFolder, 'AllData_Wake.csv'))


%% run mixed models

FormulaString = ' ~ Task + Hour*Age + Group + Sex + (1|Participant) + (1|Participant:SessionUnique)'; % MAIN ONE
% FormulaString = ' ~ Task + Hour*Age + (1|Participant) + (1|Participant:SessionUnique)'; % this model provides the better BIC
% FormulaString = ' ~ Task + Hour*Age + Group + Sex + (1|Participant) + (1|Participant:SessionUnique) + (1|Participant:Dataset)'; % control

%%% setup metadata for statistics
MetadataStat = Metadata;

% make categorical (removes the interpetable names, but makes sure the
% order is correct, so that the first category is the "baseline" kind).
% Also selects the categories to be considered.
MetadataStat.Participant = categorical(MetadataStat.Participant);

MetadataStat = make_categorical(MetadataStat, 'Task', {'Oddball', 'GoNoGo', 'Alertness', 'Fixation'}); % compare all tasks to the oddball
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'}); % compare morning to evening
MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'}); % compare patietns to controls
MetadataStat = make_categorical(MetadataStat, 'Sex', {'f', 'm'}); % compare males to females


%%% run models
clc
if exist(fullfile(ResultsFolder, "BasicModel_AllStats.txt"), 'file')
    delete(fullfile(ResultsFolder, "BasicModel_AllStats.txt"))
end

diary(fullfile(ResultsFolder, "BasicModel_AllStats.txt"))
diary on
OutcomeMeasures_Extended = [OutcomeMeasures, ErrorMeasures];
OutcomeMeasures_ExtendedLabels = [OutcomeMeasuresTitles, ErrorMeasuresTitles];
for MeasureIdx = 1:numel(OutcomeMeasures_Extended)
    formula = [OutcomeMeasures_Extended{MeasureIdx}, FormulaString];
    Model = fitlme(MetadataStat, formula);

    % Display the model summary
    disp('   ')
disp('   ')
    disp(['____________________ ', OutcomeMeasures_ExtendedLabels{MeasureIdx}, ' ____________________'])
    disp(Model);
    disp_mixed_stat(Model, 'Age')
    disp_mixed_stat(Model, 'Group_2')
    disp_mixed_stat(Model, 'Hour_2')
    disp_mixed_stat(Model, 'Sex_2')
    disp_mixed_stat(Model, 'Age:Hour_2')


    save_model(Model, fullfile(ResultsFolder, ['BasicModel_', OutcomeMeasures_ExtendedLabels{MeasureIdx}, '.txt']))
end
diary off


%% average error and r squared

disp_stats_descriptive(MetadataStat.Error, 'Error', '', 3);
disp_stats_descriptive(MetadataStat.RSquared, 'rsquared', '', 3);

%% scatterplot of basic information (Figure 2)

close all
PlotProps = Parameters.PlotProps.Manuscript;
% PlotProps.Figure.Padding = 0;
PlotProps.Axes.xPadding = 0;
PlotProps.Axes.yPadding = 0;

Grid = [3 numel(OutcomeMeasures)];

% PlotProps.Text.FontName = 'Tw Cen MT';

% fix y lims, so same for mor and eve
YLimits = [5, 42; % amplitudes
    70, 550; % quantities
    .7 2.25; % slope
    .3, 2.5; % intercept
    -.8, 1; % power
    -.05, .705; % periodic power
      -.07, .5; % periodic power
    ];
XLim = [3 25];

HourLabels = {'Evening', 'Morning'};

% select only some of the data
MetadataScatter = Metadata;
MetadataScatter = MetadataScatter(contains(MetadataScatter.Task, {'Oddball'}), :);

OvernightMetadata = pair_recordings(MetadataScatter, 'Hour', {'eve', 'mor'});

clc
figure('Units','centimeters','OuterPosition',[0 0 25 18])

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
            chART.plot.vertical_text(HourLabels{HourIdx}, .6, .5, PlotProps)
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
        chART.plot.vertical_text('Overnight change', .6, .5, PlotProps)
        xlabel('Age (years)')
    end

    disp(['Overnight', OutcomeMeasures{VariableIdx}, ...
        'N=', num2str(numel(unique(MetadataAverage.Participant)))])
end
chART.save_figure('BasicScatterAge', ResultsFolder, PlotProps)





%% mixed model to correct for multiple recordings etc.

FormulaFixed = '~ Task + Hour*Age +';
FormulaRandom = '+ (1|Participant) + (1|Participant:SessionUnique)';

Stats = table();
Stats.OutcomeMeasures = genvarname(OutcomeMeasuresTitles)';
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
            pString = '<.001';
        else
            pString = ['=.',pString];
        end

        StatString = ['b=', num2str(beta, '%.2f'), '; t=', num2str(t, '%.1f'), '; p', pString, '; df=', num2str(df)];
        Stats.(genvarname(OutcomeMeasuresTitles{Idx1}))(Idx2) = {StatString};
        TValues.(genvarname(OutcomeMeasuresTitles{Idx1}))(Idx2) = t;
    end
end
clc
disp(Stats)
writetable(Stats, fullfile(ResultsFolder, 'CorrelationsOutcomeVariables.xlsx'))
writetable(TValues, fullfile(ResultsFolder, 'CorrelationsOutcomeVariables_TValues.csv'))



%% plot errors (Suppl. Figure 2-1)


PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 20;
PlotProps.Axes.xPadding = 20;
Grid = [3 numel(ErrorMeasures)];

% fix y lims, so same for mor and eve
YLimits = [0 max(Metadata.Error)+.005; % error
    min(Metadata.RSquared)-.005 1.005; % r^2
    ];
XLim = [3 25];

HourLabels = {'Evening', 'Morning'};

% select only some of the data
MetadataScatter = Metadata;
MetadataScatter = MetadataScatter(contains(MetadataScatter.Task, {'Oddball'}), :);

OvernightMetadata = pair_recordings(MetadataScatter, 'Hour', {'eve', 'mor'});

clc
figure('Units','centimeters','OuterPosition',[0 0 11 18])

for VariableIdx = 1:numel(ErrorMeasures)

    %%% plot age x v split by evening and morning, averaged across sessions
    for HourIdx = 1:numel(Hours)

        % select data of either evening or morning
        MetadataHour = MetadataScatter(strcmp(MetadataScatter.Hour, Hours(HourIdx)), :);

        % average sessions and multiple tasks (1oddball and 3 oddball)
        MetadataAverage = unique_metadata(MetadataHour, 'Participant');

        % plot
        chART.sub_plot([], Grid, [HourIdx, VariableIdx], [], true, '', PlotProps);
        plot_scattercloud(MetadataAverage, 'Age', ErrorMeasures{VariableIdx}, ...
            PlotProps, '', false, XLim, YLimits(VariableIdx, :))
        ylabel(ErrorUnits{VariableIdx})
        legend off

        if HourIdx==1
            title(ErrorMeasuresTitles{VariableIdx})
        end
        if VariableIdx==1
            chART.plot.vertical_text(HourLabels{HourIdx}, .55, .5, PlotProps)
        end
        disp([ Hours{HourIdx}, ErrorMeasures{VariableIdx}, ...
            'N=', num2str(numel(unique(MetadataAverage.Participant)))])

    end

    %%% plot overnight change
    chART.sub_plot([], Grid, [3, VariableIdx], [], true, '', PlotProps);
    MetadataAverage = unique_metadata(OvernightMetadata, 'Participant');

    plot_scattercloud(MetadataAverage, 'Age', ErrorMeasures{VariableIdx}, ...
        PlotProps, '', true, XLim)
    ylabel(ErrorUnits{VariableIdx})
    xlabel('Age')
    if VariableIdx ~=numel(ErrorMeasures)
        legend off
    end

    if VariableIdx==1
        chART.plot.vertical_text('Overnight change', .55, .5, PlotProps)
        xlabel('Age (years)')
    end

    disp(['Overnight', ErrorMeasures{VariableIdx}, ...
        'N=', num2str(numel(unique(MetadataAverage.Participant)))])
end
chART.save_figure('BasicScatterAge_Errors', ResultsFolder, PlotProps)



%% correlate measures (Suppl. Figure 2-2)

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 25;
PlotProps.Text.TitleSize = 10;
PlotProps.Axes.yPadding = 5;
PlotProps.Axes.xPadding = 5;
PlotProps.Scatter.Size = 5;
PlotProps.Scatter.Alpha = .4;

Grid = [numel(OutcomeMeasures) numel(OutcomeMeasures)];
figure('Units','centimeters','OuterPosition',[0 0 18 18])
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

        plot_scattercloud(Metadata, OutcomeMeasures{Idx1}, OutcomeMeasures{Idx2}, PlotProps, '', false)
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
chART.save_figure('CorrelateVariables', ResultsFolder, PlotProps)



%% calculate how many intact datasets there were 

Datasets = {'SleepLearning', 'Providence', 'ADHD', 'BMSAdults', 'BMS', 'BMSSL'};
nRecordings = [8, 2, 4, 4, 16, 12]; % total expected recordings per dataset
% average recordings (/expected recordings)

UniqueMetadata = unique_metadata(Metadata);
nParticipants = size(UniqueMetadata, 1);
UniqueMetadata.nRecordings = nan(nParticipants, 1);

for ParticipantIdx = 1:nParticipants
    Subset = Metadata.Index(strcmp(Metadata.Participant, UniqueMetadata.Participant{ParticipantIdx}));
    UniqueMetadata.nRecordings(ParticipantIdx) = numel(Subset);
end

clc
for DatasetIdx = 1:numel(Datasets)
    Average = mean(UniqueMetadata.nRecordings(strcmp(UniqueMetadata.Dataset, Datasets{DatasetIdx})));
    disp([Datasets{DatasetIdx}, ' = ', num2str(round(Average, 1)), '/', num2str(nRecordings(DatasetIdx))])
end


%% identify duration of each dataset, save data to publish

CacheName = 'Durations.mat';
load(fullfile(CacheDir, CacheName), 'Metadata')
Durations = Metadata;

CacheName = 'AllBursts.mat';
load(fullfile(CacheDir, CacheName), 'Metadata')
Metadata = basic_metadata_cleanup(Metadata);
Metadata.Dataset = Durations.Dataset; % recoded dataset names
Metadata.RecordingDuration = Durations.RecordingDuration;

% rename columns
OriginalTableLables = Metadata.Properties.VariableNames;
for Idx = 1:numel(OutcomeMeasuresTitles)

    IdxTable = strcmp(OriginalTableLables, OutcomeMeasures{Idx});
    Metadata.Properties.VariableNames(IdxTable) = genvarname(OutcomeMeasuresTitles(Idx));
end


Destination = 'D:\Dropbox\Research\Publications and Presentations\Sleep\Papers\KidsBursts\Version6_ImagingNeuroscience\SupplMaterial';
writetable(Metadata, fullfile(Destination, 'Data1_AllWakeData.csv'))

