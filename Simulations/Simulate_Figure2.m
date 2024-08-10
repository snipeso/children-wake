% gets the same stats and figure as real data, except it only assumes that
% exponent, offset and density are correct.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% setup variables and parameters

Parameters = simulationParameters();

Hours = Parameters.Hours;

OutcomeMeasures = Parameters.OutcomeMeasures.OriginalLabels;
OutcomeMeasuresTitles = Parameters.OutcomeMeasures.Titles;
MeasureUnits = Parameters.OutcomeMeasures.Units;

%%% set paths
Paths = Parameters.Paths;

% where data can be found
CacheDir = Paths.Cache;
CacheName = 'AllButAmplitudes20.mat';

% where to save figures
ResultsFolder = fullfile(Paths.Results, 'MainStats');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

%%% load data
load(fullfile(CacheDir, CacheName), 'MetadataSim')

% fixes to metadata
MetadataSim = basic_metadata_cleanup(MetadataSim);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analyses



%% run mixed models

FormulaString = ' ~ Task + Hour*Age + Group + Sex + (1|Participant) + (1|Participant:SessionUnique)'; % MAIN ONE
% FormulaString = ' ~ Task + Hour*Age + (1|Participant) + (1|Participant:SessionUnique)'; % this model provides the better BIC
% FormulaString = ' ~ Task + Hour*Age + Group + Sex + (1|Participant) + (1|Participant:SessionUnique) + (1|Participant:Dataset)'; % control

%%% setup metadata for statistics
MetadataStat = MetadataSim;

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

for MeasureIdx = 1:numel(OutcomeMeasures)
    formula = [OutcomeMeasures{MeasureIdx}, FormulaString];
    Model = fitlme(MetadataStat, formula);

    % Display the model summary
    disp('   ')
disp('   ')
    disp(['____________________ ', OutcomeMeasuresTitles{MeasureIdx}, ' ____________________'])
    disp(Model);
    disp_mixed_stat(Model, 'Age')
    disp_mixed_stat(Model, 'Group_2')
    disp_mixed_stat(Model, 'Hour_2')
    disp_mixed_stat(Model, 'Sex_2')
    disp_mixed_stat(Model, 'Age:Hour_2')


    save_model(Model, fullfile(ResultsFolder, ['BasicModel_', OutcomeMeasuresTitles{MeasureIdx}, '.txt']))
end
diary off



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
    ];
XLim = [3 25];

HourLabels = {'Evening', 'Morning'};

% select only some of the data
MetadataScatter = MetadataSim;
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
            chART.plot.vertical_text(HourLabels{HourIdx}, .5, .5, PlotProps)
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
        chART.plot.vertical_text('Overnight change', .5, .5, PlotProps)
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

