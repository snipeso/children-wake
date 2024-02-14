% script to run main mixed effects models to determine the significance of
% the most important factors for the paper's analysis.

clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% setup variables and parameters

Parameters = analysisParameters();
Hours = Parameters.Hours;

OutcomeMeasures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'Power', 'PeriodicPower'};

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
table_demographics(unique_metadata(Metadata), 'Subgroup', ResultsFolder, 'Subgroup')
table_demographics(unique_metadata(Metadata), 'Dataset', ResultsFolder, 'Dataset')
table_demographics(Metadata, 'Hour', ResultsFolder, 'Hour')


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Analyses

%% run mixed models

FormulaString = ' ~ Task + Hour*Age + Group + Sex + (1|Participant) + (1|Participant:SessionUnique)';
% FormulaString = ' ~ Task + Hour*Age + (1|Participant) + (1|Participant:SessionUnique)'; % this model provides the better BIC


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

for MeasureIdx = 1:numel(OutcomeMeasures)
    formula = [OutcomeMeasures{MeasureIdx}, FormulaString];
    Model = fitlme(MetadataStat, formula);

    % Display the model summary
    disp(['____________________ ', OutcomeMeasures{MeasureIdx}, ' ____________________'])
    disp(Model);
    disp_mixed_stat(Model, 'Age')
    disp_mixed_stat(Model, 'Group_2')
    disp_mixed_stat(Model, 'Hour_2')
    disp_mixed_stat(Model, 'Sex_2')
    disp_mixed_stat(Model, 'Age:Hour_2')


    save_model(Model, fullfile(ResultsFolder, ['BasicModel_', OutcomeMeasures{MeasureIdx}, '.txt']))
end


%% scatterplot of basic information
close all
PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 20;
Grid = [3 numel(OutcomeMeasures)];

% PlotProps.Text.FontName = 'Tw Cen MT';

% fix y lims, so same for mor and eve
YLimits = [5, 42; % amplitudes
    70, 550; % quantities
    .7 2.25; % slope
    .3, 2.5; % intercept
    -1.6, 2; % power
    -.05, .705; % periodic power
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
        legend off

        if HourIdx==1
            title(OutcomeMeasures{VariableIdx})
        end
        if VariableIdx==1
            ylabel(HourLabels{HourIdx}, 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
        end
        disp([ Hours{HourIdx}, OutcomeMeasures{VariableIdx}, ...
            'N=', num2str(numel(unique(MetadataAverage.Participant)))])

    end

    %%% plot overnight change
    chART.sub_plot([], Grid, [3, VariableIdx], [], true, '', PlotProps);
    MetadataAverage = unique_metadata(OvernightMetadata, 'Participant');

    plot_scattercloud(MetadataAverage, 'Age', OutcomeMeasures{VariableIdx}, ...
        PlotProps, '', true, XLim)
    if VariableIdx ~=numel(OutcomeMeasures)
        legend off
    end
    if VariableIdx==1
        ylabel('Overnight change', 'FontWeight','bold', 'FontSize',PlotProps.Text.TitleSize)
    end
    xlabel('Age')
    disp(['Overnight', OutcomeMeasures{VariableIdx}, ...
        'N=', num2str(numel(unique(MetadataAverage.Participant)))])
end
chART.save_figure('BasicScatterAge', ResultsFolder, PlotProps)






%% correlate measures

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
                title(OutcomeMeasures{Idx1})
                ylabel(OutcomeMeasures{Idx2})
            end
            axis off
            continue
        end

        plot_scattercloud(Metadata, OutcomeMeasures{Idx1}, OutcomeMeasures{Idx2}, PlotProps, '', false)
        set(gca, 'XTick' ,[], 'YTick', [])
        axis square
        if Idx2 == numel(OutcomeMeasures)
            xlabel(OutcomeMeasures{Idx1})
        elseif Idx2==1
            title(OutcomeMeasures{Idx1})
        end
        if Idx1==1
            ylabel(OutcomeMeasures{Idx2})
        end
    end
end
chART.save_figure('CorrelateVariables', ResultsFolder, PlotProps)



%% mixed model to correct for multiple recordings etc.

FormulaFixed = '~ Task + Hour*Age +';
FormulaRandom = '+ (1|Participant) + (1|Participant:SessionUnique)';


Stats = nan(numel(OutcomeMeasures), numel(OutcomeMeasures), 4); % estimates, tStats, DF, pValues
Stats = table();
Stats.OutcomeMeasures = OutcomeMeasures';
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
        Stats.(OutcomeMeasures{Idx1})(Idx2) = {StatString};
    end
end
writetable(Stats, fullfile(ResultsFolder, 'CorrelationsOutcomeVariables.xlsx'))


%%
figure('Units','centimeters','InnerPosition',[0 0 12 8])
imagesc(AllT)
chART.plot.vertical_colorbar('t-values', PlotProps)
chART.set_axis_properties(PlotProps)
colormap(PlotProps.Color.Maps.Linear)
set(gca, 'xtick', 1:nMeasures, 'xticklabels', OutcomeMeasures, 'XAxisLocation','top', ...
    'ytick', 1:nMeasures, 'yticklabels', OutcomeMeasures, 'TickLength', [0 0])
axis square
