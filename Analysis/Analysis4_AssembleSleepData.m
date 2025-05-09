clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% parameters

Parameters = analysisParameters();
Paths = Parameters.Paths;
SleepPaths = Parameters.SleepPaths;

% where to save figures
ResultsFolder = fullfile(Paths.Results, 'SleepWakeStatsStandaridzed');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% run

CacheDir = Paths.Cache;
CacheNameSleep = 'AllSlowWaves.mat';
load(fullfile(CacheDir, CacheNameSleep), 'MetadataSleep')

% load in wake metadata
CacheNameWake = 'AllBursts.mat';
load(fullfile(CacheDir, CacheNameWake), 'Metadata')

% fixes to metadata
Metadata = basic_metadata_cleanup(Metadata);
Metadata(~contains(Metadata.Task, {'Alertness', 'Oddball'}), :) = []; % only use oddball-like tasks
Metadata(contains(Metadata.Task, '3Oddball'), :) = [];

% change names, especially so wake "slope" becomes "exponent"
OutcomeMeasures = [Parameters.OutcomeMeasures.OriginalLabels];
OutcomeMeasuresTitles = [Parameters.OutcomeMeasures.Titles];
OriginalTableLables = Metadata.Properties.VariableNames;
for Idx = 1:numel(OutcomeMeasuresTitles)

    IdxTable = strcmp(OriginalTableLables, OutcomeMeasures{Idx});
    Metadata.Properties.VariableNames(IdxTable) = genvarname(OutcomeMeasuresTitles(Idx));
end

MetadataSleep = basic_metadata_cleanup(MetadataSleep);

% identify which columns in sleep to add to table
SleepColumns = MetadataSleep.Properties.VariableNames;
WakeColumns = Metadata.Properties.VariableNames;
SleepVariables = setdiff(SleepColumns, WakeColumns);

%%% add sleep data to wake data table
for RowIdx = 1:size(Metadata, 1)
    Participant = Metadata.Participant{RowIdx};
    Session = Metadata.Session{RowIdx};
    Hour = Metadata.Hour{RowIdx};

    % only examine sleep variables of corresponding hour
    if strcmp(Hour, 'eve')
        Stump = 'FH_';
    else
        Stump = 'LH_';
    end

    HourSleepVariables = SleepVariables(contains(SleepVariables, Stump));

    % identify sleep row index
    SleepRowIdx = strcmp(MetadataSleep.Participant, Participant) & ...
        strcmp(MetadataSleep.Session, Session);
    
    if ~any(SleepRowIdx)
        continue
    end

    for Variable = HourSleepVariables

        NewName = replace(Variable{1}, Stump, '');

        % set up blanks
        if RowIdx==1
             Metadata.(['Sleep_', NewName]) = nan(size(Metadata, 1), 1);
        end

        Metadata.(['Sleep_', NewName])(RowIdx) = MetadataSleep.(Variable{1})(SleepRowIdx);
    end
end

%%% Remove data missing sleep data
Metadata(isnan(Metadata.Sleep_Amplitude), :) = [];

%%% Remove participants missing either evening or morning
KeepRows = zeros(size(Metadata, 1), 1);
EveningIndexes = find(strcmp(Metadata.Hour, 'eve'));
KeepRows(EveningIndexes) = 1;

% check for corresponding morning recording
for RowIdx = EveningIndexes'

    MorningIndex = strcmp(Metadata.Participant, Metadata.Participant{RowIdx}) & ...
        strcmp(Metadata.Session, Metadata.Session{RowIdx}) & strcmp (Metadata.Hour, {'mor'});
    
    if ~any(MorningIndex)
           KeepRows(RowIdx) = 0;
    else
        KeepRows(MorningIndex) = 1;
    end
end

Metadata(~KeepRows, :) = [];




%%% set up nice table for CSV

% recode dataset names
Datasets = {'SleepLearning', 'Providence', 'ADHD', 'BMSAdults', 'BMS', 'BMSSL'};
DatasetsNew = {'Dataset2008', 'Dataset2009', 'Dataset2010', 'Dataset2016', 'Dataset2017', 'Dataset2019'};

for DatasetIdx = 1:numel(Datasets)
    Metadata.Dataset(strcmp(Metadata.Dataset, Datasets{DatasetIdx})) = ...
        repmat(DatasetsNew(DatasetIdx), nnz(strcmp(Metadata.Dataset, Datasets{DatasetIdx})), 1);
end


writetable(Metadata, fullfile(ResultsFolder, 'WakeSleepFullData.csv'))



%%

close all
PlotProps = Parameters.PlotProps.Manuscript;

OutcomeMeasuresAll = {'Amplitude', 'Duration', 'Exponent', 'Offset', 'Power', 'PeriodicPower', 'Sleep_Amplitude', 'Sleep_Slope_Matched'};
OutcomeMeasuresTitlesAll = [OutcomeMeasuresTitles, 'SW Amplitudes', 'SW Slopes'];

MetadataScatter = Metadata;

MetadataScatter = MetadataScatter(strcmp(MetadataScatter.Dataset, 'Dataset2017') & strcmp(MetadataScatter.Condition, 'base') , :);

OvernightMetadata = pair_recordings(MetadataScatter, 'Hour', {'eve', 'mor'});


Grid = [numel(OutcomeMeasuresAll) numel(OutcomeMeasuresAll)];
figure('Units','centimeters','OuterPosition',[0 0 20 20])
for Idx1 = 1:numel(OutcomeMeasuresAll)
    for Idx2 = 1:numel(OutcomeMeasuresAll)
        chART.sub_plot([], Grid, [Idx2, Idx1], [], false, '', PlotProps);

        if Idx1==Idx2
            if Idx1==1
                chART.set_axis_properties(PlotProps)
                title(OutcomeMeasuresTitlesAll{Idx1})
                ylabel(OutcomeMeasuresAll{Idx2})
            end
            axis off
            continue
        end

        % MetadataAverage = unique_metadata(OvernightMetadata, 'Participant');
        CorrectedMetadata = correct_for_age(OvernightMetadata);

        plot_scattercloud(CorrectedMetadata, OutcomeMeasuresAll{Idx1}, OutcomeMeasuresAll{Idx2}, PlotProps, '', false)
        set(gca, 'XTick' ,[], 'YTick', [])
        axis square
        if Idx2 == numel(OutcomeMeasuresAll)
            xlabel(OutcomeMeasuresTitlesAll{Idx1})
        elseif Idx2==1
            title(OutcomeMeasuresTitlesAll{Idx1})
        end
        if Idx1==1
            ylabel(OutcomeMeasuresTitlesAll{Idx2})
        end
    end
end
