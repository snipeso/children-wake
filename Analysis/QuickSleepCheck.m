


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

load("D:\Data\KISPISleep\Metadata\SleepScoring.mat",'Metadata')

SleepStages = Metadata;

load("E:\AllWake\Cache\children-wake\ProcessedData.mat", 'Metadata')
 
Metadata = basic_metadata_cleanup(Metadata);

FormulaString = ' ~ Task + Hour*Age + Group + Sex + (1|Participant) + (1|Participant:SessionUnique)'; % MAIN ONE
OutcomeMeasure = 'Amplitude';

MetadataStat = Metadata;

% make categorical (removes the interpetable names, but makes sure the
% order is correct, so that the first category is the "baseline" kind).
% Also selects the categories to be considered.
MetadataStat.Participant = categorical(MetadataStat.Participant);

MetadataStat = make_categorical(MetadataStat, 'Task', {'Oddball', 'GoNoGo', 'Alertness', 'Fixation'}); % compare all tasks to the oddball
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'}); % compare morning to evening
MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'}); % compare patietns to controls
MetadataStat = make_categorical(MetadataStat, 'Sex', {'f', 'm'}); % compare males to females


formula = [OutcomeMeasure, FormulaString];

Model = fitlme(MetadataStat, formula);

clc
disp(Model);

%%
    disp_mixed_stat(Model, 'Age')
    disp_mixed_stat(Model, 'Group_2')
    disp_mixed_stat(Model, 'Hour_2')
    disp_mixed_stat(Model, 'Sex_2')
    disp_mixed_stat(Model, 'Age:Hour_2')


    %%


    % select only some of the data
MetadataScatter = Metadata;
MetadataScatter = MetadataScatter(contains(MetadataScatter.Task, {'Oddball'}), :);
    OvernightMetadata = pair_recordings(MetadataScatter, 'Hour', {'eve', 'mor'});



    %%

   for RowIdx = 1:size(OvernightMetadata, 1)

       SleepRow = strcmp(OvernightMetadata.Participant(RowIdx), SleepStages.Participant) & strcmp(OvernightMetadata.Session(RowIdx), SleepStages.Session);
        if ~any(SleepRow)
            continue
        end
       OvernightMetadata.TST(RowIdx) = SleepStages.TST(SleepRow);
        OvernightMetadata.TimeN3(RowIdx) = SleepStages.timeN3(SleepRow);
         OvernightMetadata.TimeREM(RowIdx) = SleepStages.timeREM(SleepRow);
          OvernightMetadata.WASO(RowIdx) = SleepStages.WASO(SleepRow);
                  OvernightMetadata.TimeN2(RowIdx) = SleepStages.timeN2(SleepRow);

   end
close all
      figure;scatter(OvernightMetadata.TimeN2, OvernightMetadata.Amplitude)
   [R, p] = corr(OvernightMetadata.TimeN2, OvernightMetadata.Amplitude, 'Rows','complete');
   title(['r = ', num2str(round(R, 2)), '; p = ', num2str(round(p, 3))])
   xlabel('Time N2')

   figure;scatter(OvernightMetadata.TimeN3, OvernightMetadata.Amplitude)
   [R, p] = corr(OvernightMetadata.TimeN3, OvernightMetadata.Amplitude, 'Rows','complete');
   title(['r = ', num2str(round(R, 2)), '; p = ', num2str(round(p, 3))])
   xlabel('Time N3')

      figure;scatter(OvernightMetadata.TST, OvernightMetadata.Amplitude)
   [R, p] = corr(OvernightMetadata.TST, OvernightMetadata.Amplitude, 'Rows','complete');
   title(['r = ', num2str(round(R, 2)), '; p = ', num2str(round(p, 3))])
xlabel('TST')

      figure;scatter(OvernightMetadata.TimeREM, OvernightMetadata.Amplitude)
   [R, p] = corr(OvernightMetadata.TimeREM, OvernightMetadata.Amplitude, 'Rows','complete');
   title(['r = ', num2str(round(R, 2)), '; p = ', num2str(round(p, 3))])
xlabel('Time REM')

[R, p] = corr(OvernightMetadata.TimeN3, OvernightMetadata.Age, 'Rows','complete');
disp(['Age vs N3 R = ', num2str(round(R, 2)), '; p = ', num2str(round(p, 3))])
[R, p] = corr(OvernightMetadata.TimeREM, OvernightMetadata.Age, 'Rows','complete');
disp(['Age vs REM R = ', num2str(round(R, 2)), '; p = ', num2str(round(p, 3))])

[R, p] = corr(OvernightMetadata.TimeN2, OvernightMetadata.Age, 'Rows','complete');
disp(['Age vs N2 R = ', num2str(round(R, 2)), '; p = ', num2str(round(p, 3))])
