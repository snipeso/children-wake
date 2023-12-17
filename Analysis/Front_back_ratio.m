clear
clc
close all

Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Channels = Parameters.Channels;



ResultsFolder = fullfile(Paths.Results, 'Main');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';
load(fullfile(CacheDir, CacheName), 'Metadata',  ...
    'BurstInformationTopography', 'Chanlocs')

% select data for the paper
Metadata.Index = [1:size(Metadata, 1)]'; %#ok<NBRAK1> % add index so can chop up table as needed
Metadata(strcmp(Metadata.Dataset, 'SleepLearning') & ...
    contains(Metadata.Session, {'Session_2', 'Session_3'}), :) = []; % remove repeated measures 1 year later (will average recordings a couple weeks apart)
Metadata(contains(Metadata.Task, {'3Oddball', '1GoNoGo', '2Learning', '3Fixation', '4Fixation'}), :) = []; % only look at first oddball and alertness task
Metadata.Subgroup(strcmp(Metadata.Group, 'HC')) = 5;
Metadata.Globality = Metadata.Globality*100; % make it percentage instead of proportion

Measures = {'Amplitude', 'Quantity', 'Slope', 'Intercept', 'PeriodicPower'};
nMeasures = numel(Measures);
%%
XLim = [3 25];
Grid = [1 nMeasures];

FrontChannels = labels2indexes(Channels.PreROI.Front, Chanlocs);
BackChannels = labels2indexes(Channels.PreROI.Back, Chanlocs);

FrontChannels = labels2indexes([11 10 15 16 18 4 19], Chanlocs);
BackChannels = labels2indexes([74 82 70 75 83 71 76], Chanlocs);

EveningMetadata = overnight_changes(Metadata);
MorningMetadata = EveningMetadata;
MorningMetadata.Index = EveningMetadata.MorningIndexes;
OvernightMetadata = unique_metadata(EveningMetadata);

figure('Units','normalized','OuterPosition',[0 0 1 .3])
for MeasureIdx = 1:nMeasures
        Topographies = BurstInformationTopography.(Measures{MeasureIdx});
    Indexes = [1:size(EveningMetadata, 1)]';
    Evening = average_by_column(EveningMetadata, Topographies, 'Participant', Indexes);
    Morning = average_by_column(MorningMetadata, Topographies, 'Participant', Indexes);
    
    Change = Morning-Evening;
    Front = mean(Change(:, FrontChannels), 2, 'omitnan');
    Back = mean(Change(:, BackChannels), 2, 'omitnan');
    % FrontBackRatio = (Front-Back)./Back;
       FrontBackRatio = Front./Back;
       % Threshold = 1+std(FrontBackRatio)*5;
       % FrontBackRatio(isoutlier(FrontBackRatio)) = nan;
          % FrontBackRatio = (Front-Back);
    FrontBackRatio(abs(FrontBackRatio)>10) = nan;
    OvernightMetadata.FrontBackRatio = FrontBackRatio;
     chART.sub_plot([], Grid, [1, MeasureIdx], [], true, '', PlotProps);
            plot_scattercloud(OvernightMetadata, 'Age', 'FrontBackRatio', ...
                PlotProps, '', false, XLim)
            legend off
          title(Measures{MeasureIdx})  
          xlabel('Age')
          if MeasureIdx ==1
          ylabel('Front/Back')
          end
end

chART.save_figure(['FrontvBachScatterAge', GroupColumn], ResultsFolder, PlotProps)




