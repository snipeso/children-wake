% Assuming you have a table named 'Metadata' with columns 'Amplitude', 'Participant', 'Condition', 'Hour', 'Task', and 'Age'
% Make sure MATLAB Statistics and Machine Learning Toolbox is installed
clear
clc
close all
Parameters = analysisParameters();
Paths = Parameters.Paths;

CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';

load(fullfile(CacheDir, CacheName), 'Metadata', 'BurstInformationTopographyBands', ...
    'BurstInformationTopography', "BurstInformationClusters", 'Frequencies', 'Chanlocs')

% Load your data or use your existing table
MetadataStat = Metadata;
MetadataStat = make_categorical(MetadataStat, 'Condition', {'base', 'rotation'});
MetadataStat = make_categorical(MetadataStat, 'Task', {'1Oddball', '3Oddball'});
MetadataStat = make_categorical(MetadataStat, 'Hour', {'eve', 'mor'});
MetadataStat.Participant = categorical(MetadataStat.Participant);
MetadataStat = make_categorical(MetadataStat, 'Group', {'HC', 'ADHD'});


%%

clc
% Create a linear mixed-effects model with 'Age' as a fixed effect
OutcomeMeasure = 'Amplitude';
formula = [OutcomeMeasure, ' ~ Condition*Task + Hour*Age + Group + (1|Participant)'];
% mdl = fitlme(MetadataStat, formula,  'DummyVarCoding', 'effects');
mdl = fitlme(MetadataStat, formula);

% Display the model summary
disp(mdl);

% % You can also check the fixed effects and random effects separately
% disp('Fixed Effects:');
% disp(fixedEffects(mdl));
% 
% disp('Random Effects:');
% disp(randomEffects(mdl));
