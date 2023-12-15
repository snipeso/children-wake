clear
clc
close all

MetadataPath = 'D:\Data\AllWake\Metadata';
Datasets = {'ADHD', 'BMS', 'BMSSL', 'SleepLearning', 'Providence', 'BMSAdults'};


Metadata = assemble_metadata(MetadataPath, Datasets);

writetable(Metadata, fullfile(MetadataPath, 'Metadata_Children_Wake.csv'));