clear
clc
close all

MetadataPath = 'X:\Data\Metadata';
Datasets = {'ADHD', 'BMS', 'BMSSL', 'SleepLearning'};


Metadata = assemble_metadata(MetadataPath, Datasets);

writetable(Metadata, fullfile(MetadataPath, 'Metadata_Children_Wake.csv'));