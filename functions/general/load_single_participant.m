function [EEG, Bursts, BurstClusters, Power, Freqs] = load_single_participant(Filename_Core, Paths)

Levels = split(Filename_Core, '_');
Participant = Levels{1};
Dataset = Levels{2};
Session = Levels{3};
Hour = Levels{4};
Task = Levels{5};
DataOut = load_datafile(fullfile(Paths.CleanEEG, Dataset, Task), Participant, Session, Hour, {'EEG'}, '.mat');
EEG = DataOut{1};

DataOut = load_datafile(fullfile(Paths.AnalyzedData, 'EEG', 'Bursts',  Dataset, Task), Participant, Session, Hour, {'Bursts', 'BurstClusters'}, '.mat');
Bursts = DataOut{1};
BurstClusters = DataOut{2};

DataOut = load_datafile(fullfile(Paths.AnalyzedData, 'EEG', 'Power', 'window4s_allt', Dataset, Task), Participant, Session, Hour, {'Power', 'Freqs'}, '.mat');
Power = DataOut{1};
Freqs = DataOut{2};