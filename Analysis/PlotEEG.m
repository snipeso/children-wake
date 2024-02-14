clear
clc
close all

Info = analysisParameters();
Paths = Info.Paths;
Bands = Info.Narrowbands;
BandLabels = fieldnames(Bands);

%%%% Choose a file
%%%%
%%%%
Task = 'LAT';
% Core = ['P16_', Task, '_BaselineBeam'];
Core = ['P16_', Task, '_Session2Beam3'];
EEG_Source = ['E:\Data\Preprocessed\Clean\Waves\', Task];
EEG_Filename = [Core, '_Clean.mat'];


Bursts_Source = ['E:\Data\Final\EEG\Bursts_AllChannels\', Task];
Bursts_Filename = [Core, '_Bursts.mat'];
%%%%

load(fullfile(EEG_Source, EEG_Filename), 'EEG')

% load(fullfile(Bursts_Source, Bursts_Filename), 'BurstClusters')
load(fullfile(Bursts_Source, Bursts_Filename), 'AllBursts')

BurstClusters = AllBursts;


for Indx_B = 1:numel(BurstClusters)
    BurstClusters(Indx_B).Color = 1;
    BurstClusters(Indx_B).ChannelIndex = BurstClusters(Indx_B).Channel;
    Frequency = 1./mean(BurstClusters(Indx_B).periodNeg);
    if Frequency >= 4 && Frequency  <= 10
        BurstClusters(Indx_B).Color = 3;
    end
end
BurstClusters(1).Color = 1;
BurstClusters(2).Color = 2;




cycy.plot.plot_all_bursts(EEG, 20, BurstClusters, 'Color');