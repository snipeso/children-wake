clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameters

Parameters = simulationParameters();
Paths = Parameters.Paths;
ResultsFolder = Paths.Results;


% where data can be found
CacheDir = Paths.Cache;
CacheName = 'AllBursts.mat';
NewCacheName = 'AllButAmplitudes.mat';


%%% load data
load(fullfile(CacheDir, CacheName), 'Metadata')

% fixes to metadata
Metadata = basic_metadata_cleanup(Metadata);

MetadataSim = Metadata;

for RowIdx = 1:size(MetadataSim, 1)
[MetadataSim.Slope(RowIdx), MetadataSim.Intercept(RowIdx), ...
    MetadataSim.Amplitude(RowIdx), MetadataSim.Quantity(RowIdx), ...
    MetadataSim.Power(RowIdx), MetadataSim.PeriodicPower(RowIdx)] = ...
    simulate_recording(Metadata.Slope(RowIdx), Metadata.Intercept(RowIdx), Metadata.Quantity(RowIdx)/600); % divided by 6 because its pooling channels
disp(RowIdx)
end

save(fullfile(CacheDir, NewCacheName), 'MetadataSim')