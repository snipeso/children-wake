

clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Bands = Parameters.Bands;
PlotProps = Parameters.PlotProps.Manuscript;

Filename_Core = 'P117_SleepLearning_Session11_eve_1Oddball_n_1.mat';
[EEG, Bursts, ~, Power, Freqs] = load_single_participant(Filename_Core, Paths);

Bursts = burst_bands(Bursts, Bands);

%%
cycy.plot.plot_all_bursts(EEG, 15, Bursts, 'NewBand');


%%
figure('Units','normalized','OuterPosition',[0 0 1 1])

for ChannelIdx = 1:120
subplot(8, 15, ChannelIdx)
histogram([Bursts([Bursts.ChannelIndex]==ChannelIdx).BurstFrequency])
end


%%

figure
plot(Freqs, Power)
set(gca, 'YScale', 'log')
xlim([0 40])