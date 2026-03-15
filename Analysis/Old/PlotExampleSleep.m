clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% setup variables and parameters

Parameters = analysisParameters();
Paths = Parameters.Paths;

% where to save figures
ResultsFolder = fullfile(Paths.Results, 'ExampleSleep');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

Black = [0 10 53]/255;
load('D:\Data\AllSleep\Final\EEG\Power\20second_epochs_kispi\Providence\P158_Providence_Session1_kispipower.mat', 'Power', 'Freqs', 'Artefacts', 'Time')

for ChannelIdx = 1:size(Power, 1)
Power(ChannelIdx, :, Artefacts(ChannelIdx, :)~=1) = nan;
end

Delta = dsearchn(Freqs', [1; 4]);
Data = squeeze(mean(mean(Power(:, Delta(1):Delta(2), :), 1, 'omitnan'), 2, 'omitnan'));



PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 5;

% Set the axes' background color to 'none'
ax = gca;
set(ax, 'Color', 'none');
set(gcf, 'Color', 'none');


figure('Units','centimeters','OuterPosition',[0 0 30 15])

chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);
bar(Time, log(Data), 1, 'edgecolor', Black, 'FaceColor',Black)
chART.set_axis_properties(PlotProps)
xlabel('Time (h)')
ylabel('Delta power (log)')
ylim([1.8 7])

box off
set(gca, 'ycolor', Black, 'xcolor', Black, 'LineWidth', .75)

   chART.save_figure('AdultSleep', ResultsFolder, PlotProps)

%%
load('D:\Data\AllSleep\Final\EEG\Power\20second_epochs_kispi\BMSAdults\P179_BMSAdults_Session1_kispipower.mat', 'Power', 'Freqs', 'Artefacts', 'Time')

for ChannelIdx = 1:size(Power, 1)
Power(ChannelIdx, :, Artefacts(ChannelIdx, :)~=1) = nan;
end

Delta = dsearchn(Freqs', [1; 4]);
Data = squeeze(mean(mean(Power(:, Delta(1):Delta(2), :), 1, 'omitnan'), 2, 'omitnan'));

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Figure.Padding = 5;

PlotProps.Color.Background = 'none';
% Set the axes' background color to 'none'
ax = gca;
set(ax, 'Color', 'none');
set(gcf, 'Color', 'none');



figure('Units','centimeters','OuterPosition',[0 0 30 15])

chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);
bar(Time, log(Data), 1, 'edgecolor', Black, 'FaceColor',Black)
chART.set_axis_properties(PlotProps)
xlabel('Time (h)')
ylabel('Delta power (log)')
% ylim([min(log(Data)) max(log(Data))])
ylim([1.8 7])
box off
set(gca, 'ycolor', Black, 'xcolor', Black, 'LineWidth', .75)
   chART.save_figure('ChildSleep', ResultsFolder, PlotProps)
