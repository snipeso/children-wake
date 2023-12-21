
clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
Bands = Parameters.Bands;
PlotProps = Parameters.PlotProps.Manuscript;

ResultsFolder = fullfile(Paths.Results, 'Geneology');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

% Filename_Core = 'P137_SleepLearning_Session11_eve_1Oddball_n_1.mat';
Filename_Core = 'P139_SleepLearning_Session11_eve_1Oddball_n_1.mat';
Levels = split(Filename_Core, '_');
Participant = Levels{1};
Dataset = Levels{2};
Session = Levels{3};
Hour = Levels{4};
Task = Levels{5};

% load data (EEG, power, and bursts)
DataOut = load_datafile(fullfile(Paths.CleanEEG, Dataset, Task), Participant, Session, Hour, {'EEG'}, '.mat');
EEG = DataOut{1};

DataOut = load_datafile(fullfile(Paths.AnalyzedData, 'EEG', 'Bursts', Dataset, Task), Participant, Session, Hour, {'Bursts'}, '.mat');
Bursts = DataOut{1};
Bursts = burst_bands(Bursts, Bands);

DataOut = load_datafile(fullfile(Paths.AnalyzedData, 'EEG', 'Power', 'window4s_allt', Dataset, Task), Participant, Session, Hour, {'Power', 'Freqs'}, '.mat');
Power = DataOut{1};
Freqs = DataOut{2};

cycy.plot.plot_all_bursts(EEG, 15, Bursts, 'NewBand');

%%


DurationAperiodic = 15;
Start = 245;
% Start = 250;
Aperiodic = EEG.data(end, Start*EEG.srate:(Start+DurationAperiodic)*EEG.srate);

HighAlpha = EEG.data(labels2indexes(51, EEG.chanlocs), 77*EEG.srate:78*EEG.srate);
DurationHighAlpha = numel(HighAlpha);
StartHighAlpha = 3.5*EEG.srate;
Aperiodic(StartHighAlpha:StartHighAlpha+DurationHighAlpha-1) = HighAlpha;

LowAlpha = EEG.data(labels2indexes(60, EEG.chanlocs), 149.8*EEG.srate:151.2*EEG.srate);
DurationLowAlpha = numel(LowAlpha);
StartLowAlpha = 6*EEG.srate;
Aperiodic(StartLowAlpha:StartLowAlpha+DurationLowAlpha-1) = LowAlpha;

Theta = EEG.data(labels2indexes(11, EEG.chanlocs), 180*EEG.srate:181.25*EEG.srate);
DurationTheta = numel(Theta);
StartTheta = 9*EEG.srate;
Aperiodic(StartTheta:StartTheta+DurationTheta-1) = Theta;

t = linspace(0, DurationAperiodic, numel(Aperiodic));

figure('Units','centimeters', 'Position',[0 0 40 3])
chART.sub_plot([], [1 1], [1 1], [], '', '', PlotProps)
plot(t, Aperiodic, 'LineWidth', 1.5, 'Color', 'k')
axis off
ylim([-40 40])
xlim([0 15])
chART.save_figure('EEG', ResultsFolder, PlotProps)


% plot single channel snippet (composite?)
LW_Bursts = 2;
figure('Units','centimeters', 'Position', [0 0 25 3])
chART.sub_plot([], [1 1], [1 1], [], '', '', PlotProps)
plot(t, Aperiodic, 'LineWidth', 1.5, 'Color', [.5 .5 .5])
hold on
plot(t(StartTheta:StartTheta+DurationTheta-1), Aperiodic(StartTheta:StartTheta+DurationTheta-1), ...
    'Color',chART.color_picker(1, '', 'yellow'), 'LineWidth', LW_Bursts)
plot(t(StartLowAlpha:StartLowAlpha+DurationLowAlpha-1), Aperiodic(StartLowAlpha:StartLowAlpha+DurationLowAlpha-1), ...
    'Color',chART.color_picker(1, '', 'orange'), 'LineWidth', LW_Bursts)
plot(t(StartHighAlpha:StartHighAlpha+DurationHighAlpha-1), Aperiodic(StartHighAlpha:StartHighAlpha+DurationHighAlpha-1), ...
    'Color',chART.color_picker(1, '', 'red'), 'LineWidth', LW_Bursts)
axis off
ylim([-40 40])
xlim([2.5 11.5])
chART.save_figure('Bursts', ResultsFolder, PlotProps)


%%

PlotSize = [0 0 5.5 5.5];
LW_Plot = 1.5;
PowerAverage = mean(Power(labels2indexes([11, 60, 51, 129], EEG.chanlocs), :), 1);
PowerAverage = smooth_frequencies(PowerAverage, Freqs, 2);

close all

figure('Units','centimeters', 'Position', PlotSize)
plot(Freqs, PowerAverage, 'Color', 'k', 'LineWidth',LW_Plot)
chART.set_axis_properties(PlotProps)
xlabel('Frequency (Hz)')
ylabel('Power (\muV^2/Hz)')
xlim([1 20])
ylim([0 15])
axis square
box off
chART.save_figure('Power', ResultsFolder, PlotProps)


Bands = struct();
Bands.Theta = [4 7];
Bands.LowAlpha = [8 11];
Bands.HighAlpha = [12 16];

% log power
figure('Units','centimeters', 'Position', PlotSize)
plot_highlighted_spectrum(log(PowerAverage), Freqs, Bands, PlotProps)
xlabel('Frequency (Hz)')
ylabel('Log power')
xlim([4 16])
legend(flip({'Theta', 'Alpha_{low}','Alpha_{high}'}))

ylim([-1, 2.5])
axis square
box off
chART.save_figure('LogPower', ResultsFolder, PlotProps)


% FOOOF


% periodic power



% cartoon slope example


% histogram quantity and amplitude

