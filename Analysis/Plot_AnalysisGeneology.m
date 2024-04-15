
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
[EEG, Bursts, ~, Power, Freqs] = load_single_participant(Filename_Core, Paths);

Bursts = burst_bands(Bursts, Bands);

% cycy.plot.plot_all_bursts(EEG, 15, Bursts, 'NewBand');

%%

AperiodicGray = [.66 .66 .66];

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
figure('Units','centimeters', 'Position', [0 0 25 5.5])
chART.sub_plot([], [1 1], [1 1], [], '', '', PlotProps)
plot(t, Aperiodic, 'LineWidth', 1.5, 'Color', AperiodicGray)
hold on
plot(t(StartTheta:StartTheta+DurationTheta-1), Aperiodic(StartTheta:StartTheta+DurationTheta-1), ...
    'Color',chART.color_picker(1, '', 'yellow'), 'LineWidth', LW_Bursts)
plot(t(StartLowAlpha:StartLowAlpha+DurationLowAlpha-1), Aperiodic(StartLowAlpha:StartLowAlpha+DurationLowAlpha-1), ...
    'Color',chART.color_picker(1, '', 'orange'), 'LineWidth', LW_Bursts)
plot(t(StartHighAlpha:StartHighAlpha+DurationHighAlpha-1), Aperiodic(StartHighAlpha:StartHighAlpha+DurationHighAlpha-1), ...
    'Color',chART.color_picker(1, '', 'red'), 'LineWidth', LW_Bursts)
axis off
ylim([-50 50])
xlim([2.5 11.5])
chART.save_figure('Bursts', ResultsFolder, PlotProps)


%%

PlotProps.Text.AxisSize = 10;
PlotSize = [0 0 5.5 5.5];
LW_Plot = 1.5;
PowerAverage = mean(Power(labels2indexes([11, 60, 51, 129], EEG.chanlocs), :), 1);
PowerAverageSmooth = smooth_frequencies(PowerAverage, Freqs, 2);


figure('Units','centimeters', 'Position', PlotSize)
chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);
plot(Freqs, PowerAverageSmooth, 'Color', 'k', 'LineWidth',PlotProps.Line.Width)
chART.set_axis_properties(PlotProps)
xlabel('Frequency (Hz)')
ylabel('Power (\muV^2/Hz)')
xlim([1 20])
ylim([0 10])
axis square
box off
chART.save_figure('Power', ResultsFolder, PlotProps)


Bands = struct();
Bands.Theta = [4 7];
Bands.Alpha = [8 11];
Bands.Beta = [12 16];


% log power
figure('Units','centimeters', 'Position', PlotSize)
chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);
plot_highlighted_spectrum(log(PowerAverageSmooth), Freqs, Bands, PlotProps)
xlabel('Frequency (Hz)')
ylabel('Log power')
% xlim([2 18])
xlim([1 20])
legend(flip({'Theta_{ }', 'Alpha','Beta_{low}'}), 'position', [ 0.5887    0.6237    0.3450    0.3125])
legend boxoff  

ylim([-1.7, 3])
axis square
box off
chART.save_figure('LogPower', ResultsFolder, PlotProps)

% log log power
figure('Units','centimeters', 'Position', PlotSize)
chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);
hold on
plot(log(Freqs), log(PowerAverageSmooth), 'Color', 'k', 'LineWidth',PlotProps.Line.Width)
plot([.2 2.9], [2.845, -1.453], 'Color', AperiodicGray, 'LineWidth',PlotProps.Line.Width*3, ...
    'LineStyle',':')
chART.set_axis_properties(PlotProps)
xlabel('Log frequency')
ylabel('Log power')
xlim(log([1 20]))
ylim([-2 3])
axis square
box off
chART.save_figure('LogLogPower', ResultsFolder, PlotProps)



% FOOOF
[~, ~, WhitenedPower, FooofFrequencies] = fooof_spectrum(PowerAverage, Freqs);

% periodic power
figure('Units','centimeters', 'Position', PlotSize)
chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);
plot_highlighted_spectrum(WhitenedPower, FooofFrequencies, Bands, PlotProps)
legend off
xlim([1 20])
ylim([0 .7])
xlabel('Frequency (Hz)')
ylabel('Log power')
chART.save_figure('WhitePower', ResultsFolder, PlotProps)


% cartoon slope example
figure('Units','centimeters', 'Position', PlotSize)
chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);

hold on
X = [0 3];
Y = [1.5, -1.5];
Y2 = Y*2;
plot(X, Y, 'Color', AperiodicGray, 'LineWidth',PlotProps.Line.Width*3, ...
    'LineStyle',':')
chART.set_axis_properties(PlotProps)
plot(X, Y2, 'Color', [.4 .4 .4], 'LineWidth',PlotProps.Line.Width*3, ...
    'LineStyle',':')
xlabel('Log frequency')
ylabel('Log power')
xlim(log([0.8 35]))
ylim([-3 3])
set(gca, 'XTick', [], 'YTick', [])
axis square
box off
chART.save_figure('Slope', ResultsFolder, PlotProps)


% cartoon intercept example
figure('Units','centimeters', 'Position', PlotSize)
chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);

hold on
X = [0 3];
Y = [1.5, -1.5];
Y2 = Y+1.5;
plot(X, Y, 'Color', AperiodicGray, 'LineWidth',PlotProps.Line.Width*3, ...
    'LineStyle',':')
chART.set_axis_properties(PlotProps)
plot(X, Y2, 'Color', [.4 .4 .4], 'LineWidth',PlotProps.Line.Width*3, ...
    'LineStyle',':')
xlabel('Log frequency')
ylabel('Log power')
xlim(log([0.8 35]))
ylim([-2 4])
axis square
box off
chART.save_figure('Intercept', ResultsFolder, PlotProps)


% histogram quantity and amplitude
Frequencies = 4:.333:17;
[HistogramAmplitude, HistogramQuantities] = assemble_burst_distributions(Bursts, Frequencies, EEG.pnts);

figure('Units','centimeters', 'Position', PlotSize)
chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);
plot_multicolored_histogram(HistogramQuantities, Frequencies(1:end-1), Bands, PlotProps)
xlim([2 18])
xlabel('Frequency (Hz)')
ylabel('% recording')
chART.save_figure('Quantities', ResultsFolder, PlotProps)


figure('Units','centimeters', 'Position', PlotSize)
chART.sub_plot([], [1 1], [1 1], [], true, '', PlotProps);

plot_multicolored_histogram(HistogramAmplitude, Frequencies(1:end-1), Bands, PlotProps)
xlim([2 18])
xlabel('Frequency (Hz)')
ylabel('\muV')
chART.save_figure('Amplitudes', ResultsFolder, PlotProps)

