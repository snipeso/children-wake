clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameters

Parameters = simulationParameters();

Duration = 60*6; % the data's average
SampleRate = 250;

AllMeasures = struct();
AllMeasures.Amplitudes = linspace(0, 100, 10);
ProtoAmp = 20;
AllMeasures.Densities = linspace(0, 1, 10);
ProtoDens = .2;
AllMeasures.Slopes = linspace(0, 4, 10);
ProtoSlope = 1.5;
AllMeasures.Intercepts = linspace(-2, 2, 10);
ProtoInt = -0.5;

ProtoMeasures = struct();
ProtoMeasures.Amplitudes = 20;
ProtoMeasures.Densities = .2;
ProtoMeasures.Slopes = 1.5;
ProtoMeasures.Intercepts = -.5;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run

% setup outcome structure
MeasureLabels = fieldnames(AllMeasures);
nMeasures = numel(MeasureLabels);

Outcomes = struct();
for MeasureIdx = 1:nMeasures
Outcomes.(MeasureLabels{MeasureIdx}) = nan(size(AllMeasures.(MeasureLabels{MeasureIdx})));
end
Outcomes.Power = nan(size(AllMeasures.(MeasureLabels{MeasureIdx})));
Outcomes.PeriodicPower = nan(size(AllMeasures.(MeasureLabels{MeasureIdx})));

% run simulations
for MeasureIdx = 1:nMeasures

    X = AllMeasures.(MeasureLabels{MeasureIdx});

    Measures = ProtoMeasures;

    figure
hold on
    for Idx = 1:numel(X)
        Measures.(MeasureLabels{MeasureIdx}) = X(Idx);
          
    [Aperiodic, t] = cycy.utils.simulate_aperiodic_eeg(Measures.Slopes, Measures.Intercepts, Duration, SampleRate);

    fAperiodic = cycy.utils.highpass_filter(Aperiodic, SampleRate, 0.8, 0.4, 'equiripple', 1, 80);
    fAperiodic = cycy.utils.lowpass_filter(fAperiodic, SampleRate, 40, 45);

    [Periodic, ~] = cycy.sim.simulate_periodic_eeg(Duration, SampleRate, BurstFrequency, BurstAmplitude, BurstDuration, BurstDensity);

    sumData = fAperiodic + Periodic;

    [Power, Freqs] = cycy.utils.compute_power_fft(sumData, SampleRate);
    PowerSmooth = cycy.utils.smooth_spectrum(Power, Freqs, SmoothSpan);

 plot(Freqs, PowerSmooth)
 chART.set_axis_properties(PlotProps)
 xlabel('Frequency (Hz)')
    end



end


% all
figure('Units','centimeters', 'Position',[0 0 20 20])







































%%% Simulate only aperiodic changes
Duration = 50;
SampleRate = 250;
% Slopes = -1:-.5:-3;
Slopes = -1.1:-.1:-2;
Intercept = 0;
BurstAmplitude = 5;
BurstDuration = 1;
BurstDensity = .5;
BurstFrequency = 10;
SmoothSpan = 2;

figure
hold on

for Slope = Slopes
    [Aperiodic, t] = cycy.utils.simulate_aperiodic_eeg(Slope, Intercept, Duration, SampleRate);

    fAperiodic = cycy.utils.highpass_filter(Aperiodic, SampleRate, 0.8, 0.4, 'equiripple', 1, 80);
    fAperiodic = cycy.utils.lowpass_filter(fAperiodic, SampleRate, 40, 45);

    [Periodic, ~] = cycy.sim.simulate_periodic_eeg(BurstFrequency, BurstAmplitude, BurstDuration, BurstDensity, Duration, SampleRate);

    sumData = fAperiodic + Periodic;

    [Power, Freqs] = cycy.utils.compute_power_fft(sumData, SampleRate);
    PowerSmooth = cycy.utils.smooth_spectrum(Power, Freqs, SmoothSpan);
    plot(Freqs, PowerSmooth)
end
set(gca, 'YScale', 'log', 'XScale', 'log');
title('Changing slope')

% note: periodic power in this case DECREASES, which emphasizes why its not
% appropriate measure for periodic activity


%%

%%% Simulate only aperiodic changes
Duration = 50;
SampleRate = 250;
% Slopes = -1:-.5:-3;
Slope = -1.5;
Intercepts = -2:.2:2;
BurstAmplitude = 5;
BurstDuration = 1;
BurstDensity = .5;
BurstFrequency = 10;
SmoothSpan = 2;

figure
hold on

for Intercept = Intercepts
    [Aperiodic, t] = cycy.utils.simulate_aperiodic_eeg(Slope, Intercept, Duration, SampleRate);

    fAperiodic = cycy.utils.highpass_filter(Aperiodic, SampleRate, 0.8, 0.4, 'equiripple', 1, 80);
    fAperiodic = cycy.utils.lowpass_filter(fAperiodic, SampleRate, 40, 45);

    [Periodic, ~] = cycy.sim.simulate_periodic_eeg(BurstFrequency, BurstAmplitude, BurstDuration, BurstDensity, Duration, SampleRate);

    sumData = fAperiodic + Periodic;

    [Power, Freqs] = cycy.utils.compute_power_fft(sumData, SampleRate);
    PowerSmooth = cycy.utils.smooth_spectrum(Power, Freqs, SmoothSpan);
    plot(Freqs, PowerSmooth)
end
set(gca, 'YScale', 'log', 'XScale', 'log');
title('Changing intercept')

%%

%%% Simulate only density changes
Duration = 50;
SampleRate = 250;
Slope = -1.5;
Intercept = 0;
BurstAmplitude = 20;
BurstDuration = 1;
BurstDensities = .1:.1:1;
BurstFrequency = 10;
SmoothSpan = 2;

figure
hold on

for BurstDensity = BurstDensities
    [Aperiodic, t] = cycy.utils.simulate_aperiodic_eeg(Slope, Intercept, Duration, SampleRate);

    fAperiodic = cycy.utils.highpass_filter(Aperiodic, SampleRate, 0.8, 0.4, 'equiripple', 1, 80);
    fAperiodic = cycy.utils.lowpass_filter(fAperiodic, SampleRate, 40, 45);

    [Periodic, ~] = cycy.sim.simulate_periodic_eeg(BurstFrequency, BurstAmplitude, BurstDuration, BurstDensity, Duration, SampleRate);

    sumData = fAperiodic + Periodic;

    [Power, Freqs] = cycy.utils.compute_power_fft(sumData, SampleRate);
    PowerSmooth = cycy.utils.smooth_spectrum(Power, Freqs, SmoothSpan);
    plot(Freqs, PowerSmooth)
end
set(gca, 'YScale', 'log', 'XScale', 'log');
title('Changing density')


%% simulate only Amplitude changes



%%% Simulate only density changes
Duration = 50;
SampleRate = 250;
% Slopes = -1:-.5:-3;
Slope = -1.5;
Intercept = 0;
BurstAmplitudes = 10:10:100;
BurstDuration = 1;
BurstDensities = .3;
BurstFrequency = 10;
SmoothSpan = 2;

figure
hold on

for BurstAmplitude = BurstAmplitudes
    [Aperiodic, t] = cycy.utils.simulate_aperiodic_eeg(Slope, Intercept, Duration, SampleRate);

    fAperiodic = cycy.utils.highpass_filter(Aperiodic, SampleRate, 0.8, 0.4, 'equiripple', 1, 80);
    fAperiodic = cycy.utils.lowpass_filter(fAperiodic, SampleRate, 40, 45);

    [Periodic, ~] = cycy.sim.simulate_periodic_eeg(BurstFrequency, BurstAmplitude, BurstDuration, BurstDensity, Duration, SampleRate);

    sumData = fAperiodic + Periodic;

    [Power, Freqs] = cycy.utils.compute_power_fft(sumData, SampleRate);
    PowerSmooth = cycy.utils.smooth_spectrum(Power, Freqs, SmoothSpan);
    plot(Freqs, PowerSmooth)
end
set(gca, 'YScale', 'log', 'XScale', 'log');

title('Changing amplitude')
