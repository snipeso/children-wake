clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameters

Parameters = simulationParameters();
Paths = Parameters.Paths;
ResultsFolder = Paths.Results;

WelchWindow = 4;
WelchWindowOverlap = .5;
SmoothSpan = 2;
PowerRange = [4 16];
BurstRange = [6 12];

CriteriaSet = struct();
    CriteriaSet.PeriodConsistency = .5;
CriteriaSet.AmplitudeConsistency = .4;
CriteriaSet.FlankConsistency = .5;
CriteriaSet.ShapeConsistency = .2;
CriteriaSet.MonotonicityInTime = .4;
CriteriaSet.MonotonicityInAmplitude = .4;
CriteriaSet.ReversalRatio = .6;
CriteriaSet.MinCyclesPerBurst = 4;



Channel = 72;

% P 158
% EveningFilepath = 'D:\Data\AllWake\Preprocessed\Power\Clean\Providence\Oddball\P157_Providence_Session1_mor_Oddball_n_2.mat';
EveningFilepath = 'D:\Data\AllWake\Preprocessed\Power\Clean\BMSSL\1GoNoGoGopher\P072_BMSSL_Session2_eve_1GoNoGoGopher_n_1.mat';
% EveningFilepath = 'D:\Data\AllWake\Preprocessed\Power\Clean\BMSAdults\Oddball\P169_BMSAdults_Session1_eve_Oddball_n_1.mat';

load(EveningFilepath, 'EEG')
EveningEEG = EEG;

OGSignal = EEG.data(labels2indexes(Channel, EEG.chanlocs), :);
SampleRate = EEG.srate;
Duration = numel(OGSignal)/SampleRate;

figure 
hold on
[LogPower, Frequencies, Exponent, Offset, PeriodicPower, FooofFrequencies, ...
    Amplitude, Density, nBursts, Bursts] = simulate_analysis(OGSignal, ...
    SampleRate, WelchWindow, WelchWindowOverlap, SmoothSpan, PowerRange, BurstRange, CriteriaSet, true);


Aperiodic = struct();
Aperiodic.Offset = Offset;
Aperiodic.Exponent = Exponent;

Periodic = struct();
Periodic.Amplitude = Amplitude;
Periodic.Density = Density*2;
Periodic.Frequency = mean([Bursts.BurstFrequency]);
Periodic.Duration =  mean([Bursts.DurationPoints])/SampleRate;

% [Evening, t] = cycy.sim.eeg(Aperiodic, Periodic, Duration, SampleRate, Duration);
[Evening, t] = cycy.sim.eeg(Aperiodic, Periodic, Duration, SampleRate, WelchWindow);

[SimLogPower, SimFrequencies, SimExponent, SimOffset, SimPeriodicPower, FooofFrequencies, ...
    SimAmplitude, SimDensity, SimnBursts, SimBursts] = simulate_analysis(Evening, ...
    SampleRate, WelchWindow, WelchWindowOverlap, SmoothSpan, PowerRange, BurstRange, CriteriaSet, true);

disp(num2str(SimOffset-Offset))
% %%
% load(replace(EveningFilepath, '_eve_', '_mor_'), 'EEG')
% MorningEEG = EEG;
% 
%     [Power, Freqs] = cycy.utils.compute_power(Output.EEG.data, SampleRate, WelchWindow, Overlap);
%    [Slope, Intercept, WhitenedPower, FooofFrequencies] = fooof_spectrum(Power(ChannelIdx, :), AllFrequencies, [2 35]);