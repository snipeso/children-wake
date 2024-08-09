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
BurstRange = [8 12];
PowerRange = [4 16];

% sim parameters
Duration = 60*6; % the data's average
SampleRate = 250;

Aperiodic = struct();
Aperiodic.Offset = 1.6; % target: 2.25
Aperiodic.Exponent = 1.9;

Bursts = struct();
Bursts.Amplitude = 20; % target: 30
Bursts.Density = .5;
Bursts.Frequency = 10;
Bursts.Duration = 1;

CriteriaSet = struct();
CriteriaSet.PeriodConsistency = .5;
CriteriaSet.AmplitudeConsistency = .4;
CriteriaSet.FlankConsistency = .5;
CriteriaSet.ShapeConsistency = .2;
CriteriaSet.MonotonicityInTime = .4;
CriteriaSet.MonotonicityInAmplitude = .4;
CriteriaSet.ReversalRatio = .6;
CriteriaSet.MinCyclesPerBurst = 4;



%%% simulate evening measurement for a 3 year old
[Evening, t] = cycy.sim.eeg(Aperiodic, Bursts, Duration, SampleRate, WelchWindow);

figure
[EveningLogPower, Frequencies, EveningExponent, EveningOffset, EveningPeriodicPower, FooofFrequencies, ...
    EveningAmplitude, EveningDensity, EveningnBursts] = simulate_analysis(Evening, ...
    SampleRate, WelchWindow, WelchWindowOverlap, SmoothSpan, PowerRange, BurstRange, CriteriaSet, true);

disp('Evening: ')
disp(['Exponent = ', num2str(EveningExponent)])
disp(['Offset = ', num2str(EveningOffset)])
disp(['nBursts = ', num2str(EveningnBursts)])
disp(['Burst Amplitude = ', num2str(EveningAmplitude)])
disp(['Burst Density = ', num2str(EveningDensity)])
disp(['Power = ', num2str(EveningLogPower)])
disp(['Peroidic Power = ', num2str(EveningPeriodicPower)])





