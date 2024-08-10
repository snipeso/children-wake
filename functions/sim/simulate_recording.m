function [Exponent, Offset, Amplitude, Density, Power, PeriodicPower] = simulate_recording(ExponentOG, OffsetOG, DensityOG)

Duration = 20*60; % much longer than actual recording, because densities are always lower in simulated signal compared to those created (unless super high amplitudes)
SampleRate = 250;

WelchWindow = 4;
WelchWindowOverlap = .5;
SmoothSpan = 2;
BurstRange = [8 12];
PowerRange = [4 16];


Aperiodic = struct();
Aperiodic.Offset = OffsetOG; % target: 2.25
Aperiodic.Exponent = ExponentOG;

Bursts = struct();
Bursts.Amplitude = 20;
Bursts.Density = DensityOG;
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



[Evening, t] = cycy.sim.eeg(Aperiodic, Bursts, Duration, SampleRate, WelchWindow);

[Power, ~, Exponent, Offset, PeriodicPower, ~, ...
    Amplitude, Density, ~] = simulate_analysis(Evening, ...
    SampleRate, WelchWindow, WelchWindowOverlap, SmoothSpan, PowerRange, BurstRange, CriteriaSet, false);
