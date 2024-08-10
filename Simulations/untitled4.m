clear
clc
close all

Duration = 6*60;
Exponent = 1;
Offsets = -3:1:6;
SampleRate = 250;
WelchWindow = 4;
WelchWindowOverlap = .5;
SmoothSpan = 2;


WelchOffsets = nan(size(Offsets));
FFTOffsets = WelchOffsets;

for OIdx = 1:numel(Offsets)
    Offset = Offsets(OIdx);
 [Signal, t] = cycy.sim.simulate_aperiodic_eeg(Exponent, Offset, Duration, SampleRate, WelchWindow);
 % [Signal, t] = cycy.sim.simulate_aperiodic_eeg(Exponent, Offset, Duration, SampleRate);


 [Power, Frequencies] = cycy.utils.compute_power(Signal, SampleRate, WelchWindow, WelchWindowOverlap);
 PowerSmooth = cycy.utils.smooth_spectrum(Power, Frequencies, SmoothSpan);
 [Exponent,  WelchOffsets(OIdx), PeriodicPower, FooofFrequencies] = fooof_spectrum(PowerSmooth, Frequencies, [3 35]);

[Power, Frequencies] = cycy.utils.compute_power_fft(Signal, SampleRate);
PowerSmooth = cycy.utils.smooth_spectrum(Power, Frequencies, SmoothSpan);
[Exponent,  FFTOffsets(OIdx), PeriodicPower, FooofFrequencies] = fooof_spectrum(PowerSmooth, Frequencies, [3 35]);
end

figure;scatter(Offsets, WelchOffsets)