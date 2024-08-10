function [LogPower, Frequencies, Exponent, Offset, PeriodicPower, FooofFrequencies, ...
    Amplitude, Density, nBursts, Bursts] = simulate_analysis(Signal, ...
    SampleRate, WelchWindow, WelchWindowOverlap, SmoothSpan, PowerRange, BurstRange, CriteriaSet, Plot)



% calculate new power spectrum
[Power, Frequencies] = cycy.utils.compute_power(Signal, SampleRate, WelchWindow, WelchWindowOverlap);
% [Power, Frequencies] = cycy.utils.compute_power_fft(Signal, SampleRate);
PowerSmooth = cycy.utils.smooth_spectrum(Power, Frequencies, SmoothSpan);

% run FOOOF
[Exponent,  Offset, PeriodicPower, FooofFrequencies] = fooof_spectrum(PowerSmooth, Frequencies, [3 35]);

% average power
Range = dsearchn(Frequencies', PowerRange');
LogPower = mean(log10(PowerSmooth(Range(1):Range(2)))); % power

Range = dsearchn(FooofFrequencies', PowerRange');
PeriodicPower = mean(PeriodicPower(Range(1):Range(2))); % periodic power

% run cycle-by-cycle analysis
Bursts = cycy.simple_burst_detection(Signal, SampleRate, BurstRange, CriteriaSet);

% burst outcomes
nBursts = numel(Bursts); % nBursts

if isempty(Bursts) || numel(Bursts)<10
    Amplitude  = nan;
    Density = nan;
else
    Amplitude  = mean([Bursts.Amplitude]); % amplitudes
    Density = sum([Bursts.DurationPoints])/numel(Signal); % density
end

if Plot
    hold on
    plot(Frequencies, Power, 'LineWidth', 2)
    set(gca, 'YScale', 'log', 'XScale', 'log');
    xlabel('Frequency (Hz)')
    ylabel('Power')
    xlim([2 35])
end