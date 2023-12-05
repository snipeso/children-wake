function [Slope, Intercept, WhitenedPower, FooofFrequencies] = fooof_spectrum(Power, Frequencies)

FooofFittingRange = [2 40];

Power = smooth_frequencies(Power, Frequencies, 2);

FooofModel = fooof(Frequencies, Power, FooofFittingRange, struct(), true);
FooofFrequencies = FooofModel.freqs;
Slope = FooofModel.aperiodic(1);
Intercept = FooofModel.aperiodic(2);

WhitenedPower = 10.^(FooofModel.power_spectrum-FooofModel.ap_fit);