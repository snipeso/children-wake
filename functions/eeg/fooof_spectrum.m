function [Slope, Intercept, WhitenedPower, FooofFrequencies] = fooof_spectrum(Power, Frequencies)

FooofFittingRange = [2 35];

Power = smooth_frequencies(Power, Frequencies, 2);

FooofModel = fooof(Frequencies, Power, FooofFittingRange, struct(), true);
FooofFrequencies = FooofModel.freqs;
Intercept = FooofModel.aperiodic_params(1);
Slope = FooofModel.aperiodic_params(2);

WhitenedPower = FooofModel.power_spectrum-FooofModel.ap_fit;