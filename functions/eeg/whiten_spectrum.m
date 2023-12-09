function [WhitenedPower, FooofFrequencies] = whiten_spectrum(Power, Frequencies, simple_fooof_fittingRange)
% uses FOOOF

FooofModel = fooof(Frequencies, Power, simple_fooof_fittingRange, struct(), true);
FooofFrequencies = FooofModel.freqs;

WhitenedPower = 10.^(FooofModel.power_spectrum-FooofModel.ap_fit);