function [Slope, Intercept, WhitenedPower, FooofFrequencies, Fit] = fooof_spectrum(Power, Frequencies)

simple_fooof_fittingRange = [2 35];

Power = smooth_frequencies(Power, Frequencies, 2);

try
FooofModel = fooof(Frequencies, Power, simple_fooof_fittingRange, struct(), true);
FooofFrequencies = FooofModel.freqs;
Intercept = FooofModel.aperiodic_params(1);
Slope = FooofModel.aperiodic_params(2);
Fit = [FooofModel.error, FooofModel.r_squared];

WhitenedPower = FooofModel.power_spectrum-FooofModel.ap_fit;
catch
    warning('couldnt fit fooof')
    Slope = nan;
    Intercept = nan;
    WhitenedPower = nan(1, numel(Frequencies));
    FooofFrequencies = Frequencies;
end