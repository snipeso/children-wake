function [Slope, Intercept, PeriodicPower, FooofFrequencies, Fit, AperiodicPower] = fooof_spectrum(Power, Frequencies, simple_fooof_fittingRange)


Power = smooth_frequencies(Power, Frequencies, 2);

try
    FooofModel = fooof(Frequencies, Power, simple_fooof_fittingRange, struct(), true);
    FooofFrequencies = FooofModel.freqs;
    Intercept = FooofModel.aperiodic_params(1);
    Slope = FooofModel.aperiodic_params(2);
    Fit = [FooofModel.error, FooofModel.r_squared];

    PeriodicPower = FooofModel.power_spectrum-FooofModel.ap_fit;
    AperiodicPower = FooofModel.ap_fit;
catch
    warning('couldnt fit fooof')
    Slope = nan;
    Intercept = nan;
    PeriodicPower = nan(1, numel(Frequencies)); % TODO: make this expected fooof frequencies
    FooofFrequencies = Frequencies;
    AperiodicPower =  nan(1, numel(Frequencies));
end