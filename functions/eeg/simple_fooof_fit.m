function [Slope, Intercept] = simple_fooof_fit(X, Y, Range)
% used for preprocessing

Results = fooof(X, Y, Range, struct(), false);

Slope = -Results.aperiodic_params(2);
Intercept = Results.aperiodic_params(1);